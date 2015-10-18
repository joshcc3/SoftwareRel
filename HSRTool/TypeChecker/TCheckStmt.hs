module HSRTool.TypeChecker.TCheckStmt where

import Control.Applicative
import HSRTool.TypeChecker.Types
import HSRTool.Parser.Types
import Control.Monad.State
import Control.Monad.Writer
import HSRTool.TypeChecker.TCheckExpr

checkVarDecl v = do
  s <- get
  let tInfo = typeInfo s
  maybe 
   (do
     put (s{typeInfo = 
            tInfo { varMap = insert (varMap tInfo) v SCInt (scope s)}})
     return SCUnit)
   (\_ -> tell ["Redeclaraion of variable within scope: " ++ v] >> return SCAny)
   (lkup (varMap tInfo) v (scope s))

checkTypeStmt :: Stmt String -> TypeChecker SCType
checkTypeStmt (SVarDecl (VarDecl v)) = checkVarDecl v
checkTypeStmt (SAssignStmt (AssignStmt id e)) = do
  st <- get
  let s = typeInfo st
      sc = scope st
  maybe 
   (tell ["Undeclared variable: " ++ id] >> return SCAny)
   (\_ -> checkTypeExpr e)
   (lkup (varMap s) id sc)
checkTypeStmt (SAssertStmt (AssertStmt e)) = do
  checkTypeExpr e
checkTypeStmt (SAssumeStmt (AssumeStmt e)) = do
  checkTypeExpr e
checkTypeStmt (SHavocStmt (HavocStmt id)) = do
  st <- get
  let s = typeInfo st
  maybe 
   (tell ["Undeclared variable: " ++ id] >> return SCAny)
   return
   (lkup (varMap s) id (scope st))
checkTypeStmt (SIfStmt (IfStmt b t e)) = do
  st <- get
  checkTypeExpr b
  newScope
  let s = typeInfo st
  ts <- mapM checkTypeStmt t
  closeScope
  newScope
  ets <- maybe (return []) id (mapM checkTypeStmt <$> e)
  closeScope
  return SCUnit
checkTypeStmt (SBlockStmt s) = do
  newScope
  x <- mapM checkTypeStmt s
  closeScope
  return (last x)

newScope = do
  st <- get
  let stack = scopeStack st
  if null stack
  then put $ st { scope = 1, scopeStack = [0] }
  else put $ st { scope = scope st + 1, scopeStack = scope st:stack }

closeScope = do
  st <- get
  let stack = scopeStack st
  if null stack
  then error "Attempted to close a scope without any open scope"
  else put $ st { scope = head (scopeStack st), 
                  scopeStack = tail (scopeStack st) }
