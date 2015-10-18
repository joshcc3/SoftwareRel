module HSRTool.TypeChecker.TCheckStmt where

import Control.Applicative
import HSRTool.TypeChecker.Types
import HSRTool.Parser.Types
import Control.Monad.State
import Control.Monad.Writer
import HSRTool.TypeChecker.TCheckExpr

checkTypeStmt :: Stmt String -> TypeChecker SCType
checkTypeStmt (SVarDecl (VarDecl v)) = do
  s <- get
  let tInfo = typeInfo s
  maybe 
   (do
     put (s{typeInfo = tInfo { varMap = insert v SCInt (scope s) s}})
     return SCUnit)
   (\_ -> tell ["Redeclaraion of variable within scope: " ++ v] >> return SCAny)
   (lkup tInfo v (scope s))
checkTypeStmt (SAssignStmt (AssignStmt id e)) = do
  st <- get
  let s = typeInfo st
      sc = scope st
  maybe 
   (tell ["Undeclared variable: " ++ id] >> return SCAny)
   (\t -> do
     r <- checkTypes (e, t)
     maybe (return SCAny) return r)
   (lkup (varMap s) id sc)
checkTypeStmt (SAssertStmt (AssertStmt e)) = do
  mt <- checkTypes (e, SCBool)
  maybe 
   (tell ["Assert statement requires Bool"] >> return SCAny)
   (\t -> return t)
   mt
checkTypeStmt (SAssumeStmt (AssumeStmt e)) = do
  t <- checkTypeExpr e
  return t
checkTypeStmt (SHavocStmt (HavocStmt id)) = do
  s <- (typeInfo <$> get)
  maybe 
   (tell ["Undeclared variable: " ++ id] >> return SCAny)
   return
   (varMap s id)
checkTypeStmt (SIfStmt (IfStmt b t e)) = do
  st <- get
  b' <- checkTypes (b, SCBool)
  newScope
  let s = typeInfo st
  maybe (tell ["If condition should be a bool"]) (\_ -> return ()) b'
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
  then put $ st { scope = 0, scopeStack = [] }
  else put $ st { scope = scope st + 1, scopeStack = scope st:stack }

closeScope = do
  st <- get
  let stack = scopeStack st
  if null stack
  then error "Attempted to close a scope without any open scope"
  else put $ st { scope = head (scopeStack st), 
                  scopeStack = tail (scopeStack st) }
