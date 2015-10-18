module HSRTool.TypeChecker.TCheckStmt where

import Control.Applicative
import HSRTool.TypeChecker.Types
import HSRTool.Parser.Types
import Control.Monad.State
import Control.Monad.Writer
import HSRTool.TypeChecker.TCheckExpr

checkTypeStmt :: Stmt String -> TypeChecker SCType
checkTypeStmt (SVarDecl v) = do
  s <- get
  put (s{varMap = insert v SCInt s})
  return SCUnit
checkTypeStmt (SAssignStmt (AssignStmt id e)) = do
  s <- get
  maybe 
   (tell ["Undeclared variable: " ++ id] >> return SCAny)
   (\t -> do
     r <- checkTypes (e, t)
     maybe (return SCAny) return r)
   (varMap s id)
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
  s <- get
  maybe 
   (tell ["Undeclared variable: " ++ id] >> return SCAny)
   return
   (varMap s id)
checkTypeStmt (SIfStmt (IfStmt b t e)) = do
  s <- get
  b' <- checkTypes (b, SCBool)
  maybe 
   (tell ["If condition should be a bool"])
   (\_ -> return ())
   b'
  ts <- mapM checkTypeStmt t
  ets <- maybe (return []) id (mapM checkTypeStmt <$> e) 
  return SCUnit
checkTypeStmt (SBlockStmt s) = do
  x <- mapM checkTypeStmt s
  return (last x)
