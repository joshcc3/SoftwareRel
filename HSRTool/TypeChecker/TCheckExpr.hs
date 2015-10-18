module HSRTool.TypeChecker.TCheckExpr where

import Control.Applicative
import HSRTool.TypeChecker.Types
import HSRTool.Parser.Types
import Control.Monad.Writer
import Control.Monad.State

checkTypeExpr :: Expr String -> TypeChecker SCType
checkTypeExpr (EShortIf b x y) = do
  c1 <- cond1
  c2 <- cond2
  if c1
  then tell ["Type doesn't match Bool in short cut if condition"]
  else return ()
  if c2 
  then tell ["Types don't match in short cut if alternatives"]
  else return ()
  if c1 && c2
  then xT
  else return SCAny
      where 
        xT = checkTypeExpr x
        yT = checkTypeExpr y
        cond1 = (== SCBool) <$> checkTypeExpr b 
        cond2 = (==) <$> xT <*> yT
checkTypeExpr (EBinOp b) = toTypesBinOp b
checkTypeExpr (EUnOp u) = toTypesUnOp u
checkTypeExpr (ELit _) = return SCInt
checkTypeExpr (EID x) = do
  st <- get
  let s = typeInfo st
  maybe (tell ["Undefined variable: " ++ x] >> return SCAny)
        return $ lkup (varMap s) x (scope st)
checkTypeExpr EResult = return SCInt
checkTypeExpr (EOld x) = do
  st <- get
  let s = typeInfo st
  maybe (tell ["Undefined variable: " ++ x] >> return SCAny)
        return $ lkup (varMap s) x (scope st)
toTypesBinOp (x :|| y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCBool
toTypesBinOp (x :&& y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCBool
toTypesBinOp (x :| y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCInt
toTypesBinOp (x :^ y) = do
  checkTypeExpr x
  checkTypeExpr y
toTypesBinOp (x :& y) = do
  checkTypeExpr x
  checkTypeExpr y
toTypesBinOp (x :== y) = do
  tx <- checkTypeExpr x
  ty <- checkTypeExpr y
  if tx == ty
  then return SCBool
  else tell ["Types do not match in equals expression: " ++ show tx ++ " and " ++ show ty] >> return SCAny
toTypesBinOp (x :!= y) = do
  tx <- checkTypeExpr x
  ty <- checkTypeExpr y
  if tx == ty
  then return SCBool
  else tell ["Types do not match in equals expression: " ++ show tx ++ " and " ++ show ty] >> return SCAny
toTypesBinOp (x :< y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCBool
toTypesBinOp (x :<= y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCBool
toTypesBinOp (x :> y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCBool
toTypesBinOp (x :>= y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCBool
toTypesBinOp (x :<< y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCInt
toTypesBinOp (x :>> y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCInt
toTypesBinOp (x :+ y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCInt
toTypesBinOp (x :- y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCInt
toTypesBinOp (x :* y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCInt
toTypesBinOp (x :/ y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCInt
toTypesBinOp (x :% y) = do
  checkTypeExpr x
  checkTypeExpr y
  return SCInt
toTypesBinOp (x :? y) =  do
  checkTypeExpr x
  checkTypeExpr y
toTypesBinOp (x :?: y) = do
  tx <- checkTypeExpr x
  ty <- checkTypeExpr y
  if tx == ty
  then return tx
  else tell ["Types do not match in equals expression: " ++ show tx ++ " and " ++ show ty] >> return SCAny
toTypesUnOp ((::+) x) = do
  checkTypeExpr x
  return SCInt
toTypesUnOp ((::-) x) = do
  checkTypeExpr x
  return SCInt
 -- Negated int literals are not currently parsed
toTypesUnOp ((::!) x) = do
  checkTypeExpr x
  return SCBool
{-
checkTypes (a, t) = do
  t' <- checkTypeExpr a
  if t' == t
  then return (Just t)
  else tell ["Could not unify: " ++ show t' ++ ", " ++ show t] >> return Nothing
-}
