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
checkTypeExpr EResult = return SCUnit
checkTypeExpr (EOld _) = return SCUnit

toTypesBinOp (x :|| y) = do
  m <- check2Types (x, SCBool) (y, SCBool)
  maybe (return SCAny) (\_ -> return SCBool) m

toTypesBinOp (x :&& y) = do
  m <- check2Types (x, SCBool) (y, SCBool)
  maybe (return SCAny) (\_ -> return SCBool) m
toTypesBinOp (x :| y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :^ y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :& y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
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
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :<= y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :> y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :>= y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :<< y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :>> y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :+ y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :- y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :* y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :/ y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :% y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :? y) =  do
  m <- check2Types (x, SCBool) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :?: y) = do
  m <- check2Types (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesUnOp ((::+) x) = do
  m <- checkTypes (x, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesUnOp ((::-) x) = do
  m <- checkTypes (x, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
 -- Negated int literals are not currently parsed
toTypesUnOp ((::!) x) = do
  m <- checkTypes (x, SCBool)
  maybe (return SCAny) (\_ -> return SCBool) m

check2Types (a, t) (b, t') =
  (liftA2 . liftA2) const (checkTypes (a, t)) (checkTypes (b, t'))

checkTypes (a, t) = do
  at <- checkTypeExpr a
  if at /= t
  then tell ["Could not match types: " ++ show at ++ " and " ++ show t] >> return Nothing
  else return (Just t)
