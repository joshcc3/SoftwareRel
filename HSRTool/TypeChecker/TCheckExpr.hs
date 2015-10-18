module HSRTool.TypeChecker.TCheckExpr where

import Control.Applicative
import HSRTool.TypeChecker.Types
import HSRTool.Parser.Types
import Control.Monad.Writer
import Control.Monad.State

mapToTypes :: Expr String -> WriterT [String] (State TInfo) SCType
mapToTypes (EShortIf b x y) = do
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
        xT = mapToTypes x
        yT = mapToTypes y
        cond1 = (== SCBool) <$> mapToTypes b 
        cond2 = (==) <$> xT <*> yT
mapToTypes (EBinOp b) = toTypesBinOp b
mapToTypes (EUnOp u) = undefined -- toTypesUnOp u
mapToTypes (ELit _) = return SCInt
mapToTypes (EID x) = do
  s <- lift get
  maybe (tell ["Undefined variable: " ++ x] >> return SCAny)
        return $ varMap s x
mapToTypes EResult = return SCUnit
mapToTypes (EOld _) = return SCUnit

toTypesBinOp (x :|| y) = do
  m <- checkTypes (x, SCBool) (y, SCBool)
  maybe (return SCAny) (\_ -> return SCBool) m

toTypesBinOp (x :&& y) = do
  m <- checkTypes (x, SCBool) (y, SCBool)
  maybe (return SCAny) (\_ -> return SCBool) m
toTypesBinOp (x :| y) = do
  m <- checkTypes (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :^ y) = do
  m <- checkTypes (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :& y) = do
  m <- checkTypes (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :== y) = undefined
toTypesBinOp (x :!= y) = undefined -- fTppB
toTypesBinOp (x :< y) = undefined -- fTIIB
toTypesBinOp (x :<= y) = undefined -- fTIIB
toTypesBinOp (x :> y) = undefined -- fTIIB
toTypesBinOp (x :>= y) = undefined -- fTIIB

toTypesBinOp (x :<< y) = do
  m <- checkTypes (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :>> y) = do
  m <- checkTypes (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :+ y) = do
  m <- checkTypes (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :- y) = do
  m <- checkTypes (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :* y) = do
  m <- checkTypes (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :/ y) = do
  m <- checkTypes (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :% y) = do
  m <- checkTypes (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :? y) =  do
  m <- checkTypes (x, SCBool) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesBinOp (x :?: y) = do
  m <- checkTypes (x, SCInt) (y, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesUnOp ((::+) x) = do
  m <- checkTypes' (x, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
toTypesUnOp ((::-) x) = do
  m <- checkTypes' (x, SCInt)
  maybe (return SCAny) (\_ -> return SCInt) m
 -- Negated int literals are not currently parsed
toTypesUnOp ((::!) x) = do
  m <- checkTypes' (x, SCBool)
  maybe (return SCAny) (\_ -> return SCBool) m

checkTypes (a, t) (b, t') = do
  at <- mapToTypes a
  bt <- mapToTypes b
  if at /= t
  then tell ["Could not match types: " ++ show at ++ " and " ++ show t] >> return Nothing
  else if bt /= t'
       then tell ["Could not match types: " ++ show bt ++ " and " ++ show t'] >> return Nothing
       else return (Just t)

checkTypes' (a, t) = do
  at <- mapToTypes a
  if at /= t
  then tell ["Could not match types: " ++ show at ++ " and " ++ show t] >> return Nothing
  else return (Just t)
