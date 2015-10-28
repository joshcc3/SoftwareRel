{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module HSRTool.CodeGen.IntermStmt(updateState, subst, stmt, St'(..)) where

import HSRTool.Parser.Types as T
import HSRTool.CodeGen.Utils
import Control.Monad.State
import Data.Distributive
import Data.Traversable
import Data.Foldable
import Control.Comonad
import Data.Monoid
import Control.Applicative
import Control.Lens
import qualified Data.Map as M
import Data.Bitraversable


data NewId = NewId { 
      _varId :: String, 
      _count :: Int } deriving(Eq, Ord, Read)
instance Show NewId where
    show (NewId v c) = v ++ show c
makeLenses ''NewId

type NextCount = Int
type Mp = M.Map String ([NewId], NextCount)
data St' = St' {
      _mp :: Mp
    } deriving (Eq, Ord, Show, Read)
makeLenses ''St'

updateState :: (Functor m, MonadState St' m) => String -> m Mp
updateState id = do
  mp %= M.alter (upd id) id
  _mp <$> get
      where 
        upd id = Just . maybe (freshId id) updateId
        freshId id = ([NewId id 1], 2)
        updateId id =  id & _1.ix 0.count .~ view _2 id & _2 %~ (+1)
    
openScope :: (Functor m, MonadState St' m) => m Mp
openScope = do
  mp.traverse._1 %= g
  _mp <$> get
    where 
      g [] = []
      g (x:xs) = x:x:xs

closeScope :: (Functor m, MonadState St' m) => m Mp
closeScope = do
  mp %= M.mapMaybe g
  _mp <$> get
    where 
      g ([], _) = Nothing
      g (_:[], _) = Nothing
      g x = Just (x & _1 %~ tail)
subst :: String -> State St' NewId
subst s = do
  st <- get
  return (lkup (_mp st) s)

lkup :: Mp -> String -> NewId
lkup mp var | null val = error "No variable defined in scope"
            | otherwise = head val
    where 
      val = maybe 
             (error "Uninitialized Variable encountered in lookup") 
             fst
             (M.lookup var mp)

stmt :: Stmt String a -> Stmt String (State St' Mp)    
stmt s = s =>> stmtAction
    where 
      stmtAction (SVarDecl (VarDecl _ id)) = updateState id
      stmtAction (SAssignStmt (AssignStmt _ id _)) = updateState id
      stmtAction (SHavocStmt (HavocStmt _ id)) = updateState id
      stmtAction (SBlockStmt (e,_) _) 
          = either' (\_ -> openScope) (\_ -> closeScope) e
      stmtAction _ = _mp <$> get

vdecl :: VarDecl String a -> VarDecl String (State St' Mp)
vdecl v = v & vInfo .~ updateState (T._varId v)

fparams :: FormalParam String a -> FormalParam String (State St' Mp)
fparams v = v & fpInfo .~ updateState (_fID v)

prePost :: PrePost id a -> PrePost id (State St' Mp)
prePost p = (_mp <$> get) <$ p

genIntermProg :: ProcedureDecl String a -> State St' (ProcedureDecl NewId Mp)
genIntermProg procDecl = undefined -- bitraverse subst id (procedureDecl procDecl)

procedureDecl :: ProcedureDecl' Mp String a -> ProcedureDecl' Mp String (State St' Mp)
procedureDecl p  
    =  undefined {-PDecl 
      (updateState (_pId p)) 
      (_pId p) 
      (map fparams (_pFParams p))
      (map prePost (_pPrepost p)) 
      (map stmt (_pStmts p))
      (_pExpr p)-}

{-program p
      = Program
        get
        (map vdecl (_pVarDecls p))
        (map -}
