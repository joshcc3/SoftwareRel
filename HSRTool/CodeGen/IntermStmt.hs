{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module HSRTool.CodeGen.IntermStmt(genIntermProg, genIntermPDecl, initSt) where

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
      stmtAction (SIfStmt' a)
          = either' (either' enterIf atThen) (either' atElse exitIf) (extract a)
            where 
              enterIf = (\_ -> _mp <$> get)
              atThen = (\_ -> openScope)
              atElse = (\_ -> closeScope >> openScope)
              exitIf = (\_ -> closeScope)
      stmtAction _ = _mp <$> get

vdecl :: VarDecl String a -> VarDecl String (State St' Mp)
vdecl v = v & vInfo .~ updateState (T._varId v)

fparams :: FormalParam String a -> FormalParam String (State St' Mp)
fparams v = v & fpInfo .~ updateState (_fID v)

prePost :: PrePost id a -> PrePost id (State St' Mp)
prePost p = (_mp <$> get) <$ p

procedureDecl :: ProcedureDecl String a -> ProcedureDecl String (State St' Mp)
procedureDecl p = g (p =>> pdAction)
    where 
      g (PDecl i id fp pp sts e) 
          = PDecl i id (map fparams fp) (map prePost pp) (map stmt sts) e

pdAction p = do
        let pInfo = fst (_pdeclInfo p)
            pName = _pId p
        either' (\_ -> updateState pName >> openScope) (\_ -> closeScope) pInfo

program (Program _ vs ps)
      = Program get (map vdecl vs) (map procedureDecl ps)

genIntermProg :: Program a String a -> State St' (Program Mp NewId St')
genIntermProg p
    = case program p of
        Program a vs ps -> Program <$> a 
                          <*> traverse (bitraverse subst id) vs
                          <*> traverse genIntermPDecl ps

genIntermPDecl p = getPDecl <$> bitraverse subst id (PD p)

initSt = St' M.empty
