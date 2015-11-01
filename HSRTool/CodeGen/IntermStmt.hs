{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module HSRTool.CodeGen.IntermStmt
    (genIntermProg, initSt, IntermId(..), St'(..)) where

import Debug.Trace
import HSRTool.Parser.Types as T
import HSRTool.CodeGen.Utils
import Control.Monad.State hiding (mapM_)
import Control.Monad.Writer hiding (mapM_)
import Data.Distributive
import Data.Traversable
import Data.Foldable hiding (mapM_)
import Control.Comonad
import Data.Monoid
import Control.Applicative
import Control.Lens
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Bitraversable


data IntermId = IntermId {
      _varId :: String,
      _count :: Int } deriving(Eq, Ord, Read)
instance Show IntermId where
    show (IntermId v c) = v ++ show c
makeLenses ''IntermId

type NextCount = Int
type Mp = M.Map String ([(IntermId, ScopeNum)], NextCount)
data St' = St' {
      _mp :: Mp
    } deriving (Eq, Ord, Show, Read)
makeLenses ''St'

newDef :: (Functor m, MonadState St' m) => String -> m Mp
newDef id = do
  mp %= M.alter (update id) id
  _mp <$> get
      where 



updateState :: (Functor m, MonadState St' m) => String -> m Mp
updateState id = do
  mp.ix id %= M.alter (update id) id
  _mp <$> get
      where
        updateId id = id & _1.ix 0._1.count .~ view _2 id & _2 %~ (+1)

openScope :: (Functor m, MonadState St' m) => m Mp
openScope = do
  mp.traverse._1.ix 0._2 += 1
  _mp <$> get

closeScope :: (Functor m, MonadState St' m) => m Mp
closeScope = do
  mp %= M.mapMaybe g
  _mp <$> get
    where
      g ([], _) = Nothing
      g (((iId, 0), nextCount):[], _) = Nothing
      g (((iId, 0), nextCount):r, c) = (r, c)
      g (((iId, n), nextCount):r, c) = (((iId, n-1), nextCount):r, c)
subst :: String -> State St' IntermId
subst s = do
  st <- get
  return (lkup (_mp st) s)

lkup :: [Mp] -> String -> IntermId
lkup mps var | null val = error "No variable defined in scope"
            | otherwise = head val
    where
      innerMostDef = foldMap (First . M.lookup var) mps
      val = maybe
             (error "Uninitialized Variable encountered in lookup")
             (fst . fst)
             innerMostDef

stmt :: Stmt String a -> Stmt String (State St' Mp)
stmt s = s =>> stmtAction
    where
      stmtAction (S (SVarDecl (VarDecl _ id))) = newDef id
      stmtAction (S (SAssignStmt (AssignStmt _ id _))) = updateState id
      stmtAction (S (SHavocStmt (HavocStmt _ id))) = updateState id
      stmtAction (S (SBlockStmt (e,_) _))
          = either' (\_ -> openScope) (\_ -> closeScope) e
      stmtAction (S (SIfStmt a _ th el))
          = either' (either' enterIf afterThen) (either' afterElse exitIf) a
            where 
              enterIf _ = openScope
              afterThen _ = do
                m <- _mp <$> get
                closeScope
                openScope
                return m
              afterElse _ = closeScope
              exitIf _ = mapM_ updateState (S.elems mset) >> _mp <$> get
                  where 
                    mset = S.union (foldMap modset th) ((foldMap.foldMap) modset el)
      stmtAction (S (SIfStmt' a))
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

program :: Program String a -> Program String (State St' Mp)
program (P (Program _ vs ps))
      = P (Program (_mp <$> get) (map vdecl vs) (map procedureDecl ps))

genIntermProg :: Program String a -> State St' (Program IntermId Mp)
genIntermProg p
    = case program p of
        P (Program a vs ps) -> P <$> (Program <$> a
                          <*> traverse (bitraverse subst id) vs
                          <*> traverse genIntermPDecl ps)

genIntermPDecl :: ProcedureDecl String (State St' a) 
               -> State St' (ProcedureDecl IntermId a)
genIntermPDecl p = getPDecl <$> bitraverse subst id (PD p)

initSt = St' M.empty
