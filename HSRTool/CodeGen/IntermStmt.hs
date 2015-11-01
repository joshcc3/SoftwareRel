{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module HSRTool.CodeGen.IntermStmt
    (genIntermProg, initSt, IntermId(..), St'(..)) where

--import HSRTool.Test.TestIntermForm
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
import qualified HSRTool.CodeGen.ScopedMap as SM

data IntermId = IntermId {
      _varId :: String,
      _count :: Maybe Int } deriving(Eq, Ord, Read)
instance Show IntermId where
    show (IntermId v c) = v ++ maybe "" show c
makeLenses ''IntermId

type NextCount = Int
type Mp = SM.ScopedMap String (IntermId, NextCount) 

data St' = St' {
      _mp :: Mp
    } deriving (Eq, Ord, Show, Read)
makeLenses ''St'

subst :: String -> State St' IntermId
subst id = do
  map <- _mp <$> get
  return . maybe (undefVarErr id) fst $ SM.lookup id map

undefVarErr :: String -> a
undefVarErr varId = error $ "No value present for " ++ varId

openScope :: State St' Mp 
openScope = do
  mp %= SM.newScope
  _mp <$> get

closeScope :: State St' Mp 
closeScope = do
  mp %= SM.closeScope
  _mp <$> get


updateId :: (IntermId, NextCount) -> (IntermId, NextCount)
updateId (id, n) = (id&count .~ Just n, n+1)
           
updateState :: String -> State St' Mp
updateState varId = do
  m <- _mp <$> get
  let curId = maybe (undefVarErr varId) id (SM.lookup varId m)
      newId = updateId curId
  mp .= SM.updateDef m varId newId
  _mp <$> get

newDefVal id = (IntermId id (Just 0), 1)

stmt :: Stmt String a -> Stmt String (State St' Mp)
stmt s = s =>> stmtAction
    where
      stmtAction (S (SVarDecl (VarDecl _ id))) = do
        mp %= \x -> SM.newDef x id (newDefVal id)
        _mp <$> get
      stmtAction (S (SAssignStmt (AssignStmt (a,_) id _))) 
          = either' (\_ -> _mp <$> get) (\_ -> updateState id) a
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
              afterElse _ = do
                m <- _mp <$> get
                closeScope
                return m
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
vdecl v@(VarDecl _ id) = v & vInfo .~ do
                           mp %= \x -> SM.newDef x id (newDefVal id)
                           _mp <$> get

fparams :: FormalParam String a -> FormalParam String (State St' Mp)
fparams f@(FParam _ id) = f & fpInfo .~ do
                            mp %= \x -> SM.newDef x id (newDefVal id)
                            _mp <$> get

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
        either' (\_ -> do
                   mp %= \x -> SM.newDef x pName (newDefVal pName)
                   openScope) 
                (\_ -> closeScope) 
                pInfo

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
