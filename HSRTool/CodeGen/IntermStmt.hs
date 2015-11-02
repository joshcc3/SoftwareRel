{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module HSRTool.CodeGen.IntermStmt
    (program, initSt, IntermId(..), St'(..)) where

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

stmt :: Stmt String a -> State St' (Stmt String Mp)
stmt s = undefined --res
    where 
      zipTrees (S (SVarDecl x)) (S (SVarDecl x')) = S (SVarDecl (x' & vInfo .~ (x ^.vInfo)))
      zipTrees (S (SAssignStmt x)) (S (SAssignStmt x')) = (S (SAssignStmt (x' & assInfo .~ (x ^. assInfo)))) 
      zipTrees (S (SHavocStmt x)) (S (SHavocStmt x')) = (S (SHavocStmt (x' & havocInfo .~ (x ^. havocInfo))))
      zipTrees (S (SBlockStmt x sts')) (S (SBlockStmt x' sts)) = S (SBlockStmt x (zipWith zipTrees sts' sts))
      zipTrees (S (SIfStmt a _ th' el')) (S (SIfStmt _ e th el)) = S (SIfStmt a e (zipWith zipTrees th' th) (zipWith zipTrees <$> el' <*> el))
      res = undefined -- fmap (\s' -> traverse id (zipTrees s' s =>> stmtAction)) res
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
              afterThen (_, oldMap, _, _) = do
                closeScope
                m <- _mp <$> get
                openScope
                mp .= oldMap
                return m
              afterElse _ = do
                closeScope
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


procedureDecl :: ProcedureDecl String a -> State St' (ProcedureDecl String Mp)
procedureDecl p = undefined {-traverse id-} (p =>> pdaction) 
    where 
      pdaction :: ProcedureDecl String a -> State St' Mp
      pdaction p = do
        let pInfo = fst (_pdeclInfo p)
            pName = _pId p
        either' (\_ -> do
                   mp %= \x -> SM.newDef x pName (newDefVal pName)
                   openScope)
                (\_ -> closeScope)
                pInfo

program :: Program String a -> State St' (Program String Mp)
program (P (Program a vs ps))
      = P <$> (Program <$> (_mp <$> get) <*> ((traverse.traverse) id . map vdecl $ vs) <*> (traverse procedureDecl ps))

genIntermPDecl :: ProcedureDecl String (State St' a) 
               -> State St' (ProcedureDecl IntermId a)
genIntermPDecl p = getPDecl <$> bitraverse subst id (PD p)

initSt = St' M.empty
