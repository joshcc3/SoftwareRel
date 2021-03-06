{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE
  DeriveFunctor,
  DeriveFoldable,
  DeriveTraversable,
  DeriveGeneric,
  TypeSynonymInstances,
  TemplateHaskell #-}

module HSRTool.CodeGen.IntermStmt
    (toIntermediateForm, initSt', IntermId(..), St'(..)) where

{- |
     This module converts the 'AST' generated by the parser into
     intermediate form.
     The intermediate form, relabels all nodes with their intermediate
     ids and annotates all the info nodes in the ast with the map
     from string variable ids to their intermediate ids.
     It also replaces occurences of EResult with the return expression
   | -}

import HSRTool.Utils
import Control.DeepSeq
import HSRTool.Test.TestIntermForm
import Debug.Trace
import HSRTool.Parser.Types as T
import Data.Either
import HSRTool.CodeGen.Types
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

{- |
     Substituates an identifier for its Intermediate ID
     Precondition: id must already be present in the map
     ids are inserted into the map at the time of variable declarations
   | -}
subst :: String -> State St' IntermId
subst varId = do
  map <- _mp <$> get
  return . maybe (undefVarErr varId) id $ SM.lookup varId map

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

{- |
     Updates an (IntermId, NextCount) pair
   | -}
updateId :: (IntermId, Int) -> IntermId
updateId (id, n) = id&countIntermId .~ Just n



{- |
     Updates the definition for varId in the map to reflect a
     Havoc/Assign statement
   | -}
updateState :: String -> State St' Mp
updateState varId = do
  cm <- _countMap <$> get
  m <- _mp <$> get
  let curCount = maybe initialSSAId id (M.lookup varId cm) -- TODO: Add a better way to synchronize the maps
      curId = maybe (undefVarErr varId) id (SM.lookup varId m)
      newId = updateId (curId, curCount)
  mp .= SM.updateDef m varId newId
  countMap %= M.insertWith (+) varId 1
  _mp <$> get

initialSSAId = 0

newDefVal id = IntermId id (Just initialSSAId)

{- |
     The following helper functions annotate each info node in the
     ast with the map from string id to intermediate id
   | -}

stmt :: Stmt String () -> Stmt String (State St' Mp)
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
                closeScope
                m <- _mp <$> get
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

vdecl :: VarDecl String () -> VarDecl String (State St' Mp)
vdecl v@(VarDecl _ id) = v & vInfo .~ do
                           mp %= \x -> SM.newDef x id (newDefVal id)
                           _mp <$> get

fparams :: FormalParam String () -> FormalParam String (State St' Mp)
fparams f@(FParam _ id) = f & fpInfo .~ do
                            mp %= \x -> SM.newDef x id (newDefVal id)
                            _mp <$> get

prePost :: PrePost id () -> PrePost id (State St' Mp)
prePost p = (_mp <$> get) <$ p


procedureDecl :: NFData () => ProcedureDecl' () String () -> State St' (ProcedureDecl' Mp IntermId Mp)
procedureDecl p@(PDecl info pName fps pps pstmts pexpr) = getPDecl <$> bitraversePDecl subst id (PD p')
    where
      pInfo = _pdeclInfo (p =>> pdaction)
      fps' = map fparams fps
      pps' = map prePost pps
      pstmts' = map stmt pstmts
      p' = PDecl pInfo pName fps' pps' pstmts' pexpr
      pdaction p = do
        let pInfo = fst (_pdeclInfo p)
            pName = _pId p
        either' (\_ -> do
                   mp %= \x -> SM.newDef x pName (newDefVal pName)
                   openScope)
                (\_ -> closeScope)
                pInfo

genStat :: PrePost id a -> Either (Stmt id a) (Stmt id a)
genStat (PPReq a e) = Left (S (SAssumeStmt (AssumeStmt a e)))
genStat (PPEns a e) = Right (S (SAssertStmt (AssertStmt a e)))

transProg :: Program String () -> Program String ()
transProg = unP.pPDecls.traverse %~ g
    where
      g p = case partitionEithers (map genStat (_pPrepost p)) of
              (ls, rs) -> p & pStmts %~ \s -> ls ++ s ++ (assignRetExpr (_pExpr p)):rs
      assignRetExpr e = S (SAssignStmt (AssignStmt accInfo specialId e))
      accInfo = (Either' (Left ()), Either' (Right ()))


toIntermediateForm :: Program String () -> State St' (Program IntermId Mp)
toIntermediateForm p
    = case transProg p of
        (P (Program a vs ps)) -> P <$> (Program <$> (_mp <$> get)
                                 <*> (traverse (bitraverse subst id) . map vdecl $ vs)
                                 <*> (traverse procedureDecl ps))

initSt' = St' (SM.newDef SM.empty specialId specialIntermId)
             (M.fromList [(specialId, 1)])
