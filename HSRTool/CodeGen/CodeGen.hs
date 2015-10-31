{-# LANGUAGE TemplateHaskell #-}

module HSRTool.CodeGen.CodeGen(runSSAGenerator) where

import Control.Monad.Cont
import Control.Comonad
import Data.Either (partitionEithers)
import Data.Maybe
import Data.Monoid
import Control.Applicative
import qualified Data.Set as S
import Control.Lens
import HSRTool.CodeGen.Types
import HSRTool.Parser.Types hiding (_varId)
import HSRTool.CodeGen.IntermStmt
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Data.Foldable (foldMap)
type Assumption = NewExpr
type Pred = Expr
type M id id' = M.Map id id'

data St id = St {
      _ass :: Assumption Op id
} deriving (Eq, Ord, Show, Read)
makeLenses ''St

runStack :: b -> StateT b (WriterT w m) a -> m ((a, b), w)
runStack s = runWriterT . flip runStateT s


runSSAGenerator :: Program IntermId (M String IntermId) -> IO (SSA Op IntermId)
runSSAGenerator (P (Program _ vD pD)) = do
  o <- runStack initSt pDecls
  return (snd o)
    where
      initSt = St (NE (ELit 1))
      pDecls = mapM_ (\x -> toSSA x (ELit 1)) $ pD >>= _pStmts

type SSAEval id = StateT (St id) (WriterT (SSA Op id) IO)

genStat (PPReq _ e) = Left (S (SAssumeStmt (AssumeStmt () e)))
genStat (PPEns _ e) = Right (S (SAssertStmt (AssertStmt () e)))
transProg = pPDecls.traverse %~ g
    where
      g p = case partitionEithers (map genStat (_pPrepost p)) of
              (ls, rs) -> p & pStmts %~ (ls ++) . (++ rs)

toSSA :: Stmt IntermId (M String IntermId) -> Pred Op IntermId -> SSAEval IntermId ()
toSSA (S (SAssignStmt (AssignStmt _ v e))) p = tell [SSAAssign v (NE e)]
toSSA (S (SAssertStmt (AssertStmt _ e))) p = do
  assmpts <- _ass <$> get
  tell [SSAAssert ((NEBinOp LAnd (NE p) assmpts) :=> NE e)]
toSSA (S (SBlockStmt _ l)) p = mapM_ (\x -> toSSA x p) l
toSSA (S (SIfStmt'' aInfo e tn (Just el))) p = do
  let
      (_, thenMap, elseMap, exitMap) = (extract . extract) aInfo
      thenModset = foldMap (S.map _varId . modset) tn
      elseModset = foldMap (S.map _varId . modset) el
      thenCond = EBinOp LAnd (Pair p e)
      elseCond = EBinOp LAnd (Pair p (EUnOp LNot (UnOp e)))
      g v = tell [SSAAssign (lkup exitMap v)
                  (NE $ EShortIf e
                          (EID $ lkup thenMap v) 
                          (EID $ lkup elseMap v))]
  mapM_ (\x -> toSSA x thenCond) tn
  mapM_ (\x -> toSSA x elseCond) el
  mapM_ g (S.elems (S.union thenModset elseModset))
toSSA (S (SAssumeStmt (AssumeStmt _ e))) p 
    = ass %= \x -> NEBinOp LAnd x (NE p) :=> NE e
toSSA _ _ = return ()

modset :: Ord id => Stmt id a -> S.Set id 
modset (S(SVarDecl (VarDecl _ _))) = S.empty 
modset (S(SAssignStmt (AssignStmt _ id e))) = S.fromList [id]
modset (S(SAssertStmt (AssertStmt _ e))) = S.empty
modset (S(SAssumeStmt (AssumeStmt _ e))) = S.empty
modset (S(SHavocStmt (HavocStmt _ id))) = S.fromList [id]
modset (S(SIfStmt (IfStmt _ b th Nothing))) = foldMap modset th
modset (S(SIfStmt (IfStmt _ b th (Just el)))) = foldMap modset th `S.union` foldMap modset el
modset (S(SBlockStmt _ stmts)) = foldMap modset stmts

lkup m v = case M.lookup v m of
             Just x -> x
             Nothing -> error ("Undefined variable in lookup: " ++ v)
