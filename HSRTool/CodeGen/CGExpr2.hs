{-# LANGUAGE TemplateHaskell #-}

module HSRTool.CodeGen.CGExpr2 where

import Control.Monad.Cont
import Control.Comonad
import Data.Either (partitionEithers)
import Data.Maybe
import Data.Monoid
import Control.Applicative
import qualified Data.Set as S
import Control.Lens
import HSRTool.CodeGen.Types
import HSRTool.Parser.Types
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

runSSAGenerator :: Program a String a -> IO (SSA Op NewId)
runSSAGenerator (Program _ vD pD) = do
  o <- runStack initSt pDecls
  return (snd o)
    where
      initSt = St (NE (ELit 1))
      g x = do
        m %= initialize (_varId x)
      pDecls = do
        mapM_ g vD
        mapM_ (flip toSSA (ELit 1)) $ pD >>= _pStmts

type SSAEval id = StateT (St id) (WriterT (SSA Op id) IO)

genStat (PPReq _ e) = Left (SAssumeStmt (AssumeStmt () e))
genStat (PPEns _ e) = Right (SAssertStmt (AssertStmt () e))
transProg = pPDecls.traverse %~ g
    where
      g p = case partitionEithers (map genStat (_pPrepost p)) of
              (ls, rs) -> p & pStmts %~ (ls ++) . (++ rs)


toSSA :: Stmt NewId a -> Pred Op NewId -> SSAEval NewId ()
toSSA (SAssignStmt (AssignStmt _ v e)) p = tell [SSAAssign v (NE e)]
toSSA (SAssertStmt (AssertStmt _ e)) p =
  assmpts <- _ass <$> get
  tell [SSAAssert ((NEBinOp LAnd (NE p) assmpts) :=> NE mp)]
toSSA (SBlockStmt _ l) p = mapM_ (\x -> toSSA x p) l
toSSA (SIfStmt (IfStmt aInfo e tn (Just el))) p = do
  let
      thenCond = EBinOp LAnd (Pair p newPred)
      elseCond = EBinOp LAnd (Pair p (EUnOp LNot (UnOp newPred)))
      thenInstr = mapM_ (\x -> toSSA x thenCond) tn
      elseInstr = mapM_ (\x -> toSSA x elseCond) el
      g v = do
        newId <- fresh v
        m.ix v .= newId
        tell [SSAAssign newId (NE $ EShortIf newPred (EID $ lkup m' v) (EID $ lkup m'' v))]
      endInstrs = mapM_ g (S.elems (S.union (foldMap modset tn) (foldMap modset el)))
  in thenInstr ++ elseInstr ++ endInstrs
toSSA (SAssumeStmt (AssumeStmt _ e)) p =
  ass %= \x -> NEBinOp LAnd x (NE p) :=> NE e


lkup :: (Ord id, Show id) => M id id' -> id -> id'
lkup m varId = maybe
            (error $ "Uninitialized Variable encountered in lkup: " ++ show varId)
            id
            (M.lookup varId m)
fresh :: (Ord id, Show id) => id -> SSAEval id NewId
fresh v = do
  m.ix v.count %= (+1)
  mp <- _m <$> get
  return (lkup mp v)

apply e mp = fmap (lkup mp) e
-- should use bifoldable to accumulate all of the ids
modset :: Ord id => Stmt id a -> S.Set id
modset (SVarDecl (VarDecl _ _)) = S.empty
modset (SAssignStmt (AssignStmt _ id e)) = S.fromList [id]
modset (SAssertStmt (AssertStmt _ e)) = S.empty
modset (SAssumeStmt (AssumeStmt _ e)) = S.empty
modset (SHavocStmt (HavocStmt _ id)) = S.fromList [id]
modset (SIfStmt (IfStmt _ b th Nothing)) = foldMap modset th
modset (SIfStmt (IfStmt _ b th (Just el))) = foldMap modset th `S.union` foldMap modset el
modset (SBlockStmt _ stmts) = foldMap modset stmts
initialize id m = M.insert id (NewId 0 id) m
