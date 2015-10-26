{-# LANGUAGE TemplateHaskell #-}

module HSRTool.CodeGen.CGExpr where

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
data NewId = NewId {
      _count :: Int,
      _newVarId :: String
} deriving (Eq, Ord, Show, Read)
makeLenses ''NewId
data St id id' = St {
      _m :: M id id',
      _ass :: Assumption Op NewId
}
makeLenses ''St

runStack :: b -> StateT b (WriterT w m) a -> m ((a, b), w)
runStack s = runWriterT . flip runStateT s

runSSAGenerator :: Program String a -> IO (SSA Op NewId)
runSSAGenerator (Program _ vD pD) = do
  o <- runStack initSt pDecls
  return (snd o)
    where 
      initSt = St M.empty (NE (ELit 1))
      g x = do
        m %= initialize (_varId x)
      pDecls = do
        mapM_ g vD
        mapM_ (flip toSSA (ELit 1)) $ pD >>= _pStmts

type SSAEval id = StateT (St id NewId) (WriterT (SSA Op NewId) IO)

genStat (PPReq _ e) = Left (SAssumeStmt () (AssumeStmt () e))
genStat (PPEns _ e) = Right (SAssertStmt () (AssertStmt () e))
transProg = pPDecls.traverse %~ g
    where 
      g p = case partitionEithers (map genStat (_pPrepost p)) of
              (ls, rs) -> p & pStmts %~ (ls ++) . (++ rs)


toSSA :: Stmt String a -> Pred Op NewId -> SSAEval String ()
toSSA (SVarDecl _ (VarDecl _ id)) _ = do
  m %= initialize id
toSSA (SAssignStmt _ (AssignStmt _ v e)) p = do
  mp <- _m <$> get
  newId <- fresh v
  tell [SSAAssign newId (NE $ apply e mp)]
  m.ix v .= newId
toSSA (SAssertStmt _ (AssertStmt _ e)) p = do
  st <- get
  let mp = _m st
      assmpts = _ass st
  tell [SSAAssert ((NEBinOp LAnd (NE p) assmpts) :=> NE (apply e mp))]
toSSA (SBlockStmt _ l) p = mapM_ (flip toSSA p) l
toSSA (SIfStmt _ (IfStmt _ e tn (Just el))) p = do
  st      <- get
  let mp  = _m st
      newPred = apply e mp
  mapM_ (flip toSSA (EBinOp LAnd (BinOp p newPred))) tn
  st'     <- get
  let m'  = _m st'
  m .= mp
  mapM_ (flip toSSA (EBinOp LAnd  (BinOp p (EUnOp LNot (UnOp newPred))))) el
  st''    <- get
  let g v = do
        newId <- fresh v
        m.ix v .= newId
        tell [SSAAssign newId (NE $ EShortIf newPred (EID $ (lkup m' v)) (EID $ lkup m'' v))]
      m'' = _m st''
  mapM_ g (S.elems (S.union (foldMap modset tn) (foldMap modset el)))
toSSA (SIfStmt _ (IfStmt _ e tn Nothing)) p = do
  st <- get
  let mp = _m st
      newPred = apply e mp
  mapM_ (flip toSSA (EBinOp LAnd (BinOp p newPred))) tn
  st' <- get
  let
      g v = do
        newId <- fresh v
        m.ix v .= newId
        tell [SSAAssign newId (NE . EID $ lkup m' v)]
      m' = _m st'
  mapM_ g (S.elems (foldMap modset tn))
toSSA (SHavocStmt _ (HavocStmt _ v)) _ = do
  newId <- fresh v
  m.ix v .= newId
toSSA (SAssumeStmt _ (AssumeStmt _ e)) p = do
  mp <- _m <$> get
  ass %= \x -> NEBinOp LAnd x (NE p) :=> NE (apply e mp)


lkup :: Ord id => M id id' -> id -> id'
lkup = fmap (maybe (error "Uninitialized Variable encountered in lkup") id) . flip M.lookup
fresh :: Ord id => id -> SSAEval id NewId
fresh v = do
  m.ix v.count %= (+1)
  mp <- _m <$> get
  return (lkup mp v)

apply e mp = fmap (lkup mp) e
-- should use bifoldable to accumulate all of the ids
modset :: Ord id => Stmt id a -> S.Set id
modset (SVarDecl _ (VarDecl _ _)) = S.empty
modset (SAssignStmt _ (AssignStmt _ id e)) = S.fromList [id]
modset (SAssertStmt _ (AssertStmt _ e)) = S.empty
modset (SAssumeStmt _ (AssumeStmt _ e)) = S.empty
modset (SHavocStmt _ (HavocStmt _ id)) = S.fromList [id]
modset (SIfStmt _ (IfStmt _ b th Nothing)) = foldMap modset th
modset (SIfStmt _ (IfStmt _ b th (Just el))) = foldMap modset th `S.union` foldMap modset el
modset (SBlockStmt _ stmts) = foldMap modset stmts
initialize id m = M.insert id (NewId 0 id) m
{-

-- loeb :: Functor f => f (f a -> b) -> f b
-- loeb f = fmap ($loeb f) f
enter a new scope:



int x1;
x1 = 1;
{
x1 = 3;
int x2;
x2 = 10;
}
assert x1 == 3;
  
-}
