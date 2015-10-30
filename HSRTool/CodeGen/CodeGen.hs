{-# LANGUAGE TemplateHaskell #-}

module HSRTool.CodeGen.CodeGen where

-- Attempt 2 at codegeneration, cleaner implementation

import Control.Monad.Cont
import Control.Comonad
import Data.Either (partitionEithers)
import Data.Maybe
import Data.Monoid
import Control.Applicative
import qualified Data.Set as S
import Control.Lens
import HSRTool.CodeGen.Types
import HSRTool.CodeGen.IntermStmt
import HSRTool.Parser.Types
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Data.Foldable (foldMap)
type Assumption = NewExpr
type Pred = Expr
type M id id' = M.Map id id'


genStat (PPReq _ e) = Left (SAssumeStmt (AssumeStmt () e))
genStat (PPEns _ e) = Right (SAssertStmt (AssertStmt () e))
transProg = pPDecls.traverse %~ g
    where 
      g p = case partitionEithers (map genStat (_pPrepost p)) of
              (ls, rs) -> p & pStmts %~ (ls ++) . (++ rs)

toSSA :: Stmt NewId a -> Pred Op NewId -> SSA Op NewId
toSSA (SAssignStmt (AssignStmt _ v e)) p  = [SSAAssign v (NE e)]
toSSA (SBlockStmt _ l) p = l >>= \x -> toSSA x p
toSSA (SIfStmt (IfStmt _ e tn (Just el))) p =
  let
      tnInstr = tn >>= \x -> toSSA x (EBinOp LAnd (Pair p e))
      elInstr = el >>= \x -> toSSA x (EBinOp LAnd  (Pair p (EUnOp LNot (UnOp e))))
  in tnInstr ++ elInstr
  -- tell [SSAAssign newId (NE $ EShortIf newPred (EID $ lkup m' v) (EID $ lkup m'' v))]
toSSA (SIfStmt (IfStmt _ e tn Nothing)) p =
  tn >>= \x -> toSSA x (EBinOp LAnd (Pair p e))
toSSA (SAssumeStmt (AssumeStmt _ e)) p = []
--  ass %= \x -> NEBinOp LAnd x (NE p) :=> NE (apply e mp)
toSSA (SAssertStmt (AssertStmt _ e)) p 
    = [SSAAssert ((NE p) :=> NE e)]
{-    =  [SSAAssert ((NEBinOp LAnd (NE p) assmpts) :=> NE e)]
       where 
         assmpts = undefined-}
toSSA _ _ = []

