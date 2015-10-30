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

stmtToSSA :: Stmt NewId (Pred Op NewId, Assumption Op NewId) -> SSA Op NewId
stmtToSSA (SAssignStmt (AssignStmt _ v e)) = [SSAAssign v (NE e)]
stmtToSSA (SBlockStmt _ l) = l >>= stmtToSSA
stmtToSSA (SIfStmt (IfStmt _ e tn el)) =
  let
      tnInstr = tn >>= stmtToSSA
      elInstr = maybe [] id el >>= stmtToSSA
  in tnInstr ++ elInstr
stmtToSSA s@(SAssertStmt (AssertStmt _ e))
    = let (p, a) = extract s
      in [SSAAssert ((NEBinOp LAnd (NE p) a) :=> NE e)]
stmtToSSA _ = []

-- (extract <$> a^?_Outer._2)^?_Just._Left._Left
{-
accumPred (SIfStmt' a)
    = either' (either' enterIf atThen) (either' atElse exitIf) (extract a)
      where 
        enterIf _ = let v = extract a
                    in openScope >> id %= updatePred v >> listToMaybe <$> get
        atThen _ = get
        atElse _ = let v = extract a
                   in do
                     closeScope
                     openScope 
                     id %= updatePred (EUnOp LNot (UnOp v))
                     listToMaybe <$> get
        exitIf = (\_ -> closeScope)
        updatePred mv p' = maybe p' (:p') $ do
                             p <- listToMaybe p'
                             newPred <- mv
                             return (EBinOp LAnd (Pair p newPred))              
accumPred (SAssumeStmt (AssumeStmt _ e))
    = listToMaybe <$> get
--  id %= undefined -- \x -> NEBinOp LAnd x undefined {-(NE p)-} :=> NE undefined -- p
accumPred _ = listToMaybe <$> get
-}

accumAssump :: Stmt String a -> 
              State [(Pred Op String, Assumption Op String)] 
                    (Maybe (Pred Op String, Assumption Op String))
accumAssump (SIfStmt' a)
  = either' (either' enterIf atThen) (either' atElse exitIf) (extract a)
      where 
        enterIf _ = listToMaybe <$> get
        atThen _ = do
          let bExpr = (extract <$> a^?_Outer._2)^?_Just._Left._Right._1
              ifBExpr = maybe (error "Couldn't find bool expr in if") id bExpr
          openScope
          ix 0._1 %= \p ->  EBinOp LAnd (Pair ifBExpr p)
          listToMaybe <$> get
        atElse _ = do
          closeScope
          let bExpr' = (extract <$> a^?_Outer._2)^?_Just._Right._Just._1
              bExpr = maybe (error "Couldn't find bool expr in else") id bExpr'
              elseBExpr = EUnOp LNot (UnOp bExpr)
          openScope
          ix 0._1 %= \p -> EBinOp LAnd (Pair p elseBExpr)
          listToMaybe <$> get
        exitIf _ = closeScope
accumAssump (SAssumeStmt (AssumeStmt _ e))
    = do
  ix 0 %= \(p, a) -> (p, NEBinOp LAnd a (NE p) :=> NE e)
  listToMaybe <$> get
accumAssump _ = listToMaybe <$> get

openScope :: State [a] (Maybe a)
openScope = do
  id %= g
  listToMaybe <$> get
    where 
      g [] = []
      g (x:xs) = x:x:xs
closeScope :: State [a] (Maybe a)
closeScope = do
  id %= g
  listToMaybe <$> get
      where 
        g [] = []
        g (x:xs) = xs

