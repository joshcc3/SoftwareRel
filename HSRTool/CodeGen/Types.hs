{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module HSRTool.CodeGen.Types where

import qualified Data.Map as M
import HSRTool.Parser.Types
import Control.Monad
import Control.Monad.Free
import Control.Lens

data SSAAlt id e = SSAAssign id e | SSAAssert e
                   deriving (Eq, Ord, Read, Show, Functor)

instance Bifunctor SSAAlt where
    bimap f g (SSAAssign id e) = SSAAssign (f id) (g e)
    bimap f g (SSAAssert e) = SSAAssert (g e)

type SSA op id = [SSAAlt id (NewExpr op id)]

data NewExpr op id = NEBinOp op (NewExpr op id) (NewExpr op id)
                   | NE (Expr op id)
                   | NewExpr op id :=> NewExpr op id
                   deriving (Eq, Ord, Read, Show, Functor)

instance Bifunctor NewExpr where
    bimap f g (NEBinOp op e e') = NEBinOp (f op) (bimap f g e) (bimap f g e')
    bimap f g (NE e) = NE (bimap f g e)
    bimap f g (e :=> e') = bimap f g e :=> bimap f g e'

type Ident = String
type IdNum = Int

data NewId = NewId {
      _count :: IdNum,
      _newVarId :: Ident
} deriving (Eq, Ord, Show, Read)

makeLenses ''NewId

type ExprAST = Expr Op Ident

type ExprSSA = Expr Op NewId

type NewExprSSA = NewExpr Op NewId

-- this is a type for predicates and assumptions
type PropSSAs = [ExprSSA]

type StmtSSAs = SSA Op NewId
