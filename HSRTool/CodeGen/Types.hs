{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module HSRTool.CodeGen.Types where

import HSRTool.Parser.Types
import Control.Monad
import Control.Monad.Free
import Control.Lens

data SSAAlt id e = SSAAssign id e | SSAAssert e
                   deriving (Eq, Ord, Read, Show, Functor)

type SSA op id = [SSAAlt id (NewExpr op id)]

data NewExpr op id = NEBinOp op (NewExpr op id) (NewExpr op id)
                   | NE (Expr op id)
                   | NewExpr op id :=> NewExpr op id
                   deriving (Eq, Ord, Read, Show, Functor)

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
