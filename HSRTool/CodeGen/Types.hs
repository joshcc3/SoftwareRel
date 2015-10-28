{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module HSRTool.CodeGen.Types where

import qualified Data.Map as M
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
