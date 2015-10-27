{-# LANGUAGE TemplateHaskell #-}

module HSRTool.CodeGen.CGExpr where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Lens
import HSRTool.CodeGen.Types
import HSRTool.Parser.Types

data NewId = NewId {
      _count :: Int,
      _newVarId :: String
} deriving (Eq, Ord, Show, Read)

makeLenses ''NewId
