{-# LANGUAGE FlexibleContexts #-}
module HSRTool.CodeGen.Utils where

import Control.Monad.State
import Data.Foldable
import Data.Monoid
import qualified Data.Set as S
import HSRTool.Parser.Types
import Control.Comonad

modset p = fold (p =>> f)
    where 
      f (S(SAssignStmt (AssignStmt _ id e))) = S.fromList [id]
      f (S(SHavocStmt (HavocStmt _ id))) = S.fromList [id]
      f _ = mempty
