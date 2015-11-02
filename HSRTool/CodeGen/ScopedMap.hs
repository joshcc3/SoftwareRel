module HSRTool.CodeGen.ScopedMap(ScopedMap(..), newScope, closeScope, newDef, updateDef, lookup) where

{- |
     This module provides an implementation of a ScopedMap.
     A scopedMap keeps track of the mapping from variable ids to
     their SSA ids in the context of nested scopes.
     A new scope allows the variables to be redeclared.
     A scoped map can be though of as a stack of maps which are
     pushed/popped on entering/leaving a scope.
   | -}
import qualified Data.Map as M
import Control.Lens
import Data.Maybe
import Prelude hiding (lookup)

type ScopeNum = Int

type ScopedMap a b = M.Map a [(b,ScopeNum)]

lookup :: Ord a => a -> ScopedMap a b -> Maybe b
lookup a m = fmap fst . listToMaybe $ m^.ix a

newScope :: Ord a => ScopedMap a b -> ScopedMap a b
newScope = traverse.ix 0._2 %~ (+1)

closeScope :: Ord a => ScopedMap a b -> ScopedMap a b
closeScope = traverse %~ f
    where
      f [] = []
      f ((_,0):r) = r
      f ((v,n):r) = (v,n-1):r

newDef :: Ord a => ScopedMap a b -> a -> b -> ScopedMap a b
newDef m a b = M.alter f a m
    where
      f Nothing = Just [(b,0)]
      f (Just []) = Just [(b,0)]
      f (Just ((v,n):r)) = Just ((b,0):(v,n-1):r)


updateDef :: Ord a => ScopedMap a b -> a -> b -> ScopedMap a b
updateDef s a b = s&ix a.ix 0._1 .~ b

{-
int x;
x = 2;

if(..){

 x = 23;
 int x = 300;
 int y;

 y = 10;

}

x = 11
-}

simpleprog :: ScopedMap String Int
simpleprog = M.empty &
             newScope
               & \x -> newDef x "x" 2
                 & newScope
                    & \x -> updateDef x "x" 23 & \x -> newDef x "x" 0
                    & \x -> updateDef x "x" 300 & \x -> newDef x "y" 0
                    & \x -> updateDef x "y" 10
                 & closeScope
                 & \x -> updateDef x "x" 11
             & closeScope
