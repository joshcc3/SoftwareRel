{-# LANGUAGE FlexibleContexts #-}
module HSRTool.CodeGen.Utils where

import Control.Applicative
import Data.Traversable
import Data.Bitraversable
import Control.Monad.State
import Data.Foldable
import Data.Monoid
import qualified Data.Set as S
import HSRTool.CodeGen.Types
import HSRTool.Parser.Types
import Control.Comonad
import Control.Lens

modset p = fold (p =>> f)
    where
      f (S(SAssignStmt (AssignStmt _ id e))) = S.fromList [id]
      f (S(SHavocStmt (HavocStmt _ id))) = S.fromList [id]
      f _ = mempty


bitraverseStmt f g (S (SVarDecl d)) = S . SVarDecl <$> (bitraverse f g d)
bitraverseStmt f g (S (SAssignStmt a')) = S . SAssignStmt <$>  (bitraverse f g a')
bitraverseStmt f g (S (SAssertStmt as)) = S . SAssertStmt <$>  (bitraverse f g as)
bitraverseStmt f g (S (SAssumeStmt s)) = S . SAssumeStmt <$> (bitraverse f g s)
bitraverseStmt f g (S (SHavocStmt h)) = S . SHavocStmt <$> (bitraverse f g h)
bitraverseStmt f g (S (SIfStmt'' i)) = S . SIfStmt'' <$> (bitraverse f g i)
bitraverseStmt f g (S (SBlockStmt (l, r) s))
    = h <$> traverse g l
      <*> traverse (bitraverseStmt f g) s
      <*> traverse g r
    where
      h x y z = S (SBlockStmt (x, z) y)
bitraverseStmt f g (S (SIfStmt aInfo e th el))
    = do
      a <- g enterIf
      b <- traverse f e
      oldScopedMap <- _mp <$> get
      c <- traverse (bitraverseStmt f g) th
      d <- g afterThen
      mp .= oldScopedMap
      e <- (traverse.traverse) (bitraverseStmt f g) el
      f <- g afterElse
      h <- g exitIf
      return (S (SIfStmt ((fmap.fmap) (const (a,d,f,h)) aInfo) b c e))
    where
      (enterIf, afterThen, afterElse, exitIf)
          = either' (either' id (\(b,a,c,d) -> (a,b,c,d)))
            (either' (\(c,a,b,d) -> (a,b,c,d))
                     (\(d,a,b,c) -> (a,b,c,d))) aInfo




bitraversePDecl f g (PD (PDecl (l, r) vId fp pp sts e))
    = PD <$>
      (h <$> traverse g l
       <*> f vId
       <*> traverse (bitraverse f g) fp
       <*> traverse (bitraverse f g) pp
       <*> traverse (bitraverseStmt f g) sts
       <*> traverse f e
       <*> traverse g r)
    where
      h l a b c d e r = PDecl (l, r) a b c d e
