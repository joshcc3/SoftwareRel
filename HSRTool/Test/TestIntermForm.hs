{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module HSRTool.Test.TestIntermForm where

import HSRTool.CodeGen.IntermStmt
import HSRTool.Parser.Types
import Control.Monad.State
import Data.Distributive
import Data.Foldable
import Control.Comonad
import Data.Monoid
import Control.Applicative
import Control.Lens
import HSRTool.Test.Utils
import qualified Data.Map as M
import Data.Bitraversable

ex0 = SVarDecl (VarDecl () "x")
ex1 = SBlockStmt (Either' (Left ()), Either' (Right ()))
      [SVarDecl (VarDecl () "x")]
ex2 = SBlockStmt (Either' (Left ()), Either' (Right ()))
      [SVarDecl (VarDecl () "x"), 
       SVarDecl (VarDecl () "y"),
       SAssignStmt (AssignStmt () "x" (ELit 3)),
       SAssignStmt (AssignStmt () "y" (ELit 3))]
ex3 = SBlockStmt (Either' (Left ()), Either' (Right ()))
      [SVarDecl (VarDecl () "x"), ex2, ex1]

runIntermStmtGen p = do
  res <- runParserTest p
  either (error . show) f res
      where 
        f = return . g . traverse (bitraverse subst id . stmt) . _pStmts . head . _pPDecls
        g s = runState s (St' M.empty)
