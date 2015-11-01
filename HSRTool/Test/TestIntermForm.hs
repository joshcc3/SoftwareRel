{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module HSRTool.Test.TestIntermForm where

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
ex1 = S $ SBlockStmt (Either' (Left ()), Either' (Right ()))
      [S $ SVarDecl (VarDecl () "x")]
ex2 = S $ SBlockStmt (Either' (Left ()), Either' (Right ()))
      [S $ SVarDecl (VarDecl () "x"), 
       S $ SVarDecl (VarDecl () "y"),
       S $ SAssignStmt (AssignStmt (Either' (Left ()), Either' (Right ())) "x" (ELit 3)),
       S $ SAssignStmt (AssignStmt (Either' (Left ()), Either' (Right ())) "y" (ELit 3))]
ex3 = S $ SBlockStmt (Either' (Left ()), Either' (Right ()))
      [S $ SVarDecl (VarDecl () "x"), ex2, ex1]

ex4' = PDecl (Either' (Left ()), Either' (Right ())) "a" [] [] [] (ELit 3)
ex4'' = PDecl (Either' (Left ()), Either' (Right ())) "a" [] [] [S $ SVarDecl (VarDecl () "y")] (ELit 3)
ex4 = Program () [VarDecl () "x"] 
      [ex4']

ex5 = SIfStmt' a
    where 
      a = Centre (Either' (Left (Either' (Left (ELit 3)))))

