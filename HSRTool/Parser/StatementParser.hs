{-# LANGUAGE LambdaCase, FlexibleContexts, FlexibleInstances   #-}

module HSRTool.Parser.StatementParser where

import HSRTool.Parser.Utils
import HSRTool.Parser.ExpressionParser
import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import HSRTool.Parser.Types
import Data.Functor.Identity

statementParser = 
    SVarDecl <$> try varDecl <|> 
    SAssignStmt <$> try assignStmt <|> 
    SAssertStmt <$> try assertStmt <|> 
    SAssumeStmt <$> try assumeStmt <|> 
    SHavocStmt <$> try havocStmt <|> 
    SIfStmt <$> try ifStmt <|> 
    blockStmt

--varDecl :: ParsecT String u Identity (VarDecl id)
varDecl = VarDecl <$> 
          (string intK *> 
           many space *> 
           ident <* string semicolonT)

assignStmt = do
  i <- ident
  many space
  string equalsT
  many space
  e <- parseExpr
  many space
  string semicolonT
  many space
  return (AssignStmt i e)
assertStmt = do
  string assertK
  many space
  e <- parseExpr
  many space
  string semicolonT
  many space
  return (AssertStmt e)
assumeStmt = do
  string assumeK
  many space
  e <- parseExpr
  many space
  string semicolonT
  many space
  return (AssumeStmt e)
havocStmt = do
  string havocK
  many space
  i <- ident
  many space
  string semicolonT
  many space
  return (HavocStmt i)
ifStmt = do
  string ifK
  many space
  string lparenT
  many space
  e <- parseExpr
  many space
  string rparenT
  many space
  b <- blockStmt
  many space
  string lparenT 
  many space
  string elseK
  many space
  b' <- blockStmt
  return (IfStmt e b b')
blockStmt = do
  string ocurlyT
  many space
  s <- statementParser
  many space
  string ccurlyT
  return s
