{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase, FlexibleContexts, FlexibleInstances   #-}

module HSRTool.Parser.StatementParser where

import HSRTool.Parser.Utils
import HSRTool.Parser.ExpressionParser
import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import HSRTool.Parser.Types
import Data.Functor.Identity

statementParser =
    S . SVarDecl <$> try varDecl <|>
    S . SAssignStmt <$> try assignStmt <|>
    S . SAssertStmt <$> try assertStmt <|>
    S . SAssumeStmt <$> try assumeStmt <|>
    S . SHavocStmt <$> try havocStmt <|>
    S <$> try ifStmt'' <|>
    --SIfStmt <$> try ifStmt <|>
    --SIfStmt' <$> try ifStmt' <|>
    S . SBlockStmt (Either' (Left ()), Either' (Right ())) <$> blockStmt
-- varDecl :: ParsecT String u Identity (VarDecl String ASTInfo)

varDecl = VarDecl () <$>
          (string intK *> many space *>
           ident
           <* string semicolonT)

assignStmt = do
  i <- ident
  many space
  string equalsT
  many space
  e <- parseExpr
  many space
  string semicolonT
  many space
  return (AssignStmt () i e)
assertStmt = do
  string assertK
  many space
  e <- parseExpr
  many space
  string semicolonT
  many space
  return (AssertStmt () e)
assumeStmt = do
  string assumeK
  many space
  e <- parseExpr
  many space
  string semicolonT
  many space
  return (AssumeStmt () e)
havocStmt = do
  string havocK
  many space
  i <- ident
  many space
  string semicolonT
  many space
  return (HavocStmt () i)
-- ifStmt :: Stream String Identity Char => Parsec String u String
ifStmt = do
  string ifK
  many space
  e <- parseExpr
  many space
  b <- blockStmt
  many space
  b' <- try (do
        string elseK
        many space
        b' <- blockStmt
        many space
        return (Just b)
      ) <|> return Nothing
  return (IfStmt () e b b')
      where
        toIfSt e b b' = IfStmt
ifStmt' = do
  string ifK
  many space
  e <- parseExpr
  many space
  b <- blockStmt
  many space
  b' <- try (do
        string elseK
        many space
        b' <- blockStmt
        many space
        return (Just b)
      ) <|> return Nothing
  return (toIfSt e b b')
      where
        toIfSt e b b'
            = (Outer (pos 0 ())
               (Outer (Left (Left e))
                (Outer (pos 1 ())
                 (Centre (Left (Right (e, b))))
                (pos 2 ()))
               (Right ((e,) <$> b')))
              (pos 3 ()))
ifStmt'' = do
  string ifK
  many space
  e <- parseExpr
  many space
  b <- blockStmt
  many space
  b' <- try (do
        string elseK
        many space
        b' <- blockStmt
        many space
        return (Just b)
      ) <|> return Nothing
  return (toIfSt e b b')
      where
        toIfSt e b b' 
            = SIfStmt (pos 0 ((), (), (), ())) e b b'
blockStmt = do
  string ocurlyT
  many space
  s <- many (many space *> statementParser <* many space)
  many space
  string ccurlyT
  return s
