{-# LANGUAGE LambdaCase, FlexibleContexts, FlexibleInstances #-}

module HSRTool.Parser.ProgramParser where

import HSRTool.Parser.Utils
import HSRTool.Parser.ExpressionParser
import HSRTool.Parser.StatementParser
import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import HSRTool.Parser.Types
import Data.Functor.Identity
import Text.Parsec

programParser :: ParsecT String u Identity (Program String)
programParser = 
    Program <$> 
            many (many space *> varDecl <* many space) <*> 
            many (many space *> pDecl <* many space)
    where
      pDecl = PDecl <$> 
              declIdent <*> 
              fParams <*> 
              ppConds <*> 
              stmts <*> 
              rExpr
      declIdent = string intK *> many space *> ident <* many space
      fParams = string lparenT *> many space *> bar formalParams <* many space <* string rparenT <* many space
      ppConds = bar prepost <* many space
      stmts = string ocurlyT *> many space *> 
               many statementParser <* many space <*
               string returnK <* many space
      rExpr = parseExpr <* many space <* string semicolonT <* many space <* string ccurlyT <* many space

formalParams = FParam <$> (string intK *> many space *> ident)
prepost = PPReq <$> (string requiresK *> parseExpr) <|>
          PPEns <$> (string ensuresK *> parseExpr)
