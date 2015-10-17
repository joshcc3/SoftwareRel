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

--programParser :: ParsecT String u Identity (Program String)
programParser = 
    Program <$> 
            many (try (many space *> varDecl <* many space)) <*> 
            many (many space *> pDecl <* many space)

declIdent = string intK *> many space *> 
            ident 
            <* many space
fParams = string lparenT *> many space *> 
          bar formalParams 
                  <* many space <* string rparenT <* many space
ppConds = bar prepost <* many space
stmts = string ocurlyT *> many space *> 
        many (many space *> statementParser <* many space) 
        <* many space <* string returnK <* many space
rExpr = many space *> 
        parseExpr 
        <* many space <* string semicolonT <* many space <* string ccurlyT <* many space
pDecl = PDecl <$> 
              declIdent <*> 
              fParams <*> 
              ppConds <*> 
              stmts <*> 
              rExpr

formalParams = FParam <$> (string intK *> many space *> ident <* many space)
prepost = PPReq <$> (string requiresK *> many space *> parseExpr <* many space) <|>
          PPEns <$> (string ensuresK *> many space *> parseExpr <* many space)
