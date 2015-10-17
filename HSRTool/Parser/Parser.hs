{-# LANGUAGE FlexibleContexts #-}
module HSRTool.Parser.Parser(parse) where


import HSRTool.Parser.Utils
import HSRTool.Parser.ProgramParser
import Control.Applicative hiding ((<|>), many)
import HSRTool.Parser.Types
import Data.Functor.Identity
import Text.Parsec hiding (parse)

singleLineComment :: Stream String Identity Char => Parsec String u String
singleLineComment = "\n" <$ (string commentT 
                           <* manyTill anyChar newline)

multiLineComment :: Stream String Identity Char => Parsec String u String
multiLineComment = "" <$ (string oMultiLineCT <* 
                   manyTill anyChar (try (string cMultiLineCT)))

stripComments = try((++) <$> (try singleLineComment <|> multiLineComment) <*> 
                     stripComments)
                <|> (:) <$> anyChar <*> stripComments <|> ([] <$ eof)

parse s = rp stripComments s >>= rp programParser
