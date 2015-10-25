{-# LANGUAGE FlexibleContexts, TupleSections #-}

module HSRTool.Parser.Utils where

import Control.Monad.Trans
import Data.List (nub)
import Data.Functor.Identity
import Control.Applicative hiding (many, (<|>))
import Text.Parsec
--import Directory.WalkDir

-- rp :: Stream s Identity c => Parsec s () a -> s -> Either ParseError a
rp p s = parse p "" s

intK :: String
intK = "int"

assertK :: String
assertK = "assert"

assumeK :: String
assumeK = "assume"

havocK :: String
havocK = "havoc"

ifK :: String
ifK = "if"

elseK :: String
elseK = "else"

returnK :: String
returnK = "return"

requiresK ::String
requiresK = "requires"

ensuresK :: String
ensuresK = "ensures"

keywords :: [String]
keywords = [intK, assertK, assumeK, havocK, ifK, elseK, returnK, requiresK, ensuresK]

semicolonT = ";"
lparenT = "("
rparenT = ")"
ocurlyT = "{"
ccurlyT = "}"
equalsT = "="
commaT = ","
commentT = "//"
oMultiLineCT = "/*"
cMultiLineCT = "*/"
tokens = [semicolonT, lparenT, rparenT, ocurlyT, ccurlyT, equalsT, commaT, commentT, oMultiLineCT, cMultiLineCT]

type FileContent = String
type Dep = String
type Deps = [String]

getRes p a = either (const a) id . rp p

chs = "_"

valid_chars :: Stream String Identity Char => Parsec String u Char
valid_chars = oneOf chs <|> alphaNum

stripws = reverse . dropWhile (flip elem ws) . reverse
ws = [' ',  '\t']

nos = all digits
digits x = x >= '0' && x <= '9'

parseSeq [] = return []
parseSeq (x:xs) = ((:) <$> try x <*> parseSeq xs) <|> anyChar *> parseSeq (x:xs)

oneOfParsers l = foldl1 (<|>) l

toStringParser delimiter s = string s <* notFollowedBy delimiter

manyTill' x y = (try y *> pure []) <|> ((:) <$> x <*> manyTill' x y)

ident :: Stream String Identity Char => Parsec String u String
ident = (:) <$> chs <*> many (chs <|> digit)
    where
      chs = lower <|> upper <|> char '_'

bar x = try ((:) <$> (x <* many space) <*> (try (string commaT *> many space *> bar x) <|> ([] <$ many space)))
        <|> ([] <$ many space)
