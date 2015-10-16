{-# LANGUAGE FlexibleContexts, TupleSections #-}

module HSRTool.Parser.CoreParser(rp, h, getFileContents, getFilePaths, getRes, stripws, ws,
       			 valid_chars, chs, FileContent, Dep, Deps, parseLine,
			 nos, digits, parseSeq, oneOfParsers, toStringParser,
			 manyTill') where

import Control.Monad
import Data.List (nub)
import Data.Functor.Identity
import Control.Applicative hiding (many, (<|>))
import Text.Parsec
--import Directory.WalkDir

rp :: Stream s Identity c => Parsec s () a -> s -> Either ParseError a
rp p s = parse p "" s

process :: String -> Either ParseError String
process = fmap (unlines . nub . map stripws) . mapM f . lines
 where
  f :: Stream String Identity Char => String -> Either ParseError String
  f = rp (many anyChar)

parseLine :: Stream s Identity Char => Parsec s u String
parseLine = concat <$> h (string ":") ((:[]) <$> anyChar)

h x y = (try x *> pure [])  <|> ((:) <$> y <*> h x y)

type FileContent = String
type Dep = String
type Deps = [String]

getFileContents :: FilePath -> IO [(FilePath, FileContent)]
getFileContents s = do
  fps <- getFilePaths s
  mapM ((\f -> (f,) <$> readFile f) . g) fps
  where
   g = ("../kido/" ++)

-- This is a bit of a weird function that uses a file as directory in a weird format 
getFilePaths :: FilePath -> IO [String]
getFilePaths = fmap (lines . either (const []) id . process) . readFile

getRes p a = either (const a) id . rp p

chs = "_.$" 

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