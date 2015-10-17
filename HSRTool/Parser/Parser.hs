{-# LANGUAGE FlexibleContexts #-}
module HSRTool.Parser.Parser where

import Data.List
import Control.Applicative hiding ((<|>), many)
import HSRTool.Parser.Utils
import Text.Parsec
import HSRTool.Parser.Types
import Data.Functor.Identity

-- type Strm = Stream String Identity Char
type P u a = Parsec String u a


{-
1 + 2*3
1*2 + 3
(1+2)*3
1 - 2*3
1 - (2 + 4)
1 - 2 + 3
1+2*3/5-3+5
(1+(2*(3/5))-3)+5
+,*,/,-,+
1+2-3/5*3+5
(1+2)>((3^5)/3)*5

1+2*3^2/2+3
1+(2*(3^2))+3

(1+(2*(3/5))-3)+5
(1+2)-((3/5)*3)+5


-}
data ExpTok id = TLit Int | TOp String | TID id | TResult | TOld id deriving (Eq, Ord, Show, Read)

-- tokenise :: Stream String Identity Char => P u (ExpTok String)
tokenise = foldl1 (<|>) (number:ops ++ [tid, tres, told])
    where
      number = (TLit . read) <$> (many1 digit <* many space)
      tid = TID <$> ident
      ident = many1 valid_chars
      tres :: Stream String Identity Char => P u (ExpTok String)
      tres = TResult <$ string "\\result"
      told = TOld <$> ident
      oper c = TOp <$> (string c <* many space)
      ops = map oper ["||", "&&", "|", "^", "&", "==", "!=", "<", "<=", ">", ">=", "<<", ">>", "+",
                      "-", "*", "/", "%", "~", "!", "(", ")"]

infixToExp :: ExpTok String -> Expr String
infixToExp = undefined -- toExpr . reverse . toPostfix . map flipParens . reverse
    where 
      flipParens (TOp "(") = TOp ")"
      flipParens (TOp ")") = TOp "("
      flipParens x = x
      toPostfix = undefined -- unfoldr f . (++ ["("]) . push ")"
      f (TOp ")":os, st, b) = Just (os, TOp ")":st)
      f (TOp "(":os, st, b) = undefined -- case popWhile "(" st of
                              -- (pre, st') -> (os, st', reverse pre++b)
      f (TOp o:os, st, b) = case popWhile o st of
                              _ -> undefined -- (pre, st') -> (os, st', TOp o:reverse pre++b)
      popWhile o (x, []) = (x, [])
      popWhile o (x, o':os) | leq o o' = popWhile o (o':x, os)
                            | otherwise = (x, o':os)
      push = undefined
      leq = undefined
      toExpr = undefined


type St = ([ExpTok String])

simple2P = f <$> (numberP <* op '*') <*> numberP <*> (op '+' *> numberP)
    where 
      f x y z = EBinOp (EBinOp (Lit x :* Lit y) :+ Lit z)


simpleP = f <$> (numberP <* op '+') <*> numberP <*> (op '*' *> numberP)
    where 
      f x y z = EBinOp (Lit x :+ EBinOp (Lit y :* Lit z))


numberP :: Stream String Identity Char => P u Int
numberP = read <$> (many1 digit <* many space)

op c = char c <* many space

oneOpP :: Stream String Identity Char => Char -> P u (Expr String)
oneOpP c = f <$> numberP <*> rest
    where 
      rest = Just <$> (op c *> oneOpP c) <|> return Nothing
      f x r = maybe (Lit x) (\v -> EBinOp (Lit x :+ v)) r

