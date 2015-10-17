{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module HSRTool.Parser.ExpressionParser where

import Control.Applicative hiding (many, (<|>))
import Text.Parsec
import Control.Lens
import Control.Monad
import Control.Monad.State
import HSRTool.Parser.Utils
import Data.Bifunctor

data ExpTok id = TLit Int | TOp String | TID id | TLParen | TRParen | TResult | TOld id deriving (Eq, Ord, Show, Read)

type P u a = Parsec String u a

tokenise = many1 $ foldl1 (<|>) (number:ops ++ [tid, tres, told])
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

tokenise' :: Stream String Identity Char => P u [InToken String]
tokenise' = many1 $ foldl1 (<|>) (number:lparen:rparen:ops) --  ++ [tid, tres, told])
    where
      number :: Stream String Identity Char => P u (InToken String)
      number = (ITLit . read) <$> (many1 digit <* many space)
      -- tid = TID <$> ident
      lparen = ITLParen <$ char '('
      rparen = ITRParen <$ char ')'
      ident = many1 valid_chars
      --tres :: Stream String Identity Char => P u InToken
      --tres = TResult <$ string "\\result"
      --told = TOld <$> ident
      oper c = (ITOp . mapOps) <$> (try (string c) <* many space)
      ops = map oper charOps

charOps =  [":", "?", "||", "&&", "~", "!", "==", "!=", "<", "<=", ">", ">=", "<<", ">>", "+", "-", "*", "/", "%", "^", "|", "&"]
mapOps "*" = Mul
mapOps "/" = Div
mapOps "+" = Add
mapOps "-" = Sub
mapOps "%" = Mod
mapOps "<<" = LShift
mapOps ">>" = RShift
mapOps "&" = BitAnd
mapOps "|"  = BitOr
mapOps "^" = BitXOr
mapOps ">=" = GrEq
mapOps ">" = Gr
mapOps "<" = Lt
mapOps "<=" = LtEq
mapOps "!=" = NEq
mapOps "==" = Eq
mapOps "!" = LNot
mapOps "~" = BitNot
mapOps "&&" = LAnd
mapOps "||" = LOr
mapOps "?" = SIfCond
mapOps ":" = SIfAlt

chain s = first show (rp tokenise' s) >>= toRPN

data InToken id = ITOp Op | ITLit Int | ITLParen | ITRParen deriving (Show)
data OutToken = OutOp Op | OutVal Int
data StackElem = StOp Op | Paren deriving (Show)
data Op = Mul | Div | Add | Sub | Exp | Mod | LShift | RShift |
          BitXOr | BitAnd | BitOr | GrEq | Gr | Lt | LtEq | NEq | Eq | Not | BitNot |
          LAnd | LOr | LNot | SIfCond | SIfAlt
          deriving (Show)
data Assoc = L | R deriving (Eq)
 
type Env = ([OutToken], [StackElem])
type RPNComp = StateT Env (Either String) 
 
instance Show OutToken where
    show (OutOp x) = snd $ opInfo x
    show (OutVal v) = show v
 
opInfo = \case
    LNot -> (15, "!")
    BitNot -> (15, "~")
    Mul -> (14, "*")
    Div -> (14, "/")
    Mod -> (14, "%")
    Add -> (13, "+")
    Sub -> (13, "-")
    LShift -> (12, "<<")
    RShift -> (12, ">>")
    Gr -> (10, ">")
    Lt -> (10, "<")
    GrEq -> (10, ">=")
    LtEq -> (10, "<=")
    NEq -> (9, "!=")
    Eq -> (9, "==")
    BitAnd -> (8, "&")
    BitXOr -> (7, "^")
    BitOr -> (6, "|")
    LAnd -> (5, "&&")
    LOr -> (4, "||")
    SIfCond -> (3, "?")
    SIfAlt -> (3, ":")
    
    

prec = fst . opInfo
leftAssoc LNot = False
leftAssoc BitNot = False
leftAssoc SIfCond = False
leftAssoc SIfAlt = False
leftAssoc _   = True
 
processToken :: InToken String -> RPNComp ()
processToken = \case
    (ITLit z) -> pushVal z
    (ITOp op) -> pushOp op
    ITLParen    -> pushParen
    ITRParen    -> pushTillParen
 
pushTillParen :: RPNComp ()
pushTillParen = use _2 >>= \case 
    []     -> lift (Left "Unmatched right parenthesis")
    (s:st) -> case s of
         StOp o -> _1 %= (OutOp o:) >> _2 %= tail >> pushTillParen
         Paren  -> _2 %= tail
 
pushOp :: Op -> RPNComp ()
pushOp o = use _2 >>= \case
    [] -> _2 .= [StOp o]
    (s:st) -> case s of 
        (StOp o2) -> if leftAssoc o && prec o == prec o2 
                     || prec o < prec o2 
                     then _1 %= (OutOp o2:) >> _2 %= tail >> pushOp o
                     else _2 %= (StOp o:) 
        Paren     -> _2 %= (StOp o:)
 
pushVal :: Int -> RPNComp ()
pushVal n = _1 %= (OutVal n:)
 
pushParen :: RPNComp ()
pushParen = _2 %= (Paren:)
 
--Run StateT
toRPN :: [InToken String] -> Either String [OutToken]
toRPN xs = evalStateT process ([],[])
    where process = mapM_ processToken xs
                      >> get >>= \(a,b) -> (reverse a++) <$> (mapM toOut b)
          toOut :: StackElem -> RPNComp OutToken
          toOut (StOp o) = return $ OutOp o
          toOut Paren    = lift (Left "Unmatched left parenthesis")
