{-# LANGUAGE LambdaCase, FlexibleContexts, FlexibleInstances #-}

module HSRTool.Parser.ExpressionParser(parseExpr) where

import Control.Applicative hiding (many, (<|>))
import Text.Parsec
import Control.Lens
import Control.Monad
import Control.Monad.State
import HSRTool.Parser.Utils
import HSRTool.Parser.Types
import Data.Bifunctor

type P u a = Parsec String u a

{-
To add new tokens, must modify:
tokenise
InToken
processToken
modify OutToken
modify show instance of OutToken

To add new mathematical operators, must modify:
data Op
mapOps
opInfo
leftAssoc
-}

tokeniseExpr :: Stream String Identity Char => P u [InToken String]
tokeniseExpr = g 0
    where
      g 0 = (try rparen *> fail "")
            <|> ((:) <$> try lparen <*> g 1)
            <|> ((:) <$> oPoss <*> g 0)
            <|> return []
      g n = ((:) <$> try rparen <*> g (n-1)) 
            <|> ((:) <$> try lparen <*> g (n+1))
            <|> ((:) <$> oPoss <*> g n)
      oPoss = foldl1 (<|>) (number:tres:tid:told:ops)
      number :: Stream String Identity Char => P u (InToken String)
      number = many space *> ((ITLit . read) <$> many1 digit) <* many space
      tres = many space *> (ITResult <$ try (string "\\result")) <* many space
      tid =  many space *> (ITID <$> ident) <* many space
      lparen = many space *> (ITLParen <$ char '(') <* many space
      rparen = many space *> (ITRParen <$ char ')') <* many space
      told = many space *> (ITOld <$> ((string "\\old") *> many space *> string "(" *>
             many space *> ident <* many space <* string ")")) <* many space
      oper c = many space *> ((ITOp . mapOps) <$> try (string c)) <* many space
      ops = map oper charOps

charOps =  [":", "?", "||", "&&", "~", "!", "==", "!=",  "<<", ">>", "<=", "<", ">=", ">", "+", "-", "*", "/", "%", "^", "|", "&"]
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

-- Need to add better error messages here
-- parseExpr :: P u (Expr String)
parseExpr = tokeniseExpr >>= toRPN >>= fmap (head . fst) . go []
    where
      go l [] = return (l, [])
      go l (OutVal i:r) = go (ELit i:l) r
      go l (OutVar id:r) = go (EID id:l) r
      go l (OutResult:r) = go (EResult:l) r
      go l (OutOld id:r) = go (EOld id:l) r
      go [e1] (OutOp op:r) = either
                             (\be -> fail "Bad Expr")
                             (\ue -> go (EUnOp (ue e1):[]) r)
                             (optype $ opInfo op) 
      go (e2:e1:es) (OutOp op:r)
          = either 
            (\be -> go (EBinOp (be e1 e2):es) r)
            (\ue -> go (EUnOp (ue e2):e1:es) r)
            (optype $ opInfo op)


data InToken id = ITID id | ITOp Op | ITLit Int | ITLParen | ITRParen | ITResult | ITOld id deriving (Show)
data OutToken id = OutOp Op | OutVal Int | OutVar id | OutResult | OutOld id
data StackElem = StOp Op | Paren deriving (Show)
data Op = Mul | Div | Add | Sub | Exp | Mod | LShift | RShift |
          BitXOr | BitAnd | BitOr | GrEq | Gr | Lt | LtEq | NEq | Eq | Not | BitNot |
          LAnd | LOr | LNot | SIfCond | SIfAlt
          deriving (Show)

data OpInfo id = OpInfo {
      precedence :: Int,
      symbol :: String,
      optype :: Either (Expr id -> Expr id -> BinOp id) (Expr id -> UnOp id)
}

data Assoc = L | R deriving (Eq)
 
type Env id = ([OutToken id], [StackElem])
type RPNComp id = StateT (Env id) (Either String) 
 
instance Show (OutToken String) where
    show (OutOp x) = symbol $ opInfo x
    show (OutVal v) = show v
    show (OutVar v) = v
    show OutResult = "\\result"
    show (OutOld id) = "old(" ++ id ++ ")"

opInfo :: Op -> OpInfo id
opInfo = \case
    LNot -> OpInfo 15 "!" (Right (::!))
    BitNot -> OpInfo 15 "~" (Right (::~))
    Mul -> OpInfo 14 "*" (Left (:*))
    Div -> OpInfo 14 "/" (Left (:/))
    Mod -> OpInfo 14 "%" (Left (:%))
    Add -> OpInfo 13 "+" (Left (:+))
    Sub -> OpInfo 13 "-" (Left (:-))
    LShift -> OpInfo 12 "<<" (Left (:<<))
    RShift -> OpInfo 12 ">>" (Left (:>>))
    Gr -> OpInfo 10 ">" (Left (:>))
    Lt -> OpInfo 10 "<" (Left (:<))
    GrEq -> OpInfo 10 ">=" (Left (:>=))
    LtEq -> OpInfo 10 "<=" (Left (:<=))
    NEq -> OpInfo 9 "!=" (Left (:!=))
    Eq -> OpInfo 9 "==" (Left (:==))
    BitAnd -> OpInfo 8 "&" (Left (:&))
    BitXOr -> OpInfo 7 "^" (Left (:^))
    BitOr -> OpInfo 6 "|" (Left (:|))
    LAnd -> OpInfo 5 "&&" (Left (:&&))
    LOr -> OpInfo 4 "||" (Left (:||))
    SIfCond -> OpInfo 3 "?" (Left (:?))
    SIfAlt -> OpInfo 3 ":" (Left (:?:))
    
    

prec = precedence . opInfo
leftAssoc LNot = False
leftAssoc BitNot = False
leftAssoc SIfCond = False
leftAssoc SIfAlt = False
leftAssoc _   = True
 
processToken :: InToken id -> RPNComp id ()
processToken = \case
    (ITLit z) -> pushVal z
    (ITOp op) -> pushOp op
    ITLParen    -> pushParen
    ITRParen    -> pushTillParen
    ITID id -> pushVar id
    ITResult -> pushResult
    ITOld x -> pushOld x

pushTillParen :: RPNComp id ()
pushTillParen = use _2 >>= \case 
    []     -> lift (Left "Unmatched right parenthesis")
    (s:st) -> case s of
         StOp o -> _1 %= (OutOp o:) >> _2 %= tail >> pushTillParen
         Paren  -> _2 %= tail
 
pushOp :: Op -> RPNComp id ()
pushOp o = use _2 >>= \case
    [] -> _2 .= [StOp o]
    (s:st) -> case s of 
        (StOp o2) -> if leftAssoc o && prec o == prec o2 
                     || prec o < prec o2 
                     then _1 %= (OutOp o2:) >> _2 %= tail >> pushOp o
                     else _2 %= (StOp o:) 
        Paren     -> _2 %= (StOp o:)
 
pushVal :: Int -> RPNComp id ()
pushVal n = _1 %= (OutVal n:)

pushResult :: RPNComp id ()
pushResult = _1 %= (OutResult:)

pushOld :: id -> RPNComp id ()
pushOld id = _1 %= (OutOld id:)

pushVar :: id -> RPNComp id ()
pushVar id = _1 %= (OutVar id:)
 
pushParen :: RPNComp id ()
pushParen = _2 %= (Paren:)
 
--Run StateT
toRPN :: [InToken id] -> P u [OutToken id]
toRPN xs = g (evalStateT process ([],[]))
    where g (Left x) = fail x
          g (Right x) = return x
          process = mapM_ processToken xs
                      >> get >>= \(a,b) -> (reverse a++) <$> (mapM toOut b)
          toOut :: StackElem -> RPNComp id (OutToken id)
          toOut (StOp o) = return $ OutOp o
          toOut Paren    = lift (Left "Unmatched left parenthesis")
