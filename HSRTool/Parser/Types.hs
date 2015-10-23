{-# LANGUAGE DeriveFunctor #-}

module HSRTool.Parser.Types where

import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Traversable
import Data.Foldable
import Data.Bifoldable
import Data.Bitraversable
import Data.Bifunctor

data Program id = Program {
      pVarDecls :: [VarDecl id],
      pPDecls :: [ProcedureDecl id]
} deriving (Eq, Ord, Show, Read, Functor)

data VarDecl id = VarDecl {
      varId :: id
} deriving (Eq, Ord, Show, Read, Functor)

data ProcedureDecl id = PDecl {
      pId :: id,
      pFParams :: [FormalParam id],
      pPrepost :: [PrePost id],
      pStmts :: [Stmt id],
      pExpr :: Expr Op id
} deriving (Eq, Ord, Show, Read, Functor)

data FormalParam id = FParam { fID :: id }
                      deriving (Eq, Ord, Show, Read, Functor)
data PrePost id = PPReq (Expr Op id) | PPEns (Expr Op id)
                  deriving (Eq, Ord, Show, Read, Functor)
data Stmt id = SVarDecl (VarDecl id) |
            SAssignStmt (AssignStmt id) |
            SAssertStmt (AssertStmt id) |
            SAssumeStmt (AssumeStmt id) |
            SHavocStmt (HavocStmt id) |
            SIfStmt (IfStmt id) |
            SBlockStmt [Stmt id] deriving (Eq, Ord, Show, Read, Functor)

data AssignStmt id = AssignStmt {
      assgnID :: id,
      assgnExpr :: (Expr Op id)
} deriving (Eq, Ord, Show, Read, Functor)

data AssertStmt id = AssertStmt {
      assrtExpr :: (Expr Op id)
} deriving (Eq, Ord, Show, Read, Functor)

data AssumeStmt id = AssumeStmt {
      assmeExpr :: (Expr Op id)
} deriving (Eq, Ord, Show, Read, Functor)

data HavocStmt id = HavocStmt {
      hID :: id
} deriving (Eq, Ord, Show, Read, Functor)

data IfStmt id = IfStmt {
      ifExpr :: Expr Op id,
      ifThenB :: [Stmt id],
      ifElseB :: Maybe [Stmt id]
} deriving (Eq, Ord, Show, Read, Functor)

data Expr op id = EShortIf (Expr op id) (Expr op id) (Expr op id) |
                  EBinOp op (BinOp (Expr op id)) |
                  EUnOp op (UnOp (Expr op id)) |
                  ELit Int |
                  EID id |
                  EResult |
                  EOld id deriving (Eq, Ord, Show, Read, Functor)

instance Foldable (Expr op) where
    foldMap = bifoldMap (const mempty)

instance Bifoldable Expr where
  bifoldMap f g (EShortIf b e e') = bifoldMap f g b <> bifoldMap f g e <> bifoldMap f g e'
  bifoldMap f g (EBinOp op b) = f op <> foldMap (bifoldMap f g) b
  bifoldMap f g (EUnOp op b) = f op <> foldMap (bifoldMap f g) b
  bifoldMap _ _ (ELit n) = mempty
  bifoldMap f g (EID id) = g id
  bifoldMap _ _ EResult = mempty
  bifoldMap f g (EOld id) = g id

instance Traversable (Expr op) where
  traverse = bitraverse pure

instance Bifunctor Expr where
  bimap f g (EShortIf b e e')
      = EShortIf (bimap f g b) (bimap f g e) (bimap f g e')
  bimap f g (EBinOp op b) = EBinOp (f op) (bimap f g <$> b)
  bimap f g (EUnOp op b) =  EUnOp (f op) (bimap f g <$> b)
  bimap f g (ELit x) = ELit x
  bimap f g (EID id) = EID (g id)
  bimap f g EResult = EResult
  bimap f g (EOld id) = EOld (g id)

instance Bitraversable Expr where
  bitraverse f g (EShortIf b e e') = EShortIf <$> bitraverse f g b <*> bitraverse f g e <*> bitraverse f g e'
  bitraverse f g (EBinOp op b) = EBinOp <$> f op <*> traverse (bitraverse f g) b
  bitraverse f g (EUnOp op b) = EUnOp <$> f op <*> traverse (bitraverse f g) b
  bitraverse _ _ (ELit n) = pure (ELit n)
  bitraverse _ g (EID id) = EID <$> g id
  bitraverse _ _ EResult = pure EResult
  bitraverse _ g (EOld id) = EOld <$> (g id)

instance Monad (Expr op) where
   return = EID
   EShortIf b e e' >>= f = EShortIf (b >>= f) (e >>= f) (e' >>= f)
   EBinOp op b >>= f = EBinOp op (fmap (>>= f) b)
   EUnOp op b >>= f = EUnOp op (fmap (>>= f) b)
   ELit x >>= f = ELit x
   EID id >>= f = f id
   EResult >>= _ = EResult
   EOld id >>= f = f id

data BinOp n = BinOp { fst' :: n, snd' :: n } deriving (Eq, Ord, Show, Read, Functor)

instance Monad BinOp where
  return x = BinOp x x
  BinOp x x' >>= f = BinOp (fst' (f x)) (snd' (f x'))

instance Foldable BinOp where
  foldMap f (BinOp n n') = f n <> f n'

instance Traversable BinOp where
  traverse f (BinOp n n') = BinOp <$> f n <*> f n'

instance Applicative BinOp where
  pure = return
  (<*>) = ap

data UnOp n = UnOp n deriving (Eq, Ord, Show, Read, Functor)

instance Foldable UnOp where
  foldMap f (UnOp n) = f n

instance Traversable UnOp where
  traverse f (UnOp n) = fmap UnOp (f n)

instance Monad UnOp where
  return = UnOp
  UnOp n >>= f = f n

data OpInfo = OpInfo {
      precedence :: Int,
      symbol :: String,
      opType :: Either () ()
} deriving (Eq, Ord, Show, Read)

data Op = Mul | Div | Add | Sub | Exp | Mod | LShift | RShift |
          BitXOr | BitAnd | BitOr | GrEq | Gr | Lt | LtEq | NEq |
          Eq | Not | BitNot | LAnd | LOr | LNot | SIfCond | SIfAlt
          deriving (Show, Eq, Read, Ord)
