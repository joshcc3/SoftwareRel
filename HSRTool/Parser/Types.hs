{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module HSRTool.Parser.Types where

import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Traversable
import Data.Foldable
import Data.Bifoldable
import Data.Bitraversable
import Data.Bifunctor
import Control.Lens

type ASTInfo = ()

data Op = Mul | Div | Add | Sub | Exp | Mod | LShift | RShift |
          BitXOr | BitAnd | BitOr | GrEq | Gr | Lt | LtEq | NEq |
          Eq | Not | BitNot | LAnd | LOr | LNot | SIfCond | SIfAlt
          deriving (Show, Eq, Read, Ord)

data BinOp n = BinOp { _fst' :: n, _snd' :: n } deriving (Eq, Ord, Show, Read, Functor)
makeLenses ''BinOp
data UnOp n = UnOp { _only :: n } deriving (Eq, Ord, Show, Read, Functor)
makeLenses ''UnOp

data Expr op id = EShortIf (Expr op id) (Expr op id) (Expr op id) |
                  EBinOp op (BinOp (Expr op id)) |
                  EUnOp op (UnOp (Expr op id)) |
                  ELit Int |
                  EID id |
                  EResult |
                  EOld id deriving (Eq, Ord, Show, Read, Functor)
makePrisms ''Expr

data Program id a = Program {
      _pInfo :: a,
      _pVarDecls :: [VarDecl id a],
      _pPDecls :: [ProcedureDecl id a]
} deriving (Eq, Ord, Show, Read, Functor)

data VarDecl id a = VarDecl {
      _vInfo :: a,
      _varId :: id
} deriving (Eq, Ord, Show, Read, Functor)

data ProcedureDecl id a = PDecl {
      _pdeclInfo :: a,
      _pId :: id,
      _pFParams :: [FormalParam id a],
      _pPrepost :: [PrePost id a],
      _pStmts :: [Stmt id a],
      _pExpr :: Expr Op id
} deriving (Eq, Ord, Show, Read, Functor)

data FormalParam id a = FParam { _fpInfo :: a, _fID :: id }
                      deriving (Eq, Ord, Show, Read, Functor)
data PrePost id a = PPReq a (Expr Op id) | PPEns a (Expr Op id)
                  deriving (Eq, Ord, Show, Read, Functor)
data Stmt id a = SVarDecl a (VarDecl id a) |
            SAssignStmt a (AssignStmt id a) |
            SAssertStmt a (AssertStmt id a) |
            SAssumeStmt a (AssumeStmt id a) |
            SHavocStmt a (HavocStmt id a) |
            SIfStmt a (IfStmt id a) |
            SBlockStmt a [Stmt id a]
            deriving (Eq, Ord, Show, Read, Functor)

data AssignStmt id a = AssignStmt {
      _assInfo :: a,
      _assgnID :: id,
      _assgnExpr :: (Expr Op id)
} deriving (Eq, Ord, Show, Read, Functor)

data AssertStmt id a = AssertStmt {
      _assStmtInfo :: a,
      _assrtExpr :: (Expr Op id)
} deriving (Eq, Ord, Show, Read, Functor)

data AssumeStmt id a = AssumeStmt {
      _assmeInfo :: a,
      _assmeExpr :: (Expr Op id)
} deriving (Eq, Ord, Show, Read, Functor)

data HavocStmt id a = HavocStmt {
      _havocInfo :: a,
      _hID :: id
} deriving (Eq, Ord, Show, Read, Functor)

data IfStmt id a = IfStmt {
      _ifInfo :: a,
      _ifExpr :: Expr Op id,
      _ifThenB :: [Stmt id a],
      _ifElseB :: Maybe [Stmt id a]
} deriving (Eq, Ord, Show, Read, Functor)
makePrisms ''PrePost
makePrisms ''Stmt
makeLenses ''FormalParam
makeLenses ''ProcedureDecl
makeLenses ''AssignStmt
makeLenses ''AssumeStmt
makeLenses ''AssertStmt
makeLenses ''HavocStmt
makeLenses ''IfStmt
makeLenses ''VarDecl
makeLenses ''Program

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

instance Monad BinOp where
  return x = BinOp x x
  BinOp x x' >>= f = BinOp (_fst' (f x)) (_snd' (f x'))

instance Foldable BinOp where
  foldMap f (BinOp n n') = f n <> f n'

instance Traversable BinOp where
  traverse f (BinOp n n') = BinOp <$> f n <*> f n'

instance Applicative BinOp where
  pure = return
  (<*>) = ap

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
