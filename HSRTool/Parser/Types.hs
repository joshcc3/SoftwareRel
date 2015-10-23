{-# LANGUAGE
  DeriveFunctor,
  DeriveFoldable,
  DeriveTraversable,
  TemplateHaskell #-}

module HSRTool.Parser.Types where

import Control.Comonad
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

data Op = Mul | Div | Add | Sub | Mod | LShift | RShift |
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
} deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data VarDecl id a = VarDecl {
      _vInfo :: a,
      _varId :: id
} deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Bifoldable VarDecl where
    bifoldMap f g b = fold (bimap f g b)

instance Bifunctor VarDecl where
    bimap f g (VarDecl a id) = VarDecl (g a) (f id)

instance Bitraversable VarDecl where
    bitraverse f g (VarDecl a id) = VarDecl <$> g a <*> f id

data ProcedureDecl id a = PDecl {
      _pdeclInfo :: a,
      _pId :: id,
      _pFParams :: [FormalParam id a],
      _pPrepost :: [PrePost id a],
      _pStmts :: [Stmt id a],
      _pExpr :: Expr Op id
} deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Bifoldable ProcedureDecl where
    bifoldMap f g b = fold (bimap f g b)

instance Bifunctor ProcedureDecl where
    bimap f g (PDecl a id fp pp sts e)
        = PDecl (g a) (f id) (map (bimap f g) fp) (map (bimap f g) pp)
           (map (bimap f g) sts) (fmap f e)

instance Bitraversable ProcedureDecl where
    bitraverse f g (PDecl a id fp pp sts e)
        = PDecl <$> (g a) <*> (f id) <*> (traverse (bitraverse f g) fp) <*> (traverse (bitraverse f g) pp) <*>
           (traverse (bitraverse f g) sts) <*> (traverse f e)

data FormalParam id a = FParam { _fpInfo :: a, _fID :: id }
                      deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
instance Bifoldable FormalParam where
    bifoldMap f g (FParam a id) = f id <> g a

instance Bifunctor FormalParam where
    bimap f g (FParam a id) = FParam (g a) (f id)

instance Bitraversable FormalParam where
    bitraverse f g (FParam a id) = FParam <$> g a <*> f id

data PrePost id a = PPReq a (Expr Op id) | PPEns a (Expr Op id)
                  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Bifoldable PrePost where
    bifoldMap f g p = fold (bimap f g p)

instance Bifunctor PrePost where
    bimap f g (PPEns a e) = PPEns (g a) (bimap id f e)

instance Bitraversable PrePost where
    bitraverse f g (PPReq a e)
        = PPReq <$> g a <*> bitraverse pure f e


data Stmt id a = SVarDecl { _svdInfo :: a, _svdVDecl :: (VarDecl id a) } |
            SAssignStmt { _sasInfo :: a, _sasAStmt :: (AssignStmt id a) } |
            SAssertStmt { _sassInfo :: a, _sassAStmt :: (AssertStmt id a) } |
            SAssumeStmt { _sassumeInfo :: a, _sassumeStmt :: (AssumeStmt id a) } |
            SHavocStmt { _shInfo :: a, _shHavocStmt :: (HavocStmt id a) } |
            SIfStmt { _sifInfo :: a, _sifIfStmt :: (IfStmt id a) } |
            SBlockStmt { _sIBInfo :: a, _sbBlockStmt :: [Stmt id a] }
            deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
{-
instance Comonad (Stmt id) where
    extract (SVarDecl a _) = a
    extract (SAssignStmt a _) = a
    extract (SAssertStmt a _) = a
    extract (SAssumeStmt a _) = a
    extract (SHavocStmt a _) = a
    extract (SIfStmt a _) = a
    extract (SBlockStmt a _) = a

    duplicate s@(SVarDecl _ v) = SVarDecl s (duplicate v)
    duplicate s@(SAssignStmt a v) = SAssignStmt s (duplicate v)
    duplicate s@(SAssertStmt a v) = SAssertStmt s (duplicate v)
    duplicate s@(SAssumeStmt a v) = SAssumeStmt s (duplicate v)
    duplicate s@(SHavocStmt a v) = SHavocStmt s (duplicate v)
    duplicate s@(SIfStmt a v) = SIfStmt s (duplicate v)
    duplicate s@(SBlockStmt a v) = SBlockStmt s (map duplicate v)
-}

instance Bifunctor Stmt where
    bimap f g (SVarDecl a d) = SVarDecl (g a) (bimap f g d)
    bimap f g (SAssignStmt a a') = SAssignStmt (g a) (bimap f g a')
    bimap f g (SAssertStmt a as) = SAssertStmt (g a) (bimap f g as)
    bimap f g (SAssumeStmt a s) = SAssumeStmt (g a) (bimap f g s)
    bimap f g (SHavocStmt a h) = SHavocStmt (g a) (bimap f g h)
    bimap f g (SIfStmt a i) = SIfStmt (g a) (bimap f g i)
    bimap f g (SBlockStmt a s) = SBlockStmt (g a) (map (bimap f g) s)

instance Bifoldable Stmt where
    bifoldMap f g = fold . bimap f g

instance Bitraversable Stmt where
    bitraverse f g (SVarDecl a d) = SVarDecl <$> (g a) <*> (bitraverse f g d)
    bitraverse f g (SAssignStmt a a') = SAssignStmt <$> (g a) <*> (bitraverse f g a')
    bitraverse f g (SAssertStmt a as) = SAssertStmt <$> (g a) <*> (bitraverse f g as)
    bitraverse f g (SAssumeStmt a s) = SAssumeStmt <$> (g a) <*> (bitraverse f g s)
    bitraverse f g (SHavocStmt a h) = SHavocStmt <$> (g a) <*> (bitraverse f g h)
    bitraverse f g (SIfStmt a i) = SIfStmt <$> (g a) <*> (bitraverse f g i)
    bitraverse f g (SBlockStmt a s) = SBlockStmt <$> (g a) <*> (traverse (bitraverse f g) s)

data AssignStmt id a = AssignStmt {
      _assInfo :: a,
      _assgnID :: id,
      _assgnExpr :: (Expr Op id)
} deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Comonad (AssignStmt id) where
    extract (AssignStmt a _ _) = a
    duplicate a@(AssignStmt _ i e) = AssignStmt a i e

instance Bifunctor AssignStmt where
    bimap f g (AssignStmt a b e)
          = AssignStmt (g a) (f b) (fmap f e)

instance Bifoldable AssignStmt where
    bifoldMap f g = fold . bimap f g

instance Bitraversable AssignStmt where
    bitraverse f g (AssignStmt a b e) = AssignStmt <$> g a <*> f b <*> traverse f e

data AssertStmt id a = AssertStmt {
      _assStmtInfo :: a,
      _assrtExpr :: (Expr Op id)
} deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Comonad (AssertStmt id) where
    extract (AssertStmt a _) = a
    duplicate a@(AssertStmt _ e) = AssertStmt a e

instance Bifunctor AssertStmt where
    bimap f g (AssertStmt a e)
          = AssertStmt (g a) (fmap f e)

instance Bifoldable AssertStmt where
    bifoldMap f g = fold . bimap f g

instance Bitraversable AssertStmt where
    bitraverse f g (AssertStmt a e)
        = AssertStmt <$> g a <*> traverse f e

data AssumeStmt id a = AssumeStmt {
      _assmeInfo :: a,
      _assmeExpr :: (Expr Op id)
} deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Comonad (AssumeStmt id) where
    extract (AssumeStmt a _) = a
    duplicate a@(AssumeStmt _ e) = AssumeStmt a e


instance Bifunctor AssumeStmt where
    bimap f g (AssumeStmt a e)
          = AssumeStmt (g a) (fmap f e)

instance Bifoldable AssumeStmt where
    bifoldMap f g = fold . bimap f g

instance Bitraversable AssumeStmt where
    bitraverse f g (AssumeStmt a e)
        = AssumeStmt <$> g a <*> traverse f e

data HavocStmt id a = HavocStmt {
      _havocInfo :: a,
      _hID :: id
} deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Comonad (HavocStmt id) where
    extract (HavocStmt a _) = a
    duplicate a@(HavocStmt _ i) = HavocStmt a i

instance Bifunctor HavocStmt where
    bimap f g (HavocStmt a e)
          = HavocStmt (g a) (f e)

instance Bifoldable HavocStmt where
    bifoldMap f g = fold . bimap f g

instance Bitraversable HavocStmt where
    bitraverse f g (HavocStmt a e)
        = HavocStmt <$> g a <*> f e

data IfStmt id a = IfStmt {
      _ifInfo :: a,
      _ifExpr :: Expr Op id,
      _ifThenB :: [Stmt id a],
      _ifElseB :: Maybe [Stmt id a]
} deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Bifunctor IfStmt where
    bimap f g (IfStmt a e t els)
          = IfStmt (g a) (fmap f e) (fmap (bimap f g) t) ((fmap.fmap) (bimap f g) els)

instance Bifoldable IfStmt where
    bifoldMap f g = fold . bimap f g

instance Bitraversable IfStmt where
    bitraverse f g (IfStmt a e t els)
        = IfStmt <$> g a <*> traverse f e <*> (traverse (bitraverse f g) t)
          <*> ((traverse.traverse) (bitraverse f g) els)

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

instance Applicative (Expr op) where
    pure = return
    (<*>) = ap


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

instance Applicative UnOp where
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
