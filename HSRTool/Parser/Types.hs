{-# LANGUAGE
  DeriveFunctor,
  DeriveFoldable,
  DeriveTraversable,
  GeneralizedNewtypeDeriving,
  TypeSynonymInstances,
  TemplateHaskell #-}

module HSRTool.Parser.Types where

import Data.Distributive
import Control.Monad.State
import Control.Comonad
import HSRTool.Parser.Utils
import HSRTool.Utils
import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Traversable
import Data.Foldable
import Data.Bifoldable
import Data.Bitraversable
import Data.Bifunctor
import Control.Lens

newtype Either' a = Either' { _unE :: Either a a }
    deriving(Eq, Ord, Read, Show)
makeLenses ''Either'

either' f g (Either' (Left x)) = f x
either' f g (Either' (Right x)) = g x

instance Applicative Either' where
    pure = return
    (<*>) = ap

instance Monad Either' where
    return = Either' . Right
    Either' (Left x) >>= f = f x
    Either' (Right x) >>= f = f x

instance Functor Either' where
    fmap f (Either' (Left x)) = Either' (Left (f x))
    fmap f (Either' (Right x)) = Either' (Right (f x))

instance Foldable Either' where
    foldMap f = either' f f

instance Traversable Either' where
    traverse f (Either' (Left x)) = (Either' . Left) <$> f x
    traverse f (Either' (Right x)) = (Either' . Right) <$> f x

instance Comonad Either' where
    extract = either' id id
    duplicate s = s <$ s


data ZList a = ZList { leftZ :: [a], focus :: a, rightZ :: [a] }
               deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)


shiftL :: ZList a -> Maybe (ZList a)
shiftL (ZList [] _ _) = Nothing
shiftL (ZList (a':as) a r) = Just (ZList as a' (a:r))

shiftR :: ZList a -> Maybe (ZList a)
shiftR (ZList _ _ []) = Nothing
shiftR (ZList l a (a':r)) = Just (ZList (a:l) a' r)

iterateWhile :: (a -> Maybe a) -> a -> [a]
iterateWhile f a = a:maybe [] (iterateWhile f) (f a)

instance Comonad ZList where
    extract = focus
    duplicate z = ZList l' z r'
        where
          l' = tail (iterateWhile shiftL z)
          r' = tail (iterateWhile shiftR z)

type ASTInfo = ()

data Op = Mul | Div | Add | Sub | Mod | LShift | RShift |
          BitXOr | BitAnd | BitOr | GrEq | Gr | Lt | LtEq | NEq |
          Eq | Not | BitNot | LAnd | LOr | LNot | SIfCond | SIfAlt

          deriving (Show, Eq, Read, Ord)

data Pair n = Pair { _fst' :: n, _snd' :: n } deriving (Eq, Ord, Show, Read, Functor)
makeLenses ''Pair
data UnOp n = UnOp { _only :: n } deriving (Eq, Ord, Show, Read, Functor)
makeLenses ''UnOp

data Expr op id = EShortIf (Expr op id) (Expr op id) (Expr op id) |
                  EBinOp op (Pair (Expr op id)) |
                  EUnOp op (UnOp (Expr op id)) |
                  ELit Int |
                  EID id |
                  EResult |
                  EOld id deriving (Eq, Ord, Show, Read, Functor)
makePrisms ''Expr
makeLenses ''Expr

data Program a' id a = Program {
      _pInfo :: a,
      _pVarDecls :: [VarDecl id a'],
      _pPDecls :: [ProcedureDecl id a']
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

type ProcedureDecl id a = ProcedureDecl' a id a
data ProcedureDecl' a' id a = PDecl {
      _pdeclInfo :: (Either' a, Either' a),
      _pId :: id,
      _pFParams :: [FormalParam id a'],
      _pPrepost :: [PrePost id a'],
      _pStmts :: [Stmt id a'],
      _pExpr :: Expr Op id
} deriving (Eq, Ord, Show, Read, Functor)

newtype ProcedureDeclT id a = PD { getPDecl :: ProcedureDecl' a id a }
    deriving (Eq, Ord, Read, Show)

instance Functor (ProcedureDeclT id) where
    fmap = bimap id

instance Bifunctor ProcedureDeclT where
    bimap f g (PD (PDecl (l,r) i fp pp sts e))
        = PD (PDecl (fmap g l, fmap g r)
                    (f i)
                    (fmap (bimap f g) fp)
                    (fmap (bimap f g) pp)
                    (fmap (bimap f g) sts)
                    (fmap f e)
             )

instance Foldable (ProcedureDeclT id) where
    foldMap = bifoldMap (const mempty)

instance Bifoldable ProcedureDeclT where
    bifoldMap f g (PD (PDecl (l, r) id fp pp sts e)) =
        foldMap g l
        <> f id
        <> foldMap (bifoldMap f g) fp
        <> foldMap (bifoldMap f g) pp
        <> foldMap (bifoldMap f g) sts
        <> foldMap f e
        <> foldMap g r

instance Bitraversable ProcedureDeclT where
    bitraverse f g (PD (PDecl (l, r) vId fp pp sts e))
        = PD <$>
          (h <$> traverse g l
           <*> f vId
           <*> traverse (bitraverse f g) fp
           <*> traverse (bitraverse f g) pp
           <*> traverse (bitraverse f g) sts
           <*> traverse f e
           <*> traverse g r)
              where
                h l a b c d e r = PDecl (l, r) a b c d e

instance Traversable (ProcedureDeclT id) where
    traverse = bitraverse pure

instance Comonad (ProcedureDecl' a' id) where
    extract = either' id id . fst . _pdeclInfo
    duplicate s
        = s { _pdeclInfo = a }
          where
            (l, r) = _pdeclInfo s
            a = (s <$ l, s' <$ r)
            s' = s { _pdeclInfo = (r, l) }

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
    bimap f g (PPEns a e) = PPEns (g a) (fmap f e)
    bimap f g (PPReq a e) = PPReq (g a) (fmap f e)

instance Bitraversable PrePost where
    bitraverse f g (PPReq a e)
        = PPReq <$> g a <*> bitraverse pure f e
    bitraverse f g (PPEns a e)
        = PPEns <$> g a <*> bitraverse pure f e


data Stmt id a = SVarDecl { _svdVDecl :: (VarDecl id a) } |
            SAssignStmt { _sasAStmt :: (AssignStmt id a) } |
            SAssertStmt { _sassAStmt :: (AssertStmt id a) } |
            SAssumeStmt { _sassumeStmt :: (AssumeStmt id a) } |
            SHavocStmt { _shHavocStmt :: (HavocStmt id a) } |
            SIfStmt { _sifIfStmt :: (IfStmt id a) } |
            SBlockStmt { _sbAInfo :: (Either' a, Either' a),
                       _sbBlockStmt :: [Stmt id a] } |
            SIfStmt' { _sifIfStmt' :: AltList (IfInfo id a) (Either' (Either' a)) }
            deriving (Eq, Ord, Show, Read)

instance Functor (Stmt id) where
    fmap = bimap id

instance Foldable (Stmt id) where
    foldMap = bifoldMap (const mempty)

instance Traversable (Stmt id) where
    traverse = bitraverse pure

instance Bifunctor Stmt where
    bimap f g (SVarDecl d) = SVarDecl (bimap f g d)
    bimap f g (SAssignStmt a') = SAssignStmt (bimap f g a')
    bimap f g (SAssertStmt as) = SAssertStmt (bimap f g as)
    bimap f g (SAssumeStmt s) = SAssumeStmt (bimap f g s)
    bimap f g (SHavocStmt h) = SHavocStmt  (bimap f g h)
    bimap f g (SIfStmt i) = SIfStmt  (bimap f g i)
    bimap f g (SBlockStmt (l, r) s)
        = SBlockStmt (fmap g l, fmap g r) (map (bimap f g) s)
    bimap f g (SIfStmt' a) = SIfStmt' (bimap h h' a)
        where
          h' = (fmap . fmap) g
          h =  bimap (bimap (fmap f) (bimap (fmap f) (fmap (bimap f g))))
                      (fmap (bimap (fmap f) (fmap (bimap f g))))

instance Bifoldable Stmt where
    bifoldMap f g = fold . bimap f g

instance Bitraversable Stmt where
    bitraverse f g (SVarDecl d) = SVarDecl <$> (bitraverse f g d)
    bitraverse f g (SAssignStmt a') = SAssignStmt <$>  (bitraverse f g a')
    bitraverse f g (SAssertStmt as) = SAssertStmt <$>  (bitraverse f g as)
    bitraverse f g (SAssumeStmt s) = SAssumeStmt <$> (bitraverse f g s)
    bitraverse f g (SHavocStmt h) = SHavocStmt <$> (bitraverse f g h)
    bitraverse f g (SIfStmt i) = SIfStmt <$> (bitraverse f g i)
    bitraverse f g (SBlockStmt (l, r) s)
        = h <$>
          traverse g l
          <*> traverse (bitraverse f g) s
          <*> traverse g r
              where
                h x y z = SBlockStmt (x, z) y
    bitraverse f g (SIfStmt' a) = SIfStmt' <$> bitraverse h h' a
        where
          h =  bitraverse
                 (bitraverse
                    (traverse f)
                    (bitraverse (traverse f) (traverse (bitraverse f g))))
                 (traverse (bitraverse (traverse f) (traverse (bitraverse f g))))
          h' = (traverse.traverse) g

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
    bimap f g (IfStmt a e t els )
          = IfStmt (g a) (fmap f e) (fmap (bimap f g) t) ((fmap.fmap) (bimap f g) els)

instance Bifoldable IfStmt where
    bifoldMap f g = fold . bimap f g

instance Bitraversable IfStmt where
    bitraverse f g (IfStmt a e t els)
        = IfStmt <$> g a <*> traverse f e <*> (traverse (bitraverse f g) t)
          <*> ((traverse.traverse) (bitraverse f g) els)

data AltList t a = Centre a |
                   Outer a (AltList a t) a
                   deriving (Eq, Ord, Show, Read)

type IfInfo id a
    = Either (Either (Expr Op id) (Expr Op id, [Stmt id a])) (Maybe (Expr Op id, [Stmt id a]))

makePrisms ''PrePost
makeLenses ''PrePost
makePrisms ''Stmt
makeLenses ''Stmt
makeLenses ''FormalParam
makeLenses ''ProcedureDecl'
makeLenses ''AssignStmt
makeLenses ''AssumeStmt
makeLenses ''AssertStmt
makeLenses ''HavocStmt
makeLenses ''IfStmt
makeLenses ''VarDecl
makeLenses ''Program
makePrisms ''AltList

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

instance Comonad Pair where
    extract (Pair x _) = x
    duplicate (Pair x x') = Pair (Pair x x') (Pair x' x)

instance Monad Pair where
  return x = Pair x x
  Pair x x' >>= f = Pair (_fst' (f x)) (_snd' (f x'))

instance Foldable Pair where
  foldMap f (Pair n n') = f n <> f n'

instance Traversable Pair where
  traverse f (Pair n n') = Pair <$> f n <*> f n'

instance Applicative Pair where
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

instance Comonad (Stmt id) where
    extract (SVarDecl v) = _vInfo v
    extract (SAssignStmt a) = _assInfo a
    extract (SAssertStmt a) = _assStmtInfo a
    extract (SAssumeStmt a) = _assmeInfo a
    extract (SHavocStmt h) = _havocInfo h
    extract (SIfStmt i) = _ifInfo i
    extract (SBlockStmt e _) = either' id id . fst $ e
    extract (SIfStmt' a) = extract . extract . extract $ a

    duplicate s@(SVarDecl (VarDecl _ id)) = SVarDecl (VarDecl s id)
    duplicate s@(SAssignStmt (AssignStmt _ e v)) = SAssignStmt (AssignStmt s e v)
    duplicate s@(SAssertStmt (AssertStmt _ v)) = SAssertStmt (AssertStmt s v)
    duplicate s@(SAssumeStmt (AssumeStmt _ v)) = SAssumeStmt (AssumeStmt s v)
    duplicate s@(SHavocStmt (HavocStmt _ v)) = SHavocStmt (HavocStmt s v)
    duplicate s@(SIfStmt ifS@(IfStmt _ e st els ))
        = SIfStmt (IfStmt s e (map duplicate st) ((fmap.map) duplicate els))
    duplicate s@(SBlockStmt (l, r) st)
        = SBlockStmt a' (fmap duplicate st)
          where
            a' = (s <$ l, s' <$ r)
            s' = s & sbAInfo %~ swap
    duplicate (SIfStmt' a) = undefined {-SIfStmt' (first f a')
        where
          f  = bimap (second (fmap duplicate)) ((fmap.fmap) duplicate)
          a' = zipWithAltList (flip const) h' a (duplicate a)
          h' b d = (fmap . fmap) (const (SIfStmt' d)) b
          -}
instance Functor (AltList t) where
    fmap = bimap id

instance Bifunctor AltList where
    bimap f g (Centre a) = Centre (g a)
    bimap f g (Outer a c a') = Outer (g a) (bimap g f c) (g a')

instance Foldable (AltList t) where
    foldMap f = bifold . bimap (const mempty) f

instance Traversable (AltList t) where
    traverse = bitraverse pure

instance Bifoldable AltList where
    bifoldMap f g (Centre a) = g a
    bifoldMap f g (Outer a x a') = g a <> bifoldMap g f x <> g a'

instance Bitraversable AltList where
    bitraverse f g (Centre a) = Centre <$> g a
    bitraverse f g (Outer a x a') = Outer <$> g a <*> bitraverse g f x <*> g a'

instance Comonad (AltList t) where
    extract (Centre x) = x
    extract (Outer a _ _) = a

    duplicate c@(Centre a) = Centre c
    duplicate c@(Outer a x a') = Outer c x' (rev c)
        where
          x' = x&_Outer._2 %~ duplicate
          rev :: AltList a b -> AltList a b
          rev (Centre x) = Centre x
          rev (Outer a x a') = Outer a' (rev x) a

zipWithAltList :: (a -> c -> e) -> (b -> d -> f) ->
                 AltList a b -> AltList c d -> AltList e f
zipWithAltList _ f (Centre x) (Centre y) = Centre (f x y)
zipWithAltList _ f (Centre x) (Outer a _ b) = Centre (f x a)
zipWithAltList _ f (Outer a _ _) (Centre x) = Centre (f a x)
zipWithAltList f g (Outer a m b) (Outer c m' d)
    = Outer (g a c) (zipWithAltList g f m m') (g b d)


pos :: Int -> a -> Either' (Either' a)
pos = (l !!)
    where
      l' = [Either' . Left, Either' . Right]
      l = (.) <$> l' <*> l'
