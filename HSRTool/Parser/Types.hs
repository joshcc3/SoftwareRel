{-# LANGUAGE FlexibleContexts #-}
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

data Program' a' id a = Program {
      _pInfo :: a,
      _pVarDecls :: [VarDecl id a'],
      _pPDecls :: [ProcedureDecl id a']
} deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

newtype Program id a = P { _unP :: Program' a id a }
    deriving (Eq, Ord, Show, Read)

instance Bifunctor Program where
    bimap f g (P (Program a vs pds))
        = P $ Program (g a) (map (bimap f g) vs)
                            (map (getPDecl . bimap f g . PD) pds)

instance Functor (Program id) where
    fmap f = bimap id f

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

instance Bifunctor (ProcedureDecl' a') where
    bimap f g (PDecl i id fp pp pst pexpr)
          = PDecl (bimap (fmap g) (fmap g) i) (f id) ((map.first) f fp)
            ((map.first) f pp) ((map.first) f pst)
            (fmap f pexpr)

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

newtype Stmt'' a' id a = S' { _unSt' :: Stmt' (Stmt'' a') a' id a }
    deriving (Eq, Ord, Show, Read)
newtype Stmt id a = S { _unSt :: Stmt' Stmt a id a }
   deriving(Eq, Ord, Show, Read)

data Stmt' n a' id a = SVarDecl { _svdVDecl :: (VarDecl id a') } |
            SAssignStmt { _sasAStmt :: (AssignStmt id a') } |
            SAssertStmt { _sassAStmt :: (AssertStmt id a') } |
            SAssumeStmt { _sassumeStmt :: (AssumeStmt id a') } |
            SHavocStmt { _shHavocStmt :: (HavocStmt id a') } |
            SIfStmt'' { _sifIfStmt :: (IfStmt' n id a) } |
            SBlockStmt { _sbAInfo :: (Either' a, Either' a),
                       _sbBlockStmt :: [n id a] } |
            SIfStmt' { _sifIfStmt' :: AltList (IfInfo n id a) (Either' (Either' a)) } |
            SIfStmt { _ifInfo'' :: Either' (Either' (a, a, a, a)),
                        _ifExpr'' :: Expr Op id,
                        _ifThenB'' :: [n id a],
                        _ifElseB'' :: Maybe [n id a]
                      }

            deriving (Eq, Ord, Show, Read)

instance Functor (Stmt id) where
    fmap = bimap id

instance Foldable (Stmt id) where
    foldMap = bifoldMap (const mempty)

instance Traversable (Stmt id) where
    traverse = bitraverse pure

instance Bifunctor Stmt where
    bimap f g (S (SVarDecl d)) = S (SVarDecl (bimap f g d))
    bimap f g (S (SAssignStmt a')) = S $ SAssignStmt (bimap f g a')
    bimap f g (S (SAssertStmt as)) = S $ SAssertStmt (bimap f g as)
    bimap f g (S (SAssumeStmt s)) = S $ SAssumeStmt (bimap f g s)
    bimap f g (S (SHavocStmt h)) = S $ SHavocStmt  (bimap f g h)
    bimap f g (S (SIfStmt'' i)) = S $ SIfStmt''  (bimap f g i)
    bimap f g (S (SBlockStmt (l, r) s))
        = S $ SBlockStmt (fmap g l, fmap g r) (map (bimap f g) s)
    bimap f g (S (SIfStmt' a)) = S $ SIfStmt' (bimap h h' a)
        where
          h' = (fmap . fmap) g
          h =  bimap (bimap (fmap f) (bimap (fmap f) (fmap (bimap f g))))
               (fmap (bimap (fmap f) (fmap (bimap f g))))
    bimap f g (S (SIfStmt a e th el)) 
        = S $ SIfStmt ((fmap.fmap) (\x -> x&each %~ g) a) (fmap f e) (fmap (bimap f g) th)
          ((fmap.fmap) (bimap f g) el)
{-
naturality
t . traverse f = traverse (t . f) for every applicative transformation t
identity
traverse Identity = Identity
composition
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
-}
instance Bifoldable Stmt where
    bifoldMap f g (S (SVarDecl d)) = bifoldMap f g d
    bifoldMap f g (S (SAssignStmt a)) = bifoldMap f g a
    bifoldMap f g (S (SAssertStmt a)) = bifoldMap f g a
    bifoldMap f g (S (SAssumeStmt a)) = bifoldMap f g a
    bifoldMap f g (S (SHavocStmt h)) = bifoldMap f g h
    bifoldMap f g (S (SIfStmt'' i)) = bifoldMap f g i
    bifoldMap f g (S (SBlockStmt (l, r) s)) 
        = foldMap g l <> foldMap (bifoldMap f g) s <> foldMap g r
    bifoldMap f g (S (SIfStmt a e th el))
        = g a1 <> 
          foldMap f e <> 
          foldMap (bifoldMap f g) th <> g a2 <> 
          (foldMap.foldMap) (bifoldMap f g) el <> g a3 <> 
          g a4
        where 
          (a1, a2, a3, a4) = either' 
                             (either' id (\(b,a,c,d) -> (a,b,c,d)))
                             (either' (\(c,a,b,d) -> (a,b,c,d))
                                      (\(d,a,b,c) -> (a,b,c,d))) a
    bifoldMap f g (S (SIfStmt' a))
        = bifoldMap 
          (bifoldMap 
           (bifoldMap 
            (foldMap f) 
            (bifoldMap 
             (foldMap f)
             (foldMap (bifoldMap f g))))
           (foldMap 
            (bifoldMap
             (foldMap f)
             (foldMap (bifoldMap f g)))))
          (foldMap (foldMap g))
          a
          
instance Bitraversable Stmt where
    bitraverse f g (S (SVarDecl d)) = S . SVarDecl <$> (bitraverse f g d)
    bitraverse f g (S (SAssignStmt a')) = S . SAssignStmt <$>  (bitraverse f g a')
    bitraverse f g (S (SAssertStmt as)) = S . SAssertStmt <$>  (bitraverse f g as)
    bitraverse f g (S (SAssumeStmt s)) = S . SAssumeStmt <$> (bitraverse f g s)
    bitraverse f g (S (SHavocStmt h)) = S . SHavocStmt <$> (bitraverse f g h)
    bitraverse f g (S (SIfStmt'' i)) = S . SIfStmt'' <$> (bitraverse f g i)
    bitraverse f g (S (SBlockStmt (l, r) s))
        = h <$> traverse g l
          <*> traverse (bitraverse f g) s
          <*> traverse g r
        where
          h x y z = S (SBlockStmt (x, z) y)
    bitraverse f g (S (SIfStmt aInfo e th el))
        = h <$> g enterIf <*>
          traverse f e <*>
          traverse (bitraverse f g) th <*> g afterThen <*>
          (traverse.traverse) (bitraverse f g) el <*> g afterElse <*>
          g exitIf
        where 
          h a b c d e f g = S (SIfStmt ((fmap.fmap) (const (a,d,f,g)) aInfo) b c e)
          (enterIf, afterThen, afterElse, exitIf)
              = either' (either' id (\(b,a,c,d) -> (a,b,c,d)))
                (either' (\(c,a,b,d) -> (a,b,c,d))
                         (\(d,a,b,c) -> (a,b,c,d))) aInfo
    bitraverse f g (S (SIfStmt' a)) = S . SIfStmt' <$> bitraverse h h' a
        where
          h =  bitraverse
               (bitraverse
                (traverse f)
                (bitraverse (traverse f) (traverse (bitraverse f g))))
               (traverse (bitraverse (traverse f) (traverse (bitraverse f g))))
          h' = (traverse.traverse) g

data AssignStmt id a = AssignStmt {
      _assInfo :: (Either' a, Either' a),
      _assgnID :: id,
      _assgnExpr :: (Expr Op id)
} deriving (Eq, Ord, Show, Read, Functor)

instance Comonad (AssignStmt id) where
    extract (AssignStmt a _ _) = extract (fst a)
    duplicate a@(AssignStmt (l, r) i e) = AssignStmt (a <$ l, a' <$ r) i e
        where 
          a' = AssignStmt (r, l) i e

instance Bifunctor AssignStmt where
    bimap f g (AssignStmt a b e)
          = AssignStmt (bimap (fmap g) (fmap g) a) (f b) (fmap f e)

instance Bifoldable AssignStmt where
    bifoldMap f g (AssignStmt (l, r) id e) 
        = foldMap g l <> f id <> foldMap g r <> foldMap f e

instance Foldable (AssignStmt id) where
    foldMap = bifoldMap (const mempty)


instance Bitraversable AssignStmt where
    bitraverse f g (AssignStmt (l, r) b e) 
        = h <$> traverse g l <*> traverse f e <*> traverse g r <*> f b
          where 
            h a b c d = AssignStmt (a, c) d b

instance Traversable (AssignStmt id) where
    traverse = bitraverse pure

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
type IfStmt id a = IfStmt' Stmt
data IfStmt' n id a = IfStmt {
      _ifInfo :: a,
      _ifExpr :: Expr Op id,
      _ifThenB :: [n id a],
      _ifElseB :: Maybe [n id a]
} deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Bifunctor n => Bifunctor (IfStmt' n) where
    bimap f g (IfStmt a e t els )
          = IfStmt (g a) (fmap f e) (fmap (bimap f g) t) ((fmap.fmap) (bimap f g) els)

instance (Bifunctor n, Bifoldable n) => Bifoldable (IfStmt' n) where
    bifoldMap f g (IfStmt a e th el) 
        = g a <> foldMap f e <> foldMap (bifoldMap f g) th 
          <> ((foldMap.foldMap) (bifoldMap f g) el)

instance Bitraversable n => Bitraversable (IfStmt' n) where
    bitraverse f g (IfStmt a e t els)
        = IfStmt <$> g a <*> traverse f e <*> (traverse (bitraverse f g) t)
          <*> ((traverse.traverse) (bitraverse f g) els)

data AltList t a = Centre a |
                   Outer a (AltList a t) a
                   deriving (Eq, Ord, Show, Read)

type IfInfo n id a
    = Either (Either (Expr Op id) (Expr Op id, [n id a])) (Maybe (Expr Op id, [n id a]))

makePrisms ''PrePost
makeLenses ''PrePost
makePrisms ''Stmt
makeLenses ''Stmt
makePrisms ''Stmt'
makeLenses ''Stmt'
makeLenses ''FormalParam
makeLenses ''ProcedureDecl'
makeLenses ''AssignStmt
makeLenses ''AssumeStmt
makeLenses ''AssertStmt
makeLenses ''HavocStmt
makeLenses ''IfStmt'
makeLenses ''VarDecl
makeLenses ''Program
makeLenses ''Program'
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

{-
Pure x >>= f = f x

return a >>= f ~ f a
m >>= return ~ m
(m >>= f) >>= g ~ m >>= (f >>= g)
-}
    
instance Comonad (Stmt id) where
    extract (S x) = extract' x
        where 
          extract' (SVarDecl v) = _vInfo v
          extract' (SAssignStmt a) = extract a
          extract' (SAssertStmt a) = _assStmtInfo a
          extract' (SAssumeStmt a) = _assmeInfo a
          extract' (SHavocStmt h) = _havocInfo h
          extract' (SIfStmt'' i) = _ifInfo i
          extract' (SBlockStmt e _) = either' id id . fst $ e
          extract' (SIfStmt' a) = extract . extract . extract $ a
          extract' (SIfStmt a _ _ _) = (extract.extract) a^._1

    duplicate s@(S(SVarDecl (VarDecl _ id))) = S (SVarDecl (VarDecl s id))
    duplicate s@(S(SAssignStmt (AssignStmt (l, r) e v))) = S(SAssignStmt (AssignStmt a' e v))
        where
          a' = (s <$ l, s' <$ r)
          s' = S (SAssignStmt (AssignStmt (r, l) e v))

    duplicate s@(S(SAssertStmt (AssertStmt _ v))) = S(SAssertStmt (AssertStmt s v))
    duplicate s@(S(SAssumeStmt (AssumeStmt _ v))) = S(SAssumeStmt (AssumeStmt s v))
    duplicate s@(S(SHavocStmt (HavocStmt _ v))) = S(SHavocStmt (HavocStmt s v))
    duplicate s@(S(SIfStmt'' ifS@(IfStmt _ e st els )))
        = S(SIfStmt'' (IfStmt s e (map duplicate st) ((fmap.map) duplicate els)))
    duplicate s@(S(SBlockStmt (l, r) st))
        = S(SBlockStmt a' (fmap duplicate st))
        where
          a' = (s <$ l, s' <$ r)
          s' = S (SBlockStmt (r, l) st)
    duplicate s@(S(SIfStmt a e th el))
        = S(SIfStmt
            ((fmap.fmap) (const (s, s', s'', s''')) a)
            e 
            (map duplicate th)
            ((fmap.map) duplicate el))
        where
          (a1, a2, a3, a4) = either' 
                             (either' id (\(b,a,c,d) -> (a,b,c,d)))
                             (either' (\(c,a,b,d) -> (a,b,c,d))
                                       (\(d,a,b,c) -> (a,b,c,d))) a
          s' = S (SIfStmt (pos 1 a') e th el)
          s'' = S (SIfStmt (pos 2 a'') e th el)
          s''' = S (SIfStmt (pos 3 a''') e th el)
          a' = (a2, a1, a3, a4)
          a'' = (a3, a1, a2, a4)
          a''' = (a4, a1, a2, a3)
    duplicate (S(SIfStmt' a)) = error "TODO: Implement duplicate for IfStmt case" {-SIfStmt' (first f a')
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

instance Bifunctor (Program' a') where
    bimap f g (Program a ps pds)
        = Program (g a) ((map.first) f ps) ((map.first) f pds)
