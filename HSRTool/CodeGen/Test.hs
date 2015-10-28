{-# LANGUAGE LambdaCase, TemplateHaskell #-}

import HSRTool.Parser.Types
import Control.Monad.State
import Data.Distributive
import Data.Traversable
import Data.Foldable
import Control.Comonad
import Data.Monoid
import Control.Applicative
import Control.Lens
import qualified Data.Map as M
import Data.Bitraversable

data NewId = NewId { 
      _varId :: String, 
      _count :: Int } deriving(Eq, Ord, Read)
instance Show NewId where
    show (NewId v c) = v ++ show c
makeLenses ''NewId

type NextCount = Int
type Mp = M.Map String ([NewId], NextCount)
data St = St {
      _mp :: Mp
    } deriving (Eq, Ord, Show, Read)
makeLenses ''St

upd id = Just . maybe freshId updateId
    where 
      freshId = ([NewId id 1], 2)
      updateId x =  x & _1.ix 0.count .~ view _2 x & _2 %~ (+1)

mapDiff :: Stmt String a -> State St Mp
mapDiff (SVarDecl (VarDecl _ id)) = do
  mp %= M.alter (upd id) id
  _mp <$> get
mapDiff (SAssignStmt (AssignStmt _ id _)) = do
  mp %= M.alter (upd id) id
  _mp <$> get
mapDiff (SHavocStmt (HavocStmt _ id)) = do
  mp %= M.alter (upd id) id
  _mp <$> get
mapDiff (SBlockStmt' (Either' (Left _), _) _) = do
  mp.traverse._1 %= g
  _mp <$> get
      where 
        g [] = []
        g (x:xs) = x:x:xs
mapDiff (SBlockStmt' (Either' (Right _), _) _) = do
  mp %= M.mapMaybe g
  _mp <$> get
      where 
        g ([], _) = Nothing
        g (_:[], _) = Nothing
        g x = Just (x & _1 %~ tail)
mapDiff _ = _mp <$> get

run prog = runState (bitraverse subst id (prog =>> mapDiff)) initSt
    where 
      initSt = St M.empty

subst :: String -> State St NewId
subst s = do
  st <- get
  return (lkup (_mp st) s)

lkup :: Mp -> String -> NewId
lkup mp var | null val = error "No variable defined in scope"
            | otherwise = head val
    where 
      val = maybe 
             (error "Uninitialized Variable encountered in lookup") 
             fst
             (M.lookup var mp)

--  (integrate . fst $ runState (traverse id $ ex1 =>> asd') 0) []
ex0 = SVarDecl (VarDecl () "x")
ex1 = SBlockStmt' (Either' (Left ()), Either' (Right ()))
      [SVarDecl (VarDecl () "x")]
ex2 = SBlockStmt' (Either' (Left ()), Either' (Right ()))
      [SVarDecl (VarDecl () "x"), 
       SVarDecl (VarDecl () "y"),
       SAssignStmt (AssignStmt () "x" (ELit 3)),
       SAssignStmt (AssignStmt () "y" (ELit 3))]
ex3 = SBlockStmt' (Either' (Left ()), Either' (Right ()))
      [SVarDecl (VarDecl () "x"), ex2, ex1]

