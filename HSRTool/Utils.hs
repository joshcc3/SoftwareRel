{-# LANGUAGE TupleSections, ScopedTypeVariables, DeriveFunctor #-}

module HSRTool.Utils where

import qualified System.Random as SR
import Data.Distributive
import qualified Data.Tree as T
import Data.Monoid
import Control.Lens
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Data.Char
import Prelude hiding ((^))
import Data.List
import qualified Data.Map as M
import Debug.Trace
import Control.Monad
import Control.Applicative

data Tmp x = A x | N deriving (Eq, Ord, Show, Read)

tMaybe :: Tmp a -> Maybe a
tMaybe N = Nothing
tMaybe (A x) = Just x

fMaybe :: Maybe a -> Tmp a
fMaybe (Nothing) = N
fMaybe (Just x) = A x


invert :: Ord b => [a] -> (a -> [b]) -> b -> [a]
invert ls f = al . flip M.lookup m
  where
    al (Just x) = x
    al _ = []
    m = M.fromListWith (++) ((fmap . fmap) (:[]) pairs)
    pairs = ls >>= (\x -> map (,x) . f $ x)

--fromList :: (Show b, Ord a) => [(a, b)] -> a -> b
fromList p = al . flip M.lookup m
  where
    m = M.fromList p
    al (Just x) = x
    al _ = []

replace' '_' = '-'
replace' x = x

swap (x, y) = (y, x)

escape c = flip (>>=) repl
 where
   repl x | x == c =  ['\\', c]
   	  | otherwise = [x]

groupWith :: (a -> a -> Bool) -> (a -> a -> Ordering) -> [a] -> [[a]]
groupWith f g = groupBy f . sortBy g

traceVal x = join (trace . show) $ x

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ a = a
iterateN n f a = iterateN (n-1) f (f a)

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 f a = return a
iterateM n f a = f a >>= iterateM (n-1) f

iterateWhile :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateWhile c f a | c a = a:iterateWhile c f (f a)
                   | otherwise = [a]

(^) :: (a -> a) -> Int -> a -> a
f ^ n = foldl (.) id (replicate n f)

infixl 9 ^

data KMP a = KMP { done :: Bool, next :: a -> KMP a }

table :: Eq a => [a] -> KMP a -> KMP a
table = f
    where 
      f [] fail = KMP True (const (f [] fail))
      f (a:as) fail = KMP False g
          where 
            g a' | a == a' = f as (next fail a)
                 | otherwise = fail

      

kmp :: Eq a => [a] -> [a] -> Bool
kmp super sub = done . drive super $ machine
    where 
      machine = table sub (KMP False (const machine))
      drive [] m = m
      drive (x:xs) m = drive xs (next m x)

al (Just x) = x


splitUpF :: a -> (a -> b) -> ((a, b), a -> b)
splitUpF a f = ((a, f a), f)

mergeF :: Eq a => ((a, b), a -> b) -> a -> b
mergeF ((a, b), g) a' | a == a' = b
                      | otherwise = g a'

iso1 :: Iso' (a -> b, a -> c) (a -> (b, c))
iso1 = iso f g
    where 
      f (h, h') a = (h a, h' a)
      g h = (fst . h, snd . h)



avg :: Int -> [Float] -> [Float]
avg b = map getAvg . tail . scanl f def 
    where 
      getAvg (x, n, _) = x/fi n
      def = (0, 0, [])
      f :: (Float, Int, [Float]) -> Float -> (Float, Int, [Float])
      f (_, 0, _) v = (v, 1, [v])
      f (s, n', x:xs) v | n' == b = (s - x + v, n', xs++[v])
                        | otherwise = (s + v, n'+1, x:xs++[v])


fi = fromIntegral

satAny :: [a -> Bool] -> a -> Bool
satAny = foldl1 (liftA2 (||))

satAll :: [a -> Bool] -> a -> Bool
satAll = foldl1 (liftA2 (&&))

qna x = putStrLn x >> getLine

toCap :: Char -> Char
toCap x | x <= 'Z' && x >= 'A' = x
        | otherwise = chr $ ord x - 32


multiLineEntry = do
 x <- getLine
 if null x then return []
 else (x:) <$> multiLineEntry



accumStrings :: IO [a] -> IO [[a]]
accumStrings f = fmap snd . runWriterT . runMaybeT . forever $ do
  x <- liftIO f
  if null x
     then MaybeT (return Nothing)
     else
     	  tell [x]

rotateByN :: forall a. Int -> [a] -> [a]
rotateByN r ol = map snd l'
    where 
      l' :: [(a, a)]
      l' =  eval list
      len = length ol
      list :: [[(a, a)] -> (a, a)]
      list = map g [0..len-1]
      g :: Int -> [(a, a)] -> (a, a)
      g i l = (ol !! i, fst (l !! ((i + r) `mod` len)))
      



eval :: Functor f => f (f b -> b) -> f b
eval f = fmap ($ eval f) f


randomStream :: Int -> IO [Int]
randomStream r = do
  x <- SR.newStdGen
  let s = snd $ SR.genRange x
  return $ map ((`div`s) . (*r) . fst) (iterate (SR.next . snd) (SR.next x))
