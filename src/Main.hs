
import System.Random
import System.Environment

import Control.Monad
import Control.Monad.Random

import Data.List (sortBy, find)

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

import Debug.Trace (traceShowId)

type Color = Int

type Width  = Int
type Height = Int
type Coord  = (Int, Int)

data Board = Board { boardW    :: Width
                   , boardH    :: Height
                   , boardK    :: Int
                   , boardData :: Vector Color }

instance Show Board where
  show b@(Board w h _ d) = unlines $ do
    y <- [0..h-1]
    return $ do
      x <- [0..w-1]
      show (b `at` (x, y))

swap (x, y) = (y, x)

size' :: Board -> Int
size' (Board w h _ _) = w * h

i2c :: Board -> Int -> Coord
i2c (Board w _ _ _) i = swap $ divMod i w

c2i :: Board -> Coord -> Int
c2i (Board w _ _ _) (x, y) = y * w + x

board :: (Width, Height) -> Int -> [Color] -> Board
board (w, h) k cs = Board w h k (V.fromList cs)

at :: Board -> Coord -> Color
at b c = at' b (c2i b c)

at' :: Board -> Int -> Color
at' b i = boardData b ! i

cross :: Coord -> [Coord]
cross (x, y) = [ (x-1, y)
               , (x+1, y)
               , (x, y-1)
               , (x, y+1) ]

inside :: Board -> Coord -> Bool
inside (Board w h _ _) (x, y) = x >= 0 && x < w && y >= 0 && y < h

neighbors :: Board -> Coord -> [Coord]
neighbors b p = filter (inside b) (cross p)

expand :: Board -> Color -> NatSet -> NatSet
expand b x s = insertList s $ do
  i <- elems s
  c <- neighbors b (i2c b i)
  let i' = c2i b c
  guard (not $ nselem s i')
  guard (x == b `at'` i')
  return i'

expandall :: Board -> Color -> NatSet -> NatSet
expandall b x s =
  if s' == s then
    s
  else
    expandall b x s'
  where
    s' = expand b x s

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

branch :: Board -> ([Color], NatSet) -> [([Color], NatSet)]
branch b (ps, s) = do
  k <- ks
  return (k:ps, expandall b k s)
  where
    ks =
      case head' ps of
        Nothing -> [0..boardK b - 1]
        Just k  -> [0,1..k-1] ++ [k+1..boardK b - 1]

reduceBranches :: [(a, NatSet)] -> [(a, NatSet)]
reduceBranches ss = g $ sortBy f ss
  where
    f s t = compare (nscount $ snd s) (nscount $ snd t)
    g [] = []
    g (s:ss) =
      if or $ map (\t -> snd s `isSubsetOf` snd t) ss then
        g ss
      else
        s : g ss

solve :: Board -> [Color]
solve b =
  go [([], init)]
  where
    init = expandall b (b `at'` 0) $ natset sz [0]
    sz = size' b
    done (_, s) = nscount s == sz
    go hs =
      case find done hs of
        Just (ps, _) -> ps
        Nothing -> go $ reduceBranches $ concatMap (branch b) hs

data NatSet = NatSet Int (Vector Bool) deriving (Show, Eq)

natset :: Int -> [Int] -> NatSet
natset n is = insertList (newset n) is

newset :: Int -> NatSet
newset n = NatSet 0 $ V.replicate n False

insert :: NatSet -> Int -> NatSet
insert s@(NatSet n v) i =
  if nselem s i then
    s
  else
    NatSet (n+1) $ v // [(i, True)]

insertList :: NatSet -> [Int] -> NatSet
insertList = foldl insert

nselem :: NatSet -> Int -> Bool
nselem (NatSet _ v) i = v ! i

nscount :: NatSet -> Int
nscount (NatSet n _) = n

elems :: NatSet -> [Int]
elems (NatSet _ v) = V.ifoldl (\r i x -> if x then i:r else r) [] v

implies :: Bool -> Bool -> Bool
implies False _ = True
implies True  x = x

isSubsetOf :: NatSet -> NatSet -> Bool
isSubsetOf (NatSet n v) (NatSet m w) = n <= m && V.and (V.zipWith implies v w)

nsfmt :: Board -> NatSet -> String
nsfmt b@(Board w h _ _) s = unlines $ do
  y <- [0..h-1]
  return $ do
    x <- [0..w-1]
    show $ fromEnum $ nselem s (c2i b (x, y))

nsprint b = putStrLn . nsfmt b

randBoard :: (RandomGen g) => (Width, Height) -> Int -> Rand g Board
randBoard (w, h) k = do
  ps <- sequence (replicate (w*h) (getRandomR (0, k-1)))
  return $ board (w, h) k ps

main = do
  args <- getArgs
  let (w:h:k:_) = map read args
  let g = mkStdGen 0
  let b = evalRand (randBoard (w, h) k) g
  print b
  let sol = reverse $ solve b
  print sol
  putStrLn ""
  let init = (expandall b (at' b 0) $ natset (w*h) [0])
  nsprint b init
  foldM_ (step b) init sol
  where
    step b s k = do
      let s' = expandall b k s
      putStrLn $ "==> " ++ show k
      -- print b
      nsprint b s'
      return s'
