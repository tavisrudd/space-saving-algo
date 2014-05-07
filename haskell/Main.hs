{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances  #-}
module Main where

import qualified Data.Map.Lazy as M
import Data.List (minimumBy, foldl')

import qualified "mtl" Control.Monad.State as St

import Pipes
import qualified Pipes.Prelude as Pipes
-- import qualified Pipes.Lift as Pipes

class Ord a => StreamSummary s a | s -> a where
  size :: s -> Int
  member :: a -> s -> Bool
  insert :: s -> a -> s
  incr :: s -> a -> s
  updateLowest :: s -> a -> s

  update :: Int -> s -> a -> s
  update k ss x
      | member x ss = incr ss x
      | size ss < k = insert ss x
      | otherwise   = updateLowest ss x

-- derived from https://github.com/Cipherwraith/space-saving
-- see discussion at
-- http://www.reddit.com/r/haskell/comments/1sj5su/
--  ... spacesaving_algorithm_in_haskell_approximate_the/

instance Ord a => StreamSummary (M.Map a Integer) a where
  size = M.size
  member = M.member
  insert m v = M.insert v 1 m
  incr m v = M.adjust (+ 1) v m
  updateLowest m s = M.insert s (lowestValue + 1) lowestKeyRemoved
    where
      (lowestKey, lowestValue) = minimum' m
      lowestKeyRemoved = M.delete lowestKey m

      comp :: Ord a => (t, a) -> (t1, a) -> Ordering
      comp (_, x) (_, y) = compare x y

      minimum' :: (Ord a) => M.Map k a -> (k, a)
      minimum' = minimumBy comp . M.toList

type MapSummary a = M.Map a Integer

spaceSavingOnList :: (Ord a, StreamSummary ss a, a ~ a) => ss -> Int -> [a] -> ss
spaceSavingOnList ss0 k = foldl' step ss0
  where
    step ss x = update k ss x

spaceSavingOnPipe :: (St.MonadState s m, StreamSummary s a) => Int -> Proxy () a () s m b
spaceSavingOnPipe k = go
  where
    step ss x = update k ss x
    go = do
      x <- await
      lift $ St.modify (`step` x)
      yield =<< St.get
      go

main :: IO ()
main = do
  let input = words "aa bb ab aa aa cc dd cc"
      ss0 = M.empty :: MapSummary String
      k = 3
  print $ spaceSavingOnList ss0 k $ input
  (`St.evalStateT` ss0) . runEffect $ each input >-> spaceSavingOnPipe k >-> Pipes.print
  -- Pipes.evalStateP ss0 ...
