{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances  #-}
module Main where

import qualified Data.Map.Lazy as M
import Data.List (minimumBy)

class Ord a => StreamSummary s a | s -> a where
  toList :: s -> [(a, Integer)]
  size :: s -> Int
  member :: a -> s -> Bool
  insert :: s -> a -> s
  incr :: s -> a -> s
  updateLowest :: s -> a -> s

-- derived from https://github.com/Cipherwraith/space-saving
-- see discussion at
-- http://www.reddit.com/r/haskell/comments/1sj5su/
--  ... spacesaving_algorithm_in_haskell_approximate_the/

instance Ord a => StreamSummary (M.Map a Integer) a where
  toList = M.toList
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
spaceSavingOnList ss0 k = spaceSave ss0
  where
    spaceSave ss [] = ss
    spaceSave ss (x:xs)
      | member x ss = spaceSave (incr ss x) xs
      | size ss < k = spaceSave (insert ss x) xs
      | otherwise   = spaceSave (updateLowest ss x) xs

-- spaceSavingOnPipe ... using pipes library

main :: IO ()
main = do
  let contents = "aa bb ab aa aa cc dd cc"
      input = words contents
      ss0 = M.empty :: MapSummary String
      k = 3
  print $ spaceSavingOnList ss0 k $ input
