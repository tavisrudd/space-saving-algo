{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances  #-}
module Main where

import qualified Data.Map.Lazy as M
import Data.List (minimumBy, foldl')

import qualified Control.Monad.State.Strict as St

import Pipes
import qualified Pipes.Prelude as Pipes

-- derived from https://github.com/Cipherwraith/space-saving
-- see discussion at
-- http://www.reddit.com/r/haskell/comments/1sj5su/
--  ... spacesaving_algorithm_in_haskell_approximate_the/

-- see section 5 of http://www.haskell.org/haskellwiki/Type_families
-- re: Type synonym families
-- This can also be done with fundeps: `class StreamSummary s a | s -> a`
-- but I find the relationships to be more explicit when reading the code
-- with type synonym families.
class StreamSummary s where
  type Elem s
  size :: s -> Int
  member :: Elem s -> s -> Bool
  insert :: s -> Elem s -> s
  incr :: s -> Elem s -> s
  updateLowest :: s -> Elem s -> s
  update :: Int -> s -> Elem s -> s
  update k ss x
      | member x ss = incr ss x
      | size ss < k = insert ss x
      | otherwise   = updateLowest ss x

instance Ord a => StreamSummary (M.Map a Integer) where
  type Elem (M.Map a Integer) = a
  size = M.size
  member = M.member
  insert m x = M.insert x 1 m
  incr m x = M.adjust (+ 1) x m
  updateLowest m x = M.insert x (lowestValue + 1) lowestKeyRemoved
    where
      (lowestKey, lowestValue) = minimum' m
      lowestKeyRemoved = M.delete lowestKey m

      comp :: Ord a => (t, a) -> (t1, a) -> Ordering
      comp (_, x') (_, y) = compare x' y

      minimum' :: (Ord a) => M.Map k a -> (k, a)
      minimum' = minimumBy comp . M.toList

-- | It is now legal to specialize on the value type like this:
-- instance StreamSummary (M.Map Int Integer) where
--   type Elem (M.Map Int Integer) = Int
--   ...

spaceSavingOnList :: StreamSummary s => Int -> s -> [Elem s] -> s
spaceSavingOnList = foldl' . update

spaceSavingScan :: StreamSummary s => Int -> s -> [Elem s] -> [s]
spaceSavingScan k ss0 = (drop 1) . scanl (update k) ss0

spaceSavingOnPipe :: (Monad m, StreamSummary s) => Int -> s -> Pipe (Elem s) s m r
spaceSavingOnPipe k ss0 = Pipes.scan (update k) ss0 id >-> Pipes.drop 1

-- uglier versions explicit state management / recursion
spaceSavingOnPipeManual
  :: (Monad m, StreamSummary s) => Int -> s -> Pipe (Elem s) s m r
spaceSavingOnPipeManual k ss0 = go ss0
  where
    step = update k
    go ss = do
      ss' <- step ss `fmap` await
      yield ss'
      go $! ss'

spaceSavingOnPipeST ::
  (St.MonadState s m, StreamSummary s) => Int -> Pipe (Elem s) s m r
spaceSavingOnPipeST k = go
  where
    step = update k
    go = do
      x <- await
      lift $ St.modify (`step` x)
      yield =<< St.get
      go

main :: IO ()
main = do
  let input = words "aa bb ab aa aa cc dd cc ab ac da dc dc"
      ss0 = M.empty :: M.Map String Integer
      k = 5
  -- single output
  print $ spaceSavingOnList k ss0 input
  -- remaining results should look the same
  runEffect $ each (spaceSavingScan k ss0 input) >-> Pipes.print
  runEffect $ each input >-> spaceSavingOnPipe k ss0 >-> Pipes.print
  runEffect $ each input >-> spaceSavingOnPipeManual k ss0 >-> Pipes.print
  (`St.evalStateT` ss0) . runEffect $ each input >-> spaceSavingOnPipeST k >-> Pipes.print
  -- Pipes.evalStateP ss0 ...
