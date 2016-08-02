{-# LANGUAGE FlexibleContexts #-}
module DAG (
  DAGError(..),
  checkDAG
  ) where

import Data.Foldable
import Data.Traversable
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Semigroup

data DAGError a = DAGMissing a a | DAGCycle a

data SlurpState t a = Normal (t a) | Fail (DAGError a) | Done

instance (Semigroup (t a)) => Monoid (SlurpState t a) where
  mappend x@(Fail _) _ = x
  mappend _ x@(Fail _) = x
  mappend (Normal x) (Normal y) = Normal (x <> y)
  mappend Done x = x
  mappend x Done = x
  mempty = Done

checkDAG :: (Ord a, Traversable t, Semigroup (t a)) => Map a (t a) -> Either (DAGError a) ()
checkDAG dMap = checkDAG' (Map.map Normal dMap) >> return ()

checkDAG' :: (Ord a, Traversable t, Semigroup (t a)) => Map a (SlurpState t a) -> Either (DAGError a) ()
checkDAG' dMap =
  case foldMap id dMap' of
    Done -> Right ()
    Normal _ -> checkDAG' dMap'
    Fail e -> Left e
    where dMap' = slurpOnce dMap

slurpOnce :: (Ord a, Traversable t, Semigroup (t a)) => Map a (SlurpState t a) -> Map a (SlurpState t a)
slurpOnce dMap = Map.mapWithKey shiftState dMap
  where
    shiftState x (Normal y) = checkCycles $ foldMap getKey y
      where
        getKey k = Map.findWithDefault (Fail $ DAGMissing x k) k dMap
        checkCycles z@(Normal y) =
          if x `elem` y
          then Fail $ DAGCycle x
          else z
        checkCycles x = x
    shiftState _ x = x

