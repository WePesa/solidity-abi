module DAG (
  DAGError(..),
  checkDAG
  ) where

import Data.Foldable
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Semigroup

data DAGError a = DAGMissing a a | DAGCycle a

data SlurpState a b = Normal b | Fail (DAGError a) | Done

instance (Functor b) => Functor (SlurpState a b) where
  fmap f (Normal x) = Normal (f <$> x)
  fmap x = x

instance (Semigroup b) => Monoid (SlurpState a b) where
  mappend x@(Fail _) _ = x
  mappend _ x@(Fail _) = x
  mappend (Normal x) (Normal y) = Normal (x <> y)
  mappend Done x = x
  mappend x Done = x
  mempty = Done

checkDAG :: (Ord a, Monoid b) => (a -> b -> Bool) -> Map a b -> Either (DAGError a) ()
checkDAG melem dMap =
  case foldMap (const mempty <$>) dMap' of
    Done -> Right ()
    Normal _ -> checkDAG melem dMap'
    Fail e -> Left e
    where dMap' = slurpOnce melem dMap''
          dMap'' = map Normal dMap

slurpOnce :: (Ord a, Semigroup b) => (a -> b -> Bool) -> Map a (SlurpState b) -> Map a (SlurpState b)
slurpOnce melem dMap = Map.mapWithKey shiftState dMap
  where
    shiftState x (Normal y) = checkCycles $ foldMap id $ map getKey y
      where
        getKey k = Map.findWithDefault (Fail $ DAGMissing x k) k dMap
        checkCycles z@(Normal y) =
          if x `melem` y
          then Fail $ DAGCycle x
          else z
        checkCycles x = x
    shiftState _ x = x

