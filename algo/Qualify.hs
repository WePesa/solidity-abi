module Qualify (
  getQualifiedNames,
  QualifyAs(..), QualifyError(..)
  ) where

import Data.Bifunctor
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map

data QualifyError a b c = 
  LocalNameMissing a b |
  GlobalNameMissing a |
  LocalNameDuplicate a c

data QualifyAs b c = 
  QualifyAll (b -> c) |
  QualifySome [(b, c)]

getQualifiedNames :: (Ord a, Ord b, Ord c) => [(a, QualifyAs b c)] ->
                     Map a (Map b d) -> Either (QualifyError a b c) (Map a (Map c d))
getQualifiedNames qs m = Map.foldrWithKey (\k _ _ -> Left $ GlobalNameMissing k) result missingMap
  where
    result = sequence $ Map.intersectionWithKey qualifyNames qMap m
    missingMap = Map.difference qMap m
    qMap = Map.fromList qs
 
qualifyNames :: (Ord b, Ord c) => a -> QualifyAs b c -> Map b d -> Either (QualifyError a b c) (Map c d)
qualifyNames name (QualifyAll f) m = fromListWithError name $ Map.foldrWithKey (\k x xs -> (f k, x) : xs) [] m
qualifyNames name (QualifySome l) m = fromListWithError name =<< mapM (getName m) l
  where getName m (k, x) = do
          val <- Map.findWithDefault (Left $ LocalNameMissing name k) k $ Map.map Right m
          return (x, val)

fromListWithError :: (Ord c) => a -> [(c, d)] -> Either (QualifyError a b c) (Map c d)
fromListWithError name =
   sequence . Map.fromListWithKey (\k _ _ -> Left $ LocalNameDuplicate name k) . map (second Right)

