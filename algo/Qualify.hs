module Qualify (
  getQualifiedNames,
  QualifyAs(..), QualifyError(..)
  ) where

import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map

data QualifyError a b = 
  LocalNameMissing a b |
  GlobalNameMissing a |
  LocalNameDuplicate a c |
  GlobalNameDuplicate a
  deriving (Eq, Ord)

data QualifyAs b c = 
  QualifyAll (b -> c) |
  QualifySome [(b, c)]
  deriving (Eq, Ord)

getQualifiedNames :: (Ord a, Ord b, Ord c) => [(a, QualifyAs b c)] ->
                     Map a (Map b d) -> Either (QualifyError a b) (Map a (Map c d))
getQualifiedNames qs aDefsE = do
  let qsLocal = map (\(x, qa) -> (x, (LocalNameMissing x, LocalNameDuplicate x, qa))) qs
  globalVals <- qualifyNames GlobalNameMissing GlobalNameDuplicate (QualifySome qsLocal) aDefsE
  Map.mapWithKey (\(x, y, z) -> qualifyNames x y z) globalVals

qualifyNames :: (Ord b) => (b -> QualifyError a b) -> (c -> QualifyError a c) ->
                QualifyAs b c -> Map b d -> Either (QualifyError a b) (Map c d)
qualifyNames _ eD (QualifyAll f) = sequence . mapKeysWithError f
  where mapKeysWithError f = fromListWithError eD . Map.foldrWithKey (\k x xs -> (f x) : xs) []
qualifyNames eM eD (QualifySome l) = sequence . (fromListWithError eD) . flip map l . getName
  where getName m (k, x) = (x, getNameEither k m)
        getNameEither k = Map.findWithDefault (Left $ eM k) k

fromListWithError :: (c -> QualifyError a c) -> [(c, d)] -> Either (QualifyError a c) (Map c d)
fromListWithError e = Map.fromListWithKey (\k _ _ -> Left $ e k)

