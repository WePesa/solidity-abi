module C3 (c3Linearize, c3Memoize) where

import Data.Either
import Data.List.NonEmpty
import Data.Map (Map)
import Data.Semigroup
import qualified Data.Map as Map

import Prelude hiding (map, head, tail, dropWhile)
import Prelude as P (map)

c3Linearize :: (Ord a) => Map a [a] -> Map a (NonEmpty a)
c3Linearize xM = xLM
  where xLM = Map.mapWithKey (c3Memoize xLM) xM

c3Memoize :: (Ord a) => Map a (NonEmpty a) -> a -> [a] -> NonEmpty a
c3Memoize xLM x xDeps = c3Combine x $ P.map getDeps xDeps
  where
    getDeps y = Map.findWithDefault (error $ "Invalid base class found") y xLM

c3Combine :: (Eq a) => a -> [NonEmpty a] -> NonEmpty a
c3Combine [] x = fromList [x]
c3Combine xDepsL x  = x <| c3Merge (xDepsLNE <> fromList [xDepsNE])
  where
    xDepsLNE = fromList xDepsL
    xDepsNE = map head xDepsLNE

c3Merge :: (Eq a) => NonEmpty (NonEmpty a) -> NonEmpty a
c3Merge = unfoldr c3Split

c3Split :: (Eq a) => NonEmpty (NonEmpty a) -> (a, Maybe (NonEmpty (NonEmpty a)))
c3Split x = (h, if null t then Nothing else Just $ fromList t)
  where
    t = foldr takeNonHeads [] xEL
    h = case dropWhile isRight xEL of
          [] -> error $ "Cyclic inheritance graph" 
          Left hl : _ -> head hl
    xEL = map (\l@(h' :| _) -> (if any ((h' `elem`) . tail) x then Right else Left) l) x
    takeNonHeads xE l = either (addNonEmpty l . tail) (: l) xE
    addNonEmpty ll l = if null l then ll else fromList l : ll
