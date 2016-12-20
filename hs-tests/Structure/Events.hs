module Structure.Events (test) where

import qualified Data.Map as Map

import Blockchain.Ethereum.Solidity
import Structure.Common
import Test.Combinators
import Test.Common

test :: TestTree
test = doTests "events" structureTest [
  eventNoTopics,
  anonymousEvent,
  eventOneTopic,
  eventOneTopicOneIndexed,
  eventFourTopics
  ]

eventNoTopics :: StructureTestInput
eventNoTopics = eventTest "eventNoTopics" False [] $
  EventDef{
    eventTopics = TupleValue [],
    eventIsAnonymous = False
    }

anonymousEvent :: StructureTestInput
anonymousEvent = eventTest "anonymousEvent" True [] $
  EventDef{
    eventTopics = TupleValue [],
    eventIsAnonymous = True
    }

eventOneTopic :: StructureTestInput
eventOneTopic = eventTest "eventOneTopic" False ["int"] $
  EventDef{
    eventTopics = TupleValue [ArgDef "" (SignedInt 32) StorageStorage],
    eventIsAnonymous = False
    }

eventOneTopicOneIndexed :: StructureTestInput
eventOneTopicOneIndexed = 
  eventTest "eventOneTopicOneIndexed" False ["int", "int indexed"] $
    EventDef{
      eventTopics = TupleValue [makeArgDef StorageStorage, makeArgDef IndexedStorage],
      eventIsAnonymous = False
      }

  where makeArgDef = ArgDef "" (SignedInt 32) 

eventFourTopics :: StructureTestInput
eventFourTopics = 
  eventTest "eventFourTopics" False ["int", "bool", "byte", "address"] $
    EventDef{
      eventTopics = TupleValue [
        makeArgDef $ SignedInt 32,
        makeArgDef $ Boolean,
        makeArgDef $ FixedBytes 1,
        makeArgDef $ Address
        ],
      eventIsAnonymous = False
      }

  where makeArgDef t = ArgDef "" t StorageStorage


eventTest :: String -> Bool -> [String] -> EventDef -> StructureTestInput
eventTest name isAnon topics defn = (name, sources, tester)
  where
    sources = Map.singleton name $
      contractDefn "C" $
        eventDecl "e" isAnon topics
    tester contracts = eventDefnIs name contracts "C" "e" defn

