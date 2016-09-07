module Test (tests) where

import Distribution.TestSuite

import qualified Parser
-- import qualified Import
-- import qualified Layout
import qualified Json

tests :: IO [Test]
tests = return [
  Parser.test,
--  Import.test,
--  Layout.test,
  Json.test
  ]
