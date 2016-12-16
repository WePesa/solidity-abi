-- |
-- Module: Blockchain.Ethereum.Solidity
-- Description: Function and data types to parse a Solidity file into
--   a high-level data type.
-- Maintainer: Ryan Reich <ryan@blockapps.net>
module Blockchain.Ethereum.Solidity (
  parseToStructure,
  module SolidityTypes,
  ) where

import JSON ()
import Structure
import SolidityTypes

