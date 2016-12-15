-- |
-- Module: LayoutTypes
-- Description: Formerly types for layout, now this module has only a few
--   aliases and some global values.
-- Maintainer: Ryan Reich <ryan@blockapps.net>
module LayoutTypes where

import Numeric.Natural

type StorageKey = Natural
-- | Byte locations in storage, as-is
type StorageBytes = Natural

-- | Size of an 'address' variable is always 160 bits
addressBytes :: StorageBytes
addressBytes = 20

-- | Alignment of storage keys is always 32 bytes
keyBytes :: StorageBytes
keyBytes = 32

-- | Converting from the linear representation to keys
bytesToKey :: StorageBytes -> StorageKey
bytesToKey = (`quot` keyBytes)

-- | Byte location of storage indices
keyToBytes :: StorageKey -> StorageBytes
keyToBytes = (* keyBytes)
