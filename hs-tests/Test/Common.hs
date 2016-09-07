{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Common where

import Distribution.TestSuite

import Blockchain.Ethereum.Solidity.Parse
import Test.Combinators

-- Since Semigroup is not available unless we can upgrade base
infixr 0 <>
(<>) :: Result -> Result -> Result
(<>) Pass x = x
(<>) x y = x

type TestM = Either String

runTestM :: (a -> Result) -> TestM a -> Result
runTestM = either Error

makeTest :: String -> (a -> Result) -> TestM a -> Test
makeTest name tester result = Test theTest
  where
    theTest = TestInstance{
      run = return $ Finished $ runTestM tester result,
      name,
      tags = [],
      options = [],
      setOption = const $ const $ Right theTest
      }

deriving instance Eq SolidityObjDef
deriving instance Eq SolidityBasicType
deriving instance Eq SolidityTuple
deriving instance Eq SolidityNewType

infixr 1 |!
(|!) :: Bool -> String -> Result
(|!) b e = if b then Pass else Fail e

