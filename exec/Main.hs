import Blockchain.Ethereum.Solidity

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS

import Data.List
import Data.Maybe

import qualified Data.Map as Map

import System.Environment

main :: IO ()
main = do
  sourceFiles <- getArgs
  (mainFile, sourceMap) <- do
    let (mainFile, imports) = fromMaybe (error "No source files given") $ uncons sourceFiles
    mainSrc <- case mainFile of
      "--stdin" -> getContents
      f -> readFile f
    importMap <- sequence $ Map.fromList $ zip imports $ map readFile imports
    return (mainFile, Map.insert mainFile mainSrc importMap)
  BS.putStr $ Aeson.encodePretty $ parseToStructure mainFile sourceMap 

