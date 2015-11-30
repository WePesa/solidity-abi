import Blockchain.Ethereum.Solidity.Parse
import Blockchain.Ethereum.Solidity.External.JSON

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS

import Data.List
import Data.Maybe

import qualified Data.Map as Map
import Data.Map (Map)

import System.Environment

main = do
  sourceFiles <- getArgs
  let (mainSrc, imports) =
        fromMaybe (error "No source files given") $ uncons sourceFiles
  sources <- sequence $ map readFile sourceFiles
  let sourceMap = Map.fromList $ zip imports $ tail sources
      doImport i = Map.findWithDefault (error "Import not found") i sourceMap
      parsed = parse doImport mainSrc $ head sources
  either print (BS.putStr . Aeson.encodePretty) $ jsonABI <$> parsed
