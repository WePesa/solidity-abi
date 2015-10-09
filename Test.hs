import Blockchain.Solidity.ABI

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
  solidityCode <- getContents
  either print (BS.putStr . Aeson.encodePretty) $ makeABISymbols <$> getABI "stdin" solidityCode
