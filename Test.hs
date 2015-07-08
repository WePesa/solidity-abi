import Parser
import SymbolTable
import JSON

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Functor

main = do
  solidityCode <- getContents
  either print (BS.putStr . Aeson.encodePretty) $ map makeContractABI <$> getABI "stdin" solidityCode
