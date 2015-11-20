import Blockchain.Ethereum.Solidity.Parse
import Blockchain.Ethereum.Solidity.Storage

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import Data.Map (fromList)

main = do
  solidityCode <- getContents
  either print (BS.putStr . Aeson.encodePretty) $ symtab <$> parse "stdin" solidityCode
