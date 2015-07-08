import Parser
import SymbolTable
import JSON

import qualified Data.Aeson as Aeson
import Data.Functor

main = do
  solidityCode <- getContents
  either print (print . Aeson.encode) $ map makeContractABI <$> getABI "stdin" solidityCode
