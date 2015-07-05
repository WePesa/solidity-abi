import Parser

main = do
  solidityCode <- getContents
  either print (print . pretty) $ getABI "stdin" solidityCode
