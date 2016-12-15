# solidity-abi

A partial Solidity parser that emits a JSON object describing the
top-level definitions in a Solidity file.

This version is compatible with solc-0.2.0.

## Usage

```sh
# Command-line
solidity-abi (main-file.sol | --stdin) import1.sol ... 
```
```haskell
-- Haskell
import Blockchain.Ethereum.Solidity.Layout
import Blockchain.Ethereum.Solidity.Parse            
import Blockchain.Ethereum.Solidity.External.JSON    
import Blockchain.Ethereum.Solidity.External.Contract

import Data.Map (Map)
import qualified Data.Map as Map

sourceFiles :: Map FileName SourceCode

parsedFiles :: Either ParseError (Map FileName SolidityFile)
parsedFiles = sequence $ Map.mapWithKey parseSolidity sourceFiles

-- `Left error` reports errors that we catch explicitly
-- `Right value` is the successful result
mainJSON :: FileName -> Either Value Value
mainJSON mainFileName = jsonABI mainFileName =<< parsedFiles'
  where parsedFiles' = either (Left . toJSON . show) id parsedFiles
```

## Handling of imports
The emitted JSON contains information for the provided _main_ file, with all
imported contracts included.  The imported files are not emitted separately.
The following import syntaxes are accepted:

- (Basic) `import "filename";`
- (Qualified basic) `import "filename" as Qualifier;`
- (Star ES6) `import * from "filename";`
- (Selective and aliasing ES6) `import {C, D as E} from "filename";`

All but the last one import every contract from `filename`; in the case of the
second, each one is qualified as `C -> Qualifier.C`.  In the last case, only the
given contract names are imported, and possibly renamed as shown; with the
statement shown, the imported names are `C` and `E`, where `E` represents the
contract declared as `D`.

*Difference with solc* At least through version 0.3.6, the solc compiler does
_not_ rename imported contracts when emitting binaries.  That is, in the second
case above, a contract `C` in `filename` is reported as `C` alongside its
binary.  This appears to be an error and we do not imitate it.

Import filenames are resolved by consulting the list of command-line arguments
(for the CLI tool) or map keys (for the library).  In order to maintain the
functional purity of the library, we do not (currently) support import file
discovery at runtime.  If an import is missing, then the result will be an error
JSON object of the form
```js
missingImport = {
  "missingImport": <file path>
}
```
containing the first missing import encountered.

## Unsuccessful output
Most errors in parsing will emit en error message and quit immediately.  If the error is a missing import, the result is instead the successful output of a JSON object:
```js
missingImport = {
  "missingImport": <file path>
}
```
containing the first missing import encountered.

## Successful output
Upon successful parsing, `solidity-abi` returns a JSON object object (the
"ABI") with the following structure:

### File level
```js
file = {
  "contract name" : contract,
  ...
}
```

## JSON output structure

Both the `jsonABI` Haskell function or the `solidity-abi` executable
produce JSON of the following form

### Contract level
```js
contract = {
   "realName" : string (name contract was defined with),
   "vars" : {
     "var name" : variable (visible externally)
     ...
   },
   "funcs" : {
     "func name" : function (visible externally),
     ...
   },
   "types" : {
     "type name" : type defn,
     ...
   },
   "libraryTypes" : {
      "library name" : {
        "type name" : type defn,
        ...
      },
      ...
   },
   "constr" : function args,
   // For libraries only
   "library" : true
}
```
where any of the fields, if empty, is omitted.

### State variables

The short version is that a variable contains, in addition to the
"atBytes" field shown below, a "type" field describing its general
category; a "bytes" field describing its byte usage, if applicable; a
"dynamic" field if it is of dynamic length; an "entry" field
describing its entries, if it is an array; and a few type-specific
fields as shown below.

```js
variable = {
  "atBytes" : decimal number (byte position in storage),
  basic type ABI
}
basic type ABI = {
  // intN
  "type" : "Int",
  "signed" : true,
  "bytes" : decimal number (= N/8; byte length)
  // uintN
  "type" : "Int",
  "bytes" : decimal number (= N/8; byte length)
  // bytesN
  "type" : "Bytes",
  "bytes" : decimal number (= N; byte length)
  // bytes
  "type" : "Bytes",
  "dynamic" : true
  // string
  "type" : "String",
  "dynamic" : true
  // T[n]
  "type" : "Array",
  "length" : decimal number (= n; number of entries)
  "entry" : basic type ABI of T
  // T[]
  "type" : "Array",
  "dynamic" : true,
  "entry" : basic type ABI of T
  // mapping(keyT => valT)
  "type" : "Mapping",
  "dynamic" : true,
  "key" : basic type ABI of keyT,
  "value" : basic type ABI of valT
  // name
  "typedef" : name (identifier of user-defined type)
  "library" : name (name of library containing the type; absent if not from a library)
}
```

### Functions
```js
function = {
  "selector" : 4-byte hex string (the "function selector"),
  "args" : function args
  "vals" : function args
}
function args = {
  "arg name" : {
    "index" : decimal integer (place in argument list),
    basic type ABI
  },
  "#n" : {
    "index" : decimal integer (= n, only if this arg is unnamed),
    basic type ABI
  },
  ...
}
```

### Types

```js
type defn = {
  // struct
  "type" : "Struct",
  "fields" : {
    "field name" : variable,
    ...
  }
  // enum
  "type" : "Enum",
  "bytes" : decimal integer (smallest number of bytes holding all values),
  "names" : ["named value", ... ]
  // "using"
  "type" : "Using",
  "usingContract" : string (contract name),
  "usingType" : string (type name within the contract)
}
```
