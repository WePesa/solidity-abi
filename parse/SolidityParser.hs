module SolidityParser where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.Parsec

import SolidityTypes

type SolidityParser = Parsec SourceCode SolidityContract

initContract :: ContractID -> SolidityParser ()
initContract cID = putState $ emptyContract cID

addVar :: SolidityVarDef -> SolidityParser ()
addVar v = modifyState $
  \c@Contract{contractDeclarationsByID = cDIs, contractDeclarationsByName = cDNs} =
    c@Contract{
      contractStorageVars = 
        case varStorage v of
          StorageStorage -> varID v:contractStorageVars c
          _ -> contractStorageVars c,
      contractDeclarationsByID = cDIs{declaredVars = Map.insert (varID v) v $ declaredVars cDIs},
      contractDeclarationsByName = cDNs{declaredVars = Map.insert (declarationRealName $ varID v) v $ declaredVars cDNs}
      }

addFunc :: SolidityFuncDef -> SolidityParser ()
addFunc f = modifyState $
  \c@Contract{contractDeclarationsByID = cDIs, contractDeclarationsByName = cDNs} =
    c{
      contractDeclarationsByID = cDIs{declaredVars = Map.insert (funcID f) f $ declaredFuncs cDIs},
      contractDeclarationsByName = cDNs{declaredVars = Map.insert (declarationRealName $ funcID f) f $ declaredFuncs cDNs},
      contractIsConcrete = contractIsConcrete c && funcHasCode f
     }

addEvent :: SolidityEventDef -> SolidityParser ()
addEvent e = modifyState $ 
  \c@Contract{contractDeclarationsByID = cDIs, contractDeclarationsByName = cDNs} =
    c{
      contractDeclarationsByID = cDIs{declaredVars = Map.insert (eventID e) e $ declaredEvents cDIs},
      contractDeclarationsByName = cDNs{declaredVars = Map.insert (declarationRealName $ eventID e) e $ declaredEvents cDNs}
     }

addModifier :: SolidityModifierDef -> SolidityParser ()
addModifier m = modifyState $
  \c@Contract{contractDeclarationsByID = cDIs, contractDeclarationsByName = cDNs} =
    c{
      contractDeclarationsByID = cDIs{declaredModifiers = Map.insert (modID m) m $ declaredModifiers cDIs},
      contractDeclarationsByName = cDNs{declaredModifiers = Map.insert (declarationRealName $ declaredModifiers m) m $ declaredModifiers cDNs}
     }

addType :: SolidityTypeDef -> SolidityParser ()
addType t = modifyState $
  \c@Contract{contractDeclarationsByID = cDIs, contractDeclarationsByName = cDNs} =
    c{
      contractDeclarationsByID = cDIs{declaredTypes = Map.insert (typeID t) t $ declaredTypes cDIs},
      contractDeclarationsByName = cDNs{declaredTypes = Map.insert (declarationRealName $ declaredTypes t) t $ declaredTypes cDNs}
     }

addLibraryType :: ContractName -> Identifier -> SolidityParser ()
addLibraryType n t = modifyState $
  \c@Contract{contractLibraryTypes = lts} =
    c{ contractLibraryTypes = Map.alter (maybe (Just Set.empty) (Just . Set.insert t)) n lts } 
addBase :: ContractName -> SolidityParser ()
addBase n = modifyState $
  \c = c{contractInherits = x : contractInherits c}

setIsLibrary :: Bool -> SolidityParser ()
setIsLibrary b = modifyState $ \c -> c{contractIsLibrary = b}

getIsAbstract :: SolidityParser Bool
getIsAbstract = not . contractIsConcrete <$> getState

emptyContract :: ContractID -> SolidityContract
emptyContract cID = 
  Contract {
    contractID = cID,
    contractStorageVars = [],
    contractDeclarationsByID = emptyDeclarations,
    contractDeclarationsByName = emptyDeclarations,
    contractInherits = [],
    contractLibraryTypes = Map.empty,
    contractIsConcrete = True,
    contractIsLibrary = False
    }

emptyDeclarations :: SolidityDeclarations a
emptyDeclarations =
  Declarations {
    declaredVars = Map.empty,
    declaredFuncs = Map.empty,
    declaredEvents = Map.empty,
    declaredModifiers = Map.empty,
    declaredTypes = Map.empty
    }

