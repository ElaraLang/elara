module Compile where
  
import qualified AST.Source as AST
import Elara.Name (Name)
import qualified Error.Error as E

compile :: Name -> AST.Module -> Either E.Error ()
compile packageName module' = do
  canonical <- canonicalize packageName module'
  return ()

canonicalize :: Name ->  AST.Module -> Either E.Error ()
canonicalize pkg module' = do
  undefined