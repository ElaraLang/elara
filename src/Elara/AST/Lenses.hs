{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Common lenses that all the AST types can use. These are mostly unpleasant
module Elara.AST.Lenses where

import Control.Lens
import Elara.AST.Desugared qualified as Desugared
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Region (Located, Unlocate, unlocated)
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.Typed qualified as Typed

class HasDeclarationBody h db | h -> db where
    declarationBody :: Lens' h db
    unlocatedDeclarationBody :: Lens' h (Unlocate db)

instance HasDeclarationBody Frontend.Declaration (Located Frontend.DeclarationBody) where
    declarationBody = Frontend._Declaration . unlocated . declarationBody
    unlocatedDeclarationBody = declarationBody . unlocated

instance HasDeclarationBody Frontend.Declaration' (Located Frontend.DeclarationBody) where
    declarationBody = Frontend.declaration'Body
    unlocatedDeclarationBody = declarationBody . unlocated

instance HasDeclarationBody Desugared.Declaration (Located Desugared.DeclarationBody) where
    declarationBody = Desugared._Declaration . unlocated . declarationBody
    unlocatedDeclarationBody = declarationBody . unlocated

instance HasDeclarationBody Desugared.Declaration' (Located Desugared.DeclarationBody) where
    declarationBody = Desugared.declaration'Body
    unlocatedDeclarationBody = declarationBody . unlocated

instance HasDeclarationBody Renamed.Declaration (Located Renamed.DeclarationBody) where
    declarationBody = Renamed._Declaration . unlocated . declarationBody
    unlocatedDeclarationBody = declarationBody . unlocated

instance HasDeclarationBody Renamed.Declaration' (Located Renamed.DeclarationBody) where
    declarationBody = Renamed.declaration'Body
    unlocatedDeclarationBody = declarationBody . unlocated

instance HasDeclarationBody Typed.Declaration (Located Typed.DeclarationBody) where
    declarationBody = Typed._Declaration . unlocated . declarationBody
    unlocatedDeclarationBody = declarationBody . unlocated

instance HasDeclarationBody Typed.Declaration' (Located Typed.DeclarationBody) where
    declarationBody = Typed.declaration'Body
    unlocatedDeclarationBody = declarationBody . unlocated

class HasDeclarationBody' h db | h -> db where
    declarationBody' :: Lens' h db
    unlocatedDeclarationBody' :: Lens' h (Unlocate db)

instance HasDeclarationBody' h db => HasDeclarationBody' (Located h) db where
    declarationBody' = unlocated . declarationBody' @h @db
    unlocatedDeclarationBody' = unlocated . unlocatedDeclarationBody' @h @db

instance HasDeclarationBody' Frontend.Declaration (Located Frontend.DeclarationBody') where
    declarationBody' = declarationBody . unlocated . Frontend._DeclarationBody
    unlocatedDeclarationBody' = declarationBody' . unlocated

instance HasDeclarationBody' Frontend.Declaration' (Located Frontend.DeclarationBody') where
    declarationBody' = declarationBody . unlocated . Frontend._DeclarationBody
    unlocatedDeclarationBody' = declarationBody' . unlocated

instance HasDeclarationBody' Desugared.Declaration (Located Desugared.DeclarationBody') where
    declarationBody' = declarationBody . unlocated . Desugared._DeclarationBody
    unlocatedDeclarationBody' = declarationBody' . unlocated

instance HasDeclarationBody' Desugared.Declaration' (Located Desugared.DeclarationBody') where
    declarationBody' = declarationBody . unlocated . Desugared._DeclarationBody
    unlocatedDeclarationBody' = declarationBody' . unlocated

instance HasDeclarationBody' Renamed.Declaration (Located Renamed.DeclarationBody') where
    declarationBody' = declarationBody . unlocated . Renamed._DeclarationBody
    unlocatedDeclarationBody' = declarationBody' . unlocated

instance HasDeclarationBody' Renamed.Declaration' (Located Renamed.DeclarationBody') where
    declarationBody' = declarationBody . unlocated . Renamed._DeclarationBody
    unlocatedDeclarationBody' = declarationBody' . unlocated

instance HasDeclarationBody' Typed.Declaration (Located Typed.DeclarationBody') where
    declarationBody' = declarationBody . unlocated . Typed._DeclarationBody
    unlocatedDeclarationBody' = declarationBody' . unlocated

instance HasDeclarationBody' Typed.Declaration' (Located Typed.DeclarationBody') where
    declarationBody' = declarationBody . unlocated . Typed._DeclarationBody
    unlocatedDeclarationBody' = declarationBody' . unlocated
