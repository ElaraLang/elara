module Elara.Rename.Error where

import Data.Generics.Product
import Data.Generics.Wrapped (_Unwrapped)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text.Metrics (levenshtein)
import Elara.AST.Desugared
import Elara.AST.Generic.Instances ()
import Elara.AST.Module
import Elara.AST.Name
import Elara.AST.Region
import Elara.AST.Select
import Elara.AST.VarRef
import Elara.Data.Pretty

import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Rename.Imports (isImportedBy)
import Error.Diagnose

data RenameError
    = UnknownModule ModuleName
    | QualifiedInWrongModule ModuleName ModuleName
    | NonExistentModuleDeclaration ModuleName (Located Name)
    | UnknownTypeVariable LowerAlphaName
    | forall name.
        (ToName name, Show name) =>
      UnknownName
        -- | The name that was unknown
        (Located Name)
        -- | The module we're renaming that the unknown name was referenced in
        (Maybe (Module Desugared))
        -- | All known names
        (Map name (NonEmpty (VarRef name)))
    | AmbiguousVarName (Located Name) (NonEmpty (VarRef VarName))
    | AmbiguousTypeName (Located Name) (NonEmpty (VarRef TypeName))
    | NativeDefUnsupported (Located DesugaredDeclaration')
    | BlockEndsWithLet DesugaredExpr (Maybe DesugaredDeclarationBody)
    | UnknownCurrentModule

deriving instance Show RenameError

instance ReportableError RenameError where
    report (UnknownModule mn) =
        writeReport $
            Err
                (Just Codes.unknownModule)
                ("Unknown module: " <> show mn)
                []
                []
    report (QualifiedInWrongModule m1 m2) =
        writeReport $
            Err
                Nothing
                ("Qualified name in wrong module:" <+> show m1 <+> "in" <+> show m2)
                []
                []
    report (NonExistentModuleDeclaration m n) =
        let nPos = sourceRegionToDiagnosePosition (n ^. sourceRegion)
         in writeReport $
                Err
                    (Just Codes.nonExistentModuleDeclaration)
                    ("Element" <+> n ^. unlocated % to pretty <+> "does not exist in in module" <+> pretty m)
                    [(nPos, This "referenced here")]
                    []
    report (UnknownTypeVariable n) =
        writeReport $
            Err
                Nothing
                ("Unknown type variable: " <> pretty n)
                []
                []
    report (UnknownName n m names) = do
        let nameKind = case n of
                Located _ (NVarName (NormalVarName _)) -> "variable"
                Located _ (NVarName (OperatorVarName _)) -> "operator"
                Located _ (NTypeName _) -> "type"
        -- search all names to suggest imports / similar names
        let namesMap = Map.mapKeys toName names
        let allNames = maybe [] toList (fmap toName <<$>> Map.lookup (n ^. unlocated) namesMap)
        let namesThatMightveBeenIntendedButNotImported =
                case m of
                    Nothing -> []
                    Just m -> case filter (not . isImportedBy m) allNames of
                        [] -> []
                        ns ->
                            [ Hint $
                                vsep
                                    [ "This name is defined in the following modules, but none of them are imported:"
                                    , hsep (punctuate comma (ns ^.. each % _Ctor' @"Global" % unlocated % field' @"qualifier" % to pretty))
                                    , "Try importing one of the modules."
                                    ]
                            ]
        let prettyVarRef n@(Local{}) = pretty (toName $ view unlocated $ varRefVal n) <+> "(local variable)"
            prettyVarRef (Global (Located _ (Qualified n m))) = pretty (toName n) <+> "(imported from" <+> pretty m <> ")"
            possibleTypos = case m of
                Nothing -> []
                Just m ->
                    let intendedText = nameText n
                        isTypo name = levenshtein (nameText name) intendedText < 3
                        typos =
                            Map.filterWithKey
                                (\k _ -> isTypo k)
                                (NonEmpty.filter (\x -> isImportedBy m (toName <$> x)) <$> namesMap)
                     in case join (Map.elems typos) of
                            [] -> []
                            ts ->
                                [ Hint $
                                    vsep
                                        [ "You may have meant one of:"
                                        , listToText (prettyVarRef <$> ts)
                                        ]
                                ]

        writeReport $
            Err
                (Just Codes.unknownName)
                ("Unknown" <+> nameKind <+> "name: " <> pretty n)
                [(n ^. sourceRegion % to sourceRegionToDiagnosePosition, This "referenced here")]
                (namesThatMightveBeenIntendedButNotImported <> possibleTypos)
    report (NativeDefUnsupported _) =
        writeReport $
            Err
                Nothing
                "Native definitions are not supported"
                []
                []
    report (BlockEndsWithLet l decl) =
        writeReport $
            Err
                Nothing
                "Block ends with let"
                ( (l ^. _Unwrapped % _1 % sourceRegion % to sourceRegionToDiagnosePosition, This "let occurs here")
                    : maybe [] (\d -> [(d ^. _Unwrapped % sourceRegion % to sourceRegionToDiagnosePosition, Where "as part of this declaration")]) decl
                )
                [ Note "Blocks cannot end with let statements, as they are not expressions."
                , Hint "Perhaps you meant to use a let ... in construct?"
                ]
    report (AmbiguousTypeName n options) =
        writeReport $
            Err
                Nothing
                ("Ambiguous type name: " <> pretty n)
                [(n ^. sourceRegion % to sourceRegionToDiagnosePosition, This "referenced here")]
                [ Note $
                    vsep
                        [ "The name is ambiguous, and could refer to any of the following:"
                        , listToText (pretty <$> toList options)
                        ]
                , Hint "Try qualifying the name with the module name"
                , Hint
                    "Try removing all but one of the imports that is causing the ambiguity"
                , Hint $
                    "Try excluding " <> pretty n <> " from the exposing list of all but one of the imports"
                ]
    report (AmbiguousVarName n options) =
        writeReport $
            Err
                Nothing
                ("Ambiguous variable name: " <> pretty n)
                [ (n ^. sourceRegion % to sourceRegionToDiagnosePosition, This "referenced here")
                ]
                [ Note $
                    vsep
                        [ "The name is ambiguous, and could refer to any of the following:"
                        , listToText (pretty <$> toList options)
                        ]
                , Hint "Try qualifying the name with the module name"
                , Hint
                    "Try removing all but one of the imports that is causing the ambiguity"
                , Hint $
                    "Try excluding " <> pretty n <> " from the exposing list of all but one of the imports"
                ]
    report UnknownCurrentModule =
        writeReport $
            Err
                Nothing
                "Unknown current module (internal error!)"
                []
                []
