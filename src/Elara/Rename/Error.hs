module Elara.Rename.Error where

import Data.Generics.Product
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text.Metrics (levenshtein)
import Elara.AST.Instances ()
import Elara.AST.Module qualified as NewModule
import Elara.AST.Name
import Elara.AST.Phases.Desugared qualified as NewD
import Elara.AST.Region
import Elara.AST.Types qualified as New
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Rename.Imports (isImportedBy)
import Error.Diagnose

data RenameError
    = -- | A requested module was not found
      UnknownModule ModuleName
    | {- | A qualified name was used in a declaration but the qualifier doesn't match the current module's name
      For example, @module A; B.x = 1@ would trigger this error, because the qualified name @B.x@ is in the wrong module
      -}
      QualifiedInWrongModule ModuleName ModuleName
    | {- | A name was referenced with an explicit module qualifier or in an explicit import\/hiding list,
      but that name doesn't exist in (or isn't exported by) the named module.
      For example, @import Foo (nonExistent)@ or a use of @Foo.nonExistent@ where @Foo@ has no such export
      -}
      NonExistentModuleDeclaration ModuleName (Located Name)
    | {- | A type variable was referenced that isn't in scope.
      For example, @type Foo = a@ would trigger this, because @a@ is not bound anywhere
      -}
      UnknownTypeVariable LowerAlphaName
    | {- | A name was used that isn't in scope — neither defined in the current module nor imported.
      Carries the unknown name, the module being renamed (for import suggestions), and the full known-names map (for typo hints)
      -}
      forall name.
        (ToName name, Show name) =>
      UnknownName
        -- | The name that was unknown
        (Located Name)
        -- | The module we're renaming that the unknown name was referenced in
        (Maybe (NewModule.Module SourceRegion NewD.Desugared))
        -- | All known names
        (Map name (NonEmpty (VarRef name)))
    | {- | A variable name is imported by more than one unqualified import and the use site is ambiguous.
      For example, importing both @Data.Map (lookup)@ and @Data.List (lookup)@ and then using @lookup@ unqualified
      -}
      AmbiguousVarName (Located Name) (NonEmpty (VarRef VarName))
    | -- | Same as 'AmbiguousVarName' but for type names
      AmbiguousTypeName (Located Name) (NonEmpty (VarRef TypeName))
    | -- | A block ends with a @let@ binding rather than an expression.
      BlockEndsWithLet
        -- | The @let@ expression that ends the block
        (New.Expr SourceRegion NewD.Desugared)
        -- | The surrounding declaration, if any, for better error location
        (Maybe (New.DeclarationBody SourceRegion NewD.Desugared))
    | -- | The current module couldn't be determined (internal compiler error — should not occur in normal use)
      UnknownCurrentModule
    | {- | A type alias directly or indirectly refers to itself, which is forbidden.
      The first argument is the alias being defined; the second is the use site where the cycle is detected
      -}
      RecursiveTypeAlias (Located (Qualified TypeName)) (Located (Qualified TypeName))
    | {- | The module name in the source file doesn't match the name it was requested under.
      The first argument is the expected name (from the import or query); the second is what was found in the file
      -}
      ModuleNameMismatch (Located ModuleName) (Located ModuleName)

deriving instance Show RenameError

instance ReportableError RenameError where
    report (ModuleNameMismatch expected actual) =
        let actualName = actual ^. unlocated
            isImplicitMain =
                case actual of
                    Located (RealSourceRegion r) (ModuleName ("Main" :| [])) -> r ^. startPos == r ^. endPos
                    Located (GeneratedRegion _) (ModuleName ("Main" :| [])) -> True
                    _ -> False
            message =
                if isImplicitMain
                    then "Module implicitly declared as Main"
                    else "Module declared as " <> pretty actualName
            hint =
                if isImplicitMain
                    then Hint "You can define a module name with the `module` keyword at the top of the file."
                    else Hint "The module name must match the name used to import it."
         in writeReport $
                Err
                    Nothing
                    "Module name mismatch"
                    [ (actual ^. sourceRegion % to sourceRegionToDiagnosePosition, This message)
                    , (expected ^. sourceRegion % to sourceRegionToDiagnosePosition, Where $ "Imported as " <> pretty (expected ^. unlocated))
                    ]
                    [ Note $ "Expected module name: " <> pretty (expected ^. unlocated)
                    , hint
                    ]
    report (UnknownModule mn) =
        writeReport $
            Err
                (Just Codes.unknownModule)
                ("Unknown module: " <> pretty mn)
                []
                [Hint "Check that the module name is spelled correctly and that the file exists."]
    report (QualifiedInWrongModule actual expected) =
        writeReport $
            Err
                (Just Codes.qualifiedWithWrongModule)
                ("Qualified name belongs to module " <> pretty actual <> " but was used inside module " <> pretty expected)
                []
                [Hint $ "Remove the " <> pretty actual <> " qualifier, or move this definition into module " <> pretty actual]
    report (NonExistentModuleDeclaration m n) =
        let nPos = sourceRegionToDiagnosePosition (n ^. sourceRegion)
         in writeReport $
                Err
                    (Just Codes.nonExistentModuleDeclaration)
                    (pretty (n ^. unlocated) <+> "does not exist in module" <+> pretty m)
                    [(nPos, This "referenced here")]
                    [Hint $ "Check the spelling, or consult the documentation for " <> pretty m]
    report (UnknownTypeVariable n) =
        writeReport $
            Err
                (Just Codes.unknownTypeVariable)
                ("Unknown type variable: " <> pretty n)
                []
                [Hint "Type variables must be bound in the enclosing type declaration or forall."]
    report (UnknownName n m names) = do
        let nameKind = case n of
                Located _ (NameValue _) -> "variable"
                Located _ (NameOp _) -> "operator"
                Located _ (NameType _) -> "type"
        -- search all names to suggest imports / similar names
        let namesMap = Map.mapKeys toName names
        let allNames = maybe [] toList (fmap toName <<$>> Map.lookup (n ^. unlocated) namesMap)
        let namesThatMightveBeenIntendedButNotImported =
                case m of
                    Nothing -> []
                    Just m' -> case filter (not . isImportedBy m') allNames of
                        [] -> []
                        ns ->
                            [ Hint $
                                vsep
                                    [ "This name is defined in the following modules, but none of them are imported:"
                                    , hsep (punctuate comma (ns ^.. each % _Ctor' @"Global" % unlocated % field' @"qualifier" % to pretty))
                                    , "Try importing one of the modules."
                                    ]
                            ]
        let prettyVarRef n'@(Local{}) = pretty (toName $ view unlocated $ varRefVal n') <+> "(local variable)"
            prettyVarRef (Global (Located _ (Qualified n' m'))) = pretty (toName n') <+> "(imported from" <+> pretty m' <> ")"
            possibleTypos = case m of
                Nothing -> []
                Just m' ->
                    let intendedText = nameText n
                        isTypo name = levenshtein (nameText name) intendedText < 3
                        typos =
                            Map.filterWithKey
                                (\k _ -> isTypo k)
                                (NonEmpty.filter (\x -> isImportedBy m' (toName <$> x)) <$> namesMap)
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
    report (BlockEndsWithLet (New.Expr loc _ _) decl) =
        writeReport $
            Err
                (Just Codes.blockEndsWithLet)
                "Block ends with a let binding"
                ( (sourceRegionToDiagnosePosition loc, This "this let has no body")
                    : maybe [] (\(New.DeclarationBody dloc _) -> [(sourceRegionToDiagnosePosition dloc, Where "inside this declaration")]) decl
                )
                [ Note "Blocks must end with an expression, not a let binding."
                , Hint "Perhaps you meant to use a let ... in construct?"
                ]
    report (AmbiguousVarName n options) =
        writeReport $
            Err
                (Just Codes.ambiguousName)
                ("Ambiguous variable name: " <> pretty n)
                [(n ^. sourceRegion % to sourceRegionToDiagnosePosition, This "referenced here")]
                [ Note $
                    vsep
                        [ "The name is ambiguous, and could refer to any of the following:"
                        , listToText (pretty <$> toList options)
                        ]
                , Hint "Try qualifying the name with the module name."
                , Hint "Try removing all but one of the imports causing the ambiguity."
                , Hint $ "Try excluding " <> pretty n <> " from the exposing list of all but one of the imports."
                ]
    report (AmbiguousTypeName n options) =
        writeReport $
            Err
                (Just Codes.ambiguousName)
                ("Ambiguous type name: " <> pretty n)
                [(n ^. sourceRegion % to sourceRegionToDiagnosePosition, This "referenced here")]
                [ Note $
                    vsep
                        [ "The name is ambiguous, and could refer to any of the following:"
                        , listToText (pretty <$> toList options)
                        ]
                , Hint "Try qualifying the name with the module name."
                , Hint "Try removing all but one of the imports causing the ambiguity."
                , Hint $ "Try excluding " <> pretty n <> " from the exposing list of all but one of the imports."
                ]
    report UnknownCurrentModule =
        writeReport $
            Err
                (Just Codes.unknownCurrentModule)
                "Could not determine the current module (internal error)"
                []
                [Note "This is a compiler bug. Please report it."]
    report (RecursiveTypeAlias n usePoint) =
        writeReport $
            Err
                (Just Codes.recursiveTypeAlias)
                ("Recursive type alias: " <> pretty n)
                [ (n ^. sourceRegion % to sourceRegionToDiagnosePosition, Where "alias defined here")
                , (usePoint ^. sourceRegion % to sourceRegionToDiagnosePosition, This "refers back to itself here")
                ]
                [ Note "Type aliases cannot be recursive."
                , Hint "Define a data type instead: use `|` to create an ADT with a single constructor."
                ]
