module Elara.Rename.Error where

import Data.Generics.Product
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text.Metrics (levenshtein)
import Elara.AST.Instances ()
import Elara.AST.Location
import Elara.AST.Module qualified as NewModule
import Elara.AST.Name
import Elara.AST.Phases.Desugared qualified as NewD
import Elara.AST.Region
import Elara.AST.Types qualified as New
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Error.Diagnose (toDiagnoseReports)
import Elara.Rename.Imports (isImportedBy)
import Error.Diagnose hiding (Hint, Note)

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
    | {- | A name was used that isn't in scope, either because it doesn't exist or because it wasn't imported.
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
    | -- | The current module couldn't be determined (internal compiler error, should not occur in normal use)
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

instance Exception RenameError

instance ElaraDiagnostic RenameError where
    diagnosticMessage (ModuleNameMismatch _ _) = "Module name mismatch"
    diagnosticMessage (UnknownModule mn) = "Unknown module: " <> pretty mn
    diagnosticMessage (QualifiedInWrongModule actual expected) = "Qualified name belongs to module " <> pretty actual <> " but was used inside module " <> pretty expected
    diagnosticMessage (NonExistentModuleDeclaration m n) = pretty (n ^. unlocated) <+> "does not exist in module" <+> pretty m
    diagnosticMessage (UnknownTypeVariable n) = "Unknown type variable: " <> pretty n
    diagnosticMessage (UnknownName n _ _) = "Unknown name: " <> pretty n
    diagnosticMessage (BlockEndsWithLet _ _) = "Block ends with a let binding"
    diagnosticMessage (AmbiguousVarName n _) = "Ambiguous variable name: " <> pretty n
    diagnosticMessage (AmbiguousTypeName n _) = "Ambiguous type name: " <> pretty n
    diagnosticMessage UnknownCurrentModule = "Could not determine the current module (internal error)"
    diagnosticMessage (RecursiveTypeAlias n _) = "Recursive type alias: " <> pretty n

    diagnosticCode (UnknownModule _) = Just Codes.unknownModule
    diagnosticCode (QualifiedInWrongModule _ _) = Just Codes.qualifiedWithWrongModule
    diagnosticCode (NonExistentModuleDeclaration _ _) = Just Codes.nonExistentModuleDeclaration
    diagnosticCode (UnknownTypeVariable _) = Just Codes.unknownTypeVariable
    diagnosticCode (UnknownName{}) = Just Codes.unknownName
    diagnosticCode (BlockEndsWithLet _ _) = Just Codes.blockEndsWithLet
    diagnosticCode (AmbiguousVarName _ _) = Just Codes.ambiguousName
    diagnosticCode (AmbiguousTypeName _ _) = Just Codes.ambiguousName
    diagnosticCode UnknownCurrentModule = Just Codes.unknownCurrentModule
    diagnosticCode (RecursiveTypeAlias _ _) = Just Codes.recursiveTypeAlias
    diagnosticCode _ = Nothing

    diagnosticMarkers (ModuleNameMismatch expected actual) =
        let isImplicitMain =
                case actual of
                    Located (RealSourceRegion r) (ModuleName ("Main" :| [])) -> r ^. startPos == r ^. endPos
                    Located (GeneratedRegion _) (ModuleName ("Main" :| [])) -> True
                    _ -> False
            message =
                if isImplicitMain
                    then "Module implicitly declared as Main"
                    else "Module declared as " <> pretty (actual ^. unlocated)
         in [ ElaraMarker (actual ^. sourceRegion) PrimaryMarker message
            , ElaraMarker (expected ^. sourceRegion) SecondaryMarker ("Imported as " <> pretty (expected ^. unlocated))
            ]
    diagnosticMarkers (NonExistentModuleDeclaration _ n) = [ElaraMarker (n ^. sourceRegion) PrimaryMarker "referenced here"]
    diagnosticMarkers (UnknownName n _ _) = [ElaraMarker (n ^. sourceRegion) PrimaryMarker "referenced here"]
    diagnosticMarkers (BlockEndsWithLet (New.Expr (ExprLoc loc) _ _) decl) =
        ElaraMarker loc PrimaryMarker "this let has no body"
            : maybe [] (\(New.DeclarationBody dloc _) -> [ElaraMarker (unwrapLoc dloc) SecondaryMarker "inside this declaration"]) decl
    diagnosticMarkers (AmbiguousVarName n _) = [ElaraMarker (n ^. sourceRegion) PrimaryMarker "referenced here"]
    diagnosticMarkers (AmbiguousTypeName n _) = [ElaraMarker (n ^. sourceRegion) PrimaryMarker "referenced here"]
    diagnosticMarkers (RecursiveTypeAlias n usePoint) =
        [ ElaraMarker (n ^. sourceRegion) SecondaryMarker "alias defined here"
        , ElaraMarker (usePoint ^. sourceRegion) PrimaryMarker "refers back to itself here"
        ]
    diagnosticMarkers _ = []

    diagnosticNotes (ModuleNameMismatch expected actual) =
        let actualName = actual ^. unlocated
            isImplicitMain =
                case actual of
                    Located (RealSourceRegion r) (ModuleName ("Main" :| [])) -> r ^. startPos == r ^. endPos
                    Located (GeneratedRegion _) (ModuleName ("Main" :| [])) -> True
                    _ -> False
            hint =
                if isImplicitMain
                    then Hint "You can define a module name with the `module` keyword at the top of the file."
                    else Hint "The module name must match the name used to import it."
         in [ Note $ "Expected module name: " <> pretty (expected ^. unlocated)
            , hint
            ]
    diagnosticNotes (UnknownModule _) = [Hint "Check that the module name is spelled correctly and that the file exists."]
    diagnosticNotes (QualifiedInWrongModule actual _) = [Hint $ "Remove the " <> pretty actual <> " qualifier, or move this definition into module " <> pretty actual]
    diagnosticNotes (NonExistentModuleDeclaration m _) = [Hint $ "Check the spelling, or consult the documentation for " <> pretty m]
    diagnosticNotes (UnknownTypeVariable _) = [Hint "Type variables must be bound in the enclosing type declaration or forall."]
    diagnosticNotes (UnknownName n m names) =
        let namesMap = Map.mapKeys toName names
            allNames = maybe [] toList (fmap toName <<$>> Map.lookup (n ^. unlocated) namesMap)
            namesThatMightveBeenIntendedButNotImported =
                case m of
                    Nothing -> []
                    Just m' -> case filter (not . isImportedBy m') allNames of
                        [] -> []
                        ns ->
                            [ Note $
                                vsep
                                    [ "This name is defined in the following modules, but none of them are imported:"
                                    , hsep (punctuate comma (ns ^.. each % _Ctor' @"Global" % unlocated % field' @"qualifier" % to pretty))
                                    , "Try importing one of the modules."
                                    ]
                            ]
            prettyVarRef n'@(Local{}) = pretty (toName $ view unlocated $ varRefVal n') <+> "(local variable)"
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
         in namesThatMightveBeenIntendedButNotImported <> possibleTypos
    diagnosticNotes (BlockEndsWithLet _ _) =
        [ Note "Blocks must end with an expression, not a let binding."
        , Hint "Perhaps you meant to use a let ... in construct?"
        ]
    diagnosticNotes (AmbiguousVarName n options) =
        [ Note $
            vsep
                [ "The name is ambiguous, and could refer to any of the following:"
                , listToText (pretty <$> toList options)
                ]
        , Hint "Try qualifying the name with the module name."
        , Hint "Try removing all but one of the imports causing the ambiguity."
        , Hint $ "Try excluding " <> pretty n <> " from the exposing list of all but one of the imports."
        ]
    diagnosticNotes (AmbiguousTypeName n options) =
        [ Note $
            vsep
                [ "The name is ambiguous, and could refer to any of the following:"
                , listToText (pretty <$> toList options)
                ]
        , Hint "Try qualifying the name with the module name."
        , Hint "Try removing all but one of the imports causing the ambiguity."
        , Hint $ "Try excluding " <> pretty n <> " from the exposing list of all but one of the imports."
        ]
    diagnosticNotes UnknownCurrentModule = [Note "This is a compiler bug. Please report it."]
    diagnosticNotes (RecursiveTypeAlias _ _) =
        [ Note "Type aliases cannot be recursive."
        , Hint "Define a data type instead: use `|` to create an ADT with a single constructor."
        ]

instance Pretty RenameError where
    pretty = diagnosticMessage
