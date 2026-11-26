{- | User-friendly type rendering for error messages.
Maps internal primitive names to user-friendly names.
-}
module Elara.TypeInfer.Render (
    renderTypeName,
    renderMonotype,
)
where

import Elara.AST.Name (Qualified (..), TypeName (..))
import Elara.Data.Pretty
import Elara.Data.Unique (Unique (..))
import Elara.Prim (KnownTypeInfo (..), knownTypeInfo, lookupByQualifiedTypeName)
import Elara.TypeInfer.Type (Monotype (..), TypeVariable (..))

import Elara.Data.Pretty.Styles qualified as Style

-- | Map internal primitive names to user-friendly names
renderTypeName :: Qualified TypeName -> Doc Ann
renderTypeName qn = case lookupByQualifiedTypeName qn of
    Just kt -> Style.typeName (pretty (knownUserFacingName (knownTypeInfo kt)))
    Nothing -> Style.typeName (pretty (_qualifiedName qn))

-- | Render a monotype with user-friendly names
renderMonotype :: Monotype loc -> Doc Ann
renderMonotype = \case
    TypeVar _ tv args ->
        renderTypeVariable tv <> case args of
            [] -> mempty
            _ -> " " <> hsep (renderMonotypeParens <$> args)
    TypeConstructor _ name args ->
        let renderedName = renderTypeName name
         in case args of
                [] -> renderedName
                _ -> renderedName <+> hsep (renderMonotypeParens <$> args)
    Function _ from to ->
        renderMonotypeArrow from <+> Style.operator "->" <+> renderMonotype to

-- | Render a monotype, wrapping function types in parentheses
renderMonotypeParens :: Monotype loc -> Doc Ann
renderMonotypeParens m@Function{} = parens (renderMonotype m)
renderMonotypeParens m@(TypeConstructor _ _ (_ : _)) = parens (renderMonotype m)
renderMonotypeParens m = renderMonotype m

-- | Render a monotype on the left of an arrow, wrapping if needed
renderMonotypeArrow :: Monotype loc -> Doc Ann
renderMonotypeArrow m@Function{} = parens (renderMonotype m)
renderMonotypeArrow m = renderMonotype m

-- | Render a type variable
renderTypeVariable :: TypeVariable -> Doc Ann
renderTypeVariable (UnificationVar (Unique (Just name) _)) = Style.varName (pretty name)
renderTypeVariable (UnificationVar (Unique Nothing i)) = Style.varName ("_" <> pretty i)
renderTypeVariable (SkolemVar (Unique (Just name) _)) = Style.varName ("#" <> pretty name)
renderTypeVariable (SkolemVar (Unique Nothing i)) = Style.varName ("#_" <> pretty i)
