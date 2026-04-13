{- | User-friendly type rendering for error messages.
Maps internal primitive names to user-friendly names.
-}
module Elara.TypeInfer.Render (
    renderTypeName,
    renderMonotype,
    renderType,
    renderPolytype,
) where

import Elara.AST.Name (Qualified (..), TypeName (..))
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Prim (KnownTypeInfo (..), knownTypeInfo, lookupByQualifiedTypeName)
import Elara.TypeInfer.Type (Monotype (..), Polytype (..), Type (..), TypeVariable (..))

-- | Map internal primitive names to user-friendly names
renderTypeName :: Qualified TypeName -> Doc AnsiStyle
renderTypeName qn = case lookupByQualifiedTypeName qn of
    Just kt -> Style.typeName (pretty (knownUserFacingName (knownTypeInfo kt)))
    Nothing -> Style.typeName (pretty (_qualifiedName qn))

-- | Render a monotype with user-friendly names
renderMonotype :: Monotype loc -> Doc AnsiStyle
renderMonotype = \case
    TypeVar _ tv -> renderTypeVariable tv
    TypeConstructor _ name args ->
        let renderedName = renderTypeName name
         in case args of
                [] -> renderedName
                _ -> renderedName <+> hsep (renderMonotypeParens <$> args)
    Function _ from to ->
        renderMonotypeArrow from <+> Style.operator "->" <+> renderMonotype to

-- | Render a monotype, wrapping function types in parentheses
renderMonotypeParens :: Monotype loc -> Doc AnsiStyle
renderMonotypeParens m@Function{} = parens (renderMonotype m)
renderMonotypeParens m@(TypeConstructor _ _ (_ : _)) = parens (renderMonotype m)
renderMonotypeParens m = renderMonotype m

-- | Render a monotype on the left of an arrow, wrapping if needed
renderMonotypeArrow :: Monotype loc -> Doc AnsiStyle
renderMonotypeArrow m@Function{} = parens (renderMonotype m)
renderMonotypeArrow m = renderMonotype m

-- | Render a type variable
renderTypeVariable :: TypeVariable -> Doc AnsiStyle
renderTypeVariable (UnificationVar tv) = Style.varName (pretty tv)
renderTypeVariable (SkolemVar tv) = Style.varName ("#" <> pretty tv)

-- | Render a polytype with user-friendly names
renderPolytype :: Polytype loc -> Doc AnsiStyle
renderPolytype (Forall _ tvs _ body) = case tvs of
    [] -> renderMonotype body
    _ -> "forall" <+> hsep (pretty <$> tvs) <> "." <+> renderMonotype body

-- | Render a type with user-friendly names
renderType :: Type loc -> Doc AnsiStyle
renderType (Lifted m) = renderMonotype m
renderType (Polytype p) = renderPolytype p
