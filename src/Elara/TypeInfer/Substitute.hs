module Elara.TypeInfer.Substitute (SubstitutableExpr (..)) where

import Relude.Extra (bimapF)

import Data.Map qualified as Map

import Elara.AST.Instances ()
import Elara.AST.Location
import Elara.AST.Name
import Elara.AST.Phases.Typed
import Elara.AST.Region
import Elara.AST.Types
import Elara.Data.Kind
import Elara.Data.Unique
import Elara.TypeInfer.Type

newtype SubstitutableExpr loc = SubstitutableExpr {getExpr :: Expr SourceRegion Typed}
    deriving (Eq, Ord, Show)

instance Substitutable SubstitutableExpr SourceRegion where
    substitute tv t (SubstitutableExpr expr) =
        let Expr loc meta e' = expr
            meta' = substitute tv t meta
            e'' = substituteExpr' tv t e'
         in SubstitutableExpr (Expr loc meta' e'')

    -- overridden default for performance, avoids repeated traversals of the expression tree
    substituteAll s (SubstitutableExpr expr) =
        let Expr loc meta e' = expr
            meta' = substituteAll s meta
            e'' = substituteAllExpr' s e'
         in SubstitutableExpr (Expr loc meta' e'')

-- | Substitute a type variable in an expression body
substituteExpr' :: UniqueTyVar -> Monotype SourceRegion -> TypedExpr' -> TypedExpr'
substituteExpr' tv t = \case
    EInt i -> EInt i
    EFloat f -> EFloat f
    EString s -> EString s
    EChar c -> EChar c
    EUnit -> EUnit
    EVar vt v -> EVar (substitute tv t vt) v
    ECon ext v -> ECon ext v
    ELam ext binder body -> ELam ext (substituteLambdaBinder tv t binder) (substituteExpr tv t body)
    EApp ext f x -> EApp ext (substituteExpr tv t f) (substituteExpr tv t x)
    ETyApp e ty -> ETyApp (substituteExpr tv t e) (substituteAstType tv t ty)
    EIf c th el -> EIf (substituteExpr tv t c) (substituteExpr tv t th) (substituteExpr tv t el)
    EMatch e cases -> EMatch (substituteExpr tv t e) (bimapF (substitutePattern tv t) (substituteExpr tv t) cases)
    ELetIn ext binder e1 e2 -> ELetIn ext binder (substituteExpr tv t e1) (substituteExpr tv t e2)
    ELet ext binder e1 -> ELet ext binder (substituteExpr tv t e1)
    EBlock exprs -> EBlock (fmap (substituteExpr tv t) exprs)
    EAnn e ty -> EAnn (substituteExpr tv t e) (substituteAstType tv t ty)
    EExtension v -> absurd v

substituteAllExpr' :: Substitution SourceRegion -> TypedExpr' -> TypedExpr'
substituteAllExpr' s = \case
    EInt i -> EInt i
    EFloat f -> EFloat f
    EString s' -> EString s'
    EChar c -> EChar c
    EUnit -> EUnit
    EVar vt v -> EVar (substituteAll s vt) v
    ECon ext v -> ECon ext v
    ELam ext binder body -> ELam ext (substituteAllLambdaBinder s binder) (substituteAllExpr s body)
    EApp ext f x -> EApp ext (substituteAllExpr s f) (substituteAllExpr s x)
    ETyApp e ty -> ETyApp (substituteAllExpr s e) (substituteAllAstType s ty)
    EIf c th el -> EIf (substituteAllExpr s c) (substituteAllExpr s th) (substituteAllExpr s el)
    EMatch e cases -> EMatch (substituteAllExpr s e) (bimapF (substituteAllPattern s) (substituteAllExpr s) cases)
    ELetIn ext binder e1 e2 -> ELetIn ext binder (substituteAllExpr s e1) (substituteAllExpr s e2)
    ELet ext binder e1 -> ELet ext binder (substituteAllExpr s e1)
    EBlock exprs -> EBlock (fmap (substituteAllExpr s) exprs)
    EAnn e ty -> EAnn (substituteAllExpr s e) (substituteAllAstType s ty)
    EExtension v -> absurd v

substituteExpr :: UniqueTyVar -> Monotype SourceRegion -> TypedExpr -> TypedExpr
substituteExpr tv t (Expr loc meta e') = Expr loc (substitute tv t meta) (substituteExpr' tv t e')

substituteAllExpr :: Substitution SourceRegion -> TypedExpr -> TypedExpr
substituteAllExpr s (Expr loc meta e') = Expr loc (substituteAll s meta) (substituteAllExpr' s e')

substitutePattern :: UniqueTyVar -> Monotype SourceRegion -> TypedPattern -> TypedPattern
substitutePattern tv t (Pattern loc meta p') = Pattern loc (substitute tv t meta) (substitutePattern' tv t p')

substituteAllPattern :: Substitution SourceRegion -> TypedPattern -> TypedPattern
substituteAllPattern s (Pattern loc meta p') = Pattern loc (substituteAll s meta) (substituteAllPattern' s p')

substitutePattern' :: UniqueTyVar -> Monotype SourceRegion -> TypedPattern' -> TypedPattern'
substitutePattern' tv t = \case
    PVar v -> PVar v
    PCon c ps -> PCon c (fmap (substitutePattern tv t) ps)
    PWildcard -> PWildcard
    PInt i -> PInt i
    PFloat f -> PFloat f
    PString s -> PString s
    PChar c -> PChar c
    PUnit -> PUnit
    PExtension v -> absurd v

substituteAllPattern' :: Substitution SourceRegion -> TypedPattern' -> TypedPattern'
substituteAllPattern' s = \case
    PVar v -> PVar v
    PCon c ps -> PCon c (fmap (substituteAllPattern s) ps)
    PWildcard -> PWildcard
    PInt i -> PInt i
    PFloat f -> PFloat f
    PString s -> PString s
    PChar c -> PChar c
    PUnit -> PUnit
    PExtension v -> absurd v

substituteLambdaBinder :: UniqueTyVar -> Monotype SourceRegion -> TypedLambdaParam (Unique VarName) SourceRegion Typed -> TypedLambdaParam (Unique VarName) SourceRegion Typed
substituteLambdaBinder tv t (TypedLambdaParam v meta) = TypedLambdaParam v (substitute tv t meta)

substituteAllLambdaBinder :: Substitution SourceRegion -> TypedLambdaParam (Unique VarName) SourceRegion Typed -> TypedLambdaParam (Unique VarName) SourceRegion Typed
substituteAllLambdaBinder s (TypedLambdaParam v meta) = TypedLambdaParam v (substituteAll s meta)

-- | Substitute a type variable in an AST type (Type SourceRegion Typed)
substituteAstType :: UniqueTyVar -> Monotype SourceRegion -> TypedType -> TypedType
substituteAstType tv t (Type loc kind t') = case t' of
    TVar (TaggedLocate _ tv')
        | tv == tv' -> monotypeToAstType (unwrapLoc loc) kind t
    _ -> Type loc kind (substituteAstType' tv t t')

substituteAstType' :: UniqueTyVar -> Monotype SourceRegion -> TypedType' -> TypedType'
substituteAstType' tv t = \case
    TVar v -> TVar v -- not matching (matching case handled in substituteAstType)
    TFun a b -> TFun (substituteAstType tv t a) (substituteAstType tv t b)
    TUnit -> TUnit
    TApp a b -> TApp (substituteAstType tv t a) (substituteAstType tv t b)
    TUserDefined n -> TUserDefined n
    TRecord fields -> TRecord (substituteAstType tv t <<$>> fields)
    TList a -> TList (substituteAstType tv t a)
    TExtension v -> absurd v

-- | Bulk substitute in an AST type
substituteAllAstType :: Substitution SourceRegion -> TypedType -> TypedType
substituteAllAstType (Substitution s) ty = foldl' (\acc (tv, t) -> substituteAstType tv t acc) ty (Map.toList s)

-- | Convert a Monotype to an AST Type for the Typed phase
monotypeToAstType :: SourceRegion -> ElaraKind -> Monotype SourceRegion -> TypedType
monotypeToAstType loc kind = \case
    TypeVar _ tv ->
        let typeLoc = wrap @TypeNode loc
         in Type typeLoc kind (TVar (TaggedLocate typeLoc (typeVarToUniqueTyVar tv)))
    Function _ a b ->
        let typeLoc = wrap @TypeNode loc
         in Type typeLoc kind (TFun (monotypeToAstType loc kind a) (monotypeToAstType loc kind b))
    TypeConstructor _ qn args -> case args of
        [] ->
            let typeLoc = wrap @TypeNode loc
             in Type typeLoc kind (TUserDefined (TaggedLocate typeLoc qn))
        _ ->
            let typeLoc = wrap @TypeNode loc
             in foldl' (\acc arg -> Type typeLoc kind (TApp acc (monotypeToAstType loc kind arg))) (Type typeLoc kind (TUserDefined (TaggedLocate typeLoc qn))) args

typeVarToUniqueTyVar :: TypeVariable -> UniqueTyVar
typeVarToUniqueTyVar (UnificationVar tv) = tv
typeVarToUniqueTyVar (SkolemVar tv) = tv
