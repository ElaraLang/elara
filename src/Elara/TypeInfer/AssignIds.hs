-- | Gives every sub-expression a unique ID and converts from @Elara.AST.Shunted@ to @Elara.AST.Typed@.
module Elara.TypeInfer.AssignIds where

import Elara.Data.Unique

import Control.Lens
import Elara.AST.Module (Module, traverseModule)
import Elara.AST.Region
import Elara.AST.Select (HasModuleName (..), HasName (name), PartialTyped, Shunted)
import Elara.AST.Shunted as In
import Elara.AST.Typed as Out
import Polysemy
import Relude.Extra (dup)

assignIdsToModule :: Member UniqueGen r => Module Shunted -> Sem r (Module PartialTyped)
assignIdsToModule = traverseModule assignIdsToDecl

assignIdsToDecl :: forall r. Member UniqueGen r => In.Declaration -> Sem r (Out.Declaration PartialType)
assignIdsToDecl (In.Declaration ld) =
  Out.Declaration
    <$> traverseOf
      unlocated
      ( \decl' -> do
          body' <- assignIdsToDeclarationBody (decl' ^. In.declaration'Body)
          pure (Out.Declaration' (decl' ^. moduleName) (decl' ^. name) body')
      )
      ld
 where
  assignIdsToDeclarationBody :: In.DeclarationBody -> Sem r (Out.DeclarationBody PartialType)
  assignIdsToDeclarationBody (In.DeclarationBody db) = Out.DeclarationBody <$> traverseOf unlocated assignIdsToDeclarationBody' db
  assignIdsToDeclarationBody' :: In.DeclarationBody' -> Sem r (Out.DeclarationBody' PartialType)
  assignIdsToDeclarationBody' (In.Value e ty) = do
    (e', _) <- assignIds e
    ty' <- traverse (traverse assignIdsToTypeAnnotation) ty
    pure (Out.Value e' ty')
  assignIdsToDeclarationBody' (In.TypeAlias ty) = Out.TypeAlias <$> traverse assignIdsToType ty

assignIdsToTypeAnnotation :: Member UniqueGen r => In.TypeAnnotation -> Sem r (Out.TypeAnnotation PartialType)
assignIdsToTypeAnnotation (In.TypeAnnotation n t) = Out.TypeAnnotation n <$> assignIdsToType t

assignIdsToType :: Member UniqueGen r => In.Type -> Sem r Out.PartialType
assignIdsToType t =
  Out.Partial <$> case t of
    In.TypeVar tv -> pure (Out.TypeVar $ Out.TyVar tv)
    In.FunctionType a b -> Out.FunctionType <$> assignIdsToType a <*> assignIdsToType b
    In.TypeConstructorApplication a b -> Out.TypeConstructorApplication <$> assignIdsToType a <*> assignIdsToType b
    In.UserDefinedType n -> pure (Out.UserDefinedType n)
    In.RecordType n -> Out.RecordType <$> traverseOf (traverse . _2) assignIdsToType n
    In.UnitType -> pure Out.UnitType

assignIds :: forall r. Member UniqueGen r => In.Expr -> Sem r (Out.Expr PartialType, PartialType)
assignIds (In.Expr le) = bimap Out.Expr snd . dup . unwrap <$> traverseOf unlocated assignIds' le
 where
  assignIds' :: In.Expr' -> Sem r (Out.Expr' PartialType, PartialType)
  assignIds' (In.Int i) = (Out.Int i,) <$> newId
  assignIds' (In.Float i) = (Out.Float i,) <$> newId
  assignIds' (In.String i) = (Out.String i,) <$> newId
  assignIds' (In.Char i) = (Out.Char i,) <$> newId
  assignIds' In.Unit = (Out.Unit,) <$> newId
  assignIds' (In.Var v) = (Out.Var (transformVarRef <$> v),) <$> newId
  assignIds' (In.Constructor v) = (Out.Constructor (transformVarRef <$> v),) <$> newId
  assignIds' (In.Let v e1) = do
    (e1', _) <- assignIds e1
    t <- newId
    pure (Out.Let v e1', t)
  assignIds' (In.Lambda v e) = do
    (e', _) <- assignIds e
    t <- newId
    pure (Out.Lambda v e', t)
  assignIds' (In.FunctionCall e1 e2) = do
    (e1', _) <- assignIds e1
    (e2', _) <- assignIds e2
    t <- newId
    pure (Out.FunctionCall e1' e2', t)
  assignIds' (In.If e1 e2 e3) = do
    (e1', _) <- assignIds e1
    (e2', _) <- assignIds e2
    (e3', _) <- assignIds e3
    t <- newId
    pure (Out.If e1' e2' e3', t)
  assignIds' (In.List es) = do
    es' <- fst <<$>> traverse assignIds es
    t <- newId
    pure (Out.List es', t)
  assignIds' (In.LetIn n e b) = do
    (e', _) <- assignIds e
    (b', t) <- assignIds b
    pure (Out.LetIn n e' b', t)
  assignIds' (In.Match e ps) = do
    (e', _) <- assignIds e
    ps' <- fsts <<$>> traverse (bitraverse assignIdsToPattern assignIds) ps
    t <- newId
    pure (Out.Match e' ps', t)
  assignIds' (In.Block es) = do
    es' <- fst <<$>> traverse assignIds es
    t <- newId
    pure (Out.Block es', t)

assignIdsToPattern :: forall r. Member UniqueGen r => In.Pattern -> Sem r (Out.Pattern PartialType, PartialType)
assignIdsToPattern (In.Pattern lp) = bimap Out.Pattern snd . dup . unwrap <$> traverseOf unlocated assignIdsToPattern' lp
 where
  assignIdsToPattern' :: In.Pattern' -> Sem r (Out.Pattern' PartialType, PartialType)
  assignIdsToPattern' (In.VarPattern v) = (Out.VarPattern (transformVarRef <$> v),) <$> newId
  assignIdsToPattern' (In.ConstructorPattern v as) = do
    as' <- fst <<$>> traverse assignIdsToPattern as
    t <- newId
    pure (Out.ConstructorPattern v as', t)
  assignIdsToPattern' In.WildcardPattern = (Out.WildcardPattern,) <$> newId
  assignIdsToPattern' (In.IntegerPattern i) = (Out.IntegerPattern i,) <$> newId
  assignIdsToPattern' (In.FloatPattern i) = (Out.FloatPattern i,) <$> newId
  assignIdsToPattern' (In.StringPattern i) = (Out.StringPattern i,) <$> newId
  assignIdsToPattern' (In.CharPattern i) = (Out.CharPattern i,) <$> newId
  assignIdsToPattern' (In.ListPattern ps) = do
    ps' <- fst <<$>> traverse assignIdsToPattern ps
    t <- newId
    pure (Out.ListPattern ps', t)

transformVarRef :: In.VarRef i -> Out.VarRef i
transformVarRef (In.Global n) = Out.Global n
transformVarRef (In.Local n) = Out.Local n

fsts :: ((a, b), (c, d)) -> (a, c)
fsts ((a, _), (c, _)) = (a, c)

unwrap :: Located (a, b) -> (Located a, b)
unwrap l = (fst <$> l, view (unlocated . _2) l)

newId :: Member UniqueGen r => Sem r PartialType
newId = Id <$> makeUniqueId
