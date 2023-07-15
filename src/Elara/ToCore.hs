-- | Converts typed AST to Core
module Elara.ToCore where

import Control.Lens (mapping, over, to, view, (^.))
import Data.Map qualified as M
import Data.Traversable (for)
import Elara.AST.Lenses (HasDeclarationBody (declarationBody, unlocatedDeclarationBody), HasDeclarationBody' (unlocatedDeclarationBody'))
import Elara.AST.Module (Module (Module), module'Declarations, module'Name)
import Elara.AST.Name (ModuleName (ModuleName), Name (NVarName), NameLike (..), Qualified (..), TypeName, VarName (..), unqualified)
import Elara.AST.Region (Located (Located), unlocated)
import Elara.AST.Select (HasDeclarationName (declarationName), HasModuleName (unlocatedModuleName), Typed)
import Elara.AST.StripLocation
import Elara.AST.Typed as AST
import Elara.AST.VarRef (VarRef, VarRef' (Global, Local), varRefVal)
import Elara.Core as Core
import Elara.Core.Module (CoreDeclaration (CoreDeclaration), CoreDeclarationBody (CoreValue), CoreModule (..))
import Elara.Data.Unique (Unique, UniqueGen, makeUnique)
import Polysemy (Member, Sem)
import Polysemy.Error
import Polysemy.State
import TODO (todo)
import Elara.Error (ReportableError)

data ToCoreError
  = LetInTopLevel AST.Expr
  | UnknownConstructor (Located (Qualified TypeName))
  | UnknownPrimConstructor (Qualified Text)
  deriving (Show, Eq)

instance ReportableError ToCoreError where
    

type CtorSymbolTable = Map (Qualified Text) DataCon

primCtorSymbolTable :: CtorSymbolTable
primCtorSymbolTable =
  fromList
    [ (trueCtorName, DataCon trueCtorName),
      (falseCtorName, DataCon falseCtorName),
      (consCtorName, DataCon consCtorName),
      (emptyListCtorName, DataCon emptyListCtorName)
    ]

lookupCtor :: ToCoreC r => Located (Qualified TypeName) -> Sem r DataCon
lookupCtor qn = do
  table <- get @CtorSymbolTable
  let plainName = nameText <$> qn ^. unlocated
  case M.lookup plainName table of
    Just ctor -> pure ctor
    Nothing -> throw (UnknownConstructor qn)

lookupPrimCtor :: ToCoreC r => Qualified Text -> Sem r DataCon
lookupPrimCtor qn = do
  table <- get @CtorSymbolTable
  case M.lookup qn table of
    Just ctor -> pure ctor
    Nothing -> throw (UnknownPrimConstructor qn)

type ToCoreC r = (Member (Error ToCoreError) r, Member (State CtorSymbolTable) r, Member UniqueGen r)

runToCoreC :: Sem (State CtorSymbolTable : Error ToCoreError : r) a -> Sem r (Either ToCoreError a)
runToCoreC = runError . evalState primCtorSymbolTable

moduleToCore :: (ToCoreC r) => Module Typed -> Sem r CoreModule
moduleToCore (Module (Located _ m)) = do
  let name = nameText (m ^. module'Name)
  decls <- for (m ^. module'Declarations) $ \decl -> do
    body' <- case decl ^. unlocatedDeclarationBody' of
      Value v -> CoreValue <$> toCore v
      other -> error "TODO: other declaration types"
    pure (CoreDeclaration (decl ^. declarationName . to fullNameText) body')
  pure $ CoreModule name decls



toCore :: (ToCoreC r) => AST.Expr -> Sem r CoreExpr
toCore le@(AST.Expr (Located _ e, t)) = toCore' e
  where
    toCore' :: (ToCoreC r) => AST.Expr' -> Sem r CoreExpr
    toCore' = \case
      AST.Int i -> pure $ Lit (Core.Int i)
      AST.Float f -> pure $ Lit (Core.Double f)
      AST.String s -> pure $ Lit (Core.String s)
      AST.Char c -> pure $ Lit (Core.Char c)
      AST.Unit -> pure $ Lit Core.Unit
      AST.Var (Located _ v) -> pure $ Core.Var (nameText <$> stripLocation v)
      AST.Constructor (Located _ v) -> pure $ Core.Var (let y = nameText <$> v in Global (Identity y))
      AST.Lambda (Located _ vn) body -> Core.Lam (Local $ Identity (nameText <$> vn)) <$> toCore body
      AST.FunctionCall e1 e2 -> Core.App <$> toCore e1 <*> toCore e2
      AST.If cond ifTrue ifFalse -> do
        cond' <- toCore cond
        ifTrue' <- toCore ifTrue
        ifFalse' <- toCore ifFalse
        pure $
          Core.Match
            cond'
            Nothing
            [ (Core.DataAlt (Core.DataCon trueCtorName), [], ifTrue'),
              (Core.DataAlt (Core.DataCon falseCtorName), [], ifFalse')
            ]
      AST.List [] -> pure $ Core.Var (Global (Identity emptyListCtorName))
      AST.List (x : xs) -> do
        x' <- toCore x
        xs' <- toCore' (AST.List xs)
        pure $
          Core.App
            (Core.App (Core.Var (Global (Identity consCtorName))) x')
            xs'
      AST.Match e pats -> desugarMatch e pats
      AST.Let {} -> throw (LetInTopLevel le)
      AST.LetIn (Located _ vn) e1 e2 -> do
        e1' <- toCore e1
        e2' <- toCore e2
        let isRecursive = False -- todo
        let name = Local (Identity (nameText <$> vn))
        pure $
          Core.Let
            ( if isRecursive then Core.Recursive [(name, e1')] else Core.NonRecursive (name, e1')
            )
            e2'
      AST.Tuple (one :| []) -> toCore one
      AST.Tuple (one :| [two]) -> do
        one' <- toCore one
        two' <- toCore two
        pure $ Core.App (Core.App (Core.Var (Global (Identity tuple2CtorName))) one') two'
      AST.Tuple _ -> error "TODO: tuple with more than 2 elements"
      AST.Block exprs -> desugarBlock exprs

desugarMatch :: ToCoreC r => AST.Expr -> [(AST.Pattern, AST.Expr)] -> Sem r CoreExpr
desugarMatch e pats = do
  e' <- toCore e
  bind' <- mkBindName e

  pats' <- for pats $ \(AST.Pattern (Located _ p, t), branch) -> do
    branch' <- toCore branch
    case p of
      AST.IntegerPattern i -> pure (Core.LitAlt $ Core.Int i, [], branch')
      AST.FloatPattern f -> pure (Core.LitAlt $ Core.Double f, [], branch')
      AST.StringPattern s -> pure (Core.LitAlt $ Core.String s, [], branch')
      AST.CharPattern c -> pure (Core.LitAlt $ Core.Char c, [], branch')
      AST.UnitPattern -> pure (Core.LitAlt Core.Unit, [], branch')
      AST.WildcardPattern -> pure (Core.DEFAULT, [], branch')
      AST.VarPattern (Located _ vn) -> pure (Core.DEFAULT, [Local $ Identity (view (to nameText) <$> vn)], branch')
      AST.ConstructorPattern cn pats -> do
        c <- lookupCtor cn
        pure (Core.DataAlt c, [], branch')
      -- todo: bind the pattern variables
      AST.ListPattern [] -> pure (Core.DataAlt (Core.DataCon emptyListCtorName), [], branch')
      AST.ConsPattern (Pattern (Located _ p1, _)) (Pattern (Located _ p2, _)) -> do
        c <- lookupPrimCtor consCtorName
        pure (Core.DataAlt c, [], branch')
      other -> error $ "TODO: pattern " <> show other

  pure $ Core.Match e' (Just bind') pats'

mkBindName :: Member UniqueGen r => AST.Expr -> Sem r Var
mkBindName (AST.Expr (Located _ (AST.Var (Located _ vn)), _)) = do
  Local . Identity <$> makeUnique (nameText $ varRefVal vn)
mkBindName _ = Local . Identity <$> makeUnique "bind"

desugarBlock :: ToCoreC r => NonEmpty AST.Expr -> Sem r CoreExpr
desugarBlock (one :| []) = toCore one
desugarBlock _ = error "todo"

trueCtorName :: Qualified Text
trueCtorName = Qualified "True" (ModuleName ("Elara" :| ["Prim"]))

falseCtorName :: Qualified Text
falseCtorName = Qualified "False" (ModuleName ("Elara" :| ["Prim"]))

emptyListCtorName :: Qualified Text
emptyListCtorName = Qualified "[]" (ModuleName ("Elara" :| ["Prim"]))

consCtorName :: Qualified Text
consCtorName = Qualified "::" (ModuleName ("Elara" :| ["Prim"]))

tuple2CtorName :: Qualified Text
tuple2CtorName = Qualified "Tuple2" (ModuleName ("Elara" :| ["Prim"]))