{-# LANGUAGE RecordWildCards #-}

{- | Performs kind inference on a type.


 >> infer "type alias Identity a = a"
 "k -> k"

 >> infer "type Maybe a = Either a Unit"
 Type -> Type

 >> infer "type Either a b = Left a | Right b"
 Type -> Type -> Type

 >> infer "type Proxy a = Proxy"
 k -> Type

 TODO: this is extremely basic and needs some nicer error messages, no support for polykinds or higher-kinded types yet
-}
module Elara.Data.Kind.Infer where

import Control.Lens (Each (each), traverseOf_, view, (^.), _2)
import Data.Map qualified as Map
import Elara.AST.Name (LowerAlphaName, Qualified, TypeName)
import Elara.AST.Region (Located, unlocated)
import Elara.AST.Renamed qualified as AST
import Elara.Data.Kind
import Elara.Data.Unique (Unique, UniqueGen, getUniqueId, makeUniqueId)
import Elara.Prim (primKindCheckContext)
import Polysemy (Member, Sem)
import Polysemy.Error
import Polysemy.State
import TODO (todo)

newtype InferState = InferState
    { kinds :: Map (Qualified TypeName) ElaraKind
    }
    deriving (Eq, Show)

initialInferState :: InferState
initialInferState =
    InferState
        { kinds = primKindCheckContext
        }

data KindInferError
    = UnknownKind (Qualified TypeName) (Map (Qualified TypeName) ElaraKind)
    | CannotUnify ElaraKind ElaraKind
    | NotFunctionKind ElaraKind
    deriving (Eq, Show)

freshKindVar :: (Member UniqueGen r) => Sem r ElaraKind
freshKindVar = VarKind <$> makeUniqueId

lookupKind :: (Member (State InferState) r, Member (Error KindInferError) r) => Qualified TypeName -> Sem r ElaraKind
lookupKind name = do
    InferState{..} <- get
    case Map.lookup name kinds of
        Just k -> pure k
        Nothing -> throw (UnknownKind name kinds)

inferKind ::
    (Member (State InferState) r, Member (Error KindInferError) r) =>
    Qualified TypeName ->
    [Located (Unique LowerAlphaName)] ->
    AST.TypeDeclaration ->
    Sem r ElaraKind
inferKind tName args t = do
    let args' = fmap (getUniqueId . view unlocated) args
    t' <- case t of
        AST.Alias a -> inferTypeKind (a ^. unlocated)
        AST.ADT constructors -> do
            traverseOf_ (each . _2 . each . unlocated) (unifyKinds TypeKind <=< inferTypeKind) constructors
            pure TypeKind

    let funcKind = foldr FunctionKind t' (VarKind <$> args')
    InferState{..} <- get
    put
        InferState
            { kinds = Map.insert tName funcKind kinds
            }
    pure funcKind

inferTypeKind :: (Member (State InferState) r, Member (Error KindInferError) r) => AST.Type -> Sem r ElaraKind
inferTypeKind AST.UnitType = pure TypeKind
inferTypeKind (AST.TypeVar v) = pure (VarKind (getUniqueId (v ^. unlocated))) -- no higher kinded types yet
inferTypeKind (AST.UserDefinedType name) = lookupKind (name ^. unlocated)
inferTypeKind (AST.FunctionType a b) = do
    a' <- inferTypeKind (a ^. unlocated)
    b' <- inferTypeKind (b ^. unlocated)
    unifyKinds a' TypeKind
    unifyKinds b' TypeKind
    pure TypeKind
inferTypeKind (AST.TypeConstructorApplication ctor a) = do
    ctor' <- inferTypeKind (ctor ^. unlocated)
    a' <- inferTypeKind (a ^. unlocated)
    case ctor' of
        FunctionKind a'' b -> do
            unifyKinds a' a''
            pure b
        e -> throw (NotFunctionKind e)
inferTypeKind (AST.RecordType _) = todo
inferTypeKind (AST.TupleType fields) = do
    traverse_ (unifyKinds TypeKind <=< inferTypeKind) (fmap (view unlocated) fields)
    pure TypeKind

unifyKinds :: (Member (Error KindInferError) r) => ElaraKind -> ElaraKind -> Sem r ()
unifyKinds (VarKind _) (VarKind _) = pass
unifyKinds TypeKind (VarKind _) = pass -- type vars are always kind * atm
unifyKinds (VarKind v) v' = unifyKinds v' (VarKind v)
unifyKinds v v' = when (v /= v') (throw (CannotUnify v v'))
