-- | Simple interpreter for the Core language
module Elara.Interpreter where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Elara.AST.Name (ModuleName (..), Qualified (..))
import Elara.AST.VarRef
import Elara.Core hiding (Literal (..))
import Elara.Core qualified as Core
import Elara.Core.Generic (Bind (..))
import Elara.Core.Module
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.Unique
import Elara.Error (ReportableError, runErrorOrReport)
import Elara.Logging (StructuredDebug, logDebug, logDebugWith)
import Elara.Prim.Core (consCtorName, emptyListCtorName, falseCtor, fetchPrimitiveName, trueCtor, tuple2CtorName, unitCtor)
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects, QueryEffects)
import Elara.ReadFile
import Elara.Settings
import Rock qualified
import Prelude hiding (force)

type Interpreter r =
    ( State ElaraState :> r
    , Error InterpreterError :> r
    , QueryEffects r
    , Rock.Rock Elara.Query.Query :> r
    , StructuredDebug :> r
    , InterpreterOutput :> r
    , IOE :> r
    , HasCallStack
    )

data InterpreterOutput :: Effect where
    Print :: Text -> InterpreterOutput m ()

type instance DispatchOf InterpreterOutput = Dynamic

printText :: InterpreterOutput :> r => Text -> Eff r ()
printText t = send (Print t)

interpretInterpreterOutput :: (Text -> Eff r ()) -> Eff (InterpreterOutput : r) a -> Eff r a
interpretInterpreterOutput f = interpret (const $ \case Print t -> f t)

runInterpreterOutput :: IOE :> r => Eff (InterpreterOutput : r) a -> Eff r a
runInterpreterOutput = interpretInterpreterOutput putTextLn

type InterpreterEffects =
    ConsQueryEffects
        '[ InterpreterOutput
         , State ElaraState
         , Error InterpreterError
         , StructuredDebug
         , Rock.Rock Elara.Query.Query
         , IOE
         ]

data ElaraState = ElaraState
    { globalBindings :: Map (Qualified Text) Value
    , localBindings :: Map (Unique Text) Value
    , loadedModules :: Set ModuleName
    , stateSource :: StateSource
    }
    deriving (Generic)

instance Pretty ElaraState

data StateSource = FromGlobal | FromClosure deriving (Generic)
instance Pretty StateSource

data InterpreterError
    = UnboundVariable (UnlocatedVarRef Text) ElaraState
    | NotAFunction Value
    | UnknownPrimitive Text
    | UnhandledExpr CoreExpr
    | NonExhaustiveMatch Value [Core.Alt Var]
    | NoMainFound ElaraState
    | TypeMismatch
        { expected :: Text
        , actual :: Value
        }
    | CodeThrewError Value
    | RecursiveValueDetected (Unique Text)
    | MissingPrimType (Qualified Text)
    deriving (Generic)

data ThunkState = Unevaluated | Evaluating | Evaluated Value
    deriving (Generic)

instance Pretty InterpreterError where
    pretty (TypeMismatch expected actual) =
        "Type mismatch: expected" <+> Style.bold (pretty expected) <+> "but got" <+> Style.bold (prettyValueWithType actual)
    pretty x = gpretty x
instance ReportableError InterpreterError

data Value
    = Int Integer
    | String Text
    | Char Char
    | Double Double
    | -- | Data constructor invocation
      Ctor DataCon [Value]
    | Closure
        { env :: Map (Unique Text) Value -- Captured environment, locals only
        , param :: Unique Text -- Parameter name
        , body :: CoreExpr -- Function body
        }
    | RecClosure
        { recEnv :: Map (Unique Text) Value -- Captured environment, locals only
        , name :: UnlocatedVarRef Text -- This function's name
        , param :: Unique Text -- Parameter
        , body :: CoreExpr
        }
    | PrimOp Text
    | PartialApplication Value Value
    | IOAction (Eff InterpreterEffects Value)
    | Thunk (IORef ThunkState) (Eff InterpreterEffects Value)
    deriving (Generic)

instance Eq Value where
    Int a == Int b = a == b
    String a == String b = a == b
    Char a == Char b = a == b
    Double a == Double b = a == b
    Ctor a b == Ctor c d = a == c && b == d
    Closure{} == Closure{} = False
    PrimOp a == PrimOp b = a == b
    PartialApplication a b == PartialApplication c d = a == c && b == d
    IOAction{} == IOAction{} = False
    _ == _ = False

-- bit of a hack
instance Pretty (Eff InterpreterEffects a) where
    pretty _ = "<IO>"

instance Pretty (IORef ThunkState) where
    pretty _ = "<Thunk>"

instance Pretty Value where
    pretty (Closure _ p _) = "Closure accepting" <+> pretty p
    pretty (RecClosure env n p _) = "RecClosure" <+> pretty n <+> "accepting" <+> pretty p <+> "with env" <+> pretty env
    pretty (Int i) = pretty i
    pretty (String s) = pretty '"' <> pretty s <> pretty '"'
    pretty (Char c) = pretty c
    pretty (Double d) = pretty d
    pretty (Ctor (DataCon listName _ _) []) | listName == emptyListCtorName = "[]"
    pretty (Ctor (DataCon listName _ _) [head', tail']) | listName == consCtorName = "[" <> go head' tail' <> "]"
      where
        go h (Ctor (DataCon listName' _ _) []) | listName' == emptyListCtorName = pretty h
        go h (Ctor (DataCon listName' _ _) [h', t']) | listName' == consCtorName = pretty h <> ", " <> go h' t'
        go h t = pretty h <> ", " <> pretty t
    pretty (Ctor c []) = pretty c.name
    pretty (Ctor (DataCon tuple2 _ _) [arg1, arg2]) | tuple2 == tuple2CtorName = parens (pretty arg1 <> comma <+> pretty arg2)
    pretty (Ctor c args) = parens (pretty c.name <+> hsep (pretty <$> args))
    pretty (Thunk _ _) = "<Thunk>"
    pretty p = gpretty p

prettyValueWithType = \case
    Int x -> pretty x <+> ":: Int"
    s@String{} -> pretty s <+> ":: String"
    c@Char{} -> pretty c <+> ":: Char"
    d@Double{} -> pretty d <+> ":: Double"
    ctor@(Ctor c _) -> pretty ctor <+> "::" <+> pretty c.dataConType
    Closure{} -> "Closure"
    RecClosure{} -> "RecClosure"
    PrimOp name -> "Primitive operation" <+> pretty name
    PartialApplication f a ->
        "Partial application of"
            <+> prettyValueWithType f
            <+> "to"
            <+> prettyValueWithType a
    IOAction{} -> "<IO>"
    Thunk _ _ -> "<Thunk>"

primOps =
    Map.fromList
        [ ("==", 2)
        , ("+", 2)
        , ("*", 2)
        , ("/", 2)
        , ("%", 2)
        , (">>=", 2)
        , ("stringCons", 2)
        , ("compare", 2)
        , ("debugWithMsg", 2)
        ]

boolValue :: Bool -> Value
boolValue True = Ctor trueCtor []
boolValue False = Ctor falseCtor []

-- Force a value once (for thunks); detects recursive value loops
force :: Interpreter r => Value -> Eff r Value
force (Thunk ref act) = do
    st <- liftIO (readIORef ref)
    case st of
        Evaluated v -> pure v
        Evaluating -> throwError_ $ CodeThrewError (String "Recursive value detected")
        Unevaluated -> do
            liftIO (writeIORef ref Evaluating)
            v <- inject act
            liftIO (writeIORef ref (Evaluated v))
            pure v
force v = pure v

mkThunk :: Interpreter r => Eff InterpreterEffects Value -> Eff r Value
mkThunk act = do
    r <- liftIO (newIORef Unevaluated)
    pure (Thunk r act)

scopedOverLocals :: Interpreter r => Eff r a -> Eff r a
scopedOverLocals eff = do
    oldLocals <- gets localBindings
    r <- eff
    modify (\s -> s{localBindings = oldLocals})
    pure r

lookupVar :: Interpreter r => UnlocatedVarRef Text -> Eff r Value
lookupVar (Local v) = do
    s <- get
    case Map.lookup v (localBindings s) of
        Just val -> pure val
        Nothing -> throwError_ (UnboundVariable (Local v) s)
lookupVar (Global v) = logDebugWith ("lookupVar: " <> pretty v) $ do
    s <- get
    case Map.lookup v (globalBindings s) of
        Just val -> pure val
        Nothing -> case v of
            qn -> do
                mod <- Rock.fetch (Elara.Query.GetFinalisedCoreModule (qualifier qn))
                loadModule mod
                newS <- get
                logDebug $ "After loading " <+> pretty (qualifier qn) <+> ", bindings: " <+> pretty (globalBindings newS)
                case Map.lookup v (globalBindings newS) of
                    Just val -> pure val
                    Nothing -> throwError_ (UnboundVariable (Global v) newS)

evalPrim :: Interpreter r => Text -> Eff r Value
evalPrim "getArgs" = do
    -- has to be handled specially because it has 0 args
    nilCon <-
        Rock.fetch (Elara.Query.GetDataCon emptyListCtorName)
            ?:! throwError_ (MissingPrimType emptyListCtorName)
    consCon <-
        Rock.fetch (Elara.Query.GetDataCon consCtorName)
            ?:! throwError_ (MissingPrimType consCtorName)

    pure $ IOAction $ do
        args <- (.programArgs) <$> Rock.fetch Elara.Query.GetCompilerSettings
        let toValueList [] = Ctor nilCon []
            toValueList (x : xs) = Ctor consCon [String (toText x), toValueList xs]
        pure $ toValueList args
evalPrim name = pure $ PrimOp name

interpretExpr :: forall r. Interpreter r => CoreExpr -> Eff r Value
interpretExpr (Lit (Core.Int i)) = pure $ Int i
interpretExpr (Lit (Core.String s)) = pure $ String s
interpretExpr (Lit (Core.Char c)) = pure $ Char c
interpretExpr (Lit (Core.Double d)) = pure $ Double d
interpretExpr (Lit Core.Unit) = pure $ Ctor unitCtor []
interpretExpr (App (Var (Id (Global primName) _ _)) (Lit (Core.String primArg))) | primName == fetchPrimitiveName = evalPrim primArg
-- elaraPrimitive should be called with a tyapp - i.e. `elaraPrimitive @T "f"`
interpretExpr (App (TyApp (Var (Id (Global primName) _ _)) _) (Lit (Core.String primArg))) | primName == fetchPrimitiveName = evalPrim primArg
interpretExpr (Var (Id v _ _)) = lookupVar v
interpretExpr (Let bind in') = logDebugWith ("interpretExpr: " <+> pretty bind) $ scopedOverLocals $ do
    interpretBinding bind
    interpretExpr in'
interpretExpr (Lam (Id (Local v) _ _) e) = do
    s <- get
    pure $ Closure (localBindings s) v e
interpretExpr (App f a) = do
    f' <- interpretExpr f
    a' <- interpretExpr a
    apply f' a'
interpretExpr (Match e of' alts) = do
    e' <- interpretExpr e >>= force
    logDebug $ "forced match scrutinee to" <+> pretty e'
    whenJust of' $ \(Id (Local v) _ _) -> do
        modify (\s -> s{localBindings = Map.insert v e' (localBindings s)})

    let go :: [Core.Alt Var] -> Eff r Value
        go [] = throwError_ $ NonExhaustiveMatch e' alts
        go ((DataAlt c, vs, branchBody) : rest) = do
            case e' of
                Ctor c' args | c == c' -> do
                    let vs' = (\(Id (Local i) _ _) -> i) <$> vs
                    modify (\s -> s{localBindings = foldr (uncurry Map.insert) (localBindings s) (zip vs' args)})
                    interpretExpr branchBody
                _ -> go rest
        go ((LitAlt l, _, branchBody) : rest) = do
            case e' of
                Int i | l == Core.Int i -> interpretExpr branchBody
                String s | l == Core.String s -> interpretExpr branchBody
                Char c | l == Core.Char c -> interpretExpr branchBody
                Double d | l == Core.Double d -> interpretExpr branchBody
                _ -> go rest
        go ((DEFAULT, _, branchBody) : _) = interpretExpr branchBody
    go alts
interpretExpr (TyApp e _) = interpretExpr e -- lol
interpretExpr other = throwError_ $ UnhandledExpr other

apply :: Interpreter r => Value -> Value -> Eff r Value
apply f' a' = do
    case f' of
        rec@(RecClosure env n p e) -> do
            -- add the argument to the env
            let env'' = Map.insert p a' env

            scopedOverLocals $ do
                modify
                    ( \s ->
                        case n of
                            Local n' ->
                                s
                                    { localBindings = Map.insert n' rec (env'' <> localBindings s) -- add itself to the env
                                    , stateSource = FromClosure
                                    }
                            Global n' ->
                                s
                                    { globalBindings = Map.insert n' rec (globalBindings s) -- add itself to the env
                                    , localBindings = env''
                                    , stateSource = FromClosure
                                    }
                    )

                interpretExpr e
        Closure env p e -> do
            let env' = Map.insert p a' env
            scopedOverLocals $ do
                modify (\s -> s{localBindings = env', stateSource = FromClosure})
                interpretExpr e
        PrimOp "toString" -> do
            let asText = case a' of
                    Ctor (DataCon (Qualified "Tuple2" _) _ _) [arg1, arg2] ->
                        prettyToText (arg1, arg2)
                    other -> prettyToText other
            pure $ String asText
        PrimOp "error" -> do
            throwError_ $ CodeThrewError a'
        PrimOp "println" -> do
            pure $ IOAction $ do
                let asString = case a' of
                        String s -> s
                        other -> prettyToText other
                printText asString
                pure (Ctor unitCtor [])
        PrimOp "getArgs" -> do
            nilCon <-
                Rock.fetch (Elara.Query.GetDataCon emptyListCtorName)
                    ?:! throwError_ (MissingPrimType emptyListCtorName)
            consCon <-
                Rock.fetch (Elara.Query.GetDataCon consCtorName)
                    ?:! throwError_ (MissingPrimType consCtorName)

            pure $ IOAction $ do
                args <- getArgs
                let toValueList [] = Ctor nilCon []
                    toValueList (x : xs) = Ctor consCon [String (toText x), toValueList xs]
                pure $ toValueList args
        PrimOp "readFile" -> do
            case a' of
                String s -> pure $ IOAction $ do
                    contents <-
                        runErrorOrReport @ReadFileError $
                            Rock.fetch (Elara.Query.GetFileContents (toString s))
                    pure $ String contents.fileContents
                _ -> throwError_ TypeMismatch{expected = "String", actual = a'}
        PrimOp "negate" -> do
            case a' of
                Int i -> pure $ Int (-i)
                Double d -> pure $ Double (-d)
                _ -> throwError_ TypeMismatch{expected = "Int or Double", actual = a'}
        PrimOp "stringIsEmpty" -> do
            case a' of
                String s -> pure $ boolValue (Text.null s)
                _ -> throwError_ TypeMismatch{expected = "String", actual = a'}
        PrimOp "stringHead" -> do
            case a' of
                String s -> pure $ Char (Text.head s)
                _ -> throwError_ TypeMismatch{expected = "String", actual = a'}
        PrimOp "stringTail" -> do
            case a' of
                String s -> pure $ String (Text.tail s)
                _ -> throwError_ TypeMismatch{expected = "String", actual = a'}
        PartialApplication (PrimOp "stringCons") fst -> do
            case (fst, a') of
                (Char c, String s) -> pure $ String (Text.cons c s)
                _ -> throwError_ TypeMismatch{expected = "Char and String", actual = a'}
        PartialApplication (PrimOp "==") fst -> do
            pure (boolValue (a' == fst))
        PartialApplication (PrimOp "+") fst -> do
            case (a', fst) of
                (Int a, Int b) -> pure $ Int (a + b)
                (Double a, Double b) -> pure $ Double (a + b)
                _ -> throwError_ TypeMismatch{expected = "(+): Int or Double", actual = a'}
        PartialApplication (PrimOp "*") fst -> do
            case (a', fst) of
                (Int a, Int b) -> pure $ Int (a * b)
                (Double a, Double b) -> pure $ Double (a * b)
                _ -> throwError_ TypeMismatch{expected = "(*): Int or Double", actual = a'}
        PartialApplication (PrimOp "/") fst -> do
            case (fst, a') of
                (Int a, Int b) -> pure $ Int (a `div` b)
                (Double a, Double b) -> pure $ Double (a / b)
                _ -> throwError_ TypeMismatch{expected = "(/): Int or Double", actual = a'}
        PartialApplication (PrimOp "%") fst -> do
            case (fst, a') of
                (Int a, Int b) -> pure $ Int (a `mod` b)
                _ -> throwError_ TypeMismatch{expected = "(%): Int", actual = a'}
        PartialApplication (PrimOp "compare") fst -> do
            let orderingToInt = \case
                    LT -> -1
                    EQ -> 0
                    GT -> 1
            case (fst, a') of
                (Int a, Int b) -> pure $ Int (orderingToInt (compare a b))
                (Double a, Double b) -> pure $ Int (orderingToInt (compare a b))
                (Char a, Char b) -> pure $ Int (orderingToInt (compare a b))
                _ -> throwError_ TypeMismatch{expected = "(compare): Int or Double or Char", actual = a'}
        PartialApplication (PrimOp "debugWithMsg") fst -> do
            logDebug $ "Source Code Debug  (" <> pretty fst <> "):  " <> pretty a'
            pure a'
        PrimOp primName -> do
            case Map.lookup primName primOps of
                Just 1 -> error "1-arg primop Should be handled here"
                Just _ -> pure $ PartialApplication f' a'
                Nothing -> throwError_ $ UnknownPrimitive primName
        PartialApplication (PrimOp ">>=") fst -> do
            case fst of
                IOAction io -> do
                    pure $ IOAction $ do
                        val <- io
                        apply a' val
                _ -> throwError_ TypeMismatch{expected = "IO", actual = a'}
        Ctor c args | typeArity c.dataConType < length args -> do
            throwError_ $ NotAFunction f'
        Ctor c args -> do
            pure $ Ctor c (args <> [a'])
        Thunk{} -> do
            forced <- force f'
            apply forced a'
        other -> throwError_ $ NotAFunction other

interpretBinding :: Interpreter r => CoreBind -> Eff r ()
interpretBinding (NonRecursive (Id v _ _, e)) = logDebugWith ("interpretBinding: " <> pretty v) $ do
    val <- interpretExpr e
    case v of
        Global qn -> modify (\s -> s{globalBindings = Map.insert qn val (globalBindings s)})
        Local v -> modify (\s -> s{localBindings = Map.insert v val (localBindings s)})
-- Tie-the-knot letrec with thunks for non-functions, RecClosure for functions
interpretBinding (Recursive bs) = logDebugWith ("interpretBinding Rec: " <> pretty (fst <$> bs)) $ do
    curLocals <- gets localBindings
    -- Split bindings by Local/Global to avoid partial pattern matches
    let (localBs, globalBs) =
            partitionEithers
                [ case v of
                    Local u -> Left (u, rhs)
                    Global q -> Right (q, rhs)
                | (Id v _ _, rhs) <- bs
                ]

    -- Placeholders for all bindings
    placeholdersLocal <-
        Map.fromList
            <$> forM
                localBs
                ( \(v, rhs) -> case v of
                    u ->
                        case rhs of
                            Lam (Id (Local p) _ _) body -> pure (u, RecClosure Map.empty (Local u) p body)
                            _ -> do th <- mkThunk (interpretExpr rhs); pure (u, th)
                )
    placeholdersGlobal <-
        Map.fromList
            <$> forM
                globalBs
                ( \(v, rhs) -> case v of
                    q ->
                        case rhs of
                            Lam (Id (Local p) _ _) body -> pure (q, RecClosure Map.empty (Global q) p body)
                            _ -> do th <- mkThunk (interpretExpr rhs); pure (q, th)
                )

    -- Final env captured by recursive functions (locals only)
    let finalLocalEnv = Map.union placeholdersLocal curLocals

    -- Update RecClosures to capture finalLocalEnv
    let upd = \case
            RecClosure _ n p b -> RecClosure finalLocalEnv n p b
            v -> v
    let updatedLocals = Map.map upd placeholdersLocal
        updatedGlobals = Map.map upd placeholdersGlobal

    -- Install all placeholders (functions and thunks) so RHS can refer mutually
    modify
        ( \s ->
            s
                { localBindings = Map.union updatedLocals (localBindings s)
                , globalBindings = Map.union updatedGlobals (globalBindings s)
                }
        )

-- | Load a module into the interpreter
loadModule :: Interpreter r => CoreModule CoreBind -> Eff r ()
loadModule (CoreModule name decls) = logDebugWith ("loadModule:" <+> pretty name) $ do
    loaded <- gets loadedModules
    if Set.member name loaded
        then logDebug $ "Module" <+> pretty name <+> "already loaded, skipping"
        else do
            modify (\s -> s{loadedModules = Set.insert name (loadedModules s)})
            oldBindings <- gets globalBindings
            for_
                decls
                ( \case
                    (CoreValue v) -> void $ interpretBinding v
                    (CoreType decl) -> loadTypeDecl decl
                )
            newEnv <- gets globalBindings
            logDebug $ "Loaded module" <+> pretty name <+> "with bindings" <+> pretty (Map.difference newEnv oldBindings)

loadTypeDecl :: Interpreter r => CoreTypeDecl -> Eff r ()
loadTypeDecl (CoreTypeDecl _ _ _ (CoreDataDecl _ cons)) = do
    for_ cons $ \con@(DataCon name _ _) -> do
        modify (\s -> s{globalBindings = Map.insert name (Ctor con []) (globalBindings s)})
loadTypeDecl (CoreTypeDecl _ _ _ (CoreTypeAlias _)) = pass

evalIO :: Interpreter r => Value -> Eff r Value
evalIO (IOAction io) = do
    val <- inject io
    evalIO val
evalIO other = pure other

-- | Looks for a value Main.main in the bindings and runs it
run :: (Interpreter r, QueryEffects r, Rock.Rock Elara.Query.Query :> r) => Eff r ()
run = do
    mainModule <- Rock.fetch (Elara.Query.GetFinalisedCoreModule (ModuleName ("Main" :| [])))
    loadModule mainModule
    s <- get
    case Map.lookup (Qualified "main" (ModuleName ("Main" :| []))) s.globalBindings of
        Just val -> do
            evalIO val
            pass
        _ -> throwError_ $ NoMainFound s

runInterpreter eff = do
    let s = ElaraState{localBindings = Map.empty, globalBindings = Map.empty, loadedModules = Set.empty, stateSource = FromGlobal}
    inject (runErrorOrReport @InterpreterError $ evalState s eff)
