-- | Simple interpreter for the Core language
module Elara.Interpreter where

import Control.Concurrent (threadDelay)
import Data.Map qualified as Map
import Elara.AST.Name (ModuleName (..), Qualified (..), VarName)
import Elara.AST.VarRef
import Elara.Core hiding (Literal (..))
import Elara.Core qualified as Core
import Elara.Core.Generic (Bind (..))
import Elara.Core.Module
import Elara.Data.Pretty
import Elara.Error (ReportableError, runErrorOrReport)
import Elara.Logging (StructuredDebug, debug, debugWith, structuredDebugToLog)
import Elara.Pipeline
import Elara.Prim.Core (falseCtor, fetchPrimitiveName, trueCtor, unitCtor)
import Polysemy hiding (run)
import Polysemy.Error
import Polysemy.State
import Polysemy.State.Extra
import Print (prettyToString)

type Interpreter r = Members InterpreterEffects r

type InterpreterEffects = [State ElaraState, Error InterpreterError, StructuredDebug, Embed IO]

data ElaraState = ElaraState
    { bindings :: Map (UnlocatedVarRef Text) Value
    }
    deriving (Generic)

instance Pretty ElaraState

data InterpreterError
    = UnboundVariable (UnlocatedVarRef Text) ElaraState
    | NotAClosure Value
    | UnknownPrimitive Text
    | UnhandledExpr CoreExpr
    | NoMainFound ElaraState
    | TypeMismatch
        { expected :: Text
        , actual :: Value
        }
    deriving (Generic)

instance Pretty InterpreterError

instance ReportableError InterpreterError

data Value
    = Int Integer
    | String Text
    | Char Char
    | Double Double
    | Ctor DataCon [Value]
    | Closure
        { env :: Map (UnlocatedVarRef Text) Value -- Captured environment
        , param :: UnlocatedVarRef Text -- Parameter name
        , body :: CoreExpr -- Function body
        }
    | RecClosure
        { recEnv :: Map (UnlocatedVarRef Text) Value
        , name :: UnlocatedVarRef Text -- This function's name
        , param :: UnlocatedVarRef Text -- Parameter
        , body :: CoreExpr
        }
    | PrimOp Text
    | PartialApplication Value Value
    | IOAction (IO Value)
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
instance Pretty (IO a) where
    pretty _ = "<IO>"

instance Pretty Value where
    pretty (Closure _ p _) = "Closure accepting" <+> pretty p
    pretty (RecClosure _ n p _) = "RecClosure" <+> pretty n <+> "accepting" <+> pretty p
    pretty p = gpretty p

primOps =
    Map.fromList
        [ ("==", 2)
        , ("-", 2)
        ]

boolValue :: Bool -> Value
boolValue True = Ctor trueCtor []
boolValue False = Ctor falseCtor []

interpretExpr :: forall r. Interpreter r => CoreExpr -> Sem r Value
interpretExpr (Lit (Core.Int i)) = pure $ Int i
interpretExpr (Lit (Core.String s)) = pure $ String s
interpretExpr (Lit (Core.Char c)) = pure $ Char c
interpretExpr (Lit (Core.Double d)) = pure $ Double d
interpretExpr (Lit Core.Unit) = pure $ Ctor unitCtor []
interpretExpr (App (Var (Id (UnlocatedGlobal primName) _ _)) (Lit (Core.String primArg))) | primName == fetchPrimitiveName = do
    pure $ PrimOp primArg
interpretExpr (Var (Id v _ _)) = do
    s <- get
    case Map.lookup v (bindings s) of
        Just val -> pure val
        Nothing -> throw $ UnboundVariable v s
interpretExpr (Let bind in') = scoped $ do
    interpretBinding bind
    interpretExpr in'
interpretExpr (Lam (Id v _ _) e) = do
    s <- get
    pure $ Closure (bindings s) v e
interpretExpr (App f a) = debugWith ("Applying " <> pretty a <+> "to" <+> pretty f) $ do
    f' <- interpretExpr f
    debug $ pretty f <> " = " <> pretty f'
    a' <- interpretExpr a
    debug $ pretty a <> " = " <> pretty a'
    case f' of
        rec@(RecClosure env n p e) -> scoped $ do
            -- add itself to the env
            let env' = Map.insert n rec env

            -- add the argument to the env
            let env'' = Map.insert p a' env'

            scoped $ do
                modify (\s -> s{bindings = env''})
                interpretExpr e
        Closure env p e -> do
            let env' = Map.insert p a' env
            scoped $ do
                modify (\s -> s{bindings = env'})
                interpretExpr e
        PrimOp "println" -> do
            pure $ IOAction $ do
                putStrLn (prettyToString a')
                pure (Ctor unitCtor [])
        PartialApplication (PrimOp "==") fst -> do
            pure $ (boolValue (a' == fst))
        PartialApplication (PrimOp "-") fst -> do
            case (a', fst) of
                (Int a, Int b) -> pure $ Int (a - b)
                (Double a, Double b) -> pure $ Double (a - b)
                _ -> throw TypeMismatch{expected = "Int or Double", actual = a'}
        PrimOp primName -> do
            case Map.lookup primName primOps of
                Just 1 -> error "1-arg primop Should be handled here"
                Just _ -> pure $ PartialApplication f' a'
                Nothing -> throw $ UnknownPrimitive primName
        other -> throw $ NotAClosure other
interpretExpr (Match e of' alts) = debugWith ("Matching " <> pretty e) $ do
    e' <- interpretExpr e
    whenJust of' $ \(Id v _ _) -> do
        modify (\s -> s{bindings = Map.insert v e' (bindings s)})

    let go :: [Core.Alt Var] -> Sem r Value
        go [] = throw $ UnhandledExpr e
        go ((DataAlt c, vs, branchBody) : rest) = do
            case e' of
                Ctor c' args | c == c' -> do
                    let vs' = (\(Id i _ _) -> i) <$> vs
                    modify (\s -> s{bindings = foldr (uncurry Map.insert) (bindings s) (zip vs' args)})
                    debug $ "Matched " <> pretty c <> ", so we eval " <> pretty branchBody
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
interpretExpr other = throw $ UnhandledExpr other

interpretBinding :: Interpreter r => CoreBind -> Sem r ()
interpretBinding (NonRecursive (Id v _ _, e)) = debugWith ("Interpreting let" <+> pretty v <+> "=" <+> pretty e) $ do
    val <- interpretExpr e
    modify (\s -> s{bindings = Map.insert v val (bindings s)})
interpretBinding (Recursive bs) = debugWith ("Interpreting letrec" <+> pretty bs) $ do
    currentEnv <- gets bindings
    -- Create RecClosures that share the same environment
    let recEnv = Map.fromList [(v, RecClosure Map.empty v p e) | (Id v _ _, Lam (Id p _ _) e) <- bs]
    -- Update the RecClosures with the complete recursive environment
    let finalEnv = Map.union recEnv currentEnv
    let finalClosures = Map.map (\(RecClosure _ n p b) -> RecClosure finalEnv n p b) recEnv

    modify (\s -> s{bindings = Map.union finalClosures (bindings s)})

-- | Load a module into the interpreter
loadModule :: Interpreter r => CoreModule CoreBind -> Sem r ()
loadModule (CoreModule _ decls) = do
    for_
        decls
        ( \(CoreValue v) -> do
            interpretBinding v
        )
    pure ()

evalIO :: Interpreter r => Value -> Sem r Value
evalIO (IOAction io) = do
    val <- embed io
    evalIO val
evalIO other = pure other

{- | Looks for a value Main.main in the bindings and runs it
The module must have been already loaded
-}
run :: Interpreter r => Sem r ()
run = do
    s <- get
    case Map.lookup (UnlocatedGlobal (Qualified "main" (ModuleName ("Main" :| [])))) (s.bindings) of
        Just (val) -> do
            evalIO val
            pure ()
        _ -> throw $ NoMainFound s

runInterpreter :: IsPipeline r => Sem (EffectsAsPrefixOf InterpreterEffects r) a -> Sem r a
runInterpreter m = do
    let s = ElaraState{bindings = Map.empty}
    subsume $ subsume (runErrorOrReport $ evalState s m)
