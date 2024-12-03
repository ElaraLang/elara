-- | Simple interpreter for the Core language
module Elara.Interpreter where

import Data.Map qualified as Map
import Elara.AST.Name (ModuleName (..), Qualified (..), VarName)
import Elara.AST.VarRef
import Elara.Core hiding (Literal (..))
import Elara.Core qualified as Core
import Elara.Core.Generic (Bind (..))
import Elara.Core.Module
import Elara.Pipeline
import Elara.Prim.Core (unitCon)
import Polysemy hiding (run)
import Polysemy.State
import Elara.Data.Pretty
import Elara.Error (ReportableError, runErrorOrReport)
import Polysemy.Error

type Interpreter r = Members InterpreterEffects r

type InterpreterEffects = [State ElaraState, Error InterpreterError, Embed IO]

data ElaraState = ElaraState
    { bindings :: Map (UnlocatedVarRef Text) Value
    }
    deriving (Generic)

instance Pretty ElaraState

data InterpreterError = 
    UnboundVariable (UnlocatedVarRef Text)
    | NotAClosure Value
    deriving (Generic)

instance Pretty InterpreterError

instance ReportableError InterpreterError

data Value
    = Int Integer
    | String Text
    | Char Char
    | Double Double
    | Ctor TyCon [Value]
    | Closure (ElaraState) CoreExpr
    deriving (Generic)

instance Pretty Value where


interpretExpr :: Interpreter r => CoreExpr -> Sem r Value
interpretExpr (Lit (Core.Int i)) = pure $ Int i
interpretExpr (Lit (Core.String s)) = pure $ String s
interpretExpr (Lit (Core.Char c)) = pure $ Char c
interpretExpr (Lit (Core.Double d)) = pure $ Double d
interpretExpr (Lit Core.Unit) = pure $ Ctor unitCon []
interpretExpr (Var (Id v _ _)) = do
    s <- get
    case Map.lookup v (bindings s) of
        Just val -> pure val
        Nothing -> error $ "Unbound variable: " <> show v

loadModule :: Interpreter r => CoreModule CoreBind -> Sem r ()
loadModule (CoreModule _ decls) = do
    bindings <-
        traverse
            ( \(CoreValue (NonRecursive (Id v _ _, e))) -> do
                val <- interpretExpr e
                pure (v, val)
            )
            decls
    modify (\s -> s{bindings = Map.fromList bindings})
    pure ()

{- | Looks for a value Main.main in the bindings and runs it
The module must have been already loaded
-}
run :: Interpreter r =>  Sem r ()
run = do
    s <- get
    case Map.lookup (UnlocatedGlobal (Qualified "main" (ModuleName ("Main" :| [])))) (s.bindings) of
        Just (Closure s' e) -> do
            put s'
            interpretExpr e
            pure ()
        _ -> error "No Main.main found"

runInterpreter :: IsPipeline r => Sem (EffectsAsPrefixOf InterpreterEffects r) a -> Sem r a
runInterpreter m = do
    let s = ElaraState{bindings = Map.empty}
    subsume (runErrorOrReport $ evalState s m)
