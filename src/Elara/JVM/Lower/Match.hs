module Elara.JVM.Lower.Match where

import Effectful
import Effectful.Error.Static (throwError)
import Elara.AST.VarRef (VarRef' (..))
import Elara.Core qualified as Core
import Elara.Data.Unique
import Elara.JVM.Error (JVMLoweringError (..))
import Elara.JVM.IR qualified as IR
import Elara.JVM.Lower.Monad
import Elara.JVM.Lower.Util
import JVM.Data.Abstract.Type qualified as JVM

import Effectful.Dispatch.Dynamic (localSeqUnlift)
import Effectful.Writer.Static.Local (runWriter)
import Elara.Core.Analysis
import Elara.Data.Unique.Effect
import Elara.Logging
import JVM.Data.Abstract.Type

{- | Emits instructions to bind pattern variables from a pattern match alternative.
For data constructor patterns, it generates field accesses to extract the fields from the
scrutinee expression and assigns them to the corresponding pattern variables.
-}
bindPatternVars ::
    InnerLower r =>
    -- | The scrutinee expression and its type
    (IR.Expr, JVM.ClassInfoType) ->
    -- | The pattern alternative
    Core.Alt Core.Var ->
    Eff r ()
bindPatternVars (scrut, _) (Core.DataAlt con, args, _) = do
    let conClassName = JVM.ClassInfoType (qualifiedTextToClass con.name)
    let fieldTypes = Core.functionTypeArgs con.dataConType
    forM_ (zip3 args fieldTypes [0 ..]) $ \(argId, fieldType, index) -> do
        case argId of
            Core.Id (Local argUnique) _ _ -> do
                let fieldName = fieldNameForIndex index
                let fieldJVMType = lowerType fieldType -- Use constructor's actual field type
                emitInst $ IR.Assign argUnique fieldJVMType (IR.GetField scrut conClassName fieldName fieldJVMType)
            Core.Id (Global global) _ _ -> throwError $ GlobalVarInPattern global
            Core.TyVar _ -> throwError TypeVarInPattern
bindPatternVars _ _ = pass

-- | Simple way to break the circular dependency between lowering expressions and lowering matches
type ExprLoweringFn = forall r. InnerLower r => Core.CoreExpr -> Eff r IR.Expr

lowerMatch ::
    forall r.
    InnerLower r =>
    ExprLoweringFn ->
    Core.Type ->
    Core.CoreExpr ->
    Maybe Core.Var ->
    [Core.Alt Core.Var] ->
    Eff r IR.Expr
lowerMatch lowerExpr matchType scrutinee binder alts = do
    scrutVar <- lowerExpr scrutinee
    resultVarName <- freshVar
    let matchType' = lowerType matchType
    let resultVar = IR.LocalVar resultVarName matchType'

    mergeLabel <- makeUnique "match_merge"
    firstCheckLabel <- makeUnique "match_start"

    scrutineeType <- traceFn guesstimateExprType scrutinee
    buildDecisionLadder (scrutVar, fieldTypeToClassInfoType $ lowerType scrutineeType) resultVarName mergeLabel firstCheckLabel alts
    emitBlock (IR.Block mergeLabel [])

    pure resultVar
  where
    buildDecisionLadder ::
        (IR.Expr, _) -> Unique Text -> Unique Text -> Unique Text -> [Core.Alt Core.Var] -> Eff r ()
    buildDecisionLadder _ _ _ _ [] = pass
    buildDecisionLadder (scrut, scrutType) resultVarName mergeLabel checkLabel (alt : rest) = do
        branchLabel <- makeUnique "match_branch"
        let (con, _, bodyExpr) = alt
        bodyType <- lowerType <$> traceFn guesstimateExprType bodyExpr

        -- Determine next label
        (nextCheckLabel, failBlock) <- case rest of
            [] -> case con of
                Core.DEFAULT -> pure (Nothing, Nothing)
                _ -> do
                    failLabel <- makeUnique "match_fail"
                    let failBlk =
                            IR.Block
                                failLabel
                                [ IR.Assign
                                    resultVarName
                                    bodyType
                                    (IR.PrimOp IR.PatternMatchFailedError [])
                                ]
                    pure (Just failLabel, Just failBlk)
            _ -> do
                nextLabel <- makeUnique "match_next"
                pure (Just nextLabel, Nothing)

        -- Emit check block
        let checkInstrs = case con of
                Core.DEFAULT -> [IR.Jump branchLabel]
                _ -> case nextCheckLabel of
                    Nothing -> error "Non-default final alternative should have fail label"
                    Just nextLabel -> [IR.JumpIf (matchCond scrut alt) branchLabel nextLabel]

        emitBlock $ IR.Block checkLabel checkInstrs
        whenJust failBlock emitBlock

        -- Emit branch body
        (bodyResult, (bodyInstrs, bodyBlocks)) <- captureInstructions $ do
            bindPatternVars (scrut, scrutType) alt
            lowerExpr bodyExpr

        case bodyBlocks of
            [] -> do
                let branchBody =
                        bodyInstrs
                            ++ [ IR.Assign resultVarName bodyType bodyResult
                               , IR.Jump mergeLabel
                               ]
                emitBlock $ IR.Block branchLabel branchBody
            firstBlock : restBlocks -> do
                contLabel <- makeUnique "match_branch_cont"
                emitBlock $ IR.Block branchLabel (bodyInstrs ++ [IR.Jump (IR.blockLabel firstBlock)])
                emitBlock firstBlock
                mapM_ emitBlock restBlocks
                emitBlock $
                    IR.Block
                        contLabel
                        [ IR.Assign resultVarName bodyType bodyResult
                        , IR.Jump mergeLabel
                        ]

        -- Process remaining alternatives
        whenJust nextCheckLabel $ \nextLabel ->
            unless (null rest) $
                buildDecisionLadder (scrut, scrutType) resultVarName mergeLabel nextLabel rest

matchCond :: IR.Expr -> Core.Alt Core.Var -> IR.Expr
matchCond scrut (altCon, _, _) = case altCon of
    Core.LitAlt lit -> IR.BinaryOp IR.Equals scrut (lowerLiteral lit)
    Core.DataAlt dataCon ->
        let className = qualifiedTextToClass dataCon.name
         in IR.InstanceOf scrut (JVM.ObjectFieldType className)
    Core.DEFAULT -> IR.LitBool True
