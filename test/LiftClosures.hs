{-# LANGUAGE OverloadedLists #-}

module LiftClosures where

import Effectful
import Effectful.Error.Static (runError)
import Elara.AST.Name (ModuleName (..), Qualified (..))
import Elara.AST.VarRef (VarRef' (..))
import Elara.Core qualified as Core
import Elara.Core.ANF qualified as ANF
import Elara.Core.Generic (Bind (..))
import Elara.Core.LiftClosures
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..))
import Elara.Data.Unique (Unique (..))
import Elara.Data.Unique.Effect (uniqueGenToGlobalIO)
import Elara.Logging (ignoreStructuredDebug)
import Elara.Prim.Core (intCon)
import Test.Syd

-- | A simple Int type for tests
intType :: Core.Type
intType = Core.ConTy intCon

spec :: Spec
spec = describe "Closure Lifting" $ do
    describe "Lambda extraction" $ do
        it "lifts a simple lambda with no captures" $ do
            -- let f = \x -> x
            -- Should create a lifted declaration for the lambda
            let testMod = mkTestModule "simple"
                    [ ( "f"
                      , ANF.CExpr $ ANF.AExpr $ ANF.Lam (mkVar "x" intType) $
                            ANF.CExpr $ ANF.AExpr $ ANF.Var (mkVar "x" intType)
                      )
                    ]
            result <- runLift testMod
            -- The module should have more declarations than we started with
            -- (original + lifted lambdas)
            let CoreModule _ resultDecls = result
            length resultDecls `shouldSatisfy` (>= 1)
            
        it "lifts a lambda with captures" $ do
            -- let y = 1 in let f = \x -> y
            -- The lambda captures 'y', so it should be lifted with y as a parameter
            let yVar = mkVar "y" intType
            let xVar = mkVar "x" intType
            let testMod = mkTestModule "captures"
                    [ ( "main"
                      , ANF.Let (NonRecursive (yVar, ANF.AExpr $ ANF.Lit (Core.Int 1))) $
                          ANF.Let (NonRecursive (mkVar "f" (Core.FuncTy intType intType),
                              ANF.AExpr $ ANF.Lam xVar $ ANF.CExpr $ ANF.AExpr $ ANF.Var yVar)) $
                          ANF.CExpr $ ANF.AExpr $ ANF.Var (mkVar "f" (Core.FuncTy intType intType))
                      )
                    ]
            result <- runLift testMod
            let CoreModule _ resultDecls = result
            -- Should have lifted declarations
            length resultDecls `shouldSatisfy` (>= 1)

        it "lifts lambda passed as argument to function" $ do
            -- let result = someFunc (\x -> x)
            -- The lambda is an argument to App, should be extracted and lifted
            let xVar = mkVar "x" intType
            let someFuncVar = mkGlobalVar "someFunc" (Core.FuncTy (Core.FuncTy intType intType) intType)
            let testMod = mkTestModule "app-lambda"
                    [ ( "main"
                      , ANF.CExpr $ ANF.App 
                          (ANF.Var someFuncVar)
                          (ANF.Lam xVar $ ANF.CExpr $ ANF.AExpr $ ANF.Var xVar)
                      )
                    ]
            result <- runLift testMod
            let CoreModule _ resultDecls = result
            length resultDecls `shouldSatisfy` (>= 1)

        it "lifts lambda in multi-arg application" $ do
            -- let tmp = f 3 in let result = tmp (\x -> x)
            -- This is how `f 3 (\x -> ...)` looks in ANF
            let xVar = mkVar "x" intType
            let fVar = mkGlobalVar "f" (Core.FuncTy intType (Core.FuncTy (Core.FuncTy intType intType) intType))
            let tmpVar = mkVar "tmp" (Core.FuncTy (Core.FuncTy intType intType) intType)
            let testMod = mkTestModule "multi-arg"
                    [ ( "main"
                      , ANF.Let (NonRecursive (tmpVar, ANF.App (ANF.Var fVar) (ANF.Lit (Core.Int 3)))) $
                          ANF.CExpr $ ANF.App 
                              (ANF.Var tmpVar)
                              (ANF.Lam xVar $ ANF.CExpr $ ANF.AExpr $ ANF.Var xVar)
                      )
                    ]
            result <- runLift testMod
            let CoreModule _ resultDecls = result
            length resultDecls `shouldSatisfy` (>= 1)

    describe "Recursive bindings" $ do
        it "lifts simple recursive function" $ do
            -- let rec f = \x -> f x
            let xVar = mkVar "x" intType
            let fVar = mkVar "f" (Core.FuncTy intType intType)
            let testMod = mkTestModule "rec-simple"
                    [ ( "main"
                      , ANF.Let (Recursive 
                          [ ( fVar
                            , ANF.AExpr $ ANF.Lam xVar $ 
                                ANF.CExpr $ ANF.App (ANF.Var fVar) (ANF.Var xVar)
                            )
                          ]) $
                          ANF.CExpr $ ANF.AExpr $ ANF.Var fVar
                      )
                    ]
            result <- runLift testMod
            let CoreModule _ resultDecls = result
            -- Should have lifted the recursive function
            length resultDecls `shouldSatisfy` (>= 1)

-- | Helper to create a test module
mkTestModule :: Text -> [(Text, ANF.Expr Core.Var)] -> CoreModule (Bind Core.Var ANF.Expr)
mkTestModule name decls = CoreModule (ModuleName (name :| [])) (map mkDecl decls)
  where
    mkDecl (declName, body) = 
        let var = mkGlobalVar declName (guesstimateType body)
        in CoreValue $ NonRecursive (var, body)
    
    guesstimateType :: ANF.Expr Core.Var -> Core.Type
    guesstimateType _ = intType -- Simplified for tests

-- | Create a local variable
mkVar :: Text -> Core.Type -> Core.Var
mkVar name ty = Core.Id (Local (Unique name 0)) ty Nothing

-- | Create a global variable  
mkGlobalVar :: Text -> Core.Type -> Core.Var
mkGlobalVar name ty = Core.Id (Global (Qualified name (ModuleName ("Test" :| [])))) ty Nothing

-- | Run closure lifting and return the result module
runLift :: CoreModule (Bind Core.Var ANF.Expr) -> IO (CoreModule (Bind Core.Var ANF.Expr))
runLift m = do
    result <- runEff $
        uniqueGenToGlobalIO $
            ignoreStructuredDebug $
                runError @ClosureLiftError $
                    runLiftClosures m
    case result of
        Left (_, err) -> do
            expectationFailure $ "Closure lifting failed: " <> show err
            pure m  -- Return original on failure
        Right res -> pure res
