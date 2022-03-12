module InferTest where

import qualified Data.Map as M
import qualified Interpreter.AST as A
import Parse.Parser (parse)
import Preprocess.Preprocessor (preprocessAll)
import System.Exit (exitFailure)
import Test.Hspec
import TypeInfer.Env (Scheme (..), Type (..), TypeEnv (..), baseEnv)
import TypeInfer.Infer

spec :: Spec
spec = describe "Test Type Inference" $ do
  it "Infers the type of an integer literal" $ do
    "1" <=> Forall [] (TCon "Int")
  it "Infers the type of a string literal" $ do
    "\"Hello\"" <=> Forall [] (TCon "String")
  it "Infers the type of a unit value" $ do
    "()" <=> Forall [] (TCon "()")
  it "Infers the type of a simple let expression" $ do
    "let a = 1 in a" <=> Forall [] (TCon "Int")
  it "Infers the type of a simple let binding" $ do
    "let a = 1" <!> TypeEnv (M.fromList [("a", Forall [] (TCon "Int"))])

(<=>) :: String -> Scheme -> IO ()
(<=>) expr ast = case preprocessAll (parse expr) of
  [A.ExpressionLine l] -> case inferExpr l baseEnv of
    Left err -> expectationFailure (show err)
    Right (_, ty) -> ty `shouldBe` ast
  _ -> exitFailure

(<!>) :: String -> TypeEnv -> IO ()
(<!>) expr (TypeEnv env) = case preprocessAll (parse expr) of
  [A.ExpressionLine l] -> case inferExpr l baseEnv of
    Left err -> expectationFailure (show err)
    Right (TypeEnv env', _) -> env' `shouldSatisfy` M.isSubmapOf env
  _ -> exitFailure
