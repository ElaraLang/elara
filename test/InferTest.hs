module InferTest where

import qualified Data.Map as M
import qualified Interpreter.AST as A
import Parse.Parser (parse)
import Preprocess.Preprocessor (preprocessAll)
import System.Exit (exitFailure)
import Test.Hspec
import TypeInfer.Env (Scheme (..), TVar (..), Type (..), TypeEnv (..), baseEnv)
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
  it "Infers the type of a nested let expression" $ do
    "let a = 1 in let b = a in b" <=> Forall [] (TCon "Int")
  it "Infers the type of a lambda expression" $ do
    "\\x -> x" <=> Forall [TV "a"] (TFunc (TVariable $ TV "a") (TVariable $ TV "a"))
  it "Infers the type of a simple application" $ do
    "(\\x -> x) 1" <=> Forall [] (TCon "Int")
  it "Infers the type of an impure function" $ do
    "\\_ -> println 3" <=> Forall [TV "a"] (TImpure (TFunc (TVariable $ TV "a") (TCon "()")))
  it "Infers the type of an impure function that does 2 things" $ do
    "\\_ -> { println 3; println 4 }" <=> Forall [TV "a"] (TImpure (TFunc (TVariable $ TV "a") (TCon "()")))

  it "Infers the type of an impure function that returns a pure function" $ do
    "\\_ -> { println 3; \\x -> x }" <=> Forall [TV "a", TV "b"] (TImpure (TFunc (TVariable $ TV "a") (TFunc (TVariable $ TV "b") (TVariable $ TV "b"))))
  it "Infers the type of an impure function that returns an impure function" $ do
    "\\_ -> { println 3; \\x -> println x }" <=> Forall [TV "a", TV "b"] (TImpure (TFunc (TVariable $ TV "a") (TImpure (TFunc (TVariable $ TV "b") (TCon "()")))))
  it "Infers the type of an impure function that returns a normal value" $ do
    "\\x -> { println x; x }" <=> Forall [TV "a"] (TImpure (TFunc (TVariable $ TV "a") (TVariable $ TV "a")))

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
