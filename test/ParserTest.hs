module ParserTest where

import Parse.AST
import Parse.Parser (parse)
import System.Exit (exitFailure)
import Test.Hspec

spec :: Spec
spec = describe "Test Parser" $ do
  it "Parses integers" $ do
    "123" <=> ConstE (IntC 123)
  it "Parses Strings" $ do
    "\"hello\"" <=> ConstE (StringC "hello")
  it "Parses Empty Strings" $ do
    "\"\"" <=> ConstE (StringC "")
  it "Parses Unit" $ do
    "()" <=> ConstE UnitC
  it "Parses Empty List" $ do
    "[]" <=> ListE []
  it "Parses a singleton list" $ do
    "[1]" <=> ListE [ConstE (IntC 1)]
  it "Parses a list" $ do
    "[1, 2,3]" <=> ListE [ConstE (IntC 1), ConstE (IntC 2), ConstE (IntC 3)]
  it "Parses a list with a nested list" $ do
    "[1, [2, 3]]" <=> ListE [ConstE (IntC 1), ListE [ConstE (IntC 2), ConstE (IntC 3)]]
  it "Parses a list with a nested list and a string" $ do
    "[1, [2, \"hello\"]]" <=> ListE [ConstE (IntC 1), ListE [ConstE (IntC 2), ConstE (StringC "hello")]]

  it "Parses an If expression" $ do
    "if 1 then 2 else 3" <=> IfElseE (ConstE (IntC 1)) (ConstE (IntC 2)) (ConstE (IntC 3))
  it "Parses a nested If expression" $ do
    "if 1 then if 2 then 3 else 4 else 5" <=> IfElseE (ConstE (IntC 1)) (IfElseE (ConstE (IntC 2)) (ConstE (IntC 3)) (ConstE (IntC 4))) (ConstE (IntC 5))

  it "Parses a parenthesized expression" $ do
    "(1)" <=> ConstE (IntC 1)

  it "Parses an infix expression" $ do
    "1 + 2" <=> InfixApplicationE (OpIdentifier "+") (ConstE (IntC 1)) (ConstE (IntC 2))
  it "Parses an infix expression with a nested expression" $ do
    "1 + (2 + 3)" <=> InfixApplicationE (OpIdentifier "+") (ConstE (IntC 1)) (InfixApplicationE (OpIdentifier "+") (ConstE (IntC 2)) (ConstE (IntC 3)))

  it "Parses a function application" $ do
    "f 1" <=> FuncApplicationE (IdentifierE (NormalIdentifier "f")) (ConstE (IntC 1))
  it "Parses a function application with an infix expression" $ do
    "f 1 + 2" <=> InfixApplicationE (OpIdentifier "+") (FuncApplicationE (IdentifierE (NormalIdentifier "f")) (ConstE (IntC 1))) (ConstE (IntC 2))
  it "Parses an infix expression with a function on the RHS" $ do
    "n * (f (n - 1))" <=> InfixApplicationE (OpIdentifier "*") (IdentifierE (NormalIdentifier "n")) (FuncApplicationE (IdentifierE (NormalIdentifier "f")) (InfixApplicationE (OpIdentifier "-") (IdentifierE (NormalIdentifier "n")) (ConstE (IntC 1))))

  it "Parses a normal variable binding" $ do
    "let x = 1" <=> LetE (IdentifierP (NormalIdentifier "x")) (ConstE (IntC 1))
  it "Parses a variable binding with an infix expression" $ do
    "let x = 1 + 2" <=> LetE (IdentifierP (NormalIdentifier "x")) (InfixApplicationE (OpIdentifier "+") (ConstE (IntC 1)) (ConstE (IntC 2)))
  it "Parses a variable binding with a function application" $ do
    "let x = f 1" <=> LetE (IdentifierP (NormalIdentifier "x")) (FuncApplicationE (IdentifierE (NormalIdentifier "f")) (ConstE (IntC 1)))

  it "Parses a function definition" $ do
    "let f x = x" <=> LetE (FunctionP (NormalIdentifier "f") [IdentifierP $ NormalIdentifier "x"]) (IdentifierE (NormalIdentifier "x"))
  it "Parses a function definition with an infix expression" $ do
    "let f x = x + 1" <=> LetE (FunctionP (NormalIdentifier "f") [IdentifierP $ NormalIdentifier "x"]) (InfixApplicationE (OpIdentifier "+") (IdentifierE (NormalIdentifier "x")) (ConstE (IntC 1)))
  it "Parses a function definition with 2 parameters" $ do
    "let f x y = x + y" <=> LetE (FunctionP (NormalIdentifier "f") [IdentifierP $ NormalIdentifier "x", IdentifierP $ NormalIdentifier "y"]) (InfixApplicationE (OpIdentifier "+") (IdentifierE (NormalIdentifier "x")) (IdentifierE (NormalIdentifier "y")))

  it "Parses a function definition with an indented body" $ do
    "let f x = \n\tx" <=> LetE (FunctionP (NormalIdentifier "f") [IdentifierP $ NormalIdentifier "x"]) (IdentifierE (NormalIdentifier "x"))
  it "Parses a function definition with a block body" $ do
    "let f x =\n    x\n    x\n" <=> LetE (FunctionP (NormalIdentifier "f") [IdentifierP $ NormalIdentifier "x"]) (BlockE [IdentifierE (NormalIdentifier "x"), IdentifierE (NormalIdentifier "x")])

  it "Parses a function definition with a cons pattern argument" $ do
    "let f (x:xs) = x" <=> LetE (FunctionP (NormalIdentifier "f") [ConsP (IdentifierP $ NormalIdentifier "x") (IdentifierP $ NormalIdentifier "xs")]) (IdentifierE (NormalIdentifier "x"))
    
  it "Parses cons operator precedence correctly" $ do
    "f x : map f xs" <=> ConsE (FuncApplicationE (IdentifierE (NormalIdentifier "f")) (IdentifierE (NormalIdentifier "x"))) (FuncApplicationE (FuncApplicationE (IdentifierE $ NormalIdentifier "map") (IdentifierE $ NormalIdentifier "f")) (IdentifierE (NormalIdentifier "xs")))

infix 4 <=>

(<=>) :: String -> Expression -> IO ()
(<=>) expr ast = case parse expr of
  [ExpressionL ast'] -> ast' `shouldBe` ast
  _ -> exitFailure
