module Lib
  ( someFunc,
  )
where

import Parse.Lexer
import Parse.Parser
import Control.Monad (forM_)

someFunc :: IO ()
someFunc = do
  content <- readFile "source.elr"


  let lines = parse content
  forM_ lines $ \(ExpressionL ast) -> do
    putStrLn $ showASTNode ast

showASTNode :: Expression -> String
showASTNode (FuncApplicationE a b) = "(" ++ showASTNode a ++ " " ++ showASTNode b ++ ")"
showASTNode (LetE pattern value) = "let " ++ showPattern pattern ++ " = " ++ showASTNode value
showASTNode (IdentifierE i) = showIdentifier i
showASTNode (ConstE const) = showConst const
showASTNode a = show a

showConst :: Constant -> String
showConst (StringC s) = s

showIdentifier (NormalIdentifier i) = i
showIdentifier (OpIdentifier i) = i

showPattern (IdentifierP p) = showIdentifier p
