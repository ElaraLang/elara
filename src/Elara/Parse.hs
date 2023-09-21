module Elara.Parse where

import Elara.AST.Module (Module)
import Elara.AST.Select
import Elara.Error (runErrorOrReport)
import Elara.Lexer.Pipeline (LexPipelineEffects, runLexPipeline)
import Elara.Lexer.Token (Lexeme)
import Elara.Parse.Error
import Elara.Parse.Module (module')
import Elara.Parse.Primitives (HParser, IsParser (fromParsec), Parser, toParsec)
import Elara.Parse.Stream (TokenStream (..))
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Polysemy
import Polysemy.Error
import Text.Megaparsec (MonadParsec (eof), runParser)

parseModule :: FilePath -> TokenStream -> Either (WParseErrorBundle TokenStream ElaraParseError) (Module 'Frontend)
parseModule y = first WParseErrorBundle . runParser (toParsec module' <* eof) y

moduleParser :: HParser (Module 'Frontend)
moduleParser = module' <* fromParsec eof

parse :: (Members ParsePipelineEffects r) => HParser a -> FilePath -> TokenStream -> Sem r a
parse p path = fromEither . first WParseErrorBundle . runParser (toParsec p <* eof) path

type ParsePipelineEffects = '[Error (WParseErrorBundle TokenStream ElaraParseError)]

createTokenStream :: String -> [Lexeme] -> TokenStream
createTokenStream source lexemes = TokenStream source lexemes 0

parsePipeline ::
  (Members ParsePipelineEffects r) =>
  HParser a ->
  FilePath ->
  [Lexeme] ->
  Sem r a
parsePipeline parser fp lexemes =
  parse parser fp $ createTokenStream fp lexemes

-- | Interpret a result of 'parsePipeline' in terms of the common effects
runParsePipeline :: (IsPipeline r) => Sem (EffectsAsPrefixOf ParsePipelineEffects r) a -> Sem r a
runParsePipeline = runErrorOrReport @(WParseErrorBundle TokenStream ElaraParseError)

runParsePipelinePure ::
  Sem (EffectsAsPrefixOf ParsePipelineEffects r) a ->
  Sem r (Either (WParseErrorBundle TokenStream ElaraParseError) a)
runParsePipelinePure = runError @(WParseErrorBundle TokenStream ElaraParseError)
