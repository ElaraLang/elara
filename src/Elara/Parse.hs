module Elara.Parse where

import Effectful (Eff, inject, (:>))
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as Eff
import Effectful.FileSystem (FileSystem)
import Elara.AST.Module (Module)
import Elara.AST.Name
import Elara.AST.Select
import Elara.Data.Pretty
import Elara.Error (SomeReportableError, runErrorOrReport, runErrorOrReportEff)
import Elara.Error.EffectNew (DiagnosticWriter)
import Elara.Lexer.Token (Lexeme)
import Elara.Parse.Error
import Elara.Parse.Module (module')
import Elara.Parse.Primitives (Parser)
import Elara.Parse.Stream (TokenStream (..))
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Elara.Query (Query (GetFileContents, LexedFile, ModulePath))
import Elara.ReadFile (FileContents (FileContents))
import Polysemy
import Polysemy.Error
import Rock (Rock, fetch)
import Text.Megaparsec (MonadParsec (eof), runParser)

parseModule :: FilePath -> TokenStream -> Either (WParseErrorBundle TokenStream ElaraParseError) (Module 'Frontend)
parseModule y = first WParseErrorBundle . runParser (module' <* eof) y

moduleParser :: Parser (Module 'Frontend)
moduleParser = module' <* eof

parse :: Members ParsePipelineEffects r => Parser a -> FilePath -> TokenStream -> Sem r a
parse p path = fromEither . first WParseErrorBundle . runParser p path

-- parseEff :: Parser a -> FilePath -> TokenStream -> Eff _ a
-- parseEff p path stream = _ . inject $ runParser p path

getParsedFileQuery ::
    FilePath ->
    Eff
        '[ FileSystem
         , Rock Query
         , Eff.Error SomeReportableError
         , DiagnosticWriter (Doc AnsiStyle)
         , Eff.Error (WParseErrorBundle TokenStream ElaraParseError)
         ]
        (Module 'Frontend)
getParsedFileQuery fp = do
    (FileContents filePath contents) <- fetch (GetFileContents fp)
    lexemes <- runErrorOrReportEff $ fetch (LexedFile fp)
    let tokenStream = createTokenStream contents lexemes
    let parseResult = runParser moduleParser filePath tokenStream
    let firstError = first WParseErrorBundle parseResult
    case firstError of
        Left err -> throwError err
        Right mod -> pure mod

getParsedModuleQuery :: ModuleName -> Eff '[FileSystem, Rock Query, Eff.Error SomeReportableError, DiagnosticWriter (Doc AnsiStyle), Eff.Error (WParseErrorBundle TokenStream ElaraParseError)] (Module 'Frontend)
getParsedModuleQuery mn = do
    fp <- fetch (ModulePath mn)
    getParsedFileQuery fp

type ParsePipelineEffects = '[Error (WParseErrorBundle TokenStream ElaraParseError)]

createTokenStream :: Text -> [Lexeme] -> TokenStream
createTokenStream i tokens = TokenStream i tokens False

parsePipeline ::
    Members ParsePipelineEffects r =>
    Parser a ->
    FilePath ->
    (Text, [Lexeme]) ->
    Sem r a
parsePipeline parser fp (fileContents, lexemes) =
    parse parser fp $ createTokenStream fileContents lexemes

-- | Interpret a result of 'parsePipeline' in terms of the common effects
runParsePipeline :: IsPipeline r => Sem (EffectsAsPrefixOf ParsePipelineEffects r) a -> Sem r a
runParsePipeline = runErrorOrReport @(WParseErrorBundle TokenStream ElaraParseError)

runParsePipelinePure ::
    Sem (EffectsAsPrefixOf ParsePipelineEffects r) a ->
    Sem r (Either (WParseErrorBundle TokenStream ElaraParseError) a)
runParsePipelinePure = runError @(WParseErrorBundle TokenStream ElaraParseError)
