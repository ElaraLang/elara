module Elara.Parse where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as Eff
import Elara.AST.Module (Module)
import Elara.AST.Name
import Elara.AST.Select
import Elara.Error (runErrorOrReport)
import Elara.Lexer.Token (Lexeme)
import Elara.Lexer.Utils (LexerError)
import Elara.Parse.Error
import Elara.Parse.Module (module')
import Elara.Parse.Primitives (Parser)
import Elara.Parse.Stream (TokenStream (..))
import Elara.Query (Query (GetFileContents, LexedFile, ModulePath))
import Elara.Query.Effects (ConsQueryEffects)
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
        ( ConsQueryEffects
            '[ Eff.Error (WParseErrorBundle TokenStream ElaraParseError)
             , Rock.Rock Elara.Query.Query
             ]
        )
        (Module 'Frontend)
getParsedFileQuery fp = do
    (FileContents filePath contents) <- fetch (GetFileContents fp)
    lexemes <- runErrorOrReport @LexerError $ fetch (LexedFile fp)
    let tokenStream = createTokenStream contents lexemes
    let parseResult = runParser moduleParser filePath tokenStream
    let firstError = first WParseErrorBundle parseResult
    case firstError of
        Left err -> throwError err
        Right mod -> pure mod

getParsedModuleQuery ::
    ModuleName ->
    Eff
        ( ConsQueryEffects
            '[ Eff.Error (WParseErrorBundle TokenStream ElaraParseError)
             , Rock.Rock Elara.Query.Query
             ]
        )
        (Module 'Frontend)
getParsedModuleQuery mn = do
    fp <- fetch (ModulePath mn)
    getParsedFileQuery fp

type ParsePipelineEffects = '[Error (WParseErrorBundle TokenStream ElaraParseError)]

createTokenStream :: Text -> [Lexeme] -> TokenStream
createTokenStream i tokens = TokenStream i tokens False
