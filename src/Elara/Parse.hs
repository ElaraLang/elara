module Elara.Parse (getParsedModuleQuery, getParsedFileQuery) where

import Effectful (Eff, inject, (:>))
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
import Rock (Rock, fetch)
import Text.Megaparsec (MonadParsec (eof), runParserT)

moduleParser :: Parser (Module Frontend)
moduleParser = module' <* eof

getParsedFileQuery ::
    HasCallStack =>
    FilePath ->
    Eff
        ( ConsQueryEffects
            '[ Eff.Error (WParseErrorBundle TokenStream ElaraParseError)
             , Rock.Rock Elara.Query.Query
             ]
        )
        (Module Frontend)
getParsedFileQuery fp = do
    (FileContents filePath contents) <- runErrorOrReport $ fetch (GetFileContents fp)
    lexemes <- runErrorOrReport @LexerError $ fetch (LexedFile fp)
    let tokenStream = createTokenStream contents lexemes
    parseResult <- inject $ runParserT moduleParser filePath tokenStream
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
        (Module Frontend)
getParsedModuleQuery mn = do
    fp <- fetch (ModulePath mn)
    getParsedFileQuery fp

createTokenStream :: Text -> [Lexeme] -> TokenStream
createTokenStream i tokens = TokenStream i tokens False
