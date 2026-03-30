module Elara.Parse (getParsedModuleQuery, getParsedFileQuery) where

import Effectful (Eff, inject)
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as Eff
import Elara.AST.Module (Module (..), Module' (..))
import Elara.AST.Name (ModuleName (..))
import Elara.AST.Phases.Frontend (Frontend)
import Elara.AST.Region
import Elara.Error (runErrorOrReport)
import Elara.Lexer.Token (Lexeme)
import Elara.Lexer.Utils (LexerError)
import Elara.Parse.Error
import Elara.Parse.Module (module')
import Elara.Parse.Primitives (Parser)
import Elara.Parse.Stream (TokenStream (..))
import Elara.Query (Query (GetFileContents, LexedFile, ModulePath))
import Elara.Query.Effects (ConsQueryEffects)
import Elara.ReadFile (FileContents (FileContents), ModulePathError)
import Rock (Rock, fetch)
import Text.Megaparsec (MonadParsec (eof), runParserT)
import Text.Megaparsec qualified as MP

moduleParser :: Parser (Module SourceRegion Frontend)
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
        (Module SourceRegion Frontend)
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
        (Module SourceRegion Frontend)
getParsedModuleQuery mn = do
    fp <- runErrorOrReport @ModulePathError $ fetch (ModulePath mn)
    parsed <- getParsedFileQuery fp
    let (Module _ modInner) = parsed
    let nameLoc = moduleName modInner
    let declaredName = nameLoc ^. unlocated
    -- Check if the module declaration is implicit (zero-width region with "Main")
    let isImplicit = case nameLoc ^. sourceRegion of
            RealSourceRegion r -> r ^. startPos == r ^. endPos && declaredName == ModuleName ("Main" :| [])
            GeneratedRegion _ -> True
    -- If explicit declaration exists and doesn't match, throw an error
    if not isImplicit && declaredName /= mn
        then do
            -- Reconstruct token stream to create a proper error bundle
            (FileContents filePath contents) <- runErrorOrReport $ fetch (GetFileContents fp)
            lexemes <- runErrorOrReport @LexerError $ fetch (LexedFile fp)
            let tokenStream = createTokenStream contents lexemes
            let offset = 0 -- Module declaration is typically at the start
            let parseError = MP.FancyError offset $ one $ MP.ErrorCustom $ ModuleNameMismatch mn nameLoc
            let posState = MP.PosState tokenStream offset (MP.initialPos filePath) MP.defaultTabWidth ""
            throwError $ WParseErrorBundle $ MP.ParseErrorBundle (one parseError) posState
        else do
            let (Module loc _) = parsed
            pure $ Module loc (modInner{moduleName = Located (nameLoc ^. sourceRegion) mn})

createTokenStream :: Text -> [Lexeme] -> TokenStream
createTokenStream i tokens = TokenStream i tokens False
