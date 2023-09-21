{-# LANGUAGE UndecidableInstances #-}

module Elara.Lexer.Pipeline where

import Elara.Error (runErrorOrReport)
import Elara.Lexer.Reader (readTokens, readTokensWith)
import Elara.Lexer.Token (Lexeme)
import Elara.Lexer.Utils (LexerError, initialState)
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline, PipelineResultEff)
import Elara.ReadFile
import Polysemy
import Polysemy.Error

type LexPipelineEffects = '[Error LexerError]

runLexPipeline :: (IsPipeline r) => Sem (EffectsAsPrefixOf LexPipelineEffects r) a -> Sem r a
runLexPipeline =
  runErrorOrReport @LexerError

runLexPipelinePure :: Sem (EffectsAsPrefixOf LexPipelineEffects r) a -> Sem r (Either LexerError a)
runLexPipelinePure =
  runError @LexerError
