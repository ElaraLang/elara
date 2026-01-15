{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Elara.Parse.Stream where

import Data.Text qualified as T
import Elara.AST.Region (HasPath (path), Located (Located), RealPosition (Position), generatedFileName, generatedSourcePos, sourceRegion, startPos, unlocated, _RealSourceRegion)
import Elara.Lexer.Token
import Text.Megaparsec

data TokenStream = TokenStream
    { tokenStreamInput :: !Text
    -- ^ The full, original input text. This never changes throughout parsing.
    , tokenStreamTokens :: ![Lexeme]
    -- ^ The list of tokens remaining to be parsed
    , skipIndents :: Bool
    -- ^ Whether to skip Indent tokens encountered during parsing
    }
    deriving (Show, Eq)

pattern L :: a -> Located a
pattern L i <- Located _ i

instance Stream TokenStream where
    type Token TokenStream = Lexeme
    type Tokens TokenStream = [Lexeme]
    tokenToChunk Proxy x = [x]
    tokensToChunk Proxy xs = xs
    chunkToTokens Proxy = identity
    chunkLength Proxy = length
    chunkEmpty Proxy = null

    take1_ stream@(TokenStream _str tokens skip)
        | skip = case dropWhile (isIndent . view unlocated) tokens of
            [] -> Nothing
            (t : ts) -> Just (t, stream{tokenStreamTokens = ts})
        | otherwise = case tokens of
            [] -> Nothing
            (t : ts) -> Just (t, stream{tokenStreamTokens = ts})

    takeN_ n (TokenStream str s skipIndents)
        | n <= 0 = Just ([], TokenStream str s skipIndents)
        | null s = Nothing
        | otherwise =
            let (x, s') = takeWhile_ (const True) (TokenStream str s skipIndents)
             in case takeN_ (n - length x) s' of
                    Nothing -> Nothing
                    Just (xs, s'') -> Just (x ++ xs, s'')

    takeWhile_ f (TokenStream str s skipIndents) =
        let (x, s') = span f s
         in (x, TokenStream str s' skipIndents) -- Again, preserve 'str'

instance VisualStream TokenStream where
    showTokens Proxy =
        toString
            . T.intercalate " "
            . toList
            . fmap (tokenRepr . view unlocated)
    tokensLength Proxy xs = sum (tokenLength <$> xs)

instance TraversableStream TokenStream where
    -- Since we have the full text and tokens with absolute positions,
    -- we can implement reachOffset by simple lookup rather than incremental slicing.
    reachOffset o PosState{..} =
        ( Just (toString lineStr)
        , PosState
            { pstateInput =
                TokenStream
                    { tokenStreamInput = fullText
                    , tokenStreamTokens = postLexemes
                    , skipIndents = pstateInput.skipIndents
                    }
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = newSourcePos
            , pstateTabWidth = pstateTabWidth
            , pstateLinePrefix = toString prefixStr
            }
        )
      where
        -- split into tokens before and after offset o
        (preLexemes, postLexemes) = splitAt (o - pstateOffset) (tokenStreamTokens pstateInput)

        -- determine new source position, based on first token after offset,
        newSourcePos = case postLexemes of
            (x : _) -> sourceRegionToSourcePos x -- first token after offset
            [] -> case viaNonEmpty last preLexemes of
                Just lastTok -> sourceRegionToSourcePos lastTok -- fallback to last token
                Nothing -> pstateSourcePos

        fullText = tokenStreamInput pstateInput
        lineIndex = unPos (sourceLine newSourcePos) - 1
        allLines = lines fullText

        lineStr = fromMaybe "" (allLines !!? lineIndex)

        col = unPos (sourceColumn newSourcePos)
        prefixStr = T.take (col - 1) lineStr

sourceRegionToSourcePos :: Located a -> SourcePos
sourceRegionToSourcePos loc =
    let fp = view path loc
        pos = loc ^? sourceRegion % _RealSourceRegion % startPos
     in case pos of
            Just p -> realPositionToSourcePos fp p
            Nothing -> generatedSourcePos fp

realPositionToSourcePos :: Maybe FilePath -> RealPosition -> SourcePos
realPositionToSourcePos fp (Position line column) =
    SourcePos
        { sourceName = fromMaybe generatedFileName fp
        , sourceLine = mkPos line
        , sourceColumn = mkPos column
        }
tokenLength :: Lexeme -> Int
tokenLength = T.length . tokenRepr . view unlocated
