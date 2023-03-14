{-# LANGUAGE RecordWildCards #-}

module Elara.Parse.Stream where

import Control.Lens
import Data.Text qualified as T
import Elara.AST.Region (HasPath (path), Located, RealPosition (Position), RealSourceRegion (SourceRegion), generatedFileName, generatedSourcePos, sourceRegion, startPos, unlocated, _RealSourceRegion)
import Elara.Lexer.Lexer
import Elara.Lexer.Token
import Text.Megaparsec

data TokenStream = TokenStream
    { tokenStreamInput :: String
    , tokenStreamTokens :: [Lexeme]
    }

instance Stream TokenStream where
    type Token TokenStream = Lexeme
    type Tokens TokenStream = [Lexeme]
    tokenToChunk Proxy x = [x]
    tokensToChunk Proxy xs = xs
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    take1_ (TokenStream _ []) = Nothing
    take1_ (TokenStream str (t : ts)) = Just (t, TokenStream (drop (tokensLength (Proxy @TokenStream) (t :| [])) str) ts)

    takeN_ n (TokenStream str s)
        | n <= 0 = Just ([], TokenStream str s)
        | null s = Nothing
        | otherwise =
            let (x, s') = splitAt n s
             in case nonEmpty x of
                    Nothing -> Just (x, TokenStream str s')
                    Just nex -> Just (x, TokenStream (drop (tokensLength (Proxy @TokenStream) nex) str) s')
    takeWhile_ f (TokenStream str s) =
        let (x, s') = span f s
         in case nonEmpty x of
                Nothing -> (x, TokenStream str s')
                Just nex -> (x, TokenStream (drop (tokensLength (Proxy @TokenStream) nex) str) s')

instance VisualStream TokenStream where
    showTokens Proxy =
        toString
            . T.intercalate " "
            . toList
            . fmap (tokenRepr . view unlocated)
    tokensLength Proxy xs = sum (tokenLength <$> xs)

instance TraversableStream TokenStream where
    reachOffset o PosState{..} =
        ( Just (prefix ++ restOfLine)
        , PosState
            { pstateInput =
                TokenStream
                    { tokenStreamInput = postStr
                    , tokenStreamTokens = postLexemes
                    }
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = newSourcePos
            , pstateTabWidth = pstateTabWidth
            , pstateLinePrefix = prefix
            }
        )
      where
        prefix =
            if sameLine
                then pstateLinePrefix ++ preLine
                else preLine
        sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
        newSourcePos =
            case postLexemes of
                [] -> pstateSourcePos
                (x : _) -> sourceRegionToSourcePos x sourceRegion startPos
        (preLexemes, postLexemes) = splitAt (o - pstateOffset) (tokenStreamTokens pstateInput)
        (preStr, postStr) = splitAt tokensConsumed (tokenStreamInput pstateInput)
        preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
        tokensConsumed =
            case nonEmpty preLexemes of
                Nothing -> 0
                Just nePre -> tokensLength (Proxy @TokenStream) nePre
        restOfLine = takeWhile (/= '\n') postStr

sourceRegionToSourcePos :: HasPath a1 => Located a2 -> ((a1 -> Const (Maybe FilePath) a1) -> Located a2 -> Const (Maybe FilePath) (Located a2)) -> ((RealPosition -> Const (First RealPosition) RealPosition) -> RealSourceRegion -> Const (First RealPosition) RealSourceRegion) -> SourcePos
sourceRegionToSourcePos sr l which = do
    let fp = view (l . path) sr
    case preview (sourceRegion . _RealSourceRegion . which) sr of
        Just pos -> realPositionToSourcePos fp pos
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