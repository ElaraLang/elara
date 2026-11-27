module LSP (spec) where

import Effectful (runEff)
import Effectful.Temporary (runTemporary, withSystemTempDirectory)
import Hedgehog (Gen, annotateShow, assert, forAll, property, (===))
import NeatInterpolation (text)
import Prettyprinter (Doc, hardline, (<+>))
import System.FilePath ((</>))
import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.LSP.Protocol.Types qualified as LSP

import Common ((<=>))
import Elara.AST.Region (RealPosition (..), RealSourceRegion (..), SourceRegion (..))
import Elara.Data.Pretty (Ann)
import Elara.LSP (snapshotDiagnostics)
import Elara.LSP.Check (FileSnapshot (..), checkDocument)
import Elara.LSP.Markdown (docToMarkdown)
import Elara.LSP.Position
import Elara.Settings (CompilerSettings (..), defaultSettings)

import Elara.Data.Pretty.Styles qualified as Style

spec :: Spec
spec = do
    describe "Position conversion" $ do
        examples
        properties
    describe "Checking documents" checking
    describe "Rendering documents as Markdown" markdown

-- | Characters of a variety of UTF-16 widths
genChar :: Gen Char
genChar =
    Gen.frequency
        [ (6, Gen.element ['a', 'b', 'x', ' '])
        , (1, Gen.element ['\t', '\r']) -- ordinary columns, as far as the lexer is concerned
        , (2, Gen.element ['é', 'λ', '日']) -- multi-byte in UTF-8, but one-byte in UTF-16
        , (2, Gen.element ['😀', '\x1F680', '\x10FFFF']) -- surrogate pairs in UTF-16
        ]

genLines :: Gen [Text]
genLines = Gen.list (Range.linear 1 6) (toText <$> Gen.list (Range.linear 0 8) genChar)

-- | Every position inside the text
allPositions :: [Text] -> [RealPosition]
allPositions ls = concatMap (\(n, l) -> Position n <$> [1 .. Text.length l + 1]) (zip [1 ..] ls)

-- | Arbitrary text with at least two distinct positions, and two distinct positions in it
genTextAndTwoPositions :: Gen ([Text], RealPosition, RealPosition)
genTextAndTwoPositions = do
    ls <- Gen.filter ((>= 2) . length . allPositions) genLines
    shuffled <- Gen.shuffle (allPositions ls)
    case shuffled of
        (a : b : _) -> pure (ls, a, b)
        _ -> Gen.discard -- unreachable

-- | The text of a 1-based line number, or empty if there is no such line
lineAt :: [Text] -> Int -> Text
lineAt ls n = fromMaybe "" (ls !!? (n - 1))

properties :: Spec
properties = describe "Properties" $ do
    it "lspPositionToElara . toLspPosition = id, for every position inside the text" $ property $ do
        ls <- forAll genLines
        p <- forAll (Gen.element (allPositions ls))
        let text = unlines ls
        lspPositionToElara text (toLspPosition ls p) === p

    it "toLspPosition . lspPositionToElara = id, for every LSP position that is inside the text and a valid Elara position" $ property $ do
        ls <- forAll genLines
        -- Generate via an Elara position so that we only generate LSP positions that actually can become Elara positions
        lspPos <- toLspPosition ls <$> forAll (Gen.element (allPositions ls))
        let text = unlines ls
        toLspPosition ls (lspPositionToElara text lspPos) === lspPos

    it "regionToRange keeps both ends of a non-empty region recoverable" $ property $ do
        (ls, start, end) <- forAll genTextAndTwoPositions
        let (lo, hi) = (min start end, max start end)
            text = unlines ls
            LSP.Range s e = regionToRange text (RealSourceRegion (UnsafeMkSourceRegion (Just "test.elr") lo hi))
        (lspPositionToElara text s, lspPositionToElara text e) === (lo, hi)

    it "normalises arbitrary LSP positions to a valid position idempotently" $ property $ do
        generated <- forAll genLines
        let text = unlines generated
            -- 'unlines' ends the text with a newline, which starts one more (empty) line than we generated
            ls = Text.splitOn "\n" text
        -- deliberately generate positions beyond the end of lines and the file
        lspLine <- forAll $ Gen.int (Range.constant 0 (length ls + 2))
        lspChar <- forAll $ Gen.int (Range.constant 0 20)
        let lspPos = LSP.Position (fromIntegral lspLine) (fromIntegral lspChar)
            normalised@(Position ln col) = lspPositionToElara text lspPos
        annotateShow normalised

        -- position must be inside the text
        assert (ln >= 1 && ln <= length ls)
        assert (col >= 1 && col <= Text.length (lineAt ls ln) + 1)

        -- must only round backwards or clamp
        let LSP.Position backLine backChar = toLspPosition ls normalised
            lineLen = fromIntegral (utf16Length (lineAt ls ln)) :: Int
            backChar' = fromIntegral backChar :: Int
        assert (backChar' <= lspChar || lspLine >= length ls)
        assert (lspChar - backChar' <= 1 || lspChar > lineLen || fromIntegral lspLine /= backLine)

        -- idempotent
        lspPositionToElara text (toLspPosition ls normalised) === normalised

examples :: Spec
examples = describe "Examples" $ do
    let region file sl sc el ec = RealSourceRegion (UnsafeMkSourceRegion file (Position sl sc) (Position el ec))
        range sl sc el ec = LSP.Range (LSP.Position sl sc) (LSP.Position el ec)
        addText = "import Elara.Prim\nimport Prelude\n\nlet add1 x = x + 1\nlet main = println (add1 41)\n"

    describe "regionToRange" $ do
        it "counts an emoji as two UTF-16 units" $
            regionToRange "a😀b\n" (region Nothing 1 3 1 4) <=> range 0 3 0 4
        it "shifts to 0-based" $
            regionToRange addText (region Nothing 4 5 4 9) <=> range 3 4 3 8
        it "maps generated regions to the start of the file" $
            regionToRange "abc" (GeneratedRegion "<generated>") <=> range 0 0 0 0
        it "widens zero-width regions by one column" $
            regionToRange "ab\ncdef" (region Nothing 2 3 2 3) <=> range 1 2 1 3
        it "clamps a widened zero-width region at the end of its line" $
            regionToRange "ab\n" (region Nothing 1 3 1 3) <=> range 0 2 0 2
        it "clamps lines past the end of the file" $
            regionToRange "ab\ncd" (region Nothing 9 1 9 5) <=> range 1 2 1 2

    describe "regionToRangeIn" $ do
        let files path = if path == "a.elr" then Just "ab\ncdef" else Nothing
        it "uses the text of the region's file" $
            regionToRangeIn files (region (Just "a.elr") 2 2 2 4) <=> range 1 1 1 3
        it "gives the zero range for a file it doesn't know" $
            regionToRangeIn files (region (Just "other.elr") 2 2 2 4) <=> range 0 0 0 0
        it "gives the zero range for a region with no file" $
            regionToRangeIn files (region Nothing 2 2 2 4) <=> range 0 0 0 0
        it "gives the zero range for a generated region, without consulting the lookup" $
            regionToRangeIn (error "should not be called") (GeneratedRegion "<generated>") <=> range 0 0 0 0

    describe "lspPositionToElara" $ do
        it "shifts to 1-based" $
            lspPositionToElara addText (LSP.Position 3 4) <=> Position 4 5
        it "converts UTF-16 units back to code points" $
            lspPositionToElara "a😀b" (LSP.Position 0 3) <=> Position 1 3
        it "rounds a position inside a surrogate pair down to before the character" $
            lspPositionToElara "a😀b" (LSP.Position 0 2) <=> Position 1 2
        it "clamps past the end of a line" $
            lspPositionToElara "ab\ncd" (LSP.Position 0 99) <=> Position 1 3
        it "clamps past the end of the file" $
            lspPositionToElara "ab\ncd" (LSP.Position 9 0) <=> Position 2 3

    describe "regionContains" $ do
        let r = region Nothing 2 3 2 7
        it "includes the start" $ regionContains (Position 2 3) r <=> True
        it "excludes the end" $ regionContains (Position 2 7) r <=> False
        it "includes the middle" $ regionContains (Position 2 6) r <=> True
        it "excludes earlier and later lines" $ do
            regionContains (Position 1 5) r <=> False
            regionContains (Position 3 5) r <=> False
        it "spans lines lexicographically" $ do
            let multi = region Nothing 2 5 4 2
            regionContains (Position 3 1) multi <=> True
            regionContains (Position 2 4) multi <=> False
            regionContains (Position 4 2) multi <=> False
        it "contains only the start of a zero-width region" $ do
            let z = region Nothing 2 3 2 3
            regionContains (Position 2 3) z <=> True
            regionContains (Position 2 4) z <=> False
        it "contains nothing if generated" $
            regionContains (Position 1 1) (GeneratedRegion "<generated>") <=> False

-- | Write some files into a temporary directory, and run an action with its path.
withProject :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withProject files act = runEff . runTemporary $ withSystemTempDirectory "elara-lsp-test" $ \dir -> liftIO $ do
    for_ files $ \(name, contents) -> writeFileText (dir </> name) contents
    act dir

-- | The diagnostics that the editor would be given for one file of a project.
diagnosticsFor :: FilePath -> [FilePath] -> FilePath -> IO (FileSnapshot, [LSP.Diagnostic])
diagnosticsFor projectDir allFiles file = do
    snapshot <- checkDocument defaultSettings{stdlibDir = "stdlib", sourceDirs = [projectDir]} (projectDir </> file)
    texts <- for allFiles $ \name -> (projectDir </> name,) . decodeUtf8 <$> readFileBS (projectDir </> name)
    pure (snapshot, snapshotDiagnostics (`Map.lookup` Map.fromList texts) snapshot)

checking :: Spec
checking = do
    let header =
            [text|
            import Elara.Prim
            import Prelude
            |]
        -- a module with a type error in it
        brokenB =
            ( "B.elr"
            , [text|
              module B
              $header

              let f = 1 + "oops"
              |]
            )
        goodB =
            ( "B.elr"
            , [text|
              module B
              $header

              let f = 1
              |]
            )
        mainUsing =
            ( "Main.elr"
            , [text|
              module Main
              import B
              $header

              let main = println B.f
              |]
            )
        mainNotUsing =
            ( "Main.elr"
            , [text|
              module Main
              import B
              $header

              let main = println "hi"
              |]
            )
        files = ["B.elr", "Main.elr"]
        blockedMessage = "This module could not be checked because of errors in other modules"

    describe "a module that uses something from a module with a type error" $ do
        it "produces one diagnostic saying it couldn't be checked and pointing at the real error" $
            withProject [brokenB, mainUsing] $ \dir -> do
                (snapshot, diagnostics) <- diagnosticsFor dir files "Main.elr"
                isNothing (snapshotTyped snapshot) <=> True
                map (._message) diagnostics <=> [blockedMessage]
                let related = concat (mapMaybe (._relatedInformation) diagnostics)
                map (\r -> Text.takeWhileEnd (/= '/') (LSP.getUri r._location._uri)) related <=> ["B.elr"]
                map (Text.isInfixOf "Type mismatch" . (._message)) related <=> [True]
                -- the error is on line 5 of B.elr, `let f = 1 + "oops"`, 0-based 4
                map (\r -> r._location._range._start._line) related <=> [4]

        it "has the real error in the module that contains it, and no 'couldn't be checked' diagnostic" $
            withProject [brokenB, mainUsing] $ \dir -> do
                (_, diagnostics) <- diagnosticsFor dir files "B.elr"
                map (\d -> (d._code, d._range._start._line)) diagnostics <=> [(Just (LSP.InR "E3011"), 4)]
                any ((== blockedMessage) . (._message)) diagnostics <=> False

    describe "a module that uses a module without errors" $
        it "has no diagnostics" $
            withProject [goodB, mainUsing] $ \dir -> do
                (snapshot, diagnostics) <- diagnosticsFor dir files "Main.elr"
                isJust (snapshotTyped snapshot) <=> True
                diagnostics <=> []

    describe "a module that imports a module with a type error but doesn't use it" $
        it "has no diagnostics" $
            withProject [brokenB, mainNotUsing] $ \dir -> do
                (_, diagnostics) <- diagnosticsFor dir files "Main.elr"
                diagnostics <=> []

markdown :: Spec
markdown = do
    let render :: Doc Ann -> Text
        render = docToMarkdown

    describe "plain text" $ do
        it "is left alone" $
            render "hello world" <=> "hello world"
        it "has Markdown formatting escaped, so that it is shown as written" $
            render "a*b_c" <=> "a\\*b\\_c"
        it "keeps line breaks" $
            render ("one" <> hardline <> "two") <=> "one  \ntwo"
        it "ignores styles that aren't about what something is" $
            render (Style.label "x") <=> "x"

    describe "code" $ do
        it "is used for types, names and so on" $
            render ("expected" <+> Style.typeName "Int" <+> "but found" <+> Style.typeName "String")
                <=> "expected `Int` but found `String`"
        it "keeps surrounding punctuation outside" $
            render ("found" <+> Style.typeName "String" <> ".") <=> "found `String`."
        it "keeps quotes outside" $
            render ("'" <> Style.varName "x" <> "'") <=> "'`x`'"
        it "can't contain backticks" $
            render (Style.varName "a`b") <=> "`ab`"
        it "keeps spaces outside the delimiters, as Markdown requires" $
            render (Style.typeName " Int ") <=> " `Int` "

    describe "neighbouring code" $ do
        it "is one piece when only separated by spaces, like a function type" $
            render (Style.typeName "Int" <+> Style.operator "->" <+> Style.typeName "Int") <=> "`Int -> Int`"
        it "is one piece across a qualified operator, including its brackets" $
            render (Style.moduleName "Elara.Prim" <> "." <> "(" <> Style.operator "+" <> ")") <=> "`Elara.Prim.(+)`"
        it "is one piece across a type application with brackets" $
            render (Style.typeName "List" <+> "(" <> Style.typeName "Int" <> ")") <=> "`List (Int)`"
        it "stays separate when words are between" $
            render (Style.typeName "Int" <+> "and" <+> Style.typeName "String") <=> "`Int` and `String`"

    describe "bold" $
        it "stays bold" $
            render (Style.bold "careful") <=> "**careful**"
