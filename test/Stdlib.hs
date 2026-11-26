module Stdlib where

import Data.Typeable (cast)
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runError)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Temporary (Temporary, runTemporary, withSystemTempDirectory)
import H2JVM.JVMVersion
import Relude.Extra.Bifunctor (firstF)
import System.FilePath ((</>))
import System.Info (arch)
import Test.Syd

import Data.ByteString qualified as BS

import Elara.Data.Embed (readDirRecursive)
import Elara.Error (ElaraError (..))
import Elara.JVM.Stdlib (jvmStdlibClasses)
import Elara.JVM.Version (targetJavaVersion)
import Elara.Stdlib

spec :: Spec
spec = do
    it "embeds the current contents of stdlib/" $ do
        onDisk <- readDirRecursive "stdlib"
        embeddedStdlib `shouldBe` onDisk

    unless (arch == "wasm32") $ -- no javac available to pack the class files
        it "embeds the JVM stdlib as class files for the target Java version" $ do
            map fst jvmStdlibClasses `shouldContain` ["Elara/IO.class"]
            for_ jvmStdlibClasses $ \(_, bytes) ->
                BS.take 8 bytes `shouldBe` BS.pack [0xCA, 0xFE, 0xBA, 0xBE, 0, 0, 0, fromIntegral (unwrapMajor (getMajor targetJavaVersion))]

    it "ejects a copy of the bundled stdlib" $ do
        ejected <- succeeds $ runStdlib $ withSystemTempDirectory "eject" $ \tmp -> do
            let dir = tmp </> "my-stdlib"
            ejectStdlib dir
            liftIO (readDirRecursive dir)
        ejected `shouldBe` embeddedStdlib

    it "refuses to eject over an existing directory" $ do
        result <- runStdlib $ withSystemTempDirectory "eject" ejectStdlib
        stdlibError result `shouldSatisfy` \case
            Just (EjectTargetExists _) -> True
            _ -> False

    it "rejects a stdlib directory that doesn't exist" $ do
        result <- runStdlib $ resolveStdlibDir (Just "does-not-exist")
        stdlibError result `shouldSatisfy` \case
            Just (StdlibDirNotFound "does-not-exist") -> True
            _ -> False

    it "uses an explicitly given stdlib directory as is" $ do
        dir <- succeeds $ runStdlib $ resolveStdlibDir (Just "stdlib")
        dir `shouldBe` "stdlib"

runStdlib :: Eff '[Error ElaraError, FileSystem, Temporary, IOE] a -> IO (Either ElaraError a)
runStdlib = firstF snd . runEff . runTemporary . runFileSystem . runError @ElaraError

-- todo move this to a utils
succeeds :: IO (Either ElaraError a) -> IO a
succeeds = (>>= either (\err -> expectationFailure ("unexpected error: " <> show err)) pure)

stdlibError :: Either ElaraError a -> Maybe StdlibError
stdlibError (Left (ElaraError e)) = cast e
stdlibError (Right _) = Nothing
