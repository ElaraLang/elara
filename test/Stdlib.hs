module Stdlib where

import Data.Typeable (cast)
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runError)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Temporary (Temporary, runTemporary, withSystemTempDirectory)
import Relude.Extra.Bifunctor (firstF)
import System.FilePath ((</>))
import Test.Syd

import Elara.Data.Embed (readDirRecursive)
import Elara.Error (ElaraError (..))
import Elara.Stdlib

spec :: Spec
spec = do
    it "embeds the current contents of stdlib/" $ do
        onDisk <- readDirRecursive "stdlib"
        embeddedStdlib `shouldBe` onDisk

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
