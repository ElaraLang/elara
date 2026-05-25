module Elara.Parse.Debug where

import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Text.Show (Show (..))

import Elara.Data.Pretty

-- | Like 'dbg' but uses a 'Pretty' instance instead of a 'Show' instance
dbgPretty :: MonadParsecDbg e s m => Pretty a => String -> m a -> m a
dbgPretty label p = unPrettyDbg <$> dbg label (PrettyDbg <$> p)

newtype PrettyDbg p = PrettyDbg {unPrettyDbg :: p}
instance Pretty a => Pretty (PrettyDbg a) where
    pretty (PrettyDbg x) = pretty x

instance Pretty a => Show (PrettyDbg a) where
    show (PrettyDbg x) = toString $ prettyToText x
