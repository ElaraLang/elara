module Elara.Parse.Debug where

import Text.Show (Show (..))

import Elara.Data.Pretty

newtype PrettyDbg p = PrettyDbg {unPrettyDbg :: p}
instance Pretty a => Pretty (PrettyDbg a) where
    pretty (PrettyDbg x) = pretty x

instance Pretty a => Show (PrettyDbg a) where
    show (PrettyDbg x) = toString $ prettyToText x
