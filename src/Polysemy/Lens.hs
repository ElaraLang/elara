{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Polysemy.Lens where

import Control.Lens (Getting, view)
import Polysemy
import Polysemy.State
import Prelude hiding (gets)

use' :: Member (State s) r => Getting a s a -> Sem r a
use' l = gets (view l)