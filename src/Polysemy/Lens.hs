module Polysemy.Lens where

import Control.Lens (Getting, view)
import Polysemy
import Polysemy.State

use' :: Member (State s) r => Getting a s a -> Sem r a
use' l = gets (view l)
