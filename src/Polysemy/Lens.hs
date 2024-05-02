module Polysemy.Lens where

import Optics (A_Getter, Is)
import Polysemy
import Polysemy.State

use' :: (Member (State s) r, Is k A_Getter) => Optic' k is s a -> Sem r a
use' l = gets (view l)
