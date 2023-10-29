module Polysemy.Utils where

import Polysemy
import Polysemy.State

withModified :: Member (State s) r => (s -> s) -> Sem r a -> Sem r a
withModified f m = do
    s <- get
    put (f s)
    a <- m
    put s
    pure a
