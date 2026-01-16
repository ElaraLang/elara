module TODO where

{-# WARNING TODO "TODO" #-}

type TODO = Void

{-# WARNING todo "TODO" #-}
todo :: HasCallStack => a
todo = withFrozenCallStack (error "TODO")
