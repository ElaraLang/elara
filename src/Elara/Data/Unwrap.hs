module Elara.Data.Unwrap where

import Elara.AST.Region (IgnoreLocation (IgnoreLocation), Located (Located))

class Unwrap c where
    unwrap :: c a -> a

instance Unwrap Identity where
    unwrap (Identity a) = a

instance Unwrap Located where
    unwrap (Located _ a) = a

instance Unwrap IgnoreLocation where
    unwrap (IgnoreLocation a) = unwrap a
