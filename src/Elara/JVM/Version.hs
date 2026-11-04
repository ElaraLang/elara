{- | The JVM version targeted by most normal uses.
This will probably be refactored to be configurable soon
-}
module Elara.JVM.Version (targetJavaVersion) where

import H2JVM.JVMVersion (JVMVersion, java8)

targetJavaVersion :: JVMVersion
targetJavaVersion = java8
