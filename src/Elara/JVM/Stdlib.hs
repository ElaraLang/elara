{-# LANGUAGE TemplateHaskell #-}

-- | The Java half of the standard library, bundled into the compiler as _compiled_ bytecode so the JVM backend is portable
module Elara.JVM.Stdlib (jvmStdlibClasses) where

import Elara.Data.Embed (embedJavaClasses)

-- | The compiled @jvm-stdlib@ classes, as paths relative to the classpath root and their contents
jvmStdlibClasses :: [(FilePath, ByteString)]
jvmStdlibClasses = $(embedJavaClasses "jvm-stdlib")
