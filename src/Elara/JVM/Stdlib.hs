{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The Java half of the standard library, bundled into the compiler as _compiled_ bytecode so the JVM backend is portable
module Elara.JVM.Stdlib (jvmStdlibClasses) where

#if !defined(wasm32_HOST_ARCH)
import Elara.Data.Embed (embedJavaClasses)
#endif

{- | The compiled @jvm-stdlib@ classes, as paths relative to the classpath root and their contents.
Empty on wasm since there's no `javac` to run.
-}
jvmStdlibClasses :: [(FilePath, ByteString)]
#if defined(wasm32_HOST_ARCH)
jvmStdlibClasses = []
#else
jvmStdlibClasses = $(embedJavaClasses "jvm-stdlib")
#endif
