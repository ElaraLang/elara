# JVM Backend

## Overview

The JVM backend translates finalised Core into JVM class files in three sub-stages: IR generation, class file emission, and byte serialisation. Value declarations become static methods; type declarations (ADTs) become class hierarchies. The backend uses [H2JVM](https://github.com/ElaraLang/H2JVM) for class file representation and serialisation, and partial application closures are emitted using `invokedynamic` with `LambdaMetafactory`.


TODO expand
