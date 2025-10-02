# Queries

The compiler uses a query-based architecture to manage the computational dependencies between different stages of the compilation process.

This is implemented using the [Rock](https://github.com/ollef/rock) query system, and practically functions similarly to a `Makefile`.

The complete list of queries can be found in the [Query.hs](https://github.com/ElaraLang/elara/blob/master/src/Elara/Query.hs) file. Every query is a constructor of the `Query` GADT.
