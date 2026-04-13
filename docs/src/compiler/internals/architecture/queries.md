# Queries

The compiler uses a query-based architecture to manage the computational dependencies between different stages of the compilation process.

This is implemented using the [Rock](https://github.com/ollef/rock) query system, and practically functions similarly to a `Makefile`.

The complete list of queries can be found in the [Query.hs](https://github.com/ElaraLang/elara/blob/master/src/Elara/Query.hs) file. Every query is a constructor of the `Query` GADT.


## Example Query Graph

This diagram shows a real example of the flow of queries during the execution of a simple module. Each node is a query, and an edge from query A to query B indicates that A depends on B.

> [!TIP]
> You can produce these diagrams yourself by running the compiler with the `--dump=query-graph` flag

```d2
"ConstructorDeclaration" -> "ModuleByName"
"DeclarationAnnotations" -> "RequiredDeclarationByName"
"DeclarationAnnotationsOfType" -> "ConstructorDeclaration"
"DeclarationAnnotationsOfType" -> "DeclarationAnnotations"
"DeclarationByName" -> "ModuleByName"
"DesugaredModule" -> "ParsedModule"
"FreeVarsOf" -> "RequiredDeclarationByName"
"GetANFCoreModule" -> "GetOptimisedCoreModule"
"GetClosureLiftedModule" -> "GetANFCoreModule"
"GetCoreModule" -> "FreeVarsOf"
"GetCoreModule" -> "GetDataCon"
"GetCoreModule" -> "GetTyCon"
"GetCoreModule" -> "TypeCheckedExpr"
"GetCoreModule" -> "TypeCheckedModule"
"GetDataCon" -> "GetCoreModule"
"GetFinalisedCoreModule" -> "GetClosureLiftedModule"
"GetOpInfo" -> "DeclarationAnnotationsOfType"
"GetOptimisedCoreModule" -> "GetCoreModule"
"GetSCCsOf" -> "FreeVarsOf"
"GetTyCon" -> "TypeCheckedDeclaration"
"GetTypeAlias" -> "ModuleByName"
"KindOf" -> "KindOf"
"KindOf" -> "ModuleByName"
"LexedFile" -> "GetFileContents"
"ModuleByName" -> "DesugaredModule"
"ModuleByName" -> "GetOpInfo"
"ModuleByName" -> "RenamedModule"
"ModulePath" -> "ModuleIndex"
"ModulePath" -> "ParsedFile"
"ParsedFile" -> "GetFileContents"
"ParsedFile" -> "LexedFile"
"ParsedModule" -> "GetFileContents"
"ParsedModule" -> "LexedFile"
"ParsedModule" -> "ModulePath"
"RenamedModule" -> "DesugaredModule"
"RequiredDeclarationByName" -> "DeclarationByName"
"TypeCheckedDeclaration" -> "ModuleByName"
"TypeCheckedDeclaration" -> "RequiredDeclarationByName"
"TypeCheckedExpr" -> "TypeCheckedModule"
"TypeCheckedModule" -> "GetTypeAlias"
"TypeCheckedModule" -> "KindOf"
"TypeCheckedModule" -> "ModuleByName"
"TypeCheckedModule" -> "TypeOf"
"TypeOf" -> "GetSCCsOf"
"TypeOf" -> "GetTypeAlias"
"TypeOf" -> "KindOf"
"TypeOf" -> "ModuleByName"
"TypeOf" -> "RequiredDeclarationByName"
"TypeOf" -> "TypeOf"
```