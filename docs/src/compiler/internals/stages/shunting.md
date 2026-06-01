# Shunting

## Overview

The shunting stage resolves operator precedence and associativity, transforming flat sequences of infix operators into correctly nested expression trees. It converts binary operator syntax into curried prefix application - for example, `a + b` becomes `((+) a) b`.

Operators are annotated with precedence (0–9) and associativity via source-level [annotations](../../../language/syntax/annotations.md); when no annotation is present, the defaults are precedence 9 and left-associative.

## Core Mechanisms

The module-level entry point is [`runGetShuntedModuleQuery`]({{hackage_base}}/Elara-Shunt.html#v:runGetShuntedModuleQuery), which takes a [`Module SourceRegion Renamed`]({{hackage_base}}/Elara-AST-Module.html#t:Module) and produces a [`Module SourceRegion Shunted`]({{hackage_base}}/Elara-AST-Module.html#t:Module). The per-expression entry point is [`fixExpr`]({{hackage_base}}/Elara-Shunt.html#v:fixExpr), which first calls [`fixOperators`]({{hackage_base}}/Elara-Shunt.html#v:fixOperators) to resolve precedence and associativity, then calls [`shuntExpr`]({{hackage_base}}/Elara-Shunt.html#v:shuntExpr) to convert binary operators into curried prefix calls.

### Operator Table

Precedence levels are integers in the range 0–9, validated by [`mkPrecedence`]({{hackage_base}}/Elara-Shunt-Operator.html#v:mkPrecedence), with higher precedence binding tighter. The operator table is stored as [`OpTable`]({{hackage_base}}/Elara-Shunt-Operator.html#t:OpTable) - a `Map` from location-ignored operator names to [`OpInfo`]({{hackage_base}}/Elara-Shunt-Operator.html#t:OpInfo) records containing precedence and associativity.

Associativity is one of:

- `LeftAssociative` - `a + b + c` parses as `(a + b) + c`
- `RightAssociative` - `a :: b :: c` parses as `a :: (b :: c)`
- `NonAssociative` - cannot chain without explicit parentheses

Operator info is fetched per-declaration via the [`GetOpInfo`]({{hackage_base}}/Elara-Query.html#v:GetOpInfo) query, which looks up `Fixity` and `Associativity` annotations from [annotations](../../../language/syntax/annotations.md) in the source. [`runGetOpTableInQuery`]({{hackage_base}}/Elara-Shunt.html#v:runGetOpTableInQuery) currently returns an empty map as a placeholder for future per-module operator scoping.

### Algorithm

[`fixOperators`]({{hackage_base}}/Elara-Shunt.html#v:fixOperators) recursively descends through expressions containing [`BinaryOperatorExpression`]({{hackage_base}}/Elara-AST-Expression.html#v:BinaryOperatorExpression) nodes. The core logic lives in `reassoc'`: given two adjacent operators `o1` and `o2` with operands `e1`, `e2`, `e3`:

- `o1` has higher precedence → left-associate
- `o2` has higher precedence → right-associate
- Equal precedence, same associativity → associate accordingly
- Equal precedence, different associativity → error (`SamePrecedenceError`)

Parenthesised expressions preserve their boundaries and re-enter recursion. After restructuring, [`shuntExpr`]({{hackage_base}}/Elara-Shunt.html#v:shuntExpr) converts each binary operator into a curried prefix call by applying the operator to the left operand, then applying the result to the right operand.

## Error Handling

When the shunting stage encounters problems, it throws a [`ShuntError`]({{hackage_base}}/Elara-Shunt-Error.html#t:ShuntError):

- `SamePrecedenceError`- operators with same precedence but different associativity
- `LocalOperatorInfoNotSupported` - local operator references cannot carry operator info (for now?)

There is also a `ShuntWarning` type with `UnknownPrecedence`, when an operator is declared without a precedennce annotation. 