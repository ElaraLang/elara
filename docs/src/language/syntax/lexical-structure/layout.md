# Layout

Elara's syntax uses a layout system, where indentation is used to denote code blocks. This is similar to languages like Haskell, F#, and Python.

## The Indentation Rule

The core layout rule is simple: **Code that is part of a block must be indented further than the line that started that block.**

When you use a keyword that introduces a new block (`let`, `match`, `if`, etc.), the compiler looks at the indentation of the following token to determine the expected indentation level for that block.

### Starting a Block

Blocks are typically started by a newline followed by an increase in indentation after a **Layout Trigger**.

The following tokens trigger a layout check:

- `=` (Equality / Assignment)
- `->` (Function arrows / Match cases)
- `with` (The body of a `match` expression)
- `then` (The body of an `if` expression)
- `else` (The second body of an `if` expression)

**Example:**

```fsharp
let pythagoras a b =
    let a2 = a * a
    let b2 = b * b
    sqrt (a2 + b2)
```

In this example, the `=` after `let pythagoras a b` triggers a layout expectation. The next line is indented, so a new block starts there.

Lines that are indented to the same level as the new block are considered part of that block, and treated as separate statements, i.e. as if they were separated by semicolons.

```hs
-- Treated as 3 separate statements
let a = 1
let b = 2
let c = 3
```

#### Line Continuation

To continue a single expression across multiple lines, make sure the subsequent lines are indented further than the initial line.

```hs
let sum =
    1 + 2 +
      3 + 4 -- Indented further, so it continues the expression
```

If you wrote it like this instead:

```hs
let sum =
    1 + 2 +
    3 + 4 -- Same indentation, so treated as separate statements
```

The compiler would treat `3 + 4` as a separate statement, leading to an error.

### Inline Definitions

The layout rule can be ignored if the entire block is defined on the same line. If no newline is present after a layout trigger, no block is created and so the expression continues to the end of the line.

This allows for concise inline definitions:

```hs
let add x y = x + y  -- No newline after '=', so no block is created
```

### Delimiters

Explicit delimiters (parentheses `()`, braces `{}`, and brackets `[]`) can be used to group expressions and override the layout rules.

You can freely indent inside delimiters without affecting the layout, as long as a layout trigger is not encountered.

If you start a layout block inside a pair of delimiters, the block is automatically closed when the closing delimiter is reached.

```hs
let x = (
    let y = 10
    y * 2
)
```

This code is valid because the block started by `let y = 10` is closed by the closing parenthesis.

## Explicit Layout

While not typically recommended, you can disable the layout system entirely by using explicit braces `{}` and semicolons `;` to denote blocks and separate statements.

When using explicit layout, the indentation rules are ignored.

```hs
let pythagoras a b = {
    let a2 = a * a;
 let b2 = b * b;
    sqrt (a2 + b2);
}
```

This code is valid despite the inconsistent indentation because the braces and semicolons explicitly define the block structure.

Using explicit layout disables layout entirely, including for child blocks. For example, the following code is valid:

```hs
let main =
    let x = 10
    let y = { let i = x * 2; i + 1; } -- Explicit block for y
    print y
```

but the following code is not:

```hs
let main = {
    let x = 10;
    let f n =
        sqrt n  -- Error: Cannot use indentation inside explicit block!
    f x;
}
```
