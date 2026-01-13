# Comments

## Single-Line Comments

Single line comments are initiated using two dashes (`--`). Everything following the `--` on that line is considered part of the comment.

```fs
-- This is a single line comment
let x = 42  -- This comment is after code
```

## Multi-Line Comments

Multi-line comments are enclosed between `/-` and `-/`. They can span multiple lines and can also be nested.

```fs
/- This is a
   multi-line comment
   which spans several lines -/
/- This is a multi-line comment
   /- which contains a nested comment -/
   and continues here -/
let y = 100
```
