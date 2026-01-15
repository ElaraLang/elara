# Lexical Structure

## Identifiers

Identifiers in Elara have different semantic meaning based on the capitalisation of the first letter:

- **Lowercase Identifiers** identify _terms_, i.e. variables and function names. They must start with a lowercase letter (a-z) or an underscore (\_) and can be followed by any combination of letters, digits (0-9), and underscores (\_). Examples: `myVariable`, `compute_sum`, `_temp123`
- **Uppercase Identifiers** identify _types_ and _constructors_. They must start with an uppercase letter (A-Z) and can be followed by any combination of letters, digits (0-9), and underscores (\_). Examples: `MyType`, `Option`, `TreeNode`

## Literals

Elara supports the following literal types:

- **Integer Literals**: A sequence of digits representing whole numbers. Examples: `0`, `42`, `-123456`
- **Floating-Point Literals**: A sequence of digits with a decimal point representing real numbers. Examples: `3.14`, `0.001`, `-2.5`
- **String Literals**: A sequence of characters enclosed in double quotes. Examples: `"Hello, World!"`, `"Elara is great!"`
- **Character Literals**: A single character enclosed in single quotes. Examples: `'a'`, `'Z'`, `'\n'`

## Keywords

The following are reserved keywords in Elara and cannot be used as identifiers:

- `def`
- `let`
- `in`
- `type`
- `if`
- `then`
- `else`
- `match`
- `with`
- `module`
- `import`
- `class`
- `alias`
