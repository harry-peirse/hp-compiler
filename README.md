# HP Compiler
'HP' is a language I've invented - it's just an excuse to learn how to write a compiler really.

At the moment it produces x64 windows assembly compatible with GCC.

The language so far supports:
- signed 64 bit variables only (ints)
- function calls that always return an int - and can support 0 or more int arguments
- C-style +, -, /, *, ==, ||, <, >, <=, >= operations with the correct operator precedence (including parentheses)
- mutable local variables being declared and assigned (with the = operator)
- C style loops (while, do-while, for, break, continue)

An example function looks like this:
```
increment :: (i: s64): s64 {
    var one: s64 = 1
    i + one
}
```
What's going on here?
- `increment ::` declare a function called `increment`.
- `()` brackets contains the function arguments. These are optional (in which case you should leave out the following `:` also)
- `i: s64` is the first and only argument, where `i` is the name of the argument, and `s64` is the type.
- `: s64` after the parentheses means the return type of the function is `s64`
- `var one: s64 = 1;` means a mutable variable is being declared, called `one`, of type `s64`, initialised to `1`. The initialisation is optional.
- `return` precedes the expression to be returned by the function.
- Functions need to have a body defined by braces.
- Every statement should end with a semicolon.