# Application layout preferences

An application that fits on one line stays compact. When it does not fit,
Brittany prefers keeping a complete expression together at an enclosing break
before splitting its arguments solely to retain the enclosing prefix.

For a `do` binding, the choices are:

1. Keep the pattern, `<-`, and complete expression on one line.
2. Put the complete expression on the next structurally indented line, if it fits.
3. Use the existing multiline expression alternatives.

For example, at 80 columns:

```haskell
    children <-
      sequence $ zipWith projectChild childNames $ Data.gmapQ Box value
```

This preference also applies when the expression contains nested applications
or operator chains. It does not require preserving the input's line breaks.

Under `IndentPolicyFree`, an application can align successive arguments after
the function name. This hanging alternative must leave at least the function
head's width available for arguments at their aligned column. Otherwise the
formatter tries its structural alternatives, which can also allow an enclosing
expression to break earlier. The available width includes the widest argument;
arguments wider than the function head need no additional reservation.

The rule compares the rendered function and argument widths with the configured
line width. It does not impose an absolute indentation limit. Moderate hanging
alignment remains available, while a long qualified function name cannot consume
nearly all the space needed to display its short arguments. Comment-bearing expressions retain existing alternatives, since comments can be
emitted outside the measured RHS. Multiline components without a valid single-line
width also retain existing handling.
`IndentPolicyLeft` and `IndentPolicyMultiple` do not offer this free hanging
alternative and retain their application indentation rules.
