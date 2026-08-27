# Canonical semantic model

Brittany reparses formatted output and compares it with the parsed input before
emitting source. The comparison uses a Brittany-owned semantic tree rather than
equality or a textual fingerprint of the GHC AST. Nodes contain named fields,
which also provide stable paths for safety diagnostics.

`SemanticFingerprint.Ghc` is the GHC 9.14 adapter. It removes source locations,
parser annotations, comments, source spelling, and redundant parentheses. A
generic projection retains constructors and payloads for syntax without a
dedicated policy. Values with no generic representation fail closed, so adding
an unprojected opaque AST payload cannot silently weaken validation.

## Canonicalization policies

The adapter has explicit policies only for transformations that Brittany
intentionally performs:

- Module imports are compared as multisets. Reordering imports is equivalent,
  while duplicate import declarations remain observable.
- Explicit and hiding import items are compared as multisets. Child names in
  `ThingWith` items use the same policy. Item kinds, namespaces, warnings,
  names, wildcard positions, and duplicate items remain observable.
- Import module, package, source, level, safety, qualification placement, alias,
  implicit status, and explicit-versus-hiding mode are named semantic fields.
- `deriving C` and `deriving (C)` share one singleton class-list model. Strategy,
  via type, class identity, and multi-class order remain observable.

Declarations, equations, guards, statements, patterns, export lists, and all
other lists remain order-sensitive. New canonicalization must be implemented as
a typed adapter policy with expected, edge, mutation, and formatting tests; it
must not be added as a constructor-name exception in the generic projection.
