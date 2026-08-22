# GHC 9.14 compatibility matrix

The machine-readable compatibility matrix lives in
[`data/compatibility.yaml`](../data/compatibility.yaml). It classifies every
language edition and extension currently enabled by the test corpus.

## Support modes

- `native`: Brittany has a native layout path for the classified feature.
- `exact-source`: formatting is safe, reparses, and is idempotent, but the
  feature is conservatively treated as exact-source pass-through until its AST
  paths have dedicated coverage.
- `unsupported`: Brittany rejects the covered form without modifying the input.

The classifications describe verified formatter behavior, not whether GHC can
parse or type-check a feature. An extension can move from `exact-source` to
`native` only when its native layout paths have expected, edge, and error
coverage.

## Test contract

The compatibility test harness enforces the following rules:

1. Haskell2010, GHC2021, and GHC2024 must be classified.
2. Every classified feature must have a real fixture and test case.
3. Every `LANGUAGE` pragma in the golden and regression fixtures must have a
   matrix entry.
4. Cases cannot be skipped, and expected, edge, and malformed categories must
   remain represented.
5. Successful cases must preserve comments, produce parseable output, and be
   byte-identical after a second formatting pass.
6. Unsupported and malformed cases must fail without modifying their input.

Adding a fixture that enables an unclassified extension therefore fails the
test suite and CI. The manifest's `tracking-issue` fields identify the roadmap
issue responsible for auditing and improving each classification.
