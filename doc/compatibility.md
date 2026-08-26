# GHC 9.14 compatibility matrix

The machine-readable compatibility matrix lives in
[`data/compatibility.yaml`](../data/compatibility.yaml). It classifies every
language edition and extension currently enabled by the test corpus, plus
syntax categories that do not have a dedicated `LANGUAGE` pragma.

Feature kinds are `edition`, `extension`, and `syntax`. Edition and extension
cases must enable their feature in the fixture. Syntax cases cover constructs
such as module headers, export lists, safe or source imports, and warning
pragmas directly.

## Support modes

- `native`: Brittany has a native layout path for the classified feature.
- `exact-source`: formatting is safe, reparses, and is idempotent, but the
  feature uses exact-source pass-through because it does not yet have a safe
  native layout path or its AST paths still need dedicated coverage.
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

Issue #14 verifies Haskell2010, GHC2021, and GHC2024 module syntax; native
module and namespace layout including GHC 9.14 `data` namespace specifiers;
package, safe, and source imports; and exact-source preservation for explicit
level imports, postpositive qualified imports, and module or export warning
pragmas.

Issue #15 verifies traditional record construction, updates, and patterns;
field puns and wildcards; duplicate and disambiguated fields; generated and
suppressed field selectors; and overloaded field access and updates. These
forms use native layout except for `OverloadedRecordUpdate`, whose containing
value declaration is preserved through exact-source rendering. Comments around
record fields are preserved in construction, update, and pattern positions.

Issue #16 verifies required and visible type arguments, type abstractions,
type-level data and operators, standalone and inline kind signatures, unlifted
declarations, unboxed tuples and sums, quantified constraints, and rank-n
types. Stable native layout is used for forall, kind, promoted, application,
constraint, `MagicHash`, and unboxed tuple paths. Experimental declaration,
type-abstraction, and unboxed-sum paths retain their containing
declaration through exact-source rendering. Comments around binders, arrows,
contexts, and constructor signatures are preserved.

Issue #80 verifies native, precedence-preserving type-operator layout in record
fields, including symbolic, backticked, parenthesized, and promoted operators.
Containing declarations retain configured indentation and operator comments
remain parseable and idempotent.

Issue #81 verifies native layout for multiline type signatures with argument
and result Haddock post-docs. Continuations and post-doc markers use configured
indentation, final-result ownership is retained, and unsupported type leaves
remain scoped inline fallbacks.

Issue #17 verifies modern patterns, literals, do notation, control expressions,
tuple sections, and list and tuple puns. Numeric and multiline literals retain
their exact source spelling, including sized primitive suffixes. Qualified do
blocks and declarations containing bang or lazy patterns use exact-source
rendering where native layout would change syntax or detach comments. Comments
at pattern, guard, statement, operator, and tuple punctuation boundaries are
preserved.

Issue #18 verifies Template Haskell quotes and splices, quasiquotes,
`SPECIALISE` and expression pragmas, `RULES`, `ANN`, warning and deprecation
pragmas, and foreign imports and exports. These parser-boundary constructs use
exact-source rendering, including `CApiFFI`, so their syntax and comments remain
unchanged. CPP remains unsupported: Brittany rejects it before preprocessing
or formatting and directs users to preprocess the input or remove `-XCPP`.

Adding a fixture that enables an unclassified extension therefore fails the
test suite and CI. The manifest's `tracking-issue` fields identify the roadmap
issue responsible for auditing and improving each classification.

Issue #19 adds the machine-readable
[fallback inventory](fallbacks.md), opt-in `--dump-fallbacks` diagnostics, and
native layout for simple multi-constructor Haskell 98 data declarations.
Commented and extension-specific constructor forms remain exact-source until
their annotation paths have the same expected, edge, and malformed coverage.
