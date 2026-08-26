# Exact-source fallback inventory

Brittany uses native layout where the AST and comment annotations are fully
modeled. Exact-source fallback copies a scoped source fragment unchanged when
native reconstruction is not yet safe. The whole-module safety net returns the
original module if an unknown GHC AST node reaches a layouter.

Run Brittany with `--dump-fallbacks` to report every pass-through path on
standard error. Reporting is disabled by default, does not alter formatter
output, does not affect exit status, and is ignored by `--werror`. A run with
no fallback reports during normal formatting used native layout for every
reached syntax path.

Use `--fail-on-fallback` when a workflow requires native formatting for every
reached syntax path. This mode reports the same fallback identifiers, scopes,
and source spans without requiring `--dump-fallbacks`, suppresses formatter
output, and exits with status 70 when any scoped or whole-module fallback is
used. `--output-on-errors` permits output while retaining the failing exit
status. Parse failures remain status 60 and never overwrite an input file.

The equivalent YAML setting is
`conf_errorHandling.econf_failOnExactSourceFallback: true`. Strict fallback
handling is independent of `--werror` and applies to display, check, inplace,
exactprint-only, and library use.

The typed, machine-readable registry is exposed as `fallbackInventory` from
`Language.Haskell.Brittany`. Each entry includes its scope, support mode,
trigger, rationale, compatibility features, and regression fixtures.

Each scoped fallback carries its source range, annotation keys, and exact
comment identities. Rendering consumes only the comments explicitly present in
that source range. A fragment with an out-of-range comment identity is rejected
and leaves the remaining comment state unchanged, so pass-through output cannot
hide a comment that it did not render.

## Inventory

| Identifier | Scope | Support | Trigger and rationale |
| --- | --- | --- | --- |
| `DataDeclarationFallback` | declaration | exact source | Data/newtype constructors outside the supported Haskell 98 and basic GADT subset. |
| `DeclarationFallback` | declaration | exact source | Foreign declarations, pragmas, splices, and experimental top-level forms outside the native declaration model. |
| `SignatureFallback` | declaration | exact source | Specialisation, fixity, and other signatures outside the native signature subset. |
| `ImplicitParameterFallback` | declaration | exact source | Implicit-parameter bindings outside the native binding subset. |
| `TypeClassDeclarationFallback` | declaration | exact source | Type, class, family, unsupported instance heads, and experimental binder forms outside native declaration layout. |
| `FamilyDefaultFallback` | declaration | exact source | Default associated family equations whose annotations are not safely reconstructed. |
| `ExpressionFallback` | inline | exact source | Extension-specific expressions whose punctuation or comments need exact preservation. |
| `TypeFallback` | inline | exact source | Type splices, sums, and other type forms outside native layout. |
| `PatternFallback` | inline | exact source | Extension-specific patterns outside the native pattern subset. |
| `StatementFallback` | inline | exact source | Statements and qualifiers outside the native statement subset. |
| `ImportFallback` | declaration | exact source | Explicit-level and comment-sensitive imports. |
| `ExactPrintOnlyFallback` | declaration | exact source | Formatting disabled for a declaration by user configuration. |
| `WholeModuleFallback` | module | safety net | Unknown AST node detected; the original source is returned instead of partial output. |

## Removal contract

A fallback can be narrowed or removed only after all of these cases exist:

1. An expected fixture for the common native layout.
2. An edge fixture covering comments or a complex syntax boundary.
3. A malformed fixture proving parse failures do not modify input.

All successful cases continue through the output reparse, idempotence, and
comment-preservation checks. Issues #19 and #55 promote simple
multi-constructor Haskell 98 declarations, constructor comments, and basic GADT
signatures to native layout under this contract. Extended constructor forms
retain `DataDeclarationFallback`.

Issue #80 promotes type operators to native, precedence-preserving layout.
Containing declarations retain configured indentation, while comments within
operator types remain attached and idempotent.

Issue #81 promotes unambiguous Haddock post-doc signatures to native layout and
keeps unsupported type leaves at inline `TypeFallback` scope. Signatures with
ordinary comments retain declaration fallback when ownership is ambiguous.

Issue #82 promotes supported standalone deriving declarations to native layout.
Ambiguously attached comments, unstable type forms, explicit binders in via
types, and unsupported overlap modes retain `DeclarationFallback`.
