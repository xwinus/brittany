# Render disposition inventory

Brittany uses native layout where the AST and comment annotations are fully
modeled. Exact-source fallback copies a scoped source fragment unchanged when
native reconstruction is not yet safe. The whole-module safety net returns the
original module if an unknown GHC AST node reaches a layouter.

Atomic Template Haskell and quasiquote leaves are different. Brittany does not
own their internal syntax and intentionally preserves it byte-for-byte while
laying out the parent node natively. These leaves have the `SupportedOpaque`
disposition and are not actionable formatter gaps.

Run Brittany with `--dump-fallbacks` to report actionable fallbacks and
supported opaque leaves on standard error. Reporting is disabled by default,
does not alter formatter output, does not affect exit status, and is ignored by
`--werror`.

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

Use `--fail-on-opaque` for a stricter policy that rejects intentional opaque
preservation as well. It is independent of `--fail-on-fallback` and has the
equivalent YAML setting `conf_errorHandling.econf_failOnOpaque: true`.

`--dump-fallbacks-json` emits a schema-versioned JSON inventory on standard
error. Its summary reports actionable and opaque occurrence counts separately,
along with the number and occurrence count of each stable family. Every
occurrence includes `disposition`, `family`, `scope`, `location`, and `reason`.
An empty inventory is still emitted with zero counts. Schema version 1 uses
`unsupported-fallback`, `whole-module-fallback`, and `supported-opaque` as the
non-native disposition names.

The typed, machine-readable registry is exposed as `fallbackInventory` from
`Language.Haskell.Brittany`. Each entry includes its scope, support mode,
trigger, rationale, compatibility features, and regression fixtures.

Each scoped fallback carries its source range, annotation keys, and exact
comment identities. Rendering consumes only the comments explicitly present in
that source range. A fragment with an out-of-range comment identity is rejected
and leaves the remaining comment state unchanged, so pass-through output cannot
hide a comment that it did not render.

An opaque leaf is accepted only when its range is complete and every contained
comment has a placement owned by that leaf or one of its descendants. Missing
or external ownership keeps the node on its actionable fallback path. The
opening boundary composes with native parent indentation; multiline opaque
continuation bytes are emitted unchanged.

## Supported opaque families

| Identifier | Scope | Boundary |
| --- | --- | --- |
| `QuasiQuote` | inline or declaration | Expression, pattern, type, and declaration quasiquotes. |
| `TemplateHaskellQuote` | inline | Typed and untyped Template Haskell quotations. |
| `TemplateHaskellSplice` | inline or declaration | Typed and untyped expression/type splices and top-level declaration splices. |

## Inventory

| Identifier | Scope | Support | Trigger and rationale |
| --- | --- | --- | --- |
| `DataDeclarationFallback` | declaration | exact source | Data/newtype constructors outside the supported Haskell 98 and basic GADT subset. |
| `DeclarationFallback` | declaration | exact source | Foreign declarations, pragmas, unsafe splice boundaries, and experimental top-level forms outside the native declaration model. |
| `SignatureFallback` | declaration | exact source | Specialisation, fixity, and other signatures outside the native signature subset. |
| `ImplicitParameterFallback` | declaration | exact source | Implicit-parameter bindings outside the native binding subset. |
| `TypeClassDeclarationFallback` | declaration | exact source | Type, class, family, unsupported instance heads, and experimental binder forms outside native declaration layout. |
| `FamilyDefaultFallback` | declaration | exact source | Default associated family equations whose annotations are not safely reconstructed. |
| `ExpressionFallback` | inline | exact source | Extension-specific expressions whose punctuation or comments need exact preservation. |
| `TypeFallback` | inline | exact source | Unsafe type-splice boundaries, sums, and other type forms outside native layout. |
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

Issue #93 separates supported opaque Template Haskell and quasiquote leaves
from actionable formatter fallback. Strict fallback mode ignores those leaves;
strict opaque mode and the JSON inventory expose them independently.
