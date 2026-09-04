# Annotation traversal inventory

This inventory describes the formatting path after the GHC 9.14 annotation
index consolidation. It distinguishes traversals that share reusable data from
traversals whose purpose or scope is intentionally independent.

## Consolidated traversal

`buildModuleAnnotationIndex` performs one generic traversal of the declaration
forest. Typed queries collect nested expression, declaration, binding, match,
guarded-RHS, statement, type, pattern, constructor, deriving, record-field,
family-instance, local-binding, and match-group annotations. The result keeps:

- source-ordered annotation nodes with canonical keys, real spans, and comments;
- synthetic compatibility nodes such as `HsValBinds` and `MatchGroup`;
- expression-specific annotation overrides for `HsIf`, `HsDo`, and `GRHS`;
- a filtered span view used by comment ownership.

Annotation extraction and its span ownership lookup now project from this
single index. Missing-comment recovery also reuses the first extraction and
only rebuilds annotations when it actually modifies the parsed AST.

## Intentionally independent traversals

| Traversal | Scope | Why it remains independent |
| --- | --- | --- |
| Quasiquote discovery in `recoverMissingComments` | Whole module, before possible AST mutation | It protects source scanning and must run before the final annotation index is known. |
| Exact-print comment redistribution | The fragment being transformed, stateful | It mutates annotation ownership and cannot be represented by a read-only node index. |
| `foldedAnnKeys` | Individual fallback or layouter subtree | Callers need descendant membership for arbitrary subtrees, while the module index currently covers the declaration extraction path. |
| Exact-source fragment key collection | Individual unsupported subtree | It combines descendant keys with synthetic fallback keys specific to that fragment. |
| Comment, expression, pattern, type, and constructor policy queries | Small policy-specific subtrees | They apply caller predicates or build boundary structures rather than extract `EpAnn` data. |
| Semantic projection | Input and output modules during validation | It deliberately constructs normalized syntax trees and does not consume source annotations. |

Dynamic `EpAnn` discovery remains local to each located node visited by the
shared index. It is needed because GHC uses several location wrapper shapes;
the conversion chain is not repeated by another whole-declaration traversal.

Comment-boundary validation has its own reusable indexes. Case regions,
delimiter regions, and expression owners are each built once per module and
queried for all comments. Previously the case and delimiter AST traversals ran
once per comment.

## Traversal counts

For nested annotation extraction, the declaration forest now has one generic
walk instead of three overlapping walks: nested annotations, nested span maps,
and special expression overrides. Case and delimiter boundary discovery now
adds two whole-module walks per prepared comment plan, independent of comment
count, instead of two walks for every comment.

Further consolidation should be driven by phase measurements. In particular,
turning every arbitrary-subtree `foldedAnnKeys` call into an ancestry index
would broaden cache lifetime and should only be done if it remains material in
a current profile.
