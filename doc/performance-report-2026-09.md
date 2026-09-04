# Performance optimization report, September 2026

This report compares commit `29f6c0e` with the working-tree implementation of
issue #169 and the completed slices of #170 and #171. Both versions were built
with GHC 9.14.1 and Cabal's default optimized `-O1` profile on the same machine.
Each scenario ran in its own process with RTS statistics enabled. The complete
machine-readable reports are in
[`benchmark/results/2026-09-baseline.json`](../benchmark/results/2026-09-baseline.json)
and
[`benchmark/results/2026-09-candidate.json`](../benchmark/results/2026-09-candidate.json).

## End-to-end results

| Scenario | CPU before | CPU after | Change | Allocation before | Allocation after | Change | Residency before | Residency after |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `Alt.hs` parse | 432 ms | 211 ms | -51.1% | 934 MB | 465 MB | -50.2% | 12.9 MB | 11.9 MB |
| `ExtractAnns.hs` parse | 1.08 s | 926 ms | -14.2% | 2.22 GB | 2.11 GB | -4.8% | 22.1 MB | 24.7 MB |
| `Alt.hs` format, no validation | 4.11 s | 1.38 s | -66.4% | 8.42 GB | 3.95 GB | -53.1% | 257 MB | 38.1 MB |
| `Alt.hs` full safe format | 8.38 s | 1.76 s | -79.0% | 18.0 GB | 4.98 GB | -72.4% | 268 MB | 38.1 MB |
| nesting depth 15, no validation | 8.47 s | 30 ms | -99.6% | 27.6 GB | 60.1 MB | -99.8% | 1.82 MB | 1.83 MB |
| declaration size 400, no validation | 7.77 s | 94 ms | -98.8% | 61.4 GB | 199 MB | -99.7% | 4.52 MB | 3.46 MB |

`ExtractAnns.hs` changed from 89,762 bytes in the baseline checkout to 88,903
bytes in the candidate, so that row is directionally useful rather than a
strict same-input microbenchmark. The file contains comments missed by GHC and
therefore still requires a second annotation extraction after recovery.

## Phase findings

The shared annotation index reduced `Alt.hs` annotation preparation to one
extraction. Comment recovery fell from 441 MB to 19.9 MB because it consumes
the already-extracted annotations instead of extracting them again.

Formatted-output validation now reuses the input's effective `DynFlags`.
`Alt.hs` full formatting therefore opens one GHC session instead of two while
retaining source parsing, annotation extraction, semantic comparison, and
comment comparison for the output.

The first post-annotation profile identified comment-boundary validation as
the largest remaining non-layout hotspot: 8.32 GB and 4.14 s CPU. Case and
delimiter region discovery were traversing the complete output AST once per
comment. Building those region indexes once per comment plan, together with
reusing the input comment plan, reduced that phase to 232 MB and 82 ms.

Cost-centre profiling of the real layout pipeline was built with
`--enable-profiling --ghc-options=-fprof-auto`. It identified two independent
scaling problems:

- The alignment dynamic program rebuilt and rescored every complete candidate
  plan. It now caches score metadata while prepending groups and immediately
  accepts a valid zero-overflow single group, which is provably optimal under
  the existing score ordering. Layout allocation for the 400-item declaration
  fell from 61.4 GB to 172 MB.
- Alternative layout repeatedly scanned suffixes and shared branches to find
  trailing line comments. A once-per-document, node-ID-aware line-comment
  check and a right-folded sequence check reduced layout allocation at nesting
  depth 15 from 27.6 GB to 45.3 MB.

Delimiter boundary extraction also preserves the original document when no
matching comment exists, avoiding reconstruction in its common no-op path.
Together these changes reduced `Alt.hs` layout and rendering from 7.46 GB and
3.67 s CPU to 3.46 GB and 1.16 s CPU. Focused 2,000-element measurements still
show linear costs in the individual simplification and rendering passes;
switching the default chooser to `SimpleQuick` remains unsupported by the
profile.

## Correctness checks

- The complete test suite passes: 1,087 examples, 0 failures.
- New unit coverage exercises line-comment discovery in a shared alternative,
  the required-break case, empty/block-comment edge cases, and a 400-row
  zero-overflow alignment.
- Boundary graph tests cover ordinary, duplicate, constructor, delimiter,
  expression, and malformed cases with three-pass idempotence checks.
- Parser-context tests cover repeated parsing, different filenames, source
  pragmas, forwarded GHC options, and recovery after a malformed parse.
- The benchmark's full-safe mode retained output parsing, semantic validation,
  and comment validation.

The parser context currently reuses effective flags for the input/output pair
and can parse multiple buffers with that option set. The remaining #170 batch
session work may additionally share GHC session initialization across unrelated
input files, but must derive fresh per-file flags so LANGUAGE pragmas cannot
leak. For #171, the next evidence-driven target is the remaining 3.46 GB real
`Alt.hs` layout allocation, not the already-linear synthetic passes.
