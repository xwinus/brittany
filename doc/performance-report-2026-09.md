# Performance optimization report, September 2026

This report compares commit `29f6c0e` with the completed implementations of
issues #169, #170, and #171. All versions were built
with GHC 9.14.1 and Cabal's default optimized `-O1` profile on the same machine.
Each scenario ran in its own process with RTS statistics enabled. The complete
machine-readable reports are in
[`benchmark/results/2026-09-baseline.json`](../benchmark/results/2026-09-baseline.json)
and
[`benchmark/results/2026-09-candidate.json`](../benchmark/results/2026-09-candidate.json).
The follow-up batch-session result is in
[`benchmark/results/2026-09-session.json`](../benchmark/results/2026-09-session.json).
The final BriDoc candidate is in
[`benchmark/results/2026-09-bridoc.json`](../benchmark/results/2026-09-bridoc.json).

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

## Batch parser session

The CLI and formatter-mode benchmark now create one scoped GHC session for a
batch. Before every input, parsing restores the original `HscEnv` and derives
fresh source pragmas and forwarded options. The formatted-output
`ParserContext` remains per-file, so validation uses exactly the input's
effective flags. No global state or cross-invocation cache is retained.

| Scenario | CPU before | CPU after | Allocation before | Allocation after | GHC sessions before | GHC sessions after |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| five repetitions of one small module | 69.4 ms | 69.8 ms | 160 MB | 150 MB | 5 | 1 |
| batch of ten small modules | 130 ms | 117 ms | 307 MB | 283 MB | 10 | 1 |

For the ten-file batch, GHC-session setup itself fell from 30.7 ms and 100.6 MB
to 0.7 ms and 2.9 MB. Large single-module scenarios remained within ordinary
run-to-run noise, as expected.

## BriDoc pipeline follow-up

The #171 re-profile first compared the same instrumented build before and after
the optimization. Cost-centre profiling of `Alt.hs` attributed the largest
remaining avoidable allocation to `extractBoundaryComments`: shared BriDoc
subtrees were traversed once per reference and comment results were deduplicated
quadratically. The profiling run allocated 5.07 GB before the change and 3.58 GB
after it; delimiter boundary extraction disappeared from the leading cost
centres.

The implementation now memoizes boundary extraction by stable BriDoc node ID
and performs stable-order comment deduplication with a `Set`. The common no-op
case still returns the original subtree.

| Scenario | CPU before | CPU after | Change | Allocation before | Allocation after | Change | Residency before | Residency after |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `Alt.hs` format, no validation | 1.416 s | 1.296 s | -8.5% | 4.105 GB | 3.267 GB | -20.4% | 41.6 MB | 41.6 MB |
| `Alt.hs` full safe format | 1.779 s | 1.673 s | -6.0% | 5.172 GB | 4.334 GB | -16.2% | 41.6 MB | 41.6 MB |

Detailed diagnostics now report BriDoc construction, comment lowering,
alternative resolution, every simplifier, backend rendering, and planned
comment validation. They also report raw and post-phase node counts,
alternative and delimiter structure, spacing calls and memo hits, and pruning
widths. On the nesting-depth-15 fixture, the measured phase costs were:

| Phase | CPU | Allocation |
| --- | ---: | ---: |
| BriDoc construction | 2.40 ms | 4.03 MB |
| Comment lowering | 0.27 ms | 0.40 MB |
| Alternative resolution | 0.66 ms | 0.29 MB |
| All four simplifiers | 0.62 ms | 3.95 MB |
| Backend rendering | 0.01 ms | 0.01 MB |
| Planned-comment validation | 0.02 ms | 0.09 MB |

The same fixture retained 2,948 raw nodes, 236 alternatives at maximum depth
17, and 716 nodes after the final simplifier. It made 236 `getSpacings` calls,
including 225 memo hits, and pruned a maximum spacing width from 9 to 3. The
delimiter-count-25 fixture retained 26 groups and 52 generated variants. These
structural and search counters were identical before and after the optimization,
as expected for a traversal-only change.

Forcing every intermediate value of the self-hosted `Alt.hs` graph changes its
lazy memory behaviour, so the benchmark deliberately disables detailed BriDoc
diagnostics for `alt-format` and `alt-full`. Those scenarios retain
representative end-to-end RTS metrics and cost-centre profiling; deterministic
quick and scaling fixtures retain the detailed counters. Normal formatter
entrypoints pass no collector and incur no diagnostics.

## Correctness checks

- The complete test suite passes: 1,099 examples, 0 failures.
- New unit coverage exercises line-comment discovery in a shared alternative,
  the required-break case, empty/block-comment edge cases, and a 400-row
  zero-overflow alignment.
- Boundary graph tests cover ordinary, duplicate, constructor, delimiter,
  expression, and malformed cases with three-pass idempotence checks.
- Parser-context tests cover repeated parsing, different filenames, source
  pragmas, forwarded GHC options, multi-file session accounting, and recovery
  after malformed input or an invalid option. They also verify that pragmas and
  forwarded options do not leak between files.
- The benchmark's full-safe mode retained output parsing, semantic validation,
  and comment validation.

The parser-context, batch-session, and BriDoc allocation work for #170 and #171
is complete. The remaining layout allocation is distributed across the inherited
multi-pass pipeline; no further architectural change is justified by the
current synthetic phase profile.
