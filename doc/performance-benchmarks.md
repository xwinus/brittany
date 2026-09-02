# Performance benchmarks

The `brittany-performance` benchmark records reproducible end-to-end and
phase-level measurements. It is intended for before/after comparisons on the
same machine, not for enforcing absolute wall-clock limits in ordinary CI.

## Standard benchmark

Run the standard suite from a clean checkout with an optimized build:

```sh
cabal build bench:brittany-performance
BENCH_BIN=$(cabal list-bin bench:brittany-performance)
"$BENCH_BIN" --suite standard --output benchmark-results.json
```

The runner always loads `data/brittany.yaml` without a user configuration.
Use `--config PATH` to measure a different explicit configuration.

The standard suite covers:

- parse and annotation preparation, formatting without output validation, and
  full safe formatting of the self-hosted `Transformations/Alt.hs` module;
- parse and annotation preparation of the self-hosted `ExtractAnns.hs` module;
- warm repeated formatting of one generated module;
- batch formatting of several generated modules in one process;
- focused `AnnKey` comparison and `Map` workloads;
- focused annotation extraction, annotation-index construction, comment-plan
  preparation, and top-level grouping;
- BriDoc alternative resolution, every simplification pass, and backend
  rendering as separate focused operations;
- focused semantic and comment comparison;
- declaration-count scaling at 50, 100, 200, and 400 declarations for both
  parsing and focused top-level grouping;
- nesting-depth scaling at depths 5, 10, and 15;
- independent layout-alternative count and depth workloads;
- comment-count, independent delimiter count and depth, and
  fixed-declaration-count size scaling;
- malformed inputs as expected, measured parser and layout failures.

`FullSafeFormatting` keeps output parsing, semantic comparison, comment
fingerprinting, and comment ownership validation enabled.

For a fast installation and schema check, use:

```sh
"$BENCH_BIN" --suite quick --output benchmark-quick.json
```

The scaling cases can also be run independently:

```sh
"$BENCH_BIN" --suite scaling --output benchmark-scaling.json
```

Run only the focused operations with:

```sh
"$BENCH_BIN" --suite micro --output benchmark-micro.json
```

Each scenario runs in a separate worker process with RTS statistics enabled.
This isolates maximum residency and cold-start costs between scenarios. Warm
and batch scenarios perform several formatting operations inside their one
worker process.

The issue baseline on master commit `c981fed`, measured on a MacBookPro18,3
with GHC 9.14.1, is retained as the initial comparison point:

| Input and mode | CPU time | Allocation | Maximum residency |
| --- | ---: | ---: | ---: |
| `Alt.hs`, parse only | 10.0 s | 109 GB | 11 MB |
| `Alt.hs`, format without validation | 41.5 s | 422 GB | 285 MB |
| `Alt.hs`, full safe formatting | 48.1 s | 465 GB | 216 MB |
| `ExtractAnns.hs`, parse only | 58.6 s | 670 GB | 21 MB |

Treat these numbers as historical context only; use two reports produced on
the same machine for decisions.

## Metrics and phases

Every scenario reports:

- elapsed and CPU nanoseconds;
- allocated and copied bytes;
- maximum residency;
- GC and mutator CPU time, GC count, and productivity as a ratio from 0 to 1;
- input bytes, lines, declaration count, nesting depth, layout-alternative
  count/depth, delimiter count, and other relevant counters when known;
- formatter error count and an explicit success, expected-failure,
  unexpected-failure, or harness-error outcome.

The phase report contains stable names for GHC session setup, flag parsing,
source parsing, missing-comment recovery, annotation extraction, inline
configuration, comment planning, top-level grouping, layout and rendering,
`AnnKey` operations, annotation-index construction, alternative resolution,
the floating/par/columns/indent simplifiers, backend rendering, output parsing,
semantic validation, and comment validation. All known phases are present even
when a scenario does not execute them.

Phase timings are inclusive, so a parent phase includes its nested phases.
They must not be added together to derive the scenario total. Phase allocation
uses the current thread's allocation counter. Scenario totals use RTS
statistics and are finalized after a major GC. Instrumentation is opt-in;
normal formatter entrypoints pass no collector and do not read clocks or RTS
counters.

For focused scenarios, the human table and comparison use the selected
phase's CPU and allocation values. The JSON keeps both those phase values and
the complete worker runtime, including fixture preparation. Maximum residency
is necessarily worker-wide.

## JSON report

The output file is a versioned JSON document. `schemaVersion` changes only when
the machine-readable shape changes incompatibly. Metadata includes the commit,
dirty-worktree state, compiler, Cabal version, operating system, architecture,
machine description, configuration path, Cabal optimization profile, and
invoked command. The documented command uses Cabal's normal optimized `-O1`
profile for both the library and benchmark executable.

Failed scenarios remain in the report. Expected malformed parser and layout
failures do not fail the suite. Unexpected formatter or harness failures make
the runner exit unsuccessfully after it writes the complete report.

## Comparing a performance change

Produce the baseline and candidate on the same otherwise-idle machine with the
same compiler, configuration, build flags, and suite:

```sh
"$BENCH_BIN" --suite standard --output baseline.json
"$BENCH_BIN" --suite standard --output candidate.json \
  --compare baseline.json
```

The human summary shows per-scenario CPU and allocation changes. Attach both
JSON files and the summary to the performance PR. Investigate a phase only
after confirming that its call count and outcome match between runs. Report
CPU time, allocation, and maximum residency for `Alt.hs` and `ExtractAnns.hs`.

Do not compare a dirty worktree with a clean one without documenting the
difference. Do not add noisy absolute timing gates to the ordinary test suite;
use allocation trends or repeated dedicated benchmark jobs when automation is
needed.
