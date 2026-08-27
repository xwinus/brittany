# Transactional inplace formatting

Multi-file `--write-mode=inplace` execution separates formatter work from
target mutation. Each distinct input path is read and transformed into a
temporary candidate while the normal parser, comment, fallback, output, and
semantic checks run. Diagnostics are prefixed with the input path. No target is
written during this plan phase.

If any planned file fails, all candidates are removed and the existing batch
exit-code rules apply. This provides **validation atomicity**: formatter or
validator failures leave every target byte-identical.

## Commit protocol

After every plan succeeds, changed candidates enter the filesystem transaction:

1. A replacement and recovery copy are staged next to each target so rename
   operations remain on the target filesystem.
2. The target's `System.Directory.Permissions` are applied to both files.
3. Immediately before each replacement, the target bytes are compared with the
   snapshot taken before formatting. A difference aborts the commit rather than
   overwriting an external edit.
4. Each staged replacement is atomically renamed over its target.
5. If an ordinary read or rename operation fails after earlier replacements,
   their recovery copies are renamed back in reverse commit order.
6. Candidate, staging, and recovery files are removed on success, validation
   failure, handled I/O failure, and asynchronous interruption before commit.

Unchanged files are not staged or renamed. This allows read-only canonical
files to participate in a successful batch and avoids unnecessary metadata
changes. Repeated identical path arguments are planned once.

## Guarantee boundary

The transaction provides rollback for ordinary I/O failures reported to the
running process. Filesystems do not offer one atomic transaction across
multiple paths. A process crash, forced termination, kernel failure, or power
loss after one rename and before the remaining renames can therefore expose a
partially committed batch or leave recovery files. Extended attributes,
ownership, and timestamps are not preserved; the portable permissions exposed
by `System.Directory` are preserved.

Display, stdin/stdout, and check modes retain their non-mutating execution path.
