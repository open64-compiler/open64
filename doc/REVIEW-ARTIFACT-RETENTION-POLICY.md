# Review Artifact Retention Policy

Status: normative policy for expensive, human-reviewable Open64 compiler
evidence that is intentionally not committed to Git.

## Purpose

Milestone artifacts are part of design review, not disposable test debris.
They let a reviewer inspect compiler progress without rerunning a costly model
producer, and they let downstream tests reuse an authenticated binary when the
producer inputs are unchanged. Storage is cheaper than repeating hour-long
runs during this stage of the project.

`/private/tmp`, a container filesystem, and a managed Codex worktree are
staging locations only. None is an acceptable sole copy of an accepted
milestone artifact.

## Canonical Location

The durable review root is:

```text
<primary-open64-root>/artifacts/review/
```

On the current development machine this is:

```text
/Users/shinmingliu/open64/artifacts/review/
```

The repository's root `.gitignore` excludes `/artifacts`, so neither the
review root nor its large binary contents are candidates for Git commits.
Managed worktrees must use the primary checkout's absolute review root rather
than create the only retained copy inside a worktree that may be archived.

Use this hierarchy:

```text
artifacts/review/
  INDEX.md
  index.json
  <domain>/
    <sync>/
      <milestone>/
        sha256-<full-primary-artifact-digest>/
          receipt.json
          SHA256SUMS
          source/
          binary/
          traces/
          payloads/
          generated-c/
          compile/
          reports/
          logs/
```

For the current FHE checkpoint, the destination will be:

```text
/Users/shinmingliu/open64/artifacts/review/fhe/sync-6/s6-0c/
  sha256-d7bccc537c422b2266add7437040cac14ef7b33af93c9f5e65eb7ebb5f89876f/
```

## Promotion Contract

1. Produce into a fresh staging directory outside `artifacts/review`.
2. Finish all producer, mapped-reopen, source-interleave, independent-audit,
   malformed-input, and atomic-publication checks required by the milestone.
3. Generate `SHA256SUMS` over every retained file.
4. Generate `receipt.json` with the domain, sync point, milestone, status,
   primary artifact and digest, producer commit and binary digest, exact option
   vector, input digests, start/end times, trace/audit identities, measured
   census, and source staging path.
5. Copy to a sibling temporary directory under the final milestone directory,
   verify every digest again, then atomically rename it to the full
   `sha256-<digest>` destination using no-replace behavior.
6. Update `INDEX.md` and `index.json` only after the receipt directory exists.
   A human-friendly `current` symlink may point to an accepted receipt, but it
   is never the identity or the only route to the data.
7. Keep staging evidence until the durable receipt has been independently
   reopened from its final path. Disposable staging may then be cleaned under
   the normal test policy.

An existing receipt destination is immutable. A repeated producer either
matches it byte-for-byte or creates a different content-addressed directory.
It never overwrites the existing receipt.

## Required Receipt Contents

An expensive compiler milestone receipt retains, as applicable:

- the binary WHIRL `.B` and separate-process `ir_b2a -st -src` `.T`;
- the source file at the pathname needed for source interleave;
- external plaintext, model, coefficient, calibration, and other side files;
- deterministic conversion, materialization, code-generation, and audit
  reports;
- phase traces, diagnostics, command log, environment summary, and elapsed
  times;
- generated C or other review products and their compile/run evidence;
- `SHA256SUMS` and `receipt.json`.

## Stable Human Review Paths

Each milestone directory maintains a `current` symlink after successful
promotion. The symlink is a convenience; the immutable full-digest receipt is
the authority. Updating `current` never removes the older receipt.

The complete S6-0c CKKS-semantic WHIRL family is examined at:

```text
/Users/shinmingliu/open64/artifacts/review/fhe/sync-6/s6-0c/current/
  binary/secure_resnet20.ckks_ops.B
  traces/secure_resnet20.ckks_ops.T
  source/secure_resnet20.py
  payloads/secure_resnet20.ckks_ops.B.conv-plaintexts.f32
  payloads/secure_resnet20.ckks_ops.B.relu-scalars.f64
  payloads/secure_resnet20.ckks_ops.B.tail-plaintexts.f32
  reports/complete-audit.json
  reports/complete-audit.txt
  logs/commands.txt
  SHA256SUMS
  receipt.json
```

The S6-0d D3 CKKS2C transactional output is a separate derived receipt:

```text
/Users/shinmingliu/open64/artifacts/review/fhe/sync-6/s6-0d/d3/current/
  binary/secure_resnet20.ckks2c.B
  traces/secure_resnet20.ckks2c.T
  generated-c/secure_resnet20.ckks2c.c
  compile/secure_resnet20.ckks2c.o
  reports/secure_resnet20.ckks2c-report.txt
  reports/group-manifest.json
  logs/commands.txt
  logs/compile.log
  SHA256SUMS
  receipt.json
```

The D3 receipt records the exact S6-0c source receipt and `.ckks_ops.B` digest,
so a reviewer can move from generated C back to its complete typed WHIRL input.

The receipt records whether evidence is focused, cached-artifact inspection,
or fresh whole-model certification. It must not upgrade historical or partial
evidence into a milestone claim.

## Deletion And Mutation Rules

Nothing under `artifacts/review` may be deleted, overwritten, truncated,
renamed out of the review root, or pruned unless the user explicitly approves
that operation and identifies the exact receipt or subtree. The following are
not deletion approval:

- beginning another test or milestone;
- obtaining a newer artifact;
- a branch merge, deletion, rebase, or worktree archive;
- an age threshold, disk-space threshold, operating-system cleanup policy, or
  automated retention policy;
- a request to clean ordinary build or temporary output.

Automation must not execute `rm -rf`, recursive replacement, or generic clean
targets against `artifacts/review` or an ancestor that would include it. When
space is insufficient, stop before the run and request a storage decision.

Approved removal must first write a deletion record to the local index with
the approved path, date, reason, and approving user statement. Removing local
evidence does not change historical hashes or claims in checked-in documents.

## Review And Recovery

`INDEX.md` is the reviewer entry point and lists milestone, status, primary
hash, size, creation date, producer commit, and direct relative links to the
binary trace and reports. `index.json` provides the same inventory for tools.

Every downstream cached-artifact test must verify the receipt and artifact
hashes before use. If the durable copy is missing or differs, fail closed and
report a retention incident. Do not silently regenerate an expensive producer
or substitute an older checkpoint.

Temporary artifacts remain useful during a run, but only the durable review
root is the project-controlled retention boundary.
