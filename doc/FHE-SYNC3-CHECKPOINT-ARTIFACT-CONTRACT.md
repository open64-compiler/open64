# FHE SYNC-3 Checkpoint Artifact Contract

## Purpose

FHE conversion may produce a converted tensor side payload and a conversion
report in addition to the binary WHIRL checkpoint. Per-PU conversion callbacks
cannot safely publish those files because complete validation occurs only after
every PU has been processed. This contract adds an all-PU lifecycle without
giving FHE code access to backend driver or binary writer internals.

## Published Lifecycle

The backend-safe FHE conversion service publishes:

```c++
BOOL VHO_FHE_Convert_Register_Checkpoint_Lifecycle(
    VHO_FHE_CHECKPOINT_FINALIZER finalizer,
    VHO_FHE_CHECKPOINT_COMPLETION completion);

BOOL VHO_FHE_Convert_Checkpoint_Begin(
    const char *temporary_binary_path,
    const char *final_binary_path,
    FILE *diagnostic);

BOOL VHO_FHE_Convert_Checkpoint_Register_Artifact(
    const char *temporary_path,
    const char *final_path);

BOOL VHO_FHE_Convert_Checkpoint_Finalize(
    const VHO_FHE_CONVERT_RESULT *aggregate,
    FILE *diagnostic);

BOOL VHO_FHE_Convert_Checkpoint_Publish_Artifacts(FILE *diagnostic);
void VHO_FHE_Convert_Checkpoint_Complete(void);
void VHO_FHE_Convert_Checkpoint_Abort(void);
```

The driver begins the transaction by recording an in-process reservation for
the temporary and final binary WHIRL paths. This is not a cross-process lock;
atomic no-replace publication handles a concurrent destination race. The final
destination must not exist; a stale `.fhe.B` is rejected
before conversion begins so it cannot be mistaken for the commit marker of a
failed new run. The FHE semantic module registers one finalizer and one
completion callback. During conversion or finalization it may register owned
temporary/final path pairs. Paths are copied into runtime-only backend state.
Empty or identical endpoints are rejected. Every auxiliary endpoint must be
distinct from both endpoints of every other pair and from the reserved binary
temporary and final paths. A producer must register each path pair before
creating or opening its temporary file so the shared abort path can remove a
partially written file.

The finalizer receives the complete aggregate result after PU coverage and all
managed DSL/FHE image validation. It may finalize and digest temporary side
payloads, write a temporary report, and reject incorrect aggregate semantic
counts. It must not mutate WN, ST, TY, RID, or managed image tables at this
late boundary.

The completion callback receives `TRUE` only after every registered auxiliary
artifact and the binary WHIRL checkpoint have been published. It receives
`FALSE` on the shared abort path and must discard any FHE-owned runtime state.

## Driver Order

Checkpoint publication follows this order:

1. Convert every PU and write its in-memory checkpoint contribution while its
   local symbol table is active.
2. Validate PU coverage, aggregate counters, and all managed images.
3. Invoke the registered FHE checkpoint finalizer.
4. Write global WHIRL tables and close the temporary `.fhe.B`.
5. Preflight all registered auxiliary artifacts and publish them without
   replacement in final-path order.
6. Publish the temporary binary WHIRL file without replacement as `.fhe.B`
   last.
7. Disable rollback cleanup and invoke the successful completion callback.

This is a deterministic publication transaction, not a claim that POSIX
provides one multi-file atomic rename. The binary checkpoint is the commit
marker because it is published last. Publication uses an atomic hard-link
creation followed by removal of the temporary name. This provides no-clobber
semantics and requires each temporary/final pair to reside on the same file
system. If an auxiliary publication or the final binary publication fails, the
abort path removes all temporary files and every
auxiliary member already published by this run. Final auxiliary destinations
must not exist when publication starts. The binary final destination must not
exist when the transaction begins and is protected again by atomic no-replace
publication.

The standard backend cleanup callback invokes the same abort path for compiler
errors and handled signals. Handled signals are blocked across each atomic
publication and its corresponding published-state update, so cleanup cannot
miss a newly visible transaction member. A post-materialization failure
remains terminal; the process must not retry or continue conversion on the
mutated memory image.

## Ownership

- FHE owns converted payload bytes, payload digest verification, report
  contents, aggregate semantic checks, and lifecycle callback registration.
- `fhe_convert.cxx` owns runtime lifecycle and auxiliary-artifact records so
  `be.so` remains link-closed and has no driver-only callback dependency.
- `driver.cxx` owns all-PU ordering, binary WHIRL close, final publication, and
  the standard cleanup callback.
- Python and torch2whirl do not use these APIs and gain no backend dependency.

This stage adds no mapped-image row, ELF section, opcode, TY encoding, or
binary WHIRL revision.

## Diagnostics And Tests

- `CFHE-CHECKPOINT-004` reports invalid or failed finalization.
- `CFHE-CHECKPOINT-005` reports an unready or failed auxiliary publication.
- `CFHE-CHECKPOINT-006` reports an invalid reservation or pre-existing binary
  checkpoint destination.

`fhe_convert_contract_test.cxx` covers registration, the complete cross-alias
matrix, binary-path collision rejection,
aggregate finalization, deterministic publication, rollback after auxiliary
publication, no-clobber preservation of a foreign destination, successful
completion, stale binary destination rejection, and failed-finalizer cleanup.
`dsl_fhe_sync3_vho_driver_test.sh` checks the driver phase order and retains
the before/after conversion trace. Full FHE acceptance additionally requires a
real six-PU SecureResNet checkpoint that jointly publishes `.fhe.B`, its
converted tensor payload, the conversion report, and `ir_b2a -st -src` output.
