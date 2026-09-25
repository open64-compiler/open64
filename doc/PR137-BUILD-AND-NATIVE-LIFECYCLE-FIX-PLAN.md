# PR 137 Build and Native Lifecycle Fix Plan

## 1. Purpose

This document defines a focused repair plan for the build and native-test
problems found while validating PR 137 at commit
`7cac0d86b6548805b53c05675ffb7e1dff9395fa`.

It is intentionally separate from the existing PR 137 review record. It does
not import, reprioritize, or attempt to resolve the semantic review findings in
that record.

## 2. Scope

This plan covers only:

1. Closing the build dependencies introduced by PR 137.
2. Attributing the repeated native-export DST failure to either PR 137 or its
   `develop` base.
3. Fixing the DST lifecycle only if the attribution shows that PR 137
   introduced it or changed the lifecycle contract that exposes it.
4. Rebuilding and rerunning the directly affected tests.

The following work is explicitly out of scope:

- GCC 13 exposure of the invalid `Generate_Core()` return contract.
- LLVM 15 compatibility for `BIstrlcpy` and `BIstrlcat`.
- LLVM system-library discovery and linkage.
- glibc 2.39 compatibility for `__morecore` declarations.
- Fortran frontend and runtime builds.
- Any finding already tracked by the separate PR 137 review and remediation
  documents.

The generic GCC, LLVM, and glibc items should be handled in a separate branch
based on `develop`. Temporary compatibility changes may be applied only in an
isolated validation worktree and must not be committed as part of this repair.

## 3. Confirmed Baseline

The following behavior has already been observed against the frozen PR head:

- Standalone `torch2whirl` builds successfully.
- Its command-line smoke tests pass.
- The Python suite ran 170 tests, with 13 skipped.
- The PyTorch suite passes 56 tests.
- The driver PyTorch smoke test passes.
- The full Open64 configure step succeeds.
- The unmodified full build does not complete.
- `wgen42` links `dsl_ir_rewrite.o`, which references
  `DSL_Shape_Tensor_Core_Complete`, `DSL_Shape_Fact_From_Type`, and
  `DSL_Gatekeeper_Verify_PU_Mode`, but its copied common-source list does not
  include the translation units that define those symbols.
- An isolated validation patch that added the missing common sources, together
  with separate Ubuntu 24 compatibility changes, completed `make build`,
  `make lib`, and `make install`; the installed `opencc` compiled and ran a C
  smoke program successfully.
- Process-boundary native IR and `opencc` smoke tests pass.
- The complete in-process `python_native_test` suite aborts when a second
  native export initializes DST file scope. Each of the first two affected
  tests passes when executed alone in a fresh Python process.

These results establish that the environment is capable of building and
running the relevant components. They do not establish that the unmodified PR
head is build-clean.

## 4. Workstream A: Close the PR 137 Build Dependency Graph

### 4.1 Objective

Every frontend or utility that compiles `dsl_ir_rewrite.cxx` must also link the
common DSL implementations required by that object. The fix must be expressed
in the source lists, not by weakening symbol use or adding frontend-to-backend
dependencies.

### 4.2 Implementation steps

1. Audit every `Makefile.gbase` source list that includes
   `dsl_ir_rewrite.cxx`.
2. For each link target that consumes that object, determine whether
   `dsl_shape.cxx` and `dsl_gatekeeper.cxx` are already supplied through
   another object or library.
3. Add only the missing common translation units to the copied common-source
   list.
4. Keep the dependency inside `osprey/common/com`; do not introduce a
   dependency on `osprey/be`, `torch2whirl`, Python, or code-generation
   internals.
5. Check the generated object names for collisions and ensure each source is
   linked exactly once per target.

The audit must cover the currently observed copied source lists in:

- `osprey/wgen/Makefile.gbase`
- `osprey/clang2whirl/Makefile.gbase`
- `osprey/kgccfe/Makefile.gbase`
- `osprey/kg++fe/Makefile.gbase`
- `osprey/crayf90/sgi/Makefile.gbase`
- `osprey/libwffi/Makefile.gbase`
- `osprey/jfe/libb2w/wgen-java/wgen/Makefile.gbase`

The Fortran and Java products do not become acceptance targets merely because
their shared source lists are audited. The goal is to prevent the copied
source lists from drifting into different link contracts.

### 4.3 Required verification

- Search all build descriptions to prove that no consumer of
  `dsl_ir_rewrite.cxx` was missed.
- Configure a clean full build.
- Build all enabled C/C++ compiler components.
- Confirm that no unresolved `DSL_Shape_*` or `DSL_Gatekeeper_*` symbol remains
  in the build log.
- Install the C/C++ toolchain.
- Run `opencc -v`.
- Compile and run a small C program with the installed `opencc`.
- Rerun the standalone `torch2whirl` command-line, Python, PyTorch, and driver
  smoke tests to detect accidental frontend coupling.

### 4.4 Exit criteria

- A clean build no longer fails at `wgen42` or another copied-source consumer
  because of missing DSL shape or gatekeeper definitions.
- No new backend or Python dependency is introduced.
- The source-list fix works in the collaborator's Ubuntu 20/LLVM 11 reference
  environment.
- The same fix works in the Ubuntu 24 validation environment when the generic
  compatibility overlay is applied separately.

## 5. Workstream B: Attribute the Native DST Lifecycle Failure

### 5.1 Objective

Determine whether the in-process repeated-export failure belongs to PR 137
before changing product code.

### 5.2 Attribution matrix

Run the same minimal two-export reproducer in isolated build trees for:

1. PR 137 head `7cac0d86b6548805b53c05675ffb7e1dff9395fa`.
2. PR 137 base `590bdf3d58944cb93aafd89480a5590a863ded60`.

Both runs must use the same compiler, Python environment, configure flags, test
order, and compatibility overlay. Record whether each individual export and
the sequential pair pass or fail.

Decision rule:

- If only PR 137 fails, continue with Workstream C in this branch.
- If both revisions fail identically, record it as a pre-existing `develop`
  issue and move its implementation fix to a separate `develop`-based branch.
  PR 137 must not hide the problem by splitting tests into separate processes.
- If results vary with test order or environment, reduce the reproducer until
  the owner of the stale global state is identified before deciding scope.

### 5.3 Required evidence

- The exact two test names and execution order.
- Fresh-process results for each test individually.
- Same-process results for both orders.
- The assertion or diagnostic showing the second DST file-scope start.
- A stack trace reaching `DSL_Builder_Create_Minimal_PU` and the DST producer.
- Base-versus-head result table with exact commit IDs.

### 5.4 Exit criteria

- The failure is reproducible with a minimal deterministic test.
- Its ownership is assigned to PR 137 or the base using executable evidence,
  not inference from the newer host toolchain.

## 6. Workstream C: Repair DST Lifecycle if PR 137 Owns the Failure

This workstream is conditional on Workstream B assigning the regression to
PR 137.

### 6.1 Design constraints

- Program, file, PU, and DST ownership must remain explicit.
- Initialization and finalization must be symmetric and safe across multiple
  sequential exports in one process.
- A failed export must not poison the next export.
- The repair must reset only frontend-owned state. It must not erase state
  owned by an active compiler phase or another PU.
- Do not work around the defect with subprocess isolation, test reordering, or
  by disabling the DST assertion.

### 6.2 Implementation steps

1. Trace the native export entry, program creation, minimal-PU creation,
   artifact finalization, and error cleanup paths.
2. Identify every process-global or file-global DST variable changed during an
   export.
3. Define one owner for starting and ending DST file scope.
4. Add idempotent cleanup for normal completion and every failure path after
   initialization.
5. Ensure a second program starts from the same clean state as the first.
6. Add a focused lifecycle test before expanding the full suite.

### 6.3 Required tests

- One successful native export.
- Two successful exports sequentially in one Python process.
- The same two exports in reverse order.
- A failed export followed by a successful export.
- Repeated save/reopen inspection with `ir_b2a -st -src`.
- The complete `python_native_test` suite.
- `python_native_ir_tools_smoke`.
- `driver_native_ir_tools_smoke`.
- `driver_opencc_smoke` using the installed compiler.

### 6.4 Exit criteria

- No duplicate DST file-scope start occurs.
- Every export produces an independently readable binary WHIRL artifact.
- Failure cleanup leaves the next export usable.
- All native and process-boundary smoke tests pass without process-isolation
  workarounds.

## 7. Validation Environments

Two lanes are required because build compatibility and PR correctness are
separate claims.

### Reference lane

- Collaborator's x86_64 Ubuntu 20 image created by
  `build-open64-docker.sh`.
- LLVM 11 and the image's original compiler stack.
- PR 137 repair commits only.

This lane proves that the repair remains compatible with the project's known
lower-version environment.

### Current diagnostic lane

- Ubuntu 24, GCC 13, LLVM 15, and glibc 2.39.
- PR 137 repair commits plus an explicitly separate, uncommitted compatibility
  overlay.

This lane provides additional evidence but must not cause generic portability
changes to enter PR 137.

For both lanes, record the source commit, dirty state, configure command,
compiler and LLVM versions, test commands, exit status, and retained artifact
paths.

## 8. Proposed Commit Structure

Keep the repair reviewable with small commits:

1. `build: close PR 137 DSL common-source dependencies`
2. `tests: add repeated native-export DST reproducer`
3. `frontend: reset native DST lifecycle between exports` -- only if PR 137
   owns the failure
4. `tests: certify PR 137 build and native lifecycle repair`

The plan documents may be committed separately. Generic toolchain
compatibility changes must not be included in these commits.

## 9. Final Acceptance Gate

The focused repair is complete only when:

1. The branch diff contains only the planned build and, when applicable, DST
   lifecycle changes.
2. `git diff --check` and the repository no-tab checks pass.
3. A clean C/C++ Open64 build and install complete in the reference lane.
4. The installed `opencc` compiles and runs a C smoke program.
5. Standalone `torch2whirl`, Python, PyTorch, and driver tests remain green.
6. The DST base-versus-head attribution is retained.
7. If PR 137 owns the DST failure, the complete native suite and all
   process-boundary smoke tests pass after the repair.
8. If the base owns the DST failure, no DST product-code change is included in
   PR 137 and a separate `develop` follow-up is recorded.
9. Fortran failures are reported as excluded rather than treated as blockers.

## 10. Implementation Status

Status: partially implemented and validated; the final acceptance gate is not
yet complete.

### Phase 1: build dependency closure

The build dependency closure has been implemented. `dsl_gatekeeper.cxx` and
`dsl_shape.cxx` were added once to each copied common-source list in these
seven files, for 14 insertions total:

- `osprey/wgen/Makefile.gbase`
- `osprey/clang2whirl/Makefile.gbase`
- `osprey/kgccfe/Makefile.gbase`
- `osprey/kg++fe/Makefile.gbase`
- `osprey/crayf90/sgi/Makefile.gbase`
- `osprey/libwffi/Makefile.gbase`
- `osprey/jfe/libb2w/wgen-java/wgen/Makefile.gbase`

All 10 build consumers of `dsl_ir_rewrite.cxx` were audited. The other three,
`osprey/ir_tools/Makefile.gbase`, `osprey/be/be/Makefile.gbase`, and
`osprey/torch2whirl/Makefile.gbase`, already had a closed source or object
dependency set and were not modified. The audit found one rewrite, gatekeeper,
and shape object per consumer and no source basename collision.

Before the focused fix, the frozen PR head failed while linking `wgen42`
because `DSL_Shape_Tensor_Core_Complete`, `DSL_Shape_Fact_From_Type`, and
`DSL_Gatekeeper_Verify_PU_Mode` were undefined. After the focused fix, the
isolated Ubuntu 24 diagnostic lane passed a clean build and install, an
installed-`opencc` C compile-and-run smoke test, standalone `torch2whirl`
command-line checks, the Python suite (170 tests run, 13 skipped), the PyTorch
suite (56 passed), the driver smoke test, and the common DSL syntax fixture.

The Ubuntu 24 build used a separate, uncommitted generic compatibility overlay.
No overlay change entered the focused worktree. The first SELF runtime
`make lib` failed because the build-tree `libgcc_s.so.1` did not provide the
`GCC_4.3.0` symbol required by the host `libstdc++.so.6`; this was treated as an
environment/toolchain issue, not a focused product fix. A clean runtime rebuild
and install using the existing `LIB_BUILD_COMPILER=GNU` lane then passed.

The Fortran and JFE builds were excluded from the acceptance scope. Their
copied source lists were updated for link-contract consistency, but neither
product is an acceptance gate for this repair.

### Phases 2 and 3: DST attribution and conditional repair

The symmetric native attribution lanes used PR head
`7cac0d86b6548805b53c05675ffb7e1dff9395fa` and develop base
`590bdf3d58944cb93aafd89480a5590a863ded60`, both on Ubuntu 24 with GCC 13,
LLVM 15, Python 3.12, identical configure flags and reproducer commands, and no
compatibility overlay. On both revisions, fresh-process A and B passed, while
same-process AB and BA failed on the second export with
`Illegal attempt to start DST file-scope twice`. GDB showed the same producer
state and call path in both lanes:
`DST_mk_compile_unit <- DSL_Builder_Create_Minimal_PU`.

Under the plan's decision rule, the repeated-export DST failure is a
pre-existing `develop` issue. Phase 3 was not implemented, and this PR 137
repair branch must not contain a DST product-code fix.

### Phase 4: independent verification

Focused implementation and retained evidence PASS; complete acceptance gate
BLOCKED on the unexecuted Ubuntu 20/LLVM 11 reference lane.

The independent verifier confirmed the seven-file, 14-insertion build change,
closed source or object dependencies for all 10 consumers, unchanged expected
SHA-256 values for both protected review documents, and separation of the
Ubuntu 24 compatibility overlay from the focused worktree. The verifier
reviewed the retained installed-`opencc` C-smoke compile log, reran the retained
C-smoke binary, and separately ran installed `opencc -v` and standalone
`torch2whirl --version`; each rerun exited 0. The independently checked head and
base A/B/AB/BA matrices were both 0/0/1/1, so skipping the conditional phase 3
DST product-code repair was correct. The verifier reviewed the retained
full-build and test evidence but did not duplicate the complete long-running
build and test suite.

### Remaining gates and retained evidence

The Ubuntu 20/LLVM 11 reference lane was not run because Docker is unavailable
on the current machine. Therefore the complete final acceptance gate is not
satisfied, and this status does not claim that the focused repair is fully
complete or passed.

Retained phase 1 evidence:

- Root: `/home/zikai/workspace/open64-workspace/validation/pr137-phase1-build-closure-20260923-01`
- Command and result summary: `/home/zikai/workspace/open64-workspace/validation/pr137-phase1-build-closure-20260923-01/artifacts/commands-and-results.txt`

Retained phase 2 evidence:

- Root: `/home/zikai/workspace/open64-workspace/validation/pr137-dst-attribution-20260923-01`
- Attribution report: `/home/zikai/workspace/open64-workspace/validation/pr137-dst-attribution-20260923-01/evidence/common/attribution-report.md`
- Machine-readable comparison: `/home/zikai/workspace/open64-workspace/validation/pr137-dst-attribution-20260923-01/evidence/common/comparison.tsv`
