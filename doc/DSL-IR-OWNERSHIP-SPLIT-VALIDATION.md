# DSL Image Ownership Split And Fixes

Status: PR submission validation against `develop` at
`ecbb857fc26a83dd35c7800845b654890f0dffbd`, 2026-10-28.

## Scope

This change packages the reviewed common registry/image comment cleanup, the
`dsl_ir_image.{h,cxx}` ownership split and directly related service closure,
and the subsequent program-interface, DST lifecycle, builder-fixture and
document-digest fixes. Unrelated ordered-reduction planning source, tests,
design document and Make rules remain outside this PR. Updated `develop`'s
normalization changes are preserved, not replaced by the older working copy.

- `common/com/dsl_ir_image.h` contains only the `.B`-preserved schema: 25
  headers/fixed rows and 119 retained record/symbolic contracts. It no longer
  imports WN/PU services, transient requests or reverse owner headers.
- `common/ir_image` owns the single managed store, mapped views, structural
  checks, printing, image-only checkpoints and derived relation queries.
- `be/com` owns backend-free physical PU verification, external-tensor source
  views, rewrite/lower/retype mechanisms and runtime/program ABI transactions.
  `be/vho` owns physical CKKS expansion. The string checkpoint remains with
  the existing `common/util` string store.
- The former implementation and service companions are relocated, not left
  behind as forwarding sources or duplicate stores. Direct consumers and
  source-list lookup are migrated together. No new backend library is added.
- Contract comments describe borrowed lifetimes, scope, preflight, mutation,
  failure and rollback limits. Trivial struct comparator methods are explicitly
  inline in their owner-private headers. Registry comment cleanup preserves
  all non-comment tokens in its six header/implementation files.

The directory change deliberately changes service include locations. Public
names/signatures and C++ linkage are preserved; a schema-to-service include
shim is not retained. Existing managed row layouts, section contracts,
versions, opcode values and reader/writer behavior are unchanged. This is
not repository-wide common/com ownership or legacy-comment certification.

## Follow-up Fixes

1. Program-interface fixtures now define projected local handles after native
   definitions are removed. Missing/self-referential definitions and inactive
   PU validation remain independent negative cases; production validators are
   not weakened.
2. Builder begin/abort/lazy restart use the existing frontend `DST_init_mem`
   boundary to reset producer cursors, counters and locks as well as memory.
   Two PUs within a program still share the CU and source-file numbering.
   Mapped readers retain memory-only DST initialization.
3. Legacy native/transformer fixtures prove unsealed rejection and then seal
   the selected PU's restored descriptors before mapped publication. FHE
   lookup checks the existing authenticated typed TCON and rejects explicit
   zero overrides; checksum-free legacy references still permit zero TCONs.
   REGION source locations identify the real C++ producer.
4. Development-only `Is_True` checks catch internal verifier PU/map and
   status/count contradictions. Deliberately malformed inputs still follow
   documented diagnostic/failure paths, and release checks remain intact.
5. The Python authority test distinguishes the original SYNC-3 v0.10 digest
   from the later committed section 11.9 revision at the same DOCX path.
   Git objects, ZIP integrity, all XML parts and the complete content diff
   were verified before re-pinning. Historical certifications remain unchanged.
6. PR integration caught a physical-clone harness link dependency: its explicit
   frontend builder object now also links `dwarf_DST_producer.o`, which supplies
   the frontend-only fresh-program initializer. `be.so` does not gain the
   frontend initializer or any builder dependency. Shared consumers are built
   before the harness so updated develop's shape helpers are current.

## Fresh PR Validation

Linux GNU x86-64 shared build under Docker on the development host:

| Gate | Result |
| --- | --- |
| Independent Clang before/after source oracle | 351 function-definition bodies and 119 schema contracts unchanged |
| Registry comment-only token comparison | All six common registry header/implementation files match develop |
| Rebuild | `ir_b2a`, builder/shape/clone tools, `be`, `be.so`, `lw_inline`, WOPT and native Python extension pass |
| Native focused contracts | All nine lanes pass: shape/retype, runtime ABI, program ABI, external rewrite, FHE native rewrite, standard lowering, grouped lowering, dead constants and CKKS expansion |
| Combined builder and physical clone | Pass, including negative contracts and DST lifecycle |
| Isolated fixtures | Production-native, FHE SYNC-1, all three Llama families and three independent two-PU DST programs pass |
| Mapped compatibility | 28 current `.B` images reopen in independent pre-split and PR-reader processes; all 28 full `-st -src` dumps match |
| Additional rollback/publication | Cross-TY view/REGION retirement and six CKKS2C checkpoint publication cases pass |
| Syntax/layout | Complete native syntax matrix and six physical operator configurations pass |
| Shared boundaries | No `DSL_Builder_*` or `Json::` symbols enter `be.so`/`lw_inline`; loader closure passes |
| Python | 292 run, 291 pass, one external ACE-source skip, zero failures/errors |
| Instruction/comment checks | New owner files have no detected function-comment gaps; actual contracts retain their prior manual reviews; eight selector self-tests pass |
| Hygiene | Full PR diff passes `git diff --check`; new C++/Python files contain no tabs |

The single Python skip is `test_pinned_ace_source_hashes` because
`ACE_COMPILER_ROOT` is unset. The three additional tests relative to the earlier
289-test run come from updated develop's native normalization coverage.

Representative retained dump evidence, not just a pass/fail label:

```text
tensor_tcon storage=side_file descriptor_ty=13570 element=F4 elements=4 bytes=16 alignment=4
```

The complete row also records the authenticated side-file range/checksum.
The program-interface dumps preserve projected handle definitions and call ABI
joins. Each DST image contains two procedures sharing one CU/source identity.
REGION dumps interleave the original C++ source rather than a nonexistent
Python pathname. Every old/new-reader comparison retains its raw, empty diff.

## Reproduction

Mount this source at `/open64`, a configured Linux build at `/build`, and a
fresh host-bound staging directory at `/evidence`. Build the shared consumers
before the frontend-linked clone harness. The retained command record contains
the complete Docker mount vectors, image identities and environment settings.

```sh
make -C /build/osprey/targdir/be -j8
make -C /build/osprey/targdir/lw_inline -j8
make -C /build/osprey/targdir/wopt -j8
make -C /build/osprey/targdir/ir_tools -j8 SKIP_DEP_BUILD=1 \
  ir_b2a dsl_builder_contract_test dsl_shape_refine_contract_test \
  dsl_pu_transaction_contract_test
make -C /build/osprey/targdir/torch2whirl -j8 python_native_extension \
  PYTHON_INCLUDE=/usr/include/python3.8
PYTHONDONTWRITEBYTECODE=1 \
PYTHONPATH=/build/osprey/targdir/torch2whirl/python:/open64/osprey/torch2whirl/python \
python3 -m unittest discover -v -s /open64/osprey/torch2whirl/python/tests
```

The ownership oracle's source/Clang commands are documented in the
[inventory](DSL-IR-PERSISTENCE-OWNERSHIP-INVENTORY.md). Fixtures retain exact
producer and `ir_b2a -st -src` commands beside their binary/trace families.

## Retained Evidence

Large generated evidence remains local and Git-ignored. These are immutable
host receipts, not public download links. Resolve them beneath
`/Users/shinmingliu/open64/artifacts/review/code-quality/`:

| Evidence family | Immutable receipt |
| --- | --- |
| Comment contracts and token/object/layout checks | `dsl-comments/batch-1-registries` through `batch-15-remaining-header-contracts`, individually indexed receipts |
| Original owner split and source/reader comparisons | `dsl-image-ownership/full-pair-split/sha256-c981aec45da67fe102e39a0194921c775e6a4521ee5f1a6da9625707c88d508e` |
| Program-interface and DST fixes | `dsl-image-ownership/validation-fixes/sha256-0610ecfcbbbcb2831948a07345dae605290dd3d6891f1defff2386d9143d0ba6` |
| Builder sealing, FHE lookup and development assertions | `dsl-image-ownership/combined-builder-fixes/sha256-7f242f48aaea5258532ba8f234cd7919d0bad9002553d84cfa0143fd12d5f573` |
| Verified authority revision and digest fix | `dsl-image-ownership/python-document-digest/sha256-33f69514ddfbb4459e3350fdc27642b0b64423efe3c3343836b69c6bed734baa` |
| Fresh merged PR validation | `dsl-image-ownership/pr-submission/sha256-8bb75c64663b5d46fa7b442fa3891fd54fd4a315c32a2f4fa0bb3879214314b3` |

The PR receipt retains source snapshots, all 28 `.B` files, matching old/new
`.T` files, raw diffs, producer/build/negative diagnostics, symbol/loader
reports, source oracle and comment audit, full Python output, original
source/patch manifest, commands, `receipt.json` and `SHA256SUMS`.
It is independently replayed from its durable path before PR submission.

## Historical Exceptions And Limits

The original relocation run predated the follow-up fixes: eight of nine native
lanes passed and program/DST failures were explicitly retained. Those records
are not rewritten; the subsequent receipts and fresh PR results supersede the
failure status without claiming the original run passed. The original raw
binary comparison had two one-byte differences in reserved local ST row zero;
all meaningful rows and all 17 paired dumps matched. Raw differences and their
source attribution remain retained rather than normalized away.

This is shared Linux build, native contract, source-equivalence and mapped
compatibility evidence, not a full static/release compiler, full-model,
external ACE execution, numerical/GPU or timing/performance certification.
Prior assertions-disabled checks remain historical evidence; this PR replay
does not claim a fresh whole-release build. `WHIRL.pdf` is absent and no IR
representation, layout or version change is designed or approved here.
