# WHIRL2C DSL Implementation Plan

Status: working plan, July 2026.

## Purpose

`whirl2c` historically tries to regenerate compilable C from canonical WHIRL.
For very-high-level DSL WHIRL, that is not the first goal.  Until DSL operators
are lowered to C-equivalent WHIRL, `whirl2c` should produce a readable
C-extension diagnostic projection.

The output should help reviewers debug tensor values, tile views, logical DSL
operators, source correlation, and lowering readiness.  It should not claim to
be ordinary C, allocate new binary WHIRL encodings, expose private physical
`OPR_DSL` storage, or bypass `ir_b2a -st -src` as the compatibility gate.

## Design Model

The closest model is Triton-C Listing 1 from the original Triton paper: C-like
syntax plus explicit tile declarations, tile pointer declarations, mask tiles,
tunable tile shapes, tile loads, tile dot operations, and predicated stores.
That style is intuitive for debugging because element type, tile role, tile
shape, and operator intent are visible in one place.

The initial `whirl2c` DSL dialect should prefer compact extension keywords:

```c
float16 TENSOR /* tensor[M,K], strides=[stride_am,stride_ak] */ A;
int32 TILE_IDX row[BM] = GLOBAL_RANGE(0);
float16 *TILE_PTR A_ptr[BM, BK] =
    TILE_PTR_VIEW(A, row[:, newaxis], kidx);
bool MASK A_mask[BM, BK] = row < M && kidx < k;
float16 TILE A_tile[BM, BK] = LOAD(A_ptr, A_mask, 0);
float32 TILE C_acc[BM, BN] =
    OPR_DSLDOT.v1(A_tile, OPR_DSLTRANS.v1(B_tile), C_acc);
STORE(C_ptr, C_acc, C_mask);
```

The dialect is a diagnostic output convention, not a new source-language ABI.
Element type should appear in declarations because it is the fastest way to
read tensor and tile state during debugging.

## Compatibility Rules

1. Preserve binary WHIRL compatibility.  Do not change WHIRL image layout, ELF
   section contracts, opcode encodings, type-kind encodings, or reader/writer
   behavior for this workstream.
2. Treat physical `OPR_DSL` as private.  `whirl2c` must use logical APIs such
   as `DSL_WN_Is_Native`, `DSL_WN_operator`,
   `DSL_WN_Get_Logical_Opcode`, and `DSL_OPERATOR_name`.
3. Keep `ir_b2a -st -src` as the normative inspection and compatibility gate.
   `whirl2c` is an additional review surface.
4. Do not lower DSL operators prematurely in `whirl2c`.  It prints what exists;
   lowering remains owned by VHO/lowering passes.
5. Keep tensor semantic identity separate from tile views.  A full tensor
   descriptor owns element type, rank, shape, layout, strides, placement, and
   semantic state.  A tile view records origin, region, tile shape, memory
   space, boundary policy, and operator role.

## Output Vocabulary

### Tensor Declarations

Use a carrier keyword with element type in the declaration:

```c
float32 TENSOR /* tensor[64,64], layout=row_major, strides=[64,1] */ A;
```

The comment should be derived from TensorDescriptorIR facts on the `TY_IDX`:
rank, shape, layout, strides, placement, memory, quantization, runtime state,
lineage, traits, and canonical/diagnostic state when present.

### Tile Views

Print tile views as derived values:

```c
float32 TILE /* origin=A, region=[0:16,0:32], shape=[16,32],
                strides=[64,1], memory=global, role=matmul.lhs */ A_tile;
```

When tile index or pointer structure is available, use explicit tile variables:

```c
int32 TILE_IDX row[BM] = GLOBAL_RANGE(0);
float32 *TILE_PTR A_ptr[BM, BK] =
    TILE_PTR_VIEW(A, row[:, newaxis], kidx);
float32 TILE A_tile[BM, BK] = LOAD(A_ptr, A_mask, 0);
```

### Logical DSL Operators

Print logical operator names and versions:

```c
C_acc = OPR_DSLDOT.v1(A_tile, OPR_DSLTRANS.v1(B_tile), C_acc)
        /* result_tile=[BM,BN], accum=float32 */;
```

Do not print `OPR_DSL`, `OPC_MDSL`, `MDSL`, or private node/table IDs in user
output.  Private IDs may only appear in explicitly requested low-level debug
channels.

### Metadata Comments

Print PU identity and callsite metadata near function headers and calls once the
existing DSL image APIs expose the needed data:

```c
/* dsl_pu: class=TransformerBlock instance=layers.0 ordinal=0 */
/* dsl_callsite: callee=attention.q_proj context=forward ordinal=3 */
```

## Implementation Stages

### Stage 0: Documentation and Naming

- Land this implementation plan.
- Keep `doc/TENSOR-DSL-API-COMPARISON.md` as the research backing.
- Update `doc/WHIRL-DSL-TENSOR-TYPE-HANDLING.md` if the output vocabulary
  changes.
- Decide whether the short keywords `TENSOR`, `TILE`, `TILE_PTR`, `TILE_IDX`,
  and `MASK` need a prefix in a later compatibility mode.  The current plan
  keeps them short for readability.

### Stage 1: Tensor Type Projection

- Add or refine `TY2C_tensor()` in `osprey/be/whirl2c/ty2c.cxx`.
- Wire `KIND_TENSOR` into `TY2C_Handle`.
- Print element type directly in the declaration.
- Attach TensorDescriptorIR facts in a stable comment.
- Tolerate malformed `KIND_TENSOR` carriers by printing a missing-descriptor
  diagnostic rather than crashing.
- Do not add new type kinds, binary sections, or reader/writer behavior.

### Stage 2: Logical DSL Expression Projection

- Add `WN2C_dsl()` in `osprey/be/whirl2c/wn2c.cxx`.
- Register it for the physical carrier only as an implementation detail.
- Resolve the logical operator through the DSL accessor API.
- Print `OPR_DSL<NAME>.v<version>(...)` or the established stable logical
  spelling once the registry names are confirmed.
- Recursively print operand kids as direct operands.
- Print typed attributes, result facts, tile roles, and important masks as
  comments when available.

### Stage 3: Tile View Projection

- Introduce a printer helper for tile-like values derived from tensor
  descriptors, DSL node attributes, or explicit DSL image rows.
- Print tile origin, origin region, tile shape, element type, layout/stride,
  memory space, boundary policy, and role.
- Keep tile values distinct from full tensor declarations.
- Prefer element type plus tile kind in the declaration:

```c
float16 TILE A_blk[BM, BK] = LOAD(A_ptr, A_mask, 0);
```

### Stage 4: Metadata Projection

- Use `DSL_IR_Image_Get_*`, `DSL_Effect_Image_Get_*`,
  `DSL_Call_Image_Get_*`, `DSL_Call_Image_Find_PU_Identity`, and
  `DSL_Call_Image_Find_Callsite` where available.
- Print stable PU identity near function headers.
- Print stable callsite metadata near calls and DSL call projections.
- Keep compiler metadata out of tensor type equivalence.

### Stage 5: Tests and Review Artifacts

- Add deterministic native tests under an appropriate whirl2c or common/com
  test location.
- Use small binary WHIRL fixtures that include:
  - tensor declarations;
  - `common.add`;
  - `common.matmul` or tile-equivalent dot;
  - at least one masked/boundary tile example once represented;
  - PU identity and callsite metadata when available.
- Run `ir_b2a -st -src` and `whirl2c` across a process boundary.
- Retain artifacts under `artifacts/whirl2c-dsl/<test-name>/`:
  - `.B`;
  - `.T` from `ir_b2a -st -src`;
  - `.w2c.c`;
  - `.w2c.h`;
  - logs and diagnostics.
- Clean the artifact directory at the start of the next run, not at the end of
  the current run.

### Stage 6: Lowered C Path

- Only after DSL lowering produces C-equivalent WHIRL should `whirl2c` attempt
  ordinary compilable C for those lowered regions.
- Preserve the DSL projection mode for pre-lowering artifacts and diagnostics.
- If both high-level and lowered views are available, prefer an option to emit
  both rather than silently replacing DSL evidence.

## Code Touchpoints

Primary files:

- `osprey/be/whirl2c/ty2c.cxx`: tensor declarations.
- `osprey/be/whirl2c/wn2c.cxx`: logical DSL expression and tile projection.
- `osprey/be/whirl2c/st2c.cxx`: symbol declarations and symbol-use context.
- `osprey/be/whirl2c/w2c_driver.cxx`: output files, phase behavior, options if
  a projection mode flag becomes necessary.

Reference behavior:

- `osprey/common/com/ir_reader.cxx`: logical DSL names in `ir_b2a`.
- `osprey/common/com/dsl_ir_print.cxx`: TensorDescriptorIR and DSL image
  printing.
- `osprey/common/com/symtab.h` / `symtab.cxx`: tensor descriptor accessors.

## Validation Gates

For each implementation stage:

1. `git diff --check`.
2. No tab characters in touched files, except required make recipes.
3. Build `whirl2c` in a configured build tree.
4. Run existing non-DSL `whirl2c` smoke tests if available.
5. Run DSL artifact tests and retain evidence.
6. Compare `ir_b2a -st -src` and `whirl2c` output for logical operator names,
   tensor descriptors, and source correlation.

## Open Questions

1. Should short keywords such as `TENSOR` and `TILE` be emitted only in a DSL
   projection mode, with legacy `whirl2c` preserving older spelling for
   canonical WHIRL?
2. Should `TILE_PTR` be represented explicitly in WHIRL, or only inferred by
   `whirl2c` from tile views and load/store nodes?
3. Should tile metadata live in TensorDescriptorIR, a separate TileDescriptorIR,
   DSL node attributes, or a staged combination?
4. How much of Triton-C's tunable syntax should appear before autotuning
   metadata exists natively?
5. Should `whirl2c` print a warning banner when output contains DSL extension
   syntax and is not expected to compile?

## Near-Term Checklist

- [x] Document the `whirl2c` DSL projection goal.
- [x] Add initial `KIND_TENSOR` handling in `ty2c.cxx`.
- [ ] Replace `OPEN64_DSL_TENSOR` spelling with the agreed compact `TENSOR`
      spelling in the tensor type printer.
- [ ] Add `WN2C_dsl()` using logical DSL APIs.
- [ ] Add first tile projection helper and syntax.
- [ ] Add PU identity and callsite metadata comments.
- [ ] Add retained artifact tests for `ir_b2a -st -src` plus `whirl2c`.
