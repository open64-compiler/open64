# WHIRL DSL Shape Propagation SP10 Certification

## Status

SP10 is complete. The Shape Inference Trigger Contract is documented,
implemented with runtime-only structured trigger identities, and certified
against the existing per-PU shape pipeline.

SP10 does not add a WHIRL table, ELF section, persisted generation number,
operator, tensor-type field, or frontend API. It does not introduce cross-PU
shape propagation. Future IPA, inlining, cloning, specialization, and outlining
owners must adopt the trigger contract when those transformations are added.

## Implemented Contract

`VHO_DSL_SHAPE_TRIGGER` replaces free-form invalidation strings. Its stable
diagnostic names cover:

- initial PU admission and PU identity changes;
- seed refinement;
- operator-constraint and value-relationship changes;
- structural transformation and PU/REGION restructuring;
- symbolic resolution;
- DSL WOPT/Preopt;
- FHE conversion; and
- fixed-order VHO DSL optimization.

Invalid, zero, and out-of-range invalidation triggers fail without advancing
the PU generation. A valid trigger advances the runtime-only generation and is
printed in `DSL-SHAPE-INVALIDATE` and `DSL-SHAPE-STALE` diagnostics. Successful
per-PU refinement marks that generation current.

The VHO DSL stage contract now distinguishes:

- shape preserving;
- monotonic refining;
- locally shape invalidating; and
- call/return/REGION-boundary invalidating.

Every current fixed-order stage remains conservatively locally invalidating.
Unknown shape-effect values are also treated as invalidating, so a new or
unreviewed stage cannot silently retain stale shape state.

## Current Trigger Audit

| Owner | Trigger | Enforcement |
| --- | --- | --- |
| Backend PU driver | Initial PU admission | Mandatory refinement for every selected PU at `-O0` and above, including `whirl2c`-only processing |
| Backend PU driver | `dsl_wopt` | Invalidate before DSL WOPT/Preopt and refine afterward |
| Backend PU driver | `fhe_conversion` | Invalidate after a successful FHE conversion pass and refine before checkpoint publication or lowering |
| VHO DSL lowering driver | `vho_dsl_optimization` | Invalidate before enabled fixed-order stages, refine after executed invalidating stages, and require current state before lowering |
| Shape consumer | Current-generation check | Reject stale state before native DSL lowering |

Frontend builder finalization continues to perform admission checking over seed
facts. Authoritative mutable refinement and canonical `TY_IDX` rebinding remain
owned by the compiler-side per-PU VHO pass.

## Validation

The following checks pass:

- all structured trigger names and out-of-range lookup behavior;
- invalid trigger rejection without generation mutation;
- recognized-trigger invalidation, stale-state detection, and revalidation;
- default-invalidating behavior for unknown VHO shape effects;
- current driver and VHO call-site audit;
- SP9 broadcasting and symbolic batched-matmul regression matrix;
- rebuilt `be.so`, `be`, forced `lw_inline`, focused VHO tests, and `ir_b2a`;
- zero `DSL_Builder_*` and `Json::` symbols in `be.so` and `lw_inline`;
- x86-64, MIPS, MIPS-SL, KEY-generic, Loongson, and baseline syntax/operator
  layout lanes; and
- normal six-PU ResNet `-O0` processing with the rebuilt backend, retained
  intermediate WHIRL, x86-64 assembly, and separate-process
  `ir_b2a -st -src` inspection.

The forced `lw_inline` build emits its pre-existing missing return-type warning
for `ipa/inline/inline_driver.cxx`; SP10 introduces no new warning in the files
it changes.

## Retained Evidence

Review artifacts are retained outside Git at:

```text
/private/tmp/open64-shape-sp5/artifacts/shape/sp10-trigger-contract/
```

Important files include:

```text
shape_refine.B
shape_refine.T
shape_refine.before.B
shape_refine.before.T
shape_refine.after.B
shape_refine.after.T
shape_refine.before-after.diff
validation.log
certification.txt
sp9-regression/shape_sp9.B
sp9-regression/shape_sp9.T
target-matrix.log
shape-trigger-symbol.log
pipeline-resnet/resnet.B
pipeline-resnet/resnet.T
pipeline-resnet/resnet.I
pipeline-resnet/resnet.s
pipeline-resnet/resnet.shape-after.t
pipeline-resnet/resnet.safetensors
pipeline-resnet/resnet_model.py
```

The focused before/after traces are independently produced binary WHIRL images
reopened with `ir_b2a -st -src`. Their unified diff shows `shape_add` and
`shape_relu` changing from the shared `[2,<pending>]` `TY_IDX` to one newly
interned canonical `[2,3]` `TY_IDX`. The unrelated pending symbol remains on the
old immutable type. The ResNet trace shows the source model's logical
CNN/common operators and complete tensor descriptors.
