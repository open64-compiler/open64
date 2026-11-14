# MLA cache tensor traits and High WHIRL preservation v1.0

November 8, 2026 | Implementation and focused native validation

This amendment resolves the ingestion and propagation gaps identified in the
[MR0 cache-trait audit](AI-COMPILER-MLA-MR0-CACHE-TRAIT-AUDIT-v0.1.md).
It implements the semantic-role portion of MR1 in the
[data movement implementation plan](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.9.md).
MR1's broader owned reuse/effect facts and MR0 hardware measurement remain
open. No grouped-head optimization, cache-aware cost-model change or GPU
performance claim is introduced. Primary owner, successor, reviewer and
approving lead remain TBD.

## Semantic contract

The existing TensorDescriptorIR `TensorTraitSet.traits` string carries these
versioned, single-role declarations:

| Trait | Declared root representation | Meaning |
| --- | --- | --- |
| `attention.latent_kv_cache.v1` | FP32 rank-3 `[batch,token,latent-column]` | Compressed latent content used by absorbed key and weighted-value contractions |
| `attention.rotary_key_cache.v1` | FP32 rank-3 `[batch,token,rotary-column]` | Rotary key content used by the positional score contraction |

The current pure tensor-expression lane supports one of these roles or an
absent role. Unsupported combinations, versions, types, unbound declarations
and declarations on non-input allocations are refused by lowering. A
storage-aliasing reshape inherits its root's role, including a rank-changing
view. Ordinary arithmetic results, materialized transpose results, outputs
and scratch allocations do not acquire the role. The frontend's transpose
is materialized by the current common High algorithm; it is not an aliasing
reshape in the detached storage model.

The role describes semantic content. It does not guarantee GPU residency,
SMEM staging, uniqueness, valid length beyond the admitted static shape,
capacity, a ready event, alias exclusion, or immutability between invocations.
Allocation identity, source/view/effect versions and ownership stay in value
and analysis facts. Two distinct input allocations can have equal canonical
tensor types and the same role without becoming the same storage.

## Explicit frontend declaration

The single-PU pure tensor-expression ingestion lane accepts a declarative
model attribute, keyed by actual source argument names:

```python
from open64_dsc.tensor_traits import LATENT_KV_CACHE, ROTARY_KEY_CACHE

class AbsorbedMLA(torch.nn.Module):
    __open64_tensor_traits__ = {
        "cache": LATENT_KV_CACHE,
        "rope_cache": ROTARY_KEY_CACHE,
    }
    # forward(..., cache, rope_cache, ...) contains the normal tensor algorithm.
```

No public export function signature changes. The retained MLA fixture uses
this declaration. An adapter can supply it when the source model cannot be
edited. An empty mapping preserves the existing unannotated path. Unknown
argument names and unsupported roles/ranks/dtypes are explicit errors before
tensor PU/type construction. Truthy declarations on unsupported ingestion
lanes are refused instead of silently discarded. There is no classification
based on the spelling `cache`, sample values, or GPU execution.

## Preservation and admission

1. Ingestion binds the role in the original tensor TY descriptor, including
   storage-aliasing reshape results. Native `.B` and `ir_b2a -st -src` retain
   the ordinary existing tensor-extension representation.
2. `VHO_TENSOR_CAPTURE` admits only the published roles, copies them into
   `VHO_TENSOR_VALUE.cache_trait`, and keeps rejecting other unsupported
   descriptor semantics. Whole-program validation requires an FP32 rank-3
   input root, matching alias roles and the existing pure read-only input ABI.
3. A nonempty role set is persisted as an independently versioned optional
   extension on existing tensor-kernel recipes and High algorithm bindings:

   ```json
   "tensor_cache_traits": {
     "schema": "open64.tensor-cache-traits.v1",
     "values": [
       {"value": 2, "trait": "attention.latent_kv_cache.v1"},
       {"value": 3, "trait": "attention.rotary_key_cache.v1"}
     ]
   }
   ```

   The example shows roots only; actual documents also list tagged reshape
   aliases in increasing value order. The six-field value records and physical
   recipe versions v1-v8 are unchanged. Old unannotated documents and semantic
   keys remain identical. Older strict readers refuse the new extension rather
   than ignore it. New readers reject duplicates, invalid IDs/roles, unknown
   extension versions and inconsistent alias annotations. The recipe checksum
   and complete semantic key seal the extension, so removal is not a valid
   handoff of an annotated artifact.
4. Common High lowering keeps ordinary scalar pointer formals and explicit
   DO_LOOP/ARRAY indexing. A formal's `tensor.cache.trait` binding projects the
   declared role from the sealed source recipe into that scalar ABI. It is not
   a new tensor TY flag, pointer-type identity, or independent source of truth.
   LNO validates it against the full producer recipe before using the input.
5. Owned High source keys include nonempty formal role bindings. An unannounced
   role edit changes recaptured content and fails revalidation even without
   an epoch bump. Existing owner/activation/mutation-epoch checks remain in force.
   The read-only observer copies the extension and performs no native writes.
6. The closed absorbed-MLA graph recognizer requires any declared input roles
   to agree with the actual latent/rotary operands. It rejects contradictory
   declarations instead of silently reclassifying them. Selected-plan seals
   bind formal roles, and device argument bindings retain `cache_trait` through
   the selected High/Mid/Low bridge. Admission verifies those argument roles;
   existing bridge keys also bind them. PTX uses the resulting physical accesses.

No mapped-image/ELF layout, opcode, TY kind, reader/writer format or numerical
permission changed. The WHIRL specification's High-level DO_LOOP/ARRAY and
continuous-lowering semantics remain intact. The implementation adds no new
`be.so` exported/undefined symbols or library dependencies. Incremental build
dependencies cover all consumers of the extended detached C++ value record.
COMMENT explanations and the existing PREOPT workaround remain unchanged.

## When ingestion supplies no trait

Prefer an explicit declaration from the model's cache abstraction or an import
adapter that knows its semantic interface. Do not invent persistent cache
state from a buffer name, rank or observed address.

When annotations are absent, the existing absorbed-attention recognizer can
still derive invocation-local operand roles from the complete admitted graph,
shapes and dataflow. That is enough to retain the existing G=1 implementation
and its fused score/softmax/latent algorithm. It does not synthesize a declared
persistent-cache type, cross-call lifetime or cache-update contract.

Future MR1 reuse proofs may also derive head-invariant access groups from
affine indices, shared symbolic storage/view identities, read/write effects,
alias information and a common version/lifetime. Record that evidence as
derived analysis, separate from a source declaration. Invalidate it when those
facts change. If the graph, identity or effects are not proved, retain ordinary
lowering/G=1 and do not enable a cache-dependent grouped schedule. Inference
requires no schedule search, WN/ST/TY mutation or profiling inside estimation.
A source cache append ordered before the read invocation remains a runtime
data update, not an automatic reason to JIT compile.

## Implementation and validation plan

| Step | Result / acceptance |
| --- | --- |
| CT1: publish roles and explicit frontend capture | Implemented; root/reshape descriptors, invalid declarations, equal types with distinct values, and missing annotations covered |
| CT2: preserve detached recipes and scalar High bindings | Implemented; original `.B`, before/after `.B`/`.T`, raw lowering diffs, and trait-bearing manifests retained |
| CT3: bind observer/application identities | Implemented; pure observation, contradictory declarations, stripping/corruption refusals, and live trait-edit invalidation covered |
| CT4: verify compatibility and selected route | Implemented; short/long retained oracle cases, legacy recipe/binding checks, explicit search-off fallback, and selected High/Mid/Low/PTX route covered |
| MR1 remainder | Open: complete owned cache/access/version/lifetime facts and later grouped-head materialization proofs |
| MR0 measurements / MR4 GPU acceptance | Open: no Hopper access; no GPU execution, timing or counters claimed |

The focused native review is
`osprey/be/lno/tests/run_cache_trait_review.py`; it requires an explicit tools
root, the immutable M6 baseline receipt and a fresh output directory. It checks
seven retained oracle cases per context using real whirl2c High output compiled
as host code, both before and after selected application (28 executions), and
observes read-only search and exercises the selected bridge.
It checks ordinary unannotated input compatibility and existing refusal behavior.
The separate test-only snapshot component mutates each existing formal trait
record, proves refusal, restores it and verifies subsequent live facts.

Validation is separate from candidate estimation. Existing CUDA/WHIRL comments
are preserved, and no new GPU optimization is implemented by the source printer.
GPU correctness and performance remain unmeasured. The broad Python suite has
one pre-existing FHE v0.10 document-digest failure; its document bytes match
HEAD and neither that document nor its test was changed here.
