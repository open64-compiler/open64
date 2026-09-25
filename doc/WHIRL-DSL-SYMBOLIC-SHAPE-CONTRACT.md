# WHIRL DSL Symbolic Shape Contract

## Status

SP8 implementation contract for symbolic and runtime-dynamic tensor dimensions.
This contract is additive to `WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md` and
`WHIRL-DSL-TENSOR-TYPE-HANDLING.md`.

## Scope

SP8 extends the existing compiler-owned, per-PU shape service. It does not add
an ELF section, change a mapped-image record, perform cross-PU inference, or
make the Python frontend responsible for graph-wide shape reasoning.

The existing canonical TensorDescriptorIR `logical_shape` field carries a
deterministic textual form. That field is already part of tensor type identity
and the mapped WHIRL image. The compiler parses it into runtime shape facts;
the text is not an untyped metadata convention.

## Dimension Forms

| Form | Canonical example | Meaning |
| --- | --- | --- |
| Static | `128` | Positive compile-time extent |
| Anonymous dynamic | `?` | Complete runtime extent without reusable identity |
| Named symbol | `L@pu00003201` | Complete extent scoped to one owning PU |
| Expression | `L@pu00003201+1` | Canonical symbol plus or minus a constant |
| Pending | `<pending>` | Incomplete extent requiring refinement |

Static dimensions remain positive. A symbolic extent may represent a runtime
value such as a zero-length cache prefix when the operator contract permits
it. SP8 expressions are intentionally limited to one named symbol followed by
an optional `+N` or `-N`. Products, division, min/max, and general algebra are
deferred until a published operator contract requires them.

## Symbol Identity

The builder accepts an unqualified identifier such as `L` only while a PU is
active. It normalizes the identifier to:

```text
<identifier>@pu<eight-lowercase-hex-digits>
```

The hexadecimal payload is the owning global function `ST_IDX`. Consequently,
`L` in two unrelated PUs does not imply equality. A fully qualified symbol can
be carried through an explicitly reviewed PU interface or call contract.
Unqualified symbols outside an active PU are rejected.

The normalizer removes insignificant whitespace and leading zeroes from
constants. Canonical text participates in immutable tensor-type uniquing.
Refinement creates or reuses a new canonical `TY_IDX`; it never edits a sealed
type in place.

## Proof Rules

Named dimensions prove equality only when kind and canonical text match.
Static dimensions prove equality by value. Anonymous dynamic dimensions do
not prove equality between independent operands, even if both print as `?`.
They may flow through a reviewed unary operator whose result preserves the
same operand shape.

Pending dimensions are admission facts, not strict lowering facts. A strict
gate rejects any required pending or unresolved extent before DSL lowering.

SP8 reconstructs constraints from versioned logical operators, attributes,
and tensor descriptors. It does not persist a second constraint table.

## Llama Decode Slice

The first symbolic contract covers `transformer.attention.v2` with BHSD
operands:

```text
query         [B,Hq,1,D]
updated_key   [B,Hkv,L+1,D]
updated_value [B,Hkv,L+1,D]
result        [B,Hq,1,D]
```

The checker proves batch equality, exact key/value shape equality, static
head-count and head-width attributes, single-token query extent, and exact
result/query shape equality. The reviewed decode attributes and distinct
key/value state effects remain mandatory.

This slice does not infer `L+1` from a cache append operation. The frontend may
seed the relationship, and the compiler validates it. Backward inference and
cache-update expression construction remain future work.

## Runtime Guards

No runtime guard is fabricated in SP8. When a versioned operator requires an
equality or bound that static or named-symbol facts cannot prove, verification
fails closed. The registered `common.shape_assert` and
`common.runtime_shape_guard` descriptors remain reserved for a later reviewed
executable contract with explicit placement, effect, lowering, and diagnostic
semantics.

## Inspection And Compatibility

`ir_b2a -st -src` prints the canonical `logical_shape`, preserving visible
distinctions among static, `?`, scoped symbols, expressions, and `<pending>`.
The existing tensor type and DSL image sections carry the data without a
binary layout or WHIRL revision change.

Older readers continue to map the same records and print the shape string.
They may fail closed semantically because they do not understand symbolic
legality. Previously stored unqualified symbolic strings were diagnostic-only
and remain invalid as stable compiler symbols.

## Ownership

`dsl_shape.{h,cxx}` owns parsing, normalization, runtime facts, proof, and
operator checking. `dsl_builder.cxx` supplies the active PU identity when
normalizing frontend seed descriptors. `VHO_DSL_Shape_Refine_Driver()` formats
refined facts and reuses the existing immutable tensor-type interner.

The backend driver invokes this service for its currently selected PU. REGION
checking follows that same PU scope. Cross-PU propagation belongs to a future
explicit `-ipa` pass.

## Deferred Work

- symbolic products and quotient expressions for reshape and flatten;
- convolution, pooling, slice, and concat expression algebra;
- reviewed runtime shape assertion/guard nodes and lowering;
- backward propagation required by a published operator contract;
- IPA summaries and context-sensitive cross-PU specialization;
- frontend migration from sample cache lengths to symbolic decode seeds.
