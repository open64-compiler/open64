# Open64 FHE SYNC-3 Native Planning-Image Contract

Status: Stage 3 VHO phase and all-PU conversion-checkpoint substrate
implemented, pending review

Semantic authority:

- `../open64-plans/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.9.docx`
  is the highest semantic authority; the reviewed copy has SHA-256
  `4B9DAC9927E86518142CA9A9E71AEAE7AEA5C454D01C544311359680639DF4B6`.
- `doc/FHE-SYNC3-CONVERSION-CONTRACT.md`
- `doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md`
- `doc/FHE-WHIRL-INTEGRATION-PLAN.md`
- `doc/FHE-SYNC1-NATIVE-CONTRACT.md`

This contract is subordinate to v0.9 and implements only the narrowed C3 /
SYNC-3 planning checkpoint permitted by Appendix F.1. It does not establish
completion of v0.9 Architecture Phase 3 or focused milestone M4. C4 / SYNC-4
bootstrap-plus-polynomial materialization and the remaining v0.9 execution
evidence stay mandatory.

## Prerequisite Gate

SYNC-3 source coding remains blocked until corrective SYNC-1 validation fixes
version-1 tensor-binding identity and FHE entry-value atomicity, PU ownership
and exact source provenance are corrected, and SYNC-2 is recertified with
fail-closed checks plus retained evidence. Main/common and FHE reviewers must
also accept an exact node-retirement contract for physical BatchNorm removal,
including users, provenance, rollback, old-reader behavior, mapped reopen, and
tree/image consistency. This document does not invent that representation.

## Decision

SYNC-3 adds one optional `.WHIRL.dsl_fhe_plan` section with `sh_info` value
`WT_DSL_FHE_PLAN` (`0x24`). It does not change the exact-sized
version-1 `.WHIRL.dsl_fhe` image, WN, TY, ST, existing DSL images, or the
`WHIRL::0.34` revision string.

The planning image carries only FHE conversion facts that have no existing
authoritative home:

1. accepted source-node disposition;
2. reusable approximation contracts;
3. value-specific CKKS state; and
4. context-specific BatchNorm-fold provenance.

Source positions remain in WN/ST/DST/value records. TensorDescriptorIR identity
remains the canonical `TY_IDX`. EncryptionDescriptorIR and configuration remain
in `.WHIRL.dsl_fhe`. Call context remains in `.WHIRL.dsl_calls`. Tensor payloads
remain TCON-backed, including side-file carriers. The planning image references
those records and does not duplicate them.

## Domain-Wrapper Clarification

The existing `DSL_Opcode_Register_Domain_Wrapper()` registry preserves the
relationship between a domain-visible name and an existing semantic target,
but it does not currently create a native result-producing WN for an operator
that lacks a `DSL_OPERATOR` enum value. SYNC-3 therefore records an accepted
`fhe.cnn.*` wrapper name and version in the disposition row while retaining the
source-semantic DSL node in the WHIRL tree.

DSL-aware inspection and FHE lowering present the logical FHE disposition and
must not expose the physical `OPR_DSL` escape tag. The wrapper name is a
first-class planning contract, not a new executable WN opcode in SYNC-3. A
future operation with genuinely new expression semantics, including
`fhe.cnn.poly_activation`, still requires the normal reviewed append-only
`DSL_OPERATOR` allocation before it is emitted as a native DSL WN.

On image load, a nonempty wrapper name must resolve through the domain-wrapper
registry, and its wrapper target must match the source node's logical operator.

`fhe.cnn.linear` has two append-only wrapper identities. The original
`fhe.cnn.linear.v1` continues to target the two-kid rank-generic
`common.linear.v3` contract. `fhe.cnn.linear.v2` targets the three-kid
`common.linear.v2` contract used by the certified ResNet capture. Conversion
selects the wrapper version from the source logical operator version; the two
wrapper versions may coexist in one program and neither reinterprets the
other's persisted planning-image rows.
The mapped image stores the stable name and version, never the runtime-assigned
`DSL_OPCODE_ID`.

## Version 1 Image

The section and every row begin at 8-byte alignment. All records contain only
fixed-width scalars, existing table IDs, Open64 indices, and `STR_IDX`. There
are no pointers, STL objects, runtime handles, payload bytes, or backend
objects.

```text
magic:        0x46485031  ("FHP1")
version:      1
header size:  64 bytes
capabilities: DISPOSITION | APPROXIMATION | CKKS_STATE | BN_FOLD
```

| Record | Size | Count field |
| --- | ---: | --- |
| `DSL_FHE_PLAN_IMAGE_HEADER` | 64 | one |
| `DSL_FHE_CONVERSION_DISPOSITION_RECORD` | 56 | `disposition_count` |
| `DSL_FHE_APPROXIMATION_CONTRACT_RECORD` | 64 | `approximation_count` |
| `DSL_FHE_CKKS_VALUE_STATE_RECORD` | 64 | `ckks_value_state_count` |
| `DSL_FHE_BN_FOLD_PROVENANCE_RECORD` | 64 | `bn_fold_count` |

All four record IDs are one-based `UINT32`. Their named `INVALID_ID` value is
zero. `ST_IDX`, `TY_IDX`, `TCON_IDX`, DSL image IDs, PU identity IDs, and
callsite IDs retain their existing 32-bit carriers; `STR_IDX` remains 64-bit.
Compile-time width and record-size assertions are required on every supported
host target.

### Image Header: 64 Bytes

```c++
typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 header_size;
    UINT32 record_kind_count;
    UINT32 capabilities;
    UINT32 flags;
    UINT32 disposition_count;
    UINT32 approximation_count;
    UINT32 ckks_value_state_count;
    UINT32 bn_fold_count;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
    UINT32 reserved3;
    UINT32 reserved4;
    UINT32 reserved5;
} DSL_FHE_PLAN_IMAGE_HEADER;
```

Version 1 requires exactly four record kinds and all four capability bits.
Flags and reserved fields are zero.

```c++
typedef enum {
    DSL_FHE_PLAN_RECORD_UNKNOWN = 0,
    DSL_FHE_PLAN_RECORD_DISPOSITION = 1,
    DSL_FHE_PLAN_RECORD_APPROXIMATION = 2,
    DSL_FHE_PLAN_RECORD_CKKS_VALUE_STATE = 3,
    DSL_FHE_PLAN_RECORD_BN_FOLD = 4
} DSL_FHE_PLAN_RECORD_KIND;

typedef enum {
    DSL_FHE_PLAN_CAP_DISPOSITION = 0x00000001,
    DSL_FHE_PLAN_CAP_APPROXIMATION = 0x00000002,
    DSL_FHE_PLAN_CAP_CKKS_VALUE_STATE = 0x00000004,
    DSL_FHE_PLAN_CAP_BN_FOLD = 0x00000008
} DSL_FHE_PLAN_CAPABILITY;
```

### Conversion Disposition: 56 Bytes

```c++
typedef struct {
    DSL_FHE_CONVERSION_DISPOSITION_ID id;
    DSL_IR_NODE_ID source_node_id;
    DSL_IR_VALUE_ID result_value_id;
    UINT32 disposition;
    ST_IDX owner_pu_st;
    UINT32 wrapper_version;
    STR_IDX wrapper_name;
    DSL_FHE_APPROXIMATION_CONTRACT_ID approximation_contract_id;
    DSL_FHE_CKKS_VALUE_STATE_ID result_ckks_value_state_id;
    DSL_FHE_BN_FOLD_PROVENANCE_ID first_bn_fold_id;
    UINT32 bn_fold_count;
    UINT32 flags;
    UINT32 reserved;
} DSL_FHE_CONVERSION_DISPOSITION_RECORD;
```

Disposition values and flags are append-only:

```c++
typedef enum {
    DSL_FHE_DISPOSITION_UNKNOWN = 0,
    DSL_FHE_DISPOSITION_PRESERVE = 1,
    DSL_FHE_DISPOSITION_DOMAIN_WRAPPER = 2,
    DSL_FHE_DISPOSITION_FOLD_INTO_PRODUCER = 3,
    DSL_FHE_DISPOSITION_LAYOUT_REINTERPRET = 4,
    DSL_FHE_DISPOSITION_REQUIRE_APPROXIMATION = 5
} DSL_FHE_CONVERSION_DISPOSITION;

typedef enum {
    DSL_FHE_DISPOSITION_FLAG_NONE = 0,
    DSL_FHE_DISPOSITION_DEFINITION_REWRITE = 0x00000001,
    DSL_FHE_DISPOSITION_CONTEXT_SENSITIVE = 0x00000002,
    DSL_FHE_DISPOSITION_NO_DATA_MOVEMENT = 0x00000004,
    DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED = 0x00000008
} DSL_FHE_CONVERSION_DISPOSITION_FLAG;
```

| Value | Meaning |
| --- | --- |
| `PRESERVE` | Keep the source-semantic operation with FHE state evidence. |
| `DOMAIN_WRAPPER` | Lower according to the named FHE domain wrapper. |
| `FOLD_INTO_PRODUCER` | The operation is absorbed by a legal producer rewrite. |
| `LAYOUT_REINTERPRET` | Preserve semantics without encrypted data movement. |
| `REQUIRE_APPROXIMATION` | Preserve source activation and bind an approximation contract. |

Rejected conversion does not produce a valid planning image. It is reported
through stable diagnostics and the failed-run report instead of a `REJECTED`
row. There is exactly one disposition per source node. The source node's
result must equal `result_value_id`.

`wrapper_name` and `wrapper_version` are present only for `DOMAIN_WRAPPER`.
`approximation_contract_id` is present only for `REQUIRE_APPROXIMATION`.
`first_bn_fold_id` and `bn_fold_count` are either both zero or name one
contiguous group belonging to the source convolution definition.

### Approximation Contract: 64 Bytes

```c++
typedef struct {
    DSL_FHE_APPROXIMATION_CONTRACT_ID id;
    DSL_FHE_CONFIG_ID config_id;
    STR_IDX polynomial_name;
    UINT32 approximation_family;
    UINT32 polynomial_version;
    UINT32 degree;
    TCON_IDX coefficient_tensor_tcon;
    TCON_IDX valid_range_min_tcon;
    TCON_IDX valid_range_max_tcon;
    TCON_IDX max_abs_error_tcon;
    UINT32 scale_policy;
    UINT32 required_multiplicative_depth;
    UINT32 bootstrap_policy;
    UINT32 requires_pre_refresh;
    UINT32 flags;
} DSL_FHE_APPROXIMATION_CONTRACT_RECORD;
```

Approximation families initially include `MINIMAX`, `CHEBYSHEV`, and `TAYLOR`.
Policy value zero means unknown and is rejected; an explicit `INHERIT` value is
used when a field is inherited from the referenced FHE configuration.

```c++
typedef enum {
    DSL_FHE_APPROXIMATION_UNKNOWN = 0,
    DSL_FHE_APPROXIMATION_MINIMAX = 1,
    DSL_FHE_APPROXIMATION_CHEBYSHEV = 2,
    DSL_FHE_APPROXIMATION_TAYLOR = 3
} DSL_FHE_APPROXIMATION_FAMILY;

typedef enum {
    DSL_FHE_APPROX_SCALE_UNKNOWN = 0,
    DSL_FHE_APPROX_SCALE_INHERIT = 1,
    DSL_FHE_APPROX_SCALE_PRESERVE = 2,
    DSL_FHE_APPROX_SCALE_EXPLICIT = 3
} DSL_FHE_APPROXIMATION_SCALE_POLICY;
```

Version 1 approximation flags must be zero. Later flag bits are append-only.

The semantic interning key is every field except `id`. Source operator,
source position, owning PU, and call context are not part of that key. They are
reached through each disposition's source node. This permits all compatible
ReLU contexts to share one approximation contract without losing provenance.

The coefficient TCON is a rank-1 dense or side-file tensor constant with
`degree + 1` elements. Range and error TCONs are finite scalar constants;
minimum is less than maximum and maximum absolute error is nonnegative.

### CKKS Value State: 64 Bytes

```c++
typedef struct {
    DSL_FHE_CKKS_VALUE_STATE_ID id;
    DSL_IR_VALUE_ID value_id;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption_descriptor_id;
    UINT32 state_version;
    UINT32 scheme;
    UINT32 value_class;
    INT32 level;
    INT32 scale_bits;
    INT32 component_count;
    INT32 precision_bits;
    UINT32 slot_count;
    UINT32 alignment_group;
    STR_IDX encrypted_layout_name;
    UINT32 pending_actions;
    UINT32 pending_bootstrap_reason;
} DSL_FHE_CKKS_VALUE_STATE_RECORD;
```

The interning identity is `(value_id, state_version)` and duplicates are
rejected. Versions for one value start at one and remain contiguous. Version
one is the SYNC-3 converted state; later passes append a version rather than
mutating prior evidence.

`level`, `scale_bits`, `component_count`, and `precision_bits` use `-1` for
`PENDING`; values less than `-1` are invalid. `slot_count == 0` means pending
or inherited from the encryption descriptor/configuration. Alignment group
zero means no residual-alignment group. An empty encrypted-layout name means
pending layout. Pending-action bits initially cover `RESCALE`, `RELINEARIZE`,
and `BOOTSTRAP`; unknown bits are rejected. Bootstrap reasons initially cover
`NONE`, `PRE_RELU_REFRESH`, `DEPTH_EXHAUSTION`, and
`MANUAL_BOUNDARY_REQUIRED`.

```c++
typedef enum {
    DSL_FHE_CKKS_PENDING_NONE = 0,
    DSL_FHE_CKKS_PENDING_RESCALE = 0x00000001,
    DSL_FHE_CKKS_PENDING_RELINEARIZE = 0x00000002,
    DSL_FHE_CKKS_PENDING_BOOTSTRAP = 0x00000004
} DSL_FHE_CKKS_PENDING_ACTION;

typedef enum {
    DSL_FHE_BOOTSTRAP_REASON_NONE = 0,
    DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH = 1,
    DSL_FHE_BOOTSTRAP_REASON_DEPTH_EXHAUSTION = 2,
    DSL_FHE_BOOTSTRAP_REASON_MANUAL_BOUNDARY_REQUIRED = 3
} DSL_FHE_BOOTSTRAP_REASON;
```

A set pending-action bit means the action is required but not yet materialized.
A later state version records satisfaction by clearing the bit. Rejection
produces a diagnostic and no valid output image; it is not encoded as value
state. This avoids maintaining a second status enum for each action.

CKKS state is never part of canonical TY or EncryptionDescriptorIR interning.
The referenced encryption descriptor must use CKKS and its value class must
match this row. Existing key-requirement rows remain authoritative; this row
does not duplicate key IDs.

### BatchNorm Fold Provenance: 64 Bytes

```c++
typedef struct {
    DSL_FHE_BN_FOLD_PROVENANCE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_NODE_ID conv_node_id;
    DSL_IR_NODE_ID batch_norm_node_id;
    DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id;
    DSL_CALLSITE_METADATA_ID context_callsite_id;
    DSL_IR_VALUE_ID source_conv_weight_value_id;
    DSL_IR_VALUE_ID source_conv_bias_value_id;
    DSL_IR_VALUE_ID source_bn_scale_value_id;
    DSL_IR_VALUE_ID source_bn_bias_value_id;
    DSL_IR_VALUE_ID source_bn_mean_value_id;
    DSL_IR_VALUE_ID source_bn_variance_value_id;
    TCON_IDX folded_weight_tcon;
    TCON_IDX folded_bias_tcon;
    UINT32 flags;
    UINT32 reserved;
} DSL_FHE_BN_FOLD_PROVENANCE_RECORD;
```

```c++
typedef enum {
    DSL_FHE_BN_FOLD_FLAG_NONE = 0,
    DSL_FHE_BN_FOLD_IMPLICIT_ZERO_BIAS = 0x00000001,
    DSL_FHE_BN_FOLD_SHARED_PU_DEFINITION = 0x00000002
} DSL_FHE_BN_FOLD_FLAG;
```

There is one row per `(conv_node_id, batch_norm_node_id, source context)`.
`context_pu_identity_id` is required. `context_callsite_id` is zero for a root
PU context; otherwise it must identify a call whose callee is `owner_pu_st`.
The optional source convolution bias value is zero only when the source
convolution had legal implicit-zero bias. All other source value IDs and both
folded TCONs are required.

Rows for one physical convolution definition are contiguous and are referenced
by that convolution's disposition. This records the distinction between one
physical clone-body rewrite and several context-specific folded payloads.

`owner_pu_st` identifies the rewritten signature-specialized clone. The first
version preserves a shared clone only when all represented contexts retain the
same rewritten structural and tensor signature. If context-specific folding
would require divergent signatures, SYNC-3 v1 rejects the conversion with a
stable diagnostic. Automatic clone splitting is deferred until a separate
clone/value/callsite provenance contract is reviewed.

## Native Image API

`dsl_fhe_plan.h` owns records, enums, initialization, lookup, validation,
mapped-image loading, and logical printing. The low-level API follows the
existing `dsl_fhe.h` service:

```c++
void DSL_FHE_Plan_Image_Reset(void);
void DSL_FHE_Plan_Image_Get_Header(DSL_FHE_PLAN_IMAGE_HEADER *header);
BOOL DSL_FHE_Plan_Image_Has_Records(void);
BOOL DSL_FHE_Plan_Image_Validate(FILE *diagnostic);
BOOL DSL_FHE_Plan_Image_Load_Mapped(const void *section_base,
                                    UINT64 section_size,
                                    FILE *diagnostic);
void DSL_FHE_Plan_Image_Print(FILE *file);

DSL_FHE_CONVERSION_DISPOSITION_ID
DSL_FHE_Plan_Add_Conversion_Disposition(
    const DSL_FHE_CONVERSION_DISPOSITION_RECORD *record);

DSL_FHE_APPROXIMATION_CONTRACT_ID
DSL_FHE_Plan_Intern_Approximation_Contract(
    const DSL_FHE_APPROXIMATION_CONTRACT_RECORD *record);

DSL_FHE_CKKS_VALUE_STATE_ID
DSL_FHE_Plan_Add_CKKS_Value_State(
    const DSL_FHE_CKKS_VALUE_STATE_RECORD *record);

DSL_FHE_BN_FOLD_PROVENANCE_ID
DSL_FHE_Plan_Add_BN_Fold_Provenance(
    const DSL_FHE_BN_FOLD_PROVENANCE_RECORD *record);
```

Each record has a matching `Record_Init`, count, `Get`, and semantic `Find`
operation. CKKS lookup supports exact `(value_id, state_version)` and latest
state. BN-fold lookup supports physical definition plus context. Returned
string pointers remain valid until image reset.

## Producer Wrappers and Compiler-Phase API

`DSL_BUILDER_VALUE` is an opaque producer-runtime WN handle. Its private
builder registry is not reconstructed after a separate compiler process opens
the `.B` file. Builder wrappers are therefore conveniences for native producer
tests and future frontend attachment only; they are not the FHE VHO pass ABI:

```c++
DSL_FHE_CONVERSION_DISPOSITION_ID
DSL_Builder_Record_FHE_Conversion_Disposition(
    DSL_BUILDER_VALUE source_value,
    const DSL_FHE_CONVERSION_DISPOSITION_INFO *info);

DSL_FHE_CKKS_VALUE_STATE_ID
DSL_Builder_Bind_FHE_Value_CKKS_State(
    DSL_BUILDER_VALUE value,
    const DSL_FHE_CKKS_VALUE_STATE_INFO *info);

DSL_FHE_BN_FOLD_PROVENANCE_ID
DSL_Builder_Record_FHE_BN_Fold(
    DSL_BUILDER_VALUE conv_value,
    DSL_BUILDER_VALUE batch_norm_value,
    const DSL_FHE_BN_FOLD_INFO *info);
```

The producer-runtime input structures are pointer-bearing API objects only;
they are resolved immediately and are never copied into the mapped image:

```c++
typedef struct {
    UINT32 disposition;
    UINT32 wrapper_version;
    const char *wrapper_name;
    DSL_FHE_APPROXIMATION_CONTRACT_ID approximation_contract_id;
    DSL_FHE_CKKS_VALUE_STATE_ID result_ckks_value_state_id;
    DSL_FHE_BN_FOLD_PROVENANCE_ID first_bn_fold_id;
    UINT32 bn_fold_count;
    UINT32 flags;
} DSL_FHE_CONVERSION_DISPOSITION_INFO;

typedef struct {
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption_descriptor_id;
    UINT32 state_version;
    UINT32 scheme;
    UINT32 value_class;
    INT32 level;
    INT32 scale_bits;
    INT32 component_count;
    INT32 precision_bits;
    UINT32 slot_count;
    UINT32 alignment_group;
    const char *encrypted_layout_name;
    UINT32 pending_actions;
    UINT32 pending_bootstrap_reason;
} DSL_FHE_CKKS_VALUE_STATE_INFO;

typedef struct {
    DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id;
    DSL_CALLSITE_METADATA_ID context_callsite_id;
    DSL_BUILDER_VALUE source_conv_weight;
    DSL_BUILDER_VALUE source_conv_bias;
    DSL_BUILDER_VALUE source_bn_scale;
    DSL_BUILDER_VALUE source_bn_bias;
    DSL_BUILDER_VALUE source_bn_mean;
    DSL_BUILDER_VALUE source_bn_variance;
    TCON_IDX folded_weight_tcon;
    TCON_IDX folded_bias_tcon;
    UINT32 flags;
} DSL_FHE_BN_FOLD_INFO;
```

`source_conv_bias` may be null only with
`DSL_FHE_BN_FOLD_IMPLICIT_ZERO_BIAS`; the remaining source values must be
registered builder values owned by the same PU as the convolution and
BatchNorm definitions.

After mapped-image reopen, compiler phases use stable DSL image IDs. A common
accessor resolves a physical definition in its PU context without requiring a
builder registry or comparing ambiguous PU-local `ST_IDX` values:

```c++
BOOL DSL_IR_Image_Find_Definition_Value(
    ST_IDX owner_pu_st,
    const WN *definition,
    DSL_IR_VALUE_RECORD *value_record);

BOOL DSL_IR_Rewrite_Native_Value(
    ST_IDX owner_pu_st,
    WN *definition,
    DSL_IR_VALUE_ID value_id,
    const DSL_IR_NATIVE_VALUE_REWRITE_REQUEST *request);
```

The runtime-only rewrite request is not part of any mapped row:

```c++
typedef struct {
    DSL_OPERATOR expected_operator;
    UINT16 expected_version;
    UINT16 replacement_version;
    DSL_OPERATOR replacement_operator;
    const WN *const *operand_templates;
    const DSL_IR_VALUE_ID *operand_value_ids;
    UINT32 operand_count;
    const DSL_IR_ATTRIBUTE_RECORD *attributes;
    UINT32 attribute_count;
    STR_IDX payload;
    UINT32 result_value_kind;
} DSL_IR_NATIVE_VALUE_REWRITE_REQUEST;
```

Operand templates and arrays are borrowed for the call. The helper copies WN
templates into normal WN memory-pool storage only after all preparation checks
pass; ownership never crosses the API. The physical and logical operand arrays
must have the same count and describe the same values.

The lookup requires a DSL result definition, matching result ST/TY, matching
`owner_pu` compiler metadata, and a unique mapped value record. It returns no
match rather than guessing when local symbol indices collide across PUs.

FHE plan insertion APIs consume the resolved `DSL_IR_NODE_ID` and
`DSL_IR_VALUE_ID` directly. They do not accept a stale producer handle.

The rewrite helper performs the traditional prepare/apply/postprocess protocol
and updates the physical WN and logical image as one operation:

Preparation validates logical source operator/version, operands, attributes,
result TY/ST, source position, ownership, and source image record. Apply
updates the physical expression WN and its direct kids. Postprocessing updates
the existing DSL node, value references, attributes, result metadata, lineage,
and plan relationships while preserving node ID, result value ID, result ST,
result TY, and source position. Failure leaves both representations unchanged.

BatchNorm removal from a BLOCK and PU signature/call-actual rewriting remain
VHO transformation responsibilities, but they must call common helpers for
logical node/image updates. No pass may manually decode the physical
`OPR_DSL` escape representation.

## Reader, Writer, and Validation

The writer omits an empty image. For a nonempty image it validates first,
writes the header followed by the four tables in the order above, and sets
8-byte `sh_addralign`.

The reader treats an absent section as an empty plan. For a present section it
performs checked multiplication/addition to compute the exact expected byte
size before reading a row. It then copies validated records into managed
tables before the ELF mapping is released. It never adopts mapped pointers.

Validation rejects:

- bad magic, version, header size, record-kind count, or capabilities;
- nonzero header/record reserved fields or unknown flag bits;
- count arithmetic overflow, truncation, trailing bytes, or non-sequential IDs;
- invalid STR, DSL node/value, PU, callsite, FHE descriptor/config, or TCON IDs;
- inconsistent source node/result/owner relationships;
- duplicate disposition or CKKS `(value, version)` identity;
- noncontiguous disposition fold ranges or context mismatch;
- an unresolved wrapper name/version or wrong wrapper target;
- malformed polynomial coefficient/range/error constants; and
- CKKS state incompatible with its encryption descriptor.

Only accepted folds and dispositions appear in a valid image. Rejected and
pending-contract cases remain stable diagnostics and conversion-report rows;
they are not persisted as apparently executable conversion plans.

The ordinary DSL/Tensor/FHE structural gatekeepers run first. The FHE planning
validator then runs before any FHE conversion consumer or lowering pass.
Fixture-specific ResNet-20 counts remain certification assertions and are not
part of generic image validation.

## Inspection Contract

`ir_b2a -st -src` prints the following stable logical headings only when the
optional section exists:

```text
FHE Conversion Disposition Table:
FHE Approximation Contract Table:
FHE CKKS Value State Table:
FHE BatchNorm Fold Provenance Table:
```

Disposition output includes the source logical operator and the logical
`fhe.cnn.*` wrapper when present. Fold output shows physical definition IDs,
source context identity, original parameter value names, and folded TCON
side-file evidence. CKKS output prints `<pending>` for every `-1` or inherited
field. Source locations are resolved from existing records rather than copied
into these rows.

The printer never exposes physical `OPR_DSL`, mapped offsets, key material,
ciphertext bytes, or backend object state.

## Compatibility

- A non-FHE or pre-SYNC-3 writer emits no plan section.
- A new reader accepts old 0.33, non-FHE 0.34, and FHE-v1 0.34 artifacts with
  an empty plan.
- Existing readers ignore the unknown optional `SHT_MIPS_WHIRL` section and
  continue reading the unchanged `.WHIRL.dsl_fhe` version-1 image.
- The new reader rejects a malformed plan section before pass use.
- Public enums, row fields, printer names, and diagnostic identities are
  append-only after release. A row-layout change requires a new plan-image
  version and a documented migration path.

## Implementation Stages

1. Add `dsl_fhe_plan.{h,cxx}` with record sizes, initialization, managed
   tables, interning/add/get/find/reset, and structural validation tests.
2. Add `WT_DSL_FHE_PLAN`, reader/writer/reset wiring, mapped-image malformed
   tests, and old/non-FHE/FHE-v1 compatibility tests.
3. Add logical printer output and a native producer that writes, reopens, and
   prints all four record kinds with `ir_b2a -st -src`.
4. Add producer-only builder attachments, owner-aware compiler-phase lookup,
   and atomic native value rewriting, with rollback, cross-PU local-index
   collision, and tree/image-consistency tests.
5. Add independently controlled `config_fhe.{h,cxx}` options and the
   `VHO_FHE_Convert_Driver()` hook after optional DSL WOPT/Preopt and before
   `VHO_DSL_Lower_Driver()`.
6. Hand the merged APIs to the FHE task for semantic gatekeeper, BatchNorm
   folding, conversion reports, and retained ResNet-20 artifacts.
7. Add a backend-owned all-PU conversion checkpoint that writes each converted
   PU while its local symbol table is active, validates the complete managed
   image after traversal, and atomically publishes a binary WHIRL artifact
   before ordinary DSL or language lowering.

Stage 5 is implemented by the Stage 3 infrastructure PR. The public option
surface is:

- `-FHE:convert=on|off`, enabled by default and a no-op when the artifact has
  no FHE records;
- `-FHE:strict_o0=on|off`, enabled by default so the conversion pass can
  distinguish mandatory semantic adaptation from optional optimization;
- `-FHE:dump_before=on|off`; and
- `-FHE:dump_after=on|off`;
- `-FHE:checkpoint=<path>`, which selects conversion-only certification and
  names the binary WHIRL output.

`VHO_FHE_Convert_Program_Unit()` performs the structural DSL/FHE/FHE-plan
gate, invokes the registered semantic gatekeeper, invokes the registered
conversion pass, then repeats structural and semantic verification. The
backend-facing `VHO_FHE_Convert_Driver()` wraps that checked service and owns
the optional before/after WHIRL trace. `VHO_FHE_Convert_Register_*` APIs let
the FHE task install semantic behavior without editing backend phase order or
mapped-image services. A registered pass may inspect its supplied WN root but
must use the reviewed DSL/FHE lookup and rewrite APIs for persistent changes;
it must not mutate WN, ST, TY, or mapped-image table fields directly.

Until the FHE semantic implementation is linked, enabling conversion for an
artifact that contains FHE records fails with `CFHE-CONVERT-001` instead of
silently lowering away FHE semantics. Non-FHE and legacy artifacts remain
unchanged even though the option defaults to enabled.

### All-PU Conversion Checkpoint

`-FHE:checkpoint=<path>` is an explicit file-level certification mode. It may
run the option-controlled DSL WOPT/Preopt preparation, then invokes
`VHO_FHE_Convert_Driver_Try()` exactly once for every PU. It does not
run `VHO_DSL_Lower_Driver()`, language VHO lowering, WOPT/LNO/CG, whirl2c, or
whirl2f after conversion. Unrelated phase options continue to be accepted and
are silently ignored in this mode according to the Open64 phase-option
convention.

`VHO_FHE_Convert_Driver_With_Result()` remains the public ordinary-pipeline
helper that returns the per-PU counters and preserves the established
fail-closed assertion. `VHO_FHE_Convert_Driver()` remains its source-compatible
wrapper for callers that do not need the result.

The backend owns traversal and file construction. While each PU and its local
symbol table are selected, it verifies the tree and symbol table and calls the
standard `Write_PU_Info()` service. After all PUs have succeeded, it aggregates
the `VHO_FHE_CONVERT_RESULT` counters and calls
`VHO_FHE_Convert_Checkpoint_Validate()` to prove that every expected PU was
converted without an error and that the complete DSL, effect, call, FHE, and
FHE-plan images are valid. It then calls the standard `Write_Global_Info()`
and closes the binary WHIRL image.

Checkpoint mode does not initialize the backend REGION optimization service
after conversion, and its matching PU postprocessing does not finalize that
service. The checkpoint only preserves and writes REGION WN nodes and their
managed RID records; it does not consume, lower, or optimize them. Optional
DSL WOPT preparation retains its own paired REGION initialization and
finalization before FHE conversion.

The writer initially uses `<path>.tmp` in the destination directory. Only a
fully converted, validated, and closed file is atomically renamed to `<path>`.
A failed run removes the temporary file and never publishes a partial artifact
under the requested checkpoint name. The temporary output is registered with
the backend's standard error and signal cleanup callback service so failures
outside the conversion callback also remove it without introducing a shared
library dependency on the backend driver executable. The FHE semantic task consumes this mode;
it must not reproduce PU selection, local-symbol-table lifetime, managed image
validation, or binary writer orchestration.

The retained integration fixture is
`osprey/common/com/tests/dsl_fhe_conversion_checkpoint_test.sh`. It requires a
reviewed multi-PU input, reopens the checkpoint with `ir_b2a -st -src`, checks
the expected `FUNC_ENTRY` count, and stages the source named by
`OPEN64_FHE_CHECKPOINT_SOURCE` beside the artifact for source-interleaved
inspection. Optional REGION count and contract-name checks prove that legal DSL
REGION metadata passes through checkpoint mode without invoking backend REGION
optimization initialization. The fixture may also prove fail-closed behavior
with an FHE-bearing input when semantic conversion support is intentionally
absent.

Stable checkpoint diagnostics are `CFHE-CHECKPOINT-001` for incomplete PU
coverage, `CFHE-CHECKPOINT-002` for aggregated conversion errors, and
`CFHE-CHECKPOINT-003` for an invalid complete managed image. A successful run
reports PU, semantic-gate, pass, disposition, rewrite, BatchNorm-fold,
approximation, and error counts.

No bootstrap insertion, SIHE/CKKS arithmetic opcode allocation,
`fhe.cnn.poly_activation` emission, OpenFHE/runtime lowering, or generated-C
work is part of this native planning-image stage.

## Exit Criteria

The contract is ready for implementation only after main/common and FHE review
agree on every row field, sentinel, identity, range rule, and API name and the
prerequisite gate above is closed. Implementation closes the focused SYNC-3
planning checkpoint when syntax/size tests, malformed-image tests, old-image
tests, producer reopen, gatekeeper checks, and retained `.B`/`.T` evidence all
pass without changing existing binary WHIRL behavior. It does not close v0.9
Architecture Phase 3 or M4.

## FHE Semantic Review Resolution

The FHE task accepted the four record families and confirmed that source,
tensor, FHE configuration/descriptor/key, payload, call-context, and source
operator facts should be referenced rather than copied. This physical contract
resolves the remaining representation choices as follows:

- disposition and CKKS state are keyed by existing node/value identity;
- approximation semantics are interned independently of source context, while
  each source association comes from its disposition row;
- signed CKKS quantities use `-1` as the named `<pending>` sentinel, and
  unsigned optional IDs use zero;
- `slot_count == 0` and an empty layout name mean inherited or pending;
- bootstrap reason is explicit rather than inferred from ReLU; and
- rejected/pending fold or conversion status remains diagnostic/report
  evidence and cannot masquerade as a valid mapped plan.
