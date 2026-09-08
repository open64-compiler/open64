# Open64 FHE SYNC-1 Native Image and API Proposal

Status: FHE-owned planning proposal for SYNC-1 review  
Coordination authority:
`/Users/shinmingliu/open64/doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md`  
Scope: contract freeze only; no opcode allocation or shared-file implementation

Main/common audit resolution: `FHE-SYNC1-NATIVE-CONTRACT.md` is authoritative
for the implemented physical rows, Open64 index widths, exact version-1 sizes,
and builder names. This proposal remains the FHE semantic requirements input.

This document is the FHE support task's SYNC-1 handoff to the main
WHIRL/common infrastructure task. It proposes the minimal first FHE image,
semantic interning rules, opaque builder API, `ir_b2a -st -src` spelling,
malformed-image tests, required main hooks, and FHE-owned file boundaries.

## Constraints

1. Preserve binary WHIRL compatibility and the private physical `OPR_DSL`
   abstraction.
2. Do not edit existing shared common/com, ELF, reader/writer, tensor, or
   printer files from the FHE branch before the main SYNC-1 infrastructure PR.
3. Reuse existing `OPR_DSLRELU` and logical `common.relu`; do not allocate a
   second ReLU operator.
4. Keep Python as a source frontend. Python receives opaque handles only.
5. Treat CKKS level/scale as value state, not a mutation of canonical `TY_IDX`
   or TensorDescriptorIR identity.
6. Do not persist C++ pointers, STL containers, OpenFHE objects, backend
   handles, ciphertext bytes, key bytes, or secret-key material in WHIRL.

## Fixed-Width Row Layout Rules

The historical FHE-side proposal below used fixed-width mapped-image fields and
4-byte alignment. Main/common review replaced that physical proposal with the
exact version-1 rows and 8-byte section/row-start alignment defined in
`FHE-SYNC1-NATIVE-CONTRACT.md`. The proposed
minimum C assumption is `sizeof(UINT32) == 4` and two's-complement `INT32`.
Rows intentionally avoid `UINT64` so layout is stable on 32-bit and 64-bit
hosts. Any 64-bit scalar is stored as `{lo, hi}` little-endian words in the
mapped image. Every row begins with `record_size` and `record_version` so a
future reader can reject or skip compatible extensions.

All persisted FHE IDs are `UINT32`. ID zero is invalid unless the field name or
row contract explicitly says `0 means absent`. Record arrays are one-based for
semantic references: row ID `1` means the first physical row in that table.

```c++
typedef UINT32 FHE_CONFIG_ID;
typedef UINT32 FHE_ENTRY_CONTRACT_ID;
typedef UINT32 FHE_ENTRY_VALUE_ID;
typedef UINT32 FHE_ENCRYPTION_DESCRIPTOR_ID;
typedef UINT32 FHE_TENSOR_BINDING_ID;
typedef UINT32 FHE_KEY_REQUIREMENT_ID;
typedef UINT32 FHE_CKKS_VALUE_STATE_ID;  // Future/value-specific; 0 means absent.
typedef UINT32 FHE_LAYOUT_ID;            // Future; 0 means absent.
typedef UINT32 FHE_APPROXIMATION_ID;     // Future/minimal optional; 0 means absent.
```

Append-only rules:

1. Never reorder fields in a published row.
2. Never reuse enum values or record IDs with changed meaning.
3. Add fields only by increasing `record_size` and setting a capability bit.
4. Readers must reject unknown required capabilities.
5. Reserved fields must be written as zero and rejected if nonzero unless a
   later capability gives them meaning.

## Minimal FHE Image Rows

The minimal first image contains seven row families: image header, compilation
config, entry contract, entry value, encryption descriptor, tensor binding, and
key requirement. CKKS value-state rows are intentionally not canonical type
rows; value-state persistence is a separate later table or phase-local binding
owned by CKKS planning.

### Header

Expected size: 96 bytes. Alignment: 4 bytes.

```c++
typedef struct FHE_IMAGE_HEADER {
  UINT32 magic;                       // "FHE1" encoded by main-owned convention.
  UINT32 major_version;               // Initial: 1.
  UINT32 minor_version;               // Initial: 0.
  UINT32 header_size;                 // sizeof(FHE_IMAGE_HEADER), initially 96.
  UINT32 flags;                       // FHE_IMAGE_FLAG_*.
  UINT32 capability_bits_lo;          // FHE_IMAGE_CAP_* lower 32 bits.
  UINT32 capability_bits_hi;          // FHE_IMAGE_CAP_* upper 32 bits.
  UINT32 row_alignment;               // Initial: 4.
  UINT32 config_row_size;             // sizeof(FHE_COMPILATION_CONFIG_RECORD).
  UINT32 entry_contract_row_size;     // sizeof(FHE_ENTRY_CONTRACT_RECORD).
  UINT32 entry_value_row_size;        // sizeof(FHE_ENTRY_VALUE_RECORD).
  UINT32 encryption_desc_row_size;    // sizeof(FHE_ENCRYPTION_DESCRIPTOR_RECORD).
  UINT32 tensor_binding_row_size;     // sizeof(FHE_TENSOR_BINDING_RECORD).
  UINT32 key_requirement_row_size;    // sizeof(FHE_KEY_REQUIREMENT_RECORD).
  UINT32 reserved_row_size_0;         // Must be zero at SYNC-1.
  UINT32 config_count;
  UINT32 entry_contract_count;
  UINT32 entry_value_count;
  UINT32 encryption_desc_count;
  UINT32 tensor_binding_count;
  UINT32 key_requirement_count;
  UINT32 reserved_count_0;            // Must be zero at SYNC-1.
  UINT32 reserved0;                   // Must be zero.
  UINT32 reserved1;                   // Must be zero.
} FHE_IMAGE_HEADER;
```

Initial header flags:

| Flag | Meaning |
| --- | --- |
| `FHE_IMAGE_FLAG_REQUIRED` | File requires FHE-aware validation before normal compilation. |
| `FHE_IMAGE_FLAG_REDACTED` | Printer/runtime must treat payload as redacted of secrets. |

Initial capability bits:

| Capability | Meaning |
| --- | --- |
| `FHE_IMAGE_CAP_MINIMAL_ENTRY` | Minimal seven-row image is present. |
| `FHE_IMAGE_CAP_EXTERNAL_PLAINTEXT_PARAMS` | Entry values may reference side-file plaintext parameters. |
| `FHE_IMAGE_CAP_APPROX_CONTRACT_REF` | Entry values or later nodes may reference approximation contracts by ID. |
| `FHE_IMAGE_CAP_VALUE_STATE_BINDING` | CKKS value state is carried outside canonical type identity. |

### Compilation Config

Expected size: 64 bytes. Alignment: 4 bytes. ID zero is invalid.

```c++
typedef struct FHE_COMPILATION_CONFIG_RECORD {
  UINT32 record_size;                 // Initial: 64.
  UINT32 record_version;              // Initial: 1.
  UINT32 flags;                       // FHE_CONFIG_FLAG_*.
  UINT32 provenance_mask;             // USER | PROFILE | COMPILER_DERIVED.
  UINT32 scheme;                      // FHE_SCHEME_CKKS initially.
  UINT32 security_level;              // e.g. HE128_CLASSIC.
  UINT32 ring_dimension;              // 0 means auto.
  UINT32 mult_depth_policy;           // AUTO | EXPLICIT.
  UINT32 mult_depth_value;            // Meaningful when explicit; otherwise 0.
  UINT32 scale_bits;                  // 0 means auto/profile default.
  UINT32 first_mod_bits;              // 0 means auto/profile default.
  UINT32 slots_policy;                // AUTO | EXPLICIT.
  UINT32 slots_value;                 // Meaningful when explicit; otherwise 0.
  UINT32 key_switch_policy;           // DEFAULT | HYBRID | BV, etc.
  UINT32 bootstrap_policy;            // AUTO | ON | MANUAL | OFF.
  UINT32 backend_policy;              // OPENFHE | MOCK | AUTO.
} FHE_COMPILATION_CONFIG_RECORD;
```

Semantic dedup key: scheme, security level, ring dimension, depth policy/value,
scale bits, first modulus bits, slots policy/value, key-switch policy,
bootstrap policy, backend policy, and flags that affect legality. Excluded:
source option spelling, profile name, diagnostics, source position, report
paths, and transformation history.

### Entry Contract

Expected size: 64 bytes. Alignment: 4 bytes. ID zero is invalid.

```c++
typedef struct FHE_ENTRY_CONTRACT_RECORD {
  UINT32 record_size;                 // Initial: 64.
  UINT32 record_version;              // Initial: 1.
  UINT32 flags;                       // FHE_ENTRY_FLAG_*.
  UINT32 owner_pu_id;                 // Main-owned stable PU reference.
  UINT32 config_id;                   // FHE_CONFIG_ID, nonzero.
  UINT32 entry_value_first;           // First FHE_ENTRY_VALUE_ID, 0 if none.
  UINT32 entry_value_count;           // Count; first/count must be in range.
  UINT32 input_count;
  UINT32 output_count;
  UINT32 parameter_count;
  UINT32 encrypted_io_policy;         // Cipher input/output policy.
  UINT32 parameter_policy;            // Plaintext/encoded plaintext initially.
  UINT32 accuracy_budget_id;          // 0 means absent at SYNC-1.
  UINT32 source_file_str_idx;         // 0 means absent.
  UINT32 source_line;                 // 1-based, 0 means absent.
  UINT32 source_column;               // 1-based, 0 means absent.
} FHE_ENTRY_CONTRACT_RECORD;
```

Semantic dedup key: owner PU, config ID, boundary roles/ordinals, encrypted I/O
policy, parameter policy, and accuracy budget ID when present. Excluded: source
file/line/column, user spelling, diagnostics, and report paths.

### Entry Value

Expected size: 96 bytes. Alignment: 4 bytes. ID zero is invalid.

```c++
typedef struct FHE_ENTRY_VALUE_RECORD {
  UINT32 record_size;                 // Initial: 96.
  UINT32 record_version;              // Initial: 1.
  UINT32 flags;                       // FHE_ENTRY_VALUE_FLAG_*.
  UINT32 entry_contract_id;           // FHE_ENTRY_CONTRACT_ID, nonzero.
  UINT32 st_idx;                      // ST_IDX carrier, 0 only if value_id used.
  UINT32 ty_idx;                      // TY_IDX carrier, nonzero for tensor value.
  UINT32 tensor_desc_id;              // Main TensorDescriptorIR ID, nonzero.
  UINT32 encryption_desc_id;          // FHE_ENCRYPTION_DESCRIPTOR_ID, nonzero.
  UINT32 ordinal;                     // Boundary ordinal within role.
  UINT32 role;                        // INPUT | OUTPUT | PARAMETER | INTERMEDIATE.
  UINT32 value_class;                 // CIPHERTEXT | ENCODED_PLAINTEXT | CLEAR.
  UINT32 source_file_str_idx;         // 0 means absent.
  UINT32 source_line;                 // 1-based, 0 means absent.
  UINT32 source_column;               // 1-based, 0 means absent.
  UINT32 side_file_path_str_idx;      // Plaintext parameter side file; 0 absent.
  UINT32 side_file_key_str_idx;       // Logical tensor key/name; 0 absent.
  UINT32 side_file_offset_lo;         // Byte offset low word.
  UINT32 side_file_offset_hi;         // Byte offset high word.
  UINT32 side_file_size_lo;           // Byte length low word.
  UINT32 side_file_size_hi;           // Byte length high word.
  UINT32 checksum_str_idx;            // 0 absent.
  UINT32 checksum_kind;               // NONE | SHA256 initially.
  UINT32 reserved0;                   // Must be zero.
  UINT32 reserved1;                   // Must be zero.
} FHE_ENTRY_VALUE_RECORD;
```

Semantic dedup key: entry contract ID, role, ordinal, `ty_idx`,
TensorDescriptorIR ID, EncryptionDescriptorIR ID, value class, side-file
logical key, byte range, checksum, and checksum kind for plaintext parameters.
Excluded: source position, symbol spelling when ordinal/key is stable,
diagnostics, local file path aliases, and transformation history.

### Encryption Descriptor

Expected size: 64 bytes. Alignment: 4 bytes. ID zero is invalid.

```c++
typedef struct FHE_ENCRYPTION_DESCRIPTOR_RECORD {
  UINT32 record_size;                 // Initial: 64.
  UINT32 record_version;              // Initial: 1.
  UINT32 flags;                       // FHE_ENC_DESC_FLAG_*.
  UINT32 value_class;                 // CIPHERTEXT | ENCODED_PLAINTEXT | CLEAR.
  UINT32 scheme;                      // CKKS initially; 0 only for CLEAR.
  UINT32 config_id;                   // FHE_CONFIG_ID; 0 only for CLEAR.
  UINT32 key_set_id;                  // Opaque public/eval key-set ID; 0 absent.
  UINT32 boundary_role;               // INPUT | OUTPUT | PARAMETER | INTERNAL.
  UINT32 slot_count_policy;           // AUTO | EXPLICIT | INHERIT.
  UINT32 slot_count_value;            // 0 unless explicit.
  UINT32 encoding_policy;             // NONE | CKKSPACKED | BACKEND_CACHE.
  UINT32 scale_policy;                // AUTO | EXPLICIT | INHERIT.
  UINT32 initial_scale_bits;          // 0 unless explicit.
  UINT32 initial_level_policy;        // AUTO | EXPLICIT | INHERIT.
  UINT32 initial_level;               // 0 unless explicit.
  UINT32 packing_policy;              // AUTO | METAKERNEL | FHELIPE | INHERIT.
} FHE_ENCRYPTION_DESCRIPTOR_RECORD;
```

Semantic dedup key: value class, scheme, config ID, key-set ID, boundary role,
slot-count policy/value, encoding policy, scale policy, initial scale bits,
initial level policy/value, packing policy, and flags that affect
confidentiality or legality. Excluded: source name, source position,
diagnostics, profile name, backend local path, and transformation history.

Corrected tensor encryption split: this descriptor is representation semantics
that can be interned and associated with canonical tensor identity. It must not
store changing CKKS level/scale for each produced value after arithmetic.
Post-lowering CKKS level/scale belongs to value-specific state bindings.

### Tensor Binding

Expected size: 48 bytes. Alignment: 4 bytes. ID zero is invalid.

```c++
typedef struct FHE_TENSOR_BINDING_RECORD {
  UINT32 record_size;                 // Initial: 48.
  UINT32 record_version;              // Initial: 1.
  UINT32 flags;                       // FHE_TENSOR_BINDING_FLAG_*.
  UINT32 ty_idx;                      // Canonical tensor TY_IDX, nonzero.
  UINT32 tensor_desc_id;              // Main TensorDescriptorIR ID, nonzero.
  UINT32 encryption_desc_id;          // FHE_ENCRYPTION_DESCRIPTOR_ID, nonzero.
  UINT32 binding_kind;                // TYPE_CANONICAL | VALUE_SPECIFIC_VIEW.
  UINT32 value_id;                    // 0 for canonical type binding.
  UINT32 source_file_str_idx;         // 0 means absent.
  UINT32 source_line;                 // 1-based, 0 means absent.
  UINT32 reserved0;                   // Must be zero.
  UINT32 reserved1;                   // Must be zero.
} FHE_TENSOR_BINDING_RECORD;
```

Semantic dedup key: canonical TensorDescriptorIR ID, EncryptionDescriptorIR ID,
and `binding_kind`; include `value_id` only for value-specific views. Excluded:
source position, symbol names, diagnostics, and pass provenance.

### Key Requirement

Expected size: 64 bytes. Alignment: 4 bytes. ID zero is invalid.

```c++
typedef struct FHE_KEY_REQUIREMENT_RECORD {
  UINT32 record_size;                 // Initial: 64.
  UINT32 record_version;              // Initial: 1.
  UINT32 flags;                       // FHE_KEY_REQ_FLAG_*.
  UINT32 config_id;                   // FHE_CONFIG_ID, nonzero.
  UINT32 key_set_id;                  // Opaque public/eval key set, nonzero.
  UINT32 key_class;                   // PUBLIC | RELIN | ROTATION | BOOTSTRAP.
  INT32  rotation_offset;             // Valid only for ROTATION; otherwise 0.
  UINT32 bootstrap_profile_id;        // Valid only for BOOTSTRAP; otherwise 0.
  UINT32 source_entry_value_id;       // 0 means not boundary-derived.
  UINT32 source_operator_id;          // Logical operator/node ID; 0 absent.
  UINT32 source_file_str_idx;         // 0 means absent.
  UINT32 source_line;                 // 1-based, 0 means absent.
  UINT32 source_column;               // 1-based, 0 means absent.
  UINT32 requirement_group;           // Groups keys needed by one plan; 0 absent.
  UINT32 reserved0;                   // Must be zero.
  UINT32 reserved1;                   // Must be zero.
} FHE_KEY_REQUIREMENT_RECORD;
```

Semantic dedup key: config ID, key-set ID, key class, rotation offset for
rotation keys, bootstrap profile for bootstrap keys, and requirement group when
nonzero. Excluded: source position, diagnostics, key file path spelling,
secret-key provenance, and runtime installation path.

## Value-Specific CKKS State Binding

The minimal image deliberately separates canonical tensor encryption from
changing CKKS value state. Arithmetic, rescale, relinearization, mod switch, and
bootstrap produce new value-state associations. They must not mutate a
canonical `TY_IDX`, TensorDescriptorIR, or EncryptionDescriptorIR behind
existing values.

SYNC-1 should freeze the API below even if the persisted
`FHE_CKKS_VALUE_STATE_RECORD` table is materialized later:

```c++
typedef UINT32 DSL_FHE_CKKS_VALUE_STATE_ID;

DSL_FHE_CKKS_VALUE_STATE_ID
DSL_FHE_Intern_CKKS_Value_State(
    const DSL_FHE_CKKS_VALUE_STATE *state);

BOOL
DSL_Builder_Bind_FHE_Value_CKKS_State(
    DSL_BUILDER_VALUE value,
    DSL_FHE_CKKS_VALUE_STATE_ID state);

BOOL
DSL_Builder_Get_FHE_Value_CKKS_State(
    DSL_BUILDER_VALUE value,
    DSL_FHE_CKKS_VALUE_STATE *state);
```

If SYNC-1 does not add a persisted CKKS state table, these APIs may bind to a
phase-local or existing extensible DSL association. `ir_b2a` may print
`ckks_state_id=0` for source-level FHE records until CKKS planning materializes
state rows.

## Opaque Builder API Proposal

The FHE frontend and tests use opaque IDs and `DSL_BUILDER_VALUE` handles only.
They must not inspect `TY_IDX`, `ST_IDX`, WN fields, mapped-image offsets,
physical `OPR_DSL`, or backend objects.

```c++
typedef UINT32 DSL_FHE_CONFIG_ID;
typedef UINT32 DSL_FHE_ENTRY_CONTRACT_ID;
typedef UINT32 DSL_FHE_ENTRY_VALUE_ID;
typedef UINT32 DSL_FHE_ENCRYPTION_DESCRIPTOR_ID;
typedef UINT32 DSL_FHE_TENSOR_BINDING_ID;
typedef UINT32 DSL_FHE_KEY_REQUIREMENT_ID;
typedef UINT32 DSL_FHE_CKKS_VALUE_STATE_ID;

void
DSL_FHE_Reset_Image(DSL_BUILDER_CONTEXT ctx);

BOOL
DSL_FHE_Validate_Image(
    DSL_BUILDER_CONTEXT ctx,
    DSL_DIAGNOSTIC_SINK diagnostics);

DSL_FHE_CONFIG_ID
DSL_FHE_Intern_Compilation_Config(
    DSL_BUILDER_CONTEXT ctx,
    const DSL_FHE_COMPILATION_CONFIG *config);

BOOL
DSL_FHE_Get_Compilation_Config(
    DSL_BUILDER_CONTEXT ctx,
    DSL_FHE_CONFIG_ID config,
    DSL_FHE_COMPILATION_CONFIG *out_config);

DSL_FHE_ENCRYPTION_DESCRIPTOR_ID
DSL_FHE_Intern_Encryption_Descriptor(
    DSL_BUILDER_CONTEXT ctx,
    const DSL_FHE_ENCRYPTION_DESCRIPTOR *descriptor);

BOOL
DSL_FHE_Get_Encryption_Descriptor(
    DSL_BUILDER_CONTEXT ctx,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor,
    DSL_FHE_ENCRYPTION_DESCRIPTOR *out_descriptor);

DSL_FHE_TENSOR_BINDING_ID
DSL_Builder_Bind_FHE_Tensor_Descriptor(
    DSL_BUILDER_CONTEXT ctx,
    TY_IDX canonical_ty,
    DSL_TENSOR_DESCRIPTOR_ID tensor,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption);

BOOL
DSL_Builder_Get_FHE_Tensor_Binding(
    DSL_BUILDER_CONTEXT ctx,
    TY_IDX canonical_ty,
    DSL_FHE_TENSOR_BINDING *out_binding);

DSL_FHE_ENTRY_CONTRACT_ID
DSL_Builder_Attach_FHE_Entry_Contract(
    DSL_BUILDER_PROGRAM_UNIT pu,
    const DSL_FHE_ENTRY_CONTRACT *contract);

DSL_FHE_ENTRY_VALUE_ID
DSL_Builder_Declare_FHE_Entry_Value(
    DSL_FHE_ENTRY_CONTRACT_ID entry,
    DSL_BUILDER_VALUE value,
    UINT32 ordinal,
    DSL_FHE_ENTRY_VALUE_ROLE role,
    const DSL_FHE_ENTRY_VALUE_INFO *info);

DSL_FHE_KEY_REQUIREMENT_ID
DSL_Builder_Add_FHE_Key_Requirement(
    DSL_BUILDER_CONTEXT ctx,
    DSL_FHE_CONFIG_ID config,
    const DSL_FHE_KEY_REQUIREMENT *requirement);

DSL_FHE_APPROXIMATION_ID
DSL_Builder_Attach_FHE_Approximation_Contract(
    DSL_BUILDER_VALUE value,
    const DSL_FHE_APPROXIMATION_CONTRACT *contract);

DSL_FHE_CKKS_VALUE_STATE_ID
DSL_FHE_Intern_CKKS_Value_State(
    DSL_BUILDER_CONTEXT ctx,
    const DSL_FHE_CKKS_VALUE_STATE *state);

BOOL
DSL_Builder_Bind_FHE_Value_CKKS_State(
    DSL_BUILDER_VALUE value,
    DSL_FHE_CKKS_VALUE_STATE_ID state);

BOOL
DSL_Builder_Get_FHE_Value_CKKS_State(
    DSL_BUILDER_VALUE value,
    DSL_FHE_CKKS_VALUE_STATE *out_state);
```

`DSL_Builder_Bind_FHE_Tensor_Descriptor` binds the canonical tensor descriptor
to an encryption descriptor. `DSL_Builder_Bind_FHE_Value_CKKS_State` binds the
current CKKS state to a produced value. These APIs are intentionally separate.

## `ir_b2a -st -src` Spelling

When FHE rows exist, `ir_b2a -st -src` prints these headings in this order:

```text
FHE Compilation Configurations
FHE Entry Contracts
FHE Entry Values
FHE Encryption Descriptors
FHE Tensor Bindings
FHE Key Requirements
FHE Approximation Contracts
FHE CKKS Value States
FHE Backend Requirements
```

Representative output:

```text
FHE Compilation Configurations
  #1 scheme=ckks security=128-classic ring_dimension=65536
     scale_bits=56 first_mod_bits=60 slots=auto
     key_switch=hybrid bootstrap=auto backend=openfhe

FHE Entry Contracts
  #1 pu=secure_resnet20_forward config=#1
     inputs=1 outputs=1 parameters=62 parameter_policy=plaintext_encoded
     source=/work/models/secure_resnet20.py:14:5

FHE Entry Values
  #1 role=input ordinal=0 st=x ty=TENSOR[1,3,32,32]
     tensor_desc=#17 encryption=#1 value_class=ciphertext
     source=/work/models/secure_resnet20.py:22:18
  #2 role=parameter ordinal=0 st=layer1.0.conv1.weight ty=TENSOR[16,16,3,3]
     tensor_desc=#31 encryption=#2 value_class=encoded_plaintext
     side_file=secure_resnet20.safetensors key=layer1.0.conv1.weight
     offset=4096 size=9216 checksum.sha256=9a4c...
     source=/work/models/resnet20.py:47:13

FHE Encryption Descriptors
  #1 value_class=ciphertext scheme=ckks config=#1 boundary=input
     slots=auto scale=inherit level=inherit packing=auto key_set=request_key
  #2 value_class=encoded_plaintext scheme=ckks config=#1 boundary=parameter
     encoding=ckks_packed scale=auto level=auto packing=metakernel

FHE Tensor Bindings
  #1 ty=TENSOR[1,3,32,32] tensor_desc=#17 encryption=#1 kind=type_canonical
  #2 ty=TENSOR[16,16,3,3] tensor_desc=#31 encryption=#2 kind=type_canonical

FHE Key Requirements
  #1 class=public config=#1 key_set=request_key
  #2 class=relinearization config=#1 key_set=request_key
  #3 class=bootstrap config=#1 key_set=request_key source=common.relu@/work/models/resnet20.py:52:16

Source Interleaving
  /work/models/resnet20.py:52:16
    %43 = common.relu(%42) tensor_desc=#44 encryption=#1
          fhe.approximation=#7 bootstrap_policy=auto
```

The printer must not expose physical `OPR_DSL` payload details, mapped-image
offsets as user-facing semantics, secret-key paths or bytes, ciphertext bytes,
OpenFHE C++ types, or backend object layouts.

## Bounds and Malformed-Image Test Matrix

| Test | Required result |
| --- | --- |
| Legacy non-FHE `.B` | Reopens unchanged; no FHE headings appear. |
| New non-FHE `.B` | Writer emits no empty FHE image. |
| FHE image with bad magic | Reader rejects before table access. |
| Unsupported major version | Reader rejects with unsupported-version diagnostic. |
| Unknown required capability bit | Reader rejects with unsupported-capability diagnostic. |
| Nonzero reserved field without capability | Reader rejects as malformed. |
| Row size smaller than SYNC-1 minimum | Reader rejects before row traversal. |
| Row size larger with no compatible capability | Reader rejects or skips only if the main compatibility rule allows skip. |
| Count overflows image bounds | Reader rejects before exposing records. |
| `first/count` span overflows table | Reader rejects before pass use. |
| Entry references config ID zero or out of range | Reader or gatekeeper rejects. |
| Entry value references missing tensor descriptor | Gatekeeper rejects. |
| Entry value references missing encryption descriptor | Gatekeeper rejects. |
| Duplicate input/output ordinal in one entry | Gatekeeper rejects. |
| Plaintext parameter side-file has missing key/range/checksum | Gatekeeper rejects. |
| Secret-key material or decrypt/server-keygen marker | Gatekeeper rejects; no valid redacted record is created. |
| `common.relu` in encrypted CKKS path without approximation contract | FHE gatekeeper rejects. |
| `bootstrap=manual` and surviving `common.relu` lacks explicit refresh | FHE gatekeeper rejects. |
| `bootstrap=off` and surviving `common.relu` remains | FHE gatekeeper rejects in first release. |

## Generic Hooks Required From Main Task

The FHE task requests these main-owned hooks for SYNC-1:

1. Optional mapped-image registration or existing extensible DSL image carrier
   for FHE fixed rows.
2. Reader/writer dispatch hooks with row-size, capability, and range validation
   before pass use.
3. TensorDescriptorIR association hook that binds an FHE encryption descriptor
   without changing `TY_KIND`, canonical tensor shape/dtype, or WHIRL type
   encoding.
4. Value-specific association hook for CKKS state that is not part of canonical
   type equivalence.
5. Opaque builder context/value/program-unit handles and source-position
   propagation.
6. Logical DSL printer hooks for FHE section headings and per-node descriptor
   summaries.
7. Generic gatekeeper invocation point after DSL/TensorDescriptorIR validation
   and before FHE conversion/lowering.
8. Diagnostic sink for stable `CFHE-*`, `CFHECNN-*`, `CFHECKKS-*`, and
   `CFHERT-*` errors.
9. Unlowered-node verifier before `whirl2c`.

## FHE-Owned Files Proposed After SYNC-1 Review

The FHE task proposes these new files after main hook review. Existing shared
files remain main-owned unless explicitly assigned in a checkpoint.

| File | Purpose |
| --- | --- |
| `osprey/common/com/dsl_fhe.h` | Public fixed-row structs, IDs, enums, and builder declarations reviewed by main. |
| `osprey/common/com/dsl_fhe.cxx` | FHE row interning, validation helpers, and descriptor association implementation behind main hooks. |
| `osprey/common/com/dsl_fhe_reader.cxx` | FHE-specific row validation and image import helpers, called from main reader hooks. |
| `osprey/common/com/dsl_fhe_writer.cxx` | FHE-specific image finalization helpers, called from main writer hooks. |
| `osprey/common/com/dsl_fhe_print.cxx` | FHE `ir_b2a -st -src` section formatting behind main printer hooks. |
| `osprey/common/com/dsl_fhe_gatekeeper.cxx` | FHE entry, encryption, secret-key, approximation, and bootstrap validation. |
| `osprey/common/com/tests/dsl_fhe_image_contract_test.cxx` | Minimal producer/read/print test and non-FHE compatibility test. |
| `osprey/common/com/tests/dsl_fhe_malformed_image_test.cxx` | Bad version, capability, ID, row-size, range, and reserved-field tests. |
| `osprey/torch2whirl/FHE-INGESTION-PLAN.md` | SYNC-2 frontend plan for ResNet-20 capture, after native APIs merge. |

## WT_DSL_FHE_IMAGE Assessment

There is no SYNC-1 semantic conflict with adding `WT_DSL_FHE_IMAGE` if the main
task confirms the WHIRL extension point is optional, versioned, ignored or
rejected deterministically by legacy readers according to existing policy, and
covered by reader/writer and `ir_b2a -st -src` tests.

The minimal records can safely stage through an existing extensible DSL image
only if that carrier supports:

1. fixed-row typed payloads or an equivalent schema table;
2. image-level version and capability bits;
3. row-size validation;
4. deterministic `first/count` range checks;
5. optional absence for non-FHE files with no empty section emission;
6. stable FHE printer headings; and
7. rejection of required unknown versions/capabilities.

Recommendation: prefer `WT_DSL_FHE_IMAGE` if the existing DSL image cannot
provide typed fixed rows and capability-gated versioning cleanly. Otherwise,
stage the minimal FHE records through the existing extensible DSL image for
SYNC-1 and reserve `WT_DSL_FHE_IMAGE` for the point where main concludes a
separate section is necessary for compatibility or maintenance.

## Main-Task Handoff

For SYNC-1, the FHE task asks the main task to audit:

1. whether the current DSL image can carry the minimal fixed FHE rows, or
   whether `WT_DSL_FHE_IMAGE` is required now;
2. exact `TY_IDX`, `ST_IDX`, PU, source-position, and TensorDescriptorIR ID
   carrier widths for the mapped-image rows;
3. whether row alignment must be 4 bytes or match an existing mapped-image
   alignment rule;
4. the generic attachment hook for EncryptionDescriptorIR on canonical tensor
   descriptors;
5. the separate value-state association hook for CKKS level/scale;
6. final names for the opaque builder entry points;
7. `ir_b2a -st -src` printer integration points and section ordering; and
8. the legacy-reader behavior for an unknown optional FHE image.

Until that audit closes, the FHE branch should not implement reader/writer
wiring, allocate FHE/SIHE/CKKS opcodes, or edit existing shared files.
