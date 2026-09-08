# Open64 FHE SYNC-1 Semantic Proposal and Consumption Notes

Status: FHE-owned semantic input, consumed after PR #102 merge
Coordination authority:
`/Users/shinmingliu/open64/doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md`  
Authoritative native contract: `FHE-SYNC1-NATIVE-CONTRACT.md`
Scope: semantic requirements and post-merge consumption notes only; no opcode
allocation or shared-file implementation

Main/common audit resolution: `FHE-SYNC1-NATIVE-CONTRACT.md` is authoritative
for the implemented physical rows, Open64 index widths, exact version-1 sizes,
builder names, optional-section behavior, validation scope, and printer
headings. This document remains the FHE semantic requirements input and records
where the earlier proposal was superseded by source-verified main/common
decisions.

This document is the FHE support task's SYNC-1 handoff to the main
WHIRL/common infrastructure task. It proposes the minimal first FHE image,
semantic interning rules, opaque builder API, `ir_b2a -st -src` spelling,
malformed-image tests, required main hooks, and FHE-owned file boundaries.

## Constraints

1. Preserve binary WHIRL compatibility and the private physical `OPR_DSL`
   abstraction.
2. After the SYNC-1 infrastructure merge, consume only the published
   `DSL_FHE_*` managed-image services and `DSL_Builder_*` opaque wrappers from
   FHE-owned work.
3. Reuse existing `OPR_DSLRELU` and logical `common.relu`; do not allocate a
   second ReLU operator.
4. Keep Python as a source frontend. Python receives opaque handles only.
5. Treat CKKS level/scale as value state, not a mutation of canonical `TY_IDX`
   or TensorDescriptorIR identity.
6. Do not persist C++ pointers, STL containers, OpenFHE objects, backend
   handles, ciphertext bytes, key bytes, or secret-key material in WHIRL.

## Authoritative SYNC-1 Contract Consumption

The FHE task accepts the merged main/common SYNC-1 substrate without blocking
conflicts:

1. The minimal FHE image is a separate optional `.WHIRL.dsl_fhe` section with
   `sh_info` value `WT_DSL_FHE_IMAGE` (`0x23`). The existing `.WHIRL.dsl` image
   is exact-sized and must not be extended in place for these rows.
2. The section and every row start are 8-byte aligned. Version-1 row sizes are
   exact: header 64, config 64, entry contract 48, entry value 32, encryption
   descriptor 56, tensor binding 24, and key requirement 48 bytes.
3. `TY_IDX` and `ST_IDX`, including PU global function ST identity, are
   32-bit. `STR_IDX` is 64-bit.
4. The canonical tensor `TY_IDX` is the TensorDescriptorIR identity. There is
   no separate persisted TensorDescriptorIR ID in the FHE image.
5. Source positions, symbol names, diagnostics, profiles, physical side-file
   paths, and transformation history remain in existing WN/ST/DST/value records
   and compiler metadata. They are not duplicated into FHE source-position
   rows and do not participate in type or encryption equivalence.
6. EncryptionDescriptorIR stores stable representation semantics and binds
   independently to canonical tensor TY. CKKS level, scale, component count,
   and precision are value-specific state and remain deferred to a later
   capability/API; canonical TY and encryption descriptors are not mutated for
   arithmetic state changes.
7. `DSL_Builder_Verify_Program` invokes `DSL_FHE_Image_Validate` after normal
   DSL/tensor validation. SYNC-1 structural validation does not allocate FHE
   opcodes, convert ReLU, insert bootstrap, lower to OpenFHE, or own later
   stable `CFHE*` semantic diagnostics.
8. Legacy and non-FHE compatibility follows the native contract: old readers
   ignore unknown optional `SHT_MIPS_WHIRL` sections, new readers treat absence
   as an empty FHE image, and non-FHE writers omit `.WHIRL.dsl_fhe`.

The historical physical row sketches below are retained only as the semantic
requirements that fed SYNC-1 review. They are not ABI text and must not be used
for implementation when they differ from `FHE-SYNC1-NATIVE-CONTRACT.md`.

## Historical Fixed-Width Row Layout Proposal, Superseded Physically

The historical FHE-side proposal below used fixed-width mapped-image fields and
4-byte alignment. Main/common review replaced that physical proposal with the
exact version-1 rows and 8-byte section/row-start alignment defined in
`FHE-SYNC1-NATIVE-CONTRACT.md`. The authoritative contract uses current Open64
fixed-width table-index types, including 64-bit `STR_IDX`, and exact v1 row
sizes rather than per-row `record_size`/`record_version` fields.

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

## Historical Minimal FHE Image Rows, Superseded Physically

The authoritative minimal image contains these seven row families: image
header, compilation config, entry contract, entry value, encryption descriptor,
tensor binding, and key requirement. CKKS value-state rows are intentionally
not canonical type rows; value-state persistence remains a separate later table
or phase-local binding owned by CKKS planning.

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

## Value-Specific CKKS State Binding, Deferred

The minimal image deliberately separates canonical tensor encryption from
changing CKKS value state. Arithmetic, rescale, relinearization, mod switch, and
bootstrap produce new value-state associations. They must not mutate a
canonical `TY_IDX`, TensorDescriptorIR, or EncryptionDescriptorIR behind
existing values.

SYNC-1 intentionally does not freeze a persisted CKKS value-state table. A
later FHE/CKKS checkpoint may add APIs equivalent to:

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

Those deferred APIs must not reinterpret any version-1
EncryptionDescriptorIR or tensor-binding fields.

## Opaque Builder API Consumption

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

BOOL
DSL_FHE_Image_Validate(...);

DSL_FHE_CONFIG_ID
DSL_FHE_Intern_Compilation_Config(
    const DSL_FHE_COMPILATION_CONFIG_RECORD *record);

DSL_FHE_ENCRYPTION_DESCRIPTOR_ID
DSL_FHE_Intern_Encryption_Descriptor(
    const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record);

DSL_FHE_TENSOR_BINDING_ID
DSL_Builder_Bind_FHE_Tensor_Descriptor(
    TY_IDX canonical_ty,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption_descriptor_id,
    UINT32 flags);

DSL_FHE_ENTRY_CONTRACT_ID
DSL_Builder_Attach_FHE_Entry_Contract(
    DSL_BUILDER_PROGRAM_UNIT pu,
    const DSL_FHE_ENTRY_CONTRACT_INFO *info);

DSL_FHE_ENTRY_VALUE_ID
DSL_Builder_Declare_FHE_Entry_Value(
    DSL_FHE_ENTRY_CONTRACT_ID entry,
    DSL_BUILDER_VALUE value,
    UINT32 ordinal,
    DSL_FHE_ENTRY_VALUE_ROLE role,
    const DSL_FHE_ENTRY_VALUE_INFO *info);

DSL_FHE_KEY_REQUIREMENT_ID
DSL_FHE_Intern_Key_Requirement(
    const DSL_FHE_KEY_REQUIREMENT_RECORD *record);

BOOL
DSL_Builder_Get_FHE_Value_Encryption_Descriptor(
    DSL_BUILDER_VALUE value,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record);
```

`DSL_Builder_Bind_FHE_Tensor_Descriptor` binds the canonical tensor descriptor
to an encryption descriptor. Later CKKS value-state APIs remain intentionally
separate.

## `ir_b2a -st -src` Spelling

When FHE rows exist, the authoritative `ir_b2a -st -src` headings are:

```text
FHE Compilation Configuration Table:
FHE Entry Contract Table:
FHE Entry Value Table:
FHE Encryption Descriptor Table:
FHE Tensor Binding Table:
FHE Key Requirement Table:
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

## Generic Hooks Resolved By Main Task

The main/common SYNC-1 implementation resolved the structural hooks required
for the first FHE image:

1. Optional mapped-image registration through `.WHIRL.dsl_fhe`.
2. Reader/writer dispatch hooks with exact row-size, capability, reserved-field,
   ID, string/table-reference, and first/count validation.
3. Tensor encryption binding against canonical `TY_IDX` without changing
   `TY_KIND`, tensor shape/dtype identity, or WHIRL type encoding.
4. Opaque builder wrappers for FHE config, descriptor, tensor/type binding,
   entry contract, entry value, key requirement, query, reset, and validation.
5. Logical `ir_b2a -st -src` table headings and printing for the six v1 row
   tables.
6. Structural validation from `DSL_Builder_Verify_Program`.

Deferred to later FHE milestones: CKKS value-state persistence, FHE semantic
gatekeeper diagnostics, ReLU approximation/refresh checks, unlowered
FHE/SIHE/CKKS-node gates, frontend bindings, and lowering.

## FHE-Owned Files After SYNC-1 Review

The main/common PR owns the initial native infrastructure files. FHE-owned
later work should avoid overlapping shared files unless assigned at a sync
checkpoint.

| File | Purpose |
| --- | --- |
| File area | Purpose |
| --- | --- |
| Existing `osprey/common/com/dsl_fhe*` native files | Main/common-owned structural image, validation, print, reader/writer, and builder substrate from SYNC-1. |
| FHE semantic gatekeeper files, exact path TBD | Entry, encryption, secret-key, approximation, and bootstrap validation after ownership assignment. |
| FHE conversion/lowering files, exact path TBD | ResNet-20 FHE adaptation, SIHE/CKKS planning, and runtime ABI lowering after later checkpoints. |
| `osprey/torch2whirl/FHE-INGESTION-PLAN.md` | SYNC-2 frontend plan for ResNet-20 capture, after native APIs merge. |

## WT_DSL_FHE_IMAGE Assessment

Resolved: SYNC-1 requires `WT_DSL_FHE_IMAGE` and `.WHIRL.dsl_fhe`. The existing
`.WHIRL.dsl` v1 header is exact-sized, so staging the minimal records through
that image would violate compatibility.

## Main-Task Handoff

SYNC-1 main/common audit is closed by `FHE-SYNC1-NATIVE-CONTRACT.md` and PR
#102. FHE follow-on work should now consume that substrate and prepare the
SYNC-2 ResNet-20 frontend artifact certification plan. FHE opcode allocation,
semantic gatekeeper implementation, ReLU conversion, CKKS value-state planning,
and lowering still require their later synchronized checkpoints.
