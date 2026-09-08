# Open64 FHE SYNC-1 Native Contract

Status: main/common implementation authority for SYNC-1
FHE input proposal: `FHE-SYNC1-NATIVE-IMAGE-API-PROPOSAL.md`
Coordination authority: `FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md`

## Decision

SYNC-1 adds one optional `.WHIRL.dsl_fhe` section with `sh_info` value
`WT_DSL_FHE_IMAGE`. The existing `.WHIRL.dsl`, effect, callsite, symbol/type,
and PU images remain unchanged. A non-FHE compilation emits no FHE section.

This is a compatible extension under the existing `WHIRL::0.34` DSL revision:

- existing 0.34-aware readers ignore the unknown optional section;
- the new reader accepts old 0.33 and non-FHE 0.34 files with an empty FHE
  image;
- a file containing FHE records requires an FHE-aware gate before ordinary
  lowering, but does not change the physical WN, TY, ST, or existing DSL image;
- version 1 rows have exact sizes. A future row-layout change requires a
  reviewed image version change rather than interpreting a variable row size.

This follows the WHIRL model: the tree remains strict, TY/ST references remain
table indices, and supplemental facts live in a mapped table rather than
expanding every WN. The new image is copied from the mapped file into the
existing table service before the file mapping is released.

## Semantic Split

The current canonical tensor `TY_IDX` is the TensorDescriptorIR identity. The
FHE image does not invent a parallel tensor descriptor ID or a new `TY_KIND`.

```text
canonical TY_IDX / TensorDescriptorIR
  + interned EncryptionDescriptorIR
  = FHE tensor binding

DSL value
  + later CKKSValueStateIR
  = value-specific level/scale/component/precision state
```

EncryptionDescriptorIR contains stable representation semantics: value class,
scheme/configuration, public/evaluation key-set identity, slot policy, encoding
policy, and packing policy. Changing CKKS level, scale, component count, or
precision never mutates a canonical TY or interned encryption descriptor.

Source positions, symbol names, diagnostics, profiles, lineage, transformation
history, and physical side-file paths do not participate in tensor or
encryption equivalence. Entry values refer to existing DSL values. Their
source and external-constant evidence remains authoritative in the referenced
WHIRL value, ST, TCON, DST, and compiler metadata.

The side-file placement class does participate in TensorDescriptorIR
equivalence. An external plaintext parameter therefore uses a canonical tensor
TY distinct from an otherwise shape/dtype-identical runtime host input. The
physical filename remains symbol/TCON metadata and does not enter type
equivalence.

## Version 1 Image

Fields use Open64's existing fixed-width scalar and table-index types:
`ST_IDX` and `TY_IDX` are 32-bit, while `STR_IDX` is 64-bit. Rows contain no
pointers, STL objects, runtime handles, key bytes, or ciphertext bytes. The ELF
section and every row start at 8-byte alignment. Exact row sizes are explicit
and do not depend on the host compiler's natural alignment rules.

| Row | Size | Semantic identity |
| --- | ---: | --- |
| `DSL_FHE_IMAGE_HEADER` | 64 | One per section; exact version/capability/count contract |
| `DSL_FHE_COMPILATION_CONFIG_RECORD` | 64 | All legality-affecting configuration fields |
| `DSL_FHE_ENTRY_CONTRACT_RECORD` | 48 | Owner PU plus config and boundary policies |
| `DSL_FHE_ENTRY_VALUE_RECORD` | 32 | Entry, role, ordinal, DSL value, value class, encryption descriptor |
| `DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD` | 56 | Stable representation semantics only |
| `DSL_FHE_TENSOR_BINDING_RECORD` | 24 | Canonical `TY_IDX` plus encryption descriptor |
| `DSL_FHE_KEY_REQUIREMENT_RECORD` | 48 | Config, public/evaluation key set, class, and class-specific parameter |

All IDs are one-based `UINT32`; zero means invalid or absent only where the
field contract explicitly permits absence. Published enum values and row
fields are append-only. Reserved fields are written as zero and rejected when
nonzero.

The header carries magic, image version, header size, record-kind count,
capabilities, flags, six table counts, and four reserved words. Version 1
requires exactly these six capability bits:

```text
CONFIG | ENTRY_CONTRACT | ENTRY_VALUE |
ENCRYPTION_DESCRIPTOR | TENSOR_BINDING | KEY_REQUIREMENT
```

The reader computes the exact expected section size with overflow checks before
examining any row. It then validates sequential IDs, enum ranges, string IDs,
foreign IDs, and every entry `first/count` range before publishing the image.

## Native API

`dsl_fhe.h` owns fixed rows, public enums, initialization, interning, lookup,
reset, validation, mapped-image loading, and logical printing. The initial
opaque builder surface is:

```c++
DSL_FHE_CONFIG_ID
DSL_FHE_Intern_Compilation_Config(
    const DSL_FHE_COMPILATION_CONFIG_RECORD *record);

DSL_FHE_ENCRYPTION_DESCRIPTOR_ID
DSL_FHE_Intern_Encryption_Descriptor(
    const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record);

DSL_FHE_TENSOR_BINDING_ID
DSL_FHE_Intern_Tensor_Binding(
    TY_IDX tensor_ty,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption_descriptor_id,
    UINT32 flags);

DSL_FHE_ENTRY_CONTRACT_ID
DSL_FHE_Add_Entry_Contract(
    const DSL_FHE_ENTRY_CONTRACT_RECORD *record);

DSL_FHE_ENTRY_VALUE_ID
DSL_FHE_Add_Entry_Value(
    const DSL_FHE_ENTRY_VALUE_RECORD *record);

BOOL
DSL_FHE_Set_Entry_Value_Range(
    DSL_FHE_ENTRY_CONTRACT_ID entry_contract_id,
    DSL_FHE_ENTRY_VALUE_ID first_entry_value_id,
    UINT32 entry_value_count);

DSL_FHE_KEY_REQUIREMENT_ID
DSL_FHE_Intern_Key_Requirement(
    const DSL_FHE_KEY_REQUIREMENT_RECORD *record);

DSL_FHE_TENSOR_BINDING_ID
DSL_Builder_Bind_FHE_Tensor_Descriptor(
    TY_IDX tensor_ty,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption_descriptor_id,
    UINT32 flags);

DSL_FHE_ENTRY_CONTRACT_ID
DSL_Builder_Attach_FHE_Entry_Contract(
    DSL_BUILDER_PROGRAM_UNIT pu,
    const DSL_FHE_ENTRY_CONTRACT_INFO *info);

DSL_FHE_ENTRY_VALUE_ID
DSL_Builder_Declare_FHE_Entry_Value(
    DSL_FHE_ENTRY_CONTRACT_ID entry_contract_id,
    DSL_BUILDER_VALUE value,
    UINT32 ordinal,
    DSL_FHE_ENTRY_VALUE_ROLE role,
    const DSL_FHE_ENTRY_VALUE_INFO *info);

BOOL
DSL_Builder_Get_FHE_Value_Encryption_Descriptor(
    DSL_BUILDER_VALUE value,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record);
```

These builder wrappers accept the existing opaque PU/value handles. A future
torch2whirl binding may expose equivalent opaque operations after this contract
merges, but Python must not see physical rows, WHIRL indices, or WN layout.

The later CKKS capability will add a separate value-state row and opaque APIs
equivalent to:

```c++
DSL_FHE_CKKS_VALUE_STATE_ID DSL_FHE_Intern_CKKS_Value_State(...);
BOOL DSL_Builder_Bind_FHE_Value_CKKS_State(
    DSL_BUILDER_VALUE value,
    DSL_FHE_CKKS_VALUE_STATE_ID state);
```

That deferred addition must not reinterpret any version-1 descriptor field.

## Inspection Contract

`ir_b2a -st -src` prints FHE data only when the optional section is present,
using these stable headings:

```text
FHE Compilation Configuration Table:
FHE Entry Contract Table:
FHE Entry Value Table:
FHE Encryption Descriptor Table:
FHE Tensor Binding Table:
FHE Key Requirement Table:
```

Entry-value output names the referenced DSL value and symbol. Plaintext
parameter payload evidence remains in the symbol/TCON dump, including the
side-file name, byte range, checksum, and source location. The FHE printer does
not expose physical `OPR_DSL`, mapped offsets, key material, ciphertext bytes,
or backend object layouts.

## SYNC-1 Exit

SYNC-1 closes only after all of these pass:

1. record-size assertions and syntax tests;
2. empty-image reset and no empty section for non-FHE output;
3. legacy 0.33 and non-FHE 0.34 `.B` reopen behavior;
4. bad magic, version, capability, reserved field, count overflow, bad ID,
   bad first/count range, and bad string/table reference rejection;
5. a native FHE producer writes and reopens the optional section;
6. `ir_b2a -st -src` shows the stable tables, logical DSL operations, tensor
   descriptors, source positions, and plaintext parameter side-file evidence;
7. retained `.B` and `.T` artifacts are reported for human review.

No FHE/SIHE/CKKS opcode allocation, Python ingestion, ReLU conversion,
bootstrap insertion, OpenFHE call lowering, or CKKS value-state planning is
part of SYNC-1.
