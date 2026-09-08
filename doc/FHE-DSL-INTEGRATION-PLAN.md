# FHE DSL Integration Plan

## Status And Purpose

This document turns
`DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.9.docx` into an
implementation plan for Open64. It covers the complete first path from a
Python model with an FHE boundary to binary very-high-level WHIRL, reviewable
`ir_b2a` output, FHE conversion and CKKS planning, standard middle-WHIRL,
`whirl2c` C output, OpenFHE runtime binding, and a linked executable.

The first executable target is CKKS inference with ciphertext inputs and
ciphertext outputs, plaintext model parameters, and an OpenFHE CPU provider.
The first end-to-end model target is ResNet-20/CIFAR-10. Small deterministic
add, linear, and ReLU fixtures remain mandatory diagnostic unit tests, but they
are not earlier model milestones and do not gate capture of the complete
ResNet-20 graph.

This is a staged implementation, not a commitment to one packing or FHE
optimization algorithm. MetaKernel, Fhelipe, ReSBM, HPOLY, and GPU providers
must all fit behind the contracts defined here without changing the Python
frontend boundary or the original application-level `.B` file.

## Normative Decisions

1. Python is a source frontend. It does not own WHIRL table layout, WN layout,
   ELF sections, OpenFHE objects, or cryptographic key material.
2. The binary WHIRL artifact remains the frontend boundary. The compiler must
   be able to reopen it in a separate process without Python loaded.
3. Existing tensor `TY_IDX` and TensorDescriptorIR machinery remains the type
   carrier. Do not add a parallel `KIND_FHE`, expose a C++ OpenFHE type as a
   WHIRL type, or duplicate logical tensor shape and dtype.
4. EncryptionDescriptorIR is semantic representation state and participates
   in FHE tensor compatibility. Source names, diagnostics, pass ownership, and
   profiling remain compiler metadata and do not affect type equivalence.
5. The original model keeps common and CNN semantics until the FHE gatekeeper
   validates the encrypted boundary. FHE conversion then creates domain-visible
   FHE operators. It must not disguise unsupported CNN semantics as generic
   calls.
6. Every logical operator, record kind, enum value, and string name is
   append-only after publication. Physical `OPR_DSL` remains a private escape
   mechanism and must not appear in diagnostics or ASCII output.
7. No FHE, CKKS, or HPOLY DSL operator may reach unmodified `whirl2c`. The FHE
   lowering gate produces standard WHIRL, primarily `OPR_CALL`, `LDID`, `STID`,
   `PARM`, ordinary control flow, and static initializer records.
8. Generated C calls a versioned C ABI using opaque pointer-sized handles.
   OpenFHE C++ templates, smart pointers, exceptions, and object layouts remain
   confined to the OpenFHE provider.
9. The server executable may import a crypto context, public key, evaluation
   keys, and ciphertexts. It must never contain, import, generate, or request a
   secret key. Key generation and decryption belong to a separate client/test
   provisioning program.
10. The first linkable path is OpenFHE CPU/reference. GPU library and native
    POLY/RNS paths are later providers, not alternate frontend encodings.
11. ReLU is represented by the common-substrate operator `common.relu`. For
    the first CKKS path, every surviving `common.relu` is a mandatory refresh
    boundary at `-O0`: bootstrap first, then evaluate the approved polynomial
    ReLU approximation. Bootstrap restores ciphertext capacity; it does not
    itself implement ReLU.

## End To End Flow

```text
Python model plus @open64_dsc.fhe.entry
  -> torch.export or FX capture
  -> torch2whirl semantic census and legality checks
  -> opaque native DSL builder APIs
  -> application.B
       common and CNN operators
       TensorDescriptorIR
       FHEEntryContractIR
       FHECompilationConfigIR
       EncryptionDescriptorIR bindings
  -> ir_b2a -st -src application.B application.T
  -> FHE gatekeeper
  -> CNN-to-FHE conversion
  -> application.fhe.B
       cnn_fhe and fhe semantic operators
  -> encrypted layout and key-requirement planning
  -> SIHE and CKKS lowering with scale and level management
  -> application.ckks.B
  -> FHE runtime-call lowering
  -> application.mid.B
       standard WHIRL calls and C-compatible static descriptors only
  -> whirl2c
  -> application.c plus application.w2c.h
  -> C compilation
  -> C++ final link with libopen64_fhe_runtime and OpenFHE provider
  -> a.out
  -> import context, evaluation keys, and ciphertext input
  -> encrypted evaluation
  -> ciphertext output for client-side decryption
```

Each `.B` checkpoint uses the existing mapped-image and ELF WHIRL framework.
The compiler does not convert the in-memory WHIRL image into a separate object
stream format.

## Ownership By Subsystem

| Area | Owner |
| --- | --- |
| Python declaration, model capture, reachable PU discovery, source identity | `osprey/torch2whirl` |
| Stable domains, logical operators, fixed records, type bindings, builder API, mapped-image read/write, `ir_b2a` printing | `osprey/common/com` |
| FHE gatekeeper and target-independent semantic validation | `osprey/common/com` with domain verifier implementation |
| CNN-to-FHE, SIHE-to-CKKS, and runtime-call lowering | `osprey/be/vho` initially, behind dedicated FHE drivers |
| FHE optimization controls | `config_fhe.{h,cxx}`, following Open64 phase-option conventions |
| Standard WHIRL-to-C output | `osprey/be/whirl2c` |
| Stable C ABI and provider dispatch | new Open64 FHE runtime library |
| Mapping C ABI operations to OpenFHE | OpenFHE C++ provider library |
| Client key generation, encryption, and decryption | separate test/client harness, never the server compiler or generated server PU |

## DSL Domain And Operator Inventory

### Domain registration

Register these stable domains without changing existing domain IDs:

| Domain | Purpose |
| --- | --- |
| `fhe` | Encryption boundary and scheme-independent encrypted computation |
| `cnn_fhe` | CNN semantics after legal FHE adaptation |
| `ckks` | CKKS-specific scale, level, component, and bootstrap operations |
| `hpoly` | Whole-polynomial and extended-basis optimization operations |

The common and CNN domains remain unchanged. Repeated use of a common operator
with an encrypted descriptor is context sensitivity, not a new version of that
common operator.

`common.relu` is required in the common substrate because its source semantics
are shared across CNN, Transformer, and future neural-network domains. The CNN
and FHE gatekeepers must still validate domain legality before conversion. FHE
conversion preserves the logical `common.relu` evidence while attaching the
approximation and refresh contracts needed by CKKS lowering.

### Ingestion and boundary contracts

These records or contract operators are required before torch2whirl can claim
FHE ingestion:

| Stable name | Kind | Required semantics |
| --- | --- | --- |
| `fhe.entry.v1` | Region/PU contract | Scheme, encrypted input/output ordinals, parameter policy, trust boundary, config ID |
| `fhe.ciphertext_input.v1` | Declaration | Input ordinal, encrypted tensor type, external ciphertext contract |
| `fhe.ciphertext_output.v1` | Declaration | Result ordinal, client export layout, ciphertext requirement |
| `fhe.plaintext_parameter.v1` | Declaration | External parameter identity, encoding policy, checksum, no secret material |
| `fhe.approximation_contract.v1` | Contract | Function, polynomial degree, coefficients, valid range, error budget |
| `fhe.key_requirement.v1` | Contract | Public, rotation, relinearization, or bootstrap key requirement without key bytes |

`fhe.entry` should use the existing PU/REGION abstraction and declare input and
result values by ordinal. It is not an expression node. Ciphertext and
plaintext parameters remain values with normal source positions and symbols.

### FHE semantic operators

Allocate `DSL_OPERATOR` enum values only after rebasing onto current `develop`.
Append after the current maximum and never reuse or reorder an existing value.
The initial operator set is:

| Stable logical operator | Kids | Result | Static attributes |
| --- | ---: | --- | --- |
| `fhe.encode_plain.v1` | 1 | encoded plaintext tensor | encoding kind, target scale policy |
| `fhe.add_ct.v1` | 2 | ciphertext tensor | numeric policy |
| `fhe.add_plain.v1` | 2 | ciphertext tensor | numeric policy |
| `fhe.sub_ct.v1` | 2 | ciphertext tensor | numeric policy |
| `fhe.sub_plain.v1` | 2 | ciphertext tensor | numeric policy |
| `fhe.mul_plain.v1` | 2 | ciphertext tensor | rescale policy |
| `fhe.mul_ct.v1` | 2 | ciphertext tensor | relinearization and rescale policy |
| `fhe.rotate.v1` | 1 | ciphertext tensor | signed slot offset |
| `fhe.relinearize.v1` | 1 | ciphertext tensor | target component count |
| `fhe.rescale.v1` | 1 | ciphertext tensor | target scale/level policy |
| `fhe.mod_switch.v1` | 1 | ciphertext tensor | target level |
| `fhe.bootstrap.v1` | 1 | ciphertext tensor | bootstrap profile and result-level policy |
| `fhe.mask.v1` | 2 | ciphertext tensor | mask layout identity |
| `fhe.strided_slice.v1` | 1 | ciphertext tensor | begin, end, stride, gap semantics |
| `fhe.layout_convert.v1` | 1 | ciphertext tensor | source and destination encrypted-layout IDs |
| `fhe.poly_eval.v1` | 1 | ciphertext tensor | coefficient constant ID, degree, approximation contract ID |

`fhe.add_ct` and `fhe.mul_ct` names state operand classes explicitly. This
avoids making an untyped overload decision after the encryption descriptors
have been lost. `fhe.mul_ct` does not silently mean multiplication plus
relinearization; a fused policy is explicit and versioned.

### CNN FHE operators

The first CNN conversion layer requires:

| Stable logical operator | Purpose |
| --- | --- |
| `cnn_fhe.conv2d_plain_weight.v1` | Ciphertext activation with encoded plaintext weight and optional bias |
| `cnn_fhe.residual_add.v1` | Residual merge with layout, scale, and level compatibility obligations |
| `cnn_fhe.poly_activation.v1` | Reviewed polynomial realization of a preserved source `common.relu`, with approximation and refresh provenance |
| `cnn_fhe.average_pool.v1` | Linear pooling plus encoded plaintext scaling |
| `cnn_fhe.linear_plain_weight.v1` | Encrypted classifier or projection with plaintext parameters |
| `cnn_fhe.encrypted_logits.v1` | Ciphertext model result with client export contract |

Batch normalization is folded into plaintext convolution weights and bias when
legal. At `-O0`, `bootstrap=auto` or `bootstrap=on` inserts a bootstrap
immediately before every surviving `common.relu`; `bootstrap=manual` requires
an explicit boundary, and `bootstrap=off` rejects a surviving ReLU. Exact max
pooling, softmax, argmax, encrypted-data control flow, and server decryption are
rejected in the first release.

### CKKS and HPOLY operators

CKKS operators make per-value scheme state explicit: `ckks.add`, `ckks.sub`,
`ckks.mul`, `ckks.rotate`, `ckks.relinearize`, `ckks.rescale`,
`ckks.mod_switch`, and `ckks.bootstrap`. HPOLY later adds `hpoly.add`,
`hpoly.mul`, `hpoly.rotate`, `hpoly.rescale`, `hpoly.extend`,
`hpoly.modup`, `hpoly.dotprod`, `hpoly.moddown`, and reviewed extended-basis
variants.

These sets are not required to complete the first high-level OpenFHE-call MVP.
They are required before primitive-plan optimization claims. Their enum values
and attribute schemas must be published in separate append-only batches.

## FHE IR Image And Type Extensions

### Preferred image boundary

Add one optional fixed-row `.WHIRL.dsl_fhe` image using a new
`WT_DSL_FHE_IMAGE` extension identifier. This leaves the current DSL image,
effect image, callsite image, TY records, and PU trees unchanged. Legacy files
omit the section and load with an empty FHE image. Legacy readers continue to
ignore an unrecognized optional WHIRL section.

Do not place C++ pointers, `std::string`, `std::vector`, maps, OpenFHE objects,
or variable-size ownership inside mapped rows. Use fixed-width scalars,
`STR_IDX`, `TY_IDX`, `ST_IDX`, and first-record/count ranges. Every table has an
invalid zero ID and append-only enum values.

### Fixed record families

| Record | Key fields |
| --- | --- |
| `FHE_IMAGE_HEADER` | Magic, version, capabilities, count for every table |
| `FHE_COMPILATION_CONFIG_RECORD` | Scheme, security, ring dimension, depth policy, scale bits, first modulus bits, slots, key-switch and bootstrap policies |
| `FHE_ENTRY_CONTRACT_RECORD` | Owner PU, config ID, first boundary value, input/output counts, parameter policy, flags |
| `FHE_ENTRY_VALUE_RECORD` | Entry ID, ST/TY identity, ordinal, input/output/parameter role, value class, flags |
| `FHE_ENCRYPTION_DESCRIPTOR_RECORD` | Value class, scheme, config ID, CKKS state ID, encrypted-layout ID, key-set ID, flags |
| `FHE_TENSOR_BINDING_RECORD` | Canonical tensor `TY_IDX`, TensorDescriptorIR ID, EncryptionDescriptorIR ID |
| `FHE_APPROXIMATION_CONTRACT_RECORD` | Function name, degree, coefficient constant/value ID, valid range, error policy |
| `FHE_ENCRYPTED_LAYOUT_RECORD` | Ciphertext count, active slots, slot-map identity, gap/validity identity, planner and layout flags |
| `FHE_CKKS_VALUE_STATE_RECORD` | Level, scale bits, basis, component count, precision estimate, relinearization obligation |
| `FHE_KEY_REQUIREMENT_RECORD` | Key class, key-set ID, config ID, first rotation ID/count, flags |
| `FHE_ROTATION_REQUIREMENT_RECORD` | Key requirement ID, signed rotation offset, flags |
| `FHE_BACKEND_REQUIREMENT_RECORD` | Provider ABI version, required capabilities, target class, memory policy |

The initial image can publish only config, entry, entry-value, encryption
descriptor, tensor binding, and key requirement tables. Later rows are added
through image capability bits and a reviewed version rule. Reader bounds checks
must use recorded sizes and reject malformed first/count ranges before exposing
records to compiler passes.

### Deduplication and type identity

1. Intern `FHE_COMPILATION_CONFIG_RECORD` by all semantic configuration fields.
2. Intern `FHE_ENCRYPTION_DESCRIPTOR_RECORD` by value class, scheme, config,
   state, layout, and key-set identity.
3. Intern an FHE tensor type by the pair of canonical TensorDescriptorIR and
   canonical EncryptionDescriptorIR identities.
4. Do not include source location, source symbol name, diagnostics, lineage,
   profile data, or selected backend implementation in semantic equivalence.
5. A ciphertext type is never equivalent to encoded plaintext or clear tensor
   type even when dtype and logical shape match.
6. CKKS state changes create a new value descriptor association; they do not
   mutate a shared canonical type record behind existing values.

This typed association replaces free-form FHE key/value lookup on legality
hot paths. A compatibility metadata spelling may be accepted temporarily, but
the builder redirects it into these records and the printer labels it as
compatibility input.

### Effects and ownership

Ciphertext and plaintext results are opaque owning handles stored in no-alias
temporaries. Runtime context, key set, ciphertext import/export channel, and
provider status are explicit DSL state objects. Operations declare READ or
MODIFY effects through the existing effect image; do not encode them as string
metadata. The first OpenFHE provider uses functional result handles even when
OpenFHE offers an in-place operation.

## Native Builder API

Add declarations in a focused `dsl_fhe.h` and implementation in
`dsl_fhe.cxx`, with only opaque handles exposed to torch2whirl. Proposed C++
entry points are:

```c++
typedef UINT32 DSL_FHE_CONFIG_ID;
typedef UINT32 DSL_FHE_ENTRY_CONTRACT_ID;
typedef UINT32 DSL_FHE_ENCRYPTION_DESCRIPTOR_ID;
typedef UINT32 DSL_FHE_KEY_REQUIREMENT_ID;

DSL_FHE_CONFIG_ID
DSL_FHE_Intern_Compilation_Config(
    const DSL_FHE_COMPILATION_CONFIG *config);

DSL_FHE_ENCRYPTION_DESCRIPTOR_ID
DSL_FHE_Intern_Encryption_Descriptor(
    const DSL_FHE_ENCRYPTION_DESCRIPTOR *descriptor);

TY_IDX
DSL_Builder_Intern_FHE_Tensor_Type(
    const char *name,
    const DSL_BUILDER_TENSOR_DESCRIPTOR *tensor,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption);

DSL_FHE_ENTRY_CONTRACT_ID
DSL_Builder_Attach_FHE_Entry_Contract(
    DSL_BUILDER_PROGRAM_UNIT pu,
    const DSL_FHE_ENTRY_CONTRACT *contract);

BOOL
DSL_Builder_Declare_FHE_Entry_Value(
    DSL_FHE_ENTRY_CONTRACT_ID entry,
    DSL_BUILDER_VALUE value,
    UINT32 ordinal,
    DSL_FHE_ENTRY_VALUE_ROLE role);

DSL_FHE_KEY_REQUIREMENT_ID
DSL_Builder_Add_FHE_Key_Requirement(
    DSL_FHE_CONFIG_ID config,
    const DSL_FHE_KEY_REQUIREMENT *requirement);

BOOL
DSL_Builder_Attach_FHE_Approximation_Contract(
    DSL_BUILDER_VALUE value,
    const DSL_FHE_APPROXIMATION_CONTRACT *contract);

BOOL
DSL_Builder_Get_FHE_Encryption_Descriptor(
    DSL_BUILDER_VALUE value,
    DSL_FHE_ENCRYPTION_DESCRIPTOR *descriptor);
```

Use the existing `DSL_Builder_Create_Operator_With_Result` for ordinary FHE
expressions after the stable opcode is found. Add an FHE convenience wrapper
only if it performs real validation or descriptor propagation; do not create a
parallel WN constructor.

Every symbol-producing API receives or derives a
`DSL_BUILDER_SOURCE_POSITION` and calls `Set_ST_Srcpos()`. Every operator value
receives its FX/source position. Python never receives `TY_IDX`, `ST_IDX`, WN
fields, physical opcode tags, or table offsets as inspectable values.

## Torch2whirl Ingestion Plan

### Python surface

Add an importable package surface with no OpenFHE dependency:

```python
from open64_dsc.fhe import CipherTensor, entry

class SecureModel(torch.nn.Module):
    @entry(
        scheme="ckks",
        encrypted_inputs=("x",),
        encrypted_outputs=("return",),
        parameter_policy="plaintext",
        security="128-classic",
    )
    def forward(self, x: CipherTensor[8]) -> CipherTensor[8]:
        return (x + self.bias) * self.scale
```

The decorator records source intent. It does not instantiate OpenFHE,
generate keys, encrypt data, or execute cryptography during capture.

### Frontend actions

1. Discover the decorated reachable PU and preserve class-centric PU identity.
2. Resolve declaration precedence: per-value declaration, decorator, export
   options, then driver defaults.
3. Classify placeholders, parameters, buffers, constants, and returns as
   ciphertext, encoded plaintext, clear metadata, or illegal secret material.
4. Build canonical tensor descriptors before creating FHE bindings.
5. Intern one module/program FHE configuration and attach one entry contract.
6. Emit ordinary common/CNN operators for the source model, preserving source
   module and region structure.
7. Attach typed encryption descriptors to values and declare boundary ordinals.
8. Preserve external plaintext parameters using the existing side-file tensor
   reference contract plus encoding policy; do not place large weights in
   Python-owned compiler metadata.
9. Reject decrypt, secret-key access, training mutation, encrypted-data Python
   branching, unknown operators, and dynamic encrypted shapes not supported by
   the selected profile.
10. Run the native gatekeeper before finalizing the mapped image.

### Native bridge additions

Bind config creation, encryption descriptor interning, FHE tensor interning,
entry contract attachment, entry-value declaration, key requirement creation,
and approximation contract attachment. Python-facing objects remain opaque
capsules/handles. Capability probes must let an older native extension reject
FHE ingestion with one precise diagnostic rather than silently dropping the
contracts.

The frontend plan and tests belong in
`osprey/torch2whirl/FHE-INGESTION-PLAN.md`. That plan owns fixtures, capture,
operator census, Python diagnostics, and frontend artifacts. This document and
`WHIRL-DSL-INFRASTRUCTURE.md` own native contracts and binary behavior.

## Gatekeeper And Conversion

### Ingestion gate

Before the original `.B` is accepted, verify:

- exactly one compatible config for each FHE entry;
- every encrypted input and output ordinal is present and unique;
- every value has complete tensor and encryption descriptors;
- parameter policy is plaintext/encoded plaintext for the first release;
- no secret-key value, decrypt operation, plaintext escape, or server-side
  key-generation request exists;
- every common/CNN operator has a defined FHE propagation rule or rejection;
- source positions and PU ownership are valid; and
- all side-file references have stable names, bounds, and checksums.

### CNN-to-FHE conversion

Add a dedicated `VHO_FHE_Convert_Driver()` after the ordinary DSL gatekeeper
and before FHE layout/CKKS planning. It performs a fixed, option-controlled
pipeline:

1. inference-only and encrypted-boundary validation;
2. BatchNorm folding into plaintext weight/bias where legal;
3. nonlinear activation replacement using a reviewed approximation contract;
4. encryption-state propagation;
5. conversion of supported CNN/common expressions into `cnn_fhe` operators;
6. residual shape and eventual layout/scale/level obligation creation;
7. slot-validity, mask, gap, and cleanup canonicalization; and
8. a second gatekeeper before layout planning.

At `-O0`, conversion performs only mandatory semantic adaptation and
deterministic legality work. Fusion, global packing search, ReSBM, and HPOLY
profitability transforms remain off unless their own phase option enables them.

## Runtime Call Lowering And Whirl2c

### FHE lowering gate

Add `VHO_FHE_Lower_Driver()` after the selected semantic/CKKS pipeline and
before ordinary backend or `whirl2c` processing. For the library-call MVP it:

1. maps each FHE result to a distinct no-alias opaque handle temporary;
2. materializes C-compatible static descriptor objects through normal WHIRL
   symbol and initializer tables;
3. converts operands to `OPR_PARM` nodes with explicit pointer/scalar types;
4. emits versioned `OPR_CALL` nodes to the stable C ABI;
5. preserves source positions on generated calls and result stores;
6. emits explicit status checks where the ABI returns status separately;
7. preserves context/key state effects and handle lifetime;
8. rejects any unmapped operator; and
9. verifies that no DSL/FHE/CKKS/HPOLY node remains in `application.mid.B`.

### Whirl2c work

The first implementation does not teach `whirl2c` FHE semantics. It extends
the normal C emission boundary only where needed:

- emit or include `open64_fhe_runtime_abi.h` when an FHE runtime call is
  present;
- render opaque context, ciphertext, plaintext, model, and status handles as
  C-compatible incomplete-struct pointers or typedefs;
- emit standard static descriptor initializers without C++ syntax;
- preserve stable function, variable, and source identities;
- diagnose an unlowered DSL node instead of printing malformed C; and
- keep generated C independent of OpenFHE headers and namespaces.

The acceptance gate is that `whirl2c application.mid.B` produces C accepted by
the selected C compiler, and the final C++ link succeeds without modifying the
generated C by hand.

## Stable FHE C ABI

Create a public header such as `osprey/include/open64_fhe_runtime_abi.h`.
Published structures begin with `abi_version` and `struct_size`; fields are
append-only. All functions return a stable status code or document an
unambiguous null-handle failure path.

The first ABI families are:

| Family | Initial calls |
| --- | --- |
| Lifecycle | `open64_fhe_context_create_ckks_v1`, `open64_fhe_context_destroy_v1`, `open64_fhe_synchronize_v1` |
| Import/export | `open64_fhe_context_import_v1`, `open64_fhe_keyset_import_v1`, `open64_fhe_ciphertext_import_v1`, `open64_fhe_ciphertext_export_v1` |
| Plaintext | `open64_fhe_plaintext_encode_v1`, `open64_fhe_plaintext_import_v1` |
| Lifetime | `open64_fhe_ciphertext_retain_v1`, `open64_fhe_ciphertext_release_v1`, `open64_fhe_plaintext_release_v1` |
| Arithmetic | `open64_fhe_add_ct_v1`, `open64_fhe_add_plain_v1`, `open64_fhe_sub_ct_v1`, `open64_fhe_sub_plain_v1`, `open64_fhe_mul_plain_v1`, `open64_fhe_mul_ct_v1` |
| Scheme operations | `open64_fhe_rotate_v1`, `open64_fhe_relinearize_v1`, `open64_fhe_rescale_v1`, `open64_fhe_mod_switch_v1`, `open64_fhe_bootstrap_v1` |
| High-level MVP | `open64_fhe_poly_eval_v1`, `open64_fhe_conv2d_plain_v1`, `open64_fhe_linear_plain_v1`, `open64_fhe_average_pool_v1`, `open64_fhe_layout_convert_v1` |
| Inspection | `open64_fhe_get_level_v1`, `open64_fhe_get_scale_v1`, `open64_fhe_get_size_bytes_v1` |
| Diagnostics | `open64_fhe_get_last_status_v1`, `open64_fhe_get_last_error_v1`, optional diagnostic callback |

The runtime owns provider selection and exception containment. No C++
exception crosses the C ABI. Every caught OpenFHE exception becomes a stable
status plus a retained diagnostic containing operation, source identity,
config ID, and backend detail without exposing secret values.

## OpenFHE Provider Mapping

### Provider boundary

Implement `libopen64_fhe_openfhe.so` in C++. It owns all
`lbcrypto::CryptoContext<DCRTPoly>`, `Ciphertext<DCRTPoly>`, `Plaintext`, key,
and serialization objects. The public runtime and generated C see only opaque
handles and fixed C descriptors.

Pin and record the tested OpenFHE release, build options, native integer size,
math backend, compiler ABI, shared/static choice, and transitive libraries. Do
not build against an unrecorded moving `main` branch for certification.

### Initial API map

| Open64 runtime operation | OpenFHE service or adapter behavior |
| --- | --- |
| CKKS context creation | Build `CCParams<CryptoContextCKKSRNS>`, apply resolved setters such as multiplicative depth, scale modulus size, batch size, ring dimension and security policy, then call `GenCryptoContext` |
| Feature enablement | `Enable(PKE)`, `Enable(KEYSWITCH)`, `Enable(LEVELEDSHE)`, and only enable `ADVANCEDSHE`/`FHE` when the plan requires them |
| Context/ciphertext import and export | OpenFHE binary `Serial::DeserializeFromFile` and `Serial::SerializeToFile` behind checked runtime paths |
| Evaluation-key import | OpenFHE evaluation-multiplication and automorphism-key import services; bootstrap key material when required |
| Plaintext encoding | `MakeCKKSPackedPlaintext` using compiler-resolved slots, scale, and level |
| Ciphertext addition/subtraction | `EvalAdd` and `EvalSub` after compiler/runtime descriptor checks |
| Ciphertext-by-plaintext multiplication | `EvalMult(ciphertext, plaintext)` with explicit state validation |
| Ciphertext multiplication | `EvalMult`; explicit `Relinearize` and `Rescale` remain separate unless a reviewed fused ABI call is selected |
| Rotation | `EvalRotate`; all signed offsets must appear in the key-requirement manifest |
| Relinearization | `Relinearize` or the matching non-mutating API |
| Rescale | `Rescale`, with output state checked against CKKSValueStateIR |
| Modulus/level change | Map only after proving the exact OpenFHE `LevelReduce`/scheme API semantics; do not equate CKKS `ModReduce` with a scale-preserving mod-switch by name alone |
| Bootstrap | Setup/profile validation plus `EvalBootstrap` after compatible bootstrap keys are loaded |
| Polynomial activation | `EvalPoly` for the reviewed coefficient and range contract |
| Linear/conv/pool | Adapter-controlled sequences of rotate, multiply-plaintext, add, and scale operations; no claim that OpenFHE has one matching CNN API |

OpenFHE `KeyGen`, `EvalMultKeyGen`, `EvalRotateKeyGen`, and
`EvalBootstrapKeyGen` are used only by the client/test provisioning utility.
They are prohibited in the compiled server path because they require secret
key access.

## Driver, Build, And Link Flow

The target user command is conceptually:

```text
openpy -O0 -keep secure_model.py \
  -dsc-fhe=cnn \
  -dsc-fhe-scheme=ckks \
  -dsc-fhe-backend=openfhe \
  -dsc-fhe-codegen=whirl2c \
  -o a.out
```

Driver control becomes:

```text
openpy
  -> torch2whirl -> secure_model.B
  -> FHE gatekeeper and conversion -> secure_model.fhe.B
  -> CKKS/library-call planning -> secure_model.ckks.B
  -> FHE runtime-call lowering -> secure_model.mid.B
  -> whirl2c -> secure_model.c and secure_model.w2c.h
  -> C compiler -> secure_model.o
  -> C++ linker driver
       secure_model.o
       libopen64_fhe_runtime.so
       libopen64_fhe_openfhe.so
       OpenFHE shared libraries and required system libraries
  -> a.out
```

Build the OpenFHE provider with OpenFHE's installed CMake package configuration
and capture its `OpenFHE_CXX_FLAGS`, include path, library path, shared-library
set, and executable linker flags into an installed Open64 provider manifest or
linker response file. The Open64 driver consumes that generated configuration;
it does not guess library names or scrape command output.

The adapter, not generated C, links to OpenFHE. `a.out` must have a complete
runtime dependency graph visible through normal platform inspection. Missing
provider, incompatible OpenFHE ABI, missing context/key files, or unsupported
scheme capability is a driver/runtime diagnostic, not an unresolved symbol or
Python error.

All user options continue through the phase pipeline. Each phase consumes its
own `-dsc-fhe-*` options and silently ignores options owned elsewhere. `-keep`
retains every checkpoint and command line needed to reproduce the link.

## Ir_b2a Inspection Contract

`ir_b2a -st -src` must expose stable logical evidence without private physical
encoding details. Add these named sections when records exist:

```text
FHE Compilation Configurations
FHE Entry Contracts
FHE Entry Values
FHE Encryption Descriptors
FHE Tensor Bindings
FHE Approximation Contracts
FHE Encrypted Layouts
FHE CKKS Value States
FHE Key Requirements
FHE Backend Requirements
```

For each DSL expression, print the logical operator name such as
`fhe.mul_plain.v1`, direct kids, result symbol/TY, static attributes, source
position, encryption descriptor ID, and CKKS state ID when applicable. For
external plaintext weights and encrypted artifacts, print side-file name,
logical key, offset, length, and checksum in the symbol-related dump.

The printer must redact or reject secret-key paths and bytes. Unknown record
versions print a precise unsupported-version diagnostic. A file without FHE
records produces no empty FHE section and retains baseline output.

Required traces are:

- `secure_model.T` for the original application-level `.B`;
- `secure_model.fhe.T` after CNN-to-FHE conversion;
- `secure_model.ckks.T` after scheme planning; and
- `secure_model.mid.T` proving that only standard WHIRL runtime calls remain.

## Binary Compatibility

1. Do not renumber existing DSL operators, domains, effect kinds, state kinds,
   record kinds, ELF section identifiers, or enum values.
2. Add `WT_DSL_FHE_IMAGE` as an optional section with its own magic, version,
   header size, record sizes, capability mask, bounds validation, and reset.
3. Add reader, writer, printer, mapped-image tests, and malformed-image tests in
   the same change as the section definition.
4. Old `.B` files load with an empty FHE image. New non-FHE `.B` files remain
   byte-contract compatible and do not emit the optional section.
5. Keep compatibility comment projections for high-level review until their
   retirement is separately approved, but never use them as the executable
   carrier.
6. Decide the WHIRL revision change only after reviewing whether the optional
   section is an incompatible change. Document the reader matrix either way.
7. `ir_a2b` support must be specified before claiming an editable ASCII
   roundtrip. Until then, `ir_b2a` is inspection output, not a promise that all
   FHE tables can be reconstructed from free-form text.

## Validation And Retained Artifacts

### Test ladder

| Stage | Test | Exit evidence |
| --- | --- | --- |
| F0 | Record and enum unit tests | Append-only IDs, initialization, deduplication, malformed-row rejection |
| F1 | Native builder tests | Focused add, linear, and `common.relu` values with FHE entry, encrypted input, plaintext parameters, and source positions |
| F2 | Binary image test | Write, reopen through mapped image, verify all IDs and cross-references |
| F3 | `ir_b2a` test | Stable FHE tables, symbols/types, operators, source lines, side-file references |
| F4 | torch2whirl ResNet-20 smoke | Decorated full model produces the certified original `.B` in a separate process |
| F5 | Conversion test | Full common/CNN graph converts without lost source, type, region, residual, or class-centric PU state |
| F6 | Mock runtime path | Full ResNet-20 runtime-call lowering, `whirl2c`, C compile, link, and opaque-handle execution |
| F7 | ReLU correctness test | Every source `common.relu` has a pre-ReLU bootstrap and approved polynomial activation at `-O0` |
| F8 | CKKS operator diagnostics | Add, linear, convolution, residual, pooling, and ReLU kernels satisfy focused numerical and state checks |
| F9 | ResNet-20 structural gate | Residual regions, layouts, scales, levels, rotation keys, ownership joins, and encrypted logits pass |
| F10 | ResNet-20 encrypted execution | End-to-end inference reports accuracy, CKKS error, depth, keys, memory, runtime, and retained artifacts |

### First model fixture

Use a deterministic inference-only ResNet-20/CIFAR-10 source model with
ciphertext input, ciphertext logits, and plaintext parameters. Preserve the
complete graph, including convolution, folded batch normalization, residual
regions, `common.residual_add`, `common.relu`, pooling, flatten, and classifier.

The add, linear, and ReLU fixtures remain focused tests for isolating builder,
type, conversion, and runtime failures. In particular, the ReLU fixture must
show this `-O0` logical sequence in `ir_b2a -st -src` evidence:

```text
common.relu(x)
  -> sihe.bootstrap(x, reason=relu_boundary)
  -> cnn_fhe.poly_activation(..., source_op=common.relu)
```

A provider may fuse bootstrap and polynomial evaluation, but the logical trace,
source position, approximation contract, bootstrap reason, and resulting CKKS
state must remain inspectable.

### Artifact family

```text
artifacts/fhe/resnet20_openfhe/
  secure_resnet20.py
  secure_resnet20.B
  secure_resnet20.T
  secure_resnet20.fhe.B
  secure_resnet20.fhe.T
  secure_resnet20.ckks.B
  secure_resnet20.ckks.T
  secure_resnet20.mid.B
  secure_resnet20.mid.T
  secure_resnet20.c
  secure_resnet20.w2c.h
  secure_resnet20.o
  a.out
  compile.options.txt
  whirl2c.log
  c_compile.log
  link.log
  runtime.log
  fhe_config.txt
  key_requirements.txt
  openfhe_build_manifest.txt
  client_context.bin
  client_public_key.bin
  server_evaluation_keys.bin
  input_ciphertext.bin
  output_ciphertext.bin
  validation.txt
```

The test-only secret key is kept in a separate access-controlled client
directory, never copied into the compiler artifact family, never mounted into
the server run, and never committed to Git. Docker tests bind-mount the artifact
directory and clean it at the start of the next run rather than at completion.

Validation compares the decrypted result with a high-precision clear
reference using the CKKS tolerance and reports maximum/mean absolute and
relative error, precision estimate, level, scale, ciphertext size, and runtime.
CKKS is approximate; bit-exact floating-point equality is not an acceptance
criterion.

## Implementation Milestones

### M0 Contract freeze

- Rebase onto current `develop` and inventory the latest DSL enum/table IDs.
- Review the stable names, operand classes, descriptor equivalence, C ABI, and
  secret-key boundary.
- Add this plan to `WHIRL-DSL-INFRASTRUCTURE.md` and create the separate
  torch2whirl FHE ingestion plan.

Exit: reviewers approve the original ResNet-20 `.B` contract, the
`common.relu` promotion, and the mandatory `-O0` ReLU bootstrap policy.

### M1 FHE image and type association

- Add fixed rows, registries, deduplication, reset, validation, optional ELF
  section, reader, and writer.
- Add canonical tensor-to-encryption descriptor association.
- Add unit and malformed-image tests.

Exit: a native producer writes and reopens FHE records with no WN behavior
change and no regression for old `.B` files.

### M2 Logical operators, builder, gatekeeper, and printer

- Register `fhe` operators and contracts append-only.
- Add opaque native builder APIs and source-position handling.
- Add boundary/security/type gatekeeper rules.
- Print all FHE tables and logical operators through `ir_b2a -st -src`.

Exit: focused native add, linear, and ReLU producers have reviewable `.B` and
`.T` artifacts, including logical ReLU/bootstrap evidence.

### M3 Torch2whirl ingestion

- Add decorator/type surface and native bridge bindings.
- Add deterministic fixture, capture census, capability checks, diagnostics,
  and process-boundary artifact test.
- Preserve the ordinary non-FHE frontend path unchanged.

Exit: `torch2whirl secure_resnet20.py --fhe ...` emits the complete certified
ResNet-20 source contract without exposing WHIRL internals to Python.

### M4 Conversion and mock executable

- Add `VHO_FHE_Convert_Driver`, `VHO_FHE_Lower_Driver`, and phase controls.
- Define the C ABI and implement a mock provider.
- Add the `whirl2c` header/prelude and unlowered-node gate.
- Compile and link the generated full-model C into a mock `a.out`.

Exit: all ResNet-20 operators traverse the entire compiler and mock executable
through opaque encrypted handles without OpenFHE installed.

### M5 OpenFHE provider and real a.out

- Pin an OpenFHE release and generate its provider/link manifest from the
  installed CMake package.
- Implement context/config mapping, imports, encode, add, mul_plain,
  bootstrap, polynomial evaluation, export, lifetime, and exception/status
  translation. Use focused operator fixtures to diagnose provider failures.
- Add the separate client provisioning and decryption harness.
- Complete driver link, RUNPATH, missing-provider diagnostics, and artifact
  preservation.

Exit: the focused operator fixtures execute through OpenFHE and establish the
runtime primitives needed by the complete ResNet-20 path.

### M6 Primitive CKKS path

- Add rotate, ct multiplication, relinearize, rescale, mod-switch review, key
  manifests, and scale/level propagation.
- At `-O0`, insert mandatory bootstrap before every surviving `common.relu`
  under auto/on policy, require explicit boundaries under manual policy, and
  reject surviving ReLU under off policy.

Exit: primitive-plan results match the high-level OpenFHE-call baseline, and
every ReLU boundary has stable source-linked inspection evidence.

### M7 CNN and ResNet path

- Add BatchNorm folding, polynomial activation, convolution/linear plaintext
  weights, average pooling, residual joins, encrypted logits, and packing.
- Certify the complete ResNet-20 model directly. Focused operator tests remain
  diagnostic aids; no micro-block or reduced CNN is a prerequisite model.

Exit: `openpy -O0 -keep ... -o a.out` creates a server executable that runs
without Python or a secret key; encrypted ResNet-20 inference satisfies
accuracy, security, layout, key, depth, memory, and artifact-review gates.

## Immediate Action Queue

1. Approve the full ResNet-20 source contract, stable operator names,
   `common.relu` semantics, and the mandatory `-O0` bootstrap policy.
2. Decide and reserve the optional `WT_DSL_FHE_IMAGE` identifier.
3. Define fixed row layouts and compile-time size assertions.
4. Publish descriptor interning and tensor-equivalence rules.
5. Implement M1 without adding frontend code.
6. Implement native M2 producer and retain its `.B`/`.T` evidence.
7. Hand exact APIs and expected trace evidence to the torch2whirl task for M3.
8. Freeze `open64_fhe_runtime_abi.h` v1 and implement the mock provider.
9. Add FHE conversion/runtime-call lowering and the unlowered-node verifier.
10. Add `whirl2c` C-header emission and compile/link smoke test.
11. Build the OpenFHE provider and client provisioning harness.
12. Run focused OpenFHE operator certification, then complete the direct
    ResNet-20 `-O0` process-boundary certification.

## Deferred Work

- MetaKernel and Fhelipe planner implementation and A/B comparison;
- FHEFusion graph search beyond mandatory canonicalization;
- ReSBM global scale/bootstrap placement;
- HPOLY/HPAO and optional native POLY/RNS lowering;
- FIDESlib, Cheddar-like, or native NVIDIA GPU providers;
- encrypted model weights, training, dynamic encrypted control flow;
- TFHE/BGV/BFV scheme domains; and
- production key service, multi-tenant isolation, and remote execution protocol.

## References

- `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.7.docx`
- `doc/WHIRL-DSL-INFRASTRUCTURE.md`
- `doc/WHIRL-DSL-TENSOR-TYPE-HANDLING.md`
- `doc/Open64_Python_FE_Plan.md`
- [OpenFHE development repository](https://github.com/openfheorg/openfhe-development)
- [OpenFHE simple CKKS real-number example](https://github.com/openfheorg/openfhe-development/blob/main/src/pke/examples/simple-real-numbers.cpp)
- [OpenFHE CKKS serialization example](https://github.com/openfheorg/openfhe-development/blob/main/src/pke/examples/simple-real-numbers-serial.cpp)
- [OpenFHE CKKS bootstrapping guide](https://github.com/openfheorg/openfhe-development/blob/main/src/pke/examples/CKKS_BOOTSTRAPPING.md)
- [OpenFHE user CMake example](https://github.com/openfheorg/openfhe-development/blob/main/CMakeLists.User.txt)
