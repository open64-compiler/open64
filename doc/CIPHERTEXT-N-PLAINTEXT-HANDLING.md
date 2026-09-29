# Ciphertext N Plaintext Handling

## Purpose

This note explains how Open64 represents and processes ciphertext inputs and
outputs versus plaintext model weights and biases in the first FHE ResNet-20
path. It records the distinction between a compiler value's semantic FHE
classification and the runtime operation that creates an OpenFHE ciphertext or
CKKS plaintext object.

The initial execution model is:

- the private ResNet input is ciphertext;
- the ResNet output logits are ciphertext;
- model weights and biases are clear data that become CKKS encoded plaintext;
- the server program never receives a secret key; and
- client-side provisioning owns encryption and decryption.

## Type-System Principle

Open64 does not replace a tensor's canonical `TY_IDX` with an OpenFHE C++ type
or a separate ciphertext type kind.

The canonical tensor type continues to describe facts such as:

- element dtype;
- rank and logical shape;
- layout and representation requirements; and
- other TensorDescriptorIR semantics.

FHE representation is attached separately through EncryptionDescriptorIR.
This descriptor identifies whether a value is ciphertext, encoded plaintext,
clear data, or another reviewed FHE value class. A CKKS state change creates a
new value-state association; it does not mutate a shared canonical tensor type.

This separation preserves tensor-type interning and allows the same logical
tensor shape to participate in different representation contexts without
creating an unrelated OpenFHE type universe.

## Ciphertext Input And Output

### Compiler declaration

During frontend capture, torch2whirl creates a ciphertext encryption descriptor
and attaches it to the ResNet model input and the `common.output_logits`
result. This records the encrypted boundary; it does not execute cryptography.

Conceptually:

```text
model input TY_IDX
  + EncryptionDescriptorIR(value_class=ciphertext, scheme=CKKS, ...)

output logits TY_IDX
  + EncryptionDescriptorIR(value_class=ciphertext, scheme=CKKS, ...)
```

The builder also records the input/output ordinal and entry-contract role. The
FHE gatekeeper verifies that every encrypted input and output has a complete,
compatible tensor descriptor and CKKS encryption descriptor.

### Compilation stages

| Stage | Ciphertext handling |
| --- | --- |
| SYNC-2 frontend capture | Declare input and output values as ciphertext and preserve their canonical tensor types. |
| SYNC-3 FHE conversion | Validate and propagate ciphertext semantics through the ResNet computation. No data is encrypted. |
| SYNC-4 materialization | Materialize required bootstrap and polynomial-ReLU operations plus context-specific CKKS planning state. Values remain abstract ciphertext values in WHIRL. |
| SYNC-5 runtime-call lowering | Lower the encrypted boundary and computation to standard WHIRL calls using opaque runtime ciphertext handles. |
| SYNC-6 OpenFHE execution | Import real ciphertext envelopes, execute through OpenFHE, and export ciphertext logits. |

### Actual encryption and decryption

Actual encryption is not performed by torch2whirl, VHO, the generated server
PU, or the compiler. It belongs to the separate trusted client/provisioning
program:

```text
client plaintext image
  -> client encrypts with the secret key
  -> serialized CKKS ciphertext envelope
  -> generated ResNet server
  -> open64_fhe_ciphertext_import_v1
  -> encrypted ResNet evaluation
  -> open64_fhe_ciphertext_export_v1
  -> serialized ciphertext logits
  -> client decrypts with the secret key
```

The generated server may import the public context, public/evaluation keys,
and ciphertext input. It must not contain, import, generate, or request the
secret key.

The output does not begin as a clear tensor that the server subsequently
encrypts. It is produced by ciphertext operations and remains ciphertext
through `open64_fhe_ciphertext_export_v1`. Only the client decrypts it.

## Plaintext Weight And Bias Handling

### Source and captured representation

PyTorch weights and biases begin as ordinary floating-point tensors. The
frontend stores their bytes as external tensor constants in the model's
SafeTensors side file and records:

- canonical tensor type and descriptor;
- side-file name and tensor key;
- byte offset and length;
- checksum;
- source position and parameter identity; and
- FHE value class `encoded_plaintext`.

The `encoded_plaintext` declaration is an intended runtime representation. It
does not mean torch2whirl has already constructed an OpenFHE plaintext object.
The side-file bytes remain ordinary floating-point tensor data.

The first ResNet implementation intentionally does not encrypt model weights
or biases. They are visible to the server runtime under the selected private-
input/public-model threat model.

### BatchNorm folding in SYNC-3

Before runtime lowering, FHE conversion folds legal inference BatchNorm
operations into convolution weights and biases. For each output channel:

```text
factor = gamma / sqrt(variance + epsilon)

folded_weight = original_weight * factor

folded_bias = beta + (original_bias - mean) * factor
```

An implicit-zero convolution bias is treated as a typed zero tensor during the
calculation. The converter produces new immutable external tensors for the
folded weight and folded bias, preserves their source lineage, and retires the
executable BatchNorm operation.

The converted tensors are stored in the converted SafeTensors family, such as
`secure_resnet20.fhe.safetensors`. Their WHIRL evidence includes exact tensor
type, key, byte range, checksum, source value, owner PU, and call-context
identity.

The certified ResNet profile contains:

- 13 physical Conv/BatchNorm definitions;
- 21 invocation contexts; and
- 42 folded tensors: one folded weight and one folded bias per context.

BatchNorm therefore has no runtime call in the first path.

### SYNC-5 standard-WHIRL and runtime boundary

SYNC-5 lowers every required plain tensor into ordinary WHIRL calls that use
the stable FHE C ABI. A generated host sequence conceptually performs:

```c
open64_fhe_plain_tensor_v1_t weight;
open64_fhe_plain_tensor_v1_t bias;

status = open64_fhe_plain_tensor_import_v1(
    model, weight_envelope, weight_envelope_size, &weight);

status = open64_fhe_plain_tensor_import_v1(
    model, bias_envelope, bias_envelope_size, &bias);
```

The imported handles are explicit operands of the generated evaluation call:

```c
status = open64_fhe_conv2d_plain_v1(
    model,
    encrypted_input,
    weight,
    bias,
    &operation_descriptor,
    &encrypted_result);
```

Convolution and linear weights and biases are not implicit provider state.
Their handles are supplied explicitly at each evaluation call and must match
the identities and roles in the authenticated model and weight manifests.

Other descriptor assets, such as input ranges, normalization constants,
reconstruction constants, pool scales, slot permutations, and masks, may be
bound to the model through the separately reviewed asset-binding lifecycle.
Weights, biases, and polynomial coefficients remain explicit call operands.

### Plain-data to CKKS-plaintext conversion

The real transformation from floating-point tensor bytes into a
provider-owned CKKS plaintext object occurs in the SYNC-6 provider. It has two
distinct parts:

```text
ordinary tensor data
  -> compiler-selected tensor-to-slot transformation
  -> CKKS mathematical encoding
  -> provider-owned encoded plaintext polynomial
```

This operation is encoding, not encryption. It uses no public or secret key.

#### Tensor-to-slot transformation

The compiler first determines how tensor elements occupy CKKS SIMD slots. For
a weight or bias tensor, the selected layout plan may:

- flatten or reorder tensor dimensions;
- transform OIHW convolution weights into the selected kernel form;
- duplicate values that will be consumed after ciphertext rotations;
- diagonalize matrix or convolution data;
- pad unused slots with zero;
- generate masks for invalid slots; or
- split one tensor across several plaintext vectors when it exceeds the slot
  capacity.

MetaKernel or Fhelipe owns this planning decision in the optimized path. The
CKKS encoder receives already ordered vectors and does not independently
choose the global tensor layout.

For ring dimension `N`, one packed vector has at most `N/2` complex CKKS slots:

```text
z = [z0, z1, ..., zS-1], where S <= N/2
```

#### CKKS encoding algorithm

For each packed vector, the CKKS encoder conceptually performs these steps:

1. Select the ring dimension, active RNS moduli, CKKS level, slot count, and
   scale, such as `Delta = 2^56`.
2. Apply the inverse canonical embedding, implemented by an inverse FFT-like
   transform, to map slot values to polynomial coefficients.
3. Multiply the approximate coefficients by `Delta`.
4. Round the scaled real and imaginary components to integers.
5. Construct a polynomial in `Z_Q[X] / (X^N + 1)`.
6. Represent its coefficients in the active RNS towers for the selected CKKS
   level.
7. Convert the polynomial to the evaluation representation expected by later
   ciphertext/plaintext operations.

OpenFHE is a concrete reference implementation. Its
`MakeCKKSPackedPlaintext` service validates the level and slot capacity,
selects the level-specific scale, constructs a `CKKSPackedEncoding`, and calls
`Encode()`. The encoder applies `FFTSpecialInv`, scales and rounds the values,
fills the active DCRT/RNS towers, and changes the polynomial to evaluation
form:

- <https://github.com/openfheorg/openfhe-development/blob/main/src/pke/include/cryptocontext.h>
- <https://github.com/openfheorg/openfhe-development/blob/main/src/pke/lib/encoding/ckkspackedencoding.cpp>
- <https://github.com/openfheorg/openfhe-development/blob/main/docs/sphinx_rsts/modules/pke/pke_encoding.rst>

A representative provider call is:

```cpp
Plaintext plaintext = crypto_context->MakeCKKSPackedPlaintext(
    packed_values,
    noise_scale_degree,
    level,
    params,
    slots);
```

The resulting plaintext can participate in ciphertext/plaintext arithmetic,
such as provider operations equivalent to `EvalMult(ciphertext, plaintext)`
and `EvalAdd(ciphertext, plaintext)`.

#### Open64 logical operator

The intended compiler-visible conversion operator is:

```text
fhe.encode_plain.v1
```

Its proposed contract is:

```text
kid0:
  clear tensor

static attributes:
  encoding kind
  slot/layout identity
  target CKKS level
  target scale policy
  active slot count

result:
  encoded plaintext tensor
```

`fhe.encode_plain.v1` appears in the FHE integration plan, but it has not yet
been allocated and materialized as an executable DSL operator. This distinction
must remain visible in implementation and review.

#### Current SYNC-5 library-call path

The first high-level runtime path imports authenticated clear tensor data with
standard WHIRL equivalent to:

```text
OPR_CALL open64_fhe_plain_tensor_import_v1(...)
```

That call establishes the plain tensor's immutable identity and returns an
opaque `open64_fhe_plain_tensor_v1_t` handle. It does not by itself prove that
one unique CKKS encoding is sufficient for every consumer.

The handle is passed to a consuming operation such as:

```text
OPR_CALL open64_fhe_conv2d_plain_v1(...)
OPR_CALL open64_fhe_linear_plain_v1(...)
```

In this initial library-call path, the provider may prepare the context-
specific CKKS plaintext while servicing or preparing the consuming operation.
One immutable source tensor may need multiple encoded plaintext variants when
consumers require different layouts, levels, scales, or slot counts.

A safe provider cache identity therefore includes at least:

```text
source tensor identity
+ FHE configuration identity
+ layout identity
+ target level
+ target scale
+ active slot count
+ encoding version
```

The cache must not alter the explicit ABI identity, ownership, or manifest
checks.

#### Future explicit materialization requirement

The high-level provider path is sufficient for the first SYNC-5 executable.
Primitive CKKS optimization should make `fhe.encode_plain.v1` explicit before
runtime-call lowering. Otherwise the compiler cannot inspect, schedule,
verify, or optimize:

- the number of encodings created;
- reuse of encoded weights and biases;
- the level and scale of each plaintext;
- slot and tensor layout;
- encoded-plaintext memory cost; or
- whether two consumers can legally share one encoded representation.

The reviewed lowering must eventually choose whether an explicit
`fhe.encode_plain.v1` becomes a dedicated runtime ABI call or is discharged by
an authenticated provider-preparation contract. It must not silently disappear
into an unrelated arithmetic operation without equivalent state and
inspection evidence.

## End-To-End Value Classes

| Value | Stored compiler payload | Runtime representation | Secret key required by server |
| --- | --- | --- | --- |
| Model input image | No clear input payload in the server artifact | Imported ciphertext handle | No |
| Intermediate activation | Abstract ciphertext value and CKKS state | Provider ciphertext handle | No |
| Output logits | Abstract ciphertext result | Exported ciphertext envelope | No |
| Conv/linear weight | Floating-point external tensor | CKKS encoded plaintext handle | No |
| Conv/linear bias | Floating-point external tensor, possibly BatchNorm-folded | CKKS encoded plaintext handle | No |
| ReLU polynomial coefficients | Authenticated floating-point tensor | CKKS encoded plaintext handle | No |
| Secret key | Not represented in server WHIRL or artifacts | Client-only key object | Client only |

## Current Implementation Status

| Capability | Status |
| --- | --- |
| Ciphertext input/output entry declarations | Implemented |
| EncryptionDescriptorIR and tensor association | Implemented |
| FHE entry and value gatekeeper checks | Implemented |
| Plaintext parameter classification and SafeTensors evidence | Implemented |
| BatchNorm folding and converted weight/bias payloads | Implemented and certified |
| ReLU bootstrap/polynomial materialization planning | Implemented and certified as SYNC-4 planning evidence |
| Public plain-tensor and ciphertext runtime ABI | Contract defined; SYNC-5 implementation underway |
| Standard-WHIRL import/evaluation/export calls | Planned main/FHE coordinated SYNC-5 work |
| Deterministic mock executable | FHE-owned SYNC-5 work underway |
| Logical `fhe.encode_plain.v1` operator | Planned; not yet allocated or materialized |
| Real OpenFHE CKKS plaintext encoding | SYNC-6 |
| Client encryption and decryption harness | SYNC-6 |
| Encrypted model weights | Intentionally unsupported in the first release |

## Review Invariants

1. Canonical tensor types are not mutated into provider-specific ciphertext or
   plaintext C++ types.
2. EncryptionDescriptorIR remains separate from canonical tensor equivalence
   and from value-specific CKKS state.
3. The frontend declares encryption intent but never performs cryptography.
4. Plain model parameters remain authenticated external tensor data until the
   runtime provider encodes them.
5. BatchNorm folding completes before runtime-call lowering and preserves
   source and context provenance.
6. Every runtime weight and bias operand has an explicit manifest identity,
   type, shape, checksum, and consumer relationship.
7. The generated server never has access to the secret key and cannot decrypt
   intermediate values or output logits.
8. The output remains ciphertext until the separate trusted client decrypts
   it.

## Related Documents

- `doc/FHE-DSL-INTEGRATION-PLAN.md`
- `doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md`
- `doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md`
- `doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md`
- `doc/WHIRL-DSL-TENSOR-TYPE-HANDLING.md`
