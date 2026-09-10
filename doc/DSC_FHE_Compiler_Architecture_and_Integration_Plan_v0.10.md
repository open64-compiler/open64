<!--
Review-friendly Markdown companion to
DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx.
Keep architectural decisions synchronized between the two files.
-->

# DSC FHE Compiler Architecture and Integration Plan

*Python Ciphertext Declarations, WHIRL DSL Conversion, FHE CNN Canonicalization/Fusion, Selectable MetaKernel or Fhelipe Packing, SIHE/CKKS Scale-Level and Bootstrap Management, HPOLY Optimization, whirl2c C Emission, and GPU Runtime Integration*

Architecture-level draft | Version 0.10 | September 10, 2026

Design status: integration proposal; API and option names are proposed, not yet implemented.

### Purpose

Define the first integration architecture for fully homomorphic encryption support in DSC. The document establishes the source declaration, driver controls, WHIRL representation, CNN-to-FHE conversion, middle-WHIRL lowering, whirl2c C emission, C ABI runtime boundary, and staged CPU/GPU backend plan before committing to detailed FHE algorithms or a specific library implementation.

### Primary architectural decision

DSC will ingest an ordinary CNN model and preserve CNN/ResNet operators in very-high-level WHIRL. A Python ciphertext declaration and/or DSC driver option marks the model boundary as ciphertext input and ciphertext output with plaintext weights. A compiler conversion pass then creates an FHE CNN program, performs FHE-specific planning, lowers all custom operators into standard middle-WHIRL calls, and invokes whirl2c to generate C that is compiled and linked with a DSC FHE C-ABI runtime and the selected FHE library.

| Decision | Selected direction |
| --- | --- |
| Initial workload | ResNet-20/CIFAR-10 is the first end-to-end model vertical slice. Operator-level add, linear, and ReLU fixtures remain diagnostic unit tests rather than preceding model milestones. |
| Initial FHE scheme | CKKS, because the target workload uses approximate real-valued inference and the strongest NVIDIA GPU backends are CKKS-oriented. |
| Privacy boundary | Input and result are ciphertext; model weights and biases are plaintext/encoded plaintext; the secret key is never present in the server program. |
| Compiler front end | Python/torch.export/FX or DSC DSL produces binary very-high-level WHIRL with first-class CNN operators and encryption contracts. |
| Compiler conversion | A driver-controlled FHE CNN conversion pass transforms the ordinary CNN semantic graph into a legal encrypted inference graph. |
| Code generation | Custom DSC/FHE operators are lowered to standard middle-WHIRL calls before whirl2c; whirl2c emits C. |
| Runtime integration | Generated C calls an opaque C ABI. The C ABI wraps C++/CUDA FHE libraries such as OpenFHE, FIDESlib, or Cheddar-like backends. |
| GPU strategy | Establish a CPU/reference backend first, then add FIDESlib/OpenFHE interoperability, followed by a performance-oriented Cheddar-like CUDA backend. |

## Document Map

| Part / Section | Purpose |
| --- | --- |
| Part I - Integration Plan | Defines the minimum end-to-end path and freezes the major integration boundaries before deeper algorithm work. |
| 1. Scope and Non-Goals | Defines the direct ResNet-20 first vertical slice, its diagnostic unit tests, and explicit non-goals. |
| 2. End-to-End Integration Plan | Defines source declarations, driver options, WHIRL passes, whirl2c, C compilation, and FHE library linkage. |
| 3. Architectural Placement in DSC | Maps FHE into the Common Compiler Substrate, CNN domain, FHE domain, and backend namespaces. |
| 4. Python Ciphertext Declaration | Defines the user-facing declarator and its mapping into compiler contracts. |
| 5. Driver and Build Interface | Defines the Open64/DSC driver option family, CKKS scheme-configuration interface, parameter capture, and artifact flow. |
| Part II - Compiler Architecture | Defines IR, transformations, lowering phases, runtime ABI, and the gatekeeper. |
| 6. Very-High-Level WHIRL Representation | Preserves CNN semantics while attaching encryption descriptors and publishes the reviewed logical contract ownership boundary. |
| 7. CNN-to-FHE Conversion Pass | Transforms ResNet-20 semantics into a legal FHE graph; every surviving common.relu creates the required -O0 bootstrap and polynomial-activation boundary. |
| 8. FHE-Aware Canonicalization, Junk/Gap Simplification, and Graph Fusion | Makes FHE-specific masking, compaction, valid/junk-slot state, polynomial activation form, and gap propagation explicit; applies FHEFusion-style algebraic simplification before layout planning. |
| 9. Multi-Level WHIRL Lowering | Separates CNN/FHE semantics, encrypted tensor/vector planning, SIHE, CKKS, HPOLY, optional POLY/RNS, and C ABI lowering. |
| 10. Encrypted Tensor Layout Planner Selection: MetaKernel or Fhelipe | Defines mutually exclusive MetaKernel and Fhelipe planning modes behind one encrypted-layout interface so their effectiveness can be measured under identical model, scheme, and backend settings. |
| 11. SIHE-to-CKKS Scale, Level, and Bootstrap Management | Defines mandatory scheme-state propagation, a bootstrap boundary for every ReLU at -O0, local rescale/level legality, and optional ReSBM global planning before HPOLY. |
| 12. HPOLY and HPAO Polynomial-Level Optimization | Adds a compact polynomial IR between CKKS and POLY/RNS for cross-primitive optimization. |
| 13. whirl2c and C ABI Runtime | Defines how generated C links safely to C++/CUDA FHE libraries. |
| 14. GPU Backend Architecture | Defines library and native GPU backend paths, with HPOLY as the optimization boundary before low-level RNS expansion. |
| 15. Gatekeeper, Security, and Diagnostics | Defines compile-time and runtime certification obligations, mapped-image inspection, and retained review evidence. |
| Part III - Execution Plan | Defines implementation phases, benchmarks, success criteria, risks, and research threads. |
| 16. Implementation Roadmap | Builds the direct ResNet-20 integration in reviewable stages and cross-references the focused M0-M8 implementation milestones. |
| 17. Benchmark and Validation Plan | Defines functional, cryptographic, accuracy, and performance validation. |
| 18. Open Research Threads | Defers detailed packing, bootstrap-internal HPOLY, and native GPU optimization work until the integration path is running. |
| 19. Final Recommendation | Summarizes the recommended implementation sequence and abstraction boundaries. |
| Appendices | Provide source examples, IR evolution, runtime ABI sketches, detailed scheme-configuration analysis, the FHEFusion rule catalog, WHIRL implementation synchronization, the MetaKernel/Fhelipe comparative layout-planning contract, diagnostics, and references. |

## Part I. Integration Plan

> **Architecture note:** This part is normative for the first implementation. Detailed FHE optimization algorithms remain replaceable as long as they preserve these boundaries.

## 1. Scope and Non-Goals

### 1.1 Initial scope

- Inference-only ResNet-20/CIFAR-10 as the first end-to-end model workload. Small add, linear, and ReLU fixtures remain focused unit tests for diagnosis and contract certification; they are not preceding model milestones.

- Ciphertext input and ciphertext output; plaintext model weights and biases.

- CKKS as the first scheme.

- Python model capture to binary very-high-level WHIRL.

- Compiler-controlled conversion from ordinary CNN semantics to FHE CNN semantics.

- Middle-WHIRL lowering to standard calls accepted by whirl2c.

- Generated C linked with a DSC FHE C-ABI runtime and a selectable FHE library.

- CPU/reference execution first, followed by NVIDIA GPU acceleration.

### 1.2 Explicit non-goals for the first vertical slice

- FHE training or FHE post-training.

- Encrypted model weights.

- General dynamic Python control flow based on encrypted data.

- All FHE schemes in the first release.

- Direct generation of CUDA kernels from WHIRL in the first release.

- Secret-key management inside the server program.

- Automatic support for every CNN activation, pooling operation, or normalization operation.

- A production security certification; the first gatekeeper provides engineering validation and policy enforcement.

### 1.3 Why this boundary is practical

Ciphertext input with plaintext weights matches the common private-inference setting: a client encrypts private data, the server applies a proprietary or public model without learning the input, and the server returns encrypted results. CKKS directly supports approximate arithmetic over packed vectors and has active CPU and GPU ecosystems. Fhelipe shows that a high-level tensor program can be automatically packed and lowered into an efficient FHE circuit, while FIDESlib and Cheddar demonstrate practical GPU-oriented CKKS execution paths [2][6][8].

## 2. End-to-End Integration Plan

### 2.1 Complete compilation and execution flow

```text
Python ResNet-20 model
  + @dsc.fhe.entry declaration or driver-level FHE policy
  -> torch.export / FX / DSC graph capture
  -> WhirlExportInterpreter
  -> binary very-high-level WHIRL DSL IR
       cnn.model / cnn.resnet.basic_block / cnn.conv2d / common.residual_add
       TensorDescriptorIR + EncryptionDescriptorIR + FHEModelContractIR
  -> openpy -x whirl
  -> DSC FHE gatekeeper
  -> CNN-to-FHE conversion pass
  -> FHE tensor layout / scale / level / bootstrap / backend planning
  -> middle-level WHIRL containing standard calls and control flow
  -> whirl2c
  -> generated C + generated descriptor tables
  -> C compilation
  -> final C++/CUDA link with libdsc_fhe_cabi + selected FHE backend
  -> server executable or shared library
  -> encrypted request -> encrypted response
```

### 2.2 Integration decisions that should be frozen first

1.  The Python declaration marks encryption intent; it does not encrypt data during compilation.

2.  The existing DSC TENSOR type remains authoritative, with TY_KIND = TY_TENSOR. Ciphertext is represented by a tensor trait and an EncryptionDescriptorIR, not by creating a parallel type universe in the Open64 core.

3.  CNN/ResNet operations remain first-class in very-high-level WHIRL until the FHE gatekeeper and model adaptation pass have validated them.

4.  The driver option initiates the model-wide transformation and supplies default FHE policy. Source declarations may refine individual values.

5.  The first output of FHE lowering is a library-call program, not hand-generated cryptographic loops.

6.  No custom DSC CNN/FHE opcode may reach unmodified whirl2c. Every custom operation must be lowered to standard WHIRL constructs, OPR_CALL nodes, or a deliberately supported intrinsic set.

7.  Generated C uses opaque handles and a stable C ABI. C++ object layout and CUDA types remain hidden in the backend runtime.

8.  The final link uses a C++ or CUDA-aware linker driver when the selected FHE library is C++/CUDA, even though whirl2c emits C source.

### 2.3 MVP execution boundary

| Component | MVP responsibility | Deferred optimization |
| --- | --- | --- |
| Python front end | Capture model, declare encrypted inputs/outputs, lift parameters, emit high-level WHIRL. | Automatic model rewriting in Python. |
| DSC high-level middle end | Verify CNN semantics, propagate encryption state, fold constants/batch norm, reject unsupported operators. | Full cross-model FHE autotuning. |
| FHE lowering | Map supported CNN ops to C-ABI runtime calls with explicit descriptors. | Primitive-level rotation/NTT/kernel generation. |
| whirl2c | Translate standard middle-WHIRL and calls to C. | Understand FHE-specific high-level opcodes. |
| Runtime wrapper | Own keys, contexts, ciphertext/plaintext objects, weights, and backend calls. | Multi-GPU scheduling and fused GPU graph capture. |
| Backend | OpenFHE/reference or mock backend. | FIDESlib/Cheddar GPU performance path. |
| Gatekeeper | Reject decrypt-on-server, unsupported ops, missing keys, invalid depth/scale policy. | Formal cryptographic proof and production security certification. |

### 2.4 First acceptance test

ResNet-20 should compile directly from Python to binary WHIRL, transform under a driver option into an FHE program, lower to standard calls, pass through whirl2c, compile as C, link with a mock/reference FHE runtime, and return an opaque ciphertext result. Operator-level add, linear, and ReLU tests remain mandatory for isolating failures, but acceptance of the vertical slice is based on the complete ResNet-20 path.

## 3. Architectural Placement in DSC

### 3.1 Domain hierarchy

The existing DSC architecture places reusable tensor, shape, layout, numeric, sharding, memory, runtime-guard, and diagnostic concepts in the Common Compiler Substrate. CNN owns convolution, pooling, preprocessing, image-layout, and residual semantics. FHE support should follow the same rule: the common substrate owns generic representation and lowering mechanisms, while the FHE domain owns ciphertext, scheme, key, scale/level/noise, packing, and cryptographic legality [1].

```text
ant::compiler::common
  tensor | shape | layout | numeric | reduction | linalg
  memory | runtime_guard | contracts | diagnostics

ant::domain::cnn
  model | conv2d | batch_norm | residual_block | pooling | classifier_head

ant::domain::fhe
  scheme | ciphertext | plaintext_encoding | key_contract
  encrypted_layout | scale_level_noise | bootstrap | permutation

ant::domain::cnn::fhe_inference
  encrypted_cnn_contract | encrypted_resnet_contract
  fhe_conv2d | fhe_residual_add | fhe_poly_activation | encrypted_logits

ant::target::fhe
  mock | openfhe | fideslib | cheddar | future_gpu_backend
```

### 3.2 Flat tensor type with encryption representation

The Python API may expose CipherTensor as a convenient DSL type, but WHIRL should preserve the DSC TENSOR type design (TY_KIND = TY_TENSOR). The value is a tensor carrying an encryption trait and an EncryptionDescriptorIR. This keeps common tensor and shape reasoning reusable and avoids duplicating the entire tensor type system.

```text
TensorDescriptorIR {
  type: TENSOR
  ty_kind: TY_TENSOR
  dtype: f32 | f64 | i32 | encoded_real
  logical_shape: [N, C, H, W]
  semantic_role: image | feature_map | weight | logits
  traits: [EncryptedValue | PlaintextEncodedValue | ClearMetadata]
  representation_descriptor: EncryptionDescriptorIR
  lineage: source_tensor_id + transformation_history
}

EncryptionDescriptorIR {
  value_class: ciphertext | encoded_plaintext | clear_value | key_handle
  scheme: ckks | future_scheme
  scheme_config_id: FHECompilationConfigID
  value_state_id: optional CKKSValueStateID
  packing_layout_id: optional EncryptedTensorLayoutID
  key_set_id: optional
}
```

### 3.3 Domain ownership rule

| Concern | Owner |
| --- | --- |
| CNN operator meaning, residual path, padding, stride, channels, classifier shape | CNN/ResNet domain |
| Ciphertext/plaintext state, scheme legality, key material, scale/level/noise | FHE domain |
| Logical tensor shape, common arithmetic, call lowering, memory descriptors, diagnostics infrastructure | Common Compiler Substrate |
| Automatic packing, compaction, rotation plan, bootstrap placement | CNN/FHE lowering subsystem |
| GPU NTT, key switching, RNS operations, bootstrapping implementation | FHE backend plugin |
| Client/server encryption boundary and no-secret-key-on-server policy | FHE contract and runtime gatekeeper |

## 4. Python Ciphertext Declaration

### 4.1 Selected source form

The first API should use a function or method decorator because it declares a model boundary without rewriting every operator. Optional CipherTensor annotations improve readability and allow per-input refinement. Parameters lifted from an nn.Module are plaintext by default.

```text
from open64_dsc import export_to_whirl
from open64_dsc.fhe import entry, CipherTensor

class SecureResNet20(nn.Module):
    @entry(
        scheme="ckks",
        encrypted_inputs=("x",),
        encrypted_outputs=("return",),
        parameter_policy="plaintext",
        security_level=128,
    )
    def forward(
        self,
        x: CipherTensor[1, 3, 32, 32],
    ) -> CipherTensor[1, 10]:
        return self.model(x)
```

> **Architecture note:** CipherTensor is a declarative compiler type. It does not contain or create a runtime ciphertext in Python. The client-side runtime performs encryption; the compiler only records the boundary contract.

### 4.2 Lower-intrusion alternative

For models that should not be edited, the export API may supply the same policy externally:

```text
whirl_module = export_to_whirl(
    exported_program,
    options=WhirlExportOptions(
        preserve_domain_ops=True,
        fhe_mode="cnn_inference",
        fhe_scheme="ckks",
        encrypted_inputs=["x"],
        encrypted_outputs=["return"],
        parameter_policy="plaintext",
    ),
)
```

### 4.3 Declaration precedence

| Priority | Source | Behavior |
| --- | --- | --- |
| 1 - highest | Per-value Python annotation or explicit dsc.fhe.ciphertext_input/output marker | Overrides broader defaults for the specific value. |
| 2 | @dsc.fhe.entry decorator | Defines the model or function boundary. |
| 3 | WhirlExportOptions | Defines export-time policy without source editing. |
| 4 - lowest | Open64/DSC driver option | Defines default transformation policy for unannotated WHIRL artifacts. |

### 4.4 Boundary semantics

- Ciphertext input: imported as an opaque encrypted tensor whose logical shape and approximate dtype are known but whose values are unavailable to the server compiler/runtime.

- Plaintext weight: a model parameter is available to the server and is encoded into scheme-specific plaintext form, potentially prepacked and cached.

- Ciphertext activation: any supported operation consuming ciphertext and plaintext/ciphertext operands produces ciphertext.

- Ciphertext output: the server returns the encrypted result without decrypting it.

- Secret key: never appears in WHIRL, generated C, server descriptors, or backend context.

- Evaluation keys: represented by opaque runtime handles and a key manifest, not embedded as ordinary compiler constants.

## 5. Driver and Build Interface

### 5.1 Proposed option family

```text
openpy -x whirl resnet20.B \
  -dsc-fhe=cnn \
  -dsc-fhe-scheme=ckks \
  -dsc-fhe-ckks-ring-dim=65536 \
  -dsc-fhe-security=128-classic \
  -dsc-fhe-ckks-mult-depth=auto \
  -dsc-fhe-ckks-scale-bits=56 \
  -dsc-fhe-ckks-first-mod-bits=60 \
  -dsc-fhe-io=ciphertext \
  -dsc-fhe-weights=plaintext \
  -dsc-fhe-backend=openfhe \
  -dsc-fhe-target=cpu \
  -dsc-fhe-layout=automatic \
  -dsc-fhe-layout-planner=metakernel \
  -dsc-fhe-bootstrap=auto \
  -dsc-fhe-codegen=whirl2c \
  -o resnet20_fhe
```

| Option | Initial values | Purpose |
| --- | --- | --- |
| -dsc-fhe | off \| cnn \| auto | Enables the model-wide FHE conversion pipeline. |
| -dsc-fhe-scheme | ckks | Selects the first supported scheme. |
| -dsc-fhe-io | ciphertext | Requires encrypted input and encrypted output. |
| -dsc-fhe-weights | plaintext | Marks lifted parameters as server-known encoded plaintext. |
| -dsc-fhe-backend | mock \| openfhe \| fideslib \| cheddar | Selects runtime/backend capabilities and cost model. |
| -dsc-fhe-target | cpu \| cuda | Selects execution device policy. |
| -dsc-fhe-security | 128-classic \| 192-classic \| 256-classic | Defines the minimum cryptographic security target; explicit N must still pass the compiler security check. |
| -dsc-fhe-layout | automatic \| fixed | Controls whether encrypted layout is compiler-planned or externally fixed. |
| -dsc-fhe-ckks-scale-policy | auto \| local \| eva \| pars \| resbm | Selects CKKS scale/bootstrap planning. auto maps to mandatory local management at -O0, optimized local management at -O1, and optional advanced ReSBM planning at -O2. -O3 inherits the selected -O2 plan before parallelization and memory optimization. |
| -dsc-fhe-bootstrap | auto \| on \| off \| manual | Controls bootstrap permission and planning. At -O0, auto/on inserts a mandatory boundary before every common.relu polynomial approximation and uses greedy just-in-time refresh elsewhere; manual requires explicit ReLU boundaries; off rejects a surviving ReLU. |
| -dsc-fhe-codegen | whirl2c | Selects C emission through standard middle-WHIRL. |
| -dsc-fhe-dump | contracts \| layouts \| levels \| calls \| all | Emits diagnostic planning artifacts. |
| -dsc-fhe-layout-planner | metakernel \| fhelipe | Selects exactly one encrypted tensor planning method. Compile the same model with each value to compare effectiveness; both must emit the same common layout/rotation/key-requirement interface. |
| -dsc-fhe-fusion | auto \| off \| fhefusion | Controls FHE-aware graph fusion. Canonicalization remains mandatory. auto maps to canonicalization-only at -O0, local simplification at -O1, and full advanced graph search at -O2. -O3 inherits the enabled -O2 semantic result. |
| -dsc-fhe-advanced | auto \| on \| off | Master control for optional -O2 FHE planning and transformation. off preserves mandatory legality and the lower-level baseline while disabling ReSBM, global FHEFusion search, and other advanced FHE profitability transformations. |
| -dsc-fhe-parallel | auto \| on \| off | Controls the -O3 parallelization stage. auto enables it at -O3 and disables it below -O3; on/off supports debugging and controlled comparison. |
| -dsc-fhe-memory-opt | auto \| on \| off | Controls the -O3 memory-hierarchy stage, extending CPU data-cache optimization to NUMA, ciphertext packing, polynomial working sets, GPU register/shared/L2/HBM placement, and host/device transfer scheduling. |

### 5.2 Build pipeline

```text
# Compiler-generated C
whirl2c resnet20_fhe_mid.B -o resnet20_fhe.c

# Compile generated C
cc -O2 -I${DSC_FHE}/include -c resnet20_fhe.c -o resnet20_fhe.o

# Final link: use a C++/CUDA-aware linker because the runtime libraries are C++/CUDA
c++ resnet20_fhe.o \
  -ldsc_fhe_cabi -lopenfhe -lfideslib -lcudart \
  -o resnet20_fhe_server
```

> **Architecture note:** The generated source is C, but the final link is not necessarily a pure C link. The DSC driver should own this detail and choose cc, c++, or nvcc/host-link flags based on the backend manifest.

### 5.3 Artifact boundary

| Artifact | Contents |
| --- | --- |
| resnet20.B | Very-high-level binary WHIRL with CNN operators, TensorDescriptorIR, encryption boundary contracts, and source provenance. |
| resnet20_fhe.B | FHE semantic WHIRL after conversion and legality checks. |
| resnet20_fhe_plan.json | Packing, level/scale, bootstrap, key, backend, and diagnostic plan for inspection. |
| resnet20_fhe_mid.B | Standard middle-WHIRL with calls, structs, constants, and ordinary control flow accepted by whirl2c. |
| resnet20_fhe.c | Generated C source and static descriptor tables. |
| libdsc_fhe_cabi | Stable opaque C ABI wrapping C++/CUDA backend implementations. |
| resnet20_fhe_server or .so | Linked server executable or model shared library. |

### 5.4 FHE Scheme Configuration and Compiler Options

Scheme parameters are compiler inputs, not backend-only runtime settings. The user-facing interface shall expose a small stable CKKS option set, with ring dimension N as an explicit expert/user constraint. DSC captures the requested configuration in FHECompilationConfigIR, derives the remaining modulus, packing, key, and schedule information, verifies security, and propagates resolved CKKS state to the later WHIRL levels. q0 and the CKKS scaling factor are distinct: q0 is the first modulus prime, while the numerical scale is Δ; the command line therefore uses separate first-modulus and scale-bit options. [5][17][18][19]

| Compiler option | Meaning | Example / default | Internal destination |
| --- | --- | --- | --- |
| -dsc-fhe-scheme | FHE scheme | ckks | FHECompilationConfigIR.scheme |
| -dsc-fhe-ckks-ring-dim | Ring dimension N; user-selected constraint | 65536 | FHECompilationConfigIR.ckks.ring_dimension |
| -dsc-fhe-security | Security target | 128-classic | FHECompilationConfigIR.security.target |
| -dsc-fhe-ckks-mult-depth | Multiplicative-depth budget or compiler derivation | auto \| 18 | requested depth -> CKKSResolvedParameterIR |
| -dsc-fhe-ckks-scale-bits | log2(Δ), regular CKKS scaling-prime/scale size | 56 | FHECompilationConfigIR.ckks.scaling_mod_bits |
| -dsc-fhe-ckks-first-mod-bits | log2(q0), first modulus size; not the scale | 60 | FHECompilationConfigIR.ckks.first_mod_bits |
| -dsc-fhe-ckks-slots | Active SIMD slots; must not exceed N/2 | auto \| 32768 | packing constraint -> resolved active_slots |
| -dsc-fhe-ckks-scaling | CKKS rescaling technique | flexible-auto | scheme configuration / backend mapping |
| -dsc-fhe-ckks-key-switch | Key-switch technique | hybrid | KeySwitchConfigIR.technique |
| -dsc-fhe-ckks-large-digits | HYBRID decomposition digit count | 3 | KeySwitchConfigIR.decomposition_digits |
| -dsc-fhe-ckks-secret-key-dist | Secret-key distribution policy | uniform-ternary | FHECompilationConfigIR.security.secret_key_distribution |
| -dsc-fhe-ckks-scale-policy | auto \| local \| eva \| pars \| resbm | Selects local or global planning. auto uses mandatory local management at -O0, optimized local management at -O1, optional advanced ReSBM at -O2, and the same finalized plan as input to -O3 parallelization and memory optimization. | CKKSSchemeConfigIR.scale_policy |
| -dsc-fhe-bootstrap | High-level bootstrap policy | Controls bootstrap permission and planning. At -O0, auto/on inserts a mandatory boundary before every common.relu polynomial approximation and uses greedy just-in-time refresh elsewhere; manual requires explicit ReLU boundaries; off rejects a surviving ReLU. | CKKSBootstrapConfigIR.policy |
| -dsc-fhe-ckks-bootstrap-level-budget | Advanced CoeffsToSlots/SlotsToCoeffs level budget | 4,4 | CKKSBootstrapConfigIR.level_budget |
| -dsc-fhe-ckks-bootstrap-bsgs | Advanced BSGS dimensions for bootstrap transforms | auto | CKKSBootstrapConfigIR.bsgs_dim |
| -dsc-fhe-ckks-levels-after-bootstrap | Levels requested after bootstrap | 10 | CKKSBootstrapConfigIR.levels_after_bootstrap |
| -dsc-fhe-advanced | auto \| on \| off | Master enable for optional advanced -O2 FHE transformations. | FHEOptimizationConfigIR.advanced |
| -dsc-fhe-parallel | auto \| on \| off | Controls Open64-style -O3 parallelization after the selected -O2 plan. | FHEOptimizationConfigIR.parallel |
| -dsc-fhe-memory-opt | auto \| on \| off | Controls -O3 CPU/GPU memory-hierarchy optimization. | FHEOptimizationConfigIR.memory_opt |

Ownership rule: users select deployment policy and may constrain N, depth, precision, and security; the compiler derives the modulus chain, rotation set, packing layout, key requirements, bootstrap positions, basis transitions, and per-value scale/level state. Explicit user settings are constraints and are never allowed to bypass security or legality checks. Detailed parameter semantics, IR structures, diagnostics, and OpenFHE mappings are documented in Appendix D.

## Part II. Compiler Architecture

## 6. Very-High-Level WHIRL Representation

### 6.1 Ingestion rule

The Python front end remains a source-language capture layer. It emits first-class CNN operators into binary very-high-level WHIRL, consistent with the DSC Python ingestion architecture. High-level operations are not converted into FHE library calls during ingestion; the compiler must first verify CNN semantics and encryption contracts [1].

### 6.2 Required high-level records

| IR record | Purpose |
| --- | --- |
| FHEEntryContractIR | Declares encrypted inputs, encrypted outputs, plaintext parameter policy, scheme, security level, and client/server trust boundary. |
| EncryptionDescriptorIR | Attaches ciphertext/plaintext representation state to each tensor value. |
| FHEModelContractIR | Declares inference-only policy, allowed operators, activation policy, output policy, and backend requirements. |
| KeyMaterialContractIR | Declares required public/evaluation/rotation/relinearization/bootstrap keys and prohibits secret-key access. |
| FHEApproximationContractIR | Declares activation or function approximations, degree, valid input range, and accuracy tolerance. |
| EncryptedTensorLayoutIR | Introduced later to describe ciphertext count, slot mapping, gaps, scale, and level. |
| FHEBackendCapabilityIR | Declares supported primitives, bootstrapping, GPU target, memory limits, and runtime ABI version. |
| FHECompilationConfigIR | Captures module-level FHE/CKKS user and profile configuration such as ring dimension N, security target, requested multiplicative depth, scale/first-modulus sizes, slot policy, key-switch policy, and bootstrap policy before packing and scheme lowering. |

### 6.3 Example very-high-level WHIRL intent

```text
FUNCTION secure_resnet20_forward
  INPUT x : TENSOR[1,3,32,32]
    traits = { EncryptedValue }
    encryption = { scheme=CKKS, boundary=input, key_set=request_key }

  PARAM conv1.weight : TENSOR[16,3,3,3]
    traits = { PlaintextWeight }

  %1 = OPR_DSC_CNN_CONV2D(x, conv1.weight)
  %2 = OPR_DSC_CNN_BATCH_NORM_INFER(%1, ...)
  %3 = OPR_DSC_COMMON_RELU(%2)
  ...
  %logits = OPR_DSC_CNN_CLASSIFIER_HEAD(...)
  OPR_DSC_FHE_CIPHERTEXT_OUTPUT(%logits)
```

### 6.4 Encryption-state propagation

| Operation / operator class | Input state | Output state | Type propagation / policy effect |
| --- | --- | --- | --- |
| Conv2d | ciphertext activation + plaintext/encoded weight (+ plaintext bias) | ciphertext | Propagate ciphertext to the output. The weight remains plaintext/encoded plaintext; lowering may use rotations, multiply-plaintext, and additions. |
| BatchNorm inference | ciphertext activation + plaintext BN constants | ciphertext | Propagate ciphertext if BatchNorm remains; preferred first-release policy is to fold BN into convolution weights/bias before encrypted execution. |
| Residual add / residual merge | ciphertext main path + ciphertext shortcut | ciphertext | Both branches must be ciphertext and eventually compatible in logical shape, encrypted layout, CKKS scale, and level. |
| Average pooling | ciphertext | ciphertext | Linear reduction preserves ciphertext state; plaintext scaling factors remain encoded plaintext. |
| Max pooling | ciphertext | illegal until rewritten; otherwise ciphertext after declared approximation | Exact max is not a first-release CKKS operation. Rewrite to an approved approximation/policy before normal type propagation, or reject. |
| Flatten / reshape | ciphertext | ciphertext | Logical type/shape transformation preserves encryption state. Physical encrypted layout conversion may be deferred to layout planning. |
| Linear / classifier | ciphertext activation + plaintext/encoded weight (+ plaintext bias) | ciphertext | Propagate ciphertext to logits. Lowering becomes encrypted MVM/tensor contraction with plaintext parameters. |
| Polynomial activation | ciphertext | ciphertext | Propagate ciphertext. A source common.relu requires an approved polynomial approximation. At -O0 its conversion creates a mandatory bootstrap boundary before polynomial evaluation; ciphertext multiply then consumes the refreshed level budget. |
| Add / subtract | ciphertext + ciphertext, or ciphertext + encoded plaintext | ciphertext | Preserve ciphertext; require legal operand type combination and later layout/scale/level compatibility. |
| Multiply by plaintext weight/constant | ciphertext x encoded plaintext | ciphertext | Preserve ciphertext; consumes level/precision according to the resolved CKKS plan. |
| Ciphertext multiply | ciphertext x ciphertext | ciphertext | Preserve ciphertext but consume greater depth; allowed only when required by the transformed FHE algorithm. |
| Return / ciphertext output | ciphertext | ciphertext response | Preserve ciphertext and the client-visible export layout/serialization contract. |
| Decrypt | ciphertext + secret key | plaintext | Illegal in server compilation. The secret key must never appear in server WHIRL or generated C. |

Table 6.4 is the single normative type-propagation table for the CNN-to-FHE conversion pass. The implementation shall encode these rules once and reuse them during Section 7.2 type propagation; a second operator-specific propagation table shall not be maintained.

### 6.5 Logical Operator and Descriptor Ownership

The focused integration plan publishes proposed stable names and ownership before opcode allocation. These names describe the logical compiler interface; physical OPR_DSL encoding remains private. A proposed name becomes a binary and ASCII compatibility contract only after common/com review accepts its version, operands, attributes, descriptor requirements, effects, verifier rules, and lowering owner.

| Contract family | Initial logical contracts | Ownership and visibility rule |
| --- | --- | --- |
| Common substrate | common.model_input, common.model_output, common.output_logits, common.add, common.bias_add, common.mul, common.relu, common.linear, common.flatten, common.reshape, common.window_reduce, common.residual_add | Reuse accepted common contracts where semantics match. common.relu preserves source-level ReLU meaning across domains. New names or versions require common/com review. CNN and FHE legality must remain visible until their gatekeepers complete. |
| CNN domain | cnn.conv2d, cnn.batch_norm_infer, cnn.max_pool2d, cnn.global_avg_pool2d | Preserve stride, padding, dilation, groups, layout, inference mode, and pooling semantics through CNN and FHE verification. |
| FHE contracts and wrappers | fhe.entry_contract, fhe.encryption_descriptor, fhe.cnn.conv2d, fhe.cnn.poly_activation, fhe.cnn.residual_add | FHE owns encryption policy, approximation policy, encrypted residual alignment, planning descriptors, and diagnostics. These are not a parallel scalar or tensor type universe. |
| Scheme-independent HE | sihe.add, sihe.sub, sihe.mul, sihe.neg, sihe.rotate, sihe.encode, sihe.bootstrap | Internal FHE logical layer after source-domain verification. Align naming with ACE SIHE where semantics match; validation-message operations remain optional validation artifacts. |
| CKKS | ckks.add, ckks.sub, ckks.mul, ckks.neg, ckks.rotate, ckks.encode, ckks.rescale, ckks.upscale, ckks.modswitch, ckks.relin, ckks.bootstrap, ckks.scale, ckks.level, ckks.batch_size, ckks.raise_mod, ckks.mul_mono, ckks.conjugate, ckks.free | CKKS-specific value state, legality, key requirements, lifetime, and scale/level management. Exact versions are accepted explicitly; unknown versions are rejected. |
| HPOLY and optional POLY/RNS | poly.add, poly.sub, poly.mul, poly.rotate, poly.rescale, poly.modup, poly.dotprod, poly.moddown, poly.extend, plus reviewed lower POLY/RNS operations | HPOLY remains the compact optimization layer. Native POLY/RNS and GPU operations are introduced only after reviewed CKKS-to-POLY lowering and backend selection. |
| Runtime ABI | dsc_fhe_* calls with opaque context, model, ciphertext, plaintext, and key-set handles | Runtime calls are generated only after FHE lowering to standard OPR_CALL. They are not ingestion opcodes and never expose backend C++ or CUDA object layouts. |

TensorDescriptorIR remains the shared tensor identity. Encryption state, scale, level, packing, key-set references, and backend plans are FHE representation and domain compatibility facts; compiler metadata remains source context, diagnostics, pass ownership, lowering hints, and profile data.

## 7. CNN-to-FHE Conversion Pass

### 7.1 Role

The FHE conversion pass is the semantic bridge between an ordinary CNN model and an executable encrypted CNN. It is activated by the driver and consumes CNN operators plus encryption boundary contracts. The pass may rewrite the model only when the transformation is explicit, legal, and recorded in a conversion report.

### 7.2 Required transformation sequence

1.  Validate inference mode and freeze training-only behavior.

2.  Classify all values as ciphertext, encoded plaintext, clear metadata, or illegal secret material.

3.  Fold batch normalization into convolution weights and bias where legal.

4.  Preserve each source ReLU as common.relu. At -O0, create the mandatory pre-ReLU bootstrap boundary and then apply the declared polynomial approximation; reject any other unsupported nonlinear activation.

5.  Replace max pooling with average pooling or another declared FHE-compatible approximation when the model policy permits.

6.  Run FHE type propagation using the single rule table in Section 6.4. Apply those rules to convolution, residual paths, pooling, flatten/reshape, classifier, activation, and common arithmetic operations; do not duplicate the propagation logic in this pass.

7.  Verify residual branch shape, encryption state, scale/level compatibility obligations, and eventual layout alignment.

8.  Create FHE semantic operators plus explicit valid/junk/gap annotations needed by the FHE-aware canonicalization phase.

9.  Hand the legal graph first to FHE-aware canonicalization/simplification/fusion, then to encrypted tensor layout, scale/level, bootstrap, and backend planning.

### 7.3 CNN operation policy for the first release

| CNN operation | First-release policy | Lowering direction |
| --- | --- | --- |
| Conv2d | Supported under static shape, known stride/padding/dilation/groups subset. | Ciphertext rotations + encoded plaintext weights + additions, initially through runtime call. |
| BatchNorm inference | Fold into preceding convolution when possible. | Compile-time plaintext weight/bias rewrite. |
| Residual add | Supported when logical shape and encrypted representation can be aligned. | Ciphertext add after scale/level/layout alignment. |
| ReLU | Supported as common.relu source semantics. FHE conversion requires an approved polynomial approximation and a mandatory bootstrap boundary at -O0. | Preserve common.relu through common/CNN/FHE gatekeepers. Then emit SIHE bootstrap immediately before the polynomial activation, or a backend-supported fused bootstrap-plus-activation with equivalent logical evidence; gatecheck degree, range, and error budget. |
| Average pooling | Supported. | Linear encrypted reduction and plaintext scaling. |
| Max pooling | Reject or explicitly approximate. | Future scheme-specific lowering. |
| Flatten/reshape | Supported as logical layout transformation. | May require no data movement or an encrypted layout conversion. |
| Linear/classifier | Supported with plaintext weights. | Encrypted matrix-vector/tensor contraction. |
| Softmax/argmax | Deferred from server path. | Return encrypted logits; client may decrypt and decide. |

> **Architecture note:** Bootstrap does not itself define ReLU in the CKKS path. It restores ciphertext capacity for the approved polynomial approximation. A provider may implement a fused bootstrap-plus-activation primitive, but the compiler must retain the logical common.relu, approximation contract, bootstrap reason, source position, and resulting CKKS state for verification and inspection.

### 7.4 Conversion contract

```text
FHECNNConversionContract {
  source_model_hash
  source_operator_set
  scheme = CKKS
  input_policy = ciphertext
  output_policy = ciphertext_logits
  parameter_policy = plaintext_encoded
  batchnorm_policy = fold
  activation_policy = polynomial
  relu_semantic_op = common.relu
  o0_bootstrap_before_relu = required
  pooling_policy = average_or_reject
  security_level = 128
  accuracy_budget
  supported_backend_set
  certification_status
}
```

## 8. FHE-Aware Canonicalization, Junk/Gap Simplification, and Graph Fusion

### 8.1 Architectural Role

This phase executes after CNN-to-FHE semantic conversion and before encrypted tensor layout planning. Its purpose is to normalize the transformed CNN into a graph where CKKS-relevant data validity is explicit without prematurely lowering into rotations, ciphertext multiplications, or RNS operations. FHEFusion shows that a conventional DNN graph hides masking/compaction behavior, while primitive CKKS lowering is too fine-grained for effective graph optimization [16].

DSC shall therefore preserve high-level CNN/FHE operators and introduce explicit FHE-aware Masking and Strided_Slice (compaction) operators. Canonicalization is required even at -O0 because downstream layout, scale/depth, and correctness passes need to know which tensor positions are valid, guaranteed zero, junk, or separated by stride-induced gaps. Fusion and algebraic movement of these operators are optimization decisions controlled separately.

### 8.2 Canonical FHE-CNN Graph Form

| Source / semantic form | Canonical DSC FHE form | Reason |
| --- | --- | --- |
| Masking implicit in lowering | fhe.masking(t, mask_01) | Makes junk-slot clearing explicit. In CKKS this is ct×pt multiplication and may consume multiplicative depth. |
| Compaction hidden inside packing | fhe.strided_slice(t, begin, end, strides) | Represents extraction/reorganization of valid slots without losing graph-level meaning. |
| AvgPool(x) | Strided_Slice(Masking((1/(h*w)) * AvgPool'(x, divisor_override=1))) | Separates summation from averaging so the scalar multiply and cleanup can move/fuse independently. |
| Conv(x,w,b) | Strided_Slice(Masking(Conv'(x,w)+b)) when cleanup is required | Makes Conv-produced junk slots and stride-induced gaps visible. Omit Strided_Slice when stride=1 and no compaction is required. |
| Polynomial RELU ax^2+bx+c | a * (x^2 + (b/a)x + c/a), when a != 0 | Normalizes the polynomial so outer scalar constants can be propagated/folded into AvgPool/Conv/GEMV plaintext constants. |
| Flatten / Reshape | value-preserving operator + propagated slot-validity state | These operators change shape but not values; scalar factors, masks, and compaction information may commute across them. |
| Concat | value-preserving composition + merged gap/validity map | Allows distribution of common scalars and compaction through concatenation when shapes/slices align. |

The paper specifically reexpresses AvgPool and Conv using Masking and Strided Slice so that cleanup costs become analyzable at graph level. Masking removes junk with a 0/1 plaintext; Strided Slice preserves valid values while changing their placement. Complex Strided Slice implementations may themselves require masking, so the compiler must not assume compaction is always depth-free [16].

### 8.3 Slot Validity, Junk, and Gap State

DSC shall attach a separate slot-validity descriptor to TENSOR values after FHE model adaptation. This is representation state, not a new TY_KIND: the base type remains TENSOR with TY_KIND = TY_TENSOR. The descriptor records which logical/packed positions contain semantic values and which positions are junk, guaranteed zero, or gaps introduced by stride/padding.

```text
FHESlotValidityIR {
  tensor_id
  logical_shape
  valid_region_or_formula
  junk_region_or_formula
  guaranteed_zero_region
  gap_strides                 // inherited stride/gap state distinct from operator stride
  mask_descriptor_id          // optional 0/1 plaintext mask
  compaction_state            // none | required | deferred | absorbed
  provenance                  // generating Conv/Pool/Stride/etc.
}
```

A consumer may read junk positions only if its semantics prove those positions are irrelevant or a later fused operation absorbs cleanup. If a consumer can observe junk slots semantically, the compiler must materialize Masking or reject the transformation. When Strided Slice moves through an operator, the successor inherits the slice information as Gap_Strides, matching the paper's distinction between inherited gap stride and the operator's normal stride [16].

### 8.4 Mandatory Canonicalization and Garbage Simplification

Canonicalization occurs before fusion search. The paper notes that normalization simplifies later constant folding; for example, a(x^2+(b/a)x+c/a) exposes an outer scalar that can be absorbed into a later plaintext-weight operator more directly than ax^2+bx+c. DSC generalizes this into a canonicalization pass with the following required transformations [16].

| Canonicalization / simplification | Required DSC behavior |
| --- | --- |
| Expose implicit cleanup | Insert explicit fhe.masking and fhe.strided_slice nodes where the semantic lowering would otherwise hide junk removal or compaction. |
| Normalize polynomial activation | Factor common outer scalar when legal; canonicalize coefficient ordering and constant operands. |
| Separate reduction from scaling | Represent AvgPool summation separately from the 1/(h*w) scalar so the multiply can be folded. |
| Classify value-preserving ops | Mark Flatten, Reshape, and Concat so scalar/mask/gap state can be propagated through them. |
| Eliminate redundant cleanup | Remove a mask absorbed by Strided Slice; combine adjacent Strided Slice operations; avoid masking if downstream semantics do not observe junk. |
| Propagate gap metadata | Update Gap_Strides and slice attributes when compaction moves across AvgPool, Conv, RELU, Flatten, or Reshape. |
| Prioritize constant sinks | Prefer folding constants into Conv/GEMV plaintext weights or polynomial coefficients rather than into Masking when that eliminates ct×pt multiplies and preserves later fusion. |
| Preserve legality evidence | Every rewrite records shape/slice/mask preconditions, updated operator attributes, and provenance in the transformation report. |

### 8.5 Algebraic Simplification and Fusion Rule Classes

FHEFusion derives its transformations from associativity, distributivity, and commutativity. The first twelve rules in the paper directly reduce multiplicative depth; later rules primarily move masks, scalars, and compaction so additional depth-reducing fusions become possible. DSC shall organize the rules into three implementation families: Constant Folding, Masking Folding, and Compaction Folding [16].

| Family | Representative paper rules | DSC intent |
| --- | --- | --- |
| Constant folding | FUSED-GEMV-SCALAR, FUSED-SCALAR-GEMV, FUSED-CONV-SCALAR, FUSED-SCALAR-CONV, FUSED-SCALAR-RELU, SCALAR, CONCAT, RELU-DIS, AVGPOOL-SCALAR, FLATTEN-SCALAR, RESHAPE-SCALAR | Move/fold compile-time scalars into plaintext weights or polynomial coefficients; reduce ct×pt multiplications and multiplicative depth. |
| Masking folding | FUSED-RELU-MASKING, MASKING-SCALAR, SCALAR-MASKING, FUSED-CMPT-MASKING, MASKING-MUL, MASKING-ADD-RELU, FLATTEN-MASKING | Remove or absorb junk-clearing multiplications while preserving zero/junk semantics. |
| Compaction folding | FUSED-CMPT, FUSED-GEMV-CMPT, CONCAT-CMPT, CMPT-MUL, CMPT-ADD, CONV-CMPT, RELU-CMPT, AVGPOOL-CMPT, CMPT-SCALAR, SCALAR-CMPT, FLATTEN-CMPT, RESHAPE-CMPT | Move/merge Strided Slice, propagate gaps, avoid repeated compaction, and reduce rotations/cleanup operations. |

A full paper-derived rule catalog is retained in Appendix E so implementation can use one stable rule registry rather than embedding ad-hoc peepholes in individual CNN operators.

### 8.6 Representative Garbage-Reduction Patterns

| Before | After | Effect |
| --- | --- | --- |
| RELU(Masking(t)) | RELU_fused_mask(t) | Absorb the binary mask into polynomial coefficients; eliminates one ct×pt multiply and one depth level when preconditions hold. |
| Strided_Slice(Masking(t)) | Strided_Slice'(t) | Compaction extracts only valid slots, making the explicit mask redundant. |
| Strided_Slice2(Strided_Slice1(t)) | Strided_Slice3(t) | Combine adjacent compactions and their stride/slice attributes. |
| GEMV(Strided_Slice(t)) | GEMV'(t) | Let GEMV consume the gapped layout directly when legal; avoids explicit compaction. |
| Masking(t) * scalar | t * (scalar * mask_01) | Fold cleanup mask into a compile-time plaintext constant. |
| Masking(t1) * Masking(t2) | Masking(t1*t2) | Use distributivity to postpone one cleanup operation. |
| AvgPool(Strided_Slice(t)) | Strided_Slice'(AvgPool'(t, Gap_Strides=...)) | Move compaction through pooling, propagate inherited gap strides, then merge with later compaction. |
| Conv(AvgPool(RELU(x)),W) | Conv(AvgPool'(x^2+(b/a)x+c/a,do=1), (a/(h*w))*W) | Fold polynomial/AvgPool scalars into plaintext Conv weights; paper example reduces multiplicative depth by two. |

### 8.7 Fusion Search Algorithm and Pass Ordering

DSC adopts the paper's graph-search structure rather than fixed operator-pattern matching. First apply distributive rules to expose opportunities. Then iteratively choose a fusion-source node (for example Masking, Strided Slice, or a movable scalar), search successors first and predecessors if no successor target is found, and use rule properties to traverse the graph. An associative neighbor is a fusion target; a commutative neighbor may be crossed recursively; a non-compatible neighbor terminates that path. Apply the fusion only after legality and profitability checks, update the DAG, and continue to a fixed point [16].

```text
FHEFusionPass {
  CanonicalizeFHEGraph();
  ApplyDistributiveRules();
  while (source = FindFusionSource()) {
    targets = FindFusionTargets(source); // successor-first, predecessor fallback
    if (Legal(source, targets) && Profitable(source, targets)) {
      FuseAndUpdateAttributes(source, targets);
      CleanupCSE_DCE_CopyProp();
    }
  }
}
```

For constant folding, target priority matters: Masking is a low-priority constant sink, while Conv/GEMV should be preferred because their plaintext weight constants can absorb scalars and eliminate runtime ciphertext-plaintext multiplications. This target-priority rule should be explicit in the DSC fusion registry rather than dependent on incidental traversal order [16].

### 8.8 Profitability Model and Interaction with Ring Dimension N

The paper applies non-Strided-Slice rules aggressively, but treats Strided Slice propagation specially because leaving gaps can reduce slot utilization and SIMD parallelism and may force a larger ring dimension N. Its simple cost model uses gap sparsity and the resulting impact on N: absent/minimal gaps are generally acceptable; stride-induced gaps are allowed through AvgPool; propagation through Conv/GEMV is allowed only when it does not increase N [16].

DSC shall preserve that legality/profitability boundary but connect it to the selected MetaKernel/Fhelipe planner. The fusion pass estimates logical gap density and whether a rewrite changes the minimum feasible N. The later layout planner refines ciphertext count, packing density, rotation count, key-switch count, and backend cost. At -O2, when advanced FHE optimization is enabled, ReSBM and any extended ReSBM variant may consume the selected target architecture description. That description supplies machine-verifiable capabilities and costs such as HBM traffic, batched rotation/NTT latency, bootstrap and key-switch cost, available parallel resources, and kernel-fusion benefit while the high-level FHE graph remains independent of a runtime library. The target description is an input to -O2 legality and profitability; it does not move ReSBM into -O3. -O3 consumes the finalized -O2 semantic plan and applies separate parallelization and memory-hierarchy transformations.

### 8.9 Optimization-Level Policy

| Optimization level | Canonicalization / fusion policy |
| --- | --- |
| -O0 | Mandatory FHE graph canonicalization only: expose Masking/Strided_Slice, normalize polynomial activation and AvgPool/Conv forms, compute slot-validity/gap state. Do not move, merge, or eliminate cleanup nodes except identities required for correctness. |
| -O1 | Apply local, obviously depth-reducing associative simplifications (e.g., adjacent scalar folding, RELU+Masking, adjacent Strided_Slice) with local legality checks; no graph-wide commutative traversal. |
| -O2 | Advanced FHE optimization. Enable the full FHEFusion-style graph algorithm, target/backend-sensitive profitability, ReSBM planning including future extended variants, and related global transformations. ReSBM obtains required capability and cost facts from the selected target architecture description. Every activity remains independently controllable; -dsc-fhe-advanced=off disables the optional -O2 FHE transformations without disabling mandatory legality passes. |
| -O3 | Preserve the existing Open64 -O3 role. Start from the selected -O2 result, then perform parallelization and memory-hierarchy optimization. Extend traditional CPU data-cache work to NUMA placement, ciphertext and polynomial working sets, GPU register/shared-memory/L2/HBM placement, transfer overlap, batching, tiling, and dependence-safe scheduling. -O3 does not define a more aggressive semantic FHE or ReSBM mode. |

This -O mapping is a DSC policy, not a claim about the ANT-ACE command-line interface. The mandatory -O0 canonicalization is intentionally separated from optimization so that the unoptimized compiler still exposes junk/gap semantics correctly to later layout and CKKS passes.

### 8.10 Required IR Records and Diagnostics

| Record / diagnostic family | Purpose |
| --- | --- |
| FHESlotValidityIR | Tracks valid, junk, guaranteed-zero, and gap positions plus Gap_Strides and cleanup provenance. |
| FHEMaskDescriptorIR | Represents derivable 0/1 plaintext masks, shape compatibility, and constant-folding identity. |
| FHEStridedSliceIR | Represents begin/end/strides plus inherited Gap_Strides and compaction provenance. |
| FHEFusionRuleIR | Registers rule name, algebraic class, pattern, predicates, attribute-update function, benefit class, and priority. |
| FHEFusionReportIR | Records canonicalization, applied/rejected fusions, depth delta, mask/compaction removal, gap-density change, N impact, and target cost. |
| CFHEFUSE-* | NonCanonicalFHEGraph, MaskDerivationFailed, SliceBoundaryMismatch, JunkSlotMayBeObserved, GapStateLost, FusionWouldIncreaseRingDimension, RulePreconditionFailed. |

The output of this section is a canonical, FHE-aware CNN graph whose cleanup semantics are explicit and whose profitable algebraic simplifications have already been applied. Only then does DSC proceed to multi-level WHIRL lowering and the selected MetaKernel or Fhelipe encrypted-layout planner.

## 9. Multi-Level WHIRL Lowering

### 9.1 Required levels

| Level | Representative form | Purpose |
| --- | --- | --- |
| L0 Application intent | secure inference of CNN + FHECompilationConfigIR | Captures user goal, encryption/trust boundary, scheme selection, ring dimension N, security target, and requested CKKS configuration before packing. |
| L1 CNN semantic WHIRL | cnn.conv2d, cnn.resnet.basic_block, common.residual_add | Preserves original CNN/ResNet structure and contracts. |
| L2 FHE CNN semantic WHIRL | cnn_fhe.conv2d, cnn_fhe.poly_activation, encrypted_logits | Represents the legal transformed model after BatchNorm folding, activation adaptation, and cipher/plain classification. |
| L3 FHE-aware graph optimization | FHEFusion-style fusion groups, depth annotations, residual-path transforms | Reduces multiplicative depth and preserves high-level opportunities before vector expansion. |
| L4 Encrypted tensor/vector WHIRL | EncryptedTensorLayoutIR + selected MetaKernelPlanIR or FhelipePackingPlanIR + PackingPlanIR / FHEPermutationPlanIR | Runs one selected encrypted layout/packing method, then emits a common ciphertext layout, rotation, and key-requirement interface. |
| L5 Scheme-independent HE WHIRL | Cipher, Plain, ClearVector; sihe.rotate/add/sub/mul/neg/encode | Separates generic encrypted computation from CKKS-specific scale, level, and RNS semantics. |
| L6 CKKS WHIRL | ckks.rotate/mul/rescale/bootstrap + CKKSResolvedParameterIR + CKKSValueStateIR | Owns resolved CKKS modulus/scale/depth/key state, per-value scale/level, ReSBM planning, and bootstrap semantics. |
| L7 HPOLY optimization WHIRL | poly.modup, dotprod, moddown, extend, add_ext, mul_ext, fast_mul | Exposes polynomial sub-operations hidden inside CKKS primitives while preserving Level, Scale, Extended-basis, and ct×ct/ct×pt state for global optimization. |
| L8 Optional POLY/RNS WHIRL | NTT/iNTT, RNS basis conversion, decomposition, key-switch internals, modular arithmetic | Provides native accelerator-oriented lowering after HPOLY optimization; may be bypassed by a library backend. |
| L9 Standard middle-WHIRL | OPR_CALL, standard control flow, structs, constants | Provides a whirl2c-compatible artifact after FHE-specific lowering. |
| L10 Generated C | dsc_fhe_* C ABI calls | Provides portable source compilation and runtime linkage. |

### 9.2 Two implementation modes

| Mode | Description | Use |
| --- | --- | --- |
| Library-call MVP | Lower each supported FHE CNN operation to a relatively high-level C ABI call such as dsc_fhe_conv2d_plain or dsc_fhe_poly_eval. | Fastest integration; validates Python -> WHIRL -> C -> library. |
| Primitive-plan mode | Lower CNN ops into rotate, multiply-plaintext, add, rescale, relinearize, bootstrap, and layout-conversion calls. | Enables Fhelipe-style planning and GPU-specific optimization. |
| Fused backend mode | Recognize primitive groups and emit backend-specific fused plan handles or calls. | Later GPU performance path. |

### 9.3 whirl2c compatibility rule

The default integration should not modify whirl2c to understand every new FHE opcode. Instead, the FHE lowering pass converts all domain and FHE-specific nodes into standard WHIRL calls and data structures. Extending whirl2c is reserved for presentation quality or a small stable intrinsic set; it is not required for the MVP. Open64 documents whirl2c as the WHIRL-to-C converter, which makes this a practical source-emission boundary [4].

## 10. Encrypted Tensor Layout Planner Selection MetaKernel or Fhelipe

### 10.1 Role in the architecture

The encrypted tensor/vector stage supports two alternative planning strategies selected by compiler option. Both consume the same gatekeeper-approved FHE-CNN graph, tensor and encryption descriptors, ring dimension, backend capabilities, and option set. MetaKernel analyzes Conv/MVM decomposition and batching, immediately transforms the kernel iteration space, and then derives packing and masks. Fhelipe preserves the logical tensor graph while performing graph-wide layout assignment, dimension-bit mapping, interleaving, compaction, and conversion selection; it materializes the resulting one-dimensional CKKS schedule only after those global decisions. Both modes emit the same common EncryptedTensorLayoutIR, EncryptedIterationSpaceIR, rotation schedule, gap/mask description, and key-requirement interface. CKKS scale and bootstrap placement remain later CKKS/ReSBM responsibilities.

### 10.2 Encrypted tensor after layout planning

```text
EncryptedTensorLayoutIR {
  tensor_id
  logical_shape: [N, C, H, W]
  semantic_role: encrypted_feature_map
  scheme_config_id
  ciphertext_count
  slot_count_per_ciphertext
  layout_bits: [dimension_bit | gap_bit ...]
  ciphertext_partition_bits
  slot_map_formula
  gap_bits
  packing_density
  residual_alignment_group
  rotation_schedule_id
  backend_batch_group
}

// CKKS level/scale/basis/precision state is carried separately by
// CKKSResolvedParameterIR and CKKSValueStateIR, not by generic layout IR.
```

### 10.3 Selectable MetaKernel and Fhelipe planning modes

| Planner / shared stage | Method | DSC representation / comparison rule |
| --- | --- | --- |
| MetaKernel mode | Analyze IMRA-style decomposition, horizontal/vertical batching, rotation cost, and output compatibility; transform the Conv/MVM iteration space immediately; then derive packing and masks for that transformed space. | Emit MetaKernelPlanIR plus the common EncryptedTensorLayoutIR, EncryptedIterationSpaceIR, RotationScheduleIR, gap/mask records, and key requirements. |
| Fhelipe mode | Preserve the logical graph while globally assigning dimension-bit/interleaved layouts; choose compaction, conversion hoisting, and permutation decomposition; then materialize the selected one-dimensional CKKS schedule. | Emit FhelipePackingPlanIR plus the same common EncryptedTensorLayoutIR, EncryptedIterationSpaceIR, RotationScheduleIR, gap/mask records, and key requirements. |
| Shared comparison contract | Both planners consume one frozen FHE-CNN image and identical scheme, backend, fusion, bootstrap, scale, and optimization controls. | Compare recomputed static and execution-weighted rotations, unique offsets, total/active/gap slots, gap ratio, compaction, conversions, masks, ciphertexts, packing density, depth effect, memory, estimated cost, and runtime. |
| Scale/bootstrap handoff | Neither layout planner finalizes CKKS scale/level/bootstrap placement. | Pass the selected transformed graph to SIHE/CKKS, then ReSBM and HPOLY/HPAO. |

### 10.4 GPU-aware cost model

```text
GPUFHECostModel =
    NTT_iNTT_cost
  + RNS_base_conversion_cost
  + automorphism_and_key_switch_cost
  + bootstrap_cost
  + ciphertext_memory_traffic
  + host_device_conversion_cost
  + kernel_launch_cost
  - batching_benefit
  - kernel_fusion_benefit
```

The selected planner shall use backend-supplied cost tables and capability records. Compile the same model once with MetaKernel and once with Fhelipe under identical scheme/backend and optimization options. Compare both the planner decision image and the materialized CKKS-vector image. Rotation and gap counts must be recomputed from the materialized WHIRL rather than accepted only from a planner estimate. Bootstrap placement remains a CKKS/ReSBM concern rather than a layout-planner decision [2][6][8].

### 10.5 Common transformation census

After planner transformation is materialized and before ReSBM or later CKKS scheduling changes the graph, the compiler shall publish a deterministic census. It records static rotation-operation count, execution-weighted rotation count when trip counts are known, unique signed rotation offsets, total CKKS slots, active logical slots, gap or invalid slots, peak and introduced gaps, gaps removed by compaction, slots cleared by masks, ciphertext count, packing density, layout conversions, permutations, masks, and rotate-add reductions. Counts are emitted per value, operator, PU, and program and retain stable source and logical DSL identities.

> **Architecture note:** The canonical aggregate is gap_slots = total_slots - active_slots. Padding and replicated values are reported separately and do not count as active logical slots. Static and execution-weighted rotation totals are distinct metrics. A common verifier recomputes the census from the materialized image and rejects disagreement with planner-reported values.

### 10.6 SYNC 7 layout planner gates

| Gate | Analysis and transformation | Required retained evidence |
| --- | --- | --- |
| SYNC-7A Common baseline | Freeze the validated FHE-CNN graph, descriptors, ring dimension, backend manifest, and options. | One pre-layout B and T image and one manifest shared unchanged by both planners. |
| SYNC-7B MetaKernel | Select kernel decomposition and batching, transform the iteration space, then derive packing and masks. | Original and transformed domains, decomposition, packed layout, rotation schedule, and verified census. |
| SYNC-7C Fhelipe | Assign graph-wide layouts, select compaction and conversions, then materialize the CKKS schedule. | Layout decisions, compaction and conversion provenance, materialized schedule, and verified census. |
| SYNC-7D Normalized comparison | Canonicalize both results into the common encrypted layout and iteration-space interface and prove semantic equivalence. | Stable per-value, per-operator, per-PU, and whole-program rotation and gap comparison. |
| SYNC-7E Selected-plan handoff | Select under reviewed policy and pass the common plan to SIHE, CKKS, ReSBM, and HPOLY. | Selection rationale, fallback proof, and downstream B and T checkpoints. |

## 11. SIHE-to-CKKS Scale, Level, and Bootstrap Management

### 11.1 Architectural Role

This stage sits after encrypted tensor/layout planning and before HPOLY. SIHE carries scheme-independent Cipher/Plain semantics and explicit bootstrap boundaries, including the boundary created for every surviving common.relu in the first ResNet-20 path. SIHE-to-CKKS lowering commits the program to CKKS, introduces CKKS-specific ciphertext forms such as the three-component result of ciphertext×ciphertext multiplication, inserts relinearization where required, and creates the state on which scale, level, rescale, modulus-switch, and bootstrap management operate. Scale/level management and ReLU bootstrap materialization are mandatory for correctness at -O0; profitability-driven placement and global bootstrap planning are optimizations.

### 11.2 Findings from the Current ANT-ACE Implementation

The current ANT-ACE source separates scheme lowering, local scale management, and ReSBM. The following findings are used as implementation evidence, but the -O-level policy defined in Section 11.4 is a DSC design decision; ANT-ACE exposes CKKS feature options rather than a standard -O0/-O2 mapping for these passes.

| ANT-ACE source area | Observed behavior | DSC design implication |
| --- | --- | --- |
| SIHE generator and SIHE→CKKS lowering | SIHE has an explicit bootstrap operator. SIHE bootstrap lowers to CKKS.bootstrap. SIHE ciphertext×ciphertext multiply lowers to CKKS.mul producing a three-component ciphertext followed by CKKS.relin; ciphertext×plaintext multiply remains a normal ciphertext. | Keep bootstrap representable at SIHE, but make scale/level state CKKS-specific. Treat Relin after ct×ct multiply as mandatory canonical lowering, not an optional optimization. |
| CKKS local scale manager | Scale management tracks scale degree and rescale level. When ReSBM is not doing joint region scale/bootstrap management, local managers are used. ACE_SM is the default entry-function policy when EVA/PARS/ReSBM are not selected; callees fall back to EVA waterline management for correctness. | DSC needs a mandatory local legality manager that runs even at -O0. It owns scale propagation, immediate/local Rescale placement, level matching, and required ModSwitch operations. |
| ACE_SM / PARS behavior | PARS performs rescale analysis and scale matching; ACE_SM is a waterline-derived local policy that tries to avoid rescale inside loops and may place rescale before expensive Rotate/Relinearize operations. | Treat these as optimized local policies. -O0 should use deterministic legality placement rather than motion/hoisting heuristics. |
| ReSBM input contract | ReSBM requires fixed scale/level state, requires problematic scale-growing loops to be unrolled/inlined, and explicitly removes previously inserted bootstrap/scale-management operations before replanning. | Only invoke ReSBM after CKKS canonicalization and analysis. Manual user bootstrap mode must bypass ReSBM bootstrap replanning unless the user explicitly permits it. |
| ReSBM planning | The DFG is partitioned into regions of multiplicative depth one. Dynamic programming considers later regions within the configured bootstrap-level window and uses min-cut to determine minimal-latency scale/bootstrap management. | Represent the result as a CKKSScaleBootstrapPlanIR rather than immediately losing the decision in runtime calls. |
| ReSBM insertion | The selected plan materializes bootstrap and rescale points. Bootstrap result level is carried per planned point; a bootstrap-only region mode skips ReSBM rescale insertion. The implementation also handles bypass edges and avoids redundant planned bootstraps. | Separate analysis/planning from materialization. The inserter consumes the plan and emits CKKS.rescale / CKKS.bootstrap nodes. |
| CKKS option defaults | The source exposes max bootstrap result level 16, minimum 5, maximum bootstrap count per region 3, and an option to set bootstrap result level to actual consumed value. Region scale/bootstrap management is separately selectable. | Capture these as advanced CKKS policy fields. Do not make them implicit properties of a TensorDescriptorIR. |

### 11.3 Scheme-State Propagation Through SIHE and CKKS

Section 6.4 remains the single source of truth for encryption/type propagation. The table below is a different table: it is the single source of truth for CKKS scheme-state propagation after the value has already been classified as Cipher or Plain. Implementations should not duplicate these transfer functions in ReSBM; ReSBM consumes the state computed by this analysis and chooses better placement points.

| Operation | State effect | Mandatory CKKS action / invariant |
| --- | --- | --- |
| SIHE add / sub | No scheme-specific state is committed at SIHE. After CKKS lowering, output remains ciphertext. | Ciphertext operands at a CKKS add/sub must have compatible scale and level. Insert only the normalization required by the selected scale policy. |
| SIHE mul: Cipher × Plain | Produces Cipher. At CKKS, multiplication raises scale degree and may consume a level when rescaled. | Propagate resulting scale degree; encode the plaintext at a compatible scale/level; Rescale when required by the policy. |
| SIHE mul: Cipher × Cipher | Produces Cipher. CKKS multiplication initially creates a three-component ciphertext and increases scale degree. | Emit CKKS.mul followed by mandatory CKKS.relin; propagate scale, then Rescale according to policy. |
| SIHE rotate | Produces Cipher; logical scale/level is unchanged by rotation. | Lower to CKKS.rotate. An optimized local policy may choose to Rescale before Rotate, but -O0 does not move a Rescale merely for profitability. |
| SIHE encode | Produces Plain. | Choose encoded plaintext level/scale to match the consuming ciphertext operation; static weights may later be pre-encoded by HPAO-FM. |
| SIHE bootstrap | Explicit semantic refresh boundary. | Preserve as CKKS.bootstrap during SIHE→CKKS lowering unless a permitted global planner is going to remove and replan it. |
| CKKS rescale | Reduces scale and consumes modulus-chain level(s). | Update CKKSValueStateIR.scale and level/rescale_level immediately. |
| CKKS modswitch | Moves a value down the modulus chain without changing the intended numerical scale. | Use to establish level compatibility when legal; never use it to raise a ciphertext to a higher level. |
| CKKS relin | Converts the three-component multiply result back to the normal ciphertext representation. | Preserve scale/level; update component-count state. |
| CKKS bootstrap | Refreshes a ciphertext and restores a configured/planned level budget. | Record the exact result level and normalized scale in CKKSValueStateIR; key and slot requirements become deployment obligations. |

### 11.4 Normative DSC Policy for -O0 (No Optimization)

The DSC definition of -O0 is "no profitability optimization," not "skip FHE correctness management." A CKKS program is not executable merely because high-level optimization is disabled. For the first ResNet-20 implementation, every surviving common.relu is a mandatory refresh boundary under the default auto/on bootstrap policy. Therefore the following legality pipeline is mandatory at -O0.

1. Preserve every explicit SIHE bootstrap boundary exactly as written by FHE model adaptation or by a manual source declaration. Under bootstrap=auto or on, FHE model adaptation must create one such boundary immediately before every surviving common.relu polynomial approximation.

2. Perform canonical SIHE→CKKS lowering. Cipher×Cipher multiply emits CKKS.mul plus mandatory CKKS.relin; Cipher×Plain uses the ordinary ciphertext result form.

3. Run a forward CKKS scale/level propagation pass and attach CKKSValueStateIR to every ciphertext-producing value.

4. Insert only mandatory local normalization: Rescale at the first point required to keep the next operation legal, and ModSwitch only to establish legal operand level compatibility. Do not hoist, sink, merge, or speculate on Rescale placement.

5. With -dsc-fhe-bootstrap=manual, insert no compiler-created bootstrap. Every surviving common.relu must already have an explicit bootstrap boundary; otherwise compilation fails. Any other execution path that exhausts the legal multiplicative-level budget before the next explicit bootstrap also fails with a diagnostic.

6. With -dsc-fhe-bootstrap=off, bootstrap is forbidden. A surviving common.relu is rejected by the first-release CKKS policy, and compilation also fails if any other path cannot execute within the resolved modulus chain without refresh.

7. With -dsc-fhe-bootstrap=auto or on, first materialize the mandatory pre-ReLU bootstrap boundaries. For all other depth pressure, use a deterministic greedy just-in-time rule: immediately before the first depth-consuming operation whose operand lacks sufficient remaining level, insert CKKS.bootstrap. Every -O0 bootstrap result uses the configured safe result level; do not perform minimal-result-level optimization.

8. At a multi-ciphertext join such as a residual add, all operands must arrive with compatible scale and level. If a just-in-time refresh is required and a low-level peer cannot be raised by ModSwitch, refresh the peer as well to the same configured result level, then apply only the required normalization. This deliberately favors deterministic correctness over bootstrap count.

9. Do not remove, move, merge, deduplicate, or globally re-place ReLU or just-in-time bootstrap nodes at -O0. Those transformations belong to optimized scale/bootstrap management and require preserved common.relu semantics, approximation equivalence, CKKS state legality, and reviewable provenance.

> **Architecture note:** Rationale: the inspected ANT-ACE source shows that explicit SIHE bootstrap survives SIHE→CKKS lowering and that local CKKS scale managers are distinct from ReSBM. DSC adds a deliberately conservative ResNet-20 correctness baseline: each common.relu creates an explicit refresh boundary before polynomial evaluation, while local scale/level management inserts additional refresh only when required. This policy is simple to inspect at -O0 and gives optimized planners an unambiguous baseline to improve.

### 11.5 Bootstrap Policy and Optimization-Level Mapping

| DSC mode | Scale/level policy | Bootstrap insertion policy | What is deliberately disabled |
| --- | --- | --- | --- |
| -O0 | Mandatory deterministic local legality manager. Immediate required Rescale/ModSwitch; mandatory Relin. | manual: preserve explicit ReLU boundaries only; off: reject surviving ReLU; auto/on: mandatory pre-ReLU bootstrap plus greedy just-in-time refresh to the configured safe result level. | ReSBM, min-cut/DP planning, bootstrap-level minimization, global rescale motion, ReLU-boundary movement/fusion, and HPAO profitability rewrites. |
| -O1 | Local optimized scale placement may use ACE_SM/EVA/PARS-like heuristics while preserving the same scheme-state transfer functions. | The -O0 ReLU boundaries remain the baseline for auto/on. A local optimized policy may change them only with explicit legality, approximation-equivalence, and provenance checks. | No mandatory global DFG scale/bootstrap search. |
| -O2 | Advanced global joint scale/bootstrap planning is available for scale-policy=auto/resbm when bootstrap policy permits replanning and -dsc-fhe-advanced is enabled. This -O2 ownership includes future extended ReSBM variants. The selected target architecture description and measured profile data provide required capability, resource, and latency inputs for legality and profitability. | ReSBM or an extended ReSBM variant may replace provisional ReLU/JIT bootstrap operations with a global plan only when it preserves common.relu approximation semantics, scale/level legality, target capability constraints, and source-visible evidence. | Manual boundaries remain authoritative. The advanced or individual ReSBM option can disable global replanning and retain the local correctness plan. |
| -O3 | Use the finalized plan selected by the enabled -O2 or lower-level policy. Then run Open64-style parallelization and memory-hierarchy optimization; do not create a distinct or more aggressive ReSBM plan space. | Preserve the finalized bootstrap boundaries while scheduling parallel ciphertext, polynomial, NTT, and key-switch work and optimizing CPU/GPU data placement and movement. | Parallel and memory transformations may be turned off independently. They cannot change approximation, scale/level, manual-boundary, or source-provenance contracts. |
| FHE graph canonicalization gate | Masking/Strided_Slice preconditions are derivable; no observable consumer reads junk slots; Gap_Strides and slice boundaries remain consistent after rewrites. |  |  |

> **Architecture note:** Optimization-level rule: The main DSC architecture owns the shared -O3 umbrella for both openpy and opencc. -O2 owns advanced FHE semantic and profitability optimization, including ReSBM, future extended ReSBM variants, and their use of the selected target architecture description. These activities remain optional under explicit controls. FHE contributes target-aware parallelization and memory activities beneath the shared -O3 pipeline; it does not redefine that level and must not recompute or extend the finalized ReSBM plan. For FHE, this extends beyond CPU data-cache locality to ciphertext-level and polynomial-level concurrency, vector packing working sets, NUMA placement, GPU register/shared-memory/L2/HBM placement, host/device transfer overlap, and dependence-safe batching and tiling.

### 11.6 ReSBM Integration Contract

When ReSBM or an extended ReSBM variant is selected at -O2, DSC first canonicalizes CKKS state and builds SSA/DFG information. The planner consumes the selected target architecture description through a stable capability and cost interface; target facts guide legal and profitable plan selection without becoming semantic tensor-type identity. In auto mode, provisional local Rescale/Bootstrap nodes may be removed so the planner sees the arithmetic/rotation graph rather than a previously chosen placement. ReSBM then partitions the graph into multiplicative-depth-one regions, computes rescale cuts and bootstrap boundaries, and produces a plan that is materialized only after analysis.

```text
CKKSScaleBootstrapPlanIR {
  scale_policy: local | eva | pars | resbm
  bootstrap_policy: manual | off | greedy | resbm
  region_id_by_value
  multiplicative_depth_by_region
  rescale_cut_points
  bootstrap_points { value_id, result_level, reason }
  bypass_edge_bootstraps
  formal_scale_level_by_callsite
  maximum_bootstrap_result_level
  minimum_bootstrap_result_level
  max_bootstrap_count_per_region
  estimated_latency
  provenance: O0_legality | local_heuristic | ReSBM
}
```

The materialization pass consumes CKKSScaleBootstrapPlanIR and emits CKKS.rescale, CKKS.modswitch, and CKKS.bootstrap nodes. HPOLY/HPAO then runs on the finalized non-bootstrap CKKS regions. This keeps the HPAO v0.3 design boundary intact: bootstrap remains opaque to HPOLY in the first implementation.

### 11.7 Compiler Option Mapping

| Option | Meaning | Default mapping |
| --- | --- | --- |
| -dsc-fhe-ckks-scale-policy=auto\|local\|eva\|pars\|resbm | Selects local or global CKKS scale/level planning. | auto -> local legality at -O0; optimized local policy at -O1; optional ReSBM, including extended variants, at -O2 when bootstrap and advanced controls permit. ReSBM consumes the selected target architecture description. -O3 inherits that finalized plan before parallel and memory optimization. |
| -dsc-fhe-bootstrap=manual | Explicit SIHE bootstrap boundaries are authoritative. | No compiler-created bootstrap. Every surviving common.relu requires an explicit boundary; missing boundaries and insufficient-depth paths are errors. |
| -dsc-fhe-bootstrap=off | Bootstrap is prohibited. | Bootstrap is prohibited. A surviving common.relu is rejected in the first CKKS release, and every remaining path must execute without refresh. |
| -dsc-fhe-bootstrap=auto | Compiler may insert bootstrap when required. | -O0 inserts one boundary before every common.relu and greedy JIT refresh elsewhere. Enabled -O2 advanced planning may use ReSBM after proof. -O3 preserves the selected bootstrap plan while optimizing parallel execution and memory behavior. |
| -dsc-fhe-bootstrap=on | Compiler insertion is permitted and backend bootstrap capability is required. | Compiler insertion and backend bootstrap support are required. -O0 preserves one boundary per common.relu; optimized planners may re-place only with proof and provenance. |
| advanced: max/min bootstrap result level, max bootstrap count | Constrains ReSBM and O0 safe-result-level selection. | Captured in CKKSBootstrapConfigIR / CKKSResolvedParameterIR, not in generic tensor type state. |
| CFHEFUSE-* | NonCanonicalFHEGraph, MaskDerivationFailed, SliceBoundaryMismatch, JunkSlotMayBeObserved, GapStateLost, FusionWouldIncreaseRingDimension. |  |
| -dsc-fhe-advanced=auto\|on\|off | Master control for optional advanced -O2 FHE planning and transformation. | auto enables at -O2/-O3; off retains mandatory legality and lower-level plans. Individual phase controls may further restrict enabled work. |
| -dsc-fhe-parallel=auto\|on\|off | Controls the -O3 parallelization stage. | auto enables at -O3 only; on/off permits isolated validation and triage. |
| -dsc-fhe-memory-opt=auto\|on\|off | Controls the -O3 memory-hierarchy stage. | auto enables at -O3 only; on/off permits isolated CPU/GPU memory experiments. |

### 11.8 Architectural Rule

Type propagation, scheme-state propagation, and placement optimization are three distinct compiler responsibilities. Section 6.4 determines Cipher/Plain type. Section 11.3 computes CKKS scale/level transfer functions. Section 11.4 guarantees an executable correctness baseline at -O0. ReSBM is allowed to replace that provisional placement only when optimization and bootstrap policy permit it. HPOLY/HPAO must consume the finalized CKKS scale/bootstrap schedule rather than attempting to rediscover bootstrap placement.

## 12. HPOLY and HPAO Polynomial-Level Optimization

### 12.1 Architectural Motivation

The new HPAO work identifies an abstraction gap directly relevant to DSC. CKKS operations such as Rotate, Multiply, and Relinearize are too coarse because they hide KeySwitch sub-operations; the existing low-level POLY/RNS form is too expanded because limb-wise loops obscure global polynomial structure. DSC therefore introduces HPOLY between CKKS WHIRL and optional POLY/RNS WHIRL. HPOLY keeps polynomials as first-class values while exposing basis-management operations such as ModUp, DotProd, ModDown, and Extend.

### 12.2 Position in the DSC Pipeline

HPOLY is downstream of all CNN/FHE, packing, SIHE, CKKS scale/level, and ReSBM decisions. It is intentionally independent of DNN semantics. This lets the same polynomial optimization layer serve ResNet today and other RNS-CKKS applications later.

```text
CNN/FHE Semantic WHIRL
  -> Encrypted Tensor/Vector WHIRL
  -> SIHE WHIRL
  -> CKKS WHIRL + finalized scale/bootstrap plan
  -> HPOLY WHIRL + HPAO
     -> Library backend lowering
     OR
     -> POLY/RNS WHIRL -> native GPU lowering
```

### 12.3 Canonical HPOLY Operations and Static State

| Category | Representative HPOLY operations | Purpose |
| --- | --- | --- |
| Polynomial arithmetic | poly.add, poly.sub, poly.mul, poly.rotate, poly.rescale | Keep whole polynomials explicit instead of immediately expanding into RNS-limb loops. |
| Extended-basis arithmetic | poly.add_ext, poly.sub_ext, poly.mul_ext, poly.rotate_ext | Represent operations on the extended basis used around KeySwitch and delayed ModDown. |
| KeySwitch decomposition | poly.modup, poly.dotprod, poly.moddown | Expose the ModUp-DotProd-ModDown structure hidden inside CKKS Rotate/Relinearize/Multiply. |
| Basis conversion | poly.extend | Lift a polynomial to the extended basis without decomposition when rewrites require basis consistency. |
| Plaintext encoding | poly.encode; derived poly.const_encode | Represent runtime encoding and enable compile-time replacement for static weights. |
| Derived optimized operations | poly.fast_mul, add/mul no-mod variants | Materialize profitable specialization after legality and cost analysis. |

Each HPOLY node should carry CKKS-derived static attributes: Level, Scale, Extended-basis state, and whether multiplication is ciphertext×ciphertext or ciphertext×plaintext. These attributes become legality predicates for polynomial rewrites rather than being implicit runtime assumptions.

### 12.4 HPAO Passes Adopted by DSC

| Pass | Core transformation | DSC adoption |
| --- | --- | --- |
| HPAO-MU: ModUp hoisting | Detect identical ModUp operations on the same polynomial and share/hoist them to a dominating point. | Implement a dedicated HPOLY HPAO-MU phase using the SSAPRE algorithmic model to perform GVN/CSE-equivalent redundancy elimination for pure polynomial operations. Preserve level, scale, basis, decomposition, and effect attributes in equivalence tests. The existing WOPT SSAPRE implementation remains unchanged; HPOLY specialization belongs to the new phase. |
| HPAO-MD: ModDown sinking | Commute ModDown through legal operations, promote them to _ext variants, then merge multiple reductions at convergence points. | Design TBD. The intended transformation remains profitability-aware ModDown sinking and merging, but legality, extended-basis lifetime, placement, interaction with control flow, and target cost modeling require a separate design review before implementation. Fewer ModDowns are not automatically better because extended-basis arithmetic is more expensive. |
| HPAO-FM: static encoding + fast multiply | Replace runtime encoding of constant weights with compile-time encoded polynomials and specialize ct×pt multiplication. | Integrate with DSC model-package generation and preencoded-weight caches; this is especially natural because the first DSC threat model keeps weights plaintext. |
| HPAO-LM: lazy modular reduction | Use forward/backward bit-width analysis to defer modular reduction while machine-word safety is provable. | Represent admissible and estimated bit width in HPOLY analysis state; use explicit reduction when safety cannot be proved. |

### 12.5 Rule Engine and Open64 Analysis Reuse

HPAO represents rewrites as pattern/predicate/action rules and applies Commutative rules first, then Distributive rules, then Equivalent replacements, with copy propagation, CSE, and DCE between stages. DSC should implement the same semantic rule classes over HPOLY WHIRL. HPAO-MU shall run as a new, independently controlled HPOLY optimization phase that follows the SSAPRE algorithmic model for value numbering, redundancy discovery, placement, and elimination. It may reuse stable, non-invasive Open64 analysis services where appropriate, but it shall not modify the existing WOPT SSAPRE implementation or route HPOLY operators through that legacy phase. This isolation preserves WOPT behavior while allowing polynomial-specific equivalence, basis-state, and profitability rules. HPAO-MD remains a separate design item: its exact analysis, legality proof, transformation order, and profitability interface are TBD and must be approved before coding.

### 12.6 Profitability Model and GPU Retargeting

The paper uses measured CPU latency tables to decide whether ModDown sinking is profitable. DSC should make this cost table a backend capability artifact. For GPU targets the model must account for extended-basis limb count, batched NTT/iNTT, key-switch batching, HBM traffic, ciphertext batch occupancy, kernel launch overhead, and fusion opportunities. This allows the same HPOLY legality rules to make different profitability decisions on OpenFHE CPU, FIDESlib/Cheddar-like GPU libraries, and a future native CUDA/Triton backend.

### 12.7 Static Weight Encoding Becomes a Deployment Artifact

HPAO-FM is particularly important for the DSC architecture because model weights are plaintext and compile-time constant in the initial threat model. The compiler should pre-encode eligible weights once, store parameter-set-specific encoded polynomial objects in the model package, and let the runtime load or upload them directly. The paper reports that this shifts substantial work from inference to offline compilation; DSC should treat that trade as intentional deployment-time precomputation, not as compiler overhead to eliminate at all costs.

### 12.8 Bootstrap Boundary

The current HPAO paper deliberately bypasses bootstrap: bootstrap remains an opaque library call, and only non-bootstrap CKKS regions lower through HPOLY. DSC should adopt the same boundary for the first HPOLY implementation. Section 11 finalizes bootstrap placement at CKKS level: the -O0 correctness policy uses explicit/manual or greedy just-in-time insertion, while optimized auto mode may replace the provisional placement with ReSBM. HPOLY optimizes the finalized regions between bootstraps. A later research phase may expose bootstrap stages and polynomial dependencies so HPAO-style transformations can cross bootstrap internals.

### 12.9 Evidence and Design Implication

On a single-threaded Xeon CPU, the paper reports up to 2.06× end-to-end speedup over ANT-ACE, with geometric-mean speedups of 1.75× for LReLU models and 1.36× for HReLU models. On workload-derived kernels, HPAO-ALL reports geometric-mean speedups of 2.61× for convolution, 4.23× for average pooling, and 2.95× for GEMM. The smaller HReLU benefit is explained by bootstrap consuming a larger fraction of runtime, reinforcing the architectural decision to keep HPOLY focused on non-bootstrap regions initially.

### 12.10 Architectural Rule

Do not lower CKKS primitives directly into RNS loops when cross-primitive optimization is desired. First lower the non-bootstrap CKKS region into compact HPOLY, perform global basis-management and polynomial optimization, and only then lower either to backend library calls or to explicit POLY/RNS operations for native GPU code generation.

## 13. whirl2c and C ABI Runtime

### 13.1 Why a C ABI is required

OpenFHE, FIDESlib, Cheddar, and most practical FHE runtimes expose C++ and/or CUDA object models. Directly emitting those C++ classes from whirl2c would couple generated source to unstable library details. The preferred boundary is an opaque C ABI implemented by a C++/CUDA wrapper library.

### 13.2 Opaque handle model

```text
typedef struct dsc_fhe_context_s*    dsc_fhe_context_t;
typedef struct dsc_fhe_model_s*      dsc_fhe_model_t;
typedef struct dsc_fhe_ciphertext_s* dsc_fhe_ciphertext_t;
typedef struct dsc_fhe_plaintext_s*  dsc_fhe_plaintext_t;
typedef struct dsc_fhe_keyset_s*     dsc_fhe_keyset_t;

int dsc_fhe_context_create(const dsc_fhe_context_desc_t*, dsc_fhe_context_t*);
int dsc_fhe_model_create(dsc_fhe_context_t, const dsc_fhe_model_desc_t*, dsc_fhe_model_t*);
int dsc_fhe_model_run(dsc_fhe_model_t, dsc_fhe_ciphertext_t input,
                      dsc_fhe_ciphertext_t* output);
void dsc_fhe_model_destroy(dsc_fhe_model_t);
void dsc_fhe_context_destroy(dsc_fhe_context_t);
```

### 13.3 Generated C shape

```text
int secure_resnet20_run(
    dsc_fhe_model_t model,
    dsc_fhe_ciphertext_t x,
    dsc_fhe_ciphertext_t *out) {

  dsc_fhe_ciphertext_t t1 = 0;
  dsc_fhe_ciphertext_t t2 = 0;

  DSC_FHE_CHECK(dsc_fhe_conv2d_plain(model, x, &k_conv1, &t1));
  DSC_FHE_CHECK(dsc_fhe_poly_eval(model, t1, &k_relu_poly, &t2));
  dsc_fhe_release_ciphertext(t1);
  /* additional generated calls */
  *out = t2;
  return DSC_FHE_OK;
}
```

### 13.4 Static descriptors and weight preparation

- Generated C contains static operation descriptors but not backend C++ object layouts.

- Plaintext weights may be emitted as external binary assets, model-package sections, or preencoded backend caches.

- The runtime may prepack/encode weights once per context or key parameter set.

- Key-dependent precomputation must be explicit in the model/runtime manifest.

- The generated entry point accepts opaque ciphertext handles rather than raw ciphertext byte layouts unless the service ABI defines serialization.

### 13.5 Two runtime ABI layers

| Layer | Purpose |
| --- | --- |
| Stable DSC FHE C ABI | Generated C calls this layer. It is versioned, backend-neutral, and uses opaque handles. |
| Backend adapter C++ API | Maps DSC descriptors and calls to OpenFHE, FIDESlib, Cheddar, mock, or other implementations. |
| Backend library | Owns cryptographic primitives, GPU kernels, streams, memory pools, and ciphertext objects. |

## 14. GPU Backend Architecture

### 14.1 Backend sequence

| Stage | Backend | Reason |
| --- | --- | --- |
| Stage 0 | Mock ciphertext runtime | Validates compilation, object lifetime, C ABI, driver, and service interface without cryptographic complexity. |
| Stage 1 | OpenFHE CPU/reference | Provides a broad, mature CKKS reference for correctness and bootstrapping experiments [5]. |
| Stage 2 | FIDESlib GPU | Preserves OpenFHE client interoperability and supplies optimized GPU CKKS primitives including bootstrapping; current project material also describes multi-GPU/NCCL support [6][7]. |
| Stage 3 | Cheddar-like CUDA backend | Provides a performance-oriented CUDA CKKS implementation with 32-bit arithmetic, optimized NTT/base conversion, and kernel fusion [8][9]. |
| Research | TensorFHE-style primitive batching | Investigates operation-level batching and Tensor Core/linear-algebra reformulation of NTT-heavy work [10]. |
| FHEFusion canonicalization and garbage-state analysis | Which junk/gap patterns should be mandatory canonical state versus planner-specific layout state? How should gap density, minimum feasible N, and GPU packing cost interact without coupling semantic fusion to one backend? |  |

### 14.2 Backend capability record

```text
FHEBackendCapabilityIR {
  backend_name
  abi_version
  schemes
  supports_bootstrap
  supports_rotation
  supported_ring_degrees
  supported_modulus_profiles
  supported_gpu_architectures
  max_device_memory
  multi_gpu_support
  operation_cost_table
  fused_operation_patterns
  serialization_format
  openfhe_interoperability
}
```

### 14.3 GPU runtime responsibilities

- Allocate and reuse ciphertext/RNS/NTT workspaces in GPU memory.

- Batch compatible rotations, key switches, multiplies, rescale operations, and bootstraps.

- Keep weights and evaluation keys resident when memory permits.

- Avoid repeated OpenFHE/FIDESlib host-device or object-format conversions.

- Expose asynchronous execution and synchronization through the DSC C ABI without leaking CUDA types into generated C.

- Report operation-level telemetry to the compiler cost-model database.

- Support a CPU fallback only when the certification policy allows mixed execution.

### 14.4 Backend selection rule

Backend selection is a compiler/runtime planning decision. The model author should not encode library-specific names in the CNN semantics. The driver may constrain the backend, but the legal plan must still satisfy scheme, bootstrapping, key, security, memory, and operation capability contracts. For optimized CKKS execution, backend selection also determines whether HPOLY is lowered to coarse library calls or further expanded into POLY/RNS operations for native accelerator code generation.

## 15. Gatekeeper, Security, and Diagnostics

### 15.1 Mandatory compile-time checks

| Gate | Required checks |
| --- | --- |
| Boundary gate | All declared encrypted inputs and outputs are represented consistently; no implicit plaintext escape. |
| Secret-key gate | No server function, generated C object, descriptor, or runtime call requires the secret key. |
| Operator gate | Every CNN operator has a supported FHE conversion or an explicit rejection. |
| Approximation gate | Every common.relu has an approved polynomial approximation, valid input range, error budget, and the required -O0 bootstrap boundary or reviewed fused equivalent. |
| Depth/level gate | The ResNet-20 circuit remains within the modulus/level budget after mandatory ReLU boundaries, or has a legal additional just-in-time/global bootstrap plan. |
| Layout gate | Residual and binary operands have compatible encrypted layouts or a legal conversion. |
| Key gate | Rotation, relinearization, and bootstrapping keys required by the plan are declared in the key manifest. |
| Backend gate | The selected backend supports the scheme, parameter profile, operations, target GPU, and memory requirement. |
| whirl2c gate | No unsupported custom opcode remains in the middle-WHIRL artifact. |
| Output gate | The exported value remains ciphertext and has a client-readable serialization/layout contract. |
| Scheme parameter gate | N/ring dimension is valid; active slots <= N/2; explicit depth covers required depth; scale/first-modulus policy is legal; resolved total modulus and secret distribution satisfy the requested security target; bootstrap/key-switch settings are backend-supported. |

### 15.2 Diagnostic families

| Family | Examples |
| --- | --- |
| CFHEBOUND-* | MissingCiphertextBoundary, PlaintextEscape, SecretKeyReferencedByServer |
| CFHEOP-* | UnsupportedEncryptedOperator, MaxPoolRequiresApproximation, DynamicCiphertextBranchUnsupported |
| CFHEAPPROX-* | ApproximationRangeMissing, AccuracyBudgetExceeded |
| CFHEPACK-* | MissingEncryptedTensorLayout, ResidualBranchLayoutMismatch, RequiredGaloisKeyMissing |
| CFHESCALE-* | ScaleMismatchAtResidualAdd, LevelMismatchAtBinaryOp, PrecisionBudgetInsufficient |
| CFHEBOOT-* | BootstrapPlanMissing, BackendBootstrapUnsupported, BootstrapCostModelMissing |
| CFHEGPU-* | GPUDeviceMemoryInsufficient, KernelBatchPlanInvalid, HostDeviceConversionDominates |
| CFHECABI-* | RuntimeABIVersionMismatch, UnsupportedDescriptorVersion, BackendLinkConfigurationMissing |
| CFHEW2C-* | HighLevelOpcodeReachedWhirl2C, UnsupportedIntrinsicReachedWhirl2C |
| CFHEPARAM-* | InvalidRingDimension, RingDimensionBelowSecurityRequirement, SlotCountExceedsNOver2, MultiplicativeDepthInsufficient, TotalModulusExceedsSecurityEnvelope, BootstrapConfigurationIncompatible. |

### 15.3 Runtime security policy

- The client owns and retains the secret key.

- The server receives ciphertexts and only the evaluation-key material required by the certified plan.

- Generated C and model packages do not contain secret-key APIs.

- Backend serialization and deserialization are versioned and validated.

- Model weights are plaintext by design in the first threat model; protecting model confidentiality is a separate problem.

- Temporary buffers and GPU memory pools should be scrubbed or recycled according to the runtime security policy.

- Logs and diagnostics must not dump ciphertext internals, keys, or client payloads unless an explicit secure-debug mode is enabled.

### 15.4 Mapped-Image Compatibility and Review Evidence

FHE extensions use the existing ELF mapped-image WHIRL path. Legacy binary WHIRL without FHE records remains valid. Images containing required unlowered FHE contracts must be recognized and verified by an aware phase or rejected with a stable diagnostic; they must never be silently compiled as ordinary WHIRL. No FHE implementation stage may add an ELF section, change a type-kind encoding, or expose a physical DSL carrier without a separate common/com compatibility review.

| Retained artifact | Required evidence |
| --- | --- |
| Source fixture | The exact Python or DSC source, including imported reachable definitions needed for -src interleaving and review. |
| Binary WHIRL | The finalized .B artifact produced across the frontend process boundary, with no Python dependency during compiler consumption. |
| ASCII WHIRL trace | ir_b2a -st -src input.B input.T, using the .B stem; show common.relu before conversion and the resulting bootstrap reason, polynomial activation contract, logical FHE names, descriptors, symbols, and source positions after conversion. |
| Planning reports | Conversion, layout, depth, bootstrap, backend capability, key requirements, accuracy, and certification reports linked to the same artifact family. |
| Lowering evidence | Requested phase traces and generated C after all FHE logical operators have lowered to standard WHIRL calls accepted by whirl2c. |
| Runtime evidence | Build/link diagnostics, backend selection, execution status, decrypted correctness comparison, latency, memory, transfer, and key-memory measurements. |
| External assets | Plaintext weights, encoded caches, descriptors, and manifests with deterministic names; never secret keys or unredacted ciphertext internals. |

Tests clean the designated host-visible artifact directory at the start of the next run, not at the end of the current run. Docker validation must publish through a host bind mount so reviewers can inspect the complete artifact family after the container exits.

## Part III. Execution Plan

## 16. Implementation Roadmap

| Phase | Deliverables | Exit criterion |
| --- | --- | --- |
| 0. Contract freeze | ResNet-20 source fixture/census, Python declarator, driver options, Tensor/Encryption descriptors, common.relu contract, C ABI header, and mock backend. | A design review approves the integration boundary. |
| 1. End-to-end mock path | Complete ResNet-20 capture, high-level WHIRL, FHE conversion skeleton, mandatory ReLU bootstrap markers, middle-WHIRL calls, whirl2c C, and mock runtime link. | The ResNet-20 graph reaches encrypted-handle output through generated C; operator-level add, linear, and ReLU tests isolate failures. |
| 2. CKKS reference path | OpenFHE backend adapter, context/key manifest, plaintext weight encoding, ResNet-20 add/mul_plain/rotate/rescale operations, polynomial ReLU, and bootstrap. | ResNet-20 reference execution matches the adapted plaintext model within the declared tolerance. |
| 3. FHE-compatible CNN conversion | Complete ResNet-20 BatchNorm folding, common.relu bootstrap-plus-polynomial conversion, average pooling, residual checks, classifier path, and conversion report. | The complete ResNet-20 model compiles and executes under the reference CKKS path. |
| 4. Encrypted tensor/vector planning | Selectable MetaKernel and Fhelipe planner implementations behind one common EncryptedTensorLayoutIR interface; common metrics/reporting for A/B comparison. | ResNet-20 compiles independently with each planner; each produces a legal plan and a comparable packing/rotation/cost report. |
| 5. CKKS + ReSBM planning | SIHE→CKKS lowering, scale/level analysis, minimum-level bootstrap placement, key requirement manifest. | ResNet-20 has a legal CKKS schedule and passes level/scale gatekeeping. |
| 6. HPOLY/HPAO optimization | HPOLY lowering; a dedicated SSAPRE-model HPAO-MU phase for ModUp GVN/CSE without changing WOPT SSAPRE; static weight encoding/fast multiply; lazy modular reduction; and a reviewed HPAO-MD design gate before ModDown sinking is implemented. | Non-bootstrap linear regions show verified HPAO-MU redundancy elimination and HPAO-FM/HPAO-LM improvements over the unoptimized CKKS→backend path. HPAO-MD implementation remains blocked until its design, legality, and profitability review is accepted. |
| 7. GPU library backend | FIDESlib/Cheddar-like adapter, GPU capability/cost model, memory pools, batching, telemetry. | ResNet-20 encrypted inference runs on NVIDIA GPU and passes conformance. |
| 8. Native GPU POLY/RNS backend | HPOLY→POLY/RNS lowering, NTT/RNS/key-switch primitives, fused GPU kernels, CUDA/Triton codegen. | Native path improves selected kernels over the library path with no contract regression. |
| 9. Productionization | Service packaging, concurrency, failure handling, CI certification, security review. | On-prem encrypted inference service meets declared latency, memory, accuracy, and security targets. |

### 16.1 Recommended first coding sequence

1.  Capture the complete ResNet-20 model with @dsc.fhe.entry metadata and verify its class-centric PUs, common/CNN operators, contracts, and source positions appear in the WHIRL dump.

2.  Add EncryptionDescriptorIR and FHEEntryContractIR to the mapped WHIRL bundle for every ResNet-20 boundary and intermediate tensor value.

3.  Add the -dsc-fhe option family, including ring dimension, security, depth, scale, first-modulus bits, scale policy, and bootstrap policy; create FHECompilationConfigIR.

4.  Create libdsc_fhe_cabi with a mock backend and opaque handles.

5.  Certify encrypted add, linear, and common.relu as focused unit tests. For ReLU, prove the -O0 bootstrap boundary and polynomial activation trace before using the same contracts in ResNet-20.

6.  Compile and link the generated C through the DSC driver.

7.  Add OpenFHE/reference CKKS execution.

8.  Implement SIHE→CKKS canonical lowering, CKKSValueStateIR propagation, mandatory -O0 bootstrap before every common.relu polynomial approximation, local scale/level management, and additional greedy/manual/off bootstrap policy; add source-linked dumps and diagnostics.

9.  Add ReSBM behind -O2/scale-policy=resbm and verify its plan against the -O0 correctness baseline.

10.  Complete the full ResNet-20 conversion and execution path, including convolution, folded batch norm, residual add, common.relu bootstrap-plus-polynomial lowering, pooling, classifier, and encrypted logits.

11.  Only after the full path works, implement MetaKernel/Fhelipe layout planners, HPOLY/HPAO, and GPU-specific optimization.

## 17. Benchmark and Validation Plan

| Benchmark stage | Workload | Purpose |
| --- | --- | --- |
| Integration smoke test | Encrypted vector add and plaintext multiply | Diagnostic unit evidence for descriptors, common.add/common.linear/common.relu contracts, mandatory ReLU bootstrap, C ABI, object lifetime, and whirl2c. |
| ResNet operator test | Conv2d + folded BatchNorm + common.relu + residual add | Isolates weight encoding, residual compatibility, ReLU bootstrap/approximation, level state, and accuracy before rerunning the full model. |
| Residual test | Two-convolution basic block with identity/projection shortcut | Validates branch scale/level/layout alignment. |
| Model test | Deterministic ResNet-20/CIFAR-10 fixture | First end-to-end model acceptance target; validates complete capture, conversion, lowering, generated C, and reference execution. |
| Primary benchmark | ResNet-20/CIFAR-10 | Matches the research literature and stresses packing, residual connections, polynomial activation, and bootstrapping. |
| Expansion benchmark | ResNet-18/ImageNet profile | Tests capacity, memory, and deeper-network planning after the vertical slice succeeds. |

### 17.1 Required reports

- Compilation report: operator coverage, rewrites, unsupported operations, and emitted runtime calls.

- Encryption report: scheme, ring degree, modulus chain, scale, security level, and key requirements.

- Layout report: ciphertext count, slot occupancy, gap/compaction decisions, rotations, and conversions.

- Depth report: multiplicative depth, level by node, bootstrap locations, and residual alignment.

- Accuracy report: plaintext model, adapted plaintext model, and encrypted result comparison.

- Runtime report: per-operation latency, GPU memory, key memory, host-device transfer, and backend utilization.

- Certification report: gatekeeper status and stable diagnostic codes.

### 17.2 Success metrics for the first GPU milestone

| Dimension | Initial success criterion |
| --- | --- |
| Correctness | Encrypted/decrypted logits agree with the adapted plaintext model within the declared CKKS tolerance. |
| Security policy | No secret key is present on the server and the parameter profile meets the declared security floor. |
| Compiler coverage | All ResNet-20 operators are either legally converted or rejected with stable diagnostics. |
| Integration | The complete Python -> WHIRL -> middle-WHIRL -> whirl2c -> C -> GPU library flow is automated by the driver. |
| Performance visibility | Packing, rotation, bootstrap, memory, and kernel costs are reported and attributable to IR decisions. |
| Backend interchangeability | The same FHE semantic WHIRL can target at least a reference backend and one GPU backend through the C ABI. |

## 18. Open Research Threads

The integration plan intentionally defers several research choices. They should be investigated after the end-to-end path is runnable, because their value can then be measured against a stable reference implementation.

| Research thread | Questions |
| --- | --- |
| Encrypted tensor packing | How should MetaKernelPlanIR and FhelipePackingPlanIR remain separate while producing one common layout contract? Which metrics best predict CPU/GPU performance, and which planner wins by model/operator shape? |
| Activation adaptation | Which polynomial family and training procedure preserve ResNet accuracy at acceptable depth? Should the compiler select from pre-certified approximations? |
| CKKS scale/bootstrap | How should ReSBM incorporate GPU batching, residual paths, memory, and backend bootstrap variants? |
| HPOLY/HPAO | Which CKKS operations should lower into first-class HPOLY? How should basis-state, Level, Scale, Extended, and ct×pt attributes be represented in WHIRL descriptors? HPAO-MU uses a dedicated SSAPRE-model phase without modifying WOPT SSAPRE. HPAO-MD analysis, legality, ordering, and profitability design remain TBD. |
| Bootstrap-internal HPOLY | The paper keeps bootstrap opaque. Which bootstrap stages should later be exposed so ModUp/ModDown/lazy-reduction rules can cross bootstrap internals safely? |
| Static plaintext-weight encoding | How should compile-time encoded polynomial weights be packaged, cached, versioned by CKKS parameter set, and uploaded to GPU memory? |
| GPU profitability model | Replace CPU operator timings with target-specific HPOLY/POLY cost tables including HBM traffic, NTT batches, key-switch batches, occupancy, and fusion savings. |
| Primitive vs library-call lowering | Which operations should remain high-level runtime calls, which should lower through HPOLY, and which should become explicit POLY/RNS primitives? |
| GPU fusion | Which ModUp/DotProd/ModDown, NTT, key-switch, multiply, and reduction sequences can be fused or batched safely? |
| Multi-GPU | When does ciphertext sharding or operation batching across GPUs overcome communication cost? |
| Alternative schemes | Can HPOLY generalize to BFV/BGV while SIHE remains scheme-neutral? When should TFHE/PBS take a separate path? |
| Model confidentiality | Should later threat models encrypt weights, use secure enclaves, or combine FHE with other protocols? |
| Service concurrency | How should key sets, contexts, preencoded weight objects, ciphertext batches, and GPU memory pools be scheduled across clients? |

## 19. Final Recommendation

The first DSC/FHE implementation should optimize for architectural proof, not immediate cryptographic performance. Implement the Python declaration, driver pipeline, encryption descriptors, FHE CNN conversion skeleton, middle-WHIRL call lowering, whirl2c emission, and C ABI mock/reference runtime first. This proves that FHE is a true DSC domain extension rather than an external script or framework.

Once the integration path is stable, implement MetaKernel and Fhelipe as separate selectable encrypted tensor planners behind one common interface, then benchmark both before choosing any default. Retarget the selected planner cost model and runtime calls to FIDESlib and a Cheddar-like CUDA backend. The decisive rule is that CNN semantics remain visible long enough for DSC verification, but all custom semantics are lowered away before whirl2c. This preserves the DSC domain-centered design while making the packing method an explicit, measurable compiler policy rather than a hard-wired architecture choice.

## Version 0.3 Change Summary

Version 0.3 adds an HPOLY optimization level between CKKS and POLY/RNS, adopts the HPAO polynomial optimization framework, moves MetaKernel/Fhelipe responsibility strictly to encrypted tensor/vector planning, keeps ReSBM at CKKS scale/bootstrap planning, and defines two post-HPOLY backend choices: optimized FHE library calls or native POLY/RNS GPU lowering.

> **Architecture note:** The principal new design rule is: preserve polynomial structure long enough for cross-primitive optimization. CKKS primitives may be too opaque, while RNS loops may be too low-level; HPOLY is the compact optimization window between them.

## Version 0.4 Change Summary

Version 0.4 adds an explicit FHE scheme-configuration model. It defines user-facing CKKS compiler options, makes ring dimension N a first-class compilation constraint, separates q0/first-modulus size from numerical scale Δ, introduces FHECompilationConfigIR, CKKSResolvedParameterIR, CKKSValueStateIR, and CKKSBootstrapConfigIR responsibilities, and adds security/parameter gatekeeping. The detailed design rationale and OpenFHE/HPAO mapping are preserved in Appendix D.

> **Architecture note:** The principal rule is: compiler options define policy and constraints; the IR captures requested configuration; analysis resolves derived cryptographic state; and every later optimization consumes the resolved state at the abstraction level where it is semantically meaningful. Generic encrypted layout does not own CKKS scale/level, and backend libraries do not become the authority for compiler-visible scheme configuration.

## Version 0.5 Change Summary

Version 0.5 incorporates design-review feedback: user-facing Python compilation examples use openpy rather than opencc; the DSC tensor terminology is corrected to TENSOR with TY_KIND = TY_TENSOR; Section 6.4 becomes the single normative encryption/type-propagation table used by the Section 7.2 conversion pass; and MetaKernel and Fhelipe are separated into mutually exclusive encrypted-layout planner modes selected with -dsc-fhe-layout-planner=metakernel|fhelipe so their effectiveness can be compared under identical compilation conditions.

> **Architecture note:** The principal rule is: keep one type-propagation implementation and one common encrypted-layout result interface, while allowing multiple independently selectable planning algorithms to compete behind that interface.

## Version 0.6 Change Summary

Version 0.6 adds the missing SIHE-to-CKKS scheme-state management stage between encrypted-layout planning and HPOLY. It distinguishes mandatory scale/level legality from optimization; documents ANT-ACE local scale manager and ReSBM behavior from source; defines the normative DSC -O0 bootstrap policy; adds CKKSScaleBootstrapPlanIR; maps bootstrap policy across -O0 through -O3; and makes ReSBM an optimized global replacement for provisional local placement rather than an implicit correctness requirement.

> **Architecture note:** The principal rule is: -O0 must still produce a legal CKKS program. It performs canonical SIHE-to-CKKS lowering, mandatory Relin, forward scale/level propagation, local Rescale/ModSwitch normalization, and a conservative just-in-time bootstrap only when compiler insertion is allowed. ReSBM is a higher-optimization global placement pass, not the definition of basic CKKS correctness.

## Version 0.7 Change Summary

Version 0.7 adds a dedicated FHEFusion-derived canonicalization and graph-fusion phase before encrypted layout planning. It introduces explicit Masking and Strided_Slice semantics, FHESlotValidityIR and Gap_Strides, mandatory -O0 graph canonicalization, paper-derived constant/masking/compaction rules, source-specific fusion search, a gap-density/N profitability model, new CFHEFUSE diagnostics, a driver fusion option, and Appendix E containing the full CGO'26 rule catalog. Existing sections 8-18 are renumbered 9-19, and References move to Appendix F.

## Version 0.8 Change Summary

Version 0.8 synchronizes this architecture document with FHE-WHIRL-INTEGRATION-PLAN.md. It publishes the logical operator and descriptor ownership boundary, makes mapped-image compatibility and ir_b2a -st -src evidence explicit, defines retained artifact requirements, and cross-references the focused M0-M8 implementation checkpoints without removing the more detailed CKKS, FHEFusion, HPOLY/HPAO, GPU, and productionization stages in this document.

> **Architecture note:** The principal synchronization rule is: the DOCX remains the architecture and semantic authority, while FHE-WHIRL-INTEGRATION-PLAN.md is the focused implementation and coordination tracker. When the two differ, the implementation plan must be reconciled with the architecture before opcode allocation, type-system change, mapped-image change, or lowering work proceeds.

## Version 0.9 Change Summary

Version 0.9 makes ResNet-20/CIFAR-10 the first end-to-end model milestone instead of staging through a smaller CNN. It adds common.relu to the common-substrate contract family and defines the first-release CKKS -O0 baseline: every surviving common.relu creates a mandatory bootstrap boundary before its approved polynomial approximation when bootstrap policy is auto or on. Manual mode requires an explicit ReLU boundary, while off rejects a surviving ReLU. Add, linear, and ReLU fixtures remain mandatory diagnostic unit tests but are not separate model milestones.

> **Architecture note:** The principal rule is: preserve ReLU as common source semantics, make its FHE approximation and refresh boundary explicit, and use the complete ResNet-20 path as the integration acceptance test. Bootstrap restores CKKS capacity; it does not replace the ReLU approximation. Optimized planners may move, combine, or fuse the baseline boundary only after proving semantic, numerical, scale/level, and source-provenance equivalence.

## Version 0.10 Change Summary

Version 0.10 distinguishes MetaKernel's immediate kernel iteration-space transformation followed by packing and masks from Fhelipe's delayed, graph-wide layout assignment followed by compaction and CKKS materialization. It adds a common post-transformation rotation and gap census and divides the comparison into SYNC-7A through SYNC-7E review gates. It also assigns advanced, independently disableable FHE planning to -O2 and reserves -O3 for Open64-style parallelization and memory-hierarchy optimization extended from CPU caches to encrypted-vector, polynomial, NUMA, and GPU memories. HPAO-MU is assigned to a new HPOLY phase based on the SSAPRE algorithmic model so existing WOPT SSAPRE code and behavior remain undisturbed; HPAO-MD is explicitly design TBD pending a separate legality and profitability review.

## Appendix A. Proposed Python and Driver Examples

### A.1 Source-annotated export

```text
from open64_dsc import export_to_whirl
from open64_dsc.options import WhirlExportOptions
from open64_dsc.fhe import entry, CipherTensor

class SecureResNet20(nn.Module):
    @entry(
        scheme="ckks",
        encrypted_inputs=("x",),
        encrypted_outputs=("return",),
        parameter_policy="plaintext",
        security_level=128,
    )
    def forward(self, x: CipherTensor[1, 3, 32, 32]):
        return self.net(x)

model = SecureResNet20().eval()
exported = torch.export.export(model, (sample_x,))
whirl = export_to_whirl(
    exported,
    options=WhirlExportOptions(
        preserve_domain_ops=True,
        emit_tensor_descriptors=True,
        emit_contracts=True,
        output_format="binary_whirl",
        weight_policy="external_binary",
    ),
)
whirl.save("secure_resnet20.B")
```

### A.2 Driver-controlled conversion

```text
openpy -x whirl secure_resnet20.B \
  -dsc-fhe=cnn \
  -dsc-fhe-scheme=ckks \
  -dsc-fhe-ckks-ring-dim=65536 \
  -dsc-fhe-security=128-classic \
  -dsc-fhe-ckks-mult-depth=auto \
  -dsc-fhe-ckks-scale-bits=56 \
  -dsc-fhe-ckks-first-mod-bits=60 \
  -dsc-fhe-ckks-slots=auto \
  -dsc-fhe-ckks-key-switch=hybrid \
  -dsc-fhe-ckks-large-digits=3 \
  -dsc-fhe-bootstrap=auto \
  -dsc-fhe-backend=fideslib \
  -dsc-fhe-target=cuda \
  -dsc-fhe-layout=automatic \
  -dsc-fhe-layout-planner=metakernel \
  -dsc-fhe-dump=all \
  -o libsecure_resnet20.so
```

## Appendix B. Representative IR Evolution

| Stage | Representative operation |
| --- | --- |
| Python/FX | call_module conv1; call_module bn1; call_function relu; call_module layer1.0 |
| Very-high-level WHIRL | cnn.conv2d; cnn.batch_norm_infer; common.relu; cnn.resnet.basic_block |
| FHE CNN WHIRL | cnn_fhe.conv2d_plain_weight; cnn_fhe.poly_activation; cnn_fhe.residual_add |
| Encrypted tensor planning | layout_assign; compact; align_residual; bootstrap_plan; key_manifest |
| Primitive/library WHIRL | fhe.rotate; fhe.mul_plain; fhe.add; fhe.rescale; or dsc_fhe_conv2d_plain call |
| Middle-WHIRL | OPR_CALL dsc_fhe_*; standard IF/DO/LDID/STID; static descriptors |
| C | Opaque handle variables and dsc_fhe_* function calls |
| Backend | OpenFHE/FIDESlib/Cheddar objects, CUDA kernels, streams, and memory pools |

## Appendix C. Initial C ABI Operation Set

| Category | Proposed calls |
| --- | --- |
| Lifecycle | context_create/destroy, model_create/destroy, model_run, synchronize |
| Serialization | ciphertext_import/export, keyset_import, model_weight_import |
| Value lifetime | ciphertext_retain/release, plaintext_retain/release |
| Linear operations | add_ct, add_plain, mul_plain, linear_plain, conv2d_plain, average_pool |
| FHE primitives | rotate, relinearize, rescale, mod_switch, bootstrap, layout_convert |
| Polynomial | poly_eval, square, multiply_ct |
| Inspection | get_level, get_scale, get_size_bytes, get_backend_status |
| Error handling | status_code, get_last_error, diagnostic_callback |

## Appendix D. FHE Scheme Configuration, Parameter Resolution, and Compiler Option Analysis

### D.1 Architectural Objective

The FHE scheme configuration must be represented inside the compiler because the same values affect several different stages. Ring dimension N determines the CKKS SIMD capacity and therefore constrains EncryptedTensorLayoutIR and MetaKernel packing; multiplicative depth and precision determine the modulus chain; security constrains the legal combination of N and total modulus; bootstrap parameters affect ReSBM and runtime setup; and HPOLY transformations require stable Level/Scale/basis attributes. Treating these facts only as OpenFHE runtime flags would make earlier compiler decisions blind to cryptographic constraints. [5][17][18][19]

### D.2 Terminology and Parameter Semantics

| Term | Meaning | DSC policy |
| --- | --- | --- |
| N / ring dimension | Cyclotomic ring parameter for R_Q = Z_Q[X]/(X^N+1). Polynomials have degree < N. In CKKS, full-packing capacity is at most N/2 slots. | User-visible and security-checked. |
| Δ / numerical scale | Approximate fixed-point scaling factor used by CKKS. The proposed CLI represents log2(Δ) as -dsc-fhe-ckks-scale-bits. | Distinct from q0. |
| q0 / first modulus | First modulus prime in Q=q0*q1*...*qL. The proposed CLI configures its bit size, not the prime itself. | Do not call q0 the scale. |
| Multiplicative depth | Number/budget of sequential multiplication/rescaling levels required by the transformed program, including any explicit bootstrap-depth requirements. | Prefer auto; explicit user value is a constraint. |
| Q / modulus chain | Product of RNS primes used by the resolved CKKS configuration. | Compiler-derived; security checked. |
| P / auxiliary basis | Extended RNS basis used by HYBRID key switching / ModUp-DotProd-ModDown. | Compiler/backend derived; visible in HPOLY state. |
| Slots | Active packed values per ciphertext. | User constraint or compiler-derived; <= N/2. |

The HPAO manuscript uses log2(q0)=60 and log2(Delta)=56 in its evaluation, illustrating why first-modulus size and numerical scale must remain distinct compiler fields. [17]

### D.3 Three Classes of Scheme Information

| Class | Representative information | Purpose |
| --- | --- | --- |
| User/profile configuration | N, security target, requested depth, precision/scale bits, first-modulus bits, slots, key-switch policy, bootstrap policy. | Expresses deployment intent and constraints. |
| Compiler-resolved parameters | Required computation depth, modulus chain, total Q bits, active slots, ciphertext count, rotations, evaluation-key requirements, bootstrap schedule, auxiliary basis, derived security result. | Produced from the transformed program plus user constraints. |
| Per-value CKKS/HPOLY state | Current level, current scale, Q versus QP/extended basis, ciphertext component count, ct×ct versus ct×pt status, precision estimate, relinearization obligation. | Changes as encrypted values move through CKKS and HPOLY WHIRL. |

### D.4 User-Facing Compiler Option Model

The stable user interface should remain deliberately smaller than the full OpenFHE parameter surface. The primary options are scheme, N, security, depth, scale bits, first-modulus bits, slots, and bootstrap policy. Key-switch and bootstrap-transform tuning are advanced options. Command-line values override configuration profiles; all origins are recorded for certification and reproducibility.

| Option | Recommended role | Validation / derivation |
| --- | --- | --- |
| -dsc-fhe-ckks-ring-dim=N | Expert/user choice of N. | Must be a supported ring dimension and satisfy security for resolved Q. |
| -dsc-fhe-security=128-classic | Deployment security policy. | Compiler rejects an explicit N that cannot support resolved Q at this target. |
| -dsc-fhe-ckks-mult-depth=auto\|D | Depth budget. | auto derives from transformed graph; explicit D must be >= required depth. |
| -dsc-fhe-ckks-scale-bits=P | Requested precision/scaling size. | Feeds CKKS parameter resolution and value-state initialization. |
| -dsc-fhe-ckks-first-mod-bits=B0 | First modulus bit size. | Feeds context generation; distinct from scale bits. |
| -dsc-fhe-ckks-slots=auto\|S | Packing capacity constraint. | S <= N/2; auto chosen by layout/MetaKernel planning. |
| -dsc-fhe-bootstrap=auto\|on\|off\|manual | Bootstrap policy. | At -O0, auto/on inserts before every common.relu and as needed elsewhere; manual requires explicit ReLU boundaries; off rejects surviving ReLU. ReSBM may replan only at optimized levels. |
| -dsc-fhe-ckks-key-switch=hybrid\|bv | Advanced scheme/backend choice. | Determines decomposition and HPOLY/POLY key-switch structure. |

### D.5 FHECompilationConfigIR

```text
FHECompilationConfigIR {
  scheme = CKKS
  source = { driver_options | configuration_profile }

  security {
    target = HE128_CLASSIC
    secret_key_distribution = UNIFORM_TERNARY
  }

  ckks {
    ring_dimension = 65536
    requested_mult_depth = AUTO
    scaling_mod_bits = 56
    first_mod_bits = 60
    slots = AUTO
    scaling_technique = FLEXIBLE_AUTO
    key_switch_technique = HYBRID
    num_large_digits = 3
    bootstrap_policy = AUTO
  }
}
```

This record is module/program configuration and exists before encrypted tensor packing. It is not attached independently to every tensor. TensorDescriptorIR and encrypted values reference the active FHE configuration by ID.

### D.6 CKKSResolvedParameterIR

```text
CKKSResolvedParameterIR {
  ring_dimension = 65536
  cyclotomic_order = 131072
  max_slots = 32768
  active_slots = 16384

  required_computation_depth = 12
  bootstrap_depth = 5
  configured_depth = 17

  scale_bits = 56
  first_mod_bits = 60
  modulus_chain = { q0_bits=60, q1_bits=56, ... }
  total_Q_bits = derived

  key_switch {
    technique = HYBRID
    auxiliary_basis = derived
    decomposition_digits = 3
  }

  security {
    requested = HE128_CLASSIC
    verified = true
  }
}
```

The resolved record separates requested policy from the configuration that is actually legal for the transformed program. MetaKernel/Fhelipe determine slot/rotation pressure; FHEFusion and activation lowering affect depth; ReSBM finalizes scale/bootstrap behavior; the parameter resolver then validates the total modulus and security envelope before code generation.

### D.7 CKKSValueStateIR and HPOLY State

```text
CKKSValueStateIR {
  level
  scale_bits
  basis = Q | QP
  extended = false | true
  cipher_components = 2 | 3
  value_kind = ciphertext | encoded_plaintext
  precision_estimate
  requires_relinearization
}
```

Per-value state is distinct from module configuration. CKKS multiplication, relinearization, rescaling, ModUp, ModDown, and bootstrapping change the value state over time. HPOLY reinforces this separation by attaching Level, Scale, Extended, and Ciph_mul attributes to operations so that rewrites such as ModDown sinking can prove basis, level, and scale consistency. [17]

### D.8 Progressive Parameter Resolution Through the Pipeline

```text
Driver options / profile
  -> FHECompilationConfigIR
       N and security are already known
  -> CNN/FHE Semantic WHIRL
  -> FHE-aware canonicalization + FHEFusion graph optimization
  -> Encrypted Tensor / selected MetaKernel or Fhelipe planner
       uses N; derives packing, ciphertext count, rotations
  -> SIHE WHIRL
       resolves Cipher / Plain flow
  -> CKKS Parameter Resolution
       derives depth, Q chain, scale policy, key requirements
  -> CKKS WHIRL + finalized scale/bootstrap plan
       finalizes scale/level/bootstrap schedule
  -> HPOLY
       tracks Q/QP basis, ModUp/ModDown, constant encoding
  -> Library backend or POLY/RNS native backend
```

### D.9 Security Gatekeeper for Explicit N

N is intentionally user-selectable, but it is a constraint rather than a security override. The compiler resolves total Q and the secret-key distribution, then verifies that the requested N satisfies the selected security policy. If not, compilation fails with the minimum acceptable configuration or a recommendation to enlarge N. OpenFHE normally supports security-driven ring-dimension selection; DSC reverses the control only in the sense that the user may explicitly choose N, while preserving the same requirement that the parameter set meet the selected security standard. [5][18]

```text
CFHEPARAM-002 RingDimensionBelowSecurityRequirement
  requested_N        = 8192
  security_target    = 128-classic
  resolved_total_Q   = ... bits
  result             = reject
  recommendation     = increase -dsc-fhe-ckks-ring-dim
```

### D.10 Advanced Bootstrap Configuration

Bootstrap planning needs two layers of control. The common user policy is auto/on/off/manual. Advanced CKKS profiles may additionally specify the level budget for CoeffsToSlots and SlotsToCoeffs, BSGS dimensions, and the number of levels required after bootstrap. OpenFHE exposes these concepts through EvalBootstrapSetup; its documentation notes that levelBudget trades levels against rotations, while bsgsDim controls baby-step/giant-step structure. [19]

```text
CKKSBootstrapConfigIR {
  policy = AUTO
  coeff_to_slot_level_budget = 4
  slot_to_coeff_level_budget = 4
  coeff_to_slot_bsgs = AUTO
  slot_to_coeff_bsgs = AUTO
  levels_after_bootstrap = 10
  iterative_refinement = false
}
```

### D.11 Key-Switch Configuration and HPOLY Interaction

Key-switch configuration is scheme-specific and should be visible before HPOLY lowering. HYBRID decomposition parameters determine the extended RNS basis and therefore influence ModUp, DotProd, and ModDown cost. HPAO models KeySwitch as ModDown(DotProd(ModUp(x), evk)), making this configuration relevant to both legality and profitability of HPOLY transformations. [17]

### D.12 Parameters Deliberately Hidden from Ordinary Users

Ordinary users should not specify individual q_i or P primes, exact rotation-key indices, actual bootstrap insertion points, per-operator levels, ModUp/ModDown placement, extended-basis lifetimes, NTT schedules, or RNS-limb allocation. These are compiler-derived results. The compiler records them in CKKSResolvedParameterIR, KeyMaterialContractIR, CKKSValueStateIR, HPOLY state, and optional POLY/RNS plans so that they remain inspectable without becoming source-level tuning obligations.

### D.13 Configuration File and Provenance

```text
scheme: ckks
security:
  level: 128-classic
  secret_key_distribution: uniform_ternary
ckks:
  ring_dimension: 65536
  multiplicative_depth: auto
  scaling:
    scale_bits: 56
    first_modulus_bits: 60
    technique: flexible_auto
  packing:
    slots: auto
  key_switch:
    technique: hybrid
    large_digits: 3
  bootstrap:
    policy: auto
backend:
  library: openfhe
```

Each resolved field should carry provenance such as USER, PROFILE, COMPILER_DERIVED, or BACKEND_DEFAULT. This is important for certification, reproducibility, and explaining why two builds of the same CNN use different ring dimensions, modulus chains, or bootstrap schedules.

### D.14 Diagnostics

| Diagnostic | Meaning |
| --- | --- |
| CFHEPARAM-001 | InvalidRingDimension |
| CFHEPARAM-002 | RingDimensionBelowSecurityRequirement |
| CFHEPARAM-003 | SlotCountExceedsNOver2 |
| CFHEPARAM-004 | MultiplicativeDepthInsufficient |
| CFHEPARAM-005 | FirstModulusTooSmall |
| CFHEPARAM-006 | ScalePrecisionInsufficient |
| CFHEPARAM-007 | TotalModulusExceedsSecurityEnvelope |
| CFHEPARAM-008 | UnsupportedScalingTechnique |
| CFHEPARAM-009 | InvalidKeySwitchConfiguration |
| CFHEPARAM-010 | BootstrapDepthInsufficient |
| CFHEPARAM-011 | BootstrapConfigurationIncompatible |
| CFHEPARAM-012 | BackendParameterUnsupported |

### D.15 Recommended First Implementation Interface

```text
openpy resnet20.B \
  -dsc-fhe=cnn \
  -dsc-fhe-scheme=ckks \
  -dsc-fhe-ckks-ring-dim=65536 \
  -dsc-fhe-security=128-classic \
  -dsc-fhe-ckks-mult-depth=auto \
  -dsc-fhe-ckks-scale-bits=56 \
  -dsc-fhe-ckks-first-mod-bits=60 \
  -dsc-fhe-ckks-slots=auto \
  -dsc-fhe-layout-planner=metakernel \
  -dsc-fhe-bootstrap=auto \
  -dsc-fhe-backend=openfhe
```

The minimal stable interface should stop here. Other parameters can be introduced first through a configuration profile and promoted to public command-line options only when repeated use demonstrates that they are durable user-facing choices rather than backend implementation details.

### D.16 Final Scheme-Configuration Rules

- Compiler options express FHE policy and explicit constraints; they do not directly construct backend runtime objects.

- Ring dimension N is a first-class user option because it affects both security and encrypted-tensor packing before CKKS lowering.

- q0/first-modulus size and numerical scale Δ are distinct fields and must never share a compiler option.

- Multiplicative depth defaults to compiler-derived; an explicit user depth is a hard minimum/budget contract.

- FHECompilationConfigIR is program-level configuration; CKKSResolvedParameterIR is compiler-derived scheme state; CKKSValueStateIR is per-value evolving state.

- Generic EncryptedTensorLayoutIR owns packing/layout, not CKKS level/scale/basis.

- Explicit N must pass security validation against the resolved modulus chain and selected security target.

- Low-level q_i/P primes, rotation keys, bootstrap placement, ModUp/ModDown placement, and NTT/RNS scheduling remain compiler-derived.

## Appendix E. FHEFusion Canonicalization and Algebraic Rule Catalog

### E.1 Purpose

This appendix records the CGO'26 FHEFusion rule set used as the research basis for Section 8. DSC treats these as the initial rule registry, subject to independent legality validation and future extension. [16]

### E.2 Paper-Derived Rule Catalog

| Rule | Class | Before | After | Benefit |
| --- | --- | --- | --- | --- |
| FUSED-CMPT | Assoc. | Slice2(Slice1(t)) | Slice3(t) | reduces depth |
| FUSED-GEMV-CMPT | Assoc. | GEMV1(Slice(t)) | GEMV2(t) | reduces depth |
| FUSED-GEMV-SCALAR | Assoc. | GEMV1(t*scalar) | GEMV2(t) | reduces depth |
| FUSED-SCALAR-GEMV | Assoc. | scalar*GEMV1(t) | GEMV2(t) | reduces depth |
| FUSED-CONV-SCALAR | Assoc. | Conv1(t*scalar) | Conv2(t) | reduces depth |
| FUSED-SCALAR-CONV | Assoc. | scalar*Conv1(t) | Conv2(t) | reduces depth |
| FUSED-SCALAR-RELU | Assoc. | scalar*RELU1(t) | RELU2(t) | reduces depth |
| SCALAR | Assoc. | (t*s1)*s2 | t*(s1*s2) | reduces depth |
| FUSED-RELU-MASKING | Assoc. | RELU1(Masking(t)) | RELU2(t) | reduces depth |
| MASKING-SCALAR | Assoc. | Masking(t)*scalar | t*(scalar*mask01) | reduces depth |
| SCALAR-MASKING | Assoc. | Masking(t*scalar) | t*(scalar*mask01) | reduces depth |
| FUSED-CMPT-MASKING | Assoc. | Slice(Masking(t)) | Slice(t) | reduces depth |
| CONCAT | Distr. | Concat(t1*s,t2*s) | s*Concat(t1,t2) | enables further opt. |
| CONCAT-CMPT | Distr. | Concat(Slice(t1),Slice(t2)) | Slice(Concat(t1,t2)) | enables further opt. |
| CMPT-MUL | Distr. | Slice(t1)*Slice(t2) | Slice(t1*t2) | enables further opt. |
| CMPT-ADD | Distr. | Slice(t1)+Slice(t2) | Slice(t1+t2) | enables further opt. |
| MASKING-MUL | Distr. | Masking(t1)*Masking(t2) | Masking(t1*t2) | enables further opt. |
| RELU-DIS | Distr. | a*t^2+b*t | a*(t^2+(b/a)*t) | enables further opt. |
| MASKING-ADD-RELU | Distr. | Masking(t1)+RELU(t2) | Masking(t1+RELU(t2)) | enables further opt. |
| CONV-CMPT | Comm. | Conv1(Slice1(t)) | Slice2(Conv2(t)) | enables further opt. |
| RELU-CMPT | Comm. | RELU(Slice(t)) | Slice(RELU(t)) | enables further opt. |
| AVGPOOL-CMPT | Comm. | AvgPool1(Slice1(t)) | Slice2(AvgPool2(t)) | enables further opt. |
| CMPT-SCALAR | Comm. | Slice(t*scalar) | scalar*Slice(t) | enables further opt. |
| SCALAR-CMPT | Comm. | scalar*Slice(t) | Slice(t*scalar) | enables further opt. |
| AVGPOOL-SCALAR | Comm. | AvgPool(t*scalar) | scalar*AvgPool(t) | enables further opt. |
| FLATTEN-MASKING | Comm. | Flatten(Masking(t)) | Masking(Flatten(t)) | enables further opt. |
| FLATTEN-CMPT | Comm. | Flatten1(Slice1(t)) | Slice2(Flatten2(t)) | enables further opt. |
| FLATTEN-SCALAR | Comm. | Flatten(t*scalar) | scalar*Flatten(t) | enables further opt. |
| RESHAPE-CMPT | Comm. | Reshape1(Slice1(t)) | Slice2(Reshape2(t)) | enables further opt. |
| RESHAPE-SCALAR | Comm. | Reshape(t*scalar) | scalar*Reshape(t) | enables further opt. |

### E.3 Legality Preconditions

Masking rules require a binary mask derivable from input shape, kernel, stride, and valid-output positions. Strided_Slice rules require shape/kernel/stride/padding consistency so slice boundaries select exactly the valid output region. MASKING-ADD-RELU additionally requires the zero/junk region of the RELU operand to align with the positions cleared by the mask. Operators moved across Strided_Slice must inherit/update Gap_Strides and related attributes. [16]

### E.4 Profitability Policy

The paper treats most rules aggressively but uses a gap-aware cost test for Strided_Slice propagation because sparse layouts reduce SIMD utilization and can increase the ring dimension N. DSC preserves that distinction and extends the cost interface so later layout/GPU passes can provide target-specific estimates without changing the semantic legality rules. [16]

## Appendix F. FHE WHIRL Implementation Synchronization

### F.1 Document Roles and Synchronization Rule

This appendix records the shared implementation contract synchronized from FHE-WHIRL-INTEGRATION-PLAN.md. This architecture document owns semantic decisions, abstraction boundaries, and the complete pipeline. The Markdown plan owns focused task staging, handoffs, implementation status, and review checkpoints. The Markdown plan may narrow a milestone, but it may not independently allocate shared opcodes, change tensor or WHIRL binary encodings, or bypass a gatekeeper defined here.

### F.2 Non-Negotiable Implementation Boundaries

- Preserve existing binary WHIRL compatibility. Any proposed ELF section, opcode encoding, type-kind encoding, node-layout, reader/writer, or ir_a2b/ir_b2a change requires a versioned common/com plan and compatibility tests.

- Treat physical OPR_DSL and transitional carriers as private. Compiler APIs, diagnostics, traces, and ASCII dumps expose only stable logical operator names and versions.

- Keep shared opcode and type-system ownership in common/com. The FHE task publishes requests with operands, attributes, descriptors, effects, verifier obligations, lowering ownership, and compatibility impact before allocation.

- Represent FHE as semantic and representation dimensions around shared tensor types. Do not create a disconnected encrypted tensor type universe.

- Never place secret keys in WHIRL, generated C, descriptor assets, diagnostics, model packages, or server-owned runtime state.

- Keep source-domain and FHE operators visible until both gatekeepers complete. Ingestion must not replace them with runtime calls, intrinsics, or target kernels.

### F.3 Focused Milestone Crosswalk

The focused checkpoints below are review gates for the architecture phases in Section 16. Completion requires retained binary and ASCII evidence, not only an in-memory or mock-frontend result.

| Focused milestone | Architecture phase | Synchronized exit evidence |
| --- | --- | --- |
| M0 Contract review | Phase 0 Contract freeze | Common/type owners accept or revise the proposed names, descriptors, effects, verifier rules, lowering ownership, and compatibility impact. |
| M1 FHE metadata capture | Phases 0-1 | The complete ResNet-20 model emits binary WHIRL whose boundary contracts, common.relu nodes, tensor traits, class-centric PUs, and source positions are visible in ir_b2a -st -src. |
| M2 Validation-only driver path | Phase 1 | The -dsc-fhe option family and gatekeeper accept a complete fixture and reject malformed contracts with stable CFHE diagnostics. |
| M3 Mock call lowering | Phase 1 | Add, linear, and ReLU unit tests certify opaque C ABI lowering; the ResNet-20 mock path then compiles and links with explicit ReLU bootstrap evidence. |
| M4 CNN adaptation | Phase 3 | The full ResNet-20 graph performs convolution, folded batch norm, residual, pooling, classifier, and common.relu bootstrap-plus-polynomial conversion with a reviewable report. |
| M5 ResNet-20 end-to-end | Phases 3-5 | The complete ResNet-20 reference path verifies shape, lineage, layout, scale, level, bootstrap, approximation, encrypted logits, and mismatch diagnostics before lowering. |
| M6 Reference CKKS | Phases 2 and 5 | OpenFHE/reference execution matches the adapted plaintext model within the declared accuracy budget and emits resolved scheme-state evidence. |
| M7 GPU planning | Phases 4 and 7 | Backend capability, layout, rotation, memory, and cost reports drive planning without requiring native POLY/RNS code generation. |
| M8 Native GPU research path | Phases 6 and 8 | A separately reviewed HPOLY-to-POLY/RNS and CUDA/Triton path improves selected kernels without changing source-domain contracts. |

Architecture Phase 9 productionization remains outside the focused M0-M8 tracker and begins only after the reference and GPU paths satisfy compatibility, correctness, security, and artifact-review gates.

### F.4 Immediate Coordinated Actions

1. The shared common/com owner reviews the Section 6.5 contract families and publishes accepted versions before FHE opcode or type work begins.

2. The FHE frontend owner captures the complete deterministic ResNet-20/CIFAR-10 fixture and records its source/operator census before adding lowering behavior. Smaller operator fixtures remain diagnostic tests only.

3. The FHE gatekeeper owner publishes the first diagnostic registry and positive/negative verification matrix.

4. The runtime owner defines libdsc_fhe_cabi ABI v1 with opaque handles and a mock backend before selecting OpenFHE or a GPU provider.

5. The build and driver owner certifies the separate-process .B boundary, mapped-image reopen, ir_b2a -st -src trace, whirl2c output, final C/C++ link, and retained host-visible artifacts.

### F.5 Deferred Decisions Requiring Review

The exact storage slot for EncryptionDescriptorIR, the final shared tensor-trait mechanism, the breadth of common.window_reduce, the diagnostic-code registry policy, runtime key-manifest ABI, backend capability schema, async GPU ownership, native POLY/RNS lowering, and production artifact location remain reviewed decisions. Implementations must stop at these boundaries rather than inventing frontend-only or backend-private substitutes.

## Appendix G. MetaKernel and Fhelipe Layout Planning Comparison

### G.1 Shared question and different decision order

Both planners map multidimensional tensor computation onto one-dimensional CKKS ciphertext slots while preserving logical operator semantics. Their principal difference is decision order. MetaKernel starts from a Conv or MVM kernel, selects composable units and batching, transforms the iteration space, and then packs and masks the transformed computation. Fhelipe keeps the logical tensor graph intact while assigning layouts globally; producer-consumer compatibility, dimension-bit order, interleaving, gaps, and conversion cost determine the layout before the concrete CKKS schedule is materialized.

### G.2 What packing means

Packing has three distinct meanings that reports must not conflate: assignment of logical elements to ciphertext and slot coordinates; compaction or repacking after operations create gaps; and masks that clear invalid or discarded slots. MetaKernel derives these after its kernel transformation. Fhelipe plans the first two during global layout assignment and realizes masks, rotations, permutations, and rotate-add reductions during lowering. A mask contributes to correct packing but is not by itself the complete packing algorithm.

### G.3 Fair comparison rule

The compiler shall compare the alternatives only from one frozen source graph, scheme configuration, backend capability manifest, and option set. Both planners must lower into the same common encrypted-layout and iteration-space records. A shared verifier derives rotation and gap metrics from the resulting WHIRL image. The selected algorithm remains explicit provenance; semantic correctness, binary inspection, and later CKKS lowering cannot depend on private planner structures.

### G.4 Required measurements

| Metric family | Required values | Reason |
| --- | --- | --- |
| Rotations | Static operations, execution-weighted operations, unique signed offsets, rotate-add reductions | Measures schedule cost and rotation-key demand without conflating static and dynamic work. |
| Slots and gaps | Total, active, gap or invalid, padding, replicated, peak, introduced, compacted, and masked slots | Measures utilization and identifies where each planner creates or removes wasted capacity. |
| Packing operations | Ciphertexts, density, compactions, conversions, permutations, and masks | Explains the mechanism behind rotation and gap differences. |
| Identity and scope | Per value, logical operator, PU, and whole program with source identity | Supports human review and isolates regressions to stable compiler entities. |

## Appendix H. References

[1] DSC_Master_Design_Doc_v0.9.docx, Domain-Specific Compiler IR and Expert System Architecture, Chapters 2, 6, 7 and Appendices E/I, June 20, 2026.

[2] A. Krastev, N. Samardzic, S. Langowski, S. Devadas, and D. Sanchez, “A Tensor Compiler with Automatic Data Packing for Simple and Efficient Fully Homomorphic Encryption,” Proc. ACM Program. Lang., 8 (PLDI), Article 152, June 2024. https://doi.org/10.1145/3656382

[3] Fhelipe compiler source repository. https://github.com/fhelipe-compiler/fhelipe

[4] Open64 README.Pro64.src, WHIRL-to-C converter component description. https://github.com/open64-compiler/open64/blob/develop/README.Pro64.src

[5] OpenFHE development repository and documentation. https://github.com/openfheorg/openfhe-development and https://openfhe.org/

[6] C. Agulló-Domingo et al., “FIDESlib: A Fully-Fledged Open-Source FHE Library for Efficient CKKS on GPUs,” arXiv:2507.04775, 2025.

[7] FIDESlib source repository. https://github.com/CAPS-UMU/FIDESlib

[8] J. Kim, W. Choi, and J. H. Ahn, “Cheddar: A Swift Fully Homomorphic Encryption Library for CUDA GPUs,” arXiv:2407.13055, 2024/2025 revision.

[9] Cheddar source repository. https://github.com/scale-snu/cheddar-fhe

[10] S. Fan, Z. Wang, W. Xu, R. Hou, D. Meng, and M. Zhang, “TensorFHE: Achieving Practical Computation on Encrypted Data Using GPGPU,” HPCA 2023. https://arxiv.org/abs/2212.14191

[11] Microsoft SEAL / EVA and CHET research are useful references for CKKS scale management and neural-network lowering, but they are not selected as the initial GPU execution backend.

[12] Zama Concrete and Concrete-ML are useful references for a future TFHE/PBS path, but the first DSC/FHE vertical slice selects CKKS.

[13] L. Li et al., “ANT-ACE: An FHE Compiler Framework for Automating Neural Network Inference,” CGO 2025, pp. 193-208.

[14] P. Yuan et al., “MetaKernel: Enabling Efficient Encrypted Neural Network Inference through Unified MVM and Convolution,” Proc. ACM Program. Lang. 9, OOPSLA2, Article 317, 2025.

[15] Y. Liu et al., “ReSBM: Region-based Scale and Minimal-Level Bootstrapping Management for FHE via Min-Cut,” ASPLOS 2025.

[16] T. Sui et al., “FHEFusion: Enabling Operator Fusion in FHE Compilers for Depth-Efficient DNN Inference,” CGO 2026, pp. 70-83, doi:10.1109/CGO68049.2026.11395213. Preprint: https://ant-research.github.io/ace-compiler/assets/FHEFusion_paper.pdf

[17] “HPAO: Polynomial-Level Optimization for RNS-CKKS Programs,” anonymous CGO final-review manuscript supplied for DSC architecture review, 15 pages. Introduces HPOLY and HPAO-MU/MD/FM/LM.

[18] OpenFHE Development Team, CKKS parameterization examples including simple-real-numbers.cpp and function-evaluation.cpp, current openfhe-development repository; documents multiplicative depth, scaling-modulus size, first-modulus size, batch size, ring dimension, and security-level parameterization. https://github.com/openfheorg/openfhe-development

[19] OpenFHE Development Team, CKKS Bootstrapping Guide and simple-ckks-bootstrapping.cpp; documents levelBudget, BSGS dimensions, levels available after bootstrap, EvalBootstrapSetup, and bootstrap key generation. https://github.com/openfheorg/openfhe-development/blob/main/src/pke/examples/CKKS_BOOTSTRAPPING.md

[20] ANT-ACE source, fhe-cmplr/ckks/config/option.yml, current main branch. Defines CKKS q0/scale/N/input-level and bootstrap/ReSBM option family. https://github.com/ant-research/ace-compiler/blob/main/fhe-cmplr/ckks/config/option.yml

[21] ANT-ACE source, fhe-cmplr/ckks/include/scale_manager.h and ckks/src/scale_manager.cxx, current main branch. Defines scale-degree/rescale-level propagation and local EVA/PARS/ACE_SM policies. https://github.com/ant-research/ace-compiler

[22] ANT-ACE source, fhe-cmplr/ckks/include/resbm.h, current main branch. Documents ReSBM preconditions, depth-one-region construction, min-cut/dynamic-programming planning, and insertion stage. https://github.com/ant-research/ace-compiler/blob/main/fhe-cmplr/ckks/include/resbm.h

[23] ANT-ACE source, fhe-cmplr/ckks/include/smo_bootstrap_inserter.h and ckks/src/smo_bootstrap_inserter.cxx, current main branch. Implements collection/materialization of ReSBM rescale and bootstrap points. https://github.com/ant-research/ace-compiler

[24] ANT-ACE source, fhe-cmplr/include/fhe/ckks/sihe2ckks_impl.h and fhe-cmplr/include/fhe/sihe/sihe_gen.h, current main branch. Shows explicit SIHE bootstrap lowering to CKKS.bootstrap and canonical ct×ct multiplication followed by relinearization. https://github.com/ant-research/ace-compiler
