# O2-E5C Final CKKS, ANT Projection, and Materialization Detailed Execution Plan

Status: Proposed detailed plan; implementation remains locked until all entry gates are accepted
Plan version: 0.1
Date: 2026-09-15
Engineering stage: O2-E5C
Governing milestones: S1.5B and S1.8
Exit gate: O2-E5C-EXIT
## 1. Authority and Metadata

### 1.1 Governing authority

This plan is subordinate to the following authorities, in order:

1. Explicit accepted user decisions and repository invariants in AGENTS.md.
2. The accepted master architecture and its accepted successor or amendment.
3. Accepted architecture decision records within their stated scope.
4. doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md, version 1.6.
5. doc/FHE-O2-DETAILED-EXECUTION-PLAN.md, plan set version 0.2, for navigation.
6. This detailed stage plan.
7. Locked source artifacts within their declared claim boundaries.
The currently recorded master baseline is commit ee1dc6382246c58f49a3097157a8c4e8ff2440c8.
The recorded master Markdown SHA-256 is 7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25.
The recorded master DOCX SHA-256 is 0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9.
The pending ownership decision is doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md.
The accepted replacement master and ADR identifiers, versions, commits, and hashes must replace pending references before implementation begins.
### 1.2 ACE rtlib and ANT source lock

The only provider in this stage is ANT through the public C surfaces exported by FHErt_common and FHErt_ant. LIB_ANT is distinct from LIB_ACE.
The reviewed ACE compiler repository input is:

- repository path for review: ../../ace-compiler; exact commit: 929e9b621f11bebbaa9ec1e215f4a52e3d07109b; provider: LIB_ANT; libraries: FHErt_common and FHErt_ant only; no opaque dsc_fhe facade;
- no ACE AIR, compiler IR, C++ provider class, STL, or exception ABI.
The reviewed public-header hash lock is:

| Header relative to ../../ace-compiler | SHA-256 |
| --- | --- |
| fhe-cmplr/rtlib/include/common/rtlib.h | 11551822041457F0743B67BCE111F09BD0AF5ADE02C80E3715F37CCB76FCFD67 |
| fhe-cmplr/rtlib/include/common/rt_api.h | 0025B6FB981F9578B15C9F05435DB0951E4B6C5E7547B398496F39CD0C5BC5C8 |
| fhe-cmplr/rtlib/include/common/common.h | 1DF878C051ADACCEABD3ECB57A88C5566E19F446AAE9C974FF9430FBD64E4AAB |
| fhe-cmplr/rtlib/include/common/cmplr_api.h | 0C7A5B1060FE39D10C0F07B1C502EFC0ACCDA4C729A5735A41A4AC5986C87F9D |
| fhe-cmplr/rtlib/include/rt_ant/rt_ant.h | AB4189F3970C2CEB80EC70018A31650E8ECC9C8102AB7BB55C3DB92A67E50C6E |
| fhe-cmplr/rtlib/include/rt_ant/rt_api.h | F5338B8893684898EDFF70A50EB495176671CA4C96B34241233FA681DDD084DF |
| fhe-cmplr/rtlib/include/rt_ant/ant_api.h | B9AE0016F701DE2FBE6D12BE59604DDF5C04B7666A90AFA44B8E1B671B20577A |
| fhe-cmplr/rtlib/include/rt_ant/rt_def.h | 58A9CBDFFACB5F1B28EB03697541EA687F27849809A9F4974B2FB47BE41789F9 |
| fhe-cmplr/rtlib/ant/include/ckks/cipher.h | D5166E651AE61828841AFE8991A6AB0E73DAA36D161DE8A2C19344EBF4684469 |
| fhe-cmplr/rtlib/ant/include/ckks/plain.h | CB2399080410402736FEBF4FB43F45BA0AFAA0D7C1C719844BC10A976D0C7854 |
| fhe-cmplr/rtlib/ant/include/context/ckks_context.h | 56D8A4F54B2B993575F872959B6A27BC3BCD46F5A590DBDECE2A17E4156431AE |
| fhe-cmplr/rtlib/ant/include/util/crt.h | 593216699CA2401F1B88E30EA37067F3A2D348D059E1C79913D0FE83CA3B9A30 |
| fhe-cmplr/include/fhe/core/lib_provider.h | E1C3D250F62AC7E72FDB2E24E75C150412743D39FFBE745BC5144D9E26EC40EF |
lib_provider.h is evidence for the LIB_ANT spelling. It is not a generated-C header. A changed commit, header, library, build flag, exported-symbol set, or transitive public-header set invalidates E5C and every dependent acceptance bundle until targeted requalification passes.
### 1.3 Consumed baseline and evidence

O2-E5C consumes, but does not recreate:

- a current O2-O0Q-001=Qualified bundle; accepted O2-E1-EXIT through O2-E5B-EXIT decisions; the accepted E2 record and atomic transaction contract; the accepted E3 graph, state, action, effect, and identity substrate; the accepted E4 selected layout and transformed-data identities;
- the accepted E5A CANONICAL_PRE_RESBM snapshot; the E5B ReSBMResultEnvelope and e5c-handoff.json; the stable E5B selected action schedule and SELECTED_POST_RESBM state; the qualified fixed N, security intent, scale intent, and key domain; the protected-site and baseline-DP provenance manifests;
- accepted P1a public-header and symbol evidence; accepted P1b ANT build, lifecycle, capability, and no-server-secret evidence.
E5C does not repair or reinterpret an E5B schedule. A stale or unsupported schedule is rejected or returns to the complete qualified baseline only through the accepted whole-profile fallback rule.
### 1.4 Ownership and independent review

The proposed stage owner is the Open64 CKKS finalization and materialization owner.
Required reviewers are:

- the E5B planner and selected-post-state owner; the common/com serialization and compatibility owner; an independent CKKS parameter and state reviewer; an independent cryptographic security reviewer; the ACE rtlib/ANT interface and build owner;
- an independent generated-C header and symbol reviewer; the Open64 backend and whirl2c owners; a direct-call and control-flow reviewer; a key-lifecycle and no-server-secret reviewer; an ownership, alias, lifetime, and failure-order reviewer;
- an independent result-oracle and evidence reviewer; the O2-E6 consumer owner.
One common/com editor owns any physical record change. Open64 remains the sole owner of CKKS-and-above semantic IR and the selected final plan.
### 1.5 Entry gate

Implementation may begin only when:

- PRE-O2-LOCK-EXIT is accepted; O2-O0Q-001=Qualified is current; O2-E1-EXIT through O2-E5B-EXIT are accepted and current; E5B source, E5A input, result envelope, and handoff independently reopen; E5B has a verified selected, bypass, or whole-baseline-fallback disposition;
- fixed N, selected layout, protected sites, and immediate Relin remain exact; the accepted P1a interface manifest matches the header and symbol lock; P1b has closed ANT-RT-TODO-CLIENT-SERVER-LIFECYCLE; P1b proves the server evaluator receives no secret key or decryptor; a reviewed isolated resolver/probe topology is accepted;
- no direct be.so dependency on ACE rtlib is present or proposed; the security estimator, target bits, and provider capabilities are pinned; no unresolved master or ADR conflict affects the runtime boundary.
Planning text may be reviewed before these gates close. No E5C production source, fixture, resolver result, generated code, or acceptance artifact may be created early.
### 1.6 Consumed evidence IDs

The stage manifest binds at least:

- O2-O0Q-001; O2-S10-001; O2-P2-001; O2-S11-001; O2-S12-001;
- applicable O2-S13-001 and O2-S14-001; O2-S15A-001; O2-S16-001; O2-S17-001; O2-S17-PROTECTED;
- O2-E5A-EXIT; O2-E5B-EXIT; accepted P1a and P1b evidence identifiers; O2-S15B-001 and O2-S18-001 as governing E5C tests.
### 1.7 Stale triggers

E5C stops and discards unpublished outputs when any of these changes:

- governing master, ADR, O2 plan, or option policy; O2 qualification fingerprint; record schema, capability, serialization, or normalization version; source graph, PU, node, value, edge, call, or action identity; selected layout, slot map, transformed data, or E5A snapshot;
- E5B regions, protected sites, schedule, disposition, or post-state; fixed N, key domain, scale intent, error budget, or security target; security estimator or accepted security rule; ACE commit, header hash, build flags, library hash, or symbol inventory; generated-C include or symbol allowlist;
- runtime lifecycle, ownership, alias, status, or cleanup contract; logical or provider key-expansion rule; target triple, ABI, compiler, linker, loader, or sanitizer configuration.
The stale report names the first changed input, affected output IDs, downstream invalidations, and required requalification. It never mutates the old plan.
## 2. Objective and User-Visible Capability

O2-E5C converts the independently reopened E5B final schedule and post-state into one verified final CKKS plan, one exact LIB_ANT projection, and one standard-WHIRL/generated-C execution path.
At exit:

- provisional and pre-ReSBM state are not reused as final state; selected E5B actions have been replayed exactly; fixed N is unchanged; Q, P, CRT, scale, level, range, error, and security are final; bootstrap profiles are complete and provider-supported;
- logical key requirements and ANT-expanded key requirements are separate; every provider capability used by the plan is present; the reopened final plan is the sole materialization decision source; canonical MulCC maps to ANT Mul_ciph3 followed by adjacent ANT Relin; duplicate Relin is rejected;
- all private FHE/O2 nodes are gone at the whirl2c boundary; generated C includes only public ACE rtlib headers and allowed symbols; the program links only FHErt_common and FHErt_ant for FHE execution; client/test and server-evaluation lifecycles remain separated; the server path contains no secret key, decryptor, decrypt call, or secret material;
- decoded outputs pass the clear semantic oracle and persisted error bounds.
This exit is not O2 Stage 1 acceptance and is not complete O2 acceptance.
## 3. Scope, Non-Goals, and Support

### 3.1 In scope

This stage includes:

- independent E5B handoff reopen and complete fingerprint validation; exact replay of the selected E5B schedule; verification of SELECTED_POST_RESBM state; finalization of remaining CKKS parameters; fixed-N capacity and achieved-security validation;
- exact ordered Q and P prime identities and bit lengths; CRT basis and precomputation fingerprints; final scale and level mapping; bootstrap profile completion and capability validation; final range, error, and precision checks;
- logical rotation, Relin, bootstrap, and key-switch requirements; ANT-specific key expansion as derived evidence; strict CKKS_PARAMS projection; isolated ANT context resolution and exact query comparison; bounded direct-call and control-flow materialization;
- stable critical-edge action placement; transformed plaintext and data descriptors; standard WHIRL call materialization; generated C, compile, link, load, run, status, and cleanup; public-header, symbol, dependency, and secret audits;
- final structural, semantic, state, security, and result verification; atomic publication and producer-free independent reopen; matching .B and same-stem .T evidence; exact O2-E6 handoff and invalidation metadata.
### 3.2 Explicit non-goals

O2-E5C does not:

- rerun ReSBM or baseline DP; change an E5B action, endpoint, cut, region, tie, or cost decision; change the selected layout, slot map, or transformed plaintext; move, remove, merge, or reclassify protected/manual/pre-ReLU sites; change requested N or silently select a larger N;
- accept q_w not equal to q or a non-uniform ReSBM schedule; introduce lazy or non-adjacent relinearization; map canonical MulCC to fused Mul_ciph; emit fused Mul_ciph and then an explicit duplicate Relin; import ACE AIR or private compiler headers;
- create an opaque dsc_fhe facade; make CKKS_PARAMS or provider memory the semantic source of truth; add FHErt_common or FHErt_ant linkage to be.so; assume a direct source-level ACE dependency in be.so; link FHErt_ant_encode or any provider other than the two selected libraries;
- persist a secret key, decryptor, or secret-derived private material; permit server evaluation to initialize or receive decryption capability; implement MetaKernel, ReSBM, FHEFusion, HPOLY, or HPAO; expand bootstrap internals into private WHIRL; claim ciphertext-byte equality as correctness;
- widen the accepted direct-call, SCC, loop, or frequency domain; claim Stage 1 or final O2 acceptance.
### 3.3 Inherited invariants

The following invariants are mandatory:

- binary WHIRL is the compiler process boundary; Open64 owns CKKS-and-above semantic IR and final planning state; runtime projection is checked and one-way; runtime rejection is allowed; runtime plan mutation is not; fixed N exists before canonicalization and remains unchanged;
- S equals N divided by 2; every ordinary ciphertext state has two components; MulCC has one transient three-component state only; each MulCC is followed immediately by one mandatory Relin; no ordinary consumer observes the transient state;
- protected-site identity, order, and semantics remain exact; all-PU publication is atomic; unknown required capability fails closed; provider failures preserve observable order and cleanup; no secret material enters WHIRL, generated C, manifests, or retained evidence;
- every meaningful WHIRL .B has a same-stem .T made by ir_b2a -st -src.
### 3.4 Support scopes

| Scope | E5C responsibility |
| --- | --- |
| o0-handoff-required | Validate fixed N, shared CKKS records, P1a/P1b runtime boundary, protected sites, and lifecycle. |
| shared-o0-o2-comparison-required | Use the same inputs, N, provider, parameter policy, and result oracle as the qualified O0 path. |
| o2-core-only | Finalize the E5B plan and materialize the bounded direct-call/control-flow domain. |
| o2-extension-only | Reject an unaccepted provider operation, bootstrap profile, lifecycle, or control-flow extension. |
| fallback-required | Return atomically to the complete qualified baseline when accepted policy permits; never mix schedules. |
## 4. Dependency Models

### 4.1 Implementation and build dependency

The implementation dependency chain is:

    current O2-O0Q-001 qualification
      -> E1 claims and runtime locks
      -> E2 records and atomic transaction
      -> E3 CKKS semantics and effects
      -> E4 selected layout and transformed data
      -> E5A canonical pre-ReSBM state
      -> E5B selected schedule and post-state
      -> E5C finalization, projection, materialization, and execution
      -> E6 Stage 1 acceptance
Proposed Open64 component paths are:

    osprey/be/vho/fhe_o2_final_ckks.h  osprey/be/vho/fhe_o2_final_ckks.cxx  osprey/be/vho/fhe_o2_ant_projection.h
    osprey/be/vho/fhe_o2_ant_projection.cxx  osprey/be/vho/fhe_o2_materialize.h  osprey/be/vho/fhe_o2_materialize.cxx
    osprey/be/vho/fhe_o2_materialize_verify.h  osprey/be/vho/fhe_o2_materialize_verify.cxx  osprey/tools/fhe_ant_param_resolver/
    osprey/be/vho/tests/run_ckks_state_contract.py  osprey/be/vho/tests/run_o2_materialization_contract.py  osprey/be/vho/tests/run_e5c_ant_gate.py
    testdata/fhe_o2/ckks/  testdata/fhe_o2/materialization/  testdata/fhe_o2/e5c-ant/
These are proposed locations, not claims that the files or targets exist.
### 4.2 Compiler pass-order dependency

The compiler execution order is:

    reopen selected E5B plan
      -> replay and verify selected actions
      -> verify SELECTED_POST_RESBM state
      -> finalize Q/P/CRT/scales/levels/profiles/security
      -> collect logical keys
      -> expand and verify ANT keys/capabilities
      -> publish final Open64 plan atomically
      -> exit final-plan producer
      -> independent materializer reopens final plan
      -> emit standard WHIRL calls and transformed data descriptors
      -> verify no private operation remains
      -> write materialized.o2.mid.B
      -> ir_b2a -st -src to materialized.o2.mid.T
      -> whirl2c generated C
      -> compile/link FHErt_common and FHErt_ant
      -> separated client/server run, decode, compare, cleanup
Materialization cannot run from an in-memory planner object. Finalization cannot change the E5B schedule to satisfy a provider.
### 4.3 Admissible resolver boundary

The default topology is an isolated, deterministic resolver/probe executable:

- proposed target name: fhe_ant_param_resolver; it is a standalone tool and not part of be.so; it includes only reviewed public ACE rtlib C headers; it links only FHErt_common and FHErt_ant; it accepts a normalized, hash-bound resolution request;
- it emits a strict, versioned result with complete Q/P/CRT data; it runs in a separate process; it owns no Open64 semantic decision; its output is accepted only after Open64 validation and security checking; the accepted output becomes part of CKKSResolvedParameterIR;
- a second runtime-context probe must match the persisted result exactly.
The Open64 driver or an accepted orchestration layer schedules the request, resolver process, and independent finalizer/materializer reopen. be.so does not load the resolver library and does not acquire its symbols.
If this topology is not accepted or cannot provide exact required fields, stop with an unresolved dependency decision. A direct be.so dependency is not an alternative authorized by this plan.
### 4.4 Build and link impact

Required build properties are:

- be.so has no new ACE-defined or ACE-undefined symbol; be.so includes no ACE rtlib or ACE compiler header; known be.so consumers remain link-closed without ACE rtlib; generated programs link FHErt_common and FHErt_ant only; the isolated resolver links FHErt_common and FHErt_ant only;
- no frontend-only builder library enters be.so; no FHErt_ant_encode, LIB_ACE, SEAL, OpenFHE, or Phantom dependency appears; static and shared runtime builds are recorded separately; target, compiler, linker, license, RPATH, and distribution effects are recorded in the provider manifest.
Any proposed direct be.so dependency triggers the AGENTS.md dependency review and stops E5C. It is not accepted merely because a local link succeeds.
### 4.5 External environment

Required external evidence includes:

- pinned Ubuntu 22.04 x86_64 execution host; accepted Open64 build and supported whirl2c; ir_b2a with -st and -src; original source at the DST-recorded path; pinned ACE source, installed headers, libraries, and build flags;
- accepted security estimator and target; accepted split client/test and server-evaluation lifecycle; sanitizer or equivalent lifetime instrumentation; deterministic artifact storage retained after test exit.
## 5. Authoritative Data and State Contracts

### 5.1 Input ownership and mutation

| Input | Owner | E5C use | Mutation rule |
| --- | --- | --- | --- |
| FHECompilationConfigIR | O0/common-com | Fixed N, security, bootstrap and precision intent | Read-only; N never changes. |
| Selected layout and slot map | E4 | Capacity, slots, rotations, transformed data | Read-only. |
| CANONICAL_PRE_RESBM state | E5A | Replay origin | Read-only. |
| ReSBMResultEnvelope | E5B | Selected actions, disposition, post-state, protected provenance | Read-only; no replanning. |
| P1a interface manifest | Runtime owner | Header, symbol, ABI and build lock | Read-only. |
| P1b capability/lifecycle manifest | Runtime/security owner | Provider support and server separation | Read-only. |
| Security estimator lock | Crypto owner | Achieved-security validation | Read-only and version-bound. |
| Final Open64 records | E5C/common-com | Authoritative final CKKS and materialization input | New immutable result after verification. |
| CKKS_PARAMS and runtime context | E5C projection/ANT | Execution projection and comparison evidence | Derived; never overrides Open64. |
### 5.2 E5B handoff acceptance

The consumer first:

1. exits the E5B producer;
2. reopens source, E5A state, E5B plan, and e5c-handoff.json;
3. verifies every version, ID, range, hash, and source relationship;
4. accepts only selected, manual-bypass, off-bypass, or baseline-fallback;
5. rejects an error, partial, mixed, or unknown disposition;
6. replays all selected and preserved actions in stable order;
7. independently reconstructs SELECTED_POST_RESBM state;
8. checks fixed N, layout, protected sites, range, error, and Relin adjacency.
Any mismatch rejects the handoff before parameter resolution or provider use.
### 5.3 Final CKKS parameter record

CKKSResolvedParameterIR contains or references:

- schema, capability, and normalization versions; source, config, qualification, E5A, E5B, and selected-plan IDs; resolution algorithm and provenance; fixed requested N and derived active slots; encryption and key-domain IDs;
- secret-key distribution identifier, never secret material; target security bits, estimator identity, inputs, and achieved result; complete ordered Q prime values, IDs, and bit lengths; complete ordered special P prime values, IDs, and bit lengths; Q and QP basis fingerprints;
- decomposition base, count, q-part mapping, and partition size; required CRT tables and their normalized fingerprints; scaling technique and rounding policy; first-prime and scaling-prime identities and bit lengths; level numbering direction, entry level, terminal level, and provider mapping;
- symbolic scale ID, provider scale, exact or log2 bits, and compatibility rules; required multiplicative depth and consumed-prime schedule; final value ranges and absolute and relative error bounds; bootstrap profile IDs and complete profile records; logical and provider key-manifest references;
- provider capability and runtime build fingerprints.
Checked arithmetic is required for every size, count, product, bit sum, and range. Saturation is rejection, not a conservative result.
### 5.4 Bootstrap profile record

Every selected bootstrap action references a profile containing:

- profile ID and content hash; supported slot count; legal input state and level; explicit output state and level; level budget and ordered consumed-prime sequence;
- input and output scale identities; BSGS dimensions; secret-key distribution identifier; correction factor and iteration count; accepted input range;
- output absolute and relative error bounds; provider setup parameters; logical and ANT-expanded key requirements; exact required capability IDs; source and resolution provenance.
An absent or approximate match is unsupported. E5C does not choose a nearby profile or alter the E5B bootstrap result level.
### 5.5 Security contract

Security validation:

- uses the unchanged fixed N; uses the complete final Q and P envelope; names the estimator version and parameterization; validates the requested target bits; records achieved security and all estimator inputs;
- validates secret distribution and hamming-weight assumptions; validates bootstrap and key-switch material assumptions; rejects an insecure or unrecognized combination; reports a minimum acceptable or recommended N only as a diagnostic; never changes the compilation request.
An independent security calculation must agree with the production decision. Provider support is necessary but is not security proof.
### 5.6 CKKS value and action verification

Each final value state binds:

- parameter ID; value and version ID; exact remaining Q-prime IDs; chain position and provider level; symbolic and provider-native scales;
- component count; layout and slots; range and error bounds; plaintext compatibility; completed and pending obligations.
Every action records input state IDs, consumed primes, output state ID, preconditions, range/error transfer, reason, source lineage, and provider capability. All ordinary values have two components.
### 5.7 CKKS_PARAMS projection

The exact field projection is:

| CKKS_PARAMS field | Open64 source |
| --- | --- |
| _provider | Constant LIB_ANT |
| _poly_degree | Unchanged fixed N |
| _sec_level | Accepted target/security mapping |
| _mul_depth | Verified required depth |
| _input_level | Final entry-level mapping |
| _first_mod_size | Final first-prime bit length |
| _scaling_mod_size | Final scaling-prime/scale bit policy |
| _num_q_parts | Final decomposition q-part count |
| _hamming_weight | Accepted secret-distribution parameter |
| _num_rot_idx | Canonical logical rotation count after ANT mapping validation |
| _rot_idxs | Stable normalized signed rotation list |
CKKS_PARAMS cannot represent complete Q/P values, CRT tables, all scale identities, error bounds, or bootstrap profiles. Those remain in the Open64 record and are compared against the isolated resolver and runtime query report.
### 5.8 Resolver request and response

The request binds:

- fixed N and security inputs; required depth and entry level; prime-size policy; decomposition intent; hamming weight;
- logical rotations; bootstrap profile obligations; E5B result and finalizer version; ACE commit, header, build, and symbol hashes; request schema and digest.
The response contains exact Q/P values, CRT dimensions and values, provider scale/level data, capability results, normalized hashes, tool/build identity, status, and request digest. Unknown fields, missing arrays, inconsistent counts, duplicate primes, invalid modulus order, or noncanonical encoding fail closed.
### 5.9 Logical and ANT-expanded keys

Logical requirements are authoritative:

- signed rotations normalized to [-S/2,S/2); zero removed and duplicates canonicalized; first consumer retained; one Relin-key requirement for each key domain using MulCC; bootstrap requirements reference exact profile IDs;
- explicit KeySwitch and conjugation needs remain distinct.
ANT expansion is derived:

- logical rotations map to exact automorphism or Galois elements; Relin, conjugation, decomposition, auxiliary, and bootstrap keys are added only by the accepted provider rule; each derived item names its logical cause and first consumer; logical equality and provider-expanded equality are tested separately; random key bytes need not be deterministic;
- key classes, indices, profiles, and fingerprints must be deterministic.
## 6. Public C Header and Symbol Allowlist

### 6.1 Direct header allowlist

Generated kernel C may directly include:

- rt_ant/rt_ant.h.
The generated runtime harness may directly include:

- common/rtlib.h; rt_ant/rt_ant.h.
The isolated resolver may directly include:

- common/common.h; common/cmplr_api.h.
All are public ACE rtlib C surfaces at the locked revision. Direct inclusion of ACE compiler AIR/IR headers, lib_provider.h, private implementation headers, or unlisted provider headers is rejected.
### 6.2 Isolated resolver symbol allowlist

The isolated resolver may reference exactly:

- Prepare_context_for_cmplr; Finalize_context_for_cmplr; Get_q_cnt; Get_p_cnt; Get_qlhmodp_dim3_cnt;
- Get_phmodq_dim1_cnt; Fetch_q_primes; Fetch_p_primes; Fetch_qlhinvmodq_at; Fetch_qlhmodp_at;
- Fetch_qlinvmodq_at; Fetch_phmodq_at; Fetch_qlhalfmodq_at; Fetch_phinvmodp; Fetch_pinvmodq.
Any extra symbol requires a reviewed allowlist and hash update. It cannot be silently accepted from a transitive header.
### 6.3 Generated definition hooks

Generated C may define:

- Get_context_params; Get_rt_data_info; Get_input_count; Get_output_count; Get_encode_scheme;
- Get_decode_scheme; Main_graph.
Get_extra_context_params is not in the initial E5C allowlist because it can silently merge rotation indices. Enabling it requires an exact key-census and duplicate/ordering review.
### 6.4 Test/client harness calls

Only the isolated test/client process may call:

- Prepare_context; Finalize_context; Prepare_input; Prepare_input_dup; Handle_output;
- Run_main_graph.
Current Prepare_context is not server-admissible because the inspected implementation co-constructs key generation, encryptor, decryptor, and evaluator state. P1b must provide an accepted public split lifecycle or E5C is Unverified.
### 6.5 Generated evaluation-call allowlist

The initial Stage 1 evaluation allowlist is:

- Get_input_data; Set_output_data; Free_cipher; Init_ciph_same_scale; Init_ciph_same_scale_plain;
- Init_ciph_same_scale_ciph3; Init_ciph3_up_scale; Init_ciph_down_scale; Init_ciph_up_scale_plain; Copy_ciph;
- Set_slots; Add_ciph; Add_plain; Sub_ciph; Mul_ciph3;
- Mul_plain; Relin; Rescale_ciph; Upscale_ciph; Raise_mod;
- Downscale_ciph; Modswitch_ciph; Rotate_ciph; Bootstrap; Encode_float;
- Encode_double; Encode_float_mask; Encode_double_mask; Encode_float_with_scale; Copy_plain.
Only the subset required by a verified plan may appear in that generated file. Mul_ciph, Real_relu, Encrypt, Get_msg, Get_msg_with_imag, Decrypt, key-generation APIs, low-level bootstrap stages, and unlisted LPOLY/POLY/HAL symbols are forbidden in Stage 1 generated C.
### 6.6 Library allowlist

The final FHE link closure contains:

- FHErt_common; FHErt_ant; ordinary system libraries already required by the accepted ANT build and recorded in its manifest.
FHErt_ant_encode, FHErt_ace, FHErt_seal, FHErt_openfhe, FHErt_phantom, and an opaque dsc_fhe library are forbidden.
## 7. Materialization and Execution Contracts

### 7.1 Canonical operation mapping

| Open64 semantic action | ANT public C mapping | Required check |
| --- | --- | --- |
| AddCC | Add_ciph | Level, scale, layout, alias, and output state exact. |
| AddCP | Add_plain | Plain compatibility, level, scale, and payload hash exact. |
| SubCC | Sub_ciph | Same checks as AddCC. |
| MulCP | Mul_plain | Scale growth, output state, and later selected actions exact. |
| MulCC | Mul_ciph3 then adjacent Relin | One transient CIPHER3, one Relin, no intervening call. |
| Rescale | Rescale_ciph | Exact consumed Q-prime and output scale/level. |
| ModSwitch | Modswitch_ciph | Exact target level and no hidden rescale. |
| AddAlign | Verified Upscale/Downscale/ModSwitch sequence | Exact selected actions; no provider choice. |
| Rotate | Rotate_ciph | Signed offset and logical/provider key exact. |
| Bootstrap | Bootstrap | Exact profile, input state, output level, scale, and error. |
| Encode | Accepted Encode function | Exact data hash, level, scale, and slot map. |
An Open64 KeySwitch without an accepted high-level public mapping is rejected. The materializer cannot expand it into private provider internals.
### 7.2 Mandatory multiply and Relin mapping

The sole canonical ciphertext multiply mapping is:

    Open64 MulCC(two,two)
      -> ANT Mul_ciph3
      -> immediately adjacent ANT Relin
      -> Open64 ordinary two-component result
The materializer rejects:

- MulCC mapped to Mul_ciph; Mul_ciph followed by explicit Relin; Mul_ciph3 without Relin; any call, store, phi, branch, lifetime action, or consumer between the pair; a second Relin on the resulting two-component value;
- a three-component ordinary SSA value; a moved Relin caused by WOPT, scheduling, or control-flow lowering.
### 7.3 Direct calls and control flow

Accepted S1.8 behavior is limited to:

- nonrecursive direct calls with one resolved body; persisted formal/actual state and key-domain contracts; call depth at most 8; exact positive frequency or proven unreachable zero; compile-time-known zero-level retained loops;
- trip count at most 64; at most 10000 expanded encrypted nodes after legal unrolling; stable critical-edge action identity; explicit phi and return joins.
Recursion, mutually recursive calls, indirect calls, unresolved externals, unknown frequency, unknown trip, scale-changing recurrence, and retained multiplication SCCs fail before final-plan publication.
### 7.4 Effect, alias, lifetime, and failure order

Every runtime call records:

- immutable-result or in-place behavior; input/output and may-alias sets; allocation ownership; escape and return ownership; context and key read effects;
- status and failure behavior; cleanup action and dominance; source action and state IDs.
Provider calls are ordered and effectful by default. Ordinary WOPT may optimize surrounding scalar control flow but may not CSE, PRE, hoist, speculate, merge, duplicate, or remove an FHE runtime action.
Each allocation is released exactly once on success and every failure path. Cleanup cannot observe or introduce secret material in the server process.
### 7.5 Standard WHIRL boundary

The materializer:

1. reopens the accepted final plan in a fresh process;
2. validates source, plan, provider, and transformed-data fingerprints;
3. replaces each virtual operation with standard WHIRL calls;
4. emits explicit descriptors, lifetimes, status checks, and cleanup;
5. preserves source position and stable action lineage;
6. runs WHIRL, DSL, FHE, CKKS, effect, and plan verifiers;
7. rejects every remaining private FHE, SIHE, CKKS, O2, or HPOLY operation;
8. atomically writes materialized.o2.mid.B.
No in-memory shortcut from the planner to generated C is accepted.
### 7.6 Generated C and linkage

whirl2c receives only the verified standard-WHIRL artifact. Generated C:

- includes only the direct allowlisted public headers; defines only required generated hooks; calls only the per-plan subset of allowed symbols; carries no Open64 planner or private node representation; contains no secret, decryptor, private key, or raw random key bytes;
- records transformed-data content hashes and descriptors; compiles as C under the accepted ABI; links to the exact accepted FHErt_common and FHErt_ant libraries; fails link audit on an extra provider or undefined nonallowlisted symbol.
### 7.7 Client/test and server-evaluation separation

The accepted execution topology has at least:

- a client/test process that may create ephemeral test keys, encrypt inputs, receive outputs, decrypt, decode, compare, and destroy all private material; a server-evaluation process that receives ciphertexts, public/evaluation keys, public parameters, and transformed plaintexts only; an authenticated or hash-bound exchange format; no shared address space carrying a secret or decryptor into the server; no secret-bearing retained artifact.
The server symbol and object scan forbids Prepare_context under the inspected lifecycle, Decrypt, Get_msg, Get_msg_with_imag, Real_relu, Encrypt, secret-key generation, and decryptor construction. If accepted P1b cannot provide a separate public server initialization path, E5C cannot close.
### 7.8 Result verification

Correctness compares decrypted and decoded values to the clear tensor oracle. Ciphertext bytes are never compared.
Required gates are:

- clear layout/kernel abs and rel error at most 1e-12; focused encrypted elementwise abs error at most 1e-4; focused encrypted elementwise rel error at most 1e-6; required-zero decoded absolute value at most 1e-8; application max absolute error at most 1e-3 when exercised by this stage;
- application max relative error at most 1e-4 for reference magnitude at least 1e-2 when exercised by this stage; identical top-1 with lowest-index tie handling when applicable; no NaN or Inf; every observed error within its persisted bound.
Sentinel-filled junk and gap slots must not contaminate valid or required-zero outputs.
### 7.9 Serialization and atomic publication

Final records use:

- fixed-width fields; invalid-zero IDs; checked first/count ranges; explicit versions and capabilities; deterministic ordering;
- no pointers or host-sized enums; no nested STL container in mapped-image records; complete source/config/plan/provider hashes.
The proof sequence is producer build, whole-program verify, atomic publication, producer exit, independent reopen, normalized digest comparison, materializer publication, second independent reopen, and same-stem .T generation.
Old and feature-absent WHIRL retains accepted behavior. Unknown required versions fail closed. O2 records do not become an O0 runtime prerequisite.
## 8. Diagnostics and Fallback

The accepted E1 catalog owns exact spellings. Proposed purposes are:

| Purpose | Proposed stable diagnostic |
| --- | --- |
| E5B or qualification input stale | FHE-O2-FINAL-PLAN-STALE |
| E5B action replay or post-state mismatch | FHE-O2-POST-STATE-MISMATCH |
| Fixed N changed or missing | FHE-O2-FIXED-N-MISMATCH |
| Q/P or CRT invalid | FHE-O2-CKKS-PARAMETER-MISMATCH |
| Achieved security below target | FHE-O2-CKKS-SECURITY-FAILED |
| Provider ring or profile unsupported | FHE-O2-ANT-CAPABILITY-MISMATCH |
| Runtime context differs from final record | FHE-O2-ANT-CONTEXT-MISMATCH |
| Resolver request or response stale/corrupt | FHE-O2-ANT-RESOLVER-MISMATCH |
| Direct be.so dependency detected | FHE-O2-BACKEND-DEPENDENCY-UNAPPROVED |
| MulCC mapping is not Mul_ciph3 then Relin | FHE-O2-ANT-MUL-RELIN-MAPPING |
| Duplicate Relin | FHE-O2-CKKS-DUPLICATE-RELIN |
| Nonallowlisted header or symbol | FHE-O2-ANT-ALLOWLIST-VIOLATION |
| Unsupported call or control flow | FHE-O2-CONTROL-FLOW-UNSUPPORTED |
| Server receives secret or decryptor | FHE-O2-SERVER-SECRET-MATERIAL |
| Lifetime or cleanup is unbalanced | FHE-O2-ANT-LIFETIME-FAILED |
| Private operation reaches whirl2c | FHE-O2-MATERIALIZATION-INCOMPLETE |
| Atomic publication fails | FHE-O2-MATERIALIZATION-PUBLISH-FAILED |
Fallback is the complete qualified O0 profile only where accepted policy and support rows permit it. E5C never combines an E5B schedule with baseline state, keys, parameters, or provider data. Provider rejection before publication may select the whole baseline atomically; rejection after publication invalidates the candidate bundle and requires a fresh run.
## 9. Detailed Work Breakdown

| Task ID | Concrete change | Components | Dependency type | Tests and artifacts | Owner/reviewer | Merge or exit rule |
| --- | --- | --- | --- | --- | --- | --- |
| E5C-W01 | Freeze consumed IDs, source/header/library hashes, support rows, diagnostics, and stale triggers. | Stage manifest | Governance/build | Lock and mismatch tests | Stage owner; architecture reviewer | No pending runtime authority. |
| E5C-W02 | Reopen and validate the exact E5B handoff. | Finalizer input adapter | Pass order | Reopen and corruption corpus | E5B/E5C owners | No producer memory or partial disposition. |
| E5C-W03 | Replay selected actions and reconstruct post-state independently. | CKKS verifier | Pass order | Action/state oracle | CKKS owner; E5B reviewer | Exact SELECTED_POST_RESBM equality. |
| E5C-W04 | Define final parameter record ownership and version path. | common/com crosswalk | Build | Compatibility audit | Record owner; common/com reviewer | One authority per field. |
| E5C-W05 | Implement fixed-N depth, scale, and level finalization. | CKKS finalizer | Implementation | Boundary suites | CKKS owner; crypto reviewer | Zero N mutation. |
| E5C-W06 | Implement exact Q/P and decomposition finalization. | CKKS finalizer | Implementation | Prime/order/count tests | CKKS owner; crypto reviewer | Exact ordered basis. |
| E5C-W07 | Implement CRT value/fingerprint contract. | CKKS finalizer | Implementation | Independent CRT recomputation | Crypto owner; oracle reviewer | Every table exact. |
| E5C-W08 | Implement security-estimator validation. | Security adapter | Implementation | Below/equal/above target | Security owner; independent reviewer | Target met without N change. |
| E5C-W09 | Finalize bootstrap profiles, range, error, and precision. | Profile/state verifier | Pass order | Profile boundaries and negatives | CKKS owner; numerical reviewer | Exact state and conservative bounds. |
| E5C-W10 | Finalize logical key requirements. | Key collector | Implementation | Independent logical census | Key owner; CKKS reviewer | Exact signed rotations and profiles. |
| E5C-W11 | Implement ANT key expansion and capability checks. | Provider adapter | Implementation | Independent expansion | Runtime owner; security reviewer | Exact derived manifest. |
| E5C-W12 | Define strict CKKS_PARAMS field projection. | Projection library | Implementation | Field-by-field comparison | CKKS/runtime owners | No provider override. |
| E5C-W13 | Build isolated resolver request/result schemas. | Resolver tool | Build boundary | Schema, stale, corruption tests | Build owner; common/com reviewer | Standalone only. |
| E5C-W14 | Implement public C resolver/probe allowlist. | Resolver tool | Build boundary | Include/symbol/library audit | Runtime owner; build reviewer | Only FHErt_common/FHErt_ant. |
| E5C-W15 | Compare resolver and runtime Q/P/CRT exactly. | Projection verifier | Verification | Exact query comparison | Runtime owner; crypto reviewer | Zero field mismatch. |
| E5C-W16 | Implement canonical operation-to-ANT mapping. | Materializer | Pass order | Per-operation structural tests | Materializer owner; runtime reviewer | Every action mapped once. |
| E5C-W17 | Enforce Mul_ciph3 then adjacent Relin. | Materializer verifier | Pass order | Missing/fused/delayed/duplicate negatives | CKKS owner; WOPT reviewer | Exact adjacency. |
| E5C-W18 | Materialize direct calls, phis, returns, and critical edges. | Materializer | Pass order | Bounded positive/negative graph tests | Backend owner; CFG reviewer | Only accepted control flow. |
| E5C-W19 | Materialize transformed data descriptors and encodes. | Data materializer | Implementation | Hash/slot/scale tests | Layout owner; runtime reviewer | Exact payload and state. |
| E5C-W20 | Add alias, ownership, status, and cleanup lowering. | Materializer/runtime adapter | Implementation | Failure at every call | Runtime owner; reliability reviewer | No leak or reordered failure. |
| E5C-W21 | Publish final plan and materialized WHIRL atomically. | E2 transaction/driver | Build | Failure injection | Transaction owner; reliability reviewer | No valid-looking partial .B. |
| E5C-W22 | Add producer-free final-plan and materialized reopen. | Reader/verifier | Verification | Two producers and consumers | Record owner; independent reviewer | Equal normalized digests. |
| E5C-W23 | Add final private-op and WOPT non-interference audits. | WHIRL/FHE verifier | Pass order | Census and optimizer negatives | Backend/WOPT owners | No private op or moved action. |
| E5C-W24 | Implement split client/test and server run harness. | Runtime tests | Security/build | Process and object scans | Security/runtime owners | No server secret/decryptor. |
| E5C-W25 | Compile, link, load, run, decode, and compare. | End-to-end runner | Verification | Focused S1.8 suite | Test owner; oracle reviewer | All semantic/result gates pass. |
| E5C-W26 | Produce .B/.T, C, manifests, logs, and hashes. | Evidence runner | Verification | Evidence audit | Evidence owner; independent reviewer | Complete retained bundle. |
| E5C-W27 | Audit be.so and all selected link closures. | Build tooling | Build | Defined/undefined and dependency diffs | Build owner; consumer reviewer | No unauthorized dependency. |
| E5C-W28 | Review and sign the E6 handoff. | Handoff manifest | Exit | Validation-only consumer dry run | E5C/E6 owners | E6 reopens without rematerializing. |
## 10. Proposed Commit and Pull Request Sequence

Every command and path in Sections 10 and 11 is a proposed future interface, not a claim that it exists or was executed while writing this plan.
Each commit has one semantic goal and maps to governing groups S1-11B, S1-12, or S1-13:

1. e5c-contract-lock adds manifests, traceability, diagnostics, failing tests, and the reviewed header/symbol/library allowlists.
2. e5c-post-state-replay independently reopens E5B and verifies final schedule replay without parameter or provider work.
3. e5c-final-parameters adds fixed-N Q/P/CRT, scale, level, range, error, and bootstrap-profile finalization.
4. e5c-security adds pinned estimator validation and security negatives.
5. e5c-resolver-boundary adds the standalone public-C resolver/probe plus strict request/result schemas; no be.so dependency.
6. e5c-ant-projection adds CKKS_PARAMS, capability, and exact context comparison.
7. e5c-logical-keys adds independent logical collection.
8. e5c-ant-keys adds separately reviewed provider expansion.
9. e5c-materialize-core adds standard-call mapping and transformed descriptors.
10. e5c-mul-relin adds canonical Mul_ciph3-to-Relin adjacency and negatives.
11. e5c-control-flow adds the bounded direct-call, phi, loop, and critical-edge extension with rejection tests.
12. e5c-lifetime adds alias, status, failure ordering, and cleanup.
13. e5c-transaction-reopen adds atomic publication, corruption tests, and two independent process boundaries.
14. e5c-client-server adds split lifecycle execution and no-secret evidence.
15. e5c-end-to-end adds generated-C compile/link/load/run and result checks.
16. e5c-acceptance-evidence adds only the runner, bundle schema, and decision template; no algorithm or support change is allowed.
A common/com physical change is isolated in its own reviewed commit. A new symbol, provider operation, bootstrap profile, or control-flow extension needs a plan amendment, support row, independent oracle, and separate commit.
Generated run artifacts are not committed unless an accepted contract names a golden.
## 11. Proposed Verification Plan

### 11.1 Final CKKS and ANT projection

Test ID: O2-S15B-001
Proposed command:

    python3 osprey/be/vho/tests/run_ckks_state_contract.py --mode final-ant --build-dir build --manifest testdata/fhe_o2/ckks/SHA256SUMS --artifacts test-artifacts/o2/O2-S15B-001
Inputs include selected baseline/MetaKernel layouts, baseline-DP/ReSBM actions, fixed and auto-remaining parameter suites, pinned ANT manifests, and unsupported N, security, depth, chain, CRT, and bootstrap cases.
Protocol:

- seed 0x434b4b42; fixed N in 2^12 through 2^16; chain length 1 through 16; zero warmups; one resolver run per case;
- timeout 20 minutes; exact structural, parameter, state, security, key, and projection equality.
Pass requires zero N mutation, stale state, parameter mismatch, security mismatch, unreviewed be.so dependency, or provider override.
### 11.2 Standard-call materialization and execution

Test ID: O2-S18-001
Proposed command:

    python3 osprey/be/vho/tests/run_o2_materialization_contract.py --build-dir build --manifest testdata/fhe_o2/materialization/SHA256SUMS --artifacts test-artifacts/o2/O2-S18-001
Inputs include direct calls, phi/fanout/critical edges, zero-level loops, unrollable and rejected multiplication SCCs, residuals, MVM, Conv, ReLU, bootstrap, unknown-trip, recursion, indirect-call, and unknown-frequency cases.
Protocol:

- seed 0x4d415431; call depth at most 8; trip count at most 64; expanded encrypted nodes at most 10000; fixed N;
- pinned Ubuntu 22.04 x86_64; accepted P1a/P1b ACE rtlib/ANT manifests; two warmups and ten measured samples; timeout 30 minutes per sample; decoded elementwise tolerances 1e-4 absolute and 1e-6 relative.
Pass requires compile/link/load/run success, exact state and key manifests, no private operation, no retained multiplication SCC, no server secret/decryptor, no leak, and exact first diagnostics.
### 11.3 E5B handoff, final-plan transaction, and reopen

Test ID: O2-E5C-001
Proposed command:

    python3 osprey/be/vho/tests/run_e5c_ant_gate.py --build-dir build --manifest testdata/fhe_o2/e5c-ant/SHA256SUMS --mode final-plan-reopen --artifacts test-artifacts/o2/O2-E5C-001
Required checks:

- independent E5B input reopen; exact selected-action replay and post-state; two clean final-plan productions; producer exit; one independent reopen per result;
- exact normalized digests; failure-atomic publication; no materialization decision outside the reopened final plan.
Bounds are 1 through 8 PUs, at most 2000 operations, chain length 1 through 16, and timeout 20 minutes.
### 11.4 Resolver and runtime context probe

Test ID: O2-E5C-002
Proposed command:

    python3 osprey/be/vho/tests/run_e5c_ant_gate.py --build-dir build --manifest testdata/fhe_o2/e5c-ant/SHA256SUMS --mode resolver-context-probe --ace-root ../../ace-compiler --ace-commit 929e9b621f11bebbaa9ec1e215f4a52e3d07109b --artifacts test-artifacts/o2/O2-E5C-002
Pass requires:

- exact header hashes; only resolver-allowlisted symbols; only FHErt_common and FHErt_ant in link closure; exact Q/P values, counts, order, and bit lengths; exact CRT dimensions, values, and fingerprints;
- exact provider scale and level mapping; exact request/result digest binding; deterministic normalized output; complete cleanup after success and injected failure.
### 11.5 Generated-C header, symbol, and dependency audit

Test ID: O2-E5C-003
Proposed command:

    python3 osprey/be/vho/tests/run_e5c_ant_gate.py --build-dir build --manifest testdata/fhe_o2/e5c-ant/SHA256SUMS --mode generated-c-link-audit --artifacts test-artifacts/o2/O2-E5C-003
The audit preprocesses generated C, inventories direct and transitive headers, extracts defined and undefined symbols from C, objects, executables, libraries, be.so, lw_inline, plugins, and standalone backend tools, and compares link closures before and after E5C.
Any dsc_fhe, private ACE, extra provider, FHErt_ant_encode, Mul_ciph, decryptor, or unexpected be.so symbol fails the test.
### 11.6 Canonical multiply mapping

Test ID: O2-E5C-004
Proposed command:

    python3 osprey/be/vho/tests/run_e5c_ant_gate.py --build-dir build --manifest testdata/fhe_o2/e5c-ant/SHA256SUMS --mode mul-relin-mapping --artifacts test-artifacts/o2/O2-E5C-004
Positive cases cover straight-line, call, phi, fanout, critical edge, residual, and loop-unrolled MulCC. Negative cases inject fused Mul_ciph, missing Relin, delayed Relin, intervening consumers, duplicate Relin, and component-three SSA escape.
Pass is exact structural adjacency and exact state equality; numeric tolerance does not excuse a structural mismatch.
### 11.7 Lifecycle, secret, failure, and cleanup

Test ID: O2-E5C-005
Proposed command:

    python3 osprey/be/vho/tests/run_e5c_ant_gate.py --build-dir build --manifest testdata/fhe_o2/e5c-ant/SHA256SUMS --mode lifecycle-secret-failure --artifacts test-artifacts/o2/O2-E5C-005
The runner:

- starts separate client/test and server-evaluation processes; scans process maps, symbols, serialized exchange, generated C, WHIRL, manifests, logs, core-dump policy, and retained artifacts; injects failure before and after every context, input, call, output, and cleanup boundary; verifies exact status propagation and balanced release; proves server absence of secret key, decryptor, decrypt call, and private key bytes.
Any reliance on current unsplit Prepare_context in the server marks the gate Unverified.
### 11.8 WHIRL and generated-C evidence

Test ID: O2-E5C-006
Proposed commands:

    ir_b2a -st -src test-artifacts/o2/O2-E5C-001/final-resolved-plan.B test-artifacts/o2/O2-E5C-001/final-resolved-plan.T
    ir_b2a -st -src test-artifacts/o2/O2-S18-001/materialized.o2.mid.B test-artifacts/o2/O2-S18-001/materialized.o2.mid.T
    whirl2c test-artifacts/o2/O2-S18-001/materialized.o2.mid.B
    python3 osprey/be/vho/tests/run_e5c_ant_gate.py --build-dir build --manifest testdata/fhe_o2/e5c-ant/SHA256SUMS --mode evidence-audit --artifacts test-artifacts/o2/O2-E5C-006
The runner must capture the exact generated-C pathname rather than assume whirl2c naming. The original source must remain at its DST-recorded path. Missing -src support makes the result Unverified.
### 11.9 Verification matrix

| Test ID | Source/input | Expected | Seed/bounds | Platform/capability | Samples/timeout | Artifacts | Owner/pass rule |
| --- | --- | --- | --- | --- | --- | --- | --- |
| O2-S15B-001 | Current E5B plus final parameter suites | Exact state, Q/P/CRT, security, projection, keys | 0x434b4b42; N 2^12..2^16; chain 1..16 | Pinned ANT and resolver | 0/1; 20 min | Final records, query, state, keys, .B/.T | CKKS/runtime; zero field mismatch |
| O2-S18-001 | Bounded graph and kernel corpus | Structural and decoded correctness | 0x4d415431; stated graph bounds | Ubuntu 22.04 x86_64; P1a/P1b | 2/10; 30 min/sample | Plan/mid .B/.T, C, binary, outputs | Materializer; all hard gates |
| O2-E5C-001 | E5B result and transaction corpus | Deterministic final plan and reopen | 1..8 PUs; <=2000 ops | Supported Open64 host | 2 producers; 20 min | Digests, .B/.T, failures | Reliability; exact reopen |
| O2-E5C-002 | ACE source/header/build lock | Exact resolver/context result | Chain 1..16 | FHErt_common/FHErt_ant only | 1 each; 20 min | Requests, responses, Q/P/CRT | Crypto/build; exact equality |
| O2-E5C-003 | Generated C and link graph | Exact allowlist and no be dependency | Complete inventory | Accepted toolchain | 1 clean audit; 20 min | Includes, symbols, link maps | Build; zero unexpected edge |
| O2-E5C-004 | MulCC mapping corpus | Mul_ciph3 then one Relin | All control contexts | Pinned ANT symbols | 1; 20 min | Action/C/WHIRL traces | CKKS/WOPT; exact adjacency |
| O2-E5C-005 | Lifecycle and failure corpus | No server secret and balanced cleanup | Every failure boundary | Accepted split lifecycle | 1 each; 30 min | Process, secret, sanitizer logs | Security/runtime; zero leak |
| O2-E5C-006 | Accepted plan and materialized image | Same-stem source-aware .T | Deterministic | ir_b2a -st -src | 1; 10 min | .B/.T/C/audit | Evidence; complete bundle |
## 12. Negative, Failure, and Compatibility Matrix

| Case | Required behavior | First evidence | Publication |
| --- | --- | --- | --- |
| O2 qualification changed | Stop for targeted requalification | Stale report | None |
| E5B result or handoff changed | Reject before replay | Fingerprint mismatch | None |
| Unknown E5B disposition | Reject | Disposition diagnostic | None |
| E5B action replay differs | Reject before resolution | State replay diff | None |
| Protected site changed | Reject | Protected identity diff | None |
| Fixed N missing or changed | Reject; recommend only | Fixed-N report | None |
| Capacity exceeds S by one | Reject | Checked capacity report | None |
| Q/P count or order invalid | Reject | Parameter verifier | None |
| Duplicate or composite prime | Reject | Independent prime test | None |
| CRT value or dimension differs | Reject | CRT comparison | None |
| Arithmetic overflows or saturates | Reject | Checked-arithmetic log | None |
| Achieved security below target | Reject | Independent security report | None |
| Estimator identity stale | Stop for requalification | Estimator hash report | None |
| Bootstrap profile approximate match only | Reject unsupported | Capability/profile diff | None |
| Provider wants a different N or schedule | Reject provider result | Projection diff | None |
| CKKS_PARAMS field truncates | Reject | Field-width audit | None |
| Resolver request/result digest differs | Reject | Resolver schema log | None |
| Resolver emits unknown required field | Reject | Schema diagnostic | None |
| Runtime Q/P/CRT differs from Open64 | Reject and invalidate | Context query diff | None |
| Logical rotation zero or duplicate remains | Reject | Logical key census | None |
| ANT expansion misses/adds a key | Reject | Provider key diff | None |
| MulCC maps to Mul_ciph | Reject | Operation map trace | None |
| Mul_ciph followed by Relin | Reject duplicate mapping | Call adjacency trace | None |
| Mul_ciph3 lacks adjacent Relin | Reject | Call adjacency trace | None |
| Ordinary consumer sees CIPHER3 | Reject | SSA/state audit | None |
| Unsupported KeySwitch mapping | Reject before C emission | Capability diagnostic | None |
| Recursive or indirect call | Reject before publication | CFG diagnostic | None |
| Unknown trip or frequency | Reject | Control/frequency report | None |
| Retained multiplication SCC | Reject | SCC census | None |
| Private op reaches whirl2c | Reject materialization | Final opcode census | No mid .B |
| Nonallowlisted header appears | Reject | Include graph | No accepted C |
| Nonallowlisted symbol appears | Reject | Symbol inventory | No accepted binary |
| dsc_fhe facade appears | Reject architecture violation | Link/include audit | None |
| FHErt_ant_encode or other provider links | Reject | Link map | No accepted binary |
| be.so gains ACE dependency | Stop for design review | Consumer link diff | None |
| Server calls unsplit Prepare_context | Mark Unverified | Server symbol/process scan | No exit |
| Server contains decryptor or secret | Reject and quarantine | Secret scan | None |
| Secret appears in .B/.T/C/JSON/log | Reject and quarantine | Content scan | None |
| Provider call fails mid-graph | Run ordered cleanup and fail | Injection/sanitizer log | No valid output |
| Cleanup is missing or duplicated | Reject | Lifetime trace | None |
| Ciphertext bytes differ | Ignore as oracle | Result protocol | Decoded check only |
| Decoded error exceeds tolerance | Reject | Oracle comparison | No accepted result |
| Persisted error bound is violated | Reject even inside global tolerance | Bound comparison | None |
| Nonzero gap sentinel contaminates output | Reject | Slot/result trace | None |
| Legacy WHIRL lacks O2 feature | Preserve accepted legacy behavior | Compatibility report | No E5C record |
| Unknown required record version | Reject before use | Version diagnostic | None |
| Failure during atomic publication | Roll back staged files | Injection log | No valid-looking .B |
| Two clean runs differ | Reject determinism | Normalized diff | Neither accepted |
| Independent reopen fails | Mark Unverified | Reopen log | Not accepted |
## 13. Evidence and Retention

The accepted O2-E5C-EXIT bundle contains at least:

    qualification-reference.json  master-and-adr-lock.json  source-lock.json
    support-matrix.json  diagnostics.json  environment.json
    ace-rtlib-ant-manifest.json  ace-public-header-hashes.json  ace-symbol-allowlist.json
    ace-library-hashes.json  e5b-handoff.json  e5b-selected-plan.B
    e5b-selected-plan.T  selected-action-replay.json  selected-post-state.json
    requested-configuration.json  resolved-parameters.json  security-estimator.json
    security-validation.json  q-p-primes.json  crt-values-and-fingerprints.json
    bootstrap-profiles.json  ckks-state-trace.json  logical-key-requirements.json
    ant-key-manifest.json  provider-capabilities.json  ant-resolution-request.json
    ant-resolution-response.json  ant-runtime-context-query.json  ckks-params-projection.json
    final-resolved-plan.B  final-resolved-plan.T  final-resolved-plan.ckks-final.t
    independent-final-plan-reopen.json  transformed-plaintext-manifest.json  materialized.o2.mid.B
    materialized.o2.mid.T  materialized.o2.mid.calls.t  independent-materialized-reopen.json
    generated.c  generated-c-includes.txt  generated-c-symbols.txt
    generated.o  generated-program  link-map.txt
    build-and-link.log  be-symbol-diff.txt  be-consumer-link-closure.json
    client-lifecycle.json  server-lifecycle.json  server-symbol-scan.json
    secret-scan.json  sanitizer-and-cleanup.log  decoded-output.json
    oracle-output.json  result-comparison.json  negative-tests.json
    failure-injection.json  producer-normalized-digest.json  consumer-normalized-digest.json
    stage-decision.md  e6-handoff.json  SHA256SUMS
final-resolved-plan.T is generated from final-resolved-plan.B. materialized.o2.mid.T is generated from materialized.o2.mid.B.
Both use ir_b2a -st -src and preserve the source pathname recorded in DST. Phase traces use noncolliding lowercase .t names.
Failed development evidence is retained at least 30 days. Milestone evidence is retained at least 180 days. An accepted bundle and its governing fingerprints are retained for repository lifetime.
Artifact directories are cleaned at the start of the next run, not the end of the current run. A failed run may retain logs and rejected inputs but cannot leave partial output under an accepted .B name.
## 14. Exit Decision

### 14.1 Accepted

O2-E5C-EXIT=Accepted requires:

- every entry gate remains current; O2-S15B-001 and O2-S18-001 pass; all O2-E5C-001 through O2-E5C-006 gates pass; E5B independently reopens and action replay matches exactly; one final CKKS state and parameter record owns every decision;
- fixed N is unchanged; complete Q/P/CRT, scale, level, range, error, and profiles verify; achieved security meets the accepted target; CKKS_PARAMS and runtime-created context match the Open64 plan exactly; logical and ANT-expanded key manifests each match independent collection;
- generated C uses only the public header and symbol allowlists; the only FHE libraries are FHErt_common and FHErt_ant; be.so and every existing consumer remain free of a new ACE dependency; every MulCC maps to Mul_ciph3 followed immediately by exactly one Relin; no private FHE/O2/HPOLY operation reaches whirl2c;
- direct-call and control-flow bounds are enforced; ownership, alias, status, and cleanup tests pass; client/test and server-evaluation lifecycles are separated; the server has no secret key, decryptor, or decrypt capability; decoded outputs and persisted error bounds pass;
- two clean runs are deterministic in normalized semantic evidence; final plan and materialized WHIRL independently reopen; matching .B and same-stem .T evidence exists; independent reviewers sign the stage decision.
Exact completion wording:

    O2-E5C accepted: the independently reopened E5B schedule has one verified
    final Open64 CKKS plan, an exact LIB_ANT projection, and a standard-WHIRL
    generated-C execution through FHErt_common and FHErt_ant.
This wording does not claim O2 Stage 1 accepted or O2 complete.
### 14.2 Rejected

O2-E5C-EXIT=Rejected means an implemented input or stage behavior violates an accepted contract. The decision records the first diagnostic, owning upstream or E5C work item, retained evidence, and correction path.
E6 cannot consume a rejected final plan.
### 14.3 Unverified

O2-E5C-EXIT=Unverified means required authority, provider build, exact query, security evidence, split lifecycle, independent reopen, execution environment, or reviewer evidence is missing.
In particular, an unresolved server-safe replacement for current unsplit Prepare_context and an unresolved resolver topology both require Unverified. Unverified does not permit provisional E6 acceptance work.
## 15. Rollback, Invalidation, and Stop Rules

Rollback disables the E5C increment and atomically returns to the complete qualified O0 profile when the accepted policy allows fallback. It does not:

- select an old parameter set; combine an E5B schedule with baseline parameters; change N; drop protected sites; weaken security;
- replace Mul_ciph3 and Relin with fused Mul_ciph; retain partially generated C, keys, or transformed data as accepted output.
An accepted E5C result is invalidated by any graph, layout, E5A, E5B, parameter, security, provider, header, symbol, library, lifecycle, key, materializer, toolchain, target, or result-oracle change.
Invalidation propagates to O2-E6, Stage 1 acceptance, every Stage 2 plan, costs, keys, generated program, and O3 handoff that names this result.
Stop and return to architecture, common/com, build, or security review if:

- two owners or no owner exist for a final semantic field; runtime data would become a second CKKS truth; finalization requires changing E5B placement; fixed N cannot be preserved; exact Q/P/CRT cannot be obtained and independently verified;
- a provider profile cannot express the selected bootstrap transition; a new physical record or operator is needed without review; generated C requires a private ACE or nonallowlisted header; materialization requires an unlisted or private symbol; an opaque dsc_fhe facade is proposed;
- be.so would acquire any direct or indirect ACE rtlib dependency; an isolated resolver cannot remain a strict process boundary; the current unsplit ANT context is proposed for server evaluation; server evaluation would receive a secret, decryptor, or private material; immediate Relin cannot be expressed as Mul_ciph3 followed by Relin;
- a private operation remains at whirl2c; ownership or cleanup cannot be made exact; independent reopen needs producer memory; a tolerance or support boundary would be relaxed after observing output.
## 16. Exact O2-E6 Handoff

E5C publishes one immutable e6-handoff.json containing:

- O2-E5C-EXIT decision and final-plan ID; current O2-O0Q-001 and O2-E5B-EXIT IDs; master, ADR, O2 plan, execution-index, schema, and tool fingerprints; source graph, selected layout, E5A, E5B, and protected-site IDs; E5B disposition and selected action schedule digest;
- selected post-state and independent replay digest; fixed N and active slots; complete resolved Q/P/CRT and security fingerprints; scale, level, range, error, and bootstrap profile identities; logical and ANT-expanded key-manifest fingerprints;
- ACE commit, public-header, symbol, build, and library hashes; CKKS_PARAMS and runtime-context comparison digest; generated-C include and symbol inventories; be.so and consumer link-closure audit; client/test and server-evaluation lifecycle evidence;
- secret and sanitizer/cleanup reports; final-plan and materialized-WHIRL producer/consumer digests; decoded/oracle result comparison; matching .B/.T, C, binary, logs, and SHA256SUMS hashes; support rows, diagnostics, invalidation triggers, and fallback identity.
The E6 consumer must:

1. reopen the final plan and materialized WHIRL in an independent process;
2. validate every governing and provider fingerprint;
3. confirm current O2 qualification;
4. verify fixed N, security, protected sites, state, actions, and keys;
5. rerun the private-op and Mul_ciph3-to-Relin structural audits;
6. use the exact generated-C and runtime evidence for Stage 1 comparison;
7. reject a stale result rather than rematerialize it silently;
8. add no algorithm or support extension during acceptance.
## 17. Requirement Traceability

| Requirement | Master/ADR status | O2 milestone | Work items | Verification | Retained evidence |
| --- | --- | --- | --- | --- | --- |
| Complete O0 before O2 | Master successor pending; user decision frozen | O2-O0Q-001 | E5C-W01, W22 | Every stale check | Qualification reference |
| Consume E5B final schedule/state | O2 pass-order contract | S1.5B | E5C-W02, W03 | O2-E5C-001 | E5B handoff and replay |
| Fixed N and complete final CKKS | Master successor pending; O2 invariant | S1.5B | E5C-W05 through W09 | O2-S15B-001 | Parameters/state/security |
| Exact Q/P/CRT and security | FRZ-08/09 pending acceptance | S1.5B | E5C-W06 through W09, W15 | O2-S15B-001, E5C-002 | Resolver/context reports |
| Open64 semantic ownership | O2 plan Sections 5, 7, 8, 9, 12 | S1.5B | E5C-W04, W12, W15 | Final record crosswalk | Final plan and digests |
| Public ACE rtlib/ANT boundary | FRZ-08 pending acceptance | S1.5B, S1.8 | E5C-W12 through W16 | E5C-002/003 | Header/symbol/link locks |
| No direct be.so dependency | AGENTS.md; FRZ-09 pending | S1.5B | E5C-W13, W14, W27 | E5C-003 | be and consumer link audit |
| MulCC to Mul_ciph3 to Relin | Current master and O2 frozen invariant | S1.5B, S1.8 | E5C-W16, W17 | E5C-004, S18-001 | WHIRL/C/action trace |
| Bounded calls/control flow | Open64 extension | S1.8 | E5C-W18 | S18-001 | CFG and diagnostic report |
| Logical/provider keys separate | O2 plan Section 11.2 | S1.5B, S1.8 | E5C-W10, W11 | S15B-001, S18-001 | Two key manifests |
| Standard WHIRL materialization | O2 plan Section 12 | S1.8 | E5C-W16 through W23 | S18-001, E5C-006 | Mid .B/.T and C |
| No server secret/decryptor | FRZ-09 and P1b | S1.8 | E5C-W24 | E5C-005, S18-001 | Lifecycle and secret scans |
| Semantic/result checks | O2 acceptance contract | S1.8 | E5C-W25 | S18-001 | Decoded/oracle comparison |
| Atomic publication/reopen | P2/S1.1 contract | S1.5B, S1.8 | E5C-W21, W22 | E5C-001 | Digests and failure logs |
| Meaningful WHIRL evidence | AGENTS.md | S1.5B, S1.8 | E5C-W26 | E5C-006 | Same-stem .B and .T |
| Exact E6 input | O2 stage boundary | S1.9 prerequisite | E5C-W28 | E6 validation-only dry run | e6-handoff.json |
Every accepted row closes:

    requirement
      -> accepted master section, version, and hash
      -> accepted ADR decision
      -> O2 plan milestone and test
      -> E5C work item
      -> exact proposed command
      -> retained artifact and reviewer decision
A row containing pending, an unknown owner, a moving source reference, or a placeholder hash prevents O2-E5C-EXIT=Accepted.
