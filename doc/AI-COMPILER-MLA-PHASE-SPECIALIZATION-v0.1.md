# MLA phase-specialized AOT direction v0.1

November 10, 2026 | Adopted direction; selection mechanism deferred

This addendum records the user's decision to support separate prefill and decode MLA specializations declared through compiler options. It supplements [Q2 workload inference](AI-COMPILER-MLA-Q2-WORKLOAD-v1.0.md), [Q3 serving binding](AI-COMPILER-MLA-Q3-SERVING-v1.0.md) and [implementation plan v1.18](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.18.md). Existing design material and prior implementation evidence remain preserved.

## Adopted direction

AOT remains the baseline. The compiler should be able to produce a qualified MLA specialization for **prefill** and another for **decode**, using a compiler option to declare each compilation's intended phase contract. The option is a declaration of intended use; it does not establish serving intent by analyzing indistinguishable source graphs. A Q=1 prefill tail and a Q=1 decode call can have identical graphs and inputs. Q>1 can be prefill or speculative verification. Quantization, cache extent and an append do not settle that distinction.

Each specialization must have its own explicit geometry, representation, numerical, resource and serving contract. Decode may require one active query per request. Prefill must qualify its admitted query ranges, including a one-token tail where supported. Speculative verification, mixed requests, padding and unknown phase require explicit admission or a compatible generic path; they must not be silently relabeled. The existing single-query GPU materializer is not certified for general prefill by adding a phase flag.

Serving phase and context-length tuning are separate dimensions. Both prefill and decode can encounter short or long contexts. A phase-specialized implementation may contain separately qualified geometry/resource choices, but cache length must not substitute for phase. The common High WHIRL algorithm and existing two-phase methodology remain the foundation: estimate eligible plans without native mutation, apply only the selected legal plan, rebuild invalidated analyses, and run correctness checks after application.

Compiler options will eventually declare the intended phase. The earlier illustrative spelling `-DSL:mla_phase=prefill` / `-DSL:mla_phase=decode` is **not implemented or frozen as a CLI contract** in this revision. This document adopts the capability and intent, not an option parser change or a claim that optimized prefill/decode kernels already exist.

## Deliberately deferred decisions

The AOT **selection mechanism will be designed later**, when its requirements and evidence are clearer. This revision does not choose where selection runs, who invokes it, how variants are packaged, which dispatch ABI is exposed, how mixed batches are segmented, or which thresholds/policies select a variant. It introduces no runtime dispatcher, source conditional, device-side decision maker, JIT, autotuning loop or length-based phase heuristic.

Before that design is accepted, the project must qualify each variant, determine the available serving declarations and geometry evidence, establish compatible buffer/cache/scale/mapping/publication contracts, and preserve a safe generic route on mismatch. Q3 supplies part of this correctness foundation; it does not select a variant. Its current synchronous host adapter rejects cross-registry overlapping ownership and does not define a multi-variant sharing/selection ABI.

The eventual mechanism must select only implementations whose contracts hold for the actual invocation. A compiler option cannot waive mask, position, page, generation, lifetime, numerical or dependence checks. Unavailable evidence remains unknown, and an incompatible specialization must refuse before execution or use an explicitly compatible generic implementation. No raw packed cache may be redirected to an FP32 interface.

## Work sequencing and acceptance

1. Finish Q3 buffer/mapping/generation/publication binding and its correctness evidence.
2. Preserve phase-specialized AOT support as a design requirement for subsequent selected realization work. Qualify prefill and decode algorithms/materializers independently against their source/reference contracts.
3. Keep AOT selection design as an explicit open item. Do not infer a dispatcher implementation from this direction or close it using a phase label alone.
4. When sufficient information exists, propose and review the selection ABI/policy separately, including mismatch behavior and per-invocation correctness. No particular mechanism is selected here.

Owner, successor, independent reviewer and approving lead remain **TBD**. This is an adopted architectural direction and a deferred design decision, not implementation or hardware-performance acceptance.
