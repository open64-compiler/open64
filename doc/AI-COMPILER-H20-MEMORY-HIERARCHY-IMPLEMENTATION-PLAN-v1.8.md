# H20 memory-hierarchy implementation plan v1.8

This additive revision preserves [v1.7](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.7.md)
and all earlier designs/deliveries. [H7 specification](AI-COMPILER-H7-H20-QUALIFICATION-v1.0.md)
adds an H20 packet to the existing GPU testing, collection, shared receiver and
GS1/GS2 paths without changing compiler search/lowering or the old test suites.

| Gate | Implementation / acceptance |
| --- | --- |
| H7a frozen artifacts | Authenticate H3/H4/H5/H6 receipts; eleven selected four-family cases, actual native device CUDA/B/T/COMMENTs, original launch/numerical/profile/model bindings and independent server-owned corpus. Separate explicit 96/141 target packets |
| H7b observed device/resources | Actual Driver name/UUID/SM90/capacity/full-device, grid/thread/RF/shared/local/occupancy and cubin identity. No ISA-only board alias or fixture rates relabeled hardware. Wrong/missing evidence refuses |
| H7c correctness/races | All variants and two legal address residues; input/guard/output checks and returned output bytes. Four Q7-compatible sanitizer children with full clean summaries/matrix. Deny dependent timing on failure or missing proof |
| H7d timing | Persistent buffers; allocation/reset/reference outside samples. Separate unprofiled wall-only enqueue/completion and CUDA-event whole-operation batches; raw samples and final output verification. No pooling, no profiler durations, no absent native baseline manufactured |
| H7e profiling | Optional separate NCU capability query/counter replay and Systems trace; original raw reports, binary/case/GPU joins and shared counter interpretation. Measured/derived/proxy/unknown labels stay distinct; no search/model feedback |
| H7f deployment/receiver | Optional v2 inner packet and four shared profiles; old inner scripts byte-identical. Automatic failure/partial returns; original safe archive scanner/receiver plus deep H7 checks. Registry-derived GS2 capability advertisement; job/lease/cancel API unchanged |
| Hardware acceptance | Pending real H20 execution. Synthetic compiler-profile calibration, native same-GPU baselines, broader H4 TMA launch-provider qualification and personnel transfer remain explicit gaps |

H7 preserves two-phase estimation-only search, finite compiler budgets,
selected-only native construction, explanatory COMMENTs and original fallback.
Source packet execution runs no compiler search or new WHIRL transformation.
AOT compile is outside timing; AOT serving selection, PREOPT COMMENT repair and
OpenCTI stay deferred. Software/mock/static-build validation does not close
actual device correctness/race/performance or H8 acceptance. Roles remain TBD.
