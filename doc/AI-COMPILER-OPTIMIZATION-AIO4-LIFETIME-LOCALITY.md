# AIO-4 Tensor Lifetime, Reuse, And Locality

## Status

AIO-4 implements the check-only `AI-P1` analysis substrate. It consumes the
runtime-only per-PU tensor facts from AIO-3 and a snapshot of Open64 control
flow facts. It does not rewrite WHIRL, persist a new ELF section, choose a
memory tier, or select an optimization plan.

## Compilation Scope

The analysis owns no interprocedural state. One invocation analyzes one active
PU and its active local symbol table. The normal backend driver remains
responsible for visiting every PU. Cross-PU lifetime or reuse analysis belongs
to a future explicit `-ipa` summary and is not inferred by this service.

The same rule applies to REGION. REGION identity is a control fact inside the
current PU; the driver processing that PU owns the REGION lifetime.

## Open64 CFG Reuse

`DSL_TENSOR_CONTROL_SNAPSHOT` is an immutable runtime view of control facts,
not a second CFG. It records only the stable facts needed by AI-P1:

- basic-block identity and reverse-postorder position;
- immediate dominator and immediate postdominator identity;
- loop depth;
- REGION identity;
- branch and effect-barrier flags; and
- logical DSL node statement placement.

`WOPT_DSL_Populate_Tensor_Control_Snapshot()` copies these facts from the
active `CFG` and `BB_NODE` services while their WN statement mapping is valid.
The common analysis does not include or retain `CFG *`, `BB_NODE *`, `WN *`,
or another backend-private pointer. Callers without a valid Open64 CFG must
leave the corresponding result unknown rather than synthesize optimistic
dominance, loop, or REGION claims.

## Analysis Records

For each AIO-3 tensor fact, `DSL_TENSOR_LOCALITY_FACT_RECORD` records:

- producer and deterministically ordered consumers;
- a last-consumer representative and path positions;
- static, symbolic, unknown, or overflow object-size state;
- exact-block, dominated, branch, loop, REGION, effect, alias, or unknown
  lifetime state;
- exact, conservative, or unknown reuse distance;
- target-independent working-set bytes when exactly computable;
- elementwise, contraction, reduction, view, indexed, mixed, or unknown
  access pattern;
- target-independent residency benefit seed;
- unweighted logical dataflow critical-path membership;
- unique, conservative, or unknown alias state; and
- estimated read and write bytes as raw cost-model inputs.

Unknown is never encoded as zero. Unknown byte and distance quantities use an
explicit sentinel in memory and print as `<unknown>`.

## Conservative Rules

An exact same-block lifetime requires an ordered producer and all consumers in
one block, unique tensor ownership, read-only operands, and no effect barrier.
The analysis degrades conservatively when:

- consumers occupy incomparable branch paths;
- a use enters or crosses a loop;
- producer and consumer REGION identities differ;
- an operand may modify, alias, update in place, or consume the value;
- a control block contains an effect barrier;
- ownership is shared or unresolved;
- a control position is missing; or
- shape or element size is unresolved.

Critical-path evidence is the longest unweighted logical DSL dataflow path.
It is a candidate-selection seed, not a latency prediction. Branch, loop,
REGION, effect, and alias uncertainty makes this field unknown. Target-weighted
critical paths belong in later CandidateCostIR refinement.

Residency benefit is likewise a target-independent seed. AIO-4 may recognize
that a short-distance, multiply used tensor is worth considering for
residency, but AI-P6 owns memory-tier capacity and placement decisions.

## Compatibility

All AIO-4 records are runtime-only C++ analysis state. They do not change:

- WN, opcode, TY, ST, REGION, or TensorDescriptorIR layout;
- binary WHIRL or ELF sections;
- reader/writer behavior;
- `ir_b2a` logical output; or
- frontend and backend interfaces.

The before, after, and repeated `.B` images and `ir_b2a -st -src` traces must
remain byte-for-byte identical. The separate AIO-4 trace is the reviewable
analysis evidence.

## Validation

The focused contract covers:

1. a straight-line reused tensor with known static object size;
2. deterministic consumer order, last use, reuse distance, working set,
   access pattern, residency seed, critical path, and traffic cost;
3. conservative branch, loop, REGION, and effect-barrier classifications;
4. explicit unknown size for `<pending>` shape;
5. active-PU ownership and wrong-PU rejection;
6. repeated-build determinism and no WHIRL/table mutation;
7. byte-identical binary and ASCII WHIRL before and after analysis;
8. WOPT adapter compilation against the established CFG interfaces;
9. the Open64 target syntax and logical opcode-layout matrix; and
10. `be.so`, `be`, and `lw_inline` dependency closure.

## Next Boundary

AIO-5 now consumes these facts to generate check-only fusion candidates. It
does not reinterpret a conservative lifetime as exact, and it keeps candidate
generation and plan selection separate from WHIRL mutation. See
`AI-COMPILER-OPTIMIZATION-AIO5-FUSION-CANDIDATES.md`.
