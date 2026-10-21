# WHIRL DSL Grouped Lowering M1

## Scope

`DSL_IR_Lower_Native_Value_Groups_To_Standard_Blocks()` is a generic
common/com transaction for one active PU. It consumes a complete request
array, verifies pure native value definitions and their physical/logical
uses, then inserts one caller-provided standard-WHIRL block per group. It
retires the group's native definitions and marks their logical rows lowered
only after complete preflight. A rejected request leaves the PU tree, image
tables, detached blocks, and caller results unchanged.

The FHE consumer boundary is described in
`doc/FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md`. This service does not select FHE
operations, allocate runtime calls, or implement FHE lowering policy.

## Closure And Inspection

All private member uses must stay inside their group. One group output may
feed a later submitted group; the complete source-context request projects
only its typed CKKS `FINAL_RESULT` replacement. The semantic source remains
distinct provenance and has no runtime projection. For a ReLU-style chain,
submit all ordered private subgroups as one member union whose final output
is that `FINAL_RESULT`; bootstrap/stage outputs remain unprojected internal
members. Physical returns, calls, address escapes, unmanaged external reads,
and private REGION or runtime-interface uses reject. The mapped DSL image
retains logical provenance for retired members, and `ir_b2a -st -src` prints
the final runtime projection without exposing the physical `OPR_DSL` escape.

The focused CKKS fixture now joins two ordered static subgroups in one
request. Its final replacement has the sole projection, an intermediate
member has none, a fabricated intermediate projection rejects before
mutation, and the accepted `.B` reopens with `ir_b2a -st -src`. This proves
the existing M1 request shape without a new row, API, or ABI revision. The
FHE consumer still owns the exact six-subgroup ReLU union and complete-model
event/ABI census.

The optional CKKS event rows remain in this intermediate M1 image. An
immediately previous `ir_b2a` can parse the unchanged physical row layouts
but **must reject** private lowered members at its DSL image gate rather than
silently reinterpret them. The terminal standard-WHIRL checkpoint is a
separate M3 contract: after complete event consumption is proved, omit the
optional event section and certify previous-reader reopen of that terminal
artifact. M1 does not claim that terminal compatibility result.

## Cost And Validation

The transaction indexes member value/node/ST/definition identities, PU tree
positions and physical uses, logical value references, runtime projections,
inputs, calls, call arguments, effects, and REGION interface rows once. The
request-specific closure work is linear in member and edge counts with
ordered-map lookups, not a whole-PU scan per member. Existing global
gatekeeper and REGION transition validators retain their own cost contracts.

The focused test retains a `.B`, current-reader `.T`, prior-reader rejection
log, and separate rollback logs for malformed requests and private
cross-group, return, call, REGION, and interface escapes. Full FHE model
certification belongs to later milestones.
