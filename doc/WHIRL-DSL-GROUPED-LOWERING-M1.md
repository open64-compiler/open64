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
members. Physical returns, raw calls, address escapes, unmanaged external
reads, and private REGION or runtime-interface uses reject. The sole final
member may feed an already rewritten runtime call: its call projection must
refer to that same value projection and handle, its active call-ABI argument
and callee formal must agree, and the physical actual must use the handle
rather than the retiring tensor ST. Verified-retired original call arguments
carry provenance but have no live call projection. M1 checks the complete
program interface before mutation; intermediate members retain the no-call
rule. The mapped DSL image retains logical provenance for retired members,
and `ir_b2a -st -src` prints the final runtime projection without exposing
the physical `OPR_DSL` escape.

A final member may also be named by retained non-executable semantic rows.
M1 admits such an external logical reference only when its destination is an
already retired, lowered, or dead-elided node with a valid existing relation,
the destination result belongs to the active PU, and its ST has no physical
tree use, REGION interface, or state effect. The existing lowered-relation
verifier must accept the full image before commit. A live destination or any
external reference from an intermediate private member still rejects.

The focused CKKS fixture now joins two ordered static subgroups in one
request. Its final replacement has the sole projection, an intermediate
member has none, a fabricated intermediate projection rejects before
mutation, and the accepted `.B` reopens with `ir_b2a -st -src`. This proves
the existing M1 request shape without a new row, API, or ABI revision. The
FHE consumer still owns the exact six-subgroup ReLU union and complete-model
event/ABI census.
The two-PU fixture additionally proves a `FINAL_RESULT` handle passed to a
rewritten runtime `VCALL` while a second original actual for that same value
is verified retired. A wrong physical actual or wrong projection ID rejects
with native definitions and the detached standard block unchanged.
Another mapped fixture retains both a retired redirect and a CKKS-expanded
lowered source row that reference the final member. Live, malformed-lowered,
and intermediate-member provenance variants reject without mutation.
The immediately previous reader rejects this intermediate image at its CKKS
event gate, while its `-st -src` output for the standard-only terminal image
is byte-identical to the current reader's output.

The optional CKKS event rows remain in this intermediate M1 image. An
immediately previous `ir_b2a` can parse the unchanged physical row layouts
but **must reject** private lowered members at its DSL image gate rather than
silently reinterpret them. The terminal standard-WHIRL checkpoint is a
separate M3 contract. Omitting only the optional event section is insufficient
for previous-reader reopen: the pre-M1 lowered-relation validator rejects
private members without individual runtime projections. The focused M1 test
already proves that fail-closed behavior on an artifact with no event section.
M3 must either publish a fully standard-WHIRL terminal image without the
private DSL provenance sections, after a complete no-live-DSL preflight, or
explicitly accept previous-reader rejection. It must not claim compatibility
from event-section omission alone. Retain the immutable CKKS input and
authenticated report for detailed provenance.
The CKKS fixture also elides its verified-dead tensor seed, writes the
actual grouped result through the standard-only writer, and reopens that
terminal `.B` in both current and pre-M1 readers with identical
`ir_b2a -st -src` traces. This proves one-PU CKKS event-section disposition,
not the complete FHE image, all-PU event census, or public ABI join.

`DSL_Gatekeeper_Verify_No_Native_PU()` is the first PU-local terminal
preflight. It rejects any executable native DSL node in the active tree; it
does not consume CKKS events, prove all-PU coverage, or authorize section
omission. The grouped fixture correctly fails this check after its add groups
lower because its `common.tensor_const` seed remains live. An isolated
standard-WHIRL tree passes the same check.

The staged `Open_Standard_Only_Output_Info()` writer mode checks every flat
PU as it is written, requires the complete linked PU list at global-table
finalization, omits the optional DSL/FHE ELF sections, and writes the baseline
WHIRL revision. It does not erase in-memory provenance or change existing
symbol/type tables. A two-PU synthetic terminal image reopens through both
current and pre-M1 readers; the grouped intermediate still carries its full
logical image and fails closed in the pre-M1 reader. This proves the writer
boundary, not the S6-0d event-consumption or complete-model terminal gate.
The FHE producer must finish those proofs before opening a standard-only
temporary output and publish it only after all-PU validation succeeds.
The terminal writer now admits a managed `WT_REGIONS` subsection when the
active PU's REGION records still name WNs in its standard physical tree, all
interface symbols are valid local STs, and each output/result symbol is
defined inside its REGION. It writes the unchanged per-PU REGION subsection
while omitting the incompatible optional DSL/FHE global images. A focused
two-PU image with `cnn.basic_block.v1` reopens in both the current and pre-M1
readers; a stale result interface rejects before PU writing. This proves the
generic writer/reader disposition, not that the full CKKS producer has
redirected every REGION output to its terminal runtime handle. D4 must still
prove those transitions and the authenticated event-to-ABI join. No source
CKKS event or private group row is discarded from the immutable intermediate
image.

A separate focused producer now places two dependent native groups inside a
managed `cnn.basic_block.v1` REGION. The grouped transaction replaces their
six native definitions with two standard handle definitions and redirects the
REGION result interface to the second handle. A mapped reopen shows the same
interface ST as the physical `STID`; it still shows the unrelated native
tensor seed, so this is an intermediate-image proof. A second producer run
uses the existing verified-dead-source transaction to elide that seed, then
writes the actual grouped REGION tree through the standard-only writer. Its
terminal `.B` reopens with identical `ir_b2a -st -src` output in the current
and pre-M1 readers. This proves the generic transaction-to-writer path for
one PU, not CKKS event/ABI consumption or complete-model terminal coverage.

`DSL_Gatekeeper_Verify_No_Native_PU()` is the first PU-local terminal
preflight. It rejects any executable native DSL node in the active tree; it
does not consume CKKS events, prove all-PU coverage, or authorize section
omission. The grouped fixture correctly fails this check after its add groups
lower because its `common.tensor_const` seed remains live. An isolated
standard-WHIRL tree passes the same check.

The staged `Open_Standard_Only_Output_Info()` writer mode checks every flat
PU as it is written, requires the complete linked PU list at global-table
finalization, omits the optional DSL/FHE ELF sections, and writes the baseline
WHIRL revision. It does not erase in-memory provenance or change existing
symbol/type tables. A two-PU synthetic terminal image reopens through both
current and pre-M1 readers; the grouped intermediate still carries its full
logical image and fails closed in the pre-M1 reader. This proves the writer
boundary, not the S6-0d event-consumption or complete-model terminal gate.
The FHE producer must finish those proofs before opening a standard-only
temporary output and publish it only after all-PU validation succeeds.
The terminal writer now admits a managed `WT_REGIONS` subsection when the
active PU's REGION records still name WNs in its standard physical tree, all
interface symbols are valid local STs, and each output/result symbol is
defined inside its REGION. It writes the unchanged per-PU REGION subsection
while omitting the incompatible optional DSL/FHE global images. A focused
two-PU image with `cnn.basic_block.v1` reopens in both the current and pre-M1
readers; a stale result interface rejects before PU writing. This proves the
generic writer/reader disposition, not that the full CKKS producer has
redirected every REGION output to its terminal runtime handle. D4 must still
prove those transitions and the authenticated event-to-ABI join. No source
CKKS event or private group row is discarded from the immutable intermediate
image.

A separate focused producer now places two dependent native groups inside a
managed `cnn.basic_block.v1` REGION. The grouped transaction replaces their
six native definitions with two standard handle definitions and redirects the
REGION result interface to the second handle. A mapped reopen shows the same
interface ST as the physical `STID`; it still shows the unrelated native
tensor seed, so this is an intermediate-image proof, not a terminal PU proof.

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
