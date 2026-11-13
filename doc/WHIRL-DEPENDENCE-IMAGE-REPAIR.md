# Post-LNO dependence-image call offsets

The long common-High MLA regression exposed an auxiliary-image construction
defect. LNO's CG dependence graph includes a vector `INTRINSIC_CALL`. The tree
writer recorded offsets for loads, stores and plain `CALL`, but omitted other
call operators. Its graph vertex was therefore written with offset zero.
The downstream reader interpreted the tree header as a WN and asserted while
reconstructing that vertex.

The repair belongs to the existing PU-local mapped tree/map service. Under
`Write_BE_Maps`, record offsets for every `OPCODE_is_call` node. Do not replace
the intrinsic, remove graph vertices, discard edges, or alter numerical or
optimization policy. The dependence writer checks its live-node/offset join
before writing graph rows, with an internal development assertion and a
release diagnostic for missing offsets.

The graph reader preflights the complete vertex table before constructing
vertices or changing WN maps. An absent offset is malformed input: zero names
the tree header, not a node. The owning reader returns the existing IR error
status for a normal dependence-graph diagnostic. It does not guess a missing
node or silently remove required dependence information. This focused guard
does not claim to implement general validation of every graph-image field.

No .B version, ELF subsection, WN layout, opcode encoding, graph record,
alignment or offset width changes. Valid historical images retain their
meaning. Historical images already containing zero vertex offsets are corrupt
and require regeneration with the repaired producer; the missing identity is
not recoverable in general. WHIRL's normative mapping contract remains per-PU
and category-sensitive (WHIRL.pdf sections 1.4.7 and 1.4.10); the implementation's
current layouts and writer alignment remain authoritative for physical bytes.

Validation must include all call forms through the production tree/graph
services, incomplete/zero vertex tables with no native-map mutation, the old
failing long image's diagnostic, regenerated short/long images with nonzero
vertex offsets, separate-process `ir_b2a -st -src` inspection, and downstream
host execution against the saved numerical references. Preserve the original
failure, actual producer commands, graph censuses, raw images/traces and
before/after diffs. Recheck the shared backend/inliner boundary. These tests
do not establish GPU execution or performance. Keep the repair local for a
future PR until upload is explicitly requested.

The focused Linux x86-64 development validation covers four call forms, four
malformed vertex-table cases (41 checks, also with NDEBUG), and an injected
producer invariant. The regression fails on the preceding backend at the
missing call offset. The repaired long graph retains 263 vertices and an edge
high-water count of 315; vertex 1 changes from offset zero to 79,988. Serialized
tree bytes and `-st -src` text are unchanged. Other raw byte differences are
confined to reserved ST index-zero rows and are retained explicitly.

Fresh short/long pre/post-LNO host validation passes all 28 saved reference
executions, linking the production strided `vexpf` runtime. The preceding
backend reader consumes newly written short/long images without a migration.
The old short image remains readable; the old malformed long image receives
the normal dependence-section diagnostic. Backend exports/imports/dependencies,
configured inliner closure, syntax/layout, miniature checkpoint and 37 detached
search checks also pass. Review evidence is retained under
`artifacts/review/ai-optimization/reader-repair/long-post-lno/`; this does not
reclassify historical receipts or claim release-build/GPU/full-model execution.
