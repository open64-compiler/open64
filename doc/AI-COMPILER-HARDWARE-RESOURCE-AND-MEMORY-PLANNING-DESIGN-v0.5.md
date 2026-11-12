# Hardware resource and memory planning v0.5

2026-11-12. This additive revision preserves [v0.4](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.4.md)
and extends its H1 device/resource/profile separation with the implemented
[H2 owned movement facts](AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md).
[Plan v1.3](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.3.md)
records deliveries and the remaining canonical-route/hardware gates.

Native WHIRL opcodes own semantics; existing tensor descriptors/traits retain
cache, encoding and layout properties. Derived access/reuse/schedule/lifetime
facts belong to backend-owned records with exact source/proof/device dependency
bindings and explicit validity. H2 introduces no MLIR interface/trait mechanism.

Each family projects its existing admitted owner into Work Partition, storage
and access records, and Resource Lifetime/Execution Events. Useful payload,
source requests, selected recipe requests, physical transactions, L2/HBM service,
shared requests, spills and metadata/partial buffers remain separate. H2 cannot
infer physical service or spill-free execution. A larger HBM capacity constrains
live allocations/budgets; it does not by itself imply bandwidth or latency.

The H20 96 GB and 141 GB device identities are bound independently to immutable
resource/partition/execution/model/toolchain/budget evidence. Architecture-only
compilation remains compatible. Neither shared sm_90 nor matching shape/counts
permits reuse across unmatching profiles, allocation versions, page mappings or
numerical permissions. Q3/H1d retain actual serving and device-observation
responsibility; a copied fact or profile digest is not a runtime lease.

Phase one remains detached estimation. H2 captures before exploration and
recaptures before selected application/emission; only the selected plan is
lowered and reference checked. Q6 projection is after selection and consumes
its existing full verified action trace. COMMENT explanations, atomic selected
application, analysis invalidation/rebuild, High exit checks and fallback stay
in their owning services. Physical calibration, new common executable routes,
operator-specific H4-H6 materializers and H7 GPU qualification remain distinct
gates, with no measured performance claim from synthetic profiles.
