# Hardware resource and memory planning v0.6

2026-11-12. This revision extends [v0.5](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.5.md)
with [H3 physical costing](AI-COMPILER-H3-MOVEMENT-COSTING-v1.0.md) and
[plan v1.4](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.4.md).
Prior versions are preserved.

H2 facts and H1 H20 device/resource/execution/budget identities now bind
separately versioned physical-unit estimates. Source useful bytes, source
requests and unknown physical facts remain immutable. An explicit pinned H3
supplement supplies missing issue/collective/queue/cache/spill assumptions;
missing requirements refuse H3. Device-wide HBM/L2 service is shared across
SMs; per-SM work and finite occupancy/coverage are modeled separately. Ns ranks
never replace the meanings of archived reference-cycle fields.

One request-wide finite estimate cache spans the existing owning phases and
M3 strategy portfolio. Source, alias, layout, lifetime, numerical, model,
resource, profile, conditions and budget changes invalidate acceptance. Only
admitted executable recipes can enter existing selected-only WHIRL lowering;
modeled grouped/staged alternatives retain explicit capability restrictions.
Selected source/model revalidation, COMMENT explanations, rollback and fallback
remain controlling requirements. No runtime ABI, WHIRL representation or MLIR
operation-interface/trait mechanism is added.

Software model qualification establishes neither measured H20 performance nor
actual spill-free/register allocation. External calibration input/output is
versioned; physical collection and independent timing/profiling remain H0/H7.
Larger HBM capacity constrains live allocations and budget; it does not imply
another board's bandwidth, latency, cache or topology. H20 96 GB and 141 GB
remain distinct complete dependency identities. Existing architecture-only
behavior remains available without H3. AOT selection remains deferred.
