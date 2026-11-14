/* Copyright (C) 2026 Open64 Project */
/* Policy-free H1b runtime quantity vocabulary and sealed owned device context.
 * No mapped WHIRL, inference, cost policy, file I/O or native pointers. Only
 * the backend import adapter may construct a context; consumers borrow const
 * facts under the driver's lifetime. Design:
 * doc/AI-COMPILER-H1B-GPU-PROFILE-IMPORT-v1.0.md. */
#ifndef dsl_device_profile_INCLUDED
#define dsl_device_profile_INCLUDED
#include <cstddef>
#include <stdint.h>
#include <string>

enum DSL_GPU_FACT_PROVENANCE {
    DSL_GPU_FACT_UNKNOWN, DSL_GPU_FACT_QUERIED, DSL_GPU_FACT_DOCUMENTED,
    DSL_GPU_FACT_ESTIMATED, DSL_GPU_FACT_SYNTHETIC, DSL_GPU_FACT_MEASURED
};

struct DSL_GPU_RESOURCE_FACT {
    bool known;
    uint64_t lower, value, upper;
    DSL_GPU_FACT_PROVENANCE provenance;
    std::string evidence;
    /* Unknown has defined storage but conveys no usable zero-valued fact. */
    inline DSL_GPU_RESOURCE_FACT() : known(false), lower(0), value(0), upper(0),
        provenance(DSL_GPU_FACT_UNKNOWN) {}
};

struct DSL_GPU_EXECUTION_FACT {
    bool known;
    double lower, value, upper;
    DSL_GPU_FACT_PROVENANCE provenance;
    std::string evidence;
    /* Consumers must check known before using any numeric field. */
    inline DSL_GPU_EXECUTION_FACT() : known(false), lower(0), value(0), upper(0),
        provenance(DSL_GPU_FACT_UNKNOWN) {}
};

enum DSL_GPU_RESOURCE_FIELD {
    DSL_GPU_VISIBLE_SMS, DSL_GPU_WARP_LANES, DSL_GPU_PHYSICAL_HBM_BYTES,
    DSL_GPU_VISIBLE_HBM_BYTES, DSL_GPU_L2_BYTES, DSL_GPU_REGISTERS_PER_SM,
    DSL_GPU_SHARED_BYTES_PER_SM, DSL_GPU_SHARED_BYTES_PER_CTA_DEFAULT,
    DSL_GPU_SHARED_BYTES_PER_CTA_OPTIN, DSL_GPU_MAX_THREADS_PER_SM,
    DSL_GPU_MAX_THREADS_PER_CTA, DSL_GPU_MAX_BLOCKS_PER_SM,
    DSL_GPU_REGISTER_QUANTUM, DSL_GPU_SHARED_QUANTUM, DSL_GPU_RESOURCE_COUNT
};

enum DSL_GPU_EXECUTION_FIELD {
    DSL_GPU_HBM_READ_RATE, DSL_GPU_HBM_WRITE_RATE, DSL_GPU_HBM_MIXED_RATE,
    DSL_GPU_L2_RATE, DSL_GPU_SHARED_RATE_PER_SM, DSL_GPU_SIMT_RATE_PER_SM,
    DSL_GPU_MEMORY_STARTUP_NS, DSL_GPU_ADD_DEPENDENT_NS, DSL_GPU_MUL_DEPENDENT_NS,
    DSL_GPU_EXP_DEPENDENT_NS, DSL_GPU_DIV_DEPENDENT_NS, DSL_GPU_LAUNCH_NS,
    DSL_GPU_ASYNC_STARTUP_NS, DSL_GPU_DEQUANT_RATE_PER_SM, DSL_GPU_EXECUTION_COUNT
};

enum DSL_GPU_BUDGET_FIELD {
    DSL_GPU_BUDGET_LIMIT, DSL_GPU_BUDGET_RESERVED, DSL_GPU_BUDGET_USABLE,
    DSL_GPU_BUDGET_COUNT
};

class DSL_GPU_PROFILE_IMPORTER;
class DSL_GPU_DEVICE_CONTEXT {
    friend class DSL_GPU_PROFILE_IMPORTER;
    bool _imported, _execution_bound, _budget_bounds_proved;
    unsigned _device_id, _device_revision, _resource_revision, _execution_revision;
    unsigned _budget_revision, _resource_known, _execution_known;
    std::string _device, _architecture, _toolchain, _resource_id, _partition_kind;
    std::string _partition_id, _inventory_id, _resource_observed_at;
    std::string _execution_id, _model, _execution_observed_at, _budget_id;
    std::string _input_sha256, _resource_sha256, _execution_sha256;
    std::string _budget_sha256, _context_sha256;
    std::string _conditions[5];
    DSL_GPU_RESOURCE_FACT _resources[DSL_GPU_RESOURCE_COUNT];
    DSL_GPU_EXECUTION_FACT _execution[DSL_GPU_EXECUTION_COUNT];
    DSL_GPU_RESOURCE_FACT _budget[DSL_GPU_BUDGET_COUNT];
    /* Construct defined unknown facts privately, then populate before sealing. */
    inline DSL_GPU_DEVICE_CONTEXT() : _imported(false), _execution_bound(false),
        _budget_bounds_proved(false), _device_id(0), _device_revision(0),
        _resource_revision(0), _execution_revision(0), _budget_revision(0),
        _resource_known(0), _execution_known(0) {}
    /* Prevent mutable copies or reassignment of a published snapshot. */
    DSL_GPU_DEVICE_CONTEXT(const DSL_GPU_DEVICE_CONTEXT &);
    /* Only the driver-owned const object is published by the importer. */
    DSL_GPU_DEVICE_CONTEXT &operator=(const DSL_GPU_DEVICE_CONTEXT &);
public:
    /* Release only owned standard-library data; no native manager lifetime. */
    inline ~DSL_GPU_DEVICE_CONTEXT() {}
    /* Distinguish imported evidence from device-only unknown context. */
    inline bool Imported() const { return _imported; }
    /* True means an execution section was provided, not that rates are known. */
    inline bool Execution_bound() const { return _execution_bound; }
    /* Structural capacity/budget joins were proved from known conservative bounds. */
    inline bool Budget_bounds_proved() const { return _budget_bounds_proved; }
    /* Return the exact registry identity, not an inferred memory-size class. */
    inline unsigned Device_id() const { return _device_id; }
    /* Return the registry revision independently of resource revisions. */
    inline unsigned Device_revision() const { return _device_revision; }
    /* Borrow an owned exact device name for this context's lifetime. */
    inline const std::string &Device() const { return _device; }
    /* Borrow the validated exact baseline ISA name. */
    inline const std::string &Architecture() const { return _architecture; }
    /* Borrow the declared expected target toolchain binding; no live tool probe. */
    inline const std::string &Toolchain() const { return _toolchain; }
    /* Resource inventory identity is distinct from calibration/model identity. */
    inline const std::string &Resource_id() const { return _resource_id; }
    /* Return the bound inventory generation. */
    inline unsigned Resource_revision() const { return _resource_revision; }
    /* Borrow full/mig identity, never infer it from visible memory size. */
    inline const std::string &Partition_kind() const { return _partition_kind; }
    /* Borrow the explicitly bound partition identity. */
    inline const std::string &Partition_id() const { return _partition_id; }
    /* Borrow the external inventory evidence identity. */
    inline const std::string &Inventory_id() const { return _inventory_id; }
    /* Borrow the declared observation time; this is not clock verification. */
    inline const std::string &Resource_observed_at() const { return _resource_observed_at; }
    /* Borrow the optional execution profile identity. */
    inline const std::string &Execution_id() const { return _execution_id; }
    /* Return the execution profile generation separately from resource generation. */
    inline unsigned Execution_revision() const { return _execution_revision; }
    /* Borrow the externally declared model identity. */
    inline const std::string &Model() const { return _model; }
    /* Borrow the declared execution observation time or explicit unknown string. */
    inline const std::string &Execution_observed_at() const { return _execution_observed_at; }
    /* Borrow the workload/serving budget identity; no allocation guarantee. */
    inline const std::string &Budget_id() const { return _budget_id; }
    /* Return budget generation independently of physical inventory. */
    inline unsigned Budget_revision() const { return _budget_revision; }
    /* Number of usable resource facts; missing facts remain explicitly unknown. */
    inline unsigned Resource_known() const { return _resource_known; }
    /* Count known rates/latencies without promoting estimates into measurements. */
    inline unsigned Execution_known() const { return _execution_known; }
    /* Borrow exact input content digest, or none for device-only context. */
    inline const std::string &Input_sha256() const { return _input_sha256; }
    /* Resource dependency key excludes calibration and workload-budget changes. */
    inline const std::string &Resource_sha256() const { return _resource_sha256; }
    /* Execution dependency key includes the resource key and toolchain binding. */
    inline const std::string &Execution_sha256() const { return _execution_sha256; }
    /* Budget dependency key includes its resource binding, not rate values. */
    inline const std::string &Budget_sha256() const { return _budget_sha256; }
    /* Complete snapshot identity binds pinned input plus all validated sections. */
    inline const std::string &Context_sha256() const { return _context_sha256; }
    /* Bounds-check a const resource fact; known must be tested by consumers. */
    inline const DSL_GPU_RESOURCE_FACT *Resource(int field) const
        { return field >= 0 && field < DSL_GPU_RESOURCE_COUNT ? &_resources[field] : NULL; }
    /* Bounds-check an execution quantity whose unit is fixed by the schema. */
    inline const DSL_GPU_EXECUTION_FACT *Execution(int field) const
        { return field >= 0 && field < DSL_GPU_EXECUTION_COUNT ? &_execution[field] : NULL; }
    /* Bounds-check a const usable-budget fact; zero reserved bytes can be known. */
    inline const DSL_GPU_RESOURCE_FACT *Budget(int field) const
        { return field >= 0 && field < DSL_GPU_BUDGET_COUNT ? &_budget[field] : NULL; }
    /* Conditions are clock, power, ECC, cache regime and workload; empty=unknown. */
    inline const std::string *Condition(unsigned index) const
        { return index < 5 ? &_conditions[index] : NULL; }
};

/* C++98-compatible unique owner. Construction/import is backend-controlled;
 * callers can borrow or release the sealed context but cannot mutate its data. */
class DSL_GPU_DEVICE_CONTEXT_OWNER {
    friend class DSL_GPU_PROFILE_IMPORTER;
    const DSL_GPU_DEVICE_CONTEXT *_context;
    /* Commit only a completely validated new context; previous borrows expire. */
    inline void Publish(const DSL_GPU_DEVICE_CONTEXT *context)
        { delete _context; _context = context; }
    /* Ownership cannot be copied or implicitly transferred by a client. */
    DSL_GPU_DEVICE_CONTEXT_OWNER(const DSL_GPU_DEVICE_CONTEXT_OWNER &);
    /* Reassignment is reserved to the importing transaction. */
    DSL_GPU_DEVICE_CONTEXT_OWNER &operator=(const DSL_GPU_DEVICE_CONTEXT_OWNER &);
public:
    /* Empty ownership conveys no fabricated context or facts. */
    inline DSL_GPU_DEVICE_CONTEXT_OWNER() : _context(NULL) {}
    /* Release owned standard data on ordinary driver return. */
    inline ~DSL_GPU_DEVICE_CONTEXT_OWNER() { delete _context; }
    /* Borrow an immutable context only during this owner's current lifetime. */
    inline const DSL_GPU_DEVICE_CONTEXT *Context() const { return _context; }
    /* Explicit driver cleanup invalidates borrows and releases the context. */
    inline void Reset() { Publish(NULL); }
};
#endif
