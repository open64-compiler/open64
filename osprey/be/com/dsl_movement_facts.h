/* Copyright (C) 2026 Open64 Project */
/* Backend-owned H2 detached Work Partition/Resource Lifetime/Execution Events
 * projection. Owning phases harvest native proofs before search; these records
 * contain no WN/ST/TY/map/runtime pointers and grant no lowering authority.
 * Existing family verifiers remain authoritative. No mapped/runtime ABI change.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#ifndef dsl_movement_facts_INCLUDED
#define dsl_movement_facts_INCLUDED
#include "dsl_gpu_plan.h"
#include <vector>
#include <string>

/* Unknown is distinct from a proved zero. All counters have byte units; source
 * requests and projected algorithm requests are never physical bus traffic. */
struct DSL_MOVEMENT_BYTES {
    bool known;
    uint64_t value;
    /* Default does not grant a zero-traffic or spill-free assumption. */
    inline DSL_MOVEMENT_BYTES() : known(false),value(0) {}
    /* Only an owning adapter supplies an exact source/contract count. */
    explicit inline DSL_MOVEMENT_BYTES(uint64_t bytes) : known(true),value(bytes) {}
};
struct DSL_MOVEMENT_LEDGER {
    DSL_MOVEMENT_BYTES useful_reads,useful_writes,source_reads,source_writes;
    DSL_MOVEMENT_BYTES requested_reads,requested_writes,transactions,l2,hbm,shared,spills;
    DSL_MOVEMENT_BYTES scale,page,index,partial;
};
struct DSL_MOVEMENT_STORAGE {
    std::string id,allocation,version,encoding,role,layout,first_use,last_use;
    uint64_t offset,bytes,capacity,element_bytes,alignment;
    std::vector<uint64_t> shape,strides;
    bool alias_proved,strides_known;
    /* Empty storage has no range, layout or disjointness permission. */
    inline DSL_MOVEMENT_STORAGE() : offset(0),bytes(0),capacity(0),element_bytes(0),alignment(0),alias_proved(false),strides_known(false) {}
};
struct DSL_MOVEMENT_ACCESS {
    std::string id,storage,version,owner,relation,reuse_scope;
    uint64_t offset,extent,read_bytes,write_bytes,multiplicity;
    bool reuse_proved,counts_known;
    /* Unknown reuse cannot be inferred from matching shapes or role names. */
    inline DSL_MOVEMENT_ACCESS() : offset(0),extent(0),read_bytes(0),write_bytes(0),multiplicity(1),reuse_proved(false),counts_known(false) {}
};
struct DSL_MOVEMENT_EVENT {
    std::string id,kind,storage,version,scope,evidence;
};
struct DSL_MOVEMENT_EDGE {
    unsigned from,to;
    /* Indices reference owned events, never native statement positions. */
    inline DSL_MOVEMENT_EDGE(unsigned first=0,unsigned second=0) : from(first),to(second) {}
};
struct DSL_MOVEMENT_LIFETIME {
    std::string storage,version,next_version,raw_storage;
    unsigned raw_ready,decoded_ready,consumer,readers_done,reuse_ready;
    bool decoded,has_reuse;
    /* Missing indices are invalid until a family supplies its event proof. */
    inline DSL_MOVEMENT_LIFETIME() : raw_ready(~0U),decoded_ready(~0U),consumer(~0U),
        readers_done(~0U),reuse_ready(~0U),decoded(false),has_reuse(false) {}
};
struct DSL_MOVEMENT_WORK {
    std::string owner,domain,tail_policy,numerical;
    uint64_t instances,width,active_last,inactive_last;
    std::vector<std::string> axes;
    std::vector<uint64_t> extents;
    /* No participating work is invented by an empty record. */
    inline DSL_MOVEMENT_WORK() : instances(0),width(0),active_last(0),inactive_last(0) {}
};
struct DSL_MOVEMENT_BINDING {
    std::string family,owner,source_key,proof_key,controls,serving_key,device_key;
    DSL_GPU_PLAN_SNAPSHOT device;
    uint64_t activation,mutation;
    bool epoch_known;
    /* WOPT/VHO require fresh native recapture; only an explicit live LNO epoch
     * can authorize a token-level freshness check. Neither grants application. */
    inline DSL_MOVEMENT_BINDING() : activation(0),mutation(0),epoch_known(false) {}
};
struct DSL_MOVEMENT_CAPTURE {
    DSL_MOVEMENT_BINDING binding;
    DSL_MOVEMENT_WORK work;
    DSL_MOVEMENT_LEDGER ledger;
    std::vector<DSL_MOVEMENT_STORAGE> storages;
    std::vector<DSL_MOVEMENT_ACCESS> accesses;
    std::vector<DSL_MOVEMENT_EVENT> events;
    std::vector<DSL_MOVEMENT_EDGE> edges;
    std::vector<DSL_MOVEMENT_LIFETIME> lifetimes;
};
class DSL_MOVEMENT_FACTS {
    bool _valid;
    DSL_MOVEMENT_CAPTURE _capture;
    std::string _content,_key;
public:
    /* Refused captures expose no previous usable body. */
    inline DSL_MOVEMENT_FACTS() : _valid(false) {}
    /* Validate structure/joins/events, then atomically copy the complete owned
     * projection. Refusal clears output; no native state or search is touched. */
    static bool Create(const DSL_MOVEMENT_CAPTURE &,DSL_MOVEMENT_FACTS &,std::string &);
    /* Availability is diagnostic, not a certificate for applying a schedule. */
    inline bool Available() const { return _valid; }
    /* Borrow only owned records; safe after the native source lifetime ends. */
    inline const DSL_MOVEMENT_CAPTURE &Capture() const { return _capture; }
    /* Full-content comparison supplements, rather than trusts, compact hashes. */
    inline bool Same_content(const DSL_MOVEMENT_FACTS &other) const
        { return _valid && other._valid && _content==other._content; }
    /* Return the versioned digest for downstream cache binding and diagnostics. */
    inline const std::string &Key() const { return _key; }
    /* Require a live owner-supplied epoch and all source/control/profile joins.
     * WOPT/VHO callers without epochs must freshly recapture and Same_content. */
    bool Is_current(const DSL_MOVEMENT_BINDING &,bool epoch_live) const;
    /* Conservative charge for copying owned strings/vectors and serialization;
     * H3 budgets this detached storage, not process RSS or native analyses. */
    uint64_t Storage_charge() const;
    /* Print detached facts without reading a compiler context or hardware. */
    void Print(FILE *,const char *stage) const;
};
/* Reuse the existing stateless backend SHA-256 utility for bounded diagnostics;
 * native recapture comparisons still use complete content, never only a hash. */
extern std::string DSL_movement_digest(const std::string &);
/* Checked byte arithmetic reserves UINT64_MAX as unknown in existing ledgers. */
extern bool DSL_movement_add(uint64_t,uint64_t,uint64_t &);
/* Checked products cannot turn overflow into a small reusable footprint. */
extern bool DSL_movement_mul(uint64_t,uint64_t,uint64_t &);
/* Construct one already-admitted dense layout projection with checked byte
 * strides; caller supplies canonical allocation/version/alias proof. Throws on
 * overflow/malformed shape before any native state can be changed. */
extern DSL_MOVEMENT_STORAGE DSL_movement_dense_storage(const std::string &,const std::string &,
    const std::string &,const std::string &,const std::vector<uint64_t> &,uint64_t,bool);
/* Add a source/contract-ordered read lifetime, not an asynchronous pipeline.
 * Existing owner must establish immutable contents over this scope first. */
extern void DSL_movement_source_lifetime(DSL_MOVEMENT_CAPTURE &,const std::string &,
    const std::string &,const std::string &,const std::string &);
#endif
