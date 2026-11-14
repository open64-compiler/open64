/* Copyright (C) 2026 Open64 Project */
/* LNO's owned, PU-local High facts and mutation-lifetime contract. No native
 * pointers or binary WHIRL changes. Design:
 * doc/AI-COMPILER-SEARCH-M2B-OWNED-SNAPSHOT-v1.0.md. */
#ifndef dsl_high_snapshot_INCLUDED
#define dsl_high_snapshot_INCLUDED
#include <stdint.h>
#include <stdio.h>
#include <string>
#include <vector>

struct DSL_HIGH_STAMP {
    uint64_t activation, mutation;
    std::string owner;
    /* Empty snapshots carry a defined, unusable token instead of indeterminacy. */
    DSL_HIGH_STAMP() : activation(0), mutation(0) {}
};

class DSL_HIGH_EPOCH {
    DSL_HIGH_STAMP _stamp;
    bool _open;
    DSL_HIGH_EPOCH(const DSL_HIGH_EPOCH &);
    DSL_HIGH_EPOCH &operator=(const DSL_HIGH_EPOCH &);
public:
    /* Create a distinct activation even when the same PU name is reused. */
    explicit DSL_HIGH_EPOCH(const std::string &owner);
    /* Expire the activation at scope exit; owned facts remain readable. */
    ~DSL_HIGH_EPOCH();
    /* Return a value token, never a reference to driver-owned mutable state. */
    DSL_HIGH_STAMP Stamp() const { return _stamp; }
    /* Test owner, activation and generation before touching any native facts. */
    bool Matches(const DSL_HIGH_STAMP &stamp) const;
    /* Call before every source/analysis/context mutation; overflow closes. */
    void Before_mutation();
    /* Permanently expire this activation, including on early driver exit. */
    void Close() { _open = false; }
};

struct DSL_HIGH_BINDINGS {
    std::string unit, numeric, target, manifest, model;
    /* Exact binding comparison; absent profiles must be explicit strings. */
    bool operator==(const DSL_HIGH_BINDINGS &other) const;
};
struct DSL_HIGH_LOOP {
    std::string id, parent, induction, comparison;
    int64_t lower, upper, step;
};
struct DSL_HIGH_DIM {
    int64_t constant;
    unsigned nonconstant_loops;
    std::vector<int64_t> loop_coefficients;
    std::vector<std::string> linear_symbols;
};
struct DSL_HIGH_ACCESS {
    std::string id, node, enclosing_loop, base, element_type;
    bool write;
    int64_t element_bytes;
    std::vector<DSL_HIGH_DIM> dimensions;
    /* MR1 owned projection; does not alter legacy source/fact key spelling. */
    std::string storage, execution_group, copied_from_access, element_encoding;
    unsigned execution_order;
    int64_t array_extent, byte_offset;
    bool direct_formal, conditional;
    std::vector<std::string> loop_ids;
    DSL_HIGH_ACCESS() : write(false), element_bytes(0), execution_order(0),
        array_extent(0), byte_offset(0), direct_formal(false), conditional(false) {}
};
struct DSL_HIGH_STORAGE {
    std::string id, symbol, element_type, element_encoding, cache_trait;
    unsigned formal;
    int64_t element_bytes;
    bool pointer_formal, restrict_qualified, base_stable, uses_only_as_array_base;
    DSL_HIGH_STORAGE() : formal(0), element_bytes(0), pointer_formal(false),
        restrict_qualified(false), base_stable(true), uses_only_as_array_base(true) {}
};
struct DSL_HIGH_DEP_COMPONENT {
    bool is_distance;
    int64_t value;
    unsigned direction, distance_bound;
};
struct DSL_HIGH_DEPENDENCE {
    std::string source, sink;
    unsigned unused_dimensions;
    std::vector<std::vector<DSL_HIGH_DEP_COMPONENT> > vectors;
};
struct DSL_HIGH_DU {
    std::string node, loop;
    bool definitions, available, incomplete;
    std::vector<std::string> endpoints;
};
struct DSL_HIGH_ALIAS {
    std::string first, second;
    int result;
    bool disjoint;
    DSL_HIGH_ALIAS() : result(0), disjoint(false) {}
};

class DSL_HIGH_CAPTURE;
class DSL_HIGH_SNAPSHOT {
    friend class DSL_HIGH_CAPTURE;
    bool _valid;
    DSL_HIGH_STAMP _stamp;
    DSL_HIGH_BINDINGS _bindings;
    std::string _identity, _source_key, _facts_key, _owned_memory_key;
    std::string _producer_recipe, _producer_schema, _buffer_contract;
    std::vector<DSL_HIGH_STORAGE> _storages;
    std::vector<DSL_HIGH_LOOP> _loops;
    std::vector<DSL_HIGH_ACCESS> _accesses;
    std::vector<DSL_HIGH_DEPENDENCE> _dependences;
    std::vector<DSL_HIGH_DU> _du;
    std::vector<DSL_HIGH_ALIAS> _aliases;
public:
    /* Empty output is not a usable snapshot; capture publishes atomically. */
    DSL_HIGH_SNAPSHOT() : _valid(false) {}
    /* Pure token/context gate, safe after native maps/managers/pools die. */
    bool Is_current(const DSL_HIGH_EPOCH &epoch, const DSL_HIGH_BINDINGS &bindings) const;
    /* Compare complete owned content, not a compact hash or ephemeral stamp. */
    bool Same_content(const DSL_HIGH_SNAPSHOT &other) const;
    /* Return immutable owned values; no compiler node/table references escape. */
    const std::string &Identity() const { return _identity; }
    /* Return the complete canonical source key used for collision-safe equality. */
    const std::string &Source_key() const { return _source_key; }
    /* Return the complete copied fact key, including uncertainty. */
    const std::string &Facts_key() const { return _facts_key; }
    /* New projections have a separate key so existing v1/v2 joins stay valid. */
    const std::string &Owned_memory_key() const { return _owned_memory_key; }
    const std::vector<DSL_HIGH_STORAGE> &Storages() const { return _storages; }
    const std::string &Producer_recipe() const { return _producer_recipe; }
    const std::string &Producer_schema() const { return _producer_schema; }
    const std::string &Buffer_contract() const { return _buffer_contract; }
    /* Return an owned token for a separately derived analysis lifetime. */
    DSL_HIGH_STAMP Stamp() const { return _stamp; }
    /* Return immutable loop records in semantic tree order. */
    const std::vector<DSL_HIGH_LOOP> &Loops() const { return _loops; }
    /* Return immutable affine memory records. */
    const std::vector<DSL_HIGH_ACCESS> &Accesses() const { return _accesses; }
    /* Return the full copied graph edge vectors, including scalar endpoints. */
    const std::vector<DSL_HIGH_DEPENDENCE> &Dependences() const { return _dependences; }
    /* Return DU lists with explicit unavailable/incomplete/external endpoints. */
    const std::vector<DSL_HIGH_DU> &DU() const { return _du; }
    /* Return all pairwise memory alias answers, including self pairs. */
    const std::vector<DSL_HIGH_ALIAS> &Aliases() const { return _aliases; }
    /* Emit owned review facts only; callable after native analysis destruction. */
    void Print(FILE *file, const char *stage) const;
};

class WN;
class DU_MANAGER;
struct ALIAS_MANAGER;
class ARRAY_DIRECTED_GRAPH16;
/* Capture effective numerical/ABI controls. GPU resources, source tensor
 * manifest and cost model remain explicitly unbound at this native boundary. */
extern DSL_HIGH_BINDINGS LNO_DSL_high_bindings(const char *);
/* Capture only an admitted active FUNC_ENTRY with live full LNO facts. A
 * refusal clears output and never changes source/analyses or fallback. */
extern bool LNO_DSL_capture_high(WN *, DU_MANAGER *, ALIAS_MANAGER *,
    ARRAY_DIRECTED_GRAPH16 *, const DSL_HIGH_EPOCH &, const DSL_HIGH_BINDINGS &,
    DSL_HIGH_SNAPSHOT &, std::string &);
/* Gate epoch/context first, then recapture live facts and compare full content.
 * Caller must still be at the admitted native lifetime boundary when current. */
extern bool LNO_DSL_revalidate_high(WN *, DU_MANAGER *, ALIAS_MANAGER *,
    ARRAY_DIRECTED_GRAPH16 *, const DSL_HIGH_EPOCH &, const DSL_HIGH_BINDINGS &,
    const DSL_HIGH_SNAPSHOT &, std::string &);
#endif
