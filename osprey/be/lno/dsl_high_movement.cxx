/* Copyright (C) 2026 Open64 Project */
/* LNO-owned H2 projection from MR1 source proofs, outside candidate search.
 * Reuses existing native loop/access/alias analysis; copies only detached values.
 * No WN/ST/TY/map mutation, inferred hardware traffic or numerical permission.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#include "dsl_high_movement.h"
#include <map>
#include <sstream>

/* Bind the current source activation, exact facts and H20 evidence; existing
 * High recapture is still mandatory before applying the selected plan. */
bool LNO_DSL_capture_movement(const DSL_HIGH_SNAPSHOT &snapshot,const DSL_HIGH_EPOCH &epoch,
    const DSL_HIGH_BINDINGS &bindings,const VHO_TENSOR_PROGRAM &program,const DSL_HIGH_REUSE_FACTS &reuse,
    DSL_MOVEMENT_FACTS &out,std::string &reason)
{
    out=DSL_MOVEMENT_FACTS();
    if(!snapshot.Is_current(epoch,bindings) || !reuse.Is_current(snapshot,epoch,bindings,program)) {
        reason="movement_stale_high_reuse";return false;
    }
    try {
        DSL_MOVEMENT_CAPTURE c;c.binding.family="MLA";c.binding.owner=snapshot.Stamp().owner;
        c.binding.source_key=snapshot.Source_key();c.binding.proof_key=reuse.Binding_key();
        c.binding.controls=bindings.numeric+";cache-operands-only;"+snapshot.Owned_memory_key();
        c.binding.device=bindings.device;c.binding.device_key=bindings.device.Cache_key();
        c.binding.epoch_known=true;c.binding.activation=snapshot.Stamp().activation;c.binding.mutation=snapshot.Stamp().mutation;
        c.work.owner="query/head with source-ordered cache accesses";c.work.domain="MR1 latent/rotary cache operands; other storage inventory only";
        c.work.instances=1;c.work.width=1;c.work.active_last=1;c.work.tail_policy="native-constant-loop-domains";
        c.work.numerical="source F32 dot/folds/finite/zero guards unchanged";
        const DSL_HIGH_REUSE_SUMMARY &summary=reuse.Summary();std::map<unsigned,std::string> storage;
        for(size_t i=0;i<summary.storages.size();++i) {
            const DSL_HIGH_REUSE_STORAGE &s=summary.storages[i];
            std::string version=s.version.empty()?"source-inventory/"+s.native_storage:s.version;
            DSL_MOVEMENT_STORAGE value=DSL_movement_dense_storage(s.native_storage,s.native_storage,version,s.native.cache_trait,
                s.shape,s.native.element_bytes,s.native.restrict_qualified && s.native.base_stable && s.native.uses_only_as_array_base);
            value.first_use=s.first_use;value.last_use=s.last_use;c.storages.push_back(value);storage[s.value]=value.id;
        }
        for(size_t i=0;i<summary.accesses.size();++i) {
            const DSL_HIGH_REUSE_ACCESS &a=summary.accesses[i];
            DSL_MOVEMENT_ACCESS value;value.counts_known=true;value.id=a.id;value.storage=storage[a.storage];value.version=a.storage_version;
            value.owner=a.execution_group;value.relation=a.head_loop+";"+a.purpose+";"+a.native.node;
            value.extent=a.footprint_bytes;value.read_bytes=a.read_bytes;value.multiplicity=a.head_multiplicity;
            value.reuse_scope="cross-head-invocation";value.reuse_proved=a.head_invariant;c.accesses.push_back(value);
        }
        uint64_t payload=0,reads=0,writes=0;
        for(size_t i=0;i<summary.caches.size();++i) {
            const DSL_HIGH_REUSE_CACHE &cache=summary.caches[i];
            if(!DSL_movement_add(payload,cache.payload_bytes,payload) || !DSL_movement_add(reads,cache.source_read_bytes,reads) ||
                !DSL_movement_add(writes,cache.source_write_bytes,writes)) {reason="movement_cache_bytes_overflow";return false;}
            DSL_movement_source_lifetime(c,storage[cache.root],cache.version,"invocation","MR1-native-readonly-version-and-alias-proof");
            for(size_t j=0;j<summary.storages.size();++j) if(summary.storages[j].value==cache.root) {
                const std::vector<uint64_t> &shape=summary.storages[j].shape;
                if(shape.size()!=3 || !DSL_movement_mul(shape[0],cache.heads,c.work.instances)) {reason="movement_cache_partition_unproved";return false;}
                c.work.width=c.work.active_last=shape[1];
                if(c.work.axes.empty()) {c.work.axes.push_back("batch");c.work.extents.push_back(shape[0]);c.work.axes.push_back("head");c.work.extents.push_back(cache.heads);c.work.axes.push_back("cache_token");c.work.extents.push_back(shape[1]);}
                else if(c.work.extents[0]!=shape[0] || c.work.extents[1]!=cache.heads || c.work.extents[2]!=shape[1]) {reason="movement_cache_domains_disagree";return false;}
            }
        }
        c.ledger.useful_reads=DSL_MOVEMENT_BYTES(payload);c.ledger.source_reads=DSL_MOVEMENT_BYTES(reads);
        c.ledger.source_writes=DSL_MOVEMENT_BYTES(writes);
        // Fused requests, sector service, physical cache hits and spills are not
        // source facts. Existing MR2 evaluates those separately in phase one.
        return DSL_MOVEMENT_FACTS::Create(c,out,reason);
    } catch(const std::exception &e) {reason=e.what();return false;}
}
