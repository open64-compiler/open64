/* Copyright (C) 2026 Open64 Project */
/* Owned snapshot value/lifetime operations for LNO, independent of WN pools.
 * Design: doc/AI-COMPILER-SEARCH-M2B-OWNED-SNAPSHOT-v1.0.md.
 * No IR, analysis maps, target policy or compiler-library dependencies. */
#include "dsl_high_snapshot.h"
#include <limits>

/* Backend PU compilation is serial. Never wrap/reuse an activation token. */
DSL_HIGH_EPOCH::DSL_HIGH_EPOCH(const std::string &owner) : _open(false) {
    static uint64_t next_activation = 0;
    _stamp.owner = owner;
    _stamp.mutation = 0;
    _stamp.activation = 0;
    if (next_activation != std::numeric_limits<uint64_t>::max()) {
        _stamp.activation = ++next_activation;
        _open = true;
    }
}
/* Conservatively invalidate all facts before a possible mutation. */
void DSL_HIGH_EPOCH::Before_mutation() {
    if (!_open) return;
    if (_stamp.mutation == std::numeric_limits<uint64_t>::max()) Close();
    else ++_stamp.mutation;
}
/* Serialize a readable, deterministic owned record after any native lifetime. */
void DSL_HIGH_SNAPSHOT::Print(FILE *file, const char *stage) const {
    if (!file || !_valid) return;
    fprintf(file, "AI_HIGH_SNAPSHOT 1 %s id=%s epoch=%llu:%llu loops=%lu accesses=%lu edges=%lu du=%lu aliases=%lu schedule_legality=not_proved\n",
        stage, _identity.c_str(), (unsigned long long)_stamp.activation,
        (unsigned long long)_stamp.mutation, (unsigned long)_loops.size(),
        (unsigned long)_accesses.size(), (unsigned long)_dependences.size(),
        (unsigned long)_du.size(), (unsigned long)_aliases.size());
    if (std::string(stage) == "CAPTURE") {
        fprintf(file, "AI_HIGH_SNAPSHOT BINDINGS numeric=%s target=%s manifest=%s model=%s\n",
            _bindings.numeric.c_str(), _bindings.target.c_str(), _bindings.manifest.c_str(), _bindings.model.c_str());
        _bindings.device.Print(file, "MLA-snapshot", 0);
        fprintf(file, "AI_HIGH_SNAPSHOT SOURCE %s\nAI_HIGH_SNAPSHOT FACTS %s\n",
            _source_key.c_str(), _facts_key.c_str());
        for(size_t i=0;i<_storages.size();++i) {
            const DSL_HIGH_STORAGE &s=_storages[i];
            fprintf(file,"AI_HIGH_MEMORY 1 STORAGE id=%s formal=%u bytes=%lld stable=%s array_uses_only=%s trait=%s\n",
                s.id.c_str(),s.formal,(long long)s.element_bytes,s.base_stable?"true":"false",
                s.uses_only_as_array_base?"true":"false",s.cache_trait.empty()?"absent":s.cache_trait.c_str());
        }
        for(size_t i=0;i<_accesses.size();++i) {
            const DSL_HIGH_ACCESS &a=_accesses[i];
            fprintf(file,"AI_HIGH_MEMORY 1 ACCESS id=%s storage=%s group=%s order=%u write=%s extent=%lld conditional=%s copy=%s loops=",
                a.id.c_str(),a.storage.c_str(),a.execution_group.c_str(),a.execution_order,a.write?"true":"false",
                (long long)a.array_extent,a.conditional?"true":"false",a.copied_from_access.c_str());
            for(size_t j=0;j<a.loop_ids.size();++j) {
                fprintf(file,"%s%s:",j?",":"",a.loop_ids[j].c_str());
                if(a.dimensions.size()==1 && j<a.dimensions[0].loop_coefficients.size())
                    fprintf(file,"%lld",(long long)a.dimensions[0].loop_coefficients[j]);
                else fprintf(file,"unknown");
                for(size_t k=0;k<_loops.size();++k) if(_loops[k].id==a.loop_ids[j])
                    fprintf(file,"[%lld,%lld,%lld,%s]",(long long)_loops[k].lower,(long long)_loops[k].upper,
                        (long long)_loops[k].step,_loops[k].comparison.c_str());
            }
            fprintf(file,"\n");
        }
    }
}
