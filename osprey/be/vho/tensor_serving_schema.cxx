/* Copyright (C) 2026 Open64 Project */
/* VHO-owned serving schema discovery from the existing guarded Q1 program.
 * Source geometry is not live allocation, mapping or publication evidence.
 * Design: doc/AI-COMPILER-MLA-Q3-SERVING-v1.0.md. */
#include "tensor_serving_schema.h"
#include <set>
#include <sstream>
#include <stdexcept>
namespace {
/* Reject incomplete source joins without publishing a partial ABI. */
void require(bool b,const char *why) {if(!b) throw std::runtime_error(why);}
class SERVING_SOURCE {
    const VHO_TENSOR_PROGRAM &_p;
    std::map<UINT32,unsigned> _defs;
    std::map<std::string,UINT32> _names;
public:
    /* All descriptors are copied; this helper borrows only detached values. */
    SERVING_SOURCE(const VHO_TENSOR_PROGRAM &p):_p(p) {
        for(unsigned i=0;i<p.operations.size();++i) _defs[p.operations[i].output]=i;
        for(unsigned i=0;i<p.values.size();++i) _names[p.values[i].name]=i;
    }
    /* Demand the exact source producer, not an inferred kernel meaning. */
    const VHO_TENSOR_OPERATION &Op(UINT32 v,const char *name) {
        require(_defs.count(v),"q3_missing_source_producer");const VHO_TENSOR_OPERATION &op=_p.operations[_defs[v]];
        require(op.name==name,"q3_source_topology");return op;
    }
    /* Strip only semantic reshape views, keeping gather/scatter identities. */
    UINT32 Unshape(UINT32 v) {
        for(unsigned n=0;n<256 && _defs.count(v);++n) {
            const VHO_TENSOR_OPERATION &op=_p.operations[_defs[v]];if(op.name!="common.reshape") return v;v=op.inputs[0];
        }return v;
    }
    /* Resolve a scale name to its real Q1 storage root. */
    UINT32 Scale(UINT32 v) {
        const std::string &name=_p.values[v].cache_scale;require(!name.empty() && _names.count(name),"q3_missing_scale_join");return _p.values[_names[name]].storage;
    }
    /* Validate the integer fill participating in the source page formula. */
    void Constant(UINT32 v,UINT64 number) {
        const VHO_TENSOR_OPERATION &op=Op(v,"common.fill_like");std::ostringstream s;s<<number;
        require(op.attributes.count("attr.value_i64") && op.attributes.find("attr.value_i64")->second==s.str(),"q3_page_constant");
    }
    /* Populate four output roots through actual copy and paired scale SSA. */
    void Outputs(VHO_TENSOR_SERVING_SCHEMA &s) {
        for(unsigned i=0;i<_p.values.size();++i) {
            const VHO_TENSOR_VALUE &v=_p.values[i];if(v.role!=2 || v.cache_trait.empty()) continue;
            unsigned pair=v.cache_trait==DSL_TENSOR_TRAIT_LATENT_KV_CACHE?0:v.cache_trait==DSL_TENSOR_TRAIT_ROTARY_KEY_CACHE?2:4;
            require(pair<4 && s.returned[pair]==~0U,"q3_ambiguous_returned_cache");
            const VHO_TENSOR_OPERATION &copy=Op(i,"copy");UINT32 scale=Scale(copy.inputs[0]),match=~0U;
            for(unsigned k=0;k<_p.operations.size();++k) {const VHO_TENSOR_OPERATION &candidate=_p.operations[k];
                if(candidate.name=="copy" && _p.values[candidate.output].role==2 && _p.values[candidate.inputs[0]].storage==scale) {
                    require(match==~0U,"q3_ambiguous_returned_scale");match=candidate.output;
                }}
            require(match!=~0U && _p.values[match].dtype=="float32","q3_returned_scale");s.returned[pair]=i;s.returned[pair+1]=match;
        }
        for(unsigned i=0;i<4;++i) require(s.returned[i]!=~0U,"q3_incomplete_return_bundle");
        for(unsigned pair=0;pair<4;pair+=2) {
            const VHO_TENSOR_VALUE &cache=_p.values[s.returned[pair]],&scale=_p.values[s.returned[pair+1]];
            require(cache.shape.size()==scale.shape.size() && scale.shape.back()==1,"q3_scale_shape");
            for(unsigned i=0;i+1<cache.shape.size();++i) require(cache.shape[i]==scale.shape[i],"q3_scale_shape");
        }
        s.encoding=_p.values[s.returned[0]].cache_encoding;
        require(s.encoding==_p.values[s.returned[2]].cache_encoding,"q3_encoding_pair");
    }
    /* Incoming encoded pools are declared uses, not float construction inputs. */
    void Inputs(VHO_TENSOR_SERVING_SCHEMA &s) {
        for(unsigned i=0;i<_p.values.size();++i) {
            const VHO_TENSOR_VALUE &v=_p.values[i];if(v.role!=1 || v.cache_trait.empty()) continue;
            unsigned pair=v.cache_trait==DSL_TENSOR_TRAIT_LATENT_KV_CACHE?0:v.cache_trait==DSL_TENSOR_TRAIT_ROTARY_KEY_CACHE?2:4;
            require(pair<4 && s.incoming[pair]==~0U,"q3_ambiguous_incoming_cache");
            s.incoming[pair]=i;s.incoming[pair+1]=Scale(i);
        }
        if(s.geometry.memory_form=="dense") {
            require(s.geometry.source_effect=="construction" && s.incoming[0]==~0U && s.incoming[2]==~0U,"q3_dense_scope");return;
        }
        s.paged=true;for(unsigned i=0;i<4;++i) require(s.incoming[i]!=~0U,"q3_incoming_bundle");
        const VHO_TENSOR_VALUE &c=_p.values[s.incoming[0]],&r=_p.values[s.incoming[2]];
        require(c.shape.size()==3 && r.shape.size()==3 && c.shape[0]==r.shape[0] && c.shape[1]==r.shape[1],"q3_pool_shape");
        s.pages=c.shape[0];s.page_tokens=c.shape[1];
        for(unsigned i=0;i<4;++i) {
            require(_p.values[s.incoming[i]].shape==_p.values[s.returned[i]].shape && _p.values[s.incoming[i]].dtype==_p.values[s.returned[i]].dtype,"q3_successor_shape");
            if(!(i%2)) require(_p.values[s.incoming[i]].cache_encoding==s.encoding,"q3_incoming_encoding");
        }
        UINT32 indices=~0U;
        for(unsigned i=0;i<_p.operations.size();++i) {
            const VHO_TENSOR_OPERATION &op=_p.operations[i];if(op.name!="common.gather" || _p.values[op.output].cache_trait!=DSL_TENSOR_TRAIT_LATENT_KV_CACHE) continue;
            require(indices==~0U && op.attributes.find("attr.axis")->second=="0","q3_cache_gather");indices=Unshape(op.inputs[1]);
        }
        require(indices!=~0U,"q3_missing_page_mapping");const VHO_TENSOR_OPERATION &add=Op(indices,"common.add");
        const VHO_TENSOR_OPERATION &mul=Op(add.inputs[0],"common.mul");Constant(mul.inputs[1],s.page_tokens);
        const VHO_TENSOR_OPERATION &gather=Op(mul.inputs[0],"common.gather");s.table=gather.inputs[0];
        const VHO_TENSOR_OPERATION &div=Op(gather.inputs[1],"common.div");Constant(div.inputs[1],s.page_tokens);s.tokens=div.inputs[0];
        const VHO_TENSOR_OPERATION &rem=Op(Unshape(add.inputs[1]),"common.rem");Constant(rem.inputs[1],s.page_tokens);
        require(rem.inputs[0]==s.tokens && _p.values[s.table].role==1 && _p.values[s.tokens].role==1 && gather.attributes.find("attr.axis")->second=="1","q3_page_operand_join");
        require(_p.values[s.table].dtype=="int64" && _p.values[s.tokens].dtype=="int64","q3_page_integer_type");
        const UINT64 table_shape[]={s.geometry.batch,(s.geometry.keys+s.page_tokens-1)/s.page_tokens};
        require(_p.values[s.table].shape==std::vector<UINT64>(table_shape,table_shape+2) && _p.values[s.tokens].shape==std::vector<UINT64>(1,s.geometry.keys),"q3_page_domain");
        for(unsigned i=0;i<_p.operations.size();++i) {
            const VHO_TENSOR_OPERATION &op=_p.operations[i];if(op.name=="common.gather" && op.output!=mul.inputs[0]) {
                require(Unshape(op.inputs[1])==indices,"q3_cache_scale_mapping_mismatch");
            }
            if(op.name=="common.scatter") {
                UINT32 slots=Unshape(op.inputs[1]);require(s.slots==~0U || s.slots==slots,"q3_update_index_join");s.slots=slots;
            }
        }
        if(s.geometry.source_effect=="functional-index-update") {
            require(s.slots!=~0U && _p.values[s.slots].role==1 && s.geometry.update_items_known && _p.values[s.slots].dtype=="int64","q3_update_slots");
            const UINT64 shape[]={s.geometry.batch,s.geometry.update_items};
            require(_p.values[s.slots].shape==std::vector<UINT64>(shape,shape+2),"q3_update_slot_shape");
        }
    }
};
}
/* Discovery does not consume pointer declarations as live engine evidence. */
bool VHO_tensor_serving_schema(const VHO_TENSOR_PROGRAM &p,VHO_TENSOR_SERVING_SCHEMA &out,std::string &why) {
    out=VHO_TENSOR_SERVING_SCHEMA();why.clear();
    try {
        VHO_TENSOR_WORKLOAD_FACTS f;bool inferred=VHO_tensor_infer_workload(p,NULL,f,why);require(inferred && f.geometry.known,"q3_unknown_source_geometry");
        require(f.geometry.batch<=16,"q3_request_bound");
        VHO_TENSOR_SERVING_SCHEMA s;s.geometry=f.geometry;s.source_key=f.geometry.source_key;SERVING_SOURCE source(p);source.Outputs(s);source.Inputs(s);
        for(unsigned i=0;i<p.values.size();++i) if(p.values[i].storage==i) s.roots.push_back(i);
        s.metadata_columns=9+s.geometry.queries+s.geometry.keys+(s.geometry.update_items_known?s.geometry.update_items:0);
        out=s;return true;
    } catch(const std::exception &e) {why=e.what();return false;}
}
/* Source-side explanation is not a runtime publication certificate. */
std::string VHO_tensor_serving_explanation(const VHO_TENSOR_PROGRAM &p) {
    VHO_TENSOR_SERVING_SCHEMA s;std::string why;std::ostringstream out;
    out<<"Q3 serving binding: unbound at compile time. ";
    if(VHO_tensor_serving_schema(p,s,why)) out<<(s.paged?"Indirect page table, token and update-slot buffers; ":"Dense construction; ")<<"source P="<<s.page_tokens<<" is allocator layout, not compute tile width. ";
    else out<<"Unsupported serving schema ("<<why<<"); ordinary source route retained. ";
    out<<"A synchronous owner must lease actual buffers, join cache/scale generations, validate page ranges and masks, and publish all four successor buffers only after status=0. Readers pin old bundles until completion; no GPU/TMA, search or dispatch permission is implied. Q4 costing requires a fresh Q3 snapshot; quantized cache-service studies account for packed payload, paired scales, page fragments, dequantization, retained-tile reuse and split tuples. All Q4 candidates remain analysis-only; source capacity work, correctness fallback and deferred AOT selection are preserved.";return out.str();
}
