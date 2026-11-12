/* Copyright (C) 2026 Open64 Project
 * Source-frontend-owned, read-only, intraprocedural QDQ/use inference.
 * Operates on copied logical values; owns no WN/ST/TY/DST or runtime data.
 * It infers content/encoding, never allocation identity, phase or lifetime.
 * Design: doc/AI-COMPILER-MLA-Q1-NATIVE-INGESTION-v1.0.md. */
#ifndef open64_quantized_cache_inference_INCLUDED
#define open64_quantized_cache_inference_INCLUDED
#include <map>
#include <set>
#include <string>
#include <vector>
#include <stdexcept>
#include <stdint.h>
#include <string.h>
#include "dsl_tensor_traits.h"

namespace open64_q1 {
struct DSC_QUANTIZED_SOURCE_VALUE {
    std::string name, op, dtype;
    unsigned version;
    std::vector<size_t> kids;
    std::vector<uint64_t> shape;
    std::map<std::string,std::string> attrs;
};
struct DSC_QUANTIZED_SOURCE_FACT {
    std::string role, encoding, scale, evidence;
};
typedef std::map<size_t,DSC_QUANTIZED_SOURCE_FACT> DSC_QUANTIZED_SOURCE_FACTS;

class DSC_QUANTIZED_SOURCE_INFERENCE {
    const std::vector<DSC_QUANTIZED_SOURCE_VALUE> &_graph;
    DSC_QUANTIZED_SOURCE_FACTS _facts;
    /* Test copied identity/arity only; missing values do not match. */
    bool Is(size_t i,const char *op,size_t arity) const {
        return i<_graph.size() && _graph[i].op==op && _graph[i].kids.size()==arity;
    }
    /* Match exact source semantics without repairing a graph attribute. */
    bool Has_attr(size_t i,const char *key,const char *value) const {
        std::map<std::string,std::string>::const_iterator a=_graph[i].attrs.find(key);
        return a!=_graph[i].attrs.end() && a->second==value;
    }
    /* Follow topological logical reindexing, without implying storage aliasing. */
    size_t View_root(size_t i) const {
        while(Is(i,"common.reshape",1) || Is(i,"common.transpose",1)) i=_graph[i].kids[0];
        return i;
    }
    /* Recognize source binary32 bits, independent of sample tensor values. */
    bool Is_literal(size_t i,const char *bits) const {
        return Is(i,"common.fill_like",1) && _graph[i].version==1 && Has_attr(i,"attr.value_bits",bits);
    }
    /* Prove the closed source scale formula; opaque producers remain unknown. */
    bool Scale_profile(size_t scale,size_t raw,const char *qmax) const {
        if(!Is(scale,"common.div",2) || _graph[scale].version!=1 || !Is_literal(_graph[scale].kids[1],qmax)) return false;
        size_t clamp=_graph[scale].kids[0];
        if(!Is(clamp,"common.clamp",1) || !Has_attr(clamp,"attr.has_min","true") ||
           !Has_attr(clamp,"attr.min_bits","39800000") || !Has_attr(clamp,"attr.has_max","false")) return false;
        size_t reduce=_graph[clamp].kids[0];
        if(!Is(reduce,"common.reduce_max",1) || !Has_attr(reduce,"attr.keepdim","true")) return false;
        size_t abs=_graph[reduce].kids[0];
        return Is(abs,"common.abs",1) && _graph[abs].kids[0]==raw;
    }
    /* Match RNE/clamp/divide joins and the identical SSA scale; no reassociation. */
    bool Match_encoder(size_t clamp,size_t scale,bool u4,size_t *raw) const {
        if(!Is(clamp,"common.clamp",1) || !Has_attr(clamp,"attr.has_min","true") ||
           !Has_attr(clamp,"attr.has_max","true") ||
           !Has_attr(clamp,"attr.min_bits",u4?"00000000":"c2fe0000") ||
           !Has_attr(clamp,"attr.max_bits",u4?"41700000":"42fe0000")) return false;
        size_t round=_graph[clamp].kids[0];
        if(u4) {
            if(!Is(round,"common.add",2) || !Is_literal(_graph[round].kids[1],"41000000")) return false;
            round=_graph[round].kids[0];
        }
        if(!Is(round,"common.round_even",1) || !Has_attr(round,"attr.rounding","ties_to_even")) return false;
        size_t divide=_graph[round].kids[0];
        if(!Is(divide,"common.div",2) || _graph[divide].kids[1]!=scale) return false;
        *raw=_graph[divide].kids[0];
        return Scale_profile(scale,*raw,u4?"40e00000":"42fe0000");
    }
    /* Compare data/scale gather indices, and scale's singleton logical column.
     * A common storage dtype alone is never sufficient evidence. */
    bool Paired_scale(size_t storage,size_t scale,size_t decoded) const {
        if(_graph[scale].dtype!="float32" || _graph[scale].shape.size()!=_graph[decoded].shape.size() ||
           _graph[scale].shape.empty() || _graph[scale].shape.back()!=1) return false;
        for(size_t d=0;d+1<_graph[scale].shape.size();++d)
            if(_graph[scale].shape[d]!=_graph[decoded].shape[d]) return false;
        size_t data=View_root(storage), auxiliary=View_root(scale);
        bool gathered=Is(data,"common.gather",2), scaled=Is(auxiliary,"common.gather",2);
        return gathered==scaled && (!gathered ||
            (_graph[data].kids[1]==_graph[auxiliary].kids[1] && Has_attr(data,"attr.axis","0") && Has_attr(auxiliary,"attr.axis","0")));
    }
    /* Derive only decoder meaning from byte dtype/unpack/scale uses. */
    bool Match_decode(size_t decoded,size_t *storage,size_t *scale,std::string *encoding) const {
        if(!Is(decoded,"common.mul",2) || _graph[decoded].dtype!="float32" || _graph[decoded].shape.size()!=3) return false;
        size_t component=_graph[decoded].kids[0]; *scale=_graph[decoded].kids[1];
        if(Is(component,"common.convert",1) && _graph[_graph[component].kids[0]].dtype=="int8" &&
           Has_attr(component,"attr.src_dtype","int8") && Has_attr(component,"attr.dst_dtype","float32") &&
           Has_attr(component,"attr.mode","exact")) {
            *storage=_graph[component].kids[0]; *encoding=DSL_TENSOR_ENCODING_S8_SCALED_F32;
        } else if(Is(component,"common.sub",2) && Is_literal(_graph[component].kids[1],"41000000")) {
            size_t convert=_graph[component].kids[0];
            if(!Is(convert,"common.convert",1) || !Has_attr(convert,"attr.src_dtype","uint8") ||
               !Has_attr(convert,"attr.dst_dtype","float32") || !Has_attr(convert,"attr.mode","exact")) return false;
            size_t unpack=_graph[convert].kids[0];
            if(!Is(unpack,"common.unpack_u4",1) || !Has_attr(unpack,"attr.nibble_order","even_low")) return false;
            *storage=_graph[unpack].kids[0]; *encoding=DSL_TENSOR_ENCODING_U4_OFFSET8_F32;
        } else {
            size_t raw;
            if(!Match_encoder(component,*scale,false,&raw)) return false;
            *storage=decoded; *encoding=DSL_TENSOR_FAKE_QUANT_S8_F32;
        }
        return Paired_scale(*storage,*scale,decoded);
    }
    /* Accumulate detached facts; contradictions abort before native products. */
    void Record_fact(size_t i,const std::string &role,const std::string &encoding,size_t scale) {
        DSC_QUANTIZED_SOURCE_FACT f; f.role=role; f.encoding=encoding; f.scale=_graph[scale].name;
        f.evidence="native-source-qdq-use.v1";
        DSC_QUANTIZED_SOURCE_FACTS::const_iterator old=_facts.find(i);
        if(old!=_facts.end() && (old->second.role!=role || old->second.encoding!=encoding || old->second.scale!=f.scale))
            throw std::runtime_error("conflicting cache role/encoding/scale evidence");
        _facts[i]=f;
    }
    /* Preserve paired data/scale SSA through views, gathers and fresh updates;
     * mismatched versions/indices refuse instead of crediting cache reuse. */
    void Trace_storage(size_t storage,const std::string &role,const std::string &encoding,size_t scale,unsigned depth=0) {
        if(depth>256) throw std::runtime_error("cache inference view-chain bound");
        Record_fact(storage,role,encoding,scale);
        if(Is(storage,"common.reshape",1)) Trace_storage(_graph[storage].kids[0],role,encoding,scale,depth+1);
        else if(Is(storage,"common.gather",2)) {
            size_t auxiliary=View_root(scale);
            if(!Is(auxiliary,"common.gather",2)) throw std::runtime_error("missing paired scale gather");
            Trace_storage(_graph[storage].kids[0],role,encoding,_graph[auxiliary].kids[0],depth+1);
        } else if(Is(storage,"common.scatter",3) && _graph[storage].version==2) {
            size_t auxiliary=View_root(scale);
            if(!Is(auxiliary,"common.scatter",3) || _graph[storage].kids[1]!=_graph[auxiliary].kids[1])
                throw std::runtime_error("cache update lacks paired scale version/indices");
            Trace_storage(_graph[storage].kids[0],role,encoding,_graph[auxiliary].kids[0],depth+1);
            Trace_storage(_graph[storage].kids[2],role,encoding,_graph[auxiliary].kids[2],depth+1);
        }
    }
    /* Mark content roles; decoded F32 and fake quantization get no byte layout. */
    void Mark_cache(size_t decoded,const char *role) {
        size_t storage,scale; std::string encoding;
        if(!Match_decode(decoded,&storage,&scale,&encoding)) return;
        if(encoding==DSL_TENSOR_FAKE_QUANT_S8_F32) {
            Record_fact(decoded,role,encoding,scale); return;
        }
        Record_fact(decoded,role,"",scale); /* Decoded F32 is not byte-compressed. */
        Trace_storage(storage,role,encoding,scale);
    }
    /* Require normalized weights derived from this score computation. */
    bool Normalized_from(size_t weights,size_t score_add) const {
        if(!Is(weights,"common.div",2)) return false;
        size_t exp=_graph[weights].kids[0],safe=_graph[weights].kids[1];
        if(!Is(exp,"common.exp",1) || !Is(safe,"common.select",3)) return false;
        size_t den=_graph[safe].kids[1];
        if(!Is(den,"common.reduce_sum",1) || _graph[den].kids[0]!=exp) return false;
        std::set<size_t> pending,seen;pending.insert(_graph[exp].kids[0]);
        while(!pending.empty()) {
            size_t i=*pending.begin();pending.erase(pending.begin());
            if(i==score_add) return true;
            if(!seen.insert(i).second) continue;
            pending.insert(_graph[i].kids.begin(),_graph[i].kids.end());
        }
        return false;
    }
public:
    /* Borrow copied source records for this query only; no compiler scope switch. */
    explicit DSC_QUANTIZED_SOURCE_INFERENCE(const std::vector<DSC_QUANTIZED_SOURCE_VALUE> &graph):_graph(graph) {}
    /* Validate topology, then infer bounded source facts. A pair-check budget
     * exhaustion returns unknown; malformed/contradictory joins throw. */
    DSC_QUANTIZED_SOURCE_FACTS Infer() {
        if(_graph.size()>32768) throw std::runtime_error("cache inference graph bound");
        std::set<std::string> names;
        for(size_t i=0;i<_graph.size();++i) {
            if(_graph[i].name.empty() || !names.insert(_graph[i].name).second) throw std::runtime_error("cache inference duplicate/empty value");
            for(size_t k=0;k<_graph[i].kids.size();++k)
                if(_graph[i].kids[k]>=i) throw std::runtime_error("cache inference non-topological operand");
        }
        size_t work=0;
        for(size_t a=0;a<_graph.size();++a) if(Is(a,"common.add",2)) {
            size_t score[2]={View_root(_graph[a].kids[0]),View_root(_graph[a].kids[1])};
            if(!Is(score[0],"common.matmul",2) || !Is(score[1],"common.matmul",2)) continue;
            for(unsigned side=0;side<2;++side) {
                size_t latent=View_root(_graph[score[side]].kids[1]),rotary=View_root(_graph[score[1-side]].kids[1]);
                if(latent==rotary) continue;
                for(size_t m=a+1;m<_graph.size();++m) {
                    if(++work>1000000) return DSC_QUANTIZED_SOURCE_FACTS(); /* Unknown, never partial evidence. */
                    if(Is(m,"common.matmul",2) &&
                       View_root(_graph[m].kids[1])==latent && Normalized_from(_graph[m].kids[0],a)) {
                        size_t storage,scale;std::string encoding;
                        if(Match_decode(latent,&storage,&scale,&encoding) && Match_decode(rotary,&storage,&scale,&encoding)) {
                            Mark_cache(latent,DSL_TENSOR_TRAIT_LATENT_KV_CACHE);
                            Mark_cache(rotary,DSL_TENSOR_TRAIT_ROTARY_KEY_CACHE);
                        }
                    }
                }
            }
        }
        return _facts;
    }
};
} /* namespace open64_q1 */
#endif
