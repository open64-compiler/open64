/* Copyright (C) 2026 Open64 Project */
/* Detached source geometry and checked invocation phase facts, owned by VHO.
 * Reuses Q1 semantic admission; the legacy BHSD decoder-profile verifier is not
 * an MLA/shared-latent phase proof. No native allocation, JSON or search calls.
 * Design: doc/AI-COMPILER-MLA-Q2-WORKLOAD-v1.0.md. */
#include "tensor_workload.h"
#include <set>
#include <sstream>
#include <stdexcept>
namespace {
const UINT64 limit=16777216;
/* Whole-result refusal prevents a partially validated phase from escaping. */
void need(bool b,const char *why) {if(!b) throw std::runtime_error(why);}
/* Length-delimited fields bind exact source and invocation values, not hashes. */
void text(std::ostringstream &s,const std::string &v) {s<<v.size()<<':'<<v<<';';}
/* Bound external identifiers used in logs and seals. */
bool token(const std::string &s) {return !s.empty() && s.size()<=128 && s.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.:-")==std::string::npos;}
/* The source validator has already checked semantic types/attrs. Recognize the
 * complete ordered attention tail, never a name, dtype or one matmul alone. */
bool attention(const VHO_TENSOR_PROGRAM &p,unsigned start,VHO_TENSOR_WORKLOAD_GEOMETRY &g) {
    static const char *names[]={"common.matmul","common.reshape","common.transpose","common.matmul",
        "common.reshape","common.transpose","common.matmul","common.add","common.fill_like","common.mul",
        "common.logical_not","common.fill_like","common.select","common.reduce_max","common.isfinite",
        "common.fill_like","common.select","common.sub","common.exp","common.reduce_sum","common.fill_like",
        "common.greater","common.fill_like","common.select","common.div","common.reshape","common.matmul",
        "common.transpose","common.matmul","common.fill_like","common.greater","common.log","common.add",
        "common.fill_like","common.select","common.reshape","copy","copy"};
    static const int refs[][3]={{-1,-2,-99},{-3,-99,-99},{1,-99,-99},{0,2,-99},{-4,-99,-99},{4,-99,-99},
        {-5,5,-99},{3,6,-99},{7,-99,-99},{7,8,-99},{-6,-99,-99},{9,-99,-99},{10,11,9},{12,-99,-99},
        {13,-99,-99},{13,-99,-99},{14,13,15},{12,16,-99},{17,-99,-99},{18,-99,-99},{19,-99,-99},
        {19,20,-99},{19,-99,-99},{21,19,22},{18,23,-99},{-3,-99,-99},{24,25,-99},{-7,-99,-99},
        {26,27,-99},{19,-99,-99},{19,29,-99},{23,-99,-99},{16,31,-99},{16,-99,-99},{30,32,33},
        {34,-99,-99},{28,-99,-99},{35,-99,-99}};
    if(start+38>p.operations.size()) return false;
    UINT32 inputs[]={p.operations[start].inputs[0],p.operations[start].inputs[1],p.operations[start+1].inputs[0],
        p.operations[start+4].inputs[0],p.operations[start+6].inputs[0],p.operations[start+10].inputs[0],p.operations[start+27].inputs[0]};
    for(unsigned i=0;i<38;++i) {
        const VHO_TENSOR_OPERATION &op=p.operations[start+i];if(op.name!=names[i]) return false;
        unsigned n=0;while(n<3 && refs[i][n]!=-99) ++n;if(op.inputs.size()!=n) return false;
        for(unsigned k=0;k<n;++k) {int r=refs[i][k];if(op.inputs[k]!=(r<0?inputs[-r-1]:p.operations[start+r].output)) return false;}
    }
    const VHO_TENSOR_VALUE &q=p.values[inputs[0]],&qr=p.values[inputs[4]],&c=p.values[inputs[2]],&r=p.values[inputs[3]],&mask=p.values[inputs[5]];
    if(q.role!=1 || qr.role!=1 || mask.role!=1 || q.shape.size()!=4 || qr.shape.size()!=4 ||
       c.shape.size()!=3 || r.shape.size()!=3 || mask.shape.size()!=4 ||
       c.cache_trait!=DSL_TENSOR_TRAIT_LATENT_KV_CACHE || r.cache_trait!=DSL_TENSOR_TRAIT_ROTARY_KEY_CACHE) return false;
    UINT64 b=q.shape[0],h=q.shape[1],n=q.shape[2],l=c.shape[1];
    UINT64 dims[]={b,h,n,l};if(mask.shape!=std::vector<UINT64>(dims,dims+4) || c.shape[0]!=b || r.shape[0]!=b || r.shape[1]!=l ||
        qr.shape[0]!=b || qr.shape[1]!=h || qr.shape[2]!=n || qr.shape[3]!=r.shape[2]) return false;
    // Exact guards/axis are necessary to call this an attention query/key domain.
    const unsigned fills[]={11,15,20,22,29,33};const char *bits[]={"ff800000","00000000","00000000","3f800000","00000000","ff800000"};
    for(unsigned i=0;i<6;++i) if(p.operations[start+fills[i]].attributes.find("attr.value_bits")->second!=bits[i]) return false;
    for(unsigned i=0;i<2;++i) {const VHO_TENSOR_OPERATION &op=p.operations[start+(i?19:13)];
        if(op.attributes.find("attr.axis")->second!="3" || op.attributes.find("attr.keepdim")->second!="true") return false;}
    if(p.values[p.operations[start+36].output].role!=2 || p.values[p.operations[start+37].output].role!=2) return false;
    g.known=true;g.batch=b;g.heads=h;g.queries=n;g.keys=l;g.latent=c.shape[2];g.rotary=r.shape[2];
    g.query_value=inputs[0];g.rotary_query_value=inputs[4];g.mask_value=inputs[5];g.latent_value=inputs[2];g.rotary_value=inputs[3];
    g.reason="closed_attention_dataflow";return true;
}
/* Inspect actual cache ancestors. Functional scatter is an index update, not
 * automatically an append to the end of a request's chronological history. */
void ancestors(const VHO_TENSOR_PROGRAM &p,const std::map<UINT32,unsigned> &defs,UINT32 v,std::set<UINT32> &seen,std::set<unsigned> &ops) {
    if(!seen.insert(v).second || !defs.count(v)) return;
    unsigned i=defs.find(v)->second;ops.insert(i);
    for(unsigned k=0;k<p.operations[i].inputs.size();++k) ancestors(p,defs,p.operations[i].inputs[k],seen,ops);
}
/* Unwrap only storage-preserving reshapes for update-index cardinality. */
UINT32 unshape(const VHO_TENSOR_PROGRAM &p,const std::map<UINT32,unsigned> &defs,UINT32 v) {
    for(unsigned i=0;i<256 && defs.count(v);++i) {const VHO_TENSOR_OPERATION &op=p.operations[defs.find(v)->second];if(op.name!="common.reshape") break;v=op.inputs[0];}return v;
}
/* Shape recognition never reads mask/index buffers or assumes valid lengths. */
VHO_TENSOR_WORKLOAD_GEOMETRY geometry(const VHO_TENSOR_PROGRAM &p) {
    VHO_TENSOR_WORKLOAD_GEOMETRY g;g.source_key=VHO_tensor_quantized_high_key(p);g.reason="unrecognized_attention_dataflow";
    unsigned matches=0;
    for(unsigned i=0;i+38<=p.operations.size();++i) if(p.operations[i].name=="common.matmul") {
        VHO_TENSOR_WORKLOAD_GEOMETRY found;if(attention(p,i,found)) {++matches;found.source_key=g.source_key;g=found;}
    }
    if(matches!=1) {VHO_TENSOR_WORKLOAD_GEOMETRY unknown;unknown.source_key=g.source_key;unknown.reason=matches?"multiple_attention_domains":"unrecognized_attention_dataflow";return unknown;}
    std::map<UINT32,unsigned> defs;for(unsigned i=0;i<p.operations.size();++i) defs[p.operations[i].output]=i;
    std::set<UINT32> seen;std::set<unsigned> ops;ancestors(p,defs,g.latent_value,seen,ops);ancestors(p,defs,g.rotary_value,seen,ops);
    bool gather=false,encode=false;std::set<UINT32> indices;
    for(std::set<unsigned>::const_iterator i=ops.begin();i!=ops.end();++i) {
        const VHO_TENSOR_OPERATION &op=p.operations[*i];gather|=op.name=="common.gather";encode|=op.name=="common.round_even";
        if(op.name=="common.scatter") indices.insert(unshape(p,defs,op.inputs[1]));
    }
    g.memory_form=gather?"indirect-indexed":"dense";
    g.source_effect=!indices.empty()?"functional-index-update":encode?"construction":"read-only";
    if(indices.size()==1) {const VHO_TENSOR_VALUE &idx=p.values[*indices.begin()];
        if(idx.role==1 && idx.shape.size()==2 && idx.shape[0]==g.batch) {g.update_items_known=true;g.update_items=idx.shape[1];}}
    return g;
}
/* An exact mask witness validates padding, read-domain and causal/window claims.
 * Missing entries do not grant a weaker guarded phase; they refuse the witness. */
void mask(const VHO_TENSOR_WORKLOAD_GEOMETRY &g,const VHO_TENSOR_WORKLOAD_REQUEST &r) {
    UINT64 count=g.heads*g.queries*g.keys;need(count<=limit && r.allowed.size()==count,"q2_mask_extent");
    need(r.mask_policy=="opaque" || r.mask_policy=="causal" || r.mask_policy=="causal-window","q2_mask_policy");
    need(r.mask_policy=="causal-window"?r.window_left>0:r.window_left==0,"q2_window_extent");
    for(UINT64 h=0;h<g.heads;++h) for(UINT64 q=0;q<g.queries;++q) for(UINT64 k=0;k<g.keys;++k) {
        unsigned bit=r.allowed[(h*g.queries+q)*g.keys+k];need(bit<=1,"q2_mask_boolean");
        if(!bit) continue;
        need(q<r.queries && k<r.valid,"q2_mask_outside_active_domain");
        if(r.mask_policy!="opaque") {
            need(r.key_positions[k]<=r.query_positions[q],"q2_mask_future_key");
            if(r.mask_policy=="causal-window") need(r.query_positions[q]-r.key_positions[k]<r.window_left,"q2_mask_outside_window");
        }
    }
}
/* Exact ordered position witnesses can begin above zero (prefix/window reuse). */
void positions(const std::vector<UINT64> &v,UINT64 count) {
    need(v.size()==count,"q2_position_extent");
    for(unsigned i=0;i<v.size();++i) need(v[i]<=limit && (!i || v[i]>v[i-1]),"q2_position_order");
}
/* Revalidation compares every copied result as well as its exact input seal;
 * a changed public review field cannot turn an old unknown into a known phase. */
std::string result_key(const VHO_TENSOR_WORKLOAD_FACTS &f) {
    std::ostringstream s;const VHO_TENSOR_WORKLOAD_GEOMETRY &g=f.geometry;
    const std::string *texts[]={&g.source_key,&g.reason,&g.memory_form,&g.source_effect,&f.phase,&f.workload,&f.reason,&f.evidence,&f.invocation_id,&f.binding_key};
    for(unsigned i=0;i<10;++i) text(s,*texts[i]);
    s<<g.known<<';'<<g.update_items_known<<';'<<g.batch<<';'<<g.heads<<';'<<g.queries<<';'<<g.keys<<';'<<g.latent<<';'<<g.rotary<<';'<<g.update_items<<';'
        <<g.query_value<<';'<<g.rotary_query_value<<';'<<g.mask_value<<';'<<g.latent_value<<';'<<g.rotary_value<<';'<<f.has_unknown<<';'<<f.ragged<<';'<<f.zero_active_queries<<';'<<f.segments.size()<<';';
    for(unsigned i=0;i<f.segments.size();++i) {
        const VHO_TENSOR_WORKLOAD_SEGMENT &r=f.segments[i];s<<r.request<<';'<<r.query_begin<<';'<<r.queries<<';'<<r.history<<';'<<r.valid<<';'<<r.append_tokens<<';';
        text(s,r.phase);text(s,r.reason);text(s,r.mask_policy);text(s,r.update);text(s,r.work);
        const std::vector<UINT64> *lists[]={&r.query_positions,&r.key_positions,&r.update_positions};
        for(unsigned j=0;j<3;++j) {s<<lists[j]->size()<<';';for(unsigned k=0;k<lists[j]->size();++k) s<<(*lists[j])[k]<<';';}
    }
    return s.str();
}
/* Validate declarations against semantic witnesses, never infer a phase from
 * their dimensions. Q3 must join this witness to actual live serving buffers. */
void invocation(const VHO_TENSOR_WORKLOAD_GEOMETRY &g,const VHO_TENSOR_WORKLOAD_INVOCATION &in,VHO_TENSOR_WORKLOAD_FACTS &out) {
    need(g.known,"q2_invocation_without_geometry");need(in.version==1,"q2_invocation_version");
    need(in.source_key==g.source_key,"q2_stale_source_binding");need(token(in.invocation_id),"q2_invocation_identity");
    need(in.evidence=="bound-serving-invocation-v1" || in.evidence=="fixture-intent-not-inferred-from-graph","q2_evidence");
    need(in.requests.size()==g.batch && g.batch<=16,"q2_request_extent");
    std::ostringstream key;text(key,g.source_key);text(key,in.invocation_id);text(key,in.evidence);key<<in.version<<';';
    std::set<std::string> phases;std::set<UINT64> qs,ls,all_qs,all_ls;bool unknown=false;UINT64 begin=0,total=0;
    out.segments.clear();
    for(unsigned b=0;b<in.requests.size();++b) {
        const VHO_TENSOR_WORKLOAD_REQUEST &r=in.requests[b];
        need(r.request==b && r.query_begin==begin && r.queries<=g.queries,"q2_query_segment");
        need(r.valid<=g.keys && r.history<=r.valid && r.window_left<=limit,"q2_length_domain");
        need(r.phase=="unknown" || r.phase=="prefill" || r.phase=="decode" || r.phase=="speculative-verify","q2_phase_label");
        positions(r.query_positions,r.queries);positions(r.key_positions,r.valid);positions(r.update_positions,r.append_tokens);
        for(unsigned q=0;q<r.query_positions.size();++q) {
            bool found=false;for(unsigned k=0;k<r.key_positions.size();++k) found|=r.key_positions[k]==r.query_positions[q];
            need(found,"q2_query_outside_key_positions");
        }
        need(r.update=="none" || r.update=="construction" || r.update=="append" || r.update=="indexed-unknown-order","q2_update_kind");
        if(g.source_effect=="functional-index-update") {
            need(r.update=="append" || r.update=="indexed-unknown-order","q2_missing_source_update");
            need(g.update_items_known && r.append_tokens==g.update_items,"q2_update_extent");
            if(r.update=="append") {
                need(r.valid-r.history==r.append_tokens,"q2_append_length");
                for(unsigned k=0;k<r.update_positions.size();++k) need(r.update_positions[k]==r.key_positions[r.history+k],"q2_append_order");
            }
        } else {
            need(!r.append_tokens && r.update_positions.empty(),"q2_unexpected_append");
            need(r.update==(g.source_effect=="construction"?"construction":"none"),"q2_source_effect_mismatch");
            need(r.history==r.valid,"q2_readonly_or_construction_history");
        }
        mask(g,r);
        bool trusted=in.evidence=="bound-serving-invocation-v1";
        if(trusted && r.phase!="unknown" && r.queries) {
            need(r.mask_policy!="opaque","q2_phase_without_visibility_contract");
            if(r.phase=="decode") need(r.queries==1,"q2_decode_query_count");
            if(r.phase=="speculative-verify") need(r.queries>1,"q2_verify_query_count");
            if(g.source_effect=="functional-index-update") {
                need(r.update=="append","q2_phase_unknown_update_order");
                need(r.append_tokens==r.queries && r.update_positions==r.query_positions,"q2_phase_query_append_mismatch");
            }
        }
        VHO_TENSOR_WORKLOAD_SEGMENT s;s.request=b;s.query_begin=r.query_begin;s.queries=r.queries;s.history=r.history;s.valid=r.valid;s.append_tokens=r.append_tokens;
        s.phase=trusted && r.queries?r.phase:"unknown";s.reason=!r.queries?"zero_active_queries":!trusted?"untrusted_fixture_intent":r.phase=="unknown"?"phase_not_declared":"validated_invocation_declaration";
        s.mask_policy=r.mask_policy;s.update=r.update;s.work=r.queries?"active":"zero-active-queries";
        s.query_positions=r.query_positions;s.key_positions=r.key_positions;s.update_positions=r.update_positions;out.segments.push_back(s);
        all_qs.insert(r.queries);all_ls.insert(r.valid);
        if(r.queries) {if(s.phase=="unknown") unknown=true;else phases.insert(s.phase);qs.insert(r.queries);ls.insert(r.valid);}
        begin+=r.queries;total+=r.queries;need(begin<=limit,"q2_query_offset_overflow");
        key<<b<<';'<<r.query_begin<<';'<<r.queries<<';'<<r.history<<';'<<r.valid<<';'<<r.append_tokens<<';'<<r.window_left<<';';text(key,r.phase);text(key,r.mask_policy);text(key,r.update);
        const std::vector<UINT64> *lists[]={&r.query_positions,&r.key_positions,&r.update_positions};
        for(unsigned i=0;i<3;++i) {key<<lists[i]->size()<<';';for(unsigned k=0;k<lists[i]->size();++k) key<<(*lists[i])[k]<<';';}
        key<<r.allowed.size()<<';';for(unsigned k=0;k<r.allowed.size();++k) key<<unsigned(r.allowed[k]);key<<';';
    }
    out.has_unknown=unknown || !total;out.ragged=all_qs.size()>1 || all_ls.size()>1;
    out.zero_active_queries=!total;out.phase=out.has_unknown?"unknown":phases.size()>1?"mixed":*phases.begin();
    out.workload=!total?"zero-active-queries":qs.size()>1?"mixed-query-counts":*qs.begin()==1?"single-query":"multi-query";
    out.reason=!total?"zero_active_queries":unknown?"some_active_phases_unknown":phases.size()>1?"different_validated_request_phases":"validated_invocation_declaration";
    out.evidence=in.evidence;out.invocation_id=in.invocation_id;out.binding_key=key.str();
}
}
/* Source-only results preserve unknown active/position/mask/history facts. */
bool VHO_tensor_infer_workload(const VHO_TENSOR_PROGRAM &p,const VHO_TENSOR_WORKLOAD_INVOCATION *in,VHO_TENSOR_WORKLOAD_FACTS &out,std::string &why) {
    out=VHO_TENSOR_WORKLOAD_FACTS();why.clear();
    try {
        need(VHO_tensor_validate_quantized_high(p,NULL),"q2_source_admission");VHO_TENSOR_WORKLOAD_FACTS result;result.geometry=geometry(p);
        const VHO_TENSOR_WORKLOAD_GEOMETRY &g=result.geometry;
        if(g.known) {
            result.reason="no_bound_serving_invocation";result.workload=g.queries==1?"single-query-axis":"multi-query-axis";
            if(g.batch>16) result.reason="request_census_bound";
            else for(unsigned b=0;b<g.batch;++b) {VHO_TENSOR_WORKLOAD_SEGMENT s;s.request=b;result.segments.push_back(s);}
        }
        if(in) invocation(g,*in,result);out=result;return true;
    } catch(const std::exception &e) {why=e.what();return false;}
}
/* A source seal and complete invocation seal are required together. Neither
 * stale engine labels nor matching dimensions can revive former facts. */
bool VHO_tensor_revalidate_workload(const VHO_TENSOR_PROGRAM &p,const VHO_TENSOR_WORKLOAD_INVOCATION *in,
    const VHO_TENSOR_WORKLOAD_FACTS &facts,std::string &why) {
    VHO_TENSOR_WORKLOAD_FACTS fresh;if(!VHO_tensor_infer_workload(p,in,fresh,why)) return false;
    if(result_key(facts)!=result_key(fresh)) {
        why="q2_workload_binding_changed";return false;
    }
    return true;
}
/* Annotation is separate from inference and cannot grant a phase certificate. */
std::string VHO_tensor_workload_explanation(const VHO_TENSOR_PROGRAM &p) {
    VHO_TENSOR_WORKLOAD_FACTS f;std::string why;
    if(!VHO_tensor_infer_workload(p,NULL,f,why)) return "Q2 workload: unknown; source admission unavailable; no dispatch permission.";
    std::ostringstream s;s<<"Q2 workload: serving phase=unknown; ";
    if(f.geometry.known) s<<"source dense evaluation B="<<f.geometry.batch<<", H="<<f.geometry.heads<<", Q="<<f.geometry.queries<<", KV extent="<<f.geometry.keys<<" (capacities, not active/valid serving lengths); "<<f.geometry.source_effect<<"; ";
    else s<<f.geometry.reason<<"; ";
    s<<"Q=1 can be a prefill tail; Q>1 can be verification. Dtype, cache extent and index updates do not prove phase, causal mask, chronological append, allocator lifetime or cross-head reuse. No search, dispatch, JIT or GPU permission is inferred.";return s.str();
}
/* A copied report remains readable after the surrounding native epoch expires. */
void VHO_tensor_print_workload(FILE *file,const VHO_TENSOR_WORKLOAD_FACTS &f,const char *stage) {
    if(!file) return;const VHO_TENSOR_WORKLOAD_GEOMETRY &g=f.geometry;
    fprintf(file,"AI_Q2_WORKLOAD 1 %s geometry=%s phase=%s workload=%s reason=%s B=%llu H=%llu Q_capacity=%llu KV_extent=%llu memory=%s effect=%s search=false dispatch=false\n",
        stage,g.known?"known":"unknown",f.phase.c_str(),f.workload.c_str(),f.reason.c_str(),(unsigned long long)g.batch,(unsigned long long)g.heads,(unsigned long long)g.queries,(unsigned long long)g.keys,g.memory_form.c_str(),g.source_effect.c_str());
    for(unsigned i=0;i<f.segments.size();++i) {const VHO_TENSOR_WORKLOAD_SEGMENT &s=f.segments[i];
        fprintf(file,"AI_Q2_SEGMENT 1 %s request=%llu active_Q=%lld history=%lld valid=%lld phase=%s reason=%s mask=%s update=%s\n",stage,(unsigned long long)s.request,(long long)s.queries,(long long)s.history,(long long)s.valid,s.phase.c_str(),s.reason.c_str(),s.mask_policy.c_str(),s.update.c_str());}
}
