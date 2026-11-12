/* Copyright (C) 2026 Open64 Project */
/* LNO-owned copied facts for the guarded Q1 common High semantic boundary.
 * Design: doc/AI-COMPILER-MLA-Q1-HIGH-OWNED-FACTS-v1.0.md. Does not run LNO
 * dependence analysis, infer serving phase or authorize reuse/transformation.
 * No JSON, frontend builder, profiler or candidate-materializer dependency. */
#include "dsl_high_quantized.h"
#include "wn.h"
#include "wn_util.h"
#include "stab.h"
#include "pu_info.h"
#include "config.h"
#include "config_opt.h"
#include "targ_const.h"
#include "dsl_native_tree_key.h"
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>

namespace {
/* Fail closed; all output publication follows complete read-only capture. */
void require(bool ok,const char *why) {if(!ok) throw std::runtime_error(why);}
/* Canonical decimal tree identities never include native pointer values. */
template<class T> std::string number(T n) {std::ostringstream s;s<<n;return s.str();}
}
class DSL_HIGH_QUANTIZED_CAPTURE {
    WN *_entry;
    const VHO_TENSOR_PROGRAM &_program;
    DSL_HIGH_QUANTIZED_FACTS _facts;
    std::map<ST_IDX,unsigned> _formals;
    std::map<unsigned,unsigned> _roots;
    std::map<std::string,unsigned> _values;
    std::map<unsigned,unsigned> _producers;
    std::set<unsigned> _groups;
    std::set<ST_IDX> _data_derived;
    std::vector<std::string> _loop_stack;
    std::map<unsigned,const VHO_TENSOR_OPERATION *> _definitions;
    unsigned _operation;
    bool _post;
    DSL_HIGH_Q1_RESOLVER _resolver;
    /* Stable syntactic expression key retains indirect loads and scalar
     * definitions. It is not an affine/alias or equality proof. */
    std::string Tree(WN *node) {
        return DSL_NATIVE_TREE_KEY().Build(node);
    }
    /* Locate the actual scale operand of the closed source encoder. Exact
     * encoder legality was verified at ingestion; this joins its SSA version. */
    bool Encoder_scale(unsigned value,unsigned scale,unsigned depth=0) {
        if(depth>=256 || !_definitions.count(value)) return false;
        const VHO_TENSOR_OPERATION &op=*_definitions[value];
        if(op.name=="common.div" && op.version==1 && op.inputs.size()==2 &&
           _program.values[op.inputs[1]].storage==scale) return true;
        if(op.name!="common.convert" && op.name!="common.clamp" && op.name!="common.round_even" &&
           op.name!="common.pack_u4" && op.name!="common.add" && op.name!="common.reshape") return false;
        for(unsigned i=0;i<op.inputs.size();++i) if(Encoder_scale(op.inputs[i],scale,depth+1)) return true;
        return false;
    }
    /* Require matching cache/scale storage lineage for paging and fresh
     * functional successors. Source names/types alone never prove a version. */
    void Pair(unsigned cache,unsigned scale,unsigned depth=0) {
        require(depth<256,"q1_scale_lineage_depth");
        cache=_program.values[cache].storage;scale=_program.values[scale].storage;
        const VHO_TENSOR_VALUE &c=_program.values[cache],&s=_program.values[scale];
        require(s.dtype=="float32" && c.shape.size()==s.shape.size() && s.shape.back()==1,"q1_scale_shape");
        for(unsigned i=0;i+1<c.shape.size();++i) require(c.shape[i]==s.shape[i],"q1_scale_extent");
        if(c.role==1 && s.role==1) return;
        const VHO_TENSOR_OPERATION *co=_definitions.count(cache)?_definitions[cache]:NULL;
        const VHO_TENSOR_OPERATION *so=_definitions.count(scale)?_definitions[scale]:NULL;
        if(Encoder_scale(cache,scale)) return;
        require(co && so,"q1_scale_version_unproved");
        require(co->name==so->name,"q1_scale_version_mismatch");
        if(co->name=="copy") {Pair(co->inputs[0],so->inputs[0],depth+1);return;}
        if(co->name=="common.reshape") {Pair(co->inputs[0],so->inputs[0],depth+1);return;}
        require(co->name=="common.gather" || co->name=="common.scatter","q1_scale_source_unproved");
        require(co->inputs[1]==so->inputs[1] && co->attributes==so->attributes,"q1_scale_index_version");
        Pair(co->inputs[0],so->inputs[0],depth+1);
        if(co->name=="common.scatter") Pair(co->inputs[2],so->inputs[2],depth+1);
        /* The local successor joins both updates, never an allocator epoch. */
    }
    /* Follow unique live DU definitions for a scalarized formal address. This
     * is identity recovery, not alias disambiguation or affine inference. */
    ST_IDX Base(WN *node,unsigned depth=0) {
        if(!node || depth>64) return ST_IDX_ZERO;
        OPERATOR op=WN_operator(node);
        if(op==OPR_LDID) {
            if(_formals.count(WN_st_idx(node))) return WN_st_idx(node);
            WN *def=_resolver.definition?_resolver.definition(_resolver.context,node):NULL;
            return def && WN_operator(def)==OPR_STID?Base(WN_kid0(def),depth+1):ST_IDX_ZERO;
        }
        if(op==OPR_ARRAY || op==OPR_CVT) return Base(WN_kid0(node),depth+1);
        if(op==OPR_ADD) {
            ST_IDX a=Base(WN_kid0(node),depth+1),b=Base(WN_kid1(node),depth+1);
            return a && !b?a:b && !a?b:ST_IDX_ZERO;
        }
        return ST_IDX_ZERO;
    }
    /* A load-derived scalar or conditional choice in an ARRAY coordinate
     * remains indirect even when its base formal is known. Never project it as an affine/cacheline access. */
    bool Indirect(WN *node) {
        if(WN_operator(node)==OPR_ILOAD || (WN_operator(node)==OPR_LDID &&
           (_data_derived.count(WN_st_idx(node)) || (_post && ST_class(WN_st(node))==CLASS_PREG)))) return true;
        for(INT i=0;i<WN_kid_count(node);++i) if(WN_kid(node,i) && Indirect(WN_kid(node,i))) return true;
        return false;
    }
    /* Query established Open64 canonical-loop accessors without normalization
     * or dependence allocation. Guarded memory remains a distinct Q1 form. */
    void Loop(WN *node,const std::string &id) {
        OPCODE comparison;BOOL increasing;
        WN *lo=WN_LOOP_LowerBound(node),*hi=WN_LOOP_UpperBound(node,&comparison);
        WN *step=WN_LOOP_Increment(node,&increasing);
        require(lo && hi && step && (WN_desc(WN_start(node))==MTYPE_I8 || (_post && WN_desc(WN_start(node))==MTYPE_I4)) &&
            OPCODE_desc(comparison)==WN_desc(WN_start(node)) &&
            WN_operator(lo)==OPR_INTCONST && WN_operator(hi)==OPR_INTCONST && WN_operator(step)==OPR_INTCONST &&
            WN_const_val(lo)==0 && WN_const_val(step)==1 && increasing &&
            (OPCODE_operator(comparison)==OPR_LT || (_post && OPCODE_operator(comparison)==OPR_LE)) &&
            WN_const_val(hi)>=(_post?0:1) && WN_const_val(hi)<=16777216,"q1_noncanonical_loop");
        DSL_HIGH_LOOP loop;loop.id=id;loop.parent=_loop_stack.empty()?"":_loop_stack.back();
        loop.induction=ST_name(WN_st(WN_index(node)));loop.comparison=OPCODE_name(comparison);
        loop.lower=0;loop.upper=WN_const_val(hi);loop.step=1;_facts._loops.push_back(loop);_loop_stack.push_back(id);
    }
    /* Record each actual memory operation with lexical lifetime endpoints.
     * Access order describes syntax, not a dynamic execution timestamp. */
    void Visit(WN *node,bool conditional,const std::string &path,bool data_control=false) {
        OPERATOR op=WN_operator(node);unsigned previous_operation=_operation;
        /* Preserve transitive data/control provenance across local temporaries.
         * Never clear within a branch/loop: conservative uncertainty is safer
         * than falsely treating a loaded page or chosen update as affine. */
        if(op==OPR_STID && (data_control || Indirect(WN_kid0(node)))) _data_derived.insert(WN_st_idx(node));
        bool dependent_condition=op==OPR_IF && Indirect(WN_kid0(node));
        if(op==OPR_DO_LOOP) {
            const char *tag=ST_tensor_metadata(WN_st_idx(WN_index(node)),"tensor.q1.operation");
            if(tag) {char *end;unsigned long n=strtoul(tag,&end,10);require(end!=tag && !*end && tag[0]>='0' && tag[0]<='9' && n<_program.operations.size() && number(n)==tag,"q1_operation_join");_operation=n;_groups.insert(n);}
            Loop(node,path);
        }
        if(op==OPR_STID) require(!_formals.count(WN_st_idx(node)),"q1_base_reassignment");
        if(op==OPR_COMMENT && !_post) {
            const char *text=Index_To_Str(WN_GetComment(node));unsigned n;
            if(sscanf(text,"Common High operation %u:",&n)==1) {
                require(n<_program.operations.size(),"q1_operation_group");_operation=n;_groups.insert(n);
            }
        }
        require(!DSL_WN_Is_Native(node) && op!=OPR_CALL && op!=OPR_ICALL,"q1_unlowered_or_call");
        if(op==OPR_ILOAD || op==OPR_ISTORE) {
            WN *addr=op==OPR_ILOAD?WN_kid0(node):WN_kid1(node);
            ST_IDX resolved=_post?Base(addr):ST_IDX_ZERO;
            if((_post && resolved && _formals[resolved]==_facts._storages.size()) ||
               (WN_operator(addr)==OPR_LDID && _formals.count(WN_st_idx(addr)) && _formals[WN_st_idx(addr)]==_facts._storages.size())) {
                require(op==OPR_ISTORE && WN_desc(node)==MTYPE_I4 && !WN_offset(node) &&
                    WN_operator(WN_kid0(node))==OPR_INTCONST &&
                    (WN_const_val(WN_kid0(node))==0 || WN_const_val(WN_kid0(node))==1),"q1_status_write");return;
            }
            if(!_post) require(WN_operator(addr)==OPR_ARRAY && WN_kid_count(addr)==3 && WN_operator(WN_kid0(addr))==OPR_LDID,"q1_opaque_address");
            ST_IDX base=_post?resolved:WN_st_idx(WN_kid0(addr));require(base && _formals.count(base),"q1_nonformal_address");
            unsigned formal=_formals[base];require(formal<_facts._storages.size() && (_post || _operation<_program.operations.size()),"q1_access_group");
            DSL_HIGH_QUANTIZED_STORAGE &storage=_facts._storages[formal];
            if(WN_operator(addr)==OPR_ARRAY) require(WN_operator(WN_kid1(addr))==OPR_INTCONST &&
                    WN_const_val(WN_kid1(addr))==(INT64)VHO_tensor_elements(_program.values[storage.source_root]),"q1_array_extent");
            TYPE_ID type=TY_mtype(TY_pointed(ST_type(St_Table[base])));
            require(WN_desc(node)==type && (WN_operator(addr)!=OPR_ARRAY || WN_element_size(addr)==MTYPE_byte_size(type)),"q1_memory_encoding");
            DSL_HIGH_QUANTIZED_ACCESS access;access.id=path;access.storage=storage.id;access.version=storage.version;
            access.group=_operation==~0U?"unknown-canonical-group":"operation:"+number(_operation);access.operation=_operation;access.loops=_loop_stack;
            access.order=_facts._accesses.size();access.write=op==OPR_ISTORE;
            access.indirect=WN_operator(addr)!=OPR_ARRAY || Indirect(WN_kid2(addr));access.conditional=conditional;
            access.element_bytes=MTYPE_byte_size(type);access.encoding=MTYPE_name(type);access.address_key=Tree(addr);
            // Executable identity alone omits ST operation metadata and live
            // DU edges. Seal the observed join as well, so a different group
            // or formal resolution cannot revalidate former owned facts.
            _facts._native_key+="|q1-access:"+access.id+":"+access.storage+":"+number(access.operation)+":"+
                number(access.indirect)+":"+number(access.conditional);
            if(!_post && !storage.input && !access.write) require(_producers[storage.source_root]<_operation,"q1_read_before_version");
            if(access.write) require(!storage.input && (_post || _producers[storage.source_root]==_operation),"q1_unexpected_write");
            if(storage.first_use.empty()) storage.first_use=access.id;storage.last_use=access.id;
            _facts._accesses.push_back(access);
        }
        if(op==OPR_BLOCK) {
            unsigned n=0;for(WN *s=WN_first(node);s;s=WN_next(s)) Visit(s,conditional,path+"/s"+number(n++),data_control);
        } else for(INT i=0;i<WN_kid_count(node);++i) if(WN_kid(node,i)) Visit(WN_kid(node,i),conditional || op==OPR_IF,path+"/k"+number(i),data_control || dependent_condition);
        if(op==OPR_DO_LOOP) {_loop_stack.pop_back();_operation=previous_operation;}
    }
public:
    /* Borrow only the active driver's PU and detached admitted source. */
    DSL_HIGH_QUANTIZED_CAPTURE(WN *entry,const VHO_TENSOR_PROGRAM &p,bool post=false,const DSL_HIGH_Q1_RESOLVER &resolver=DSL_HIGH_Q1_RESOLVER())
        : _entry(entry),_program(p),_operation(~0U),_post(post),_resolver(resolver) {}
    /* Capture formals, real memory nodes and paired local scale versions.
     * No allocator generation, inter-call lifetime or no-alias fact beyond
     * this explicit disjoint-buffer ABI is inferred. */
    DSL_HIGH_QUANTIZED_FACTS Capture(const DSL_HIGH_EPOCH &epoch) {
        require(!OPT_Fast_Math && Roundoff_Level==ROUNDOFF_NONE && IEEE_Arithmetic==IEEE_ACCURATE,"q1_numeric_context");
        require(Current_PU_Info && Current_pu==&PU_Info_pu(Current_PU_Info) &&
                CURRENT_SYMTAB==PU_lexical_level(*Current_pu) && PU_Info_tree_ptr(Current_PU_Info)==_entry &&
                WN_operator(_entry)==OPR_FUNC_ENTRY && VHO_tensor_validate_quantized_high(_program,NULL),"q1_scope_or_source");
        ST_IDX fn=WN_st_idx(_entry);require(fn==PU_Info_proc_sym(Current_PU_Info),"q1_function_scope");
        require(ST_tensor_metadata(fn,"tensor.high.schema") &&
                std::string(ST_tensor_metadata(fn,"tensor.high.schema"))=="quantized-common-high-v1" &&
                ST_tensor_metadata(fn,"tensor.high.buffer_contract") &&
                std::string(ST_tensor_metadata(fn,"tensor.high.buffer_contract"))=="disjoint-storage-formals","q1_producer_contract");
        _facts._source_key=VHO_tensor_quantized_high_key(_program);
        require(ST_tensor_metadata(fn,"tensor.high.recipe_key") &&
            _facts._source_key==ST_tensor_metadata(fn,"tensor.high.recipe_key"),"q1_producer_source_binding");
        for(unsigned i=0;i<_program.values.size();++i) {
            const VHO_TENSOR_VALUE &v=_program.values[i];require(_values.insert(std::make_pair(v.name,i)).second,"q1_duplicate_value");
        }
        for(unsigned i=0;i<_program.operations.size();++i) {
            const VHO_TENSOR_OPERATION &op=_program.operations[i];
            _definitions[op.output]=&op;
            if(op.name!="common.reshape") _producers[op.output]=i;
        }
        unsigned n=0;
        for(unsigned i=0;i<_program.values.size();++i) {
            const VHO_TENSOR_VALUE &v=_program.values[i];if(v.storage!=i) continue;
            require(n<(unsigned)WN_num_formals(_entry),"q1_formal_count");
            ST_IDX st=WN_st_idx(WN_formal(_entry,n));TY_IDX ty=ST_type(St_Table[st]);
            require(TY_kind(ty)==KIND_POINTER && TY_is_restrict(ty),"q1_alias_unknown");
            require(ST_tensor_metadata(st,"tensor.q1.value") && v.name==ST_tensor_metadata(st,"tensor.q1.value"),"q1_formal_binding");
            const char *keys[]={"tensor.q1.encoding","tensor.q1.scale","tensor.q1.evidence","tensor.cache.trait"};
            const std::string texts[]={v.cache_encoding,v.cache_scale,v.cache_evidence,v.cache_trait};
            for(unsigned k=0;k<4;++k) {const char *text=ST_tensor_metadata(st,keys[k]);require(texts[k]==(text?text:""),"q1_scale_or_trait_binding");}
            DSL_HIGH_QUANTIZED_STORAGE storage;storage.formal=n;storage.source_root=i;storage.id="formal:"+number(n);
            storage.value=v.name;storage.input=v.role==1;storage.output=v.role==2;
            storage.version=storage.id+(storage.input?"@entry":"@operation:"+number(_producers[i]));
            storage.encoding=v.cache_encoding;storage.role=v.cache_trait;
            storage.shape.assign(v.shape.begin(),v.shape.end());storage.strides.resize(v.shape.size());
            uint64_t elements=1;for(unsigned d=v.shape.size();d>0;--d) {storage.strides[d-1]=elements;elements*=v.shape[d-1];}
            TYPE_ID element=TY_mtype(TY_pointed(ty));
            TYPE_ID expected=v.dtype=="float32"?MTYPE_F4:v.dtype=="int64"?MTYPE_I8:v.dtype=="int8"?MTYPE_I1:MTYPE_U1;
            require(element==expected,"q1_formal_element_type");storage.payload_bytes=elements*MTYPE_byte_size(element);
            storage.lineage=storage.input?"entry":"fresh-semantic-result";
            _roots[i]=n;_formals[st]=n++;_facts._storages.push_back(storage);
        }
        require((unsigned)WN_num_formals(_entry)==n+1,"q1_status_formal");
        ST_IDX status=WN_st_idx(WN_formal(_entry,n));TY_IDX status_type=ST_type(St_Table[status]);
        require(TY_kind(status_type)==KIND_POINTER && TY_mtype(TY_pointed(status_type))==MTYPE_I4 && TY_is_restrict(status_type),"q1_status_type");
        _formals[status]=n;
        for(unsigned i=0;i<_facts._storages.size();++i) {
            DSL_HIGH_QUANTIZED_STORAGE &s=_facts._storages[i];const VHO_TENSOR_VALUE &v=_program.values[s.source_root];
            if(_definitions.count(s.source_root)) {
                const VHO_TENSOR_OPERATION &producer=*_definitions[s.source_root];
                if(producer.name=="common.scatter" || producer.name=="common.gather" || producer.name=="copy") {
                    const DSL_HIGH_QUANTIZED_STORAGE &previous=_facts._storages[_roots[_program.values[producer.inputs[0]].storage]];
                    s.origin_storage=previous.id;s.origin_version=previous.version;
                    s.lineage=producer.name=="common.scatter"?"functional-update":producer.name=="copy"?"returned-copy":"gather-copy";
                }
            }
            std::string scale_name=v.cache_scale;
            if(s.output && !v.cache_trait.empty() && _definitions.count(s.source_root)) {
                const VHO_TENSOR_OPERATION &copy=*_definitions[s.source_root];
                require(copy.name=="copy","q1_output_cache_copy");
                const VHO_TENSOR_VALUE &source=_program.values[copy.inputs[0]];
                require(!source.cache_scale.empty() && _values.count(source.cache_scale),"q1_output_source_scale");
                unsigned scale_root=_program.values[_values[source.cache_scale]].storage,matches=0;
                for(unsigned k=0;k<_program.operations.size();++k) {
                    const VHO_TENSOR_OPERATION &candidate=_program.operations[k];
                    if(candidate.name=="copy" && _program.values[candidate.output].role==2 &&
                       _program.values[candidate.inputs[0]].storage==scale_root) {
                        scale_name=_program.values[candidate.output].name;++matches;
                    }
                }
                require(matches==1,"q1_output_scale_unproved");
            }
            if(scale_name.empty()) continue;
            require(_values.count(scale_name),"q1_missing_scale");unsigned scale=_values[scale_name];
            if(!v.cache_encoding.empty()) Pair(s.source_root,scale);
            require(_program.values[scale].dtype=="float32" && _roots.count(_program.values[scale].storage),"q1_scale_type_or_storage");
            const DSL_HIGH_QUANTIZED_STORAGE &ss=_facts._storages[_roots[_program.values[scale].storage]];
            s.scale_storage=ss.id;s.scale_version=ss.version;
            unsigned ready=s.input?0:_producers[s.source_root];
            if(!ss.input && _producers[ss.source_root]>ready) ready=_producers[ss.source_root];
            s.scale_pair_ready_group=s.input && ss.input?"entry":"operation:"+number(ready);
        }
        // Q2 is a detached read-only observation, not an admission gate for Q1.
        // Unknown/unsupported workload evidence cannot change ordinary lowering.
        std::string workload_reason;
        if(!VHO_tensor_infer_workload(_program,NULL,_facts._workload,workload_reason))
            _facts._workload.reason=workload_reason;
        _facts._native_key=Tree(_entry);Visit(WN_func_body(_entry),false,"body");
        for(unsigned i=0;i<_program.operations.size();++i) if(!_post && _program.operations[i].name!="common.reshape")
            require(_groups.count(i),"q1_missing_operation_group");
        _facts._post_preopt=_post;_facts._stamp=epoch.Stamp();require(epoch.Matches(_facts._stamp),"q1_expired_epoch");_facts._valid=true;return _facts;
    }
};
/* Clear first, then publish complete owned facts. Native tables are read only. */
bool LNO_DSL_capture_quantized_high(WN *entry,const VHO_TENSOR_PROGRAM &p,const DSL_HIGH_EPOCH &epoch,
    DSL_HIGH_QUANTIZED_FACTS &out,std::string &why) {
    out=DSL_HIGH_QUANTIZED_FACTS();why.clear();
    try {DSL_HIGH_QUANTIZED_FACTS result=DSL_HIGH_QUANTIZED_CAPTURE(entry,p).Capture(epoch);out=result;return true;}
    catch(const std::exception &e) {why=e.what();return false;}
}
/* Reject expired epochs before looking at any former native context. */
bool LNO_DSL_revalidate_quantized_high(WN *entry,const VHO_TENSOR_PROGRAM &p,const DSL_HIGH_EPOCH &epoch,
    const DSL_HIGH_QUANTIZED_FACTS &facts,std::string &why) {
    if(!facts.Is_live(epoch)) {why="q1_stale_epoch";return false;}
    DSL_HIGH_QUANTIZED_FACTS fresh;
    if(!LNO_DSL_capture_quantized_high(entry,p,epoch,fresh,why)) return false;
    if(!facts.Same_content(fresh)) {why="q1_native_or_source_changed";return false;}return true;
}

/* New activation/generation plus actual canonical sites; no source WN is reused. */
bool LNO_DSL_rebind_quantized_high(WN *entry,const VHO_TENSOR_PROGRAM &p,const DSL_HIGH_EPOCH &epoch,
    const DSL_HIGH_Q1_RESOLVER &resolver,DSL_HIGH_QUANTIZED_FACTS &out,std::string &why) {
    out=DSL_HIGH_QUANTIZED_FACTS();why.clear();
    try {DSL_HIGH_QUANTIZED_FACTS fresh=DSL_HIGH_QUANTIZED_CAPTURE(entry,p,true,resolver).Capture(epoch);out=fresh;return true;}
    catch(const std::exception &e) {why=e.what();return false;}
}
/* Owned proof reports are safe after native analysis lifetime expiry. */
void LNO_DSL_print_quantized_high(FILE *file,const DSL_HIGH_QUANTIZED_FACTS &facts,const char *stage,bool live) {
    if(!file) return;
    VHO_tensor_print_workload(file,facts.Workload(),stage);
    unsigned indirect=0,unknown=0;
    for(unsigned i=0;i<facts.Accesses().size();++i) {indirect+=facts.Accesses()[i].indirect;unknown+=facts.Accesses()[i].operation==~0U;}
    fprintf(file,"AI_Q1_FACTS 1 %s live=%s post_preopt=%s storages=%lu loops=%lu accesses=%lu indirect=%u unknown_groups=%u search=false legality=not_proved\n",
        stage,live?"true":"false",facts.Post_preopt()?"true":"false",(unsigned long)facts.Storages().size(),(unsigned long)facts.Loops().size(),(unsigned long)facts.Accesses().size(),indirect,unknown);
    for(unsigned i=0;i<facts.Storages().size();++i) {
        const DSL_HIGH_QUANTIZED_STORAGE &s=facts.Storages()[i];
        fprintf(file,"AI_Q1_STORAGE 1 %s id=%s value=%s version=%s scale=%s scale_version=%s origin=%s role=%s encoding=%s first=%s last=%s lifetime=invocation_only\n",
            stage,s.id.c_str(),s.value.c_str(),s.version.c_str(),s.scale_storage.c_str(),s.scale_version.c_str(),s.origin_version.c_str(),s.role.c_str(),s.encoding.c_str(),s.first_use.c_str(),s.last_use.c_str());
    }
}

/* Fresh canonical equality, never equality against an expired source snapshot. */
bool LNO_DSL_revalidate_rebound_quantized_high(WN *entry,const VHO_TENSOR_PROGRAM &p,const DSL_HIGH_EPOCH &epoch,
    const DSL_HIGH_Q1_RESOLVER &resolver,const DSL_HIGH_QUANTIZED_FACTS &facts,std::string &why) {
    if(!facts.Post_preopt() || !facts.Is_live(epoch)) {why="q1_stale_rebound_epoch";return false;}
    DSL_HIGH_QUANTIZED_FACTS fresh;if(!LNO_DSL_rebind_quantized_high(entry,p,epoch,resolver,fresh,why)) return false;
    if(!facts.Same_content(fresh)) {why="q1_canonical_facts_changed";return false;}return true;
}
