/* Copyright (C) 2026 Open64 Project */
/* Active-PU realization of an already accepted closed MLA algorithm. Reuses the
 * existing selected staging generator and native WN_Lower ARRAY/SCF services.
 * No search, PU switching or mapped-image publication occurs in these phases.
 * Contract: doc/AI-COMPILER-SEARCH-M5-DEVICE-BRIDGE-v1.0.md. */
#include "tensor_selected_bridge.h"
#include "tensor_selected_key.h"
#include "pu_info.h"
#include "wn_util.h"
#include "wn_lower.h"
#include "symtab_verify.h"
#include <cstring>
#include <cstdlib>

namespace {
/* Refuse an unsupported/stale product before the next phase mutates it. */
void require(bool ok,const char *reason) {if(!ok) throw std::runtime_error(reason);}
/* Borrow only the driver-selected active native procedure. */
WN *entry() {require(Current_PU_Info!=NULL,"bridge_active_PU");return PU_Info_tree_ptr(Current_PU_Info);}
/* Read required fixed bindings without making a new scheduling decision. */
std::string meta(const char *key) {
    const char *v=ST_tensor_metadata(WN_st_idx(entry()),key);require(v!=NULL,"bridge_missing_binding");return v;
}
/* Bind only the active procedure through the existing metadata mechanism. */
void bind(const char *key,const std::string &v) {ST_tensor_bind_metadata(WN_st_idx(entry()),key,v.c_str());}
/* Borrow the full native level key without switching the active PU. */
std::string key() {return VHO_tensor_bridge_key(entry());}
/* Retire each previous level key; source/analysis generations never transfer. */
void seal(const char *level) {bind("tensor.bridge.level",level);bind("tensor.bridge.key",key());}
/* Find only the previously validated, canonical dim3 declarations. */
ST *coordinate(const char *name) {
    const ST *s;ST_IDX idx;FOREACH_SYMBOL(GLOBAL_SYMTAB,s,idx)
        if(!strcmp(ST_name(s),name)) return &St_Table[ST_st_idx(s)];
    throw std::runtime_error("bridge_missing_coordinate");
}
/* Substitute coordinate leaves only; every returned node has one tree owner. */
WN *ownership(WN *n,ST **owners,bool physical) {
    if(WN_operator(n)==OPR_LDID) for(unsigned i=0;i<2;++i) {
        ST *gpu=coordinate(i?"threadIdx":"blockIdx");ST *from=physical?owners[i]:gpu;
        if(WN_st_idx(n)==ST_st_idx(from)) {
            require(WN_offset(n)==0 && WN_desc(n)==MTYPE_U4 &&
                (WN_rtype(n)==MTYPE_U4 || WN_rtype(n)==MTYPE_U8),"bridge_coordinate_payload");
            ST *target=physical?gpu:owners[i];
            WN *r=WN_CreateLdid(OPCODE_make_op(OPR_LDID,WN_rtype(n),MTYPE_U4),0,
                ST_st_idx(target),ST_type(target),physical?1:0);
            return r;
        }
    }
    if(WN_operator(n)==OPR_BLOCK) {
        for(WN *s=WN_first(n);s;s=WN_next(s)) ownership(s,owners,physical);
    } else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) WN_kid(n,i)=ownership(WN_kid(n,i),owners,physical);
    return n;
}
/* Rebuild the entry/prototype with explicit owner inputs at High/Mid only. */
void interface(ST **owners,bool physical) {
    WN *old=entry();INT count=WN_num_formals(old)+(physical?-2:2);
    ST *function=WN_st(old);WN *next=WN_CreateEntry(count,function,WN_func_body(old),NULL,NULL);
    WN_Set_Linenum(next,WN_Get_Linenum(old));
    for(INT i=0;i<count;++i) WN_kid(next,i)=i<WN_num_formals(old)?WN_COPY_Tree(WN_formal(old,i)):WN_CreateIdname(0,owners[i-WN_num_formals(old)]);
    TY_IDX type;TY &t=New_TY(type);TY_Init(t,0,KIND_FUNCTION,MTYPE_UNKNOWN,0);Set_TY_has_prototype(type);
    TYLIST_IDX a;Set_TYLIST_type(New_TYLIST(a),MTYPE_To_TY(MTYPE_V));Set_TY_tylist(t,a);
    for(INT i=0;i<count;++i) Set_TYLIST_type(New_TYLIST(a),ST_type(WN_st(WN_formal(next,i))));
    Set_TYLIST_type(New_TYLIST(a),TY_IDX_ZERO);Set_PU_prototype(Pu_Table[ST_pu(function)],type);
    /* The existing setter admits declarations once. Refining this owned
     * declaration updates its unique existing attribute, never adds a second. */
    unsigned changed=0;SYMTAB_IDX level=ST_IDX_level(ST_st_idx(function));
    for(ST_ATTR_IDX i=1;i<ST_ATTR_Table_Size(level);++i) {
        ST_ATTR &attr=St_Attr_Table(level,i);
        if(ST_ATTR_st_idx(attr)==ST_st_idx(function) && ST_ATTR_kind(attr)==ST_ATTR_DEVICE_SYMBOL) {
            attr.Set_value(physical?DEVICE_SYMBOL_KERNEL:DEVICE_SYMBOL_FUNCTION);++changed;
        }
    }require(changed==1,"bridge_unique_device_attribute");
    Set_PU_Info_tree_ptr(Current_PU_Info,next);
}
/* Validate boundary vocabulary; exact keys additionally cover all payloads. */
void vocabulary(WN *n,bool high,bool physical) {
    OPERATOR op=WN_operator(n);
    require(op!=OPR_ARRAY || high,"bridge_ARRAY_after_High");
    require((op!=OPR_DO_LOOP && op!=OPR_IF && op!=OPR_WHILE_DO && op!=OPR_DO_WHILE) || high,"bridge_SCF_after_High");
    if(op==OPR_LDID && (!strcmp(ST_name(WN_st(n)),"threadIdx") || !strcmp(ST_name(WN_st(n)),"blockIdx")))
        require(physical,"bridge_physical_coordinate_before_Low");
    if(op==OPR_BLOCK) for(WN *s=WN_first(n);s;s=WN_next(s)) vocabulary(s,high,physical);
    else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) vocabulary(WN_kid(n,i),high,physical);
}
}

/* Adapt the established generator to an abstract worker ABI before publishing
 * High. Its temporary coordinate leaves are replaced, without changing any
 * arithmetic, work ownership, staging loop or collective selected by LNO.
 * Optional H1d device evidence is sealed in every worker boundary. */
void VHO_tensor_selected_worker(const VHO_TENSOR_PROGRAM &p,UINT32 id,SRCPOS source,const std::string &source_key,const std::string &source_comment,const std::string &device_key) {
    require(!p.auto_chunks && !p.search_selected,"bridge_cannot_search");
    require(VHO_tensor_materialize_kernel(p,id,source,stderr) && VHO_tensor_annotate_kernel(p,id,stderr) &&
        VHO_tensor_verify_kernel(p,id,stderr),"bridge_fixed_construction");
    ST *owners[2];const char *names[]={"selected_block_owner","selected_thread_owner"};
    for(unsigned i=0;i<2;++i) {
        owners[i]=New_ST(CURRENT_SYMTAB);ST_Init(owners[i],Save_Str(names[i]),CLASS_VAR,SCLASS_FORMAL,EXPORT_LOCAL,MTYPE_To_TY(MTYPE_U4));
        Set_ST_is_value_parm(owners[i]);Set_ST_Srcpos(*owners[i],source);
    }
    ownership(WN_func_body(entry()),owners,false);interface(owners,false);
    /* Preserve the accepted transformation decision as native COMMENT text,
     * alongside the more detailed existing per-kernel algorithm explanations. */
    require(!source_comment.empty() && source_comment.find("*/")==std::string::npos,"bridge_source_explanation");
    const char *texts[]={"Accepted M4 High source decision follows verbatim. Its host-view note describes that earlier checkpoint; the device realization is explained in the following per-kernel comments.",source_comment.c_str()};
    WN *body=WN_func_body(entry()),*first=WN_first(body);
    for(unsigned i=0;i<2;++i) {
        WN *comment=WN_CreateComment(texts[i]);WN_Set_Linenum(comment,source);Set_ST_Srcpos(*WN_st(comment),source);
        WN_INSERT_BlockBefore(body,first,comment);
    }
    std::ostringstream count;count<<strtoul(meta("tensor.kernel.explanations_count").c_str(),NULL,10)+2;
    bind("tensor.kernel.explanations_count",count.str());bind("tensor.bridge.source_comment_count","2");
    bind("tensor.kernel.body_hash","retired-by-selected-bridge-full-key");
    bind("tensor.bridge.schema","selected-mla-native-v1");
    bind("tensor.bridge.source_key",source_key);bind("tensor.bridge.fixed_recipe",VHO_tensor_recipe_key(p));
    if(!device_key.empty()) bind("tensor.bridge.device",device_key);
    seal("high");VHO_tensor_verify_bridge("high");
}
/* No mutation occurs in this admission; stale seals never reach WN_Lower/CG. */
void VHO_tensor_verify_bridge(const char *level) {
    require(meta("tensor.bridge.schema")=="selected-mla-native-v1","bridge_schema");
    require(meta("tensor.bridge.level")==level && meta("tensor.bridge.key")==key(),"bridge_stale_level_key");
    require(meta("tensor.bridge.source_comment_count")=="2","bridge_source_explanation_count");
    bool high=!strcmp(level,"high"),physical=!strcmp(level,"low");
    require(high || physical || !strcmp(level,"mid"),"bridge_unknown_level");
    vocabulary(entry(),high,physical);
    INT count=WN_num_formals(entry());require(count>(physical?0:2),"bridge_formals");
    require(ST_device_symbol_kind(*WN_st(entry()))==(physical?DEVICE_SYMBOL_KERNEL:DEVICE_SYMBOL_FUNCTION),"bridge_entry_kind");
    for(INT i=0;i<count;++i) require(TY_kind(ST_type(WN_st(WN_formal(entry(),i))))==
        ((!physical && i>=count-2)?KIND_SCALAR:KIND_POINTER),"bridge_formal_type");
    Verify_SYMTAB(CURRENT_SYMTAB);
    require(WN_verifier(entry()),"bridge_native_WN_verifier");
}
/* Standard ARRAY/SCF lowering owns address and control expansion. COMMENT
 * statements are retained; no PREOPT round is introduced here. */
void VHO_tensor_selected_memory() {
    VHO_tensor_verify_bridge("high");Lower_Init();
    WN_func_body(entry())=WN_Lower(WN_func_body(entry()),LOWER_ARRAY|LOWER_SCF,NULL,"selected MLA structured memory");
    Lowering_Finalize();
    seal("mid");VHO_tensor_verify_bridge("mid");
}
/* Only ownership/entry ABI changes; fixed staging, scopes and scratch survive. */
void VHO_tensor_selected_gpu() {
    VHO_tensor_verify_bridge("mid");INT count=WN_num_formals(entry());
    ST *owners[]={WN_st(WN_formal(entry(),count-2)),WN_st(WN_formal(entry(),count-1))};
    ownership(WN_func_body(entry()),owners,true);interface(owners,true);
    for(unsigned i=0;i<2;++i) {Clear_ST_is_value_parm(owners[i]);Set_ST_sclass(owners[i],SCLASS_AUTO);}
    seal("low");VHO_tensor_verify_bridge("low");
}
