/* Copyright (C) 2026 Open64 Project */
/* WOPT H6 selected ordered Reduction materializer. Driver-owned fresh PU only;
 * ordinary WHIRL loads, stores, loops and CTA barriers are the CUDA truth.
 * No source matcher, search or runtime lease discharge. No normalization math.
 * Design: doc/AI-COMPILER-H6-NORM-REDUCTION-v1.0.md. */
#include "opt_dsl_h6.h"
#include "dsl_native_tree_key.h"
#include "config_dsl.h"
#include "config.h"
#include "config_opt.h"
#include "pu_info.h"
#include "wn_util.h"
#include <fenv.h>
#include <sstream>
#if defined(__SSE__)
#include <xmmintrin.h>
#endif
namespace {
/* Validate the selected scalar FP environment, never silently change it. */
bool H6_strict() {
    if(Pointer_Size!=8 || Pointer_Mtype!=MTYPE_U8 || OPT_Fast_Math || Roundoff_Level!=ROUNDOFF_NONE || IEEE_Arithmetic!=IEEE_ACCURATE || fegetround()!=FE_TONEAREST) return false;
#if defined(__SSE__)
    unsigned control=_mm_getcsr();if((control&((1U<<15)|(1U<<6)|0x6000U)) || (control&0x1f80U)!=0x1f80U) return false;
#endif
    return true;
}
/* Numeric seal binds full owned fields, not native pointers or table indices. */
UINT64 H6_hash(UINT64 h,const void *bytes,size_t count) {const unsigned char *p=(const unsigned char *)bytes;for(size_t i=0;i<count;++i) h=(h^p[i])*1099511628211ULL;return h;}
/* Fresh typed local with the complete driver-supplied source position. */
ST *H6_local(const char *name,TY_IDX type,SRCPOS source) {ST *s=New_ST(CURRENT_SYMTAB);ST_Init(s,Save_Str(name),CLASS_VAR,SCLASS_AUTO,EXPORT_LOCAL,type);Set_ST_Srcpos(*s,source);return s;}
/* Every expression has one WHIRL tree owner. */
WN *H6_load(ST *s) {return WN_Ldid(TY_mtype(ST_type(s)),0,s,ST_type(s));}
/* Consume one expression in an ordinary typed local store. */
WN *H6_store(ST *s,WN *v) {return WN_Stid(TY_mtype(ST_type(s)),0,s,ST_type(s),v);}
/* Widened unsigned coordinates preserve legal row and byte domains. */
WN *H6_int(UINT64 n) {return WN_Intconst(MTYPE_U8,n);}
/* Global/subrange arguments or shared-array objects use explicit byte offsets. */
WN *H6_address(ST *s,WN *index) {return WN_Add(MTYPE_U8,ST_sclass(s)==SCLASS_FORMAL?H6_load(s):WN_Lda(MTYPE_U8,0,s),WN_Mpy(MTYPE_U8,index,H6_int(4)));}
/* Read one valid F32 value; callers place guards before constructing access. */
WN *H6_read(ST *s,WN *index) {return WN_Iload(MTYPE_F4,0,MTYPE_To_TY(MTYPE_F4),H6_address(s,index));}
/* Store one F32 result to an already admitted disjoint owner. */
WN *H6_write(ST *s,WN *index,WN *value) {return WN_Istore(MTYPE_F4,0,Make_Pointer_Type(MTYPE_To_TY(MTYPE_F4)),H6_address(s,index),value);}
/* Fixed counted loops are the source of ordered iteration, not emitter policy. */
WN *H6_loop(ST *iv,WN *begin,WN *end,UINT64 step,WN *body) {return WN_CreateDO(WN_CreateIdname(0,iv),H6_store(iv,begin),WN_LT(MTYPE_U8,H6_load(iv),end),H6_store(iv,WN_Add(MTYPE_U8,H6_load(iv),H6_int(step))),body,NULL);}
/* Borrow only canonical builtin declarations; collision refusal precedes all
 * mutation. Fresh declarations use the same established dim3/void ABI. */
bool H6_builtins_valid() {
    const ST *s;ST_IDX idx;FOREACH_SYMBOL(GLOBAL_SYMTAB,s,idx) {
        if(!strcmp(ST_name(s),"blockIdx") || !strcmp(ST_name(s),"threadIdx")) {
            if(ST_class(s)!=CLASS_VAR || ST_sclass(s)!=SCLASS_EXTERN || TY_kind(ST_type(s))!=KIND_STRUCT || TY_size(ST_type(s))!=12) return false;
            const char *names[]={"x","y","z"};FLD_HANDLE f=TY_fld(ST_type(s));for(unsigned i=0;i<3;++i) {if(f.Is_Null() || strcmp(FLD_name(f),names[i]) || FLD_ofst(f)!=4*i || FLD_type(f)!=MTYPE_To_TY(MTYPE_U4) || (i==2)!=!!FLD_last_field(f)) return false;if(i<2) f=FLD_next(f);}
        }
        if(!strcmp(ST_name(s),"__syncthreads") && (ST_class(s)!=CLASS_FUNC || TY_kind(ST_pu_type(s))!=KIND_FUNCTION || !TY_tylist(ST_pu_type(s)) || TYLIST_type(Tylist_Table[TY_tylist(ST_pu_type(s))])!=MTYPE_To_TY(MTYPE_V) || TYLIST_type(Tylist_Table[TY_tylist(ST_pu_type(s))+1]))) return false;
    }return true;
}
/* Create or borrow a coordinate declaration in the active global table only. */
ST *H6_coordinate(const char *name,SRCPOS source) {
    const ST *old;ST_IDX idx;FOREACH_SYMBOL(GLOBAL_SYMTAB,old,idx) if(!strcmp(ST_name(old),name)) return &St_Table[idx];
    TY_IDX ty;TY &t=New_TY(ty);TY_Init(t,12,KIND_STRUCT,MTYPE_M,Save_Str("h6_dim3"));Set_TY_align(ty,4);
    const char *names[]={"x","y","z"};for(unsigned i=0;i<3;++i) {FLD_HANDLE f=New_FLD();if(!i) Set_TY_fld(t,f);FLD_Init(f,Save_Str(names[i]),MTYPE_To_TY(MTYPE_U4),4*i);if(i==2) Set_FLD_last_field(f);}
    ST *s=New_ST(GLOBAL_SYMTAB);ST_Init(s,Save_Str(name),CLASS_VAR,SCLASS_EXTERN,EXPORT_PREEMPTIBLE,ty);Set_ST_Srcpos(*s,source);return s;
}
/* Read the existing x-field with an explicit U4-to-U8 conversion. */
WN *H6_coord(ST *s) {return WN_Cvt(MTYPE_U4,MTYPE_U8,WN_Ldid(MTYPE_U4,0,ST_st_idx(s),ST_type(s),1));}
/* Full CTA barrier is outside owner/element predicates. */
ST *H6_barrier(SRCPOS source) {
    const ST *old;ST_IDX idx;FOREACH_SYMBOL(GLOBAL_SYMTAB,old,idx) if(!strcmp(ST_name(old),"__syncthreads")) return &St_Table[idx];
    TY_IDX ty=Make_Function_Type(MTYPE_To_TY(MTYPE_V));PU_IDX pi;PU &p=New_PU(pi);PU_Init(p,ty,GLOBAL_SYMTAB+1);Set_PU_c_lang(p);
    ST *s=New_ST(GLOBAL_SYMTAB);ST_Init(s,Save_Str("__syncthreads"),CLASS_FUNC,SCLASS_EXTERN,EXPORT_PREEMPTIBLE,ty);Set_ST_pu(s,pi);Set_ST_Srcpos(*s,source);return s;
}
/* Map statements to real source coordinates; never fabricate expression fields. */
void H6_position(WN *n,SRCPOS source) {if(OPCODE_has_next_prev(WN_opcode(n)) || WN_operator(n)==OPR_FUNC_ENTRY) WN_Set_Linenum(n,source);if(WN_operator(n)==OPR_BLOCK) {for(WN *s=WN_first(n);s;s=WN_next(s)) H6_position(s,source);}else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) H6_position(WN_kid(n,i),source);}
}
/* Full field equality survives native-context cleanup; seal is not permission. */
UINT64 WOPT_DSL_h6_reduction_seal(const WOPT_DSL_H6_REDUCTION &p) {
    UINT64 h=H6_hash(1469598103934665603ULL,&p.source_guard,sizeof(p.source_guard));
    const UINT64 words[]={p.rows,p.width,p.threads,p.chunk,p.kind};h=H6_hash(h,words,sizeof(words));
    const std::string fields[]={p.device.Cache_key(),p.model,p.binding};for(unsigned i=0;i<3;++i) {size_t n=fields[i].size();h=H6_hash(h,&n,sizeof(n));h=H6_hash(h,fields[i].data(),n);}return h;
}
/* All recoverable refusals precede symbol allocation. Ascending shared reads
 * preserve the exact source F32 left fold; only the load ownership changes. */
BOOL WOPT_DSL_materialize_h6_reduction(const WOPT_DSL_H6_REDUCTION &p,SRCPOS source,std::string &key) {
    if(!Current_PU_Info || !Current_pu || !H6_strict() || !source || !SRCPOS_filenum(source) || !SRCPOS_linenum(source) ||
        !VHO_DSL_H3_Search || p.model!=VHO_DSL_H3_Search->Model().key || !p.device.Is_current(VHO_DSL_GPU_Context) ||
        p.seal!=WOPT_DSL_h6_reduction_seal(p) || !p.rows || p.rows>0xffffffffULL || !p.width || p.width>16777216 ||
        (p.kind!=1 && p.kind!=2) || (p.threads!=32 && p.threads!=64 && p.threads!=128) ||
        (p.kind==2 && (p.chunk!=p.threads || (p.threads!=32 && p.threads!=128))) || !H6_builtins_valid() ||
        !p.device.Fits_CTA(p.threads,WOPT_DSL_H6_REDUCTION_REGISTERS,p.kind==2?4*p.chunk:0)) return FALSE;
    WN *old=PU_Info_tree_ptr(Current_PU_Info);ST *fn=&St_Table[PU_Info_proc_sym(Current_PU_Info)];
    if(!old || CURRENT_SYMTAB<=GLOBAL_SYMTAB || Scope_tab[CURRENT_SYMTAB].st!=fn || WN_num_formals(old) || WN_first(WN_func_body(old)) || ST_device_symbol_kind(*fn)!=DEVICE_SYMBOL_KERNEL) return FALSE;
    TY_IDX pointer=Make_Pointer_Type(MTYPE_To_TY(MTYPE_F4)),ty;TY &t=New_TY(ty);TY_Init(t,0,KIND_FUNCTION,MTYPE_UNKNOWN,0);Set_TY_has_prototype(ty);TYLIST_IDX list;
    Set_TYLIST_type(New_TYLIST(list),MTYPE_To_TY(MTYPE_V));Set_TY_tylist(t,list);for(unsigned i=0;i<2;++i) Set_TYLIST_type(New_TYLIST(list),pointer);Set_TYLIST_type(New_TYLIST(list),TY_IDX_ZERO);Set_PU_prototype(Pu_Table[ST_pu(fn)],ty);
    ST *args[2];const char *names[]={"h6_input","h6_output"};for(unsigned i=0;i<2;++i) {args[i]=New_ST(CURRENT_SYMTAB);ST_Init(args[i],Save_Str(names[i]),CLASS_VAR,SCLASS_FORMAL,EXPORT_LOCAL,pointer);Set_ST_is_value_parm(args[i]);Set_ST_Srcpos(*args[i],source);}
    ST *tid=H6_local("h6_tid",MTYPE_To_TY(MTYPE_U8),source),*row=H6_local("h6_row",MTYPE_To_TY(MTYPE_U8),source),*k=H6_local("h6_k",MTYPE_To_TY(MTYPE_U8),source),*tile=H6_local("h6_tile",MTYPE_To_TY(MTYPE_U8),source),*sum=H6_local("h6_sum",MTYPE_To_TY(MTYPE_F4),source);
    WN *body=WN_CreateBlock();std::ostringstream comment;comment<<"H6 ordered F32 Reduction: rows="<<p.rows<<", width="<<p.width<<", threads="<<p.threads<<", chunk="<<p.chunk<<". "
        <<(p.kind==1?"Each thread owns one guarded row and its ascending F32 accumulator. ":"One CTA owns one row. Threads cooperatively copy contiguous active columns into a bounded shared panel, publish with an unconditional CTA barrier, then lane zero folds the panel in ascending order. All threads release via a second CTA barrier before refill, including tail/inactive lanes. ")
        <<"One accumulator spans every chunk; no parallel sum, reassociation, atomics, global partials or padded input loads. Source identity +0 and NaN-class policy remain. H2 allocation/version/lifetime and pinned H20/H3 selected binding were revalidated. Sector costs are declared L1-miss envelopes, not measured bandwidth or speedup. Host must enforce live disjoint buffers, completion/publication and this exact H6 launch; the old row-thread guard alone does not discharge a cooperative launch.";
    WN_INSERT_BlockLast(body,WN_CreateComment(comment.str().c_str()));
    WN_INSERT_BlockLast(body,H6_store(tid,H6_coord(H6_coordinate("threadIdx",source))));
    WN *block=H6_coord(H6_coordinate("blockIdx",source));WN_INSERT_BlockLast(body,H6_store(row,p.kind==1?WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,block,H6_int(p.threads)),H6_load(tid)):block));
    WN_INSERT_BlockLast(body,H6_store(sum,WN_Floatconst(MTYPE_F4,0.0)));
    if(p.kind==1) {
        WN *fold=WN_CreateBlock();WN *index=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,H6_load(row),H6_int(p.width)),H6_load(k));
        WN_INSERT_BlockLast(fold,H6_store(sum,WN_Add(MTYPE_F4,H6_load(sum),H6_read(args[0],index))));
        WN *valid=WN_CreateBlock();WN_INSERT_BlockLast(valid,H6_loop(k,H6_int(0),H6_int(p.width),1,fold));WN_INSERT_BlockLast(valid,H6_write(args[1],H6_load(row),H6_load(sum)));
        WN_INSERT_BlockLast(body,WN_CreateIf(WN_LT(MTYPE_U8,H6_load(row),H6_int(p.rows)),valid,WN_CreateBlock()));
    } else {
        TY_IDX array=Make_Array_Type(MTYPE_F4,1,p.chunk);Set_TY_align(array,16);Set_TY_is_user_align(array);ST *panel=H6_local("h6_panel",array,source);Set_ST_device_symbol_kind(*panel,DEVICE_SYMBOL_SHARED);ST *barrier=H6_barrier(source);
        WN *slab=WN_CreateBlock(),*copy=WN_CreateBlock();WN *index=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,H6_load(row),H6_int(p.width)),WN_Add(MTYPE_U8,H6_load(tile),H6_load(tid)));
        WN_INSERT_BlockLast(copy,H6_write(panel,H6_load(tid),H6_read(args[0],index)));
        WN_INSERT_BlockLast(slab,WN_CreateIf(WN_LT(MTYPE_U8,WN_Add(MTYPE_U8,H6_load(tile),H6_load(tid)),H6_int(p.width)),copy,WN_CreateBlock()));WN_INSERT_BlockLast(slab,WN_Call(MTYPE_V,MTYPE_V,0,ST_st_idx(barrier)));
        WN *fold=WN_CreateBlock();WN_INSERT_BlockLast(fold,H6_store(sum,WN_Add(MTYPE_F4,H6_load(sum),H6_read(panel,H6_load(k)))));
        WN *owner=WN_CreateBlock();WN *remaining=WN_Sub(MTYPE_U8,H6_int(p.width),H6_load(tile));WN *count=WN_Select(MTYPE_U8,WN_LT(MTYPE_U8,WN_COPY_Tree(remaining),H6_int(p.chunk)),remaining,H6_int(p.chunk));
        WN_INSERT_BlockLast(owner,H6_loop(k,H6_int(0),count,1,fold));WN_INSERT_BlockLast(slab,WN_CreateIf(WN_EQ(MTYPE_U8,H6_load(tid),H6_int(0)),owner,WN_CreateBlock()));WN_INSERT_BlockLast(slab,WN_Call(MTYPE_V,MTYPE_V,0,ST_st_idx(barrier)));
        WN_INSERT_BlockLast(body,H6_loop(tile,H6_int(0),H6_int(p.width),p.chunk,slab));WN *write=WN_CreateBlock();WN_INSERT_BlockLast(write,H6_write(args[1],H6_load(row),H6_load(sum)));WN_INSERT_BlockLast(body,WN_CreateIf(WN_EQ(MTYPE_U8,H6_load(tid),H6_int(0)),write,WN_CreateBlock()));
    }
    WN_INSERT_BlockLast(body,WN_CreateReturn());WN *entry=WN_CreateEntry(2,fn,body,NULL,NULL);for(unsigned i=0;i<2;++i) WN_kid(entry,i)=WN_CreateIdname(0,args[i]);H6_position(entry,source);Set_PU_Info_tree_ptr(Current_PU_Info,entry);
    std::ostringstream grid,threads,shared;grid<<(p.kind==1?(p.rows+p.threads-1)/p.threads:p.rows);threads<<p.threads;shared<<(p.kind==2?4*p.chunk:0);
    ST_tensor_bind_metadata(ST_st_idx(fn),"tensor.kernel.grid_x",grid.str().c_str());ST_tensor_bind_metadata(ST_st_idx(fn),"tensor.kernel.block_x",threads.str().c_str());ST_tensor_bind_metadata(ST_st_idx(fn),"tensor.kernel.shared_bytes",shared.str().c_str());
    key=WOPT_DSL_h6_reduction_native_key(entry);Is_True(!key.empty(),("H6 selected native identity missing"));return TRUE;
}

/* Read-only complete body/declaration identity plus launch metadata. Full text
 * equality detects stale native bodies even after ordinary mapped reopening. */
std::string WOPT_DSL_h6_reduction_native_key(WN *entry)
{
    if(!entry || WN_operator(entry)!=OPR_FUNC_ENTRY) return "";
    try {
        std::ostringstream out;out<<DSL_NATIVE_TREE_KEY().Build(entry);
        const char *keys[]={"tensor.kernel.grid_x","tensor.kernel.block_x","tensor.kernel.shared_bytes"};
        for(unsigned i=0;i<3;++i) {
            const char *value=ST_tensor_metadata(WN_st_idx(entry),keys[i]);
            if(!value) return "";out<<strlen(value)<<':'<<value<<';';
        }
        return out.str();
    } catch(const std::exception &) {return "";}
}
/* The selected owner supplies the exact retained key, not a diagnostic tag or
 * lossy tree hash. This verifies identity; source legality was prepared above. */
BOOL WOPT_DSL_verify_h6_reduction(WN *entry,const std::string &expected)
{
    return !expected.empty() && WOPT_DSL_h6_reduction_native_key(entry)==expected;
}
