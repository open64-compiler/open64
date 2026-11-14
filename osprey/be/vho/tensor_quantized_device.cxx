/* Copyright (C) 2026 Open64 Project */
/* Selected Q6b tree -> three explicitly ordered GPU entries. This active-PU
 * phase owns all partition, placement and synchronization choices. The driver
 * invokes it separately for each stage; no service switches PU contexts. */
#include "tensor_quantized_device.h"
#include "pu_info.h"
#include "wn_util.h"
#include "wn_lower.h"
#include "symtab_verify.h"
#include <map>
namespace {
void Qneed(bool b,const char *s) {if(!b) throw std::runtime_error(s);}
WN *Root() {Qneed(Current_PU_Info!=NULL,"q6c_active_PU");return PU_Info_tree_ptr(Current_PU_Info);}
void Bind(const char *k,const std::string &v) {ST_tensor_bind_metadata(WN_st_idx(Root()),k,v.c_str());}
std::string Num(UINT64 n) {std::ostringstream s;s<<n;return s.str();}
WN *Int(INT64 n) {return WN_Intconst(MTYPE_I8,n);}
WN *Load(ST *s) {return WN_Ldid(TY_mtype(ST_type(s)),0,s,ST_type(s));}
WN *Store(ST *s,WN *v) {return WN_Stid(TY_mtype(ST_type(s)),0,s,ST_type(s),v);}
ST *Local(const char *name,TYPE_ID type) {
    ST *s=New_ST(CURRENT_SYMTAB);ST_Init(s,Save_Str(name),CLASS_VAR,SCLASS_AUTO,EXPORT_LOCAL,MTYPE_To_TY(type));return s;
}
/* Exact producer-only regions never contain CTA collectives. */
WN *Leader(WN *body,ST *thread) {return WN_CreateIf(WN_EQ(MTYPE_U4,Load(thread),WN_Intconst(MTYPE_U4,0)),body,WN_CreateBlock());}
WN *Block(WN *s) {WN *b=WN_CreateBlock();WN_INSERT_BlockLast(b,s);return b;}
bool Has(WN *n,const char *name) {
    if(WN_operator(n)==OPR_CALL && !strcmp(ST_name(WN_st(n)),name)) return true;
    if(WN_operator(n)==OPR_BLOCK) {for(WN *s=WN_first(n);s;s=WN_next(s)) if(Has(s,name)) return true;}
    else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i) && Has(WN_kid(n,i),name)) return true;
    return false;
}
/* The checked source seal fixes the tree. Also require a unique top-level
 * three-loop nest; names alone are never transformation evidence. */
WN *Only_loop(WN *b) {
    WN *result=NULL;for(WN *s=WN_first(b);s;s=WN_next(s)) {
        Qneed(WN_operator(s)==OPR_DO_LOOP && !result,"q6c_outer_nest");result=s;
    }Qneed(result!=NULL,"q6c_missing_outer_loop");return result;
}
/* Private concrete instruction declarations. No high-level staging helper may
 * escape. Header/CG consumers spell these exact effects without choosing policy. */
class Mapper {
    VHO_Q6_MAPPING m;ST *owner[2],*axis[6],*phase[2];std::map<std::string,ST *> operations;
    WN *Copy(WN *n) {return WN_COPY_Tree(n);}
    WN *Argument(WN *n,unsigned i) {Qneed(i<(unsigned)WN_kid_count(n) && WN_operator(WN_kid(n,i))==OPR_PARM,"q6c_stage_argument");return Copy(WN_kid0(WN_kid(n,i)));}
    WN *Call(const char *suffix,std::vector<WN *> a,const char *kinds) {
        std::string name=std::string("__open64_q6c_")+suffix;std::vector<TY_IDX> types;
        for(unsigned i=0;i<a.size();++i) types.push_back(kinds[i]=='p'?Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)):MTYPE_To_TY(MTYPE_U4));
        ST *s=NULL;
        if(operations.count(name)) s=operations[name];
        else {
            const ST *old;ST_IDX index;FOREACH_SYMBOL(GLOBAL_SYMTAB,old,index) Qneed(name!=ST_name(old),"q6c_primitive_collision");
            TY_IDX ty;TY &t=New_TY(ty);TY_Init(t,0,KIND_FUNCTION,MTYPE_UNKNOWN,0);Set_TY_has_prototype(ty);TYLIST_IDX l;
            Set_TYLIST_type(New_TYLIST(l),MTYPE_To_TY(MTYPE_V));Set_TY_tylist(t,l);
            for(unsigned i=0;i<types.size();++i) Set_TYLIST_type(New_TYLIST(l),types[i]);Set_TYLIST_type(New_TYLIST(l),TY_IDX_ZERO);
            PU_IDX p;PU &pu=New_PU(p);PU_Init(pu,ty,GLOBAL_SYMTAB+1);Set_PU_c_lang(pu);
            s=New_ST(GLOBAL_SYMTAB);ST_Init(s,Save_Str(name.c_str()),CLASS_FUNC,SCLASS_EXTERN,EXPORT_PREEMPTIBLE,ty);Set_ST_pu(s,p);
            Set_ST_device_symbol_kind(*s,DEVICE_SYMBOL_FUNCTION);ST_tensor_bind_metadata(ST_st_idx(s),"tensor.q6c.primitive","sm90-byte-ops-v1");operations[name]=s;
        }
        WN *c=WN_Call(MTYPE_V,MTYPE_V,a.size(),ST_st_idx(s));WN_Set_Call_Default_Flags(c);
        for(unsigned i=0;i<a.size();++i) WN_kid(c,i)=WN_CreateParm(kinds[i]=='p'?Pointer_Mtype:MTYPE_U4,a[i],types[i],WN_PARM_BY_VALUE);return c;
    }
    WN *Op(const char *name) {return Call(name,std::vector<WN *>(),"");}
    WN *One(const char *name,WN *a) {std::vector<WN *> x;x.push_back(a);return Call(name,x,"p");}
    WN *Two(const char *name,WN *a,WN *b) {std::vector<WN *> x;x.push_back(a);x.push_back(b);return Call(name,x,"pu");}
    /* Exact byte loops cover every synchronous fragment with no overread. */
    WN *Byte_copy(WN *destination,WN *source,WN *begin,WN *end) {
        ST *i=Local("q6c_byte",MTYPE_I8);WN *b=WN_CreateBlock();TY_IDX byte=MTYPE_To_TY(MTYPE_U1);
        WN *src=WN_Add(Pointer_Mtype,source,WN_Cvt(MTYPE_I8,Pointer_Mtype,Load(i)));
        WN *dst=WN_Add(Pointer_Mtype,destination,WN_Cvt(MTYPE_I8,Pointer_Mtype,Load(i)));
        WN_INSERT_BlockLast(b,WN_Istore(MTYPE_U1,0,Make_Pointer_Type(byte),dst,WN_Iload(MTYPE_U1,0,byte,src)));
        return WN_CreateDO(WN_CreateIdname(0,i),Store(i,begin),WN_LT(MTYPE_I8,Load(i),end),Store(i,WN_Add(MTYPE_I8,Load(i),Int(1))),b,NULL);
    }
    /* Partition using actual runtime address residues. Equal residues admit a
     * head, 16-byte bulk middle and tail; unequal residues copy synchronously.
     * expect_tx counts only the emitted bulk middle, before its issue. */
    WN *Transfer(WN *n) {
        WN *b=WN_CreateBlock();ST *dst=Local("q6c_destination",MTYPE_U8),*src=Local("q6c_source",MTYPE_U8),*len=Local("q6c_length",MTYPE_I8);
        WN_INSERT_BlockLast(b,Store(dst,Argument(n,1)));WN_INSERT_BlockLast(b,Store(src,Argument(n,2)));
        WN_INSERT_BlockLast(b,Store(len,WN_Cvt(MTYPE_U8,MTYPE_I8,Argument(n,3))));
        if(m.slots==1) WN_INSERT_BlockLast(b,Byte_copy(Load(dst),Load(src),Int(0),Load(len)));
        else {
            ST *head=Local("q6c_head_bytes",MTYPE_I8),*bulk=Local("q6c_bulk_bytes",MTYPE_I8);
            WN *residue=WN_Band(MTYPE_U8,Load(src),WN_Intconst(MTYPE_U8,15));
            WN *h=WN_Cvt(MTYPE_U8,MTYPE_I8,WN_Band(MTYPE_U8,WN_Sub(MTYPE_U8,WN_Intconst(MTYPE_U8,16),residue),WN_Intconst(MTYPE_U8,15)));
            WN_INSERT_BlockLast(b,Store(head,WN_Select(MTYPE_I8,WN_LT(MTYPE_I8,Load(len),Copy(h)),Load(len),h)));
            WN *aligned=WN_EQ(MTYPE_U8,WN_Band(MTYPE_U8,Load(src),WN_Intconst(MTYPE_U8,15)),WN_Band(MTYPE_U8,Load(dst),WN_Intconst(MTYPE_U8,15)));
            WN *middle=WN_Mpy(MTYPE_I8,WN_Div(MTYPE_I8,WN_Sub(MTYPE_I8,Load(len),Load(head)),Int(16)),Int(16));
            WN_INSERT_BlockLast(b,Store(bulk,WN_Select(MTYPE_I8,aligned,middle,Int(0))));
            WN_INSERT_BlockLast(b,Byte_copy(Load(dst),Load(src),Int(0),Load(head)));
            WN *issue=WN_CreateBlock();WN_INSERT_BlockLast(issue,Two("expect",Argument(n,0),WN_Cvt(MTYPE_I8,MTYPE_U4,Load(bulk))));
            std::vector<WN *> args;args.push_back(WN_Add(MTYPE_U8,Load(dst),WN_Cvt(MTYPE_I8,MTYPE_U8,Load(head))));
            args.push_back(WN_Add(MTYPE_U8,Load(src),WN_Cvt(MTYPE_I8,MTYPE_U8,Load(head))));args.push_back(WN_Cvt(MTYPE_I8,MTYPE_U4,Load(bulk)));args.push_back(Argument(n,0));
            WN_INSERT_BlockLast(issue,Call("bulk",args,"ppup"));WN_INSERT_BlockLast(b,WN_CreateIf(WN_GT(MTYPE_I8,Load(bulk),Int(0)),issue,WN_CreateBlock()));
            WN_INSERT_BlockLast(b,Byte_copy(Load(dst),Load(src),WN_Add(MTYPE_I8,Load(head),Load(bulk)),Load(len)));
        }return Leader(b,owner[1]);
    }
    /* Phase registers are producer-local and persist across all three passes.
     * Each slot toggles only after a completed acquire wait, including zero-
     * bulk tiles. Unused initialized slots can be invalidated without a wait. */
    WN *Slot_wait(WN *n) {
        WN *b=WN_CreateBlock();WN_INSERT_BlockLast(b,One("arrive",Argument(n,0)));
        for(unsigned i=0;i<2;++i) {
            WN *body=WN_CreateBlock();WN_INSERT_BlockLast(body,Two("wait",Argument(n,0),Load(phase[i])));
            WN_INSERT_BlockLast(body,Store(phase[i],WN_Sub(MTYPE_U4,WN_Intconst(MTYPE_U4,1),Load(phase[i]))));
            /* Q6 identity = arena + barrier_offset + 8*slot. */
            ST *arena=NULL;const ST *s;ST_IDX idx;FOREACH_SYMBOL(CURRENT_SYMTAB,s,idx)
                if(ST_device_symbol_kind(*s)==DEVICE_SYMBOL_SHARED) {Qneed(!arena,"q6c_shared_inventory");arena=&St_Table[ST_st_idx(s)];}
            Qneed(arena!=NULL,"q6c_missing_arena");
            WN *id=WN_Add(MTYPE_U8,WN_Lda(Pointer_Mtype,0,arena),WN_Intconst(MTYPE_U8,m.barrier_offset+8*i));
            WN_INSERT_BlockLast(b,WN_CreateIf(WN_EQ(MTYPE_U8,Argument(n,0),id),body,WN_CreateBlock()));
        }
        WN *all=WN_CreateBlock();WN_INSERT_BlockLast(all,Leader(b,owner[1]));WN_INSERT_BlockLast(all,Op("cta_sync"));return all;
    }
    /* Replace abstract staging statements. Publication and consumer barriers
     * stay outside divergent head/producer guards; release follows consumption. */
    WN *Stage(WN *n) {
        std::string name=ST_name(WN_st(n));name=name.substr(strlen("__open64_mla_stage_"));
        WN *b=WN_CreateBlock();
        if(name=="copy") return Transfer(n);
        if(name=="init") {
            if(m.slots==2) {
                WN_INSERT_BlockLast(b,One("init",Argument(n,0)));WN_INSERT_BlockLast(b,Op("proxy"));
                for(unsigned i=0;i<2;++i) WN_INSERT_BlockLast(b,Store(phase[i],WN_Intconst(MTYPE_U4,0)));
            }
            WN *all=WN_CreateBlock();WN_INSERT_BlockLast(all,Leader(b,owner[1]));WN_INSERT_BlockLast(all,Op("cta_sync"));return all;
        }
        if(name=="wait") return m.slots==2?Slot_wait(n):Block(Op("cta_sync"));
        if(name=="publish" || name=="consume") return Block(Op("cta_sync"));
        if(name=="end") {
            WN_INSERT_BlockLast(b,Op("cta_sync"));if(m.slots==2) WN_INSERT_BlockLast(b,Leader(Block(One("invalidate",Argument(n,0))),owner[1]));
            WN_INSERT_BlockLast(b,Op("cta_sync"));return b;
        }
        if(name=="acquire" && m.slots==2) return Leader(Block(Op("proxy")),owner[1]);
        Qneed(name=="acquire" || name=="release","q6c_unknown_stage_operation");return b;
    }
    /* Heads and decoded tokens are independent. Their ordered inner F32 folds
     * remain serial within one lane; no reduction reassociation is introduced. */
    void Stride(WN *n) {
        ST *iv=WN_st(WN_index(n));WN_kid0(WN_start(n))=WN_Cvt(MTYPE_U4,MTYPE_I8,Load(owner[1]));
        WN_kid0(WN_step(n))=WN_Add(MTYPE_I8,Load(iv),Int(m.threads));
    }
    void Walk(WN *n,bool inside_head=false) {
        if(WN_operator(n)==OPR_BLOCK) {
            for(WN *s=WN_first(n),*next;s;s=next) {
                next=WN_next(s);
                if(WN_operator(s)==OPR_CALL && std::string(ST_name(WN_st(s))).find("__open64_mla_stage_")==0) {
                    WN *replacement=Stage(s);WN_INSERT_BlockBefore(n,s,replacement);WN_EXTRACT_FromBlock(n,s);
                } else Walk(s,inside_head);
            }return;
        }
        if(WN_operator(n)==OPR_DO_LOOP) {
            bool head=WN_st_idx(WN_index(n))==ST_st_idx(axis[5]);
            bool tokens=WN_st_idx(WN_index(n))==ST_st_idx(axis[4]);
            if(head || (tokens && !inside_head && !Has(n,"__open64_mla_stage_copy"))) Stride(n);
            Walk(WN_do_body(n),inside_head||head);return;
        }
        for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) Walk(WN_kid(n,i),inside_head);
    }
public:
    Mapper(const VHO_Q6_MAPPING &mapping):m(mapping) {for(unsigned i=0;i<6;++i) axis[i]=NULL;}
    void Build(const std::string &stage,const std::string &source_key) {
        WN *old=Root(),*original=WN_func_body(old),*outer=NULL,*first_view=NULL;unsigned views=0;
        for(WN *s=WN_first(original);s;s=WN_next(s)) {
            if(WN_operator(s)==OPR_STID && WN_operator(WN_kid0(s))==OPR_TAS) {if(!first_view) first_view=s;++views;}
            if(Has(s,"__open64_mla_stage_init")) {Qneed(!outer && WN_operator(s)==OPR_DO_LOOP,"q6c_unique_attention_nest");outer=s;}
        }
        Qneed(outer && first_view && views==6,"q6c_partition_boundaries");
        WN *h=Only_loop(WN_do_body(outer)),*q=Only_loop(WN_do_body(h));axis[0]=WN_st(WN_index(outer));axis[1]=WN_st(WN_index(h));axis[2]=WN_st(WN_index(q));
        /* Remaining IVs are defined by actual executable loop uses in the exact
         * accepted tree; all q5-axis locals were allocated consecutively. */
        Qneed(ST_IDX_index(ST_st_idx(axis[0]))+5<ST_Table_Size(CURRENT_SYMTAB),"q6c_axis_extent");
        for(unsigned i=1;i<3;++i) Qneed(ST_IDX_index(ST_st_idx(axis[i]))==ST_IDX_index(ST_st_idx(axis[0]))+i,"q6c_outer_axis_identity");
        for(unsigned i=3;i<6;++i) {ST_IDX id=make_ST_IDX(ST_IDX_index(ST_st_idx(axis[0]))+i,CURRENT_SYMTAB);axis[i]=&St_Table[id];Qneed(!strcmp(ST_name(axis[i]),"q5_axis"),"q6c_axis_inventory");}
        owner[0]=Local("q6c_block_owner",MTYPE_U4);owner[1]=Local("q6c_thread_owner",MTYPE_U4);
        for(unsigned i=0;i<2;++i) {Set_ST_sclass(owner[i],SCLASS_FORMAL);Set_ST_is_value_parm(owner[i]);phase[i]=Local("q6c_phase",MTYPE_U4);}
        WN *body=WN_CreateBlock();
        /* Every entry preserves native source guards. Prepare alone writes
         * status; later ordered launches uniformly refuse nonzero status. */
        if(stage!="prepare") {
            ST *status=WN_st(WN_formal(old,WN_num_formals(old)-1));
            WN *bad=WN_NE(MTYPE_I4,WN_Iload(MTYPE_I4,0,MTYPE_To_TY(MTYPE_I4),Load(status)),WN_Intconst(MTYPE_I4,0));
            WN_INSERT_BlockLast(body,WN_CreateIf(bad,Block(WN_Create(OPR_RETURN,MTYPE_V,MTYPE_V,0)),WN_CreateBlock()));
        }
        if(stage=="attention") {
            for(WN *s=first_view;s!=outer;s=WN_next(s)) {Qneed(s!=NULL,"q6c_view_order");WN_INSERT_BlockLast(body,Copy(s));}
            UINT64 groups=(m.heads+m.group-1)/m.group;
            WN *block=WN_Cvt(MTYPE_U4,MTYPE_I8,Load(owner[0]));
            WN_INSERT_BlockLast(body,Store(axis[2],WN_Binary(OPR_REM,MTYPE_I8,Copy(block),Int(m.queries))));
            WN *row=WN_Div(MTYPE_I8,block,Int(m.queries));
            WN_INSERT_BlockLast(body,Store(axis[1],WN_Binary(OPR_REM,MTYPE_I8,Copy(row),Int(groups))));
            WN_INSERT_BlockLast(body,Store(axis[0],WN_Div(MTYPE_I8,row,Int(groups))));
            WN *attention=Copy(WN_do_body(q));Walk(attention);WN_INSERT_BlockLast(body,attention);
        } else {
            WN *serial=WN_CreateBlock();bool active=stage=="prepare";
            for(WN *s=WN_first(original);s;s=WN_next(s)) {
                if(s==first_view) active=false;
                if(active && WN_operator(s)!=OPR_COMMENT && WN_operator(s)!=OPR_RETURN) WN_INSERT_BlockLast(serial,Copy(s));
                if(s==outer && stage=="finalize") active=true;
            }
            WN_INSERT_BlockLast(body,Leader(serial,owner[1]));
            const ST *s;ST_IDX idx;FOREACH_SYMBOL(CURRENT_SYMTAB,s,idx) if(ST_device_symbol_kind(*s)==DEVICE_SYMBOL_SHARED) {
                for(ST_ATTR_IDX a=1;a<ST_ATTR_Table_Size(CURRENT_SYMTAB);++a) {
                    ST_ATTR &attribute=St_Attr_Table(CURRENT_SYMTAB,a);
                    if(ST_ATTR_st_idx(attribute)==ST_st_idx(s) && ST_ATTR_kind(attribute)==ST_ATTR_DEVICE_SYMBOL) {attribute.kind=ST_ATTR_UNKNOWN;attribute.Set_value(0);}
                }
            }
        }
        WN_INSERT_BlockLast(body,WN_Create(OPR_RETURN,MTYPE_V,MTYPE_V,0));
        ST *function=WN_st(old);std::string name=std::string(ST_name(function))+"_q6c_"+stage;Set_ST_name(*function,Save_Str(name.c_str()));
        Set_ST_device_symbol_kind(*function,DEVICE_SYMBOL_FUNCTION);
        WN *entry=WN_CreateEntry(WN_num_formals(old)+2,function,body,NULL,NULL);WN_Set_Linenum(entry,WN_Get_Linenum(old));
        for(INT i=0;i<WN_num_formals(old);++i) WN_formal(entry,i)=Copy(WN_formal(old,i));
        for(unsigned i=0;i<2;++i) WN_formal(entry,WN_num_formals(old)+i)=WN_CreateIdname(0,owner[i]);
        Set_PU_Info_tree_ptr(Current_PU_Info,entry);Prototype(entry);
        std::ostringstream explanation;explanation<<"Q6c native GPU realization: "<<stage<<". Ordered launches are prepare -> attention -> finalize in one stream. "
            <<"Original pointer/formal order and Q3 source/snapshot/cache/scale identities remain; device leases/publication are Q6d.\n";
        if(stage=="attention") explanation<<"Grid "<<m.grid<<", block "<<m.threads<<"; CTA owns batch/query/head-group. Tokens are cooperatively decoded, each head's three source-ordered token folds stay in one lane. "
            <<"Producer lane zero splits exact runtime-aligned 16-byte bulk middles from synchronous byte heads/tails; unequal address residues use only synchronous copies. "
            <<"Only bulk bytes increment expect_tx. Initialize and proxy-publish barriers, arrive/wait with slot phase, then CTA sync before raw reads. "
            <<"CTA publication follows dequantization; CTA consumer join precedes slot reuse; drain/invalidate before return. All lanes including padded heads reach collectives.\n";
        else explanation<<"One CTA/one lane executes the existing native source statements. Separate ordered launch boundaries preserve global scratch dependencies; status guards prevent continuation after source failure.\n";
        explanation<<"No search or alternate-plan materialization; no F32 reassociation. Prior Q6b COMMENT is historical logical-stage context. AOT selection deferred; no GPU measurement is claimed.";
        WN *comment=WN_CreateComment(explanation.str().c_str());WN_Set_Linenum(comment,WN_Get_Linenum(old));Set_ST_Srcpos(*WN_st(comment),WN_Get_Linenum(old));
        WN_INSERT_BlockBefore(body,WN_first(body),comment);
        /* Retain the actual prior explanation as COMMENT, not a replacement
         * source algorithm. It is labelled historical by the new comment. */
        for(WN *s=WN_first(original);s;s=WN_next(s)) if(WN_operator(s)==OPR_COMMENT) WN_INSERT_BlockBefore(body,comment,Copy(s));
        Position(body,WN_Get_Linenum(old));
        Bind("tensor.q6c.schema","mla-gpu-staging-v1");Bind("tensor.q6c.stage",stage);Bind("tensor.q6c.source_key",source_key);Bind("tensor.q6c.explanation",explanation.str());
        Bind("tensor.kernel.block_x",Num(stage=="attention"?m.threads:1));Bind("tensor.kernel.grid_x",Num(stage=="attention"?m.grid:1));Bind("tensor.kernel.shared_bytes",Num(stage=="attention"?m.shared_bytes:0));
    }
    /* Every generated statement retains the original source lineage. */
    static void Position(WN *n,SRCPOS source) {
        if(OPCODE_has_next_prev(WN_opcode(n))) WN_Set_Linenum(n,source);
        if(WN_operator(n)==OPR_BLOCK) {for(WN *s=WN_first(n);s;s=WN_next(s)) Position(s,source);}
        else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) Position(WN_kid(n,i),source);
    }
    /* Interface is refined only in the owning active scope. */
    static void Prototype(WN *entry) {
        TY_IDX ty;TY &t=New_TY(ty);TY_Init(t,0,KIND_FUNCTION,MTYPE_UNKNOWN,0);Set_TY_has_prototype(ty);TYLIST_IDX l;
        Set_TYLIST_type(New_TYLIST(l),MTYPE_To_TY(MTYPE_V));Set_TY_tylist(t,l);
        for(INT i=0;i<WN_num_formals(entry);++i) Set_TYLIST_type(New_TYLIST(l),ST_type(WN_st(WN_formal(entry,i))));
        Set_TYLIST_type(New_TYLIST(l),TY_IDX_ZERO);Set_PU_prototype(Pu_Table[ST_pu(WN_st(entry))],ty);
    }
};
void Seal(const char *level) {Bind("tensor.q6c.level",level);Bind("tensor.q6c.key",VHO_q6c_key(Root()));}
ST *Coordinate(const char *name) {
    const ST *old;ST_IDX idx;FOREACH_SYMBOL(GLOBAL_SYMTAB,old,idx) Qneed(strcmp(ST_name(old),name)!=0,"q6c_foreign_coordinate_collision");
    TY_IDX ty;TY &t=New_TY(ty);TY_Init(t,12,KIND_STRUCT,MTYPE_M,Save_Str("open64_tensor_dim3"));Set_TY_align(ty,4);
    const char *fields[]={"x","y","z"};for(unsigned i=0;i<3;++i) {FLD_HANDLE f=New_FLD();if(!i) Set_TY_fld(t,f);FLD_Init(f,Save_Str(fields[i]),MTYPE_To_TY(MTYPE_U4),i*4);if(i==2) Set_FLD_last_field(f);}
    ST *s=New_ST(GLOBAL_SYMTAB);ST_Init(s,Save_Str(name),CLASS_VAR,SCLASS_EXTERN,EXPORT_PREEMPTIBLE,ty);return s;
}
void Physical(WN *n,ST **owners,ST **coords) {
    if(WN_operator(n)==OPR_BLOCK) {for(WN *s=WN_first(n);s;s=WN_next(s)) Physical(s,owners,coords);return;}
    for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) {
        WN *k=WN_kid(n,i);bool replaced=false;
        if(WN_operator(k)==OPR_LDID) for(unsigned j=0;j<2;++j) if(WN_st_idx(k)==ST_st_idx(owners[j])) {
            WN_kid(n,i)=WN_CreateLdid(OPC_U4U4LDID,0,ST_st_idx(coords[j]),ST_type(coords[j]),1);replaced=true;
        }
        if(!replaced) Physical(k,owners,coords);
    }
}
void Vocabulary(WN *n,bool high) {
    OPERATOR op=WN_operator(n);
    Qneed(high || (op!=OPR_DO_LOOP && op!=OPR_IF && op!=OPR_ARRAY),"q6c_structured_after_High");
    if(op==OPR_CALL) Qneed(std::string(ST_name(WN_st(n))).find("__open64_q6c_")==0,"q6c_unlowered_call");
    if(op==OPR_BLOCK) {for(WN *s=WN_first(n);s;s=WN_next(s)) Vocabulary(s,high);}
    else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) Vocabulary(WN_kid(n,i),high);
}
}
void VHO_q6c_verify(const char *level) {
    VHO_q6c_check_key(Root(),level);Vocabulary(Root(),!strcmp(level,"high"));
    Qneed(ST_device_symbol_kind(*WN_st(Root()))==(!strcmp(level,"low")?DEVICE_SYMBOL_KERNEL:DEVICE_SYMBOL_FUNCTION),"q6c_entry_kind");
    Verify_SYMTAB(CURRENT_SYMTAB);Qneed(WN_verifier(Root()),"q6c_WN_verifier");
}
void VHO_q6c_worker(const VHO_Q6_MAPPING &m,const std::string &stage,const std::string &source_key) {
    Qneed(stage=="prepare" || stage=="attention" || stage=="finalize","q6c_stage");Mapper(m).Build(stage,source_key);Seal("high");VHO_q6c_verify("high");
}
void VHO_q6c_memory() {
    VHO_q6c_verify("high");Lower_Init();WN_func_body(Root())=WN_Lower(WN_func_body(Root()),LOWER_ARRAY|LOWER_SCF,NULL,"Q6c structured memory");Lowering_Finalize();Seal("mid");VHO_q6c_verify("mid");
}
void VHO_q6c_gpu() {
    VHO_q6c_verify("mid");WN *old=Root();INT count=WN_num_formals(old)-2;
    ST *owners[]={WN_st(WN_formal(old,count)),WN_st(WN_formal(old,count+1))},*coords[]={Coordinate("blockIdx"),Coordinate("threadIdx")};
    Physical(WN_func_body(old),owners,coords);WN *entry=WN_CreateEntry(count,WN_st(old),WN_func_body(old),NULL,NULL);WN_Set_Linenum(entry,WN_Get_Linenum(old));
    for(INT i=0;i<count;++i) WN_formal(entry,i)=WN_COPY_Tree(WN_formal(old,i));
    Set_PU_Info_tree_ptr(Current_PU_Info,entry);Mapper::Prototype(entry);
    for(unsigned j=0;j<2;++j) {Clear_ST_is_value_parm(owners[j]);Set_ST_sclass(owners[j],SCLASS_AUTO);}
    unsigned changed=0;for(ST_ATTR_IDX a=1;a<ST_ATTR_Table_Size(GLOBAL_SYMTAB);++a) {
        ST_ATTR &attr=St_Attr_Table(GLOBAL_SYMTAB,a);if(ST_ATTR_st_idx(attr)==WN_st_idx(entry) && ST_ATTR_kind(attr)==ST_ATTR_DEVICE_SYMBOL) {attr.Set_value(DEVICE_SYMBOL_KERNEL);++changed;}
    }Qneed(changed==1,"q6c_unique_entry_attribute");Seal("low");VHO_q6c_verify("low");
}
