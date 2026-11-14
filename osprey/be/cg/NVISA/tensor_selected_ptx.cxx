/* Copyright (C) 2026 Open64 Project */
/* Bounded native PTX emission from the selected bridge's verified Low PU.
 * This standalone consumer shares no CUDA algorithm templates and introduces
 * no be.so dependency or replacement of the historical general NVISA backend.
 * Unsupported native semantics fail closed before text publication. Strict
 * exp/log use separately linked device math helpers, not approximate ex2/lg2.
 * Contract: doc/AI-COMPILER-SEARCH-M5-DEVICE-BRIDGE-v1.0.md. */
#include "tensor_selected_bridge.h"
#include "../../vho/tensor_quantized_device.h"
#include "wn.h"
#include "stab.h"
#include "pu_info.h"
#include "targ_const.h"
#include <map>
#include <sstream>
#include <iomanip>
#include <stdexcept>
#include <cstring>

namespace {
/* Keep this native consumer compatible with Open64's C++98 tool build. */
std::string number(INT64 value) {std::ostringstream s;s<<value;return s.str();}
/* Register values retain address-space provenance independently of bit type. */
struct DSL_PTX_VALUE {
    std::string reg,space;TYPE_ID type;
    /* Own copied register facts; expressions never retain a WN pointer. */
    DSL_PTX_VALUE(const std::string &r="",TYPE_ID t=MTYPE_V,const std::string &s=""):reg(r),space(s),type(t) {}
};
class DSL_SELECTED_PTX {
    std::ostringstream _decl,_code;
    std::map<std::pair<ST_IDX,INT64>,DSL_PTX_VALUE> _locals;
    unsigned _registers,_predicates,_calls;
    DSL_PTX_VALUE _return;
    /* Refuse missing semantics instead of selecting an alternative algorithm. */
    void Need(bool ok,const char *why) {if(!ok) throw std::runtime_error(why);}
    /* Normalize narrow scalar register types; memory retains its actual width. */
    TYPE_ID Register_type(TYPE_ID t) {
        if(t==MTYPE_B || t==MTYPE_U1 || t==MTYPE_U2) return MTYPE_U4;
        if(t==MTYPE_I1 || t==MTYPE_I2) return MTYPE_I4;
        Need(t==MTYPE_F4||t==MTYPE_I4||t==MTYPE_U4||t==MTYPE_I8||t==MTYPE_U8,"PTX_unsupported_register_type");return t;
    }
    /* Return PTX arithmetic signedness and width for one native scalar. */
    std::string Type(TYPE_ID t) {
        if(t==MTYPE_U1) return "u8";if(t==MTYPE_I1) return "s8";
        if(t==MTYPE_F4) return "f32";
        t=Register_type(t);return std::string(MTYPE_is_signed(t)?"s":"u")+(MTYPE_byte_size(t)==8?"64":"32");
    }
    /* Fresh virtual registers are assembler-allocated; resource usage is later evidence. */
    DSL_PTX_VALUE Fresh(TYPE_ID type,const std::string &space="") {
        TYPE_ID t=Register_type(type);std::ostringstream r;r<<"%v"<<_registers++;
        _decl<<" .reg ."<<(t==MTYPE_F4?"f32":MTYPE_byte_size(t)==8?"b64":"b32")<<' '<<r.str()<<";\n";
        return DSL_PTX_VALUE(r.str(),t,space);
    }
    /* Predicate temporaries are private to this emitted entry. */
    std::string Predicate() {
        std::ostringstream p;p<<"%p"<<_predicates++;_decl<<" .reg .pred "<<p.str()<<";\n";return p.str();
    }
    /* A native variable/preg is a distinct persistent PTX virtual register. */
    DSL_PTX_VALUE Local(ST_IDX st,INT64 offset,TYPE_ID type) {
        std::pair<ST_IDX,INT64> id(st,offset);
        if(!_locals.count(id)) _locals[id]=Fresh(type);
        Need(_locals[id].type==Register_type(type),"PTX_local_type_conflict");return _locals[id];
    }
    /* Issue one instruction without modifying the source tree or tables. */
    void Instruction(const std::string &op,const std::string &args) {_code<<' '<<op<<' '<<args<<";\n";}
    /* Preserve COMMENT text as line comments, including multiline explanations. */
    void Comment(const char *text) {
        std::istringstream s(text);std::string line;while(std::getline(s,line)) _code<<" // "<<line<<'\n';
    }
    /* Convert a C/WHIRL truth value to a PTX predicate without changing NaNs. */
    std::string Truth(const DSL_PTX_VALUE &v) {
        Need(v.type!=MTYPE_F4,"PTX_float_condition_unsupported");
        std::string p=Predicate();Instruction("setp.ne."+Type(v.type),p+", "+v.reg+", 0");return p;
    }
    /* Materialize a native exact bit-pattern; no host decimal FP round trip. */
    DSL_PTX_VALUE Constant(WN *n) {
        DSL_PTX_VALUE r=Fresh(WN_rtype(n));std::ostringstream v;
        if(WN_operator(n)==OPR_CONST) {
            Need(WN_rtype(n)==MTYPE_F4,"PTX_constant_type");
            v<<"0f"<<std::hex<<std::setw(8)<<std::setfill('0')<<(UINT32)TCON_uval(STC_val(WN_st(n)));
        } else v<<(MTYPE_byte_size(r.type)==8?(UINT64)WN_const_val(n):(UINT64)(UINT32)WN_const_val(n));
        Instruction("mov."+(r.type==MTYPE_F4?std::string("f32"):MTYPE_byte_size(r.type)==8?"b64":"b32"),r.reg+", "+v.str());return r;
    }
    /* Device math ABI uses ordinary PTX param calls; helper objects are linked
     * after assembly. It neither approximates nor fuses source arithmetic. */
    DSL_PTX_VALUE Math(WN *n) {
        Need(WN_intrinsic(n)==INTRN_F4EXP||WN_intrinsic(n)==INTRN_F4LOG,"PTX_math_intrinsic");
        DSL_PTX_VALUE x=Expression(WN_kid0(n)),r=Fresh(MTYPE_F4);std::ostringstream id;id<<_calls++;
        std::string a="math_a"+id.str(),b="math_r"+id.str();
        _code<<" { .param .b32 "<<a<<"; .param .b32 "<<b<<";\n";
        Instruction("st.param.f32","["+a+"], "+x.reg);
        Instruction("call","("+b+"), "+(WN_intrinsic(n)==INTRN_F4EXP?std::string("__open64_exp_f32"):"__open64_log_f32")+", ("+a+")");
        Instruction("ld.param.f32",r.reg+", ["+b+"]");_code<<" }\n";return r;
    }
    /* Consume explicit Low expressions only. Address spaces follow formal/LDA
     * roots and ADDs, so global and shared loads cannot silently interchange. */
    DSL_PTX_VALUE Expression(WN *n) {
        OPERATOR op=WN_operator(n);TYPE_ID type=WN_rtype(n);
        if(op==OPR_PARM) return Expression(WN_kid0(n));
        if(op==OPR_INTCONST||op==OPR_CONST) return Constant(n);
        if(op==OPR_LDID) {
            ST *s=WN_st(n);
            if(ST_class(s)==CLASS_PREG && WN_offset(n)==-1) {Need(!_return.reg.empty(),"PTX_missing_call_return");return _return;}
            if(!strcmp(ST_name(s),"threadIdx")||!strcmp(ST_name(s),"blockIdx")) {
                Need(WN_desc(n)==MTYPE_U4 && WN_offset(n)==0 && WN_field_id(n)==1,"PTX_GPU_coordinate");
                DSL_PTX_VALUE r=Fresh(MTYPE_U4);Instruction("mov.u32",r.reg+", "+(!strcmp(ST_name(s),"threadIdx")?"%tid.x":"%ctaid.x"));
                if(type==MTYPE_U4) return r;
                Need(type==MTYPE_U8,"PTX_GPU_coordinate_extension");DSL_PTX_VALUE wide=Fresh(type);
                Instruction("cvt.u64.u32",wide.reg+", "+r.reg);return wide;
            }
            return Local(ST_st_idx(s),WN_offset(n),type);
        }
        if(op==OPR_LDA) {
            Need(ST_device_symbol_kind(*WN_st(n))==DEVICE_SYMBOL_SHARED && WN_offset(n)>=0 &&
                (UINT64)WN_offset(n)<=TY_size(ST_type(WN_st(n))),"PTX_LDA_space");
            DSL_PTX_VALUE r=Fresh(MTYPE_U8,"shared");
            Instruction("mov.u64",r.reg+", smem_"+number(WN_st_idx(n))+"+"+number(WN_offset(n)));return r;
        }
        if(op==OPR_INTRINSIC_OP) return Math(n);
        if(op==OPR_ILOAD) {
            DSL_PTX_VALUE a=Expression(WN_kid0(n)),r=Fresh(WN_desc(n));
            Need(!a.space.empty() && !WN_field_id(n),"PTX_load_address");
            Instruction("ld."+a.space+"."+Type(WN_desc(n)),r.reg+", ["+a.reg+"+"+number(WN_offset(n))+"]");
            if(r.type==Register_type(type)) return r;
            Need(r.type!=MTYPE_F4 && type!=MTYPE_F4,"PTX_load_conversion");DSL_PTX_VALUE wide=Fresh(type);
            Instruction("cvt."+Type(wide.type)+"."+Type(r.type),wide.reg+", "+r.reg);return wide;
        }
        DSL_PTX_VALUE a=Expression(WN_kid0(n)),r=Fresh(type);
        if(op==OPR_EXTRACT_BITS) {
            Need(type==MTYPE_U4 && WN_bit_size(n)>0 && WN_bit_size(n)<=32 && WN_bit_offset(n)<=32-WN_bit_size(n),"PTX_extract_payload");
            Instruction("bfe.u32",r.reg+", "+a.reg+", "+number(WN_bit_offset(n))+", "+number(WN_bit_size(n)));return r;
        }
        if(op==OPR_TAS) {Need(a.type==r.type,"PTX_TAS_type");r.space=a.space;Instruction("mov.b"+number(MTYPE_byte_size(r.type)*8),r.reg+", "+a.reg);return r;}
        if(op==OPR_TRUNC) {Need(a.type==MTYPE_F4 && MTYPE_is_integral(r.type),"PTX_TRUNC_type");Instruction("cvt.rzi."+Type(r.type)+".f32",r.reg+", "+a.reg);return r;}
        if(op==OPR_CVT || op==OPR_CVTL) {
            if(op==OPR_CVTL && WN_cvtl_bits(n)!=MTYPE_byte_size(r.type)*8) {
                Need(WN_cvtl_bits(n)==8 || WN_cvtl_bits(n)==16,"PTX_CVTL_width");
                Instruction("cvt."+std::string(MTYPE_is_signed(r.type)?"s":"u")+number(WN_cvtl_bits(n))+"."+Type(a.type),r.reg+", "+a.reg);return r;
            }
            if(a.type!=MTYPE_F4 && r.type==MTYPE_F4) {
                Instruction("cvt.rn.f32."+Type(a.type),r.reg+", "+a.reg);return r;
            }
            Need(a.type==r.type || (a.type!=MTYPE_F4 && r.type!=MTYPE_F4),"PTX_numeric_conversion_unsupported");
            if(a.type==r.type) Instruction("mov.b"+number(MTYPE_byte_size(r.type)*8),r.reg+", "+a.reg);
            else Instruction("cvt."+Type(r.type)+"."+Type(a.type),r.reg+", "+a.reg);r.space=a.space;return r;
        }
        if(op==OPR_ABS||op==OPR_NEG) {Instruction((op==OPR_ABS?"abs.":"neg.")+Type(type),r.reg+", "+a.reg);return r;}
        DSL_PTX_VALUE b=Expression(WN_kid1(n));
        if(op==OPR_SELECT) {
            DSL_PTX_VALUE c=Expression(WN_kid2(n));
            Instruction("selp."+Type(type),r.reg+", "+b.reg+", "+c.reg+", "+Truth(a));return r;
        }
        if(op==OPR_LT||op==OPR_LE||op==OPR_GT||op==OPR_GE||op==OPR_EQ||op==OPR_NE) {
            const char *comparison=op==OPR_LT?"lt":op==OPR_LE?"le":op==OPR_GT?"gt":op==OPR_GE?"ge":op==OPR_EQ?"eq":a.type==MTYPE_F4?"neu":"ne";
            std::string p=Predicate();Instruction("setp."+std::string(comparison)+"."+Type(a.type),p+", "+a.reg+", "+b.reg);
            Instruction("selp.u32",r.reg+", 1, 0, "+p);return r;
        }
        if(op==OPR_LAND||op==OPR_LIOR) {
            Need(a.type!=MTYPE_F4 && b.type!=MTYPE_F4,"PTX_logical_type");
            std::string left=Truth(a),right=Truth(b),p=Predicate();
            Instruction(op==OPR_LAND?"and.pred":"or.pred",p+", "+left+", "+right);
            Instruction("selp.u32",r.reg+", 1, 0, "+p);return r;
        }
        if(op==OPR_SHL || op==OPR_LSHR) {
            Instruction((op==OPR_SHL?"shl.b":"shr.u")+number(MTYPE_byte_size(r.type)*8),r.reg+", "+a.reg+", "+b.reg);return r;
        }
        std::string operation;
        if(op==OPR_ADD) operation="add";else if(op==OPR_SUB) operation="sub";else if(op==OPR_MPY) operation="mul";
        else if(op==OPR_DIV) operation="div";else if(op==OPR_REM) operation="rem";
        else if(op==OPR_BAND) operation="and";else if(op==OPR_BIOR) operation="or";
        else throw std::runtime_error(std::string("PTX_expression/")+OPCODE_name(WN_opcode(n)));
        if(type==MTYPE_F4) operation+=".rn";
        else if(op==OPR_MPY) operation+=".lo";
        bool bits=op==OPR_BAND||op==OPR_BIOR;
        Instruction(operation+"."+(bits?std::string("b")+number(MTYPE_byte_size(r.type)*8):Type(type)),r.reg+", "+a.reg+", "+b.reg);
        if(op==OPR_ADD) {Need(a.space.empty()||b.space.empty(),"PTX_two_address_roots");r.space=a.space.empty()?b.space:a.space;}
        return r;
    }
    /* Concrete Q6c operations have instruction-level meaning. The owning
     * native phase already chose operands, partition, participants and phases. */
    void Stage_call(WN *n,const std::string &op) {
        unsigned count=op=="bulk"?4:(op=="expect" || op=="wait")?2:(op=="proxy" || op=="cta_sync")?0:1;
        Need((unsigned)WN_kid_count(n)==count,"PTX_Q6c_arity");
        if(op=="cta_sync") {Instruction("bar.sync","0");return;}
        if(op=="proxy") {Instruction("fence.proxy.async.shared::cta","");return;}
        DSL_PTX_VALUE a=Expression(WN_kid0(n));Need(a.space=="shared","PTX_Q6c_shared_operand");
        DSL_PTX_VALUE address=Fresh(MTYPE_U4);Instruction("cvt.u32.u64",address.reg+", "+a.reg);
        if(op=="init") {Instruction("mbarrier.init.shared::cta.b64","["+address.reg+"], 1");return;}
        if(op=="invalidate") {Instruction("mbarrier.inval.shared::cta.b64","["+address.reg+"]");return;}
        if(op=="arrive") {Instruction("mbarrier.arrive.release.cta.shared::cta.b64","_, ["+address.reg+"]");return;}
        DSL_PTX_VALUE b=Expression(WN_kid(n,1));
        if(op=="expect") {Need(b.type==MTYPE_U4,"PTX_Q6c_count");Instruction("mbarrier.expect_tx.relaxed.cta.shared::cta.b64","["+address.reg+"], "+b.reg);return;}
        if(op=="wait") {
            Need(b.type==MTYPE_U4,"PTX_Q6c_phase");std::string p=Predicate(),label="stage_wait_"+number(_calls++);
            _code<<label<<":\n";Instruction("mbarrier.try_wait.parity.acquire.cta.shared::cta.b64",p+", ["+address.reg+"], "+b.reg);
            _code<<" @!"<<p<<" bra "<<label<<";\n";return;
        }
        Need(op=="bulk" && b.space=="global","PTX_Q6c_global_source");DSL_PTX_VALUE bytes=Expression(WN_kid(n,2)),bar=Expression(WN_kid(n,3)),bar32=Fresh(MTYPE_U4);
        Need(bytes.type==MTYPE_U4 && bar.space=="shared","PTX_Q6c_bulk_operands");Instruction("cvt.u32.u64",bar32.reg+", "+bar.reg);
        Instruction("cp.async.bulk.shared::cluster.global.mbarrier::complete_tx::bytes","["+address.reg+"], ["+b.reg+"], "+bytes.reg+", ["+bar32.reg+"]");
    }
    /* Collective lowering consumes the explicit typed CALL operands. */
    void Call(WN *n) {
        std::string name=ST_name(WN_st(n));
        if(name.find("__open64_q6c_")==0) {Stage_call(n,name.substr(13));return;}
        if(name=="__syncthreads") {Need(WN_kid_count(n)==0,"PTX_CTA_barrier_ABI");Instruction("bar.sync","0");return;}
        Need(WN_kid_count(n)==(name=="__syncwarp"?1:4),"PTX_collective_ABI");
        WN *mask=WN_kid0(WN_kid0(n));Need(WN_operator(mask)==OPR_INTCONST && (UINT32)WN_const_val(mask)==0xffffffffU,"PTX_full_mask");
        if(name=="__syncwarp") {Instruction("bar.warp.sync","0xffffffff");return;}
        Need(name=="__shfl_sync"||name=="__shfl_down_sync","PTX_unknown_call");
        WN *width=WN_kid0(WN_kid(n,3));Need(WN_operator(width)==OPR_INTCONST && WN_const_val(width)==32,"PTX_warp_width");
        DSL_PTX_VALUE x=Expression(WN_kid(n,1)),delta=Expression(WN_kid(n,2));_return=Fresh(MTYPE_F4);
        Instruction(name=="__shfl_sync"?"shfl.sync.idx.b32":"shfl.sync.down.b32",_return.reg+", "+x.reg+", "+delta.reg+", 31, 0xffffffff");
    }
    /* Emit native lowered labels/branches and memory statements in tree order. */
    void Statement(WN *n) {
        OPERATOR op=WN_operator(n);
        if(op==OPR_BLOCK) {for(WN *s=WN_first(n);s;s=WN_next(s)) Statement(s);return;}
        if(op==OPR_COMMENT) {Comment(Index_To_Str(WN_GetComment(n)));return;}
        if(op==OPR_STID) {
            DSL_PTX_VALUE value=Expression(WN_kid0(n)),dest=Local(WN_st_idx(n),WN_offset(n),WN_desc(n));
            Need(value.type==dest.type,"PTX_assignment_type");
            Need(dest.space.empty() || value.space==dest.space,"PTX_address_space_join_unsupported");
            _locals[std::make_pair(WN_st_idx(n),WN_offset(n))].space=value.space;
            Instruction("mov."+(value.type==MTYPE_F4?std::string("f32"):"b"+number(MTYPE_byte_size(value.type)*8)),dest.reg+", "+value.reg);return;
        }
        if(op==OPR_ISTORE) {
            DSL_PTX_VALUE value=Expression(WN_kid0(n)),a=Expression(WN_kid1(n));
            Need(!a.space.empty()&&!WN_field_id(n),"PTX_store_address");
            Instruction("st."+a.space+"."+Type(WN_desc(n)),"["+a.reg+"+"+number(WN_offset(n))+"], "+value.reg);return;
        }
        if(op==OPR_LABEL) {_code<<"L"<<WN_label_number(n)<<":\n";return;}
        if(op==OPR_GOTO) {Instruction("bra","L"+number(WN_label_number(n)));return;}
        if(op==OPR_TRUEBR||op==OPR_FALSEBR) {
            std::string p=Truth(Expression(WN_kid0(n)));_code<<" @"<<(op==OPR_FALSEBR?"!":"")<<p<<" bra L"<<WN_label_number(n)<<";\n";return;
        }
        if(op==OPR_EVAL) {Expression(WN_kid0(n));return;}
        if(op==OPR_CALL) {Call(n);return;}
        if(op==OPR_RETURN) {Instruction("ret","");return;}
        throw std::runtime_error(std::string("PTX_statement/")+OPCODE_name(WN_opcode(n)));
    }
public:
    /* Start a fresh per-PU emitter; native compilation context remains borrowed. */
    DSL_SELECTED_PTX():_registers(0),_predicates(0),_calls(0) {}
    /* Build the entire entry in memory; the driver owns atomic publication. */
    std::string Build() {
        bool q6c=ST_tensor_metadata(WN_st_idx(PU_Info_tree_ptr(Current_PU_Info)),"tensor.q6c.schema")!=NULL;
        if(q6c) VHO_q6c_verify("low");else VHO_tensor_verify_bridge("low");WN *entry=PU_Info_tree_ptr(Current_PU_Info);ST_IDX st=WN_st_idx(entry);
        std::ostringstream out;out<<(q6c?".version 8.6\n":".version 7.8\n")<<".target sm_90\n.address_size 64\n"
            ".extern .func (.param .b32 r) __open64_exp_f32(.param .b32 x);\n"
            ".extern .func (.param .b32 r) __open64_log_f32(.param .b32 x);\n"
            ".visible .entry "<<ST_name(st)<<"(\n";
        for(INT i=0;i<WN_num_formals(entry);++i) {
            if(i) out<<",\n";out<<" .param .u64 arg"<<i;
            DSL_PTX_VALUE r=Local(WN_st_idx(WN_formal(entry,i)),0,MTYPE_U8);
            _locals[std::make_pair(WN_st_idx(WN_formal(entry,i)),0)].space="global";
            Instruction("ld.param.u64",r.reg+", [arg"+number(i)+"]");
        }
        out<<"\n)\n.reqntid "<<ST_tensor_metadata(st,"tensor.kernel.block_x")<<",1,1\n{\n";
        const ST *s;ST_IDX idx;FOREACH_SYMBOL(CURRENT_SYMTAB,s,idx) if(ST_device_symbol_kind(*s)==DEVICE_SYMBOL_SHARED)
            _decl<<" .shared .align "<<TY_align(ST_type(s))<<" .b8 smem_"<<ST_st_idx(s)<<'['<<TY_size(ST_type(s))<<"];\n";
        Statement(WN_func_body(entry));out<<_decl.str()<<_code.str()<<"}\n";return out.str();
    }
};
}
/* Native emitter accepts exactly the active sealed Low product. */
std::string CG_tensor_selected_ptx() {return DSL_SELECTED_PTX().Build();}
