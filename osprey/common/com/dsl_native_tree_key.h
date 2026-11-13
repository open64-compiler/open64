/* Copyright (C) 2026 Open64 Project */
/* Exact, read-only executable identity for bounded scalar native checkpoints.
 * This policy-free helper owns no PU context or analyses. It uses existing
 * WN/ST/TY records and changes no mapped layout. Producers must establish
 * legality separately; equality detects stale products, not program equivalence.
 * Contract: doc/AI-COMPILER-SEARCH-M5-DEVICE-BRIDGE-v1.0.md. */
#ifndef DSL_NATIVE_TREE_KEY_H
#define DSL_NATIVE_TREE_KEY_H
#include <set>
#include <sstream>
#include <stdexcept>
#include "wn.h"
#include "stab.h"
#include "targ_const.h"
#include "dsl_ir_image.h"

class DSL_NATIVE_TREE_KEY {
    std::ostringstream _out;
    std::set<const WN *> _seen;
    unsigned _depth;
    bool _include_comments;
    /* Length delimit text; neither separators nor names can alias fields. */
    void Text(const char *s) { std::string v=s?s:"";_out<<v.size()<<':'<<v<<';'; }
    /* Preserve signed payload bits through an unambiguous decimal field. */
    void Number(UINT64 n) { _out<<n<<';'; }
    /* Serialize the supported complete type shape, refusing recursive types. */
    void Type(TY_IDX ty,unsigned depth=0) {
        /* Some native CALL/ENTRY auxiliary type slots are intentionally zero;
         * declaration prototypes carry their actual interface contract. */
        if(!ty) {Number(0);return;}
        if(depth>8) throw std::runtime_error("native_key_type_depth");
        Number(ty);Number(TY_kind(ty));Number(TY_mtype(ty));Number(TY_size(ty));
        Number(TY_align(ty));Number(TY_flags(ty));
        switch(TY_kind(ty)) {
        case KIND_SCALAR:case KIND_VOID:break;
        case KIND_POINTER:Type(TY_pointed(ty),depth+1);break;
        case KIND_ARRAY: {
            Type(TY_etype(ty),depth+1);ARB_HANDLE arb=TY_arb(ty);
            unsigned rank=ARB_dimension(arb);if(!rank || rank>16) throw std::runtime_error("native_key_rank");
            Number(rank);
            for(unsigned i=0;i<rank;++i) {
                ARB_HANDLE b=arb[i];
                if(!ARB_const_lbnd(b)||!ARB_const_ubnd(b)||!ARB_const_stride(b)) throw std::runtime_error("native_key_dynamic_bound");
                Number(ARB_lbnd_val(b));Number(ARB_ubnd_val(b));Number(ARB_stride_val(b));
            }break;
        }
        case KIND_STRUCT: {
            unsigned count=0;
            for(FLD_HANDLE f=TY_fld(ty);!f.Is_Null();f=FLD_next(f)) {
                if(++count>64) throw std::runtime_error("native_key_fields");
                Text(FLD_name(f));Number(FLD_ofst(f));Number(FLD_flags(f));Number(FLD_bsize(f));Number(FLD_bofst(f));Type(FLD_type(f),depth+1);
                Number(FLD_last_field(f));if(FLD_last_field(f)) break;
            }Number(count);break;
        }
        case KIND_FUNCTION: {
            Number(TY_has_prototype(ty));unsigned count=0;TYLIST_IDX a=TY_tylist(ty);
            if(!a) throw std::runtime_error("native_key_missing_prototype");
            while(TYLIST_type(Tylist_Table[a])) {
                if(++count>512) throw std::runtime_error("native_key_prototype");
                Type(TYLIST_type(Tylist_Table[a++]),depth+1);
            }Number(count);break;
        }
        default:throw std::runtime_error("native_key_type");
        }
    }
    /* Record referenced declarations and constant bits, excluding source lines. */
    void Symbol(const ST *s) {
        Number(ST_st_idx(s));Text(ST_name(s));Number(ST_class(s));Number(ST_sclass(s));
        Number(ST_export(s));Number(ST_device_symbol_kind(*s));
        if(ST_class(s)==CLASS_CONST) {
            TCON c=STC_val(s);TYPE_ID t=TCON_ty(c);Number(t);
            if(t==MTYPE_F4) Number(TCON_uval(c));
            else if(t==MTYPE_F8||t==MTYPE_I8||t==MTYPE_U8) Number(TCON_k0(c));
            else if(MTYPE_is_integral(t)) Number(TCON_uval(c));
            else throw std::runtime_error("native_key_constant_type");
        } else if(ST_class(s)==CLASS_VAR || ST_class(s)==CLASS_PREG) {
            Type(ST_type(s));Number(ST_ofst(s));Number(ST_is_value_parm(s));
            Number(ST_addr_saved(s));Number(ST_addr_passed(s));Number(ST_pt_to_unique_mem(s));
        } else if(ST_class(s)==CLASS_FUNC) Type(ST_pu_type(s));
        else if(ST_class(s)!=CLASS_NAME) throw std::runtime_error("native_key_symbol");
    }
    /* Bound recursion and reject shared executable nodes before reading payloads. */
    void Walk(const WN *n) {
        if(n && !_include_comments && WN_operator(n)==OPR_COMMENT) return;
        if(!n || ++_depth>256 || _seen.size()>200000 || !_seen.insert(n).second || DSL_WN_Is_Native(n))
            throw std::runtime_error("native_key_tree");
        OPCODE opc=WN_opcode(n);OPERATOR op=WN_operator(n);
        /* Admit only payloads covered below. Native source positions, map IDs
         * and nonexecuting LOOP_INFO trip estimates are not executable keys. */
        switch(op) {
        case OPR_FUNC_ENTRY:case OPR_BLOCK:case OPR_IDNAME:case OPR_COMMENT:case OPR_PRAGMA:
        case OPR_DO_LOOP:case OPR_IF:case OPR_RETURN:case OPR_LOOP_INFO:
        case OPR_LDID:case OPR_STID:case OPR_LDA:case OPR_ILOAD:case OPR_ISTORE:
        case OPR_INTCONST:case OPR_CONST:case OPR_ARRAY:case OPR_ADD:case OPR_SUB:
        case OPR_MPY:case OPR_DIV:case OPR_REM:case OPR_BAND:case OPR_BIOR:
        case OPR_LAND:case OPR_LIOR:case OPR_SELECT:case OPR_LT:case OPR_LE:
        case OPR_GT:case OPR_GE:case OPR_EQ:case OPR_NE:case OPR_CVT:case OPR_CVTL:
        case OPR_ABS:case OPR_NEG:case OPR_INTRINSIC_OP:case OPR_PARM:case OPR_CALL:
        case OPR_TRUNC:case OPR_SHL:case OPR_LSHR:case OPR_TAS:
        case OPR_LABEL:case OPR_TRUEBR:case OPR_FALSEBR:case OPR_GOTO:break;
        default:throw std::runtime_error("native_key_unsupported_operator");
        }
        Number(opc);Number(WN_kid_count(n));
        if(OPCODE_has_sym(opc)) {Number(WN_st_idx(n));if(WN_st_idx(n)) Symbol(WN_st(n));}
        if(OPCODE_has_offset(opc)) Number(WN_offset(n));
        if(OPCODE_has_1ty(opc)||OPCODE_has_2ty(opc)) Type(WN_ty(n));
        if(OPCODE_has_2ty(opc)) Type(WN_load_addr_ty(n));
        if(OPCODE_has_flags(opc)) Number(WN_flag(n));
        if(OPCODE_has_label(opc)) Number(WN_label_number(n));
        if(op==OPR_INTCONST) Number(WN_const_val(n));
        if(op==OPR_ARRAY) Number(WN_element_size(n));
        if(op==OPR_CVTL) Number(WN_cvtl_bits(n));
        if(op==OPR_INTRINSIC_OP || op==OPR_INTRINSIC_CALL) Number(WN_intrinsic(n));
        if(op==OPR_LDID||op==OPR_STID||op==OPR_ILOAD||op==OPR_ISTORE) Number(WN_field_id(n));
        if(op==OPR_COMMENT) Text(Index_To_Str(WN_GetComment(n)));
        if(op==OPR_PRAGMA) {Number(WN_pragma(n));Number(WN_pragma_flags(n));Number(WN_pragma_arg1(n));Number(WN_pragma_arg2(n));}
        if(op==OPR_BLOCK) {
            unsigned count=0;for(WN *s=WN_first(n);s;s=WN_next(s)) {
                if(!_include_comments && WN_operator(s)==OPR_COMMENT) continue;
                Walk(s);++count;
            }Number(count);
        } else for(INT i=0;i<WN_kid_count(n);++i) {Number(WN_kid(n,i)!=NULL);if(WN_kid(n,i)) Walk(WN_kid(n,i));}
        --_depth;
    }
public:
    /* Admission always uses the default complete key. Explicit comment-free
     * mode is only for independent executable-equivalence artifact comparisons. */
    DSL_NATIVE_TREE_KEY(bool include_comments=true):_depth(0),_include_comments(include_comments) {}
    /* Return full equality evidence; never substitute a lossy hash for it. */
    std::string Build(const WN *root) {Text("open64-native-key-v1");Walk(root);return _out.str();}
    /* Bind a declaration inventory without manufacturing a WN or new table row. */
    std::string Declaration(const ST *symbol) {Text("open64-declaration-key-v1");Symbol(symbol);return _out.str();}
};
#endif
