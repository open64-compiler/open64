/* Copyright (C) 2026 Open64 Project */
/* Shared common-High construction: faithful expansion in the driver's empty PU
 * and one already selected algorithm body staged by the owning LNO transaction.
 * Design: doc/AI-COMPILER-SEARCH-M2A-HIGH-ADMISSION-v1.0.md.
 * Produces ordered signed DO_LOOP/ARRAY WHIRL, without GPU mapping, search,
 * runtime calls or a new binary representation. Selected-body contract:
 * doc/AI-COMPILER-SEARCH-M4-ATOMIC-HIGH-v1.0.md. Q1 guarded semantic form:
 * doc/AI-COMPILER-MLA-Q1-HIGH-OWNED-FACTS-v1.0.md. The driver owns buffer ABI,
 * source/device contexts and publication; rejected preflight never mutates. */
#include <cstdlib>
#include <sstream>
#include <set>
#include <stdexcept>
#include "tensor_kernel_lower.h"
#include "tensor_q1_program.h"
#include "tensor_workload.h"
#include "tensor_serving_schema.h"
#include "wn.h"
#include "wn_util.h"
#include "stab.h"
#include "pu_info.h"
#include "config.h"
#include "config_opt.h"
#include "targ_const.h"

namespace {
/* Create a fresh signed address-coordinate constant; extents were bounded. */
WN *VHO_high_integer(INT64 value) { return WN_Intconst(MTYPE_I8, value); }
/* Retain exact source F32 bits, including infinities used by stable Softmax. */
WN *VHO_high_float(UINT32 bits) {
    float value; memcpy(&value, &bits, 4);
    return WN_Floatconst(MTYPE_F4, value);
}
/* Read one local/formal in its declared scalar type. */
WN *VHO_high_load(ST *symbol) {
    return WN_Ldid(TY_mtype(ST_type(symbol)), 0, symbol, ST_type(symbol));
}
/* Create a local scalar with the algorithm's original source position. */
ST *VHO_high_local(const char *name, TYPE_ID type, SRCPOS source) {
    ST *symbol = New_ST(CURRENT_SYMTAB);
    ST_Init(symbol, Save_Str(name), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, MTYPE_To_TY(type));
    Set_ST_Srcpos(*symbol, source);
    return symbol;
}
/* An ordinary scalar assignment does not change the declared rounding order. */
WN *VHO_high_store(ST *symbol, WN *value) {
    return WN_Stid(TY_mtype(ST_type(symbol)), 0, symbol, ST_type(symbol), value);
}

class VHO_TENSOR_HIGH {
    const VHO_TENSOR_PROGRAM &_program;
    SRCPOS _source;
    std::map<UINT32, ST *> _buffers;
    std::vector<ST *> _indices;
    ST *_reduction, *_sum, *_next;
    std::map<UINT32,WN *> _constants;
    bool _selected, _quantized;
    ST *_status, *_integer, *_scan, *_match, *_rounded;
public:
    /* Borrow only validated detached facts and the active PU's source mapping. */
    VHO_TENSOR_HIGH(const VHO_TENSOR_PROGRAM &program, SRCPOS source, bool quantized=false)
        : _program(program), _source(source), _selected(false), _quantized(quantized) {}
    /* Reuse source F32 constants during a transaction; never extend TCON or
     * global constant-ST caches that cannot be safely truncated on rollback. */
    WN *Float(UINT32 bits) {
        if(!_selected) return VHO_high_float(bits);
        if(!_constants.count(bits)) throw std::runtime_error("missing_source_F32_constant");
        return WN_COPY_Tree(_constants[bits]);
    }
    /* Index immutable source constants before constructing a selected body. */
    void Constants(WN *node) {
        if(WN_operator(node)==OPR_CONST && WN_rtype(node)==MTYPE_F4)
            _constants[TCON_uval(STC_val(WN_st(node)))]=node;
        if(WN_operator(node)==OPR_BLOCK)
            for(WN *s=WN_first(node);s;s=WN_next(s)) Constants(s);
        else for(INT i=0;i<WN_kid_count(node);++i) if(WN_kid(node,i)) Constants(WN_kid(node,i));
    }
    /* Construct a row-major affine index from fresh coordinate expressions. */
    WN *Index(const std::vector<UINT64> &shape, const std::vector<INT> &axes) {
        WN *index = VHO_high_integer(0);
        UINT64 stride = 1;
        for (INT i = (INT)shape.size() - 1; i >= 0; --i) {
            WN *coordinate = axes[i] < 0 ? VHO_high_integer(0) :
                VHO_high_load(axes[i] == 16 ? _reduction : _indices[axes[i]]);
            index = WN_Add(MTYPE_I8, index,
                WN_Mpy(MTYPE_I8, coordinate, VHO_high_integer(stride)));
            stride *= shape[i];
        }
        return index;
    }
    /* Default output coordinates are already distinct signed loop variables. */
    WN *Output_index(const VHO_TENSOR_VALUE &value) {
        std::vector<INT> axes;
        for (UINT32 i = 0; i < value.shape.size(); ++i) axes.push_back(i);
        return Index(value.shape, axes);
    }
    /* NumPy broadcast axes are statically known; no DIV/REM hides affine access. */
    WN *Broadcast_index(UINT32 input, const VHO_TENSOR_VALUE &output) {
        const VHO_TENSOR_VALUE &value = _program.values[input];
        std::vector<INT> axes;
        for (UINT32 i = 0; i < value.shape.size(); ++i)
            axes.push_back(value.shape[i] == 1 ? -1 : (INT)(output.shape.size() - value.shape.size() + i));
        return Index(value.shape, axes);
    }
    /* A single explicit flat ARRAY retains the logical allocation and extent. */
    WN *Address(UINT32 value, WN *index) {
        const VHO_TENSOR_VALUE &v = _program.values[value];
        WN *array = WN_Create(OPR_ARRAY, Pointer_Mtype, MTYPE_V, 3);
        WN_element_size(array) = MTYPE_byte_size(Scalar_type(v));
        WN_kid0(array) = WN_Ldid(Pointer_Mtype, 0, _buffers[v.storage], ST_type(_buffers[v.storage]));
        WN_kid1(array) = VHO_high_integer(VHO_tensor_elements(v));
        WN_kid2(array) = index;
        return array;
    }
    /* Read the actual materialized input; no producer fusion/reassociation. */
    WN *Read(UINT32 value, WN *index) {
        TYPE_ID type = Scalar_type(_program.values[value]);
        return WN_Iload(type, 0, MTYPE_To_TY(type), Address(value, index));
    }
#include "tensor_high_quantized.inc"
    /* Use a strict ordered signed counted loop, with no GPU/thread coordinate. */
    WN *Loop(ST *iv, UINT64 extent, WN *body) {
        return WN_CreateDO(WN_CreateIdname(0, iv), VHO_high_store(iv, VHO_high_integer(0)),
            WN_LT(MTYPE_I8, VHO_high_load(iv), VHO_high_integer(extent)),
            VHO_high_store(iv, WN_Add(MTYPE_I8, VHO_high_load(iv), VHO_high_integer(1))), body, NULL);
    }
    /* Expand one semantic expression, inserting ordered reduction statements. */
    WN *Expression(const VHO_TENSOR_OPERATION &operation, WN *body) {
        const VHO_TENSOR_VALUE &output = _program.values[operation.output];
        UINT32 input = operation.inputs[0];
        const VHO_TENSOR_VALUE &a = _program.values[input];
        const std::string &name = operation.name;
        if (name == "copy") return Read(input, Output_index(output));
        if(_quantized && Is_quantized(operation)) return Quantized_expression(operation,body);
        if (name == "common.fill_like")
            return Float(strtoul(operation.attributes.find("attr.value_bits")->second.c_str(), NULL, 16));
        if (name == "common.transpose") {
            const char *text = operation.attributes.find("attr.permutation")->second.c_str();
            std::vector<INT> axes(a.shape.size());
            for (UINT32 i = 0; i < a.shape.size(); ++i) {
                char *end; UINT32 axis = strtoul(text, &end, 10); axes[axis] = i; text = *end ? end + 1 : end;
            }
            return Read(input, Index(a.shape, axes));
        }
        if (name == "common.matmul" || name == "common.reduce_sum" || name == "common.reduce_max") {
            bool maximum = name == "common.reduce_max";
            WN_INSERT_BlockLast(body, VHO_high_store(_sum, Float(maximum ? 0xff800000 : 0)));
            WN *fold = WN_CreateBlock(), *term;
            if (name == "common.matmul") {
                const VHO_TENSOR_VALUE &b = _program.values[operation.inputs[1]];
                std::vector<INT> aa, ba;
                for (UINT32 i = 0; i + 2 < a.shape.size(); ++i)
                    aa.push_back(a.shape[i] == 1 ? -1 : (INT)(output.shape.size() - a.shape.size() + i));
                aa.push_back(output.shape.size() - 2); aa.push_back(16);
                for (UINT32 i = 0; i + 2 < b.shape.size(); ++i)
                    ba.push_back(b.shape[i] == 1 ? -1 : (INT)(output.shape.size() - b.shape.size() + i));
                ba.push_back(16); ba.push_back(output.shape.size() - 1);
                UINT32 right=operation.inputs[1];
                /* The sealed MLA value projection owns this transpose. Keep
                 * its static index permutation affine and fuse it into the
                 * projection instead of reading an unproduced scratch view. */
                if(_selected && operation.output==_program.operations[37].output) {
                    right=_program.operations[36].inputs[0];
                    std::swap(ba[ba.size()-1],ba[ba.size()-2]);
                }
                term = WN_Mpy(MTYPE_F4, Read(input, Index(a.shape, aa)),
                    Read(right, Index(_program.values[right].shape, ba)));
            } else {
                term = Read(input, WN_Add(MTYPE_I8,
                    WN_Mpy(MTYPE_I8, Output_index(output), VHO_high_integer(a.shape.back())),
                    VHO_high_load(_reduction)));
            }
            WN_INSERT_BlockLast(fold, VHO_high_store(_next, term));
            WN *combined = maximum ? WN_Select(MTYPE_F4,
                WN_LIOR(WN_NE(MTYPE_F4, VHO_high_load(_next), VHO_high_load(_next)),
                    WN_GT(MTYPE_F4, VHO_high_load(_next), VHO_high_load(_sum))),
                VHO_high_load(_next), VHO_high_load(_sum)) :
                WN_Add(MTYPE_F4, VHO_high_load(_sum), VHO_high_load(_next));
            WN_INSERT_BlockLast(fold, VHO_high_store(_sum, combined));
            WN_INSERT_BlockLast(body, Loop(_reduction, a.shape.back(), fold));
            return VHO_high_load(_sum);
        }
        std::vector<WN *> args;
        for (UINT32 i = 0; i < operation.inputs.size(); ++i)
            args.push_back(Read(operation.inputs[i], Broadcast_index(operation.inputs[i], output)));
        if (name == "common.exp" || name == "common.log") {
            WN *arg = WN_CreateParm(MTYPE_F4, args[0], MTYPE_To_TY(MTYPE_F4), WN_PARM_BY_VALUE);
            return WN_Create_Intrinsic(OPR_INTRINSIC_OP, MTYPE_F4, MTYPE_V,
                name == "common.exp" ? INTRN_F4EXP : INTRN_F4LOG, 1, &arg);
        }
        if (name == "common.isfinite") return WN_LE(MTYPE_F4, WN_Abs(MTYPE_F4, args[0]), Float(0x7f7fffff));
        if (name == "common.logical_not") return WN_EQ(MTYPE_U4, args[0], WN_Intconst(MTYPE_U4, 0));
        if (name == "common.select") return WN_Select(MTYPE_F4, args[0], args[1], args[2]);
        if (name == "common.greater") return WN_GT(MTYPE_F4, args[0], args[1]);
        if (name == "common.add") return WN_Add(MTYPE_F4, args[0], args[1]);
        if (name == "common.sub") return WN_Sub(MTYPE_F4, args[0], args[1]);
        if (name == "common.mul") return WN_Mpy(MTYPE_F4, args[0], args[1]);
        return WN_Div(MTYPE_F4, args[0], args[1]);
    }
    /* Propagate source positions without reusing WN children or source tables. */
    void Position(WN *node) {
        if (OPCODE_has_next_prev(WN_opcode(node))) WN_Set_Linenum(node, _source);
        if (WN_operator(node) == OPR_BLOCK)
            for (WN *s = WN_first(node); s; s = WN_next(s)) Position(s);
        else for (INT i = 0; i < WN_kid_count(node); ++i) if (WN_kid(node, i)) Position(WN_kid(node, i));
    }
#include "tensor_high_selected.inc"
    /* Commit faithful semantic expansion after complete preflight. Buffer
     * formals are disjoint storage allocations; reshape values share a formal. */
    void Materialize() {
        ST *function = &St_Table[PU_Info_proc_sym(Current_PU_Info)];
        Set_ST_export(*function, EXPORT_PREEMPTIBLE);
        Set_ST_Srcpos(*function, _source);
        TY_IDX type; TY &ty = New_TY(type); TY_Init(ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0); Set_TY_has_prototype(type);
        TYLIST_IDX list; Set_TYLIST_type(New_TYLIST(list), MTYPE_To_TY(MTYPE_V)); Set_TY_tylist(ty, list);
        std::vector<ST *> formals;
        for (UINT32 i = 0; i < _program.values.size(); ++i) {
            const VHO_TENSOR_VALUE &value = _program.values[i];
            if (value.storage != i) continue;
            TY_IDX pointer = Make_Pointer_Type(MTYPE_To_TY(Scalar_type(value)));
            Set_TY_is_restrict(pointer);
            std::ostringstream name; name << "storage_" << i;
            ST *formal = New_ST(CURRENT_SYMTAB);
            ST_Init(formal, Save_Str(name.str().c_str()), CLASS_VAR, SCLASS_FORMAL, EXPORT_LOCAL, pointer);
            Set_ST_is_value_parm(formal); Set_ST_Srcpos(*formal, _source);
            /* Scalar buffer ABI projects the sealed semantic recipe's role;
             * do not change pointer TY identity or invent a cache lifetime. */
            if(!value.cache_trait.empty())
                ST_tensor_bind_metadata(ST_st_idx(formal), "tensor.cache.trait", value.cache_trait.c_str());
            if(_quantized) {
                ST_tensor_bind_metadata(ST_st_idx(formal),"tensor.q1.value",value.name.c_str());
                ST_tensor_bind_metadata(ST_st_idx(formal),"tensor.q1.encoding",value.cache_encoding.c_str());
                ST_tensor_bind_metadata(ST_st_idx(formal),"tensor.q1.scale",value.cache_scale.c_str());
                ST_tensor_bind_metadata(ST_st_idx(formal),"tensor.q1.evidence",value.cache_evidence.c_str());
            }
            _buffers[i] = formal; formals.push_back(formal); Set_TYLIST_type(New_TYLIST(list), pointer);
        }
        if(_quantized) {
            TY_IDX pointer=Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4));Set_TY_is_restrict(pointer);
            _status=New_ST(CURRENT_SYMTAB);ST_Init(_status,Save_Str("q1_status"),CLASS_VAR,SCLASS_FORMAL,EXPORT_LOCAL,pointer);
            Set_ST_is_value_parm(_status);Set_ST_Srcpos(*_status,_source);
            formals.push_back(_status);Set_TYLIST_type(New_TYLIST(list),pointer);
        }
        Set_TYLIST_type(New_TYLIST(list), TY_IDX_ZERO); Set_PU_prototype(Pu_Table[ST_pu(function)], type);
        for (UINT32 i = 0; i < 16; ++i) {
            std::ostringstream name; name << "axis_" << i;
            _indices.push_back(VHO_high_local(name.str().c_str(), MTYPE_I8, _source));
        }
        _reduction = VHO_high_local("contribution", MTYPE_I8, _source);
        _sum = VHO_high_local("ordered_sum", MTYPE_F4, _source);
        _next = VHO_high_local("next_value", MTYPE_F4, _source);
        WN *body = WN_CreateBlock();
        if(_quantized) {
            _integer=VHO_high_local("checked_integer",MTYPE_I8,_source);
            _scan=VHO_high_local("update_scan",MTYPE_I8,_source);
            _match=VHO_high_local("update_match",MTYPE_I8,_source);
            _rounded=VHO_high_local("rounded_f32",MTYPE_F4,_source);
            Status(body,0);
        }
        for (UINT32 i = 0; i < _program.operations.size(); ++i) {
            const VHO_TENSOR_OPERATION &op = _program.operations[i];
            if (op.name == "common.reshape") continue;
            if(_quantized) {
                for(unsigned axis=0;axis<_program.values[op.output].shape.size();++axis) {
                    std::ostringstream name,id;name<<"q1_op_"<<i<<"_axis_"<<axis;id<<i;
                    _indices[axis]=VHO_high_local(name.str().c_str(),MTYPE_I8,_source);
                    ST_tensor_bind_metadata(ST_st_idx(_indices[axis]),"tensor.q1.operation",id.str().c_str());
                }
            }
            USRCPOS position; USRCPOS_srcpos(position) = _source;
            if (op.source_line) USRCPOS_linenum(position) = op.source_line;
            _source = USRCPOS_srcpos(position);
            const VHO_TENSOR_VALUE &output = _program.values[op.output];
            std::ostringstream explanation;
            explanation << "Common High operation " << i << ": " << op.name
                << "; materialized storage " << output.storage
                << ". Signed zero-based unit loops and " << (_quantized?"explicit ARRAY indexing (page accesses may be indirect)":"affine ARRAY indexing")
                << "; reductions retain ordered F32 folds."
                << " No GPU mapping or schedule search. Reshape aliases reuse caller storage.";
            WN *comment = WN_CreateComment(explanation.str().c_str());
            WN_Set_Linenum(comment, _source); WN_INSERT_BlockLast(body, comment);
            if(_quantized) {
                std::ostringstream contract;contract<<"Q1 semantic lowering: exact byte sign/zero extension, promoted arithmetic; "
                    <<"gather bounds, checked index arithmetic, integral byte conversion and unique fresh updates guard before unsafe operations. "
                    <<"Status 0 succeeds, 1 refuses input; scratch may be partial on refusal. No serving phase, allocator lifetime, GPU or reuse permission is inferred.";
                WN_INSERT_BlockLast(body,WN_CreateComment(contract.str().c_str()));
            }
            WN *work = WN_CreateBlock(); WN *value = Expression(op, work);
            TYPE_ID dtype = Scalar_type(output);
            WN_INSERT_BlockLast(work, WN_Istore(dtype, 0, Make_Pointer_Type(MTYPE_To_TY(dtype)), Address(op.output, Output_index(output)), value));
            for (INT axis = (INT)output.shape.size() - 1; axis >= 0; --axis) {
                WN *outer = WN_CreateBlock(); WN_INSERT_BlockLast(outer, Loop(_indices[axis], output.shape[axis], work)); work = outer;
            }
            Position(work); WN_INSERT_BlockLast(body, work);
        }
        if(_quantized) {
            // COMMENT owns a NAME ST. Allocate it after executable declarations
            // so adding an explanation does not renumber prior loop symbols.
            WN *explanation=WN_CreateComment(VHO_tensor_workload_explanation(_program).c_str());
            WN_Set_Linenum(explanation,_source);WN_INSERT_BlockBefore(body,WN_first(body),explanation);
            WN *serving=WN_CreateComment(VHO_tensor_serving_explanation(_program).c_str());
            WN_Set_Linenum(serving,_source);WN_INSERT_BlockBefore(body,WN_first(body),serving);
        }
        WN_INSERT_BlockLast(body, WN_Create(OPR_RETURN, MTYPE_V, MTYPE_V, 0));
        WN *entry = WN_CreateEntry(formals.size(), function, body, WN_CreateBlock(), WN_CreateBlock());
        for (UINT32 i = 0; i < formals.size(); ++i) WN_formal(entry, i) = WN_CreateIdname(0, formals[i]);
        WN_Set_Linenum(entry, _source); Set_PU_Info_tree_ptr(Current_PU_Info, entry);
        ST_tensor_bind_metadata(ST_st_idx(function), "tensor.high.recipe_key", (_quantized?VHO_tensor_quantized_high_key(_program):VHO_tensor_recipe_key(_program)).c_str());
        if(_quantized) ST_tensor_bind_metadata(ST_st_idx(function),"tensor.q1.algorithm",VHO_q1_program_encode(_program).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function), "tensor.high.schema", _quantized?"quantized-common-high-v1":"common-high-v1");
        ST_tensor_bind_metadata(ST_st_idx(function), "tensor.high.buffer_contract", "disjoint-storage-formals");
    }
};
}

/* Reject physical schedules, unsafe FP controls and nonempty/wrong contexts
 * before allocation. This source-level scalar algorithm is not a candidate
 * transformation: only the driver can request and publish its semantic image. */
BOOL VHO_tensor_materialize_high(const VHO_TENSOR_PROGRAM &program, SRCPOS source, FILE *diagnostic) {
    if (!VHO_tensor_validate_program(program, diagnostic) || program.fuse || program.cooperative ||
        program.split_width || program.search_selected || program.auto_chunks ||
        !source || !SRCPOS_filenum(source) || SRCPOS_linenum(source) <= 0 ||
        !Current_PU_Info || Current_pu != &PU_Info_pu(Current_PU_Info) ||
        OPT_Fast_Math || Roundoff_Level != ROUNDOFF_NONE || IEEE_Arithmetic != IEEE_ACCURATE)
        return FALSE;
    WN *entry = PU_Info_tree_ptr(Current_PU_Info);
    if (!entry || WN_operator(entry) != OPR_FUNC_ENTRY || WN_num_formals(entry) ||
        WN_first(WN_func_body(entry)) || ST_device_symbol_kind(St_Table[PU_Info_proc_sym(Current_PU_Info)]) != DEVICE_SYMBOL_NONE)
        return FALSE;
    VHO_TENSOR_HIGH(program, source).Materialize();
    return TRUE;
}

/* Stage one selected semantic High body in the active source ABI. Legality and
 * scratch capacity are checked by the owning LNO transaction before this call;
 * all allocations belong to its savepoint. No native root is published here. */
WN *VHO_tensor_build_selected_high(const VHO_TENSOR_PROGRAM &program, WN *scope) {
    return VHO_TENSOR_HIGH(program,WN_Get_Linenum(scope)).Selected_body(scope);
}

/* Opt-in unoptimized semantic expansion, with the same empty-PU and strict-FP
 * preflight as legacy High. Reject before allocation; caller owns discarded
 * target scope on exceptional failure and publishes only verified images. */
BOOL VHO_tensor_materialize_quantized_high(const VHO_TENSOR_PROGRAM &program,SRCPOS source,FILE *diagnostic) {
    if(!VHO_tensor_validate_quantized_high(program,diagnostic) ||
       !source || !SRCPOS_filenum(source) || SRCPOS_linenum(source)<=0 ||
       !Current_PU_Info || Current_pu!=&PU_Info_pu(Current_PU_Info) ||
       OPT_Fast_Math || Roundoff_Level!=ROUNDOFF_NONE || IEEE_Arithmetic!=IEEE_ACCURATE) return FALSE;
    WN *entry=PU_Info_tree_ptr(Current_PU_Info);
    if(!entry || WN_operator(entry)!=OPR_FUNC_ENTRY || WN_num_formals(entry) || WN_first(WN_func_body(entry)) ||
       ST_device_symbol_kind(St_Table[PU_Info_proc_sym(Current_PU_Info)])!=DEVICE_SYMBOL_NONE) return FALSE;
    VHO_TENSOR_HIGH(program,source,true).Materialize();return TRUE;
}
