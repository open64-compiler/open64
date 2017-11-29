/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement
  or the like.  Any license provided herein, whether implied or
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with
  other software, or any other product whatsoever.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
#endif /* _KEEP_RCS_ID */

#include <stdarg.h>
#include "defs.h"
#include "config_targ_opt.h"
#include "errors.h"
#include "mempool.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "tn_map.h"
#include "cg_loop.h"
#include "cg.h"
#include "cgexp.h"
#include "register.h"
#include "cg_region.h"
#include "wn.h"
#include "region_util.h"
#include "op_list.h"
#include "cgprep.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cg_db_op.h"
#include "whirl2ops.h"
#include "cgtarget.h"
#include "gra_live.h"
#include "reg_live.h"
#include "cflow.h"
#include "cg_spill.h"
#include "cgexp_internals.h"
#include "data_layout.h"
#include "stblock.h"
#include "cxx_hash.h"
#include "op.h"

#include "ebo.h"
#include "ebo_info.h"
#include "ebo_special.h"
#include "ebo_util.h"
#include "cgtarget.h"

#include "config_lno.h"

/* Define a macro to strip off any bits outside of the left most 4 bytes. */
#define TRUNC_32(val) (val & 0x00000000ffffffffll)

/* ===================================================================== */

typedef HASH_TABLE<ST_IDX, INITV_IDX> ST_TO_INITV_MAP;
static ST_TO_INITV_MAP *st_initv_map = NULL;
static BOOL st_initv_map_inited = FALSE;
static GTN_SET *work_gtn_set = NULL;
static BS *work_defined_set = NULL;
static MEM_POOL *work_pool = NULL;
static INT32 fixed_branch_cost, taken_branch_cost;

static BOOL Convert_Imm_And(OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo);
static BOOL Convert_Imm_Mul(OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo);
static BOOL Convert_Imm_Or(OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo);
static BOOL Convert_Imm_Add(OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo);
static BOOL Convert_Imm_Xor(OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo);
static BOOL Convert_Imm_Cmp(OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo);

enum ADDR_MODE { REG_MODE = 0,     /* reg                      */
                 BASE_MODE,	   /* offset(base)             */
                 BASE_INDEX_MODE,  /* offset(base,index,scale) */
                 INDEX_MODE,       /* offset(,index,scale)     */
                 N32_MODE,         /* offset                   */
                 UNDEFINED_MODE	   /* end marker               */
               };

static void Init_Addr_Modes();

/* Initialize and finalize ebo special routines. */
void
EBO_Special_Start(MEM_POOL *pool)
{
    st_initv_map = CXX_NEW(ST_TO_INITV_MAP(31, pool), pool);
    st_initv_map_inited = FALSE;
    work_gtn_set = GTN_SET_Create_Empty(Last_TN + 1, pool);
    work_defined_set = BS_Create_Empty(Last_TN + 1, pool);
    work_pool = pool;

    INT32 idummy;
    double ddummy;
    CGTARG_Compute_Branch_Parameters(&idummy, &fixed_branch_cost,
                                     &taken_branch_cost, &ddummy);
    Init_Addr_Modes();
}

void
EBO_Special_Finish(void)
{
    st_initv_map = NULL;
    st_initv_map_inited = FALSE;
    work_gtn_set = NULL;
    work_defined_set = NULL;
    work_pool = NULL;
}

/*
 * Identify OP's that contain a constant and operate in a way that
 * will allow the constant to be added into an offset field of
 * a load or store instruction.
 */
BOOL EBO_Can_Merge_Into_Offset(OP *op)
{
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_CAN_MERGE_INTO_OFFSET)) return FALSE;
#endif
    TOP top = OP_code(op);

    /* Add ior to the candidates for EBO optimization */
    if (!OP_iadd(op) && !OP_isub(op) && !(OP_ior(op)))
        return FALSE;

    if ((op == BB_entry_sp_adj_op(OP_bb(op))) ||
            (op == BB_exit_sp_adj_op(OP_bb(op))))
        return FALSE;

    TN *tn = OP_opnd(op,1);
    if (TN_Is_Constant(tn))
        return TRUE;

    tn = OP_opnd(op,2);
    if (TN_Is_Constant(tn))
        return TRUE;

    return FALSE;
}


static
void
EBO_Set_OP_omega(OP *op, ...)
{
    INT opnds = OP_opnds(op);
    INT i;
    va_list tninfos;

    va_start(tninfos, op);
    CG_LOOP_Init_Op(op);
    for (i = 0; i < opnds; i++) {
        /* the copy-op in GODSON2 is not the same with that in ITANIUM
        * it has one source tn which may be a constant
        * for a constant tn, do not set omega for it.
        */
        if ((OP_opnd(op,i)==Zero_TN)) {
            EBO_TN_INFO *tninfo = va_arg(tninfos, EBO_TN_INFO *);
            continue;
        }
        EBO_TN_INFO *tninfo = va_arg(tninfos, EBO_TN_INFO *);
        Set_OP_omega(op, i, ((tninfo != NULL) ? tninfo->omega : 0));
    }

    va_end(tninfos);
    return;
}



static
void
EBO_Copy_OP_omega(OP *new_op, OP *old_op)
{
    INT opnds = OP_opnds(new_op);
    INT i;

    CG_LOOP_Init_Op(new_op);
    for (i = 0; i < opnds; i++) {
        Set_OP_omega(new_op, i, OP_omega(old_op,i));
    }

    return;
}


static
void
EBO_OPS_omega(OPS *ops, EBO_TN_INFO *pred_tninfo)
{
    OP *next_op = OPS_first(ops);
    while (next_op != NULL) {
        INT opnds = OP_opnds(next_op);
        INT i;

        CG_LOOP_Init_Op(next_op);
        for (i = 0; i < opnds; i++) {
            Set_OP_omega(next_op, i, 0);
        }
        if (OP_has_predicate(next_op) && (pred_tninfo != NULL)) {
            Set_OP_omega(next_op, OP_PREDICATE_OPND, pred_tninfo->omega);
        }
        next_op = OP_next(next_op);
    }

    return;
}


BOOL Combine_L1_L2_Prefetches(OP* op, TN** opnd_tn, EBO_TN_INFO** opnd_tninfo)
{
    return FALSE;
}


BOOL
combine_adjacent_loads(OP *op,
                       EBO_TN_INFO **opnd_tninfo,
                       EBO_OP_INFO *opinfo,
                       INT64 offset_pred,
                       INT64 offset_succ)
{
    return FALSE;
}

static void
Expand_Extract_Bits(TYPE_ID rtype, TYPE_ID desc, UINT bit_offset,
                    UINT bit_size,
                    TN *tgt_tn, TN *src_tn, OPS *ops)
{
}

struct SIZE_EXT_INFO {
    mUINT8 src_size;  // measured in bytes
    mUINT8 dest_size; // measured in bytes
    bool sign_ext;
};

#define SET_SIZE_EXT_INFO(o,s,d,sign) \
do {                                  \
  (o)->src_size = s;                  \
  (o)->dest_size = d;                 \
  (o)->sign_ext = sign;               \
} while(0)

static BOOL Get_Size_Ext_Info(TOP top, SIZE_EXT_INFO* info)
{

    switch (top) {
    case TOP_lw:
    case TOP_ll:
        SET_SIZE_EXT_INFO(info, 4, 4, true);
        break;
    case TOP_lwl:
    case TOP_lwr:
        SET_SIZE_EXT_INFO(info, 4, 2, true);
        break;
    case TOP_lwu:
        SET_SIZE_EXT_INFO(info, 4, 4, false);
        break;
    case TOP_ld:
    case TOP_lld:
        SET_SIZE_EXT_INFO(info, 8, 8, false);
        break;
    case TOP_ldl:
    case TOP_ldr:
        SET_SIZE_EXT_INFO(info, 8, 4, true);
        break;
    case TOP_lh:
        SET_SIZE_EXT_INFO(info, 4, 2, true);
        break;
    case TOP_lhu:
        SET_SIZE_EXT_INFO(info, 4, 2, false);
        break;
    case TOP_lb:
        SET_SIZE_EXT_INFO(info, 4, 1, true);
        break;
    case TOP_lbu:
        SET_SIZE_EXT_INFO(info, 4, 1, false);
        break;
    case TOP_ldc1:
        SET_SIZE_EXT_INFO(info, 8, 8, false);
        break;
    default:
        return FALSE;
    }
    return TRUE;
}

BOOL
delete_subset_mem_op(OP *op,
                     EBO_TN_INFO **opnd_tninfo,
                     EBO_OP_INFO *opinfo,
                     INT64 offset_pred,
                     INT64 offset_succ)
{
    return FALSE;
}

/*
 * delete_reload_across_dependency
 *
 * For a given load or store and one it matches,
 * attempt to replace one of them.
 * Return TRUE if this op is no longer needed.
 */
BOOL
delete_reload_across_dependency(OP *op,
                                EBO_TN_INFO **opnd_tninfo,
                                EBO_OP_INFO *opinfo,
                                EBO_OP_INFO *intervening_opinfo)
{
    return FALSE;
}

// Optimize store-load sequence.  STORE_OPINFO specifies the store.  LOAD_OP is
// the load, with LOAD_ACTUAL_TNINFO and LOAD_OPND_TNINFO describing the actual
// and optimal operands, respectively.  Both the store and the load access the
// same memory location.
static BOOL
Special_Store_Load_Sequence(OP *load_op,
                            EBO_TN_INFO **load_actual_tninfo,
                            EBO_TN_INFO **load_opnd_tninfo,
                            EBO_OP_INFO *store_opinfo)
{
    return FALSE;
}


/*
 * delete_memory_op
 *
 * For a given load or store and one it matches,
 * attempt to replace one of them.
 * Return TRUE if this op is no longer needed.
 */
static
BOOL
delete_memory_op(OP *op,
                 EBO_TN_INFO **actual_tninfo,
                 EBO_TN_INFO **opnd_tninfo,
                 EBO_OP_INFO *opinfo)
{
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_DELETE_MEMORY_OP)) return FALSE;
#endif
    OPS ops = OPS_EMPTY;
    INT size_pred;
    INT size_succ;
    TN *predicate_tn = OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL;
    /* In each case below, before attempting to remove a load, store, or
       prefetch, we must make sure the instruction does not side-effect
       any state, etc. If it does, we can't remove it. This check is
       needed in addition to our general mechanism of making all state
       appear live on exit to the function. */

    ISA_REGISTER_CLASS state_rc = ISA_REGISTER_CLASS_UNDEFINED;
    if (state_rc != ISA_REGISTER_CLASS_UNDEFINED)
    {
        INT results = OP_results(op);
        for (UINT i = 0; i < results; i++)
        {
            TN *tn = OP_result(op, i);
            if (TN_is_register(tn) && (TN_register_class(tn) == state_rc))
                return FALSE;
        }
    }

    /* Remove the second OP for:
       Prefetch - Prefetch,
       Load - Prefetch,
       Store - Prefetch
    */
    if (OP_prefetch(op))
    {
        if (EBO_Trace_Optimization) {
            fprintf(TFile,"%sRemove following Prefetch combination\n",EBO_trace_pfx);
        }

        return TRUE;
    }

    /* Don't optimize:
       Prefetch - Load,
       Prefetch - Store,
    */
    if (OP_prefetch(opinfo->in_op))
    {
        return FALSE;
    }

    /* Don't try to optimize unaligned or unknown accesses. */
    if (OP_unalign_mem(op) || OP_unalign_mem(opinfo->in_op))
        return FALSE;

    /* Don't try to optimize the memory-access pair with different signedness.*/
    if (OP_unsigned(op) != OP_unsigned(opinfo->in_op))
        return FALSE;

    size_pred = CGTARG_Mem_Ref_Bytes(opinfo->in_op);
    size_succ = CGTARG_Mem_Ref_Bytes(op);

    /* Replace the result tn of the second OP for:
       Load - Load,
    */
    if (OP_load(op) && OP_load(opinfo->in_op))
    {
        /* Make sure the result TNs' regclasses and ops match. */

        if (TN_register_class(OP_result(op,0)) !=
                TN_register_class(OP_result(opinfo->in_op, 0)))
        {
            if (EBO_Trace_Optimization)
                fprintf(TFile,"%sRegclass mismatch for Load - Load combination\n",
                        EBO_trace_pfx);

            return FALSE;
        }

        /* This check should be moved after the subset check below, since
           here currently prevents us reaching the subsetting... (but too
           close to release to do it now...). */
        if (OP_code(op) != OP_code(opinfo->in_op))
        {
            if (EBO_Trace_Optimization)
                fprintf(TFile,"%sMtype mismatch for Load - Load combination\n",
                        EBO_trace_pfx);

            return FALSE;
        }

        /* If the size of the data item loaded by the two loads is
           different, but the starting memory address is the same.
           There is a chance that the predecessor load is a wider load
           and that the new load's data can be extracted.

           Notice that while performing 32-bit operations with a GPR result in
           64-bit mode, opteron will zero-extend the 32-bit result.
        */

        if ((size_pred != size_succ) ||
                (OP_results(op) != OP_results(opinfo->in_op)) ||
                (TN_size(OP_result(opinfo->in_op, 0)) != TN_size(OP_result(op, 0))))
        {
            /* Size of the data item loaded by the two loads is different,
               but the starting memory address is the same.  There is a chance
               that the predecessor load is a load-pair and that the new load
               matches one of the words that is loaded. */
            if (EBO_Trace_Optimization)
            {
                fprintf(TFile,"%sSize mismatch for Load - Load combination: %d:%d %d:%d \n",
                        EBO_trace_pfx,size_pred,size_succ,
                        TN_size(OP_result(opinfo->in_op, 0)),TN_size(OP_result(op, 0)));
            }

            return delete_subset_mem_op(op, opnd_tninfo, opinfo, 0, 0);
        }

        if (!EBO_in_peep &&
                (OP_bb(op) != OP_bb(opinfo->in_op)) &&
                !TN_Is_Constant(OP_result(opinfo->in_op, 0)) &&
                has_assigned_reg(OP_result(opinfo->in_op, 0)))
        {
            if (EBO_Trace_Data_Flow)
            {
                fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
                        EBO_trace_pfx);
            }

            return FALSE;
        }

        /* Remove the second load, and replace it with a copy of the first */

        if (EBO_Trace_Optimization)
        {
            fprintf(TFile,"%sRemove Load - Load combination\n",EBO_trace_pfx);
        }

        TOP top = OP_code(op);
        EBO_Exp_COPY(predicate_tn, OP_result(op, 0), OP_result(opinfo->in_op, 0), &ops);
        if (OP_results(op) == 2) {
            EBO_Exp_COPY(predicate_tn, OP_result(op, 1), OP_result(opinfo->in_op, 1), &ops);
        }
        if (EBO_in_loop)
            EBO_OPS_omega(&ops, (OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL));
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);

        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
    }
    /* Replace the result tn of the second OP for:
       Store - Load
    */
    else if (OP_load(op) && OP_store(opinfo->in_op))
    {
        if (OP_side_effects(opinfo->in_op)) {
            if (EBO_Trace_Optimization)
                fprintf(TFile,
                        "%sStore has side effects for Load - Store combination\n",
                        EBO_trace_pfx);

            return FALSE;
        }

        INT storeval_idx = OP_find_opnd_use(opinfo->in_op,OU_storeval);
        if (storeval_idx < 0)
        {
            if (EBO_Trace_Optimization)
                fprintf(TFile,
                        "%sStore value TN unknown for Load - Store combination\n",
                        EBO_trace_pfx);

            return FALSE;
        }

        TN *storeval_tn = OP_opnd(opinfo->in_op, storeval_idx);
        const int size_storeval = CGTARG_Mem_Ref_Bytes(opinfo->in_op);

        if ((TN_register_class(OP_result(op,0)) !=
                TN_register_class(storeval_tn)) ||
                (TN_is_fpu_int(OP_result(op,0)) !=
                 TN_is_fpu_int(storeval_tn))) {
            if (EBO_Trace_Data_Flow) {
#pragma mips_frequency_hint NEVER
                fprintf(TFile,"%sType mismatch for Store - Load combination\n",
                        EBO_trace_pfx);
            }
            return FALSE;
        }

        /* Make sure the storeval/result TNs' regclasses and mtypes
           match. It isn't sufficient to just check regclasses since
           user-defined operations for two ctypes in the same regfile can
           have different semantics. Make an exception for 32-bit
           loads/stores to the integer register file, since we know that
           they have the same semantics for both signed and unsigned. */

        if (TN_register_class(OP_result(op,0)) !=
                TN_register_class(storeval_tn))
        {
            if (Special_Store_Load_Sequence(op, actual_tninfo, opnd_tninfo, opinfo))
                return TRUE;

            if (EBO_Trace_Data_Flow)
                fprintf(TFile,"%sRegclass mismatch for Store - Load combination\n",
                        EBO_trace_pfx);

            return FALSE;
        }

        if (!EBO_in_peep &&
                (OP_bb(op) != OP_bb(opinfo->in_op)) &&
                !TN_Is_Constant(storeval_tn) &&
                has_assigned_reg(storeval_tn))
        {
            if (EBO_Trace_Data_Flow)
                fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
                        EBO_trace_pfx);

            return FALSE;
        }

        /* If the size of the data moved to and from memory is the same,
           but the size of the stored value is larger than the size of
           the value we want to load, then mask off the upper portion of
           the stored value and use that instead of the loaded value. */
        if (size_pred == size_succ)
        {
            if (size_storeval > size_succ)
            {
                if (EBO_Trace_Data_Flow)
                    fprintf(TFile,"%sSize mismatch for Store - Load combination: %d %d %d\n",
                            EBO_trace_pfx,size_pred,size_storeval,size_succ);

                return delete_subset_mem_op(op, opnd_tninfo, opinfo, 0, 0);
            }

            if (EBO_Trace_Optimization)
                fprintf(TFile,"%sRemove Store - Load combination\n",EBO_trace_pfx);

            TOP top = OP_code(op);
            struct SIZE_EXT_INFO op_size_ext_info;
            BOOL ret = Get_Size_Ext_Info(OP_code(op), &op_size_ext_info);
            if (!ret)
                return FALSE;
            if (!op_size_ext_info.sign_ext) {
                if (EBO_Trace_Optimization)
                    fprintf(TFile,"%sStore - Load unsigned combination: may need truncate the value\n",EBO_trace_pfx,size_succ);
                if (size_succ <4)
                    Build_OP(TOP_andi, OP_result(op,0), True_TN, storeval_tn,
                             Gen_Literal_TN(((1 << (size_succ * 8)) - 1) & 0xffff, 2), &ops);
                else if (size_succ == 4) {
                    Build_OP(TOP_dsll32, storeval_tn, True_TN, storeval_tn, Gen_Literal_TN(0, 2), &ops);
                    Build_OP(TOP_dsrl32, OP_result(op,0), True_TN, storeval_tn, Gen_Literal_TN(0, 2), &ops);
                }
                else
                    EBO_Exp_COPY(predicate_tn, OP_result(op, 0), storeval_tn, &ops);
            }
            else {
                if (size_succ <4) {
                    if (EBO_Trace_Optimization)
                        fprintf(TFile,"%sStore - Load signed combination: need sign-extend a value,no proper instruction yet\n",EBO_trace_pfx,size_succ);
                    return FALSE;
                }
                else if (size_succ == 4) {
                    Build_OP(TOP_addu, OP_result(op, 0), True_TN, storeval_tn, Zero_TN, &ops);
                }
                else
                    EBO_Exp_COPY(predicate_tn, OP_result(op, 0), storeval_tn, &ops);
            }

            if (EBO_in_loop) {
                CG_LOOP_Init_Op(OPS_first(&ops));
                EBO_Set_OP_omega(OPS_first(&ops),
                                 (OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL),
                                 opinfo->actual_opnd[storeval_idx]);
            }

            BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
            return TRUE;
        }
        /* The size of the memory accesses are different, but the starting
           memory address is the same.  There is a chance that the
           predecessor store is wider than the load. */
        else
        {
            return delete_subset_mem_op(op, opnd_tninfo, opinfo, 0, 0);
        }
    }
    /* Remove the first OP for:
       Store - Store
    */
    else if (OP_store(op) && OP_store(opinfo->in_op) &&
             (OP_bb(op) == OP_bb(opinfo->in_op)))
    {
        if (size_pred != size_succ)
            return FALSE;

        if (opinfo->op_must_not_be_removed)
            return FALSE;

        if (EBO_Trace_Optimization)
            fprintf(TFile,"%sRemove Store - Store combination\n",EBO_trace_pfx);

        remove_op(opinfo);
        OP_Change_To_Noop(opinfo->in_op);
        opinfo->in_op = NULL;
        opinfo->in_bb = NULL;
        return FALSE;
    } else if (OP_load(opinfo->in_op) && OP_store(op)) {
        /* The store may not be needed if the same value is put back for:
               Load - Store
        */
        INT storeval_idx = OP_find_opnd_use(op,OU_storeval);
        TN *storeval_tn = OP_opnd(op, storeval_idx);

        if (TN_register_class(OP_result(opinfo->in_op,0)) !=
                TN_register_class(storeval_tn)) {
            if (EBO_Trace_Data_Flow) {
#pragma mips_frequency_hint NEVER
                fprintf(TFile,"%sType mismatch for Load - Store combination\n",
                        EBO_trace_pfx);
            }
            return FALSE;
        }
        if ((TN_register_class(OP_result(opinfo->in_op,0)) !=
                TN_register_class(storeval_tn)) ||
                (TN_is_fpu_int(OP_result(opinfo->in_op,0)) !=
                 TN_is_fpu_int(storeval_tn))) {
            if (EBO_Trace_Data_Flow) {
#pragma mips_frequency_hint NEVER
                fprintf(TFile,"%sType mismatch for Load - Store combination\n",
                        EBO_trace_pfx);
            }
            return FALSE;
        }
        if (!EBO_in_peep &&
                (OP_bb(op) != OP_bb(opinfo->in_op)) &&
                !TN_Is_Constant(storeval_tn) &&
                has_assigned_reg(storeval_tn)) {
            if (EBO_Trace_Data_Flow) {
#pragma mips_frequency_hint NEVER
                fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
                        EBO_trace_pfx);
            }
            return FALSE;
        }

        if ((size_pred != size_succ) ||
                (TN_size(storeval_tn) != size_succ)) {

            if (EBO_Trace_Data_Flow) {
#pragma mips_frequency_hint NEVER
                fprintf(TFile,"%sSize mismatch for Load - Store combination: %d %d %d\n",
                        EBO_trace_pfx,size_pred,TN_size(storeval_tn),size_succ);
            }

            return FALSE;
        }

        if ((opnd_tninfo[storeval_idx] != NULL) &&
                (opnd_tninfo[storeval_idx]->in_op != NULL) &&
                (opinfo->in_op != NULL) &&
                (opnd_tninfo[storeval_idx]->in_op == opinfo->in_op)) {
            /* The Store is not needed! */

            if (EBO_Trace_Optimization) {
#pragma mips_frequency_hint NEVER
                fprintf(TFile,"%sRemove Load - Store combination\n",EBO_trace_pfx);
            }

            return TRUE;
        }
    }
    else {
        if (EBO_Trace_Optimization) {
            fprintf(TFile, "Load - Store combination is not optimized\n");
        }
        return FALSE;
    }
    return FALSE;
}


/*
 * delete_duplicate_op
 *
 * For a given op and one it matches, attempt to replace
 * one of them.
 * Return TRUE if this op is no longer needed.
 */
BOOL
delete_duplicate_op(OP *op,
                    EBO_TN_INFO **opnd_tninfo,
                    EBO_OP_INFO *opinfo,
                    EBO_TN_INFO **actual_tninfo)
{
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_DELETE_DUPLICATE_OP)) return FALSE;
#endif
    INT resnum;
    OPS ops = OPS_EMPTY;

    // integer compare ops can not be deleted because they
    // need to set the rflags once again.
    if (OP_icmp(op))
        return FALSE;

    if (EBO_Trace_Data_Flow) {
        fprintf(TFile,"%sDuplicate OP in BB:%d    ",EBO_trace_pfx,BB_id(OP_bb(op)));
        Print_OP_No_SrcLine(op);
        fprintf(TFile,"      Matches   OP in BB:%d    ",BB_id(opinfo->in_bb));
        Print_OP_No_SrcLine(opinfo->in_op);
    }

    /* Global TN's aren't supported at low levels of optimization. */

    if ((Opt_Level < 2) && (OP_bb(op) != opinfo->in_bb))
        return FALSE;

    /* Separate load/store processing, but logically it's just a special case. */
    if (OP_has_predicate(op) &&
            (OP_opnd(op,OP_PREDICATE_OPND) != OP_opnd(opinfo->in_op,OP_PREDICATE_OPND))) {
        OP *pred_op = opinfo->in_op;
        TN *predicate1_tn = OP_opnd(pred_op,OP_PREDICATE_OPND);
        TN *predicate2_tn = OP_opnd(op,OP_PREDICATE_OPND);
        EBO_TN_INFO *predicate1_info = opinfo->actual_opnd[OP_PREDICATE_OPND];
        EBO_TN_INFO *predicate2_info = opnd_tninfo[OP_PREDICATE_OPND];

        if (OP_store(pred_op) && OP_store(op)) {
            if (!EBO_predicate_dominates(predicate2_tn, predicate2_info,
                                         predicate1_tn, predicate1_info)) {
                if (EBO_Trace_Data_Flow) {
                    fprintf(TFile,"%sStores can not be combined because predicates do not match\n",EBO_trace_pfx);
                }
                return FALSE;
            }
        } else {
            if (!EBO_predicate_dominates(predicate1_tn, predicate1_info,
                                         predicate2_tn, predicate2_info)) {
                if (EBO_Trace_Data_Flow) {
                    fprintf(TFile,"%sExpressions can not be combined because predicates do not match\n",EBO_trace_pfx);
                }
                return FALSE;
            }
        }
    }

    if (OP_memory(op))
    {
        return delete_memory_op(op, actual_tninfo, opnd_tninfo, opinfo);
    }
    else if ((OP_results(op) == 2) &&
             ((OP_result(op,0) != NULL) &&
              ((OP_result(op,0) == True_TN) ||
               (TN_register_class(OP_result(op,0)) == ISA_REGISTER_CLASS_predicate)))) {
        return FALSE;
    } else   {
        /* Take good care of the rflags cases.
         */


        /* There is no easy way to copy FCC registers, so skip this optimization
         *  if the result is of register class FCC. */
        if (OP_has_predicate(op) &&
                (OP_opnd(op,OP_PREDICATE_OPND) != True_TN)) {
            /* Check for required copies of predicates that can not be supported. */

            for (resnum = 0; resnum < OP_results(op); resnum++) {
                if (TN_register_class(OP_result(op, resnum)) ==
                        ISA_REGISTER_CLASS_predicate) {
                    if (EBO_Trace_Data_Flow) {
                        fprintf(TFile,"%sStores can not be combined because predicate copies can not be predicated\n",
                                EBO_trace_pfx);
                    }
                    return FALSE;
                }
            }
        }

        /* Create copies of the result TN's. */
        TOP top = OP_code(op);
        /* Create copies of the result TN's. */

        for (resnum = 0; resnum < OP_results(op); resnum++) {
            TN *rslt = OP_result(op, resnum);
            if (TN_register_class(rslt) == ISA_REGISTER_CLASS_hilo) {
                /* need not copy for HI_TN and LO_TN if redundent mipsmultu exsists.
                   Register hi and lo is not writable but only readable, which means
                   they keep value unless a new mipsmultu occurs,so that copy from
                   LO_TN to LO_TN and HI_TN to HI_TN is needless and un-accaptable.*/
                break;
            } else {
                if (TN_register_class(rslt) == ISA_REGISTER_CLASS_predicate) {
                    FmtAssert(FALSE, ("unimplemented"));
                } else {
                    EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                                 rslt, OP_result(opinfo->in_op, resnum), &ops);
                }
                OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
            }
        }//end for

        if (EBO_in_loop)
            EBO_OPS_omega(&ops, OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL);

        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
    }
}


/* Return the opcode for <op> that can take <imm_val>
   as its <opnd>th operand.
*/
static TOP TOP_with_Imm_Opnd(OP* op, int opnd, INT64 imm_val)
{
    const TOP top = OP_code(op);
    const ISA_OPERAND_INFO* oinfo = ISA_OPERAND_Info(top);
    const ISA_OPERAND_VALTYP* vtype = ISA_OPERAND_INFO_Operand(oinfo, 1);

    if (ISA_OPERAND_VALTYP_Is_Literal(vtype))
        return TOP_UNDEFINED;

    //const ISA_LIT_CLASS lc = ISA_OPERAND_VALTYP_Literal_Class(vtype);
    if (!ISA_LC_Value_In_Class(imm_val, LC_i32))
        return TOP_UNDEFINED;

    return CGTARG_Immed_To_Reg(top);
}


/* Attempt to convert an add of 'tn' + 'imm_val' into an addi. Return
   TRUE if we succeed, FALSE otherwise. */
static BOOL
Convert_Imm_Add(OP *op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo)
{
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_ADD)) return FALSE;
#endif
    OP *new_op = NULL;
    OPS ops = OPS_EMPTY;
    TOP new_opcode;
    BOOL is_64bit = (TN_size(tnr) == 8);
    if (imm_val == 0) {
        EBO_Exp_COPY(NULL, tnr, tn, &ops);
        BB_Insert_Ops_After(OP_bb(op), op, &ops);
        return TRUE;
    }

    return FALSE;

}


/*
 * Look at an exression that has a constant first operand and attempt to
 * simplify the computations.
 */
BOOL
Constant_Operand0(OP *op,
                  TN **opnd_tn,
                  EBO_TN_INFO **opnd_tninfo)
{
    /* In mips64 or mips3, there is no such kind of instructions whose
       first operand is constant
     */
    return FALSE;
}


/* Attempt to convert an int and of 'tn' & '0xffff' into a move ext. Return
   TRUE if we succeed, FALSE otherwise.
*/
static BOOL Convert_Imm_And(OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo)
{
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_AND)) return FALSE;
#endif
    TOP new_top = TOP_UNDEFINED;
    OPS ops = OPS_EMPTY;

    /* First, handle special cases. */

    if (((imm_val == 0xffff)&& (TN_size(tnr) <=4))||
            ((imm_val == 0xffffffff) &&(TN_size(tnr) <=8))) {
        Exp_COPY(tnr, tn,&ops);
        return TRUE;
    }
    return FALSE;
}


static BOOL Convert_Imm_Or(OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo)
{
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_OR)) return FALSE;
#endif
    OPS ops = OPS_EMPTY;

    return FALSE;
}


static BOOL Convert_Imm_Xor(OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo)
{
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_XOR)) return FALSE;
#endif
    OPS ops = OPS_EMPTY;

    return FALSE;
}


static BOOL Convert_Imm_Cmp(OP* op, TN *tnr, TN *tn, INT64 imm_val,
                            EBO_TN_INFO *tninfo)
{
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_CMP)) return FALSE;
#endif
    const TOP new_top = TOP_with_Imm_Opnd(op, 1, imm_val);

    return FALSE;

}


/* Attempt to convert an int mul of 'tn' * 'imm_val' into a shift. Return
   TRUE if we succeed, FALSE otherwise.
*/
static BOOL Convert_Imm_Mul(OP *op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo)
{
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_MUL)) return FALSE;
#endif
    TN* tnr1 = (OP_results(op) == 2) ? OP_result(op, 1) : NULL;

    OP *new_op = NULL;
    const BOOL is_64bit = (TN_size(tnr) == 8);
    const TYPE_ID mtype = is_64bit ? MTYPE_I8 : MTYPE_I4;
    INT64 val = imm_val < 0 ? -imm_val : imm_val;
    OPS ops = OPS_EMPTY;
    if (0) {
        if (imm_val == 0) {
            Exp_Immediate(tnr, Gen_Literal_TN(0,4), false, &ops);
            if (tnr1 != NULL) {
                Exp_Immediate(tnr1, Gen_Literal_TN(0,4), false, &ops);
            }
            BB_Insert_Ops_After(OP_bb(op), op, &ops);
            return TRUE;
        }

        if (imm_val == 1) {
            Exp_COPY(tnr, tn, &ops);
            BB_Insert_Ops_After(OP_bb(op), op, &ops);
            return TRUE;
        }

        if (tnr1 != NULL)
            return FALSE;

        bool need_an_add = false;

        if (val >= 2 &&
                ((val-1) & (val-2)) == 0) {
            val--;
            need_an_add = true;
        }

        /* Check whether it can carry an imm opnd. */

        if ((val & (val - 1)) != 0) {
            const TOP new_top = TOP_with_Imm_Opnd(op, 1, imm_val);

            if (new_top == TOP_UNDEFINED)
                return FALSE;

            Build_OP(new_top, tnr, tn,
                     Gen_Literal_TN(imm_val,4),
                     &ops);
            BB_Insert_Ops_After(OP_bb(op), op, &ops);

            return TRUE;
        }

        if (TNs_Are_Equivalent(tnr, tn) && need_an_add) {
            if (TN_register(tn) != REGISTER_UNDEFINED)
                return FALSE;

            TN* tmp = Build_TN_Like(tn);
            tn = tmp;
        }

        int power = 0;
        while (val != 1) {
            power++;
            val >>= 1;
        }

        Expand_Shift(tnr, tn, Gen_Literal_TN(power, 4), mtype, shift_left, &ops);

        if (need_an_add) {
            Expand_Add(tnr, tnr, tn, mtype, &ops);
        }

        if (imm_val < 0)
            Expand_Neg(tnr, tnr, mtype, &ops);

        BB_Insert_Ops_After(OP_bb(op), op, &ops);
        return TRUE;
    }
    return FALSE;
}


/*
 * Look at an exression that has a constant second operand and attempt to
 * simplify the computations.
 */
BOOL
Constant_Operand1(OP *op,
                  TN **opnd_tn,
                  EBO_TN_INFO **opnd_tninfo)
{
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_CONSTANT_OPERAND1)) return FALSE;
#endif
    BB *bb = OP_bb(op);
    TOP opcode = OP_code(op);
    INT o0_idx = 0;
    INT o1_idx = (OP_opnds(op) > 2) ? OP_find_opnd_use(op, OU_opnd1) : -1;
    INT o2_idx = (OP_opnds(op) > 3) ? OP_find_opnd_use(op, OU_opnd2) : -1;

    /* Nothing to optimize if no operands... */
    if (OP_opnds(op) < 1)
        return FALSE;
    /* Not to optimize if op is a xfer */
    if (OP_xfer(op))
        return FALSE;

    if (EBO_Trace_Execution)
    {
        fprintf(TFile, "%sin BB:%d constant1 OP :- %s",
                EBO_trace_pfx, BB_id(OP_bb(op)), TOP_Name(OP_code(op)));
        for (UINT i = 0; i < OP_opnds(op); i++)
        {
            fprintf(TFile," ");
            Print_TN(opnd_tn[i],FALSE);
        }
        fprintf(TFile,"\n");
    }

    TN *tn1 = opnd_tn[o1_idx];
    TN *tn2 = opnd_tn[o2_idx];
    TN *tnr = OP_has_result(op) ? OP_result(op,0) : NULL;

    /* Don't mess with symbols. */
    if (TN_is_symbol(tn1))
        return FALSE;

    /* We should only be called if tn1 is constant and tn0 is not. */
    FmtAssert(TN_Is_Constant(tn2) && ((OP_opnds(op) > 2) || !TN_Is_Constant(tn1)),
              ("Constant_Operand1: Unexpected constant/non-constant operands"));

    /* For all the negative value whose TN_size
       is 4, the higher 32-bit is 0s due to the restriction of opteron.
    */
    const INT64 imm_val = TN_value(tn2);

    if (OP_iand(op))
        return Convert_Imm_And(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx]);

    if (OP_ior(op))
        return Convert_Imm_Or(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx]);

    if (OP_ixor(op))
        return Convert_Imm_Xor(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx]);

    if (opcode == TOP_add ||
            opcode == TOP_addi ||
            opcode == TOP_addiu ||
            opcode == TOP_addu)
        return Convert_Imm_Add(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx]);

    if (OP_imul(op))
        return Convert_Imm_Mul(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx]);

    if (OP_icmp(op))
        return Convert_Imm_Cmp(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx]);

    /*****************************************************************/
    /* Now, look for sequences ending in 'op' that can be optimized. */

    /* No opnd info if operand is constant. */
    if (opnd_tninfo[o1_idx] == NULL)
        return FALSE;

    OP *pred_op = opnd_tninfo[o1_idx]->in_op;
    if (pred_op == NULL)
        return FALSE;

    TOP pred_opcode = OP_code(pred_op);

    /* Look for a sequence of two addi that can be combined. */
    if (OP_iadd(op) && OP_iadd(pred_op))
    {
        INT ptn1_idx = o1_idx;
        INT ptn2_idx = o2_idx;
        TN *ptn1 = OP_opnd(pred_op, ptn1_idx);
        TN *ptn2 = OP_opnd(pred_op, ptn2_idx);

        if (TN_is_constant(ptn2) && !TN_is_symbol(ptn2))
        {
            EBO_OP_INFO *pred_opinfo = locate_opinfo_entry(opnd_tninfo[o1_idx]);
            EBO_TN_INFO *ptn1_tninfo = pred_opinfo->actual_opnd[ptn1_idx];

            if (EBO_tn_available(bb, ptn1_tninfo))
            {
                const INT64 new_val = imm_val + TN_value(ptn2);
                if (Convert_Imm_Add(op, tnr, ptn1, new_val, ptn1_tninfo))
                {
                    if (EBO_Trace_Optimization)
                        fprintf(TFile,"\tcombine immediate adds\n");

                    return TRUE;
                }
            }
        }
    }
    return FALSE;
}



/*
 * Look at a branch exression that has all constant operands and attempt to
 * evaluate the expression.
 *
 */
BOOL
Resolve_Conditional_Branch(OP *op, TN **opnd_tn)
{
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_RESOLVE_CONDITIONAL_BRANCH)) return FALSE;
#endif

    return FALSE;
}



/*
 * Look at an exression that has all constant operands and attempt to
 * evaluate the expression.
 * In LOONGSON, no op except ld can reference a data with memory location
 * directly . It is much easier to perform constant folding than on X8664
 * Supported operations are:
 *   add, sub, mult, and, or, xor, nor, sll, srl, slt
 */
BOOL
Fold_Constant_Expression(OP *op,
                         TN **opnd_tn,
                         EBO_TN_INFO **opnd_tninfo)
{
    /*TODO Maybe need examination in future, no comprehensive */
#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_FOLD_CONSTANT_EXPRESSION)) return FALSE;
#endif
    TOP opcode = OP_code(op);
    TN *tnr = OP_result(op,0);

    if (OP_opnds(op) == 0)
        return FALSE;

    /* Only attempt to do compile-time arithmetic on integers. */

    if (TN_register_class(tnr) != ISA_REGISTER_CLASS_integer)
        return FALSE;

    if (EBO_Trace_Execution)
    {
        fprintf(TFile, "%sin BB:%d Constant OP :- %s ",
                EBO_trace_pfx, BB_id(OP_bb(op)),TOP_Name(opcode));

        for (UINT i = 0; i < OP_opnds(op); i++)
        {
            fprintf(TFile," ");
            Print_TN(opnd_tn[i],TRUE);
        }

        fprintf(TFile,"\n");
    }

    /* in LOONGSON, the op should have at least 3 ops including predicate tn */
    if (OP_opnds(op) <=2)
        return FALSE;

    TN *tn0 = opnd_tn[OP_find_opnd_use(op, OU_opnd1)];
    TN *tn1 = opnd_tn[OP_find_opnd_use(op, OU_opnd2)];

    INT64 result_val;
    INT64 tn0_val, tn1_val;
    UINT64 tn0_uval, tn1_uval;

    ST *result_sym = NULL;
    INT32 result_relocs;

    /* All the rest of the operations have at most two operands. */

    if (OP_opnds(op) > 3)
        return FALSE;
    /* Determine the constant value of every operand. */

    if (!TN_has_value(tn0) ||!TN_has_value(tn1))
    {
        return FALSE;
    }

    tn0_val = tn0_uval = TN_value(tn0);
    tn1_val = tn1_uval = TN_value(tn1);

    /* Addition... */

    if (OP_opnds(op) == 3 && !TN_is_symbol(tn1))
    {
        if (opcode == TOP_add || opcode == TOP_addu || opcode == TOP_addiu)
        {
            result_val = tn0_val + tn1_val;
            if (TN_size(tnr) == 4)
                result_val = (INT32)result_val;
            goto Constant_Created;

        }
        /* Subtraction... */

        if (opcode == TOP_sub || opcode == TOP_subu)
        {
            result_val = tn0_val - tn1_val;
            goto Constant_Created;
        }

    }

    /* Logical... */

    if (opcode == TOP_and || opcode == TOP_andi)
    {
        result_val = tn0_uval & tn1_uval;
        goto Constant_Created;
    }
    else if (opcode == TOP_or || opcode == TOP_ori)
    {
        result_val = tn0_uval | tn1_uval;
        goto Constant_Created;
    }
    else if (opcode == TOP_xor || opcode == TOP_xori)
    {
        result_val = tn0_uval ^ tn1_uval;
        goto Constant_Created;
    }

    /* Shift... */

    if (opcode == TOP_sll || opcode == TOP_sllv)
    {
        result_val = TRUNC_32(tn0_uval << tn1_uval);
        goto Constant_Created;
    }

    else if (opcode == TOP_sra || opcode == TOP_srav)
    {
        result_val = tn0_val >> tn1_uval;

        goto Constant_Created;
    }

    else if (opcode == TOP_srl || opcode == TOP_srlv)
    {
        result_val = TRUNC_32(tn0_val) >> tn1_uval;
        goto Constant_Created;
    }
    else if (opcode == TOP_slti || opcode == TOP_sltiu)
    {
        result_val=(tn0_val < tn1_val);
        goto Constant_Created;
    }
    return FALSE;

    /* We've evaluated the expression, so replace it with the result. */

Constant_Created:

    OPS ops = OPS_EMPTY;
    TN *tnc;

    if (result_sym != NULL)
    {
        /* Don't consider using an offset that does not fit in the LC_simm32 class.
         */
        tnc = Gen_Symbol_TN(result_sym, result_val, result_relocs);
    }
    else
    {
        const int size = TN_size(tnr) ;
        tnc = Gen_Literal_TN(result_val, size);
    }

    Expand_Immediate(tnr, tnc, OP_result_is_signed(op,0), &ops);

    /* If generating the literal requires more than one instruction,
       then just keep the original instruction. It's not clear that this
       is always the right thing, since by eliminating the instruction
       we could create dead code. */

    if (OP_next(OPS_first(&ops)) != NULL)
        return FALSE;

    if (EBO_in_loop)
        EBO_OPS_omega(&ops, opnd_tninfo[OP_PREDICATE_OPND]);

    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);

    if (EBO_Trace_Optimization)
    {
        fprintf(TFile, "%sin BB:%d Redefine ",
                EBO_trace_pfx, BB_id(OP_bb(op)));
        Print_TN(tnr,TRUE);
        fprintf(TFile," with load of ");
        Print_TN(tnc,FALSE);
        fprintf(TFile, "\n");
    }
    return TRUE;
}

/*
 * CGTARG_Copy_Operand already catches most of the case we care about,
 * but there are some extra cases involving a 0 first operand and
 * we want to treat int->float and float->int register moves as copies.
 */
INT EBO_Copy_Operand(OP *op)
{
    INT opnd;

    opnd = CGTARG_Copy_Operand(op);
    if (opnd > 0) {
        return opnd;
    }
    return -1;
}

static BOOL is_live_tn(OP *current_op, TN *current_tn)
{
    OP *op;
    Is_True(GRA_LIVE_Phase_Invoked, ("Bad call to is_live_tn"));
    BOOL is_live = tn_has_live_def_into_BB(current_tn, OP_bb(current_op));
    BOOL past_current_op = FALSE;

    FOR_ALL_BB_OPs(OP_bb(current_op), op) {
        INT num_opnds = OP_opnds(op);
        INT num_results = OP_results(op);
        if (op == current_op) {
            past_current_op = TRUE;
            if (!is_live)
                return FALSE;
        }
        if (past_current_op) {
            for (int opndnum = 0; opndnum < num_opnds; opndnum++) {
                if (tn_registers_identical(current_tn, OP_opnd(op,opndnum)))
                    return TRUE;
            }
        }
        for (int resnum = 0; resnum < num_results; resnum++) {
            if (tn_registers_identical(current_tn, OP_result(op,resnum))) {
                if (past_current_op && !OP_cond_def(op))
                    return FALSE;
                else
                    is_live = TRUE;
            }
        }
    }
    Is_True(past_current_op && is_live, ("Bad call to is_live_tn"));
    return GTN_SET_MemberP(BB_live_out(OP_bb(current_op)), current_tn);
}

/*
 * Look at an expression and it's inputs to identify special sequences
 * that can be simplified.
 */
BOOL Special_Sequence(OP *op, TN **opnd_tn, EBO_TN_INFO **opnd_tninfo)
{
    if (!EBO_in_peep)
        return FALSE;
    if (OP_code(op) == TOP_beq ||
            OP_code(op) == TOP_bne)  {
        INT64 val = 0;
        TN *opnd0 = OP_opnd(op, OP_find_opnd_use(op, OU_opnd1));
        TN *opnd1 = OP_opnd(op, OP_find_opnd_use(op, OU_opnd2));
        EBO_TN_INFO *opnd1_tninfo = get_tn_info(opnd1);
        if (opnd1_tninfo &&
                EBO_tn_available(OP_bb(op), opnd1_tninfo) &&
                opnd1_tninfo->in_op &&
                (OP_code(opnd1_tninfo->in_op) == TOP_addiu)) {
            OP *op_in = opnd1_tninfo->in_op;
            if (TN_has_value(OP_opnd(op_in, OP_find_opnd_use(op_in, OU_opnd2))) &&
                    (TN_value(OP_opnd(op_in, OP_find_opnd_use(op_in, OU_opnd2))) != -1))
                return FALSE;
            else if (TN_has_value(OP_opnd(op_in, OP_find_opnd_use(op_in, OU_opnd2))))
                val = -1;
            else
                return FALSE;
        } else if (opnd1 != Zero_TN) {
            return FALSE;
        }

        // Try folding shift+beq/bne to bgez/bltz
        EBO_TN_INFO *opnd0_tninfo = get_tn_info(opnd0);
        if (opnd0_tninfo &&
                EBO_tn_available(OP_bb(op), opnd0_tninfo) &&
                opnd0_tninfo->in_op &&
                (OP_code(opnd0_tninfo->in_op) == TOP_sra ||
                 OP_code(opnd0_tninfo->in_op) == TOP_dsra ||
                 OP_code(opnd0_tninfo->in_op) == TOP_dsra32)) {
            EBO_TN_INFO *tninfo;
            OP *shift_op = opnd0_tninfo->in_op;

            // Quit if result of the shift is live out of this BB
            TN *shift_res = OP_result(shift_op, 0);
            if (EBO_in_peep) {
                if (REG_LIVE_Outof_BB(TN_register_class(shift_res),
                                      TN_register(shift_res),
                                      OP_bb(shift_op)))
                    return FALSE;
            } else {
                if (CG_localize_tns && (TN_is_dedicated(shift_res) ||
                                        TN_is_global_reg(shift_res)))
                    return FALSE;
                else if (GRA_LIVE_TN_Live_Outof_BB(shift_res, OP_bb(shift_op)))
                    return FALSE;
            }

            // Find the control operand for the "shift".
            TN *shift_ctrl_opnd = OP_opnd(shift_op, OP_find_opnd_use(shift_op, OU_opnd2));
            FmtAssert(TN_has_value(shift_ctrl_opnd),
                      ("shift ctrl opnd is not a constant"));
            switch (shift_op->opr) {
            case TOP_sra:
                if (TN_value(shift_ctrl_opnd) != 31)
                    return FALSE;
                break;
            case TOP_dsra32:
                if ((TN_value(shift_ctrl_opnd) % 32) != 31)
                    return FALSE;
                break;
            case TOP_dsra:
                if (TN_value(shift_ctrl_opnd) == 63)
                    FmtAssert(FALSE, ("shift of 63 with dsra encountered"));
                break;
            }

            OP *new_op;
            OPS ops = OPS_EMPTY;
            if (OP_code(op) == TOP_bne)
                val = (val == 0)?-1:0;
            new_op = Mk_OP((val==0)?TOP_bgez:TOP_bltz,
                           OP_opnd(shift_op, 1), OP_opnd(op, 3));
            OP_srcpos(new_op) = OP_srcpos(op);
            OPS_Append_Op(&ops, new_op);
            BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
            OP_Change_To_Noop(op);
            OP_Change_To_Noop(shift_op);
            if (EBO_Trace_Optimization) {
                fprintf(TFile, "shift and beq combined to produce bgez.\n");
            }
            return TRUE;
        }
    }

    TOP top = OP_code(op);

#ifdef Is_True_On
    if (EBO_Trace_Optimization)
    {
        fprintf(TFile, "OP: ");
        Print_OP_No_SrcLine(op);
        for (UINT i = 0; i < OP_opnds(op); i++)
        {
            fprintf(TFile, "  local tn: ");
            Print_TN(OP_opnd(op, i), TRUE);
            fprintf(TFile, "\n  ebo tn: ");
            Print_TN(opnd_tn[i], TRUE);
            fprintf(TFile, "\n   local in_op: ");
            fprintf(TFile, "<null>\n");
            fprintf(TFile, "   ebo in_op: ");
            if (opnd_tninfo[i] && opnd_tninfo[i]->in_op)
                Print_OP_No_SrcLine(opnd_tninfo[i]->in_op);
            else
                fprintf(TFile, "<null>\n");

            if (opnd_tninfo[i] && opnd_tninfo[i]->replacement_tn)
            {
                fprintf(TFile, "  replace tn: ");
                Print_TN(opnd_tninfo[i]->replacement_tn, TRUE);
                fprintf(TFile, "\n   replace in_op: ");
                if (opnd_tninfo[i]->replacement_tninfo &&
                        opnd_tninfo[i]->replacement_tninfo->in_op)
                    Print_OP_No_SrcLine(opnd_tninfo[i]->replacement_tninfo->in_op);
                else
                    fprintf(TFile, "<null>\n");
            }
        }
    }
#endif

    return FALSE;
}

/* This is used to eliminate unnecessry shift operations like:
 *     TN78($2) :-  dsrl32 TN1($0) <const> ;
 * Replace uses of the result with Zero_TN, and eliminate the shift op.
 * The def should not be live out of the current basic block.
 *
 * Also, as a final step eliminate all copy ops (could be by-product of
 * the removal of shifts above).
 */
void Redundancy_Elimination()
{

#if Is_True_On
    if (!(EBO_Opt_Mask & EBO_REDUNDANCY_ELIMINATION)) return;
#endif
    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
        if (BB_rid(bb) && (RID_level(BB_rid(bb)) >= RL_CGSCHED)) {
            // don't change bb's which have already been through CG
            continue;
        }
        for (OP* op = BB_first_op(bb); op != NULL; op = OP_next(op)) {
            BOOL done = FALSE;
            INT copy_operand = CGTARG_Copy_Operand(op);
            if (copy_operand >= 0) {
                // Other ops could be rendered uselesss by the shift removal.
                // Example:
                //   TN969 :- srl TN1($0) (0x3) ;
                //   TN969 :- xor TN969<defopnd> TN916 ;
                // Here, the xor operation is uselss if TN969 is not live
                // out of this block and can be replaced by TN916.

                /* The whole point is to get rid of result and replace
                   with copy_operand */

                TN* src = OP_opnd(op, copy_operand);
                TN* result = OP_result(op, 0);

                FmtAssert(TN_is_register(src) && TN_is_register(result),
                          ("Redundancy_Elimination: src/result not registers in EBO_in_peep (1)"));

                // do not delete assignment to result if result is live out
                if (REG_LIVE_Outof_BB(TN_register_class(result),
                                      TN_register(result), bb)) {
                    // see if the register is re-defined before block end
                    // we can still delete the op if there is a re-definition
                    BOOL redefined = FALSE;
                    for (OP* dw_op = OP_next(op); dw_op != NULL; dw_op = OP_next(dw_op)) {
                        if ((OP_code(dw_op) != TOP_asm) &&
                                OP_results(dw_op) &&
                                TN_is_register(OP_result(dw_op, 0)) &&
                                (TN_register_class(OP_result(dw_op, 0)) ==
                                 TN_register_class(result)) &&
                                (TN_register(OP_result(dw_op, 0)) == TN_register(result))) {
                            redefined = TRUE;
                            break;
                        }
                    }
                    if (redefined == FALSE)
                        continue;
                }

                /* In the following scenario, the copy_op can not be deleted
                 *    a = b   << copy_op >>
                 *      = a
                 *    b =     << redef >>
                 *      = a
                 * NOTE: If redef is a copy op and copy operand is 'a'
                 *       then copy_op can be removed
                 */
                BOOL cannot_be_removed = FALSE;
                for (OP* dw_op = OP_next(op); dw_op != NULL; dw_op = OP_next(dw_op)) {
                    if ((OP_code(dw_op) != TOP_asm) &&
                            OP_results(dw_op) &&
                            TN_is_register(OP_result(dw_op, 0)) &&
                            TNs_Are_Equivalent(OP_result(dw_op, 0), src)) {

                        // see NOTE above
                        INT dw_op_copy_operand = CGTARG_Copy_Operand(dw_op);
                        TN *dw_op_copy_operand_tn = (dw_op_copy_operand>=0)?OP_opnd(dw_op, dw_op_copy_operand):NULL;
                        if ((dw_op_copy_operand >= 0) &&
                                TN_is_register(dw_op_copy_operand_tn) &&
                                TNs_Are_Equivalent(dw_op_copy_operand_tn, result))
                            break;

                        // search if 'result' is being used after dw_op
                        for (OP* dw1_op = OP_next(dw_op); dw1_op != NULL; dw1_op = OP_next(dw1_op)) {
                            for (int i = 0; i < OP_opnds(dw1_op); i++) {
                                if (TN_is_register(OP_opnd(dw1_op, i)) &&
                                        TNs_Are_Equivalent(OP_opnd(dw1_op, i), result)) {
                                    cannot_be_removed = TRUE;
                                    break;
                                }
                            }
                        }
                        if (cannot_be_removed)
                            break;
                    }
                }
                if (cannot_be_removed)
                    continue;

                /* Traverse downwards; replace result with src */

                for (OP* dw_op = OP_next(op); dw_op != NULL; dw_op = OP_next(dw_op)) {

                    for (int i = 0; i < OP_opnds(dw_op); i++) {
                        if (TN_is_register(OP_opnd(dw_op, i)) &&
                                TNs_Are_Equivalent(OP_opnd(dw_op, i), result)) {
                            Set_OP_opnd(dw_op, i, src);
                            done = TRUE;
                        }
                    }

                    if (OP_results(dw_op) == 1) {
                        TN* tnr = OP_result(dw_op, 0);

                        if (TN_is_register(tnr) &&
                                TNs_Are_Equivalent(tnr, result) ||
                                TNs_Are_Equivalent(tnr, src))
                            break;
                    }
                }

                if (done) {
                    OP* dead = op;
                    op = OP_prev(op);

                    if (EBO_Trace_Optimization) {
                        fprintf(TFile, "Redundancy_Elimination removes simplified op - ");
                        Print_OP_No_SrcLine(dead);
                    }

                    BB_Remove_Op(bb, dead);

                    if (op == NULL)
                        op = BB_first_op(bb);
                }

                continue; // end for this op
            }
            if (OP_code(op) != TOP_srl &&
                    OP_code(op) != TOP_sra &&
                    OP_code(op) != TOP_sll &&
                    OP_code(op) != TOP_dsrl &&
                    OP_code(op) != TOP_dsra &&
                    OP_code(op) != TOP_dsll &&
                    OP_code(op) != TOP_dsll32 &&
                    OP_code(op) != TOP_dsra32 &&
                    OP_code(op) != TOP_dsrl32)
                continue;

            /* The whole point is to get rid of result. */

            TN* src = OP_opnd(op, OP_find_opnd_use(op, OU_opnd1));
            TN* result = OP_result(op, 0);

            FmtAssert(TN_is_register(src) && TN_is_register(result),
                      ("Redundancy_Elimination: src/result not registers in EBO_in_peep (2)"));

            FmtAssert(false, ("Redundancy_Elimination: UNIMPLEMENTED (1)"));

            // do not delete assignment to result if result is live out
            if (REG_LIVE_Outof_BB(TN_register_class(result),
                                  TN_register(result), bb)) {
                // see if the register is re-defined before block end
                // we can still delete the op if there is a re-definition
                BOOL redefined = FALSE;
                for (OP* dw_op = OP_next(op); dw_op != NULL; dw_op = OP_next(dw_op)) {
                    if (OP_has_result(dw_op) &&
                            TN_is_register(OP_result(dw_op, 0)) &&
                            (TN_register_class(OP_result(dw_op, 0)) ==
                             TN_register_class(result)) &&
                            (TN_register(OP_result(dw_op, 0)) == TN_register(result))) {
                        redefined = TRUE;
                        break;
                    }
                }
                if (redefined == FALSE)
                    continue;
            }

            /* Traverse downwards; replace result with Zero_TN */

            for (OP* dw_op = OP_next(op); dw_op != NULL; dw_op = OP_next(dw_op)) {
                for (int i = 0; i < OP_opnds(dw_op); i++) {
                    if (TN_is_register(OP_opnd(dw_op, i)) &&
                            (TN_register_class(OP_opnd(dw_op, i)) ==
                             TN_register_class(result)) &&
                            (TN_register(OP_opnd(dw_op, i)) == TN_register(result))) {
                        Set_OP_opnd(dw_op, i, Zero_TN);
                        done = TRUE;
                    }
                }

                if (OP_results(dw_op) == 1) {
                    TN* tnr = OP_result(dw_op, 0);

                    if (TN_is_register(tnr) &&
                            TNs_Are_Equivalent(tnr, result))
                        break;
                }
            }

            if (done) {
                OP* dead = op;
                op = OP_prev(op);

                if (EBO_Trace_Optimization) {
                    fprintf(TFile, "Redundancy_Elimination removes simplified op - ");
                    Print_OP_No_SrcLine(dead);
                }

                BB_Remove_Op(bb, dead);

                if (op == NULL)
                    op = BB_first_op(bb);
            }
        }
    }

}

// Group together opcodes that perform the same function but with different
// address modes.
typedef struct {
    TOP reg_mode;
    TOP base_mode;
    TOP base_index_mode;
    TOP index_mode;
    TOP n32_mode;
} Addr_Mode_Group;

// Map an opcode to its address-modes group.
static Addr_Mode_Group *Top_To_Addr_Mode_Group[TOP_count+1];

// List all address mode groups.  Each group gives the opcodes for OPs that
// perform the same function but with different address modes.  Entries can be
// listed in any order.  No duplicates allowed.  The table doesn't have to be
// complete; it may list only those OPs that EBO can do something about.
static Addr_Mode_Group Addr_Mode_Group_Table[] = {
};

// Associate an address mode group to an opcode.
static void
Add_Addr_Mode_Group(TOP top, Addr_Mode_Group *address_mode_group)
{
    if (top == TOP_UNDEFINED)
        return;

    // Don't redefine the address group for TOP if it is already defined.  This
    // is so that if TOP appears in multiple groups, we use only the first group,
    // which is assumed to be the most authoritative for TOP.
    if (Top_To_Addr_Mode_Group[top] == NULL)
        Top_To_Addr_Mode_Group[top] = address_mode_group;
}

// Build the Top_To_Addr_Modes table.
static void
Init_Addr_Modes()
{
    int i;
    ADDR_MODE mode;
    static bool table_is_initialized = false;

    if (table_is_initialized)
        return;

    table_is_initialized = true;

    for (i = 0; i < TOP_UNDEFINED; i++) {
        Top_To_Addr_Mode_Group[i] = NULL;
    }

    // Make sure the code below is in sync with the number of address modes.
    // Currently the last mode is N32_Mode.
    ADDR_MODE last_mode = N32_MODE;
    ADDR_MODE undefined_mode = UNDEFINED_MODE;
    FmtAssert(1 + (int)last_mode == (int) undefined_mode,
              ("Init_Addr_Modes: some address modes not handled"));

    UINT table_size = sizeof(Addr_Mode_Group_Table) / sizeof(Addr_Mode_Group);
    for (i = 0; i < table_size; i++) {
        Addr_Mode_Group *group = &Addr_Mode_Group_Table[i];

        Add_Addr_Mode_Group(group->reg_mode, group);
        Add_Addr_Mode_Group(group->base_mode, group);
        Add_Addr_Mode_Group(group->base_index_mode, group);
        Add_Addr_Mode_Group(group->index_mode, group);
        Add_Addr_Mode_Group(group->n32_mode, group);
    }
}
