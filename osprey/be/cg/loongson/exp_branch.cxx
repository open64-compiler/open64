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


/* CGEXP routines for expanding branches */

#include "defs.h"
#include "erglob.h"
#include "ercg.h"
#include "tracing.h"
#include "config.h"
#include "config_targ_opt.h"
#include "tn.h"
#include "cg_flags.h"
#include "op.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "whirl2ops.h"
#include "config_opt.h"      // For Force_IEEE_Comparisons

void
Initialize_Branch_Variants(void)
{
}

void
Exchange_Opnds_And_Variant(VARIANT *variant, TN **src1, TN **src2)
{
    TN *tmp = *src1;
    *src1 = *src2;
    *src2 = tmp;

    *variant = Invert_BR_Variant(*variant);
}


/* This function is only called by Pick_Branch_TOP.
 * Do not check whether the variant is legal. 
 * Also from Godson-I compiler. */
INT8
Can_Use_Branch_Cmp_With_Zero(TN **src1, TN **src2)
{
    return(TN_is_zero(*src2) && (!TN_has_value(*src1)));
}


/* This function is from the Godson-I compiler.In ORC2.1,the task was carried by
 * function Pick_Compare_TOP.In my opinion,use additional func to implement it
 * is better. George Her 2003.10 */
void
Search_Dedicate_Branch(VARIANT *variant, TN **src1, TN **src2)
{
    // check for special cases of first or second arg being zero.
    if (*src1 != NULL && TN_is_zero(*src1))
    {
        switch (*variant)
        {
        case V_BR_U8LE:
        case V_BR_U4LE:
            *variant = V_BR_ALWAYS;
            break;
        case V_BR_U8GT:
        case V_BR_U4GT:
            *variant = V_BR_NEVER;
            break;
        }
    }
    if (*src2 != NULL && TN_is_zero(*src2))
    {
        switch (*variant)
        {
        case V_BR_U8LT:
        case V_BR_U4LT:
            *variant = V_BR_NEVER;
            break;
        case V_BR_U8GE:
        case V_BR_U4GE:
            *variant = V_BR_ALWAYS;
            break;
        }
    }
}

/* Check that branch is of proper form,
 * and return TOP to use for the branch.
 * May  modify the variant and src tns. */
void
Pick_Branch_TOP(VARIANT *variant, TN **src1, TN **src2,
                TOP* brh, TOP* cmp, INT8* need_negative, INT8* need_change_src, OPS *ops)
{
    if (TN_has_value(*src1)
            // To avoid dead lock, when this function is called recursively
            // if so, do not check following condtion.
            // TODO: fully judgement
            && (*brh == TOP_UNDEFINED)
            && (*cmp == TOP_UNDEFINED))
        Exchange_Opnds_And_Variant(variant, src1, src2);

    // Inititalize variable
    TOP  cmp_i = TOP_UNDEFINED;

    *brh = TOP_UNDEFINED;
    *cmp = TOP_UNDEFINED;

    /* Src1 and Src2 can not both have value
     *  E.g. if (1<2) ...
     *  Which has been recognized by whirl and
     *  should be  converted to an always branch. */
    Is_True(!(TN_has_value(*src1) && TN_has_value(*src2)),
            ("Src1 & Src2 can not both be constant \n"));

    // Check the equal
    switch (V_br_condition(*variant))
    {

    case  V_BR_I8EQ:
    case  V_BR_U8EQ:
    case  V_BR_I4EQ:
    case  V_BR_U4EQ:
        *brh = TOP_beq;
        return;

    case  V_BR_I8NE:
    case  V_BR_U8NE:
    case  V_BR_I4NE:
    case  V_BR_U4NE:
        *brh = TOP_bne;
        return;

    case  V_BR_I8GT:
    case  V_BR_U8GT:
    case  V_BR_I4GT:
    case  V_BR_U4GT:
        *brh = TOP_bgtz;
        if (Can_Use_Branch_Cmp_With_Zero(src1, src2))
            break;

        Exchange_Opnds_And_Variant(variant, src1, src2);
        Pick_Branch_TOP(variant, src1, src2,
                        brh, cmp,
                        need_negative,
                        need_change_src,
                        ops);
        break;

    case  V_BR_I8GE:
    case  V_BR_U8GE:
    case  V_BR_I4GE:
    case  V_BR_U4GE:
        *brh = TOP_bgez;
        if (Can_Use_Branch_Cmp_With_Zero(src1, src2))
            break;

        *variant = Negate_BR_Variant(*variant);
        (*need_negative) =  !(*need_negative);
        Pick_Branch_TOP(variant, src1, src2,
                        brh, cmp,
                        need_negative,
                        need_change_src,
                        ops);

        break;

    case  V_BR_I8LT:
    case  V_BR_I4LT:

        *cmp = TOP_slt;
        cmp_i = TOP_slti;
        break;

    case  V_BR_U8LT:
    case  V_BR_U4LT:

        *cmp = TOP_sltu;
        cmp_i = TOP_sltiu;
        break;

    case  V_BR_I8LE:
    case  V_BR_U8LE:
    case  V_BR_I4LE:
    case  V_BR_U4LE:
        *brh = TOP_blez;
        if (Can_Use_Branch_Cmp_With_Zero(src1, src2))
            break;

        *variant = Negate_BR_Variant(*variant);
        (*need_negative) =  !(*need_negative);
        Pick_Branch_TOP(variant, src1, src2,
                        brh, cmp,
                        need_negative,
                        need_change_src,
                        ops);
        break;

    case V_BR_FLT:
        *cmp = TOP_c_lt_s;
        break;
    case V_BR_DLT:
        *cmp = TOP_c_lt_d;
        break;
    case V_BR_FLE:
        *cmp = TOP_c_le_s;
        break;
    case V_BR_DLE:
        *cmp = TOP_c_le_d;
        break;
    case V_BR_FGT:
        *cmp = TOP_c_lt_s;
        (*need_change_src) = TRUE;
        break;
    case V_BR_DGT:
        *cmp = TOP_c_lt_d;
        (*need_change_src) = TRUE;
        break;
    case V_BR_FGE:
        *cmp = TOP_c_le_s;
        (*need_change_src) = TRUE;
        break;
    case V_BR_DGE:
        *cmp = TOP_c_le_d;
        (*need_change_src) = TRUE;
        break;
    case V_BR_FEQ:
        *cmp = TOP_c_eq_s;
        break;
    case V_BR_DEQ:
        *cmp = TOP_c_eq_d;
        break;
    case V_BR_FNE:
        *cmp = TOP_c_eq_s;
        (*need_negative) = !(*need_negative);
        break;
    case V_BR_DNE:
        *cmp = TOP_c_eq_d;
        (*need_negative) = !(*need_negative);
        break;

    default:
        *brh = TOP_UNDEFINED;
        *cmp = TOP_UNDEFINED;
        cmp_i = TOP_UNDEFINED;
    }

    Is_True(!(*brh == TOP_UNDEFINED &&
              *cmp == TOP_UNDEFINED),
            ("Can not find the branch op in current version\n"));

    // if src2 is immed and fits, use immed form of top
    // If we to avoid deadlock, src1 maybe have value
    // handle brh=TOP_bgtz , TOP_blez or TOP_bgez.
    if (*brh != TOP_UNDEFINED &&
            Can_Use_Branch_Cmp_With_Zero(src1, src2))
    {
        *cmp = TOP_UNDEFINED;
        return;
    }

    // After recursively called this function, opnds may be changed
    // So, src1 may have value.
    *src1 = (TN_has_value(*src1)) ?
            Expand_Immediate_Into_Register(*src1, ops) :
            *src1;

    if (cmp_i != TOP_UNDEFINED && *src2 != NULL && TN_has_value(*src2))
    {
        const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(cmp_i);
        const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, 1);
        ISA_LIT_CLASS lc = ISA_OPERAND_VALTYP_Literal_Class(otype);

        if (ISA_LC_Value_In_Class(TN_value(*src2), lc))
            *cmp = cmp_i;
        else
            *src2 = Expand_Immediate_Into_Register(*src2, ops);

        return;

    }
    else if (*cmp != TOP_UNDEFINED)
    {

        return;
    }


    Is_True(false, ("Can not go to here!\n Whether cmp and brh both are UNDEFINED?\n"));

}


/* Check that compare is of proper form,
 * and return TOP to use for the compare.
 * May modify the variant and src tns.
 *  Do not use this interface in Godson2 compiler. */
TOP
Pick_Compare_TOP(VARIANT *variant, TN **src1, TN **src2, OPS *ops)
{
    FmtAssert(FALSE, ("unimplemented"));
}

void
Expand_Branch(TN *targ, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
    TOP cmp, brh;
    BOOL false_br = V_false_br(variant);
    VARIANT cond = V_br_condition(variant);
    INT32 need_negative = false;
    INT32 need_change_src = false;

    /* Trace if required: */
    if (Trace_Exp2)
    {
        fprintf(TFile, "<cgexp> Translating %s branch:\n",
                (false_br ? "false" : "true"));
    }

    FmtAssert(cond <= V_BR_LAST, ("unexpected variant in Expand_Branch"));
    FmtAssert(cond != V_BR_NONE, ("BR_NONE variant in Expand_Branch"));


    Search_Dedicate_Branch(&cond, &src1, &src2);


    if (Trace_Exp2)
    {
        fprintf(TFile, "<cgexp> transformed branch cond = %lld\n", cond);
    }

    switch (cond)
    {
    case V_BR_ALWAYS:
    case V_BR_NEVER:
        if ((cond == V_BR_ALWAYS) ^ false_br)
        {
            // Unconditional branch for ALWAYS/!false_br and NEVER/false_br
            Build_OP(TOP_j, True_TN, targ, ops);
        }
        else
        {
            // As these BB's have branch_wn (used in Build_CFG later) associated with them,
            // they cannot be treated as empty containers to contain other instructions.
            // We put a nop in it to indicate this.
            Build_OP(TOP_nop, True_TN, targ, ops);
        }
        break;

    case V_BR_PEQ:
    case V_BR_PNE:
        Is_True(false, ("Should not handle predicate equivalent...\n"));
        break;

    case V_BR_P_TRUE:
        Is_True(false, ("Should not handle predicate true...\n"));
        break;

    default:
    {
        if (false_br)
            cond = Negate_BR_Variant(cond);

        brh = TOP_UNDEFINED;
        cmp = TOP_UNDEFINED;
        Pick_Branch_TOP(&cond, &src1, &src2, &brh, &cmp,
                        &need_negative,
                        &need_change_src,
                        ops);

        Is_True(!(brh == TOP_UNDEFINED &&
                  cmp == TOP_UNDEFINED),
                ("Can not find the branch ops\n"));

        // Process float type compare and branch

        if (cmp != TOP_UNDEFINED && TOP_is_flop(cmp))
        {
            if (need_negative)
            {
                false_br = !(false_br);
            }

            need_change_src ? Build_OP(cmp, True_TN, src2, src1, ops) :
            Build_OP(cmp, True_TN, src1, src2, ops);

            if (false_br)
                Build_OP(TOP_bc1f, True_TN, targ, ops);
            else
                Build_OP(TOP_bc1t, True_TN, targ, ops);
            return;

        }

        // End processing float-type branch

        switch (brh)
        {
        case TOP_beq:
        case TOP_bne:
            // Check whether two opnds are both registers
            src1 = (TN_has_value(src1)) ?
                   Expand_Immediate_Into_Register(src1, ops) : src1;
            src2 = (TN_has_value(src2)) ?
                   Expand_Immediate_Into_Register(src2, ops) : src2;
            Build_OP(brh, True_TN, src1, src2, targ, ops);
            break;

        case  TOP_bgtz:
        case  TOP_bgez:
        case  TOP_blez:
            Build_OP(brh, True_TN, src1, targ, ops);
            break;

        case  TOP_UNDEFINED:
            if (cmp != TOP_UNDEFINED)
            {
                TN* cmp_result;
                cmp_result = Build_TN_Like(src1);
                Build_OP(cmp, cmp_result, True_TN, src1, src2, ops);
                need_negative ? Build_OP(TOP_beq, True_TN, cmp_result, Zero_TN, targ, ops) :
                Build_OP(TOP_bne, True_TN, cmp_result, Zero_TN, targ, ops);
            }
            else Is_True(!(brh == TOP_UNDEFINED &&
                               cmp == TOP_UNDEFINED),
                             ("Can not find the branch ops\n"));
            break;
        default:
            Is_True((brh == TOP_UNDEFINED),
                    ("Unexpected branch op used!\n"));
        }
    }
    }
}


void Exp_Indirect_Branch(TN *targ_reg, OPS *ops)
{
    if (TN_register_class(targ_reg) != ISA_REGISTER_CLASS_integer)
    {
        Is_True(false, ("The branch target is not in interger register!\n"));
    }
    Build_OP(TOP_jr, True_TN, targ_reg, ops);
}

void Exp_Local_Jump(BB *bb, INT64 offset, OPS *ops)
{
    FmtAssert(FALSE, ("NYI: Exp_Local_Jump"));
}

void Exp_Return(TN *return_address, OPS *ops)
{
    Build_OP(TOP_jr, True_TN,
             return_address, ops);
}

void Exp_Call(OPERATOR opr, TN *return_address, TN *target, OPS *ops)
{
    TOP top;
    TN *br_tmp;


    switch (opr)
    {
    case OPR_CALL:
        Is_True(TN_is_symbol(target),
                ("Target should be a symbol TN!\n"));
        Build_OP(TOP_jal, True_TN, target, ops);
        break;

    case OPR_ICALL:
        /* We deleted some codes for GP-restore since Callee-save GP convention */
    case OPR_PICCALL:

        Build_OP(TOP_jalr,  RA_TN, True_TN,
                 Ep_TN, ops);
        break;

    default:
        FmtAssert(FALSE, ("unexpected opr in Exp_Call"));
        /*NOTREACHED*/
    }
}

