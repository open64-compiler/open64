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

#include "defs.h"
#include "config.h"
#include "erglob.h"
#include "ercg.h"
#include "glob.h"
#include "tracing.h"
#include "util.h"

#include "topcode.h"
#include "tn.h"
#include "cg_flags.h"
#include "targ_isa_lits.h"
#include "bb.h"
#include "symtab.h"
#include "opcode.h"
#include "const.h"	/* needed to manipulate target/host consts */
#include "targ_const.h"	/* needed to manipulate target/host consts */
#include "op.h"
#include "data_layout.h"
#include "stblock.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "w2op.h"
#include "label_util.h"
#include "cgtarget.h"
#include "whirl2ops.h"
#include "cg_spill.h"

#include "expand.h"

BOOL Reuse_Temp_TNs = FALSE;

BOOL Trace_Exp2 = FALSE;      /* extra cgexp trace*/

/* Disable conversion of constant integer multiplies into shift/adds:*/
static BOOL Disable_Const_Mult_Opt = FALSE;

/* Dup_TN won't dup a dedicated tn, but for our purposes we
 * can just re-use the dedicated tn.  Don't want to re-use a
 * symbolic tn or it will mess up live ranges. */
/* DOESN'T WORK:  causes problems in Create_lvs because it causes
 * a use of a parm reg at the call-site, so it looks like the
 * parm-reg is incoming at the call?  This probably should work,
 * but for now we can use other routine that create a real dup tn. */
#define DUP_TN(tn)	Dup_TN_Even_If_Dedicated(tn)

/* Check whether the first source has value,in most cases,it is illegal to have the
src1 constant.  */
#define  Check_Src1  \
	Is_True(!(TN_has_value(src1)),("In this case src1 should not be constant!"))

static TOP
Pick_Imm_Form_TOP(TOP regform)
{
    switch (regform)
    {
    case TOP_sllv:
        return TOP_sll;
    case TOP_srlv:
        return TOP_srl;
    case TOP_srav:
        return TOP_sra;
    case TOP_and:
        return TOP_andi;
    case TOP_or:
        return TOP_ori;
    case TOP_xor:
        return TOP_xori;
    default:
        return regform;
    }

}

void
Expand_Copy(TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
    if (! TN_is_register(src))
    {
        Build_OP(TOP_ori, result, True_TN, Zero_TN, src, ops);
        return ;
    }

    Is_True(TN_is_register(src) && TN_is_register(result),
            ("not support this kind of immediate copy!"));

    if (mtype == MTYPE_I8 || mtype == MTYPE_U8
            || MTYPE_is_integral(mtype))
    {
        Build_OP(TOP_or, result, True_TN, src, Zero_TN, ops);
        Set_OP_copy(OPS_last(ops));
        return;
    }

    TOP new_opcode;

    if (TN_is_float(src) && TN_is_float(result))
    {
        switch (mtype)
        {
        case MTYPE_F4:
            new_opcode = TOP_mov_s;
            break;
        case MTYPE_F8:
            new_opcode = TOP_mov_d;
            break;
        default:
            Is_True(FALSE, ("not support float register copy without mtype being float!"));
        }
    }
    else if (TN_is_float(src) && !TN_is_float(result))
    {
        switch (mtype)
        {
        case MTYPE_F4:
            new_opcode = TOP_mfc1;
            break;
        case MTYPE_F8:
            new_opcode = TOP_dmfc1;
            break;
        default:
            Is_True(FALSE, ("not support float register copy without mtype being float!"));
        }
    }
    else if (!TN_is_float(src) && TN_is_float(result))
    {
        switch (mtype)
        {
        case MTYPE_F4:
            new_opcode = TOP_mtc1;
            break;
        case MTYPE_F8:
            new_opcode = TOP_dmtc1;
            break;
        default:
            Is_True(FALSE, ("not support float register copy without mtype being float!"));
        }
    }
    else
    {
        Is_True(FALSE, ("not support int registers copy with mtype being float!"));
    }

    Build_OP(new_opcode, result, True_TN, src, ops);
    Set_OP_copy(OPS_last(ops));
}

//
//  Helper routine to do proper sign extension
//
static void
Fixup_32_Bit_Op(TN *result, TN *src, TYPE_ID dest_type, OPS *ops)
{
    Expand_Copy(result, src, dest_type, ops);
}


/* Interface for keeping the consistency with
   the check in Check_If_Dedicated_TN_Is_Global() */
inline BOOL Arg_or_Retval(TN *tn)
{
    if (!tn || !TN_is_dedicated(tn) || TN_register(tn) == REGISTER_UNDEFINED)
        return FALSE;
    return
        REGISTER_SET_MemberP(
            REGISTER_CLASS_function_argument(TN_register_class(tn)), TN_register(tn)) ||
        REGISTER_SET_MemberP(
            REGISTER_CLASS_function_value(TN_register_class(tn)), TN_register(tn));
}

/* ====================================================================
 *
 * Expand_Convert_Length
 *
 * Generate code to expand an xCVTL operator.  The code generated is a
 * left shift to put the upper bit to be kept in the high bit of the
 * word or double-word, followed by a right shift back to either sign-
 * or zero-extend it as requested.
 *
 * ====================================================================
 */

void
Expand_Convert_Length(TN *dest, TN *src, TN *length_tn, TYPE_ID mtype, BOOL signed_extension, OPS *ops)
{
    // Handle CVTL and CVT between integers
    // For CVTL node, mtype is the result type
    //                        length_tn is the result length
    // For CVT node, mtype is the source type
    //                        length_tn is zero

    INT16 new_length;	/* Length to convert to */
    TOP opc = TOP_UNDEFINED;
    TN *tmp = Build_TN_Like(dest);

    new_length = TN_value(length_tn);

    if (new_length == 0)
    {
        if (mtype == MTYPE_U8 || mtype == MTYPE_I8)
        {
            // I4U8CVT, I4I8CVT, U4U8CVT or U4I8CVT
            Build_OP(TOP_sll, dest, True_TN, src, Gen_Literal_TN(0, 2), ops);
            return;
        }
        else if (mtype == MTYPE_U4)
        {
            // U8U4CVT, I8U4CVT
            TN *tmp = Build_TN_Like(dest);
            Build_OP(TOP_dsll32, tmp, True_TN, src, Gen_Literal_TN(0, 2), ops);
            Build_OP(TOP_dsrl32, dest, True_TN, tmp, Gen_Literal_TN(0, 2), ops);
            return;
        }
        else if (mtype == MTYPE_I4)
        {
            // I8I4CVT, U8I4CVT
            Expand_Copy(dest, src, MTYPE_U8, ops);
        }
        else
        {
            Is_True(FALSE, ("Unexpacted case.\n"));
        }
        return;
    }



    // For CVTL  32
    if (new_length == 32)
    {
        if (mtype == MTYPE_U8 || mtype == MTYPE_I8)
        {
            // U8CVTL 32, I8CVTL 32
            Expand_Copy(dest, src, MTYPE_U4, ops);
        }
        else
        {
            // I4CVTL 32
            Is_True(FALSE, ("Unexpacted case.\n"));
        }
        return;
    }


    // For CVTL I8 U8 I4 U4 -> I2 U2 I1 U1
    // WOPT can generate non-byte-sized node, such as U8CVTL 33
    if (signed_extension)
    {
        if (mtype == MTYPE_I8)
        {
            // I8CVTL 8, I8CVTL 16
            Expand_Shift(tmp, src, Gen_Literal_TN(64 - new_length, 2), MTYPE_U8, shift_left, ops);
            Expand_Shift(dest, tmp, Gen_Literal_TN(64 - new_length, 2), MTYPE_U8, shift_aright, ops);
        }
        else
        {
            // I4CVTL 8, I4CVTL 16
            Expand_Shift(tmp, src, Gen_Literal_TN(32 - new_length, 2), MTYPE_U4, shift_left, ops);
            Expand_Shift(dest, tmp, Gen_Literal_TN(32 - new_length, 2), MTYPE_U4, shift_aright, ops);
        }
    }
    else
    {
        if (new_length <= 16)
        {
            // U4CVTL 8, U8CVTL 8, U4CVTL 16, U8CVTL 16
            Build_OP(TOP_andi, dest, True_TN, src, Gen_Literal_TN(((1 << new_length) - 1) & MASK16, 2), ops);
        }
        else if (new_length == 32)
        {
            Expand_Copy(dest, src, mtype, ops);
        }
        else
        {
            // U4CVTL 24, U8CVTL 48 ...
            Expand_Shift(tmp, src, Gen_Literal_TN(64 - new_length, 2), MTYPE_U8, shift_left, ops);
            Expand_Shift(dest, tmp, Gen_Literal_TN(64 - new_length, 2), MTYPE_U8, shift_lright, ops);
        }
    }

}

BOOL
Can_Do_Fast_Immediate(INT64 val)
{
    if (ISA_LC_Value_In_Class(val, LC_i32))
    {
        // Can be handled in less than 2 instructions
        return (FALSE);
    }

    // Power of 2
    if ((val & (val - 1)) == 0 && val != 0) return (TRUE);

    // 2**N-1
    if ((val & (val + 1)) == 0 && (val + 1) != 0) return (TRUE);

    if (ISA_LC_Value_In_Class(val, LC_k32))
    {
        // Can be handled with 3 instructions
        return (FALSE);
    }

    if ((val & MASK16) == 0)
    {
        if ((ISA_LC_Value_In_Class(val, LC_i48)) ||
                (ISA_LC_Value_In_Class(val, LC_k48)))
        {
            if (((val >> 16) & MASK16) == 0)
            {
                // 0x0000****00000000 or 0xffff****00000000
                return (TRUE);
            }
            else
            {
                // 0x0000********0000 or 0xffff********0000
                return (FALSE);
            }
        }
        else
        {
            if (((val >> 16) & MASK16) == 0)
            {
                // 0x****000000000000 or 0x********00000000
                return (TRUE);
            }
            else if (((val >> 32) & MASK16) == 0)
            {
                // 0x****0000****0000
                return (TRUE);
            }
            else
            {
                return (FALSE);
            }
        }
    }

    if (((val >> 16) & MASK16) == 0) return (TRUE);

    if (((val >> 32) & MASK16) == 0) return (TRUE);

    if ((val & MASK48) == MASK48)  return (TRUE);

    return (FALSE);
}

void
Exp_Fast_Immediate(TN *dest, INT64 val, BOOL is_signed, OPS *ops)
{
    INT num_ones = 0;
    TN *tmp = Build_TN_Like(dest);
    TN *tmp2 = Build_TN_Like(dest);

    // Power of 2
    if ((val & (val - 1)) == 0 && val != 0)
    {
        INT first_1 = 0;
        UINT64 uc = val;
        while ((uc & 1) == 0)
        {
            ++first_1;
            uc >>= 1;
        }

        Build_OP(TOP_ori, tmp, True_TN, Zero_TN, Gen_Literal_TN(1, 2), ops);
        Expand_Shift(dest, tmp, Gen_Literal_TN(first_1, 2), MTYPE_U8, shift_left, ops);
        return;
    }

    // 2**N-1
    if ((val & (val + 1)) == 0 && (val + 1) != 0)
    {

        UINT64 uc = val + 1;
        INT first_1 = 0;
        while ((uc & 1) == 0)
        {
            ++first_1;
            uc >>= 1;
        }
        Build_OP(TOP_addiu, tmp, True_TN, Zero_TN, Gen_Literal_TN(-1, 2), ops);
        Expand_Shift(dest, tmp, Gen_Literal_TN(64 - first_1, 2), MTYPE_U8, shift_lright, ops);
        return;
    }

    if ((val & MASK16) == 0)
    {
        if (ISA_LC_Value_In_Class(val, LC_i48))
        {
            if (((val >> 16) & MASK16) == 0)
            {
                // 0x0000****00000000 or 0xffff****00000000
                Build_OP(TOP_lui, tmp, True_TN, Gen_Literal_TN((val >> 32) & MASK16, 2), ops);
                Expand_Shift(dest, tmp, Gen_Literal_TN(16, 2), MTYPE_U8, shift_left, ops);
            }
            else
            {
                // 0x0000********0000 or 0xffff********0000
                Is_True(FALSE, ("Exp_Immediate can handle this case!\n"));
            }
        }
        else if (ISA_LC_Value_In_Class(val, LC_k48))
        {
            if (((val >> 16) & MASK16) == 0)
            {
                // 0x0000****00000000
                Build_OP(TOP_ori, tmp, True_TN, Zero_TN, Gen_Literal_TN((val >> 32) & MASK16, 2), ops);
                Expand_Shift(dest, tmp, Gen_Literal_TN(32, 2), MTYPE_U8, shift_left, ops);
            }
            else
            {
                // 0x0000********0000
                Is_True(FALSE, ("Exp_Immediate can handle this case!\n"));
            }
        }
        else
        {
            if (((val >> 16) & MASK16) == 0)
            {
                if (((val >> 32) & MASK16) == 0)
                {
                    // 0x****000000000000
                    Build_OP(TOP_ori, tmp, True_TN, Zero_TN, Gen_Literal_TN((val >> 48) & MASK16, 2), ops);
                    Expand_Shift(dest, tmp, Gen_Literal_TN(48, 2), MTYPE_U8, shift_left, ops);
                }
                else
                {
                    // 0x********00000000
                    Build_OP(TOP_lui, tmp, True_TN, Gen_Literal_TN((val >> 48) & MASK16, 2), ops);
                    Build_OP(TOP_ori, tmp, True_TN, tmp, Gen_Literal_TN((val >> 32) & MASK16, 2), ops);
                    Expand_Shift(dest, tmp, Gen_Literal_TN(32, 2), MTYPE_U8, shift_left, ops);
                }
            }
            else if (((val >> 32) & MASK16) == 0)
            {
                // 0x****0000****0000
                Build_OP(TOP_ori, tmp, True_TN, Zero_TN, Gen_Literal_TN((val >> 48) & MASK16, 2), ops);
                Expand_Shift(tmp, tmp, Gen_Literal_TN(32, 2), MTYPE_U8, shift_left, ops);
                Build_OP(TOP_ori, tmp, True_TN, tmp, Gen_Literal_TN((val >> 16) & MASK16, 2), ops);
                Expand_Shift(dest, tmp, Gen_Literal_TN(16, 2), MTYPE_U8, shift_left, ops);

            }
            else
            {
                Is_True(FALSE, ("Exp_Immediate can handle this case!\n"));
            }

        }
        return;
    }

    if (((val >> 16) & MASK16) == 0)
    {
        if (ISA_LC_Value_In_Class(val, LC_i48))
        {
            // 0x0000****0000**** or 0xffff****0000****
            Build_OP(TOP_lui, tmp, True_TN, Gen_Literal_TN((val >> 32) & MASK16, 2), ops);
            Expand_Shift(tmp, tmp, Gen_Literal_TN(16, 2), MTYPE_U8, shift_left, ops);
            Build_OP(TOP_ori, dest, True_TN, tmp, Gen_Literal_TN(val & MASK16, 2), ops);
        }
        else if (ISA_LC_Value_In_Class(val, LC_k48))
        {
            // 0x0000****0000****
            Build_OP(TOP_ori, tmp, True_TN, Zero_TN, Gen_Literal_TN((val >> 32) & MASK16, 2), ops);
            Expand_Shift(tmp, tmp, Gen_Literal_TN(32, 2), MTYPE_U8, shift_left, ops);
            Build_OP(TOP_ori, dest, True_TN, tmp, Gen_Literal_TN(val & MASK16, 2), ops);

        }
        else
        {
            if (((val >> 32) & MASK16) == 0)
            {
                // 0x****00000000****
                Build_OP(TOP_lui, tmp, True_TN, Gen_Literal_TN((val >> 48) & MASK16, 2), ops);
                Expand_Shift(tmp, tmp, Gen_Literal_TN(32, 2), MTYPE_U8, shift_left, ops);
                Build_OP(TOP_ori, dest, True_TN, tmp, Gen_Literal_TN(val & MASK16, 2), ops);

            }
            else
            {
                // 0x********0000****
                Build_OP(TOP_lui, tmp, True_TN, Gen_Literal_TN((val >> 48) & MASK16, 2), ops);
                Build_OP(TOP_ori, tmp, True_TN, tmp, Gen_Literal_TN((val >> 32) & MASK16, 2), ops);
                Expand_Shift(tmp, tmp, Gen_Literal_TN(32, 2), MTYPE_U8, shift_left, ops);
                Build_OP(TOP_ori, dest, True_TN, tmp, Gen_Literal_TN(val & MASK16, 2), ops);
            }
        }

        return;
    }

    if (((val >> 32) & MASK16) == 0)
    {
        // 0x****0000********
        Build_OP(TOP_lui, tmp, True_TN, Gen_Literal_TN((val >> 48) & MASK16, 2), ops);
        Expand_Shift(tmp, tmp, Gen_Literal_TN(16, 2), MTYPE_U8, shift_left, ops);
        Build_OP(TOP_ori, tmp, True_TN, tmp, Gen_Literal_TN((val >> 16) & MASK16, 2), ops);
        Expand_Shift(tmp, tmp, Gen_Literal_TN(16, 2), MTYPE_U8, shift_left, ops);
        Build_OP(TOP_ori, dest, True_TN, tmp, Gen_Literal_TN(val & MASK16, 2), ops);

        return;
    }

    if ((val & MASK48) == MASK48)
    {
        // 0x****FFFFFFFFFFFF
        // 0x****+1, shift left 48, then sub -1
        Build_OP(TOP_lui, tmp, True_TN, Gen_Literal_TN(((val >> 48) + 1) & MASK16, 2), ops);
        Expand_Shift(tmp, tmp, Gen_Literal_TN(32, 2), MTYPE_U8, shift_left, ops);
        Expand_Sub(dest, tmp, Gen_Literal_TN(1, 2), MTYPE_U8, ops);

        return;
    }
}

void
Exp_Immediate(TN *dest, TN *src, BOOL is_signed, OPS *ops)
{
    INT64 val;


    if (TN_has_value(src))
    {
        val = TN_value(src);
    }
    else if (TN_is_symbol(src))
    {
        ST *base;
        Base_Symbol_And_Offset_For_Addressing(TN_var(src), TN_offset(src), &base, &val);
    }
    if (Can_Do_Fast_Immediate(val))
    {
        Exp_Fast_Immediate(dest, val, is_signed, ops);
        return;
    }

    if (ISA_LC_Value_In_Class(val, LC_k16))
    {
        // [0,65535]=[0x0,0xffff]

        if (val == 0)
        {
            // Expand_Const can reach here if float zero
            // Int usually can not reach here, handled by whirl2ops.cxx:Expand_Expr
            Build_OP(TOP_or, dest, True_TN, Zero_TN, Zero_TN, ops);

            return;
        }
        else
        {
            // when >0 can use "ori" which is 0-extended
            // the possbile reason to use "ori" instead "addiu" is that the cpu cost of "ori" is less than "addiu"
            Build_OP(TOP_ori, dest, True_TN, Zero_TN, Gen_Literal_TN(val, 2), ops);
        }
    }
    else if (ISA_LC_Value_In_Class(val, LC_i16))
    {
        // [-32768,-1]=[0x8000,0xffff]

        Build_OP(TOP_addiu, dest, True_TN, Zero_TN, Gen_Literal_TN(val, 2), ops);
    }
    else if (ISA_LC_Value_In_Class(val, LC_i32))
    {
        // [-2147483648,2147483647]=[0x80000000,0x7fffffff]

        if ((val & MASK16) != 0)
        {
            // If the low 16 bits are all zero, no need to use ori
            TN *tmp = Build_TN_Like(dest);
            Build_OP(TOP_lui, tmp, True_TN, Gen_Literal_TN((val >> 16) & MASK16, 2), ops);
            Build_OP(TOP_ori, dest, True_TN, tmp, Gen_Literal_TN(val & MASK16, 2), ops);
        }
        else
        {
            Build_OP(TOP_lui, dest, True_TN, Gen_Literal_TN((val >> 16) & MASK16, 2), ops);
        }
    }
    else if (ISA_LC_Value_In_Class(val, LC_k32))
    {
        // [2147483648,4294967295]=[0x80000000,0xffffffff]
        // can not use lui, because we need 0-extended

        TN *tmp = Build_TN_Like(dest);
        Build_OP(TOP_ori, tmp, True_TN, Zero_TN, Gen_Literal_TN((val >> 16) & MASK16, 2), ops);
        // We use dsll instead of sll because sll will sign-extended
        if ((val & MASK16) != 0)
        {
            Build_OP(TOP_dsll, tmp, True_TN, tmp, Gen_Literal_TN(16, 2), ops);
            Build_OP(TOP_ori, dest, True_TN, tmp, Gen_Literal_TN(val & MASK16, 2), ops);
        }
        else
        {
            Build_OP(TOP_dsll, dest, True_TN, tmp, Gen_Literal_TN(16, 2), ops);
        }
    }
    else if (ISA_LC_Value_In_Class(val, LC_i48))
    {
        // [-140737488355328,140737488355327]=[0x800000000000,0x7fffffffffff]

        TN *tmp = Build_TN_Like(dest);
        Build_OP(TOP_lui, tmp, True_TN, Gen_Literal_TN((val >> 32) & MASK16, 2), ops);
        Build_OP(TOP_ori, tmp, True_TN, tmp, Gen_Literal_TN((val >> 16) & MASK16, 2), ops);
        if ((val & MASK16) != 0)
        {
            Build_OP(TOP_dsll, tmp, True_TN, tmp, Gen_Literal_TN(16, 2), ops);
            Build_OP(TOP_ori, dest, True_TN, tmp, Gen_Literal_TN(val & MASK16, 2), ops);
        }
        else
        {
            Build_OP(TOP_dsll, dest, True_TN, tmp, Gen_Literal_TN(16, 2), ops);
        }
    }
    else if (ISA_LC_Value_In_Class(val, LC_k48))
    {
        // [140737488355328,281474976710655]=[0x800000000000,0xffffffffffff]
        // can not use lui, because we need 0-extended

        TN *tmp = Build_TN_Like(dest);
        Build_OP(TOP_ori, tmp, True_TN, Zero_TN, Gen_Literal_TN((val >> 32) & MASK16, 2), ops);
        Build_OP(TOP_dsll, tmp, True_TN, tmp, Gen_Literal_TN(16, 2), ops);
        Build_OP(TOP_ori, tmp, True_TN, tmp, Gen_Literal_TN((val >> 16) & MASK16, 2), ops);
        if ((val & MASK16) != 0)
        {
            Build_OP(TOP_dsll, tmp, True_TN, tmp, Gen_Literal_TN(16, 2), ops);
            Build_OP(TOP_ori, dest, True_TN, tmp, Gen_Literal_TN(val & MASK16, 2), ops);
        }
        else
        {
            Build_OP(TOP_dsll, dest, True_TN, tmp, Gen_Literal_TN(16, 2), ops);
        }

    }
    else
    {
        // [-9223372036854775808,18446744073709551615]=[0x8000000000000000,0xffffffffffffffff]

        TN *tmp = Build_TN_Like(dest);
        Build_OP(TOP_lui, tmp, True_TN, Gen_Literal_TN((val >> 48) & MASK16, 2), ops);
        Build_OP(TOP_ori, tmp, True_TN, tmp, Gen_Literal_TN((val >> 32) & MASK16, 2), ops);
        Build_OP(TOP_dsll, tmp, True_TN, tmp, Gen_Literal_TN(16, 2), ops);
        Build_OP(TOP_ori, tmp, True_TN, tmp, Gen_Literal_TN((val >> 16) & MASK16, 2), ops);
        if ((val & MASK16) != 0)
        {
            Build_OP(TOP_dsll, tmp, True_TN, tmp, Gen_Literal_TN(16, 2), ops);
            Build_OP(TOP_ori, dest, True_TN, tmp, Gen_Literal_TN(val & MASK16, 2), ops);
        }
        else
        {
            Build_OP(TOP_dsll, dest, True_TN, tmp, Gen_Literal_TN(16, 2), ops);
        }
    }

}



/*
 * Expand Immediate value.
 */
void
Expand_Immediate(TN *dest, TN *src, BOOL is_signed, OPS *ops)
{
    FmtAssert((TN_is_constant(src)),
              ("unexpected non-constant in Expand_Immediate"));
    FmtAssert((TN_has_value(src) || TN_is_symbol(src)),
              ("expected value or const in Expand_Immediate"));
    Exp_Immediate(dest, src, TRUE, ops);
}

TN*
Expand_Immediate_Into_Register(TN *src, OPS *ops)
{
    if (TN_has_value(src))
    {
        if (TN_value(src) == 0)
        {
            return Zero_TN;
        }
    }

    /* load into reg and do reg case */
    TN *tmp = Build_TN_Of_Mtype(MTYPE_I8);
    Expand_Immediate(tmp, src, TRUE, ops);

    return tmp;
}

void
Expand_Add(TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    TOP new_opcode = TOP_addu;
    INT64 val;
    BOOL is_longlong = FALSE;
    TN* tmp_src1;

    if (mtype == MTYPE_I8 || mtype == MTYPE_U8)
    {
        is_longlong = TRUE;
    }

    if (is_longlong)
    {
        new_opcode = TOP_daddu;
    }

    if (TN_is_constant(src1))
    {
        if (TN_has_value(src1))
        {
            val = TN_value(src1);
            if (val == 0)
            {
                Expand_Copy(result, src2, mtype, ops);
                return;
            }

            // if (is_longlong) we will use "daddiu" as below
            if (ISA_LC_Value_In_Class(val, LC_i16))
            {
                if (! is_longlong)
                {
                    new_opcode = TOP_addiu;
                }
                else
                {
                    new_opcode = TOP_daddiu;
                }
            }
            else
            {
                src1 = Expand_Immediate_Into_Register(src1, ops);
            }
        }
        else if (TN_is_symbol(src1))
        {
            // Symbolic constant, gp-relative or sp-relative
            if (TN_is_gp_reg(src2))
            {
                if (is_longlong)
                {
                    new_opcode = TOP_daddiu;
                }
                else
                {
                    new_opcode = TOP_addiu;
                }
            }
            else
            {
                ST *base;
                INT64 ofst;
                Base_Symbol_And_Offset_For_Addressing(TN_var(src1), TN_offset(src1), &base, &ofst);

                if (ISA_LC_Value_In_Class(ofst, LC_k16))
                {
                    if (is_longlong)
                    {
                        // can reach here if we use int* b=&(int)a, point is 64 bits
                        new_opcode = TOP_daddiu;
                    }
                    else
                    {
                        new_opcode = TOP_addiu;
                    }
                }
                else if (ST_is_export_local(TN_var(src1))
                         && ofst > 0
                         && ISA_LC_Value_In_Class(ofst, LC_k16)\
                         && !ST_on_stack(TN_var(src1))
                         && !((ST_class(TN_var(src1)) == CLASS_BLOCK\
                               || ST_class(TN_var(src1)) == CLASS_VAR)
                              && ST_gprel(TN_var(src1))))
                {

                    // Only display the name of sym in .s file
                    if (is_longlong)
                    {
                        new_opcode = TOP_daddiu;
                    }
                    else
                    {
                        new_opcode = TOP_addiu;
                    }
                }
                else
                {
                    src1 = Expand_Immediate_Into_Register(src1, ops);

                    if (is_longlong)
                    {
                        new_opcode = TOP_daddu;
                    }
                    else
                    {
                        new_opcode = TOP_addu;
                    }
                }
            }
        }
        else
        {
            FmtAssert(FALSE, ("unexpected constant in Expand_Add"));
        }
    }
    else if (TN_is_constant(src2))
    {

        // switch order of src so immediate is first
        Expand_Add(result, src2, src1, mtype, ops);
        return;
    }
    Build_OP(new_opcode, result, True_TN, src2, src1, ops);

}

void
Expand_Sub(TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    INT64 val;
    TOP new_opcode = TOP_subu;
    TN* src1_pair, *src2_pair, *result_pair, *tmp;
    BOOL is_longlong = FALSE;

    if (mtype == MTYPE_U8 || mtype == MTYPE_I8)
    {
        is_longlong = TRUE;
    }

    if (is_longlong)
    {
        new_opcode = TOP_dsubu;
    }

    if (TN_is_constant(src1))
    {
        if (TN_has_value(src1))
        {
            src1 = Expand_Immediate_Into_Register(src1, ops);
        }
        else
        {
            FmtAssert(FALSE, ("unexpected constant in Expand_Sub"));
        }
    }
    else if (TN_is_constant(src2))
    {
        if (TN_has_value(src2))
        {

            // whirl will usually change this kind of sub to add
            val = TN_value(src2);
            if (ISA_LC_Value_In_Class(val, LC_i16))
            {

                // no matter longlong and int?!
                // return the negative of the value
                val = -val;
                src2 = Gen_Literal_TN(val, 4);
                Expand_Add(result, src1, src2, mtype, ops);

                return;
            }
            else
            {
                src2 = Expand_Immediate_Into_Register(src2, ops);
            }
        }
        else if (TN_is_symbol(src2))
        {
            /* symbolic constant; return negative of value */
            src2 = Gen_Symbol_TN(TN_var(src2), TN_offset(src2),
                                 TN_RELOC_NEG);
            Expand_Add(result, src1, src2, mtype, ops);

            return;
        }
    }

    Build_OP(new_opcode, result, True_TN, src1, src2, ops);
}


void
Expand_Neg(TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
    /* neg op0 -> subu $0, op0 */
    if (TN_is_constant(src))
    {
        Expand_Immediate_Into_Register(src, ops);
        DevWarn("Negative a constant!\n");
    }

    if (TN_is_float(src))
    {
        switch (mtype)
        {
        case MTYPE_F4:
            Build_OP(TOP_neg_s, result, True_TN, src, ops);
            break;
        case MTYPE_F8:
        {
            Build_OP(TOP_neg_d, result, True_TN, src, ops);
            break;
        }
        default:
            Is_True(FALSE, ("Can not negative such float TN!\n"));
        }
    }
    else if (mtype == MTYPE_U8 || mtype == MTYPE_I8)
    {
        Build_OP(TOP_dsubu, result, True_TN, Zero_TN, src, ops);
    }
    else
    {
        Build_OP(TOP_subu, result, True_TN, Zero_TN, src, ops);
    }

}

void
Expand_Abs(TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
    // <<See MIPS Run>> Table 8.2
    // abs d,s ->
    //   sra at,s,31
    //   xor d,s,at
    //   sub d,d,at

    if (mtype == MTYPE_U4 || mtype == MTYPE_U8)
    {
        DevWarn("Try to get Abs value of an unsigned number!\n");
        Expand_Copy(dest, src, mtype, ops);
        return;
    }

    TN *tmp1, *tmp2;
    tmp1 = Build_TN_Like(dest);
    tmp2 = Build_TN_Like(dest);

    if (mtype == MTYPE_I8)
    {
        Build_OP(TOP_dsra32, tmp1, True_TN, src, Gen_Literal_TN(31, 2), ops);
        Build_OP(TOP_xor, tmp2, True_TN, src, tmp1, ops);
        Build_OP(TOP_dsub, dest, True_TN, tmp2, tmp1, ops);
    }
    else if (mtype == MTYPE_I4)
    {
        Build_OP(TOP_sra, tmp1, True_TN, src, Gen_Literal_TN(31, 2), ops);
        Build_OP(TOP_xor, tmp2, True_TN, src, tmp1, ops);
        Build_OP(TOP_sub, dest, True_TN, tmp2, tmp1, ops);
    }
    else
    {
        Is_True(FALSE, ("Can not handle this ABS case!\n"));
    }

}

void
Expand_Shift(TN *result, TN *src1, TN *src2, TYPE_ID mtype, SHIFT_DIRECTION kind, OPS *ops)
{
    TOP new_opcode;
    UINT wordsize = MTYPE_is_size_double(mtype) ? 64 : 32;

    if (TN_has_value(src2))
    {
        UINT64 val = TN_value(src2);

        // On IA-64, shifts with a shift count >= wordsize, are equivalent
        // to performing a shift by the wordsize. This is different than
        // mips where only the low log2(wordsize) bits of the shift count
        // were used. For constant shifts, the immediate field is not
        // large enough for such counts, so simulate the effect by
        // replacing the result with 0, or the sign bit of the source
        // for shift_aright.

        if (val >= wordsize)
        {
            val = val % wordsize;
            // Need front-end to change the VH whirl
        }

        if (kind == shift_left)
        {
            if (wordsize == 64)
            {
                if (val < 32)
                {
                    Build_OP(TOP_dsll, result, True_TN, src1, Gen_Literal_TN(val, 4), ops);
                }
                else
                {
                    Build_OP(TOP_dsll32, result, True_TN, src1, Gen_Literal_TN(val - 32, 4), ops);
                }
            }
            else
            {
                Build_OP(TOP_sll, result, True_TN, src1, Gen_Literal_TN(val, 4), ops);
            }
            return;
        }

        // Right-shifts
        // The immediate shift instructions perform a 64-bit shift.
        // If we use one of them, we might need to do a sign/zero-extend
        // later. So instead we use extr/extr.u to properly extend the result.
        if (wordsize == 64)
        {
            // Handle I8/U8 shift right logical
            if (val < 32)
            {
                new_opcode = (kind == shift_aright) ? TOP_dsra : TOP_dsrl;
                Build_OP(new_opcode, result, True_TN, src1, Gen_Literal_TN(val, 4), ops);
            }
            else
            {
                new_opcode = (kind == shift_aright) ? TOP_dsra32 : TOP_dsrl32;
                Build_OP(new_opcode, result, True_TN, src1, Gen_Literal_TN(val - 32, 4), ops);
            }
        }
        else
        {
            new_opcode = (kind == shift_aright) ? TOP_sra : TOP_srl;
            Build_OP(new_opcode, result, True_TN, src1, Gen_Literal_TN(val, 4), ops);
        }

        return;
    }

    // The shift is a variable shift
    if (wordsize != 64)
    {
        switch (kind)
        {
        case shift_left:
            new_opcode = TOP_sllv;
            break;
        case shift_aright:
            new_opcode = TOP_srav;
            break;
        case shift_lright:
            new_opcode = TOP_srlv;
            break;
        default:
            FmtAssert(FALSE, ("Unaccepted 32-bit variant shift operations!"));
        }

        TN *r1 = DUP_TN(result);
        if (kind == shift_left)
        {
            Build_OP(new_opcode, r1, True_TN, src1, src2, ops);
            Expand_Copy(result, r1, mtype, ops);
        }
        else
        {

            // Right shifts, we convert the source first
            if (kind == shift_aright)
            {
                Expand_Copy(r1, src1, MTYPE_I4, ops);
            }
            else
            {
                Expand_Copy(r1, src1, MTYPE_U4, ops);
            }

            Build_OP(new_opcode, result, True_TN, r1, src2, ops);
        }

    }
    else
    {
        switch (kind)
        {
        case shift_left:
            new_opcode = TOP_dsllv;
            break;
        case shift_aright:
            new_opcode = TOP_dsrav;
            break;
        case shift_lright:
            new_opcode = TOP_dsrlv;
            break;
        default:
            FmtAssert(FALSE, ("Unaccepted 64-bit variant shift operations!"));
        }

        Build_OP(new_opcode, result, True_TN, src1, src2, ops);
    }
}

inline void
Expand_G_To_F(TN *ftn, TN *gtn, OPS *ops)
{
    FmtAssert(FALSE, ("Unimplemented"));
}

inline void
Expand_F_To_G(TN *gtn, TN *ftn, OPS *ops)
{
    FmtAssert(FALSE, ("Unimplemented"));
}

/*
 *
 * Helper routine for Expand_Small_Multiply
 *
 */
static void shladd(TN *r, TN *x1, INT s, TN *x2, TYPE_ID mtype, OPS *ops)
{
    TN *r1 = r;
    if (TN_is_dedicated(r))
        r1 = Build_TN_Of_Mtype(mtype);
    Expand_Shift(r1, x1, Gen_Literal_TN(s, 2), mtype, shift_left, ops);

    // If the addend is zero, the add is not needed
    if ((TN_is_constant(x2) && TN_has_value(x2) && TN_value(x2) == 0)
            || (x2 == Zero_TN))
        return;

    Expand_Add(r, r1, x2, mtype, ops);
}

/*
 *
 * Helper routine for Expand_Small_Multiply, such as 7
 *
 */
static void shlsub(TN *r, TN *x1, INT s, TN *x2, TYPE_ID mtype, OPS *ops)
{
    TN *r1 = r;
    if (TN_is_dedicated(r))
        r1 = Build_TN_Of_Mtype(mtype);
    Expand_Shift(r1, x1, Gen_Literal_TN(s, 2), mtype, shift_left, ops);

    // If the addend is zero, the add is not needed
    if ((TN_is_constant(x2) && TN_has_value(x2) && TN_value(x2) == 0)
            || (x2 == Zero_TN))
        return;

    Expand_Sub(r, r1, x2, mtype, ops);
}

/*
 * Expand_Small_Multiply produces an optimized expansion of
 * multiplication by any constant between -1 and 63. Multiplication is done for 64
 * bit quantities only.
 *
 */
static void
Expand_Small_Multiply(TN *r,  		// result
                      TN *x,  		// source
                      INT16 val, 	// multiplicand
                      TYPE_ID mtype, 	// to diff 64-bit and 32-bit
                      OPS * ops) 	// place to put the ops
{
    TN *r1;
    TN *r2;
    TN *Z = Zero_TN;

#define ONE_TEMP r1=Build_TN_Of_Mtype(mtype)
#define TWO_TEMPS ONE_TEMP; r2=Build_TN_Of_Mtype(mtype)

    // Although ugly, a big case statement is I think the best way to express this
    // First 3 cases are already optimized by WHIRL
    switch (val)
    {
    case -1:
        Expand_Neg(r, x, mtype, ops);
        break;
    case 0:
        Expand_Copy(r, Z, mtype, ops);
        break;
    case  1 :
        Expand_Copy(r, x, mtype, ops);
        break;
    case  2 :
        shladd(r, x, 1, Z, mtype, ops);
        break;
    case  3 :
        shladd(r, x, 1, x, mtype, ops);
        break;
    case  4 :
        shladd(r, x, 2, Z, mtype, ops);
        break;
    case  5 :
        shladd(r, x, 2, x, mtype, ops);
        break;
    case  6:
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 3, mtype, ops);
        Expand_Small_Multiply(r, r1, 2, mtype, ops);
        break;
    case  7 :
        shlsub(r, x, 3, x, mtype, ops);
        break;
    case  8 :
        shladd(r, x, 3, Z, mtype, ops);
        break;
    case  9 :
        shladd(r, x, 3, x, mtype, ops);
        break;
    case  10 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 5, mtype, ops);
        Expand_Small_Multiply(r, r1, 2, mtype, ops);
        break;
    case  11 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 9, mtype, ops);
        shladd(r, x, 1, r1, mtype, ops);
        break;
    case  12 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 3, mtype, ops);
        Expand_Small_Multiply(r, r1, 4, mtype, ops);
        break;
    case  13 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 5, mtype, ops);
        shladd(r, x, 3, r1, mtype, ops);
        break;
    case  14 :
        ONE_TEMP;
        // 14=7*2
        shlsub(r1, x, 3, x, mtype, ops);
        shladd(r, r1, 1, Z, mtype, ops);
        break;
    case  15 :
        shlsub(r, x, 4, x, mtype, ops);
        break;
    case  16 :
        shladd(r, x, 4, Z, mtype, ops);
        break;
    case  17 :
        shladd(r, x, 4, x, mtype, ops);
        break;
    case  18 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 9, mtype, ops);
        Expand_Small_Multiply(r, r1, 2, mtype, ops);
        break;
    case  19 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 9, mtype, ops);
        shladd(r, r1, 1, x, mtype, ops);
        break;
    case  20 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 5, mtype, ops);
        Expand_Small_Multiply(r, r1, 4, mtype, ops);
        break;
    case  21 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 5, mtype, ops);
        shladd(r, x, 4, r1, mtype, ops);
        break;
    case  22 :
        TWO_TEMPS;
        Expand_Small_Multiply(r1, x, 17, mtype, ops);
        Expand_Small_Multiply(r2, x, 5, mtype, ops);
        Expand_Add(r, r1, r2, mtype, ops);
        break;
    case  23 :
        ONE_TEMP;
        shlsub(r1, x, 2, x, mtype, ops);
        shlsub(r, r1, 3, x, mtype, ops);
        break;
    case  24 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 3, mtype, ops);
        Expand_Small_Multiply(r, r1, 8, mtype, ops);
        break;
    case  25 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 5, mtype, ops);
        Expand_Small_Multiply(r, r1, 5, mtype, ops);
        break;
    case  26 :
        TWO_TEMPS;
        Expand_Small_Multiply(r1, x, 17, mtype, ops);
        Expand_Small_Multiply(r2, x, 9, mtype, ops);
        Expand_Add(r, r1, r2, mtype, ops);
        break;
    case  27 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 9, mtype, ops);
        Expand_Small_Multiply(r, r1, 3, mtype, ops);
        break;
    case  28 :
        ONE_TEMP;
        shlsub(r1, x, 3, x, mtype, ops);
        shladd(r, r1, 2, Z, mtype, ops);
        break;
    case  29 :
        ONE_TEMP;
        shlsub(r1, x, 3, x, mtype, ops);
        shladd(r, r1, 2, x, mtype, ops); // 7*4+1
        break;
    case  30 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 15, mtype, ops);
        Expand_Small_Multiply(r, r1, 2, mtype, ops);
        break;
    case  31 :
        shlsub(r, x, 5, x, mtype, ops);
        break;
    case  32 :
        shladd(r, x, 5, Z, mtype, ops);
        break;
    case  33 :
        shladd(r, x, 5, x, mtype, ops);
        break;
    case  34 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 17, mtype, ops);
        Expand_Small_Multiply(r, r1, 2, mtype, ops);
        break;
    case  35 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 17, mtype, ops);
        shladd(r, r1, 1, x, mtype, ops);
        break;
    case  36 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 9, mtype, ops);
        Expand_Small_Multiply(r, r1, 4, mtype, ops);
        break;
    case  37 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 9, mtype, ops);
        shladd(r, r1, 2, x, mtype, ops);
        break;
    case  38 :
        TWO_TEMPS;
        Expand_Small_Multiply(r1, x, 17, mtype, ops);
        Expand_Small_Multiply(r2, x, 4, mtype, ops);
        shladd(r, r1, 1, r2, mtype, ops);
        break;
    case  39 :
        ONE_TEMP;
        shladd(r1, x, 2, x, mtype, ops);
        shlsub(r, r1, 3, x, mtype, ops);
        break;
    case  40 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 5, mtype, ops);
        Expand_Small_Multiply(r, r1, 8, mtype, ops);
        break;
    case  41 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 5, mtype, ops);
        shladd(r, r1, 3, x, mtype, ops);
        break;
    case  42 :
        TWO_TEMPS;
        Expand_Small_Multiply(r1, x, 5, mtype, ops);
        Expand_Small_Multiply(r2, x, 2, mtype, ops);
        shladd(r, r1, 3, r2, mtype, ops);
        break;
    case  43 :
        TWO_TEMPS;
        Expand_Small_Multiply(r1, x, 5, mtype, ops);
        Expand_Small_Multiply(r2, x, 3, mtype, ops);
        shladd(r, r1, 3, r2, mtype, ops);
        break;
    case  44 :
        TWO_TEMPS;
        Expand_Small_Multiply(r1, x, 5, mtype, ops);
        Expand_Small_Multiply(r2, x, 4, mtype, ops);
        shladd(r, r1, 3, r2, mtype, ops);
        break;
    case  45 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 5, mtype, ops);
        Expand_Small_Multiply(r, r1, 9, mtype, ops);
        break;
    case  46 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 45, mtype, ops);
        Expand_Add(r, r1, x, mtype, ops);
        break;
    case  47 :
        ONE_TEMP;
        shlsub(r1, x, 2, x, mtype, ops);
        shlsub(r, r1, 4, x, mtype, ops);
        break;
    case  48 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 3, mtype, ops);
        Expand_Small_Multiply(r, r1, 16, mtype, ops);
        break;
    case  49 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 3, mtype, ops);
        shladd(r, r1, 4, x, mtype, ops);
        break;
    case  50 :
        TWO_TEMPS;
        Expand_Small_Multiply(r1, x, 3, mtype, ops);
        Expand_Small_Multiply(r2, x, 2, mtype, ops);
        shladd(r, r1, 4, r2, mtype, ops);
        break;
    case  51 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 3, mtype, ops);
        Expand_Small_Multiply(r, r1, 17, mtype, ops);
        break;
    case  52 :
        TWO_TEMPS;
        Expand_Small_Multiply(r1, x, 3, mtype, ops);
        Expand_Small_Multiply(r2, x, 4, mtype, ops);
        shladd(r, r1, 4, r2, mtype, ops);
        break;
    case  53 :
        TWO_TEMPS;
        Expand_Small_Multiply(r1, x, 3, mtype, ops);
        Expand_Small_Multiply(r2, x, 5, mtype, ops);
        shladd(r, r1, 4, r2, mtype, ops);
        break;
    case  54 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 9, mtype, ops);
        Expand_Small_Multiply(r, r1, 6, mtype, ops);
        break;
    case  55 :
        ONE_TEMP;
        shlsub(r1, x, 3, x, mtype, ops);
        shlsub(r, r1, 3, x, mtype, ops);
        break;
    case  56 :
        ONE_TEMP;
        shlsub(r1, x, 3, x, mtype, ops);
        shladd(r, r1, 3, Z, mtype, ops);
        break;
    case  57 :
        ONE_TEMP;
        shlsub(r1, x, 3, x, mtype, ops);
        shladd(r, r1, 3, x, mtype, ops);
        break;
    case  58 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 29, mtype, ops);
        Expand_Small_Multiply(r, r1, 2, mtype, ops);
        break;
    case  59 :
        ONE_TEMP;
        shlsub(r1, x, 4, x, mtype, ops);
        shlsub(r, r1, 2, x, mtype, ops);
        break;
    case  60 :
        ONE_TEMP;
        shlsub(r1, x, 4, x, mtype, ops);
        shladd(r, r1, 2, Z, mtype, ops);

        break;
    case  61 :
        ONE_TEMP;
        shlsub(r1, x, 4, x, mtype, ops);
        shladd(r, r1, 2, x, mtype, ops);
        break;
    case  62 :
        ONE_TEMP;
        Expand_Small_Multiply(r1, x, 31, mtype, ops);
        Expand_Small_Multiply(r, r1, 2, mtype, ops);
        break;
    case  63 :
        shlsub(r, x, 6, x, mtype, ops);
        break;
    default:
#pragma mips_frequency_hint NEVER
        FmtAssert(0, ("Can't small multiply by %d", val));
        /*NOTREACHED*/
    }
}

/*
 * Expand the multiply into a series of shifts and adds,
 * unless the sequence is longer than "limit".
 */
static BOOL
Expand_Multiply_Into_Shifts(
    TN	   *result_tn,
    TN	   *var_tn,
    TARG_UINT constant,
    INT16	    limit,
    TYPE_ID   mtype,
    OPS 	*ops)
{
    TN *tmp_tn;
    switch (constant)
    {
    case 0:
        if (limit < 1) return FALSE;
        Expand_Copy(result_tn, Zero_TN, mtype, ops);
        return TRUE;
    case 1:
        if (limit < 1) return FALSE;
        Expand_Copy(result_tn, var_tn, mtype, ops);
        return TRUE;
    case 2:
        if (limit < 1) return FALSE;
        Expand_Add(result_tn, var_tn, var_tn, mtype, ops);
        return TRUE;
    default:
        if ((constant % 2) == 1)  		/* odd */
        {
            tmp_tn = DUP_TN(result_tn);
            if ((constant & 2) != 0)
            {
                if (limit < 2) return FALSE;
                if (!Expand_Multiply_Into_Shifts(
                            tmp_tn, var_tn,
                            constant + 1, limit - 1,
                            mtype, ops))
                    return FALSE;
                Expand_Sub(result_tn, tmp_tn, var_tn, mtype, ops);
                return TRUE;
            }
            else
            {
                if (limit < 2) return FALSE;
                if (!Expand_Multiply_Into_Shifts(
                            tmp_tn, var_tn,
                            constant - 1, limit - 1,
                            mtype, ops))
                    return FALSE;
                Expand_Add(result_tn, tmp_tn, var_tn, mtype, ops);
                return TRUE;
            }
        }
        else                    		/* even */
        {
            INT shift_cnt = 0;
            while ((constant % 2) == 0)  	/* even */
            {
                shift_cnt++;
                constant = (TARG_UINT)constant >> 1;
            } /*while*/
            if (constant == 1)
            {
                if (limit < 1) return FALSE;
                Expand_Shift(result_tn, var_tn, Gen_Literal_TN(shift_cnt, 4), mtype, shift_left, ops);
                return TRUE;
            }
            else
            {
                if (limit < 2) return FALSE;
                tmp_tn = DUP_TN(result_tn);
                if (!Expand_Multiply_Into_Shifts(
                            tmp_tn, var_tn,
                            constant, limit - 1,
                            mtype, ops))
                    return FALSE;
                Expand_Shift(result_tn, tmp_tn, Gen_Literal_TN(shift_cnt, 4), mtype, shift_left, ops);
                return TRUE;
            }
        }
    }
}

/*
 *  Try to expand a multiply into a sequence of less expensive operations.
 */
#define NUM_FAST_MPYS 28
static INT fast_mpys[NUM_FAST_MPYS] =
    {1025, 1024, 1023, 513, 512, 511, 257, 256, 255, 129, 128, 127,
     65, 64, 63, 33, 32, 31, 17, 16, 15, 9, 8, 7, 5, 4, 3, 2
    };

static BOOL
Expand_Constant_Multiply(TN *result, TN *var_tn, TARG_INT constant, TYPE_ID mtype, OPS *ops)
{
    BOOL did_do_fast;
    INT16 limit;	/* maximum number of operations to replace the multiply */
    TN *x = var_tn;
    INT64 c = constant;
    BOOL needs_sign_extension;

    // fast special cases
    if (c == 0)
    {
        Expand_Copy(result, Zero_TN, MTYPE_I8, ops);
        return TRUE;
    }
    else if (c == 1)
    {
        // No mater MTYPE_I8 or MTYPE_I4, will use mipsor zero_tn to copy
        Expand_Copy(result, var_tn, MTYPE_I8, ops);
        return TRUE;
    }
    else if (c == -1)
    {
        Expand_Neg(result, var_tn, mtype, ops);
        return TRUE;
    }

    needs_sign_extension = MTYPE_size_reg(mtype) != 32;

    if (c < 0)
    {
        c = -c;
        x = DUP_TN(var_tn);
        Expand_Neg(x, var_tn, mtype, ops);
    }


    // Count the number of 1's in c and -c
    INT num_ones = 0;
    UINT64 uc = c;
    while (uc)
    {
        num_ones += (uc & 1);
        uc >>= 1;
    }
    uc = c;

    //
    // Small constants always make sense to use the optimized sequences
    //
    if (uc <= 63)
    {
        if (needs_sign_extension)
        {
            Expand_Small_Multiply(result, x, uc, mtype, ops);
        }
        else
        {
            TN *r1 = Build_TN_Of_Mtype(MTYPE_I8);
            Expand_Small_Multiply(r1, x, uc, mtype, ops);
            // It is a copy from I8(U8) to I8(U8)
            // Needed when shladd the addent is Zero_TN
            Fixup_32_Bit_Op(result, r1, mtype, ops);
        }
        return TRUE;
    }

    //
    // We have |constant| > 63, with the fewest number of 1's
    // Find where the (least significant) 1 is located.
    // If there is exactly one 1 in it, we will use a shift to do the multiply.
    //
    INT first_1 = 0;
    while ((uc & 1) == 0)
    {
        ++first_1;
        uc >>= 1;
    }
    if (first_1 != 0)
    {
        if (num_ones == 1)
        {
            // Just do the shift
            Expand_Shift(result, x, Gen_Literal_TN(first_1, 4), mtype, shift_left, ops);
            return TRUE;
        }
        else
        {
            TN *x1 = DUP_TN(x);
            Expand_Shift(x1, x, Gen_Literal_TN(first_1, 4), mtype, shift_left, ops);
            x = x1;
        }
    }
    //
    // Another special case, 2**N - 1
    // Note that num_ones can't be 64 (or we'd have been in the -1 case above)
    // So the shift and subtract test is safe here.
    // Also, we don't want to do this case if uc is small, because we can do better
    // with the optimized sequences.
    //
    if (uc == ((1 << num_ones) - 1) && uc > 63)
    {
        TN *r1 = DUP_TN(result);
        Expand_Shift(r1, x, Gen_Literal_TN(num_ones, 4), mtype, shift_left, ops);
        if (!needs_sign_extension)
        {
            Expand_Sub(result, r1, x, mtype, ops);
        }
        else
        {
            TN *r2 = DUP_TN(result);
            Expand_Sub(r2, r1, x, mtype, ops);
            Fixup_32_Bit_Op(result, r2, mtype, ops);
        }
        return TRUE;
    }

    //
    // Now we handle 2**N + 1
    //
    if ((((uc - 1) & (uc - 2)) == 0) && ((uc - 1) != 0) && (uc > 63))
    {
        INT first_1 = 0;
        UINT64 val = uc - 1;
        while ((val & 1) == 0)
        {
            ++first_1;
            val >>= 1;
        }
        shladd(result, x, first_1, x, mtype, ops);
        return TRUE;
    }

    //
    // Check for some cases we can do with a two-instruction multiply on top
    // of a small multiply. e.g. 2**n-1 and 2**n + 1
    //
    INT i;
    for (i = 0; i < NUM_FAST_MPYS; i++)
    {
        INT mpy = fast_mpys[i];
        if (uc % mpy == 0 && uc / mpy <= 63)
        {
            INT64 uc1;
            TN *r1 = DUP_TN(result);
            Expand_Small_Multiply(r1, x, uc / mpy, mtype, ops);
            Expand_Constant_Multiply(result, r1, mpy, mtype, ops);
            return TRUE;
        }
    }

    //
    // We put things in r to make the possible sign extension a bit easier
    //
    TN *r = result;
    if (needs_sign_extension)
    {
        r = DUP_TN(result);
    }
    //
    // If the remaining number is less than 16 bits, we will do it by
    // breaking it into chunks and combining them. We also handle a few special cases.
    // For numbers greater than 16 bits, we break things up and combine recursively.
    // This is implemented for completeness but probably shouldn't be done in practice.
    //
    if (uc <= 63)
    {
        Expand_Small_Multiply(r, x, uc, mtype, ops);
    }
    else if (uc <= 1023)
    {
        INT64 uc1, uc2;
        TN *r1 = DUP_TN(result);
        // Do in group of 4 and at most 6
        // Note that uc2 >= 4 (or we would have been in the above case)
        uc1 = uc & 15;
        uc2 = uc >> 4;

        Expand_Small_Multiply(r1, x, uc2, mtype, ops);
        if (uc1 == 0)
        {
            shladd(r, r1, 4, Zero_TN, mtype, ops);
        }
        else if (uc1 == 1)
        {
            shladd(r, r1, 4, x, mtype, ops);
        }
        else if (uc1 == uc2)
        {
            shladd(r, r1, 4, r1, mtype, ops);
        }
        else
        {
            TN *r2 = DUP_TN(result);
            Expand_Small_Multiply(r2, x, uc1, mtype, ops);
            shladd(r, r1, 4, r2, mtype, ops);
        }
    }
    else if (uc <= 65535)
    {
        // Do in two groups of 8. Note that uc2 >= 4 again.
        // Also not that because we are combining with 2 shladds, we have
        // additional opportunities for optimizations
        // if the low part is a multiple of 16 or 17 (smaller multiplies
        // tend to be a bit faster), or the low part is 16 or 17x the high part
        // we get it for free.
        //
        INT64 uc1, uc2;
        TN *r1 = DUP_TN(result);
        TN *r2 = DUP_TN(result);
        uc1 = uc & 255;
        uc2 = uc >> 8;
        Expand_Constant_Multiply(r1, x, uc2, mtype, ops);
        if (uc1 == 0)
        {
            shladd(r2, r1, 4, Zero_TN, mtype, ops);
            shladd(r, r2, 4, Zero_TN, mtype, ops);
        }
        else if (uc1 == 1)
        {
            shladd(r2, r1, 4, Zero_TN, mtype, ops);
            shladd(r, r2, 4, x, mtype, ops);

        }
        else if (uc1 == 16)
        {
            shladd(r2, r1, 4, x, mtype, ops);
            shladd(r, r2, 4, Zero_TN, mtype, ops);

        }
        else if (uc1 == 17)
        {
            shladd(r2, r1, 4, x, mtype, ops);
            shladd(r, r2, 4, x, mtype, ops);

        }
        else if (uc1 == uc2)
        {
            shladd(r2, r1, 4, Zero_TN, mtype, ops);
            shladd(r, r2, 4, r1, mtype, ops);

        }
        else if (uc1 == 16*uc2)
        {
            shladd(r2, r1, 4, r1, mtype, ops);
            shladd(r, r2, 4, Zero_TN, mtype, ops);

        }
        else if (uc1 == 17*uc2)
        {
            shladd(r2, r1, 4, r1, mtype, ops);
            shladd(r, r2, 4, r1, mtype, ops);

        }
        else if (uc1 % 16 == 0)
        {
            TN *r3 = DUP_TN(result);
            uc1 /= 16;
            Expand_Constant_Multiply(r3, x, uc1, mtype, ops);
            shladd(r2, r1, 4, r3, mtype, ops);
            shladd(r, r2, 4, Zero_TN, mtype, ops);

        }
        else if (uc1 % 17 == 0)
        {
            TN *r3 = DUP_TN(result);
            uc1 /= 17;
            Expand_Constant_Multiply(r3, x, uc1, mtype, ops);
            shladd(r2, r1, 4, r3, mtype, ops);
            shladd(r, r2, 4, r3, mtype, ops);

        }
        else
        {
            TN *r3 = DUP_TN(result);
            Expand_Constant_Multiply(r3, x, uc1, mtype, ops);
            shladd(r2, r1, 4, Zero_TN, mtype, ops);
            shladd(r, r2, 4, r3, mtype, ops);
        }
    }
    else if (uc <= ((1LL << 32) - 1))
    {
        //
        // For completeness, although it's probably getting to be not worth it
        // for the sheer number of instructions generated, even if the latency is good
        // (latency <= 8, instructions <= 34)
        //
        INT64 uc1, uc2;
        TN *r1 = DUP_TN(result);
        TN *r2 = DUP_TN(result);
        TN *r3 = DUP_TN(result);
        uc1 = uc & 65535;
        uc2 = uc >> 16;
        Expand_Constant_Multiply(r1, x, uc1, mtype, ops);
        Expand_Constant_Multiply(r2, x, uc2, mtype, ops);
        Expand_Shift(r3, r2, Gen_Literal_TN(16, 4), mtype, shift_left, ops);
        Expand_Add(r, r1, r3, mtype, ops);
    }
    else
    {
        //
        // Worst case, latency <= 11, instructions <= 70
        // You really don't want to do this, but we will just let Can_Do_Fast_Multiply stop it
        //
        // For completeness, although it's probably getting to be not worth it
        // for the sheer number of instructions generated, even if the latency is good
        // (latency <= 8, instructions <= 34)
        //
        INT64 uc1, uc2;
        TN *r1 = DUP_TN(result);
        TN *r2 = DUP_TN(result);
        TN *r3 = DUP_TN(result);
        uc1 = uc & MASK32;
        uc2 = uc >> 32;
        Expand_Constant_Multiply(r1, x, uc1, mtype, ops);
        Expand_Constant_Multiply(r2, x, uc2, mtype, ops);
        Expand_Shift(r3, r2, Gen_Literal_TN(32, 4), mtype, shift_left, ops);
        Expand_Add(r, r1, r3, mtype, ops);
    }

    if (needs_sign_extension)
    {
        Expand_Copy(result, r, mtype, ops);
    }

    return TRUE;
}

void
Expand_Multiply(TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops, OPCODE opcode)
{
    TOP new_opcode;
    INT64 constant;
    //
    // Check for two constants
    //
    if ((TN_has_value(src1) || TN_is_rematerializable(src1)) &&
            (TN_has_value(src2) || TN_is_rematerializable(src2)))
    {

        // Two constants can sometimes occur because of DIVREM production in
        TN *val_tn;
        constant = TN_has_value(src1) ? TN_value(src1) : WN_const_val(TN_home(src1));
        constant *= TN_has_value(src2) ? TN_value(src2) : WN_const_val(TN_home(src2));

        // Need to get the constant of the right length
        constant = Targ_To_Host(Host_To_Targ(mtype, constant));
        val_tn = Gen_Literal_TN(constant, 8);
        Exp_Immediate(result, val_tn, MTYPE_is_signed(mtype), ops);
        return;
    }

    if (!Disable_Const_Mult_Opt && (TN_has_value(src1) || TN_has_value(src2) ||
                                    TN_is_rematerializable(src1) || TN_is_rematerializable(src2)))
    {
        TN *var_tn;
        if (TN_has_value(src1) || TN_is_rematerializable(src1))
        {
            constant = TN_has_value(src1) ? TN_value(src1) : WN_const_val(TN_home(src1));
            var_tn = src2;
        }
        else
        {
            constant = TN_has_value(src2) ? TN_value(src2) : WN_const_val(TN_home(src2));
            var_tn = src1;
        }

        if (Can_Do_Fast_Multiply(mtype, constant))
        {
            if (Expand_Constant_Multiply(result, var_tn, constant, mtype, ops))
            {
                /* able to convert multiply into shifts/adds/subs */
                return;
            }
        }
    }

    if (TN_has_value(src2))
    {
        src2 = Expand_Immediate_Into_Register(src2, ops);
    }

    /* Optimize any special cases
     */
    if (CGEXP_fast_imul)
    {
        switch (mtype)
        {
        case MTYPE_U1:
        case MTYPE_U2:
        case MTYPE_U4:
            if (!OPT_Space)
            {
                if (CGEXP_use_Loongson2e_MultDivMod)
                {
                    Build_OP(TOP_multu_g, result, True_TN, src1, src2, ops);
                }
                else
                {
                    Build_OP(TOP_multu, HI_TN, LO_TN, True_TN, src1, src2, ops);
                    Build_OP(TOP_mflo, result, True_TN, LO_TN, ops);
                }
            }
            break;
        case MTYPE_U8:
            if (!OPT_Space)
            {
                if (CGEXP_use_Loongson2e_MultDivMod)
                {
                    Build_OP(TOP_dmultu_g, result, True_TN, src1, src2, ops);
                }
                else
                {
                    Build_OP(TOP_dmultu, HI_TN, LO_TN, True_TN, src1, src2, ops);
                    Build_OP(TOP_mflo, result, True_TN, LO_TN, ops);
                }
            }
            break;

        case MTYPE_I1:
        case MTYPE_I2:
        case MTYPE_I4:
            if (!OPT_Space)
            {

                if (CGEXP_use_Loongson2e_MultDivMod)
                {
                    Build_OP(TOP_mult_g, result, True_TN, src1, src2, ops);
                }
                else
                {
                    Build_OP(TOP_mult, HI_TN, LO_TN, True_TN, src1, src2, ops);
                    Build_OP(TOP_mflo, result, True_TN, LO_TN, ops);
                }
                return;
            }
            break;
        case MTYPE_I8:
            if (!OPT_Space)
            {

                if (CGEXP_use_Loongson2e_MultDivMod)
                {
                    Build_OP(TOP_dmult_g, result, True_TN, src1, src2, ops);
                }
                else
                {
                    Build_OP(TOP_dmult, HI_TN, LO_TN, True_TN, src1, src2, ops);
                    Build_OP(TOP_mflo, result, True_TN, LO_TN, ops);
                }
            }
            break;
        }
    }
}

/* return high part of multiply result */
void
Expand_High_Multiply(TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    TOP new_opcode;

    if (TN_has_value(src2))
    {
        src2 = Expand_Immediate_Into_Register(src2, ops);
    }

    switch (mtype)
    {
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
        if (!OPT_Space)
        {
            Build_OP(TOP_multu, HI_TN, LO_TN, True_TN, src1, src2, ops);
            Build_OP(TOP_mfhi, result, True_TN, HI_TN, ops);
            return;
        }
        break;
    case MTYPE_U8:
        if (!OPT_Space)
        {
            Build_OP(TOP_dmultu, HI_TN, LO_TN, True_TN, src1, src2, ops);
            Build_OP(TOP_mfhi, result, True_TN, HI_TN, ops);
            return;
        }
        break;
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
        if (!OPT_Space)
        {
            Build_OP(TOP_mult, HI_TN, LO_TN, True_TN, src1, src2, ops);
            Build_OP(TOP_mfhi, result, True_TN, HI_TN, ops);
            return;
        }
        break;
    case MTYPE_I8:
        if (!OPT_Space)
        {
            Build_OP(TOP_dmult, HI_TN, LO_TN, True_TN, src1, src2, ops);
            Build_OP(TOP_mfhi, result, True_TN, HI_TN, ops);
            return;
        }
        break;
    default:
        Is_True(FALSE, ("Expand_High_Multiply can not handle this case!\n"));
    }
}

static void
Expand_Normalize_Logical(TN *src, OPS *ops)
{
    FmtAssert(FALSE, ("Unimplemented"));
}

void
Expand_Logical_Not(TN *dest, TN *src, VARIANT variant, OPS *ops)
{
    /* dest = (src == 0) ? 1 : 0 */
    if (TN_register_class(dest) == ISA_REGISTER_CLASS_predicate)
    {
        FmtAssert(FALSE, ("Unimplemented"));
    }
    else
    {
        /*
         *  if CG_EXP_normalize is true we must normalized the operands
         *  (if not already normalized)
         */
        if (!V_normalized_op1(variant) && CGEXP_normalize_logical)
        {
            Expand_Normalize_Logical(src, ops);
        }
        TN *temp = NULL;
        temp = Build_TN_Like(src);
        Build_OP(TOP_sltu, temp, True_TN, Zero_TN, src, ops);
        Build_OP(TOP_xori, dest, True_TN, temp, Gen_Literal_TN(1, 4), ops);
    }
}

/*
**	dest = (src1 != 0 & src2 != 0) ? 1 : 0
**	sltu	t1, 0, s1		(if not normalized)
**	sltu	t2, 0, s2		(if not normalized)
**	and/or	d, t1, t2
*/
static void
Expand_Logical_And_Or(TOP action, TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
    /*
     *  if CG_EXP_normalize is true we must normalized the operands
     *  (if not already normalized)
     */
    if (!V_normalized_op1(variant) && CGEXP_normalize_logical)
    {
        Expand_Normalize_Logical(src1, ops);
    }
    if (!V_normalized_op2(variant) && CGEXP_normalize_logical)
    {
        Expand_Normalize_Logical(src2, ops);
    }

    Build_OP(action, dest, True_TN, src1, src2, ops);
}

void
Expand_Logical_And(TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
    Expand_Logical_And_Or(TOP_and, dest, src1, src2, variant, ops);
}

void
Expand_Logical_Or(TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
    Expand_Logical_And_Or(TOP_or, dest, src1, src2, variant, ops);
}

void
Expand_Binary_Complement(TN *dest, TN *src, TYPE_ID /* mtype */, OPS *ops)
{
    /* complement == nor src $0 or
                           == xor src -1*/

    // nor is prefered because xor need one additional register for -1.
    Build_OP(TOP_nor, dest, True_TN, Zero_TN, src, ops);
}


/* Expand special cases of AND with an immediate. These are cases
 * where the constant is too big for TOP_and_i, but can be handled
 * more simply than loading the constant into a register and using TOP_and.
 *
 * NOTE: that 'mix' could be used to zero fixed patterns of bytes and
 * shorts, but I couldn't find a case to trigger it so I left it out -- Ken
 */
BOOL Expand_Special_And_Immed(TN *dest, TN *src1, INT64 imm, OPS *ops)
{
    FmtAssert(FALSE, ("Unimplemented"));
}


static void
Expand_Binary_And_Or(TOP action, TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    /* binary AND: and || andi */
    if (TN_has_value(src2))
    {
        if (ISA_LC_Value_In_Class(TN_value(src2), LC_k16))
        {
            action = Pick_Imm_Form_TOP(action);
            Build_OP(action, dest, True_TN, src1, src2, ops);
            return;
        }
        else
        {
            src2 = Expand_Immediate_Into_Register(src2, ops);
        }
    }
    else if (TN_has_value(src1))
    {
        Expand_Binary_And_Or(action, dest, src2, src1, mtype, ops);
        return;
    }

    Build_OP(action, dest, True_TN, src1, src2, ops);
}

void
Expand_Binary_And(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    Expand_Binary_And_Or(TOP_and, dest, src1, src2, mtype, ops);
}

void
Expand_Binary_Or(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    Expand_Binary_And_Or(TOP_or, dest, src1, src2, mtype, ops);
}

void
Expand_Binary_Xor(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    Expand_Binary_And_Or(TOP_xor, dest, src1, src2, mtype, ops);
}

void
Expand_Binary_Nor(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    // nor is or s1,s2; xor -1
    // since mips provide nor instruction, use it directly

    // NOTE: if one of the operands is constant, the expansion
    // could be 'andcm ~imm,s2' which is one inst if the complemented
    // constant fits in the immed. But testing has not found a
    // case where this would occur, so leave it out. -- Ken
    Build_OP(TOP_nor, dest, True_TN, src1, src2, ops);
}

void
Expand_Int_Less(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    VARIANT variant;
    TOP action;
    switch (mtype)
    {
    case MTYPE_I8:
        variant = V_BR_I8LT;
        break;
    case MTYPE_I4:
        variant = V_BR_I4LT;
        break;
    case MTYPE_U8:
        variant = V_BR_U8LT;
        break;
    case MTYPE_U4:
        variant = V_BR_U4LT;
        break;
    default:
#pragma mips_frequency_hint NEVER
        Is_True(FALSE, ("Expand_Int_Less: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }
    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both value tn!\n"));

    TN * src2_register = NULL;
    INT32 is_signed = FALSE;

    /* Front end has already guaranteed  that src1 is not be constant.
     --George Her  11.4, 2003 */
    Check_Src1;

    switch (variant)
    {
    case V_BR_I8LT:
    case V_BR_I4LT:
        is_signed = TRUE;
    case V_BR_U8LT:
    case V_BR_U4LT:
        if (TN_has_value(src2))
        {
            if (TN_is_zero(src2))
            {
                if (is_signed)
                {
                    (mtype == MTYPE_I8) ?
                    Expand_Shift(dest, src1, Gen_Literal_TN(63, 2), mtype, shift_lright, ops)
                    : Build_OP(TOP_srl, dest, True_TN, src1, Gen_Literal_TN(31, 2), ops);
                }
                else
                    Build_OP(TOP_sltu, dest, True_TN, src1, Zero_TN, ops);
            }
            else if (ISA_LC_Value_In_Class(TN_value(src2), LC_i16))
            {
                is_signed ?
                Build_OP(TOP_slti, dest, True_TN, src1, Gen_Literal_TN(TN_value(src2), 2), ops)
                : Build_OP(TOP_sltiu, dest, True_TN, src1, Gen_Literal_TN(TN_value(src2) & MASK16, 2), ops);
            }
            else
            {
                src2_register = Build_TN_Like(dest);
                Expand_Immediate(src2_register, src2, is_signed, ops);
            }
        }

        if (!TN_has_value(src2) || src2_register)
        {
            is_signed ?
            Build_OP(TOP_slt, dest, True_TN, src1, TN_is_register(src2) ? src2 : src2_register, ops)
                    : Build_OP(TOP_sltu, dest, True_TN, src1, TN_is_register(src2) ? src2 : src2_register, ops);
        }
        break;
    default:
        Is_True(FALSE, ("Can not handle this logical operation!\n"));
    }
}

void
Expand_Int_Less_Equal(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    VARIANT variant;
    TOP action;
    switch (mtype)
    {
    case MTYPE_I8:
        variant = V_BR_I8LE;
        break;
    case MTYPE_I4:
        variant = V_BR_I4LE;
        break;
    case MTYPE_U8:
        variant = V_BR_U8LE;
        break;
    case MTYPE_U4:
        variant = V_BR_U4LE;
        break;
    default:
#pragma mips_frequency_hint NEVER
        Is_True(FALSE, ("Expand_Int_Less_Equal: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }

    TN * src2_register = NULL;
    INT32 is_signed = FALSE;

    /* Front end has already guaranteed  that src1 is not be constant.
      --George Her  11.4, 2003 */
    Check_Src1;

    switch (variant)
    {
    case V_BR_I8LE:
    case V_BR_I4LE:
        is_signed = TRUE;
    case V_BR_U8LE:
    case V_BR_U4LE:
        if (TN_has_value(src2))
        {
            if (TN_is_zero(src2))
            {
                is_signed ?
                Build_OP(TOP_slti, dest, True_TN, src1, Gen_Literal_TN(1, 2), ops)
                : Build_OP(TOP_sltiu, dest, True_TN, src1, Gen_Literal_TN(1, 2), ops);
            }
            else if (TN_value(src2) == MAX_UNSIGNED && !is_signed)
            {
                Build_OP(TOP_ori, dest, True_TN, Zero_TN, Gen_Literal_TN(1, 2), ops);
            }
            else if (ISA_LC_Value_In_Class(TN_value(src2) + 1, LC_i16))
            {
                is_signed ?
                Build_OP(TOP_slti, dest, True_TN, src1, Gen_Literal_TN(TN_value(src2) + 1, 2), ops)
                : Build_OP(TOP_sltiu, dest, True_TN, src1, Gen_Literal_TN((TN_value(src2) + 1) & MASK16, 2), ops);
            }
            else
            {
                src2_register = Build_TN_Like(dest);
                Expand_Immediate(src2_register, src2, is_signed, ops);
            }
        }

        if (!TN_has_value(src2)  || src2_register)
        {
            TN *tmp = Build_TN_Like(dest);
            is_signed ?
            Build_OP(TOP_slt, tmp, True_TN, TN_is_register(src2) ? src2 : src2_register, src1, ops)
                    : Build_OP(TOP_sltu, tmp, True_TN, TN_is_register(src2) ? src2 : src2_register, src1, ops);
            Build_OP(TOP_xori, dest, True_TN, tmp, Gen_Literal_TN(1, 2), ops);
        }
        break;
    default:
        Is_True(FALSE, ("Can not handle this logical operation!\n"));
    }

}

void
Expand_Int_Equal(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    VARIANT variant;
    TOP action;
    switch (mtype)
    {
    case MTYPE_I8:
        variant = V_BR_I8EQ;
        break;
    case MTYPE_I4:
        variant = V_BR_I4EQ;
        break;
    case MTYPE_U8:
        variant = V_BR_U8EQ;
        break;
    case MTYPE_U4:
        variant = V_BR_U4EQ;
        break;
    default:
#pragma mips_frequency_hint NEVER
        Is_True(FALSE, ("Expand_Int_Equal: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }

    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both value tn!\n"));

    /* Front end has already guaranteed  that src1 is not be constant.
    --George Her  11.4, 2003 */
    Check_Src1;

    TN * src2_register = NULL;

    switch (variant)
    {
    case V_BR_I4EQ:
    case V_BR_I8EQ:
    case V_BR_U4EQ:
    case V_BR_U8EQ:
        if (TN_has_value(src2))
        {
            if (TN_is_zero(src2))
            {
                Build_OP(TOP_sltiu, dest, True_TN, src1, Gen_Literal_TN(1, 2), ops);
            }
            else if (ISA_LC_Value_In_Class(TN_value(src2), LC_k16))
            {
                TN *tmp = Build_TN_Like(dest);
                Build_OP(TOP_xori, tmp, True_TN, src1, Gen_Literal_TN(TN_value(src2), 2), ops);
                Build_OP(TOP_sltiu, dest, True_TN, tmp, Gen_Literal_TN(1, 2), ops);
            }
            else
            {
                src2_register = Build_TN_Like(dest);
                Expand_Immediate(src2_register, src2, TRUE, ops);
            }
        }
        if (!TN_has_value(src2) || src2_register)
        {
            TN *tmp = Build_TN_Like(dest);
            Build_OP(TOP_xor, tmp, True_TN, src1, TN_is_register(src2) ? src2 : src2_register, ops);
            Build_OP(TOP_sltiu, dest, True_TN, tmp, Gen_Literal_TN(1, 2), ops);
        }
        break;

    default:
        Is_True(FALSE, ("Can not handle this logical operation!\n"));

    }
}

void
Expand_Int_Not_Equal(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    VARIANT variant;
    TOP action;
    switch (mtype)
    {
    case MTYPE_I8:
        variant = V_BR_I8NE;
        break;
    case MTYPE_I4:
        variant = V_BR_I4NE;
        break;
    case MTYPE_U8:
        variant = V_BR_U8NE;
        break;
    case MTYPE_U4:
        variant = V_BR_U4NE;
        break;
    default:
#pragma mips_frequency_hint NEVER
        Is_True(FALSE, ("Expand_Int_Not_Equal: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }

    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both value tn!\n"));

    /* Front end has already guaranteed  that src1 is not be constant.
    --George Her  11.4, 2003 */
    Check_Src1;

    TN * src2_register = NULL;

    switch (variant)
    {
    case V_BR_I4NE:
    case V_BR_I8NE:
    case V_BR_U4NE:
    case V_BR_U8NE:
        if (TN_has_value(src2))
        {
            if (TN_is_zero(src2))
            {
                Build_OP(TOP_sltu, dest, True_TN, Zero_TN, src1, ops);
            }
            else if (ISA_LC_Value_In_Class(TN_value(src2), LC_k16))
            {
                TN *tmp = Build_TN_Like(dest);
                Build_OP(TOP_xori, tmp, True_TN, src1, Gen_Literal_TN(TN_value(src2), 2), ops);
                Build_OP(TOP_sltu, dest, True_TN, Zero_TN, tmp, ops);
            }
            else
            {
                src2_register = Build_TN_Like(dest);
                Expand_Immediate(src2_register, src2, TRUE, ops);
            }
        }

        if (!TN_has_value(src2) || src2_register)
        {
            TN *tmp = Build_TN_Like(dest);
            Build_OP(TOP_xor, tmp, True_TN, src1, TN_is_register(src2) ? src2 : src2_register, ops);
            Build_OP(TOP_sltu, dest, True_TN, Zero_TN, tmp, ops);
        }
        break;

    default:
        Is_True(FALSE, ("Can not handle this logical operation!\n"));
    }
}

void
Expand_Int_Greater_Equal(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    VARIANT variant;
    TOP action;
    switch (mtype)
    {
    case MTYPE_I8:
        variant = V_BR_I8GE;
        break;
    case MTYPE_I4:
        variant = V_BR_I4GE;
        break;
    case MTYPE_U8:
        variant = V_BR_U8GE;
        break;
    case MTYPE_U4:
        variant = V_BR_U4GE;
        break;
    default:
#pragma mips_frequency_hint NEVER
        Is_True(FALSE, ("Expand_Int_Greater_Equal: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }
    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both value tn!\n"));

    /* Front end has already guaranteed  that src1 is not be constant.
    --George Her  11.4, 2003 */
    Check_Src1;

    INT32  is_signed = FALSE;
    TN * src2_register = NULL;

    switch (variant)
    {
    case V_BR_I8GE:
    case V_BR_I4GE:
        is_signed = TRUE;
    case V_BR_U8GE:
    case V_BR_U4GE:
        if (TN_has_value(src2))
        {
            if (TN_is_zero(src2))
            {
                /* If dest is a dedicated tn, putting it at SRC position may cause
                   Localize_Global_Return_Reg() check failed */
                TN *temp = Build_TN_Like(dest);
                Build_OP(TOP_nor, temp, True_TN, src1, Zero_TN, ops);
                if (is_signed)
                {
                    (mtype == MTYPE_I8) ?
                    Expand_Shift(dest, temp, Gen_Literal_TN(63, 2), mtype, shift_lright, ops)
                    : 	Build_OP(TOP_srl, dest, True_TN, temp, Gen_Literal_TN(31 , 1), ops);
                }
                else 	 Build_OP(TOP_sltu, dest, True_TN, src1, Zero_TN, ops);
            }
            else if (ISA_LC_Value_In_Class(TN_value(src2), LC_i16))
            {
                TN *tmp = Build_TN_Like(dest);
                is_signed ?
                Build_OP(TOP_slti, tmp, True_TN, src1, Gen_Literal_TN(TN_value(src2), 2), ops)
                : Build_OP(TOP_sltiu, tmp, True_TN, src1, Gen_Literal_TN(TN_value(src2), 2), ops);

                Build_OP(TOP_xori, dest, True_TN, tmp, Gen_Literal_TN(1, 2), ops);
            }
            else
            {
                src2_register = Build_TN_Like(dest);
                Expand_Immediate(src2_register, src2, is_signed, ops);
            }
        }

        if (!TN_has_value(src2) || src2_register)
        {
            TN *tmp = Build_TN_Like(dest);
            is_signed ?
            Build_OP(TOP_slt, tmp, True_TN, src1, TN_is_register(src2) ? src2 : src2_register, ops)
                    : Build_OP(TOP_sltu, tmp, True_TN, src1, TN_is_register(src2) ? src2 : src2_register, ops);

            Build_OP(TOP_xori, dest, True_TN, tmp, Gen_Literal_TN(1, 2), ops);

        }

        break;
    default:
        Is_True(FALSE, ("Can not handle this logical operation!\n"));

    }

}

void
Expand_Int_Greater(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    VARIANT variant;
    TOP action;
    switch (mtype)
    {
    case MTYPE_I8:
        variant = V_BR_I8GT;
        break;
    case MTYPE_I4:
        variant = V_BR_I4GT;
        break;
    case MTYPE_U8:
        variant = V_BR_U8GT;
        break;
    case MTYPE_U4:
        variant = V_BR_U4GT;
        break;
    default:
#pragma mips_frequency_hint NEVER
        Is_True(FALSE, ("Expand_Int_Greater: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }

    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both value tn!\n"));

    /* Front end has already guaranteed  that src1 is not be constant.
      --George Her  11.4, 2003 */
    Check_Src1;

    INT32  is_signed = FALSE;
    TN * src2_register = NULL;
    switch (variant)
    {
    case V_BR_I8GT:
    case V_BR_I4GT:
        is_signed = TRUE;
    case V_BR_U8GT:
    case V_BR_U4GT:
        if (TN_has_value(src2))
        {
            if (TN_is_zero(src2))
            {
                TN *tmp = Build_TN_Like(dest);
                is_signed ?
                Build_OP(TOP_slti, tmp, True_TN, src1, Gen_Literal_TN(1, 2), ops)
                : Build_OP(TOP_sltiu, tmp, True_TN, src1, Gen_Literal_TN(1, 2), ops);
                Build_OP(TOP_xori, dest, True_TN, tmp, Gen_Literal_TN(1, 2), ops);
            }
            else if (TN_value(src2) == MAX_UNSIGNED && !is_signed)
            {
                Expand_Copy(dest, Zero_TN, mtype, ops);
            }
            else if (ISA_LC_Value_In_Class(TN_value(src2) + 1, LC_i16))
            {
                TN *tmp = Build_TN_Like(dest);
                is_signed ?
                Build_OP(TOP_slti, tmp, True_TN, src1, Gen_Literal_TN(TN_value(src2) + 1, 2), ops)
                : Build_OP(TOP_sltiu, tmp, True_TN, src1, Gen_Literal_TN((TN_value(src2) + 1) & MASK16, 2), ops);
                Build_OP(TOP_xori, dest, True_TN, tmp, Gen_Literal_TN(1, 2), ops);
            }
            else
            {
                src2_register = Build_TN_Like(dest);
                Expand_Immediate(src2_register, src2, is_signed, ops);
            }
        }

        if (!TN_has_value(src2) || src2_register)
        {
            is_signed ?
            Build_OP(TOP_slt, dest, True_TN, TN_is_register(src2) ? src2 : src2_register, src1, ops)
                    : Build_OP(TOP_sltu, dest, True_TN, TN_is_register(src2) ? src2 : src2_register, src1, ops);
        }
        break;
    default:
        Is_True(FALSE, ("Can not handle this logical operation!\n"));

    }

}

void
Expand_Bool_Equal(TN *dest, TN *src1, TN *src2, OPS *ops)
{
    FmtAssert(FALSE, ("Unimplemented"));
}

void
Expand_Bool_Not_Equal(TN *dest, TN *src1, TN *src2, OPS *ops)
{
    FmtAssert(FALSE, ("Unimplemented"));
}

void
Expand_Bool_To_Int(TN *dest, TN *src, TYPE_ID rtype, OPS *ops)
{
    FmtAssert(FALSE, ("Unimplemented"));
}

typedef enum
{
    ROUND_USER,
    ROUND_NEAREST,
    ROUND_CHOP,
    ROUND_NEG_INF,
    ROUND_PLUS_INF
} ROUND_MODE;

static void
Expand_Float_To_Int(ROUND_MODE rm, TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
    TOP top;
    BOOL int_64bit = MTYPE_byte_size(imtype) == 8;
    if (fmtype == MTYPE_F4)
    {
        if (int_64bit || !(MTYPE_is_signed(imtype)))
        {
            switch (rm)
            {
            case ROUND_USER:
                top = TOP_cvt_l_s;
                break;
            case ROUND_NEAREST:
                top = TOP_round_l_s;
                break;
            case ROUND_CHOP:
                top = TOP_trunc_l_s;
                break;
            case ROUND_NEG_INF:
                top = TOP_floor_l_s;
                break;
            case ROUND_PLUS_INF:
                top = TOP_ceil_l_s;
                break;
            default:
                FmtAssert(FALSE, ("Unimplemented rounding mode"));
            }
        }
        else  	// int_32bit && MTYPE_is_signed(imtype)
        {
            switch (rm)
            {
            case ROUND_USER:
                top = TOP_cvt_w_s;
                break;
            case ROUND_NEAREST:
                top = TOP_round_w_s;
                break;
            case ROUND_CHOP:
                top = TOP_trunc_w_s;
                break;
            case ROUND_NEG_INF:
                top = TOP_floor_w_s;
                break;
            case ROUND_PLUS_INF:
                top = TOP_ceil_w_s;
                break;
            default:
                FmtAssert(FALSE, ("Unimplemented rounding mode"));
            }
        }
    }
    else if (fmtype == MTYPE_F8)
    {
        if (int_64bit || !(MTYPE_is_signed(imtype)))
        {
            switch (rm)
            {
            case ROUND_USER:
                top = TOP_cvt_l_d;
                break;
            case ROUND_NEAREST:
                top = TOP_round_l_d;
                break;
            case ROUND_CHOP:
                top = TOP_trunc_l_d;
                break;
            case ROUND_NEG_INF:
                top = TOP_floor_l_d;
                break;
            case ROUND_PLUS_INF:
                top = TOP_ceil_l_d;
                break;
            default:
                FmtAssert(FALSE, ("Unimplemented rounding mode"));
            }
        }
        else
        {
            switch (rm)
            {
            case ROUND_USER:
                top = TOP_cvt_w_d;
                break;
            case ROUND_NEAREST:
                top = TOP_round_w_d;
                break;
            case ROUND_CHOP:
                top = TOP_trunc_w_d;
                break;
            case ROUND_NEG_INF:
                top = TOP_floor_w_d;
                break;
            case ROUND_PLUS_INF:
                top = TOP_ceil_w_d;
                break;
            default:
                FmtAssert(FALSE, ("Unimplemented rounding mode"));
            }
        }
    }

    else FmtAssert(FALSE, ("unsupported float size in Expand_Float_To_Int"));

    TN *tmp = Build_TN_Of_Mtype(fmtype);

    /* It is quite possible that the result type of such converting
     * whirl node could be Float type,eg. FLOOR; according to definition
     * of "floor" in C/C++; the result type should be "double" instead of
     * "int" or "long"
     *      --George Her
     */
    if (TN_is_float(dest))
    {
        Build_OP(top, tmp, True_TN, src, ops);
        Build_OP(int_64bit ? TOP_cvt_d_l : TOP_cvt_s_w, dest, True_TN, tmp, ops);
        return;
    }

    Build_OP(top, tmp, True_TN, src, ops);
    Build_OP(int_64bit ? TOP_dmfc1 : TOP_mfc1, dest, True_TN, tmp, ops);


}

void
Expand_Float_To_Int_Cvt(TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
    Expand_Float_To_Int(ROUND_USER, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Round(TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
    Expand_Float_To_Int(ROUND_NEAREST, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Trunc(TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
    TN* temp_f;
    TN *tmp;

    switch (imtype)
    {
    case MTYPE_I4:
        temp_f = Build_TN_Of_Mtype(MTYPE_F4);
        if (fmtype == MTYPE_F4)
        {

            Build_OP(TOP_trunc_w_s, temp_f, True_TN, src, ops);
        }
        else
        {
            Build_OP(TOP_trunc_w_d, temp_f, True_TN, src, ops);
        }
        Build_OP(TOP_mfc1, dest, True_TN, temp_f, ops);

        break;
    case MTYPE_I8:
        temp_f = Build_TN_Of_Mtype(MTYPE_F8);
        if (fmtype == MTYPE_F4)
        {
            Build_OP(TOP_trunc_l_s, temp_f, True_TN, src, ops);
        }
        else
        {
            Build_OP(TOP_trunc_l_d, temp_f, True_TN, src, ops);
        }
        Build_OP(TOP_dmfc1, dest, True_TN, temp_f, ops);
        break;

    case MTYPE_U4:
        temp_f = Build_TN_Of_Mtype(MTYPE_F8);
        if (fmtype == MTYPE_F4)
        {
            Build_OP(TOP_trunc_l_s, temp_f, True_TN, src, ops);
        }
        else
        {
            Build_OP(TOP_trunc_l_d, temp_f, True_TN, src, ops);
        }
        tmp = Build_TN_Of_Mtype(MTYPE_I8);
        Build_OP(TOP_dmfc1, tmp, True_TN, temp_f, ops);
        Build_OP(TOP_sll, dest, True_TN, tmp, Gen_Literal_TN(0, 2), ops);
        break;

    case MTYPE_U8:
    {
        TN *bound, *temp, *temp_dest, *temp_src;
        TN *targ_gt_bound, *targ_exit;

        LABEL_IDX lab_idx_gt_bound, lab_idx_exit;
        LABEL* lb_gt_bound = &New_LABEL(CURRENT_SYMTAB, lab_idx_gt_bound);
        LABEL* lb_exit =  &New_LABEL(CURRENT_SYMTAB, lab_idx_exit);

        targ_gt_bound = Gen_Label_TN(lab_idx_gt_bound, 0);
        targ_exit = Gen_Label_TN(lab_idx_exit, 0);

        // Because the result is U8 need to use double TN
        temp_f = Build_TN_Of_Mtype(MTYPE_F8);
        // To contain the 0x8000000000000000
        bound = Build_TN_Of_Mtype(fmtype);
        temp = Build_TN_Of_Mtype(MTYPE_U8);
        temp_dest = Build_TN_Of_Mtype(MTYPE_U8);
        temp_src = Build_TN_Of_Mtype(fmtype);

        // Get the float bound 0x8000000000000000 = 9223372036854775808.0
        Expand_Const(bound, NULL, fmtype, ops);

        // Check whether the src > 0x8000000000000000
        VARIANT variant = (fmtype == MTYPE_F4) ?  V_BR_FLE : V_BR_DLE;
        Set_V_false_br(variant);
        Expand_Branch(targ_gt_bound, bound, src, variant, ops);

        // Handle the src >= 0x8000000000000000
        Begin_New_Basic_Block();

        // Sub the 9223372036854775808.0 to fit the range of trunc_l_*
        Expand_Flop((fmtype == MTYPE_F4) ? OPC_F4SUB : OPC_F8SUB,
                    temp_src, src, bound, NULL, ops);
        if (fmtype == MTYPE_F4)
        {
            Build_OP(TOP_trunc_l_s, temp_f, True_TN, src, ops);
        }
        else
        {
            Build_OP(TOP_trunc_l_d, temp_f, True_TN, src, ops);
        }
        Build_OP(TOP_dmfc1, temp_dest, True_TN, temp_f, ops);
        // Regenerate the 63th bit
        Expand_Binary_Or(temp_dest, temp_dest, Gen_Literal_TN(TWO_POWER_63_HEX_VALUE, 8), MTYPE_U8, ops);
        Build_OP(TOP_j, True_TN, targ_exit, ops);

        // Handle the src < 0x8000000000000000
        Begin_New_Basic_Block();
        Start_New_Label(lab_idx_gt_bound, lb_gt_bound, "Trunc_GT_0x8000000000000000");
        if (fmtype == MTYPE_F4)
        {
            Build_OP(TOP_trunc_l_s, temp_f, True_TN, src, ops);
        }
        else
        {
            Build_OP(TOP_trunc_l_d, temp_f, True_TN, src, ops);
        }
        Build_OP(TOP_dmfc1, temp_dest, True_TN, temp_f, ops);


        // Exit the convert
        Begin_New_Basic_Block();
        Start_New_Label(lab_idx_exit, lb_exit, "Trunc_exit");
        Expand_Copy(dest, temp_dest, MTYPE_U8, ops);

        break;
    }
    default:
        Is_True(FALSE, ("Expand_Float_To_Int_Trunc can not handle this case!\n"));
    }

}

void
Expand_Float_To_Int_Floor(TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
    Expand_Float_To_Int(ROUND_NEG_INF, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Ceil(TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
    TN* temp_f;
    TN *tmp;

    switch (imtype)
    {
    case MTYPE_I4:
        temp_f = Build_TN_Of_Mtype(MTYPE_F4);
        if (fmtype == MTYPE_F4)
        {

            Build_OP(TOP_ceil_w_s, temp_f, True_TN, src, ops);
        }
        else
        {
            Build_OP(TOP_ceil_w_d, temp_f, True_TN, src, ops);
        }
        Build_OP(TOP_mfc1, dest, True_TN, temp_f, ops);

        break;
    case MTYPE_I8:
        temp_f = Build_TN_Of_Mtype(MTYPE_F8);
        if (fmtype == MTYPE_F4)
        {
            Build_OP(TOP_ceil_l_s, temp_f, True_TN, src, ops);
        }
        else
        {
            Build_OP(TOP_ceil_l_d, temp_f, True_TN, src, ops);
        }
        Build_OP(TOP_dmfc1, dest, True_TN, temp_f, ops);
        break;

    case MTYPE_U4:
        temp_f = Build_TN_Of_Mtype(MTYPE_F8);
        if (fmtype == MTYPE_F4)
        {
            Build_OP(TOP_ceil_l_s, temp_f, True_TN, src, ops);
        }
        else
        {
            Build_OP(TOP_ceil_l_d, temp_f, True_TN, src, ops);
        }
        tmp = Build_TN_Of_Mtype(MTYPE_I8);
        Build_OP(TOP_dmfc1, tmp, True_TN, temp_f, ops);
        Build_OP(TOP_sll, dest, True_TN, tmp, Gen_Literal_TN(0, 2), ops);

        break;

    case MTYPE_U8:
    {
        TN *bound, *temp, *temp_dest, *temp_src;
        TN *targ_gt_bound, *targ_exit;

        LABEL_IDX lab_idx_gt_bound, lab_idx_exit;
        LABEL* lb_gt_bound = &New_LABEL(CURRENT_SYMTAB, lab_idx_gt_bound);
        LABEL* lb_exit =  &New_LABEL(CURRENT_SYMTAB, lab_idx_exit);

        targ_gt_bound = Gen_Label_TN(lab_idx_gt_bound, 0);
        targ_exit = Gen_Label_TN(lab_idx_exit, 0);

        // Because the result is U8 need to use double TN
        temp_f = Build_TN_Of_Mtype(MTYPE_F8);
        // To contain the 0x8000000000000000
        bound = Build_TN_Of_Mtype(fmtype);
        temp = Build_TN_Of_Mtype(MTYPE_U8);
        temp_dest = Build_TN_Of_Mtype(MTYPE_U8);
        temp_src = Build_TN_Of_Mtype(fmtype);

        // Get the float bound 0x8000000000000000 = 9223372036854775808.0
        Expand_Const(bound, NULL, fmtype, ops);

        // Check whether the src > 0x8000000000000000
        VARIANT variant = (fmtype == MTYPE_F4) ?  V_BR_FLE : V_BR_DLE;
        Set_V_false_br(variant);
        Expand_Branch(targ_gt_bound, bound, src, variant, ops);

        // Handle the src >= 0x8000000000000000
        Begin_New_Basic_Block();

        // Sub the 9223372036854775808.0 to fit the range of ceil_l_*
        Expand_Flop((fmtype == MTYPE_F4) ? OPC_F4SUB : OPC_F8SUB,
                    temp_src, src, bound, NULL, ops);
        if (fmtype == MTYPE_F4)
        {
            Build_OP(TOP_ceil_l_s, temp_f, True_TN, src, ops);
        }
        else
        {
            Build_OP(TOP_ceil_l_d, temp_f, True_TN, src, ops);
        }
        Build_OP(TOP_dmfc1, temp_dest, True_TN, temp_f, ops);
        // Regenerate the 63th bit
        Expand_Binary_Or(temp_dest, temp_dest, Gen_Literal_TN(TWO_POWER_63_HEX_VALUE, 8), MTYPE_U8, ops);
        Build_OP(TOP_j, True_TN, targ_exit, ops);

        // Handle the src < 0x8000000000000000
        Begin_New_Basic_Block();
        Start_New_Label(lab_idx_gt_bound, lb_gt_bound, "ceil_GT_0x8000000000000000");
        if (fmtype == MTYPE_F4)
        {
            Build_OP(TOP_ceil_l_s, temp_f, True_TN, src, ops);
        }
        else
        {
            Build_OP(TOP_ceil_l_d, temp_f, True_TN, src, ops);
        }
        Build_OP(TOP_dmfc1, temp_dest, True_TN, temp_f, ops);


        // Exit the convert
        Begin_New_Basic_Block();
        Start_New_Label(lab_idx_exit, lb_exit, "ceil_exit");
        Expand_Copy(dest, temp_dest, MTYPE_U8, ops);
        break;
    }
    default:
        Is_True(FALSE, ("Expand_Float_To_Int_ceil can not handle this case!\n"));
    }

}

void
Expand_Float_To_Float(TN *dest, TN *src, TYPE_ID mtype, TYPE_ID desc, OPS *ops)
{
    TOP top;
    if (mtype == MTYPE_F4 && desc == MTYPE_F8)
    {
        top = TOP_cvt_s_d;
    }
    else if ((mtype == MTYPE_F8 || mtype == MTYPE_FQ) && desc == MTYPE_F4)
    {
        top = TOP_cvt_d_s;
    }
    else
    {
        FmtAssert(FALSE, ("NYI:  Expand_Float_To_Float mtype"));
    }

    Build_OP(top, dest, True_TN, src, ops);
}

// --------------------------------------------------------------------------
// codes followed is a patch of U2F.
// when an unsigned longlong converts to float, we should check whether it is greater than 0.
// If False, we should add 1.84467d+19(2**64; 0x5f800000) to converted result above.
//	for example:
//		0xffffffffffffffff will be convert to -1.0;
//		so we must add 18446744073709551616.0 to -1.0 to get 18446744073709551615.0
//     which is the valid result of convert.
// Note: the 18446744073709551616.0 is different between when it is a
// double (0x43f0000000000000) and float (0x5f800000).
// --------------------------------------------------------------------------
void
Expand_Unsigned_Int_To_Float(TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
    TN *targ, *tmp = Build_TN_Like(dest);
    LABEL_IDX lab_idx;

    LABEL* label = &New_LABEL(CURRENT_SYMTAB, lab_idx);
    targ = Gen_Label_TN(lab_idx, 0);
    Build_OP(TOP_bgez, True_TN, src, targ, ops);
    Begin_New_Basic_Block();

    TN *int_val, *float_from_max_int, *float_from_max_int_pair, *temp;

    float_from_max_int = Gen_Register_TN(ISA_REGISTER_CLASS_float, 8);
    temp = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
    if (fmtype == MTYPE_F4)
    {
        int_val = Gen_Literal_TN(TWO_POWER_64_IEEE754_SINGLE_FMT, 4);

        Expand_Immediate(temp, int_val, FALSE, ops);
        Build_OP(TOP_mtc1, float_from_max_int, True_TN, temp, ops);
        Expand_Copy(tmp, dest, fmtype, ops);
        Build_OP(TOP_add_s, dest, True_TN, float_from_max_int, tmp, ops);
    }
    else
    {
        int_val = Gen_Literal_TN(TWO_POWER_64_IEEE754_DOUBLE_FMT, 8);

        Expand_Immediate(temp, int_val, FALSE, ops);
        Build_OP(TOP_dmtc1, float_from_max_int, True_TN, temp, ops);
        Expand_Copy(tmp, dest, fmtype, ops);

        Build_OP(TOP_add_d, dest, True_TN, float_from_max_int, tmp, ops);
    }
    Start_New_Label(lab_idx, label, "U2F");
}

void
Expand_Int_To_Float(TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
    TN *tmp, *tmp1, *tmp2, *tmp3;

    Is_True(dest && src, ("One of operands is NULL\n"));

    switch (imtype)
    {
    case MTYPE_I4:
        /* If dest is a dedicated tn, putting it at SRC positionn may cause
          Localize_Global_Return_Reg() check failed */

        tmp = Build_TN_Like(dest);
        // Move to cp1
        Build_OP(TOP_mtc1, tmp, True_TN, src, ops);

        // Convert
        if (fmtype == MTYPE_F4)
        {
            Build_OP(TOP_cvt_s_w, dest, True_TN, tmp, ops);
        }
        else
        {
            Build_OP(TOP_cvt_d_w, dest, True_TN, tmp, ops);
        }
        break;

    case MTYPE_U4:
        tmp = Build_TN_Of_Mtype(MTYPE_U8);

        // F4U4CVT => U8U4CVT; F4I8CVT
        // F8U4CVT => U8U4CVT; F8I8CVT

        Expand_Convert_Length(tmp, src, Gen_Literal_TN(0, 2), MTYPE_U4, FALSE, ops);
        Expand_Int_To_Float(dest, tmp, MTYPE_I8, fmtype, ops);

        break;

    case MTYPE_I8:
        tmp = Build_TN_Like(dest);

        Build_OP(TOP_dmtc1, tmp, True_TN, src, ops);

        if (fmtype == MTYPE_F4)
        {
            Build_OP(TOP_cvt_s_l, dest, True_TN, tmp, ops);
        }
        else
        {
            Build_OP(TOP_cvt_d_l, dest, True_TN, tmp, ops);
        }
        break;

    case MTYPE_U8:
        tmp = Build_TN_Like(dest);
        tmp1 = Build_TN_Like(dest);

        Build_OP(TOP_dmtc1, tmp, True_TN, src, ops);

        if (fmtype == MTYPE_F4)
        {
            Build_OP(TOP_cvt_s_l, tmp1, True_TN, tmp, ops);
        }
        else
        {
            Build_OP(TOP_cvt_d_l, tmp1, True_TN, tmp, ops);
        }

        Expand_Unsigned_Int_To_Float(tmp1, src, imtype, fmtype, ops);
        Expand_Copy(dest, tmp1, fmtype, ops);

        break;

    default:
        Is_True(FALSE, ("Expand_Int_To_Float can not handle this case!\n"));
    }

}

static void
Expand_Select_By_Result(
    TN *dest_tn,  TN *condition, TN *true_tn,
    TN *false_tn, TYPE_ID mtype, OPS *ops)
{
    /* condition ? dest = true_tn;
                : dest = false_tn;  */
    if (TN_is_float(true_tn))
    {
        // do float selection
#ifdef Is_True_On
        Is_True((TN_is_float(true_tn) && TN_is_float(false_tn)),
                ("Two optional destination of  SELECT node must be the same type!"));
#endif

        TN* tmp_tn = Build_TN_Of_Mtype(mtype);
        Expand_Copy(tmp_tn, false_tn, mtype, ops);

        TN *targ;
        LABEL_IDX lab_idx_ge;
        LABEL* lb_cont = &New_LABEL(CURRENT_SYMTAB, lab_idx_ge);
        targ = Gen_Label_TN(lab_idx_ge, 0);

        Build_OP(TOP_beq, True_TN, condition, Zero_TN, targ, ops);
        Begin_New_Basic_Block();

        Expand_Copy(tmp_tn, true_tn, mtype, ops);
        Start_New_Label(lab_idx_ge, lb_cont, "continue");
        Expand_Copy(dest_tn, tmp_tn, mtype, ops);

        if (Arg_or_Retval(dest_tn))
            Set_OP_volatile(OPS_last(ops));

    }
    else
    {
        TN* tmp_true = Build_TN_Of_Mtype(mtype);
        TN* tmp_false = Build_TN_Of_Mtype(mtype);
        TN* sign;
        TN* sign_not;
        sign = MTYPE_is_size_double(mtype) ? Build_TN_Of_Mtype(MTYPE_I8)
               : Build_TN_Of_Mtype(MTYPE_I4);
        sign_not = MTYPE_is_size_double(mtype) ? Build_TN_Of_Mtype(MTYPE_I8)
                   : Build_TN_Of_Mtype(MTYPE_I4);
        if (MTYPE_is_size_double(mtype))
        {
            Build_OP(TOP_dsub, sign, True_TN,  Zero_TN, condition, ops);
        }
        else
        {
            Build_OP(TOP_sub, sign, True_TN,  Zero_TN, condition, ops);
        }

        Build_OP(TOP_nor, sign_not, True_TN, sign, Zero_TN, ops);
        Expand_Binary_And(tmp_true, true_tn, sign, mtype, ops);
        Expand_Binary_And(tmp_false, false_tn, sign_not, mtype, ops);
        Expand_Add(dest_tn, tmp_true, tmp_false, mtype, ops);
    }
}

static void
Expand_Select_By_Result(
    TN *min_tn,  TN *max_tn, TN *condition, TN *true_tn,
    TN *false_tn, TYPE_ID mtype, OPS *ops)
{
    /* condition ? dest = true_tn;
                : dest = false_tn;  */

    TN* tmp_true = Build_TN_Of_Mtype(mtype);
    TN* tmp_false = Build_TN_Of_Mtype(mtype);
    TN* sign;
    TN* sign_not;
    sign = MTYPE_is_size_double(mtype) ? Build_TN_Of_Mtype(MTYPE_I8)
           : Build_TN_Of_Mtype(MTYPE_I4);
    sign_not = MTYPE_is_size_double(mtype) ? Build_TN_Of_Mtype(MTYPE_I8)
               : Build_TN_Of_Mtype(MTYPE_I4);
    /* both sign & sign_not are All 1  or  All 0;
        sign_not = !sign;
    dest = true_tn & sign + false_tn & sign_not;
    That is the essence of our implemantation which aviod use multiplying
    --George Her
    */
    if (MTYPE_is_size_double(mtype))
    {
        Build_OP(TOP_dsub, sign, True_TN,  Zero_TN, condition, ops);
    }
    else
    {
        Build_OP(TOP_sub, sign, True_TN,  Zero_TN, condition, ops);
    }

    Build_OP(TOP_nor, sign_not, True_TN, sign, Zero_TN, ops);

// the follwoing three lines to generate min value
    Expand_Binary_And(tmp_true, true_tn, sign, mtype, ops);
    Expand_Binary_And(tmp_false, false_tn, sign_not, mtype, ops);
    Expand_Add(min_tn, tmp_true, tmp_false, mtype, ops);

// the follwoing three lines to generate max value
    Expand_Binary_And(tmp_true, false_tn, sign, mtype, ops);
    Expand_Binary_And(tmp_false, true_tn, sign_not, mtype, ops);
    Expand_Add(max_tn, tmp_true, tmp_false, mtype, ops);

}

void
Expand_Select(
    TN *dest_tn,
    TN *cond_tn,
    TN *true_tn,
    TN *false_tn,
    TYPE_ID mtype,
    BOOL float_cond,
    OPS *ops)
{
    /* cond_tn ? dest = true_tn; : dest = false_tn; */
    const BOOL is_float = MTYPE_is_float(mtype);
    if (TN_register_class(cond_tn) == ISA_REGISTER_CLASS_predicate)
        Is_True(FALSE, ("Predicate REG appeared!"));
    else
    {
        if (float_cond)
        {
            Is_True(FALSE, ("Set_V_select_uses_fcc not used,\
                       Expand_Select() should not accept float_cond as true!"));
        }
        else
            /* Note: cond_tn are always INT */
            Expand_Select_By_Result(dest_tn, cond_tn, true_tn, false_tn, mtype, ops);
    }
}


static void
Exp_FP_Select(
    TN *dest_tn, TN *cmp_kid1, TN *cmp_kid2, VARIANT vs, OPS *ops)
{
    /* cmp_kid1 vs cmp_kid2 ? dest = cmp_kid1
      : dest = cmp_kid2*/
    TOP cmp;
    TN *tmp = Build_TN_Like(cmp_kid1);
    TYPE_ID mtype;
    BOOL condition = TRUE;

    switch (vs)
    {
    case V_BR_NONE:
#pragma mips_frequency_hint NEVER
        FmtAssert(FALSE, ("Exp_FP_Select given br_none variant"));
        /*NOTREACHED*/
    case V_BR_FLT:
    case V_BR_FLE:
    case V_BR_FGT:
    case V_BR_FGE:
    case V_BR_FNE:
    case V_BR_FEQ:
        mtype = MTYPE_F4;
        break;
    case V_BR_DLT:
    case V_BR_DLE:
    case V_BR_DGT:
    case V_BR_DGE:
    case V_BR_DNE:
    case V_BR_DEQ:
        mtype = MTYPE_F8;
        break;
    default:
        Is_True(FALSE, ("Exp_FP_Select: unexpected comparison"));
        break;
    }

    if (TN_has_value(cmp_kid2))
        Expand_Const(cmp_kid2, cmp_kid2, mtype, ops);

    switch (vs)
    {
    case V_BR_FLT:
        cmp = TOP_c_lt_s;
        break;
    case V_BR_DLT:
        cmp = TOP_c_lt_d;
        break;
    case V_BR_FLE:
        cmp = TOP_c_le_s;
        break;
    case V_BR_DLE:
        cmp = TOP_c_le_d;
        break;
    case V_BR_FEQ:
        cmp = TOP_c_eq_s;
        break;
    case V_BR_DEQ:
        cmp = TOP_c_eq_d;
        break;

    case V_BR_FGE:
        cmp = TOP_c_lt_s;
        break;
    case V_BR_DGE:
        cmp = TOP_c_lt_d;
        break;
    case V_BR_FGT:
        cmp = TOP_c_le_s;
        break;
    case V_BR_DGT:
        cmp = TOP_c_le_d;
        break;
    case V_BR_FNE:
        cmp = TOP_c_eq_s;
        break;
    case V_BR_DNE:
        cmp = TOP_c_eq_d;
        break;
    }

    switch (vs)
    {
    case V_BR_FGE:
    case V_BR_DGE:
    case V_BR_FGT:
    case V_BR_DGT:
    case V_BR_FNE:
    case V_BR_DNE:
        condition = FALSE;
        break;
    }

    LABEL_IDX lab_idx_ge;
    LABEL* lb_cont = &New_LABEL(CURRENT_SYMTAB, lab_idx_ge);
    TN* targ = Gen_Label_TN(lab_idx_ge, 0);
    TN* dest = Build_TN_Like(dest_tn);

    Exp_COPY(dest, cmp_kid1, ops);
    Build_OP(cmp, True_TN, cmp_kid1, cmp_kid2, ops);
    Build_OP(condition ? TOP_bc1t : TOP_bc1f, True_TN,  targ, ops);

    Begin_New_Basic_Block();
    Exp_COPY(dest, cmp_kid2, ops);

    Start_New_Label(lab_idx_ge, lb_cont, "continue");

    /* Consistent with Check_If_Dedicated_TN_Is_Global() */
    Exp_COPY(dest_tn, dest, ops);

    if (Trace_Exp)
    {
#pragma mips_frequency_hint NEVER
        OP *op;
        fprintf(TFile, "Exp_FP_Select:\n");
        FOR_ALL_OPS_OPs(ops, op)
        {
            fprintf(TFile, " into ");
            Print_OP(op);
        }
    }
}

/* This version will be only called by Expand_MinMax */
static void
Exp_FP_Select(
    TN *min_tn, TN *max_tn, TN *true_tn, TN *false_tn,
    VARIANT vs, OPS *ops)
{
    /* cmp_kid1 vs cmp_kid2 ? dest = true_tn; dest2 = false_tn;
                            : dest = false_tn; dest2 = true_tn; */
    TOP cmp;
    TN *tmp;
    TYPE_ID mtype;
    Is_True(!(true_tn != NULL && TN_has_value(true_tn)
              && false_tn != NULL && TN_has_value(false_tn)),
            ("Two operands are both FP const!\n"));

    switch (vs)
    {
    case V_BR_NONE:
#pragma mips_frequency_hint NEVER
        FmtAssert(FALSE, ("Exp_FP_Select given br_none variant"));
        /*NOTREACHED*/
    case V_BR_FLT:
    case V_BR_FLE:
    case V_BR_FGT:
    case V_BR_FGE:
    case V_BR_FNE:
    case V_BR_FEQ:
        mtype = MTYPE_F4;
        break;
    case V_BR_DLT:
    case V_BR_DLE:
    case V_BR_DGT:
    case V_BR_DGE:
    case V_BR_DNE:
    case V_BR_DEQ:
        mtype = MTYPE_F8;
        break;
    default:
        Is_True(FALSE, ("Exp_FP_Select : MinMax should not allow this kind of Comparison"));
        break;
    }

    if (TN_has_value(true_tn))
        Expand_Const(true_tn, true_tn, mtype, ops);
    else if (TN_has_value(false_tn))
        Expand_Const(false_tn, false_tn, mtype, ops);

    switch (vs)
    {
    case V_BR_FLT:
        cmp = TOP_c_lt_s;
        break;
    case V_BR_DLT:
        cmp = TOP_c_lt_d;
        break;
    case V_BR_FLE:
        cmp = TOP_c_le_s;
        break;
    case V_BR_DLE:
        cmp = TOP_c_le_d;
        break;
    case V_BR_FEQ:
        cmp = TOP_c_eq_s;
        break;
    case V_BR_DEQ:
        cmp = TOP_c_eq_d;
        break;

    case V_BR_FGE:
        cmp = TOP_c_lt_s;
        break;
    case V_BR_DGE:
        cmp = TOP_c_lt_d;
        break;
    case V_BR_FGT:
        cmp = TOP_c_le_s;
        break;
    case V_BR_DGT:
        cmp = TOP_c_le_d;
        break;
    case V_BR_FNE:
        cmp = TOP_c_eq_s;
        break;
    case V_BR_DNE:
        cmp = TOP_c_eq_d;
        break;
    }

    switch (vs)
    {
    case V_BR_FGE:
    case V_BR_DGE:
    case V_BR_FGT:
    case V_BR_DGT:
    case V_BR_FNE:
    case V_BR_DNE:
        Is_True(FALSE, ("Exp_FP_Select: unexpected comparison"));
        break;
    }


    /* Consistent with Check_If_Dedicated_TN_Is_Global() */
    TN *min_temp, *max_temp = NULL;
    min_temp = Build_TN_Like(min_tn);
    max_temp = Build_TN_Like(max_tn);

    LABEL_IDX lab_idx_ge;
    LABEL* lb_cont = &New_LABEL(CURRENT_SYMTAB, lab_idx_ge);
    TN *targ = Gen_Label_TN(lab_idx_ge, 0);

    Exp_COPY(min_temp, true_tn,  ops);
    Exp_COPY(max_temp, false_tn,  ops);

    Build_OP(cmp, True_TN, true_tn, false_tn, ops);

    Build_OP(TOP_bc1t, True_TN,  targ, ops);
    Begin_New_Basic_Block();

    Exp_COPY(min_temp, false_tn, ops);
    Exp_COPY(max_temp, true_tn,  ops);

    Start_New_Label(lab_idx_ge, lb_cont, "continue");

    Exp_COPY(min_tn, min_temp, ops);
    Exp_COPY(max_tn, max_temp, ops);

    if (Trace_Exp)
    {
#pragma mips_frequency_hint NEVER
        OP *op;
        fprintf(TFile, "Exp_FP_Select:\n");
        FOR_ALL_OPS_OPs(ops, op)
        {
            fprintf(TFile, " into ");
            Print_OP(op);
        }
    }
}

/* This version of Exp_FP_Select only for Exp_Select_And_Condition
--George Her
*/
static void
Exp_FP_Select(
    TN *dest_tn, TN *true_tn,
    TN *false_tn, TN *cmp_kid1, TN *cmp_kid2, VARIANT vs, OPS *ops)
{
    /* cmp_kid1 vs cmp_kid2 ? dest = true_tn
      : dest = false_tn */
    TOP cmp;
    TN *tmp = Build_TN_Like(cmp_kid1);
    TYPE_ID mtype;
    BOOL condition = TRUE;

    switch (vs)
    {
    case V_BR_NONE:
#pragma mips_frequency_hint NEVER
        FmtAssert(FALSE, ("Exp_FP_Select given br_none variant"));
        /*NOTREACHED*/
    case V_BR_FLT:
    case V_BR_FLE:
    case V_BR_FGT:
    case V_BR_FGE:
    case V_BR_FNE:
    case V_BR_FEQ:
        mtype = MTYPE_F4;
        break;
    case V_BR_DLT:
    case V_BR_DLE:
    case V_BR_DGT:
    case V_BR_DGE:
    case V_BR_DNE:
    case V_BR_DEQ:
        mtype = MTYPE_F8;
        break;
    default:
        Is_True(FALSE, ("Exp_FP_Select: unexpected comparison"));
        break;
    }

    if (TN_has_value(cmp_kid2))
        Expand_Const(cmp_kid2, cmp_kid2, mtype, ops);

    switch (vs)
    {
    case V_BR_FLT:
        cmp = TOP_c_lt_s;
        break;
    case V_BR_DLT:
        cmp = TOP_c_lt_d;
        break;
    case V_BR_FLE:
        cmp = TOP_c_le_s;
        break;
    case V_BR_DLE:
        cmp = TOP_c_le_d;
        break;
    case V_BR_FEQ:
        cmp = TOP_c_eq_s;
        break;
    case V_BR_DEQ:
        cmp = TOP_c_eq_d;
        break;

    case V_BR_FGE:
        cmp = TOP_c_lt_s;
        break;
    case V_BR_DGE:
        cmp = TOP_c_lt_d;
        break;
    case V_BR_FGT:
        cmp = TOP_c_le_s;
        break;
    case V_BR_DGT:
        cmp = TOP_c_le_d;
        break;
    case V_BR_FNE:
        cmp = TOP_c_eq_s;
        break;
    case V_BR_DNE:
        cmp = TOP_c_eq_d;
        break;
    }

    switch (vs)
    {
    case V_BR_FGE:
    case V_BR_DGE:
    case V_BR_FGT:
    case V_BR_DGT:
    case V_BR_FNE:
    case V_BR_DNE:
        condition = FALSE;
        break;
    }

    LABEL_IDX lab_idx_ge;
    LABEL* lb_cont = &New_LABEL(CURRENT_SYMTAB, lab_idx_ge);
    TN* targ = Gen_Label_TN(lab_idx_ge, 0);
    TN* dest = Build_TN_Like(dest_tn);

    Exp_COPY(dest, true_tn, ops);
    Build_OP(cmp, True_TN, cmp_kid1, cmp_kid2, ops);
    Build_OP(condition ? TOP_bc1t : TOP_bc1f, True_TN,  targ, ops);

    Begin_New_Basic_Block();
    Exp_COPY(dest, false_tn, ops);

    Start_New_Label(lab_idx_ge, lb_cont, "continue");

    /* Consistent with Check_If_Dedicated_TN_Is_Global() */
    Exp_COPY(dest_tn, dest, ops);

    if (Trace_Exp)
    {
#pragma mips_frequency_hint NEVER
        OP *op;
        fprintf(TFile, "Exp_FP_Select:\n");
        FOR_ALL_OPS_OPs(ops, op)
        {
            fprintf(TFile, " into ");
            Print_OP(op);
        }
    }
}

void
Expand_Min(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    // t = s1 < s2; d = select t ? s1 : s2
    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both const!\n"));

    /* Front end has already guaranteed  that src1 is not be constant.
    --George Her  11.13, 2003 */
    Check_Src1;

    VARIANT vs;
    TN *cmp_result = Gen_Register_TN(ISA_REGISTER_CLASS_integer, MTYPE_byte_size(mtype));
    switch (mtype)
    {
    case MTYPE_I4:
    case MTYPE_U4:
    case MTYPE_I8:
    case MTYPE_U8:
        Expand_Int_Less(cmp_result, src1, src2, mtype, ops);
        break;
    case MTYPE_F4:
        vs = V_BR_FLT;
        break;
    case MTYPE_F8:
        vs = V_BR_DLT;
        break;
    case MTYPE_F10:
        Is_True(FALSE, ("Expand_Min: F10 min not supported!"));
    default:
#pragma mips_frequency_hint NEVER
        Is_True(FALSE, ("Expand_Min: unexpected mtype"));
    }
    if (MTYPE_is_float(mtype))
        Exp_FP_Select(dest, src1, src2, vs, ops);
    else
        Expand_Select_By_Result(dest, cmp_result, src1, src2, mtype, ops);
}

void
Expand_Max(TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    // t = s1 < s2; d = select t ? s1 : s2
    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both const!\n"));

    /* Front end has already guaranteed  that src1 is not be constant.
    --George Her  11.13, 2003 */
    Check_Src1;

    if (TN_is_zero(src2) && MTYPE_is_unsigned(mtype))
        Exp_COPY(dest, src1,  ops);

    VARIANT vs;
    TN *cmp_result = Gen_Register_TN(ISA_REGISTER_CLASS_integer, MTYPE_byte_size(mtype));
    switch (mtype)
    {
    case MTYPE_I4:
    case MTYPE_U4:
    case MTYPE_I8:
    case MTYPE_U8:
        Expand_Int_Greater(cmp_result, src1, src2, mtype, ops);
        break;
    case MTYPE_F4:
        vs = V_BR_FGT;
        break;
    case MTYPE_F8:
        vs = V_BR_DGT;
        break;
    case MTYPE_F10:
        Is_True(FALSE, ("Expand_Max: F10 min not supported!"));
    default:
#pragma mips_frequency_hint NEVER
        Is_True(FALSE, ("Expand_Max: unexpected mtype"));
    }
    if (MTYPE_is_float(mtype))
        Exp_FP_Select(dest, src1, src2, vs, ops);
    else
        Expand_Select_By_Result(dest, cmp_result, src1, src2, mtype, ops);
}

void
Expand_MinMax(TN *dest, TN *dest2, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    // t = s1 < s2; d = select t ? s1 : s2; d2 = select t ? s2 : s1
    VARIANT vs;
    TN *cmp_result = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
    switch (mtype)
    {
    case MTYPE_I4:
        Expand_Int_Less(cmp_result, src1, src2, MTYPE_I4, ops);
        break;
    case MTYPE_U4:
        Expand_Int_Less(cmp_result, src1, src2, MTYPE_U4, ops);
        break;
    case MTYPE_F4:
        vs = V_BR_FLT;
        break;
    case MTYPE_F8:
        vs = V_BR_DLT;
        break;
    case MTYPE_F10:
        Is_True(FALSE, ("Expand_MinMax: F10 min max not supported!"));
    case MTYPE_I8:
        Expand_Int_Less(cmp_result, src1, src2, MTYPE_I8, ops);
        break;
    case MTYPE_U8:
        Expand_Int_Less(cmp_result, src1, src2, MTYPE_U8, ops);
        break;
    default:
#pragma mips_frequency_hint NEVER
        Is_True(FALSE, ("Expand_MinMax: unexpected mtype"));
    }

    if (MTYPE_is_float(mtype))
        Exp_FP_Select(dest, dest2, src1, src2, vs, ops);
    else
        Expand_Select_By_Result(dest, dest2, cmp_result, src1, src2, mtype, ops);
}

/* check whether to eval condition before select */
extern BOOL
Check_Select_Expansion(OPCODE compare)
{
    // in order to get optimal code,
    // don't evaluate the condition first,
    // but pass the condition and kids to exp_select,
    // which will do the compare and use the predicate results.
    return FALSE;
}

extern void
Exp_Select_And_Condition(
    OPCODE select, TN *result, TN *true_tn, TN *false_tn,
    OPCODE compare, TN *cmp_kid1, TN *cmp_kid2, VARIANT variant, OPS *ops)
{
    TN *cmp_result = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);

    switch (variant)
    {
    case V_BR_PEQ:
    case V_BR_PNE:
        Is_True(FALSE, ("unexpected compare op: V_BR_PEQ/V_BR_PNE"));
        break;
    case V_BR_NONE:
#pragma mips_frequency_hint NEVER
        FmtAssert(FALSE, ("Exp_Select_And_Condition given br_none variant"));
        /*NOT_REACHED*/
    case V_BR_I8GE:
        Expand_Int_Greater_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I8, ops);
        break;
    case V_BR_I8GT:
        Expand_Int_Greater(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I8, ops);
        break;
    case V_BR_I8LE:
        Expand_Int_Less_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I8, ops);
        break;
    case V_BR_I8LT:
        Expand_Int_Less(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I8, ops);
        break;
    case V_BR_U8GE:
        Expand_Int_Greater_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U8, ops);
        break;
    case V_BR_U8GT:
        Expand_Int_Greater(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U8, ops);
        break;
    case V_BR_U8LE:
        Expand_Int_Less_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U8, ops);
        break;
    case V_BR_U8LT:
        Expand_Int_Less(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U8, ops);
        break;
    case V_BR_I8EQ:
        Expand_Int_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I8, ops);
        break;
    case V_BR_U8EQ:
        Expand_Int_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U8, ops);
        break;
    case V_BR_I8NE:
        Expand_Int_Not_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I8, ops);
        break;
    case V_BR_U8NE:
        Expand_Int_Not_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U8, ops);
        break;
    case V_BR_I4GE:
        Expand_Int_Greater_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I4, ops);
        break;
    case V_BR_U4GE:
        Expand_Int_Greater_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U4, ops);
        break;
    case V_BR_I4GT:
        Expand_Int_Greater(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I4, ops);
        break;
    case V_BR_U4GT:
        Expand_Int_Greater(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U4, ops);
        break;
    case V_BR_I4LE:
        Expand_Int_Less_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I4, ops);
        break;
    case V_BR_U4LE:
        Expand_Int_Less_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U4, ops);
        break;
    case V_BR_I4EQ:
        Expand_Int_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I4, ops);
        break;
    case V_BR_U4EQ:
        Expand_Int_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U4, ops);
        break;
    case V_BR_I4NE:
        Expand_Int_Not_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I4, ops);
        break;
    case V_BR_U4NE:
        Expand_Int_Not_Equal(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U4, ops);
        break;
    case V_BR_I4LT:
        Expand_Int_Less(cmp_result, cmp_kid1, cmp_kid2, MTYPE_I4, ops);
        break;
    case V_BR_U4LT:
        Expand_Int_Less(cmp_result, cmp_kid1, cmp_kid2, MTYPE_U4, ops);
        break;
    case V_BR_FLT:
    case V_BR_FLE:
    case V_BR_FGT:
    case V_BR_FGE:
    case V_BR_FNE:
    case V_BR_FEQ:
    case V_BR_DLT:
    case V_BR_DLE:
    case V_BR_DGT:
    case V_BR_DGE:
    case V_BR_DNE:
    case V_BR_DEQ:
        Exp_FP_Select(result, true_tn, false_tn, cmp_kid1, cmp_kid2, variant, ops);
        return;
        break;
    default:
        Is_True(FALSE, ("Exp_Select_And_Condition: unexpected comparison"));
        break;
    }

    Expand_Select_By_Result(result, cmp_result, true_tn, false_tn,
                            OPCODE_rtype(select), ops);

    if (Trace_Exp)
    {
#pragma mips_frequency_hint NEVER
        OP *op;
        fprintf(TFile, "Exp_Select_And_Condition:\n");
        FOR_ALL_OPS_OPs(ops, op)
        {
            fprintf(TFile, " into ");
            Print_OP(op);
        }
    }
}

void
Expand_Sqrt(TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
    TOP new_opcode;
    switch (mtype)
    {
    case MTYPE_F4:
        new_opcode = TOP_sqrt_s;
        break;
    case MTYPE_F8:
        new_opcode = TOP_sqrt_d;
        break;
    default:
        Is_True(FALSE, ("Not support this kind of float sqrt!"));
    }

    Build_OP(new_opcode, result, True_TN, src, ops);
}


static void
Expand_Float_Compares(TOP cmp_opcode, TN *dest_tn, TN *src1, TN *src2, BOOL  need_change_brh, OPS *ops)
{
    /* Consistent with Check_If_Dedicated_TN_Is_Global() */
    TN *dest, *src_tn, *targ;
    TN *src1_pair, *src2_pair;
    LABEL* lb_cont;
    LABEL_IDX lab_idx_ge;

    if (Arg_or_Retval(dest_tn))
        dest = Build_TN_Like(dest_tn);
    else
        dest = dest_tn;

    src_tn = Gen_Literal_TN(1, 4);

    // U4 is used because the result can only be 1 or 0
    Expand_Copy(dest, src_tn, MTYPE_U4 , ops);

    lb_cont = &New_LABEL(CURRENT_SYMTAB, lab_idx_ge);
    targ = Gen_Label_TN(lab_idx_ge, 0);

    Build_OP(cmp_opcode, True_TN, src1, src2, ops);

    need_change_brh ? Build_OP(TOP_bc1f, True_TN,  targ, ops) :
    Build_OP(TOP_bc1t, True_TN,  targ, ops);
    Begin_New_Basic_Block();

    Expand_Copy(dest, Zero_TN, MTYPE_U4, ops);
    Start_New_Label(lab_idx_ge, lb_cont, "continue");

    /* Consistent with Check_If_Dedicated_TN_Is_Global() */
    if (Arg_or_Retval(dest_tn))
        Expand_Copy(dest_tn, dest, MTYPE_U4, ops);
}

void
Expand_Float_Less(TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
    TOP action;
    switch (mtype)
    {
    case MTYPE_F4:
        variant = V_BR_FLT;
        break;
    case MTYPE_F8:
        variant = V_BR_DLT;
        break;
    default:
        Is_True(FALSE, ("Expand_Float_Less: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }

    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both value tn!\n"));

    switch (variant)
    {
    case V_BR_FLT :
        action = TOP_c_lt_s;
        break;
    case V_BR_DLT :
        action = TOP_c_lt_d;
        break;
    default :
        Is_True(false, ("Can not handle this logical operation!\n"));
    }

    Expand_Float_Compares(action, dest, src1, src2, FALSE, ops);
}

void
Expand_Float_Greater(TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
    TOP action;
    switch (mtype)
    {
    case MTYPE_F4:
        variant = V_BR_FGT;
        break;
    case MTYPE_F8:
        variant = V_BR_DGT;
        break;
    default:
        Is_True(FALSE, ("Expand_Float_Less: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }
    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both value tn!\n"));
    switch (variant)
    {
    case V_BR_FGT :
        action = TOP_c_lt_s;
        break;
    case V_BR_DGT :
        action = TOP_c_lt_d;
        break;
    default :
        Is_True(false, ("Can not handle this logical operation!\n"));

    }

    Expand_Float_Compares(action, dest, src2, src1, FALSE, ops);
}

void
Expand_Float_Less_Equal(TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
    TOP action;
    switch (mtype)
    {
    case MTYPE_F4:
        variant = V_BR_FLE;
        break;
    case MTYPE_F8:
        variant = V_BR_DLE;
        break;
    default:
        Is_True(FALSE, ("Expand_Float_Less: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }
    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both value tn!\n"));
    switch (variant)
    {
    case V_BR_FLE :
        action = TOP_c_le_s;
        break;
    case V_BR_DLE :
        action = TOP_c_le_d;
        break;
    default :
        Is_True(false, ("Can not handle this logical operation!\n"));

    }

    Expand_Float_Compares(action, dest, src1, src2, FALSE, ops);
}

void
Expand_Float_Greater_Equal(TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
    TOP action;
    switch (mtype)
    {
    case MTYPE_F4:
        variant = V_BR_FGE;
        break;
    case MTYPE_F8:
        variant = V_BR_DGE;
        break;
    default:
        Is_True(FALSE, ("Expand_Float_Less: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }
    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both value tn!\n"));
    switch (variant)
    {
    case V_BR_FGE :
        action = TOP_c_le_s;
        break;
    case V_BR_DGE :
        action = TOP_c_le_d;
        break;
    default :
        Is_True(false, ("Can not handle this logical operation!\n"));

    }

    Expand_Float_Compares(action, dest, src2, src1, FALSE, ops);
}

void
Expand_Float_Equal(TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
    TOP action;
    switch (mtype)
    {
    case MTYPE_F4:
        variant = V_BR_FEQ;
        break;
    case MTYPE_F8:
        variant = V_BR_DEQ;
        break;
    default:
        Is_True(FALSE, ("Expand_Float_Less: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }
    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both value tn!\n"));
    switch (variant)
    {
    case V_BR_FEQ :
        action = TOP_c_eq_s;
        break;
    case V_BR_DEQ :
        action = TOP_c_eq_d;
        break;
    default :
        Is_True(false, ("Can not handle this logical operation!\n"));

    }

    Expand_Float_Compares(action, dest, src1, src2, FALSE, ops);
}

void
Expand_Float_Not_Equal(TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
    TOP action;
    switch (mtype)
    {
    case MTYPE_F4:
        variant = V_BR_FNE;
        break;
    case MTYPE_F8:
        variant = V_BR_DNE;
        break;
    default:
        Is_True(FALSE, ("Expand_Float_Less: MTYPE_%s is not handled", Mtype_Name(mtype)));
    }
    Is_True(!(src1 != NULL && TN_has_value(src1)
              && src2 != NULL && TN_has_value(src2)),
            ("Two operands are both value tn!\n"));
    switch (variant)
    {
    case V_BR_FNE :
        action = TOP_c_eq_s;
        break;
    case V_BR_DNE :
        action = TOP_c_eq_d;
        break;
    default :
        Is_True(false, ("Can not handle this logical operation!\n"));

    }

    Expand_Float_Compares(action, dest, src1, src2, TRUE, ops);
}

void
Expand_Recip_Sqrt(TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
    FmtAssert(FALSE, ("Unimplemented"));
}

void
Expand_Flop(OPCODE opcode, TN *result, TN *src1, TN *src2, TN *src3, OPS *ops)
{
    TOP opc, opc1, opc2, opc3;
    int insn_num = 1;
    TYPE_ID mtype;
    BOOL is_abs = FALSE;

    opc = TOP_UNDEFINED;
    switch (opcode)
    {
    case OPC_F4ABS:
        opc = TOP_abs_s;
        is_abs = TRUE;
        break;
    case OPC_F8ABS:
        opc = TOP_abs_d;
        is_abs = TRUE;
        break;

    case OPC_F4ADD:
        opc = TOP_add_s;
        break;
    case OPC_F8ADD:
        opc = TOP_add_d;
        break;

    case OPC_F4SUB:
        opc = TOP_sub_s;
        break;
    case OPC_F8SUB:
        opc = TOP_sub_d;
        break;

    case OPC_F4MPY:
        opc = TOP_mul_s;
        break;
    case OPC_F8MPY:
        opc = TOP_mul_d;
        break;

    case OPC_F4DIV:
        opc = TOP_div_s;
        break;
    case OPC_F8DIV:
        opc = TOP_div_d;
        break;

    case OPC_F4MADD:
        // (src2 * src3) + src1
        if (!CGEXP_float_use_madd)
        {
            opc1 = TOP_mul_s;
            opc2 = opc = TOP_add_s;
            insn_num = 2;
        }
        else
        {
            if (result != src2 && result != src3)
            {
                opc1 = TOP_mov_s;
                opc2 = opc = TOP_madd_s;
                insn_num = 2;
            }
            else
            {
                // not use madd insn when in need of 3 insns
                opc1 = TOP_mul_s;
                opc2 = opc = TOP_add_s;
                insn_num = 2;
            }
        }
        break;

    case OPC_F8MADD:
        if (!CGEXP_float_use_madd)
        {
            opc1 = TOP_mul_d;
            opc2 = TOP_add_d;
            insn_num = 2;
        }
        else
        {
            if (result != src2 && result != src3)
            {
                opc1 = TOP_mov_d;
                opc2 = opc = TOP_madd_d;
                insn_num = 2;
            }
            else
            {
                // not use madd insn when in need of 3 insns
                opc1 = TOP_mul_d;
                opc2 = TOP_add_d;
                insn_num = 2;
            }
        }
        insn_num = 2;
        break;

    case OPC_F4MSUB:
        // (src2 * src3) - src1
        if (!CGEXP_float_use_madd)
        {
            opc1 = TOP_mul_s;
            opc2 = TOP_sub_s;
            insn_num = 2;
        }
        else
        {
            if (result != src2 && result != src3)
            {
                opc1 = TOP_mov_s;
                opc2 = opc = TOP_msub_s;
                insn_num = 2;
            }
            else
            {
                // not use madd insn when in need of 3 insns
                opc1 = TOP_mul_s;
                opc2 = TOP_sub_s;
                insn_num = 2;
            }
        }
        break;

    case OPC_F8MSUB:
        if (!CGEXP_float_use_madd)
        {
            opc1 = TOP_mul_d;
            opc2 = TOP_sub_d;
            insn_num = 2;
        }
        else
        {
            if (result != src2 && result != src3)
            {
                opc1 = TOP_mov_d;
                opc2 = opc = TOP_msub_d;
                insn_num = 2;
            }
            else
            {
                // not use madd insn when in need of 3 insns
                opc1 = TOP_mul_d;
                opc2 = TOP_sub_d;
                insn_num = 2;
            }
        }
        break;

    case OPC_F4NMADD:
        // -((src2 * src3) + src1)
        if (!CGEXP_float_use_madd)
        {
            opc1 = TOP_mul_s;
            opc2 = TOP_add_s;
            insn_num = 3;
            mtype = MTYPE_F4;
        }
        else
        {
            if (result != src2 && result != src3)
            {
                opc1 = TOP_mov_s;
                opc2 = opc = TOP_nmadd_s;
                insn_num = 2;
            }
            else
            {
                // also use madd insn when in need of 3 insns
                opc1 = TOP_mov_s;
                opc2 = opc = TOP_nmadd_s;
                opc3 = TOP_mov_s;
                insn_num = 3;
            }
        }
        break;

    case OPC_F8NMADD:
        if (!CGEXP_float_use_madd)
        {
            opc1 = TOP_mul_d;
            opc2 = TOP_add_d;
            insn_num = 3;
            mtype = MTYPE_F8;
        }
        else
        {
            if (result != src2 && result != src3)
            {
                opc1 = TOP_mov_d;
                opc2 = opc = TOP_nmadd_d;
                insn_num = 2;
            }
            else
            {
                // also use madd insn when in need of 3 insns
                opc1 = TOP_mov_d;
                opc2 = opc = TOP_nmadd_d;
                opc3 = TOP_mov_d;
                insn_num = 3;
            }
        }
        break;

    case OPC_F4NMSUB:
        // -((src2 * src3) - src1)
        if (!CGEXP_float_use_madd)
        {
            opc1 = TOP_mul_s;
            opc2 = TOP_sub_s;
            insn_num = 3;
            mtype = MTYPE_F4;
        }
        else
        {
            if (result != src2 && result != src3)
            {
                opc1 = TOP_mov_s;
                opc2 = opc = TOP_nmsub_s;
                insn_num = 2;
            }
            else
            {
                // also use madd insn when in need of 3 insns
                opc1 = TOP_mov_s;
                opc2 = opc = TOP_nmsub_s;
                opc3 = TOP_mov_s;
                insn_num = 3;
            }
        }
        break;

    case OPC_F8NMSUB:
        if (!CGEXP_float_use_madd)
        {
            opc1 = TOP_mul_d;
            opc2 = TOP_sub_d;
            insn_num = 3;
            mtype = MTYPE_F8;
        }
        else
        {
            if (result != src2 && result != src3)
            {
                opc1 = TOP_mov_d;
                opc2 = opc = TOP_nmsub_d;
                insn_num = 2;
            }
            else
            {
                // also use madd insn when in need of 3 insns
                opc1 = TOP_mov_d;
                opc2 = opc = TOP_nmsub_d;
                opc3 = TOP_mov_d;
                insn_num = 3;
            }
        }
        break;

    case OPC_F4RECIP:
    {
        TN *tmp1 = Gen_Literal_TN(1, 4);
        TN* tmp2 = Build_TN_Like(result);
        tmp1 = Expand_Immediate_Into_Register(tmp1, ops);
        Expand_Int_To_Float(tmp2, tmp1, MTYPE_I4, MTYPE_F4, ops);
        Build_OP(TOP_div_s, result, True_TN, tmp2, src1, ops);
        return;
    }
    case OPC_F8RECIP:
    {
        TN *tmp1 = Gen_Literal_TN(1, 8);
        TN* tmp2 = Build_TN_Like(result);
        tmp1 = Expand_Immediate_Into_Register(tmp1, ops);
        Expand_Int_To_Float(tmp2, tmp1, MTYPE_I8, MTYPE_F8, ops);
        Build_OP(TOP_div_d, result, True_TN, tmp2, src1, ops);
        return;
    }

    case OPC_F4RSQRT:
    {
        TN *tmp1 = Gen_Literal_TN(1, 4);
        TN *tmp2 = Build_TN_Like(result);
        TN* tmp3 = Build_TN_Like(result);
        tmp1 = Expand_Immediate_Into_Register(tmp1, ops);
        Expand_Int_To_Float(tmp2, tmp1, MTYPE_I4, MTYPE_F4, ops);
        Expand_Sqrt(tmp3, src1, MTYPE_F4, ops);
        Build_OP(TOP_div_s, result, True_TN, tmp2, tmp3, ops);
        return;
    }
    case OPC_F8RSQRT:
    {
        TN *tmp1 = Gen_Literal_TN(1, 8);
        TN* tmp2 = Build_TN_Like(result);
        TN* tmp3 = Build_TN_Like(result);
        tmp1 = Expand_Immediate_Into_Register(tmp1, ops);
        Expand_Int_To_Float(tmp2, tmp1, MTYPE_I8, MTYPE_F8, ops);
        Expand_Sqrt(tmp3, src1, MTYPE_F8, ops);
        Build_OP(TOP_div_d, result, True_TN, tmp2, tmp3, ops);
        return;
    }

    case OPC_F10ABS:
    case OPC_F10ADD:
    case OPC_F10SUB:
    case OPC_F10MPY:
    case OPC_F10DIV:
    case OPC_F10MADD:
    case OPC_F10MSUB:
    case OPC_F10NMADD:
    case OPC_F10NMSUB:
    case OPC_F10RECIP:
    case OPC_F10RSQRT:
        Is_True(FALSE, ("Unexpected floating point type F10 in Expand_Flop!"));
        break;

    default:
#pragma mips_frequency_hint NEVER
        FmtAssert(FALSE, ("unexpected opcode %s", OPCODE_name(opcode)));
        /*NOTREACHED*/
    }

    switch (insn_num)
    {
    case 1:
        if (is_abs)
        {
            Build_OP(opc, result, True_TN, src1, ops);
        }
        else
        {
            Build_OP(opc, result, True_TN, src1, src2, ops);
        }
        break;
    case 2:
    {
        if ((opc != TOP_UNDEFINED) && TOP_is_madd(opc))
        {
            Build_OP(opc1, result, True_TN, src1, ops);
            Build_OP(opc2, result, True_TN, src2, src3, result, ops);
        }
        else
        {
            TN* tmp = Build_TN_Like(result);
            Build_OP(opc1, tmp, True_TN, src2, src3, ops);
            Build_OP(opc2, result, True_TN, tmp, src1, ops);
        }
        return;
    }
    case 3:
    {
        if ((opc != TOP_UNDEFINED) && TOP_is_madd(opc))
        {
            TN* tmp1 = Build_TN_Like(result);
            Build_OP(opc1, tmp1, True_TN, src1, ops);
            Build_OP(opc2, tmp1, True_TN, src2, src3, tmp1, ops);
            Build_OP(opc3, result, True_TN, tmp1, ops);
        }
        else
        {
            TN* tmp1 = Build_TN_Like(result);
            TN* tmp2 = Build_TN_Like(result);
            Build_OP(opc1, tmp1, True_TN, src2, src3, ops);
            Build_OP(opc2, tmp2, True_TN, tmp1, src1, ops);
            Expand_Neg(result, tmp2, mtype, ops);
        }
        return;
    }
    default:
        Is_True(FALSE, ("Unexpected #instru in Expand_Flop!"));
    }

}

/* Initialize the tracing flags for the expander. */
extern void
Init_CG_Expand(void)
{
    static BOOL Initialized = FALSE;

    // per PU:
    Trace_Exp = Get_Trace(TP_CGEXP, 1);
    /* whirl2ops uses -ttexp:2 */
    Trace_Exp2 = Get_Trace(TP_CGEXP, 4);
    Disable_Const_Mult_Opt = Get_Trace(TP_CGEXP, 32);
    /* calls.c use -ttexp:64 */

    if (Initialized) return;
    Initialized = TRUE;
    // once per file:
    Initialize_Branch_Variants();
}


/* ======================================================================
 * Exp_COPY
 *
 * Generate a register transfer copy from 'src_tn' to 'tgt_tn'.
 * ======================================================================*/
void
Exp_COPY(TN *tgt_tn, TN *src_tn, OPS *ops)
{
    if (TN_is_constant(src_tn))
    {
        FmtAssert(TN_has_value(src_tn), ("Exp_COPY: illegal source tn"));
        /* expansion for INTCONST doesn't depend on size */
        Exp_OP1(OPC_I8INTCONST, tgt_tn, src_tn, ops);
    }
    else
    {
        ISA_REGISTER_CLASS tgt_rc = TN_register_class(tgt_tn);
        ISA_REGISTER_CLASS src_rc = TN_register_class(src_tn);

        /* If a float rc is involved, figure out if it's a double.
         * For other rc's, it just doesn't matter.
         */
        BOOL is_double = FALSE;

        if ((tgt_rc == ISA_REGISTER_CLASS_float && TN_size(tgt_tn) == 8) ||
                (src_rc == ISA_REGISTER_CLASS_float && TN_size(src_tn) == 8))
        {
            is_double = TRUE;
        }

        if (tgt_rc != src_rc)
        {

            TOP opc = CGTARG_Inter_RegClass_Copy(tgt_rc, src_rc, is_double);

            FmtAssert(opc != TOP_UNDEFINED, ("NYI: Exp_COPY inter-class copy for %s to %s",
                                             ISA_REGISTER_CLASS_INFO_Name(ISA_REGISTER_CLASS_Info(src_rc)),
                                             ISA_REGISTER_CLASS_INFO_Name(ISA_REGISTER_CLASS_Info(tgt_rc))));

            Build_OP(opc, tgt_tn, True_TN, src_tn, ops);

        }
        else if (tgt_rc == ISA_REGISTER_CLASS_float)
        {
            if (is_double)
            {
                Build_OP(TOP_mov_d, tgt_tn, True_TN, src_tn, ops);
                // According to IA 64, set to copy
                Set_OP_copy(OPS_last(ops));
            }
            else
            {
                Build_OP(TOP_mov_s, tgt_tn, True_TN, src_tn, ops);
                Set_OP_copy(OPS_last(ops));
            }
        }
        else if (tgt_rc == ISA_REGISTER_CLASS_integer)
        {
            Build_OP(TOP_or, tgt_tn, True_TN, src_tn, Zero_TN, ops);
            Set_OP_copy(OPS_last(ops));
        }
        else if (tgt_rc == ISA_REGISTER_CLASS_hilo)
        {
            Build_OP(TOP_or, tgt_tn, True_TN, src_tn, Zero_TN, ops);
            Set_OP_copy(OPS_last(ops));
        }
        else
        {
#pragma mips_frequency_hint NEVER
            FmtAssert(FALSE, ("NYI: Exp_COPY intra-class copy for %s",
                              ISA_REGISTER_CLASS_INFO_Name(ISA_REGISTER_CLASS_Info(tgt_rc))));
            /*NOTREACHED*/
        }

        if (Trace_Exp2)
        {
#pragma mips_frequency_hint NEVER
            fprintf(TFile, "exp_copy into: ");
            Print_OP(OPS_last(ops));
        }
    }
}

void
Exp_Intrinsic_Op(INTRINSIC id, TN *result, TN *op0, TN * op1, OPS *ops)
{
    FmtAssert(FALSE, ("Unimplemented"));
}

// initial expansion of intrinsic call (may not be complete lowering).
// return result TN (if set).
// If the intrinsic requires a label and loop (2 bb's)
// then ops is for first bb and ops2 is for bb after the label.
// Otherwise only ops is filled in.
TN *
Exp_Intrinsic_Call(INTRINSIC id, TN *op0, TN *op1, TN *op2, OPS *ops,
                   LABEL_IDX *label, OPS *loop_ops)
{
}


/* ======================================================================
 * Exp_Simulated_Op
 *
 * Given a simulated <op>, expand it into the sequence of instructions
 * supported by the target.
 * ======================================================================*/
void Exp_Simulated_Op(const OP *op, OPS *ops, INT pc_value)
{
}


/* ======================================================================
 * Simulated_Op_Real_Ops
 *
 * Return the number of instructions that will be generated by Exp_Simulated_Op
 * ======================================================================*/
INT
Simulated_Op_Real_Ops(const OP *op)
{
}


/* ======================================================================
 * Simulated_Op_Real_Inst_Words
 *
 * Return the number of instruction words that will ultimately be emitted
 * for the expansion generated by Exp_Simulated_Op
 * ======================================================================*/
INT
Simulated_Op_Real_Inst_Words(const OP *op)
{
    switch (OP_code(op))
    {
    case TOP_spadjust:
        return 1;
    case TOP_asm:
        // this is a hack; will be a multiple of 3, but don't know
        // exact number.
        return 3;
    default:
        // we should never emit a simulated OP, so just assert.
#pragma mips_frequency_hint NEVER
        FmtAssert(FALSE, ("shouldn't be calling Simulated_Op_Real_Inst_Words for %s", TOP_Name(OP_code(op))));
        /*NOTREACHED*/
    }
}


/* ======================================================================
 * Exp_Is_Large_Stack_Sym
 *
 * determine if a given symbol is a stack relative reference that will
 * require multiple instructions to load or store.
 * ======================================================================*/
BOOL
Exp_Is_Large_Stack_Sym(ST* sym,  INT64 ofst)
{
    ST *base_sym;
    INT64 base_ofst;

    if (sym == NULL)
    {
        return FALSE;
    }

    Allocate_Object(sym);		/* make sure sym is allocated */

    Base_Symbol_And_Offset_For_Addressing(sym, ofst, &base_sym, &base_ofst);

    if (ST_on_stack(sym) && !ISA_LC_Value_In_Class(base_ofst, LC_i14))
    {
        return TRUE;
    }
    return FALSE;
}

void
Exp_Noop(OPS *ops)
{
    Build_OP(CGTARG_Noop_Top(), True_TN, Gen_Literal_TN(0, 4), ops);
}

void
Exp_Generic_Pred_Calc(TN* result1, TN *result2, COMPARE_TYPE ctype,
                      TN *qual_pred, OPS* ops)
{
    FmtAssert(FALSE, ("Unimplemented"));
}


void
Exp_Pred_Calc(TN* result, OP* cmp_op, COMPARE_TYPE ctype, BOOL false_result,
              OPS* ops)
{
    FmtAssert(FALSE, ("Unimplemented"));
}

///////////////////////////////////////////////////////////
//
// Setup the true_tn and false_tn for a BB. The true_tn is a TN such that
// it is true if the branch at the end of a BB is taken. If it false
// through the false_tn will be set true.
//
// This routine works by trying to find the compare which generates the
// branch predicate for the block. Assuming it finds one, and it's of the
// right form (i.e. an unc form), it attempts to simply re-use the two TN's
// it generates.
//
// Right now, if it doesn't find it, it asserts, but I don't think this is
// going to happen, given the current way we generate things.
//
// The above can happen if we are trying to generate the false predicate
// for a block that has a branch which came from a previous pass of
// hyperblock formation. In this case, we don't have a single defining
// compare. So if we have a predicate Pb, (which is the predicate used for
// the branch, we wan't Pf such that Pf is TRUE if Pb is false and the block
// is executed.  We can accomplish this by initializing Pf to 1 under the
// block predicate, and setting it to 0 if Pb is TRUE.
//
void
Exp_True_False_Preds_For_Block(BB *bb, TN* &true_tn, TN * &false_tn)
{
    COMPARE_TYPE comp_type;
    TN* tn1;
    TN* tn2;
    OP* compare_op;
    OP* br_op = BB_branch_op(bb);
    BOOL reusing_tns;
    VARIANT branch_variant;
    DEF_KIND kind;

    true_tn = NULL;
    false_tn = NULL;
    reusing_tns = FALSE;

    branch_variant = CGTARG_Analyze_Branch(br_op, &tn1, &tn2);
    Is_True(branch_variant == V_BR_P_TRUE, ("Can't get predicates for block %d", BB_id(bb)));

    /* Try to find the compare op */
    compare_op = TN_Reaching_Value_At_Op(tn1, br_op, &kind, TRUE);
    if (compare_op && kind == VAL_KNOWN && !OP_cond_def(compare_op))
    {
        //
        // This is the 99% case (maybe the 100% case, given the current
        // generation schemes). The result predicates are 100% defined, so we
        // can safely replace the opcode with the unconditional variant, and
        // then return the two result predicates in the appropriate slots.
        //
        reusing_tns = TRUE;
        OP_Change_Opcode(compare_op, CGTARG_Get_unc_Variant(OP_code(compare_op)));
        Set_OP_cond_def_kind(compare_op, OP_ALWAYS_UNC_DEF);

        true_tn = tn1;
        // Get the other result as the false_tn
        if (OP_result(compare_op, 1) != tn1)
        {
            false_tn = OP_result(compare_op, 1);
        }
        else
        {
            false_tn = OP_result(compare_op, 0);
        }
    }

    if (!reusing_tns)
    {
        OPS ops = OPS_EMPTY;
        DevWarn("inserting non-optimal inverse predicate in BB %d", BB_id(bb));
        true_tn = tn1;
        // must create tns before Exp_Pred* if want to return them.
        false_tn = Gen_Predicate_TN();
        Exp_Pred_Complement(false_tn, True_TN, true_tn, &ops);
        BB_Insert_Ops(bb, br_op, &ops, TRUE);
    }

    return;
}

void Expand_Const(TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
    TCON tcon;
    if (src == NULL)
        tcon = Host_To_Targ_Float(mtype, TWO_POWER_63_FLOAT_VALUE);
    float val;
    double val_d;
    TN *temp, *int_val, *dest_pair;
    Is_True(!src || TN_is_symbol(src), (" The src is not symbol tn!\n"));

    TCON tc = src ? ST_tcon_val(TN_var(src)) : tcon;

    // Must use Targ_Is_Zero to check for positive zero.
    if ((TCON_ty(tc) == MTYPE_F4 || TCON_ty(tc) == MTYPE_F8) && Targ_Is_Zero(tc))
    {
        Build_OP(TOP_dmtc1, dest, True_TN, Zero_TN, ops);
        return;
    }

    temp = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);

    if (CGEXP_float_consts_from_ints)
    {
        switch (mtype)
        {
        case MTYPE_F4:
            val = Targ_To_Host_Float(tc);
            int_val = Gen_Literal_TN(*(int *)(& val), 4);
            Expand_Immediate(temp, int_val, FALSE, ops);
            Build_OP(TOP_mtc1, dest, True_TN, temp, ops);
            break;

        case MTYPE_F8:
            val_d = Targ_To_Host_Float(tc);
            int_val = Gen_Literal_TN((*(unsigned long long *)(& val_d)), 8);

            // A negative double may eat up a lot of instructions here!!!
            // MIPs pro will use ld store, also time consuming, need tradeoff
            Expand_Immediate(temp, int_val, FALSE, ops);
            Build_OP(TOP_dmtc1, dest, True_TN, temp, ops);
            break;

        default:
            Is_True(FALSE, ("The constant is out of range!\n"));
        }
    }
    else
    {
        Exp_Load(mtype, mtype, dest, TN_var(src), 0, ops, V_NONE);
    }
}

BOOL
Target_Has_Immediate_Operand(WN *parent, WN *expr)
{
    if (WN_operator(parent) == OPR_INTRINSIC_CALL
            && (((INTRINSIC) WN_intrinsic(parent) == INTRN_FETCH_AND_ADD_I4)
                || ((INTRINSIC) WN_intrinsic(parent) == INTRN_FETCH_AND_ADD_I8)))
    {
        // can optimize for some constants
        return TRUE;
    }
    if (WN_operator(parent) == OPR_SUB)
    {
        // can handle immediates in either operand
        return TRUE;
    }
    // default to false, which really means "don't know"
    return FALSE;
}

void
Exp_Spadjust(TN *dest, TN *size, VARIANT variant, OPS *ops)
{
    Build_OP(TOP_spadjust, dest, True_TN, SP_TN, size, ops);
    switch (variant)
    {
    case V_SPADJUST_PLUS:
        Set_OP_spadjust_plus(OPS_last(ops));
        break;
    case V_SPADJUST_MINUS:
        Set_OP_spadjust_minus(OPS_last(ops));
        break;
    case V_NONE:
        break;
    default:
        Is_True(FALSE, ("bad variant (0x%llx) for Exp_Spadjust", (INT64)variant));
        break;
    }
}

