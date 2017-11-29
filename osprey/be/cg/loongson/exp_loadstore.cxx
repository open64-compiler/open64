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


/* CGEXP routines for loads and stores */
#include <elf.h>
#include <vector>
#include "defs.h"
#include "erglob.h"
#include "ercg.h"
#include "tracing.h"
#include "config.h"
#include "config_targ_opt.h"
#include "config_debug.h"
#include "xstats.h"
#include "topcode.h"
#include "tn.h"
#include "cg_flags.h"
#include "targ_isa_lits.h"
#include "op.h"
#include "stblock.h"
#include "data_layout.h"
#include "strtab.h"
#include "symtab.h"
#include "cg.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "expand.h"
#include "privatize_common.h"

void
Expand_Lda(TN *dest, TN *src, OPS *ops)
{
    FmtAssert(FALSE, ("NYI: Expand_Lda"));
}

BOOL
ST_Is_Common_Element(const ST* st)
{
    return (ST_class(st) == CLASS_VAR &&
            ST_base(st) != st &&
            ST_Is_Common_Block(ST_base(st)));
}

void
Check_Ldst_Type(TYPE_ID &mtype, TN* desc_tn)
{
    // Maybe there are some type except for integer and float
    // So, must denote the correct type

    INT8 is_integer = FALSE, is_float = FALSE;

    // Judge the type.
    if (mtype == MTYPE_I1
            || mtype == MTYPE_I2
            || mtype == MTYPE_I4
            || mtype == MTYPE_I8
            || mtype == MTYPE_U1
            || mtype == MTYPE_U2
            || mtype == MTYPE_U4
            || mtype == MTYPE_U8)
    {
        is_integer = TRUE;
    }
    else if (mtype == MTYPE_F4
             || mtype == MTYPE_F8)
    {
        is_float = TRUE;
    }

    mISA_REGISTER_CLASS rclass = TN_register_class(desc_tn);
    if ((rclass == ISA_REGISTER_CLASS_float && is_integer)
            || (rclass == ISA_REGISTER_CLASS_integer && is_float))
    {
        switch (mtype)
        {
        case MTYPE_U4:
        case MTYPE_I4:
            mtype = MTYPE_F4;
            break;
        case MTYPE_U8:
        case MTYPE_I8:
            mtype = MTYPE_F8;
            break;
        case MTYPE_F4:
            mtype = MTYPE_U4;
            break;
        case MTYPE_F8:
            mtype = MTYPE_U8;
            break;
        default:
            Is_True(FALSE, ("Can not convert mtype %d\n", mtype));
        }

    }

}


static TOP
Pick_Load_Instruction(TYPE_ID rtype, TYPE_ID desc)
{
    switch (desc)
    {
    case MTYPE_I1:
        return TOP_lb;
    case MTYPE_U1:
        return TOP_lbu;
    case MTYPE_I2:
        return TOP_lh;
    case MTYPE_U2:
        return TOP_lhu;

    case MTYPE_I4:
        return TOP_lw;
    case MTYPE_U4:
    {
        if (rtype == MTYPE_U8 || rtype == MTYPE_I8)
            return TOP_lwu;
        else
            return TOP_lw;
    }


    case MTYPE_I8:
    case MTYPE_U8:
        return TOP_ld;

    case MTYPE_F4:
        return TOP_lwc1;
    case MTYPE_F8:
        return TOP_ldc1;

    case MTYPE_V:
        if (rtype != MTYPE_V)
        {
            // use rtype to pick load (e.g. if lda)
            return Pick_Load_Instruction(rtype, rtype);
        }

        /*FALLTHROUGH*/

    default:
        FmtAssert(FALSE, ("NYI: Pick_Load_Instruction mtype, Maybe float instruction\n"));
        /*NOTREACHED*/
    }

}

void
Expand_Load(OPCODE opcode, TN *result, TN *base, TN *ofst,
            VARIANT variant, OPS *ops)
{
    TN* result_pair;
    TYPE_ID mtype = OPCODE_desc(opcode);
    TYPE_ID rtype = OPCODE_rtype(opcode);

    ST *ofst_sym, *base_sym;
    INT64 ofst_from_base, ofst_from_sym, total_ofst;

    // The total offset from base
    if (TN_is_symbol(ofst))
    {
        ofst_sym = TN_var(ofst);
        Base_Symbol_And_Offset(ofst_sym, &base_sym, &ofst_from_base);
        ofst_from_sym = TN_offset(ofst);
        total_ofst = ofst_from_sym + ofst_from_base;
    }
    else if (TN_has_value(ofst))
    {
        total_ofst = TN_value(ofst);
    }
    else
    {
        Is_True(FALSE, ("ofst_tn can only be sym of constant tn!\n"));
    }

    Check_Ldst_Type(mtype, result);

    TOP top = Pick_Load_Instruction(OPCODE_rtype(opcode), mtype);

    Is_True(TN_is_constant(ofst), ("Illegal load offset TN"));

    // when handle a large offset
    // expand load o(b) into add t=o,b; load t
    if (!ISA_LC_Value_In_Class(total_ofst, LC_i16))
    {
        TN *tmp = Build_TN_Like(base);
        Expand_Add(tmp, ofst, base, Pointer_Mtype, ops);

        // Modified the base and ofst.
        base = tmp;
        ofst = Gen_Literal_TN(0, 4);
    }

    // Eliminate the add ofset and base because the godson can contain the
    // offset tn.

    Build_OP(top, result, True_TN, ofst, base, ops);

}


static TOP
Pick_Store_Instruction(TYPE_ID mtype)
{
    switch (mtype)
    {
    case MTYPE_I1:
    case MTYPE_U1:
        return TOP_sb;

    case MTYPE_I2:
    case MTYPE_U2:
        return TOP_sh;

    case MTYPE_I4:
    case MTYPE_U4:
        return TOP_sw;

    case MTYPE_I8:
    case MTYPE_U8:
        return TOP_sd;

    case MTYPE_F4:
        return TOP_swc1;
    case MTYPE_F8:
        return TOP_sdc1;

    default:
        FmtAssert(FALSE, ("NYI: Pick_Store_Instruction mtype. Maybe float instruction\n"));
        /*NOTREACHED*/
    }
}

void
Expand_Store(TYPE_ID mtype, TN *src, TN *base, TN *ofst,
             VARIANT variant, OPS *ops)
{

    ST *ofst_sym, *base_sym;
    INT64 ofst_from_base, ofst_from_sym, total_ofst;

    Check_Ldst_Type(mtype, src);
    TOP top = Pick_Store_Instruction(mtype);

    Is_True(TN_is_constant(ofst), ("Illegal load offset TN"));

    // The total offset from base
    if (TN_is_symbol(ofst))
    {
        ofst_sym = TN_var(ofst);
        Base_Symbol_And_Offset(ofst_sym, &base_sym, &ofst_from_base);
        ofst_from_sym = TN_offset(ofst);
        total_ofst = ofst_from_sym + ofst_from_base;
    }
    else if (TN_has_value(ofst))
    {
        total_ofst = TN_value(ofst);
    }
    else
    {
        Is_True(FALSE, ("ofst_tn can only be sym of constant tn!\n"));
    }

    // When handle a large offset,
    // expand store o(b) into add t=o,b; store t
    if (!ISA_LC_Value_In_Class(total_ofst, LC_i16))
    {
        TN *tmp = Build_TN_Like(base);
        Expand_Add(tmp, ofst, base, Pointer_Mtype, ops);

        // Modified the base and ofst.
        base = tmp;
        ofst = Gen_Literal_TN(0, 2);
    }

    // Build the store instruction
    Build_OP(top, True_TN, src, ofst, base, ops);

}

static OPCODE
OPCODE_make_signed_op(OPERATOR op, TYPE_ID rtype, TYPE_ID desc, BOOL is_signed)
{
    if (MTYPE_is_signed(rtype) != is_signed)
        rtype = MTYPE_complement(rtype);
    if (MTYPE_is_signed(desc) != is_signed)
        desc =	MTYPE_complement(desc);

    return OPCODE_make_op(op, rtype, desc);
}

/* ====================================================================
 *
 * Adjust_Addr_TNs
 *
 * We have a memory reference operation, with a base and displacement,
 * where the displacement is literal.  We want to create another memop
 * with the displacement modified by a small amount.
 *
 * WARNING:  If an add operation is required, it will be expanded here.
 *
 * ====================================================================
 */

static void
Adjust_Addr_TNs(
    TOP	opcode,			/* The new memory operation */
    TN	**base_tn,		/* The base address -- may be modified */
    TN	**disp_tn,		/* The displacement -- may be modified */
    INT16 	disp,		/* A displacement to add */
    OPS *ops)
{

    if (Potential_Immediate_TN_Expr(opcode, *disp_tn, disp))
    {
        if (TN_has_value(*disp_tn))
        {
            *disp_tn = Gen_Literal_TN(TN_value(*disp_tn) + disp, 4);
        }
        else
        {
            *disp_tn = Gen_Symbol_TN(TN_var(*disp_tn),
                                     TN_offset(*disp_tn) + disp, 0);
        }
    }
    else
    {
        TN *tmp = Build_TN_Of_Mtype(Pointer_Mtype);
        // because disp may be symbolic reloc on base,
        // want to still add it with base and create new base and disp.
        Expand_Add(tmp, *disp_tn, *base_tn, Pointer_Mtype, ops);
        *base_tn = tmp;
        *disp_tn = Gen_Literal_TN(disp, 4);
    }
}

// Judging whether the given symble is a externel symble, which
// will be used by addressing the GOT item.
INT8 ST_Is_Externel(ST* sym)
{
    if (!ST_is_export_local(sym))
        return TRUE;
    else
        return FALSE;
}

static void
Expand_Composed_Load(OPCODE op, TN *result, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
    TYPE_ID rtype = OPCODE_rtype(op);
    TYPE_ID desc = OPCODE_desc(op);

    DevWarn("Misaligned load!\n");

    if (MTYPE_is_float(rtype))
    {
        TN     *load;

        switch (rtype)
        {
        case MTYPE_F4:
            load = Build_TN_Of_Mtype(MTYPE_I4);
            Expand_Composed_Load(OPC_I4I4LDID, load, base, disp, variant, ops);
            // setf_s moves bits into fp reg.
            Build_OP(TOP_mtc1, result, True_TN, load, ops);
            break;
        case MTYPE_F8:
            load = Build_TN_Of_Mtype(MTYPE_I8);
            Expand_Composed_Load(OPC_I8I8LDID, load, base, disp, variant, ops);
            Build_OP(TOP_dmtc1, result, True_TN, load, ops);
            break;
        default:
            FmtAssert(FALSE, ("Expand_Composed_Load doesn't handle %s",
                              MTYPE_name(rtype)));
            /*NOTREACHED*/
        }
        Reset_TN_is_fpu_int(result);
        return;
    }

    TN* tmp = Dup_TN(disp);
    switch (desc)
    {
    case MTYPE_I8:
    case MTYPE_U8:
    {
        TN* tmp_result = Build_TN_Like(result);
        Build_OP(TOP_ldr, tmp_result, True_TN, disp, base, ops);
        TN_offset(tmp) += MISALIGNED_DOUBLE_ADJUST;
        Build_OP(TOP_ldl, tmp_result, True_TN, tmp_result, tmp, base, ops);
        Expand_Copy(result, tmp_result, desc, ops);
        return;
    }
    case MTYPE_I4:
    case MTYPE_U4:
    {
        TN* tmp_result = Build_TN_Like(result);
        Build_OP(TOP_lwr, tmp_result, True_TN, disp, base, ops);
        TN_offset(tmp) += MISALIGNED_WORD_ADJUST;
        Build_OP(TOP_lwl, tmp_result, True_TN, tmp_result, tmp, base, ops);
        Expand_Copy(result, tmp_result, desc, ops);
        return;
    }
    case MTYPE_I2:
    case MTYPE_U2:
    {
        TN *tmp1 = Build_TN_Of_Mtype(MTYPE_U4);
        Build_OP(TOP_lwr, tmp1, True_TN, disp, base, ops);
        TN_offset(tmp) += MISALIGNED_WORD_ADJUST;
        Build_OP(TOP_lwl, tmp1, True_TN, tmp1, tmp, base, ops);
        Expand_Convert_Length(result, tmp1, Gen_Literal_TN(0, 2),
                              MTYPE_U4, MTYPE_is_unsigned(rtype) ? FALSE : TRUE, ops);

        return;
    }
    default:
        FmtAssert(FALSE, ("Expand_Composed_Load doesn't handle %s",
                          MTYPE_name(rtype)));
    }

}

void
Expand_Misaligned_Load(OPCODE op, TN *result, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
    Expand_Composed_Load(op, result, base, disp, variant, ops);
}


static void
Expand_Composed_Store(TYPE_ID mtype, TN *obj, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
    DevWarn("Misaligned store!\n");
    if (MTYPE_is_float(mtype))
    {
        TN     *tmp;

        switch (mtype)
        {
        case MTYPE_F4:
            tmp = Build_TN_Of_Mtype(MTYPE_I4);
            Build_OP(TOP_mfc1, tmp, True_TN, obj, ops);
            Expand_Composed_Store(MTYPE_I4, tmp, base, disp, variant, ops);
            break;
        case MTYPE_F8:
            tmp = Build_TN_Of_Mtype(MTYPE_I8);
            Build_OP(TOP_dmfc1, tmp, True_TN, obj, ops);
            Expand_Composed_Store(MTYPE_I8, tmp, base, disp, variant, ops);
            break;
        default:
            FmtAssert(FALSE, ("Expand_Composed_Store doesn't handle %s",
                              MTYPE_name(mtype)));
            /*NOTREACHED*/
        }
        return;
    }

    TN* temp = Dup_TN(disp);
    TN* tmp1 = Build_TN_Of_Mtype(MTYPE_U4);
    TN* tmp2 = Build_TN_Of_Mtype(MTYPE_U4);

    INT32 tmp_ofst = TN_offset(temp);

    switch (mtype)
    {
    case MTYPE_I8:
    case MTYPE_U8:
        if ((tmp_ofst >= 0) && ((tmp_ofst % 8) == 0))
        {
            // should be double-word-aligned store
            Build_OP(TOP_sd, True_TN, obj, disp, base, ops);
        }
        else
        {
            Build_OP(TOP_sdr, True_TN, obj, disp, base, ops);
            TN_offset(temp) += MISALIGNED_DOUBLE_ADJUST;
            Build_OP(TOP_sdl, True_TN, obj, temp, base, ops);
        }
        break;
    case MTYPE_I4:
    case MTYPE_U4:
        if ((tmp_ofst >= 0) && ((tmp_ofst % 4) == 0))
        {
            // should be double-word-aligned store
            Build_OP(TOP_sw, True_TN, obj, disp, base, ops);
        }
        else
        {
            Build_OP(TOP_swr, True_TN, obj, disp, base, ops);
            TN_offset(temp) += MISALIGNED_WORD_ADJUST;
            Build_OP(TOP_swl, True_TN, obj, temp, base, ops);
        }
        break;
    case MTYPE_I2:
    case MTYPE_U2:

        // The following method is according to MIPS pro
        // Store byte one by one
        Build_OP(TOP_sb, True_TN, obj, disp, base, ops);
        Expand_Shift(tmp1, obj, Gen_Literal_TN(8, 2), MTYPE_U4, shift_aright, ops);
        temp = Dup_TN(disp);
        // The next byte, Little endian
        TN_offset(temp) += 1;
        Build_OP(TOP_sb, True_TN, tmp1, temp, base, ops);
        break;

    default:
        FmtAssert(FALSE, ("Expand_Composed_Store doesn't handle %s",
                          MTYPE_name(mtype)));
    }

}

void
Expand_Misaligned_Store(TYPE_ID mtype, TN *obj_tn, TN *base_tn, TN *disp_tn, VARIANT variant, OPS *ops)
{
    Expand_Composed_Store(mtype, obj_tn, base_tn, disp_tn, variant, ops);
}

static void
Exp_Ldst(
    OPCODE opcode,
    TN *tn,
    ST *sym,
    INT64 ofst,
    BOOL indirect_call,
    BOOL is_store,
    BOOL is_load,
    OPS *ops,
    VARIANT variant)
{
    ST *base_sym;
    INT64 base_ofst;
    TN *base_tn;
    TN *ofst_tn;
    BOOL is_lda = (!is_load && !is_store);
    OPS newops;
    OP *op;
    OPS_Init(&newops);

    if (Trace_Exp2)
    {
        fprintf(TFile, "exp_ldst %s: ", OPCODE_name(opcode));
        if (tn) Print_TN(tn, FALSE);
        if (is_store) fprintf(TFile, " -> ");
        else fprintf(TFile, " <- ");
        fprintf(TFile, "%lld (%s)\n", ofst, ST_name(sym));
    }

    Allocate_Object(sym);         /* make sure sym is allocated */

    Base_Symbol_And_Offset_For_Addressing(sym, ofst, &base_sym, &base_ofst);

    // Handle call site
    if (ST_class(sym) == CLASS_FUNC)
    {
        base_tn = GP_TN;
        if (ST_Is_Externel(sym))
        {
            // Usual function, including printf
            // ld $25, %call16 (function_name)($gp)
            ofst_tn = Gen_Symbol_TN(sym, 0, TN_RELOC_CALL16);
        }
        else
        {
            // Static function
            // ld $25, %got_disp(function_name)($gp)
            ofst_tn = Gen_Symbol_TN(sym, 0, TN_RELOC_GOT_DISP);
        }

        Expand_Load(OPCODE_make_signed_op(OPR_LDID,
                                          Pointer_Mtype, Pointer_Mtype, FALSE),
                    tn, base_tn, ofst_tn, variant, &newops);

        // No further ldst operations required
        is_lda = FALSE;
    }
    else  if (ST_on_stack(sym))
    {
        if (base_sym != SP_Sym && base_sym != FP_Sym)
        {
            // This can happen when an auto-var of caller is used in the nested callee in F90.
            // The ST info shows that the var is on caller's stack, but callee cannot access
            // it directly through FP or SP.
            Is_True(ST_is_uplevelTemp(sym), ("ST should be an upper level temp"));
            ST* slink_sym = Find_Slink_Symbol(CURRENT_SYMTAB);
            TN* slink_sym_ofst_tn = Gen_Symbol_TN(slink_sym, 0, 0);
            base_tn = Build_TN_Like(SP_TN);
            Expand_Load(OPCODE_make_signed_op(OPR_LDID, Pointer_Mtype, Pointer_Mtype, FALSE),
                        base_tn, SP_TN, slink_sym_ofst_tn, variant, &newops);
            ofst_tn = Gen_Literal_TN(base_ofst, Pointer_Size);
        }
        else
        {
            base_tn = (base_sym == FP_Sym) ? FP_TN : SP_TN;
            if (sym == base_sym)
            {
                ofst_tn = Gen_Literal_TN(base_ofst, Pointer_Size);
            }
            else
            {
                ofst_tn = Gen_Symbol_TN(sym, ofst, 0);
            }
        }
    }
    else if ((ST_class(base_sym) == CLASS_BLOCK
              || ST_class(base_sym) == CLASS_VAR)
             && ST_gprel(base_sym))
    {

        // GP-relative reference
        PU_References_GP = TRUE;
        /* ipa_generated symbols:
         *   storage class is SCLASS_EXTERN,
         *   export scopes is EXPORT_LOCAL or EXPORT_LOCAL_INTERNAL.
         * symbol reference:  base + offset.
         */
        if (ST_sclass(sym) == SCLASS_EXTERN && ST_is_export_local(sym))   // only for ipa
        {
            Base_Symbol_And_Offset(sym, &base_sym, &base_ofst);
            sym = base_sym;
            ofst += base_ofst;
        }
        if (ISA_LC_Value_In_Class(base_ofst, LC_k16)
                || (!is_load && !is_store && ISA_LC_Value_In_Class(base_ofst, LC_i16)))
        {
            if (ofst == 0)
            {
                base_tn = GP_TN;
                ofst_tn = Gen_Symbol_TN(sym, 0, TN_RELOC_GPREL16);
            }
            else
            {
                base_tn = Build_TN_Of_Mtype(Pointer_Mtype);
                TN *ofst_tmp = Gen_Symbol_TN(sym, 0, TN_RELOC_GPREL16);
                Expand_Add(base_tn, GP_TN, ofst_tmp, MTYPE_U8, ops);
                ofst_tn = Gen_Literal_TN(ofst, 2);
            }
        }
        else
        {
            FmtAssert(FALSE, ("gp-relative offset doesn't fit in 16 bits"));
        }
    }
    else if (Guaranteed_Small_GOT)
    {
        TN *addr = Build_TN_Of_Mtype(Pointer_Mtype);
        TN *base_tmp = Build_TN_Of_Mtype(Pointer_Mtype);
        if (!(ST_is_export_local(sym) && (ST_sclass(sym) == SCLASS_EXTERN)))   // not ipa symbols
        {
            if (is_lda && (!ST_Is_Externel(sym) ||  !base_ofst) && !ofst
                    && ST_class(sym) == CLASS_CONST)
            {
                addr = tn;
                is_lda = FALSE;
            }
        }

        /* ipa_generated symbols:
         *   storage class is SCLASS_EXTERN,
         *   export scopes is EXPORT_LOCAL or EXPORT_LOCAL_INTERNAL.
         * symbol reference:  base + offset.
         */
        if (ST_sclass(sym) == SCLASS_EXTERN && ST_is_export_local(sym))   // only for ipa
        {
            Base_Symbol_And_Offset(sym, &base_sym, &base_ofst);
            sym = base_sym;
            ofst += base_ofst;
        }

        /* common/module symbols in fortran:
         * ST_Is_Common_Element is true.
         * symbol reference: base + offset.
         */
        if (ST_Is_Common_Element(sym))
        {
            ofst += ST_ofst(sym);
            sym = ST_base(sym);
        }
        TN *sym_ofst = Gen_Symbol_TN(sym, 0, TN_RELOC_GOT_DISP);
        // Considering fortran special attribute, e.g. integer dimension(:), save, allocate:: g
        // and integer dimension(:), allocatable, target::g
        ST* temp_base_sym;
        TN* base_for_special_f90 = Build_TN_Of_Mtype(Pointer_Mtype);
        INT64 temp_offset;
        BOOL special_f90 = false;

        if (ST_pt_to_unique_mem(sym) || ST_is_f90_target(sym))
        {
            Is_True(!(ST_pt_to_unique_mem(sym) && ST_is_f90_target(sym)),
                    ("ST can not be unique_mem and f90_target together!\n"));
            Base_Symbol_And_Offset(sym, &temp_base_sym, &temp_offset);
            if (ST_base(sym) != temp_base_sym)
            {
                Is_True(ST_sclass(sym) == SCLASS_DGLOBAL, ("unique_mem without uglobal!\n"));
                sym_ofst = Gen_Symbol_TN(ST_base(sym), 0, TN_RELOC_GOT_DISP);
                special_f90 = true;
            }

        }

        if (is_lda && (!ST_Is_Externel(sym) ||  !base_ofst) && !ofst
                && ST_class(sym) == CLASS_CONST)
        {
            addr = tn;
            is_lda = FALSE;
        }

        if (special_f90 && ST_ofst(sym))
        {
            Expand_Load(OPCODE_make_signed_op(OPR_LDID,
                                              Pointer_Mtype, Pointer_Mtype, FALSE),
                        base_for_special_f90, GP_TN, sym_ofst, variant, &newops) ;
            Expand_Add(addr, base_for_special_f90, Gen_Literal_TN(ST_ofst(sym), 2),
                       MTYPE_U8, &newops);
        }
        else
        {
            Expand_Load(OPCODE_make_signed_op(OPR_LDID,
                                              Pointer_Mtype, Pointer_Mtype, FALSE),
                        addr, GP_TN, sym_ofst, variant, &newops) ;
        }

        Set_OP_no_alias(OPS_last(&newops));

        base_tn = addr;

        if (ISA_LC_Value_In_Class(ofst, LC_i16))
        {
            ofst_tn = Gen_Literal_TN(ofst, 2);
        }
        else
        {
            Expand_Add(base_tmp, base_tn, Gen_Literal_TN(ofst, 2), MTYPE_U8, &newops);
            base_tn = base_tmp;
            ofst_tn = Gen_Literal_TN(0, 2);
        }
    }
    else
    {
        FmtAssert(FALSE, ("NYI: Exp_Ldst"));
    }

    if (is_store)
    {
        if (V_align_all(variant) == 0)
        {
            Expand_Store(OPCODE_desc(opcode), tn, base_tn, ofst_tn,
                         variant, &newops);
        }
        else
        {
            Expand_Misaligned_Store(OPCODE_desc(opcode), tn,
                                    base_tn, ofst_tn, variant, &newops);
        }
    }
    else if (is_load)
    {
        if (V_align_all(variant) == 0)
        {
            Expand_Load(opcode, tn, base_tn, ofst_tn, variant, &newops);
        }
        else
        {
            Expand_Misaligned_Load(opcode, tn, base_tn, ofst_tn, variant, &newops);
        }
    }
    else if (is_lda)
    {
        Expand_Add(tn, ofst_tn, base_tn, OPCODE_rtype(opcode), &newops);
    }

    FOR_ALL_OPS_OPs(&newops, op)
    {

        if (is_load && OP_load(op))
        {
            // If we expanded a load of a constant,
            // nothing else can alias with the loads
            // we have generated.
            if (ST_is_constant(sym))
                Set_OP_no_alias(op);
        }

        if (Trace_Exp2)
        {
            fprintf(TFile, "exp_ldst into ");
            Print_OP(op);
        }
    }
    /* Add the new OPs to the end of the list passed in */
    OPS_Append_Ops(ops, &newops);
}

void Exp_Lda(
    TYPE_ID mtype,
    TN *tgt_tn,
    ST *sym,
    INT64 ofst,
    OPERATOR call_opr,
    OPS *ops)
{
    OPCODE opcode = OPCODE_make_op(OPR_LDA, mtype, MTYPE_V);
    Exp_Ldst(opcode, tgt_tn, sym, ofst,
             (call_opr == OPR_ICALL),
             FALSE, FALSE, ops, V_NONE);
}

void
Exp_Load(
    TYPE_ID rtype,
    TYPE_ID desc,
    TN *tgt_tn,
    ST *sym,
    INT64 ofst,
    OPS *ops,
    VARIANT variant)
{
    OPCODE opcode = OPCODE_make_op(OPR_LDID, rtype, desc);
    Exp_Ldst(opcode, tgt_tn, sym, ofst, FALSE, FALSE, TRUE, ops, variant);
}

void
Exp_Store(
    TYPE_ID mtype,
    TN *src_tn,
    ST *sym,
    INT64 ofst,
    OPS *ops,
    VARIANT variant)
{
    OPCODE opcode = OPCODE_make_op(OPR_STID, MTYPE_V, mtype);
    Exp_Ldst(opcode, src_tn, sym, ofst, FALSE, TRUE, FALSE, ops, variant);
}

/* ======================================================================
 * Exp_Extract_Bits
 * ======================================================================*/
void
Exp_Extract_Bits(TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
                 TN *tgt_tn, TN *src_tn, OPS *ops)
{
    FmtAssert(bit_size != 0, ("size of bit field cannot be 0"));

    INT32 bit_size_value, bit_offset_value;
    INT32 src_value, val1;
    UINT word_size = 32;
    TN *tmp_result_tn;
    TN *tmp1_tn, *temp_tn;

    bit_size_value = TN_value(Gen_Literal_TN(bit_size, 4));
    bit_offset_value = TN_value(Gen_Literal_TN(bit_offset, 4));

    if ((bit_size_value + bit_offset_value) > 32)
    {
        word_size = 64;

        tmp_result_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 8);
        temp_tn = Build_TN_Like(tgt_tn);
        SHIFT_DIRECTION shift_type = MTYPE_signed(rtype) ? shift_aright : shift_lright;
        tmp1_tn = Gen_Literal_TN((word_size - bit_size_value - bit_offset_value), 8);
        Expand_Shift(temp_tn, src_tn, tmp1_tn, MTYPE_U8, shift_left, ops);
        tmp1_tn = Gen_Literal_TN((word_size - bit_size_value), 8);
        Expand_Shift(tgt_tn, temp_tn, tmp1_tn, MTYPE_U8, shift_type, ops);
    }
    else
    {
        tmp_result_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
        temp_tn = Build_TN_Like(tgt_tn);
        SHIFT_DIRECTION shift_type = MTYPE_signed(rtype) ? shift_aright : shift_lright;
        tmp1_tn = Gen_Literal_TN((word_size - bit_size_value - bit_offset_value), 4);
        Expand_Shift(temp_tn, src_tn, tmp1_tn, MTYPE_U4, shift_left, ops);
        tmp1_tn = Gen_Literal_TN((word_size - bit_size_value), 4);
        Expand_Shift(tgt_tn, temp_tn, tmp1_tn, MTYPE_U4, shift_type, ops);
    }
}

/* ======================================================================
 * Exp_Deposit_Bits - deposit src2_tn into a field of src1_tn returning
 * the result in tgt_tn.
 * ======================================================================*/
void
Exp_Deposit_Bits(TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
                 TN *tgt_tn, TN *src1_tn, TN *src2_tn, OPS *ops)
{
    FmtAssert(bit_size != 0, ("size of bit field cannot be 0"));
    UINT targ_bit_offset = bit_offset;

    UINT wordsize = 32;
    INT32 bit_size_value, bit_offset_value;
    bit_size_value = TN_value(Gen_Literal_TN(bit_size, 4));
    bit_offset_value = TN_value(Gen_Literal_TN(bit_offset, 4));

    TN *tmp_result_tn;
    TN *tmp1_tn;
    TN *tmp2_tn;
    TN *tmp3_tn;

    if (Target_Byte_Sex == BIG_ENDIAN)
    {
        targ_bit_offset = MTYPE_bit_size(desc) - bit_offset - bit_size;
    }

    if ((bit_size_value + bit_offset_value) > 32)
    {

        // use ld to get the memory block
        INT64 val1, val2, val3;
        wordsize = 64;

        if (TN_has_value(src2_tn) || TN_has_value(src1_tn))
        {
            Is_True(FALSE, ("The deposit value can not be constant!"));
        }
        else
        {
            tmp_result_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 8);

            if (bit_offset_value == 0)
            {
                val1 = MASK64 >> (wordsize - bit_size_value);
                Expand_Binary_And(tmp_result_tn, src2_tn, Gen_Literal_TN(val1, 8), MTYPE_I8, ops);
            }
            else
            {
                val1 = MASK64 >> (wordsize - bit_size_value);
                tmp3_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 8);
                Expand_Binary_And(tmp3_tn, src2_tn, Gen_Literal_TN(val1, 8), MTYPE_I8, ops);
                Expand_Shift(tmp_result_tn, tmp3_tn, Gen_Literal_TN(bit_offset_value, 8), MTYPE_I8, shift_left, ops);
            }

            val1 = MASK64 << bit_size_value;
            val1 = val1 << bit_offset_value;
            val2 = MASK64 >> (wordsize - bit_offset_value);
            if (bit_offset_value != 0)
            {
                val1 = val1 | val2;
            }

            tmp1_tn = Expand_Immediate_Into_Register(Gen_Literal_TN(val1, 8), ops);
            tmp2_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 8);
            Expand_Binary_And(tmp2_tn, src1_tn, tmp1_tn, MTYPE_I8, ops);

            Build_OP(TOP_or, tgt_tn, True_TN, tmp2_tn, tmp_result_tn, ops);
        }

        return ;
    }

    // bit_size_value+bit_size_value<32, use lw
    INT32 val1, val2, val3;
    if (TN_has_value(src2_tn) || TN_has_value(src1_tn))
    {
        Is_True(FALSE, ("The deposit value can not be constant!"));
    }
    else
    {
        tmp_result_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);

        if (bit_offset_value == 0)
        {
            val1 = MASK32 >> (wordsize - bit_size_value);
            Expand_Binary_And(tmp_result_tn, src2_tn, Gen_Literal_TN(val1, 4), MTYPE_I4, ops);
        }
        else
        {
            val1 = MASK32 >> (wordsize - bit_size_value);
            tmp3_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
            Expand_Binary_And(tmp3_tn, src2_tn, Gen_Literal_TN(val1, 4), MTYPE_I4, ops);
            Expand_Shift(tmp_result_tn, tmp3_tn, Gen_Literal_TN(bit_offset_value, 4), MTYPE_I4, shift_left, ops);
        }

        val1 = MASK32 << bit_size_value;
        val1 = val1 << bit_offset_value;
        val2 = MASK32 >> (wordsize - bit_offset_value);
        if (bit_offset_value != 0)
        {
            val1 = val1 | val2;
        }

        tmp1_tn = Expand_Immediate_Into_Register(Gen_Literal_TN(val1, 4), ops);
        tmp2_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
        Expand_Binary_And(tmp2_tn, src1_tn, tmp1_tn, MTYPE_I4, ops);
        Build_OP(TOP_or, tgt_tn, True_TN, tmp2_tn, tmp_result_tn, ops);
    }

}

void
Expand_Lda_Label(TN *dest, TN *lab, OPS *ops)
{
    TN *tmp1 = Build_TN_Of_Mtype(Pointer_Mtype);
    Set_TN_is_reloc_ia_ltoff22(lab);
    // first get address of LT entry
    Expand_Add(tmp1, lab, GP_TN, Pointer_Mtype, ops);
    // then get address of var
    Expand_Load(
        // load is of address, not of result type
        OPCODE_make_op(OPR_LDID, Pointer_Mtype, Pointer_Mtype),
        dest, tmp1, Gen_Literal_TN(0, 4), V_NONE, ops);
}

