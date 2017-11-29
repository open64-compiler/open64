/*
 * Copyright 2002, 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
static const char rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/access/ti_asm.c,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "topcode.h"
#include "targ_isa_operands.h"
#include "targ_isa_print.h"
#include "targ_isa_pack.h"
#include "targ_isa_bundle.h"
#include "targ_isa_decode.h"
#include "targ_isa_pseudo.h"
#include "targ_isa_enums.h"
#include "targ_isa_operands.h"
#include "targ_abi_properties.h"
#include "targ_proc_properties.h"

#include "ti_errors.h"
#include "ti_asm.h"
#include "bstring.h"

/* ====================================================================
 *
 *  TI_ASM_Pack_Inst
 *
 *  See interface description
 *
 * ====================================================================
 */
INT TI_ASM_Pack_Inst(
  TOP topcode,
  const INT64 *result,
  const INT64 *opnd,
  ISA_PACK_INST *pinst)
{
  INT comp;
  INT words;
  INT w;
  const ISA_PACK_INFO *pinfo;
  const ISA_PACK_ADJ_INFO *ainfo;
  INT64 bopnd[ISA_OPERAND_max_operands];
  INT64 bresult[ISA_OPERAND_max_results];

  memmove(bopnd, opnd, sizeof(bopnd));
  memmove(bresult, result, sizeof(bresult));

  topcode = ISA_PSEUDO_Translate(topcode, 
				 bresult, 
				 bopnd,
				 ISA_PSEUDO_to_machine);

  pinfo = ISA_PACK_Info(topcode);
  if (!pinfo) {
    #pragma mips_frequency_hint NEVER
    sprintf(TI_errmsg, "no ISA_PACK_INFO for %s", TOP_Name(topcode));
    return TI_RC_ERROR;
  }

  ainfo = ISA_PACK_Adj_Info(topcode);
  if (ainfo) ISA_PACK_Adjust_Operands(ainfo, bopnd, FALSE);

  words = ISA_PACK_Inst_Words(topcode);
  for (w = 0; w < words; ++w) {
    ISA_PACK_INST inst = ISA_PACK_Init_Mask(topcode, w);
    do {
      UINT64 mask = ISA_PACK_INFO_Mask(pinfo);
      UINT opndpos = ISA_PACK_INFO_OpndPos(pinfo);
      UINT instpos = ISA_PACK_INFO_InstPos(pinfo);
      comp = ISA_PACK_INFO_Comp(pinfo);

      switch (comp) {
      case ISA_PACK_COMP_opnd:
      case ISA_PACK_COMP_opnd+1:
      case ISA_PACK_COMP_opnd+2:
      case ISA_PACK_COMP_opnd+3:
      case ISA_PACK_COMP_opnd+4:
      case ISA_PACK_COMP_opnd+5:
	{
	  INT n = comp - ISA_PACK_COMP_opnd;
	  inst |= ((bopnd[n] >> opndpos) & mask) << instpos;
	}
	break;

      case ISA_PACK_COMP_result:
      case ISA_PACK_COMP_result+1:
	{
	  INT n = comp - ISA_PACK_COMP_result;
	  inst |= ((bresult[n] >> opndpos) & mask) << instpos;
	}
	break;

      case ISA_PACK_COMP_end:
	pinst[w] = inst;
	break;

      default:
	#pragma mips_frequency_hint NEVER
	sprintf(TI_errmsg, "Unhandled packing component %d for %s",
			   comp, TOP_Name(topcode));
	return TI_RC_ERROR;
      }
    } while (++pinfo, comp != ISA_PACK_COMP_end);
  }

  return words;
}

/* ====================================================================
 *
 *  TI_ASM_Print_Inst
 *
 *  See interface description
 *
 * ====================================================================
 */
INT TI_ASM_Print_Inst(
  TOP topcode,
  const char **result,
  const char **opnd,
  FILE *f)
{
  INT i;
  INT st;
  INT comp;
  const char *arg[ISA_PRINT_COMP_MAX];
  const ISA_PRINT_INFO *pinfo = ISA_PRINT_Info(topcode);

  if (!pinfo) {
    #pragma mips_frequency_hint NEVER
    sprintf(TI_errmsg, "no ISA_PRINT_INFO for %s", TOP_Name(topcode));
    return TI_RC_ERROR;
  }

  i = 0;
  do {
    comp = ISA_PRINT_INFO_Comp(pinfo, i);

    /* different targets may have different number of opnds,
     * so rather than do switch-case ISA_PRINT_COMP_opnd+n,
     * just do if-check that comp is in opnd or result range. */
    if (comp == ISA_PRINT_COMP_name) {
      arg[i] = ISA_PRINT_AsmName(topcode);
    }
    else if (comp >= ISA_PRINT_COMP_opnd && comp < ISA_PRINT_COMP_result) {
      arg[i] = opnd[comp - ISA_PRINT_COMP_opnd];
    }
    else if (comp >= ISA_PRINT_COMP_result && comp <= ISA_PRINT_COMP_MAX) {
      arg[i] = result[comp - ISA_PRINT_COMP_result];
    }
    else if (comp == ISA_PRINT_COMP_end) {
      ; /* ignore */
    }
    else {
      #pragma mips_frequency_hint NEVER
      sprintf(TI_errmsg, "Unhandled listing component %d for %s",
			 comp, TOP_Name(topcode));
      return TI_RC_ERROR;
      /*NOTREACHED*/
    }
  } while (++i, comp != ISA_PRINT_COMP_end);

  st = fprintf (f, ISA_PRINT_INFO_Format(pinfo),
		     arg[0], arg[1], arg[2], arg[3], 
		     arg[4], arg[5], arg[6], arg[7],
		     arg[8]
#ifdef TARG_SL
		     , arg[9]
#endif
		);
  if (st == -1) {
	sprintf(TI_errmsg, "fprintf failed:  not enough disk space");
	return TI_RC_ERROR;
  }
  else return st;
}

/* ====================================================================
 *
 *  Format_Operand
 *
 *  Format an operand for disassembly.
 *
 * ====================================================================
 */
static INT Format_Operand(
  char *buf, 
  INT64 pc,
  const ISA_OPERAND_VALTYP *vtype, 
  ISA_OPERAND_USE use,
  INT64 val,
  INT flags)
{
  if (ISA_OPERAND_VALTYP_Is_Register(vtype)) {
    const char *rname;
    const char *fmt = (use == OU_predicate) ? ISA_PRINT_PREDICATE : "%s";
    ISA_REGISTER_CLASS rc = ISA_OPERAND_VALTYP_Register_Class(vtype);
#ifdef TARG_IA64
    if (   !(flags & TI_ASM_DISASM_TRUE_PRED)
	&& (use == OU_predicate)
	&& ABI_PROPERTY_Is_true_predicate(rc, val))
    {
      rname = "";
      fmt = "%s";
    } else if (flags & TI_ASM_DISASM_ABI_REGS) {
#else
    if (flags & TI_ASM_DISASM_ABI_REGS) {
#endif
      rname = ABI_PROPERTY_Reg_Name(rc, val);
    } else {
      const ISA_REGISTER_CLASS_INFO *rcinfo = ISA_REGISTER_CLASS_Info(rc);
      rname = ISA_REGISTER_CLASS_INFO_Reg_Name(rcinfo, val);
    }
    return sprintf(buf, fmt, rname) + 1;
  } else if (ISA_OPERAND_VALTYP_Is_Enum(vtype)) {
    ISA_ENUM_CLASS_VALUE ecv = (ISA_ENUM_CLASS_VALUE)val;
    return sprintf(buf, "%s", ISA_ECV_Name(ecv)) + 1;
  }

  assert(ISA_OPERAND_VALTYP_Is_Literal(vtype));

  if (ISA_OPERAND_VALTYP_Is_PCRel(vtype)) {
    val += pc;
#ifndef TARG_LOONGSON
    if (PROC_has_branch_delay_slot()) val += sizeof(ISA_BUNDLE);
#endif
    return sprintf(buf, "0x%llx", val) + 1;
  } else if (ISA_OPERAND_VALTYP_Is_Signed(vtype)) {
    return sprintf(buf, "%lld", val) + 1;
  } else {
    return sprintf(buf, "%llu", val) + 1;
  }
}


/* ====================================================================
 *
 *  TI_ASM_DisAsm_Inst
 *
 *  See interface description
 *
 * ====================================================================
 */
INT TI_ASM_DisAsm_Inst(
  TOP topcode,
  INT64 *result,
  INT64 *opnd,
  INT64 pc,
  INT flags,
  char *bufptr)
{
  INT comp;
  const char *arg[ISA_PRINT_COMP_MAX];
  char buf[80];
  int i;
  INT cursor;
  const ISA_PRINT_INFO *pinfo = ISA_PRINT_Info(topcode);
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(topcode);

  cursor = 0;
  i = 0;
  do {
    comp = ISA_PRINT_INFO_Comp(pinfo, i);

    /* different targets may have different number of opnds,
     * so rather than do switch-case ISA_PRINT_COMP_opnd+n,
     * just do if-check that comp is in opnd or result range. */
    if (comp == ISA_PRINT_COMP_name) {
      arg[i] = ISA_PRINT_AsmName(topcode);
    }
    else if (comp >= ISA_PRINT_COMP_opnd && comp < ISA_PRINT_COMP_result) {
	INT n = comp - ISA_PRINT_COMP_opnd;
	const ISA_OPERAND_VALTYP *vtype = ISA_OPERAND_INFO_Operand(oinfo, n);
	ISA_OPERAND_USE use = ISA_OPERAND_INFO_Use(oinfo, n);
	arg[i] = buf + cursor;
	cursor += Format_Operand(buf + cursor, pc, vtype, use, opnd[n], flags);
    }
    else if (comp >= ISA_PRINT_COMP_result && comp <= ISA_PRINT_COMP_MAX) {
	INT n = comp - ISA_PRINT_COMP_result;
	const ISA_OPERAND_VALTYP *vtype = ISA_OPERAND_INFO_Result(oinfo, n);
	arg[i] = buf + cursor;
	cursor += Format_Operand(buf + cursor, 0, vtype, OU_UNDEFINED, 
				 result[n], flags);
    }
    else if (comp == ISA_PRINT_COMP_end) {
      ; /* ignore */
    }
    else {
      assert(0);
    /*NOTREACHED*/
    }
  } while (++i, comp != ISA_PRINT_COMP_end);

  return sprintf(bufptr, ISA_PRINT_INFO_Format(pinfo),
		 arg[0], arg[1], arg[2], arg[3], 
		 arg[4], arg[5], arg[6], arg[7],
		 arg[8]);
}

/* ====================================================================
 *
 *  TI_ASM_Set_Bundle_Comp
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_ASM_Set_Bundle_Comp(
  ISA_BUNDLE           *bundle,
  ISA_BUNDLE_PACK_COMP  comp,
  UINT64                val
)
{
  const ISA_BUNDLE_PACK_INFO *pinfo = ISA_BUNDLE_Pack_Info();
  INT i = ISA_BUNDLE_Pack_Info_Index(comp);
  for (pinfo = pinfo + i; ISA_BUNDLE_PACK_INFO_Comp(pinfo) == comp; ++pinfo) {
    UINT64 mask = ISA_BUNDLE_PACK_INFO_Mask(pinfo);
    INT comp_pos = ISA_BUNDLE_PACK_INFO_CompPos(pinfo);
    INT bundle_pos = ISA_BUNDLE_PACK_INFO_BundlePos(pinfo);
    INT index = ISA_BUNDLE_PACK_INFO_Index(pinfo);
    bundle->word[index] =   (bundle->word[index] & ~mask)
			  | (((val >> comp_pos) << bundle_pos) & mask);
  }
}


/* ====================================================================
 *
 *  TI_ASM_Get_Bundle_Comp
 *
 *  See interface description
 *
 * ====================================================================
 */
UINT64 TI_ASM_Get_Bundle_Comp(
  const ISA_BUNDLE     *bundle,
  ISA_BUNDLE_PACK_COMP  comp
)
{
  UINT64 val = 0;
  const ISA_BUNDLE_PACK_INFO *pinfo = ISA_BUNDLE_Pack_Info();
  INT i = ISA_BUNDLE_Pack_Info_Index(comp);
  for (pinfo = pinfo + i; ISA_BUNDLE_PACK_INFO_Comp(pinfo) == comp; ++pinfo) {
    UINT64 mask = ISA_BUNDLE_PACK_INFO_Mask(pinfo);
    INT comp_pos = ISA_BUNDLE_PACK_INFO_CompPos(pinfo);
    INT bundle_pos = ISA_BUNDLE_PACK_INFO_BundlePos(pinfo);
    INT index = ISA_BUNDLE_PACK_INFO_Index(pinfo);
    val |= ((bundle->word[index] & mask) >> bundle_pos) << comp_pos;
  }
  return val;
}

/* ====================================================================
 *
 *  TI_ASM_Set_Bundle_Reloc_Value
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_ASM_Set_Bundle_Reloc_Value(
  ISA_BUNDLE *bundle,
  INT         slot,
  UINT64      val
)
{
  INT opnd;
  INT words;
  const ISA_PACK_INFO *pinfo;
  INT comp;
  INT i;
  ISA_PACK_INST inst[ISA_MAX_SLOTS];
  TOP topcode;
  INT template_bits = TI_ASM_Get_Bundle_Comp(bundle, 
					     ISA_BUNDLE_PACK_COMP_template);
  ISA_EXEC_UNIT ex_unit = ISA_EXEC_Unit(template_bits, slot);

  for (i = 0; i < ISA_MAX_SLOTS; ++i) {
    ISA_BUNDLE_PACK_COMP slot_comp = 
      (ISA_BUNDLE_PACK_COMP)(ISA_BUNDLE_PACK_COMP_slot + i);
    inst[i] = TI_ASM_Get_Bundle_Comp(bundle, slot_comp);
  }
  topcode = ISA_Decode_Inst(inst + slot, ex_unit);

  /* What should we really do if a bad topcode?
   */
  if (topcode == TOP_UNDEFINED) {
    fprintf(stderr, "TI_ASM_Set_Bundle_Reloc_Value: couldn't decode instruction\n");
    assert(FALSE);
  }

  opnd = TOP_Relocatable_Operand(topcode, NULL);
  if (opnd < 0) {
    fprintf(stderr, "TI_ASM_Set_Bundle_Reloc_Value: %s does not have a relocatable field\n",
		    TOP_Name(topcode));
    assert(FALSE);
  }

  words = ISA_PACK_Inst_Words(topcode);
  if (words <= 0) {
    fprintf(stderr, "TI_ASM_Set_Bundle_Reloc_Value: bad number of inst words (%d) for %s\n",
		    words, TOP_Name(topcode));
    assert(FALSE);
  }

  pinfo = ISA_PACK_Info(topcode);
  for (;;) {
    ISA_BUNDLE_PACK_COMP slot_comp;
    do {
      comp = ISA_PACK_INFO_Comp(pinfo);
      if (comp == ISA_PACK_COMP_opnd + opnd) {
	UINT64 mask = ISA_PACK_INFO_Mask(pinfo);
	UINT32 opndpos = ISA_PACK_INFO_OpndPos(pinfo);
	UINT32 instpos = ISA_PACK_INFO_InstPos(pinfo);
	inst[slot] =   (inst[slot] & ~(mask << instpos))
		     | (((val >> opndpos) & mask) << instpos);
      }
    } while (++pinfo, comp != ISA_PACK_COMP_end);

    slot_comp = (ISA_BUNDLE_PACK_COMP)(ISA_BUNDLE_PACK_COMP_slot + slot);
    TI_ASM_Set_Bundle_Comp(bundle, slot_comp, inst[slot]);

    if (--words == 0) break;

    ++slot;
    if (slot >= ISA_MAX_SLOTS) {
      fprintf(stderr, "TI_ASM_Set_Bundle_Reloc_Value: can't handle cross bundle reloc for %s\n",
		      TOP_Name(topcode));
      assert(FALSE);
    }
  }
}


/* ====================================================================
 *
 *  TI_ASM_Get_Bundle_Reloc_Value
 *
 *  See interface description
 *
 * ====================================================================
 */
UINT64 TI_ASM_Get_Bundle_Reloc_Value(
  const ISA_BUNDLE *bundle,
  INT               slot
)
{
  INT opnd;
  const ISA_PACK_INFO *pinfo;
  INT comp;
  UINT64 val;
  INT words;
  INT i;
  const ISA_OPERAND_INFO *oinfo;
  const ISA_OPERAND_VALTYP *vtype;
  ISA_PACK_INST inst[ISA_MAX_SLOTS];
  TOP topcode;
  INT template_bits = TI_ASM_Get_Bundle_Comp(bundle, 
					     ISA_BUNDLE_PACK_COMP_template);
  ISA_EXEC_UNIT ex_unit = ISA_EXEC_Unit(template_bits, slot);

  for (i = 0; i < ISA_MAX_SLOTS; ++i) {
    ISA_BUNDLE_PACK_COMP slot_comp = 
      (ISA_BUNDLE_PACK_COMP)(ISA_BUNDLE_PACK_COMP_slot + i);
    inst[i] = TI_ASM_Get_Bundle_Comp(bundle, slot_comp);
  }
  topcode = ISA_Decode_Inst(inst + slot, ex_unit);

  /* What should we really do if a bad topcode?
   */
  if (topcode == TOP_UNDEFINED) {
    fprintf(stderr, "TI_ASM_Get_Bundle_Reloc_Value: couldn't decode instruction\n");
    assert(FALSE);
  }

  opnd = TOP_Relocatable_Operand(topcode, NULL);
  if (opnd < 0) {
    fprintf(stderr, "TI_ASM_Get_Bundle_Reloc_Value: %s does not have a relocatable field\n",
		    TOP_Name(topcode));
    assert(FALSE);
  }

  words = ISA_PACK_Inst_Words(topcode);
  if (words <= 0) {
    fprintf(stderr, "TI_ASM_Get_Bundle_Reloc_Value: bad number of inst words (%d) for %s\n",
		    words, TOP_Name(topcode));
    assert(FALSE);
  }

  pinfo = ISA_PACK_Info(topcode);
  val = 0;
  for (;;) {
    do {
      comp = ISA_PACK_INFO_Comp(pinfo);
      if (comp == ISA_PACK_COMP_opnd + opnd) {
	UINT64 mask = ISA_PACK_INFO_Mask(pinfo);
	UINT32 opndpos = ISA_PACK_INFO_OpndPos(pinfo);
	UINT32 instpos = ISA_PACK_INFO_InstPos(pinfo);
	val |= ((inst[slot] >> instpos) & mask) << opndpos;
      }
    } while (++pinfo, comp != ISA_PACK_COMP_end);

    if (--words == 0) break;

    ++slot;
    if (slot >= ISA_MAX_SLOTS) {
      fprintf(stderr, "TI_ASM_Get_Bundle_Reloc_Value: can't handle cross bundle reloc for %s\n",
		      TOP_Name(topcode));
      assert(FALSE);
    }
  }

  oinfo = ISA_OPERAND_Info(topcode);
  vtype = ISA_OPERAND_INFO_Operand(oinfo, opnd);
  if (ISA_OPERAND_VALTYP_Is_Signed(vtype)) {
    INT size = ISA_OPERAND_VALTYP_Size(vtype);
    INT shift = 64 - size;
    val = ((INT64)val << shift) >> shift;
  }

  return val;
}

/* ====================================================================
 *
 *  TI_ASM_Unpack_Inst
 *
 *  See interface description
 *
 * ====================================================================
 */
TOP TI_ASM_Unpack_Inst(
  const ISA_PACK_INST *pinst,
  ISA_EXEC_UNIT ex_unit, 
  INT64 *result, 
  INT64 *opnd,
  BOOL xlate_pseudo)
{
  INT comp;
  INT i;
  INT j;
  INT words;
  const ISA_PACK_INFO *pinfo;
  const ISA_PACK_ADJ_INFO *ainfo;
  const ISA_OPERAND_INFO *oinfo;
  TOP topcode;

  /* Decode the instruction opcode.
   */
  topcode = ISA_Decode_Inst(pinst, ex_unit);
  if (topcode == TOP_UNDEFINED) return topcode;

  /* Unpack the raw operands and results.
   */
  memset(result, 0, sizeof(*result) * ISA_OPERAND_max_results);
  memset(opnd, 0, sizeof(*opnd) * ISA_OPERAND_max_operands);
  pinfo = ISA_PACK_Info(topcode);
  words = ISA_PACK_Inst_Words(topcode);
  for (j = 0; j < words; ++j) {
    ISA_PACK_INST inst = pinst[j];
    do {
      UINT64 mask = ISA_PACK_INFO_Mask(pinfo);
      UINT32 opndpos = ISA_PACK_INFO_OpndPos(pinfo);
      UINT32 instpos = ISA_PACK_INFO_InstPos(pinfo);
      INT64 val = ((inst >> instpos) & mask) << opndpos;

      comp = ISA_PACK_INFO_Comp(pinfo);
      switch (comp) {
      case ISA_PACK_COMP_opnd:
      case ISA_PACK_COMP_opnd+1:
      case ISA_PACK_COMP_opnd+2:
      case ISA_PACK_COMP_opnd+3:
      case ISA_PACK_COMP_opnd+4:
      case ISA_PACK_COMP_opnd+5:
	{
	  INT n = comp - ISA_PACK_COMP_opnd;
	  opnd[n] |= val;
	}
	break;

      case ISA_PACK_COMP_result:
      case ISA_PACK_COMP_result+1:
	{
	  INT n = comp - ISA_PACK_COMP_result;
	  result[n] |= val;
	}
	break;

      case ISA_PACK_COMP_end:
	break;

      default:
	assert(0);
	/*NOTREACHED*/
      }
    } while (++pinfo, comp != ISA_PACK_COMP_end);
  }

  /* Provide any adjustments for the operands between packed and assembly
   * language forms.
   */
  ainfo = ISA_PACK_Adj_Info(topcode);
  if (ainfo) ISA_PACK_Adjust_Operands(ainfo, opnd, TRUE);

  /* If desired, translated pseudo instructions.
   */
  if (xlate_pseudo) {
    topcode = ISA_PSEUDO_Translate(topcode, result, opnd, ISA_PSEUDO_to_pseudo);
  }

  /* Provide any normalization to any operands: convert enums to their
   * 'biased' form and sign-extend signed literals.
   */
  oinfo = ISA_OPERAND_Info(topcode);
  for (i = 0; i < ISA_OPERAND_INFO_Operands(oinfo); ++i) {
    const ISA_OPERAND_VALTYP *vtype = ISA_OPERAND_INFO_Operand(oinfo, i);
    UINT64 val = opnd[i];

    if (ISA_OPERAND_VALTYP_Is_Enum(vtype)) {
      ISA_ENUM_CLASS_VALUE e;
      ISA_ENUM_CLASS_VALUE ecv = ECV_UNDEFINED;
      ISA_ENUM_CLASS ec = ISA_OPERAND_VALTYP_Enum_Class(vtype);

      for (e = ISA_EC_First_Value(ec);
	   e <= ISA_EC_Last_Value(ec);
	   e = (ISA_ENUM_CLASS_VALUE)(e + 1))
      {
        if (ISA_ECV_Intval(e) == val) {
	  ecv = e;
	  break;
	}
      }
      opnd[i] = ecv;
    } else if (ISA_OPERAND_VALTYP_Is_Signed(vtype)) {
      INT size = ISA_OPERAND_VALTYP_Size(vtype);
      INT shift = 64 - size;
      opnd[i] = ((INT64)val << shift) >> shift;
    }
  }

  return topcode;
}
