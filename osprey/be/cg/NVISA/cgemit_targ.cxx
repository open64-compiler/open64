/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 *
 * Module: cgemit_targ.c
 * $Revision: 1.169 $
 * $Date: 05/07/19 19:01:12-07:00 $
 * $Author: fchow@fluorspar.internal.keyresearch.com $
 * $Source: /scratch/mee/Patch0002-taketwo/kpro64-pending/be/cg/x8664/SCCS/s.cgemit_targ.cxx $
 *
 * Description:
 *
 * Target-specific cgemit code.
 *
 * ====================================================================
 * ====================================================================
 */


#include <stdint.h>
#include <ctype.h>
#include "elf_stuff.h"

#define	USE_STANDARD_TYPES 1
#include "defs.h"
#include "config_targ_opt.h"
#include "targ_const.h"
#include "targ_const_private.h"
#include "vstring.h"
#include "config_asm.h"
#include "em_elf.h"
#include "symtab.h"
#include "tn.h"
#include "cgemit.h"
#include "cgemit_targ.h"
#include "cgdwarf.h"
#include "cgdwarf_targ.h"
#include "data_layout.h"
#include "bb.h"
#include "op.h"
#include "iface_scn.h"
#include "cg_flags.h"
#include "glob.h"
#include "sections.h"
#include "targ_isa_print.h"
#include "config_debug.h"
#include "cgtarget.h"
#include "tracing.h"
#include "erglob.h"
#include "erbe.h"
#include "ttype.h"
#include "whirl2ops.h"

static void CGEMIT_Function_Prototype (ST*);

static ST *current_pu = NULL;

static BOOL
Non_Default_Text_Section (ST *pu)
{
  if (!pu || !ST_base(pu))
    return FALSE;

  return ((ST_sclass(ST_base(pu)) == SCLASS_TEXT) && 
	  strcmp(ST_name(ST_base(pu)), ".text"));
}


void
CGEMIT_Targ_Initialize (ST *pu)
{
  current_pu = pu;
}


void
CGEMIT_Targ_Text_Initialize (ST *pu)
{
  if (Non_Default_Text_Section(pu))
    fprintf (Asm_File, "\t.begin\tliteral_prefix %s\n", ST_name(ST_base(pu)));
}


void
CGEMIT_Targ_Text_Finalize (ST *pu)
{
  if (Non_Default_Text_Section(pu))
    fprintf (Asm_File, "\t.end\tliteral_prefix\n");
}


BOOL
CGEMIT_Align_Section_Once (const char *scn_name)
{
  return strcmp(scn_name, ".literal") && strcmp(scn_name, ".text");
}

void
CGEMIT_Prn_File_Dir_In_Asm(USRCPOS usrcpos,
                        const char *pathname,
                        const char *filename)
{
  if (CG_emit_non_gas_syntax)
    fprintf (Asm_File, "\t%s\t%d\t\"%s/%s\"\n",
	     AS_FILE, USRCPOS_filenum(usrcpos)-1, pathname, filename);
  else fprintf (Asm_File, "\t%s\t%d\t\"%s/%s\"\n",
	   AS_FILE, USRCPOS_filenum(usrcpos), pathname, filename);
}

extern void
CGEMIT_Prn_Line_Dir_In_Asm (USRCPOS usrcpos)
{
  if (CG_emit_non_gas_syntax)
    fprintf (Asm_File, "\t.loc\t%d\t%d\t%d\n", 
	     USRCPOS_filenum(usrcpos)-1,
	     USRCPOS_linenum(usrcpos),
	     USRCPOS_column(usrcpos));
  else
    fprintf (Asm_File, "\t.loc\t%d\t%d\t%d\n", 
	     USRCPOS_filenum(usrcpos),
	     USRCPOS_linenum(usrcpos),
	     USRCPOS_column(usrcpos));    
  }


void
CGEMIT_Prn_Scn_In_Asm (ST *st, ST *cur_section)
{
  UINT32 tmp, power;
  // Bug 511
  // Do not emit section attributes for the __libc_ sections. Assumes that
  // user inline assembly will do the job. We will avoid duplicate entries.
  {
    char *name = ST_name(st);
    if (strstr(name, "__libc_") == name)
      return;
  }
  power = 0;
  for (tmp = STB_align(st); tmp > 1; tmp >>= 1) power++;
  CGEMIT_Prn_Scn_In_Asm(Asm_File,
			ST_name(st),
			Get_Section_Elf_Type(STB_section_idx(st)),
			Get_Section_Elf_Flags(STB_section_idx(st)),
			Get_Section_Elf_Entsize(STB_section_idx(st)),
			power,
			(cur_section != NULL) ? ST_name(cur_section) : NULL);
}

void
CGEMIT_Prn_Scn_In_Asm (FILE       *asm_file,
		       const char *scn_name,
		       Elf64_Word  scn_type,
		       Elf64_Word  scn_flags,
		       Elf64_Xword scn_entsize,
		       Elf64_Word  scn_align,
		       const char *cur_scn_name)
{
  // don't print section info, except for special sections like dwarf sections.
  // otherwise assume variable declarations handle data.
  if ( ! Is_Dwarf_Section_To_Emit(scn_name))
	return;

  // For now, emit debug sections in comments
  {
    char scn_flags_string[5];
    char *p = &scn_flags_string[0];
    
    fprintf (asm_file, "\n \t//DWARF %s %s", AS_SECTION, scn_name);
    if (CG_emit_non_gas_syntax && strcmp(scn_name, ".srdata") == 0) {
      static BOOL printed = FALSE;
      if (!printed) {
	fprintf(asm_file, ", %d, %#x, %" LL_FORMAT "d, ", 
		scn_type, scn_flags, (UINT64) scn_entsize);
	int tmp1 = 1, tmp2 = scn_align;
	for (; tmp2 >= 1; tmp1 *= 2, tmp2 --);
	fprintf(asm_file, "%d", tmp1);
	printed = TRUE;
      }
    }
    if (! CG_emit_non_gas_syntax) {
      if (scn_flags & SHF_WRITE) *p++ = 'w';
      if (scn_flags & SHF_ALLOC) *p++ = 'a';
      if (scn_flags & SHF_EXECINSTR) *p++ = 'x';
      *p = '\0'; // null terminate the string.
      fprintf (asm_file, ", \"%s\"", scn_flags_string);
      if (scn_type == SHT_PROGBITS)
        fprintf (asm_file, ",@progbits");
    }
    if (strcmp(scn_name, ".debug_frame") == 0) // bug 2463
      fprintf(asm_file, "\n.LCIE:");
  }

  fprintf (asm_file, "\n");

  /* For most sections, we only emit the alignment the first time we
     see it (in cgemit.cxx:Init_Section), because .org is used to
     place/align all data items relative to the start of the
     section. But some we always align. */

  if (!CGEMIT_Align_Section_Once(scn_name))
    fprintf (asm_file, "\t//DWARF %s\t%d\n", AS_ALIGN, scn_align);
}

void
CGEMIT_Change_Origin_In_Asm (ST *st, INT64 offset)
{
  /* Make sure these match what is used in eh_region.cxx (with "t"
     changed to "e" or "h"). */
#define EH_REGION_LINKONCE_PREFIX ".gnu.linkonce.e."
#define EH_DESC_LINKONCE_PREFIX ".gnu.linkonce.h."
    
  /* We don't want to emit .org for literal sections, since the .org
     for these gets reset for every pu; and because we don't need them
     here.

     We don't want to emit .org for exception region or descriptors
     since the section contains both .xt_except_table/.xt_except_desc
     and .gnu.linkonce.e.* / .gnu.linkonce.h.* sections. We don't need
     the .org for these because there are no alignment issues since
     all INITVs in the section are 4 bytes, and the section start is 4
     byte aligned. */

  if (strcmp(ST_name(st), ".literal") &&
      strcmp(ST_name(st), ".xt_except_table") &&
      strcmp(ST_name(st), ".xt_desc_table") &&
      strncmp(ST_name(st), EH_REGION_LINKONCE_PREFIX,
	      strlen(EH_REGION_LINKONCE_PREFIX)) &&
      strncmp(ST_name(st), EH_DESC_LINKONCE_PREFIX,
	      strlen(EH_DESC_LINKONCE_PREFIX)))
  {
    if (CG_emit_non_gas_syntax)
      fprintf (Asm_File, "\t%s 0x%" LL_FORMAT "x\n", ".origin", offset);
    else fprintf (Asm_File, "\t%s 0x%" LL_FORMAT "x\n", AS_ORIGIN, offset);
    fprintf ( Asm_File, "\t%s\t0\n", AS_ALIGN );
  }
}


// whether to use the base st for the reloc
extern BOOL
CGEMIT_Use_Base_ST_For_Reloc (INT reloc, ST *st)
{
	// to handle function pointers.
	// example: see gcc.c-torture/execute/func-ptr-1.c
	if (ST_sclass(st) == SCLASS_TEXT)
	        return FALSE;
	else 
		return ST_is_export_local(st);
}

	  
extern INT
CGEMIT_Relocs_In_Asm (TN *t, ST *st, vstring *buf, INT64 *val)
{
	INT paren = 1;	// num parens
	// only add in GP_DISP if based on gprel section
	// not if based on ipa-generated extern.
	if (ST_class(st) == CLASS_BLOCK && STB_section(st)) {
		*val -= GP_DISP;
	}
	switch (TN_relocs(t)) {
	case TN_RELOC_GOT_DISP:
        	*buf = vstr_concat (*buf, "%got_disp");
		break;
	case TN_RELOC_GOT_PAGE:
        	*buf = vstr_concat (*buf, "%got_page");
		break;
	case TN_RELOC_GOT_OFST:
        	*buf = vstr_concat (*buf, "%got_ofst");
		break;
	case TN_RELOC_HI_GPSUB:
        	*buf = vstr_concat (*buf, "%hi(%neg(%gp_rel");
		paren += 2;
		break;
	case TN_RELOC_LO_GPSUB:
        	*buf = vstr_concat (*buf, "%lo(%neg(%gp_rel");
		paren += 2;
		break;
	case TN_RELOC_GPREL16:
        	*buf = vstr_concat (*buf, "%gp_rel");
		break;
	case TN_RELOC_HIGH16:
        	*buf = vstr_concat (*buf, "%hi");
		break;
	case TN_RELOC_LOW16:
        	*buf = vstr_concat (*buf, "%lo");
		break;
    	default:
		#pragma mips_frequency_hint NEVER
    		FmtAssert (FALSE, ("relocs_asm: illegal reloc TN"));
		/*NOTREACHED*/
	}
	*buf = vstr_concat (*buf, "(" );
	*buf = vstr_concat (*buf, ST_name(st));
	*buf = vstr_concat (*buf, Symbol_Name_Suffix);
	return paren;
}


extern void
CGEMIT_Relocs_In_Object (TN *t, ST *st, INT32 PC, pSCNINFO PU_section, INT64 *val)
{
  FmtAssert(FALSE, ("NYI"));
} 

// add events and relocs as needed for call
extern void 
CGEMIT_Add_Call_Information (OP *op, BB *bb, INT32 PC, pSCNINFO PU_section)
{
	ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_CALLINFO);
	ST *call_sym = CALLINFO_call_st(ANNOT_callinfo(ant));
    	Elf_Event_Kind event_type;

	if (call_sym == NULL) return;
	if (ST_is_export_local(call_sym)) {
		event_type = EK_FCALL_LOCAL;
	}
	else {
		event_type = EK_FCALL_EXTERN;
      	}
	Em_Add_New_Event (event_type, PC, EMT_Put_Elf_Symbol(call_sym),
			0, 0, PU_section);
      
	// TODO: if indirect call add plt reloc

	// do pcrel relocation for all calls,
	// as even statics may be forward refs so don't know pc.
	// Ld will generate a stub if needed.
	Em_Add_New_Rela (EMT_Put_Elf_Symbol(call_sym), 
		R_IA_64_PCREL21B, PC, 0, PU_section);

}


/* Generate the .frame, .mask and the .fmask directives for the assembler. */
void
CGEMIT_Gen_Asm_Frame (INT64 frame_len)
{
  // no stack in nvisa
}


// Generate the entry (.proc) directive.
void 
CGEMIT_Prn_Ent_In_Asm (ST *pu)
{
  FmtAssert(false, ("No AS_ENT for x86_64"));
}


// Preprocess FP registers before emit.  Needed only for IA-32.
void
STACK_FP_Fixup_PU()
{}

void
CGEMIT_Weak_Alias (ST *sym, ST *strongsym) 
{
  fprintf ( Asm_File, "\t%s\t%s\n", AS_WEAK, ST_name(sym));
  fprintf ( Asm_File, "\t%s = %s", ST_name(sym), ST_name(strongsym));
  if (ST_is_export_local(strongsym) && ST_class(strongsym) == CLASS_VAR) {
    // modelled after EMT_Write_Qualified_Name (bug 6899)
    if (ST_level(strongsym) == GLOBAL_SYMTAB)
      fprintf ( Asm_File, "%s%d", Label_Name_Separator, ST_index(strongsym));
    else
      fprintf ( Asm_File, "%s%d%s%d", Label_Name_Separator, 
		ST_pu(Get_Current_PU_ST()),
      		Label_Name_Separator, ST_index(strongsym));
  }
  fprintf ( Asm_File, "\n");
}

void CGEMIT_Write_Literal_TCON(ST *lit_st, TCON tcon)
{
  INT64 val;
  if (TCON_ty(tcon) == MTYPE_F4)
    val = TCON_word0(tcon);
  else if ((TCON_ty(tcon) == MTYPE_I4) || (TCON_ty(tcon) == MTYPE_U4))
    val = TCON_v0(tcon);
  else
    FmtAssert(FALSE, ("Invalid literal value"));
  fprintf ( Asm_File, "\t%s\t", ".literal");
  EMT_Write_Qualified_Name(Asm_File, lit_st);
  if ((val >= INT32_MIN) && (val <= INT32_MAX)) 
    fprintf(Asm_File, ", %" LL_FORMAT "d\n", val);
  else
    fprintf(Asm_File, ", %#" LL_FORMAT "x\n", val);
  
}

void CGEMIT_Write_Literal_Label (ST *lit_st, LABEL_IDX lab)
{
  fprintf ( Asm_File, "\t%s\t", ".literal");
  EMT_Write_Qualified_Name(Asm_File, lit_st);
  union {
    UINT64 u;
    void *p;
  } u;
  u.u = 0;
  u.p = LABEL_name(lab);
  fprintf(Asm_File, ", %" LL_FORMAT "d\n", u.u);
}

void CGEMIT_Write_Literal_Symbol (ST *lit_st, ST *sym, 
				  Elf64_Sxword sym_ofst)
{
  ST *basesym;
  basesym = sym;
  INT64 base_ofst = 0;

  if (Has_Base_Block(sym) && ST_is_export_local(sym) && ST_class(sym) != CLASS_FUNC) {
    Base_Symbol_And_Offset (sym, &basesym, &base_ofst);
  }
  base_ofst += sym_ofst;

  fprintf ( Asm_File, "\t%s\t", ".literal");
  EMT_Write_Qualified_Name(Asm_File, lit_st);
  fprintf ( Asm_File, ", ");
  if (ST_class(sym) == CLASS_CONST) {
    EMT_Write_Qualified_Name (Asm_File, basesym);
    if (base_ofst == 0)
      fprintf (Asm_File, "\n");
    else
      fprintf (Asm_File, " %+lld\n", base_ofst);
  }
  else {
    EMT_Write_Qualified_Name (Asm_File, sym);
    if (sym_ofst == 0)
      fprintf (Asm_File, "\n");
    else
      fprintf (Asm_File, " %+lld\n", (INT64) sym_ofst);
  }
}

void CGEMIT_Alias (ST *sym, ST *strongsym) 
{
  fprintf ( Asm_File, "\t%s = %s\n", ST_name(sym), ST_name(strongsym));
}

void CGEMIT_Global_Decls (void)
{
  // emit global parameter regs
  // %r<N> enables regs %r0..%r[N-1]
  // we don't use 0 reg (reserved for empty), so add one to last number
  fprintf(Asm_File, "\n");
  fprintf(Asm_File, "\t.reg .u32 %%ra<17>;\n");
  fprintf(Asm_File, "\t.reg .u64 %%rda<17>;\n");
  fprintf(Asm_File, "\t.reg .f32 %%fa<17>;\n");
  fprintf(Asm_File, "\t.reg .f64 %%fda<17>;\n");
  fprintf(Asm_File, "\t.reg .u32 %%rv<5>;\n");
  fprintf(Asm_File, "\t.reg .u64 %%rdv<5>;\n");
  fprintf(Asm_File, "\t.reg .f32 %%fv<5>;\n");
  fprintf(Asm_File, "\t.reg .f64 %%fdv<5>;\n");
  fprintf(Asm_File, "\n");

  // iterate thru functions, generating prototypes where needed.
  INT i;
  ST *sym;
  FOREACH_SYMBOL (GLOBAL_SYMTAB, sym, i) {
    if (ST_class(sym) == CLASS_FUNC
      && ST_is_export_local(sym)
      && !ST_is_not_used(sym)
      && PU_no_inline(ST_pu(sym))
      && TY_has_prototype(ST_pu_type(sym)))
    {
      CGEMIT_Function_Prototype(sym);
    }
  }
}

static char*
Register_Type_Name (ISA_REGISTER_CLASS rc)
{
	switch (rc) {
	case ISA_REGISTER_CLASS_integer:
		return ".u32";
	case ISA_REGISTER_CLASS_integer16:
		return ".u16";
	case ISA_REGISTER_CLASS_integer64:
		return ".u64";
	case ISA_REGISTER_CLASS_float:
		return ".f32";
	case ISA_REGISTER_CLASS_float64:
		return ".f64";
	case ISA_REGISTER_CLASS_predicate:
		return ".pred";
	default:
		FmtAssert(FALSE, ("unexpected register class"));
	}
}

static
void CGEMIT_Register_Definitions (void)
{
  ISA_REGISTER_CLASS rc;
  REGISTER reg;
  INT i;
  FOR_ALL_ISA_REGISTER_CLASS(rc) {
    reg = Last_Reg_Allocated(rc);
    if (reg != REGISTER_UNDEFINED) {
        fprintf(Asm_File, "\t.reg ");
        fprintf(Asm_File, "%s ", Register_Type_Name(rc));
        // replace %rN with %r<N>
        char rname[16];
        strcpy(rname, REGISTER_name(rc,reg));
        char *p = rname;
        for (; !isdigit(*p); ++p) 
          ;
        *p = '\0';
        fprintf(Asm_File, "%s<%d>;\n", rname, reg+1);
    }
  }
}

void
CGEMIT_Function_Prototype (ST *pu)
{
  FmtAssert(!ST_in_global_mem(pu), ("only do prototypes for called functions"));
  // .func (rv) name (ra)
  INT i;
  TN *tn;
  ISA_REGISTER_CLASS rc;
  fprintf ( Asm_File, "\n\t.func (");
  RETURN_INFO return_info = Get_Return_Info (TY_ret_type(ST_pu_type(pu)),
                                           No_Simulated);
  for (i = 0; i < RETURN_INFO_count(return_info); i++) {
    tn = PREG_To_TN (
      MTYPE_To_PREG(RETURN_INFO_mtype(return_info,i)),
      RETURN_INFO_preg (return_info, i));
    rc = TN_register_class(tn);
    if (i > 0) fprintf (Asm_File, ", ");
    fprintf ( Asm_File, ".reg %s %s", 
      Register_Type_Name(rc),
      ABI_PROPERTY_Reg_Name(rc, REGISTER_machine_id(rc, TN_register(tn))));
  }

  fprintf ( Asm_File, ") %s (", ST_name(pu) );
  i = 0;
  PLOC ploc;
  ploc = Setup_Input_Parameter_Locations (ST_pu_type(pu));
  TYLIST_IDX tl = TY_parms(ST_pu_type(pu));
  if (tl != (TYLIST_IDX) NULL) {
    i = 0;
    for (; TYLIST_ty(tl); tl = TYLIST_next(tl)) {
      TY_IDX ty = TYLIST_ty(tl);
      ploc = Get_Input_Parameter_Location (ty);
      ploc = First_Input_PLOC_Reg (ploc, ty);
      while (PLOC_is_nonempty(ploc)) {
        FmtAssert(!PLOC_on_stack(ploc), ("stack params not supported"));
        if (i > 0) fprintf (Asm_File, ", ");
        ++i;
        tn = PREG_To_TN(ty,PLOC_reg(ploc));
        rc = TN_register_class(tn);
        fprintf ( Asm_File, ".reg %s %s", 
          Register_Type_Name(rc),
          ABI_PROPERTY_Reg_Name(rc,REGISTER_machine_id(rc, TN_register(tn))));
        ploc = Next_Input_PLOC_Reg (ploc);
      }
    }
  }
  fprintf ( Asm_File, ")\n");
}

void
CGEMIT_Function_Definition (ST *pu)
{
  if (ST_in_global_mem(pu)) {
    fprintf ( Asm_File, "\n\t.entry %s\n", ST_name(pu) );
  } else {
    CGEMIT_Function_Prototype(pu);
  }
  fprintf ( Asm_File, "\t{\n");
  CGEMIT_Register_Definitions();
}

void
CGEMIT_Call (OP *op)
{
  // call (rv), name, (ra)
  INT i;
  TN *tn;
  ISA_REGISTER_CLASS rc;
  ST *call_st = TN_var(OP_opnd(op,0));
  TY_IDX func_ty = ST_pu_type(call_st);
  if (OP_code(op) == TOP_call)
    fprintf(Asm_File, "\tcall ");
  else if (OP_code(op) == TOP_call_uni)
    fprintf(Asm_File, "\tcall.uni ");
  else
    FmtAssert(FALSE, ("NYI"));

  RETURN_INFO return_info = Get_Return_Info(TY_ret_type(func_ty), No_Simulated);
  if (RETURN_INFO_count(return_info) > 0) {
    fprintf(Asm_File, "(");
    for (i = 0; i < RETURN_INFO_count(return_info); i++) {
      if (i > 0) fprintf (Asm_File, ", ");
      tn = PREG_To_TN (
        MTYPE_To_PREG(RETURN_INFO_mtype(return_info,i)),
        RETURN_INFO_preg (return_info, i));
      rc = TN_register_class(tn);
      fprintf ( Asm_File, "%s", 
        ABI_PROPERTY_Reg_Name(rc, REGISTER_machine_id(rc, TN_register(tn))));
    }
    fprintf(Asm_File, "), ");
  }

  fprintf(Asm_File, "%s", ST_name(call_st));

  PLOC ploc = Setup_Output_Parameter_Locations (func_ty);
  TYLIST_IDX tl = TY_parms(func_ty);
  if (tl != (TYLIST_IDX) NULL) {
    fprintf(Asm_File, ", (");
    i = 0;
    for (; TYLIST_ty(tl); tl = TYLIST_next(tl)) {
      TY_IDX ty = TYLIST_ty(tl);
      ploc = Get_Output_Parameter_Location(ty);
      ploc = First_Output_PLOC_Reg (ploc, ty);
      while (PLOC_is_nonempty(ploc)) {
        FmtAssert(!PLOC_on_stack(ploc), ("stack params not supported"));
        if (i > 0) fprintf (Asm_File, ", ");
        ++i;
        tn = PREG_To_TN(ty,PLOC_reg(ploc));
        rc = TN_register_class(tn);
        fprintf ( Asm_File, "%s", 
          ABI_PROPERTY_Reg_Name(rc,REGISTER_machine_id(rc, TN_register(tn))));
        ploc = Next_Output_PLOC_Reg (ploc);
      }
    }
    fprintf(Asm_File, ")");
  }
  fprintf(Asm_File, ";\n");
}

static BOOL
Is_Dynamic_Size_Shared_Array (ST *st)
{
  // do we need to worry about actual variable-sized arrays?
  // do we need to worry about multi-dim arrays?
  TY_IDX ty = ST_type(st);
  if ( ! ST_in_shared_mem(st))
	return FALSE;
  if (TY_kind(ty) != KIND_ARRAY)
	return FALSE;
  if ( ! TY_AR_const_ubnd(ty,TY_AR_ndims(ty)-1))
	return TRUE;
  return FALSE;
}

// initially, constant space is limited to 64K;
// give error if we overflow it.
static UINT total_const_size = 0;

static void 
CGEMIT_Print_Variable_Info (ST *st)
{
  TY_IDX ty = ST_type(st);
  TYPE_ID mtype = TY_mtype(ty);

  if (Get_Trace ( TP_EMIT, 4 )) {
	fprintf(TFile, "<emit> print variable %s\n", ST_name(st));
  }

  if (ST_in_global_mem(st)) {
	fprintf(Asm_File, "\t.global ");
  }
  else if (ST_in_shared_mem(st)) {
	if (ST_sclass(st) == SCLASS_FORMAL)
		fprintf(Asm_File, "\t.param ");
	else
		fprintf(Asm_File, "\t.shared ");
  }
  else if (ST_in_local_mem(st)) {
	fprintf(Asm_File, "\t.local ");
  }
  else if (ST_in_constant_mem(st)) {
	fprintf(Asm_File, "\t.const ");
  }
  else if (ST_in_texture_mem(st)) {
	fprintf(Asm_File, "\t.tex ");
  }
  else {
	FmtAssert(FALSE, ("unexpected variable memory space"));
  }

  if (TY_kind(ty) == KIND_ARRAY || TY_kind(ty) == KIND_STRUCT) {
	// we reference offsets not direct fields, 
	// so just emit array of bytes for struct.
	mtype = MTYPE_V;
  }

  switch (mtype) {
  case MTYPE_I1: fprintf(Asm_File, ".s8"); break;
  case MTYPE_I2: fprintf(Asm_File, ".s16"); break;
  case MTYPE_I4: fprintf(Asm_File, ".s32"); break;
  case MTYPE_I8: fprintf(Asm_File, ".s64"); break;
  case MTYPE_U1: fprintf(Asm_File, ".u8"); break;
  case MTYPE_U2: fprintf(Asm_File, ".u16"); break;
  case MTYPE_U4: fprintf(Asm_File, ".u32"); break;
  case MTYPE_U8: fprintf(Asm_File, ".u64"); break;
  case MTYPE_F4: fprintf(Asm_File, ".f32"); break;
  case MTYPE_F8: fprintf(Asm_File, ".f64"); break;
  case MTYPE_V: 
	// specify alignment of byte array
	fprintf(Asm_File, ".align %d", TY_align(ty)); 
	fprintf(Asm_File, " .b8"); 
	break;
  default:
    	FmtAssert(FALSE, ("NYI"));
  }
  if (ST_class(st) == CLASS_CONST)
    fprintf(Asm_File, " __constant%d", ST_index(st));
  else
    fprintf(Asm_File, " %s", ST_name(st));

  if (Is_Dynamic_Size_Shared_Array(st)) {
	fprintf(Asm_File, "[]");
  } else if (TY_kind(ty) == KIND_ARRAY || TY_kind(ty) == KIND_STRUCT) {
	fprintf(Asm_File, "[%" LL_FORMAT "d]", TY_size(ty));
	if (ST_in_constant_mem(st)) 
		total_const_size += TY_size(ty);
  }
  else {
	if (ST_in_constant_mem(st)) 
		total_const_size += TY_size(ty);
  }
  if (total_const_size > 65536)
	ErrMsg (EC_Const_Space_Overflow);
}

void CGEMIT_Print_TCON (TCON tcon, BOOL byte_array)
{
	if (TCON_ty(tcon) == MTYPE_STRING) {
		INT i;
		// emit as byte array even if didn't ask for one
		if (!byte_array) {
			fprintf(Asm_File, "{"); // start byte array
		}
		char *p = Targ_String_Address(tcon);
		for (i = 0; i < Targ_String_Length(tcon)-1; ++i, ++p)
			fprintf(Asm_File, "0x%x,", *p);
		fprintf(Asm_File, "0x%x", *p);
		if (!byte_array) {
			fprintf(Asm_File, "}"); // end byte array
		}
	}
	else if (byte_array) {
		INT64 val8;
		INT32 val4;
    		INT16 val2;
    		INT8 val1;
		float valf;
		double vald;
		INT i;
		char *p;
		switch (TCON_ty(tcon)) {
		case MTYPE_U1:
		case MTYPE_I1:
			val1 = (INT8) Targ_To_Host (tcon);
			fprintf(Asm_File, "%d", val1);
			break;
		case MTYPE_U2:
		case MTYPE_I2:
			val2 = (INT16) Targ_To_Host (tcon);
			p = (char*) &val2;
			fprintf(Asm_File, "%d,", *p);
			++p;
			fprintf(Asm_File, "%d", *p);
			break;
		case MTYPE_U4:
		case MTYPE_I4:
			val4 = (INT32) Targ_To_Host (tcon);
			p = (char*) &val4;
			for (i = 0; i < 3; ++i, ++p)
				fprintf(Asm_File, "%d,", *p);
			fprintf(Asm_File, "%d", *p);
			break;
		case MTYPE_U8:
		case MTYPE_I8:
			val8 = (INT64) Targ_To_Host (tcon);
			p = (char*) &val8;
			for (i = 0; i < 7; ++i, ++p)
				fprintf(Asm_File, "%d,", *p);
			fprintf(Asm_File, "%d", *p);
			break;
		case MTYPE_F4:
			valf = (float) Targ_To_Host_Float (tcon);
			p = (char*) &valf;
			for (i = 0; i < 3; ++i, ++p)
				fprintf(Asm_File, "%d,", *p);
			fprintf(Asm_File, "%d", *p);
			break;
		case MTYPE_F8:
			vald = (double) Targ_To_Host_Float (tcon);
			p = (char*) &vald;
			for (i = 0; i < 7; ++i, ++p)
				fprintf(Asm_File, "%d,", *p);
			fprintf(Asm_File, "%d", *p);
			break;
		default:
			FmtAssert(FALSE, ("NYI"));
		}
	} else {
    		fprintf(Asm_File, "%s",  Targ_Print (NULL, tcon));
  		if (MTYPE_is_float(TCON_ty(tcon))) {
  			fprintf(Asm_File, " /* %s */", Targ_Print ("%g", tcon));
		}
	}
}

void CGEMIT_Print_INITV (INITV_IDX inv, BOOL byte_array)
{
    // if byte_array, because arrays and structs are defined as array of bytes,
    // must write out initialization 1 byte at a time.  assume little endian.
    TCON tcon;
    UINT size = 0;
    UINT i;
    switch (INITV_kind(inv)) {
    case INITVKIND_ZERO:
	if (byte_array) {
		size = MTYPE_byte_size(INITV_mtype(inv)) * INITV_repeat(inv);
		for (i = 0; i < size-1; ++i)
			fprintf(Asm_File, "0,");
		fprintf(Asm_File, "0");
	} else {
		fprintf(Asm_File, "0");
	}
	break;
    case INITVKIND_PAD:
	size = INITV_pad(inv) * INITV_repeat(inv);
	for (i = 0; i < size-1; ++i)
		fprintf(Asm_File, "0,");
	fprintf(Asm_File, "0");
	break;
    case INITVKIND_ONE:
	if (byte_array) {
		size = MTYPE_byte_size(INITV_mtype(inv));
		// little-endian
		fprintf(Asm_File, "1");
		for (i = 0; i < size-1; ++i)
			fprintf(Asm_File, ",0");
	} else {
		fprintf(Asm_File, "1");
	}
	break;
    case INITVKIND_VAL:
    	tcon = INITV_tc_val(inv);
	CGEMIT_Print_TCON(tcon, byte_array);
	break;
    case INITVKIND_BLOCK:
	if (!byte_array) {
		fprintf(Asm_File, "{"); // start byte array
	}
	for (INITV_IDX ninv = INITV_blk(inv); ninv; ninv = INITV_next(ninv)) {
		CGEMIT_Print_INITV (ninv, TRUE);
		if (INITV_next(ninv) != INITV_IDX_ZERO)
			fprintf(Asm_File, ",");
	}
	if (!byte_array) {
		fprintf(Asm_File, "}"); // end byte array
	}
	break;
    default:
    	FmtAssert(FALSE, ("NYI initv kind %d", INITV_kind(inv)));
    }
}

void CGEMIT_Print_Initialized_Variable (ST *st, INITO *ino)
{
  INT count = 0;
  INITV_IDX inv;
  CGEMIT_Print_Variable_Info (st);
  fprintf(Asm_File, " = ");
  FmtAssert(INITO_val(*ino) != (INITO_IDX) NULL, ("NYI"));
  FOREACH_INITV (INITO_val(*ino), inv) {
	++count;
  }
  if (count == 1) {
    CGEMIT_Print_INITV (INITO_val(*ino), FALSE);
  }
  else {
    fprintf(Asm_File, "{"); // emit as byte array
    FOREACH_INITV (INITO_val(*ino), inv) {
      CGEMIT_Print_INITV (inv, TRUE);
      if (INITV_next(inv) != INITV_IDX_ZERO)
	fprintf(Asm_File, ",");
    }
    fprintf(Asm_File, "}");
  }
  fprintf(Asm_File, ";\n");
}

void CGEMIT_Print_Variable (ST *st)
{
  if (Is_Dynamic_Size_Shared_Array(st)) {
	// dynamic-sized array, size will be allocated at runtime.
	// mark as extern
	DevWarn("dynamic-sized array");
	fprintf(Asm_File, "\t.extern");
  }
  CGEMIT_Print_Variable_Info (st);
  if (ST_class(st) == CLASS_CONST) {
    TCON tc = ST_tcon_val(st);
    fprintf(Asm_File, " = {"); // emit as byte array
    CGEMIT_Print_TCON(tc, TRUE);
    fprintf(Asm_File, "}");
  }
  fprintf(Asm_File, ";\n");
}

