/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007, 2008 PathScale, LLC.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * $Revision: 1.173 $
 * $Date: 05/11/30 16:49:43-08:00 $
 * $Author: gautam@jacinth.keyresearch $
 * $Source: be/cg/x8664/SCCS/s.cgemit_targ.cxx $
 *
 * Description:
 *
 * Target-specific cgemit code.
 *
 * ====================================================================
 * ====================================================================
 */


#include <stdint.h>
#include "elf_stuff.h"
#ifdef KEY /* Mac port */
#include "dwarf_stuff.h"
#endif /* KEY Mac port */

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
#include "cgtarget.h"
#include "data_layout.h"
#include "bb.h"
#include "op.h"
#include "iface_scn.h"
#include "cg_flags.h"
#include "glob.h"
#include "sections.h"
#include "targ_isa_print.h"
#include "config_debug.h"

// Holds name of function used to retrieve the Instruction Pointer.
extern const char * ip_calc_funcname;

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
#if defined(BUILD_OS_DARWIN)
  /* .file takes a single string arg in v1.38 as */
  fprintf (Asm_File, "\t%s\t\"%s/%s\"\n", AS_FILE, pathname, filename);
#else /* defined(BUILD_OS_DARWIN) */
  if (CG_emit_non_gas_syntax)
    fprintf (Asm_File, "\t%s\t%d\t\"%s/%s\"\n",
	     AS_FILE, USRCPOS_filenum(usrcpos)-1, pathname, filename);
  else fprintf (Asm_File, "\t%s\t%d\t\"%s/%s\"\n",
	   AS_FILE, USRCPOS_filenum(usrcpos), pathname, filename);
#endif /* defined(BUILD_OS_DARWIN) */
}

extern void
CGEMIT_Prn_Line_Dir_In_Asm (USRCPOS usrcpos)
{
  if( !CG_emit_asm_dwarf) { 
    fprintf (Asm_File, " # "); //turn the rest into comment
  }
#if defined(BUILD_OS_DARWIN)
  /* Darwin as provides .line, not .loc */
  fprintf (Asm_File, "\t.line\t%d\n", 
	   USRCPOS_linenum(usrcpos));
#else /* defined(BUILD_OS_DARWIN) */
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
#endif /* defined(BUILD_OS_DARWIN) */
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
  if ((cur_scn_name != NULL) && !strcmp(cur_scn_name, ".literal"))
  {

    /* If we added a prefix to the literal section, then end the
       prefix region */
    if (Non_Default_Text_Section(current_pu))
      fprintf (asm_file, "\t.end\tliteral_prefix\n");
  }
#if defined(BUILD_OS_DARWIN)
  /* Darwin uses ".lcomm", not ".bss" */
  else if (0 == strcmp(scn_name, BSS_RAW_NAME)) {
    return;
  }
#endif /* defined(BUILD_OS_DARWIN) */
  
  /* Handle .text, .data, and .literal specially. */

  if (!strcmp(scn_name, ".data") || !strcmp(scn_name, ".text"))
  {
    fprintf (asm_file, "\n\t%s", scn_name);
  }
  else if (!strcmp(scn_name, ".literal"))
  {
    /* If we need to add a prefix to the literal section, then emit
       the correct cabbage for that to happen. */
    if (Non_Default_Text_Section(current_pu))
    {
      CGEMIT_Prn_Scn_In_Asm(ST_base(current_pu), NULL);
      fprintf (asm_file, "\t.begin\tliteral_prefix %s\n",
	       ST_name(ST_base(current_pu)));
    }
    else
    {
      fprintf (asm_file, "\n\t.text\n");
    }
    fprintf (asm_file, "\t.literal_position\n");
  }
  else
  {
    char scn_flags_string[5];
    char *p = &scn_flags_string[0];
    
#if defined(BUILD_OS_DARWIN)
    fprintf (asm_file, "\n\t%s %s", AS_SECTION, map_section_name(scn_name));
#else /* defined(BUILD_OS_DARWIN) */
    fprintf (asm_file, "\n\t%s %s", AS_SECTION, scn_name);
#endif /* defined(BUILD_OS_DARWIN) */
    if (CG_emit_non_gas_syntax && strcmp(scn_name, ".srdata") == 0) {
      static BOOL printed = FALSE;
      if (!printed) {
	fprintf(asm_file, ", %d, %#x, %lld, ", 
		scn_type, scn_flags, scn_entsize);
	int tmp1 = 1, tmp2 = scn_align;
	for (; tmp2 >= 1; tmp1 *= 2, tmp2 --);
	fprintf(asm_file, "%d", tmp1);
	printed = TRUE;
      }
    }
#if ! defined(BUILD_OS_DARWIN)
    /* Mach-O assembler section directives don't use this syntax */
    if (! CG_emit_non_gas_syntax) {
      if ((scn_flags & SHF_WRITE) &&
          !(scn_name && !strncmp(scn_name,".gnu.linkonce.r.",16)))
        *p++ = 'w';
      if (scn_flags & SHF_ALLOC) *p++ = 'a';
      if (scn_flags & SHF_EXECINSTR) *p++ = 'x';
#ifdef KEY
      if (scn_flags & SHF_TLS) *p++ = 'T';
#endif
      *p = '\0'; // null terminate the string.
      fprintf (asm_file, ", \"%s\"", scn_flags_string);
      if (scn_type == SHT_PROGBITS)
        fprintf (asm_file, ",@progbits");
      else if (scn_type == SHT_NOBITS)
        fprintf (asm_file, ",@nobits");
    }
#endif /* defined(BUILD_OS_DARWIN) */
#ifdef TARG_X8664
#if ! defined(BUILD_OS_DARWIN)
    /* Mach-O linker rejects non-local labels in debug sections, but we are
     * already generating a local label for each section elsewhere. */
    if (strcmp(scn_name, DEBUG_FRAME_SECTNAME) == 0) // bug 2463
      fprintf(asm_file, "\n.LCIE:");
#endif /* defined(BUILD_OS_DARWIN) */

    // Generate a label at the start of the .eh_frame CIE
    if (!strcmp (scn_name, EH_FRAME_SECTNAME)) // bug 2729
      fprintf (asm_file, "\n.LEHCIE:");
#endif
  }

  fprintf (asm_file, "\n");

  /* For most sections, we only emit the alignment the first time we
     see it (in cgemit.cxx:Init_Section), because .org is used to
     place/align all data items relative to the start of the
     section. But some we always align. */

  if (!CGEMIT_Align_Section_Once(scn_name))
    fprintf (asm_file, "\t%s\t%d\n", AS_ALIGN, scn_align);
#if defined(BUILD_OS_DARWIN)
  /* Darwin "as" doesn't automatically define the section name to be a symbol
   * whose value is the origin of the section, so we must do it explicitly */
  fprintf(asm_file, "%s:\n", scn_name);
#endif /* defined(BUILD_OS_DARWIN) */
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
      fprintf (Asm_File, "\t%s 0x%llx\n", ".origin", offset);
    else fprintf (Asm_File, "\t%s 0x%llx\n", AS_ORIGIN, offset);
    fprintf ( Asm_File, "\t%s\t0\n", AS_ALIGN );
  }
}


// whether to use the base st for the reloc
extern BOOL
CGEMIT_Use_Base_ST_For_Reloc (INT reloc, ST *st)
{
	if (reloc == TN_RELOC_IA_LTOFF_FPTR) 
		// gas doesn't like addends
		return FALSE;
	// to handle function pointers.
	// example: see gcc.c-torture/execute/func-ptr-1.c
	else if (ST_sclass(st) == SCLASS_TEXT)
	        return FALSE;
#ifdef KEY
	else if (ST_is_thread_local(st))
	        return FALSE;
#endif
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
	case TN_RELOC_X8664_64:
	  break;
	case TN_RELOC_X8664_PC32:
	  *buf = vstr_concat (*buf, ST_name(st));
	  return 0;
	case TN_RELOC_X8664_32:
	  *buf = vstr_concat (*buf, "$");
	  break;
	case TN_RELOC_IA32_GOT:
	  *buf = vstr_concat (*buf, ST_name(st));
	  *buf = vstr_concat (*buf, "@GOT");
	  return 0;
	case TN_RELOC_IA32_GOTOFF:
	  *buf = vstr_concat (*buf, ST_name(st));
	  *buf = vstr_concat (*buf, "@GOTOFF");
	  return 0;
	case TN_RELOC_X8664_GOTPCREL:
	  *buf = vstr_concat (*buf, ST_name(st));
	  *buf = vstr_concat (*buf, "@GOTPCREL");
	  return 0;
	case TN_RELOC_IA32_GLOBAL_OFFSET_TABLE:
	  {
	    char* str = NULL;
	    if (Is_Target_EM64T()    ||
                Is_Target_Wolfdale() ||
		Is_Target_Core())
	      asprintf( &str, "$_GLOBAL_OFFSET_TABLE_+[.-%s]", ST_name(st) );
	    else
	      asprintf( &str, "$_GLOBAL_OFFSET_TABLE_" );
	    *buf = vstr_concat( *buf, str );
	    free( str );
	  }
	  return 0;
	case TN_RELOC_X8664_TPOFF32_seg_reg:
	  *buf = vstr_concat(*buf, Is_Target_32bit() ? "%gs:" : "%fs:");
	  // fall through
	case TN_RELOC_X8664_TPOFF32:
	  *buf = vstr_concat(*buf, ST_name(st));
	  *buf = vstr_concat(*buf, Is_Target_32bit() ? "@NTPOFF" : "@TPOFF");
	  return 0;
	case TN_RELOC_X8664_GOTTPOFF:
	  *buf = vstr_concat(*buf, ST_name(st));
	  *buf = vstr_concat(*buf,
			     Is_Target_32bit() ? "@INDNTPOFF" : "@GOTTPOFF");
	  return 0;
        case TN_RELOC_X8664_GOTNTPOFF:
          *buf = vstr_concat(*buf, ST_name(st));
          *buf = vstr_concat(*buf, "@GOTNTPOFF");
          return 0;
        case TN_RELOC_X8664_DTPOFF:
          *buf = vstr_concat(*buf, ST_name(st));
          *buf = vstr_concat(*buf, "@DTPOFF");
          return 0;
        case TN_RELOC_X8664_TLSGD:
          *buf = vstr_concat(*buf, ST_name(st));
          *buf = vstr_concat(*buf, "@TLSGD");
          return 0;
        case TN_RELOC_X8664_TLSLD:
          *buf = vstr_concat(*buf, ST_name(st));
          *buf = vstr_concat(*buf, Is_Target_32bit() ? "@TLSLDM" : "@TLSLD");
          return 0;
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

      	if (EMIT_interface_section) {
		Interface_Scn_Add_Call( call_sym, 
			CALLINFO_call_wn(ANNOT_callinfo(ant)));
      	}
}


/* Generate the .frame, .mask and the .fmask directives for the assembler. */
void
CGEMIT_Gen_Asm_Frame (INT64 frame_len)
{
  if (CG_inhibit_size_directive)
    return;
  TN *tn = ((Current_PU_Stack_Model == SMODEL_SMALL) ? SP_TN : FP_TN);
  ISA_REGISTER_CLASS rc = TN_register_class(tn);
  REGISTER reg = TN_register(tn);
  fprintf ( Asm_File, "\t%s\t%s, %lld, %s\n",
	    AS_FRAME,
	    ABI_PROPERTY_Reg_Name(rc, REGISTER_machine_id(rc, reg)),
	    frame_len,
	    ABI_PROPERTY_Reg_Name(rc, REGISTER_machine_id(rc, reg)));
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
  CGEMIT_Alias (sym, strongsym);
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
    fprintf(Asm_File, ", %lld\n", val);
  else
    fprintf(Asm_File, ", %#llx\n", val);
  
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
  fprintf(Asm_File, ", %lld\n", u.u);
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
      fprintf (Asm_File, " %+lld\n", sym_ofst);
  }
}

void CGEMIT_Alias (ST *sym, ST *strongsym) 
{
  // bug 14491: alias statement should write out the qualified name
  fprintf ( Asm_File, "\t%s = %s", ST_name(sym), ST_name(strongsym));
  if (ST_is_export_local(strongsym) && ST_class(strongsym) == CLASS_VAR) {
    // modelled after EMT_Write_Qualified_Name (bug 6899)
    if (ST_level(strongsym) == GLOBAL_SYMTAB) {
      // bug 14517, OSP 490
      if (Emit_Global_Data || ST_sclass(strongsym) == SCLASS_PSTATIC)
        fprintf ( Asm_File, "%s%d", Label_Name_Separator, ST_index(strongsym));
    }
    else
      fprintf ( Asm_File, "%s%d%s%d", Label_Name_Separator, 
		ST_pu(Get_Current_PU_ST()),
      		Label_Name_Separator, ST_index(strongsym));
  }
  fprintf ( Asm_File, "\n");
}


////////////////////////////////////////////////////////////////////////////////////////
//
//    Emit the instructions to the .s file.
//
////////////////////////////////////////////////////////////////////////////////////////

static const char* OP_Name[TOP_count];

static void Init_OP_Name()
{
  static BOOL bInit = FALSE;

  if( bInit )
    return;

  bInit = TRUE;
  bzero( OP_Name, sizeof(OP_Name) );

  // Only put in the name which is different from isa.cxx.

  OP_Name[TOP_reti]  = "ret";
  OP_Name[TOP_comixsd]  = "comisd";
  OP_Name[TOP_comixxsd] = "comisd";
  OP_Name[TOP_comixxxsd]= "comisd";
  OP_Name[TOP_comixss]  = "comiss";
  OP_Name[TOP_comixxss] = "comiss";
  OP_Name[TOP_comixxxss]= "comiss";
  OP_Name[TOP_add32] = "addl";
  OP_Name[TOP_adc32] = "adcl";
  OP_Name[TOP_add64] = "addq";
  OP_Name[TOP_addx32] = "addl";
  OP_Name[TOP_addx64] = "addq";
  OP_Name[TOP_addxx32]  = "addl";
  OP_Name[TOP_addxxx32] = "addl";
  OP_Name[TOP_addxx64]  = "addq";
  OP_Name[TOP_addxxx64] = "addq";
  OP_Name[TOP_addxss] = "addss";
  OP_Name[TOP_addxsd] = "addsd";
  OP_Name[TOP_addxxss]  = "addss";
  OP_Name[TOP_addxxxss] = "addss";
  OP_Name[TOP_addxxsd]  = "addsd";
  OP_Name[TOP_addxxxsd] = "addsd";
  OP_Name[TOP_add128v8] = "paddb";
  OP_Name[TOP_addx128v8] = "paddb";
  OP_Name[TOP_addxx128v8] = "paddb";
  OP_Name[TOP_addxxx128v8] = "paddb";
  OP_Name[TOP_mul128v16] = "pmullw";
  OP_Name[TOP_add128v16] = "paddw";
  OP_Name[TOP_addx128v16] = "paddw";
  OP_Name[TOP_addxx128v16] = "paddw";
  OP_Name[TOP_addxxx128v16] = "paddw";
  OP_Name[TOP_add128v32] = "paddd";
  OP_Name[TOP_addx128v32] = "paddd";
  OP_Name[TOP_addxx128v32] = "paddd";
  OP_Name[TOP_addxxx128v32] = "paddd";
  OP_Name[TOP_add128v64] = "paddq";
  OP_Name[TOP_addx128v64] = "paddq";
  OP_Name[TOP_addxx128v64] = "paddq";
  OP_Name[TOP_addxxx128v64] = "paddq";
  OP_Name[TOP_fadd128v32] = "addps";
  OP_Name[TOP_faddx128v32] = "addps";
  OP_Name[TOP_faddxx128v32] = "addps";
  OP_Name[TOP_faddxxx128v32] = "addps";
  OP_Name[TOP_fadd128v64] = "addpd";
  OP_Name[TOP_faddx128v64] = "addpd";
  OP_Name[TOP_faddxx128v64] = "addpd";
  OP_Name[TOP_faddxxx128v64] = "addpd";
  OP_Name[TOP_faddsub128v32] = "addsubps";
  OP_Name[TOP_faddsubx128v32] = "addsubps";
  OP_Name[TOP_faddsubxx128v32] = "addsubps";
  OP_Name[TOP_faddsubxxx128v32] = "addsubps";
  OP_Name[TOP_faddsub128v64] = "addsubpd";
  OP_Name[TOP_faddsubx128v64] = "addsubpd";
  OP_Name[TOP_faddsubxx128v64] = "addsubpd";
  OP_Name[TOP_faddsubxxx128v64] = "addsubpd";
  OP_Name[TOP_fhadd128v32] = "haddps";
  OP_Name[TOP_fhaddx128v32] = "haddps";
  OP_Name[TOP_fhaddxx128v32] = "haddps";
  OP_Name[TOP_fhaddxxx128v32] = "haddps";
  OP_Name[TOP_fhadd128v64] = "haddpd";
  OP_Name[TOP_fhaddx128v64] = "haddpd";
  OP_Name[TOP_fhaddxx128v64] = "haddpd";
  OP_Name[TOP_fhaddxxx128v64] = "haddpd";
  OP_Name[TOP_fsub128v32] = "hsubps";
  OP_Name[TOP_fsubx128v32] = "hsubps";
  OP_Name[TOP_fsubxx128v32] = "hsubps";
  OP_Name[TOP_fsubxxx128v32] = "hsubps";
  OP_Name[TOP_fsub128v64] = "hsubpd";
  OP_Name[TOP_fsubx128v64] = "hsubpd";
  OP_Name[TOP_fsubxx128v64] = "hsubpd";
  OP_Name[TOP_fsubxxx128v64] = "hsubpd";
  OP_Name[TOP_fmovsldup] = "movsldup";
  OP_Name[TOP_fmovshdup] = "movshdup";
  OP_Name[TOP_fmovddup] = "movddup";
  OP_Name[TOP_fmovsldupx] = "movsldup";
  OP_Name[TOP_fmovshdupx] = "movshdup";
  OP_Name[TOP_fmovddupx] = "movddup";
  OP_Name[TOP_fmovsldupxx] = "movsldup";
  OP_Name[TOP_fmovshdupxx] = "movshdup";
  OP_Name[TOP_fmovddupxx] = "movddup";
  OP_Name[TOP_fmovsldupxxx] = "movsldup";
  OP_Name[TOP_fmovshdupxxx] = "movshdup";
  OP_Name[TOP_fmovddupxxx] = "movddup";
  OP_Name[TOP_addi32] = "addl";
  OP_Name[TOP_adci32] = "adcl";
  OP_Name[TOP_addi64] = "addq";
  OP_Name[TOP_imuli32] = "imull";
  OP_Name[TOP_imuli64] = "imulq";
  OP_Name[TOP_and32] = "andl";
  OP_Name[TOP_andx32]  = "andl";
  OP_Name[TOP_andxx32] = "andl";
  OP_Name[TOP_andxxx32]= "andl";
  OP_Name[TOP_and64]   = "andq";
  OP_Name[TOP_andx64]  = "andq";
  OP_Name[TOP_andxx64] = "andq";
  OP_Name[TOP_andxxx64]= "andq";
  OP_Name[TOP_andi32] = "andl";
  OP_Name[TOP_andi64] = "andq";
  OP_Name[TOP_and128v8] = "pand";
  OP_Name[TOP_andx128v8] = "pand";
  OP_Name[TOP_andxx128v8] = "pand";
  OP_Name[TOP_andxxx128v8] = "pand";
  OP_Name[TOP_and128v16] = "pand";
  OP_Name[TOP_andx128v16] = "pand";
  OP_Name[TOP_andxx128v16] = "pand";
  OP_Name[TOP_andxxx128v16] = "pand";
  OP_Name[TOP_and128v32] = "pand";
  OP_Name[TOP_andx128v32] = "pand";
  OP_Name[TOP_andxx128v32] = "pand";
  OP_Name[TOP_andxxx128v32] = "pand";
  OP_Name[TOP_and128v64] = "pand";
  OP_Name[TOP_andx128v64] = "pand";
  OP_Name[TOP_andxx128v64] = "pand";
  OP_Name[TOP_andxxx128v64] = "pand";
  OP_Name[TOP_fand128v32] = "andps";
  OP_Name[TOP_fandx128v32] = "andps";
  OP_Name[TOP_fandxx128v32] = "andps";
  OP_Name[TOP_fandxxx128v32] = "andps";
  OP_Name[TOP_fand128v64] = "andpd";
  OP_Name[TOP_fandx128v64] = "andpd";
  OP_Name[TOP_fandxx128v64] = "andpd";
  OP_Name[TOP_fandxxx128v64] = "andpd";
  OP_Name[TOP_or128v8] = "por";
  OP_Name[TOP_orx128v8] = "por";
  OP_Name[TOP_orxx128v8] = "por";
  OP_Name[TOP_orxxx128v8] = "por";
  OP_Name[TOP_or128v16] = "por";
  OP_Name[TOP_orx128v16] = "por";
  OP_Name[TOP_orxx128v16] = "por";
  OP_Name[TOP_orxxx128v16] = "por";
  OP_Name[TOP_or128v32] = "por";
  OP_Name[TOP_orx128v32] = "por";
  OP_Name[TOP_orxx128v32] = "por";
  OP_Name[TOP_orxxx128v32] = "por";
  OP_Name[TOP_or128v64] = "por";
  OP_Name[TOP_orx128v64] = "por";
  OP_Name[TOP_orxx128v64] = "por";
  OP_Name[TOP_orxxx128v64] = "por";
  OP_Name[TOP_for128v32] = "orps";
  OP_Name[TOP_forx128v32] = "orps";
  OP_Name[TOP_forxx128v32] = "orps";
  OP_Name[TOP_forxxx128v32] = "orps";
  OP_Name[TOP_for128v64] = "orpd";
  OP_Name[TOP_forx128v64] = "orpd";
  OP_Name[TOP_forxx128v64] = "orpd";
  OP_Name[TOP_forxxx128v64] = "orpd";
  OP_Name[TOP_xor128v8] = "pxor";
  OP_Name[TOP_xorx128v8] = "pxor";
  OP_Name[TOP_xorxx128v8] = "pxor";
  OP_Name[TOP_xorxxx128v8] = "pxor";
  OP_Name[TOP_xor128v16] = "pxor";
  OP_Name[TOP_xorx128v16] = "pxor";
  OP_Name[TOP_xorxx128v16] = "pxor";
  OP_Name[TOP_xorxxx128v16] = "pxor";
  OP_Name[TOP_xor128v32] = "pxor";
  OP_Name[TOP_xorx128v32] = "pxor";
  OP_Name[TOP_xorxx128v32] = "pxor";
  OP_Name[TOP_xorxxx128v32] = "pxor";
  OP_Name[TOP_xor128v64] = "pxor";
  OP_Name[TOP_xorx128v64] = "pxor";
  OP_Name[TOP_xorxx128v64] = "pxor";
  OP_Name[TOP_xorxxx128v64] = "pxor";
  OP_Name[TOP_fxor128v32] = "xorps";
  OP_Name[TOP_fxorx128v32] = "xorps";
  OP_Name[TOP_fxorxx128v32] = "xorps";
  OP_Name[TOP_fxorxxx128v32] = "xorps";
  OP_Name[TOP_fxor128v64] = "xorps";
  OP_Name[TOP_fxorx128v64] = "xorps";
  OP_Name[TOP_fxorxx128v64] = "xorps";
  OP_Name[TOP_fxorxxx128v64] = "xorps";
  OP_Name[TOP_xorpd] = "xorps";
  OP_Name[TOP_fmax128v32] = "maxps";
  OP_Name[TOP_fmaxx128v32] = "maxps";
  OP_Name[TOP_fmaxxx128v32] = "maxps";
  OP_Name[TOP_fmaxxxx128v32] = "maxps";
  OP_Name[TOP_fmax128v64] = "maxpd";
  OP_Name[TOP_fmaxx128v64] = "maxpd";
  OP_Name[TOP_fmaxxx128v64] = "maxpd";
  OP_Name[TOP_fmaxxxx128v64] = "maxpd";
  OP_Name[TOP_fmin128v32] = "minps";
  OP_Name[TOP_fminx128v32] = "minps";
  OP_Name[TOP_fminxx128v32] = "minps";
  OP_Name[TOP_fminxxx128v32] = "minps";
  OP_Name[TOP_fmin128v64] = "minpd";
  OP_Name[TOP_fminx128v64] = "minpd";
  OP_Name[TOP_fminxx128v64] = "minpd";
  OP_Name[TOP_fminxxx128v64] = "minpd";
  OP_Name[TOP_fdiv128v32] = "divps";
  OP_Name[TOP_fdivx128v32] = "divps";
  OP_Name[TOP_fdivxx128v32] = "divps";
  OP_Name[TOP_fdivxxx128v32] = "divps";
  OP_Name[TOP_fdiv128v64] = "divpd";
  OP_Name[TOP_fdivx128v64] = "divpd";
  OP_Name[TOP_fdivxx128v64] = "divpd";
  OP_Name[TOP_fdivxxx128v64] = "divpd";
  OP_Name[TOP_fmul128v32] = "mulps";
  OP_Name[TOP_fmulx128v32] = "mulps";
  OP_Name[TOP_fmulxx128v32] = "mulps";
  OP_Name[TOP_fmulxxx128v32] = "mulps";
  OP_Name[TOP_fmul128v64] = "mulpd";
  OP_Name[TOP_fmulx128v64] = "mulpd";
  OP_Name[TOP_fmulxx128v64] = "mulpd";
  OP_Name[TOP_fmulxxx128v64] = "mulpd";
  OP_Name[TOP_frsqrt128v32] = "rsqrtps";
  OP_Name[TOP_fsqrt128v32] = "sqrtps";
  OP_Name[TOP_fsqrt128v64] = "sqrtpd";
  OP_Name[TOP_frcp128v32] = "rcpps";
  OP_Name[TOP_subus128v16] = "psubusw";
  OP_Name[TOP_cmpgt128v8] = "pcmpgtb";
  OP_Name[TOP_cmpgt128v16] = "pcmpgtw";
  OP_Name[TOP_cmpgt128v32] = "pcmpgtd";
  OP_Name[TOP_cmpeq128v8] = "pcmpeqb";
  OP_Name[TOP_cmpeq128v16] = "pcmpeqw";
  OP_Name[TOP_cmpeq128v32] = "pcmpeqd";
  OP_Name[TOP_cmpgtx128v8] = "pcmpgtb";
  OP_Name[TOP_cmpgtx128v16] = "pcmpgtw";
  OP_Name[TOP_cmpgtx128v32] = "pcmpgtd";
  OP_Name[TOP_cmpeqx128v8] = "pcmpeqb";
  OP_Name[TOP_cmpeqx128v16] = "pcmpeqw";
  OP_Name[TOP_cmpeqx128v32] = "pcmpeqd";
  OP_Name[TOP_cmpgtxx128v8] = "pcmpgtb";
  OP_Name[TOP_cmpgtxx128v16] = "pcmpgtw";
  OP_Name[TOP_cmpgtxx128v32] = "pcmpgtd";
  OP_Name[TOP_cmpeqxx128v8] = "pcmpeqb";
  OP_Name[TOP_cmpeqxx128v16] = "pcmpeqw";
  OP_Name[TOP_cmpeqxx128v32] = "pcmpeqd";
  OP_Name[TOP_cmpgtxxx128v8] = "pcmpgtb";
  OP_Name[TOP_cmpgtxxx128v16] = "pcmpgtw";
  OP_Name[TOP_cmpgtxxx128v32] = "pcmpgtd";
  OP_Name[TOP_cmpeqxxx128v8] = "pcmpeqb";
  OP_Name[TOP_cmpeqxxx128v16] = "pcmpeqw";
  OP_Name[TOP_cmpeqxxx128v32] = "pcmpeqd";
  OP_Name[TOP_paddsb128] = "paddsb";
  OP_Name[TOP_paddsw128] = "paddsw";
  OP_Name[TOP_paddq128] = "paddq";
  OP_Name[TOP_paddusb128] = "paddusb";
  OP_Name[TOP_paddusw128] = "paddusw";
  OP_Name[TOP_pavgb128] = "pavgb";
  OP_Name[TOP_pavgw128] = "pavgw";
  OP_Name[TOP_psubsb128] = "psubsb";
  OP_Name[TOP_psubsw128] = "psubsw";
  OP_Name[TOP_psubusb128] = "psubusb";
  OP_Name[TOP_psubusw128] = "psubusw";
  OP_Name[TOP_pmullw128] = "pmullw";
  OP_Name[TOP_pmulhw128] = "pmulhw";
  OP_Name[TOP_pmulhuw128] = "pmulhuw";
  OP_Name[TOP_pmuludq128] = "pmuludq";
  OP_Name[TOP_pmaddwd128] = "pmaddwd";
  OP_Name[TOP_psadbw128] = "psadbw";
  OP_Name[TOP_psllq_mmx] = "psllq";
  OP_Name[TOP_psrlq_mmx] = "psrlq";
  OP_Name[TOP_psllwi] = "psllw";
  OP_Name[TOP_pslldi] = "pslld";
  OP_Name[TOP_psllqi] = "psllq";
  OP_Name[TOP_psrlwi] = "psrlw";
  OP_Name[TOP_psrldi] = "psrld";
  OP_Name[TOP_psrlqi] = "psrlq";
  OP_Name[TOP_psrawi] = "psraw";
  OP_Name[TOP_psradi] = "psrad";
  OP_Name[TOP_maxu128v8] = "pmaxub";
  OP_Name[TOP_maxux128v8] = "pmaxub";
  OP_Name[TOP_maxuxx128v8] = "pmaxub";
  OP_Name[TOP_maxuxxx128v8] = "pmaxub";
  OP_Name[TOP_maxu128v16] = "pmaxuw";
  OP_Name[TOP_maxux128v16] = "pmaxuw";
  OP_Name[TOP_maxuxx128v16] = "pmaxuw";
  OP_Name[TOP_maxuxxx128v16] = "pmaxuw";
  OP_Name[TOP_maxu128v32] = "pmaxud";
  OP_Name[TOP_maxux128v32] = "pmaxud";
  OP_Name[TOP_maxuxx128v32] = "pmaxud";
  OP_Name[TOP_maxuxxx128v32] = "pmaxud";
  OP_Name[TOP_maxs128v8] = "pmaxsb";
  OP_Name[TOP_maxsx128v8] = "pmaxsb";
  OP_Name[TOP_maxsxx128v8] = "pmaxsb";
  OP_Name[TOP_maxsxxx128v8] = "pmaxsb";
  OP_Name[TOP_maxs128v16] = "pmaxsw";
  OP_Name[TOP_maxsx128v16] = "pmaxsw";
  OP_Name[TOP_maxsxx128v16] = "pmaxsw";
  OP_Name[TOP_maxsxxx128v16] = "pmaxsw";
  OP_Name[TOP_maxs128v32] = "pmaxsd";
  OP_Name[TOP_maxsx128v32] = "pmaxsd";
  OP_Name[TOP_maxsxx128v32] = "pmaxsd";
  OP_Name[TOP_maxsxxx128v32] = "pmaxsd";
  OP_Name[TOP_minu128v8] = "pminub";
  OP_Name[TOP_minux128v8] = "pminub";
  OP_Name[TOP_minuxx128v8] = "pminub";
  OP_Name[TOP_minuxxx128v8] = "pminub";
  OP_Name[TOP_minu128v16] = "pminuw";
  OP_Name[TOP_minux128v16] = "pminuw";
  OP_Name[TOP_minuxx128v16] = "pminuw";
  OP_Name[TOP_minuxxx128v16] = "pminuw";
  OP_Name[TOP_minu128v32] = "pminud";
  OP_Name[TOP_minux128v32] = "pminud";
  OP_Name[TOP_minuxx128v32] = "pminud";
  OP_Name[TOP_minuxxx128v32] = "pminud";
  OP_Name[TOP_mins128v8] = "pminsb";
  OP_Name[TOP_minsx128v8] = "pminsb";
  OP_Name[TOP_minsxx128v8] = "pminsb";
  OP_Name[TOP_minsxxx128v8] = "pminsb";
  OP_Name[TOP_mins128v16] = "pminsw";
  OP_Name[TOP_minsx128v16] = "pminsw";
  OP_Name[TOP_minsxx128v16] = "pminsw";
  OP_Name[TOP_minsxxx128v16] = "pminsw";
  OP_Name[TOP_mins128v32] = "pminsd";
  OP_Name[TOP_minsx128v32] = "pminsd";
  OP_Name[TOP_minsxx128v32] = "pminsd";
  OP_Name[TOP_minsxxx128v32] = "pminsd";
  OP_Name[TOP_punpcklbw128] = "punpcklbw",
  OP_Name[TOP_punpcklwd128] = "punpcklwd",
  OP_Name[TOP_punpckldq128] = "punpckldq";
  OP_Name[TOP_punpckhbw128] = "punpckhbw",
  OP_Name[TOP_punpckhwd128] = "punpckhwd",
  OP_Name[TOP_punpckhdq128] = "punpckhdq";
  OP_Name[TOP_packsswb128] = "packsswb",
  OP_Name[TOP_packssdw128] = "packssdw",
  OP_Name[TOP_packuswb128] = "packuswb";

  // SSSE3 TOP to instruction map
  OP_Name[TOP_psign128v8] = "psignb";
  OP_Name[TOP_psignx128v8] = "psignb";
  OP_Name[TOP_psignxx128v8] = "psignb";
  OP_Name[TOP_psignxxx128v8] = "psignb";
  OP_Name[TOP_psign128v16] = "psignw";
  OP_Name[TOP_psignx128v16] = "psignw";
  OP_Name[TOP_psignxx128v16] = "psignw";
  OP_Name[TOP_psignxxx128v16] = "psignw";
  OP_Name[TOP_psign128v32] = "psignd";
  OP_Name[TOP_psignx128v32] = "psignd";
  OP_Name[TOP_psignxx128v32] = "psignd";
  OP_Name[TOP_psignxxx128v32] = "psignd";
  OP_Name[TOP_pabs128v8] = "pabsb";
  OP_Name[TOP_pabsx128v8] = "pabsb";
  OP_Name[TOP_pabsxx128v8] = "pabsb";
  OP_Name[TOP_pabsxxx128v8] = "pabsb";
  OP_Name[TOP_pabs128v16] = "pabsw";
  OP_Name[TOP_pabsx128v16] = "pabsw";
  OP_Name[TOP_pabsxx128v16] = "pabsw";
  OP_Name[TOP_pabsxxx128v16] = "pabsw";
  OP_Name[TOP_pabs128v32] = "pabsd";
  OP_Name[TOP_pabsx128v32] = "pabsd";
  OP_Name[TOP_pabsxx128v32] = "pabsd";
  OP_Name[TOP_pabsxxx128v32] = "pabsd";
  OP_Name[TOP_palignr128] = "palignr";
  OP_Name[TOP_palignrx128] = "palignr";
  OP_Name[TOP_palignrxx128] = "palignr";
  OP_Name[TOP_palignrxxx128] = "palignr";
  OP_Name[TOP_pshuf128v8] = "pshufb";
  OP_Name[TOP_pshufx128v8] = "pshufb";
  OP_Name[TOP_pshufxx128v8] = "pshufb";
  OP_Name[TOP_pshufxxx128v8] = "pshufb";
  OP_Name[TOP_pmulhrsw128] = "pmulhrsw";
  OP_Name[TOP_pmulhrswx128] = "pmulhrsw";
  OP_Name[TOP_pmulhrswxx128] = "pmulhrsw";
  OP_Name[TOP_pmulhrswxxx128] = "pmulhrsw";
  OP_Name[TOP_pmaddubsw128] = "pmaddubsw";
  OP_Name[TOP_pmaddubswx128] = "pmaddubsw";
  OP_Name[TOP_pmaddubswxx128] = "pmaddubsw";
  OP_Name[TOP_pmaddubswxxx128] = "pmaddubsw";
  OP_Name[TOP_phsub128v16] = "phsubw";
  OP_Name[TOP_phsubx128v16] = "phsubw";
  OP_Name[TOP_phsubxx128v16] = "phsubw";
  OP_Name[TOP_phsubxxx128v16] = "phsubw";
  OP_Name[TOP_phsub128v32] = "phsubd";
  OP_Name[TOP_phsubx128v32] = "phsubd";
  OP_Name[TOP_phsubxx128v32] = "phsubd";
  OP_Name[TOP_phsubxxx128v32] = "phsubd";
  OP_Name[TOP_phsubs128v16] = "phsubsw";
  OP_Name[TOP_phsubsx128v16] = "phsubsw";
  OP_Name[TOP_phsubsxx128v16] = "phsubsw";
  OP_Name[TOP_phsubsxxx128v16] = "phsubsw";
  OP_Name[TOP_phadd128v16] = "phaddw";
  OP_Name[TOP_phaddx128v16] = "phaddw";
  OP_Name[TOP_phaddxx128v16] = "phaddw";
  OP_Name[TOP_phaddxxx128v16] = "phaddw";
  OP_Name[TOP_phadd128v32] = "phaddd";
  OP_Name[TOP_phaddx128v32] = "phaddd";
  OP_Name[TOP_phaddxx128v32] = "phaddd";
  OP_Name[TOP_phaddxxx128v32] = "phaddd";
  OP_Name[TOP_phadds128v16] = "phaddsw";
  OP_Name[TOP_phaddsx128v16] = "phaddsw";
  OP_Name[TOP_phaddsxx128v16] = "phaddsw";
  OP_Name[TOP_phaddsxxx128v16] = "phaddsw";

  // SSE4.1 TOP to instruction map
  OP_Name[TOP_mpsadbw] = "mpsadbw";
  OP_Name[TOP_mpsadbwx] = "mpsadbw";
  OP_Name[TOP_mpsadbwxx] = "mpsadbw";
  OP_Name[TOP_mpsadbwxxx] = "mpsadbw";
  OP_Name[TOP_muldq] = "pmuldq";
  OP_Name[TOP_muldqx] = "pmuldq";
  OP_Name[TOP_muldqxx] = "pmuldq";
  OP_Name[TOP_muldqxxx] = "pmuldq";
  OP_Name[TOP_mul128v32] = "pmulld";
  OP_Name[TOP_mulx128v32] = "pmulld";
  OP_Name[TOP_mulxx128v32] = "pmulld";
  OP_Name[TOP_mulxxx128v32] = "pmulld";
  OP_Name[TOP_fdp128v32] = "dpps";
  OP_Name[TOP_fdpx128v32] = "dpps";
  OP_Name[TOP_fdpxx128v32] = "dpps";
  OP_Name[TOP_fdpxxx128v32] = "dpps";
  OP_Name[TOP_fdp128v64] = "dppd";
  OP_Name[TOP_fdpx128v64] = "dppd";
  OP_Name[TOP_fdpxx128v64] = "dppd";
  OP_Name[TOP_fdpxxx128v64] = "dppd";
  OP_Name[TOP_fblend128v32] = "blendps";
  OP_Name[TOP_fblendx128v32] = "blendps";
  OP_Name[TOP_fblendxx128v32] = "blendps";
  OP_Name[TOP_fblendxxx128v32] = "blendps";
  OP_Name[TOP_fblend128v64] = "blendpd";
  OP_Name[TOP_fblendx128v64] = "blendpd";
  OP_Name[TOP_fblendxx128v64] = "blendpd";
  OP_Name[TOP_fblendxxx128v64] = "blendpd";
  OP_Name[TOP_fblendv128v32] = "blendvps";
  OP_Name[TOP_fblendvx128v32] = "blendvps";
  OP_Name[TOP_fblendvxx128v32] = "blendvps";
  OP_Name[TOP_fblendvxxx128v32] = "blendvps";
  OP_Name[TOP_fblendv128v64] = "blendvpd";
  OP_Name[TOP_fblendvx128v64] = "blendvpd";
  OP_Name[TOP_fblendvxx128v64] = "blendvpd";
  OP_Name[TOP_fblendvxxx128v64] = "blendvpd";
  OP_Name[TOP_blendv128v8] = "pblendvb";
  OP_Name[TOP_blendvx128v8] = "pblendvb";
  OP_Name[TOP_blendvxx128v8] = "pblendvb";
  OP_Name[TOP_blendvxxx128v8] = "pblendvb";
  OP_Name[TOP_blend128v16] = "pblendw";
  OP_Name[TOP_blendx128v16] = "pblendw";
  OP_Name[TOP_blendxx128v16] = "pblendw";
  OP_Name[TOP_blendxxx128v16] = "pblendw";
  OP_Name[TOP_round128v32] = "roundps";
  OP_Name[TOP_roundx128v32] = "roundps";
  OP_Name[TOP_roundxx128v32] = "roundps";
  OP_Name[TOP_roundxxx128v32] = "roundps";
  OP_Name[TOP_roundss] = "roundss";
  OP_Name[TOP_roundxss] = "roundss";
  OP_Name[TOP_roundxxss] = "roundss";
  OP_Name[TOP_roundxxxss] = "roundss";
  OP_Name[TOP_round128v64] = "roundpd";
  OP_Name[TOP_roundx128v64] = "roundpd";
  OP_Name[TOP_roundxx128v64] = "roundpd";
  OP_Name[TOP_roundxxx128v64] = "roundpd";
  OP_Name[TOP_roundsd] = "roundsd";
  OP_Name[TOP_roundxsd] = "roundsd";
  OP_Name[TOP_roundxxsd] = "roundsd";
  OP_Name[TOP_roundxxxsd] = "roundsd";
  OP_Name[TOP_finsr128v32] = "insertps";
  OP_Name[TOP_finsrx128v32] = "insertps";
  OP_Name[TOP_finsrxx128v32] = "insertps";
  OP_Name[TOP_finsrxxx128v32] = "insertps";
  OP_Name[TOP_insr128v8] = "pinsrb";
  OP_Name[TOP_insrx128v8] = "pinsrb";
  OP_Name[TOP_insrxx128v8] = "pinsrb";
  OP_Name[TOP_insrxxx128v8] = "pinsrb";
  OP_Name[TOP_insr128v16] = "pinsrw";
  OP_Name[TOP_insrx128v16] = "pinsrw";
  OP_Name[TOP_insrxx128v16] = "pinsrw";
  OP_Name[TOP_insrxxx128v16] = "pinsrw";
  OP_Name[TOP_insr128v32] = "pinsrd";
  OP_Name[TOP_insrx128v32] = "pinsrd";
  OP_Name[TOP_insrxx128v32] = "pinsrd";
  OP_Name[TOP_insrxxx128v32] = "pinsrd";
  OP_Name[TOP_insr128v64] = "pinsrq";
  OP_Name[TOP_insrx128v64] = "pinsrq";
  OP_Name[TOP_insrxx128v64] = "pinsrq";
  OP_Name[TOP_insrxxx128v64] = "pinsrq";
  OP_Name[TOP_fextr128v32] = "extractps";
  OP_Name[TOP_fextrx128v32] = "extractps";
  OP_Name[TOP_fextrxx128v32] = "extractps";
  OP_Name[TOP_fextrxxx128v32] = "extractps";
  OP_Name[TOP_extr128v8] = "pextrb";
  OP_Name[TOP_extrx128v8] = "pextrb";
  OP_Name[TOP_extrxx128v8] = "pextrb";
  OP_Name[TOP_extrxxx128v8] = "pextrb";
  OP_Name[TOP_extr128v16] = "pextrw";
  OP_Name[TOP_extrx128v16] = "pextrw";
  OP_Name[TOP_extrxx128v16] = "pextrw";
  OP_Name[TOP_extrxxx128v16] = "pextrw";
  OP_Name[TOP_extr128v32] = "pextrd";
  OP_Name[TOP_extrx128v32] = "pextrd";
  OP_Name[TOP_extrxx128v32] = "pextrd";
  OP_Name[TOP_extrxxx128v32] = "pextrd";
  OP_Name[TOP_extr128v64] = "pextrq";
  OP_Name[TOP_extrx128v64] = "pextrq";
  OP_Name[TOP_extrxx128v64] = "pextrq";
  OP_Name[TOP_extrxxx128v64] = "pextrq";
  OP_Name[TOP_pmovsxbw] = "pmovsxbw";
  OP_Name[TOP_pmovsxbwx] = "pmovsxbw";
  OP_Name[TOP_pmovsxbwxx] = "pmovsxbw";
  OP_Name[TOP_pmovsxbwxxx] = "pmovsxbw";
  OP_Name[TOP_pmovzxbw] = "pmovzxbw";
  OP_Name[TOP_pmovzxbwx] = "pmovzxbw";
  OP_Name[TOP_pmovzxbwxx] = "pmovzxbw";
  OP_Name[TOP_pmovzxbwxxx] = "pmovzxbw";
  OP_Name[TOP_pmovsxbd] = "pmovsxbd";
  OP_Name[TOP_pmovsxbdx] = "pmovsxbd";
  OP_Name[TOP_pmovsxbdxx] = "pmovsxbd";
  OP_Name[TOP_pmovsxbdxxx] = "pmovsxbd";
  OP_Name[TOP_pmovzxbd] = "pmovzxbd";
  OP_Name[TOP_pmovzxbdx] = "pmovzxbd";
  OP_Name[TOP_pmovzxbdxx] = "pmovzxbd";
  OP_Name[TOP_pmovzxbdxxx] = "pmovzxbd";
  OP_Name[TOP_pmovsxbq] = "pmovsxbq";
  OP_Name[TOP_pmovsxbqx] = "pmovsxbq";
  OP_Name[TOP_pmovsxbqxx] = "pmovsxbq";
  OP_Name[TOP_pmovsxbqxxx] = "pmovsxbq";
  OP_Name[TOP_pmovzxbq] = "pmovzxbq";
  OP_Name[TOP_pmovzxbqx] = "pmovzxbq";
  OP_Name[TOP_pmovzxbqxx] = "pmovzxbq";
  OP_Name[TOP_pmovzxbqxxx] = "pmovzxbq";
  OP_Name[TOP_pmovsxwd] = "pmovsxwd";
  OP_Name[TOP_pmovsxwdx] = "pmovsxwd";
  OP_Name[TOP_pmovsxwdxx] = "pmovsxwd";
  OP_Name[TOP_pmovsxwdxxx] = "pmovsxwd";
  OP_Name[TOP_pmovzxwd] = "pmovzxwd";
  OP_Name[TOP_pmovzxwdx] = "pmovzxwd";
  OP_Name[TOP_pmovzxwdxx] = "pmovzxwd";
  OP_Name[TOP_pmovzxwdxxx] = "pmovzxwd";
  OP_Name[TOP_pmovsxwq] = "pmovsxwq";
  OP_Name[TOP_pmovsxwqx] = "pmovsxwq";
  OP_Name[TOP_pmovsxwqxx] = "pmovsxwq";
  OP_Name[TOP_pmovsxwqxxx] = "pmovsxwq";
  OP_Name[TOP_pmovzxwq] = "pmovzxwq";
  OP_Name[TOP_pmovzxwqx] = "pmovzxwq";
  OP_Name[TOP_pmovzxwqxx] = "pmovzxwq";
  OP_Name[TOP_pmovzxwqxxx] = "pmovzxwq";
  OP_Name[TOP_pmovsxdq] = "pmovsxdq";
  OP_Name[TOP_pmovsxdqx] = "pmovsxdq";
  OP_Name[TOP_pmovsxdqxx] = "pmovsxdq";
  OP_Name[TOP_pmovsxdqxxx] = "pmovsxdq";
  OP_Name[TOP_pmovzxdq] = "pmovzxdq";
  OP_Name[TOP_pmovzxdqx] = "pmovzxdq";
  OP_Name[TOP_pmovzxdqxx] = "pmovzxdq";
  OP_Name[TOP_pmovzxdqxxx] = "pmovzxdq";
  OP_Name[TOP_ptest128] = "ptest";
  OP_Name[TOP_ptestx128] = "ptest";
  OP_Name[TOP_ptestxx128] = "ptest";
  OP_Name[TOP_ptestxxx128] = "ptest";
  OP_Name[TOP_cmpeq128v64] = "pcmpeqq";
  OP_Name[TOP_cmpeqx128v64] = "pcmpeqq";
  OP_Name[TOP_cmpeqxx128v64] = "pcmpeqq";
  OP_Name[TOP_cmpeqxxx128v64] = "pcmpeqq";
  OP_Name[TOP_packusdw] = "packusdw";
  OP_Name[TOP_packusdwx] = "packusdw";
  OP_Name[TOP_packusdwxx] = "packusdw";
  OP_Name[TOP_packusdwxxx] = "packusdw";
  OP_Name[TOP_ldntdqa] = "movntdqa";
  OP_Name[TOP_ldntdqax] = "movntdqa";
  OP_Name[TOP_ldntdqaxx] = "movntdqa";
  OP_Name[TOP_stntdq] = "movntdq";
  OP_Name[TOP_stntdqx] = "movntdq";
  OP_Name[TOP_stntdqxx] = "movntdq";

  // SSE4.2 TOP to instruction map
  OP_Name[TOP_crc32b] = "crc32b";
  OP_Name[TOP_crc32bx] = "crc32b";
  OP_Name[TOP_crc32bxx] = "crc32b";
  OP_Name[TOP_crc32bxxx] = "crc32b";
  OP_Name[TOP_crc32w] = "crc32w";
  OP_Name[TOP_crc32wx] = "crc32w";
  OP_Name[TOP_crc32wxx] = "crc32w";
  OP_Name[TOP_crc32wxxx] = "crc32w";
  OP_Name[TOP_crc32d] = "crc32d";
  OP_Name[TOP_crc32dx] = "crc32d";
  OP_Name[TOP_crc32dxx] = "crc32d";
  OP_Name[TOP_crc32dxxx] = "crc32d";
  OP_Name[TOP_crc32q] = "crc32q";
  OP_Name[TOP_crc32qx] = "crc32q";
  OP_Name[TOP_crc32qxx] = "crc32q";
  OP_Name[TOP_crc32qxxx] = "crc32q";
  OP_Name[TOP_cmpestri] = "pcmpestri";
  OP_Name[TOP_cmpestrix] = "pcmpestri";
  OP_Name[TOP_cmpestrixx] = "pcmpestri";
  OP_Name[TOP_cmpestrixxx] = "pcmpestri";
  OP_Name[TOP_cmpestrm] = "pcmpestrm";
  OP_Name[TOP_cmpestrmx] = "pcmpestrm";
  OP_Name[TOP_cmpestrmxx] = "pcmpestrm";
  OP_Name[TOP_cmpestrmxxx] = "pcmpestrm";
  OP_Name[TOP_cmpistri] = "pcmpistri";
  OP_Name[TOP_cmpistrix] = "pcmpistri";
  OP_Name[TOP_cmpistrixx] = "pcmpistri";
  OP_Name[TOP_cmpistrixxx] = "pcmpistri";
  OP_Name[TOP_cmpistrm] = "pcmpistrm";
  OP_Name[TOP_cmpistrmx] = "pcmpistrm";
  OP_Name[TOP_cmpistrmxx] = "pcmpistrm";
  OP_Name[TOP_cmpistrmxxx] = "pcmpistrm";
  OP_Name[TOP_cmpgt128v64] = "pcmpgtq";
  OP_Name[TOP_cmpgtx128v64] = "pcmpgtq";
  OP_Name[TOP_cmpgtxx128v64] = "pcmpgtq";
  OP_Name[TOP_cmpgtxxx128v64] = "pcmpgtq";
  OP_Name[TOP_popcnt16] = "popcnt";
  OP_Name[TOP_popcntx16] = "popcnt";
  OP_Name[TOP_popcntxx16] = "popcnt";
  OP_Name[TOP_popcntxxx16] = "popcnt";
  OP_Name[TOP_popcnt32] = "popcnt";
  OP_Name[TOP_popcntx32] = "popcnt";
  OP_Name[TOP_popcntxx32] = "popcnt";
  OP_Name[TOP_popcntxxx32] = "popcnt";
  OP_Name[TOP_popcnt64] = "popcnt";
  OP_Name[TOP_popcntx64] = "popcnt";
  OP_Name[TOP_popcntxx64] = "popcnt";
  OP_Name[TOP_popcntxxx64] = "popcnt";

  // AES/PCMUL TOP to instruction map
  OP_Name[TOP_aesenc] = "aesenc";
  OP_Name[TOP_aesencx] = "aesenc";
  OP_Name[TOP_aesencxx] = "aesenc";
  OP_Name[TOP_aesencxxx] = "aesenc";
  OP_Name[TOP_aesenclast] = "aesenclast";
  OP_Name[TOP_aesenclastx] = "aesenclast";
  OP_Name[TOP_aesenclastxx] = "aesenclast";
  OP_Name[TOP_aesenclastxxx] = "aesenclast";
  OP_Name[TOP_aesdec] = "aesdec";
  OP_Name[TOP_aesdecx] = "aesdec";
  OP_Name[TOP_aesdecxx] = "aesdec";
  OP_Name[TOP_aesdecxxx] = "aesdec";
  OP_Name[TOP_aesdeclast] = "aesdeclast";
  OP_Name[TOP_aesdeclastx] = "aesdeclast";
  OP_Name[TOP_aesdeclastxx] = "aesdeclast";
  OP_Name[TOP_aesdeclastxxx] = "aesdeclast";
  OP_Name[TOP_aeskeygenassist] = "aeskeygenassist";
  OP_Name[TOP_aeskeygenassistx] = "aeskeygenassist";
  OP_Name[TOP_aeskeygenassistxx] = "aeskeygenassist";
  OP_Name[TOP_aeskeygenassistxxx] = "aeskeygenassist";
  OP_Name[TOP_pclmulqdq] = "pclmulqdq";
  OP_Name[TOP_pclmulqdqx] = "pclmulqdq";
  OP_Name[TOP_pclmulqdqxx] = "pclmulqdq";
  OP_Name[TOP_pclmulqdqxxx] = "pclmulqdq";
  OP_Name[TOP_aesimc] = "aesimc";
  OP_Name[TOP_aesimcx] = "aesimc";
  OP_Name[TOP_aesimcxx] = "aesimc";
  OP_Name[TOP_aesimcxxx] = "aesimc";

  // XOP AVX TOP to instruction map
  OP_Name[TOP_vfrczpd] = "vfrczpd";
  OP_Name[TOP_vfrczpdx] = "vfrczpd";
  OP_Name[TOP_vfrczpdxx] = "vfrczpd";
  OP_Name[TOP_vfrczpdxxx] = "vfrczpd";
  OP_Name[TOP_vfrczps] = "vfrczps";
  OP_Name[TOP_vfrczpsx] = "vfrczps";
  OP_Name[TOP_vfrczpsxx] = "vfrczps";
  OP_Name[TOP_vfrczpsxxx] = "vfrczps";
  OP_Name[TOP_vfrczsd] = "vfrczsd";
  OP_Name[TOP_vfrczsdx] = "vfrczsd";
  OP_Name[TOP_vfrczsdxx] = "vfrczsd";
  OP_Name[TOP_vfrczsdxxx] = "vfrczsd";
  OP_Name[TOP_vfrczss] = "vfrczss";
  OP_Name[TOP_vfrczssx] = "vfrczss";
  OP_Name[TOP_vfrczssxx] = "vfrczss";
  OP_Name[TOP_vfrczssxxx] = "vfrczss";
  OP_Name[TOP_vpcmov] = "vpcmov";
  OP_Name[TOP_vpcmovx] = "vpcmov";
  OP_Name[TOP_vpcmovxx] = "vpcmov";
  OP_Name[TOP_vpcmovxxx] = "vpcmov";
  OP_Name[TOP_vpcmovxr] = "vpcmov";
  OP_Name[TOP_vpcmovxxr] = "vpcmov";
  OP_Name[TOP_vpcmovxxxr] = "vpcmov";
  OP_Name[TOP_vpcomb] = "vpcomb";
  OP_Name[TOP_vpcombx] = "vpcomb";
  OP_Name[TOP_vpcombxx] = "vpcomb";
  OP_Name[TOP_vpcombxxx] = "vpcomb";
  OP_Name[TOP_vpcomd] = "vpcomd";
  OP_Name[TOP_vpcomdx] = "vpcomd";
  OP_Name[TOP_vpcomdxx] = "vpcomd";
  OP_Name[TOP_vpcomdxxx] = "vpcomd";
  OP_Name[TOP_vpcomq] = "vpcomq";
  OP_Name[TOP_vpcomqx] = "vpcomq";
  OP_Name[TOP_vpcomqxx] = "vpcomq";
  OP_Name[TOP_vpcomqxxx] = "vpcomq";
  OP_Name[TOP_vpcomw] = "vpcomw";
  OP_Name[TOP_vpcomwx] = "vpcomw";
  OP_Name[TOP_vpcomwxx] = "vpcomw";
  OP_Name[TOP_vpcomwxxx] = "vpcomw";
  OP_Name[TOP_vpcomub] = "vpcomub";
  OP_Name[TOP_vpcomubx] = "vpcomub";
  OP_Name[TOP_vpcomubxx] = "vpcomub";
  OP_Name[TOP_vpcomubxxx] = "vpcomub";
  OP_Name[TOP_vpcomud] = "vpcomud";
  OP_Name[TOP_vpcomudx] = "vpcomud";
  OP_Name[TOP_vpcomudxx] = "vpcomud";
  OP_Name[TOP_vpcomudxxx] = "vpcomud";
  OP_Name[TOP_vpcomuq] = "vpcomuq";
  OP_Name[TOP_vpcomuqx] = "vpcomuq";
  OP_Name[TOP_vpcomuqxx] = "vpcomuq";
  OP_Name[TOP_vpcomuqxxx] = "vpcomuq";
  OP_Name[TOP_vpcomuw] = "vpcomuw";
  OP_Name[TOP_vpcomuwx] = "vpcomuw";
  OP_Name[TOP_vpcomuwxx] = "vpcomuw";
  OP_Name[TOP_vpcomuwxxx] = "vpcomuw";
  OP_Name[TOP_vphaddbd] = "vphaddbd";
  OP_Name[TOP_vphaddbdx] = "vphaddbd";
  OP_Name[TOP_vphaddbdxx] = "vphaddbd";
  OP_Name[TOP_vphaddbdxxx] = "vphaddbd";
  OP_Name[TOP_vphaddbq] = "vphaddbq";
  OP_Name[TOP_vphaddbqx] = "vphaddbq";
  OP_Name[TOP_vphaddbqxx] = "vphaddbq";
  OP_Name[TOP_vphaddbqxxx] = "vphaddbq";
  OP_Name[TOP_vphaddbw] = "vphaddbw";
  OP_Name[TOP_vphaddbwx] = "vphaddbw";
  OP_Name[TOP_vphaddbwxx] = "vphaddbw";
  OP_Name[TOP_vphaddbwxxx] = "vphaddbw";
  OP_Name[TOP_vphadddq] = "vphadddq";
  OP_Name[TOP_vphadddqx] = "vphadddq";
  OP_Name[TOP_vphadddqxx] = "vphadddq";
  OP_Name[TOP_vphadddqxxx] = "vphadddq";
  OP_Name[TOP_vphaddubd] = "vphaddubd";
  OP_Name[TOP_vphaddubdx] = "vphaddubd";
  OP_Name[TOP_vphaddubdxx] = "vphaddubd";
  OP_Name[TOP_vphaddubdxxx] = "vphaddubd";
  OP_Name[TOP_vphaddubq] = "vphaddubq";
  OP_Name[TOP_vphaddubqx] = "vphaddubq";
  OP_Name[TOP_vphaddubqxx] = "vphaddubq";
  OP_Name[TOP_vphaddubqxxx] = "vphaddubq";
  OP_Name[TOP_vphaddubw] = "vphaddubw";
  OP_Name[TOP_vphaddubwx] = "vphaddubw";
  OP_Name[TOP_vphaddubwxx] = "vphaddubw";
  OP_Name[TOP_vphaddubwxxx] = "vphaddubw";
  OP_Name[TOP_vphaddudq] = "vphaddudq";
  OP_Name[TOP_vphaddudqx] = "vphaddudq";
  OP_Name[TOP_vphaddudqxx] = "vphaddudq";
  OP_Name[TOP_vphaddudqxxx] = "vphaddudq";
  OP_Name[TOP_vphadduwd] = "vphadduwd";
  OP_Name[TOP_vphadduwdx] = "vphadduwd";
  OP_Name[TOP_vphadduwdxx] = "vphadduwd";
  OP_Name[TOP_vphadduwdxxx] = "vphadduwd";
  OP_Name[TOP_vphadduwq] = "vphadduwq";
  OP_Name[TOP_vphadduwqx] = "vphadduwq";
  OP_Name[TOP_vphadduwqxx] = "vphadduwq";
  OP_Name[TOP_vphadduwqxxx] = "vphadduwq";
  OP_Name[TOP_vphaddwd] = "vphaddwd";
  OP_Name[TOP_vphaddwdx] = "vphaddwd";
  OP_Name[TOP_vphaddwdxx] = "vphaddwd";
  OP_Name[TOP_vphaddwdxxx] = "vphaddwd";
  OP_Name[TOP_vphaddwq] = "vphaddwq";
  OP_Name[TOP_vphaddwqx] = "vphaddwq";
  OP_Name[TOP_vphaddwqxx] = "vphaddwq";
  OP_Name[TOP_vphaddwqxxx] = "vphaddwq";
  OP_Name[TOP_vphsubbw] = "vphsubbw";
  OP_Name[TOP_vphsubbwx] = "vphsubbw";
  OP_Name[TOP_vphsubbwxx] = "vphsubbw";
  OP_Name[TOP_vphsubbwxxx] = "vphsubbw";
  OP_Name[TOP_vphsubdq] = "vphsubdq";
  OP_Name[TOP_vphsubdqx] = "vphsubdq";
  OP_Name[TOP_vphsubdqxx] = "vphsubdq";
  OP_Name[TOP_vphsubdqxxx] = "vphsubdq";
  OP_Name[TOP_vphsubwd] = "vphsubwd";
  OP_Name[TOP_vphsubwdx] = "vphsubwd";
  OP_Name[TOP_vphsubwdxx] = "vphsubwd";
  OP_Name[TOP_vphsubwdxxx] = "vphsubwd";
  OP_Name[TOP_vpmacsdd] = "vpmacsdd";
  OP_Name[TOP_vpmacsddx] = "vpmacsdd";
  OP_Name[TOP_vpmacsddxx] = "vpmacsdd";
  OP_Name[TOP_vpmacsddxxx] = "vpmacsdd";
  OP_Name[TOP_vpmacssdd] = "vpmacssdd";
  OP_Name[TOP_vpmacssddx] = "vpmacssdd";
  OP_Name[TOP_vpmacssddxx] = "vpmacssdd";
  OP_Name[TOP_vpmacssddxxx] = "vpmacssdd";
  OP_Name[TOP_vpmacsdqh] = "vpmacsdqh";
  OP_Name[TOP_vpmacsdqhx] = "vpmacsdqh";
  OP_Name[TOP_vpmacsdqhxx] = "vpmacsdqh";
  OP_Name[TOP_vpmacsdqhxxx] = "vpmacsdqh";
  OP_Name[TOP_vpmacsdql] = "vpmacsdql";
  OP_Name[TOP_vpmacsdqlx] = "vpmacsdql";
  OP_Name[TOP_vpmacsdqlxx] = "vpmacsdql";
  OP_Name[TOP_vpmacsdqlxxx] = "vpmacsdql";
  OP_Name[TOP_vpmacsdd] = "vpmacssdd";
  OP_Name[TOP_vpmacsddx] = "vpmacssdd";
  OP_Name[TOP_vpmacsddxx] = "vpmacssdd";
  OP_Name[TOP_vpmacsddxxx] = "vpmacssdd";
  OP_Name[TOP_vpmacssdqh] = "vpmacssdqh";
  OP_Name[TOP_vpmacssdqhx] = "vpmacssdqh";
  OP_Name[TOP_vpmacssdqhxx] = "vpmacssdqh";
  OP_Name[TOP_vpmacssdqhxxx] = "vpmacssdqh";
  OP_Name[TOP_vpmacssdql] = "vpmacssdql";
  OP_Name[TOP_vpmacssdqlx] = "vpmacssdql";
  OP_Name[TOP_vpmacssdqlxx] = "vpmacssdql";
  OP_Name[TOP_vpmacssdqlxxx] = "vpmacssdql";
  OP_Name[TOP_vpmacsswd] = "vpmacsswd";
  OP_Name[TOP_vpmacsswdx] = "vpmacsswd";
  OP_Name[TOP_vpmacsswdxx] = "vpmacsswd";
  OP_Name[TOP_vpmacsswdxxx] = "vpmacsswd";
  OP_Name[TOP_vpmacssww] = "vpmacssww";
  OP_Name[TOP_vpmacsswwx] = "vpmacssww";
  OP_Name[TOP_vpmacsswwxx] = "vpmacssww";
  OP_Name[TOP_vpmacsswwxxx] = "vpmacssww";
  OP_Name[TOP_vpmacswd] = "vpmacswd";
  OP_Name[TOP_vpmacswdx] = "vpmacswd";
  OP_Name[TOP_vpmacswdxx] = "vpmacswd";
  OP_Name[TOP_vpmacswdxxx] = "vpmacswd";
  OP_Name[TOP_vpmacsww] = "vpmacsww";
  OP_Name[TOP_vpmacswwx] = "vpmacsww";
  OP_Name[TOP_vpmacswwxx] = "vpmacsww";
  OP_Name[TOP_vpmacswwxxx] = "vpmacsww";
  OP_Name[TOP_vpmadcsswd] = "vpmadcsswd";
  OP_Name[TOP_vpmadcsswdx] = "vpmadcsswd";
  OP_Name[TOP_vpmadcsswdxx] = "vpmadcsswd";
  OP_Name[TOP_vpmadcsswdxxx] = "vpmadcsswd";
  OP_Name[TOP_vpmadcswd] = "vpmadcswd";
  OP_Name[TOP_vpmadcswdx] = "vpmadcswd";
  OP_Name[TOP_vpmadcswdxx] = "vpmadcswd";
  OP_Name[TOP_vpmadcswdxxx] = "vpmadcswd";
  OP_Name[TOP_vpperm] = "vpperm";
  OP_Name[TOP_vppermx] = "vpperm";
  OP_Name[TOP_vppermxx] = "vpperm";
  OP_Name[TOP_vppermxxx] = "vpperm";
  OP_Name[TOP_vppermxr] = "vpperm";
  OP_Name[TOP_vppermxxr] = "vpperm";
  OP_Name[TOP_vppermxxxr] = "vpperm";
  OP_Name[TOP_vprotbi] = "vprotb";
  OP_Name[TOP_vprotbix] = "vprotb";
  OP_Name[TOP_vprotbixx] = "vprotb";
  OP_Name[TOP_vprotbixxx] = "vprotb";
  OP_Name[TOP_vprotb] = "vprotb";
  OP_Name[TOP_vprotbx] = "vprotb";
  OP_Name[TOP_vprotbxx] = "vprotb";
  OP_Name[TOP_vprotbxxx] = "vprotb";
  OP_Name[TOP_vprotbxr] = "vprotb";
  OP_Name[TOP_vprotbxxr] = "vprotb";
  OP_Name[TOP_vprotbxxxr] = "vprotb";
  OP_Name[TOP_vprotdi] = "vprotd";
  OP_Name[TOP_vprotdix] = "vprotd";
  OP_Name[TOP_vprotdixx] = "vprotd";
  OP_Name[TOP_vprotdixxx] = "vprotd";
  OP_Name[TOP_vprotd] = "vprotd";
  OP_Name[TOP_vprotdx] = "vprotd";
  OP_Name[TOP_vprotdxx] = "vprotd";
  OP_Name[TOP_vprotdxxx] = "vprotd";
  OP_Name[TOP_vprotdxr] = "vprotd";
  OP_Name[TOP_vprotdxxr] = "vprotd";
  OP_Name[TOP_vprotdxxxr] = "vprotd";
  OP_Name[TOP_vprotqi] = "vprotq";
  OP_Name[TOP_vprotqix] = "vprotq";
  OP_Name[TOP_vprotqixx] = "vprotq";
  OP_Name[TOP_vprotqixxx] = "vprotq";
  OP_Name[TOP_vprotq] = "vprotq";
  OP_Name[TOP_vprotqx] = "vprotq";
  OP_Name[TOP_vprotqxx] = "vprotq";
  OP_Name[TOP_vprotqxxx] = "vprotq";
  OP_Name[TOP_vprotqxr] = "vprotq";
  OP_Name[TOP_vprotqxxr] = "vprotq";
  OP_Name[TOP_vprotqxxxr] = "vprotq";
  OP_Name[TOP_vprotwi] = "vprotw";
  OP_Name[TOP_vprotwix] = "vprotw";
  OP_Name[TOP_vprotwixx] = "vprotw";
  OP_Name[TOP_vprotwixxx] = "vprotw";
  OP_Name[TOP_vprotw] = "vprotw";
  OP_Name[TOP_vprotwx] = "vprotw";
  OP_Name[TOP_vprotwxx] = "vprotw";
  OP_Name[TOP_vprotwxxx] = "vprotw";
  OP_Name[TOP_vprotwxr] = "vprotw";
  OP_Name[TOP_vprotwxxr] = "vprotw";
  OP_Name[TOP_vprotwxxxr] = "vprotw";
  OP_Name[TOP_vpshab] = "vpshab";
  OP_Name[TOP_vpshabx] = "vpshab";
  OP_Name[TOP_vpshabxx] = "vpshab";
  OP_Name[TOP_vpshabxxx] = "vpshab";
  OP_Name[TOP_vpshabxr] = "vpshab";
  OP_Name[TOP_vpshabxxr] = "vpshab";
  OP_Name[TOP_vpshabxxxr] = "vpshab";
  OP_Name[TOP_vpshad] = "vpshad";
  OP_Name[TOP_vpshadx] = "vpshad";
  OP_Name[TOP_vpshadxx] = "vpshad";
  OP_Name[TOP_vpshadxxx] = "vpshad";
  OP_Name[TOP_vpshadxr] = "vpshad";
  OP_Name[TOP_vpshadxxr] = "vpshad";
  OP_Name[TOP_vpshadxxxr] = "vpshad";
  OP_Name[TOP_vpshaq] = "vpshaq";
  OP_Name[TOP_vpshaqx] = "vpshaq";
  OP_Name[TOP_vpshaqxx] = "vpshaq";
  OP_Name[TOP_vpshaqxxx] = "vpshaq";
  OP_Name[TOP_vpshaqxr] = "vpshaq";
  OP_Name[TOP_vpshaqxxr] = "vpshaq";
  OP_Name[TOP_vpshaqxxxr] = "vpshaq";
  OP_Name[TOP_vpshaw] = "vpshaw";
  OP_Name[TOP_vpshawx] = "vpshaw";
  OP_Name[TOP_vpshawxx] = "vpshaw";
  OP_Name[TOP_vpshawxxx] = "vpshaw";
  OP_Name[TOP_vpshawxr] = "vpshaw";
  OP_Name[TOP_vpshawxxr] = "vpshaw";
  OP_Name[TOP_vpshawxxxr] = "vpshaw";
  OP_Name[TOP_vpshlb] = "vpshlb";
  OP_Name[TOP_vpshlbx] = "vpshlb";
  OP_Name[TOP_vpshlbxx] = "vpshlb";
  OP_Name[TOP_vpshlbxxx] = "vpshlb";
  OP_Name[TOP_vpshlbxr] = "vpshlb";
  OP_Name[TOP_vpshlbxxr] = "vpshlb";
  OP_Name[TOP_vpshlbxxxr] = "vpshlb";
  OP_Name[TOP_vpshld] = "vpshld";
  OP_Name[TOP_vpshldx] = "vpshld";
  OP_Name[TOP_vpshldxx] = "vpshld";
  OP_Name[TOP_vpshldxxx] = "vpshld";
  OP_Name[TOP_vpshldxr] = "vpshld";
  OP_Name[TOP_vpshldxxr] = "vpshld";
  OP_Name[TOP_vpshldxxxr] = "vpshld";
  OP_Name[TOP_vpshlq] = "vpshlq";
  OP_Name[TOP_vpshlqx] = "vpshlq";
  OP_Name[TOP_vpshlqxx] = "vpshlq";
  OP_Name[TOP_vpshlqxxx] = "vpshlq";
  OP_Name[TOP_vpshlqxr] = "vpshlq";
  OP_Name[TOP_vpshlqxxr] = "vpshlq";
  OP_Name[TOP_vpshlqxxxr] = "vpshlq";
  OP_Name[TOP_vpshlw] = "vpshlw";
  OP_Name[TOP_vpshlwx] = "vpshlw";
  OP_Name[TOP_vpshlwxx] = "vpshlw";
  OP_Name[TOP_vpshlwxxx] = "vpshlw";
  OP_Name[TOP_vpshlwxr] = "vpshlw";
  OP_Name[TOP_vpshlwxxr] = "vpshlw";
  OP_Name[TOP_vpshlwxxxr] = "vpshlw";

  // AES, SSSE3, SSE4.1 SSE4.2 AVX TOP to instruction map
  OP_Name[TOP_vaesenc] = "vaesenc";
  OP_Name[TOP_vaesencx] = "vaesenc";
  OP_Name[TOP_vaesencxx] = "vaesenc";
  OP_Name[TOP_vaesencxxx] = "vaesenc";
  OP_Name[TOP_vaesenclast] = "vaesenclast";
  OP_Name[TOP_vaesenclastx] = "vaesenclast";
  OP_Name[TOP_vaesenclastxx] = "vaesenclast";
  OP_Name[TOP_vaesenclastxxx] = "vaesenclast";
  OP_Name[TOP_vaesdec] = "vaesdec";
  OP_Name[TOP_vaesdecx] = "vaesdec";
  OP_Name[TOP_vaesdecxx] = "vaesdec";
  OP_Name[TOP_vaesdecxxx] = "vaesdec";
  OP_Name[TOP_vaesdeclast] = "vaesdeclast";
  OP_Name[TOP_vaesdeclastx] = "vaesdeclast";
  OP_Name[TOP_vaesdeclastxx] = "vaesdeclast";
  OP_Name[TOP_vaesdeclastxxx] = "vaesdeclast";
  OP_Name[TOP_vaesimc] = "vaesimc";
  OP_Name[TOP_vaesimcx] = "vaesimc";
  OP_Name[TOP_vaesimcxx] = "vaesimc";
  OP_Name[TOP_vaesimcxxx] = "vaesimc";
  OP_Name[TOP_vaeskeygenassist] = "vaeskeygenassist";
  OP_Name[TOP_vaeskeygenassistx] = "vaeskeygenassist";
  OP_Name[TOP_vaeskeygenassistxx] = "vaeskeygenassist";
  OP_Name[TOP_vaeskeygenassistxxx] = "vaeskeygenassist";
  OP_Name[TOP_vpclmulqdq] = "vpclmulqdq";
  OP_Name[TOP_vpclmulqdqx] = "vpclmulqdq";
  OP_Name[TOP_vpclmulqdqxx] = "vpclmulqdq";
  OP_Name[TOP_vpclmulqdqxxx] = "vpclmulqdq";
  OP_Name[TOP_vfadd128v64] = "vaddpd";
  OP_Name[TOP_vfaddx128v64] = "vaddpd";
  OP_Name[TOP_vfaddxx128v64] = "vaddpd";
  OP_Name[TOP_vfaddxxx128v64] = "vaddpd";
  OP_Name[TOP_vfadd128v32] = "vaddps";
  OP_Name[TOP_vfaddx128v32] = "vaddps";
  OP_Name[TOP_vfaddxx128v32] = "vaddps";
  OP_Name[TOP_vfaddxxx128v32] = "vaddps";
  OP_Name[TOP_vfaddsd] = "vaddsd";
  OP_Name[TOP_vfaddxsd] = "vaddsd";
  OP_Name[TOP_vfaddxxsd] = "vaddsd";
  OP_Name[TOP_vfaddxxxsd] = "vaddsd";
  OP_Name[TOP_vfaddss] = "vaddss";
  OP_Name[TOP_vfaddxss] = "vaddss";
  OP_Name[TOP_vfaddxxss] = "vaddss";
  OP_Name[TOP_vfaddxxxss] = "vaddss";
  OP_Name[TOP_vfaddsub128v64] = "vaddsubpd";
  OP_Name[TOP_vfaddsubx128v64] = "vaddsubpd";
  OP_Name[TOP_vfaddsubxx128v64] = "vaddsubpd";
  OP_Name[TOP_vfaddsubxxx128v64] = "vaddsubpd";
  OP_Name[TOP_vfaddsub128v32] = "vaddsubps";
  OP_Name[TOP_vfaddsubx128v32] = "vaddsubps";
  OP_Name[TOP_vfaddsubxx128v32] = "vaddsubps";
  OP_Name[TOP_vfaddsubxxx128v32] = "vaddsubps";
  OP_Name[TOP_vfand128v64] = "vandpd";
  OP_Name[TOP_vfandx128v64] = "vandpd";
  OP_Name[TOP_vfandxx128v64] = "vandpd";
  OP_Name[TOP_vfandxxx128v64] = "vandpd";
  OP_Name[TOP_vfand128v32] = "vandps";
  OP_Name[TOP_vfandx128v32] = "vandps";
  OP_Name[TOP_vfandxx128v32] = "vandps";
  OP_Name[TOP_vfandxxx128v32] = "vandps";
  OP_Name[TOP_vfandn128v64] = "vandnpd";
  OP_Name[TOP_vfandnx128v64] = "vandnpd";
  OP_Name[TOP_vfandnxx128v64] = "vandnpd";
  OP_Name[TOP_vfandnxxx128v64] = "vandnpd";
  OP_Name[TOP_vfandn128v32] = "vandnps";
  OP_Name[TOP_vfandnx128v32] = "vandnps";
  OP_Name[TOP_vfandnxx128v32] = "vandnps";
  OP_Name[TOP_vfandnxxx128v32] = "vandnps";
  OP_Name[TOP_vfblend128v64] = "vblendpd";
  OP_Name[TOP_vfblendx128v64] = "vblendpd";
  OP_Name[TOP_vfblendxx128v64] = "vblendpd";
  OP_Name[TOP_vfblendxxx128v64] = "vblendpd";
  OP_Name[TOP_vfblend128v32] = "vblendps";
  OP_Name[TOP_vfblendx128v32] = "vblendps";
  OP_Name[TOP_vfblendxx128v32] = "vblendps";
  OP_Name[TOP_vfblendxxx128v32] = "vblendps";
  OP_Name[TOP_vfblendv128v64] = "vblendvpd";
  OP_Name[TOP_vfblendvx128v64] = "vblendvpd";
  OP_Name[TOP_vfblendvxx128v64] = "vblendvpd";
  OP_Name[TOP_vfblendvxxx128v64] = "vblendvpd";
  OP_Name[TOP_vfblendv128v32] = "vblendvps";
  OP_Name[TOP_vfblendvx128v32] = "vblendvps";
  OP_Name[TOP_vfblendvxx128v32] = "vblendvps";
  OP_Name[TOP_vfblendvxxx128v32] = "vblendvps";
  OP_Name[TOP_vfbroadcastss] = "vbroadcastss";
  OP_Name[TOP_vfbroadcastxss] = "vbroadcastss";
  OP_Name[TOP_vfbroadcastxxss] = "vbroadcastss";
  OP_Name[TOP_vfbroadcastsd] = "vbroadcastsd";
  OP_Name[TOP_vfbroadcastxsd] = "vbroadcastsd";
  OP_Name[TOP_vfbroadcastxxsd] = "vbroadcastsd";
  OP_Name[TOP_vfbroadcastf128] = "vbroadcastf128";
  OP_Name[TOP_vfbroadcastxf128] = "vbroadcastf128";
  OP_Name[TOP_vfbroadcastxxf128] = "vbroadcastf128";
  OP_Name[TOP_vfcmp128v64] = "vcmppd";
  OP_Name[TOP_vfcmpx128v64] = "vcmppd";
  OP_Name[TOP_vfcmpxx128v64] = "vcmppd";
  OP_Name[TOP_vfcmpxxx128v64] = "vcmppd";
  OP_Name[TOP_vfcmp128v32] = "vcmpps";
  OP_Name[TOP_vfcmpx128v32] = "vcmpps";
  OP_Name[TOP_vfcmpxx128v32] = "vcmpps";
  OP_Name[TOP_vfcmpxxx128v32] = "vcmpps";
  OP_Name[TOP_vcmpsd] = "vcmpsd";
  OP_Name[TOP_vcmpxsd] = "vcmpsd";
  OP_Name[TOP_vcmpxxsd] = "vcmpsd";
  OP_Name[TOP_vcmpxxxsd] = "vcmpsd";
  OP_Name[TOP_vcmpss] = "vcmpss";
  OP_Name[TOP_vcmpxss] = "vcmpss";
  OP_Name[TOP_vcmpxxss] = "vcmpss";
  OP_Name[TOP_vcmpxxxss] = "vcmpss";
  OP_Name[TOP_vcomisd] = "vcomisd";
  OP_Name[TOP_vcomixsd] = "vcomisd";
  OP_Name[TOP_vcomixxsd] = "vcomisd";
  OP_Name[TOP_vcomixxxsd] = "vcomisd";
  OP_Name[TOP_vcomiss] = "vcomiss";
  OP_Name[TOP_vcomixss] = "vcomiss";
  OP_Name[TOP_vcomixxss] = "vcomiss";
  OP_Name[TOP_vcomixxxss] = "vcomiss";
  OP_Name[TOP_vcvtdq2pd] = "vcvtdq2pd";
  OP_Name[TOP_vcvtdq2pdx] = "vcvtdq2pd";
  OP_Name[TOP_vcvtdq2pdxx] = "vcvtdq2pd";
  OP_Name[TOP_vcvtdq2pdxxx] = "vcvtdq2pd";
  OP_Name[TOP_vcvtdq2ps] = "vcvtdq2ps";
  OP_Name[TOP_vcvtdq2psx] = "vcvtdq2ps";
  OP_Name[TOP_vcvtdq2psxx] = "vcvtdq2ps";
  OP_Name[TOP_vcvtdq2psxxx] = "vcvtdq2ps";

  // TODO: add y suffix additions for 256-bit cg.
  //   affected insns: vcvtpd2dq, vcvtpd2ps, vcvttpd2dq
  //                   vcvtsi2sdq
  OP_Name[TOP_vcvtpd2dq] = "vcvtpd2dq";
  OP_Name[TOP_vcvtpd2dqx] = "vcvtpd2dq";
  OP_Name[TOP_vcvtpd2dqxx] = "vcvtpd2dq";
  OP_Name[TOP_vcvtpd2dqxxx] = "vcvtpd2dq";
  OP_Name[TOP_vcvtpd2dqy] = "vcvtpd2dqy";
  OP_Name[TOP_vcvtpd2dqyx] = "vcvtpd2dqy";
  OP_Name[TOP_vcvtpd2dqyxx] = "vcvtpd2dqy";
  OP_Name[TOP_vcvtpd2dqyxxx] = "vcvtpd2dqy";
  OP_Name[TOP_vcvtpd2ps] = "vcvtpd2ps";
  OP_Name[TOP_vcvtpd2psx] = "vcvtpd2psy";
  OP_Name[TOP_vcvtpd2psxx] = "vcvtpd2psy";
  OP_Name[TOP_vcvtpd2psxxx] = "vcvtpd2psy";
  OP_Name[TOP_vcvtpd2psy] = "vcvtpd2psy";
  OP_Name[TOP_vcvtpd2psyx] = "vcvtpd2psy";
  OP_Name[TOP_vcvtpd2psyxx] = "vcvtpd2psy";
  OP_Name[TOP_vcvtpd2psyxxx] = "vcvtpd2psy";
  OP_Name[TOP_vcvttpd2dq] = "vcvttpd2dq";
  OP_Name[TOP_vcvttpd2dqx] = "vcvttpd2dq";
  OP_Name[TOP_vcvttpd2dqxx] = "vcvttpd2dq";
  OP_Name[TOP_vcvttpd2dqxxx] = "vcvttpd2dq";
  OP_Name[TOP_vcvttpd2dqy] = "vcvttpd2dqy";
  OP_Name[TOP_vcvttpd2dqyx] = "vcvttpd2dqy";
  OP_Name[TOP_vcvttpd2dqyxx] = "vcvttpd2dqy";
  OP_Name[TOP_vcvttpd2dqyxxx] = "vcvttpd2dqy";

  OP_Name[TOP_vcvtps2dq] = "vcvtps2dq";
  OP_Name[TOP_vcvtps2dqx] = "vcvtps2dq";
  OP_Name[TOP_vcvtps2dqxx] = "vcvtps2dq";
  OP_Name[TOP_vcvtps2dqxxx] = "vcvtps2dq";
  OP_Name[TOP_vcvtps2pd] = "vcvtps2pd";
  OP_Name[TOP_vcvtps2pdx] = "vcvtps2pd";
  OP_Name[TOP_vcvtps2pdxx] = "vcvtps2pd";
  OP_Name[TOP_vcvtps2pdxxx] = "vcvtps2pd";
  OP_Name[TOP_vcvtsd2si] = "vcvtsd2si";
  OP_Name[TOP_vcvtsd2siq] = "vcvtsd2siq";
  OP_Name[TOP_vcvtsd2ss] = "vcvtsd2ss";
  OP_Name[TOP_vcvtsd2ssx] = "vcvtsd2ss";
  OP_Name[TOP_vcvtsd2ssxx] = "vcvtsd2ss";
  OP_Name[TOP_vcvtsd2ssxxx] = "vcvtsd2ss";
  OP_Name[TOP_vcvtsi2sd] = "vcvtsi2sd";
  OP_Name[TOP_vcvtsi2sdx] = "vcvtsi2sd";
  OP_Name[TOP_vcvtsi2sdxx] = "vcvtsi2sd";
  OP_Name[TOP_vcvtsi2sdxxx] = "vcvtsi2sd";
  OP_Name[TOP_vcvtsi2sdq] = "vcvtsi2sdq";
  OP_Name[TOP_vcvtsi2sdqx] = "vcvtsi2sdq";
  OP_Name[TOP_vcvtsi2sdqxx] = "vcvtsi2sdq";
  OP_Name[TOP_vcvtsi2sdqxxx] = "vcvtsi2sdq";
  OP_Name[TOP_vcvtsi2ss] = "vcvtsi2ss";
  OP_Name[TOP_vcvtsi2ssx] = "vcvtsi2ss";
  OP_Name[TOP_vcvtsi2ssxx] = "vcvtsi2ss";
  OP_Name[TOP_vcvtsi2ssxxx] = "vcvtsi2ss";
  OP_Name[TOP_vcvtsi2ssq] = "vcvtsi2ssq";
  OP_Name[TOP_vcvtsi2ssqx] = "vcvtsi2ssq";
  OP_Name[TOP_vcvtsi2ssqxx] = "vcvtsi2ssq";
  OP_Name[TOP_vcvtsi2ssqxxx] = "vcvtsi2ssq";
  OP_Name[TOP_vcvtss2sd] = "vcvtss2sd";
  OP_Name[TOP_vcvtss2sdx] = "vcvtss2sd";
  OP_Name[TOP_vcvtss2sdxx] = "vcvtss2sd";
  OP_Name[TOP_vcvtss2sdxxx] = "vcvtss2sd";
  OP_Name[TOP_vcvtss2si] = "vcvtss2si";
  OP_Name[TOP_vcvtss2siq] = "vcvtss2siq";
  OP_Name[TOP_vcvttps2dq] = "vcvttps2dq";
  OP_Name[TOP_vcvttps2dqx] = "vcvttps2dq";
  OP_Name[TOP_vcvttps2dqxx] = "vcvttps2dq";
  OP_Name[TOP_vcvttps2dqxxx] = "vcvttps2dq";
  OP_Name[TOP_vcvttsd2si] = "vcvttsd2si";
  OP_Name[TOP_vcvttsd2siq] = "vcvttsd2siq";
  OP_Name[TOP_vcvttss2si] = "vcvttss2si";
  OP_Name[TOP_vcvttss2siq] = "vcvttss2siq";
  OP_Name[TOP_vfdiv128v64] = "vdivpd";
  OP_Name[TOP_vfdivx128v64] = "vdivpd";
  OP_Name[TOP_vfdivxx128v64] = "vdivpd";
  OP_Name[TOP_vfdivxxx128v64] = "vdivpd";
  OP_Name[TOP_vfdiv128v32] = "vdivps";
  OP_Name[TOP_vfdivx128v32] = "vdivps";
  OP_Name[TOP_vfdivxx128v32] = "vdivps";
  OP_Name[TOP_vfdivxxx128v32] = "vdivps";
  OP_Name[TOP_vdivsd] = "vdivsd";
  OP_Name[TOP_vdivxsd] = "vdivsd";
  OP_Name[TOP_vdivxxsd] = "vdivsd";
  OP_Name[TOP_vdivxxxsd] = "vdivsd";
  OP_Name[TOP_vdivss] = "vdivss";
  OP_Name[TOP_vdivxss] = "vdivss";
  OP_Name[TOP_vdivxxss] = "vdivss";
  OP_Name[TOP_vdivxxxss] = "vdivss";
  OP_Name[TOP_vfdp128v64] = "vdppd";
  OP_Name[TOP_vfdpx128v64] = "vdppd";
  OP_Name[TOP_vfdpxx128v64] = "vdppd";
  OP_Name[TOP_vfdpxxx128v64] = "vdppd";
  OP_Name[TOP_vfdp128v32] = "vdpps";
  OP_Name[TOP_vfdpx128v32] = "vdpps";
  OP_Name[TOP_vfdpxx128v32] = "vdpps";
  OP_Name[TOP_vfdpxxx128v32] = "vdpps";
  OP_Name[TOP_vfextrf128] = "vextractf128";
  OP_Name[TOP_vfextrxf128] = "vextractf128";
  OP_Name[TOP_vfextrxxf128] = "vextractf128";
  OP_Name[TOP_vfextrxxxf128] = "vextractf128";
  OP_Name[TOP_vfextr128v32] = "vextractps";
  OP_Name[TOP_vfextrx128v32] = "vextractps";
  OP_Name[TOP_vfextrxx128v32] = "vextractps";
  OP_Name[TOP_vfextrxxx128v32] = "vextractps";
  OP_Name[TOP_vfhadd128v64] = "vhaddpd";
  OP_Name[TOP_vfhaddx128v64] = "vhaddpd";
  OP_Name[TOP_vfhaddxx128v64] = "vhaddpd";
  OP_Name[TOP_vfhaddxxx128v64] = "vhaddpd";
  OP_Name[TOP_vfhadd128v32] = "vhaddps";
  OP_Name[TOP_vfhaddx128v32] = "vhaddps";
  OP_Name[TOP_vfhaddxx128v32] = "vhaddps";
  OP_Name[TOP_vfhaddxxx128v32] = "vhaddps";
  OP_Name[TOP_vfhsub128v64] = "vhsubpd";
  OP_Name[TOP_vfhsubx128v64] = "vhsubpd";
  OP_Name[TOP_vfhsubxx128v64] = "vhsubpd";
  OP_Name[TOP_vfhsubxxx128v64] = "vhsubpd";
  OP_Name[TOP_vfhsub128v32] = "vhsubps";
  OP_Name[TOP_vfhsubx128v32] = "vhsubps";
  OP_Name[TOP_vfhsubxx128v32] = "vhsubps";
  OP_Name[TOP_vfhsubxxx128v32] = "vhsubps";
  OP_Name[TOP_vfinsrf128] = "vinsertf128";
  OP_Name[TOP_vfinsrxf128] = "vinsertf128";
  OP_Name[TOP_vfinsrxxf128] = "vinsertf128";
  OP_Name[TOP_vfinsrxxxf128] = "vinsertf128";
  OP_Name[TOP_vfinsr128v32] = "vinsertps";
  OP_Name[TOP_vfinsrx128v32] = "vinsertps";
  OP_Name[TOP_vfinsrxx128v32] = "vinsertps";
  OP_Name[TOP_vfinsrxxx128v32] = "vinsertps";
  OP_Name[TOP_vlddqu] = "vlddqu";
  OP_Name[TOP_vlddqux] = "vlddqu";
  OP_Name[TOP_vlddquxx] = "vlddqu";
  OP_Name[TOP_vlddqu_n32] = "vlddqu";
  OP_Name[TOP_vldmxcsr] = "vldmxcsr";
  OP_Name[TOP_vmaskmovdqu] = "vmaskmovdqu";
  OP_Name[TOP_vfmaskld128v32] = "vmaskmovps";
  OP_Name[TOP_vfmaskldx128v32] = "vmaskmovps";
  OP_Name[TOP_vfmaskldxx128v32] = "vmaskmovps";
  OP_Name[TOP_vfmaskld128v64] = "vmaskmovpd";
  OP_Name[TOP_vfmaskldx128v64] = "vmaskmovpd";
  OP_Name[TOP_vfmaskldxx128v64] = "vmaskmovpd";
  OP_Name[TOP_vfmaskst128v32] = "vmaskmovps";
  OP_Name[TOP_vfmaskstx128v32] = "vmaskmovps";
  OP_Name[TOP_vfmaskstxx128v32] = "vmaskmovps";
  OP_Name[TOP_vfmaskst128v64] = "vmaskmovpd";
  OP_Name[TOP_vfmaskstx128v64] = "vmaskmovpd";
  OP_Name[TOP_vfmaskstxx128v64] = "vmaskmovpd";
  OP_Name[TOP_vfmax128v64] = "vmaxpd";
  OP_Name[TOP_vfmaxx128v64] = "vmaxpd";
  OP_Name[TOP_vfmaxxx128v64] = "vmaxpd";
  OP_Name[TOP_vfmaxxxx128v64] = "vmaxpd";
  OP_Name[TOP_vfmax128v32] = "vmaxps";
  OP_Name[TOP_vfmaxx128v32] = "vmaxps";
  OP_Name[TOP_vfmaxxx128v32] = "vmaxps";
  OP_Name[TOP_vfmaxxxx128v32] = "vmaxps";
  OP_Name[TOP_vfmaxsd] = "vmaxsd";
  OP_Name[TOP_vfmaxxsd] = "vmaxsd";
  OP_Name[TOP_vfmaxxxsd] = "vmaxsd";
  OP_Name[TOP_vfmaxxxxsd] = "vmaxsd";
  OP_Name[TOP_vfmaxss] = "vmaxss";
  OP_Name[TOP_vfmaxxss] = "vmaxss";
  OP_Name[TOP_vfmaxxxss] = "vmaxss";
  OP_Name[TOP_vfmaxxxxss] = "vmaxss";
  OP_Name[TOP_vfmin128v64] = "vminpd";
  OP_Name[TOP_vfminx128v64] = "vminpd";
  OP_Name[TOP_vfminxx128v64] = "vminpd";
  OP_Name[TOP_vfminxxx128v64] = "vminpd";
  OP_Name[TOP_vfmin128v32] = "vminps";
  OP_Name[TOP_vfminx128v32] = "vminps";
  OP_Name[TOP_vfminxx128v32] = "vminps";
  OP_Name[TOP_vfminxxx128v32] = "vminps";
  OP_Name[TOP_vfminsd] = "vminsd";
  OP_Name[TOP_vfminxsd] = "vminsd";
  OP_Name[TOP_vfminxxsd] = "vminsd";
  OP_Name[TOP_vfminxxxsd] = "vminsd";
  OP_Name[TOP_vfminss] = "vminss";
  OP_Name[TOP_vfminxss] = "vminss";
  OP_Name[TOP_vfminxxss] = "vminss";
  OP_Name[TOP_vfminxxxss] = "vminss";
  OP_Name[TOP_vldapd] = "vmovapd";
  OP_Name[TOP_vldapdx] = "vmovapd";
  OP_Name[TOP_vldapdxx] = "vmovapd";
  OP_Name[TOP_vldapd_n32] = "vmovapd";
  OP_Name[TOP_vstapd] = "vmovapd";
  OP_Name[TOP_vstapdx] = "vmovapd";
  OP_Name[TOP_vstapdxx] = "vmovapd";
  OP_Name[TOP_vstapd_n32] = "vmovapd";
  OP_Name[TOP_vldaps] = "vmovaps";
  OP_Name[TOP_vldapsx] = "vmovaps";
  OP_Name[TOP_vldapsxx] = "vmovaps";
  OP_Name[TOP_vldaps_n32] = "vmovaps";
  OP_Name[TOP_vstaps] = "vmovaps";
  OP_Name[TOP_vstapsx] = "vmovaps";
  OP_Name[TOP_vstapsxx] = "vmovaps";
  OP_Name[TOP_vstaps_n32] = "vmovaps";
  OP_Name[TOP_vmovg2x] = "vmovd";
  OP_Name[TOP_vmovg2x64] = "vmovd";
  OP_Name[TOP_vmovx2g] = "vmovd";
  OP_Name[TOP_vmovx2g64] = "vmovd";
  OP_Name[TOP_vld64_2sse] = "vmovq";
  OP_Name[TOP_vldx64_2sse] = "vmovq";
  OP_Name[TOP_vldxx64_2sse] = "vmovq";
  OP_Name[TOP_vld64_2sse_n32] = "vmovq";
  OP_Name[TOP_vst64_fsse] = "vmovq";
  OP_Name[TOP_vstx64_fsse] = "vmovq";
  OP_Name[TOP_vstxx64_fsse] = "vmovq";
  OP_Name[TOP_vst64_fsse_n32] = "vmovq";
  OP_Name[TOP_vmovddup] = "vmovddup";
  OP_Name[TOP_vmovddupx] = "vmovddup";
  OP_Name[TOP_vmovddupxx] = "vmovddup";
  OP_Name[TOP_vmovddupxxx] = "vmovddup";
  OP_Name[TOP_vmovshdup] = "vmovshdup";
  OP_Name[TOP_vmovshdupx] = "vmovshdup";
  OP_Name[TOP_vmovshdupxx] = "vmovshdup";
  OP_Name[TOP_vmovshdupxxx] = "vmovshdup";
  OP_Name[TOP_vmovsldup] = "vmovsldup";
  OP_Name[TOP_vmovsldupx] = "vmovsldup";
  OP_Name[TOP_vmovsldupxx] = "vmovsldup";
  OP_Name[TOP_vmovsldupxxx] = "vmovsldup";
  OP_Name[TOP_vlddqa] = "vmovdqa";
  OP_Name[TOP_vlddqax] = "vmovdqa";
  OP_Name[TOP_vlddqaxx] = "vmovdqa";
  OP_Name[TOP_vlddqa_n32] = "vmovdqa";
  OP_Name[TOP_vmovdqa] = "vmovdqa";
  OP_Name[TOP_vmovapd] = "vmovapd";
  OP_Name[TOP_vmovaps] = "vmovaps";
  OP_Name[TOP_vstdqa] = "vmovdqa";
  OP_Name[TOP_vstdqax] = "vmovdqa";
  OP_Name[TOP_vstdqaxx] = "vmovdqa";
  OP_Name[TOP_vstdqa_n32] = "vmovdqa";
  OP_Name[TOP_vstdqu] = "vmovdqu";
  OP_Name[TOP_vstdqux] = "vmovdqu";
  OP_Name[TOP_vstdquxx] = "vmovdqu";
  OP_Name[TOP_vstdqu_n32] = "vmovdqu";
  OP_Name[TOP_vldhpd] = "vmovhpd";
  OP_Name[TOP_vldhpdx] = "vmovhpd";
  OP_Name[TOP_vldhpdxx] = "vmovhpd";
  OP_Name[TOP_vldhpd_n32] = "vmovhpd";
  OP_Name[TOP_vsthpd] = "vmovhpd";
  OP_Name[TOP_vsthpdx] = "vmovhpd";
  OP_Name[TOP_vsthpdxx] = "vmovhpd";
  OP_Name[TOP_vsthpd_n32] = "vmovhpd";
  OP_Name[TOP_vldhps] = "vmovhps";
  OP_Name[TOP_vldhpsx] = "vmovhps";
  OP_Name[TOP_vldhpsxx] = "vmovhps";
  OP_Name[TOP_vldhps_n32] = "vmovhps";
  OP_Name[TOP_vsthps] = "vmovhps";
  OP_Name[TOP_vsthpsx] = "vmovhps";
  OP_Name[TOP_vsthpsxx] = "vmovhps";
  OP_Name[TOP_vsthps_n32] = "vmovhps";
  OP_Name[TOP_vldsd] = "vmovlpd";
  OP_Name[TOP_vldsdx] = "vmovlpd";
  OP_Name[TOP_vldsdxx] = "vmovlpd";
  OP_Name[TOP_vldsd_n32] = "vmovlpd";
  OP_Name[TOP_vldlps] = "vmovlps";
  OP_Name[TOP_vldlpsx] = "vmovlps";
  OP_Name[TOP_vldlpsxx] = "vmovlps";
  OP_Name[TOP_vldlps_n32] = "vmovlps";
  OP_Name[TOP_vstlps] = "vmovlps";
  OP_Name[TOP_vstlpsx] = "vmovlps";
  OP_Name[TOP_vstlpsxx] = "vmovlps";
  OP_Name[TOP_vstlps_n32] = "vmovlps";
  OP_Name[TOP_vstorenti128] = "vmovntdq";
  OP_Name[TOP_vstorentxi128] = "vmovntdq";
  OP_Name[TOP_vstorentxxi128] = "vmovntdq";
  OP_Name[TOP_vldntdqa] = "vmovntdqa";
  OP_Name[TOP_vldntdqax] = "vmovntdqa";
  OP_Name[TOP_vldntdqaxx] = "vmovntdqa";
  OP_Name[TOP_vstntdq] = "vmovntdq";
  OP_Name[TOP_vstntdqx] = "vmovntdq";
  OP_Name[TOP_vstntdqxx] = "vmovntdq";
  OP_Name[TOP_vstntpd] = "vmovntpd";
  OP_Name[TOP_vstntpdx] = "vmovntpd";
  OP_Name[TOP_vstntpdxx] = "vmovntpd";
  OP_Name[TOP_vstntps] = "vmovntps";
  OP_Name[TOP_vstntpsx] = "vmovntps";
  OP_Name[TOP_vstntpsxx] = "vmovntps";
  OP_Name[TOP_vldlpd] = "vmovsd";
  OP_Name[TOP_vldlpdx] = "vmovsd";
  OP_Name[TOP_vldlpdxx] = "vmovsd";
  OP_Name[TOP_vldlpd_n32] = "vmovsd";
  OP_Name[TOP_vstlpd] = "vmovsd";
  OP_Name[TOP_vstlpdx] = "vmovsd";
  OP_Name[TOP_vstlpdxx] = "vmovsd";
  OP_Name[TOP_vstlpd_n32] = "vmovsd";
  OP_Name[TOP_vstsd] = "vmovsd";
  OP_Name[TOP_vstsdx] = "vmovsd";
  OP_Name[TOP_vstsdxx] = "vmovsd";
  OP_Name[TOP_vstsd_n32] = "vmovsd";
  OP_Name[TOP_vmovsd] = "vmovsd";
  OP_Name[TOP_vldss] = "vmovss";
  OP_Name[TOP_vldssx] = "vmovss";
  OP_Name[TOP_vldssxx] = "vmovss";
  OP_Name[TOP_vldss_n32] = "vmovss";
  OP_Name[TOP_vstss] = "vmovss";
  OP_Name[TOP_vstssx] = "vmovss";
  OP_Name[TOP_vstssxx] = "vmovss";
  OP_Name[TOP_vstss_n32] = "vmovss";
  OP_Name[TOP_vmovss] = "vmovss";
  OP_Name[TOP_vldupd] = "vmovupd";
  OP_Name[TOP_vldupdx] = "vmovupd";
  OP_Name[TOP_vldupdxx] = "vmovupd";
  OP_Name[TOP_vldupd_n32] = "vmovupd";
  OP_Name[TOP_vstupd] = "vmovupd";
  OP_Name[TOP_vstupdx] = "vmovupd";
  OP_Name[TOP_vstupdxx] = "vmovupd";
  OP_Name[TOP_vstupd_n32] = "vmovupd";
  OP_Name[TOP_vldups] = "vmovups";
  OP_Name[TOP_vldupsx] = "vmovups";
  OP_Name[TOP_vldupsxx] = "vmovups";
  OP_Name[TOP_vldups_n32] = "vmovups";
  OP_Name[TOP_vstups] = "vmovups";
  OP_Name[TOP_vstupsx] = "vmovups";
  OP_Name[TOP_vstupsxx] = "vmovups";
  OP_Name[TOP_vstups_n32] = "vmovups";
  OP_Name[TOP_vmpsadbw] = "vmpsadbw";
  OP_Name[TOP_vmpsadbwx] = "vmpsadbw";
  OP_Name[TOP_vmpsadbwxx] = "vmpsadbw";
  OP_Name[TOP_vmpsadbwxxx] = "vmpsadbw";
  OP_Name[TOP_vfmul128v64] = "vmulpd";
  OP_Name[TOP_vfmulx128v64] = "vmulpd";
  OP_Name[TOP_vfmulxx128v64] = "vmulpd";
  OP_Name[TOP_vfmulxxx128v64] = "vmulpd";
  OP_Name[TOP_vfmul128v32] = "vmulps";
  OP_Name[TOP_vfmulx128v32] = "vmulps";
  OP_Name[TOP_vfmulxx128v32] = "vmulps";
  OP_Name[TOP_vfmulxxx128v32] = "vmulps";
  OP_Name[TOP_vmulsd] = "vmulsd";
  OP_Name[TOP_vmulxsd] = "vmulsd";
  OP_Name[TOP_vmulxxsd] = "vmulsd";
  OP_Name[TOP_vmulxxxsd] = "vmulsd";
  OP_Name[TOP_vmulss] = "vmulss";
  OP_Name[TOP_vmulxss] = "vmulss";
  OP_Name[TOP_vmulxxss] = "vmulss";
  OP_Name[TOP_vmulxxxss] = "vmulss";
  OP_Name[TOP_vfor128v64] = "vorpd";
  OP_Name[TOP_vforx128v64] = "vorpd";
  OP_Name[TOP_vforxx128v64] = "vorpd";
  OP_Name[TOP_vforxxx128v64] = "vorpd";
  OP_Name[TOP_vfor128v32] = "vorps";
  OP_Name[TOP_vforx128v32] = "vorps";
  OP_Name[TOP_vforxx128v32] = "vorps";
  OP_Name[TOP_vforxxx128v32] = "vorps";
  OP_Name[TOP_vabs128v8] = "vpabsb";
  OP_Name[TOP_vabsx128v8] = "vpabsb";
  OP_Name[TOP_vabsxx128v8] = "vpabsb";
  OP_Name[TOP_vabsxxx128v8] = "vpabsb";
  OP_Name[TOP_vabs128v32] = "vpabsd";
  OP_Name[TOP_vabsx128v32] = "vpabsd";
  OP_Name[TOP_vabsxx128v32] = "vpabsd";
  OP_Name[TOP_vabsxxx128v32] = "vpabsd";
  OP_Name[TOP_vabs128v16] = "vpabsw";
  OP_Name[TOP_vabsx128v16] = "vpabsw";
  OP_Name[TOP_vabsxx128v16] = "vpabsw";
  OP_Name[TOP_vabsxxx128v16] = "vpabsw";
  OP_Name[TOP_vpackssdw] = "vpackssdw";
  OP_Name[TOP_vpackssdwx] = "vpackssdw";
  OP_Name[TOP_vpackssdwxx] = "vpackssdw";
  OP_Name[TOP_vpackssdwxxx] = "vpackssdw";
  OP_Name[TOP_vpacksswb] = "vpacksswb";
  OP_Name[TOP_vpacksswbx] = "vpacksswb";
  OP_Name[TOP_vpacksswbxx] = "vpacksswb";
  OP_Name[TOP_vpacksswbxxx] = "vpacksswb";
  OP_Name[TOP_vpackusdw] = "vpackusdw";
  OP_Name[TOP_vpackusdwx] = "vpackusdw";
  OP_Name[TOP_vpackusdwxx] = "vpackusdw";
  OP_Name[TOP_vpackusdwxxx] = "vpackusdw";
  OP_Name[TOP_vpackuswb] = "vpackuswb";
  OP_Name[TOP_vpackuswbx] = "vpackuswb";
  OP_Name[TOP_vpackuswbxx] = "vpackuswb";
  OP_Name[TOP_vpackuswbxxx] = "vpackuswb";
  OP_Name[TOP_vadd128v8] = "vpaddb";
  OP_Name[TOP_vaddx128v8] = "vpaddb";
  OP_Name[TOP_vaddxx128v8] = "vpaddb";
  OP_Name[TOP_vaddxxx128v8] = "vpaddb";
  OP_Name[TOP_vadd128v32] = "vpaddd";
  OP_Name[TOP_vaddx128v32] = "vpaddd";
  OP_Name[TOP_vaddxx128v32] = "vpaddd";
  OP_Name[TOP_vaddxxx128v32] = "vpaddd";
  OP_Name[TOP_vadd128v64] = "vpaddq";
  OP_Name[TOP_vaddx128v64] = "vpaddq";
  OP_Name[TOP_vaddxx128v64] = "vpaddq";
  OP_Name[TOP_vaddxxx128v64] = "vpaddq";
  OP_Name[TOP_vadd128v16] = "vpaddw";
  OP_Name[TOP_vaddx128v16] = "vpaddw";
  OP_Name[TOP_vaddxx128v16] = "vpaddw";
  OP_Name[TOP_vaddxxx128v16] = "vpaddw";
  OP_Name[TOP_vadds128v8] = "vpaddsb";
  OP_Name[TOP_vaddsx128v8] = "vpaddsb";
  OP_Name[TOP_vaddsxx128v8] = "vpaddsb";
  OP_Name[TOP_vaddsxxx128v8] = "vpaddsb";
  OP_Name[TOP_vadds128v16] = "vpaddsw";
  OP_Name[TOP_vaddsx128v16] = "vpaddsw";
  OP_Name[TOP_vaddsxx128v16] = "vpaddsw";
  OP_Name[TOP_vaddsxxx128v16] = "vpaddsw";
  OP_Name[TOP_vaddus128v8] = "vpaddusb";
  OP_Name[TOP_vaddusx128v8] = "vpaddusb";
  OP_Name[TOP_vaddusxx128v8] = "vpaddusb";
  OP_Name[TOP_vaddusxxx128v8] = "vpaddusb";
  OP_Name[TOP_vaddus128v16] = "vpaddusw";
  OP_Name[TOP_vaddusx128v16] = "vpaddusw";
  OP_Name[TOP_vaddusxx128v16] = "vpaddusw";
  OP_Name[TOP_vaddusxxx128v16] = "vpaddusw";
  OP_Name[TOP_vpalignr128] = "vpalignr";
  OP_Name[TOP_vpalignrx128] = "vpalignr";
  OP_Name[TOP_vpalignrxx128] = "vpalignr";
  OP_Name[TOP_vpalignrxxx128] = "vpalignr";
  OP_Name[TOP_vand128v8] = "vpand";
  OP_Name[TOP_vandx128v8] = "vpand";
  OP_Name[TOP_vandxx128v8] = "vpand";
  OP_Name[TOP_vandxxx128v8] = "vpand";
  OP_Name[TOP_vand128v16] = "vpand";
  OP_Name[TOP_vandx128v16] = "vpand";
  OP_Name[TOP_vandxx128v16] = "vpand";
  OP_Name[TOP_vandxxx128v16] = "vpand";
  OP_Name[TOP_vand128v32] = "vpand";
  OP_Name[TOP_vandx128v32] = "vpand";
  OP_Name[TOP_vandxx128v32] = "vpand";
  OP_Name[TOP_vandxxx128v32] = "vpand";
  OP_Name[TOP_vand128v64] = "vpand";
  OP_Name[TOP_vandx128v64] = "vpand";
  OP_Name[TOP_vandxx128v64] = "vpand";
  OP_Name[TOP_vandxxx128v64] = "vpand";
  OP_Name[TOP_vandn128v8] = "vpandn";
  OP_Name[TOP_vandnx128v8] = "vpandn";
  OP_Name[TOP_vandnxx128v8] = "vpandn";
  OP_Name[TOP_vandnxxx128v8] = "vpandn";
  OP_Name[TOP_vandn128v16] = "vpandn";
  OP_Name[TOP_vandnx128v16] = "vpandn";
  OP_Name[TOP_vandnxx128v16] = "vpandn";
  OP_Name[TOP_vandnxxx128v16] = "vpandn";
  OP_Name[TOP_vandn128v32] = "vpandn";
  OP_Name[TOP_vandnx128v32] = "vpandn";
  OP_Name[TOP_vandnxx128v32] = "vpandn";
  OP_Name[TOP_vandnxxx128v32] = "vpandn";
  OP_Name[TOP_vandn128v64] = "vpandn";
  OP_Name[TOP_vandnx128v64] = "vpandn";
  OP_Name[TOP_vandnxx128v64] = "vpandn";
  OP_Name[TOP_vandnxxx128v64] = "vpandn";
  OP_Name[TOP_vpavgb] = "vpavgb";
  OP_Name[TOP_vpavgbx] = "vpavgb";
  OP_Name[TOP_vpavgbxx] = "vpavgb";
  OP_Name[TOP_vpavgbxxx] = "vpavgb";
  OP_Name[TOP_vpavgw] = "vpavgw";
  OP_Name[TOP_vpavgwx] = "vpavgw";
  OP_Name[TOP_vpavgwxx] = "vpavgw";
  OP_Name[TOP_vpavgwxxx] = "vpavgw";
  OP_Name[TOP_vblendv128v8] = "vpblendvb";
  OP_Name[TOP_vblendvx128v8] = "vpblendvb";
  OP_Name[TOP_vblendvxx128v8] = "vpblendvb";
  OP_Name[TOP_vblendvxxx128v8] = "vpblendvb";
  OP_Name[TOP_vblend128v16] = "vpblendw";
  OP_Name[TOP_vblendx128v16] = "vpblendw";
  OP_Name[TOP_vblendxx128v16] = "vpblendw";
  OP_Name[TOP_vblendxxx128v16] = "vpblendw";
  OP_Name[TOP_vcmpeq128v8] = "vpcmpeqb";
  OP_Name[TOP_vcmpeqx128v8] = "vpcmpeqb";
  OP_Name[TOP_vcmpeqxx128v8] = "vpcmpeqb";
  OP_Name[TOP_vcmpeqxxx128v8] = "vpcmpeqb";
  OP_Name[TOP_vcmpeq128v32] = "vpcmpeqd";
  OP_Name[TOP_vcmpeqx128v32] = "vpcmpeqd";
  OP_Name[TOP_vcmpeqxx128v32] = "vpcmpeqd";
  OP_Name[TOP_vcmpeqxxx128v32] = "vpcmpeqd";
  OP_Name[TOP_vcmpeq128v64] = "vpcmpeqq";
  OP_Name[TOP_vcmpeqx128v64] = "vpcmpeqq";
  OP_Name[TOP_vcmpeqxx128v64] = "vpcmpeqq";
  OP_Name[TOP_vcmpeqxxx128v64] = "vpcmpeqq";
  OP_Name[TOP_vcmpeq128v16] = "vpcmpeqw";
  OP_Name[TOP_vcmpeqx128v16] = "vpcmpeqw";
  OP_Name[TOP_vcmpeqxx128v16] = "vpcmpeqw";
  OP_Name[TOP_vcmpeqxxx128v16] = "vpcmpeqw";
  OP_Name[TOP_vcmpestri] = "vpcmpestri";
  OP_Name[TOP_vcmpestrix] = "vpcmpestri";
  OP_Name[TOP_vcmpestrixx] = "vpcmpestri";
  OP_Name[TOP_vcmpestrixxx] = "vpcmpestri";
  OP_Name[TOP_vcmpestrm] = "vpcmpestrm";
  OP_Name[TOP_vcmpestrmx] = "vpcmpestrm";
  OP_Name[TOP_vcmpestrmxx] = "vpcmpestrm";
  OP_Name[TOP_vcmpestrmxxx] = "vpcmpestrm";
  OP_Name[TOP_vcmpgt128v8] = "vpcmpgtb";
  OP_Name[TOP_vcmpgtx128v8] = "vpcmpgtb";
  OP_Name[TOP_vcmpgtxx128v8] = "vpcmpgtb";
  OP_Name[TOP_vcmpgtxxx128v8] = "vpcmpgtb";
  OP_Name[TOP_vcmpgt128v32] = "vpcmpgtd";
  OP_Name[TOP_vcmpgtx128v32] = "vpcmpgtd";
  OP_Name[TOP_vcmpgtxx128v32] = "vpcmpgtd";
  OP_Name[TOP_vcmpgtxxx128v32] = "vpcmpgtd";
  OP_Name[TOP_vcmpgt128v64] = "vpcmpgtq";
  OP_Name[TOP_vcmpgtx128v64] = "vpcmpgtq";
  OP_Name[TOP_vcmpgtxx128v64] = "vpcmpgtq";
  OP_Name[TOP_vcmpgtxxx128v64] = "vpcmpgtq";
  OP_Name[TOP_vcmpgt128v16] = "vpcmpgtw";
  OP_Name[TOP_vcmpgtx128v16] = "vpcmpgtw";
  OP_Name[TOP_vcmpgtxx128v16] = "vpcmpgtw";
  OP_Name[TOP_vcmpgtxxx128v16] = "vpcmpgtw";
  OP_Name[TOP_vcmpistri] = "vpcmpistri";
  OP_Name[TOP_vcmpistrix] = "vpcmpistri";
  OP_Name[TOP_vcmpistrixx] = "vpcmpistri";
  OP_Name[TOP_vcmpistrixxx] = "vpcmpistri";
  OP_Name[TOP_vcmpistrm] = "vpcmpistrm";
  OP_Name[TOP_vcmpistrmx] = "vpcmpistrm";
  OP_Name[TOP_vcmpistrmxx] = "vpcmpistrm";
  OP_Name[TOP_vcmpistrmxxx] = "vpcmpistrm";
  OP_Name[TOP_vfperm128v64] = "vpermilpd";
  OP_Name[TOP_vfpermx128v64] = "vpermilpd";
  OP_Name[TOP_vfpermxx128v64] = "vpermilpd";
  OP_Name[TOP_vfpermxxx128v64] = "vpermilpd";
  OP_Name[TOP_vfpermi128v64] = "vpermilpd";
  OP_Name[TOP_vfpermix128v64] = "vpermilpd";
  OP_Name[TOP_vfpermixx128v64] = "vpermilpd";
  OP_Name[TOP_vfpermixxx128v64] = "vpermilpd";
  OP_Name[TOP_vfperm128v32] = "vpermilps";
  OP_Name[TOP_vfpermx128v32] = "vpermilps";
  OP_Name[TOP_vfpermxx128v32] = "vpermilps";
  OP_Name[TOP_vfpermxxx128v32] = "vpermilps";
  OP_Name[TOP_vfpermi128v32] = "vpermilps";
  OP_Name[TOP_vfpermix128v32] = "vpermilps";
  OP_Name[TOP_vfpermixx128v32] = "vpermilps";
  OP_Name[TOP_vfpermixxx128v32] = "vpermilps";
  OP_Name[TOP_vfperm2f128] = "vperm2f128";
  OP_Name[TOP_vfperm2xf128] = "vperm2f128";
  OP_Name[TOP_vfperm2xxf128] = "vperm2f128";
  OP_Name[TOP_vfperm2xxxf128] = "vperm2f128";
  OP_Name[TOP_vextr128v8] = "vpextrb";
  OP_Name[TOP_vextrx128v8] = "vpextrb";
  OP_Name[TOP_vextrxx128v8] = "vpextrb";
  OP_Name[TOP_vextrxxx128v8] = "vpextrb";
  OP_Name[TOP_vextr128v32] = "vpextrd";
  OP_Name[TOP_vextrx128v32] = "vpextrd";
  OP_Name[TOP_vextrxx128v32] = "vpextrd";
  OP_Name[TOP_vextrxxx128v32] = "vpextrd";
  OP_Name[TOP_vextr128v64] = "vpextrq";
  OP_Name[TOP_vextrx128v64] = "vpextrq";
  OP_Name[TOP_vextrxx128v64] = "vpextrq";
  OP_Name[TOP_vextrxxx128v64] = "vpextrq";
  OP_Name[TOP_vextr128v16] = "vpextrw";
  OP_Name[TOP_vextrx128v16] = "vpextrw";
  OP_Name[TOP_vextrxx128v16] = "vpextrw";
  OP_Name[TOP_vextrxxx128v16] = "vpextrw";
  OP_Name[TOP_vphadd128v32] = "vphaddd";
  OP_Name[TOP_vphaddx128v32] = "vphaddd";
  OP_Name[TOP_vphaddxx128v32] = "vphaddd";
  OP_Name[TOP_vphaddxxx128v32] = "vphaddd";
  OP_Name[TOP_vphadds128v16] = "vphaddsw";
  OP_Name[TOP_vphaddsx128v16] = "vphaddsw";
  OP_Name[TOP_vphaddsxx128v16] = "vphaddsw";
  OP_Name[TOP_vphaddsxxx128v16] = "vphaddsw";
  OP_Name[TOP_vphadd128v16] = "vphaddw";
  OP_Name[TOP_vphaddx128v16] = "vphaddw";
  OP_Name[TOP_vphaddxx128v16] = "vphaddw";
  OP_Name[TOP_vphaddxxx128v16] = "vphaddw";
  OP_Name[TOP_vphminposuw] = "vhminposuw";
  OP_Name[TOP_vphminposuwx] = "vhminposuw";
  OP_Name[TOP_vphminposuwxx] = "vhminposuw";
  OP_Name[TOP_vphminposuwxxx] = "vhminposuw";
  OP_Name[TOP_vphsub128v32] = "vphsubd";
  OP_Name[TOP_vphsubx128v32] = "vphsubd";
  OP_Name[TOP_vphsubxx128v32] = "vphsubd";
  OP_Name[TOP_vphsubxxx128v32] = "vphsubd";
  OP_Name[TOP_vphsubs128v16] = "vphsubsw";
  OP_Name[TOP_vphsubsx128v16] = "vphsubsw";
  OP_Name[TOP_vphsubsxx128v16] = "vphsubsw";
  OP_Name[TOP_vphsubsxxx128v16] = "vphsubsw";
  OP_Name[TOP_vphsub128v16] = "vphsubw";
  OP_Name[TOP_vphsubx128v16] = "vphsubw";
  OP_Name[TOP_vphsubxx128v16] = "vphsubw";
  OP_Name[TOP_vphsubxxx128v16] = "vphsubw";
  OP_Name[TOP_vinsr128v8] = "vpinsrb";
  OP_Name[TOP_vinsrx128v8] = "vpinsrb";
  OP_Name[TOP_vinsrxx128v8] = "vpinsrb";
  OP_Name[TOP_vinsrxxx128v8] = "vpinsrb";
  OP_Name[TOP_vinsr128v32] = "vpinsrd";
  OP_Name[TOP_vinsrx128v32] = "vpinsrd";
  OP_Name[TOP_vinsrxx128v32] = "vpinsrd";
  OP_Name[TOP_vinsrxxx128v32] = "vpinsrd";
  OP_Name[TOP_vinsr128v64] = "vpinsrq";
  OP_Name[TOP_vinsrx128v64] = "vpinsrq";
  OP_Name[TOP_vinsrxx128v64] = "vpinsrq";
  OP_Name[TOP_vinsrxxx128v64] = "vpinsrq";
  OP_Name[TOP_vinsr128v16] = "vpinsrw";
  OP_Name[TOP_vinsrx128v16] = "vpinsrw";
  OP_Name[TOP_vinsrxx128v16] = "vpinsrw";
  OP_Name[TOP_vinsrxxx128v16] = "vpinsrw";
  OP_Name[TOP_vpmaddwd] = "vpmaddwd";
  OP_Name[TOP_vpmaddwdx] = "vpmaddwd";
  OP_Name[TOP_vpmaddwdxx] = "vpmaddwd";
  OP_Name[TOP_vpmaddwdxxx] = "vpmaddwd";
  OP_Name[TOP_vpmaddubsw128] = "vpmaddubsw";
  OP_Name[TOP_vpmaddubswx128] = "vpmaddubsw";
  OP_Name[TOP_vpmaddubswxx128] = "vpmaddubsw";
  OP_Name[TOP_vpmaddubswxxx128] = "vpmaddubsw";
  OP_Name[TOP_vmaxs128v8] = "vpmaxsb";
  OP_Name[TOP_vmaxsx128v8] = "vpmaxsb";
  OP_Name[TOP_vmaxsxx128v8] = "vpmaxsb";
  OP_Name[TOP_vmaxsxxx128v8] = "vpmaxsb";
  OP_Name[TOP_vmaxs128v16] = "vpmaxsw";
  OP_Name[TOP_vmaxsx128v16] = "vpmaxsw";
  OP_Name[TOP_vmaxsxx128v16] = "vpmaxsw";
  OP_Name[TOP_vmaxsxxx128v16] = "vpmaxsw";
  OP_Name[TOP_vmaxs128v32] = "vpmaxsd";
  OP_Name[TOP_vmaxsx128v32] = "vpmaxsd";
  OP_Name[TOP_vmaxsxx128v32] = "vpmaxsd";
  OP_Name[TOP_vmaxsxxx128v32] = "vpmaxsd";
  OP_Name[TOP_vmaxu128v8] = "vpmaxub";
  OP_Name[TOP_vmaxux128v8] = "vpmaxub";
  OP_Name[TOP_vmaxuxx128v8] = "vpmaxub";
  OP_Name[TOP_vmaxuxxx128v8] = "vpmaxub";
  OP_Name[TOP_vmaxu128v16] = "vpmaxuw";
  OP_Name[TOP_vmaxux128v16] = "vpmaxuw";
  OP_Name[TOP_vmaxuxx128v16] = "vpmaxuw";
  OP_Name[TOP_vmaxuxxx128v16] = "vpmaxuw";
  OP_Name[TOP_vmaxu128v32] = "vpmaxud";
  OP_Name[TOP_vmaxux128v32] = "vpmaxud";
  OP_Name[TOP_vmaxuxx128v32] = "vpmaxud";
  OP_Name[TOP_vmaxuxxx128v32] = "vpmaxud";
  OP_Name[TOP_vmins128v8] = "vpminsb";
  OP_Name[TOP_vminsx128v8] = "vpminsb";
  OP_Name[TOP_vminsxx128v8] = "vpminsb";
  OP_Name[TOP_vminsxxx128v8] = "vpminsb";
  OP_Name[TOP_vmins128v16] = "vpminsw";
  OP_Name[TOP_vminsx128v16] = "vpminsw";
  OP_Name[TOP_vminsxx128v16] = "vpminsw";
  OP_Name[TOP_vminsxxx128v16] = "vpminsw";
  OP_Name[TOP_vmins128v32] = "vpminsd";
  OP_Name[TOP_vminsx128v32] = "vpminsd";
  OP_Name[TOP_vminsxx128v32] = "vpminsd";
  OP_Name[TOP_vminsxxx128v32] = "vpminsd";
  OP_Name[TOP_vminu128v8] = "vpminub";
  OP_Name[TOP_vminux128v8] = "vpminub";
  OP_Name[TOP_vminuxx128v8] = "vpminub";
  OP_Name[TOP_vminuxxx128v8] = "vpminub";
  OP_Name[TOP_vminu128v16] = "vpminuw";
  OP_Name[TOP_vminux128v16] = "vpminuw";
  OP_Name[TOP_vminuxx128v16] = "vpminuw";
  OP_Name[TOP_vminuxxx128v16] = "vpminuw";
  OP_Name[TOP_vminu128v32] = "vpminud";
  OP_Name[TOP_vminux128v32] = "vpminud";
  OP_Name[TOP_vminuxx128v32] = "vpminud";
  OP_Name[TOP_vminuxxx128v32] = "vpminud";
  OP_Name[TOP_vpmovmskb128] = "vpmovmskb";
  OP_Name[TOP_vpmovsxbd] = "vpmovsxbd";
  OP_Name[TOP_vpmovsxbdx] = "vpmovsxbd";
  OP_Name[TOP_vpmovsxbdxx] = "vpmovsxbd";
  OP_Name[TOP_vpmovsxbdxxx] = "vpmovsxbd";
  OP_Name[TOP_vpmovsxbq] = "vpmovsxbq";
  OP_Name[TOP_vpmovsxbqx] = "vpmovsxbq";
  OP_Name[TOP_vpmovsxbqxx] = "vpmovsxbq";
  OP_Name[TOP_vpmovsxbqxxx] = "vpmovsxbq";
  OP_Name[TOP_vpmovsxbw] = "vpmovsxbw";
  OP_Name[TOP_vpmovsxbwx] = "vpmovsxbw";
  OP_Name[TOP_vpmovsxbwxx] = "vpmovsxbw";
  OP_Name[TOP_vpmovsxbwxxx] = "vpmovsxbw";
  OP_Name[TOP_vpmovsxdq] = "vpmovsxdq";
  OP_Name[TOP_vpmovsxdqx] = "vpmovsxdq";
  OP_Name[TOP_vpmovsxdqxx] = "vpmovsxdq";
  OP_Name[TOP_vpmovsxdqxxx] = "vpmovsxdq";
  OP_Name[TOP_vpmovsxwd] = "vpmovsxwd";
  OP_Name[TOP_vpmovsxwdx] = "vpmovsxwd";
  OP_Name[TOP_vpmovsxwdxx] = "vpmovsxwd";
  OP_Name[TOP_vpmovsxwdxxx] = "vpmovsxwd";
  OP_Name[TOP_vpmovsxwq] = "vpmovsxwq";
  OP_Name[TOP_vpmovsxwqx] = "vpmovsxwq";
  OP_Name[TOP_vpmovsxwqxx] = "vpmovsxwq";
  OP_Name[TOP_vpmovsxwqxxx] = "vpmovsxwq";
  OP_Name[TOP_vpmovzxbd] = "vpmovzxbd";
  OP_Name[TOP_vpmovzxbdx] = "vpmovzxbd";
  OP_Name[TOP_vpmovzxbdxx] = "vpmovzxbd";
  OP_Name[TOP_vpmovzxbdxxx] = "vpmovzxbd";
  OP_Name[TOP_vpmovzxbq] = "vpmovzxbq";
  OP_Name[TOP_vpmovzxbqx] = "vpmovzxbq";
  OP_Name[TOP_vpmovzxbqxx] = "vpmovzxbq";
  OP_Name[TOP_vpmovzxbqxxx] = "vpmovzxbq";
  OP_Name[TOP_vpmovzxbw] = "vpmovzxbw";
  OP_Name[TOP_vpmovzxbwx] = "vpmovzxbw";
  OP_Name[TOP_vpmovzxbwxx] = "vpmovzxbw";
  OP_Name[TOP_vpmovzxbwxxx] = "vpmovzxbw";
  OP_Name[TOP_vpmovzxdq] = "vpmovzxdq";
  OP_Name[TOP_vpmovzxdqx] = "vpmovzxdq";
  OP_Name[TOP_vpmovzxdqxx] = "vpmovzxdq";
  OP_Name[TOP_vpmovzxdqxxx] = "vpmovzxdq";
  OP_Name[TOP_vpmovzxwd] = "vpmovzxwd";
  OP_Name[TOP_vpmovzxwdx] = "vpmovzxwd";
  OP_Name[TOP_vpmovzxwdxx] = "vpmovzxwd";
  OP_Name[TOP_vpmovzxwdxxx] = "vpmovzxwd";
  OP_Name[TOP_vpmovzxwq] = "vpmovzxwq";
  OP_Name[TOP_vpmovzxwqx] = "vpmovzxwq";
  OP_Name[TOP_vpmovzxwqxx] = "vpmovzxwq";
  OP_Name[TOP_vpmovzxwqxxx] = "vpmovzxwq";
  OP_Name[TOP_vmulhuw] = "vpmulhuw";
  OP_Name[TOP_vmulhuwx] = "vpmulhuw";
  OP_Name[TOP_vmulhuwxx] = "vpmulhuw";
  OP_Name[TOP_vmulhuwxxx] = "vpmulhuw";
  OP_Name[TOP_vmulhrsw] = "vpmulhrsw";
  OP_Name[TOP_vmulhrswx] = "vpmulhrsw";
  OP_Name[TOP_vmulhrswxx] = "vpmulhrsw";
  OP_Name[TOP_vmulhrswxxx] = "vpmulhrsw";
  OP_Name[TOP_vmulhw] = "vpmulhw";
  OP_Name[TOP_vmulhwx] = "vpmulhw";
  OP_Name[TOP_vmulhwxx] = "vpmulhw";
  OP_Name[TOP_vmulhwxxx] = "vpmulhw";
  OP_Name[TOP_vmulld] = "vpmulld";
  OP_Name[TOP_vmulldx] = "vpmulld";
  OP_Name[TOP_vmulldxx] = "vpmulld";
  OP_Name[TOP_vmulldxxx] = "vpmulld";
  OP_Name[TOP_vmul128v16] = "vpmullw";
  OP_Name[TOP_vmulx128v16] = "vpmullw";
  OP_Name[TOP_vmulxx128v16] = "vpmullw";
  OP_Name[TOP_vmulxxx128v16] = "vpmullw";
  OP_Name[TOP_vmul128v32] = "vpmulld";
  OP_Name[TOP_vmulx128v32] = "vpmulld";
  OP_Name[TOP_vmulxx128v32] = "vpmulld";
  OP_Name[TOP_vmulxxx128v32] = "vpmulld";
  OP_Name[TOP_vmuludq] = "vpmuludq";
  OP_Name[TOP_vmuludqx] = "vpmuludq";
  OP_Name[TOP_vmuludqxx] = "vpmuludq";
  OP_Name[TOP_vmuludqxxx] = "vpmuludq";
  OP_Name[TOP_vmuldq] = "vpmuldq";
  OP_Name[TOP_vmuldqx] = "vpmuldq";
  OP_Name[TOP_vmuldqxx] = "vpmuldq";
  OP_Name[TOP_vmuldqxxx] = "vpmuldq";
  OP_Name[TOP_vor128v8] = "vpor";
  OP_Name[TOP_vorx128v8] = "vpor";
  OP_Name[TOP_vorxx128v8] = "vpor";
  OP_Name[TOP_vorxxx128v8] = "vpor";
  OP_Name[TOP_vor128v16] = "vpor";
  OP_Name[TOP_vorx128v16] = "vpor";
  OP_Name[TOP_vorxx128v16] = "vpor";
  OP_Name[TOP_vorxxx128v16] = "vpor";
  OP_Name[TOP_vor128v32] = "vpor";
  OP_Name[TOP_vorx128v32] = "vpor";
  OP_Name[TOP_vorxx128v32] = "vpor";
  OP_Name[TOP_vorxxx128v32] = "vpor";
  OP_Name[TOP_vor128v64] = "vpor";
  OP_Name[TOP_vorx128v64] = "vpor";
  OP_Name[TOP_vorxx128v64] = "vpor";
  OP_Name[TOP_vorxxx128v64] = "vpor";
  OP_Name[TOP_vpsadbw] = "vpsadbw";
  OP_Name[TOP_vpsadbwx] = "vpsadbw";
  OP_Name[TOP_vpsadbwxx] = "vpsadbw";
  OP_Name[TOP_vpsadbwxxx] = "vpsadbw";
  OP_Name[TOP_vpshuf128v8] = "vpshufb";
  OP_Name[TOP_vpshufx128v8] = "vpshufb";
  OP_Name[TOP_vpshufxx128v8] = "vpshufb";
  OP_Name[TOP_vpshufxxx128v8] = "vpshufb";
  OP_Name[TOP_vpshuf128v32] = "vpshufd";
  OP_Name[TOP_vpshufx128v32] = "vpshufd";
  OP_Name[TOP_vpshufxx128v32] = "vpshufd";
  OP_Name[TOP_vpshufxxx128v32] = "vpshufd";
  OP_Name[TOP_vpshufw64v16] = "vpshufw";
  OP_Name[TOP_vpshufwx64v16] = "vpshufw";
  OP_Name[TOP_vpshufwxx64v16] = "vpshufw";
  OP_Name[TOP_vpshufwxxx64v16] = "vpshufw";
  OP_Name[TOP_vpshufhw] = "vpshufhw";
  OP_Name[TOP_vpshufhwx] = "vpshufhw";
  OP_Name[TOP_vpshufhwxx] = "vpshufhw";
  OP_Name[TOP_vpshufhwxxx] = "vpshufhw";
  OP_Name[TOP_vpshuflw] = "vpshuflw";
  OP_Name[TOP_vpshuflwx] = "vpshuflw";
  OP_Name[TOP_vpshuflwxx] = "vpshuflw";
  OP_Name[TOP_vpshuflwxxx] = "vpshuflw";
  OP_Name[TOP_vpsign128v8] = "vpsignb";
  OP_Name[TOP_vpsignx128v8] = "vpsignb";
  OP_Name[TOP_vpsignxx128v8] = "vpsignb";
  OP_Name[TOP_vpsignxxx128v8] = "vpsignb";
  OP_Name[TOP_vpsign128v32] = "vpsignd";
  OP_Name[TOP_vpsignx128v32] = "vpsignd";
  OP_Name[TOP_vpsignxx128v32] = "vpsignd";
  OP_Name[TOP_vpsignxxx128v32] = "vpsignd";
  OP_Name[TOP_vpsign128v16] = "vpsignw";
  OP_Name[TOP_vpsignx128v16] = "vpsignw";
  OP_Name[TOP_vpsignxx128v16] = "vpsignw";
  OP_Name[TOP_vpsignxxx128v16] = "vpsignw";
  OP_Name[TOP_vpslldq] = "vpslldq";
  OP_Name[TOP_vpsrldq] = "vpsrldq";
  OP_Name[TOP_vpslld] = "vpslld";
  OP_Name[TOP_vpslldx] = "vpslld";
  OP_Name[TOP_vpslldxx] = "vpslld";
  OP_Name[TOP_vpslldxxx] = "vpslld";
  OP_Name[TOP_vpslldi] = "vpslld";
  OP_Name[TOP_vpsllq] = "vpsllq";
  OP_Name[TOP_vpsllqx] = "vpsllq";
  OP_Name[TOP_vpsllqxx] = "vpsllq";
  OP_Name[TOP_vpsllqxxx] = "vpsllq";
  OP_Name[TOP_vpsllqi] = "vpsllq";
  OP_Name[TOP_vpsllw] = "vpsllw";
  OP_Name[TOP_vpsllwx] = "vpsllw";
  OP_Name[TOP_vpsllwxx] = "vpsllw";
  OP_Name[TOP_vpsllwxxx] = "vpsllw";
  OP_Name[TOP_vpsllwi] = "vpsllw";
  OP_Name[TOP_vpsrad] = "vpsrad";
  OP_Name[TOP_vpsradx] = "vpsrad";
  OP_Name[TOP_vpsradxx] = "vpsrad";
  OP_Name[TOP_vpsradxxx] = "vpsrad";
  OP_Name[TOP_vpsradi] = "vpsrad";
  OP_Name[TOP_vpsraw] = "vpsraw";
  OP_Name[TOP_vpsrawx] = "vpsraw";
  OP_Name[TOP_vpsrawxx] = "vpsraw";
  OP_Name[TOP_vpsrawxxx] = "vpsraw";
  OP_Name[TOP_vpsrawi] = "vpsraw";
  OP_Name[TOP_vpsrld] = "vpsrld";
  OP_Name[TOP_vpsrldx] = "vpsrld";
  OP_Name[TOP_vpsrldxx] = "vpsrld";
  OP_Name[TOP_vpsrldxxx] = "vpsrld";
  OP_Name[TOP_vpsrldi] = "vpsrld";
  OP_Name[TOP_vpsrlq] = "vpsrlq";
  OP_Name[TOP_vpsrlqx] = "vpsrlq";
  OP_Name[TOP_vpsrlqxx] = "vpsrlq";
  OP_Name[TOP_vpsrlqxxx] = "vpsrlq";
  OP_Name[TOP_vpsrlqi] = "vpsrlq";
  OP_Name[TOP_vpsrlw] = "vpsrlw";
  OP_Name[TOP_vpsrlwx] = "vpsrlw";
  OP_Name[TOP_vpsrlwxx] = "vpsrlw";
  OP_Name[TOP_vpsrlwxxx] = "vpsrlw";
  OP_Name[TOP_vpsrlwi] = "vpsrlw";
  OP_Name[TOP_vsub128v8] = "vpsubb";
  OP_Name[TOP_vsubx128v8] = "vpsubb";
  OP_Name[TOP_vsubxx128v8] = "vpsubb";
  OP_Name[TOP_vsubxxx128v8] = "vpsubb";
  OP_Name[TOP_vsub128v32] = "vpsubd";
  OP_Name[TOP_vsubx128v32] = "vpsubd";
  OP_Name[TOP_vsubxx128v32] = "vpsubd";
  OP_Name[TOP_vsubxxx128v32] = "vpsubd";
  OP_Name[TOP_vsub128v64] = "vpsubq";
  OP_Name[TOP_vsubx128v64] = "vpsubq";
  OP_Name[TOP_vsubxx128v64] = "vpsubq";
  OP_Name[TOP_vsubxxx128v64] = "vpsubq";
  OP_Name[TOP_vsub128v16] = "vpsubw";
  OP_Name[TOP_vsubx128v16] = "vpsubw";
  OP_Name[TOP_vsubxx128v16] = "vpsubw";
  OP_Name[TOP_vsubxxx128v16] = "vpsubw";
  OP_Name[TOP_vsubs128v8] = "vpsubsb";
  OP_Name[TOP_vsubsx128v8] = "vpsubsb";
  OP_Name[TOP_vsubsxx128v8] = "vpsubsb";
  OP_Name[TOP_vsubsxxx128v8] = "vpsubsb";
  OP_Name[TOP_vsubs128v16] = "vpsubsw";
  OP_Name[TOP_vsubsx128v16] = "vpsubsw";
  OP_Name[TOP_vsubsxx128v16] = "vpsubsw";
  OP_Name[TOP_vsubsxxx128v16] = "vpsubsw";
  OP_Name[TOP_vsubus128v8] = "vpsubusb";
  OP_Name[TOP_vsubusx128v8] = "vpsubusb";
  OP_Name[TOP_vsubusxx128v8] = "vpsubusb";
  OP_Name[TOP_vsubusxxx128v8] = "vpsubusb";
  OP_Name[TOP_vsubus128v16] = "vpsubusw";
  OP_Name[TOP_vsubusx128v16] = "vpsubusw";
  OP_Name[TOP_vsubusxx128v16] = "vpsubusw";
  OP_Name[TOP_vsubusxxx128v16] = "vpsubusw";
  OP_Name[TOP_vptest128] = "vptest";
  OP_Name[TOP_vptestx128] = "vptest";
  OP_Name[TOP_vptestxx128] = "vptest";
  OP_Name[TOP_vptestxxx128] = "vptest";
  OP_Name[TOP_vpunpckh64v8] = "vpunpckhbw";
  OP_Name[TOP_vpunpckhx64v8] = "vpunpckhbw";
  OP_Name[TOP_vpunpckhxx64v8] = "vpunpckhbw";
  OP_Name[TOP_vpunpckhxxx64v8] = "vpunpckhbw";
  OP_Name[TOP_vpunpckh64v32] = "vpunpckhdq";
  OP_Name[TOP_vpunpckhx64v32] = "vpunpckhdq";
  OP_Name[TOP_vpunpckhxx64v32] = "vpunpckhdq";
  OP_Name[TOP_vpunpckhxxx64v32] = "vpunpckhdq";
  OP_Name[TOP_vpunpckh64v16] = "vpunpckhwd";
  OP_Name[TOP_vpunpckhx64v16] = "vpunpckhwd";
  OP_Name[TOP_vpunpckhxx64v16] = "vpunpckhwd";
  OP_Name[TOP_vpunpckhxxx64v16] = "vpunpckhwd";
  OP_Name[TOP_vpunpckh64v64] = "vpunpckhqdq";
  OP_Name[TOP_vpunpckhx64v64] = "vpunpckhqdq";
  OP_Name[TOP_vpunpckhxx64v64] = "vpunpckhqdq";
  OP_Name[TOP_vpunpckhxxx64v64] = "vpunpckhqdq";
  OP_Name[TOP_vpunpckl64v8] = "vpunpcklbw";
  OP_Name[TOP_vpunpcklx64v8] = "vpunpcklbw";
  OP_Name[TOP_vpunpcklxx64v8] = "vpunpcklbw";
  OP_Name[TOP_vpunpcklxxx64v8] = "vpunpcklbw";
  OP_Name[TOP_vpunpckl64v32] = "vpunpckldq";
  OP_Name[TOP_vpunpcklx64v32] = "vpunpckldq";
  OP_Name[TOP_vpunpcklxx64v32] = "vpunpckldq";
  OP_Name[TOP_vpunpcklxxx64v32] = "vpunpckldq";
  OP_Name[TOP_vpunpckl64v16] = "vpunpcklwd";
  OP_Name[TOP_vpunpcklx64v16] = "vpunpcklwd";
  OP_Name[TOP_vpunpcklxx64v16] = "vpunpcklwd";
  OP_Name[TOP_vpunpcklxxx64v16] = "vpunpcklwd";
  OP_Name[TOP_vpunpckl64v64] = "vpunpcklqdq";
  OP_Name[TOP_vpunpcklx64v64] = "vpunpcklqdq";
  OP_Name[TOP_vpunpcklxx64v64] = "vpunpcklqdq";
  OP_Name[TOP_vpunpcklxxx64v64] = "vpunpcklqdq";
  OP_Name[TOP_vxor128v8] = "vpxor";
  OP_Name[TOP_vxorx128v8] = "vpxor";
  OP_Name[TOP_vxorxx128v8] = "vpxor";
  OP_Name[TOP_vxorxxx128v8] = "vpxor";
  OP_Name[TOP_vxor128v16] = "vpxor";
  OP_Name[TOP_vxorx128v16] = "vpxor";
  OP_Name[TOP_vxorxx128v16] = "vpxor";
  OP_Name[TOP_vxorxxx128v16] = "vpxor";
  OP_Name[TOP_vxor128v32] = "vpxor";
  OP_Name[TOP_vxorx128v32] = "vpxor";
  OP_Name[TOP_vxorxx128v32] = "vpxor";
  OP_Name[TOP_vxorxxx128v32] = "vpxor";
  OP_Name[TOP_vxor128v64] = "vpxor";
  OP_Name[TOP_vxorx128v64] = "vpxor";
  OP_Name[TOP_vxorxx128v64] = "vpxor";
  OP_Name[TOP_vxorxxx128v64] = "vpxor";
  OP_Name[TOP_vfrcp128v32] = "vrcpps";
  OP_Name[TOP_vfrcpx128v32] = "vrcpps";
  OP_Name[TOP_vfrcpxx128v32] = "vrcpps";
  OP_Name[TOP_vfrcpxxx128v32] = "vrcpps";
  OP_Name[TOP_vfrcpss] = "vrcpss";
  OP_Name[TOP_vfrcpxss] = "vrcpss";
  OP_Name[TOP_vfrcpxxss] = "vrcpss";
  OP_Name[TOP_vfrcpxxxss] = "vrcpss";
  OP_Name[TOP_vround128v64] = "vroundpd";
  OP_Name[TOP_vroundx128v64] = "vroundpd";
  OP_Name[TOP_vroundxx128v64] = "vroundpd";
  OP_Name[TOP_vroundxxx128v64] = "vroundpd";
  OP_Name[TOP_vround128v32] = "vroundps";
  OP_Name[TOP_vroundx128v32] = "vroundps";
  OP_Name[TOP_vroundxx128v32] = "vroundps";
  OP_Name[TOP_vroundxxx128v32] = "vroundps";
  OP_Name[TOP_vroundsd] = "vroundsd";
  OP_Name[TOP_vroundxsd] = "vroundsd";
  OP_Name[TOP_vroundxxsd] = "vroundsd";
  OP_Name[TOP_vroundxxxsd] = "vroundsd";
  OP_Name[TOP_vroundss] = "vroundss";
  OP_Name[TOP_vroundxss] = "vroundss";
  OP_Name[TOP_vroundxxss] = "vroundss";
  OP_Name[TOP_vroundxxxss] = "vroundss";
  OP_Name[TOP_vfrsqrt128v32] = "vrsqrtps";
  OP_Name[TOP_vfrsqrtx128v32] = "vrsqrtps";
  OP_Name[TOP_vfrsqrtxx128v32] = "vrsqrtps";
  OP_Name[TOP_vfrsqrtxxx128v32] = "vrsqrtps";
  OP_Name[TOP_vfrsqrtss] = "vrsqrtss";
  OP_Name[TOP_vfrsqrtxss] = "vrsqrtss";
  OP_Name[TOP_vfrsqrtxxss] = "vrsqrtss";
  OP_Name[TOP_vfrsqrtxxxss] = "vrsqrtss";
  OP_Name[TOP_vfshuf128v64] = "vshufpd";
  OP_Name[TOP_vfshufx128v64] = "vshufpd";
  OP_Name[TOP_vfshufxx128v64] = "vshufpd";
  OP_Name[TOP_vfshufxxx128v64] = "vshufpd";
  OP_Name[TOP_vfshuf128v32] = "vshufps";
  OP_Name[TOP_vfshufx128v32] = "vshufps";
  OP_Name[TOP_vfshufxx128v32] = "vshufps";
  OP_Name[TOP_vfshufxxx128v32] = "vshufps";
  OP_Name[TOP_vfsqrt128v64] = "vsqrtpd";
  OP_Name[TOP_vfsqrtx128v64] = "vsqrtpd";
  OP_Name[TOP_vfsqrtxx128v64] = "vsqrtpd";
  OP_Name[TOP_vfsqrtxxx128v64] = "vsqrtpd";
  OP_Name[TOP_vfsqrt128v32] = "vsqrtps";
  OP_Name[TOP_vfsqrtx128v32] = "vsqrtps";
  OP_Name[TOP_vfsqrtxx128v32] = "vsqrtps";
  OP_Name[TOP_vfsqrtxxx128v32] = "vsqrtps";
  OP_Name[TOP_vfsqrtsd] = "vsqrtsd";
  OP_Name[TOP_vfsqrtxsd] = "vsqrtsd";
  OP_Name[TOP_vfsqrtxxsd] = "vsqrtsd";
  OP_Name[TOP_vfsqrtxxxsd] = "vsqrtsd";
  OP_Name[TOP_vfsqrtss] = "vsqrtss";
  OP_Name[TOP_vfsqrtxss] = "vsqrtss";
  OP_Name[TOP_vfsqrtxxss] = "vsqrtss";
  OP_Name[TOP_vfsqrtxxxss] = "vsqrtss";
  OP_Name[TOP_vstmxcsr] = "vstmxcsr";
  OP_Name[TOP_vfsub128v64] = "vsubpd";
  OP_Name[TOP_vfsubx128v64] = "vsubpd";
  OP_Name[TOP_vfsubxx128v64] = "vsubpd";
  OP_Name[TOP_vfsubxxx128v64] = "vsubpd";
  OP_Name[TOP_vfsub128v32] = "vsubps";
  OP_Name[TOP_vfsubx128v32] = "vsubps";
  OP_Name[TOP_vfsubxx128v32] = "vsubps";
  OP_Name[TOP_vfsubxxx128v32] = "vsubps";
  OP_Name[TOP_vsubsd] = "vsubsd";
  OP_Name[TOP_vsubxsd] = "vsubsd";
  OP_Name[TOP_vsubxxsd] = "vsubsd";
  OP_Name[TOP_vsubxxxsd] = "vsubsd";
  OP_Name[TOP_vsubss] = "vsubss";
  OP_Name[TOP_vsubxss] = "vsubss";
  OP_Name[TOP_vsubxxss] = "vsubss";
  OP_Name[TOP_vsubxxxss] = "vsubss";
  OP_Name[TOP_vucomisd] = "vucomisd";
  OP_Name[TOP_vucomixsd] = "vucomisd";
  OP_Name[TOP_vucomixxsd] = "vucomisd";
  OP_Name[TOP_vucomixxxsd] = "vucomisd";
  OP_Name[TOP_vucomiss] = "vucomiss";
  OP_Name[TOP_vucomixss] = "vucomiss";
  OP_Name[TOP_vucomixxss] = "vucomiss";
  OP_Name[TOP_vucomixxxss] = "vucomiss";
  OP_Name[TOP_vunpckh128v64] = "vunpckhpd";
  OP_Name[TOP_vunpckhx128v64] = "vunpckhpd";
  OP_Name[TOP_vunpckhxx128v64] = "vunpckhpd";
  OP_Name[TOP_vunpckhxxx128v64] = "vunpckhpd";
  OP_Name[TOP_vunpckh128v32] = "vunpckhps";
  OP_Name[TOP_vunpckhx128v32] = "vunpckhps";
  OP_Name[TOP_vunpckhxx128v32] = "vunpckhps";
  OP_Name[TOP_vunpckhxxx128v32] = "vunpckhps";
  OP_Name[TOP_vunpckl128v64] = "vunpcklpd";
  OP_Name[TOP_vunpcklx128v64] = "vunpcklpd";
  OP_Name[TOP_vunpcklxx128v64] = "vunpcklpd";
  OP_Name[TOP_vunpcklxxx128v64] = "vunpcklpd";
  OP_Name[TOP_vunpckl128v32] = "vunpcklps";
  OP_Name[TOP_vunpcklx128v32] = "vunpcklps";
  OP_Name[TOP_vunpcklxx128v32] = "vunpcklps";
  OP_Name[TOP_vunpcklxxx128v32] = "vunpcklps";
  OP_Name[TOP_vfxor128v64] = "vxorpd";
  OP_Name[TOP_vfxorx128v64] = "vxorpd";
  OP_Name[TOP_vfxorxx128v64] = "vxorpd";
  OP_Name[TOP_vfxorxxx128v64] = "vxorpd";
  OP_Name[TOP_vxzero128v64] = "vxorpd";
  OP_Name[TOP_vxzero64] = "vxorpd";
  OP_Name[TOP_vfxor128v32] = "vxorps";
  OP_Name[TOP_vfxorx128v32] = "vxorps";
  OP_Name[TOP_vfxorxx128v32] = "vxorps";
  OP_Name[TOP_vfxorxxx128v32] = "vxorps";
  OP_Name[TOP_vxzero128v32] = "vxorps";
  OP_Name[TOP_vxzero32] = "vxorps";
  OP_Name[TOP_vzeroall] = "vzeroall";

  // start FMA3 additions
  OP_Name[TOP_xfmadd132pd] = "vfmadd132pd";
  OP_Name[TOP_xfmadd132xpd] = "vfmadd132pd";
  OP_Name[TOP_xfmadd132xxpd] = "vfmadd132pd";
  OP_Name[TOP_xfmadd132xxxpd] = "vfmadd132pd";
  OP_Name[TOP_xfmadd213pd] = "vfmadd213pd";
  OP_Name[TOP_xfmadd213xpd] = "vfmadd213pd";
  OP_Name[TOP_xfmadd213xxpd] = "vfmadd213pd";
  OP_Name[TOP_xfmadd213xxxpd] = "vfmadd213pd";
  OP_Name[TOP_xfmadd231pd] = "vfmadd231pd";
  OP_Name[TOP_xfmadd231xpd] = "vfmadd231pd";
  OP_Name[TOP_xfmadd231xxpd] = "vfmadd231pd";
  OP_Name[TOP_xfmadd231xxxpd] = "vfmadd231pd";
  OP_Name[TOP_xfmadd132ps] = "vfmadd132ps";
  OP_Name[TOP_xfmadd132xps] = "vfmadd132ps";
  OP_Name[TOP_xfmadd132xxps] = "vfmadd132ps";
  OP_Name[TOP_xfmadd132xxxps] = "vfmadd132ps";
  OP_Name[TOP_xfmadd213ps] = "vfmadd213ps";
  OP_Name[TOP_xfmadd213xps] = "vfmadd213ps";
  OP_Name[TOP_xfmadd213xxps] = "vfmadd213ps";
  OP_Name[TOP_xfmadd213xxxps] = "vfmadd213ps";
  OP_Name[TOP_xfmadd231ps] = "vfmadd231ps";
  OP_Name[TOP_xfmadd231xps] = "vfmadd231ps";
  OP_Name[TOP_xfmadd231xxps] = "vfmadd231ps";
  OP_Name[TOP_xfmadd231xxxps] = "vfmadd231ps";
  OP_Name[TOP_xfmadd132sd] = "vfmadd132sd";
  OP_Name[TOP_xfmadd132xsd] = "vfmadd132sd";
  OP_Name[TOP_xfmadd132xxsd] = "vfmadd132sd";
  OP_Name[TOP_xfmadd132xxxsd] = "vfmadd132sd";
  OP_Name[TOP_xfmadd213sd] = "vfmadd213sd";
  OP_Name[TOP_xfmadd213xsd] = "vfmadd213sd";
  OP_Name[TOP_xfmadd213xxsd] = "vfmadd213sd";
  OP_Name[TOP_xfmadd213xxxsd] = "vfmadd213sd";
  OP_Name[TOP_xfmadd231sd] = "vfmadd231sd";
  OP_Name[TOP_xfmadd231xsd] = "vfmadd231sd";
  OP_Name[TOP_xfmadd231xxsd] = "vfmadd231sd";
  OP_Name[TOP_xfmadd231xxxsd] = "vfmadd231sd";
  OP_Name[TOP_xfmadd132ss] = "vfmadd132ss";
  OP_Name[TOP_xfmadd132xss] = "vfmadd132ss";
  OP_Name[TOP_xfmadd132xxss] = "vfmadd132ss";
  OP_Name[TOP_xfmadd132xxxss] = "vfmadd132ss";
  OP_Name[TOP_xfmadd213ss] = "vfmadd213ss";
  OP_Name[TOP_xfmadd213xss] = "vfmadd213ss";
  OP_Name[TOP_xfmadd213xxss] = "vfmadd213ss";
  OP_Name[TOP_xfmadd213xxxss] = "vfmadd213ss";
  OP_Name[TOP_xfmadd231ss] = "vfmadd231ss";
  OP_Name[TOP_xfmadd231xss] = "vfmadd231ss";
  OP_Name[TOP_xfmadd231xxss] = "vfmadd231ss";
  OP_Name[TOP_xfmadd231xxxss] = "vfmadd231ss";
  OP_Name[TOP_xfmaddsub132pd] = "vfmaddsub132pd";
  OP_Name[TOP_xfmaddsub132xpd] = "vfmaddsub132pd";
  OP_Name[TOP_xfmaddsub132xxpd] = "vfmaddsub132pd";
  OP_Name[TOP_xfmaddsub132xxxpd] = "vfmaddsub132pd";
  OP_Name[TOP_xfmaddsub213pd] = "vfmaddsub213pd";
  OP_Name[TOP_xfmaddsub213xpd] = "vfmaddsub213pd";
  OP_Name[TOP_xfmaddsub213xxpd] = "vfmaddsub213pd";
  OP_Name[TOP_xfmaddsub213xxxpd] = "vfmaddsub213pd";
  OP_Name[TOP_xfmaddsub231ps] = "vfmaddsub231pd";
  OP_Name[TOP_xfmaddsub231xps] = "vfmaddsub231pd";
  OP_Name[TOP_xfmaddsub231xxps] = "vfmaddsub231pd";
  OP_Name[TOP_xfmaddsub231xxxps] = "vfmaddsub231pd";
  OP_Name[TOP_xfmaddsub132ps] = "vfmaddsub132ps";
  OP_Name[TOP_xfmaddsub132xps] = "vfmaddsub132ps";
  OP_Name[TOP_xfmaddsub132xxps] = "vfmaddsub132ps";
  OP_Name[TOP_xfmaddsub132xxxps] = "vfmaddsub132ps";
  OP_Name[TOP_xfmaddsub213ps] = "vfmaddsub213ps";
  OP_Name[TOP_xfmaddsub213xps] = "vfmaddsub213ps";
  OP_Name[TOP_xfmaddsub213xxps] = "vfmaddsub213ps";
  OP_Name[TOP_xfmaddsub213xxxps] = "vfmaddsub213ps";
  OP_Name[TOP_xfmaddsub231ps] = "vfmaddsub231ps";
  OP_Name[TOP_xfmaddsub231xps] = "vfmaddsub231ps";
  OP_Name[TOP_xfmaddsub231xxps] = "vfmaddsub231ps";
  OP_Name[TOP_xfmaddsub231xxxps] = "vfmaddsub231ps";
  OP_Name[TOP_xfmsubadd132pd] = "vfmsubadd132pd";
  OP_Name[TOP_xfmsubadd132xpd] = "vfmsubadd132pd";
  OP_Name[TOP_xfmsubadd132xxpd] = "vfmsubadd132pd";
  OP_Name[TOP_xfmsubadd132xxxpd] = "vfmsubadd132pd";
  OP_Name[TOP_xfmsubadd213pd] = "vfmsubadd213pd";
  OP_Name[TOP_xfmsubadd213xpd] = "vfmsubadd213pd";
  OP_Name[TOP_xfmsubadd213xxpd] = "vfmsubadd213pd";
  OP_Name[TOP_xfmsubadd213xxxpd] = "vfmsubadd213pd";
  OP_Name[TOP_xfmsubadd231pd] = "vfmsubadd231pd";
  OP_Name[TOP_xfmsubadd231xpd] = "vfmsubadd231pd";
  OP_Name[TOP_xfmsubadd231xxpd] = "vfmsubadd231pd";
  OP_Name[TOP_xfmsubadd231xxxpd] = "vfmsubadd231pd";
  OP_Name[TOP_xfmsubadd132ps] = "vfmsubadd132ps";
  OP_Name[TOP_xfmsubadd132xps] = "vfmsubadd132ps";
  OP_Name[TOP_xfmsubadd132xxps] = "vfmsubadd132ps";
  OP_Name[TOP_xfmsubadd132xxxps] = "vfmsubadd132ps";
  OP_Name[TOP_xfmsubadd213ps] = "vfmsubadd213ps";
  OP_Name[TOP_xfmsubadd213xps] = "vfmsubadd213ps";
  OP_Name[TOP_xfmsubadd213xxps] = "vfmsubadd213ps";
  OP_Name[TOP_xfmsubadd213xxxps] = "vfmsubadd213ps";
  OP_Name[TOP_xfmsubadd231ps] = "vfmsubadd231ps";
  OP_Name[TOP_xfmsubadd231xps] = "vfmsubadd231ps";
  OP_Name[TOP_xfmsubadd231xxps] = "vfmsubadd231ps";
  OP_Name[TOP_xfmsubadd231xxxps] = "vfmsubadd231ps";
  OP_Name[TOP_xfmsub132pd] = "vfmsub132pd";
  OP_Name[TOP_xfmsub132xpd] = "vfmsub132pd";
  OP_Name[TOP_xfmsub132xxpd] = "vfmsub132pd";
  OP_Name[TOP_xfmsub132xxxpd] = "vfmsub132pd";
  OP_Name[TOP_xfmsub213pd] = "vfmsub213pd";
  OP_Name[TOP_xfmsub213xpd] = "vfmsub213pd";
  OP_Name[TOP_xfmsub213xxpd] = "vfmsub213pd";
  OP_Name[TOP_xfmsub213xxxpd] = "vfmsub213pd";
  OP_Name[TOP_xfmsub231pd] = "vfmsub231pd";
  OP_Name[TOP_xfmsub231xpd] = "vfmsub231pd";
  OP_Name[TOP_xfmsub231xxpd] = "vfmsub231pd";
  OP_Name[TOP_xfmsub231xxxpd] = "vfmsub231pd";
  OP_Name[TOP_xfmsub132ps] = "vfmsub132ps";
  OP_Name[TOP_xfmsub132xps] = "vfmsub132ps";
  OP_Name[TOP_xfmsub132xxps] = "vfmsub132ps";
  OP_Name[TOP_xfmsub132xxxps] = "vfmsub132ps";
  OP_Name[TOP_xfmsub213ps] = "vfmsub213ps";
  OP_Name[TOP_xfmsub213xps] = "vfmsub213ps";
  OP_Name[TOP_xfmsub213xxps] = "vfmsub213ps";
  OP_Name[TOP_xfmsub213xxxps] = "vfmsub213ps";
  OP_Name[TOP_xfmsub231ps] = "vfmsub231ps";
  OP_Name[TOP_xfmsub231xps] = "vfmsub231ps";
  OP_Name[TOP_xfmsub231xxps] = "vfmsub231ps";
  OP_Name[TOP_xfmsub231xxxps] = "vfmsub231ps";
  OP_Name[TOP_xfmsub132sd] = "vfmsub132sd";
  OP_Name[TOP_xfmsub132xsd] = "vfmsub132sd";
  OP_Name[TOP_xfmsub132xxsd] = "vfmsub132sd";
  OP_Name[TOP_xfmsub132xxxsd] = "vfmsub132sd";
  OP_Name[TOP_xfmsub213sd] = "vfmsub213sd";
  OP_Name[TOP_xfmsub213xsd] = "vfmsub213sd";
  OP_Name[TOP_xfmsub213xxsd] = "vfmsub213sd";
  OP_Name[TOP_xfmsub213xxxsd] = "vfmsub213sd";
  OP_Name[TOP_xfmsub231sd] = "vfmsub231sd";
  OP_Name[TOP_xfmsub231xsd] = "vfmsub231sd";
  OP_Name[TOP_xfmsub231xxsd] = "vfmsub231sd";
  OP_Name[TOP_xfmsub231xxxsd] = "vfmsub231sd";
  OP_Name[TOP_xfmsub132ss] = "vfmsub132ss";
  OP_Name[TOP_xfmsub132xss] = "vfmsub132ss";
  OP_Name[TOP_xfmsub132xxss] = "vfmsub132ss";
  OP_Name[TOP_xfmsub132xxxss] = "vfmsub132ss";
  OP_Name[TOP_xfmsub213ss] = "vfmsub213ss";
  OP_Name[TOP_xfmsub213xss] = "vfmsub213ss";
  OP_Name[TOP_xfmsub213xxss] = "vfmsub213ss";
  OP_Name[TOP_xfmsub213xxxss] = "vfmsub213ss";
  OP_Name[TOP_xfmsub231ss] = "vfmsub231ss";
  OP_Name[TOP_xfmsub231xss] = "vfmsub231ss";
  OP_Name[TOP_xfmsub231xxss] = "vfmsub231ss";
  OP_Name[TOP_xfmsub231xxxss] = "vfmsub231ss";
  OP_Name[TOP_xfnmadd132pd] = "vfnmadd132pd";
  OP_Name[TOP_xfnmadd132xpd] = "vfnmadd132pd";
  OP_Name[TOP_xfnmadd132xxpd] = "vfnmadd132pd";
  OP_Name[TOP_xfnmadd132xxxpd] = "vfnmadd132pd";
  OP_Name[TOP_xfnmadd213pd] = "vfnmadd213pd";
  OP_Name[TOP_xfnmadd213xpd] = "vfnmadd213pd";
  OP_Name[TOP_xfnmadd213xxpd] = "vfnmadd213pd";
  OP_Name[TOP_xfnmadd213xxxpd] = "vfnmadd213pd";
  OP_Name[TOP_xfnmadd231pd] = "vfnmadd231pd";
  OP_Name[TOP_xfnmadd231xpd] = "vfnmadd231pd";
  OP_Name[TOP_xfnmadd231xxpd] = "vfnmadd231pd";
  OP_Name[TOP_xfnmadd231xxxpd] = "vfnmadd231pd";
  OP_Name[TOP_xfnmadd132ps] = "vfnmadd132ps";
  OP_Name[TOP_xfnmadd132xps] = "vfnmadd132ps";
  OP_Name[TOP_xfnmadd132xxps] = "vfnmadd132ps";
  OP_Name[TOP_xfnmadd132xxxps] = "vfnmadd132ps";
  OP_Name[TOP_xfnmadd213ps] = "vfnmadd213ps";
  OP_Name[TOP_xfnmadd213xps] = "vfnmadd213ps";
  OP_Name[TOP_xfnmadd213xxps] = "vfnmadd213ps";
  OP_Name[TOP_xfnmadd213xxxps] = "vfnmadd213ps";
  OP_Name[TOP_xfnmadd231ps] = "vfnmadd231ps";
  OP_Name[TOP_xfnmadd231xps] = "vfnmadd231ps";
  OP_Name[TOP_xfnmadd231xxps] = "vfnmadd231ps";
  OP_Name[TOP_xfnmadd231xxxps] = "vfnmadd231ps";
  OP_Name[TOP_xfnmadd132sd] = "vfnmadd132sd";
  OP_Name[TOP_xfnmadd132xsd] = "vfnmadd132sd";
  OP_Name[TOP_xfnmadd132xxsd] = "vfnmadd132sd";
  OP_Name[TOP_xfnmadd132xxxsd] = "vfnmadd132sd";
  OP_Name[TOP_xfnmadd213sd] = "vfnmadd213sd";
  OP_Name[TOP_xfnmadd213xsd] = "vfnmadd213sd";
  OP_Name[TOP_xfnmadd213xxsd] = "vfnmadd213sd";
  OP_Name[TOP_xfnmadd213xxxsd] = "vfnmadd213sd";
  OP_Name[TOP_xfnmadd231sd] = "vfnmadd231sd";
  OP_Name[TOP_xfnmadd231xsd] = "vfnmadd231sd";
  OP_Name[TOP_xfnmadd231xxsd] = "vfnmadd231sd";
  OP_Name[TOP_xfnmadd231xxxsd] = "vfnmadd231sd";
  OP_Name[TOP_xfnmadd132ss] = "vfnmadd132ss";
  OP_Name[TOP_xfnmadd132xss] = "vfnmadd132ss";
  OP_Name[TOP_xfnmadd132xxss] = "vfnmadd132ss";
  OP_Name[TOP_xfnmadd132xxxss] = "vfnmadd132ss";
  OP_Name[TOP_xfnmadd213ss] = "vfnmadd213ss";
  OP_Name[TOP_xfnmadd213xss] = "vfnmadd213ss";
  OP_Name[TOP_xfnmadd213xxss] = "vfnmadd213ss";
  OP_Name[TOP_xfnmadd213xxxss] = "vfnmadd213ss";
  OP_Name[TOP_xfnmadd231ss] = "vfnmadd231ss";
  OP_Name[TOP_xfnmadd231xss] = "vfnmadd231ss";
  OP_Name[TOP_xfnmadd231xxss] = "vfnmadd231ss";
  OP_Name[TOP_xfnmadd231xxxss] = "vfnmadd231ss";
  OP_Name[TOP_xfnmsub132pd] = "vfnmsub132pd";
  OP_Name[TOP_xfnmsub132xpd] = "vfnmsub132pd";
  OP_Name[TOP_xfnmsub132xxpd] = "vfnmsub132pd";
  OP_Name[TOP_xfnmsub132xxxpd] = "vfnmsub132pd";
  OP_Name[TOP_xfnmsub213pd] = "vfnmsub213pd";
  OP_Name[TOP_xfnmsub213xpd] = "vfnmsub213pd";
  OP_Name[TOP_xfnmsub213xxpd] = "vfnmsub213pd";
  OP_Name[TOP_xfnmsub213xxxpd] = "vfnmsub213pd";
  OP_Name[TOP_xfnmsub231pd] = "vfnmsub231pd";
  OP_Name[TOP_xfnmsub231xpd] = "vfnmsub231pd";
  OP_Name[TOP_xfnmsub231xxpd] = "vfnmsub231pd";
  OP_Name[TOP_xfnmsub231xxxpd] = "vfnmsub231pd";
  OP_Name[TOP_xfnmsub132ps] = "vfnmsub132ps";
  OP_Name[TOP_xfnmsub132xps] = "vfnmsub132ps";
  OP_Name[TOP_xfnmsub132xxps] = "vfnmsub132ps";
  OP_Name[TOP_xfnmsub132xxxps] = "vfnmsub132ps";
  OP_Name[TOP_xfnmsub213ps] = "vfnmsub213ps";
  OP_Name[TOP_xfnmsub213xps] = "vfnmsub213ps";
  OP_Name[TOP_xfnmsub213xxps] = "vfnmsub213ps";
  OP_Name[TOP_xfnmsub231xxxps] = "vfnmsub231ps";
  OP_Name[TOP_xfnmsub132sd] = "vfnmsub132sd";
  OP_Name[TOP_xfnmsub132xsd] = "vfnmsub132sd";
  OP_Name[TOP_xfnmsub132xxsd] = "vfnmsub132sd";
  OP_Name[TOP_xfnmsub132xxxsd] = "vfnmsub132sd";
  OP_Name[TOP_xfnmsub213sd] = "vfnmsub213sd";
  OP_Name[TOP_xfnmsub213xsd] = "vfnmsub213sd";
  OP_Name[TOP_xfnmsub213xxsd] = "vfnmsub213sd";
  OP_Name[TOP_xfnmsub213xxxsd] = "vfnmsub213sd";
  OP_Name[TOP_xfnmsub231sd] = "vfnmsub231sd";
  OP_Name[TOP_xfnmsub231xsd] = "vfnmsub231sd";
  OP_Name[TOP_xfnmsub231xxsd] = "vfnmsub231sd";
  OP_Name[TOP_xfnmsub231xxxsd] = "vfnmsub231sd";
  OP_Name[TOP_xfnmsub132ss] = "vfnmsub132ss";
  OP_Name[TOP_xfnmsub132xss] = "vfnmsub132ss";
  OP_Name[TOP_xfnmsub132xxss] = "vfnmsub132ss";
  OP_Name[TOP_xfnmsub132xxxss] = "vfnmsub132ss";
  OP_Name[TOP_xfnmsub213ss] = "vfnmsub213ss";
  OP_Name[TOP_xfnmsub213xss] = "vfnmsub213ss";
  OP_Name[TOP_xfnmsub213xxss] = "vfnmsub213ss";
  OP_Name[TOP_xfnmsub213xxxss] = "vfnmsub213ss";
  OP_Name[TOP_xfnmsub231ss] = "vfnmsub231ss";
  OP_Name[TOP_xfnmsub231xss] = "vfnmsub231ss";
  OP_Name[TOP_xfnmsub231xxss] = "vfnmsub231ss";
  OP_Name[TOP_xfnmsub231xxxss] = "vfnmsub231ss";

  // Start FMA4 additions
  OP_Name[TOP_vfmaddss]  = "vfmaddss";
  OP_Name[TOP_vfmaddxss] = "vfmaddss";
  OP_Name[TOP_vfmaddxxss] = "vfmaddss";
  OP_Name[TOP_vfmaddxxxss] = "vfmaddss";
  OP_Name[TOP_vfmaddxrss] = "vfmaddss";
  OP_Name[TOP_vfmaddxxrss] = "vfmaddss";
  OP_Name[TOP_vfmaddxxxrss] = "vfmaddss";
  OP_Name[TOP_vfmaddsd]  = "vfmaddsd";
  OP_Name[TOP_vfmaddxsd] = "vfmaddsd";
  OP_Name[TOP_vfmaddxxsd] = "vfmaddsd";
  OP_Name[TOP_vfmaddxxxsd] = "vfmaddsd";
  OP_Name[TOP_vfmaddxrsd] = "vfmaddsd";
  OP_Name[TOP_vfmaddxxrsd] = "vfmaddsd";
  OP_Name[TOP_vfmaddxxxrsd] = "vfmaddsd";
  OP_Name[TOP_vfmaddps]  = "vfmaddps";
  OP_Name[TOP_vfmaddxps] = "vfmaddps";
  OP_Name[TOP_vfmaddxxps] = "vfmaddps";
  OP_Name[TOP_vfmaddxxxps] = "vfmaddps";
  OP_Name[TOP_vfmaddxrps] = "vfmaddps";
  OP_Name[TOP_vfmaddxxrps] = "vfmaddps";
  OP_Name[TOP_vfmaddxxxrps] = "vfmaddps";
  OP_Name[TOP_vfmaddpd]  = "vfmaddpd";
  OP_Name[TOP_vfmaddxpd] = "vfmaddpd";
  OP_Name[TOP_vfmaddxxpd] = "vfmaddpd";
  OP_Name[TOP_vfmaddxxxpd] = "vfmaddpd";
  OP_Name[TOP_vfmaddxrpd] = "vfmaddpd";
  OP_Name[TOP_vfmaddxxrpd] = "vfmaddpd";
  OP_Name[TOP_vfmaddxxxrpd] = "vfmaddpd";
  OP_Name[TOP_vfmaddsubps]  = "vfmaddsubps";
  OP_Name[TOP_vfmaddsubxps] = "vfmaddsubps";
  OP_Name[TOP_vfmaddsubxxps] = "vfmaddsubps";
  OP_Name[TOP_vfmaddsubxxxps] = "vfmaddsubps";
  OP_Name[TOP_vfmaddsubxrps] = "vfmaddsubps";
  OP_Name[TOP_vfmaddsubxxrps] = "vfmaddsubps";
  OP_Name[TOP_vfmaddsubxxxrps] = "vfmaddsubps";
  OP_Name[TOP_vfmaddsubpd]  = "vfmaddsubpd";
  OP_Name[TOP_vfmaddsubxpd] = "vfmaddsubpd";
  OP_Name[TOP_vfmaddsubxxpd] = "vfmaddsubpd";
  OP_Name[TOP_vfmaddsubxxxpd] = "vfmaddsubpd";
  OP_Name[TOP_vfmaddsubxrpd] = "vfmaddsubpd";
  OP_Name[TOP_vfmaddsubxxrpd] = "vfmaddsubpd";
  OP_Name[TOP_vfmaddsubxxxrpd] = "vfmaddsubpd";
  OP_Name[TOP_vfnmaddss]  = "vfnmaddss";
  OP_Name[TOP_vfnmaddxss] = "vfnmaddss";
  OP_Name[TOP_vfnmaddxxss] = "vfnmaddss";
  OP_Name[TOP_vfnmaddxxxss] = "vfnmaddss";
  OP_Name[TOP_vfnmaddxrss] = "vfnmaddss";
  OP_Name[TOP_vfnmaddxxrss] = "vfnmaddss";
  OP_Name[TOP_vfnmaddxxxrss] = "vfnmaddss";
  OP_Name[TOP_vfnmaddsd]  = "vfnmaddsd";
  OP_Name[TOP_vfnmaddxsd] = "vfnmaddsd";
  OP_Name[TOP_vfnmaddxxsd] = "vfnmaddsd";
  OP_Name[TOP_vfnmaddxxxsd] = "vfnmaddsd";
  OP_Name[TOP_vfnmaddxrsd] = "vfnmaddsd";
  OP_Name[TOP_vfnmaddxxrsd] = "vfnmaddsd";
  OP_Name[TOP_vfnmaddxxxrsd] = "vfnmaddsd";
  OP_Name[TOP_vfnmaddps]  = "vfnmaddps";
  OP_Name[TOP_vfnmaddxps] = "vfnmaddps";
  OP_Name[TOP_vfnmaddxxps] = "vfnmaddps";
  OP_Name[TOP_vfnmaddxxxps] = "vfnmaddps";
  OP_Name[TOP_vfnmaddxrps] = "vfnmaddps";
  OP_Name[TOP_vfnmaddxxrps] = "vfnmaddps";
  OP_Name[TOP_vfnmaddxxxrps] = "vfnmaddps";
  OP_Name[TOP_vfnmaddpd]  = "vfnmaddpd";
  OP_Name[TOP_vfnmaddxpd] = "vfnmaddpd";
  OP_Name[TOP_vfnmaddxxpd] = "vfnmaddpd";
  OP_Name[TOP_vfnmaddxxxpd] = "vfnmaddpd";
  OP_Name[TOP_vfnmaddxrpd] = "vfnmaddpd";
  OP_Name[TOP_vfnmaddxxrpd] = "vfnmaddpd";
  OP_Name[TOP_vfnmaddxxxrpd] = "vfnmaddpd";
  OP_Name[TOP_vfmsubss]  = "vfmsubss";
  OP_Name[TOP_vfmsubxss] = "vfmsubss";
  OP_Name[TOP_vfmsubxxss] = "vfmsubss";
  OP_Name[TOP_vfmsubxxxss] = "vfmsubss";
  OP_Name[TOP_vfmsubxrss] = "vfmsubss";
  OP_Name[TOP_vfmsubxxrss] = "vfmsubss";
  OP_Name[TOP_vfmsubxxxrss] = "vfmsubss";
  OP_Name[TOP_vfmsubsd]  = "vfmsubsd";
  OP_Name[TOP_vfmsubxsd] = "vfmsubsd";
  OP_Name[TOP_vfmsubxxsd] = "vfmsubsd";
  OP_Name[TOP_vfmsubxxxsd] = "vfmsubsd";
  OP_Name[TOP_vfmsubxrsd] = "vfmsubsd";
  OP_Name[TOP_vfmsubxxrsd] = "vfmsubsd";
  OP_Name[TOP_vfmsubxxxrsd] = "vfmsubsd";
  OP_Name[TOP_vfmsubps]  = "vfmsubps";
  OP_Name[TOP_vfmsubxps] = "vfmsubps";
  OP_Name[TOP_vfmsubxxps] = "vfmsubps";
  OP_Name[TOP_vfmsubxxxps] = "vfmsubps";
  OP_Name[TOP_vfmsubxrps] = "vfmsubps";
  OP_Name[TOP_vfmsubxxrps] = "vfmsubps";
  OP_Name[TOP_vfmsubxxxrps] = "vfmsubps";
  OP_Name[TOP_vfmsubpd]  = "vfmsubpd";
  OP_Name[TOP_vfmsubxpd] = "vfmsubpd";
  OP_Name[TOP_vfmsubxxpd] = "vfmsubpd";
  OP_Name[TOP_vfmsubxxxpd] = "vfmsubpd";
  OP_Name[TOP_vfmsubxrpd] = "vfmsubpd";
  OP_Name[TOP_vfmsubxxrpd] = "vfmsubpd";
  OP_Name[TOP_vfmsubxxxrpd] = "vfmsubpd";
  OP_Name[TOP_vfmsubaddps]  = "vfmsubaddps";
  OP_Name[TOP_vfmsubaddxps] = "vfmsubaddps";
  OP_Name[TOP_vfmsubaddxxps] = "vfmsubaddps";
  OP_Name[TOP_vfmsubaddxxxps] = "vfmsubaddps";
  OP_Name[TOP_vfmsubaddxrps] = "vfmsubaddps";
  OP_Name[TOP_vfmsubaddxxrps] = "vfmsubaddps";
  OP_Name[TOP_vfmsubaddxxxrps] = "vfmsubaddps";
  OP_Name[TOP_vfmsubaddpd]  = "vfmsubaddpd";
  OP_Name[TOP_vfmsubaddxpd] = "vfmsubaddpd";
  OP_Name[TOP_vfmsubaddxxpd] = "vfmsubaddpd";
  OP_Name[TOP_vfmsubaddxxxpd] = "vfmsubaddpd";
  OP_Name[TOP_vfmsubaddxrpd] = "vfmsubaddpd";
  OP_Name[TOP_vfmsubaddxxrpd] = "vfmsubaddpd";
  OP_Name[TOP_vfmsubaddxxxrpd] = "vfmsubaddpd";
  OP_Name[TOP_vfnmsubss]  = "vfnmsubss";
  OP_Name[TOP_vfnmsubxss] = "vfnmsubss";
  OP_Name[TOP_vfnmsubxxss] = "vfnmsubss";
  OP_Name[TOP_vfnmsubxxxss] = "vfnmsubss";
  OP_Name[TOP_vfnmsubxrss] = "vfnmsubss";
  OP_Name[TOP_vfnmsubxxrss] = "vfnmsubss";
  OP_Name[TOP_vfnmsubxxxrss] = "vfnmsubss";
  OP_Name[TOP_vfnmsubsd]  = "vfnmsubsd";
  OP_Name[TOP_vfnmsubxsd] = "vfnmsubsd";
  OP_Name[TOP_vfnmsubxxsd] = "vfnmsubsd";
  OP_Name[TOP_vfnmsubxxxsd] = "vfnmsubsd";
  OP_Name[TOP_vfnmsubxrsd] = "vfnmsubsd";
  OP_Name[TOP_vfnmsubxxrsd] = "vfnmsubsd";
  OP_Name[TOP_vfnmsubxxxrsd] = "vfnmsubsd";
  OP_Name[TOP_vfnmsubps]  = "vfnmsubps";
  OP_Name[TOP_vfnmsubxps] = "vfnmsubps";
  OP_Name[TOP_vfnmsubxxps] = "vfnmsubps";
  OP_Name[TOP_vfnmsubxxxps] = "vfnmsubps";
  OP_Name[TOP_vfnmsubxrps] = "vfnmsubps";
  OP_Name[TOP_vfnmsubxxrps] = "vfnmsubps";
  OP_Name[TOP_vfnmsubxxxrps] = "vfnmsubps";
  OP_Name[TOP_vfnmsubpd]  = "vfnmsubpd";
  OP_Name[TOP_vfnmsubxpd] = "vfnmsubpd";
  OP_Name[TOP_vfnmsubxxpd] = "vfnmsubpd";
  OP_Name[TOP_vfnmsubxxxpd] = "vfnmsubpd";
  OP_Name[TOP_vfnmsubxrpd] = "vfnmsubpd";
  OP_Name[TOP_vfnmsubxxrpd] = "vfnmsubpd";
  OP_Name[TOP_vfnmsubxxxrpd] = "vfnmsubpd";
  // End FMA4 additions

  OP_Name[TOP_vzeroupper] = "vzeroupper";

  OP_Name[TOP_icall]    = "call";
  OP_Name[TOP_icallx]   = "call";
  OP_Name[TOP_icallxx]  = "call";
  OP_Name[TOP_icallxxx] = "call";
  OP_Name[TOP_ijmp]    = "jmp";
  OP_Name[TOP_ijmpx]   = "jmp";
  OP_Name[TOP_ijmpxx]  = "jmp";
  OP_Name[TOP_ijmpxxx] = "jmp";
  OP_Name[TOP_xor8]   = "xorb";
  OP_Name[TOP_xorx8]  = "xorb";
  OP_Name[TOP_xorxx8] = "xorb";
  OP_Name[TOP_xorxxx8]= "xorb";
  OP_Name[TOP_xor16]   = "xorw";
  OP_Name[TOP_xorx16]  = "xorw";
  OP_Name[TOP_xorxx16] = "xorw";
  OP_Name[TOP_xorxxx16]= "xorw";
  OP_Name[TOP_and8]   = "andb";
  OP_Name[TOP_andx8]  = "andb";
  OP_Name[TOP_andxx8] = "andb";
  OP_Name[TOP_andxxx8]= "andb";
  OP_Name[TOP_and16]   = "andw";
  OP_Name[TOP_andx16]  = "andw";
  OP_Name[TOP_andxx16] = "andw";
  OP_Name[TOP_andxxx16]= "andw";
  OP_Name[TOP_or8]   = "orb";
  OP_Name[TOP_orx8]  = "orb";
  OP_Name[TOP_orxx8] = "orb";
  OP_Name[TOP_orxxx8]= "orb";
  OP_Name[TOP_or16]   = "orw";
  OP_Name[TOP_orx16]  = "orw";
  OP_Name[TOP_orxx16] = "orw";
  OP_Name[TOP_orxxx16]= "orw";
  OP_Name[TOP_cmp8]   = "cmpb";
  OP_Name[TOP_cmpx8]  = "cmpb";
  OP_Name[TOP_cmpxx8] = "cmpb";
  OP_Name[TOP_cmpxxx8]= "cmpb";
  OP_Name[TOP_cmpi8]   = "cmpb";
  OP_Name[TOP_cmpxi8]  = "cmpb";
  OP_Name[TOP_cmpxxi8] = "cmpb";
  OP_Name[TOP_cmpxxxi8]= "cmpb";
  OP_Name[TOP_cmpxr8]  = "cmpb";
  OP_Name[TOP_cmpxxr8] = "cmpb";
  OP_Name[TOP_cmpxxxr8]= "cmpb";
  OP_Name[TOP_cmp16]   = "cmpw";
  OP_Name[TOP_cmpx16]  = "cmpw";
  OP_Name[TOP_cmpxx16] = "cmpw";
  OP_Name[TOP_cmpxxx16]= "cmpw";
  OP_Name[TOP_cmpi16]   = "cmpw";
  OP_Name[TOP_cmpxi16]  = "cmpw";
  OP_Name[TOP_cmpxxi16] = "cmpw";
  OP_Name[TOP_cmpxxxi16]= "cmpw";
  OP_Name[TOP_cmpxr16]  = "cmpw";
  OP_Name[TOP_cmpxxr16] = "cmpw";
  OP_Name[TOP_cmpxxxr16]= "cmpw";
  OP_Name[TOP_cmp32]   = "cmpl";
  OP_Name[TOP_cmpx32]  = "cmpl";
  OP_Name[TOP_cmpxx32] = "cmpl";
  OP_Name[TOP_cmpxxx32]= "cmpl";
  OP_Name[TOP_cmpxi32]  = "cmpl";
  OP_Name[TOP_cmpxxi32] = "cmpl";
  OP_Name[TOP_cmpxxxi32]= "cmpl";
  OP_Name[TOP_cmpxr32]  = "cmpl";
  OP_Name[TOP_cmpxxr32] = "cmpl";
  OP_Name[TOP_cmpxxxr32]= "cmpl";
  OP_Name[TOP_cmp64]   = "cmpq";
  OP_Name[TOP_cmpx64]  = "cmpq";
  OP_Name[TOP_cmpxx64] = "cmpq";
  OP_Name[TOP_cmpxxx64]= "cmpq";
  OP_Name[TOP_cmpxi64]  = "cmpq";
  OP_Name[TOP_cmpxxi64] = "cmpq";
  OP_Name[TOP_cmpxxxi64]= "cmpq";
  OP_Name[TOP_cmpxr64]  = "cmpq";
  OP_Name[TOP_cmpxxr64] = "cmpq";
  OP_Name[TOP_cmpxxxr64]= "cmpq";
  OP_Name[TOP_cmpi32]  = "cmpl";
  OP_Name[TOP_cmpi64]  = "cmpq";
  //OP_Name[TOP_dec32] = "decl";
  //OP_Name[TOP_dec64] = "decq";
  OP_Name[TOP_div32] = "divl";
  OP_Name[TOP_div64] = "divq";
  OP_Name[TOP_idiv32] = "idivl";
  OP_Name[TOP_idiv64] = "idivq";
  //OP_Name[TOP_inc32] = "incl";
  //OP_Name[TOP_inc64] = "incq";
  OP_Name[TOP_lea32]   = "leal";
  OP_Name[TOP_lea64]   = "leaq";
  OP_Name[TOP_leax32]  = "leal";
  OP_Name[TOP_leax64]  = "leaq";
  OP_Name[TOP_leaxx32] = "leal";
  OP_Name[TOP_leaxx64] = "leaq";
  OP_Name[TOP_prefetchx]   = "prefetch";
  OP_Name[TOP_prefetchxx]  = "prefetch";
  OP_Name[TOP_prefetchwx]  = "prefetchw";
  OP_Name[TOP_prefetchwxx] = "prefetchw";
  OP_Name[TOP_prefetcht0x] = "prefetcht0";
  OP_Name[TOP_prefetcht0xx]= "prefetcht0";
  OP_Name[TOP_prefetcht1x] = "prefetcht1";
  OP_Name[TOP_prefetcht1xx]= "prefetcht1";
  OP_Name[TOP_prefetchntax] = "prefetchnta";
  OP_Name[TOP_prefetchntaxx]= "prefetchnta";
  if (CG_use_prefetchnta) {
    OP_Name[TOP_prefetcht0] = "prefetchnta";
    OP_Name[TOP_prefetcht0x] = "prefetchnta";
    OP_Name[TOP_prefetcht0xx]= "prefetchnta";
  }
  OP_Name[TOP_ld8_32]   = "movsbl";
  OP_Name[TOP_ldx8_32]  = "movsbl";
  OP_Name[TOP_ldxx8_32] = "movsbl";
  OP_Name[TOP_ldu8_32]  = "movzbl";
  OP_Name[TOP_ldxu8_32] = "movzbl";
  OP_Name[TOP_ldxxu8_32]= "movzbl";
  OP_Name[TOP_ld8_32_n32]   = "movsbl";
  OP_Name[TOP_ldu8_32_n32]  = "movzbl";

  OP_Name[TOP_ld16_32]   = "movswl";
  OP_Name[TOP_ldx16_32]  = "movswl";
  OP_Name[TOP_ldxx16_32] = "movswl";
  OP_Name[TOP_ldu16_32]  = "movzwl";
  OP_Name[TOP_ldxu16_32] = "movzwl";
  OP_Name[TOP_ldxxu16_32]= "movzwl";
  OP_Name[TOP_ld16_32_n32]   = "movswl";
  OP_Name[TOP_ldu16_32_n32]  = "movzwl";

  OP_Name[TOP_ld8_64]		= "movsbq";
  OP_Name[TOP_ldx8_64]		= "movsbq";
  OP_Name[TOP_ldxx8_64]		= "movsbq";
  OP_Name[TOP_ld8_64_off]	= "movsbq";
  OP_Name[TOP_ldu8_64]		= "movzbq";
  OP_Name[TOP_ldxu8_64]		= "movzbq";
  OP_Name[TOP_ldxxu8_64]	= "movzbq";
  OP_Name[TOP_ldu8_64_off]	= "movzbq";
  OP_Name[TOP_ld16_64]		= "movswq";
  OP_Name[TOP_ldx16_64]		= "movswq";
  OP_Name[TOP_ldxx16_64]	= "movswq";
  OP_Name[TOP_ld16_64_off]	= "movswq";
  OP_Name[TOP_ldu16_64]		= "movzwq";
  OP_Name[TOP_ldxu16_64]	= "movzwq";
  OP_Name[TOP_ldxxu16_64]	= "movzwq";
  OP_Name[TOP_ldu16_64_off]	= "movzwq";
  OP_Name[TOP_ld32_64_off]	= "movslq";

  // load-execute-store TOPs
  OP_Name[TOP_addxr8] = "addb";
  OP_Name[TOP_addxxr8] = "addb";
  OP_Name[TOP_addxxxr8] = "addb";
  OP_Name[TOP_addxr8_n32] = "addb";
  OP_Name[TOP_addxr16] = "addw";
  OP_Name[TOP_addxxr16] = "addw";
  OP_Name[TOP_addxxxr16] = "addw";
  OP_Name[TOP_addxr16_n32] = "addw";
  OP_Name[TOP_addxr32] = "addl";
  OP_Name[TOP_addxxr32] = "addl";
  OP_Name[TOP_addxxxr32] = "addl";
  OP_Name[TOP_addxr32_n32] = "addl";
  OP_Name[TOP_addxr64] = "addq";
  OP_Name[TOP_addxxr64] = "addq";
  OP_Name[TOP_addxxxr64] = "addq";
  OP_Name[TOP_addxr64_off] = "addq";
  OP_Name[TOP_addixr8] = "addb";
  OP_Name[TOP_addixxr8] = "addb";
  OP_Name[TOP_addixxxr8] = "addb";
  OP_Name[TOP_addixr8_n32] = "addb";
  OP_Name[TOP_addixr16] = "addw";
  OP_Name[TOP_addixxr16] = "addw";
  OP_Name[TOP_addixxxr16] = "addw";
  OP_Name[TOP_addixr16_n32] = "addw";
  OP_Name[TOP_addixr32] = "addl";
  OP_Name[TOP_addixxr32] = "addl";
  OP_Name[TOP_addixxxr32] = "addl";
  OP_Name[TOP_addixr32_n32] = "addl";
  OP_Name[TOP_addixr64] = "addq";
  OP_Name[TOP_addixxr64] = "addq";
  OP_Name[TOP_addixxxr64] = "addq";
  OP_Name[TOP_addixr64_off] = "addq";
  OP_Name[TOP_subxr8] = "subb";
  OP_Name[TOP_subxxr8] = "subb";
  OP_Name[TOP_subxxxr8] = "subb";
  OP_Name[TOP_subxr8_n32] = "subb";
  OP_Name[TOP_subxr16] = "subw";
  OP_Name[TOP_subxxr16] = "subw";
  OP_Name[TOP_subxxxr16] = "subw";
  OP_Name[TOP_subxr16_n32] = "subw";
  OP_Name[TOP_subxr32] = "subl";
  OP_Name[TOP_subxxr32] = "subl";
  OP_Name[TOP_subxxxr32] = "subl";
  OP_Name[TOP_subxr32_n32] = "subl";
  OP_Name[TOP_subxr64] = "subq";
  OP_Name[TOP_subxxr64] = "subq";
  OP_Name[TOP_subxxxr64] = "subq";
  OP_Name[TOP_subxr64_off] = "subq";
  OP_Name[TOP_subixr8] = "subb";
  OP_Name[TOP_subixxr8] = "subb";
  OP_Name[TOP_subixxxr8] = "subb";
  OP_Name[TOP_subixr8_n32] = "subb";
  OP_Name[TOP_subixr16] = "subw";
  OP_Name[TOP_subixxr16] = "subw";
  OP_Name[TOP_subixxxr16] = "subw";
  OP_Name[TOP_subixr16_n32] = "subw";
  OP_Name[TOP_subixr32] = "subl";
  OP_Name[TOP_subixxr32] = "subl";
  OP_Name[TOP_subixxxr32] = "subl";
  OP_Name[TOP_subixr32_n32] = "subl";
  OP_Name[TOP_subixr64] = "subq";
  OP_Name[TOP_subixxr64] = "subq";
  OP_Name[TOP_subixxxr64] = "subq";
  OP_Name[TOP_subixr64_off] = "subq";
  OP_Name[TOP_andxr8] = "andb";
  OP_Name[TOP_andxxr8] = "andb";
  OP_Name[TOP_andxxxr8] = "andb";
  OP_Name[TOP_andxr8_n32] = "andb";
  OP_Name[TOP_andxr16] = "andw";
  OP_Name[TOP_andxxr16] = "andw";
  OP_Name[TOP_andxxxr16] = "andw";
  OP_Name[TOP_andxr16_n32] = "andw";
  OP_Name[TOP_andxr32] = "andl";
  OP_Name[TOP_andxxr32] = "andl";
  OP_Name[TOP_andxxxr32] = "andl";
  OP_Name[TOP_andxr32_n32] = "andl";
  OP_Name[TOP_andxr64] = "andq";
  OP_Name[TOP_andxxr64] = "andq";
  OP_Name[TOP_andxxxr64] = "andq";
  OP_Name[TOP_andxr64_off] = "andq";
  OP_Name[TOP_andixr8] = "andb";
  OP_Name[TOP_andixxr8] = "andb";
  OP_Name[TOP_andixxxr8] = "andb";
  OP_Name[TOP_andixr8_n32] = "andb";
  OP_Name[TOP_andixr16] = "andw";
  OP_Name[TOP_andixxr16] = "andw";
  OP_Name[TOP_andixxxr16] = "andw";
  OP_Name[TOP_andixr16_n32] = "andw";
  OP_Name[TOP_andixr32] = "andl";
  OP_Name[TOP_andixxr32] = "andl";
  OP_Name[TOP_andixxxr32] = "andl";
  OP_Name[TOP_andixr32_n32] = "andl";
  OP_Name[TOP_andixr64] = "andq";
  OP_Name[TOP_andixxr64] = "andq";
  OP_Name[TOP_andixxxr64] = "andq";
  OP_Name[TOP_andixr64_off] = "andq";
  OP_Name[TOP_orxr8] = "orb";
  OP_Name[TOP_orxxr8] = "orb";
  OP_Name[TOP_orxxxr8] = "orb";
  OP_Name[TOP_orxr8_n32] = "orb";
  OP_Name[TOP_orxr16] = "orw";
  OP_Name[TOP_orxxr16] = "orw";
  OP_Name[TOP_orxxxr16] = "orw";
  OP_Name[TOP_orxr16_n32] = "orw";
  OP_Name[TOP_orxr32] = "orl";
  OP_Name[TOP_orxxr32] = "orl";
  OP_Name[TOP_orxxxr32] = "orl";
  OP_Name[TOP_orxr32_n32] = "orl";
  OP_Name[TOP_orxr64] = "orq";
  OP_Name[TOP_orxxr64] = "orq";
  OP_Name[TOP_orxxxr64] = "orq";
  OP_Name[TOP_orxr64_off] = "orq";
  OP_Name[TOP_orixr8] = "orb";
  OP_Name[TOP_orixxr8] = "orb";
  OP_Name[TOP_orixxxr8] = "orb";
  OP_Name[TOP_orixr8_n32] = "orb";
  OP_Name[TOP_orixr16] = "orw";
  OP_Name[TOP_orixxr16] = "orw";
  OP_Name[TOP_orixxxr16] = "orw";
  OP_Name[TOP_orixr16_n32] = "orw";
  OP_Name[TOP_orixr32] = "orl";
  OP_Name[TOP_orixxr32] = "orl";
  OP_Name[TOP_orixxxr32] = "orl";
  OP_Name[TOP_orixr32_n32] = "orl";
  OP_Name[TOP_orixr64] = "orq";
  OP_Name[TOP_orixxr64] = "orq";
  OP_Name[TOP_orixxxr64] = "orq";
  OP_Name[TOP_orixr64_off] = "orq";
  OP_Name[TOP_xorxr8] = "xorb";
  OP_Name[TOP_xorxxr8] = "xorb";
  OP_Name[TOP_xorxxxr8] = "xorb";
  OP_Name[TOP_xorxr8_n32] = "xorb";
  OP_Name[TOP_xorxr16] = "xorw";
  OP_Name[TOP_xorxxr16] = "xorw";
  OP_Name[TOP_xorxxxr16] = "xorw";
  OP_Name[TOP_xorxr16_n32] = "xorw";
  OP_Name[TOP_xorxr32] = "xorl";
  OP_Name[TOP_xorxxr32] = "xorl";
  OP_Name[TOP_xorxxxr32] = "xorl";
  OP_Name[TOP_xorxr32_n32] = "xorl";
  OP_Name[TOP_xorxr64] = "xorq";
  OP_Name[TOP_xorxxr64] = "xorq";
  OP_Name[TOP_xorxxxr64] = "xorq";
  OP_Name[TOP_xorxr64_off] = "xorq";
  OP_Name[TOP_xorixr8] = "xorb";
  OP_Name[TOP_xorixxr8] = "xorb";
  OP_Name[TOP_xorixxxr8] = "xorb";
  OP_Name[TOP_xorixr8_n32] = "xorb";
  OP_Name[TOP_xorixr16] = "xorw";
  OP_Name[TOP_xorixxr16] = "xorw";
  OP_Name[TOP_xorixxxr16] = "xorw";
  OP_Name[TOP_xorixr16_n32] = "xorw";
  OP_Name[TOP_xorixr32] = "xorl";
  OP_Name[TOP_xorixxr32] = "xorl";
  OP_Name[TOP_xorixxxr32] = "xorl";
  OP_Name[TOP_xorixr32_n32] = "xorl";
  OP_Name[TOP_xorixr64] = "xorq";
  OP_Name[TOP_xorixxr64] = "xorq";
  OP_Name[TOP_xorixxxr64] = "xorq";
  OP_Name[TOP_xorixr64_off] = "xorq";
  OP_Name[TOP_negxr8] = "negb";
  OP_Name[TOP_negxr16] = "negw";
  OP_Name[TOP_negxr32] = "negl";
  OP_Name[TOP_negxr64] = "negq";
  OP_Name[TOP_negxxr8] = "negb";
  OP_Name[TOP_negxxr16] = "negw";
  OP_Name[TOP_negxxr32] = "negl";
  OP_Name[TOP_negxxr64] = "negq";
  OP_Name[TOP_negxxxr8] = "negb";
  OP_Name[TOP_negxxxr16] = "negw";
  OP_Name[TOP_negxxxr32] = "negl";
  OP_Name[TOP_negxxxr64] = "negq";
  OP_Name[TOP_negxr8_n32] = "negb";
  OP_Name[TOP_negxr16_n32] = "negw";
  OP_Name[TOP_negxr32_n32] = "negl";
  OP_Name[TOP_negxr64_off] = "negq";
  OP_Name[TOP_notxr8] = "notb";
  OP_Name[TOP_notxr16] = "notw";
  OP_Name[TOP_notxr32] = "notl";
  OP_Name[TOP_notxr64] = "notq";
  OP_Name[TOP_notxxr8] = "notb";
  OP_Name[TOP_notxxr16] = "notw";
  OP_Name[TOP_notxxr32] = "notl";
  OP_Name[TOP_notxxr64] = "notq";
  OP_Name[TOP_notxxxr8] = "notb";
  OP_Name[TOP_notxxxr16] = "notw";
  OP_Name[TOP_notxxxr32] = "notl";
  OP_Name[TOP_notxxxr64] = "notq";
  OP_Name[TOP_notxr8_n32] = "notb";
  OP_Name[TOP_notxr16_n32] = "notw";
  OP_Name[TOP_notxr32_n32] = "notl";
  OP_Name[TOP_notxr64_off] = "notq";
  OP_Name[TOP_incxr8] = "incb";
  OP_Name[TOP_incxr16] = "incw";
  OP_Name[TOP_incxr32] = "incl";
  OP_Name[TOP_incxr64] = "incq";
  OP_Name[TOP_incxxr8] = "incb";
  OP_Name[TOP_incxxr16] = "incw";
  OP_Name[TOP_incxxr32] = "incl";
  OP_Name[TOP_incxxr64] = "incq";
  OP_Name[TOP_incxxxr8] = "incb";
  OP_Name[TOP_incxxxr16] = "incw";
  OP_Name[TOP_incxxxr32] = "incl";
  OP_Name[TOP_incxxxr64] = "incq";
  OP_Name[TOP_incxr8_n32] = "incb";
  OP_Name[TOP_incxr16_n32] = "incw";
  OP_Name[TOP_incxr32_n32] = "incl";
  OP_Name[TOP_incxr64_off] = "incq";
  OP_Name[TOP_decxr8] = "decb";
  OP_Name[TOP_decxr16] = "decw";
  OP_Name[TOP_decxr32] = "decl";
  OP_Name[TOP_decxr64] = "decq";
  OP_Name[TOP_decxxr8] = "decb";
  OP_Name[TOP_decxxr16] = "decw";
  OP_Name[TOP_decxxr32] = "decl";
  OP_Name[TOP_decxxr64] = "decq";
  OP_Name[TOP_decxxxr8] = "decb";
  OP_Name[TOP_decxxxr16] = "decw";
  OP_Name[TOP_decxxxr32] = "decl";
  OP_Name[TOP_decxxxr64] = "decq";
  OP_Name[TOP_decxr8_n32] = "decb";
  OP_Name[TOP_decxr16_n32] = "decw";
  OP_Name[TOP_decxr32_n32] = "decl";
  OP_Name[TOP_decxr64_off] = "decq";

  OP_Name[TOP_ld32]  = "movl";
  OP_Name[TOP_ldx32] = "movl";
  OP_Name[TOP_ldxx32]= "movl";
  OP_Name[TOP_ld32_n32]  = "movl";
  OP_Name[TOP_ld64]  = "movq";
  OP_Name[TOP_ld64_off]  = "movq";
  OP_Name[TOP_ldx64] = "movq";
  OP_Name[TOP_ldxx64]= "movq";
  OP_Name[TOP_zero32] = "xorl";
  OP_Name[TOP_zero64] = "xorq";
  OP_Name[TOP_xzero32] = "xorps";
  OP_Name[TOP_xzero64] = "xorps";
  OP_Name[TOP_xzero128v32] = "xorps";
  OP_Name[TOP_xzero128v64] = "xorps";
  OP_Name[TOP_inc8] = "incb";
  OP_Name[TOP_inc16] = "incw";
  OP_Name[TOP_inc32] = "incl";
  OP_Name[TOP_inc64] = "incq";
  OP_Name[TOP_dec8] = "decb";
  OP_Name[TOP_dec16] = "decw";
  OP_Name[TOP_dec32] = "decl";
  OP_Name[TOP_dec64] = "decq";
  OP_Name[TOP_ldc32] = "movl";
  OP_Name[TOP_ldc64] = "movq";
  OP_Name[TOP_ld32_64]   = "movslq";
  OP_Name[TOP_ldx32_64]  = "movslq";
  OP_Name[TOP_ldxx32_64] = "movslq";
  OP_Name[TOP_mov32] = "movl";
  OP_Name[TOP_mov64] = "movq";

  OP_Name[TOP_movzlq] = "movl";

  OP_Name[TOP_fstps_n32] = "fstps";
  OP_Name[TOP_fstpl_n32] = "fstpl";
  OP_Name[TOP_fstpt_n32] = "fstpt";
  OP_Name[TOP_fsts_n32] = "fsts";
  OP_Name[TOP_fstl_n32] = "fstl";

  OP_Name[TOP_flds_n32]  = "flds";
  OP_Name[TOP_fldl_n32]  = "fldl";
  OP_Name[TOP_fldt_n32]  = "fldt";

  OP_Name[TOP_divxsd]  = "divsd";
  OP_Name[TOP_divxxsd] = "divsd";
  OP_Name[TOP_divxxxsd]= "divsd";
  OP_Name[TOP_divxss]  = "divss";
  OP_Name[TOP_divxxss] = "divss";
  OP_Name[TOP_divxxxss]= "divss";
  OP_Name[TOP_imul32] = "imull";
  OP_Name[TOP_imul64] = "imulq";
  OP_Name[TOP_imulx64] = "imulq";
  OP_Name[TOP_imulx32] = "imull";
  OP_Name[TOP_mul32] = "mull";
  OP_Name[TOP_mulx64] = "mulq";
  OP_Name[TOP_mulxss] = "mulss";
  OP_Name[TOP_mulxsd] = "mulsd";
  OP_Name[TOP_mulxxss] = "mulss";
  OP_Name[TOP_mulxxxss]= "mulss";
  OP_Name[TOP_mulxxsd] = "mulsd";
  OP_Name[TOP_mulxxxsd]= "mulsd";
  OP_Name[TOP_fmul128v32] = "mulps";
  OP_Name[TOP_fmulx128v32] = "mulps";
  OP_Name[TOP_fmulxx128v32] = "mulps";
  OP_Name[TOP_fmulxxx128v32] = "mulps";
  OP_Name[TOP_fmul128v64] = "mulpd";
  OP_Name[TOP_fmulx128v64] = "mulpd";
  OP_Name[TOP_fmulxx128v64] = "mulpd";
  OP_Name[TOP_fmulxxx128v64] = "mulpd";
  OP_Name[TOP_neg8] = "negb";
  OP_Name[TOP_neg16] = "negw";
  OP_Name[TOP_neg32] = "negl";
  OP_Name[TOP_neg64] = "negq";
  OP_Name[TOP_not8] = "notb";
  OP_Name[TOP_not16] = "notw";
  OP_Name[TOP_not32] = "notl";
  OP_Name[TOP_not64] = "notq";
  OP_Name[TOP_or32]   = "orl";
  OP_Name[TOP_orx32]  = "orl";
  OP_Name[TOP_orxx32] = "orl";
  OP_Name[TOP_orxxx32]= "orl";
  OP_Name[TOP_or64]   = "orq";
  OP_Name[TOP_orx64]  = "orq";
  OP_Name[TOP_orxx64]  = "orq";
  OP_Name[TOP_orxxx64] = "orq";
  OP_Name[TOP_ori32] = "orl";
  OP_Name[TOP_ori64] = "orq";
  OP_Name[TOP_pmovmskb128] = "pmovmskb";
  OP_Name[TOP_ror8] = "rorb";
  OP_Name[TOP_ror16] = "rorw";
  OP_Name[TOP_ror32] = "rorl";
  OP_Name[TOP_ror64] = "rorq";
  OP_Name[TOP_rori8] = "rorb";
  OP_Name[TOP_rori16] = "rorw";
  OP_Name[TOP_rori32] = "rorl";
  OP_Name[TOP_rori64] = "rorq";
  OP_Name[TOP_rol8] = "rolb";
  OP_Name[TOP_rol16] = "rolw";
  OP_Name[TOP_rol32] = "roll";
  OP_Name[TOP_rol64] = "rolq";
  OP_Name[TOP_roli8] = "rolb";
  OP_Name[TOP_roli16] = "rolw";
  OP_Name[TOP_roli32] = "roll";
  OP_Name[TOP_roli64] = "rolq";
  OP_Name[TOP_store8]   = "movb";
  OP_Name[TOP_storex8]  = "movb";
  OP_Name[TOP_storexx8] = "movb";
  OP_Name[TOP_store8_n32] = "movb";
  OP_Name[TOP_storei8]   = "movb";
  OP_Name[TOP_storeix8]  = "movb";
  OP_Name[TOP_storeixx8] = "movb";
  OP_Name[TOP_storei8_n32] = "movb";
  OP_Name[TOP_store16]   = "movw";
  OP_Name[TOP_storex16]  = "movw";
  OP_Name[TOP_storexx16] = "movw";
  OP_Name[TOP_store16_n32] = "movw";
  OP_Name[TOP_storei16]   = "movw";
  OP_Name[TOP_storeix16]  = "movw";
  OP_Name[TOP_storeixx16] = "movw";
  OP_Name[TOP_storei16_n32] = "movw";
  OP_Name[TOP_store32]  = "movl";
  OP_Name[TOP_storex32] = "movl";
  OP_Name[TOP_storexx32]= "movl";
  OP_Name[TOP_store32_n32]  = "movl";
  OP_Name[TOP_storei32]  = "movl";
  OP_Name[TOP_storeix32] = "movl";
  OP_Name[TOP_storeixx32]= "movl";
  OP_Name[TOP_storei32_n32]  = "movl";
  OP_Name[TOP_store64]  = "movq";
  OP_Name[TOP_storex64] = "movq";
  OP_Name[TOP_storexx64]= "movq";
  OP_Name[TOP_store64_off]  = "movq";
  OP_Name[TOP_storei64]  = "movq";
  OP_Name[TOP_storeix64] = "movq";
  OP_Name[TOP_storeixx64]= "movq";
  OP_Name[TOP_storei64_off]  = "movq";
  OP_Name[TOP_storenti32]  = "movnti";
  OP_Name[TOP_storentix32] = "movnti";
  OP_Name[TOP_storentixx32]= "movnti";
  OP_Name[TOP_storenti64]  = "movnti";
  OP_Name[TOP_storentix64] = "movnti";
  OP_Name[TOP_storentixx64]= "movnti";
  OP_Name[TOP_sar32] = "sarl";
  OP_Name[TOP_sar64] = "sarq";
  OP_Name[TOP_sari32] = "sarl";
  OP_Name[TOP_sari64] = "sarq";
  OP_Name[TOP_shld32]  = "shldl";
  OP_Name[TOP_shldi32] = "shldl";
  OP_Name[TOP_shrd32]  = "shrdl";
  OP_Name[TOP_shrdi32] = "shrdl";
  OP_Name[TOP_shl32] = "shll";
  OP_Name[TOP_shl64] = "shlq";
  OP_Name[TOP_shli32] = "shll";
  OP_Name[TOP_shli64] = "shlq";
  OP_Name[TOP_shr32] = "shrl";
  OP_Name[TOP_shr64] = "shrq";
  OP_Name[TOP_shri32] = "shrl";
  OP_Name[TOP_shri64] = "shrq";
  OP_Name[TOP_sub32] = "subl";
  OP_Name[TOP_sbb32] = "sbbl";
  OP_Name[TOP_sub64] = "subq";
  OP_Name[TOP_subx32] = "subl";
  OP_Name[TOP_subx64] = "subq";
  OP_Name[TOP_subxx32]  = "subl";
  OP_Name[TOP_subxxx32] = "subl";
  OP_Name[TOP_subxx64]  = "subq";
  OP_Name[TOP_subxxx64] = "subq";
  OP_Name[TOP_subi32] = "subl";
  OP_Name[TOP_sbbi32] = "sbbl";
  OP_Name[TOP_subi64] = "subq";
  OP_Name[TOP_subxss] = "subss";
  OP_Name[TOP_subxsd] = "subsd";
  OP_Name[TOP_subxxss]  = "subss";
  OP_Name[TOP_subxxxss] = "subss";
  OP_Name[TOP_subxxsd]  = "subsd";
  OP_Name[TOP_subxxxsd] = "subsd";
  OP_Name[TOP_sub128v8] = "psubb";
  OP_Name[TOP_subx128v8] = "psubb";
  OP_Name[TOP_subxx128v8] = "psubb";
  OP_Name[TOP_subxxx128v8] = "psubb";
  OP_Name[TOP_sub128v16] = "psubw";
  OP_Name[TOP_subx128v16] = "psubw";
  OP_Name[TOP_subxx128v16] = "psubw";
  OP_Name[TOP_subxxx128v16] = "psubw";
  OP_Name[TOP_sub128v32] = "psubd";
  OP_Name[TOP_subx128v32] = "psubd";
  OP_Name[TOP_subxx128v32] = "psubd";
  OP_Name[TOP_subxxx128v32] = "psubd";
  OP_Name[TOP_sub128v64] = "psubq";
  OP_Name[TOP_subx128v64] = "psubq";
  OP_Name[TOP_subxx128v64] = "psubq";
  OP_Name[TOP_subxxx128v64] = "psubq";
  OP_Name[TOP_fsub128v32] = "subps";
  OP_Name[TOP_fsubx128v32] = "subps";
  OP_Name[TOP_fsubxx128v32] = "subps";
  OP_Name[TOP_fsubxxx128v32] = "subps";
  OP_Name[TOP_fsub128v64] = "subpd";
  OP_Name[TOP_fsubx128v64] = "subpd";
  OP_Name[TOP_fsubxx128v64] = "subpd";
  OP_Name[TOP_fsubxxx128v64] = "subpd";
  OP_Name[TOP_test8]   = "testb";
  OP_Name[TOP_testx8]  = "testb";
  OP_Name[TOP_testxx8]  = "testb";
  OP_Name[TOP_testxxx8] = "testb";
  OP_Name[TOP_test16]   = "testw";
  OP_Name[TOP_testx16]  = "testw";
  OP_Name[TOP_testxx16]  = "testw";
  OP_Name[TOP_testxxx16] = "testw";
  OP_Name[TOP_test32]   = "testl";
  OP_Name[TOP_testx32]  = "testl";
  OP_Name[TOP_testxx32]  = "testl";
  OP_Name[TOP_testxxx32] = "testl";
  OP_Name[TOP_test64]   = "testq";
  OP_Name[TOP_testx64]  = "testq";
  OP_Name[TOP_testxx64] = "testq";
  OP_Name[TOP_testxxx64]= "testq";
  OP_Name[TOP_testi8]  = "testb";
  OP_Name[TOP_testi16]  = "testw";
  OP_Name[TOP_testi32]  = "testl";
  OP_Name[TOP_testi64]  = "testq";
  OP_Name[TOP_xor32]   = "xorl";
  OP_Name[TOP_xorx32]  = "xorl";
  OP_Name[TOP_xorxx32] = "xorl";
  OP_Name[TOP_xorxxx32]= "xorl";
  OP_Name[TOP_xor64]   = "xorq";
  OP_Name[TOP_xorx64]  = "xorq";
  OP_Name[TOP_xorxx64] = "xorq";
  OP_Name[TOP_xorxxx64]= "xorq";
  OP_Name[TOP_xori32] = "xorl";
  OP_Name[TOP_xori64] = "xorq";
  OP_Name[TOP_ldss] = "movss";
  OP_Name[TOP_ldss_n32] = "movss";
  OP_Name[TOP_ldssx]  = "movss";
  OP_Name[TOP_ldssxx] = "movss";
  OP_Name[TOP_lddqa] = "movdqa";
  OP_Name[TOP_lddqa_n32] = "movdqa";
  OP_Name[TOP_stdqa] = "movdqa";
  OP_Name[TOP_stdqa_n32] = "movdqa";
  OP_Name[TOP_stntpd]= "movntpd";
  OP_Name[TOP_stntps]= "movntps";
  if ( !Is_Target_SSE3() || ! CG_use_lddqu) {
    OP_Name[TOP_lddqu] = "movdqu";
    OP_Name[TOP_lddqu_n32] = "movdqu";
    OP_Name[TOP_vlddqu] = "vmovdqu";
    OP_Name[TOP_vlddqu_n32] = "vmovdqu";
  }
  else
    OP_Name[TOP_lddqu_n32] = "lddqu";
  OP_Name[TOP_stdqu] = "movdqu";
  OP_Name[TOP_stdqu_n32] = "movdqu";
  OP_Name[TOP_ldlps] = "movlps";
  OP_Name[TOP_ldlps_n32] = "movlps";
  OP_Name[TOP_stlps] = "movlps";
  OP_Name[TOP_stlps_n32] = "movlps";
  OP_Name[TOP_ldlpd] = "movsd";
  OP_Name[TOP_ldlpd_n32] = "movsd";
  OP_Name[TOP_stlpd] = "movsd";
  OP_Name[TOP_stlpd_n32] = "movsd";
  OP_Name[TOP_ldlpsx] = "movlps";
  OP_Name[TOP_stlpsx] = "movlps";
  OP_Name[TOP_ldlpdx] = "movsd";
  OP_Name[TOP_stlpdx] = "movsd";
  OP_Name[TOP_ldlpsxx] = "movlps";
  OP_Name[TOP_stlpsxx] = "movlps";
  OP_Name[TOP_ldlpdxx] = "movsd";
  OP_Name[TOP_stlpdxx] = "movsd";
  OP_Name[TOP_ldhps] = "movhps";
  OP_Name[TOP_ldhps_n32] = "movhps";
  OP_Name[TOP_sthps] = "movhps";
  OP_Name[TOP_sthps_n32] = "movhps";
  OP_Name[TOP_ldhpd] = "movhpd";
  OP_Name[TOP_ldhpd_n32] = "movhpd";
  OP_Name[TOP_sthpd] = "movhpd";
  OP_Name[TOP_sthpd_n32] = "movhpd";
  OP_Name[TOP_lddqax] = "movdqa";
  OP_Name[TOP_stdqax] = "movdqa";
  OP_Name[TOP_stntpdx]= "movntpd";
  OP_Name[TOP_stntpsx]= "movntps";
  if ( !Is_Target_SSE3() || ! CG_use_lddqu) {
    OP_Name[TOP_lddqux] = "movdqu";
    OP_Name[TOP_vlddqux] = "vmovdqu";
  } else
    OP_Name[TOP_lddqux] = "lddqu";
  OP_Name[TOP_stdqux] = "movdqu";
  OP_Name[TOP_ldhpsx] = "movhps";
  OP_Name[TOP_sthpsx] = "movhps";
  OP_Name[TOP_ldhpdx] = "movhpd";
  OP_Name[TOP_sthpdx] = "movhpd";
  OP_Name[TOP_lddqaxx] = "movdqa";
  OP_Name[TOP_stdqaxx] = "movdqa";
  OP_Name[TOP_stntpdxx]= "movntpd";
  OP_Name[TOP_stntpsxx]= "movntps";
  if ( !Is_Target_SSE3() || ! CG_use_lddqu) {
    OP_Name[TOP_lddquxx] = "movdqu";
    OP_Name[TOP_vlddquxx] = "vmovdqu";
  } else
    OP_Name[TOP_lddquxx] = "lddqu";
  OP_Name[TOP_stdquxx] = "movdqu";
  OP_Name[TOP_ldhpsxx] = "movhps";
  OP_Name[TOP_sthpsxx] = "movhps";
  OP_Name[TOP_ldhpdxx] = "movhpd";
  OP_Name[TOP_sthpdxx] = "movhpd";
  OP_Name[TOP_staps] = "movaps";
  OP_Name[TOP_staps_n32] = "movaps";
  OP_Name[TOP_stapd] = "movapd";
  OP_Name[TOP_stapd_n32] = "movapd";
  OP_Name[TOP_stupd] = "movupd";
  OP_Name[TOP_stupdx] = "movupd";
  OP_Name[TOP_stupdxx] = "movupd";
  OP_Name[TOP_stupd_n32] = "movupd";
  OP_Name[TOP_stups] = "movups";
  OP_Name[TOP_stupsx] = "movups";
  OP_Name[TOP_stupsxx] = "movups";
  OP_Name[TOP_stups_n32] = "movups";
  OP_Name[TOP_ldaps] = "movaps";
  OP_Name[TOP_ldaps_n32] = "movaps";
  OP_Name[TOP_ldapd] = "movapd";
  OP_Name[TOP_ldapd_n32] = "movapd";
  OP_Name[TOP_stapsx] = "movaps";
  OP_Name[TOP_stapdx] = "movapd";
  OP_Name[TOP_ldapsx] = "movaps";
  OP_Name[TOP_ldapdx] = "movapd";
  OP_Name[TOP_stapsxx] = "movaps";
  OP_Name[TOP_stapdxx] = "movapd";
  OP_Name[TOP_ldapsxx] = "movaps";
  OP_Name[TOP_ldapdxx] = "movapd";
  OP_Name[TOP_ldupd] = "movupd";
  OP_Name[TOP_ldupdx] = "movupd";
  OP_Name[TOP_ldupdxx] = "movupd";
  OP_Name[TOP_ldupd_n32] = "movupd";
  OP_Name[TOP_ldups] = "movups";
  OP_Name[TOP_ldupsx] = "movups";
  OP_Name[TOP_ldupsxx] = "movups";
  OP_Name[TOP_ldups_n32] = "movups";
  OP_Name[TOP_movdq] = "movdqa";
  OP_Name[TOP_movg2x] = "movd";
  OP_Name[TOP_movg2x64] = "movd";
  OP_Name[TOP_movx2g] = "movd";
  OP_Name[TOP_movx2g64] = "movd";
  OP_Name[TOP_stss_n32]   = "movss";
  OP_Name[TOP_stss]   = "movss";
  OP_Name[TOP_stssx]  = "movss";
  OP_Name[TOP_stssxx] = "movss";

  //SSE4 instructions 
  OP_Name[TOP_stntss]   = "movntss";
  OP_Name[TOP_stntssx]  = "movntss";
  OP_Name[TOP_stntssxx] = "movntss";
  OP_Name[TOP_stntsd]   = "movntsd";
  OP_Name[TOP_stntsdx]  = "movntsd";
  OP_Name[TOP_stntsdxx] = "movntsd";
  
  OP_Name[TOP_fmov] = "fld";
  OP_Name[TOP_ld8_abs]  = "movabsb";
  OP_Name[TOP_ld16_abs] = "movabsw";
  OP_Name[TOP_ld32_abs] = "movabsl";
  OP_Name[TOP_ld64_abs] = "movabsq";
  OP_Name[TOP_store8_abs]  = "movabsb";
  OP_Name[TOP_store16_abs] = "movabsw";
  OP_Name[TOP_store32_abs] = "movabsl";
  OP_Name[TOP_store64_abs] = "movabsq";

  OP_Name[TOP_ld32_gs_seg_off]	= "movl";
  OP_Name[TOP_ld64_fs_seg_off]	= "movq";

  OP_Name[TOP_cvtsd2ss_x]   = "cvtsd2ss";
  OP_Name[TOP_cvtsd2ss_xx]  = "cvtsd2ss";
  OP_Name[TOP_cvtsd2ss_xxx] = "cvtsd2ss";
  OP_Name[TOP_cvtsi2sd_x]   = "cvtsi2sd";
  OP_Name[TOP_cvtsi2sd_xx]  = "cvtsi2sd";
  OP_Name[TOP_cvtsi2sd_xxx] = "cvtsi2sd";
  OP_Name[TOP_cvtsi2ss_x]   = "cvtsi2ss";
  OP_Name[TOP_cvtsi2ss_xx]  = "cvtsi2ss";
  OP_Name[TOP_cvtsi2ss_xxx] = "cvtsi2ss";
  OP_Name[TOP_cvtsi2sdq_x]  = "cvtsi2sdq";
  OP_Name[TOP_cvtsi2sdq_xx] = "cvtsi2sdq";
  OP_Name[TOP_cvtsi2sdq_xxx]= "cvtsi2sdq";
  OP_Name[TOP_cvtsi2ssq_x]  = "cvtsi2ssq";
  OP_Name[TOP_cvtsi2ssq_xx] = "cvtsi2ssq";
  OP_Name[TOP_cvtsi2ssq_xxx]= "cvtsi2ssq";
  
  OP_Name[TOP_cvtdq2pd_x] = "cvtdq2pd";
  OP_Name[TOP_cvtdq2ps_x] = "cvtdq2ps";
  OP_Name[TOP_cvtps2pd_x] = "cvtps2pd";
  OP_Name[TOP_cvtpd2ps_x] = "cvtpd2ps";
  OP_Name[TOP_cvttps2dq_x] = "cvttps2dq";
  OP_Name[TOP_cvttpd2dq_x] = "cvttpd2dq";
  OP_Name[TOP_cvtdq2pd_xx] = "cvtdq2pd";
  OP_Name[TOP_cvtdq2ps_xx] = "cvtdq2ps";
  OP_Name[TOP_cvtps2pd_xx] = "cvtps2pd";
  OP_Name[TOP_cvtpd2ps_xx] = "cvtpd2ps";
  OP_Name[TOP_cvttps2dq_xx] = "cvttps2dq";
  OP_Name[TOP_cvttpd2dq_xx] = "cvttpd2dq";
  OP_Name[TOP_cvtdq2pd_xxx] = "cvtdq2pd";
  OP_Name[TOP_cvtdq2ps_xxx] = "cvtdq2ps";
  OP_Name[TOP_cvtps2pd_xxx] = "cvtps2pd";
  OP_Name[TOP_cvtpd2ps_xxx] = "cvtpd2ps";
  OP_Name[TOP_cvttps2dq_xxx] = "cvttps2dq";
  OP_Name[TOP_cvttpd2dq_xxx] = "cvttpd2dq";
//**********************************************************
// For barcelona (bug 13108)
// (1) "movlpd reg, mem"  ==> "movsd reg, mem"
// (2) "movsd reg, reg"   ==> "movapd reg, reg"
// NOTE: there are regardless of CG_use_movlpd TRUE or FALSE
//***********************************************************
  if (CG_use_movlpd &&
      !Is_Target_Pentium4() &&
      !Is_Target_EM64T() &&
      !Is_Target_Core() &&
      !Is_Target_Wolfdale() &&
      !Is_Target_Orochi() &&
      !Is_Target_Barcelona()){// bug 10295
    // Use movlpd only for loads.  Bug 5809.
    OP_Name[TOP_ldsd] = "movlpd";
    OP_Name[TOP_ldsd_n32] = "movlpd";
    OP_Name[TOP_stsdx]  = "movsd";
    OP_Name[TOP_stsdxx] = "movsd";
    OP_Name[TOP_ldsdx]  = "movlpd";
    OP_Name[TOP_ldsdxx] = "movlpd";
    OP_Name[TOP_stsd] = "movsd";
    OP_Name[TOP_stsd_n32] = "movsd";
    OP_Name[TOP_storelpd] = "movlpd";
  }
  else{
    OP_Name[TOP_ldsd] = "movsd";
    OP_Name[TOP_ldsd_n32] = "movsd";
    OP_Name[TOP_stsdx]  = "movsd";
    OP_Name[TOP_stsdxx] = "movsd";
    OP_Name[TOP_ldsdx]  = "movsd";
    OP_Name[TOP_ldsdxx] = "movsd";
    OP_Name[TOP_stsd] = "movsd";
    OP_Name[TOP_stsd_n32] = "movsd";
    OP_Name[TOP_storelpd] = "movsd";
    // Convert movapd reg reg moves with movaps
    OP_Name[TOP_movapd]  = "movaps";
    if (Is_Target_Orochi()) {
      OP_Name[TOP_vldsd] = "vmovsd";
      OP_Name[TOP_vldsd_n32] = "vmovsd";
      OP_Name[TOP_vstsdx]  = "vmovsd";
      OP_Name[TOP_vstsdxx] = "vmovsd";
      OP_Name[TOP_vldsdx]  = "vmovsd";
      OP_Name[TOP_vldsdxx] = "vmovsd";
      OP_Name[TOP_vstsd] = "vmovsd";
      OP_Name[TOP_vstsd_n32] = "vmovsd";
      OP_Name[TOP_vstorelpd] = "vmovsd";
    }
  }

  OP_Name[TOP_lock_add8] = "addb";
  OP_Name[TOP_lock_add16] = "addw";
  OP_Name[TOP_lock_add32] = "addl";
  OP_Name[TOP_lock_adc32] = "adcl";
  OP_Name[TOP_lock_add64] = "addq";

  OP_Name[TOP_lock_cmpxchg8] = "cmpxchgb";
  OP_Name[TOP_lock_cmpxchg16] = "cmpxchgw";
  OP_Name[TOP_lock_cmpxchg32] = "cmpxchgl";
  OP_Name[TOP_lock_cmpxchg64] = "cmpxchgq";

  OP_Name[TOP_lock_and8] = "andb";
  OP_Name[TOP_lock_and16] = "andw";
  OP_Name[TOP_lock_and32] = "andl";
  OP_Name[TOP_lock_and64] = "andq";

  OP_Name[TOP_lock_or8] = "orb";
  OP_Name[TOP_lock_or16] = "orw";
  OP_Name[TOP_lock_or32] = "orl";
  OP_Name[TOP_lock_or64] = "orq";

  OP_Name[TOP_lock_xor8] = "xorb";
  OP_Name[TOP_lock_xor16] = "xorw";
  OP_Name[TOP_lock_xor32] = "xorl";
  OP_Name[TOP_lock_xor64] = "xorq";

  OP_Name[TOP_lock_sub8] = "subb";
  OP_Name[TOP_lock_sub16] = "subw";
  OP_Name[TOP_lock_sub32] = "subl";
  OP_Name[TOP_lock_sub64] = "subq";

  OP_Name[TOP_lock_xadd8] = "xaddb";
  OP_Name[TOP_lock_xadd16] = "xaddw";
  OP_Name[TOP_lock_xadd32] = "xaddl";
  OP_Name[TOP_lock_xadd64] = "xaddq";

  OP_Name[TOP_lock_xchg8] = "xchgb";
  OP_Name[TOP_lock_xchg16] = "xchgw";
  OP_Name[TOP_lock_xchg32] = "xchgl";
  OP_Name[TOP_lock_xchg64] = "xchgq";

  OP_Name[TOP_bsf32] = "bsfl";
  OP_Name[TOP_bsf64] = "bsfq";
  OP_Name[TOP_bsr32] = "bsrl";
  OP_Name[TOP_bsr64] = "bsrq";

  OP_Name[TOP_ld64_2m] = "movq";
  OP_Name[TOP_ld64_2m_n32] = "movq";
  OP_Name[TOP_store64_fm] = "movq";
  OP_Name[TOP_store64_fm_n32] = "movq";
  OP_Name[TOP_storent64_fm] = "movntq";
  OP_Name[TOP_ld64_2sse] = "movq";
  OP_Name[TOP_ld64_2sse_n32] = "movq";
  OP_Name[TOP_store64_fsse] = "movq";
  OP_Name[TOP_store64_fsse_n32] = "movq";
  OP_Name[TOP_mov64_m] = "movq";
  OP_Name[TOP_add64v8] = "paddb";
  OP_Name[TOP_add64v16] = "paddw";
  OP_Name[TOP_add64v32] = "paddd";
  OP_Name[TOP_sub64v8] = "psubb";
  OP_Name[TOP_sub64v16] = "psubw";
  OP_Name[TOP_sub64v32] = "psubd";
  OP_Name[TOP_max64v8] = "pmaxub";
  OP_Name[TOP_max64v16] = "pmaxsw";
  OP_Name[TOP_min64v8] = "pminub";
  OP_Name[TOP_min64v16] = "pminsw";
  OP_Name[TOP_movi32_2m] = "movd";
  OP_Name[TOP_movi64_2m] = "movd";
  OP_Name[TOP_movm_2i32] = "movd";
  OP_Name[TOP_movm_2i64] = "movd";
  OP_Name[TOP_psrlq128v64] = "psrlq";
  OP_Name[TOP_storenti128] = "movntdq";
  OP_Name[TOP_pshufw64v16] = "pshufw";
  OP_Name[TOP_psllw_mmx] = "psllw";
  OP_Name[TOP_pslld_mmx] = "pslld";
  OP_Name[TOP_psrlw_mmx] = "psrlw";
  OP_Name[TOP_psrld_mmx] = "psrld";
  OP_Name[TOP_psraw_mmx] = "psraw";
  OP_Name[TOP_psrad_mmx] = "psrad";
  OP_Name[TOP_pand_mmx] = "pand";
  OP_Name[TOP_pandn_mmx] = "pandn";
  OP_Name[TOP_por_mmx] = "por";
  OP_Name[TOP_pxor_mmx] = "pxor";
  OP_Name[TOP_extrq] = "extrq";
  OP_Name[TOP_insertq] = "insertq";

  return;
}


/* bug#1592 */
// bug 3699
#define NAME_LEN 8192

enum OPND_REG { BYTE_REG = 0, WORD_REG, DWORD_REG, QWORD_REG, SSE2_REG, AVX_REG };

static enum OPND_REG Get_Opnd_Reg( OP* op, int opnd, ISA_REGISTER_CLASS rc)
{
  TOP topcode = OP_code(op);
  int num_bits = 0;
  ISA_REGISTER_CLASS opnd_rc;

  if( opnd >= 0 ){
    if( opnd == TOP_Find_Operand_Use(topcode,OU_base) ||
	opnd == TOP_Find_Operand_Use(topcode,OU_index) ){
      num_bits = Pointer_Size * 8;
      opnd_rc = ISA_REGISTER_CLASS_integer;
    } else {
      // For regular operands.
      const ISA_OPERAND_INFO* oinfo = ISA_OPERAND_Info(topcode);
      const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, opnd);

      num_bits = ISA_OPERAND_VALTYP_Size(otype);
      opnd_rc = ISA_OPERAND_VALTYP_Register_Class(otype);
    }

  } else {  // opnd < 0
    const ISA_OPERAND_INFO* oinfo = ISA_OPERAND_Info(topcode);
    const ISA_OPERAND_VALTYP* otype = ISA_OPERAND_INFO_Result(oinfo, 0);

    num_bits = ISA_OPERAND_VALTYP_Size(otype);
    opnd_rc = ISA_OPERAND_VALTYP_Register_Class(otype);
  }

  FmtAssert( rc == opnd_rc, 
             ("REGISTER_CLASS does not match. Something wrong in target info"));

  /* We might need to fix up isa_operands.cxx later.
     Also, don't count on TN_size().
   */
  if( Is_Target_32bit()           &&
      ( ( TOP_is_cond_move( topcode ) &&
	  !TOP_is_flop( topcode ) ) ||
	TOP_is_ijump( topcode ) ) ) {
    num_bits = 32;
  }

  if ( rc == ISA_REGISTER_CLASS_integer ) {
    // integer register class
    switch( num_bits ){
    case 8:   return BYTE_REG;
    case 16:  return WORD_REG;
    case 32:  return DWORD_REG;
    case 64:  return QWORD_REG;
    case 128: return SSE2_REG;
    default:
      FmtAssert( false, ("NYI") );
    }
  }
  else if ( rc == ISA_REGISTER_CLASS_float ) {
    // float register class
    // distiguish SSE and AVX size by TN_size
    if ( opnd >= 0 && TN_size ( OP_opnd ( op, opnd ) ) == 32 ) {
      num_bits = 256;
    }
    else if ( opnd == -1 && TN_size ( OP_result ( op, 0 ) ) == 32 ) {
      num_bits = 256;
    }
    switch( num_bits ) {
    case 32:
    case 64:
    case 128: return SSE2_REG;
    case 256: return AVX_REG;
    default:
      FmtAssert( false, ("NYI") );
    }
  }
  else {
    FmtAssert( false, ("NYI") );
  }

  return SSE2_REG;
}


static void Str_Prepend( char* str, char c )
{
  for( int i = strlen(str) + 1; i > 0; i-- ){
    str[i] = str[i-1];
  }

  str[0] = c;
}

static const char* int_reg_names[6][16] = {
  /* BYTE_REG: low 8-bit */
  { "%al", "%bl", "%bpl", "%spl", "%dil", "%sil", "%dl", "%cl",
    "%r8b",  "%r9b",  "%r10b", "%r11b", "%r12b", "%r13b", "%r14b", "%r15b" },
  /* WORD_REG: 16-bit */
  { "%ax", "%bx", "%bp", "%sp", "%di", "%si", "%dx", "%cx",
    "%r8w",  "%r9w",  "%r10w", "%r11w", "%r12w", "%r13w", "%r14w", "%r15w" },
  /* DWORD_REG: 32-bit */
  { "%eax", "%ebx", "%ebp", "%esp", "%edi", "%esi", "%edx", "%ecx",
    "%r8d",  "%r9d",  "%r10d", "%r11d", "%r12d", "%r13d", "%r14d", "%r15d" },
  /* QWORD_REG: 64-bit */
  { "%rax", "%rbx", "%rbp", "%rsp", "%rdi", "%rsi", "%rdx", "%rcx",
    "%r8",  "%r9",  "%r10", "%r11", "%r12", "%r13", "%r14", "%r15" },
  /* SSE2_REG: 128-bit */
  { "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7",
    "%xmm8", "%xmm9", "%xmm10", "%xmm11", "%xmm12", "%xmm13", "%xmm14", "%xmm15" },
  /* AVX_REG: 256-bit */
  { "%ymm0", "%ymm1", "%ymm2", "%ymm3", "%ymm4", "%ymm5", "%ymm6", "%ymm7",
    "%ymm8", "%ymm9", "%ymm10", "%ymm11", "%ymm12", "%ymm13", "%ymm14", "%ymm15" },
};

static void Adjust_Opnd_Name( OP* op, int opnd, char* name )
{
  const TOP topcode = OP_code( op );

  if( TOP_is_ijump( topcode ) &&
      ( opnd == OP_find_opnd_use(op,OU_target) ||
	opnd == OP_find_opnd_use(op,OU_offset) ) ){
    if ( Is_Target_32bit() ) { // Bug 4666
      const ISA_REGISTER_CLASS rc = ISA_REGISTER_CLASS_integer;
      const enum OPND_REG opnd_reg = Get_Opnd_Reg( op, opnd, rc );
      
      for( REGISTER reg = REGISTER_MIN; 
	   reg <= REGISTER_CLASS_last_register( rc ); reg++ ){
	const char* n = REGISTER_name( rc, reg );
	if( strcmp( n, name ) == 0 ){
	  strcpy( name, int_reg_names[opnd_reg][reg-REGISTER_MIN] );
	  break;
	}
      }      
    }
    Str_Prepend( name, '*' );
    return;
  }

  const ISA_OPERAND_INFO* oinfo = ISA_OPERAND_Info(topcode);
  const ISA_OPERAND_VALTYP* vtype = opnd >= 0
    ? ISA_OPERAND_INFO_Operand(oinfo, opnd)
    : ISA_OPERAND_INFO_Result(oinfo, 0);
    
  // Handle case where opnd is an imm. field.

  if( ISA_OPERAND_VALTYP_Is_Literal(vtype) ){
    // Bug 578
#if defined(BUILD_OS_DARWIN)
    // Mach-O linker evidently handles this automatically
#else /* defined(BUILD_OS_DARWIN) */
    // Add a reference to the PLT under -fPIC compilation.
    if ( Gen_PIC_Shared &&  
	 !TN_is_label( OP_opnd(op,opnd) ) &&
         TOP_is_jump(topcode) &&
         opnd == OP_find_opnd_use(op,OU_target) ) {
      ST* function = TN_var(OP_opnd(op, opnd));
      if ( function && !ST_is_export_local(function) )
        strcat( name, "@PLT" );
    }
#endif /* defined(BUILD_OS_DARWIN) */

    if (name[0] != '$' &&   /* A '$' has been added by CGEMIT_Relocs_In_Asm() */
	(opnd != OP_find_opnd_use(op,OU_offset) &&
	 opnd != OP_find_opnd_use(op,OU_scale)  &&
	 opnd != OP_find_opnd_use(op,OU_target)) &&
	// Constant is not 32-bit ABI thread-local symbol offset.
	(!(Is_Target_32bit() &&
	   (TN_relocs(OP_opnd(op, opnd)) == TN_RELOC_X8664_TPOFF32 ||
	    TN_relocs(OP_opnd(op, opnd)) == TN_RELOC_X8664_GOTTPOFF)))) {
      Str_Prepend(name, '$');
    }

    // Bug 506
    {
      char *nl = strstr(name, "\n");
      if (nl) {
	if (strstr (name, ")") == NULL)
	  name[nl - name] = '\0';
	else 
	  { name[nl - name] = ')'; name[nl - name + 1] = '\0'; }
      }
    }

    return;
  }

  // Return if the opnd is not a register.

  if( !ISA_OPERAND_VALTYP_Is_Register(vtype) )
    return;

  if( ISA_OPERAND_VALTYP_Register_Class(vtype) == ISA_REGISTER_CLASS_integer ||
      ISA_OPERAND_VALTYP_Register_Class(vtype) == ISA_REGISTER_CLASS_float ){
    const ISA_REGISTER_CLASS rc = ISA_OPERAND_VALTYP_Register_Class(vtype);
    const enum OPND_REG opnd_reg = Get_Opnd_Reg( op, opnd, rc );

    for( REGISTER reg = REGISTER_MIN; reg <= REGISTER_CLASS_last_register( rc ); reg++ ){
      const char* n = REGISTER_name( rc, reg );
      if( strcmp( n, name ) == 0 ){
	strcpy( name, int_reg_names[opnd_reg][reg-REGISTER_MIN] );
	return;
      }
    }
  } // ISA_REGISTER_CLASS_integer

  if( ISA_OPERAND_VALTYP_Register_Class(vtype) == ISA_REGISTER_CLASS_x87 ){
    extern int Get_OP_stack_reg( OP*, int );
    sprintf( name, "%%st(%d)", Get_OP_stack_reg( op, opnd ) );
    return;
  } // ISA_REGISTER_CLASS_x87
}


// Return x86 segment prefix override.
static const char *
Segment_Prefix (OP *op)
{
  if (CGTARG_Is_Thread_Local_Memory_OP(op)) {
    return Is_Target_32bit() ? "%gs:" : "%fs:";
  }
  return "";
}

INT CGEMIT_Print_Inst( OP* op, const char* result[], const char* opnd[], FILE* f )
{
  Init_OP_Name();

#ifdef TARG_X8664
  char result_name[ISA_OPERAND_max_results+1][NAME_LEN];  
#else
  char result_name[2][NAME_LEN];  // It is also the first opnd name.
#endif
  char opnd_name[9][NAME_LEN];
  const TOP topcode = OP_code(op);
  const ISA_PRINT_INFO* pinfo = ISA_PRINT_Info(topcode);
  char op_name[NAME_LEN];

  FmtAssert( pinfo != NULL, ("no ISA_PRINT_INFO for %s", TOP_Name(topcode)) );

  /* STEP 1:
     Collect all the opnds and result info. */

  const char* p = OP_Name[topcode] == NULL ? TOP_Name(topcode) : OP_Name[topcode];

  if (OP_prefix_lock(op))
    sprintf( op_name, "lock %s", p );
  else
    strcpy( op_name, p );


  INT i = 0, opnd_i = 0, result_i = 0;

  while( TRUE ){
    const INT comp = ISA_PRINT_INFO_Comp(pinfo, i);

    switch( comp ){

    case ISA_PRINT_COMP_opnd:
    case ISA_PRINT_COMP_opnd+1:
    case ISA_PRINT_COMP_opnd+2:
    case ISA_PRINT_COMP_opnd+3:
    case ISA_PRINT_COMP_opnd+4:
    case ISA_PRINT_COMP_opnd+5:
      Is_True(10 + strlen(opnd[comp - ISA_PRINT_COMP_opnd])
	        < sizeof(opnd_name[opnd_i]),
	      ("buffer size is too small") );
      strcpy(opnd_name[opnd_i], opnd[comp - ISA_PRINT_COMP_opnd]);
      Adjust_Opnd_Name(op, comp - ISA_PRINT_COMP_opnd, opnd_name[opnd_i]);
      opnd_i++;
      break;

    case ISA_PRINT_COMP_result:
    case ISA_PRINT_COMP_result+1:
      Is_True(10 + strlen(result[comp - ISA_PRINT_COMP_result])
	        < sizeof(result_name[result_i]),
	      ("buffer size is too small"));
      strcpy(result_name[result_i], result[comp - ISA_PRINT_COMP_result]);
      Adjust_Opnd_Name(op, -1, result_name[result_i]);
      result_i++;
      break;

    case ISA_PRINT_COMP_segment:
      {
	const char *segment_prefix = Segment_Prefix(op);
	Is_True(10 + strlen(segment_prefix) < sizeof(opnd_name[opnd_i]),
		("buffer size is too small"));
	strcpy(opnd_name[opnd_i], segment_prefix);
	opnd_i++;
      }
      break;

    case ISA_PRINT_COMP_name:
    case ISA_PRINT_COMP_end:
      break;

    default:
      FmtAssert(false, ("Unhandled listing component %d for %s",
			comp, TOP_Name(topcode)));
    }

    i++;
    if( comp == ISA_PRINT_COMP_end )
      break;
  }

  /* STEP 2:
     Print out the info. */

  for( i = 0; i < result_i; i++ ){
    strcpy( opnd_name[opnd_i], result_name[i] );
    opnd_i++;
  }

  INT st = fprintf( f, ISA_PRINT_INFO_Format(pinfo),
		    op_name,
		    opnd_name[0], opnd_name[1], opnd_name[2], opnd_name[3], 
		    opnd_name[4], opnd_name[5], opnd_name[6], opnd_name[7],
		    opnd_name[8] );

  FmtAssert( st != -1, ("fprintf failed: not enough disk space") );

  return st;
}


void CGEMIT_Setup_Ctrl_Register( FILE* f )
{
  // Set x87 precision (32/64/80-bit).  Bug 4327.
  int x87_mask;
  switch (Target_x87_precision()) {
    case 32:  x87_mask = 0; break;	// single precision
    case 64:  x87_mask = 0x200; break;	// double precision
    case 80:  x87_mask = 0x300; break;	// double-extended precision
    default:  FmtAssert(FALSE,
			("CGEMIT_Setup_Ctrl_Register: invalid x87 precision"));
  }
  // If trapuv, turn on exception for invalid operands.
  unsigned int x86_cntrl_mask = DEBUG_Trap_Uv ? 0xfcfe : 0xfcff;
  if( Is_Target_64bit() ){
    fprintf( f, "\taddq    $-8,%%rsp\n"             );
    fprintf( f, "\tfnstcw  (%%rsp)\n"               );
    fprintf( f, "\tandw    $0x%x,(%%rsp)\n", x86_cntrl_mask);
    fprintf( f, "\torw     $%d,(%%rsp)\n", x87_mask );
    fprintf( f, "\tfldcw   (%%rsp)\n"               );
    fprintf( f, "\taddq    $8,%%rsp\n"              );
  } else {
    fprintf( f, "\taddl    $-4,%%esp\n"             );
    fprintf( f, "\tfnstcw  (%%esp)\n"               );
    fprintf( f, "\tandw    $0x%x,(%%esp)\n", x86_cntrl_mask);
    fprintf( f, "\torw     $%d,(%%esp)\n", x87_mask );
    fprintf( f, "\tfldcw   (%%esp)\n"               );
    fprintf( f, "\taddl    $4,%%esp\n"              );
  }

  BOOL is_MAIN__ = !strcmp(Cur_PU_Name, "MAIN__");

  BOOL and_mask_set = (!SIMD_IMask || !SIMD_DMask || !SIMD_ZMask || 
		       !SIMD_OMask || !SIMD_UMask || !SIMD_PMask ||
		       DEBUG_Trap_Uv);

  BOOL or_mask_set  = (SIMD_AMask || SIMD_FMask);

  if (IEEE_Arithmetic <= IEEE_ACCURATE && !and_mask_set && !or_mask_set)
    return;

  /* The following sequence of code is used to turn off
     hardware underflow exception handling and/or to turn on
     flush to zero and denormalized as zero functionality.
   */

  const int mask = 32768 ;
  const int simd_imask = 0xff7f; // bit 7 : f f 0111 f - Invalid Operation Mask
  const int simd_dmask = 0xfeff; // bit 8 : f 1110 f f - Denormal Mask
  const int simd_zmask = 0xfdff; // bit 9 : f 1101 f f - Divide by Zero Mask
  const int simd_omask = 0xfbff; // bit 10: f 1011 f f - Overflow Mask
  const int simd_umask = 0xf7ff; // bit 11: f 0111 f f - Underflow Mask
  const int simd_pmask = 0xefff; // bit 12: 1110 f f f - Precision Mask

  const int simd_amask = 0x0040; // bit 6 : 0 0 0100 0 - Denorm as Zero
  const int simd_fmask = 0x8000; // bit 15: 1000 0 0 0 - Flush To Zero

  int and_mask = 0xffff;
  int or_mask = 0x0000;

  if ( !SIMD_IMask || DEBUG_Trap_Uv)		// trap invalid operands
    and_mask = and_mask & simd_imask;
  if ( !SIMD_DMask )
    and_mask = and_mask & simd_dmask;
  if ( !SIMD_ZMask )
    and_mask = and_mask & simd_zmask;
  if ( !SIMD_OMask )
    and_mask = and_mask & simd_omask;
  if ( !SIMD_UMask )
    and_mask = and_mask & simd_umask;
  if ( !SIMD_PMask )
    and_mask = and_mask & simd_pmask;

  if ( SIMD_AMask )
    or_mask = or_mask | simd_amask;
  if ( SIMD_FMask )
    or_mask = or_mask | simd_fmask;

  // bits 16..31 are 0, and must remain 0

  if( Is_Target_64bit() ){
    fprintf( f, "\t%s\n", "addq    $-8,%rsp"      );
    fprintf( f, "\t%s\n", "stmxcsr (%rsp)"        );
    if (IEEE_Arithmetic > IEEE_ACCURATE)
      fprintf( f, "\torq $%d, (%%rsp)\n", mask);
    else if (is_MAIN__)				// bug 8926
      fprintf( f, "\tandq $%d, (%%rsp)\n", ~mask);

    if ( and_mask_set )
      fprintf( f, "\tandq $%d, (%%rsp)\n", and_mask );
    if ( or_mask_set )
      fprintf( f, "\torq $%d, (%%rsp)\n", or_mask );

    fprintf( f, "\t%s\n", "ldmxcsr (%rsp)"        );
    fprintf( f, "\t%s\n", "addq    $8,%rsp"       );

  } else if Is_Target_SSE() {
    fprintf( f, "\t%s\n", "addl    $-8,%esp"      );
    fprintf( f, "\t%s\n", "stmxcsr (%esp)"        );
    if (IEEE_Arithmetic > IEEE_ACCURATE)
      fprintf( f, "\torl $%d, (%%esp)\n", mask);
    else if (is_MAIN__)				// bug 8926
      fprintf( f, "\tandl $%d, (%%esp)\n", ~mask);

    if ( and_mask_set )
      fprintf( f, "\tandl $%d, (%%esp)\n", and_mask );
    if ( or_mask_set )
      fprintf( f, "\torl $%d, (%%esp)\n", or_mask );

    fprintf( f, "\t%s\n", "ldmxcsr (%esp)"        );
    fprintf( f, "\t%s\n", "addl    $8,%esp"       );
  }

  return;
}

// Emits function used to retrieve the Instruction Pointer (bug 9675)
void CGEMIT_Setup_IP_Calc (void)
{
  fprintf (Asm_File, "\n\t.section .gnu.linkonce.t.%s,\"ax\",@progbits\n", ip_calc_funcname);
  fprintf (Asm_File, "\t.globl %s\n", ip_calc_funcname);
  fprintf (Asm_File, "\t.hidden %s\n", ip_calc_funcname);
  fprintf (Asm_File, "\t.type %s, @function\n", ip_calc_funcname);
  fprintf (Asm_File, "%s:\n", ip_calc_funcname);
  fprintf (Asm_File, "\tmovl (%%esp),%%ebx\n");
  fprintf (Asm_File, "\tret\n");
}
