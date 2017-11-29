/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/ia64/cgemit_targ.cxx,v $
 *
 * Description:
 *
 * Target-specific cgemit code.
 *
 * ====================================================================
 * ====================================================================
 */


#include <elf_stuff.h>

#define	USE_STANDARD_TYPES 1
#include "defs.h"
#include "targ_isa_lits.h"
#include "vstring.h"
#include "config_asm.h"
#include "em_elf.h"
#include "symtab.h"
#include "tn.h"
#include "cgemit.h"
#include "cgemit_targ.h"
#include "data_layout.h"
#include "bb.h"
#include "op.h"
#include "iface_scn.h"
#include "cg_flags.h"
#include "glob.h"
#include "targ_isa_print.h"

extern void
CGEMIT_Prn_File_Dir_In_Asm(USRCPOS usrcpos,
                        const char *pathname,
                        const char *filename)
{
    if( !CG_emit_asm_dwarf) 
      fprintf (Asm_File, "// "); //turn the rest into comment

    fprintf (Asm_File, "\t%s\t%d \"%s/%s\"\n",AS_FILE, 
		USRCPOS_filenum(usrcpos),
		pathname,filename);
}

extern void
CGEMIT_Prn_Line_Dir_In_Asm (USRCPOS usrcpos)
{
    if( !CG_emit_asm_dwarf) {
      fprintf (Asm_File, "// "); //turn the rest into comment
    }
    fprintf (Asm_File, "\t.loc\t%d\t%d\t%d\n", 
		USRCPOS_filenum(usrcpos),
		USRCPOS_linenum(usrcpos),
		USRCPOS_column(usrcpos));
}


extern void
CGEMIT_Prn_Scn_In_Asm (FILE       *asm_file,
		       const char *scn_name,
		       Elf64_Word  scn_type,
		       Elf64_Word  scn_flags,
		       Elf64_Xword scn_entsize,
		       Elf64_Word  scn_align,
		       const char *cur_scn_name)
{
  char scn_flags_string[5];
  char scn_type_string[10];  // min of strlen("progbits") + 1
  char *p = &scn_flags_string[0];

  fprintf (asm_file, "\n\t%s %s,", AS_SECTION, scn_name);
  if (scn_flags & SHF_WRITE) *p++ = 'w';
  if (scn_flags & SHF_ALLOC) *p++ = 'a';
  if (scn_flags & SHF_EXECINSTR) *p++ = 'x';
  if (scn_flags & SHF_TLS) *p++ = 'T';
  // short sections are only recognized by name, not by "s" qualifier
  // if (scn_flags & SHF_IRIX_GPREL) *p++ = 's';
  *p = '\0'; // null terminate the string.
  fprintf (asm_file, " \"%s\",", scn_flags_string);

  p = &scn_type_string[0];
  if (scn_type == SHT_NOBITS) {
    strcpy(p, "nobits");
  }
  else if (scn_type == SHT_PROGBITS) {
    strcpy(p, "progbits");
  }
  else {
    DevWarn("Intel assembler definition inadequate for "
	    "ELF section type 0x%llx; using \"progbits\"", (UINT64)scn_type);
    strcpy(p, "progbits");
  }
  fprintf (asm_file, " \"%s\"\n", scn_type_string);
  fprintf (asm_file, "\t%s\t%d\n", AS_ALIGN, scn_align);  
}

extern void
CGEMIT_Prn_Scn_In_Asm (ST *st, Elf64_Word scn_type, Elf64_Word scn_flags,
		       Elf64_Xword scn_entsize, ST *cur_section)
{
  CGEMIT_Prn_Scn_In_Asm(Asm_File, ST_name(st), scn_type, scn_flags,
			scn_entsize, STB_align(st),
			cur_section != NULL ? ST_name(cur_section) : NULL);
}

// whether to use the base st for the reloc
extern BOOL
CGEMIT_Use_Base_ST_For_Reloc (INT reloc, ST *st)
{
	if (reloc == TN_RELOC_IA_LTOFF_FPTR) 
		// gas doesn't like addends
		return FALSE;
        // OSP 490
        else if (ST_is_thread_local(st))
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
    	case TN_RELOC_IA_GPREL22:
       		*buf = vstr_concat (*buf, "@gprel");
		break;
	case TN_RELOC_IA_LTOFF22:
        	*buf = vstr_concat (*buf, "@ltoff");
		break;
	case TN_RELOC_IA_LTOFF22X:
        	*buf = vstr_concat (*buf, "@ltoffx");
		break;
	case TN_RELOC_IA_LTOFF_FPTR:
        	*buf = vstr_concat (*buf, "@ltoff(@fptr");
		++paren;
		break;
	case TN_RELOC_IA_LTOFF_DTPMOD22:
		*buf = vstr_concat (*buf, "@ltoff(@dtpmod");
		++paren;
		break;
	case TN_RELOC_IA_LTOFF_DTPREL22:
		*buf = vstr_concat (*buf, "@ltoff(@dtprel");
		++paren;
		break;
	case TN_RELOC_IA_DTPREL22:
		*buf = vstr_concat (*buf, "@dtprel");
		break;
	case TN_RELOC_IA_LTOFF_TPREL22:
		*buf = vstr_concat (*buf, "@ltoff(@tprel");
		++paren;
		break;
	case TN_RELOC_IA_TPREL22:
		*buf = vstr_concat (*buf, "@tprel");
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
	// only add in GP_DISP if based on gprel section
	// not if based on ipa-generated extern.
	if (ST_class(st) == CLASS_BLOCK && STB_section(st)) {
		*val -= GP_DISP;
	}
	switch (TN_relocs(t)) {
	case TN_RELOC_IA_GPREL22:
		Em_Add_New_Rela (EMT_Put_Elf_Symbol (st), 
			R_IA_64_GPREL22, PC, *val, PU_section);
	      	*val = 0;
		break;
	case TN_RELOC_IA_LTOFF22:
		Em_Add_New_Rela (EMT_Put_Elf_Symbol (st), 
			R_IA_64_LTOFF22, PC, *val, PU_section);
		*val = 0;
		break;
	case TN_RELOC_IA_LTOFF_FPTR:
		Em_Add_New_Rela (EMT_Put_Elf_Symbol (st), 
			R_IA_64_LTOFF_FPTR22, PC, *val, PU_section);
		*val = 0;
		break;
	default:
	      #pragma mips_frequency_hint NEVER
	      FmtAssert (FALSE, ("relocs_object: illegal reloc TN"));
	}
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
  // .fframe is only used for unwind info,
  // and we plan on emitting that info directly.
}


// Generate the entry (.proc) directive.
void 
CGEMIT_Prn_Ent_In_Asm (ST *pu)
{
  BB_LIST *ent_list;

  fprintf ( Asm_File, "\t%s\t", AS_ENT);
  EMT_Write_Qualified_Name(Asm_File, pu);

  for (ent_list = Entry_BB_Head; ent_list; ent_list = BB_LIST_rest(ent_list)) {
    BB *bb = BB_LIST_first(ent_list);
    ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_ENTRYINFO);
    ENTRYINFO *ent = ANNOT_entryinfo(ant);
    ST *entry_sym = ENTRYINFO_name(ent);

    if ( !ST_is_not_used(entry_sym)) {
      const char *entry_name = ST_name(entry_sym);
      if (strcmp( Cur_PU_Name, entry_name ) != 0) {
	fprintf (Asm_File, ", %s", entry_name);
      }
    }
  }

  fprintf ( Asm_File, "\n");
}


// Preprocess FP registers before emit.  Needed only for IA-32.
void
STACK_FP_Fixup_PU()
{}

void
CGEMIT_Weak_Alias (ST *sym, ST *strongsym) 
{
        fprintf ( Asm_File, "\t%s\t%s#\n", AS_WEAK, ST_name(sym));
        // bug fix for OSP_145
	CGEMIT_Alias(sym, strongsym);
}

void
CGEMIT_Alias (ST *sym, ST *strongsym) 
{
        // bug fix for OSP_145
	fprintf ( Asm_File, "\t.set %s#, ", ST_name(sym));
	if ( ST_is_export_local(strongsym) && ST_class(strongsym) == CLASS_VAR) {
		// file scope local symbol
		if (ST_level(strongsym) == GLOBAL_SYMTAB) {
                        // OSP 490
                        if (Emit_Global_Data || ST_sclass(strongsym) == SCLASS_PSTATIC)
			        fprintf (Asm_File, "%s%s%d#\n",
			            ST_name(strongsym), Label_Name_Separator, ST_index(strongsym));
                }
                else
			Is_True(0, ("Impossible alias to a PU scope variable"));
	}
	else // global export symbol
		fprintf (Asm_File, "%s#\n", ST_name(strongsym));
}

INT CGEMIT_Print_Inst (OP* op, const char* result[], const char* opnd[], FILE* f ) {

  INT i;
  INT st;
  INT comp;
  TOP topcode = OP_code(op);
  const char *arg[ISA_PRINT_COMP_MAX];
  const char *Opnds[ISA_PRINT_COMP_MAX];
  const char *Res[ISA_PRINT_COMP_MAX];

  const ISA_PRINT_INFO *pinfo = ISA_PRINT_Info(topcode);

  FmtAssert( pinfo != NULL, ("no ISA_PRINT_INFO for %s", TOP_Name(topcode)));

  i = 0;
  do {
    comp = ISA_PRINT_INFO_Comp(pinfo, i);

    switch (comp) {
    case ISA_PRINT_COMP_name:
      arg[i] = ISA_PRINT_AsmName(topcode);
      break;

    case ISA_PRINT_COMP_opnd:
    case ISA_PRINT_COMP_opnd+1:
    case ISA_PRINT_COMP_opnd+2:
    case ISA_PRINT_COMP_opnd+3:
    case ISA_PRINT_COMP_opnd+4:
    case ISA_PRINT_COMP_opnd+5:
      Opnds[comp-ISA_PRINT_COMP_opnd] = 
      arg[i] = opnd[comp - ISA_PRINT_COMP_opnd];
      break;

    case ISA_PRINT_COMP_result:
    case ISA_PRINT_COMP_result+1:
      Res[comp-ISA_PRINT_COMP_result] = 
      arg[i] = result[comp - ISA_PRINT_COMP_result];
      break;

    case ISA_PRINT_COMP_end:
      break;

    default:
      FmtAssert (false, ("Unhandled listing component %d for %s", 
                        comp, TOP_Name(topcode)));
      break;
    };
    
    i++;
  } while (comp != ISA_PRINT_COMP_end);

  // Intercept some special cases -- this render it easier to add some
  // simulated instructions without changing the whole machine model 
  // and CG. 
  //
  if (OP_load_GOT_entry(op) && topcode == TOP_ld8) {
    extern OP_MAP OP_Ld_GOT_2_Sym_Map; 
    ST* sym = (ST*)OP_MAP_Get (OP_Ld_GOT_2_Sym_Map, op);
    if (sym) { 
      st = fprintf (f, "%5s ld8.mov %s=[%s], %s%s", Opnds[0], 
                       Res[0], Opnds[3], ST_name(sym), 
		       Symbol_Name_Suffix);
      return st;
    }
  }

  st = fprintf (f, ISA_PRINT_INFO_Format(pinfo),
		     arg[0], arg[1], arg[2], arg[3], 
		     arg[4], arg[5], arg[6], arg[7],
		     arg[8]);
  FmtAssert( st != -1, ("fprintf failed: not enough disk space") );
  return st;
}
