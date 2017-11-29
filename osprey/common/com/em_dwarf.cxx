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


#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
static const char rcs_id[] = "$Source: common/com/SCCS/s.em_dwarf.cxx $ $Revision: 1.21 $";
#endif

#include <stdio.h>

#include "libelf/libelf.h"
#include "elf_stuff.h"

#define	USE_STANDARD_TYPES 1
#include "defs.h"
#include "erglob.h"
#include "srcpos.h"
#include "em_elf.h"
#include "em_dwarf.h"
#include "config_elf_targ.h"
#include "targ_em_dwarf.h"
#ifdef KEY
#include "config_targ.h"  // For Is_Target_64bit() 
#include "config_debug.h" // For DEBUG_Emit_Ehframe
#endif


INT data_alignment_factor;

static Dwarf_P_Debug dw_dbg;
static Dwarf_Error dw_error;

#define MAX_DWARF_SECTIONS 20
static pSCNINFO dwarf_scn[MAX_DWARF_SECTIONS];
static size_t num_dwarf_scns = 0;
/* Allocate only one cie for the whole file. */
static Dwarf_Unsigned cie_index;
#ifdef KEY
static Dwarf_Unsigned eh_cie_index;
#endif

typedef struct {
  UINT16 dwarf_idx;
  const char *path;
} INCL;

typedef struct {
  UINT16 dwarf_idx;
  UINT16 incl_idx;
  const char *filename;
} FNAME;

static INCL *incl_table;
static INT incl_table_size;
static FNAME *file_table;
static INT file_table_size;

static void
identity_function(Dwarf_Unsigned symidx,
		  Dwarf_Unsigned *elf_symidx,
		  Dwarf_Unsigned *elf_symoff)
{
  *elf_symidx = symidx;
  *elf_symoff = 0;
}

static Dwarf_Unsigned
another_identity_function(Elf64_Word x)
{
  return x;
}

static symbol_index_recorder Record_Symidx;

static int /* TODO: should be size_t */
setup_new_section_for_dwarf (
    char *name,
    int is_64bit,
    Dwarf_Unsigned type,
    Dwarf_Unsigned flags,
    Dwarf_Unsigned link,
    Dwarf_Unsigned info,
    Dwarf_Unsigned *name_idx,
    int *error)
{
  pSCNINFO newscn;
  BOOL is_reloc;
  INT scnalign;

  if (num_dwarf_scns >= MAX_DWARF_SECTIONS) {
    fprintf (stderr, "ERROR: setup_new_section_for_dwarf: too many dwarf sections\n");
    exit(-1);
  }

  is_reloc = (type == SHT_REL) || (type == SHT_RELA);
  scnalign = (is_reloc) ? (is_64bit ? ELF64_FSZ_XWORD : ELF32_FSZ_WORD) : 1;
  newscn = Em_New_Section (name, type, flags, 0, scnalign);

  /* For relocation sections, we need to setup the link field to the 
     symbol table section index. The dwarf producer does not know about
     this.
  */
  if (is_reloc) {
    Em_Set_sh_link (newscn, Em_Get_Section_Index (Symtab_Info));
  }
  else {
    Em_Set_sh_link (newscn, link);
  }
  Em_Set_sh_info (newscn, info);
  dwarf_scn[num_dwarf_scns] = newscn;
  num_dwarf_scns++;
  *name_idx = (Dwarf_Unsigned) Record_Symidx(Em_Create_Section_Symbol(newscn));
  return Em_Get_Section_Index (newscn);
}

pSCNINFO
Em_Dwarf_Find_Dwarf_Scn (size_t scndx)
{
  size_t i;

  for (i = 0; i < num_dwarf_scns; i++) {
    if (Em_Get_Section_Index (dwarf_scn[i]) == scndx) return dwarf_scn[i];
  }
  return NULL;
}

pSCNINFO
Em_Dwarf_Find_Dwarf_Scn_By_Name (char *name)
{
  size_t i;
  char *scnname;

  for (i = 0; i < num_dwarf_scns; i++) {
    scnname = Em_Get_Section_Name(dwarf_scn[i]);
    if (0 == strcmp(scnname, name)) return dwarf_scn[i];
  }
  return NULL;
}



static void
Increase_Incl_Table_Size (UINT16 incl_idx)
{
  UINT16 new_idx;

  /* realloc incl_table if needed */
  if (incl_idx >= incl_table_size) {
    new_idx = incl_idx + 5;
    if (incl_table == NULL) {
      incl_table = (INCL *) malloc (new_idx * sizeof (INCL));
    }
    else {
      incl_table = (INCL *) realloc (incl_table, new_idx * sizeof(INCL));
    }
    if (incl_table == NULL) {
      ErrMsg ( EC_No_Mem, "Increase_Incl_Table_Size" );
    }
    BZERO (&incl_table[incl_table_size], new_idx - incl_table_size);
    incl_table_size = new_idx;
  }
}

void
Em_Dwarf_Add_Include (UINT16 incl_idx, const char *name)
{
  Dwarf_Unsigned dwarf_idx;

  Increase_Incl_Table_Size (incl_idx);
  dwarf_idx = dwarf_add_directory_decl (dw_dbg, name, &dw_error);
  incl_table[incl_idx].dwarf_idx = dwarf_idx;
  incl_table[incl_idx].path = name;
}


static void
Increase_File_Table_Size (UINT16 file_idx)
{
  UINT16 new_idx;

  /* realloc file_table if needed */
  if (file_idx >= file_table_size) {
    new_idx = file_idx + 5;
    if (file_table == NULL) {
      file_table = (FNAME *) malloc (new_idx * sizeof (FNAME));
    }
    else {
      file_table = (FNAME *) realloc (file_table, new_idx * sizeof(FNAME));
    }
    if (file_table == NULL) {
      ErrMsg ( EC_No_Mem, "Increase_File_Table_Size" );
    }
    BZERO (&file_table[file_table_size], new_idx - file_table_size);
    file_table_size = new_idx;
  }
}


void
Em_Dwarf_Add_File (
  UINT16 file_idx, 
  const char *name, 
  UINT16 incl_idx,
  Dwarf_Unsigned modification_time,
  Dwarf_Unsigned file_size)
{
  Dwarf_Unsigned dwarf_idx;

  Increase_File_Table_Size (file_idx);
  dwarf_idx = dwarf_add_file_decl (
			dw_dbg, 
			name, 
			incl_table[incl_idx].dwarf_idx, 
			modification_time,
			file_size,
			&dw_error);
  file_table[file_idx].dwarf_idx = dwarf_idx;
  file_table[file_idx].filename = name;
  file_table[file_idx].incl_idx = incl_idx;
}

#ifdef KEY
#include "strtab.h"
#include "symtab.h"
#include "irbdata.h"
#endif // KEY
extern "C" void Assign_ST_To_Named_Section (ST *, STR_IDX);

Dwarf_P_Debug 
Em_Dwarf_Begin (BOOL is_64bit, BOOL dwarf_trace, BOOL is_cplus,
		symbol_index_recorder record_symidx)
{
  Dwarf_Unsigned flags;
  const char *augmenter;
  Dwarf_Unsigned personality=0;

#define EXT_OP(v)  (DW_CFA_extended | v)
  static unsigned char init_bytes[] = TARG_INIT_BYTES;
#ifdef TARG_X8664
  static unsigned char init_x86_bytes[] = TARG_INIT_X86_BYTES;
#endif

  if (record_symidx == NULL) {
    record_symidx = another_identity_function;
  }

  Record_Symidx = record_symidx;

  /* Initialize the incl_table and file_table to contain entries for index 0 */
  Increase_Incl_Table_Size (0);
  Increase_File_Table_Size (0);

  data_alignment_factor = is_64bit ? -8 : -4;
  flags = DW_DLC_WRITE | ((is_64bit) ? DW_DLC_SIZE_64 : DW_DLC_SIZE_32);
  flags |= DWARF_TARGET_FLAG;
  flags |= DW_DLC_SYMBOLIC_RELOCATIONS;
  dw_dbg = dwarf_producer_init_b (flags, setup_new_section_for_dwarf, 
		      0, 0, &dw_error);

  // Most of the following have been introduced at KEY, somehow the macro
  // got removed.
  if (is_cplus)	/* may have eh info */
  {
    	if (Gen_PIC_Call_Shared || Gen_PIC_Shared)
	    augmenter = PIC_DW_CIE_AUGMENTER_STRING_V0;
	else
	    augmenter = DW_CIE_AUGMENTER_STRING_V0;
	personality = Save_Str ("__gxx_personality_v0");
        if (Gen_PIC_Call_Shared || Gen_PIC_Shared)
        {
            ST * pic_personality_st = New_ST (GLOBAL_SYMTAB);
            STR_IDX name = Save_Str ("DW.ref.__gxx_personality_v0");
            ST_Init(pic_personality_st, name, CLASS_VAR, SCLASS_DGLOBAL, EXPORT_HIDDEN, MTYPE_TO_TY_array[MTYPE_U8]);
            Set_ST_is_weak_symbol (pic_personality_st);
	    Set_ST_is_initialized (pic_personality_st);
            ST_ATTR_IDX st_attr_idx;
            ST_ATTR&    st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
            ST_ATTR_Init (st_attr, ST_st_idx (pic_personality_st), ST_ATTR_SECTION_NAME, Save_Str (".gnu.linkonce.d.DW.ref.__gxx_personality_v0"));

	    ST * personality_st = New_ST (GLOBAL_SYMTAB);
	    ST_Init (personality_st, Save_Str("__gxx_personality_v0"), CLASS_VAR, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, MTYPE_TO_TY_array[MTYPE_U8]);
	    INITV_IDX iv = New_INITV();
	    INITV_Init_Symoff (iv, personality_st, 0, 1);
	    New_INITO (ST_st_idx (pic_personality_st), iv);
	    // bug 8315: With -ipa, if this file is not symtab.s (actually
	    // control won't ever reach here for symtab.s, but better to
	    // have the following explicit check.), this new ST won't be
	    // assigned to its section, so do it here.
	    // We also cannot move this symbol to symtab.s because that file
	    // does not have language information.
	    if (Read_Global_Data) // ipa && !symtab.I
	      Assign_ST_To_Named_Section (pic_personality_st,
	                                  ST_ATTR_section_name (st_attr));
	}
  }
  else
    // Bug 7278 - implement "zR" CFA augmentation for non-C++ code.
    if (Gen_PIC_Call_Shared || Gen_PIC_Shared)
      augmenter = PIC_NONCPLUS_DW_CIE_AUGMENTER_STRING_V0;
    else
      augmenter = "";

  const char *cie_augmenter;
  Dwarf_Small code_alignent_factor;
  Dwarf_Small return_reg;
  Dwarf_Ptr cie_init_bytes;
  Dwarf_Unsigned cie_init_byte_len;

#ifdef TARG_X8664
  code_alignent_factor = 1;
  cie_augmenter = "";
  return_reg = Is_Target_64bit() ? DW_FRAME_RA_COL : 0x8;
  // We have target description only for x86_64.  So we need to
  // specify what is the right column for x86.
  cie_init_bytes = Is_Target_64bit() ? init_bytes :  init_x86_bytes;
  cie_init_byte_len = Is_Target_64bit()
    ? sizeof(init_bytes) : sizeof(init_x86_bytes);
#else
  code_alignent_factor = 4;
  cie_augmenter = augmenter;
  return_reg = DW_FRAME_RA_COL;
  cie_init_bytes = init_bytes;
  cie_init_byte_len = sizeof(init_bytes);
#endif

  cie_index = dwf_add_frame_cie (dw_dbg,
				 cie_augmenter,
				 code_alignent_factor,
				 data_alignment_factor,
				 return_reg,
				 0, // no personality
				 (Gen_PIC_Call_Shared || Gen_PIC_Shared),
				 is_64bit,
				 cie_init_bytes,
				 cie_init_byte_len,
				 &dw_error);

#if defined(KEY) && !defined(TARG_SL) && !defined(TARG_MIPS)
  // Generate a CIE for .eh_frame only if it is C++ or if -DEBUG:eh_frame=on
  if (is_cplus || DEBUG_Emit_Ehframe)
    eh_cie_index = dwf_add_ehframe_cie (dw_dbg, augmenter,
		    1, data_alignment_factor,
		    // In common/com/em_dwarf.cxx, we have 
		    // target description only for x86_64. So, we need to 
		    // specify what is the right column for x86.
		    Is_Target_64bit() ? DW_FRAME_RA_COL: 0x8,
		    personality,
		    (Gen_PIC_Call_Shared || Gen_PIC_Shared),
		    is_64bit,
#ifdef TARG_X8664
		    Is_Target_64bit() ? init_bytes :  init_x86_bytes, 
		    Is_Target_64bit() ? sizeof(init_bytes) : sizeof(init_x86_bytes), 
#else
		    init_bytes, sizeof(init_bytes),
#endif
		    &dw_error);
#endif // KEY

  return dw_dbg;
}

extern "C" Dwarf_P_Debug 
Em_Dwarf_Begin (BOOL is_64bit, BOOL dwarf_trace, BOOL is_cplus)
{
  return Em_Dwarf_Begin(is_64bit, dwarf_trace, is_cplus, NULL);
}

static Dwarf_Signed section_count = DW_DLV_NOCOUNT;
static Dwarf_Unsigned relocation_section_count = DW_DLV_NOCOUNT;

struct index_to_buffer_map_entry {
  pSCNINFO       cursection;
  Dwarf_Signed   scndx;
  Dwarf_Ptr      buffer;
  Dwarf_Unsigned bufsize;
};

index_to_buffer_map_entry *index_to_buffer_map;

static inline int
find_indexed_section(      int          i,
		     const Dwarf_Signed scn)
{
  for (; i < section_count; ++i) {
    if (index_to_buffer_map[i].scndx == scn) {
      return i;
    }
  }
  return -1;
}

static int current_scn;

static int
advance_to_next_stream_buffer(const Dwarf_Signed scndx)
{
  if (current_scn >= section_count) {
    fprintf(stderr, "Too many calls to advance_to_next_stream_buffer");
    exit(-1);
  }
  current_scn =
    find_indexed_section(current_scn + 1, scndx);
  return current_scn != -1;
}

static Dwarf_Ptr
get_current_buffer(void)
{
  return index_to_buffer_map[current_scn].buffer;
}

static Dwarf_Unsigned
get_current_bufsize(void)
{
  return index_to_buffer_map[current_scn].bufsize;
}

void
Em_Dwarf_Write_Scns (Cg_Dwarf_Sym_To_Elfsym_Ofst translate_dwarf_sym)
{
  Dwarf_Signed i;
  Dwarf_Ptr buffer;
  Dwarf_Signed scndx;
  Dwarf_Unsigned bufsize;
  int result;
  int reloc_buffer_version;
  pSCNINFO cursection;

  if (translate_dwarf_sym == NULL) {
    translate_dwarf_sym = identity_function;
  }

  index_to_buffer_map =
    (index_to_buffer_map_entry *) malloc(section_count *
					 sizeof(index_to_buffer_map_entry));

  dwarf_reset_section_bytes(dw_dbg);

  for (i = 0; i < section_count; i++) {
    buffer = dwarf_get_section_bytes (dw_dbg, i, &scndx, &bufsize, &dw_error);
    cursection = Em_Dwarf_Find_Dwarf_Scn (scndx);
    if (cursection != NULL) {
      index_to_buffer_map[i].cursection = cursection;
      index_to_buffer_map[i].scndx = scndx;
      index_to_buffer_map[i].buffer = buffer;
      index_to_buffer_map[i].bufsize = bufsize;
    }
    else {
      fprintf (stderr, "ERROR No such section index: %d\n", (int)scndx);
      exit(-1);
    }
  }

  // Now get the relocation buffers, translate each one into an ELF
  // relocation section, and write the ELF section to the output file.
  result = dwarf_get_relocation_info_count(dw_dbg,
					   &relocation_section_count,
					   &reloc_buffer_version,
					   &dw_error);
  if (result != DW_DLV_OK) {
    fprintf(stderr, "ERROR Couldn't get relocation buffer count\n");
    exit(-1);
  }
  if (reloc_buffer_version != 2 /* DWARF_DRD_BUFFER_VERSION */) {
    fprintf(stderr,
	    "ERROR Relocation buffer format unrecognized "
	    "(version %d)\n", reloc_buffer_version);
    exit(-1);
  }

  for (i = 0; i < relocation_section_count; i++) {
    Dwarf_Relocation_Data reloc_buf;
    Dwarf_Unsigned entry_count;
    Dwarf_Signed link_scn;

    result = dwarf_get_relocation_info(dw_dbg, &scndx, &link_scn,
				       &entry_count, &reloc_buf,
				       &dw_error);
    if (result != DW_DLV_OK) {
      fprintf(stderr, "ERROR dwarf_get_relocation_info failed\n");
      exit(-1);
    }

    current_scn = -1;

    Dwarf_Signed j = find_indexed_section(current_scn + 1, link_scn);

    BOOL is_debug_line =
      (strcmp(
#ifdef KEY /* Mac port */
      DEBUG_LINE_SECTNAME,
#else /* KEY Mac port */
      ".debug_line",
#endif /* KEY Mac port */
	      Em_Get_Section_Name(index_to_buffer_map[j].cursection)) == 0);

    buffer =
      Em_Dwarf_Symbolic_Relocs_To_Elf(get_current_buffer,
				      get_current_bufsize,
				      advance_to_next_stream_buffer,
				      link_scn,
				      reloc_buf, entry_count,
				      // Cheap hack to see if
				      // this is a 64-bit compile:
				      data_alignment_factor == -8,
				      (is_debug_line ?
				       identity_function :
				       translate_dwarf_sym),
				      &bufsize);
    cursection = Em_Dwarf_Find_Dwarf_Scn(scndx);
    if (cursection != NULL) {
      Em_Add_Bytes_To_Scn(cursection, buffer, bufsize, 1);
    }
    else {
      fprintf (stderr, "ERROR No such section index: %d\n", (int)scndx);
      exit(-1);
    }
    free((void *) buffer);
  }

  for (i = 0; i < section_count; ++i) {
    Em_Add_Bytes_To_Scn (index_to_buffer_map[i].cursection,
			 index_to_buffer_map[i].buffer,
			 index_to_buffer_map[i].bufsize,
			 1);
  }

  free(index_to_buffer_map);

  for (i = 0; i < num_dwarf_scns; i++) {
    Em_End_Section (dwarf_scn[i]);
  }
}

extern "C" void
Em_Dwarf_Write_Scns (void)
{
  Em_Dwarf_Write_Scns(NULL);
}

INT
Em_Dwarf_Prepare_Output(void)
{
  section_count = dwarf_transform_to_disk_form (dw_dbg, &dw_error);
  if (section_count == DW_DLV_NOCOUNT) {
    fprintf(stderr, "ERROR in dwarf_transform_to_disk_form\n");
    exit(-1);
  }
  return (INT) section_count;
}

void 
Em_Dwarf_End (void)
{
  dwarf_producer_finish (dw_dbg, &dw_error);
}


void
Em_Dwarf_Add_Line_Entry ( INT code_address, SRCPOS srcpos)
{
  USRCPOS usrcpos;

  USRCPOS_srcpos(usrcpos) = srcpos;

  dwarf_add_line_entry (dw_dbg, 
			file_table[USRCPOS_filenum(usrcpos)].dwarf_idx, 
			code_address, 
			USRCPOS_linenum(usrcpos), 
			USRCPOS_column(usrcpos), 
			USRCPOS_stmt_begin(usrcpos), 
			USRCPOS_bb_begin(usrcpos), 
			&dw_error);
}


#ifndef MONGOOSE_BE
/* Given a file_index, return the filename and the path for the file. */
void
Em_Dwarf_File_Index_To_Name (INT file_index, char **filename, char **path)
{
  *filename = file_table[file_index].filename;
  *path = incl_table[file_table[file_index].incl_idx].path;
}
#endif /* MONGOOSE_BE */


/* the starting offset in the text region */
static INT Text_Start_Offset;
static Dwarf_Unsigned Text_Start_Label;
static Dwarf_Addr     Offset_From_Text_Start_Label;

void
Em_Dwarf_Start_Text_Lines (pSCNINFO scninfo, INT start_offset)
{
  Text_Start_Offset = start_offset;
  dwarf_lne_set_address (dw_dbg, start_offset, 
		      Em_Create_Section_Symbol(scninfo), &dw_error);
}

void
Em_Dwarf_Start_Text_Region (pSCNINFO scninfo, INT start_offset)
{
  Em_Dwarf_Start_Text_Lines (scninfo, start_offset);
}

void
Em_Dwarf_Start_Text_Region_Semi_Symbolic (pSCNINFO       scninfo,
					  INT            start_offset,
					  Dwarf_Unsigned first_label,
					  Dwarf_Addr     offset_from_first_label)
{
  Text_Start_Offset = start_offset;
  Text_Start_Label = first_label;
  Offset_From_Text_Start_Label = offset_from_first_label;
}

void
Em_Dwarf_End_Text_Lines (pSCNINFO scninfo, INT end_offset)
{
  dwarf_lne_end_sequence (dw_dbg, end_offset, &dw_error);
}

void
Em_Dwarf_End_Text_Arange (pSCNINFO scninfo, INT end_offset)
{
  INT region_size = end_offset - Text_Start_Offset;

  dwarf_add_arange (dw_dbg, Text_Start_Offset, region_size, 
		    Em_Create_Section_Symbol(scninfo), &dw_error);
}

void
Em_Dwarf_End_Text_Arange_Symbolic(Dwarf_Unsigned last_label,
				  Dwarf_Addr     offset_from_last_label)
{
  dwarf_add_arange_b (dw_dbg,
		      Offset_From_Text_Start_Label,
		      0 /* dummy length */, 
		      Text_Start_Label,
		      last_label,
		      offset_from_last_label,
		      &dw_error);
}

void
Em_Dwarf_End_Text_Region (pSCNINFO scninfo, INT end_offset)
{
  Em_Dwarf_End_Text_Arange (scninfo, end_offset);
  Em_Dwarf_End_Text_Lines (scninfo, end_offset);
}

void
Em_Dwarf_End_Text_Region_Semi_Symbolic(pSCNINFO       scninfo,
				       INT            end_offset,
				       Dwarf_Unsigned last_label,
				       Dwarf_Addr     offset_from_last_label)
{
  Em_Dwarf_End_Text_Arange_Symbolic(last_label,
				    offset_from_last_label);
  Em_Dwarf_End_Text_Lines(scninfo, end_offset);
}

void Em_Dwarf_Process_PU (Dwarf_Unsigned begin_label,
			  Dwarf_Unsigned end_label,
			  INT32          begin_offset,
			  INT32          end_offset,
			  Dwarf_P_Die    PU_die, 
			  Dwarf_P_Fde    fde,
#ifdef TARG_X8664
			  Dwarf_P_Fde    eh_fde,
#endif
			  Elf64_Word     eh_symindex,
			  INT            eh_offset)
{
  /* setup the low_pc and the high_pc attributes. */
  dwarf_add_AT_targ_address_b (dw_dbg, PU_die, DW_AT_low_pc,
			       begin_offset,
			       (Dwarf_Unsigned) begin_label,
			       &dw_error);
  dwarf_add_AT_targ_address_b (dw_dbg, PU_die, DW_AT_high_pc,
			       end_offset, (Dwarf_Unsigned) end_label,
			       &dw_error);

  if (fde == NULL)
	return;
  /* emit the debug_frame information for this procedure. */
  if (eh_offset == DW_DLX_NO_EH_OFFSET)	/* no exception handler */
  	dwarf_add_frame_fde_b (dw_dbg, fde, PU_die, cie_index, 
			       begin_offset,
			       0 /* dummy code length */,
			       (Dwarf_Unsigned) begin_label,
			       (Dwarf_Unsigned) end_label,
			       end_offset,
			       &dw_error);
  else
  	dwarf_add_frame_info_b (dw_dbg, fde, PU_die, cie_index, 
				begin_offset,
				0 /* dummy code length */,
				(Dwarf_Unsigned) begin_label,
				(Dwarf_Unsigned) end_label,
				end_offset,
				eh_offset, eh_symindex, &dw_error);
#ifdef TARG_PPC32
    if (eh_offset == DW_DLX_NO_EH_OFFSET)	/* no exception handler */
  	dwf_add_ehframe_fde_b (dw_dbg, fde, PU_die, eh_cie_index, 
			       begin_offset,
			       0 /* dummy code length */,
			       (Dwarf_Unsigned) begin_label,
			       (Dwarf_Unsigned) end_label,
			       end_offset,
			       &dw_error);
  else
  	dwf_add_ehframe_info_b (dw_dbg, fde, PU_die, eh_cie_index, 
				begin_offset,
				0 /* dummy code length */,
				(Dwarf_Unsigned) begin_label,
				(Dwarf_Unsigned) end_label,
				end_offset,
				eh_offset, eh_symindex, &dw_error);
#endif
#ifdef TARG_X8664
  if (eh_fde == NULL)
  	return;

  if (eh_offset == DW_DLX_NO_EH_OFFSET)	/* no exception handler */
  	dwf_add_ehframe_fde_b (dw_dbg, eh_fde, PU_die, eh_cie_index, 
			       begin_offset,
			       0 /* dummy code length */,
			       (Dwarf_Unsigned) begin_label,
			       (Dwarf_Unsigned) end_label,
			       end_offset,
			       &dw_error);
  else
  	dwf_add_ehframe_info_b (dw_dbg, eh_fde, PU_die, eh_cie_index, 
				begin_offset,
				0 /* dummy code length */,
				(Dwarf_Unsigned) begin_label,
				(Dwarf_Unsigned) end_label,
				end_offset,
				eh_offset, eh_symindex, &dw_error);
#endif
}
#ifdef TARG_X8664
void Em_Dwarf_Add_PU_Entries (Dwarf_Unsigned begin_label,
			      Dwarf_Unsigned end_label,
			      INT32          begin_offset,
			      INT32          end_offset,
			      Dwarf_P_Die    PU_die, 
			      Dwarf_P_Fde    fde)
{
  dwarf_add_AT_targ_address_b (dw_dbg, PU_die, DW_AT_low_pc,
			       begin_offset,
			       (Dwarf_Unsigned) begin_label,
			       &dw_error);
  dwarf_add_AT_targ_address_b (dw_dbg, PU_die, DW_AT_high_pc,
			       end_offset, (Dwarf_Unsigned) end_label,
			       &dw_error);

  if (fde == NULL)
    return;
  /* emit the debug_frame information for this procedure. */
  dwarf_add_frame_fde_b (dw_dbg, fde, PU_die, cie_index, 
  		         begin_offset,
			 0 /* dummy code length */,
			 (Dwarf_Unsigned) begin_label,
			 (Dwarf_Unsigned) end_label,
			 end_offset,
			 &dw_error);
}
#endif
