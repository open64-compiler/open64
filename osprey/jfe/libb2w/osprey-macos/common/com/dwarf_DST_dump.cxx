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
static const char rcs_id[] = "$Source: common/com/SCCS/s.dwarf_DST_dump.cxx $ $Revision: 1.13 $";
#endif

#include <stdio.h>
#include <cmplrs/rcodes.h>
#define USE_DST_INTERNALS
#include "dwarf_DST.h"
#include "dwarf_DST_dump.h"
#include "errors.h"

#define DST_DUMP_LINELENGTH 1024
#define DST_TMP_BUF_LENGTH 256


/* This truth value takes into account that we need one character 
 * at the end of as line for " ...\n".
 */
#define DST_CHARS_DO_FIT(n) (n <= (DST_DUMP_LINELENGTH - next_char - 5))

static char   line_buffer[DST_DUMP_LINELENGTH];
static char   tmp_buffer[DST_TMP_BUF_LENGTH];
static UINT32 next_char;                      /* Index into line_buffer */
static FILE  *dumpf = NULL;
static char  *dumpf_name = NULL;
static BOOL   end_of_line = FALSE;


#define DST_ASSERT(truth, msg) Is_True(truth, (msg))


   /*---------------------------------------
    * Some general purpose writing routines
    *---------------------------------------*/


/* The current contents of the line buffer is written to file and the
 * next_char is again the first element in the buffer.
*/
static void 
DST_write_line(void)
{
   size_t status;
   
   line_buffer[next_char] = '\n';
   status = fwrite(line_buffer, sizeof(char), next_char + 1, dumpf);
   DST_ASSERT(status >= next_char, "Write error while dumping DST");
   next_char = 0;
   end_of_line = FALSE;
}



static void
DST_line_overflow(void)
{
   line_buffer[next_char++] = ' ';
   line_buffer[next_char++] = '.';
   line_buffer[next_char++] = '.';
   line_buffer[next_char++] = '.';
   end_of_line = TRUE;
}



static void
DST_nput_char(size_t n, const char c)
{
   size_t c_idx;
   
   if (!end_of_line)
   {
      if (DST_CHARS_DO_FIT(n))
      {	 
	 for (c_idx = 0; c_idx < n; c_idx += 1)
	 {
	    line_buffer[next_char + c_idx] = c;
	 }
	 next_char = next_char + n;
      }
      else
	 DST_line_overflow();
   }
}



static void
DST_put_string(const char *c)
{
   size_t length, c_idx;
   
   if (!end_of_line)
   {
      if (c != NULL)
      {
	 length = strlen(c);
	 if (DST_CHARS_DO_FIT(length))
	 {
	    for (c_idx = 0; c_idx < length; c_idx += 1)
	    {
	       line_buffer[next_char + c_idx] = c[c_idx];
	    }
            next_char = next_char + length;
	 }
	 else
	    DST_line_overflow();
      }
      else
      {
	 if (DST_CHARS_DO_FIT(2))
	 {
	    line_buffer[next_char++] = '<';
	    line_buffer[next_char++] = '>';
	 }
	 else
	    DST_line_overflow();
      }
   }
}



static void
DST_put_idx(DST_IDX i)
{
   sprintf(&tmp_buffer[0], "[%d,%d]", i.block_idx, i.byte_idx);
   DST_put_string(&tmp_buffer[0]);
}

static void
DST_put_st_id (INT32 level, INT32 index)
{
   sprintf(&tmp_buffer[0], "(%d,%d)", level, index);
   DST_put_string(&tmp_buffer[0]);
}

static void
DST_put_string_attribute(const char *at_name, DST_STR_IDX istr)
{
   if (!DST_IS_NULL(istr))
   {
      DST_put_string(at_name);
      DST_nput_char(1, '(');
      DST_put_string(DST_STR_IDX_TO_PTR(istr)); 
      DST_nput_char(1, ')');
   }
}


static void
DST_put_idx_attribute(const char *at_name, DST_IDX i, BOOL is_type)
{
   if (DST_IS_FOREIGN_OBJ(i)) {
      DST_put_string(at_name);
      DST_put_string("[foreign]");
   }
   else if (!DST_IS_NULL(i))
   {
      DST_put_string(at_name);
      DST_put_idx(i);
   }
   else if (is_type)
   {
      DST_put_string(at_name);
      DST_put_string("(void)");
   }
}



static void
DST_put_hex64_attribute(const char *at_name, UINT64 num)
{
   DST_put_string(at_name);
   sprintf(&tmp_buffer[0], "(0x%llx)", num);
   DST_put_string(&tmp_buffer[0]);
}


static void
DST_put_INT32_attribute(const char *at_name, INT32 num)
{
   DST_put_string(at_name);
   sprintf(&tmp_buffer[0], "(%d)", num);
   DST_put_string(&tmp_buffer[0]);
}

static void
DST_put_UINT32_attribute(const char *at_name, UINT32 num)
{
   DST_put_string(at_name);
   sprintf(&tmp_buffer[0], "(%u)", num);
   DST_put_string(&tmp_buffer[0]);
}

#ifdef KEY
static void
DST_put_C4_attribute(const char *at_name, UINT32 real, UINT32 imag)
{
  DST_put_string(at_name);
  sprintf(&tmp_buffer[0], "(%u, %u)", real, imag);
  DST_put_string(&tmp_buffer[0]);
}

static void
DST_put_C8_attribute(const char *at_name, UINT64 real, UINT64 imag)
{
  DST_put_string(at_name);
  sprintf(&tmp_buffer[0], "(%llu, %llu)", real, imag);
  DST_put_string(&tmp_buffer[0]);
}
#endif // KEY

static void
DST_put_INT64_attribute(const char *at_name, INT64 num)
{
   DST_put_string(at_name);
   sprintf(&tmp_buffer[0], "(%lld)", num);
   DST_put_string(&tmp_buffer[0]);
}

static void
DST_put_UINT64_attribute(const char *at_name, UINT64 num)
{
   DST_put_string(at_name);
   sprintf(&tmp_buffer[0], "(%llu)", num);
   DST_put_string(&tmp_buffer[0]);
}



static void
DST_put_inline_attribute(const char *at_name, DST_inline inlin)
{
   DST_put_string(at_name);
   DST_nput_char(1, '(');
   switch (inlin) {
    case DW_INL_not_inlined:
      DST_put_string ("DW_INL_not_inlined");
      break;
    case DW_INL_inlined:
      DST_put_string ("DW_INL_inlined");
      break;
    case DW_INL_declared_not_inlined:
      DST_put_string ("DW_INL_declared_not_inlined");
      break;
    case DW_INL_declared_inlined:
      DST_put_string ("DW_INL_declared_inlined");
      break;
   }
   DST_nput_char(1, ')');
}


static void
DST_put_virtuality_attribute(const char *at_name, DST_virtuality virtuality)
{
   DST_put_string(at_name);
   DST_nput_char(1, '(');
   switch (virtuality) {
    case DW_VIRTUALITY_none:
      DST_put_string ("DW_VIRTUALITY_none");
      break;
    case DW_VIRTUALITY_virtual:
      DST_put_string("DW_VIRTUALITY_virtual");
      break;
    case DW_VIRTUALITY_pure_virtual:
      DST_put_string ("DW_VIRTUALITY_pure_virtual");
      break;
   }
   DST_nput_char(1, ')');
}

#ifdef KEY
static void
DST_put_accessibility_attribute(const char *at_name, 
                                DST_accessibility accessibility)
{
   DST_put_string(at_name);
   DST_nput_char(1, '(');
   switch (accessibility) {
    case DW_ACCESS_public:
      DST_put_string("DW_ACCESS_public");
      break;
    case DW_ACCESS_private:
      DST_put_string("DW_ACCESS_private");
      break;
    case DW_ACCESS_protected:
      DST_put_string("DW_ACCESS_protected");
      break;
   }
   DST_nput_char(1, ')');
}
#endif

static void
DST_put_language_attribute(const char *at_name, DST_language lang_code)
{
   DST_put_string(at_name);
   DST_nput_char(1, '(');
   switch (lang_code)
   {
   case DW_LANG_C89:
      DST_put_string("C89");
      break;
   case DW_LANG_Ada83:
      DST_put_string("Ada83");
      break;
   case DW_LANG_C_plus_plus:
      DST_put_string("C_plus_plus");
      break;
   case DW_LANG_Cobol74:
      DST_put_string("Cobol74");
      break;
   case DW_LANG_Cobol85:
      DST_put_string("Cobol85");
      break;
   case DW_LANG_Fortran77:
      DST_put_string("Fortran77");
      break;
   case DW_LANG_Fortran90:
      DST_put_string("Fortran90");
      break;
   case DW_LANG_Pascal83:
      DST_put_string("Pascal83");
      break;
   case DW_LANG_Modula2:
      DST_put_string("Modula2");
      break;
   }
   DST_nput_char(1, ')');
}

static void
DST_put_id_case_attribute(const char *at_name, DST_identifier_case id_case)
{
   DST_put_string(at_name);
   DST_nput_char(1, '(');
   switch (id_case)
   {
   case DW_ID_case_sensitive:
      DST_put_string("case_sensitive");
      break;
   case DW_ID_up_case:
      DST_put_string("upper_case");
      break;
   case DW_ID_down_case:
      DST_put_string("lower_case");
      break;
   case DW_ID_case_insensitive:
      DST_put_string("case_insensitive");
      break;
   }
   DST_nput_char(1, ')');
}


static void
DST_put_decl(USRCPOS decl)
{
   DST_put_UINT32_attribute(" file", (UINT32)USRCPOS_filenum(decl));
   DST_put_UINT32_attribute(" line", (UINT32)USRCPOS_linenum(decl));
   DST_put_UINT32_attribute(" column", (UINT32)USRCPOS_column(decl));
}


static void
DST_put_assoc(const char *at_name, DST_flag flag, DST_ASSOC_INFO assoc)
{
   DST_put_string(at_name);
   DST_nput_char(1, '(');
   {
      DST_put_string("ST");
      DST_put_st_id (DST_ASSOC_INFO_st_level(assoc), DST_ASSOC_INFO_st_index(assoc));
   }
   DST_nput_char(1, ')');
}


static void
DST_put_const_attribute(const char *at_name, DST_CONST_VALUE cval)
{
   switch(DST_CONST_VALUE_form(cval))
   {
   case DST_FORM_STRING:
      DST_put_idx_attribute(at_name, DST_CONST_VALUE_form_string(cval), FALSE);
      break;

   case DST_FORM_DATA1:
      DST_put_UINT32_attribute(at_name, 
			      (UINT32)DST_CONST_VALUE_form_data1(cval));
      break;

   case DST_FORM_DATA2:
      DST_put_UINT32_attribute(at_name, 
			      (UINT32)DST_CONST_VALUE_form_data2(cval));
      break;

   case DST_FORM_DATA4:
      DST_put_UINT32_attribute(at_name, 
			      (UINT32)DST_CONST_VALUE_form_data4(cval));
      break;

   case DST_FORM_DATA8:
      DST_put_UINT64_attribute(at_name, 
			      (UINT64)DST_CONST_VALUE_form_data8(cval));
      break;
#ifdef KEY
   case DST_FORM_DATAC4:
      DST_put_C4_attribute(at_name, 
			      (UINT32)DST_CONST_VALUE_form_crdata4(cval),
			      (UINT32)DST_CONST_VALUE_form_cidata4(cval));
      break;

   case DST_FORM_DATAC8:
      DST_put_C8_attribute(at_name, 
			      (UINT64)DST_CONST_VALUE_form_crdata8(cval),
			      (UINT64)DST_CONST_VALUE_form_cidata8(cval));
      break;
#endif // KEY
   }
}


      
   /*----------------------------------
    * One put routine for each DW_TAG
    *----------------------------------*/


static void
DST_put_compile_unit(DST_flag flag, DST_COMPILE_UNIT *attr)
{
   DST_put_string(":compile_unit:");
   DST_put_string_attribute(" name", DST_COMPILE_UNIT_name(attr));
   DST_put_string_attribute(" comp_dir", DST_COMPILE_UNIT_comp_dir(attr));
   DST_put_string_attribute(" producer", DST_COMPILE_UNIT_producer(attr));
   DST_put_language_attribute(" language", DST_COMPILE_UNIT_language(attr));
   DST_put_id_case_attribute(" case", DST_COMPILE_UNIT_identifier_case(attr));
}

#ifdef KEY /* Bug 3507 */
static void
DST_put_module(DST_flag flag, DST_MODULE *attr)
{
   DST_put_string(":module:");
   DST_put_decl(DST_MODULE_decl(attr));
   DST_put_string_attribute(" name", DST_MODULE_name(attr));
}

static void
DST_put_imported_decl(DST_flag flag, DST_IMPORTED_DECL *attr)
{
   DST_put_string(":imported declaration");
   DST_put_string_attribute(" name", DST_IMPORTED_DECL_name(attr));
   DST_put_assoc(" import", flag, DST_IMPORTED_DECL_import(attr));
}
#endif /* KEY Bug 3507 */

static void
DST_put_subprogram(DST_flag flag, DST_SUBPROGRAM *attr)
{
   DST_put_string(":subprogram:");
   if (DST_IS_memdef(flag))  /* Not yet supported */
   {
      DST_put_string(" a class member with AT_specification!");
   }
   else if (DST_IS_declaration(flag))
   {
      DST_put_decl(DST_SUBPROGRAM_decl_decl(attr));
      DST_put_string_attribute(" name", DST_SUBPROGRAM_decl_name(attr));
      DST_put_string_attribute(" linkage_name", 
			       DST_SUBPROGRAM_decl_linkage_name(attr));
      DST_put_string(" declaration");
      if (DST_IS_external(flag))
	 DST_put_string(" external");
      if (DST_IS_prototyped(flag))
	 DST_put_string(" prototyped");
      DST_put_idx_attribute(" type", DST_SUBPROGRAM_decl_type(attr), TRUE);
      DST_put_idx_attribute(" origin", DST_SUBPROGRAM_decl_origin(attr), FALSE);
      DST_put_inline_attribute (" inline", DST_SUBPROGRAM_decl_inline(attr));
      DST_put_virtuality_attribute(" virtuality",
				   DST_SUBPROGRAM_decl_virtuality(attr));
      DST_put_INT32_attribute(" vtable_elem_location",
  			      DST_SUBPROGRAM_decl_vtable_elem_location(attr));
   }
   else /* definition */
   {
      DST_put_decl(DST_SUBPROGRAM_def_decl(attr));
      DST_put_string_attribute(" name", DST_SUBPROGRAM_def_name(attr));
      DST_put_string_attribute(" linkage_name", 
			       DST_SUBPROGRAM_def_linkage_name(attr));
      DST_put_string_attribute(" pubname", DST_SUBPROGRAM_def_pubname(attr));
      if (DST_IS_external(flag))
	 DST_put_string(" external");
      if (DST_IS_prototyped(flag))
	 DST_put_string(" prototyped");
      DST_put_idx_attribute(" type", DST_SUBPROGRAM_def_type(attr), TRUE);
      DST_put_idx_attribute(" specification",
			    DST_SUBPROGRAM_def_specification(attr), TRUE);
      DST_put_idx_attribute(" clone_origin",
			 DST_SUBPROGRAM_def_clone_origin(attr),
			 FALSE);
      DST_put_inline_attribute (" inline", DST_SUBPROGRAM_def_inline(attr));
      DST_put_virtuality_attribute(" virtuality",
				   DST_SUBPROGRAM_def_virtuality(attr));
      DST_put_INT32_attribute(" vtable_elem_location",
  			      DST_SUBPROGRAM_def_vtable_elem_location(attr));

      DST_put_assoc(" pc", flag, DST_SUBPROGRAM_def_st(attr));
   }
}

static void
DST_put_inlined_subroutine(DST_flag flag, DST_INLINED_SUBROUTINE *attr)
{
   DST_put_string(":inlined_subroutine:");
   if (DST_IS_FOREIGN_OBJ(DST_INLINED_SUBROUTINE_abstract_origin(attr)))
       DST_put_decl(DST_INLINED_SUBROUTINE_decl(attr));
   DST_put_assoc(" low_pc", flag, DST_INLINED_SUBROUTINE_low_pc(attr));
   DST_put_assoc(" high_pc", flag, DST_INLINED_SUBROUTINE_high_pc(attr));
   DST_put_idx_attribute(" abstract_origin",
			 DST_INLINED_SUBROUTINE_abstract_origin(attr),
			 FALSE);
   if (DST_IS_FOREIGN_OBJ(DST_INLINED_SUBROUTINE_abstract_origin(attr)))
       DST_put_string_attribute(" abstract_name", DST_INLINED_SUBROUTINE_abstract_name(attr));
}

static void
DST_put_entry_point(DST_flag flag, DST_ENTRY_POINT *attr)
{
   DST_put_string(":entry point:");
   DST_put_decl(DST_ENTRY_POINT_decl(attr));
   DST_put_string_attribute(" name", DST_ENTRY_POINT_name(attr));
   DST_put_idx_attribute(" type", DST_ENTRY_POINT_type(attr), TRUE);
   DST_put_assoc(" pc", flag, DST_ENTRY_POINT_st(attr));
}

static void
DST_put_common_block(DST_flag flag, DST_COMMON_BLOCK *attr)
{
   DST_put_string(":common blk:");
   DST_put_string_attribute(" name", DST_ENTRY_POINT_name(attr));
   DST_put_assoc(" pc", flag, DST_ENTRY_POINT_st(attr));
}

static void
DST_put_common_inclusion(DST_flag flag, DST_COMMON_INCL *attr)
{
   DST_put_string(":common incl:");
   DST_put_decl(DST_COMMON_INCL_decl(attr));
   DST_put_idx_attribute(" type", DST_COMMON_INCL_com_blk(attr), TRUE);
}

static void
DST_put_lexical_block(DST_flag flag, DST_LEXICAL_BLOCK *attr)
{
   DST_put_string(":lexical_block:");
   DST_put_string_attribute(" name", DST_LEXICAL_BLOCK_name(attr));
   DST_put_assoc(" low_pc", flag, DST_LEXICAL_BLOCK_low_pc(attr));
   DST_put_assoc(" high_pc", flag, DST_LEXICAL_BLOCK_high_pc(attr));
}


static void
DST_put_label(DST_flag flag, DST_LABEL *attr)
{
   DST_put_string(":label:");
   DST_put_string_attribute(" name", DST_LABEL_name(attr));
   DST_put_assoc(" low_pc", flag, DST_LABEL_low_pc(attr));
}


static void
DST_put_variable(DST_flag flag, DST_VARIABLE *attr)
{
   DST_put_string(":variable:");
   if (DST_IS_artificial(flag))
      DST_put_string(" artificial");
   if (DST_IS_const(flag))  /* Not yet supported */
   {
#ifdef KEY /* Bug 3507 */
      DST_put_decl(DST_VARIABLE_decl_decl(attr));
      DST_put_string_attribute(" name", DST_VARIABLE_decl_name(attr));
#endif /* KEY Bug 3507 */
      DST_put_string(" a constant variable!");
   }
   else if (DST_IS_comm(flag)) 
   {
      DST_put_string("var in common:");
      if (DST_IS_deref(flag))
	DST_put_string(" deref");
      if (DST_IS_f90_pointer(flag))
	DST_put_string(" f90_pointer");
      if (DST_IS_allocatable(flag))
	DST_put_string(" allocatable");
      if (DST_IS_assumed_shape(flag))
	DST_put_string(" assumed_shape");
      if (DST_IS_assumed_size(flag))
	DST_put_string(" assumed_size");
      DST_put_decl(DST_VARIABLE_comm_decl(attr));
      DST_put_string_attribute(" name", DST_VARIABLE_comm_name(attr));
      DST_put_idx_attribute(" type", DST_VARIABLE_comm_type(attr), TRUE);
      DST_put_UINT64_attribute(" offset", DST_VARIABLE_comm_offs(attr));
      DST_put_assoc(" location", flag, DST_VARIABLE_comm_st(attr));
      DST_put_idx_attribute(" dopetype", DST_VARIABLE_comm_dopetype(attr),TRUE);
   }
   else if (DST_IS_memdef(flag))  /* Not yet supported */
   {
      DST_put_string(" a class member with AT_specification!");
   }
   else if (DST_IS_declaration(flag))
   {
      DST_put_decl(DST_VARIABLE_decl_decl(attr));
      DST_put_string_attribute(" name", DST_VARIABLE_decl_name(attr));
      DST_put_string(" declaration");
      if (DST_IS_external(flag))
	 DST_put_string(" external");
      if (DST_IS_automatic(flag))
	 DST_put_string(" <auto>");
      DST_put_idx_attribute(" type", DST_VARIABLE_decl_type(attr), TRUE);
#ifdef KEY
      DST_put_string_attribute(" linkage_name", 
			       DST_VARIABLE_decl_linkage_name(attr));
#endif
   }
   else /* definition */
   {
      DST_put_decl(DST_VARIABLE_def_decl(attr));
      DST_put_string_attribute(" name", DST_VARIABLE_def_name(attr));
      if (DST_IS_external(flag))
	 DST_put_string(" external");
      if (DST_IS_automatic(flag))
	 DST_put_string(" <auto>");
      if (DST_IS_deref(flag))
	DST_put_string(" deref");
      if (DST_IS_base_deref(flag))
	DST_put_string(" base_deref");
      if (DST_IS_f90_pointer(flag))
	DST_put_string(" f90_pointer");
      if (DST_IS_allocatable(flag))
	DST_put_string(" allocatable");
      if (DST_IS_assumed_shape(flag))
	DST_put_string(" assumed shape");
      if (DST_IS_assumed_size(flag))
	DST_put_string(" assumed_size");

      DST_put_UINT64_attribute(" offset", DST_VARIABLE_def_offs(attr));
      DST_put_idx_attribute(" type", DST_VARIABLE_def_type(attr), TRUE);
      DST_put_assoc(" location", flag, DST_VARIABLE_def_st(attr));
      DST_put_idx_attribute(" abstract_origin",
	DST_VARIABLE_def_abstract_origin(attr), FALSE); 
      DST_put_idx_attribute(" dopetype", DST_VARIABLE_def_dopetype(attr), TRUE);
#ifdef KEY
      DST_put_string_attribute(" linkage_name", 
			       DST_VARIABLE_def_linkage_name(attr));
#endif
   }
}


static void
DST_put_formal_parameter(DST_flag flag, DST_FORMAL_PARAMETER *attr)
{
   DST_put_string(":formal_parameter:");
   if (DST_IS_artificial(flag))
      DST_put_string(" artificial");
   if (DST_IS_base_deref(flag))
      DST_put_string(" base_deref");
   if (DST_IS_deref(flag))
      DST_put_string(" deref");
   if (DST_IS_f90_pointer(flag))
      DST_put_string(" f90_pointer");
   if (DST_IS_allocatable(flag))
      DST_put_string(" allocatable");
   if (DST_IS_assumed_shape(flag))
      DST_put_string(" assumed shape");
   if (DST_IS_assumed_size(flag))
      DST_put_string(" assumed_size");
   DST_put_decl(DST_FORMAL_PARAMETER_decl(attr));
   DST_put_string_attribute(" name", DST_FORMAL_PARAMETER_name(attr));
   if (DST_IS_optional_parm(flag))
      DST_put_string(" is_optional");
   if (DST_IS_variable_parm(flag))
      DST_put_string(" variable_parameter");
   DST_put_idx_attribute(" type", DST_FORMAL_PARAMETER_type(attr), TRUE);
   DST_put_idx_attribute(" default_value",
			 DST_FORMAL_PARAMETER_default_val(attr), FALSE);
   DST_put_idx_attribute(" abstract_origin",
			 DST_FORMAL_PARAMETER_abstract_origin(attr), FALSE); 
   DST_put_assoc(" location", flag, DST_FORMAL_PARAMETER_st(attr));
   DST_put_idx_attribute(" dopetype", DST_FORMAL_PARAMETER_dopetype(attr),TRUE);
}


static void
DST_put_unspecified_parameters(DST_flag flag, DST_UNSPECIFIED_PARAMETERS *attr)
{
   DST_put_string(":unspecified_parameters:");
   DST_put_decl(DST_FORMAL_PARAMETER_decl(attr));
}


static void
DST_put_basetype(DST_flag flag, DST_BASETYPE *attr)
{
   DST_put_string(":basetype:");
   DST_put_string_attribute(" name", DST_FORMAL_PARAMETER_name(attr));
   DST_put_INT32_attribute(" encoding", DST_BASETYPE_encoding(attr));
   DST_put_INT32_attribute(" byte_size", DST_BASETYPE_byte_size(attr));
}


static void
DST_put_const_type(DST_flag flag, DST_CONST_TYPE *attr)
{
   DST_put_string(":const_type:");
   DST_put_idx_attribute(" type", DST_CONST_TYPE_type(attr), TRUE);
}

static void
DST_put_constant(DST_flag flag, DST_CONSTANT *attr)
{
   DST_put_string(":constant:");
   DST_put_decl(DST_CONSTANT_def_decl(attr));
   DST_put_string_attribute(" name", DST_CONSTANT_def_name(attr));
   DST_put_idx_attribute(" type", DST_CONSTANT_def_type(attr), TRUE);
   DST_put_const_attribute(" value", DST_CONSTANT_def_cval(attr));
}


static void
DST_put_volatile_type(DST_flag flag, DST_VOLATILE_TYPE *attr)
{
   DST_put_string(":volatile_type:");
   DST_put_idx_attribute(" type", DST_VOLATILE_TYPE_type(attr), TRUE);
}


static void
DST_put_pointer_type(DST_flag flag, DST_POINTER_TYPE *attr)
{
   DST_put_string(":pointer_type:");
   DST_put_idx_attribute(" type", DST_POINTER_TYPE_type(attr), TRUE);
   DST_put_INT32_attribute(" address_class", 
			   DST_POINTER_TYPE_address_class(attr));
   DST_put_INT32_attribute(" byte_size", DST_POINTER_TYPE_byte_size(attr));
}


static void
DST_put_reference_type(DST_flag flag, DST_REFERENCE_TYPE *attr)
{
   DST_put_string(":reference_type:");
   DST_put_idx_attribute(" type", DST_REFERENCE_TYPE_type(attr), TRUE);
   DST_put_INT32_attribute(" address_class", 
			   DST_REFERENCE_TYPE_address_class(attr));
   DST_put_INT32_attribute(" byte_size", DST_REFERENCE_TYPE_byte_size(attr));
}


static void
DST_put_typedef(DST_flag flag, DST_TYPEDEF *attr)
{
   DST_put_string(":typedef:");
   DST_put_decl(DST_TYPEDEF_decl(attr));
   DST_put_string_attribute(" name", DST_TYPEDEF_name(attr));
   DST_put_idx_attribute(" type", DST_TYPEDEF_type(attr), TRUE);
   DST_put_idx_attribute(" abstract_origin",
			 DST_TYPEDEF_abstract_origin(attr), FALSE);
}


static void
DST_put_array_type(DST_flag flag, DST_ARRAY_TYPE *attr)
{
   DST_put_string(":array_type:");
   DST_put_decl(DST_ARRAY_TYPE_decl(attr));
   DST_put_string_attribute(" name", DST_ARRAY_TYPE_name(attr));
   DST_put_idx_attribute(" type", DST_ARRAY_TYPE_type(attr), TRUE);
   DST_put_INT32_attribute(" byte_size", DST_ARRAY_TYPE_byte_size(attr));
   DST_put_idx_attribute(" abstract_origin",
			 DST_ARRAY_TYPE_abstract_origin(attr), FALSE);
   if (DST_IS_declaration(flag))
      DST_put_string(" declaration");
#ifdef TARG_X8664
   if (DST_IS_GNU_vector(flag))
      DST_put_string(" GNU_vector");
#endif
}


static void
DST_put_subrange_type(DST_flag flag, DST_SUBRANGE_TYPE *attr)
{
   const char * p;
   DST_put_string(":subrange_type:");
   if (DST_IS_lb_cval(flag)) 
   	DST_put_INT32_attribute(" lower", 
		DST_SUBRANGE_TYPE_lower_cval(attr));
   else
   	DST_put_idx_attribute(" lower", 
		DST_SUBRANGE_TYPE_lower_ref(attr), FALSE);

   p = " upper";
   if (DST_IS_count(flag))
     p = " count";

   if (DST_IS_ub_cval(flag)) 
   	DST_put_INT32_attribute(p, 
		DST_SUBRANGE_TYPE_upper_cval(attr));
   else
   	DST_put_idx_attribute(p, 
		DST_SUBRANGE_TYPE_upper_ref(attr), FALSE);


   if (DST_IS_stride_1byte(flag)) 
     p = " stride_1byte" ;

   else if (DST_IS_stride_2byte(flag)) 
     p = " stride_2byte" ;

   else
     p = " stride" ;

   DST_put_idx_attribute(p,DST_SUBRANGE_TYPE_stride_ref(attr), FALSE);
}

static void
DST_put_string_type(DST_flag flag, DST_STRING_TYPE *attr)
{
   DST_put_string(":string_type:");
   DST_put_decl(DST_STRING_TYPE_decl(attr));
   DST_put_string_attribute(" name", DST_STRING_TYPE_name(attr));
   if (DST_IS_cval(flag)) 
   	DST_put_INT32_attribute(" length", 
		DST_STRING_TYPE_len_cval(attr));
   else
   	DST_put_idx_attribute(" length", 
		DST_STRING_TYPE_len_ref(attr), FALSE);
}

static void
DST_put_structure_type(DST_flag flag, DST_STRUCTURE_TYPE *attr)
{
   DST_put_string(":structure_type:");
   DST_put_decl(DST_STRUCTURE_TYPE_decl(attr));
   DST_put_string_attribute(" name", DST_STRUCTURE_TYPE_name(attr));
   DST_put_INT32_attribute(" byte_size", DST_STRUCTURE_TYPE_byte_size(attr));
   DST_put_idx_attribute(" abstract_origin",
			 DST_STRUCTURE_TYPE_abstract_origin(attr), FALSE);
   if (DST_IS_declaration(flag))
      DST_put_string(" declaration");
}

static void
DST_put_class_type(DST_flag flag, DST_CLASS_TYPE *attr)
{
   DST_put_string(":class_type:");
   DST_put_decl(DST_CLASS_TYPE_decl(attr));
   DST_put_string_attribute(" name", DST_CLASS_TYPE_name(attr));
   DST_put_INT32_attribute(" byte_size", DST_CLASS_TYPE_byte_size(attr));
   DST_put_idx_attribute(" abstract_origin",
			 DST_CLASS_TYPE_abstract_origin(attr), FALSE);
   if (DST_IS_declaration(flag))
      DST_put_string(" declaration");
}


static void
DST_put_union_type(DST_flag flag, DST_UNION_TYPE *attr)
{
   DST_put_string(":union_type:");
   DST_put_decl(DST_UNION_TYPE_decl(attr));
   DST_put_string_attribute(" name", DST_UNION_TYPE_name(attr));
   DST_put_INT32_attribute(" byte_size", DST_UNION_TYPE_byte_size(attr));
   DST_put_idx_attribute(" abstract_origin",
			 DST_UNION_TYPE_abstract_origin(attr), FALSE);
   if (DST_IS_declaration(flag))
      DST_put_string(" declaration");
}


static void
DST_put_member(DST_flag flag, DST_MEMBER *attr)
{
   DST_put_string(":member:");
   DST_put_decl(DST_MEMBER_decl(attr));
   DST_put_string_attribute(" name", DST_MEMBER_name(attr));
   DST_put_idx_attribute(" type", DST_MEMBER_type(attr), TRUE);
   DST_put_INT32_attribute(" data_member_location", 
			   DST_MEMBER_memb_loc(attr));
   DST_put_idx_attribute(" dopetype", DST_MEMBER_dopetype(attr),TRUE);

   if (DST_IS_bitfield(flag))
   {
      DST_put_INT32_attribute(" byte_size", DST_MEMBER_byte_size(attr));
      DST_put_INT32_attribute(" bit_offset", DST_MEMBER_bit_offset(attr));
      DST_put_INT32_attribute(" bit_size", DST_MEMBER_bit_size(attr));
   }
   if (DST_IS_declaration(flag))
      DST_put_string(" declaration");
   if (DST_IS_f90_pointer(flag))
      DST_put_string(" f90_pointer");
   if (DST_IS_allocatable(flag))
      DST_put_string(" allocatable");
   if (DST_IS_assumed_shape(flag))
      DST_put_string(" assumed shape");
}

static void
DST_put_inheritance(DST_flag flag, DST_INHERITANCE *attr)
{
   DST_put_string(":inheritance:");
   DST_put_idx_attribute(" type", DST_INHERITANCE_type(attr), TRUE);
   DST_put_INT32_attribute(" data_member_location", 
			   DST_INHERITANCE_memb_loc(attr));
#ifdef KEY
   DST_put_accessibility_attribute(" accessibility", 
                                   DST_INHERITANCE_accessibility(attr));
#endif
}


static void
DST_put_template_type_param(DST_flag flag,
			    DST_TEMPLATE_TYPE_PARAMETER *attr)
{
   DST_put_string(":template_type_param:");
   DST_put_string_attribute(" name", DST_TEMPLATE_TYPE_PARAMETER_name(attr));
   DST_put_idx_attribute(" type", DST_TEMPLATE_TYPE_PARAMETER_type(attr),
			 TRUE);
}


static void
DST_put_template_value_param(DST_flag flag,
			     DST_TEMPLATE_VALUE_PARAMETER *attr)
{
   DST_put_string(":template_value_param:");
   DST_put_string_attribute(" name", DST_TEMPLATE_VALUE_PARAMETER_name(attr));
   DST_put_const_attribute(" value", DST_TEMPLATE_VALUE_PARAMETER_cval(attr));
}


static void
DST_put_enumeration_type(DST_flag flag, DST_ENUMERATION_TYPE *attr)
{
   DST_put_string(":enumeration_type:");
   DST_put_decl(DST_ENUMERATION_TYPE_decl(attr));
   DST_put_string_attribute(" name", DST_ENUMERATION_TYPE_name(attr));
   DST_put_INT32_attribute(" byte_size", DST_ENUMERATION_TYPE_byte_size(attr));
   DST_put_idx_attribute(" abstract_origin",
			 DST_ENUMERATION_TYPE_abstract_origin(attr), FALSE);
   if (DST_IS_declaration(flag))
      DST_put_string(" declaration");
}


static void
DST_put_enumerator(DST_flag flag, DST_ENUMERATOR *attr)
{
   DST_put_string(":enumerator:");
   DST_put_decl(DST_ENUMERATOR_decl(attr));
   DST_put_string_attribute(" name", DST_ENUMERATOR_name(attr));
   DST_put_const_attribute(" const_value", DST_ENUMERATOR_cval(attr));
}


static void
DST_put_subroutine_type(DST_flag flag, DST_SUBROUTINE_TYPE *attr)
{
   DST_put_string(":subroutine_type:");
   DST_put_decl(DST_SUBROUTINE_TYPE_decl(attr));
   DST_put_string_attribute(" name", DST_SUBROUTINE_TYPE_name(attr));
   DST_put_idx_attribute(" type", DST_SUBROUTINE_TYPE_type(attr), TRUE);
   DST_put_idx_attribute(" abstract_origin",
			 DST_SUBROUTINE_TYPE_abstract_origin(attr), FALSE);
   if (DST_IS_prototyped(flag))
      DST_put_string(" prototyped");
}


      
   /*------------------------------------------------
    * Dump routines for info, and include files/dirs
    *------------------------------------------------*/


static INT32
DST_dump_info(INT32        indentation, 
	      DST_DW_tag   tag,
	      DST_flag     flag,
	      DST_ATTR_IDX iattr,
	      DST_INFO_IDX iinfo)
{
   DST_write_line();

   if (indentation > 80) {
	DST_put_string("infinite loop while dumping DST?");
   	DST_write_line();
	exit(RC_INTERNAL_ERROR);
   }

   /* Put the index for this info */
   DST_nput_char(indentation, ' ');
   DST_put_idx(iinfo);

   switch (tag)
   {
   case DW_TAG_compile_unit:
      DST_put_compile_unit(flag, 
			   DST_ATTR_IDX_TO_PTR(iattr, DST_COMPILE_UNIT));
      break;
#ifdef KEY /* Bug 3507 */
   case DW_TAG_module:
      DST_put_module(flag, 
			 DST_ATTR_IDX_TO_PTR(iattr, DST_MODULE));
      break;
   case DW_TAG_imported_declaration:
      DST_put_imported_decl(flag, 
			 DST_ATTR_IDX_TO_PTR(iattr, DST_IMPORTED_DECL));
      break;
#endif /* KEY Bug 3507 */
   case DW_TAG_subprogram:
      DST_put_subprogram(flag, 
			 DST_ATTR_IDX_TO_PTR(iattr, DST_SUBPROGRAM));
      break;
   case DW_TAG_inlined_subroutine:
      DST_put_inlined_subroutine(flag,
				 DST_ATTR_IDX_TO_PTR(iattr,
						     DST_INLINED_SUBROUTINE));
      break;
   case DW_TAG_entry_point:
      DST_put_entry_point(flag, 
			 DST_ATTR_IDX_TO_PTR(iattr, DST_ENTRY_POINT));
      break;
   case DW_TAG_common_block:
      DST_put_common_block(flag,
			 DST_ATTR_IDX_TO_PTR(iattr, DST_COMMON_BLOCK));
      break;
   case DW_TAG_common_inclusion:
      DST_put_common_inclusion(flag,
			 DST_ATTR_IDX_TO_PTR(iattr, DST_COMMON_INCL));
      break;
   case DW_TAG_lexical_block:
      DST_put_lexical_block(flag, 
			    DST_ATTR_IDX_TO_PTR(iattr, DST_LEXICAL_BLOCK));
      break;
   case DW_TAG_label:
      DST_put_label(flag, 
		    DST_ATTR_IDX_TO_PTR(iattr, DST_LABEL));
      break;
   case DW_TAG_variable:
      DST_put_variable(flag, 
		       DST_ATTR_IDX_TO_PTR(iattr, DST_VARIABLE));
      break;
   case DW_TAG_formal_parameter:
      DST_put_formal_parameter(
         flag, 
	 DST_ATTR_IDX_TO_PTR(iattr, DST_FORMAL_PARAMETER));
      break;
   case DW_TAG_unspecified_parameters:
      DST_put_unspecified_parameters(
	 flag, 
	 DST_ATTR_IDX_TO_PTR(iattr, DST_UNSPECIFIED_PARAMETERS));
      break;
   case DW_TAG_base_type:
      DST_put_basetype(flag, 
		       DST_ATTR_IDX_TO_PTR(iattr, DST_BASETYPE));
      break;
   case DW_TAG_const_type:
      DST_put_const_type(flag, 
			 DST_ATTR_IDX_TO_PTR(iattr, DST_CONST_TYPE));
      break;
   case DW_TAG_constant:
      DST_put_constant(flag, 
			 DST_ATTR_IDX_TO_PTR(iattr, DST_CONSTANT));
      break;
   case DW_TAG_volatile_type:
      DST_put_volatile_type(flag, 
			    DST_ATTR_IDX_TO_PTR(iattr, DST_VOLATILE_TYPE));
      break;
   case DW_TAG_pointer_type:
      DST_put_pointer_type(flag, 
			   DST_ATTR_IDX_TO_PTR(iattr, DST_POINTER_TYPE));
      break;
   case DW_TAG_reference_type:
      DST_put_reference_type(flag, 
			     DST_ATTR_IDX_TO_PTR(iattr, DST_REFERENCE_TYPE));
      break;
   case DW_TAG_typedef:
      DST_put_typedef(flag, 
		      DST_ATTR_IDX_TO_PTR(iattr, DST_TYPEDEF));
      break;
   case DW_TAG_array_type:
      DST_put_array_type(flag, 
			 DST_ATTR_IDX_TO_PTR(iattr, DST_ARRAY_TYPE));
      break;
   case DW_TAG_subrange_type:
      DST_put_subrange_type(flag, 
			    DST_ATTR_IDX_TO_PTR(iattr, DST_SUBRANGE_TYPE));
      break;
   case DW_TAG_string_type:
      DST_put_string_type(flag, 
			    DST_ATTR_IDX_TO_PTR(iattr, DST_STRING_TYPE));
      break;
   case DW_TAG_structure_type:
      DST_put_structure_type(flag, 
			     DST_ATTR_IDX_TO_PTR(iattr, DST_STRUCTURE_TYPE));
      break;
   case DW_TAG_class_type:
      DST_put_class_type(flag, 
			     DST_ATTR_IDX_TO_PTR(iattr, DST_CLASS_TYPE));
      break;
   case DW_TAG_union_type:
      DST_put_union_type(flag, 
			 DST_ATTR_IDX_TO_PTR(iattr, DST_UNION_TYPE));
      break;
   case DW_TAG_member:
      DST_put_member(flag, 
		     DST_ATTR_IDX_TO_PTR(iattr, DST_MEMBER));
      break;
   case DW_TAG_inheritance:
      DST_put_inheritance(flag, 
			  DST_ATTR_IDX_TO_PTR(iattr, DST_INHERITANCE));
      break;
   case DW_TAG_template_type_param:
      DST_put_template_type_param(flag,
			  DST_ATTR_IDX_TO_PTR(iattr,
					      DST_TEMPLATE_TYPE_PARAMETER));
      break;
   case DW_TAG_template_value_param:
      DST_put_template_value_param(flag,
			  DST_ATTR_IDX_TO_PTR(iattr,
					      DST_TEMPLATE_VALUE_PARAMETER));
      break;
   case DW_TAG_enumeration_type:
      DST_put_enumeration_type(
         flag, 
	 DST_ATTR_IDX_TO_PTR(iattr, DST_ENUMERATION_TYPE));
      break;
   case DW_TAG_enumerator:
      DST_put_enumerator(flag, 
			 DST_ATTR_IDX_TO_PTR(iattr, DST_ENUMERATOR));
      break;
   case DW_TAG_subroutine_type:
      DST_put_subroutine_type(
         flag, 
	 DST_ATTR_IDX_TO_PTR(iattr, DST_SUBROUTINE_TYPE));
      break;
   default:
      DST_put_INT32_attribute(">>> Unprintable DW_TAG", tag);
      break;
      }
   DST_write_line();
   return indentation + 2;
}



static void
DST_dump_include_dirs(DST_DIR_IDX dir_idx, INT32 indentation)
{
   DST_DIR_IDX      idx = dir_idx;
   mUINT16          num = 0;
   DST_INCLUDE_DIR *dir;
   
   DST_write_line();
   if (!DST_IS_NULL(idx))
      dir = DST_DIR_IDX_TO_PTR(idx);
   else
      dir = NULL;

   while(dir != NULL)
   {
      num += 1;
      DST_put_idx(idx);
      DST_put_UINT32_attribute(" ordinal", num);
      DST_put_string_attribute(" path", DST_INCLUDE_DIR_path(dir));
      DST_write_line();
      idx = DST_INCLUDE_DIR_next(dir);
      if (!DST_IS_NULL(idx))
	 dir = DST_DIR_IDX_TO_PTR(idx);
      else
	 dir = NULL;
   }
}



static void
DST_dump_files(DST_FILE_IDX file_idx, INT32 indentation)
{
   DST_FILE_IDX  idx = file_idx;
   mUINT16       num = 0;
   DST_FILE_NAME *f;
   
   DST_write_line();
   if (!DST_IS_NULL(idx))
      f = DST_FILE_IDX_TO_PTR(idx);
   else
      f = NULL;

   while(f != NULL)
   {
      num += 1;
      DST_put_idx(idx);
      DST_put_UINT32_attribute(" ordinal", num);
      DST_put_string_attribute(" name", DST_FILE_NAME_name(f));
      DST_put_UINT32_attribute(" path", DST_FILE_NAME_dir(f));
      DST_put_UINT64_attribute(" size", DST_FILE_NAME_size(f));
      DST_put_UINT64_attribute(" modt", DST_FILE_NAME_modt(f));
      DST_write_line();
      idx = DST_FILE_NAME_next(f);
      if (!DST_IS_NULL(idx))
	 f = DST_FILE_IDX_TO_PTR(idx);
      else
	 f = NULL;
   }
}

static void
DST_dump_block_kind (DST_BLOCK_KIND k)
{
	switch (k) {
	case DST_include_dirs_block:
		DST_put_string("include_dirs");
		break;
	case DST_file_names_block:
		DST_put_string("file_names");
		break;
	case DST_macro_info_block:
		DST_put_string("macro_info");
		break;
	case DST_file_scope_block:
		DST_put_string("file_scope");
		break;
	case DST_local_scope_block:
		DST_put_string("local_scope");
		break;
	}
}


/* The main dumping routine!
*/
void
DST_dump(DST_DIR_IDX  incl_dirs,
	 DST_FILE_IDX files,
	 DST_INFO_IDX compile_unit)
{
   DST_BLOCK_IDX i;
   
   /* Initialization */
   next_char = 0;
   if (dumpf_name != NULL) {
	   dumpf = fopen(dumpf_name, "w");
   }
   DST_ASSERT(dumpf, "Cannot open DST dump file");
      
   /* Write the stuff */
   if (!DST_IS_NULL(incl_dirs))
   {
      DST_write_line();
      DST_put_string("------------ INCLUDE_DIRECTORIES ------------");
      DST_write_line();
      DST_dump_include_dirs(incl_dirs, 0);
   }
   if (!DST_IS_NULL(files))
   {
      DST_write_line();
      DST_put_string("------------<<<<<<< FILES >>>>>>>------------");
      DST_write_line();
      DST_dump_files(files, 0);
   }
   if (!DST_IS_NULL(compile_unit))
   {
      DST_write_line();
      DST_put_string("------------<<<<<< DST INFO >>>>>------------");
      DST_write_line();
      DST_preorder_visit(compile_unit, 0, &DST_dump_info);
   }
   DST_put_string("------------<<<< BLOCK INFO >>>------------");
   DST_write_line();
   FOREACH_DST_BLOCK(i) {
	sprintf(&tmp_buffer[0], "block %d:  ", i);
	DST_put_string(&tmp_buffer[0]);
	DST_dump_block_kind (((DST_Type *)Current_DST)->dst_blocks[i].kind);
	sprintf(&tmp_buffer[0], ", size = %d",
		((DST_Type *)Current_DST)->dst_blocks[i].size);
	DST_put_string(&tmp_buffer[0]);
	DST_write_line();
   }
   /* (void)fclose(dumpf); */
}

/* alternate entry to dump routine, finds idx values implicitly. */
void
Dump_DST (FILE *f)
{
	DST_IDX inc, fn, cmp;
	if (f == NULL) dumpf = stdout;
	else dumpf = f;
	inc = DST_get_include_dirs();
	fn = DST_get_file_names();
	cmp = DST_get_compile_unit();
	DST_dump (inc, fn, cmp);
}

void
DST_set_dump_filename(char *file_name)
{
   dumpf_name = file_name;
}
