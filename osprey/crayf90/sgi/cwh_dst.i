/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

typedef struct type_trans {
	DST_size_t size;
	const char *name;
	DST_ATE_encoding encoding;
} type_trans;	

static DST_INFO_IDX base_types[MTYPE_LAST+5] =  
{
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,	
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,	
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,	
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,
DST_INVALID_INIT
} ;
	
static type_trans ate_types[] = {
 4, "BAD",       0,		
 4, "UNK",       0,                     /* bit */
 1, "integer*1", DW_ATE_signed,	/* MTYPE_I1  */
 2, "integer*2", DW_ATE_signed,	/* MTYPE_I2  */
 4, "integer*4", DW_ATE_signed,	/* MTYPE_I4  */
 8, "integer*8", DW_ATE_signed,	/* MTYPE_I8  */
 1, "integer*1", DW_ATE_unsigned,	/* MTYPE_U1  */
 2, "integer*2", DW_ATE_unsigned,	/* MTYPE_U2  */
 4, "integer*4", DW_ATE_unsigned,	/* MTYPE_U4  */
 8, "integer*8", DW_ATE_unsigned,	/* MTYPE_U8  */
 4, "real*4",    DW_ATE_float,		/* MTYPE_F4  */
 8, "real*8",    DW_ATE_float,		/* MTYPE_F8  */
 10,"UNK",        DW_ATE_float,		/* MTYPE_F10 */
 16,"real*16",   DW_ATE_float,		/* MTYPE_F16 */
 1 ,"character" , DW_ATE_signed_char,   /* MTYPE_STR */
 16,"real*16",   DW_ATE_float,		/* MTYPE_FQ  */
 1, "UNK",        DW_ATE_unsigned_char,	/* MTYPE_M   */		
 8, "complex*4", DW_ATE_complex_float,	/* MTYPE_C4  */
 16,"complex*8", DW_ATE_complex_float,	/* MTYPE_C8  */
 32,"complex*16",DW_ATE_complex_float,	/* MTYPE_CQ  */
 1, "void",      0,                     /* MTYPE_V   */
 1, "logical*1", DW_ATE_boolean,	
 2, "logical*2", DW_ATE_boolean,	
 4, "logical*4", DW_ATE_boolean,	
 8, "logical*8", DW_ATE_boolean,	

} ;


static DST_INFO_IDX current_scope_idx = DST_INVALID_INIT; /* Current scope */
#ifdef KEY /* Bug 3507 */
/* Current module, if any */
static DST_INFO_IDX current_module_idx = DST_INVALID_INIT;
static char *current_module_name = 0;
#endif /* KEY Bug 3507 */
static DST_INFO_IDX comp_unit_idx     = DST_INVALID_INIT; /* Compilation unit */

static DST_FILE_IDX file_name_idx     = DST_INVALID_INIT; /* File names */
static DST_DIR_IDX  incl_dir_idx      = DST_INVALID_INIT; /* Include directories */


static char *current_working_dir ;
static char *current_host_dir    ; 

/* 
  For the list of directory names we maintain the following state.
*/

#define DST_NAME_TABLE_SIZE 64  /* Increment size of the dir and file list */
static char       **dir_list;
static mUINT16      next_dir_idx = 0;
static mUINT16      dir_list_size = 0;

/*
  For the list of file names we maintain the following state.
*/
static char       **file_list;
static mUINT16      next_file_idx = 0;
static mUINT16      file_list_size = 0;


/*
  This is the ST which requires the DST. It's global
  o avoid propagating the ST through the type
  information, when the only reason to do so is to form
  a DW_AT_subrange locations for dope vectors. It's hoped
  that dwarf will be able to express the locations as 
  offsets into types, later. The boolean says this ST
  has a dope vector TY somewhere, so requires a separate
  DST entry, so the address and bounds can describe memory locations.

  Making_FLD_DST is a hack to distinguish a call to see if
  a recursive type has been processed and a call to create a new
  set subrange locations for a pointer array component. 

  These should go away, if DSTs in structs become more expressive.
*/

ST * Top_ST ;
BOOL Top_ST_has_dope ;
BOOL Making_FLD_DST  ;
/* 
   array of struct TYs & associated DST_INFO_IDXs 
   to control nested types.
*/

typedef struct ty_dst {
	TY_IDX  ty ;
	DST_INFO_IDX idx ;
} TYIDX ;

#define STRUCT_DST_SIZE_CHANGE 512
static  INT32   Struct_Current_Size = 0;
static  TYIDX * Struct_DSTs =  NULL;
static  INT32   Struct_Top = -1 ;

/* 
   array of DST_INFO_IDXs of internal routines.
*/

static DST_INFO_IDX * Inner_DSTs;

#define INNER_DST_SIZE_CHANGE 128
static  INT32      Inner_Current_Size = 0;
static  INT32      Inner_Top = -1 ;

enum str_knd { s_NONE, s_WORD, s_BYTE, s_TWO_BYTE, s_CHAR } ;

static DST_FILE_IDX cwh_dst_enter_files(void) ;
static DST_DIR_IDX  DST_write_directories(void) ;
static DST_FILE_IDX DST_write_files(void) ;
static void         DST_directory_of(char *file_path, char **dir_name, UINT32 *dir_length);
static mUINT16      DST_get_ordinal_num(char    *the_name, 
					char  ***the_list, 
					mUINT16 *the_next, 
					mUINT16 *the_size) ;


/* macros to indicate functions which are external */

#ifdef KEY /* Bug 5271 */
#define GET_NEXT_ELEMENT_ST(c,s) cwh_auxst_next_element(c,s,l_PU_COMLIST)
#else /* KEY Bug 5271 */
#define GET_NEXT_ELEMENT_ST(c,s) cwh_auxst_next_element(c,s,l_COMLIST)
#endif /* KEY Bug 5271 */
#define GET_NEXT_ALTENTRY(c,s)  cwh_auxst_next_element(c,s,l_ALTENTRY)
#define GET_MODIFIED_NAME(s) cwh_auxst_stem_name(s)
#define GET_NEXT_COMMON(p,c) cwh_auxst_next_element(p,c,l_DST_COMLIST)
#define GET_NEXT_PARAMETER(p,c) cwh_auxst_next_element(p,c,l_DST_PARMLIST)
#define IS_DOPE_TY(t) cwh_types_is_dope(t) 
#define GET_DOPE_BASE_TY(t) cwh_types_dope_basic_TY(t)
#define IS_ALTENTRY(s)  ST_auxst_is_altentry(s)
#define GET_ST_LINENUM(s) cwh_auxst_srcpos_val(s)
#define GET_DOPE_BOUNDS(t) cwh_types_dope_dims_FLD(t)
#define GET_MAIN_ST() cwh_stab_main_ST()


/* forward references */

static DST_INFO_IDX   cwh_dst_basetype(TY_IDX ty) ;
static DST_INFO_IDX   cwh_dst_mk_type(TY_IDX  ty) ;
static DST_INFO_IDX   cwh_dst_mk_func(ST * st) ;
static void           cwh_dst_mk_var(ST * st,DST_INFO_IDX parent);
static void           cwh_dst_mk_const(ST * st,DST_INFO_IDX parent);
static DST_INFO_IDX  cwh_dst_mk_subroutine_type(TY_IDX  ty);
static char *cwh_dst_get_command_line_options(void); 
static INT32         DST_set_assoc_idx(INT32 dummy, 
	                               DST_DW_tag tag, 
                                       DST_flag flag, 
                                       DST_ATTR_IDX iattr, 
                                       DST_INFO_IDX inode) ;
static DST_INFO_IDX cwh_dst_array_type(TY_IDX ty) ;
static DST_INFO_IDX cwh_dst_subrange(ARB_HANDLE ar)  ;
static DST_INFO_IDX cwh_dst_member(FLD_HANDLE f,DST_INFO_IDX p) ;
static DST_INFO_IDX cwh_dst_struct_type(TY_IDX ty) ;
static DST_INFO_IDX cwh_dst_pointer_type(TY_IDX ty) ;
#ifdef KEY /* Bug 3507 */
/* If parent is not DST_INVALID_IDX, then refrain from emitting the common
 * block itself, and attach all of the common variables directly to "parent"
 * instead. This is used to eliminate faked-up common blocks which represent
 * global variables in modules.  */
static DST_INFO_IDX cwh_dst_mk_common(ST * st, DST_INFO_IDX parent);
#else /* KEY Bug 3507 */
static DST_INFO_IDX cwh_dst_mk_common(ST * st) ;
#endif /* KEY Bug 3507 */
static DST_INFO_IDX cwh_dst_mk_formal(ST * st) ;
static DST_INFO_IDX cwh_dst_mk_variable(ST * st) ;
static DST_INFO_IDX cwh_dst_mk_common_inclusion(ST * com, DST_IDX c) ;
#ifdef KEY /* Bug 3507 */
static DST_INFO_IDX cwh_dst_mk_imported_decl(char *mangled_name) ;
#endif /* KEY Bug 3507 */
static BOOL         cwh_dst_is_character_TY(TY_IDX ty) ;
static DST_INFO_IDX cwh_dst_substring_type(TY_IDX ty) ;
static DST_INFO_IDX cwh_dst_dope_type(TY_IDX  ty, ST *st, mINT64 ofst, DST_INFO_IDX p, BOOL ptr, DST_INFO_IDX *dope_ty) ;
static DST_INFO_IDX cwh_dst_mk_dope_bound(ST *dp, mINT64 offset,  DST_INFO_IDX t, DST_INFO_IDX p, BOOL comp);
static void         cwh_dst_dope_bounds(TY_IDX  td, ST * st, mINT64 offset, DST_INFO_IDX aa, DST_INFO_IDX p, BOOL comp) ;
static DST_INFO_IDX cwh_dst_struct_has_DST(TY_IDX ty) ;
static void         cwh_dst_struct_set_DST(TY_IDX ty, DST_INFO_IDX i) ;
static void         cwh_dst_struct_clear_DSTs(void);
static BOOL         cwh_dst_has_dope(TY_IDX  ty) ;
static void         cwh_dst_inner_add_DST(DST_INFO_IDX i) ;
static void         cwh_dst_inner_clear_DSTs(void);
static void         cwh_dst_inner_read_DSTs(DST_INFO_IDX parent);
static enum str_knd cwh_dst_stride_kind(TY_IDX  ty) ;
static void         cwh_dst_mk_MAIN(ST *mn,DST_INFO_IDX en_idx) ;
static char         Get_ST_Id (ST_IDX st, INT *level, INT *index);

