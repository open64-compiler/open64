/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


static char USMID[] = "@(#) libcif/cifdata.c	30.8	07/26/96 07:19:13";


/* --------------------------------------------------------------------------
 * cifdata contains initialization of CIF internal global data.
 * --------------------------------------------------------------------------
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#include "cif_int.h"

/* --- size of each record structure --- */
const short _Cif_structsize[CIF_MAXRECORD][_CIF_INT_VERSION + 1] = {
						/* + 1 allows us to index by cif_version
						   which starts at 1 */

  /*	Version 1			Version 2 			Version 3*/

0,  	0,				0,				0,					/* 00 */
0,	sizeof(struct Cif_callsite),	sizeof(struct Cif_callsite),	sizeof(struct Cif_callsite),		/* 01= CIF_CALLSITE */
0,	sizeof(struct Cif_cifhdr),	sizeof(struct Cif_cifhdr),	sizeof(struct Cif_cifhdr),		/* 02= CIF_CIFHDR */
0,	sizeof(struct Cif_comblk_1),	sizeof(struct Cif_comblk),	sizeof(struct Cif_comblk),		/* 03= CIF_COMBLK */
0,	sizeof(struct Cif_const),	sizeof(struct Cif_const),	sizeof(struct Cif_const),		/* 04= CIF_CONST */
0,	0,				sizeof(struct Cif_cdir),       	sizeof(struct Cif_cdir),	       	/* 05= CIF_CDIR */
0,	sizeof(struct Cif_entry),	sizeof(struct Cif_entry),	sizeof(struct Cif_entry),		/* 06= CIF_ENTRY */
0,	sizeof(struct Cif_file_1),	sizeof(struct Cif_file_1),	sizeof(struct Cif_file),		/* 07= CIF_FILE */
0,	sizeof(struct Cif_loop),	sizeof(struct Cif_loop),	sizeof(struct Cif_loop),		/* 08= CIF_LOOP */
0,	sizeof(struct Cif_include),	sizeof(struct Cif_include),	sizeof(struct Cif_include),		/* 09= CIF_INCLUDE */
0,	sizeof(struct Cif_label),	sizeof(struct Cif_label),	sizeof(struct Cif_label),		/* 10= CIF_LABEL */
0,	sizeof(struct Cif_message_1),	sizeof(struct Cif_message_1),	sizeof(struct Cif_message),		/* 11= CIF_MESSAGE */
0,	sizeof(struct Cif_namelist),	sizeof(struct Cif_namelist),	sizeof(struct Cif_namelist),		/* 12= CIF_NAMELIST */
0,	sizeof(struct Cif_object_1),	sizeof(struct Cif_object),	sizeof(struct Cif_object),		/* 13= CIF_OBJECT */
0,	sizeof(struct Cif_srcfile),	sizeof(struct Cif_srcfile),	sizeof(struct Cif_srcfile),		/* 14= CIF_SRCFILE */
0,	sizeof(struct Cif_summary),	sizeof(struct Cif_summary),	sizeof(struct Cif_summary),		/* 15= CIF_SUMMARY */
0,	0,				sizeof(struct Cif_cdir_doshared),sizeof(struct Cif_cdir_doshared),     /* 16= CIF_CDIR_DOSHARED */
0,	sizeof(struct Cif_unit),	sizeof(struct Cif_unit),	sizeof(struct Cif_unit),		/* 17= CIF_UNIT */
0,	sizeof(struct Cif_endunit),	sizeof(struct Cif_endunit),	sizeof(struct Cif_endunit),		/* 18= CIF_ENDUNIT */
0,	sizeof(struct Cif_usage_1),	sizeof(struct Cif_usage),	sizeof(struct Cif_usage),		/* 19= CIF_USAGE */
0,	sizeof(struct Cif_nd_msg),	sizeof(struct Cif_nd_msg),	sizeof(struct Cif_nd_msg),		/* 20= CIF_ND_MSG */
0,	sizeof(struct Cif_edopts),	sizeof(struct Cif_edopts),	sizeof(struct Cif_edopts),		/* 21= CIF_EDOPTS */
0,	sizeof(struct Cif_mach_char_1),	sizeof(struct Cif_mach_char),	sizeof(struct Cif_mach_char),		/* 22= CIF_MACH_CHAR*/
0,	sizeof(struct Cif_misc_opts_1),	sizeof(struct Cif_misc_opts),	sizeof(struct Cif_misc_opts), 		/* 23= CIF_MISC_OPTS*/
0,	sizeof(struct Cif_opt_opts_1),	sizeof(struct Cif_opt_opts),	sizeof(struct Cif_opt_opts),		/* 24= CIF_OPT_OPTS */
0,	sizeof(struct Cif_stmt_type),	sizeof(struct Cif_stmt_type),	sizeof(struct Cif_stmt_type),		/* 25= CIF_STMT_TYPE*/
0,	0,				sizeof(struct Cif_geometry),	sizeof(struct Cif_geometry),		/* 26= CIF_GEOMETRY */
0,	0,				sizeof(struct Cif_continuation),sizeof(struct Cif_continuation),       	/* 27= CIF_CONTINUATION */
0,	0,				sizeof(struct Cif_f90_callsite), sizeof(struct Cif_f90_callsite), 	/* 28= CIF_F90_CALLSITE */
0,	0,				sizeof(struct Cif_f90_comblk),	sizeof(struct Cif_f90_comblk),		/* 29= CIF_F90_COMBLK */
0,	0,				sizeof(struct Cif_f90_const),	sizeof(struct Cif_f90_const),		/* 30= CIF_F90_CONST */
0,	0,				sizeof(struct Cif_f90_entry),	sizeof(struct Cif_f90_entry),		/* 31= CIF_F90_ENTRY */
0,	0,				sizeof(struct Cif_f90_loop),	sizeof(struct Cif_f90_loop),		/* 32= CIF_F90_LOOP */
0,	0,				sizeof(struct Cif_f90_derived_type_2),sizeof(struct Cif_f90_derived_type), /* 33= CIF_F90_DERIVED_TYPE */
0,	0,				sizeof(struct Cif_f90_label),	sizeof(struct Cif_f90_label),		/* 34= CIF_F90_LABEL */
0,	0,				sizeof(struct Cif_f90_namelist),  sizeof(struct Cif_f90_namelist),  	/* 35= CIF_F90_NAMELIST */
0,	0,				sizeof(struct Cif_f90_object),	sizeof(struct Cif_f90_object),		/* 36= CIF_F90_OBJECT */
0,	0,				sizeof(struct Cif_f90_misc_opts), sizeof(struct Cif_f90_misc_opts),    /* 37= CIF_F90_MISC_OPTS */
0,	0,				sizeof(struct Cif_f90_opt_opts),   sizeof(struct Cif_f90_opt_opts),   	/* 38= CIF_F90_OPT_OPTS */
0,	0,				sizeof(struct Cif_f90_begin_scope), sizeof(struct Cif_f90_begin_scope), /* 39= CIF_F90_BEGIN_SCOPE */
0,	0,				sizeof(struct Cif_f90_end_scope), sizeof(struct Cif_f90_end_scope),	/* 40= CIF_F90_END_SCOPE */
0,	0,				sizeof(struct Cif_f90_scope_info), sizeof(struct Cif_f90_scope_info), 	/* 41= CIF_F90_SCOPE_INFO */
0,	0,				sizeof(struct Cif_f90_use_module),	sizeof(struct Cif_f90_use_module), /* 42= CIF_F90_USE_MODULE */
0,	0,				sizeof(struct Cif_f90_rename),	sizeof(struct Cif_f90_rename),		/* 43= CIF_F90_RENAME */
0,	0,				sizeof(struct Cif_f90_int_block_2),   sizeof(struct Cif_f90_int_block), /* 44= CIF_F90_INT_BLOCK */
0,	0,				sizeof(struct Cif_f90_vectorization),	sizeof(struct Cif_f90_vectorization),	/* 45= CIF_F90_VECTORIZATION */
0,	0,				sizeof(struct Cif_BE_node_2),	sizeof(struct Cif_BE_node),		/* 46= CIF_BE_NODE */
0,	0,				0,	     			0,	     				/* 47 */
0,	sizeof(struct Cif_filedir),	sizeof(struct Cif_filedir),	sizeof(struct Cif_filedir),		/* 48= CIF_FILEDIR */
0,	sizeof(struct Cif_unitdir), 	sizeof(struct Cif_unitdir),	sizeof(struct Cif_unitdir),		/* 49= CIF_UNITDIR */
0,	0,				sizeof(struct Cif_BE_fid),	sizeof(struct Cif_BE_fid),		/* 50= CIF_BE_FID */
0,	sizeof(struct Cif_c_tag),	sizeof(struct Cif_c_tag),	sizeof(struct Cif_c_tag),		/* 51= CIF_C_TAG */
0,	sizeof(struct Cif_c_opts),	sizeof(struct Cif_c_opts),	sizeof(struct Cif_c_opts),		/* 52= CIF_C_OPTS */
0,	sizeof(struct Cif_c_message_1),	sizeof(struct Cif_c_message),	sizeof(struct Cif_c_message),		/* 53= CIF_C_MESSAGE*/
0,	sizeof(struct Cif_c_const),	sizeof(struct Cif_c_const),	sizeof(struct Cif_c_const),		/* 54= CIF_C_CONST */
0,	sizeof(struct Cif_c_entry_1),	sizeof(struct Cif_c_entry),	sizeof(struct Cif_c_entry),		/* 55= CIF_C_ENTRY */
0,	sizeof(struct Cif_c_object),	sizeof(struct Cif_c_object),	sizeof(struct Cif_c_object),		/* 56= CIF_C_OBJECT */
0,	0,		   sizeof(struct Cif_c_lint_directive), sizeof(struct Cif_c_lint_directive),	/* 57= CIF_C_LINT_DIRECTIVE */
0,	0,		   sizeof(struct Cif_c_macro_def),	 sizeof(struct Cif_c_macro_def),	/* 58= CIF_C_MACRO_DEF */
0,	0,		   sizeof(struct Cif_c_macro_undef),	sizeof(struct Cif_c_macro_undef),	/* 59= CIF_C_MACRO_UNDEF */
0,	0,		   sizeof(struct Cif_c_macro_usage),	sizeof(struct Cif_c_macro_usage),	/* 60= CIF_C_MACRO_USAGE */
0,	0,		   sizeof(struct Cif_c_entry_end),	 sizeof(struct Cif_c_entry_end),		/* 61= CIF_C_ENTRY_END */
0, 0, 0, 0,	/* 62 */
0, 0, 0, 0,	/* 63 */
0, 0, 0, 0,	/* 64 */
0, 0, 0, 0,	/* 65 */
0, 0, 0, 0,	/* 66 */
0, 0, 0, 0,	/* 67 */
0, 0, 0, 0,	/* 68 */
0, 0, 0, 0,	/* 69 */
0, 0, 0, sizeof(struct Cif_src_pos),	/* 70= CIF_ORIG_CMD */
0, 0, 0, 0,	/* 71 */
0, 0, 0, 0,	/* 72 */
0, 0, 0, 0,	/* 73 */
0, 0, 0, 0,	/* 74 */
0, 0, 0, 0,	/* 75 */
0, 0, 0, 0,	/* 76 */
0, 0, 0, 0,	/* 77 */
0, 0, 0, 0,	/* 78 */
0, 0, 0, 0,	/* 79 */
0,	0,	0,	sizeof(struct Cif_cc_type),	/* 80= CIF_CC_TYPE */
0,	0,	0,	sizeof(struct Cif_cc_entry),	/* 81= CIF_CC_ENTRY */
0,	0,	0,	sizeof(struct Cif_cc_obj),	/* 82= CIF_CC_OBJ */
0,	0,	0,	sizeof(struct Cif_cc_subtype),	/* 83= CIF_CC_SUBTYPE */
0,	0,	0,	sizeof(struct Cif_cc_enum),	/* 84= CIF_CC_ENUM */
0,	0,	0,	sizeof(struct Cif_cc_expr),	/* 85= CIF_CC_EXPR */
0,	0,	0,	sizeof(struct Cif_src_pos)	/* 86= CIF_SRC_POS */
};

/* --- size of each (possibly shortened) binary record structure --- */
const short _Cif_shortsize[CIF_MAXRECORD][_CIF_INT_VERSION + 1] = {
						/* + 1 allows us to index by cif_version
						   which starts at 1 */
0,	0,			0,			0,		/* 00 */
0,	CALLSITE_SSIZE,		CALLSITE_SSIZE,		CALLSITE_SSIZE,		/* 01= CIF_CALLSITE */
0,	CIFHDR_SSIZE,		CIFHDR_SSIZE,		CIFHDR_SSIZE,		/* 02= CIF_CIFHDR */
0,	COMBLK_SSIZE_1,		COMBLK_SSIZE,		COMBLK_SSIZE,		/* 03= CIF_COMBLK */
0,	CONST_SSIZE,		CONST_SSIZE,		CONST_SSIZE,		/* 04= CIF_CONST */
0,	0,			CDIR_SSIZE,    		CDIR_SSIZE,    		/* 05= CIF_CDIR */
0,	ENTRY_SSIZE,		ENTRY_SSIZE,		ENTRY_SSIZE,		/* 06= CIF_ENTRY */
0,	FILE_SSIZE_1,		FILE_SSIZE_1,		FILE_SSIZE,		/* 07= CIF_FILE */
0,	LOOP_SSIZE,		LOOP_SSIZE,		LOOP_SSIZE,		/* 08= CIF_LOOP */
0,	INCLUDE_SSIZE,		INCLUDE_SSIZE,		INCLUDE_SSIZE,		/* 09= CIF_INCLUDE */
0,	LABEL_SSIZE,		LABEL_SSIZE,		LABEL_SSIZE,		/* 10= CIF_LABEL */
0,	MESSAGE_SSIZE_1,       	MESSAGE_SSIZE_1,	MESSAGE_SSIZE,		/* 11= CIF_MESSAGE */
0,	NAMELIST_SSIZE,		NAMELIST_SSIZE,		NAMELIST_SSIZE,		/* 12= CIF_NAMELIST */
0,	OBJECT_SSIZE_1,		OBJECT_SSIZE,		OBJECT_SSIZE,		/* 13= CIF_OBJECT */
0,	SRCFILE_SSIZE,		SRCFILE_SSIZE,		SRCFILE_SSIZE,		/* 14= CIF_SRCFILE */
0,	SUMMARY_SSIZE,		SUMMARY_SSIZE,		SUMMARY_SSIZE,		/* 15= CIF_SUMMARY */
0,	0,			CDIR_DOSHARED_SSIZE,	CDIR_DOSHARED_SSIZE,	/* 16= CIF_CDIR_DOSHARED */
0,	UNIT_SSIZE,		UNIT_SSIZE,		UNIT_SSIZE,		/* 17= CIF_UNIT */
0,	ENDUNIT_SSIZE,		ENDUNIT_SSIZE,		ENDUNIT_SSIZE,		/* 18= CIF_ENDUNIT */
0,	USAGE_SSIZE_1,		USAGE_SSIZE,		USAGE_SSIZE,		/* 19= CIF_USAGE */
0,	ND_MSG_SSIZE,		ND_MSG_SSIZE,		ND_MSG_SSIZE,		/* 20= CIF_ND_MSG */
0,	EDOPTS_SSIZE,		EDOPTS_SSIZE,		EDOPTS_SSIZE,		/* 21= CIF_EDOPTS */
0,	MACH_CHAR_SSIZE_1,	MACH_CHAR_SSIZE,	MACH_CHAR_SSIZE,	/* 22= CIF_MACH_CHAR */
0,	MISC_OPTS_SSIZE_1,	MISC_OPTS_SSIZE,	MISC_OPTS_SSIZE,	/* 23= CIF_MISC_OPTS */
0,	OPT_OPTS_SSIZE_1,      	OPT_OPTS_SSIZE,		OPT_OPTS_SSIZE,		/* 24= CIF_OPT_OPTS */
0,	STMT_TYPE_SSIZE,	STMT_TYPE_SSIZE,	STMT_TYPE_SSIZE,	/* 25= CIF_STMT_TYPE */
0,	0,			GEOMETRY_SSIZE,		GEOMETRY_SSIZE,		/* 26= CIF_GEOMETRY */
0,	0,			CONTINUATION_SSIZE,  	CONTINUATION_SSIZE,  	/* 27= CIF_CONTINUATION */
0,	0,			F90_CALLSITE_SSIZE,    	F90_CALLSITE_SSIZE,    	/* 28= CIF_F90_CALLSITE */
0,	0,			F90_COMBLK_SSIZE,      	F90_COMBLK_SSIZE,      	/* 29= CIF_F90_COMBLK */
0,	0,			F90_CONST_SSIZE,       	F90_CONST_SSIZE,       	/* 30= CIF_F90_CONST */
0,	0,			F90_ENTRY_SSIZE,       	F90_ENTRY_SSIZE,       	/* 31= CIF_F90_ENTRY */
0,	0,			F90_LOOP_SSIZE,	       	F90_LOOP_SSIZE,	       	/* 32= CIF_F90_LOOP */
0,	0,			F90_DERIVED_TYPE_SSIZE_2,	F90_DERIVED_TYPE_SSIZE,	/* 33= CIF_F90_DERIVED_TYPE */
0,	0,			F90_LABEL_SSIZE,       	F90_LABEL_SSIZE,       /* 34= CIF_F90_LABEL */
0,	0,			F90_NAMELIST_SSIZE,    	F90_NAMELIST_SSIZE,    	/* 35= CIF_F90_NAMELIST */
0,	0,			F90_OBJECT_SSIZE,      	F90_OBJECT_SSIZE,      	/* 36= CIF_F90_OBJECT */
0,	0,			F90_MISC_OPTS_SSIZE,   	F90_MISC_OPTS_SSIZE,   	/* 37= CIF_F90_MISC_OPTS */
0,	0,			F90_OPT_OPTS_SSIZE,    	F90_OPT_OPTS_SSIZE,    	/* 38= CIF_F90_OPT_OPTS */
0,	0,			F90_BEGIN_SCOPE_SSIZE,  F90_BEGIN_SCOPE_SSIZE, 	/* 39= CIF_F90_BEGIN_SCOPE */
0,	0,			F90_END_SCOPE_SSIZE,   	F90_END_SCOPE_SSIZE,   	/* 40= CIF_F90_END_SCOPE */
0,	0,			F90_SCOPE_INFO_SSIZE,  	F90_SCOPE_INFO_SSIZE,  	/* 41= CIF_F90_SCOPE_INFO */
0,	0,			F90_USE_MODULE_SSIZE,  	F90_USE_MODULE_SSIZE,  	/* 42= CIF_F90_USE_MODULE */
0,	0,			F90_RENAME_SSIZE,      	F90_RENAME_SSIZE,      	/* 43= CIF_F90_RENAME */
0,	0,			F90_INT_BLOCK_SSIZE_2,  F90_INT_BLOCK_SSIZE,   	/* 44= CIF_F90_INT_BLOCK */
0,	0,			F90_VECTORIZATION_SSIZE,F90_VECTORIZATION_SSIZE,/* 45= CIF_F90_VECTORIZATION */
0,	0,			BENODE_SSIZE_2,		BENODE_SSIZE,		/* 46= CIF_BE_NODE */
0,	0,			0,			0,			/* 47 */
0,	FILEDIR_SSIZE,		FILEDIR_SSIZE,		FILEDIR_SSIZE,		/* 48= CIF_FILEDIR */
0,	UNITDIR_SSIZE, 		UNITDIR_SSIZE, 		UNITDIR_SSIZE, 		/* 49= CIF_UNITDIR */
0,	0,			BEFID_SSIZE,		BEFID_SSIZE,		/* 50= CIF_BE_FID */
0,	CTAG_SSIZE,		CTAG_SSIZE,		CTAG_SSIZE,		/* 51= CIF_C_TAG */
0,	COPTS_SSIZE,		COPTS_SSIZE,		COPTS_SSIZE,		/* 52= CIF_C_OPTS */
0,	CMESSAGE_SSIZE_1,      	CMESSAGE_SSIZE,		CMESSAGE_SSIZE,		/* 53= CIF_C_MESSAGE */
0,	CCONST_SSIZE,		CCONST_SSIZE,		CCONST_SSIZE,		/* 54= CIF_C_CONST */
0,	CENTRY_SSIZE_1,		CENTRY_SSIZE_2,		CENTRY_SSIZE,		/* 55= CIF_C_ENTRY */
0,	COBJECT_SSIZE,		COBJECT_SSIZE,		COBJECT_SSIZE,		/* 56= CIF_C_OBJECT */
0,	0,			CLINT_DIRECTIVE_SSIZE, 	CLINT_DIRECTIVE_SSIZE,  /* 57= CIF_C_LINT_DIRECTIVE */
0,	0,			CMACRO_DEF_SSIZE, 	CMACRO_DEF_SSIZE, 	/* 58= CIF_C_MACRO_DEF */
0,	0,			CMACRO_UNDEF_SSIZE, 	CMACRO_UNDEF_SSIZE, 	/* 59= CIF_C_MACRO_UNDEF */
0,	0,			CMACRO_USAGE_SSIZE, 	CMACRO_USAGE_SSIZE, 	/* 60= CIF_C_MACRO_USAGE */
0,	0,			CENTRY_END_SSIZE, 	CENTRY_END_SSIZE, 	/* 61= CIF_C_ENTRY_END */
0, 0, 0, 0,	/* 62 */
0, 0, 0, 0,	/* 63 */
0, 0, 0, 0,	/* 64 */
0, 0, 0, 0,	/* 65 */
0, 0, 0, 0,	/* 66 */
0, 0, 0, 0,	/* 67 */
0, 0, 0, 0,	/* 68 */
0, 0, 0, 0,	/* 69 */
0, 0, 0, ORIG_CMD_SSIZE,	/* 70= CIF_ORIG_CMD */
0, 0, 0, 0,	/* 71 */
0, 0, 0, 0,	/* 72 */
0, 0, 0, 0,	/* 73 */
0, 0, 0, 0,	/* 74 */
0, 0, 0, 0,	/* 75 */
0, 0, 0, 0,	/* 76 */
0, 0, 0, 0,	/* 77 */
0, 0, 0, 0,	/* 78 */
0, 0, 0, 0,	/* 79 */
0,	0,	0,	CCTYPE_SSIZE,	/* 80= CIF_CC_TYPE */
0,	0,	0,	CCENTRY_SSIZE,	/* 81= CIF_CC_ENTRY */
0,	0,	0,	CCOBJ_SSIZE,	/* 82= CIF_CC_OBJ */
0,	0,	0,	CCSUBTYPE_SSIZE, /* 83= CIF_CC_SUBTYPE */
0,	0,	0,	CCENUM_SSIZE,	/* 84= CIF_CC_ENUM */
0,	0,	0,	CCEXPR_SSIZE,	/* 85= CIF_CC_EXPR */
0,	0,	0,	SRC_POS_SSIZE	/* 86= CIF_SRC_POS */
};

/*
 * Mapping between compiler generated object data type values and
 * those that the library wants to return. ie a set that is consistent with
 * f77 as far as possible and non-overlapping when not.
 *
 * This remapping is done for CIF Version 2.  However, this is not required
 * for CIF Version 3 because the newer cf90 2.0 compiler generates the
 * correct values from the start.
 *
 * NOTE: the following names are deprecated (per lrr) and no longer used:
 *   CIF_F90_DT_SHORT_CHAR_CONST
 *   CIF_F90_DT_SHORT_TYPELESS
 *   CIF_F90_DT_LONG_TYPELESS
 */
const int _Cif_f90_to_f77_dtypes[] = {
    CIF_F90_DT_UNKNOWN,
    CIF_F90_DT_CHARACTER_1,
    CIF_F90_DT_TYPELESS,
    CIF_F90_DT_TYPELESS,
    CIF_F90_DT_INTEGER_1,
    CIF_F90_DT_INTEGER_2,
    CIF_F90_DT_INTEGER_4,
    CIF_F90_DT_INTEGER_6,
    CIF_F90_DT_INTEGER_8,
    CIF_F90_DT_REAL_4,
    CIF_F90_DT_REAL_8,
    CIF_F90_DT_REAL_16,
    CIF_F90_DT_COMPLEX_4,
    CIF_F90_DT_COMPLEX_8,
    CIF_F90_DT_COMPLEX_16,
    CIF_F90_DT_FPTR,
    CIF_F90_DT_LOGICAL_1,
    CIF_F90_DT_LOGICAL_2,
    CIF_F90_DT_LOGICAL_4,
    CIF_F90_DT_LOGICAL_8,
    CIF_F90_DT_CHARACTER_1,
    CIF_F90_DT_CHARACTER_2,
    CIF_F90_DT_CHARACTER_4,
    CIF_F90_DT_FCPTR
};


/* --------------------------------------------------------------------------
 * _Cif_space is an array of pointers to memory management functions.  The
 * appropriate function is called based on memory management mode for the
 * CIF in question.
 *
 * The fancy declarations are need to cast away the function prototypes since
 * the different functions have different number of parameters.
 * --------------------------------------------------------------------------
 */

extern char * _Cif_fixed_space __((unsigned int, int));
extern char * _Cif_managed_space __((unsigned int, int));
char * (*_Cif_space[])() = {
	(char *(*)())0,
	(char *(*)())malloc,
	(char *(*)())_Cif_fixed_space,
	(char *(*)())_Cif_managed_space
};
