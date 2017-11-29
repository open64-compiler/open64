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


/* USMID @(#) libcif/unitrecord.h	30.3	01/25/96 10:10:42 */

/* position of record type in output file - within a unit(1) or other(0) */

static const int unit_record[CIF_MAXRECORD] = {
	0,			/* 00 = unused */
	1,			/* 01 = CIF_CALLSITE */
	0,			/* 02 = CIF_CIFHDR */
	1,			/* 03 = CIF_COMBLK */
	1,			/* 04 = CIF_CONST */
	1,			/* 05 = CIF_CDIR */
	1,			/* 06 = CIF_ENTRY */
	0,			/* 07 = CIF_FILE */
	1,			/* 08 = CIF_LOOP */
	0,			/* 09 = CIF_INCLUDE */
	1,			/* 10 = CIF_LABEL */
	1,			/* 11 = CIF_MESSAGE */
	1,			/* 12 = CIF_NAMELIST */
	1,			/* 13 = CIF_OBJECT */
	0,			/* 14 = CIF_SRCFILE */
	0,			/* 15 = CIF_SUMMARY */
	1,			/* 16 = CIF_CDIR_DOSHARED */
	1,			/* 17 = CIF_UNIT */
	1,			/* 18 = CIF_ENDUNIT */
	1,			/* 19 = CIF_USAGE */
	1,			/* 20 = CIF_ND_MSG */
	0,			/* 21 = CIF_EDOPTS */
	0,			/* 22 = CIF_MACH_CHAR */
	0,			/* 23 = CIF_MISC_OPTS */
	0,			/* 24 = CIF_OPT_OPTS */
	1,			/* 25 = CIF_STMT_TYPE */
	1,			/* 26= CIF_GEOMETRY */
	1,			/* 27= CIF_CONTINUATION */
	1,			/* 28 = CIF_F90_CALLSITE */
	1,			/* 29 = CIF_F90_COMBLK */
	1,			/* 30 = CIF_F90_CONST */
	1,			/* 31 = CIF_F90_ENTRY */
	1,			/* 32 = CIF_F90_LOOP */
	1,			/* 33 = CIF_F90_DERIVED_TYPE */
	1,			/* 34 = CIF_F90_LABEL */
	1,			/* 35 = CIF_F90_NAMELIST */
	1,			/* 36 = CIF_F90_OBJECT */
	0,			/* 37 = CIF_F90_MISC_OPTS */
	0,			/* 38 = CIF_F90_OPT_OPTS */
	1,			/* 39 = CIF_F90_BEGIN_SCOPE */
	1,			/* 40 = CIF_F90_END_SCOPE */
	1,			/* 41 = CIF_F90_SCOPE_INFO */
	1,			/* 42 = CIF_F90_USE_MODULE */
	1,			/* 43 = CIF_F90_RENAME */
	1,			/* 44 = CIF_F90_INT_BLOCK */
	1,			/* 45 = CIF_F90_VECTORIZATION */
	1,			/* 46 = CIF_BE_NODE */
	1,			/* 47 = CIF_TRANSFORM */
	0,			/* 48 = CIF_FILEDIR */
	1, 			/* 49 = CIF_UNITDIR */
	1,			/* 50 = CIF_BE_FID */
	1,			/* 51= CIF_C_TAG */
	0,			/* 52= CIF_C_OPTS */
	1,			/* 53= CIF_C_MESSAGE */
	1,			/* 54= CIF_C_CONST */
	1,			/* 55= CIF_C_ENTRY */
	1,			/* 56= CIF_C_OBJECT */
	1,			/* 57= CIF_C_LINT_DIRECTIVE */
	1,			/* 58= CIF_C_MACRO_DEF */
	1,			/* 59= CIF_C_MACRO_UNDEF */
	1,			/* 60= CIF_C_MACRO_USAGE */
	1,			/* 61= CIF_C_ENTRY_END */
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 62-79 */
	1,			/* 80= CIF_CC_TYPE */
	1,			/* 81= CIF_CC_ENTRY */
	1,			/* 82= CIF_CC_OBJ */
	1,			/* 83= CIF_CC_SUBTYPE */
	1,			/* 84= CIF_CC_ENUM */
	1,			/* 85= CIF_CC_EXPR */
	0			/* 86= CIF_SRC_POS */
};

static const int has_line[CIF_MAXRECORD] = {
	0,			/* 00 = unused */
	1,			/* 01 = CIF_CALLSITE */
	0,			/* 02 = CIF_CIFHDR */
	0,			/* 03 = CIF_COMBLK */
	0,			/* 04 = CIF_CONST */
	1,			/* 05 = CIF_CDIR */
	0,			/* 06 = CIF_ENTRY */
	0,			/* 07 = CIF_FILE */
	1,			/* 08 = CIF_LOOP */
	1,			/* 09 = CIF_INCLUDE */
	0,			/* 10 = CIF_LABEL */
	1,			/* 11 = CIF_MESSAGE */
	0,			/* 12 = CIF_NAMELIST */
	0,			/* 13 = CIF_OBJECT */
	0,			/* 14 = CIF_SRCFILE */
	0,			/* 15 = CIF_SUMMARY */
	1,			/* 16 = CIF_CDIR_DOSHARED */
	1,			/* 17 = CIF_UNIT */
	1,			/* 18 = CIF_ENDUNIT */
	1,			/* 19 = CIF_USAGE */
	1,			/* 20 = CIF_ND_MSG */
	0,			/* 21 = CIF_EDOPTS */
	0,			/* 22 = CIF_MACH_CHAR */
	0,			/* 23 = CIF_MISC_OPTS */
	0,			/* 24 = CIF_OPT_OPTS */
	1,			/* 25 = CIF_STMT_TYPE */
	0,			/* 26= CIF_GEOMETRY */
	1,			/* 27= CIF_CONTINUATION */
	1,			/* 28 = CIF_F90_CALLSITE */
	0,			/* 29 = CIF_F90_COMBLK */
	0,			/* 30 = CIF_F90_CONST */
	0,			/* 31 = CIF_F90_ENTRY */
	1,			/* 32 = CIF_F90_LOOP */
	0,			/* 33 = CIF_F90_DERIVED_TYPE */
	0,			/* 34 = CIF_F90_LABEL */
	0,			/* 35 = CIF_F90_NAMELIST */
	0,			/* 36 = CIF_F90_OBJECT */
	0,			/* 37 = CIF_F90_MISC_OPTS */
	0,			/* 38 = CIF_F90_OPT_OPTS */
	1,			/* 39 = CIF_F90_BEGIN_SCOPE */
	1,			/* 40 = CIF_F90_END_SCOPE */
	0,			/* 41 = CIF_F90_SCOPE_INFO */
	0,			/* 42 = CIF_F90_USE_MODULE */
	0,			/* 43 = CIF_F90_RENAME */
	0,			/* 44 = CIF_F90_INT_BLOCK */
	0,			/* 45 = CIF_F90_VECTORIZATION */
	0,			/* 46 = CIF_BE_NODE */
	1,			/* 47 = CIF_TRANSFORM */
	0,			/* 48 = CIF_FILEDIR */
	0, 			/* 49 = CIF_UNITDIR */
	0,			/* 50 = CIF_BE_FID */
	0,			/* 51= CIF_C_TAG */
	0,			/* 52= CIF_C_OPTS */
	1,			/* 53= CIF_C_MESSAGE */
	0,			/* 54= CIF_C_CONST */
	0,			/* 55= CIF_C_ENTRY */
	0			/* 56= CIF_C_OBJECT */
#if CIF_VERSION == 1
	};
#else
	,
	1,			/* 57= CIF_C_LINT_DIRECTIVE */
	1,			/* 58= CIF_C_MACRO_DEF */
	1,			/* 59= CIF_C_MACRO_UNDEF */
	1,			/* 60= CIF_C_MACRO_USAGE */
	1,			/* 61= CIF_C_ENTRY_END */
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 62-79 */
	0,			/* 80= CIF_CC_TYPE */
	0,			/* 81= CIF_CC_ENTRY */
	0,			/* 82= CIF_CC_OBJ */
	0,			/* 83= CIF_CC_SUBTYPE */
	0,			/* 84= CIF_CC_ENUM */
	0,			/* 85= CIF_CC_EXPR */
	1			/* 86= CIF_SRC_POS */
};
#endif
