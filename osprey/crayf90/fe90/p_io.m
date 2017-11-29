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


/* USMID:  "\n@(#)5.0_pl/macros/p_io.m	5.1	04/29/99 21:22:31\n" */


/*****************\
|* MISCELLANEOUS *|
\*****************/

# define READ_UNIT_STAR_VALUE		100
# define WRITE_UNIT_STAR_VALUE		101

/********************************************\
|* STUFF TO INTERACT WITH THE FORMAT PARSER *|
\********************************************/

/* Format parser call flags  - from format.h in library */

#define LIB_CALL                        0
#define COMPILER_CALL_ANSI              1       /* Obsolete */
#define COMPILER_CALL_NO_ANSI           2
#define COMPILER_CALL_ANSI_77           3
#define COMPILER_CALL_ANSI_90           4
#define COMPILER_CALL_ANSI_95           5
#define MAX_CALL_FLAG                   5

#define END_OF_MESSAGES                 0
#define FIRST_WARNING_MESSAGE           1
#define TRAILING_CHARS                  1

#define FIRST_NON_ANSI_MESSAGE          10
#define ANSI_EMPTY_PAREN_MSG            10
#define ANSI_COMMA_REQ                  11
#define COMMA_NON_ANSI                  12
#define REP_SLASH_NON_ANSI              13
#define NON_ANSI_EDIT_DESCRIPTOR        14
#define MINUS_X_NON_ANSI                15
#define E_WITH_D_NON_ANSI               16
#define H_IS_OBSOLETE_IN_F90            17
#define NON_ANSI_NULL_DESCRIPTOR        18
#define ZERO_WIDTH_NON_ANSI             19
#define MISSING_WIDTH_NON_ANSI          20
#define LAST_NON_ANSI_MESSAGE           20

#define FIRST_FATAL_MESSAGE             30
#define EXPECTING_LEFT_PAREN            30
#define EXPECTING_RIGHT_PAREN           31
#define EXPECTING_INTEGER               32
#define EXPECTING_PERIOD                33
#define EXPECTING_P_OR_X                34
#define INVALID_REP_COUNT               35
#define ZERO_REP_COUNT                  36
#define FIELD_WIDTH_ZERO                37
#define FIELD_TOO_LARGE                 38
#define ZERO_OR_NO_HOLLERITH_CNT        39
#define UNKNOWN_EDIT_DESCRIPTOR         40
#define NONTERMINATED_LITERAL           41
#define UNABLE_TO_MALLOC_MEMORY         42


/********************\
|* SIZES AND LIMITS *|
\********************/

# define NUM_PDG_CONTROL_LIST_ITEMS     10


/******************************\
|* OBJECT REPLACEMENT STRINGS *|
\******************************/

# define UNIT_IDX		1
# define FMT_IDX		2
# define NML_IDX		2
# define ERR_IDX                3
# define INQ_ERR_IDX            4
# define END_IDX		4
# define IOSTAT_IDX             5
# define REC_IDX		6
# define ADVANCE_IDX		7
# define SIZE_IDX		8
# define EOR_IDX		9
# define FILE_IDX		2


# define ERR_IS_PRESENT         1
# define END_IS_PRESENT         2
# define EOR_IS_PRESENT         4
# define IOSTAT_IS_PRESENT      8

/* flflag values, for split io calls */

# define FL_IO_SINGLE			3
# define FL_IO_FIRST			2
# define FL_IO_LAST			1
# define FL_IO_MIDDLE			0


/***********************************\
|* NAMELIST TABLE SIZES AND STUFF  *|
\***********************************/

/* if fcd's are two words, use these */

# define NML_GRP_HDR_SIZE_FCD2		3
# define NML_GRP_ITEM_SIZE_FCD2		4
# define NML_STRCT_HDR_SIZE_FCD2	2
# define NML_STRCT_ITEM_SIZE_FCD2	4
# define NML_SCALAR_ENTRY_SIZE_FCD2	3


# define NML_GRP_HDR_SIZE		2
# define NML_GRP_ITEM_SIZE		3
# define NML_STRCT_HDR_SIZE		2
# define NML_STRCT_ITEM_SIZE		3
# define NML_SCALAR_ENTRY_SIZE		2


# define NML_VALTYPE_SCALAR		1
# define NML_VALTYPE_ARRAY		2
# define NML_VALTYPE_STRCT		4
# define NML_VALTYPE_STRCT_ARRAY	5

/***********************************\
|* CONDITIONAL REPLACEMENT STRINGS *|
\***********************************/


/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/

