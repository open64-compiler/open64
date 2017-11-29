/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/* USMID:  "\n@(#)5.0_pl/macros/cmd_line.m	5.1	04/29/99 21:22:31\n" */

/*****************\
|* MISCELLANEOUS *|
\*****************/

# define FALSE_OPTION		(0)
# define TRUE_OPTION	       (-1)


/********************\
|* SIZES AND LIMITS *|
\********************/

# define MAX_TRUNCATION_BITS	47			/* max value on -t opt*/


/******************************\
|* OBJECT REPLACEMENT STRINGS *|
\******************************/



/***********************************\
|* CONDITIONAL REPLACEMENT STRINGS *|
\***********************************/



/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/

# define MAKE_DEFAULT_NAME(NAME, STR, SUFFIX)				       \
	 { char *_nmp, *_stp;						       \
		_nmp = NAME;						       \
		_stp = strrchr (STR, SLASH);				       \
		_stp = (_stp == NULL) ? STR : _stp+1;			       \
		while (*_nmp++ = *_stp++) ;				       \
		_stp = strrchr (NAME, DOT);				       \
		if (_stp != NULL &&                                            \
                    (EQUAL_STRS(_stp, ".f") || EQUAL_STRS(_stp, ".F"))) {      \
		   strcpy (++_stp, SUFFIX);				       \
		}							       \
		else if (_stp != NULL &&                                       \
                   (EQUAL_STRS(_stp, ".f90") || EQUAL_STRS(_stp, ".F90"))) {   \
		   strcpy (++_stp, SUFFIX);				       \
		}							       \
		else if (_stp != NULL  &&  EQUAL_STRS(_stp, ".i")) {           \
		   strcpy (++_stp, SUFFIX);				       \
		}							       \
		else {							       \
		   _nmp--;						       \
		   *_nmp++ = DOT;					       \
		   strcpy (_nmp++, SUFFIX);				       \
		}							       \
	}

# define ARRAY_LEN(ARRAY)						       \
                (sizeof(ARRAY) / sizeof((ARRAY)[0]))

/* Generate a compile-time error if the constant EXPR is false. */
# define ASSERT_FALSE(EXPR)						       \
                ((void)sizeof(char[1 - 2 * !!(EXPR)]))

/* Copy a constant string STR of known length LEN into the fixed-length buffer
 * BUF and then append the character TERM.  To null-terminate the string, TERM
 * should be NULL_CHAR.  Issues a compile-time error if BUF is too small. */
# define COPY_STR_WITH_LEN(BUF, POS, STR, LEN, TERM)			       \
                ASSERT_FALSE(POS + LEN > ARRAY_LEN(BUF));		       \
                strncpy(&BUF[POS], STR, LEN - 1);			       \
                BUF[POS + LEN - 1] = TERM

/* Copy a constant string into the fixed-length buffer "msg_str" and terminate
 * the string with the character TERM. */
# define COPY_MSG_STR(POS, STR, TERM)					       \
                COPY_STR_WITH_LEN(msg_str, POS, STR, ARRAY_LEN(STR), TERM)

/* Copy the *_lvl_str constant string selected by PREFIX and LVL into the
 * fixed-length buffer "msg_str" and terminate the string with the character
 * TERM.  The length of the string is determined at compilation time by using a
 * representative example. */
# define COPY_LVL_STR(POS, PREFIX, LVL, TERM)				       \
                COPY_STR_WITH_LEN(msg_str, POS, PREFIX##_lvl_str[LVL],	       \
                                  ARRAY_LEN(PREFIX##_lvl_str[0]), TERM)

