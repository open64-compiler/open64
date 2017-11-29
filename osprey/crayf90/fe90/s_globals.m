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


/* USMID:  "\n@(#)5.0_pl/macros/s_globals.m	5.2	05/27/99 10:30:26\n" */


/*****************\
|* MISCELLANEOUS *|
\*****************/

/* LCV_CONST_IDX is used by SET_LCV_CONST.				      */

# if _HOST64
# define        LCV_CONST_IDX   2
# else
# define        LCV_CONST_IDX   4
# endif

/********************\
|* SIZES AND LIMITS *|
\********************/



/******************************\
|* OBJECT REPLACEMENT STRINGS *|
\******************************/

# define SHAPE_FOLDABLE(OPND)		OPND.flag_1
# define SHAPE_WILL_FOLD_LATER(OPND)	OPND.flag_2


/***********************************\
|* CONDITIONAL REPLACEMENT STRINGS *|
\***********************************/



/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/

# define COPY_SHAPE(SHAPE_T, SHAPE_S, RANK)                                    \
                { int   _i;                                                    \
                        for(_i = 0; _i < RANK; _i++) {                         \
                           COPY_OPND(SHAPE_T[_i], SHAPE_S[_i]);                \
                         }                                                     \
                }


/* SET_LCV_CONST is used by the routines in s_cnstrct.c and in s_data.c       */
/* (implied-DO processing) to save away the value of an implied-DO variable   */
/* in an Attr entry.							      */
/* Note - A target constant is stored and retrieved                           */

# define LCV_WORDS(VAR)  (sizeof(VAR) / sizeof(attr_tbl[0].wd[0]))
# ifdef KEY
# define SET_LCV_CONST(ATTR, VALUE, WORDS, VAL_WORDS) {				       \
	 attr_tbl[ATTR].wd[LCV_CONST_IDX] = *(long *) &((VALUE));	       \
	 if (WORDS == 2) {						       \
	    attr_tbl[ATTR].wd[LCV_CONST_IDX+1] = (VAL_WORDS<WORDS)?0:*((long *) &((VALUE))+1);     \
	 }								       \
	 else if (WORDS == 3) {						       \
	    attr_tbl[ATTR].wd[LCV_CONST_IDX+1] = (VAL_WORDS<WORDS)?0:*((long *) &((VALUE))+1);     \
            attr_tbl[ATTR].wd[LCV_CONST_IDX+2] = (VAL_WORDS<WORDS)?0:*((long *) &((VALUE))+2);     \
	 }								       \
	 else if (WORDS == 4) {						       \
	    attr_tbl[ATTR].wd[LCV_CONST_IDX+1] = (VAL_WORDS<WORDS)?0:*((long *) &((VALUE))+1);     \
            attr_tbl[ATTR].wd[LCV_CONST_IDX+2] = (VAL_WORDS<WORDS)?0:*((long *) &((VALUE))+2);     \
            attr_tbl[ATTR].wd[LCV_CONST_IDX+3] = (VAL_WORDS<WORDS)?0:*((long *) &((VALUE))+3);     \
	 }								       \
	 }
# else
# define SET_LCV_CONST(ATTR, VALUE, WORDS) {				       \
	 attr_tbl[ATTR].wd[LCV_CONST_IDX] = *(long *) &((VALUE));	       \
	 if (WORDS == 2) {						       \
	    attr_tbl[ATTR].wd[LCV_CONST_IDX+1] = *((long *) &((VALUE))+1);     \
	 }								       \
	 else if (WORDS == 3) {						       \
	    attr_tbl[ATTR].wd[LCV_CONST_IDX+1] = *((long *) &((VALUE))+1);     \
            attr_tbl[ATTR].wd[LCV_CONST_IDX+2] = *((long *) &((VALUE))+2);     \
	 }								       \
	 else if (WORDS == 4) {						       \
	    attr_tbl[ATTR].wd[LCV_CONST_IDX+1] = *((long *) &((VALUE))+1);     \
            attr_tbl[ATTR].wd[LCV_CONST_IDX+2] = *((long *) &((VALUE))+2);     \
            attr_tbl[ATTR].wd[LCV_CONST_IDX+3] = *((long *) &((VALUE))+3);     \
	 }								       \
	 }
# endif

# define GET_LCV_CONST(ATTR, VALUE, WORDS) {                                   \
         *(long *) &((VALUE)) = attr_tbl[ATTR].wd[LCV_CONST_IDX];              \
         if (WORDS == 2) {                                                     \
            *((long *) &((VALUE))+1) = attr_tbl[ATTR].wd[LCV_CONST_IDX+1];     \
         }                                                                     \
         else if (WORDS == 3) {                                                \
            *((long *) &((VALUE))+1) = attr_tbl[ATTR].wd[LCV_CONST_IDX+1];     \
            *((long *) &((VALUE))+2) = attr_tbl[ATTR].wd[LCV_CONST_IDX+2];     \
         }                                                                     \
         else if (WORDS == 4) {                                                \
            *((long *) &((VALUE))+1) = attr_tbl[ATTR].wd[LCV_CONST_IDX+1];     \
            *((long *) &((VALUE))+2) = attr_tbl[ATTR].wd[LCV_CONST_IDX+2];     \
            *((long *) &((VALUE))+3) = attr_tbl[ATTR].wd[LCV_CONST_IDX+3];     \
         }                                                                     \
         }
