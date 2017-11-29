/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/utils.c	5.9	10/14/99 12:53:57\n";

# include "defines.h"           /* Machine dependent ifdefs */

# include "host.m"              /* Host machine dependent macros.*/
# include "host.h"              /* Host machine dependent header.*/
# include "target.m"            /* Target machine dependent macros.*/
# include "target.h"            /* Target machine dependent header.*/

# ifdef _ARITH_H
# include "arith.h"
# endif

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "debug.m"
# include "globals.h"
# include "tokens.h"
# include "sytb.h"

# if defined(_HOST32) && defined(_TARGET64)

# define OCT_FMT "%llo"
# define HEX_FMT "%llx"
# define DEC_FMT "%lld"

# else

# define OCT_FMT "%o"
# define HEX_FMT "%x"
# define DEC_FMT "%d"

# endif

# define LLOCT_FMT "%llo"
# define LLHEX_FMT "%llx"
# define LLDEC_FMT "%lld"


# define STR_FMT "\"%s\""

# if defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)
#    define DBL_FMT "%Le"
# elif defined(_HOST_OS_SOLARIS) || (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX))
#    define DBL_FMT "%e"
# else
#    define DBL_FMT "%Le"
# endif

# define FLT_FMT "%e"


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|*      Convert a numeric or typeless constant to a string to be used in      *|
|*      fprintf's.  This routine is used to avoid problems with varying       *|
|*      definitions of "long" and "float" on different architectures.         *|
|*      Only one long_type word is printed for Typeless constants. If the     *|
|*      Typeless constant is longer than one word, then the calling routine   *|
|*      must make multiple calls to convert_to_string with the address        *|
|*      "the_constant" advancing by a long_type.                              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      the_constant : address of the constant to be "converted"	      *|
|*      type_idx     : the data type of the constant (index into Type Table)  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      result       : the_constant converted to a string (also returned as   *|
|*                     the result of this function)			      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      A character string version of the_constant (in result).		      *|
|*                                                                            *|
\******************************************************************************/

char *	 convert_to_string (long_type	*the_constant,
			    int		 type_idx,
			    char	*result)

{

   int		i;
   int		inc;
   int		base;
   int		stat;
   char		str1[50];
   char		str2[50];
   long_type	value[4];
   long_type	value2[4];
   char		*fmt1;


   TRACE (Func_Entry, "convert_to_string", NULL);

   switch(TYP_LINEAR(type_idx)) {

# ifdef _ARITH_INPUT_CONV

      case Integer_1 :
      case Integer_2 :
      case Integer_4 :
      case Integer_8 :
         for (i = 0; i < num_host_wds[TYP_LINEAR(type_idx)]; i++) {
            value[i] = the_constant[i];
         }

         SHIFT_ARITH_ARG(value, TYP_LINEAR(type_idx));
         base = 10;
         stat = AR_convert_int_to_str(result, 
                         (const int *)&base,
                     (const AR_DATA *)value,
                    (const AR_TYPE *)&linear_to_arith[TYP_LINEAR(type_idx)]);
         break;

      case Real_4 :
      case Real_8 :
      case Real_16 :
         for (i = 0; i < num_host_wds[TYP_LINEAR(type_idx)]; i++) {
            value[i] = the_constant[i];
         }

         SHIFT_ARITH_ARG(value, TYP_LINEAR(type_idx));

         stat = AR_convert_float_to_str(result,
                       (const AR_DATA *)value,
                      (const AR_TYPE *)&linear_to_arith[TYP_LINEAR(type_idx)]);

         break;

      case Complex_4 :
         value[0] = *the_constant;
         value2[0] = *(the_constant + 1);

         SHIFT_ARITH_ARG(value, Real_4);
         SHIFT_ARITH_ARG(value2, Real_4);

         stat = AR_convert_float_to_str(str1,
                       (const AR_DATA *)value,
                      (const AR_TYPE *)&linear_to_arith[Real_4]);

         stat = AR_convert_float_to_str(str2,
                       (const AR_DATA *)value2,
                      (const AR_TYPE *)&linear_to_arith[Real_4]);

         sprintf(result, "(%s,%s)", str1, str2);
         break;

      case Complex_8 :
# ifdef _TARGET64
         inc = 1;
# else
         inc = 2;
# endif
         stat = AR_convert_float_to_str(str1,
                       (const AR_DATA *)the_constant,
                      (const AR_TYPE *)&linear_to_arith[Real_8]);

         stat = AR_convert_float_to_str(str2,
                       (const AR_DATA *)(the_constant + inc),
                      (const AR_TYPE *)&linear_to_arith[Real_8]);

         sprintf(result, "(%s,%s)", str1, str2);
         break;

      case Complex_16 :
         stat = AR_convert_float_to_str(str1,
                       (const AR_DATA *)the_constant,
                      (const AR_TYPE *)&linear_to_arith[Real_16]);

         stat = AR_convert_float_to_str(str2,
                       (const AR_DATA *)(the_constant + 2),
                      (const AR_TYPE *)&linear_to_arith[Real_16]);

         sprintf(result, "(%s,%s)", str1, str2);
         break;

# else

# ifdef _TARGET32
      case Integer_1 :
      case Integer_2 :
      case Integer_4 :
         switch (convert_to_string_fmt) {

            case Octal_Fmt:
               fmt1 = OCT_FMT;
               break;

            case Character_Fmt:
            case Binary_Fmt:
            case Hex_Fmt:
               fmt1 = HEX_FMT;
               break;

            case Dont_Care:
            default:
               fmt1 = DEC_FMT;
               break;
         }

         sprintf(result, fmt1, *the_constant);
         break;

      case Integer_8 :
         switch (convert_to_string_fmt) {

            case Octal_Fmt:
               fmt1 = LLOCT_FMT;
               break;

            case Character_Fmt:
            case Binary_Fmt:
            case Hex_Fmt:
               fmt1 = LLHEX_FMT;
               break;

            case Dont_Care:
            default:
               fmt1 = LLDEC_FMT;
               break;
         }

         sprintf(result, fmt1, *(long long *)the_constant);
         break;

# else
      case Integer_1 :
      case Integer_2 :
      case Integer_4 :
      case Integer_8 :
         switch (convert_to_string_fmt) {

            case Octal_Fmt:
               fmt1 = OCT_FMT;
               break;

            case Character_Fmt:
            case Binary_Fmt:
            case Hex_Fmt:
               fmt1 = HEX_FMT;
               break;

            case Dont_Care:
            default:
               fmt1 = DEC_FMT;
               break;
         }

         sprintf(result, fmt1, *the_constant);
         break;
# endif

# ifdef _TARGET64
      case Real_4 :
      case Real_8 :
         if (sizeof(float_type) > sizeof(float)) {
#if defined(_HOST32)
            sprintf(result, FLT_FMT, *(float_type *)the_constant);
#else
            sprintf(result, DBL_FMT, *(ldouble *)the_constant);
#endif
         }
         else {
            sprintf(result, FLT_FMT, *(float_type *)the_constant);
         }
         break;

      case Real_16 :
         sprintf(result, DBL_FMT, *(ldouble *)the_constant);
         break;

      case Complex_4 :
      case Complex_8 :
         inc = (TYP_LINEAR(type_idx) > COMPLEX_DEFAULT_TYPE) ? 2 : 1;

         if (sizeof(float_type) > sizeof(float)) {
#if defined(_HOST32)
            sprintf(result, "(" FLT_FMT ", " FLT_FMT ")",
                                           *(float_type *)the_constant,
                                           *(float_type *)(the_constant + inc));
#else
            sprintf(result, "(" DBL_FMT ", " DBL_FMT ")",
                                           *(ldouble *)the_constant,
                                           *(ldouble *)(the_constant + inc));
#endif
         }
         else {
            sprintf(result, "(" FLT_FMT ", " FLT_FMT ")",
                                           *(float_type *)the_constant,
                                           *(float_type *)(the_constant + inc));
         }
         break;

      case Complex_16 :
         inc = (TYP_LINEAR(type_idx) > COMPLEX_DEFAULT_TYPE) ? 2 : 1;

         sprintf(result, "(" DBL_FMT ", " DBL_FMT ")",
                                        *(ldouble *)the_constant,
                                        *(ldouble *)(the_constant + inc));
         break;

# else 
      case Real_4 :
         sprintf(result, FLT_FMT, *(float *)the_constant);
         break;

      case Real_8 :
# if defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)
         sprintf(result, FLT_FMT, *(double *)the_constant);
         break;
# endif

      case Real_16 :
         if (sizeof(ldouble) > sizeof(float)) {
            sprintf(result, DBL_FMT, *(ldouble *)the_constant);
         }
         else {
            sprintf(result, FLT_FMT, *(float_type *)the_constant);
         }
         break;

      case Complex_4 :
         inc = (TYP_LINEAR(type_idx) > COMPLEX_DEFAULT_TYPE) ? 2 : 1;

         sprintf(result, "(" FLT_FMT ", " FLT_FMT ")",
                                        *(float_type *)the_constant,
                                        *(float_type *)(the_constant + inc));
         break;

      case Complex_8 :
      case Complex_16 :
         inc = (TYP_LINEAR(type_idx) > COMPLEX_DEFAULT_TYPE) ? 2 : 1;

         if (sizeof(ldouble) > sizeof(float)) {
            sprintf(result, "(" DBL_FMT ", " DBL_FMT ")",
                                           *(ldouble *)the_constant,
                                           *(ldouble *)(the_constant + inc));
         }
         break;
# endif
# endif

      case Typeless_4:
      case Typeless_8:
      case Short_Typeless_Const:
      case Long_Typeless:
         if (convert_to_string_fmt == Hex_Fmt) {
            fmt1 = HEX_FMT;
         }
         else {
            fmt1 = OCT_FMT;
         }

         /* only one word is printed */

         sprintf(result, fmt1, *the_constant);
         break;

      case Character_1:
         *result = (char) *the_constant;
         break;

      case Logical_1:
      case Logical_2:
      case Logical_4:
      case Logical_8:
         if (THIS_IS_TRUE(the_constant, type_idx)) {
            sprintf(result, ".T.");
         }
         else {
            sprintf(result, ".F.");
         }
         break;

      default :
         PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                  "valid type", "convert_to_string");
      
   }

   convert_to_string_fmt = Dont_Care;

   TRACE (Func_Exit, "convert_to_string", NULL);

   return(result);

}  /* convert_to_string */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ALL compiler temporary files should be created by this routine so     *|
|*      their location can be controlled by TMPDIR.			      *|
|*                                                                            *|
|*      Use UNIX function "tempnam" to produce a unique file name.  The first *|
|*      argument to tempnam points to a name of a directory in which the file *|
|*      is to be created.  Since we set this argument to NULL, tempname will  *|
|*      use the path in P_tmpdir in <stdio.h>.  If that directory is not      *|
|*      available, /tmp will be used.  The user can override this sequence by *|
|*      using environment variable TMPDIR to specify what directory they want *|
|*      the file to be created in.  The second argument allows for a file     *|
|*      name prefix.  Although tempnam tries to produce a unique name and     *|
|*      normally creates the file in a temp directory that should be scrubbed *|
|*      reasonably often by the system administrator, we have seen the names  *|
|*      roll over and clash with existing names.  The second argument could   *|
|*      be used to prefix the file name with the month and day for example    *|
|*      (the complete month, day, and year are too large since only 5 prefix  *|
|*      characters are allowed.						      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      open_status : the file access method(s)			              *|
|*      file_name   : if NULL, ignore this argument; otherwise, return the    *|
|*                    file name in it					      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      file_ptr  : set to point to the temporary file			      *|
|*      file_name : returns the temporary file name			      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if the temporary file could be created and opened;	otherwise,    *|
|*      returns FALSE.							      *|
|*                                                                            *|
\******************************************************************************/

boolean get_temp_file(char       *open_status,
                      FILE      **file_ptr,
                      char       *file_name)
{
   boolean      result;
   char        *tmp_file_name;



   TRACE (Func_Entry, "get_temp_file", NULL);

   result = FALSE;

#ifdef KEY	// bug 1383
   int fd;
   char buf[32];
   strcpy(buf, "/tmp/pathf90XXXXXX");
   if ((fd = mkstemp(buf)) != -1) {
     if (file_name != NULL) {
       strcpy(file_name, buf);
     }
     if ((*file_ptr = fdopen(fd, open_status)) != NULL) {
       result = TRUE;
     }
   }
#else
   tmp_file_name = (char *) tempnam(NULL, NULL);


   if (tmp_file_name != NULL) {

      if (file_name != NULL) {
         strcpy(file_name, tmp_file_name);
      }

      if ((*file_ptr = fopen(tmp_file_name, open_status)) != NULL) {
         result = TRUE;
      }
   }
#endif

   TRACE (Func_Exit, "get_temp_file", NULL);

   return(result);

}  /* get_temp_file */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      srch_name_tbl searches a name table for the identifier or label       *|
|*      specified in name_str.                                                *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*	name_str    -> The string to search for.                              *|
|*	name_len    -> The length of string to search for.                    *|
|*	table       -> The name table to search in.  This currently could be  *|
|*	               loc_name_tbl, global_name_tbl, hidden_name_tbl or      *|
|*	               cc_name_tbl.                                           *|
|*	pool        -> The pool that the table uses (str_pool or name_pool)   *|
|*	first_entry -> The first entry of the name table.                     *|
|*	last_entry  -> The last entry of the name table.                      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      name_idx    -> The name table index where the match occured           *|
|*                     or where entry should be inserted                      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      The test value.  non zero means found, zero means not found.          *|
|*                                                                            *|
|* Method:							              *|
|*       The word length of the identifier is calculated (id_len).  This is   *|
|*       used to determine the number of words to compare in the name pool.   *|
|*       The local name table is a sorted table (alphabetically) of links     *|
|*       between the name pool and the attribute table.  The body of the      *|
|*       routine is a switch statement based on id_len.                       *|
|*                                                                            *|
|*       The search samples local name table entries at a stride calculated   *|
|*       so that the loop is a shortloop (<= vector length).  The compare     *|
|*       is long integer subtraction, a word at a time.  When the sign of     *|
|*       the result goes positive, the search has gone too far, or if the     *|
|*       result is zero, there has been a match.  If there was not a match,   *|
|*       the search area becomes the entries in the table between the two     *|
|*       entries sampled where the sign has changed from negative to          *|
|*       positive.  This process continues until the stride between samplings *|
|*       is 1.  At this point, if no match is found, the entry where the      *|
|*       sign bit changed is the point where the new identifier should be     *|
|*       entered.  As an optimization, once a segment length for searching    *|
|*       is 32 or less, the search goes immediately to stride 1 search.       *|
|*                                                                            *|
|*       The above describes the basic search method for 1 word identifiers.  *|
|*       For more than one word, the first word is compared.  A mask is       *|
|*       created which is all ones (-1) if the first words did not match,,    *|
|*       and zero if they matched.  The second words are compared and the     *|
|*       result anded bitwise with the complement of the mask; thus if        *|
|*       there was a difference in the first word, any difference in the      *|
|*       second word is ignored.  The mask is recalculated based on the       *|
|*       result of the compares of the first two words, and the process is    *|
|*       repeated, until all words are compared.  The net result is to        *|
|*       carry forward a running difference, which carries the sign bit of    *|
|*       the first word of the compare which was not a match.  The general    *|
|*       computation of masks and differences is as follows:                  *|
|*                                                                            *|
|*       diff(i) = name_pool_word(i) - identifier_word(i)                     *|
|*                                                                            *|
|*       rdiff(0) = 0                                                         *|
|*       mask(0)  = 0                                                         *|
|*       ...                                                                  *|
|*       rdiff(i) = rdiff(i-1) | ((~mask(i-1)) & diff(i))                     *|
|*       mask(i)  = -(sign_bit_of(diff(i) | -(diff(i)) | mask(i-1)            *|
|*                                                                            *|
|*       The variable tst_val accumulates the running differences, mask       *|
|*       contains the current mask.  Since the values of rdiff(0) and         *|
|*       mask(0) are both 0, the computation of rdiff(1) and mask(1) have     *|
|*       have been optimized to remove the following unneeded calculations.   *|
|*                                                                            *|
|*         0 | anything = anything;  ~0 = -1;  -1 & anything = anything       *|
|*                                                                            *|
\******************************************************************************/

int srch_name_tbl(char		       *name_str,
                  int			name_len,
                  int		       *name_idx,
                  name_tbl_type	       *table,
                  name_pool_type       *pool,
                  int	 		first_entry,
                  int	 		last_entry)

{
   register int          first;         /* 1st word index of search segment */
   register long        *id ;           /* pointer to 1st word of identifier */
   register int          id_len;        /* word length of identifier */
   register int          idx;           /* name table index */
   register int          last;          /* last word index of search segment */
   register long         mask;          /* mask used in search */
   register int          seg_len;       /* number of entries in search segment*/
   register int          stride;        /* search stride between entries */
   register long         tst_val;       /* result of name comparison */

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   register long        *name_tbl_base; /* name table base address */
# endif

/*********************************************************************/
/*  Specific macros needed for this routine.  These macros reference */
/*  variable declared in this routine.                               */
/*********************************************************************/

/* Warning: for optimization purposes, don't remove parens around DIFF macro */

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
# define NAME_IDX(IDX)         (name_tbl_base[IDX] & MSK_NAME_IDX)
# else
# define NAME_IDX(IDX)         (table[IDX].name_idx)
# endif

# if defined(_HOST_LITTLE_ENDIAN)
# define DIFF(I) compare_names(&pool[NAME_IDX(idx)+(I)-1].name_long,           \
                               HOST_BYTES_PER_WORD-1,                          \
                               &id[(I) - 1],                                   \
                               HOST_BYTES_PER_WORD-1 )
# else
# define DIFF(I)  (pool[NAME_IDX(idx) + (I) - 1].name_long - id[(I) - 1])
# endif

# define MASK(I)  -(RIGHT_JUSTIFY_SIGN_BIT(DIFF((I)) | -DIFF((I))))| mask

# define RUNNING_DIFF(I)                tst_val | ((~mask) & DIFF(I))

/***************************/
/*  End macro definitions. */
/***************************/

   TRACE (Func_Entry, "srch_name_tbl", name_str);

   /* initialize first and last word of segment and segment length for search */

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   name_tbl_base = (long *) table;
# endif
   id            = (long *) name_str;
   first         = first_entry;
   last          = last_entry;
   seg_len       = last - first + 1;
   id_len        = WORD_LEN(name_len);
   stride        = STRIDE_CALC(seg_len);
   tst_val       = -1;
   mask          = 0;

   switch (id_len) {
      case 1:
         while (seg_len > 32) {
#           pragma _CRI ivdep
#           pragma _CRI shortloop
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

#        pragma _CRI ivdep
#        pragma _CRI shortloop
         for (idx = first; idx <= last; idx += 1) {
/*
            fprintf(stderr, "idx = %d, name_idx = %d, %d, value = %ld\n",
                    idx, table[idx].name_idx, NAME_IDX(idx),
                    pool[NAME_IDX(idx)].name_long);
*/
            tst_val = DIFF(1);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 2:
         while (seg_len > 32) {
#           pragma _CRI ivdep
#           pragma _CRI shortloop
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

#        pragma _CRI ivdep
#        pragma _CRI shortloop
         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 3:
         while (seg_len > 32) {
#           pragma _CRI ivdep
#           pragma _CRI shortloop
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

#        pragma _CRI ivdep
#        pragma _CRI shortloop
         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 4:
         while (seg_len > 32) {
#           pragma _CRI ivdep
#           pragma _CRI shortloop
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

#        pragma _CRI ivdep
#        pragma _CRI shortloop
         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 5:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 6:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 7:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 8:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 9:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 10:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 11:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 12:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 13:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 14:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 15:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 16:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            if (tst_val >= 0) {
               break;
            }
         }
         break;


      case 17:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

# ifdef _HOST32

      case 18:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 19:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 20:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 21:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 22:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 23:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 24:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               mask    = MASK(23);
               tst_val = RUNNING_DIFF(24);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            mask    = MASK(23);
            tst_val = RUNNING_DIFF(24);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 25:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               mask    = MASK(23);
               tst_val = RUNNING_DIFF(24);
               mask    = MASK(24);
               tst_val = RUNNING_DIFF(25);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            mask    = MASK(23);
            tst_val = RUNNING_DIFF(24);
            mask    = MASK(24);
            tst_val = RUNNING_DIFF(25);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 26:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               mask    = MASK(23);
               tst_val = RUNNING_DIFF(24);
               mask    = MASK(24);
               tst_val = RUNNING_DIFF(25);
               mask    = MASK(25);
               tst_val = RUNNING_DIFF(26);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            mask    = MASK(23);
            tst_val = RUNNING_DIFF(24);
            mask    = MASK(24);
            tst_val = RUNNING_DIFF(25);
            mask    = MASK(25);
            tst_val = RUNNING_DIFF(26);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 27:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               mask    = MASK(23);
               tst_val = RUNNING_DIFF(24);
               mask    = MASK(24);
               tst_val = RUNNING_DIFF(25);
               mask    = MASK(25);
               tst_val = RUNNING_DIFF(26);
               mask    = MASK(26);
               tst_val = RUNNING_DIFF(27);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            mask    = MASK(23);
            tst_val = RUNNING_DIFF(24);
            mask    = MASK(24);
            tst_val = RUNNING_DIFF(25);
            mask    = MASK(25);
            tst_val = RUNNING_DIFF(26);
            mask    = MASK(26);
            tst_val = RUNNING_DIFF(27);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 28:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               mask    = MASK(23);
               tst_val = RUNNING_DIFF(24);
               mask    = MASK(24);
               tst_val = RUNNING_DIFF(25);
               mask    = MASK(25);
               tst_val = RUNNING_DIFF(26);
               mask    = MASK(26);
               tst_val = RUNNING_DIFF(27);
               mask    = MASK(27);
               tst_val = RUNNING_DIFF(28);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            mask    = MASK(23);
            tst_val = RUNNING_DIFF(24);
            mask    = MASK(24);
            tst_val = RUNNING_DIFF(25);
            mask    = MASK(25);
            tst_val = RUNNING_DIFF(26);
            mask    = MASK(26);
            tst_val = RUNNING_DIFF(27);
            mask    = MASK(27);
            tst_val = RUNNING_DIFF(28);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 29:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               mask    = MASK(23);
               tst_val = RUNNING_DIFF(24);
               mask    = MASK(24);
               tst_val = RUNNING_DIFF(25);
               mask    = MASK(25);
               tst_val = RUNNING_DIFF(26);
               mask    = MASK(26);
               tst_val = RUNNING_DIFF(27);
               mask    = MASK(27);
               tst_val = RUNNING_DIFF(28);
               mask    = MASK(28);
               tst_val = RUNNING_DIFF(29);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            mask    = MASK(23);
            tst_val = RUNNING_DIFF(24);
            mask    = MASK(24);
            tst_val = RUNNING_DIFF(25);
            mask    = MASK(25);
            tst_val = RUNNING_DIFF(26);
            mask    = MASK(26);
            tst_val = RUNNING_DIFF(27);
            mask    = MASK(27);
            tst_val = RUNNING_DIFF(28);
            mask    = MASK(28);
            tst_val = RUNNING_DIFF(29);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 30:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               mask    = MASK(23);
               tst_val = RUNNING_DIFF(24);
               mask    = MASK(24);
               tst_val = RUNNING_DIFF(25);
               mask    = MASK(25);
               tst_val = RUNNING_DIFF(26);
               mask    = MASK(26);
               tst_val = RUNNING_DIFF(27);
               mask    = MASK(27);
               tst_val = RUNNING_DIFF(28);
               mask    = MASK(28);
               tst_val = RUNNING_DIFF(29);
               mask    = MASK(29);
               tst_val = RUNNING_DIFF(30);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            mask    = MASK(23);
            tst_val = RUNNING_DIFF(24);
            mask    = MASK(24);
            tst_val = RUNNING_DIFF(25);
            mask    = MASK(25);
            tst_val = RUNNING_DIFF(26);
            mask    = MASK(26);
            tst_val = RUNNING_DIFF(27);
            mask    = MASK(27);
            tst_val = RUNNING_DIFF(28);
            mask    = MASK(28);
            tst_val = RUNNING_DIFF(29);
            mask    = MASK(29);
            tst_val = RUNNING_DIFF(30);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 31:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               mask    = MASK(23);
               tst_val = RUNNING_DIFF(24);
               mask    = MASK(24);
               tst_val = RUNNING_DIFF(25);
               mask    = MASK(25);
               tst_val = RUNNING_DIFF(26);
               mask    = MASK(26);
               tst_val = RUNNING_DIFF(27);
               mask    = MASK(27);
               tst_val = RUNNING_DIFF(28);
               mask    = MASK(28);
               tst_val = RUNNING_DIFF(29);
               mask    = MASK(29);
               tst_val = RUNNING_DIFF(30);
               mask    = MASK(30);
               tst_val = RUNNING_DIFF(31);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            mask    = MASK(23);
            tst_val = RUNNING_DIFF(24);
            mask    = MASK(24);
            tst_val = RUNNING_DIFF(25);
            mask    = MASK(25);
            tst_val = RUNNING_DIFF(26);
            mask    = MASK(26);
            tst_val = RUNNING_DIFF(27);
            mask    = MASK(27);
            tst_val = RUNNING_DIFF(28);
            mask    = MASK(28);
            tst_val = RUNNING_DIFF(29);
            mask    = MASK(29);
            tst_val = RUNNING_DIFF(30);
            mask    = MASK(30);
            tst_val = RUNNING_DIFF(31);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 32:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               mask    = MASK(23);
               tst_val = RUNNING_DIFF(24);
               mask    = MASK(24);
               tst_val = RUNNING_DIFF(25);
               mask    = MASK(25);
               tst_val = RUNNING_DIFF(26);
               mask    = MASK(26);
               tst_val = RUNNING_DIFF(27);
               mask    = MASK(27);
               tst_val = RUNNING_DIFF(28);
               mask    = MASK(28);
               tst_val = RUNNING_DIFF(29);
               mask    = MASK(29);
               tst_val = RUNNING_DIFF(30);
               mask    = MASK(30);
               tst_val = RUNNING_DIFF(31);
               mask    = MASK(31);
               tst_val = RUNNING_DIFF(32);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            mask    = MASK(23);
            tst_val = RUNNING_DIFF(24);
            mask    = MASK(24);
            tst_val = RUNNING_DIFF(25);
            mask    = MASK(25);
            tst_val = RUNNING_DIFF(26);
            mask    = MASK(26);
            tst_val = RUNNING_DIFF(27);
            mask    = MASK(27);
            tst_val = RUNNING_DIFF(28);
            mask    = MASK(28);
            tst_val = RUNNING_DIFF(29);
            mask    = MASK(29);
            tst_val = RUNNING_DIFF(30);
            mask    = MASK(30);
            tst_val = RUNNING_DIFF(31);
            mask    = MASK(31);
            tst_val = RUNNING_DIFF(32);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 33:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               mask    = MASK(23);
               tst_val = RUNNING_DIFF(24);
               mask    = MASK(24);
               tst_val = RUNNING_DIFF(25);
               mask    = MASK(25);
               tst_val = RUNNING_DIFF(26);
               mask    = MASK(26);
               tst_val = RUNNING_DIFF(27);
               mask    = MASK(27);
               tst_val = RUNNING_DIFF(28);
               mask    = MASK(28);
               tst_val = RUNNING_DIFF(29);
               mask    = MASK(29);
               tst_val = RUNNING_DIFF(30);
               mask    = MASK(30);
               tst_val = RUNNING_DIFF(31);
               mask    = MASK(31);
               tst_val = RUNNING_DIFF(32);
               mask    = MASK(32);
               tst_val = RUNNING_DIFF(33);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            mask    = MASK(23);
            tst_val = RUNNING_DIFF(24);
            mask    = MASK(24);
            tst_val = RUNNING_DIFF(25);
            mask    = MASK(25);
            tst_val = RUNNING_DIFF(26);
            mask    = MASK(26);
            tst_val = RUNNING_DIFF(27);
            mask    = MASK(27);
            tst_val = RUNNING_DIFF(28);
            mask    = MASK(28);
            tst_val = RUNNING_DIFF(29);
            mask    = MASK(29);
            tst_val = RUNNING_DIFF(30);
            mask    = MASK(30);
            tst_val = RUNNING_DIFF(31);
            mask    = MASK(31);
            tst_val = RUNNING_DIFF(32);
            mask    = MASK(32);
            tst_val = RUNNING_DIFF(33);
            if (tst_val >= 0) {
               break;
            }
         }
         break;

      case 34:
         while (seg_len > 32) {
            for (idx = first + stride -1 ; idx <= last; idx += stride) {
               tst_val = DIFF(1);
               mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
               tst_val = RUNNING_DIFF(2);
               mask    = MASK(2);
               tst_val = RUNNING_DIFF(3);
               mask    = MASK(3);
               tst_val = RUNNING_DIFF(4);
               mask    = MASK(4);
               tst_val = RUNNING_DIFF(5);
               mask    = MASK(5);
               tst_val = RUNNING_DIFF(6);
               mask    = MASK(6);
               tst_val = RUNNING_DIFF(7);
               mask    = MASK(7);
               tst_val = RUNNING_DIFF(8);
               mask    = MASK(8);
               tst_val = RUNNING_DIFF(9);
               mask    = MASK(9);
               tst_val = RUNNING_DIFF(10);
               mask    = MASK(10);
               tst_val = RUNNING_DIFF(11);
               mask    = MASK(11);
               tst_val = RUNNING_DIFF(12);
               mask    = MASK(12);
               tst_val = RUNNING_DIFF(13);
               mask    = MASK(13);
               tst_val = RUNNING_DIFF(14);
               mask    = MASK(14);
               tst_val = RUNNING_DIFF(15);
               mask    = MASK(15);
               tst_val = RUNNING_DIFF(16);
               mask    = MASK(16);
               tst_val = RUNNING_DIFF(17);
               mask    = MASK(17);
               tst_val = RUNNING_DIFF(18);
               mask    = MASK(18);
               tst_val = RUNNING_DIFF(19);
               mask    = MASK(19);
               tst_val = RUNNING_DIFF(20);
               mask    = MASK(20);
               tst_val = RUNNING_DIFF(21);
               mask    = MASK(21);
               tst_val = RUNNING_DIFF(22);
               mask    = MASK(22);
               tst_val = RUNNING_DIFF(23);
               mask    = MASK(23);
               tst_val = RUNNING_DIFF(24);
               mask    = MASK(24);
               tst_val = RUNNING_DIFF(25);
               mask    = MASK(25);
               tst_val = RUNNING_DIFF(26);
               mask    = MASK(26);
               tst_val = RUNNING_DIFF(27);
               mask    = MASK(27);
               tst_val = RUNNING_DIFF(28);
               mask    = MASK(28);
               tst_val = RUNNING_DIFF(29);
               mask    = MASK(29);
               tst_val = RUNNING_DIFF(30);
               mask    = MASK(30);
               tst_val = RUNNING_DIFF(31);
               mask    = MASK(31);
               tst_val = RUNNING_DIFF(32);
               mask    = MASK(32);
               tst_val = RUNNING_DIFF(33);
               mask    = MASK(33);
               tst_val = RUNNING_DIFF(34);
               if (tst_val >= 0) {
                  break;
               }
            }  /* for */
            if (tst_val == 0) {
                goto EXIT;
            }
            else if (tst_val > 0) {
               last = idx;
            }
            first   = idx - stride + 1;
            seg_len = last - first + 1;
            stride  = STRIDE_CALC(seg_len);
         }  /* end while seg_len > 32  */

         for (idx = first; idx <= last; idx += 1) {
            tst_val = DIFF(1);
            mask    = -(RIGHT_JUSTIFY_SIGN_BIT(DIFF(1) | -DIFF(1)));
            tst_val = RUNNING_DIFF(2);
            mask    = MASK(2);
            tst_val = RUNNING_DIFF(3);
            mask    = MASK(3);
            tst_val = RUNNING_DIFF(4);
            mask    = MASK(4);
            tst_val = RUNNING_DIFF(5);
            mask    = MASK(5);
            tst_val = RUNNING_DIFF(6);
            mask    = MASK(6);
            tst_val = RUNNING_DIFF(7);
            mask    = MASK(7);
            tst_val = RUNNING_DIFF(8);
            mask    = MASK(8);
            tst_val = RUNNING_DIFF(9);
            mask    = MASK(9);
            tst_val = RUNNING_DIFF(10);
            mask    = MASK(10);
            tst_val = RUNNING_DIFF(11);
            mask    = MASK(11);
            tst_val = RUNNING_DIFF(12);
            mask    = MASK(12);
            tst_val = RUNNING_DIFF(13);
            mask    = MASK(13);
            tst_val = RUNNING_DIFF(14);
            mask    = MASK(14);
            tst_val = RUNNING_DIFF(15);
            mask    = MASK(15);
            tst_val = RUNNING_DIFF(16);
            mask    = MASK(16);
            tst_val = RUNNING_DIFF(17);
            mask    = MASK(17);
            tst_val = RUNNING_DIFF(18);
            mask    = MASK(18);
            tst_val = RUNNING_DIFF(19);
            mask    = MASK(19);
            tst_val = RUNNING_DIFF(20);
            mask    = MASK(20);
            tst_val = RUNNING_DIFF(21);
            mask    = MASK(21);
            tst_val = RUNNING_DIFF(22);
            mask    = MASK(22);
            tst_val = RUNNING_DIFF(23);
            mask    = MASK(23);
            tst_val = RUNNING_DIFF(24);
            mask    = MASK(24);
            tst_val = RUNNING_DIFF(25);
            mask    = MASK(25);
            tst_val = RUNNING_DIFF(26);
            mask    = MASK(26);
            tst_val = RUNNING_DIFF(27);
            mask    = MASK(27);
            tst_val = RUNNING_DIFF(28);
            mask    = MASK(28);
            tst_val = RUNNING_DIFF(29);
            mask    = MASK(29);
            tst_val = RUNNING_DIFF(30);
            mask    = MASK(30);
            tst_val = RUNNING_DIFF(31);
            mask    = MASK(31);
            tst_val = RUNNING_DIFF(32);
            mask    = MASK(32);
            tst_val = RUNNING_DIFF(33);
            mask    = MASK(33);
            tst_val = RUNNING_DIFF(34);
            if (tst_val >= 0) {
               break;
            }
         }
         break;
# endif

      default:
         PRINTMSG(stmt_start_line, 196, Internal, 1,
                  "srch_name_tbl", 256);
         break;
   }  /* switch (id_len) */

EXIT:

   *name_idx = idx;

   TRACE (Func_Exit, "srch_name_tbl", NULL);

   return (tst_val);

}  /* srch_name_tbl */
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|*      Convert a host integer to a string to be used in printf.              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      the_constant : address of the constant to be "converted"	      *|
|*      type_idx     : the data type of the constant (index into Type Table)  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      result       : the_constant converted to a string (also returned as   *|
|*                     the result of this function)			      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      A character string version of the_constant (in result).		      *|
|*                                                                            *|
\******************************************************************************/

char *	 convert_cval_to_string (long64	*the_constant,
		  	         int	 type_idx,
			         char	*result)

{
   long_type	cn_val[MAX_WORDS_FOR_INTEGER];


   TRACE (Func_Entry, "convert_cval_to_string", NULL);

   C_TO_F_INT(cn_val, *the_constant, TYP_LINEAR(type_idx));

   result = convert_to_string(cn_val,
                              type_idx,
                              result);

   TRACE (Func_Exit, "convert_cval_to_string", NULL);

   return(result);

}  /* convert_cval_to_string */
