/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

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



static char USMID[] = "\n@(#)5.0_pl/sources/fold_drive.c	5.19	10/14/99 14:09:57\n";

# include <stdarg.h>
#ifdef KEY /* Bug 5554 */
# include "stdlib.h"
# include "errno.h"
#endif /* KEY Bug 5554 */
# include "defines.h"           /* Machine dependent ifdefs */
# include "host.m"              /* Host machine dependent macros.*/
# include "host.h"              /* Host machine dependent header.*/
# include "target.m"            /* Target machine dependent macros.*/
# include "target.h"            /* Target machine dependent header.*/
# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "s_globals.m"
# include "debug.m"
# include "fold_drive.m"
# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"
# include "fmath.h"
# include "arith.h"
# include "fold_drive.h"



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Compare a constant table entry with a "c" long value according to     *|
|*      the relational operator "opr".                                        *|
|*	NOTE:  This does not handle values bigger than a long on the host     *|
|*	       machine.  Call folder_driver directly for that.                *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
boolean	compare_cn_and_value(int	cn_idx,
#if defined(_HOST32) && defined(_TARGET64)
			     long long	value,
#else
			     long	value,
#endif
			     int	opr)

{
   long_type		result[MAX_WORDS_FOR_NUMERIC];
   long_type		right_value[MAX_WORDS_FOR_NUMERIC];
   boolean		is_true 				= FALSE;
   int			type_idx;


   TRACE (Func_Entry,"compare_cn_and_value" , NULL);

   C_TO_F_INT(right_value, value, CG_INTEGER_DEFAULT_TYPE);

   type_idx = CG_LOGICAL_DEFAULT_TYPE;

   if (folder_driver((char *)&CN_CONST(cn_idx),
                     CN_TYPE_IDX(cn_idx),
                     (char *)&right_value,
# ifdef _WHIRL_HOST64_TARGET64
                     Integer_8,
# else
                     CG_INTEGER_DEFAULT_TYPE,
# endif /* _WHIRL_HOST64_TARGET64 */
                     result,
                     &type_idx,
                     stmt_start_line,
                     stmt_start_col,
                     2,
                     opr)) {

      if (THIS_IS_TRUE(result, type_idx)) {
         is_true = TRUE;
      }
   }

   TRACE (Func_Exit, "compare_cn_and_value", NULL);

   return(is_true);

}  /* compare_cn_and_value */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine compares two character strings according to the f90 rules*|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	returns the TRUE_VALUE or FALSE_VALUE depending on the result of      *|
|*      the compare and the operation requested.                              *|
|*									      *|
\******************************************************************************/
static void f90_character_compare(char		*ch_ptr1,
			          long64	 len1,
                                  char		*ch_ptr2,
				  long64	 len2,
				  int     	 opr,
                                  long_type     *result,
                                  int		 type_idx)

{
   char			char1;
   char			char2;
   int			comp_result = 0;
   long64		i;


   TRACE (Func_Entry, "f90_character_compare", NULL);

   set_up_logical_constant(result, type_idx, FALSE_VALUE, FALSE);

   for (i = 0; i < (len1 > len2 ? len1 : len2); i++) {

      if (i < len1) {
         char1 = ch_ptr1[i];
      }
      else {
         char1 = ' ';
      }

      if (i < len2) {
         char2 = ch_ptr2[i];
      }
      else {
         char2 = ' ';
      }

      if (char1 == char2) {
         /* intentionally blank */
      }
      else if (char1 < char2) {
         comp_result = -1;
         break;
      }
      else if (char1 > char2) {
         comp_result = 1;
         break;
      }
   }
   

   switch (opr) {
   case Eq_Opr :

      if (comp_result == 0) {
         set_up_logical_constant(result, type_idx, TRUE_VALUE, FALSE);
      }
      break;

   case Ne_Opr :

      if (comp_result != 0) {
         set_up_logical_constant(result, type_idx, TRUE_VALUE, FALSE);
      }
      break;

   case Lt_Opr :

      if (comp_result < 0) {
         set_up_logical_constant(result, type_idx, TRUE_VALUE, FALSE);
      }
      break;

   case Le_Opr :

      if (comp_result <= 0) {
         set_up_logical_constant(result, type_idx, TRUE_VALUE, FALSE);
      }
      break;

   case Gt_Opr :

      if (comp_result > 0) {
         set_up_logical_constant(result, type_idx, TRUE_VALUE, FALSE);
      }
      break;

   case Ge_Opr :

      if (comp_result >= 0) {
         set_up_logical_constant(result, type_idx, TRUE_VALUE, FALSE);
      }
      break;

   }

   TRACE (Func_Exit, "f90_character_compare", NULL);

   return;

}  /* f90_character_compare */


#ifdef KEY /* Bug 12014 */
/*
 * After a call to ntr_const_tbl(), which may have reallocated the const_pool,
 * adjust the values of a pointer known to point into that pool. For
 * use inside folder_driver().
 * l_value_offset	offset of pointer inside const_pool
 * returns		new value of pointer inside const_pool
 */
#define CORRECT_THE_POINTER(l_value_offset) \
  (((char *)const_pool) + l_value_offset)
#endif /* KEY Bug 12014 */

#ifdef KEY /* Bug 12482 */
/*
 * Copy the value of a typeless or boz constant (e.g. b'1', o'7', or z'f')
 * from its source in the constant pool to a destination in a variable which
 * can later be passed to ntr_const_tbl() along with a desired type. For
 * unknown reasons, on a little-endian host, the data is divided into
 * words in big-endian fashion when it's typeless (e.g. z'1f2f3f4f5f6f7f8f'
 * in the source generates a word 0x1f2f3f4f followed by 0x5f6f7f8f) but must
 * be little-endian when passed to ntr_const_tbl. On a little-endian host, we
 * reverse the order of words; on a big-endian host, we leave the order alone
 * but insert padding at the beginning of the buffer or omit high-order words
 * as need be. Hopefully that achieves the right result on a big-endian or
 * cross-endian compiler--no chance to test so far.
 *
 * dst		Array of words to be passed to ntr_const_tbl() later
 * dst_words	Number of words in dst
 * src		Array of words from typeless constant
 * src_words	Number of words in src
 */
void
copy_and_pad_boz(long_type *dst, Uint dst_words, long_type *src, Uint src_words) {
  for (int i = 0; i < dst_words; i++) {
     dst[i] = 0;	/* Zero the entire result */
  }
  /* For type like integer*2, if the caller computed these based on the bit
   * size, it's a fraction of a word, so round up */
  src_words = (0 == src_words) ? 1 : src_words;
  dst_words = (0 == dst_words) ? 1 : dst_words;
  int start = (src_words > dst_words) ? (src_words - dst_words) : 0;
# if defined(_HOST_LITTLE_ENDIAN) && defined(_TARGET_LITTLE_ENDIAN)
  int reverse = src_words - 1;
  for (int i = start; i < src_words; i++) {
     dst[reverse - i] = src[i];
  }
# else
  int pad = dst_words - src_words;
  for (int i = start; i < src_words; i++) {
     dst[i + pad] = src[i];
  }
# endif
}
#endif /* KEY Bug 12482 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
#define FOLD_OP fold_operation__

extern void FOLD_OP(int *,
		    void *,
		    linear_type_type *,
		    void *,
		    linear_type_type *,
		    void *,
		    linear_type_type *,
		    void *,
		    linear_type_type *);

boolean folder_driver(char		*l_value_ptr,
		      int		 l_type_idx,
		      char		*r_value_ptr,
                      int		 r_type_idx,
		      long_type		*result,
		      int		*res_type_idx,
		      int		 line,
		      int		 col,
		      int		 num_args,
		      int          	 opr,
                      ...)

{
   struct value_entry     { long_type	  v[MAX_WORDS_FOR_NUMERIC]; };

   typedef struct         value_entry     value_type;

   struct big_value_entry { long_type     v[2000]; };

   typedef struct         big_value_entry big_value_type;

   boolean			ok 			= TRUE;
   long64			count;
   big_value_type		l_value;
   value_type			r_value;
   value_type			a3_value;
   value_type			a4_value;
   value_type			str_len1;
   value_type			str_len2;
   big_value_type		loc_result;
   long64			i;
   int				j;
   int				k;
   int				cn_idx;
   long64			length;
   long64			length_o;
   long64			length_d;
   linear_type_type		l_linear_type;
   linear_type_type		r_linear_type;
   linear_type_type		a3_linear_type;
#ifdef KEY /* Bug 10177 */
   linear_type_type		a4_linear_type = Err_Res;
#else /* KEY Bug 10177 */
   linear_type_type		a4_linear_type;
#endif /* KEY Bug 10177 */
   linear_type_type		res_linear_type;
   linear_type_type		str1_linear_type;
   linear_type_type		str2_linear_type;
   long_type			mask;
#ifdef KEY /* Bug 10177 */
   char			       *a3_value_ptr = 0;
   int				a3_type_idx = 0;
   char			       *a4_value_ptr = 0;
   int				a4_type_idx = 0;
#else /* KEY Bug 10177 */
   char			       *a3_value_ptr;
   int				a3_type_idx;
   char			       *a4_value_ptr;
   int				a4_type_idx;
#endif /* KEY Bug 10177 */
   va_list                      arg_ptr;
   char                         char_buf[8000];
   int				type_idx;
   int				char_idx;
   int				tmp_opr;
   char			       *char_ptr;
   long				arith_type;
   long				arith_type_l;
   AR_COMPARE_TYPE		comp_res;
   long64			char_len;


   TRACE (Func_Entry, "folder_driver", NULL);

   if (l_type_idx != NULL_IDX) {
      l_linear_type = TYP_LINEAR(l_type_idx);
   }

   res_linear_type = TYP_LINEAR(*res_type_idx);

   if (num_args > 1 && r_type_idx != NULL_IDX) {
      r_linear_type = TYP_LINEAR(r_type_idx);
   }

   if (num_args > 2) {
      va_start (arg_ptr, opr);
      a3_value_ptr = va_arg(arg_ptr, char *);
      a3_type_idx  = va_arg(arg_ptr, long);
      a4_value_ptr = va_arg(arg_ptr, char *);
      a4_type_idx  = va_arg(arg_ptr, long);
      va_end(arg_ptr);

      if (a3_type_idx != NULL_IDX) {
         a3_linear_type = TYP_LINEAR(a3_type_idx);
      }

      if (num_args == 4 &&
          a4_type_idx != NULL_IDX) {

         a4_linear_type = TYP_LINEAR(a4_type_idx);
      }
   }

   if ((opr == SRK_Opr) || 
       (opr == Transfer_Opr) || 
       (opr == Reshape_Opr)) {
      goto CONTINUE;
   }

   /* copy arguments to local variables so that addresses of */
   /* constant tbl entries can be sent.                      */

   if (TYP_TYPE(l_type_idx) == Typeless) {
      for (i = 0; 
           i < ((TYP_BIT_LEN(l_type_idx) + TARGET_BITS_PER_WORD - 1)/
                TARGET_BITS_PER_WORD); 
           i++) {
         l_value.v[i] = ((long_type *)l_value_ptr)[i];
      }
   }
   else if (TYP_TYPE(l_type_idx) != Character) {
      for (i = 0; i < num_host_wds[TYP_LINEAR(l_type_idx)]; i++) {
         l_value.v[i] = ((long_type *)l_value_ptr)[i];
      }

# ifdef _TARGET_OS_MAX
      if (l_linear_type == Complex_4) {
         /* we need to pack it up into one word */
         l_value.v[0] = l_value.v[0] << 32;
         l_value.v[0] = l_value.v[0] | (l_value.v[1] & 0xFFFFFFFF);
      }
# endif
   }
   else {  /* processing character data */
      char_ptr = (char *)l_value.v;
      l_value.v[0] = 0;

      for (i = 0; i < CN_INT_TO_C(TYP_IDX(l_type_idx)); i++) {
         char_ptr[i] = l_value_ptr[i];
      }

      for ( ; i < TARGET_BYTES_PER_WORD; i++) {
         char_ptr[i] = ' ';
      }
   }

   if (num_args > 1) {

      if (TYP_TYPE(r_type_idx) == Typeless) {

         for (i = 0; 
              i < ((TYP_BIT_LEN(r_type_idx) + TARGET_BITS_PER_WORD - 1)/
                   TARGET_BITS_PER_WORD); 
              i++) {

            r_value.v[i] = ((long_type *)r_value_ptr)[i];
         }
      }
      else if (TYP_TYPE(r_type_idx) != Character) {

         for (i = 0; i < num_host_wds[TYP_LINEAR(r_type_idx)]; i++) {
            r_value.v[i] = ((long_type *)r_value_ptr)[i];
         }

# ifdef _TARGET_OS_MAX
         if (r_linear_type == Complex_4) {
            /* we need to pack it up into one word */
            r_value.v[0] = r_value.v[0] << 32;
            r_value.v[0] = r_value.v[0] | (r_value.v[1] & 0xFFFFFFFF);
         }
# endif
      }
      else {
         char_ptr = (char *)r_value.v;
         r_value.v[0] = 0;

         for (i = 0; i < CN_INT_TO_C(TYP_IDX(r_type_idx)) &&
                     i < TARGET_BYTES_PER_WORD;
              i++) {
            char_ptr[i] = r_value_ptr[i];
         }

         for ( ; i < TARGET_BYTES_PER_WORD; i++) {
            char_ptr[i] = ' ';
         }
      }
   }

   if (num_args > 2) {

      if (TYP_TYPE(a3_type_idx) == Typeless) {

         for (i = 0; 
              i < ((TYP_BIT_LEN(a3_type_idx) + TARGET_BITS_PER_WORD - 1)/
                   TARGET_BITS_PER_WORD); 
              i++) {

            a3_value.v[i] = ((long_type *)a3_value_ptr)[i];
         }
      }
      else if (TYP_TYPE(a3_type_idx) != Character) {

         for (i = 0; i < num_host_wds[a3_linear_type]; i++) {
            a3_value.v[i] = ((long_type *)a3_value_ptr)[i];
         }

# ifdef _TARGET_OS_MAX
         if (a3_linear_type == Complex_4) {
            /* we need to pack it up into one word */
            a3_value.v[0] = a3_value.v[0] << 32;
            a3_value.v[0] = a3_value.v[0] | (a3_value.v[1] & 0xFFFFFFFF);
         }
# endif
      }
      else {
         char_ptr = (char *)a3_value.v;
         a3_value.v[0] = 0;

         for (i = 0; i < CN_INT_TO_C(TYP_IDX(a3_type_idx)) &&
                     i < TARGET_BYTES_PER_WORD;
              i++) {
            char_ptr[i] = a3_value_ptr[i];
         }

         for ( ; i < TARGET_BYTES_PER_WORD; i++) {
            char_ptr[i] = ' ';
         }
      }
   }

   if (num_args > 3) {

      if (TYP_TYPE(a4_type_idx) == Typeless) {

         for (i = 0;
              i < ((TYP_BIT_LEN(a4_type_idx) + TARGET_BITS_PER_WORD - 1)/
                   TARGET_BITS_PER_WORD);
              i++) {

            a4_value.v[i] = ((long_type *)a4_value_ptr)[i];
         }
      }
      else if (TYP_TYPE(a4_type_idx) != Character) {

         for (i = 0; i < num_host_wds[a4_linear_type]; i++) {
            a4_value.v[i] = ((long_type *)a4_value_ptr)[i];
         }

# ifdef _TARGET_OS_MAX
         if (a4_linear_type == Complex_4) {
            /* we need to pack it up into one word */
            a4_value.v[0] = a4_value.v[0] << 32;
            a4_value.v[0] = a4_value.v[0] | (a4_value.v[1] & 0xFFFFFFFF);
         }
# endif
      }
      else {
         char_ptr = (char *)a4_value.v;
         a4_value.v[0] = 0;

         for (i = 0; i < CN_INT_TO_C(TYP_IDX(a4_type_idx)) &&
                     i < TARGET_BYTES_PER_WORD;
              i++) {
            char_ptr[i] = a4_value_ptr[i];
         }

         for ( ; i < TARGET_BYTES_PER_WORD; i++) {
            char_ptr[i] = ' ';
         }
      }
   }


CONTINUE:

#ifdef KEY /* Bug 12014 */
   /*
    * The authors outsmarted themselves by using pointers instead of indices
    * as the formal arguments to function folder_driver(). If the calls to
    * ntr_const_tbl() below reallocate the const_pool, the pointers are no
    * longer correct. Rather than change every call to folder_driver() in the
    * entire front end, we correct the pointers as needed after each call to
    * ntr_const_tbl(). (Actually it seems that r_value_ptr is never used
    * after such a call, but we correct it for safety's sake and rely on opt
    * to discard dead code.)
    */
   {
   size_t l_value_offset = l_value_ptr - (char *) const_pool;
   size_t r_value_offset = r_value_ptr - (char *) const_pool;
#endif /* KEY Bug 12014 */
   switch (opr) {
      case Reshape_Opr :
         mask = AR_reshape((void *)result,
                           (const void *)l_value_ptr,
                           (const void *)r_value_ptr,
                           (const void *)a3_value_ptr,
                           (const void *)a4_value_ptr);

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);

         goto EXIT; /* goto exit because this returns a dope vector. */


      case Transfer_Opr :
         if (a3_value_ptr != NULL) {
            for (i = 0; i < num_host_wds[a3_linear_type]; i++) {
               a3_value.v[i] = ((long_type *)a3_value_ptr)[i];
            }
            SHIFT_ARITH_ARG(a3_value.v, a3_linear_type);

            mask = AR_transfer((void *)result,
                           (const void *)l_value_ptr,
                           (const void *)r_value_ptr,
                           (const AR_DATA *)a3_value.v,
                           (const AR_TYPE *)&linear_to_arith[a3_linear_type]);
         }
         else {
            mask = AR_transfer((void *)result,
                           (const void *)l_value_ptr,
                           (const void *)r_value_ptr,
                           (const AR_DATA *)a3_value_ptr,
                      (const AR_TYPE *)&linear_to_arith[INTEGER_DEFAULT_TYPE]);
         }

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);

         goto EXIT; /* goto exit because this returns a dope vector. */


      case Trim_Opr :
         /* Do this inline */
         /* return a cn table index in result[0] */
 
         i = CN_INT_TO_C(TYP_IDX(l_type_idx));
         while (i > 0 && l_value_ptr[i-1] == ' ') {
            i--;
         }
         
         char_len = i;

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX) = Character;
         TYP_LINEAR(TYP_WORK_IDX) = CHARACTER_DEFAULT_TYPE;
         TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX) = CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX) = C_INT_TO_CN(NULL_IDX, char_len);
         *res_type_idx = ntr_type_tbl();

         result[0] = ntr_const_tbl((*res_type_idx), TRUE, NULL);
#ifdef KEY /* Bug 12014 */
         l_value_ptr = CORRECT_THE_POINTER(l_value_offset);
         r_value_ptr = CORRECT_THE_POINTER(r_value_offset);
#endif /* KEY Bug 12014 */
         char_ptr = (char *) &CN_CONST(result[0]);

         for (i = 0; i < char_len; i++) {
            char_ptr[i] = l_value_ptr[i];
         }

         break;


      case Repeat_Opr :

         /* do this inline. */

         /* JEFFL BRIANJ - Do changes need to be made here? */

         length = CP_CONSTANT(CN_POOL_IDX(TYP_IDX(l_type_idx)) +
               num_host_wds[TYP_LINEAR(CN_TYPE_IDX(TYP_IDX(l_type_idx)))] - 1);

# if defined(_HOST_LITTLE_ENDIAN) && defined(_TARGET_LITTLE_ENDIAN)
         if (r_linear_type == Integer_8)
           count = *(long long *)(&r_value.v[0]);
         else
#endif 
         count = r_value.v[num_host_wds[r_linear_type] - 1];

         length = length * count;

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX) = Character;
         TYP_LINEAR(TYP_WORK_IDX) = CHARACTER_DEFAULT_TYPE;
         TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX) = CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, length),
         *res_type_idx = ntr_type_tbl();

         result[0] = ntr_const_tbl((*res_type_idx), TRUE, NULL);
#ifdef KEY /* Bug 12014 */
         l_value_ptr = CORRECT_THE_POINTER(l_value_offset);
         r_value_ptr = CORRECT_THE_POINTER(r_value_offset);
#endif /* KEY Bug 12014 */
         char_ptr = (char *) &CN_CONST(result[0]);

         length = CP_CONSTANT(CN_POOL_IDX(TYP_IDX(l_type_idx)) +
               num_host_wds[TYP_LINEAR(CN_TYPE_IDX(TYP_IDX(l_type_idx)))] - 1);
 
         char_idx = 0;
         for (k = 0; k < count; k++) {
            for (i = 0; i < length; i++) {
               char_ptr[char_idx] = l_value_ptr[i];
               char_idx++;
            }
         }
         break;


      case SRK_Opr :
         if (r_value_ptr == NULL) {

            for (i = 0; i < num_host_wds[l_linear_type]; i++) {
               l_value.v[i] = ((long_type *)l_value_ptr)[i];
            }

            if (l_linear_type != res_linear_type) {
               SHIFT_ARITH_ARG(l_value.v, l_linear_type);

# if defined(_USE_FOLD_DOT_f)
               tmp_opr = Cvrt_Opr;
               FOLD_OP(
                              &tmp_opr, 
                              &loc_result.v,
                              &res_linear_type,
                              &l_value.v,
                              &l_linear_type,
                              &r_value.v,
                              &r_linear_type, 
                              &a3_value.v,
                              &a3_linear_type);
# else
               mask = AR_convert((AR_DATA *)loc_result.v,
                            (const AR_TYPE *)&linear_to_arith[res_linear_type],
                            (const AR_DATA *)l_value.v,
                            (const AR_TYPE *)&linear_to_arith[l_linear_type]);
# endif

               ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
               SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

               for (i = 0; i < num_host_wds[res_linear_type]; i++) {
                  l_value.v[i] = loc_result.v[i];
               }
            }

            r_linear_type = Err_Res;

            SHIFT_ARITH_ARG(l_value.v, res_linear_type);

# if defined(_USE_FOLD_DOT_f)
            FOLD_OP(
                              &opr, 
                              &loc_result.v,
                              &res_linear_type,
                              &l_value.v,
                              &l_linear_type,
                              &r_value.v,
                              &r_linear_type, 
                              &a3_value.v,
                              &a3_linear_type);
# else
            mask = AR_selected_real_kind((AR_DATA *)loc_result.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)l_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)NULL,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type]);
# endif
   
         }
         else if (l_value_ptr == NULL) {
            for (i = 0; i < num_host_wds[r_linear_type]; i++) {
               r_value.v[i] = ((long_type *)r_value_ptr)[i];
            }

            if (r_linear_type != res_linear_type) { 
               SHIFT_ARITH_ARG(r_value.v, r_linear_type);

# if defined(_USE_FOLD_DOT_f)
               tmp_opr = Cvrt_Opr;
               FOLD_OP(
                              &tmp_opr, 
                              &loc_result.v,
                              &res_linear_type,
                              &r_value.v,
                              &r_linear_type,
                              &r_value.v,
                              &r_linear_type, 
                              &a3_value.v,
                              &a3_linear_type);
# else
               mask = AR_convert((AR_DATA *)loc_result.v,
                            (const AR_TYPE *)&linear_to_arith[res_linear_type],
                            (const AR_DATA *)r_value.v,
                            (const AR_TYPE *)&linear_to_arith[r_linear_type]);
# endif

               ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
               SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

               for (i = 0; i < num_host_wds[res_linear_type]; i++) {
                  r_value.v[i] = loc_result.v[i];
               }
            }

            l_linear_type = Err_Res;

            SHIFT_ARITH_ARG(r_value.v, res_linear_type);

# if defined(_USE_FOLD_DOT_f)
            FOLD_OP(
                              &opr, 
                              &loc_result.v,
                              &res_linear_type,
                              &l_value.v,
                              &l_linear_type,
                              &r_value.v,
                              &r_linear_type, 
                              &a3_value.v,
                              &a3_linear_type);
# else
            mask = AR_selected_real_kind((AR_DATA *)loc_result.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)NULL,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)r_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type]);
# endif
         }
         else {

            for (i = 0; i < num_host_wds[l_linear_type]; i++) {
               l_value.v[i] = ((long_type *)l_value_ptr)[i];
            }

            if (l_linear_type != res_linear_type) { 
               SHIFT_ARITH_ARG(l_value.v, l_linear_type);

# if defined(_USE_FOLD_DOT_f)
               tmp_opr = Cvrt_Opr;
               FOLD_OP(
                              &tmp_opr,  
                              &loc_result.v,
                              &res_linear_type,
                              &l_value.v,
                              &l_linear_type,
                              &r_value.v,
                              &r_linear_type, 
                              &a3_value.v,
                              &a3_linear_type);
# else
               mask = AR_convert((AR_DATA *)loc_result.v,
                            (const AR_TYPE *)&linear_to_arith[res_linear_type],
                            (const AR_DATA *)l_value.v,
                            (const AR_TYPE *)&linear_to_arith[l_linear_type]);
# endif

               ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
               SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

               for (i = 0; i < num_host_wds[res_linear_type]; i++) {
                  l_value.v[i] = loc_result.v[i];
               }
            }

            SHIFT_ARITH_ARG(l_value.v, res_linear_type);

            for (i = 0; i < num_host_wds[r_linear_type]; i++) {
               r_value.v[i] = ((long_type *)r_value_ptr)[i];
            }

            if (r_linear_type != res_linear_type) { 
               SHIFT_ARITH_ARG(r_value.v, r_linear_type);

# if defined(_USE_FOLD_DOT_f)
               tmp_opr = Cvrt_Opr;
               FOLD_OP(
                              &tmp_opr, 
                              &loc_result.v,
                              &res_linear_type,
                              &r_value.v,
                              &r_linear_type,
                              &r_value.v,
                              &r_linear_type, 
                              &a3_value.v,
                              &a3_linear_type);
# else
               mask = AR_convert((AR_DATA *)loc_result.v,
                            (const AR_TYPE *)&linear_to_arith[res_linear_type],
                            (const AR_DATA *)r_value.v,
                            (const AR_TYPE *)&linear_to_arith[r_linear_type]);
# endif

               ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
               SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

               for (i = 0; i < num_host_wds[res_linear_type]; i++) {
                  r_value.v[i] = loc_result.v[i];
               }
            }

            SHIFT_ARITH_ARG(r_value.v, res_linear_type);

# if defined(_USE_FOLD_DOT_f)
            FOLD_OP(
                              &opr, 
                              &loc_result.v,
                              &res_linear_type,
                              &l_value.v,
                              &l_linear_type,
                              &r_value.v,
                              &r_linear_type, 
                              &a3_value.v,
                              &a3_linear_type);
# else
            mask = AR_selected_real_kind((AR_DATA *)loc_result.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)l_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)r_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type]);
# endif
         }

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }

         break;


      case SIK_Opr :
         cn_idx = ntr_const_tbl(l_type_idx, FALSE, &l_value.v[0]);
#ifdef KEY /* Bug 12014 */
         l_value_ptr = CORRECT_THE_POINTER(l_value_offset);
         r_value_ptr = CORRECT_THE_POINTER(r_value_offset);
#endif /* KEY Bug 12014 */

         if (compare_cn_and_value(cn_idx, RANGE_INT1_F90, Le_Opr)) {
            i = 1;
         }
         else if (compare_cn_and_value(cn_idx, RANGE_INT2_F90, Le_Opr)) {
            i = 2;
         }
         else if (compare_cn_and_value(cn_idx, RANGE_INT4_F90, Le_Opr)) {
            i = 4;
         }
         else if (compare_cn_and_value(cn_idx, RANGE_INT8_F90, Le_Opr)) {
            i = 8;
         }
         else {
            i = -1;
         }

         C_TO_F_INT(result, i, res_linear_type);

         break;


      case Uminus_Opr :
         if (l_linear_type != res_linear_type &&
             TYP_TYPE(l_type_idx) != Typeless) {

            SHIFT_ARITH_ARG(l_value.v, l_linear_type);

# if defined(_USE_FOLD_DOT_f)            
            tmp_opr = Cvrt_Opr;
            FOLD_OP(
                           &tmp_opr,  
                           &loc_result.v,
                           &res_linear_type,
                           &l_value.v,
                           &l_linear_type,
                           &r_value.v,
                           &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else
            mask = AR_convert((AR_DATA *)loc_result.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const AR_DATA *)l_value.v,
                         (const AR_TYPE *)&linear_to_arith[l_linear_type]);
# endif

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               l_value.v[i] = loc_result.v[i];
            }
         }

         SHIFT_ARITH_ARG(l_value.v, res_linear_type);

# if defined(_USE_FOLD_DOT_f)
         FOLD_OP(
                        &opr,   
                        &loc_result.v,
                        &res_linear_type,
                        &l_value.v,
                        &res_linear_type,
                        &r_value.v,
                        &res_linear_type, 
                        &a3_value.v,
                        &a3_linear_type);
# else
         mask = AR_negate((AR_DATA *)loc_result.v,
                      (const AR_TYPE *)&linear_to_arith[res_linear_type],
                      (const AR_DATA *)l_value.v,
                      (const AR_TYPE *)&linear_to_arith[res_linear_type]);
# endif

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
         break;


      case Cvrt_Opr :
      case Int_Opr :
         if (TYP_TYPE(l_type_idx) == Character) {  /* source */
            length_o = CN_INT_TO_C(TYP_IDX(l_type_idx));

            if (TYP_TYPE((*res_type_idx)) == Character) {  /* destination */
               length_d = CN_INT_TO_C(TYP_IDX((*res_type_idx)));
            }
            else {
               length_d = num_host_wds[TYP_LINEAR((*res_type_idx))] *
                                     TARGET_CHARS_PER_WORD;
            }

            char_ptr = (char *) result;
            l_value_ptr = (char *) &l_value.v;

            for (i = 0; i < length_o; i++) {
               char_ptr[i] = l_value_ptr[i];
            }

            for (j = i; j < length_d; j++) {
               char_ptr[j] = ' ';
            }

            break;
         }

         if (TYP_TYPE(l_type_idx) == Logical &&
             TYP_TYPE((*res_type_idx)) == Logical) {

# if defined(_TARGET_OS_SOLARIS) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            if (l_linear_type == Logical_8 &&
                (res_linear_type == Logical_1 ||
                 res_linear_type == Logical_2 ||
                 res_linear_type == Logical_4)) {

# if defined(_HOST_LITTLE_ENDIAN) && defined(_TARGET_LITTLE_ENDIAN)
               *(long *)result = *(long long *)(l_value.v);
# else
               result[0] = l_value.v[1];
# endif
            }
            else if (res_linear_type == Logical_8 &&
                     (l_linear_type == Logical_1 ||
                      l_linear_type == Logical_2 ||
                      l_linear_type == Logical_4)) {

# if defined(_HOST_LITTLE_ENDIAN) && defined(_TARGET_LITTLE_ENDIAN)
               *(long long *)result = *(long *)(l_value.v);
# else
               result[0] = 0;
               result[1] = l_value.v[0];
# endif
            }
            else {
               result[0] = l_value.v[0];
               result[1] = l_value.v[1];
            }
# else
            result[0] = l_value.v[0];
# endif
            break;
         }

         if (TYP_TYPE(l_type_idx) == Typeless) {   /* source */
            for (i = 0;i < (TYP_BIT_LEN(l_type_idx)/TARGET_BITS_PER_WORD);i++) {
               result[i] = l_value.v[i];
            }
            break;
         }

         if (l_linear_type == res_linear_type) {
            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               result[i] = l_value.v[i];
            }
            break;
         }

# if defined(_USE_FOLD_DOT_f)
         tmp_opr = Cvrt_Opr;
         FOLD_OP(
                        &tmp_opr,  
                        &loc_result.v,
                        &res_linear_type,
                        &l_value.v,
                        &l_linear_type,
                        &r_value.v,
                        &r_linear_type, 
                        &a3_value.v,
                        &a3_linear_type);
# else 
         arith_type = linear_to_arith[l_linear_type];

         if ((TYP_TYPE(l_type_idx) == Real ||
              TYP_TYPE(l_type_idx) == Complex) &&
              TYP_TYPE((*res_type_idx)) == Integer) {

            /* this needs to be truncation, not rounding */

            switch(linear_to_arith[l_linear_type]) {
               case AR_Float_IEEE_NR_32 :
                  arith_type = AR_Float_IEEE_ZE_32;
                  break;

               case AR_Float_IEEE_NR_64 :
                  arith_type = AR_Float_IEEE_ZE_64;
                  break;

               case AR_Float_IEEE_NR_128 :
                  arith_type = AR_Float_IEEE_ZE_128;
                  break;

               case AR_Complex_IEEE_NR_32 :
                  arith_type = AR_Complex_IEEE_ZE_32;
                  break;

               case AR_Complex_IEEE_NR_64 :
                  arith_type = AR_Complex_IEEE_ZE_64;
                  break;

               case AR_Complex_IEEE_NR_128 :
                  arith_type = AR_Complex_IEEE_ZE_128;
                  break;

            }
         }

         SHIFT_ARITH_ARG(l_value.v, l_linear_type);

         mask = AR_convert((AR_DATA *)loc_result.v,
                      (const AR_TYPE *)&linear_to_arith[res_linear_type],
                      (const AR_DATA *)l_value.v,
                      (const AR_TYPE *)&arith_type);
# endif

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
         break;


      case Cvrt_Unsigned_Opr :
         SHIFT_ARITH_ARG(l_value.v, l_linear_type);

         arith_type = linear_to_arith[res_linear_type];
         arith_type_l = linear_to_arith[l_linear_type];

# if defined(_TARGET_OS_MAX) || defined(_TARGET_OS_SOLARIS) ||  \
     (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

         if (TYP_TYPE((*res_type_idx)) == Integer) {
            arith_type = input_arith_type[res_linear_type];
         }

         if (arith_type_l == AR_Int_32_S) {
            arith_type_l = AR_Int_32_U;
         }
         else if (arith_type_l == AR_Int_64_S) {
            arith_type_l = AR_Int_64_U;
         }
# endif

# if defined(_USE_FOLD_DOT_f)
         tmp_opr = Cvrt_Opr;
         FOLD_OP(
                        &tmp_opr,   
                        &loc_result.v,
                        &res_linear_type,
                        &l_value.v,
                        &l_linear_type,
                        &r_value.v,
                        &r_linear_type, 
                        &a3_value.v,
                        &a3_linear_type);
# else
         mask = AR_convert((AR_DATA *)loc_result.v,
                      (const AR_TYPE *)&arith_type,
                      (const AR_DATA *)l_value.v,
                      (const AR_TYPE *)&arith_type_l);
# endif

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
         break;


      case Power_Opr :
         if (l_linear_type != res_linear_type &&
             TYP_TYPE(l_type_idx) != Typeless) {

            SHIFT_ARITH_ARG(l_value.v, l_linear_type);

# if defined(_USE_FOLD_DOT_f)
            tmp_opr = Cvrt_Opr;
            FOLD_OP(
                           &tmp_opr, 
                           &loc_result.v,
                           &res_linear_type,
                           &l_value.v,
                           &l_linear_type,
                           &r_value.v,
                           &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else
            mask = AR_convert((AR_DATA *)loc_result.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const AR_DATA *)l_value.v,
                         (const AR_TYPE *)&linear_to_arith[l_linear_type]);
# endif
            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               l_value.v[i] = loc_result.v[i];
            }
         }

         if (r_linear_type != res_linear_type &&
             TYP_TYPE(r_type_idx) == Integer &&
             TYP_TYPE((*res_type_idx)) == Integer) {

            SHIFT_ARITH_ARG(r_value.v, r_linear_type);

# if defined(_USE_FOLD_DOT_f)
            tmp_opr = Cvrt_Opr;
            FOLD_OP(
                           &tmp_opr,   
                           &loc_result.v,
                           &res_linear_type,
                           &r_value.v,
                           &r_linear_type,
                           &r_value.v,
                           &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else
            mask = AR_convert((AR_DATA *)loc_result.v,
                            (const AR_TYPE *)&linear_to_arith[res_linear_type],
                            (const AR_DATA *)r_value.v,
                            (const AR_TYPE *)&linear_to_arith[r_linear_type]);
# endif

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               r_value.v[i] = loc_result.v[i];
            }

            r_linear_type = res_linear_type;
         }
         else if (r_linear_type != res_linear_type &&
                  TYP_TYPE(r_type_idx) != Integer &&
                  TYP_TYPE(r_type_idx) != Typeless) {

            SHIFT_ARITH_ARG(r_value.v, r_linear_type);

# if defined(_USE_FOLD_DOT_f)
            tmp_opr = Cvrt_Opr;
            FOLD_OP(
                           &tmp_opr,  
                           &loc_result.v,
                           &res_linear_type,
                           &r_value.v,
                           &r_linear_type,
                           &r_value.v,
                           &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else
            mask = AR_convert((AR_DATA *)loc_result.v,
                            (const AR_TYPE *)&linear_to_arith[res_linear_type],
                            (const AR_DATA *)r_value.v,
                            (const AR_TYPE *)&linear_to_arith[r_linear_type]);
# endif

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               r_value.v[i] = loc_result.v[i];
            }

            r_linear_type = res_linear_type;
         }

         SHIFT_ARITH_ARG(l_value.v, res_linear_type);
         SHIFT_ARITH_ARG(r_value.v, r_linear_type);

# if defined(_USE_FOLD_DOT_f)
         FOLD_OP(
                        &opr,   
                        &loc_result.v,
                        &res_linear_type,
                        &l_value.v,
                        &res_linear_type,
                        &r_value.v,
                        &r_linear_type, 
                        &a3_value.v,
                        &a3_linear_type);
# else
         mask = AR_power((AR_DATA *)loc_result.v,
                     (const AR_TYPE *)&linear_to_arith[res_linear_type],
                     (const AR_DATA *)l_value.v,
                     (const AR_TYPE *)&linear_to_arith[res_linear_type],
                     (const AR_DATA *)r_value.v,
                     (const AR_TYPE *)&linear_to_arith[r_linear_type]);
# endif

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
         break;


      case Mult_Opr :
      case Div_Opr  :
      case Real_Div_To_Int_Opr:
      case Minus_Opr :
      case Plus_Opr :
      case Mod_Opr :
      case Modulo_Opr :
         if (l_linear_type != res_linear_type &&
             TYP_TYPE(l_type_idx) != Typeless) {

            SHIFT_ARITH_ARG(l_value.v, l_linear_type);

# if defined(_USE_FOLD_DOT_f)            
            tmp_opr = Cvrt_Opr;
            FOLD_OP(
                   	   &tmp_opr,  
                           &loc_result.v,
             	           &res_linear_type,
                   	   &l_value.v,
                   	   &l_linear_type,
                   	   &r_value.v,
                   	   &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else 
            mask = AR_convert((AR_DATA *)loc_result.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const AR_DATA *)l_value.v,
                         (const AR_TYPE *)&linear_to_arith[l_linear_type]);
# endif

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               l_value.v[i] = loc_result.v[i];
            }
         }

         if (r_linear_type != res_linear_type &&
             TYP_TYPE(r_type_idx) != Typeless) {

            SHIFT_ARITH_ARG(r_value.v, r_linear_type);

# if defined(_USE_FOLD_DOT_f)            
            tmp_opr = Cvrt_Opr;
            FOLD_OP(
                   	   &tmp_opr,  
                           &loc_result.v,
               		   &res_linear_type,
                   	   &r_value.v,
                   	   &r_linear_type,
                   	   &r_value.v,
                   	   &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else 
             mask = AR_convert((AR_DATA *)loc_result.v,
                            (const AR_TYPE *)&linear_to_arith[res_linear_type],
                            (const AR_DATA *)r_value.v,
                            (const AR_TYPE *)&linear_to_arith[r_linear_type]);
# endif

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               r_value.v[i] = loc_result.v[i];
            }
         }

         SHIFT_ARITH_ARG(l_value.v, res_linear_type);
         SHIFT_ARITH_ARG(r_value.v, res_linear_type);

# if defined(_USE_FOLD_DOT_f)            
         FOLD_OP(
                   	&opr,  
                        &loc_result.v,
               		&res_linear_type,
                   	&l_value.v,
                   	&res_linear_type,
                   	&r_value.v,
                   	&res_linear_type, 
                        &a3_value.v,
                        &a3_linear_type);
# else 
         switch (opr) {
         case Mult_Opr:
            mask = AR_multiply((AR_DATA *)loc_result.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)l_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)r_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type]);
            break;

         case Div_Opr  :
            mask = AR_divide((AR_DATA *)loc_result.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)l_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)r_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type]);
            break;

         case Real_Div_To_Int_Opr  :
            mask = AR_divide((AR_DATA *)loc_result.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)l_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)r_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type]);

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               r_value.v[i] = loc_result.v[i];
            }

            mask = AR_round_int_div((AR_DATA *)loc_result.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)r_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type]);
            break;

         case Minus_Opr :
            mask = AR_subtract((AR_DATA *)loc_result.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)l_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)r_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type]);
            break;

         case Plus_Opr :
            mask = AR_add((AR_DATA *)loc_result.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)l_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type],
                        (const AR_DATA *)r_value.v,
                        (const AR_TYPE *)&linear_to_arith[res_linear_type]);
            break;

         case Modulo_Opr :
            mask = AR_Modulo((AR_DATA *)loc_result.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)l_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)r_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type]);
            break;

         case Mod_Opr :
            mask = AR_mod((AR_DATA *)loc_result.v,
                     (const AR_TYPE *)&linear_to_arith[res_linear_type],
                     (const AR_DATA *)l_value.v,
                     (const AR_TYPE *)&linear_to_arith[res_linear_type],
                     (const AR_DATA *)r_value.v,
                     (const AR_TYPE *)&linear_to_arith[res_linear_type]);
            break;

         }
# endif

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
         break;


      case Eq_Opr :
      case Ne_Opr :
      case Lt_Opr :
      case Le_Opr :
      case Gt_Opr :
      case Ge_Opr :
         if (TYP_TYPE(l_type_idx) == Character &&
             TYP_TYPE(r_type_idx) == Character) {
            f90_character_compare(l_value_ptr, 
                                  CN_INT_TO_C(TYP_IDX(l_type_idx)),
                                  r_value_ptr,
                                  CN_INT_TO_C(TYP_IDX(r_type_idx)),
                                  opr,
                                  result,
                                  (*res_type_idx));
         }
         else {
            res_linear_type = (linear_type_type)
                              bin_add_tbl[l_linear_type][r_linear_type].type;

            if (l_linear_type != res_linear_type &&
                TYP_TYPE(l_type_idx) != Typeless) {

               SHIFT_ARITH_ARG(l_value.v, l_linear_type);

# if defined(_USE_FOLD_DOT_f)
               tmp_opr = Cvrt_Opr;
               FOLD_OP(
                              &tmp_opr,  
                              &loc_result.v,
                              &res_linear_type,
                              &l_value.v,
                              &l_linear_type,
                              &r_value.v,
                              &r_linear_type, 
                              &a3_value.v,
                              &a3_linear_type);
# else
                mask = AR_convert((AR_DATA *)loc_result.v,
                            (const AR_TYPE *)&linear_to_arith[res_linear_type],
                            (const AR_DATA *)l_value.v,
                            (const AR_TYPE *)&linear_to_arith[l_linear_type]);
# endif

               ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
               SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

               for (i = 0; i < num_host_wds[res_linear_type]; i++) {
                  l_value.v[i] = loc_result.v[i];
               }
            }

            if (r_linear_type != res_linear_type &&
                TYP_TYPE(r_type_idx) != Typeless) {

               SHIFT_ARITH_ARG(r_value.v, r_linear_type);
   
# if defined(_USE_FOLD_DOT_f)
               tmp_opr = Cvrt_Opr;
               FOLD_OP(
                              &tmp_opr,  
                              &loc_result.v,
                              &res_linear_type,
                              &r_value.v,
                              &r_linear_type,
                              &r_value.v,
                              &r_linear_type, 
                              &a3_value.v,
                              &a3_linear_type);
# else
               mask = AR_convert((AR_DATA *)loc_result.v,
                           (const AR_TYPE *)&linear_to_arith[res_linear_type],
                           (const AR_DATA *)r_value.v,
                           (const AR_TYPE *)&linear_to_arith[r_linear_type]);
# endif

               ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
               SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

               for (i = 0; i < num_host_wds[res_linear_type]; i++) {
                  r_value.v[i] = loc_result.v[i];
               }
            }

# if defined(_USE_FOLD_DOT_f)
            FOLD_OP(
                           &opr,  
                           &loc_result.v,
                           &res_linear_type,
                           &l_value.v,
                           &res_linear_type,
                           &r_value.v,
                           &res_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);

            if (loc_result.v[0] == 0) {
               set_up_logical_constant(result, 
                                       (*res_type_idx), 
                                       FALSE_VALUE, 
                                       FALSE);
            }
            else {
               set_up_logical_constant(result, 
                                       (*res_type_idx), 
                                       TRUE_VALUE, 
                                       FALSE);
            }
# else
            SHIFT_ARITH_ARG(l_value.v, res_linear_type);
            SHIFT_ARITH_ARG(r_value.v, res_linear_type);

            comp_res = AR_compare((const AR_DATA *)l_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)r_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type]);

            switch (opr) {
            case Eq_Opr :
               if (comp_res == AR_Compare_EQ) {
                  set_up_logical_constant(result, (*res_type_idx), TRUE_VALUE, 
                                          FALSE);
               }
               else {
                  set_up_logical_constant(result, (*res_type_idx), FALSE_VALUE, 
                                          FALSE);
               }
               break;

            case Ne_Opr :
               if (comp_res != AR_Compare_EQ) {
                  set_up_logical_constant(result, (*res_type_idx), TRUE_VALUE,
                                          FALSE);
               }
               else {
                  set_up_logical_constant(result, (*res_type_idx), FALSE_VALUE,
                                          FALSE);
               }
               break;

            case Lt_Opr :
               if (comp_res == AR_Compare_LT) {
                  set_up_logical_constant(result, (*res_type_idx), TRUE_VALUE,
                                          FALSE);
               }
               else {
                  set_up_logical_constant(result, (*res_type_idx), FALSE_VALUE,
                                          FALSE);
               }
               break;

            case Le_Opr :
               if (comp_res != AR_Compare_GT) {
                  set_up_logical_constant(result, (*res_type_idx), TRUE_VALUE,
                                          FALSE);
               }
               else {
                  set_up_logical_constant(result, (*res_type_idx), FALSE_VALUE,
                                          FALSE);
               }
               break;

            case Gt_Opr :
               if (comp_res == AR_Compare_GT) {
                  set_up_logical_constant(result, (*res_type_idx), TRUE_VALUE,
                                          FALSE);
               }
               else {
                  set_up_logical_constant(result, (*res_type_idx), FALSE_VALUE,
                                          FALSE);
               }
               break;

            case Ge_Opr :
               if (comp_res != AR_Compare_LT) {
                  set_up_logical_constant(result, (*res_type_idx), TRUE_VALUE,
                                          FALSE);
               }
               else {
                  set_up_logical_constant(result, (*res_type_idx), FALSE_VALUE,
                                          FALSE);
               }
               break;
            }
# endif

            res_linear_type = TYP_LINEAR(*res_type_idx);
         }
         break;


      case Not_Opr :
         if (THIS_IS_TRUE(l_value.v, l_type_idx)) {
            set_up_logical_constant(result, (*res_type_idx), FALSE_VALUE,FALSE);
         }
         else {
            set_up_logical_constant(result, (*res_type_idx), TRUE_VALUE, FALSE);
         }
         break;


      case And_Opr  :
         if (THIS_IS_TRUE(l_value.v, l_type_idx) &&
             THIS_IS_TRUE(r_value.v, r_type_idx)) {
            set_up_logical_constant(result, (*res_type_idx), TRUE_VALUE, FALSE);
         }
         else {
            set_up_logical_constant(result, (*res_type_idx), FALSE_VALUE,FALSE);
         }
         break;


      case Or_Opr   :
         if (THIS_IS_TRUE(l_value.v, l_type_idx) ||
             THIS_IS_TRUE(r_value.v, r_type_idx)) {
            set_up_logical_constant(result, (*res_type_idx), TRUE_VALUE, FALSE);
         }
         else {
            set_up_logical_constant(result, (*res_type_idx), FALSE_VALUE,FALSE);
         }
         break;


      case Eqv_Opr  :
         if ((THIS_IS_TRUE(l_value.v, l_type_idx)) == 
                 (THIS_IS_TRUE(r_value.v, r_type_idx))) {
            set_up_logical_constant(result, (*res_type_idx), TRUE_VALUE, FALSE);
         }
         else {
            set_up_logical_constant(result, (*res_type_idx), FALSE_VALUE,FALSE);
         }
         break;


      case Neqv_Opr :
         if ((THIS_IS_TRUE(l_value.v, l_type_idx)) != 
                 (THIS_IS_TRUE(r_value.v, r_type_idx))) {
            set_up_logical_constant(result, (*res_type_idx), TRUE_VALUE, FALSE);
         }
         else {
            set_up_logical_constant(result, (*res_type_idx), FALSE_VALUE,FALSE);
         }
         break;


      case Bnot_Opr :
         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = ~l_value.v[i];
         }
         break;


      case Band_Opr  :
         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = l_value.v[i] & r_value.v[i];
         }
         break;


      case Bor_Opr   :
         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = l_value.v[i] | r_value.v[i];
         }
         break;


      case Bneqv_Opr :
         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = l_value.v[i] ^ r_value.v[i];
         }
         break;


      case Beqv_Opr :
         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = ~(l_value.v[i] ^ r_value.v[i]);
         }
         break;


# if defined(_USE_FOLD_DOT_f)
      case Sqrt_Opr :
         SHIFT_ARITH_ARG(l_value.v, res_linear_type);

         FOLD_OP(
                        &opr, 
                        &loc_result.v,
                        &res_linear_type,
                        &l_value.v,
                        &res_linear_type,
                        &r_value.v,
                        &res_linear_type, 
                        &a3_value.v,
                        &a3_linear_type);

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
         break;
# endif


      case Abs_Opr :
         SHIFT_ARITH_ARG(l_value.v, res_linear_type);

# if defined(_USE_FOLD_DOT_f)
         FOLD_OP(
                        &opr, 
                        &loc_result.v,
                        &res_linear_type,
                        &l_value.v,
                        &res_linear_type,
                        &r_value.v,
                        &res_linear_type, 
                        &a3_value.v,
                        &a3_linear_type);
# else
         mask = AR_abs((AR_DATA *)loc_result.v,
                      (const AR_TYPE *)&linear_to_arith[res_linear_type],
                      (const AR_DATA *)l_value.v,
                      (const AR_TYPE *)&linear_to_arith[res_linear_type]);
# endif

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
         break;


      case Nint_Opr :
# if defined(_USE_FOLD_DOT_f)
         FOLD_OP(
                        &opr, 
                        &loc_result.v,
                        &res_linear_type,
                        &l_value.v,
                        &l_linear_type,
                        &r_value.v,
                        &r_linear_type, 
                        &a3_value.v,
                        &a3_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
# else 
         strcpy(char_buf, "0.5");
         mask = AR_convert_str_to_float((AR_DATA *)a3_value.v,
                          (const AR_TYPE *)&input_arith_type[l_linear_type],
                          (const char *)char_buf);
         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         SHIFT_ARITH_RESULT(a3_value.v, l_linear_type);

         type_idx = CG_LOGICAL_DEFAULT_TYPE;

         ok &= folder_driver((char *)l_value.v,
                           l_type_idx,
                           (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                           CG_INTEGER_DEFAULT_TYPE,
                           a4_value.v,
                           &type_idx,
                           line,
                           col,
                           2,
                           Le_Opr);

         if (THIS_IS_TRUE(a4_value.v,type_idx)) {
            type_idx = l_type_idx;
            ok &= folder_driver((char *)l_value.v,
                               l_type_idx,
                               (char *)a3_value.v,
                               l_type_idx,
                               a4_value.v,
                               &type_idx,
                               line,
                               col,
                               2,
                               Minus_Opr);
         }
         else {
            type_idx = l_type_idx;
            ok &= folder_driver((char *)l_value.v,
                               l_type_idx,
                               (char *)a3_value.v,
                               l_type_idx,
                               a4_value.v,
                               &type_idx,
                               line,
                               col,
                               2,
                               Plus_Opr);
         }

         ok &= folder_driver((char *)a4_value.v,
                             l_type_idx,
                             NULL,
                             NULL_IDX,
                             result,
                             res_type_idx,
                             line,
                             col,
                             1,
                             Int_Opr);
# endif
         break;


      case Sign_Opr :
         SHIFT_ARITH_ARG(l_value.v, res_linear_type);

# if defined(_USE_FOLD_DOT_f)
         tmp_opr = Abs_Opr;
         FOLD_OP(
                        &tmp_opr,   
                        &a3_value.v,
                        &res_linear_type,
                        &l_value.v,
                        &res_linear_type,
                        &r_value.v,
                        &r_linear_type, 
                        &a3_value.v,
                        &a3_linear_type);
# else
         mask = AR_abs((AR_DATA *)a3_value.v,
                      (const AR_TYPE *)&linear_to_arith[res_linear_type],
                      (const AR_DATA *)l_value.v,
                      (const AR_TYPE *)&linear_to_arith[res_linear_type]);
# endif

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         SHIFT_ARITH_RESULT(a3_value.v, res_linear_type);

         type_idx = CG_LOGICAL_DEFAULT_TYPE;

         ok &= folder_driver((char *)r_value.v,
                           r_type_idx,
                           (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                           CG_INTEGER_DEFAULT_TYPE,
                           a4_value.v,
                           &type_idx,
                           line,
                           col,
                           2,
                           Lt_Opr);

         if (THIS_IS_TRUE(a4_value.v, type_idx)) {
            /* negate the result */
            SHIFT_ARITH_ARG(a3_value.v, res_linear_type);

# if defined(_USE_FOLD_DOT_f)
            tmp_opr = Uminus_Opr;
            FOLD_OP(
                           &tmp_opr,  
                           &loc_result.v,
                           &res_linear_type,
                           &a3_value.v,
                           &res_linear_type,
                           &r_value.v,
                           &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else
            mask = AR_negate((AR_DATA *)loc_result.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const AR_DATA *)a3_value.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type]);
# endif
   
            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               result[i] = loc_result.v[i];
            }
         }
         else {
            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               result[i] = a3_value.v[i];
            }
         }
         break;




      case Shift_Opr :
         /* to behave like the runtime shift, we must calculate */
         /* mod(r_value.v, TARGET_BITS_PER_WORD) to use as shift count */

# if defined(_USE_FOLD_DOT_f)
         FOLD_OP(
                        &opr,  
                        &loc_result.v,
                        &res_linear_type,
                        &l_value.v,
                        &l_linear_type,
                        &r_value.v,
                        &r_linear_type, 
                        &a3_value.v,
                        &a3_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }

         break;
# endif



         a4_value.v[0] = TARGET_BITS_PER_WORD;
# ifdef _TARGET32
         if (num_host_wds[res_linear_type] != 1) {
            a4_value.v[1] = 2 * TARGET_BITS_PER_WORD;
            a4_value.v[0] = 0;
         }
# endif


         SHIFT_ARITH_ARG(r_value.v, res_linear_type);
         SHIFT_ARITH_ARG(a4_value.v, res_linear_type);

         mask = AR_mod((AR_DATA *)a3_value.v,
                     (const AR_TYPE *)&linear_to_arith[res_linear_type],
                     (const AR_DATA *)r_value.v,
                     (const AR_TYPE *)&linear_to_arith[res_linear_type],
                     (const AR_DATA *)a4_value.v,
                     (const AR_TYPE *)&linear_to_arith[res_linear_type]);

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);

         a4_value.v[0] = 0;
         a4_value.v[1] = 0;
         a4_value.v[2] = 0;
         a4_value.v[3] = 0;

         SHIFT_ARITH_ARG(l_value.v, res_linear_type);

         if ((mask & AR_STAT_NEGATIVE) != 0) {

            mask = AR_negate((AR_DATA *)loc_result.v,
                      (const AR_TYPE *)&linear_to_arith[res_linear_type],
                      (const AR_DATA *)a3_value.v,
                      (const AR_TYPE *)&linear_to_arith[res_linear_type]);

            for (i = 0; i < 4; i++) {
               a3_value.v[i] = loc_result.v[i];
            }

            mask = AR_dshiftr((AR_DATA *)r_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)l_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)a4_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)a3_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type]);

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);

            mask = AR_shiftr((AR_DATA *)loc_result.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)l_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)a3_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type]);

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         }
         else {
            mask = AR_dshiftl((AR_DATA *)r_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)a4_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)l_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)a3_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type]);

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);

            mask = AR_shiftl((AR_DATA *)loc_result.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)l_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)a3_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type]);

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         }

         SHIFT_ARITH_RESULT(r_value.v, res_linear_type);
         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);
         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            loc_result.v[i] |= r_value.v[i];
         }

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
         break;



      case Ishftc_Opr :
      case Ibits_Opr :
         if (l_linear_type != res_linear_type &&
             TYP_TYPE(l_type_idx) != Typeless) {

            SHIFT_ARITH_ARG(l_value.v, l_linear_type);

# if defined(_USE_FOLD_DOT_f)
            tmp_opr = Cvrt_Opr;
            FOLD_OP(
                           &tmp_opr,  
                           &loc_result.v,
                           &res_linear_type,
                           &l_value.v,
                           &l_linear_type,
                           &r_value.v,
                           &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else
            mask = AR_convert((AR_DATA *)loc_result.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const AR_DATA *)l_value.v,
                         (const AR_TYPE *)&linear_to_arith[l_linear_type]);
# endif

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               l_value.v[i] = loc_result.v[i];
            }
         }

         if (r_linear_type != res_linear_type &&
             TYP_TYPE(r_type_idx) != Typeless) {

            SHIFT_ARITH_ARG(r_value.v, r_linear_type);

# if defined(_USE_FOLD_DOT_f)
            tmp_opr = Cvrt_Opr;
            FOLD_OP(
                           &tmp_opr,  
                           &loc_result.v,
                           &res_linear_type,
                           &r_value.v,
                           &r_linear_type,
                           &r_value.v,
                           &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else
             mask = AR_convert((AR_DATA *)loc_result.v,
                            (const AR_TYPE *)&linear_to_arith[res_linear_type],
                            (const AR_DATA *)r_value.v,
                            (const AR_TYPE *)&linear_to_arith[r_linear_type]);
# endif

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               r_value.v[i] = loc_result.v[i];
            }
         }

         if (a3_linear_type != res_linear_type &&
             TYP_TYPE(a3_type_idx) != Typeless) {

            SHIFT_ARITH_ARG(a3_value.v, a3_linear_type);

# if defined(_USE_FOLD_DOT_f)
            tmp_opr = Cvrt_Opr;
            FOLD_OP(
                           &tmp_opr,  
                           &loc_result.v,
                           &res_linear_type,
                           &a3_value.v,
                           &a3_linear_type,
                           &r_value.v,
                           &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else
             mask = AR_convert((AR_DATA *)loc_result.v,
                            (const AR_TYPE *)&linear_to_arith[res_linear_type],
                            (const AR_DATA *)a3_value.v,
                            (const AR_TYPE *)&linear_to_arith[a3_linear_type]);
# endif

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               a3_value.v[i] = loc_result.v[i];
            }
         }

         SHIFT_ARITH_ARG(l_value.v, res_linear_type);
         SHIFT_ARITH_ARG(r_value.v, res_linear_type);
         SHIFT_ARITH_ARG(a3_value.v, res_linear_type);

# if defined(_USE_FOLD_DOT_f)
         FOLD_OP(
                          &opr,  
                          &loc_result.v,
                          &res_linear_type,
                          &l_value.v,
                          &res_linear_type,
                          &r_value.v,
                          &res_linear_type, 
                          &a3_value.v,
                          &res_linear_type);
# else
         if (opr == Ibits_Opr) {
            mask = AR_ibits((AR_DATA *)loc_result.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)l_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)r_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type], 
                          (const AR_DATA *)a3_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type]);
         }
         else {
            mask = AR_ishftc((AR_DATA *)loc_result.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)l_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type],
                          (const AR_DATA *)r_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type], 
                          (const AR_DATA *)a3_value.v,
                          (const AR_TYPE *)&linear_to_arith[res_linear_type]);
         }

         /* don't check for anything but invalid type here */

         if ((mask & AR_STAT_INVALID_TYPE) != 0) {
             PRINTMSG(line, 1079, Internal, col);
         }
# endif

         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
         break;


      case Shiftl_Opr :
      case Shiftr_Opr :
      case Shifta_Opr :
         if (l_linear_type != res_linear_type &&
             TYP_TYPE(l_type_idx) != Typeless) {

            SHIFT_ARITH_ARG(l_value.v, l_linear_type);

# if defined(_USE_FOLD_DOT_f)
            tmp_opr = Cvrt_Opr;
            FOLD_OP(
                           &tmp_opr,  
                           &loc_result.v,
                           &res_linear_type,
                           &l_value.v,
                           &l_linear_type,
                           &r_value.v,
                           &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else
            mask = AR_convert((AR_DATA *)loc_result.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const AR_DATA *)l_value.v,
                         (const AR_TYPE *)&linear_to_arith[l_linear_type]);
# endif

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               l_value.v[i] = loc_result.v[i];
            }
         }

         if (r_linear_type != res_linear_type &&
             TYP_TYPE(r_type_idx) != Typeless) {

            SHIFT_ARITH_ARG(r_value.v, r_linear_type);

# if defined(_USE_FOLD_DOT_f)
            tmp_opr = Cvrt_Opr;
            FOLD_OP(
                           &tmp_opr,  
                           &loc_result.v,
                           &res_linear_type,
                           &r_value.v,
                           &r_linear_type,
                           &r_value.v,
                           &r_linear_type, 
                           &a3_value.v,
                           &a3_linear_type);
# else
             mask = AR_convert((AR_DATA *)loc_result.v,
                            (const AR_TYPE *)&linear_to_arith[res_linear_type],
                            (const AR_DATA *)r_value.v,
                            (const AR_TYPE *)&linear_to_arith[r_linear_type]);
# endif

            ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
            SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               r_value.v[i] = loc_result.v[i];
            }
         }

         SHIFT_ARITH_ARG(l_value.v, res_linear_type);
         SHIFT_ARITH_ARG(r_value.v, res_linear_type);

         arith_type = linear_to_arith[res_linear_type];

         if (opr != Shifta_Opr) {
            if (arith_type == AR_Int_32_S) {
               arith_type = AR_Int_32_U;
            }
            else if (arith_type == AR_Int_64_S) {
               arith_type = AR_Int_64_U;
            }
         }

# if defined(_USE_FOLD_DOT_f)
         FOLD_OP(
                        &opr,  
                        &loc_result.v,
                        &res_linear_type,
                        &l_value.v,
                        &res_linear_type,
                        &r_value.v,
                        &res_linear_type, 
                        &a3_value.v,
                        &a3_linear_type);
# else
         switch (opr) {
         case Shiftl_Opr :
              mask = AR_shiftl((AR_DATA *)loc_result.v,
                               (const AR_TYPE *)&arith_type,
                               (const AR_DATA *)l_value.v,
                               (const AR_TYPE *)&arith_type,
                               (const AR_DATA *)r_value.v,
                               (const AR_TYPE *)&arith_type);
              break;

         case Shiftr_Opr :
         case Shifta_Opr :
              mask = AR_shiftr((AR_DATA *)loc_result.v,
                               (const AR_TYPE *)&arith_type,
                               (const AR_DATA *)l_value.v,
                               (const AR_TYPE *)&arith_type,
                               (const AR_DATA *)r_value.v,
                               (const AR_TYPE *)&arith_type);
              break;
         }

         /* don't check for anything but invalid type here */

         if ((mask & AR_STAT_INVALID_TYPE) != 0) {
             PRINTMSG(line, 1079, Internal, col);
         }
# endif

         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
         break;


      case Dim_Opr :
         type_idx = CG_LOGICAL_DEFAULT_TYPE;

         ok = folder_driver((char *)l_value.v,
                           l_type_idx,
                           (char *)r_value.v,
                           r_type_idx,
                           a3_value.v,
                           &type_idx,
                           line,
                           col,
                           2,
                           Le_Opr);

         if (THIS_IS_TRUE(a3_value.v, type_idx)) {
            ok &= folder_driver((char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                                CG_INTEGER_DEFAULT_TYPE,
                                NULL,
                                NULL_IDX,
                                result,
                                res_type_idx,
                                line,
                                col,
                                1,
                                Cvrt_Opr);
         }
         else {
            ok = folder_driver((char *)l_value.v,
                           l_type_idx,
                           (char *)r_value.v,
                           r_type_idx,
                           a3_value.v,
                           res_type_idx,
                           line,
                           col,
                           2,
                           Minus_Opr);

            for (i = 0; i < num_host_wds[res_linear_type]; i++) {
               result[i] = a3_value.v[i];
            }
         }
         break;



      case Ichar_Opr :
         result[0] = l_value_ptr[0];

# ifdef _TARGET32
         if (res_linear_type == Integer_8) {
# if defined(_TARGET_LITTLE_ENDIAN)
//Bug 1581
            result[1] = 0;
# else
            result[1] = result[0];
            result[0] = 0;
#endif
         }
# endif
         break;


      case Char_Opr :
# if defined(_TARGET_LITTLE_ENDIAN)
        /*
         * NOTE: 1) range checking is pre-performed by caller
         *       2) Integer_8 is swapped, so this works for it as well
         */
        result[0] = l_value.v[0];
# else

# ifdef _TARGET32
         if (l_linear_type == Integer_8) {
            l_value.v[0] = l_value.v[1];
         }
# endif

         result[0] = l_value.v[0] << (TARGET_BITS_PER_WORD - CHAR_BIT);
# endif
         break;


      case Index_Opr :
         SHIFT_ARITH_ARG(a3_value.v, a3_linear_type);

         str1_linear_type = TYP_LINEAR(CN_TYPE_IDX(TYP_IDX(l_type_idx)));
         str2_linear_type = TYP_LINEAR(CN_TYPE_IDX(TYP_IDX(r_type_idx)));

         for (i = 0; 
              i < num_host_wds[str1_linear_type];
              i++) {

            str_len1.v[i] = CP_CONSTANT(CN_POOL_IDX(TYP_IDX(l_type_idx)) + i);
         }

         for (i = 0; 
              i < num_host_wds[str2_linear_type];
              i++) {

            str_len2.v[i] = CP_CONSTANT(CN_POOL_IDX(TYP_IDX(r_type_idx)) + i);
         }

# ifdef _TARGET32
         if (num_host_wds[str1_linear_type] != num_host_wds[res_linear_type]) {
            if (res_linear_type == Integer_8) {
#ifndef _TARGET_LITTLE_ENDIAN
               str_len1.v[1] = str_len1.v[0];
               str_len1.v[0] = 0;
#endif
            }
            else {
               str_len1.v[0] = str_len1.v[1];
            }
         }

         if (num_host_wds[str2_linear_type] != num_host_wds[res_linear_type]) {
            if (res_linear_type == Integer_8) {
#ifndef _TARGET_LITTLE_ENDIAN
               str_len2.v[1] = str_len2.v[0];
               str_len2.v[0] = 0;
#endif
            }
            else {
               str_len2.v[0] = str_len2.v[1];
            }
         }
# endif

         
         SHIFT_ARITH_ARG(str_len1.v, res_linear_type);
         SHIFT_ARITH_ARG(str_len2.v, res_linear_type);

         mask = AR_index((AR_DATA *)loc_result.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const char *)l_value_ptr,
                         (const AR_DATA *)str_len1.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const char *)r_value_ptr,
                         (const AR_DATA *)str_len2.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const AR_DATA *)a3_value.v,
                         (const AR_TYPE *)&linear_to_arith[a3_linear_type]);

         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         break;


      case Scan_Opr :
         SHIFT_ARITH_ARG(a3_value.v, a3_linear_type);

         str1_linear_type = TYP_LINEAR(CN_TYPE_IDX(TYP_IDX(l_type_idx)));
         str2_linear_type = TYP_LINEAR(CN_TYPE_IDX(TYP_IDX(r_type_idx)));

         for (i = 0;
              i < num_host_wds[str1_linear_type];
              i++) {

            str_len1.v[i] = CP_CONSTANT(CN_POOL_IDX(TYP_IDX(l_type_idx)) + i);
         }

         for (i = 0;
              i < num_host_wds[str2_linear_type];
              i++) {

            str_len2.v[i] = CP_CONSTANT(CN_POOL_IDX(TYP_IDX(r_type_idx)) + i);
         }

# ifdef _TARGET32
         if (num_host_wds[str1_linear_type] != num_host_wds[res_linear_type]) {
            if (res_linear_type == Integer_8) {
#ifndef _TARGET_LITTLE_ENDIAN
               str_len1.v[1] = str_len1.v[0];
               str_len1.v[0] = 0;
#endif
            }
            else {
               str_len1.v[0] = str_len1.v[1];
            }
         }

         if (num_host_wds[str2_linear_type] != num_host_wds[res_linear_type]) {
            if (res_linear_type == Integer_8) {
#ifndef _TARGET_LITTLE_ENDIAN
               str_len2.v[1] = str_len2.v[0];
               str_len2.v[0] = 0;
#endif
            }
            else {
               str_len2.v[0] = str_len2.v[1];
            }
         }
# endif


         SHIFT_ARITH_ARG(str_len1.v, res_linear_type);
         SHIFT_ARITH_ARG(str_len2.v, res_linear_type);

         mask = AR_scan((AR_DATA *)loc_result.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const char *)l_value_ptr,
                         (const AR_DATA *)str_len1.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const char *)r_value_ptr,
                         (const AR_DATA *)str_len2.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const AR_DATA *)a3_value.v,
                         (const AR_TYPE *)&linear_to_arith[a3_linear_type]);

         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         break;


      case Verify_Opr :
         SHIFT_ARITH_ARG(a3_value.v, a3_linear_type);

         str1_linear_type = TYP_LINEAR(CN_TYPE_IDX(TYP_IDX(l_type_idx)));
         str2_linear_type = TYP_LINEAR(CN_TYPE_IDX(TYP_IDX(r_type_idx)));

         for (i = 0;
              i < num_host_wds[str1_linear_type];
              i++) {

            str_len1.v[i] = CP_CONSTANT(CN_POOL_IDX(TYP_IDX(l_type_idx)) + i);
         }

         for (i = 0;
              i < num_host_wds[str2_linear_type];
              i++) {

            str_len2.v[i] = CP_CONSTANT(CN_POOL_IDX(TYP_IDX(r_type_idx)) + i);
         }

# ifdef _TARGET32
         if (num_host_wds[str1_linear_type] != num_host_wds[res_linear_type]) {
            if (res_linear_type == Integer_8) {
#ifndef _TARGET_LITTLE_ENDIAN
               str_len1.v[1] = str_len1.v[0];
               str_len1.v[0] = 0;
#endif
            }
            else {
               str_len1.v[0] = str_len1.v[1];
            }
         }

         if (num_host_wds[str2_linear_type] != num_host_wds[res_linear_type]) {
            if (res_linear_type == Integer_8) {
#ifndef _TARGET_LITTLE_ENDIAN
               str_len2.v[1] = str_len2.v[0];
               str_len2.v[0] = 0;
#endif
            }
            else {
               str_len2.v[0] = str_len2.v[1];
            }
         }
# endif


         SHIFT_ARITH_ARG(str_len1.v, res_linear_type);
         SHIFT_ARITH_ARG(str_len2.v, res_linear_type);


         mask = AR_verify((AR_DATA *)loc_result.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const char *)l_value_ptr,
                         (const AR_DATA *)str_len1.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const char *)r_value_ptr,
                         (const AR_DATA *)str_len2.v,
                         (const AR_TYPE *)&linear_to_arith[res_linear_type],
                         (const AR_DATA *)a3_value.v,
                         (const AR_TYPE *)&linear_to_arith[a3_linear_type]);

         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }

         ARITH_ERROR_RESULT_TEST(mask, (*res_type_idx), ok, line, col);
         break;


      case Adjustl_Opr :
         /* The return value is the constant tbl index for */
         /* the character result constant.                 */

         result[0] = ntr_const_tbl(l_type_idx, TRUE, NULL);
#ifdef KEY /* Bug 12014 */
         l_value_ptr = CORRECT_THE_POINTER(l_value_offset);
         r_value_ptr = CORRECT_THE_POINTER(r_value_offset);
#endif /* KEY Bug 12014 */

         *res_type_idx = l_type_idx;

         char_len = CN_INT_TO_C(TYP_IDX(l_type_idx));

         i = 0;
         while (i < char_len &&
                l_value_ptr[i] == ' ') {
            i++;
         }

         char_ptr = (char *)&(CN_CONST(result[0]));

         for (k = 0; k < (char_len - i); k++) {
            char_ptr[k] = l_value_ptr[i + k];
         }
 
         for (; k < char_len; k++) {
            char_ptr[k] = ' ';
         }
         break;


      case Adjustr_Opr :
         /* The return value is the constant tbl index for */
         /* the character result constant.                 */

         result[0] = ntr_const_tbl(l_type_idx,
                                   TRUE,
                                   (long_type *) char_buf);
#ifdef KEY /* Bug 12014 */
         l_value_ptr = CORRECT_THE_POINTER(l_value_offset);
         r_value_ptr = CORRECT_THE_POINTER(r_value_offset);
#endif /* KEY Bug 12014 */

         *res_type_idx = l_type_idx;

         char_len = CN_INT_TO_C(TYP_IDX(l_type_idx));

         i = 0;
         while (i < char_len &&
                l_value_ptr[(char_len - i) - 1] == ' ') {
            i++;
         }

         /* i is the number of blanks */

         char_ptr = (char *)&(CN_CONST(result[0]));

         for (k = char_len; k > i; k--) {
            char_ptr[k - 1] = l_value_ptr[(k - i) - 1];
         }
 
         for (; k > 0; k--) {
            char_ptr[k - 1] = ' ';
         }
         break;


      case Len_Trim_Opr :
         char_len = CN_INT_TO_C(TYP_IDX(l_type_idx));
         while (char_len > 0 && l_value_ptr[char_len-1] == ' ') {
            char_len--;
         }

         /* char_len is a C value, result is a target value */

         C_TO_F_INT(result, char_len, TYP_LINEAR(*res_type_idx));
         break;


      case Mask_Opr :
         SHIFT_ARITH_ARG(l_value.v, l_linear_type);

         mask = AR_mask((AR_DATA *)loc_result.v,
                      (const AR_TYPE *)&linear_to_arith[res_linear_type],
                      (const AR_DATA *)l_value.v,
                      (const AR_TYPE *)&linear_to_arith[res_linear_type]);

         /* don't check for anything but invalid type here */

         if ((mask & AR_STAT_INVALID_TYPE) != 0) {
             PRINTMSG(line, 1079, Internal, col);
         }
         SHIFT_ARITH_RESULT(loc_result.v, res_linear_type);

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            result[i] = loc_result.v[i];
         }
         break;



      case Csmg_Opr :
         /* transform to this ... */
         /* csmg(x,y,z) = (x .and. z)  .or. (y .and.  (.not. z)) */

         type_idx = *res_type_idx;

         /* (x .and. z) */
         ok = folder_driver((char *)l_value.v,
                            l_type_idx,
                            (char *)a3_value.v,
                            a3_type_idx,
                            a4_value.v,
                            &type_idx,
                            line,
                            col,
                            2,
                            Band_Opr) && ok;

         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            l_value.v[i] = a4_value.v[i];
         }
         /* x now holds (x .and. z) */

         /* (.not. z) */

         ok = folder_driver((char *)a3_value.v,
                            a3_type_idx,
                            NULL,
                            NULL_IDX,
                            a4_value.v,
                            &type_idx,
                            line,
                            col,
                            1,
                            Bnot_Opr) && ok;
   
         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            a3_value.v[i] = a4_value.v[i];
         }
         /* z now holds (.not. z) */
   
         /* (y .and. (.not. z)) */
         ok = folder_driver((char *)r_value.v,
                            r_type_idx,
                            (char *)a3_value.v,
                            type_idx,
                            a4_value.v,
                            &type_idx,
                            line,
                            col,
                            2,
                            Band_Opr) && ok;
   
         for (i = 0; i < num_host_wds[res_linear_type]; i++) {
            r_value.v[i] = a4_value.v[i];
         }
         /* y now holds (y .and. (.not. z)) */
   
   
         /* (x .and. z)  .or. (y .and.  (.not. z)) */
   
         ok = folder_driver((char *)l_value.v,
                            type_idx,
                            (char *)r_value.v,
                            type_idx,
                            result,
                            res_type_idx,
                            line,
                            col,
                            2,
                            Bor_Opr) && ok;
   
         break;

      default:
         PRINTMSG(line, 828, Internal, col);
         break;
   }
#ifdef KEY /* Bug 12014 */
   }
#endif /* KEY Bug 12014 */

# ifdef _TARGET_OS_MAX
   if (res_linear_type == Complex_4) {  /* KAYKAY */
      /* we need to unpack it into two words */
      result[1] = result[0] & 0xFFFFFFFF;
      result[0] = result[0] >> 32;
   }
# endif

EXIT:

   TRACE (Func_Exit, "folder_driver", NULL);

   return(ok);

}  /* folder_driver */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Depending on input, either fold two values using the passed in        *|
|*      operator, or generate IR for the two values using the passed in       *|
|*      operator.                                                             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      *op1	Pointer to the first operator.                                *|
|*      *op2	Pointer to the first operator.                                *|
|*       opr	Operator to use for the calculation.                          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      *result	Pointer to the result.                                        *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if calculation folded okay or was created ok.                    *|
|*                                                                            *|
\******************************************************************************/
boolean size_offset_binary_calc(size_offset_type	*op1,
				size_offset_type	*op2,
				operator_type		 opr,
				size_offset_type	*result)

{
   long_type		*constant1;
   long_type		*constant2;
   int			 i;
   int			 ir_idx;
   boolean		 ok;
   opnd_type		 opnd1;
   opnd_type		 opnd2;
   boolean		 symbolic_constant	= FALSE;
   int			 type_idx;
   int			 type1_idx;
   int			 type2_idx;
   long_type		 result_long[MAX_WORDS_FOR_INTEGER];


   TRACE (Func_Entry, "size_offset_binary_calc", NULL);

   switch ((*op1).fld) {
   case NO_Tbl_Idx:
      constant1	= &((*op1).constant[0]);
      type1_idx	= (*op1).type_idx;
      break;

   case CN_Tbl_Idx:
      constant1	= &(CN_CONST((*op1).idx));
      type1_idx	= CN_TYPE_IDX((*op1).idx);
      break;

   case AT_Tbl_Idx:
      constant1		= NULL;
      type1_idx		= ATD_TYPE_IDX((*op1).idx);
      symbolic_constant	= (AT_OBJ_CLASS((*op1).idx) == Data_Obj) &&
                           ATD_SYMBOLIC_CONSTANT((*op1).idx);
      break;

   case IR_Tbl_Idx:
      constant1	= NULL;
      type1_idx	= IR_TYPE_IDX((*op1).idx);
      break;

   default:   /* IL_Tbl_Idx and SH_Tbl_Idx -> Shouldn't be, but just in case. */

      constant1	= NULL;
      type1_idx	= SA_INTEGER_DEFAULT_TYPE;
      break;

   }  /* End switch */


   switch ((*op2).fld) {
   case NO_Tbl_Idx:
      constant2	= &((*op2).constant[0]);
      type2_idx	= (*op2).type_idx;
      break;

   case CN_Tbl_Idx:
      constant2	= &(CN_CONST((*op2).idx));
      type2_idx	= CN_TYPE_IDX((*op2).idx);
      break;

   case AT_Tbl_Idx:
      constant2		 = NULL;
      type2_idx		 = ATD_TYPE_IDX((*op2).idx);
      symbolic_constant |= (AT_OBJ_CLASS((*op2).idx) == Data_Obj) &&
                            ATD_SYMBOLIC_CONSTANT((*op2).idx);
      break;

   case IR_Tbl_Idx:
      constant2	= NULL;
      type2_idx	= IR_TYPE_IDX((*op2).idx);
      break;

   default:   /* IL_Tbl_Idx and SH_Tbl_Idx -> Shouldn't be, but just in case. */

      constant2	= NULL;
      type2_idx	= SA_INTEGER_DEFAULT_TYPE;
      break;

   }  /* End switch */


   if (constant1 != NULL && constant2 != NULL) {
      type_idx = (TYP_LINEAR(type2_idx) > TYP_LINEAR(type1_idx)) ? type2_idx :
                                                                   type1_idx;
      
      issue_overflow_msg_719 = FALSE;

      ok = folder_driver((char *) constant1,
                         type1_idx,
                         (char *) constant2,
                         type2_idx,
                         result_long,
                         &type_idx,
                         stmt_start_line,
                         stmt_start_col,
                         2,
                         opr);

      if (need_to_issue_719) {

         if (TYP_LINEAR(type_idx) < LARGEST_INTEGER_TYPE) {
            need_to_issue_719	= FALSE;
            type_idx		= LARGEST_INTEGER_TYPE;
            ok			|= folder_driver((char *) constant1,
                                                 type1_idx,
                                                 (char *) constant2,
                                                 type2_idx,
                                                 result_long,
                                                 &type_idx,
                                                 stmt_start_line,
                                                 stmt_start_col,
                                                 2,
                                                 opr);
          }

         if (need_to_issue_719) {
            PRINTMSG(stmt_start_line, 1175, Error, stmt_start_col);
            need_to_issue_719	= FALSE;
         }
      }

      for (i = 0; i < MAX_WORDS_FOR_INTEGER; i++) {
         (*result).constant[i]	= result_long[i];
      }
 
      (*result).type_idx	= type_idx;
      (*result).fld		= NO_Tbl_Idx;
      issue_overflow_msg_719	= TRUE;
   }
   else {

      /* One or the other may be constant, but not both.   */
      /* But to generate IR we need both as table indexes. */

      if ((*op1).fld == NO_Tbl_Idx) { 
         (*op1).idx = ntr_const_tbl((*op1).type_idx, FALSE, (*op1).constant);
         (*op1).fld = CN_Tbl_Idx;
      }
      else if ((*op2).fld == NO_Tbl_Idx) {
         (*op2).idx = ntr_const_tbl((*op2).type_idx, FALSE, (*op2).constant);
         (*op2).fld = CN_Tbl_Idx;
      }

      OPND_FLD(opnd1)		= (*op1).fld;
      OPND_IDX(opnd1)		= (*op1).idx;
      OPND_LINE_NUM(opnd1)	= stmt_start_line;
      OPND_COL_NUM(opnd1)	= stmt_start_col;

      OPND_FLD(opnd2)		= (*op2).fld;
      OPND_IDX(opnd2)		= (*op2).idx;
      OPND_LINE_NUM(opnd2)	= stmt_start_line;
      OPND_COL_NUM(opnd2)	= stmt_start_col;

      if (!symbolic_constant) {
         type1_idx = check_type_for_size_address(&opnd1);
         type2_idx = check_type_for_size_address(&opnd2);
      }

      type_idx  = (TYP_LINEAR(type2_idx) > TYP_LINEAR(type1_idx)) ? type2_idx :
                                                                    type1_idx;

      NTR_IR_TBL(ir_idx);
      IR_TYPE_IDX(ir_idx)	= type_idx;
      IR_LINE_NUM_L(ir_idx)	= stmt_start_line;
      IR_LINE_NUM_R(ir_idx)	= stmt_start_line;
      IR_LINE_NUM(ir_idx)	= stmt_start_line;
      IR_COL_NUM_L(ir_idx)	= stmt_start_col;
      IR_COL_NUM_R(ir_idx)	= stmt_start_col;
      IR_COL_NUM(ir_idx)	= stmt_start_col;
      IR_FLD_L(ir_idx)		= OPND_FLD(opnd1);
      IR_IDX_L(ir_idx)		= OPND_IDX(opnd1);
      IR_FLD_R(ir_idx)		= OPND_FLD(opnd2);
      IR_IDX_R(ir_idx)		= OPND_IDX(opnd2);

      if (symbolic_constant) {

         switch(opr) {
         case Plus_Opr:
            opr		= Symbolic_Plus_Opr;
            break;

         case Div_Opr:
            opr		= Symbolic_Div_Opr;
            break;

         case Mult_Opr:
            opr		= Symbolic_Mult_Opr;
            break;

         case Minus_Opr:
            opr		= Symbolic_Minus_Opr;
            break;

         case Mod_Opr:
            opr		= Symbolic_Mod_Opr;
            break;

         case Shiftl_Opr:
            opr		= Symbolic_Shiftl_Opr;
            break;

         case Shiftr_Opr:
            opr		= Symbolic_Shiftr_Opr;
            break;
         }

         (*result).fld	= AT_Tbl_Idx;
         (*result).idx	= gen_compiler_tmp(stmt_start_line, stmt_start_col,
                                           Priv, TRUE);

         ATD_TYPE_IDX((*result).idx)		= INTEGER_DEFAULT_TYPE;
         ATD_FLD((*result).idx)			= IR_Tbl_Idx;
         ATD_TMP_IDX((*result).idx)		= ir_idx;
         ATD_SYMBOLIC_CONSTANT((*result).idx)	= TRUE;
      }
      else {
         (*result).idx		= ir_idx;
         (*result).fld		= IR_Tbl_Idx;
      }

      IR_OPR(ir_idx)		= opr;
      ok			= TRUE;
   }

   TRACE (Func_Exit, "size_offset_binary_calc", NULL);

   return(ok);

}  /* size_offset_binary_calc */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Depending on input, either fold two values using the passed in        *|
|*      operator, or generate IR for the two values using the passed in       *|
|*      operator.                                                             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      *op1	Pointer to the first operator.                                *|
|*      *op2	Pointer to the first operator.                                *|
|*       opr	Operator to use for the calculation.                          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      *result	Pointer to the result.                                        *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if calculation folded okay or was created ok.                    *|
|*                                                                            *|
\******************************************************************************/
boolean size_offset_logical_calc(size_offset_type	*op1,
				 size_offset_type	*op2,
				 operator_type		 opr,
				 size_offset_type	*result)

{
   long_type		*constant1;
   long_type		*constant2;
   int			 ir_idx;
   boolean		 ok;
   int			 type_idx;
   int			 type1_idx;
   int			 type2_idx;
 

   TRACE (Func_Entry, "size_offset_logical_calc", NULL);

   switch ((*op1).fld) {
   case NO_Tbl_Idx:
      constant1	= &((*op1).constant[0]);
      type1_idx	= (*op1).type_idx;
# ifdef KEY
      if (type1_idx == Integer_4 && *constant1 < 0){
        type1_idx = Integer_8;
      }
# endif
      break;

   case CN_Tbl_Idx:
      constant1	= &(CN_CONST((*op1).idx));
      type1_idx	= CN_TYPE_IDX((*op1).idx);
# ifdef KEY
      if (TYP_LINEAR(CN_TYPE_IDX((*op1).idx)) == Integer_4 && *constant1 < 0){
        type1_idx = Integer_8;
      }
# endif

      break;

   case AT_Tbl_Idx:
      constant1	= NULL;
      type1_idx	= ATD_TYPE_IDX((*op1).idx);
      break;

   case IR_Tbl_Idx:
      constant1	= NULL;
      type1_idx	= IR_TYPE_IDX((*op1).idx);
      break;

   default:   /* IL_Tbl_Idx and SH_Tbl_Idx -> Shouldn't be, but just in case. */

      constant1	= NULL;
      type1_idx	= CG_INTEGER_DEFAULT_TYPE;
      break;

   }  /* End switch */


   switch ((*op2).fld) {
   case NO_Tbl_Idx:
      constant2	= &((*op2).constant[0]);
      type2_idx	= (*op2).type_idx;
# ifdef KEY
      if (type2_idx == Integer_4 && *constant2 < 0){
        type2_idx = Integer_8;
      }
# endif
      break;

   case CN_Tbl_Idx:
      constant2	= &(CN_CONST((*op2).idx));
      type2_idx	= CN_TYPE_IDX((*op2).idx);
# ifdef KEY
      if (TYP_LINEAR(CN_TYPE_IDX((*op2).idx)) == Integer_4 && *constant2 < 0){
        type2_idx = Integer_8;
      }
# endif
      break;

   case AT_Tbl_Idx:
      constant2	= NULL;
      type2_idx	= ATD_TYPE_IDX((*op2).idx);
      break;

   case IR_Tbl_Idx:
      constant2	= NULL;
      type2_idx	= IR_TYPE_IDX((*op2).idx);
      break;

   default:   /* IL_Tbl_Idx and SH_Tbl_Idx -> Shouldn't be, but just in case. */

      constant2	= NULL;
      type2_idx	= CG_INTEGER_DEFAULT_TYPE;
      break;

   }  /* End switch */

   type_idx = CG_LOGICAL_DEFAULT_TYPE;

   if (constant1 != NULL && constant2 != NULL) {
      
      ok = folder_driver((char *) constant1,
                         type1_idx,
                         (char *) constant2,
                         type2_idx,
                         (*result).constant,
                         &type_idx,
                         stmt_start_line,
                         stmt_start_col,
                         2,
                         opr);

      (*result).type_idx	= type_idx;
      (*result).fld		= NO_Tbl_Idx;
   }
   else {

      /* One or the other may be constant, but not both.   */
      /* But to generate IR we need both as table indexes. */

      if ((*op1).fld == NO_Tbl_Idx) { 
         (*op1).idx = ntr_const_tbl((*op1).type_idx, FALSE, (*op1).constant);
         (*op1).fld = CN_Tbl_Idx;
      }
      else if ((*op2).fld == NO_Tbl_Idx) {
         (*op2).idx = ntr_const_tbl((*op2).type_idx, FALSE, (*op2).constant);
         (*op2).fld = CN_Tbl_Idx;
      }

      NTR_IR_TBL(ir_idx);

      IR_TYPE_IDX(ir_idx)	= type_idx;
      IR_LINE_NUM_L(ir_idx)	= stmt_start_line;
      IR_LINE_NUM_R(ir_idx)	= stmt_start_line;
      IR_LINE_NUM(ir_idx)	= stmt_start_line;
      IR_COL_NUM_L(ir_idx)	= stmt_start_col;
      IR_COL_NUM_R(ir_idx)	= stmt_start_col;
      IR_COL_NUM(ir_idx)	= stmt_start_col;
      IR_OPR(ir_idx)		= opr;
      IR_IDX_L(ir_idx)		= (*op1).idx;
      IR_FLD_L(ir_idx)		= (*op1).fld;
      IR_IDX_R(ir_idx)		= (*op2).idx;
      IR_FLD_R(ir_idx)		= (*op2).fld;

      (*result).idx		= ir_idx;
      (*result).fld		= IR_Tbl_Idx;
      ok			= TRUE;
   }

   TRACE (Func_Exit, "size_offset_logical_calc", NULL);

   return(ok);

}  /* size_offset_logical_calc */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Depending on input, either fold the list of values using the Min_Opr  *|
|*      or generate IR for the list of values using the Min_Opr.              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      *op1	Pointer to the first operator.                                *|
|*      *op2	Pointer to the first operator.                                *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      *result	Pointer to the result.                                        *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if calculation folded okay or was created ok.                    *|
|*                                                                            *|
\******************************************************************************/
boolean size_offset_min_max_calc(size_offset_type	*op1,
			         size_offset_type	*op2,
				 operator_type		 operator,
			         size_offset_type	*result)

{
   long_type		*constant1;
   long_type		*constant2;
   int			 il_idx;
   int			 il_idx2;
   int			 ir_idx;
   boolean		 ok;
   opnd_type		 opnd1;
   opnd_type		 opnd2;
#ifdef KEY /* Bug 10177 */
   boolean		 symbolic_constant = FALSE;
#else /* KEY Bug 10177 */
   boolean		 symbolic_constant;
#endif /* KEY Bug 10177 */
   int			 type_idx;
   int			 type1_idx;
   int			 type2_idx;
 

   TRACE (Func_Entry, "size_offset_min_calc", NULL);

   switch ((*op1).fld) {
   case NO_Tbl_Idx:
      constant1	= &((*op1).constant[0]);
      type1_idx	= (*op1).type_idx;
      break;

   case CN_Tbl_Idx:
      constant1	= &(CN_CONST((*op1).idx));
      type1_idx	= CN_TYPE_IDX((*op1).idx);
      break;

   case AT_Tbl_Idx:
      constant1	= NULL;
      type1_idx	= ATD_TYPE_IDX((*op1).idx);
      symbolic_constant = (AT_OBJ_CLASS((*op1).idx) == Data_Obj) &&
                           ATD_SYMBOLIC_CONSTANT((*op1).idx);
      break;

   case IR_Tbl_Idx:
      constant1	= NULL;
      type1_idx	= IR_TYPE_IDX((*op1).idx);
      break;

   default:   /* IL_Tbl_Idx and SH_Tbl_Idx -> Shouldn't be, but just in case. */

      constant1	= NULL;
      type1_idx	= CG_INTEGER_DEFAULT_TYPE;
      break;

   }  /* End switch */


   switch ((*op2).fld) {
   case NO_Tbl_Idx:
      constant2	= &((*op2).constant[0]);
      type2_idx	= (*op2).type_idx;
      break;

   case CN_Tbl_Idx:
      constant2	= &(CN_CONST((*op2).idx));
      type2_idx	= CN_TYPE_IDX((*op2).idx);
      break;

   case AT_Tbl_Idx:
      constant2	= NULL;
      type2_idx	= ATD_TYPE_IDX((*op2).idx);
      symbolic_constant |= (AT_OBJ_CLASS((*op2).idx) == Data_Obj) &&
                           ATD_SYMBOLIC_CONSTANT((*op2).idx);
      break;

   case IR_Tbl_Idx:
      constant2	= NULL;
      type2_idx	= IR_TYPE_IDX((*op2).idx);
      break;

   default:   /* IL_Tbl_Idx and SH_Tbl_Idx -> Shouldn't be, but just in case. */
      constant2	= NULL;
      type2_idx	= CG_INTEGER_DEFAULT_TYPE;
      break;

   }  /* End switch */

   if (constant1 != NULL && constant2 != NULL) {
      type_idx = CG_LOGICAL_DEFAULT_TYPE;
      
      ok = folder_driver((char *) constant1,
                         type1_idx,
                         (char *) constant2,
                         type2_idx,
                         (*result).constant,
                         &type_idx,
                         stmt_start_line,
                         stmt_start_col,
                         2,
                         Lt_Opr);

      if (THIS_IS_TRUE((*result).constant, (*result).type_idx)) {
         (*result)	= (operator == Min_Opr) ? (*op1) : (*op2);
      }
      else {
         (*result)	= (operator == Min_Opr) ? (*op2) : (*op1);
      }
   }
   else {

      /* One or the other may be constant, but not both.   */
      /* But to generate IR we need both as table indexes. */

      if ((*op1).fld == NO_Tbl_Idx) { 
         (*op1).idx = ntr_const_tbl((*op1).type_idx, FALSE, (*op1).constant);
         (*op1).fld = CN_Tbl_Idx;
      }
      else if ((*op2).fld == NO_Tbl_Idx) {
         (*op2).idx = ntr_const_tbl((*op2).type_idx, FALSE, (*op2).constant);
         (*op2).fld = CN_Tbl_Idx;
      }

      OPND_FLD(opnd1)		= (*op1).fld;
      OPND_IDX(opnd1)		= (*op1).idx;
      OPND_LINE_NUM(opnd1)	= stmt_start_line;
      OPND_COL_NUM(opnd1)	= stmt_start_col;

      OPND_FLD(opnd2)		= (*op2).fld;
      OPND_IDX(opnd2)		= (*op2).idx;
      OPND_LINE_NUM(opnd2)	= stmt_start_line;
      OPND_COL_NUM(opnd2)	= stmt_start_col;

      if (!symbolic_constant) {
         type1_idx = check_type_for_size_address(&opnd1);
         type2_idx = check_type_for_size_address(&opnd2);
      }

      type_idx	= (TYP_LINEAR(type2_idx) > TYP_LINEAR(type1_idx)) ? type2_idx :
                                                                    type1_idx;

      NTR_IR_TBL(ir_idx);

      IR_TYPE_IDX(ir_idx)	= type_idx;
      IR_LINE_NUM(ir_idx)	= stmt_start_line;
      IR_COL_NUM(ir_idx)	= stmt_start_col;

      if (operator == Min_Opr) {
         IR_OPR(ir_idx)		= (symbolic_constant) ? Symbolic_Min_Opr :
                                                        Min_Opr;
      }
      else {
         IR_OPR(ir_idx)		= (symbolic_constant) ? Symbolic_Max_Opr :
                                                        Max_Opr;
      }
      IR_FLD_L(ir_idx)		= IL_Tbl_Idx;
      IR_LIST_CNT_L(ir_idx)	= 2;

      NTR_IR_LIST_TBL(il_idx);
      IL_LINE_NUM(il_idx)	= stmt_start_line;
      IL_COL_NUM(il_idx)	= stmt_start_col;
      IL_FLD(il_idx)		= OPND_FLD(opnd1);
      IL_IDX(il_idx)		= OPND_IDX(opnd1);

      IR_IDX_L(ir_idx)		= il_idx;

      NTR_IR_LIST_TBL(il_idx2);
      IL_LINE_NUM(il_idx2)	= stmt_start_line;
      IL_COL_NUM(il_idx2)	= stmt_start_col;
      IL_FLD(il_idx2)		= OPND_FLD(opnd2);
      IL_IDX(il_idx2)		= OPND_IDX(opnd2);
      IL_PREV_LIST_IDX(il_idx2)	= il_idx;

      IL_NEXT_LIST_IDX(il_idx)	= il_idx2;

      (*result).idx		= ir_idx;
      (*result).fld		= IR_Tbl_Idx;
      ok			= TRUE;
   }

   TRACE (Func_Exit, "size_offset_min_calc", NULL);

   return(ok);

}  /* size_offset_min_calc */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      The following routine makes sure that integer constant                *|
|*      entries can be used in 'C' arithmetic and comparison.                 *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

long64	f_int_to_cval(long_type		*the_constant,
		      int	 	 lin_type)

{
   int		i;
   long_type    input[MAX_WORDS_FOR_INTEGER];
   long64	result;


   TRACE (Func_Entry, "f_int_to_cval", NULL);

   for (i = 0; i < num_host_wds[TYP_LINEAR(lin_type)]; i++) {
       input[i] = the_constant[i];
   }

   SHIFT_ARITH_ARG(input, lin_type);

   i = AR_convert_int_to_host_sint64((AR_HOST_SINT64 *) &result,
                                 (const AR_DATA *) &input,
                                 (const AR_TYPE *) &linear_to_arith[lin_type]);

   TRACE (Func_Exit, "f_int_to_cval", NULL);

   return(result);

}  /* f_int_to_cval */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      The following routine makes sure that a 'C' constant translates back  *|
|*      to a target constant.  This is used where host and target             *|
|*      representation of constants is not the same.  Endian and host/target  *|
|*      sizes differing are two examples of where this is used.  It is the    *|
|*      underlying routine used by the macro.  C_TO_F_INT                     *|
|*      Use the macro - do not use this routine directly, as on most          *|
|*      platforms, this much overhead is not necessary.                       *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
int	cval_to_f_int(long_type	*result,
		      long64	*the_constant,
		      int	 type_idx)

{
   int		lin_type;
   int		ret;


   TRACE (Func_Entry, "cval_to_f_int", NULL);

   lin_type = (type_idx == NULL_IDX) ? CG_INTEGER_DEFAULT_TYPE :
                                       TYP_LINEAR(type_idx);

   ret = AR_convert_host_sint64_to_int((AR_DATA *)  result,
                                 (const AR_TYPE *) &linear_to_arith[lin_type],
                                       (AR_HOST_SINT64) *the_constant);


   if (ret == AR_STAT_OVERFLOW) {  /* Overflowed specified type */

      if (type_idx == NULL_IDX &&
          CG_INTEGER_DEFAULT_TYPE < LARGEST_INTEGER_TYPE) {
         lin_type = LARGEST_INTEGER_TYPE;
         ret = AR_convert_host_sint64_to_int(
                          (AR_DATA *)  result,
                    (const AR_TYPE *) &linear_to_arith[LARGEST_INTEGER_TYPE],
                          (AR_HOST_SINT64) *the_constant);
      }

      if (ret == AR_STAT_OVERFLOW) { /* Still overflowed */
         PRINTMSG(stmt_start_line, 719, Error, stmt_start_col);
         lin_type = Err_Res;
      }
      else {
         SHIFT_ARITH_RESULT(result, lin_type);
      }
   }
   else {
      SHIFT_ARITH_RESULT(result, lin_type);
   }

   TRACE (Func_Exit, "cval_to_f_int", NULL);

   return(lin_type);

}  /* cval_to_f_int */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      The following routine enters a 'C' (host) integer constant into the   *|
|*	constant table.							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
int	ntr_int_const_tbl(int		type_idx,
		          long64	constant)

{
   int		 cn_idx;
   long_type	 the_constant[MAX_WORDS_FOR_INTEGER];

# if !defined(_HOST64) || !defined(_TARGET64)
   int		 new_type;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   long		*cn_ptr; 
# endif
# endif


   TRACE (Func_Entry, "ntr_int_const_tbl", NULL);

# if defined(_HOST64) && defined(_TARGET64)

   if (type_idx == NULL_IDX) {
      type_idx = CG_INTEGER_DEFAULT_TYPE;
   }

   the_constant[0] = constant;

# elif defined(_USE_FOLD_DOT_f)
   if (type_idx == NULL_IDX) {
      type_idx = CG_INTEGER_DEFAULT_TYPE;
   }

   if (TYP_LINEAR(type_idx) == Integer_8 ||
       TYP_LINEAR(type_idx) == Typeless_8) {
      cn_ptr = (long *) &constant;
      the_constant[0] = *cn_ptr;
      if (MAX_WORDS_FOR_INTEGER > 1 )
        the_constant[1] = *(++cn_ptr);
   }
   else {
      the_constant[0] = constant;
      if (MAX_WORDS_FOR_INTEGER > 1 )
        the_constant[1] = 0;
   }

# elif (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   if (type_idx == NULL_IDX) { /* Set type according to size */
      new_type = cval_to_f_int(the_constant,
                               &constant,
                               NULL_IDX);

      if (new_type == NULL_IDX) {  /* Error situation */
         type_idx = CG_INTEGER_DEFAULT_TYPE;
      }
      else {
         type_idx = TYP_LINEAR(new_type);
      }
   }
   else {  /* Use type passed in */

      if (TYP_LINEAR(type_idx) == Integer_8 || 
          TYP_LINEAR(type_idx) == Typeless_8) {
         cn_ptr = (long *) &constant;
         the_constant[0] = *cn_ptr;     
         if (MAX_WORDS_FOR_INTEGER > 1 )
            the_constant[1] = *(++cn_ptr);
      }
      else { 
         the_constant[0] = (long) constant;
         if (MAX_WORDS_FOR_INTEGER > 1 )
           the_constant[1] = 0;
      }
   }

# else

   /* NOTE:  type_idx may be NULL_IDX.  Then we want cval_to_f_int to */
   /*        determine what the type_idx should be for this constant. */

   new_type	= cval_to_f_int(the_constant,
                                &constant,
                                type_idx);

   if (new_type == NULL_IDX) {  /* Error situation */
      type_idx = CG_INTEGER_DEFAULT_TYPE;
   }
   else {
      type_idx = TYP_LINEAR(new_type);
   }

# endif

   cn_idx = ntr_const_tbl(type_idx,
                          FALSE,
                          the_constant);

   TRACE (Func_Exit, "ntr_int_const_tbl", NULL);

   return(cn_idx);

}  /* ntr_int_const_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	On mpp systems,  the macro CN_INT_TO_C calls this routine to ensure   *|
|*      sign extension when veiwing 32 bit ints as c ints.                    *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
long_type	mpp_cn_int_to_c(int		cn_idx)

{
   long_type		the_constant;
   int			type_idx;

   TRACE (Func_Entry, "mpp_cn_int_to_c", NULL);

   if (TYP_LINEAR(CN_TYPE_IDX(cn_idx)) == Integer_1 ||
       TYP_LINEAR(CN_TYPE_IDX(cn_idx)) == Integer_2 ||
       TYP_LINEAR(CN_TYPE_IDX(cn_idx)) == Integer_4) {

      type_idx = CG_INTEGER_DEFAULT_TYPE;

      if (folder_driver((char *)&CN_CONST(cn_idx),
			CN_TYPE_IDX(cn_idx),
			NULL,
			NULL_IDX,
			&the_constant,
			&type_idx,
			stmt_start_line,
			stmt_start_col,
			1,
			Cvrt_Opr)) {
         /* intentionally blank */
      }
   }
   else {
      the_constant = CN_CONST(cn_idx);
   }

   TRACE (Func_Exit, "mpp_cn_int_to_c", NULL);

   return(the_constant);

}  /* mpp_cn_int_to_c */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if they are the same, else FALSE.                                *|
|*                                                                            *|
\******************************************************************************/

boolean	compare_target_consts(long_type	*const1,
			      int	 type1,
			      long_type  *const2,
			      int	 type2,
			      int	 opr)

{
   boolean	is_true;
   long_type	result[MAX_WORDS_FOR_INTEGER];
   int		type_idx;


   TRACE (Func_Entry, "compare_target_consts", NULL);

   type_idx = LOGICAL_DEFAULT_TYPE;

   if (folder_driver((char *)const1,
                     type1,
                     (char *)const2,
                     type2,
                     result,
                    &type_idx,
                     stmt_start_line,
                     stmt_start_col,
                     2,
                     opr)) {

      is_true = THIS_IS_TRUE(result, type_idx);
   }
   else {
      is_true = FALSE;
   }

   TRACE (Func_Exit, "compare_target_consts", NULL);

   return(is_true);

}  /* compare_target_consts */


# ifdef _USE_FOLD_DOT_f

#ifdef KEY /* Bug 5554 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	ONLY FOR THE LINUX COMPILER. USE UNTIL ARITH IS READY.                *|
|*									      *|
|*      Convert strings to host format constants for integer                  *|
|*      and real types.                                                       *|
|*									      *|
|* Input parameters:							      *|
|*	str: string to convert						      *|
|*	type_idx: type suggested by context                                   *|
|*	promote: if TRUE, ok to promote integer type to avoid overflow        *|
|*									      *|
|* Output parameters:							      *|
|*	NONE                                                                  *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no error besides out-of-range occurred                        *|
|*									      *|
\******************************************************************************/

boolean kludge_input_conversion(char *str, int type_idx, boolean promote)
{
   long_type	number[MAX_WORDS_FOR_NUMERIC];

   for (int i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
      number[i] = 0;
   }

   linear_type_type new_linear_type = Err_Res;
   int new_nbytes = 0;
   boolean range_ok = TRUE;
   errno = 0;
   switch (TYP_LINEAR(type_idx)) {
   case Integer_1:
      *(long *)number = strtol(str, (char **) 0, 10);
      if ((*(long *)number >= -128 && *(long*)number <= 127) || !promote) {
	break;
      }
      range_ok = FALSE;
      new_linear_type = Integer_2;
      new_nbytes = 2;
      /* FALLTHROUGH to promote */

   case Integer_2:
      *(long *)number = strtol(str, (char **) 0, 10);
      if ((*(long *)number >= -32768 && *(long*)number <= 32767) || !promote) {
        break;
	}
      range_ok = FALSE;
      new_linear_type = Integer_4;
      new_nbytes = 4;
      /* FALLTHROUGH to promote */

   case Integer_4:
      *(long *)number = strtol(str, (char **) 0, 10);
      if (ERANGE != errno || !promote) {
        break;
      }
      range_ok = FALSE;
      new_linear_type = Integer_8;
      new_nbytes = 8;
      /* FALLTHROUGH to promote */

   case Integer_8:
      *(long long *)number = strtoll(str, (char **) 0, 10);
#ifdef KEY /* Bug 8767 */
      /* If the source program uses "-2^63", we arrive here with "2^63", not
       * knowing whether a "-" preceded it. We surely want to give a warning
       * because "2^63" is out of range. By trying again with "strtoull", we
       * will give the "right" result for "-2^63", and a credible (though
       * negative) value for "2^63". Otherwise, we clip at 2^63-1, and let
       * the context of the constant do whatever it will do.
       */
      if (ERANGE == errno) {
	unsigned long long tryagain = strtoull(str, (char **) 0, 10);
	if (tryagain == ((unsigned long long)1) << 63) {
	  *(unsigned long long *)number = tryagain;
	}
	errno = ERANGE;
      }
#endif /* KEY Bug 8767 */
      range_ok = range_ok && (ERANGE != errno);
      break;

   case Real_4:
      *(float *)number = strtof(str, (char **) 0);
      range_ok = (ERANGE != errno);
      break;

   case Real_8:
      *(double *)number = strtod(str, (char **) 0);
      range_ok = (ERANGE != errno);
      break;

   case Real_16:
      *(long double *)number = strtold(str, (char **) 0);
      range_ok = (ERANGE != errno);
      break;

   default:
      errno = EINVAL; /* Cause internal error below */
      break;
   }
   if (!range_ok) {
      errno = 0; /* Out-of-range is just a warning, not an error */
      PRINTMSG(TOKEN_LINE(token), 1413, Warning, TOKEN_COLUMN(token));
   }
   if (Err_Res != new_linear_type) {
      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)           = TYP_TYPE(type_idx);
      TYP_LINEAR(TYP_WORK_IDX)         = new_linear_type;
      TYP_DCL_VALUE(TYP_WORK_IDX)      = new_nbytes;
      TYP_DESC(TYP_WORK_IDX)           = Kind_Typed;
      type_idx = ntr_type_tbl();
      }
   TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(type_idx, FALSE, number);
   if (errno) {
      PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
		"Integer or Real type", "kludge_input_conversion");
      return FALSE;
   }
   return TRUE;
}
#else /* KEY Bug 5554 */
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*    ONLY FOR THE LINUX COMPILER. USE UNTIL ARITH IS READY.                  *|
|*                                                                            *|
|*      Use sscanf to convert strings to host format constants for integer    *|
|*      and real types.  No error detection.                                  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*    NONE                                                                    *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*    NONE                                                                    *|
|*                                                                            *|
|* Returns:                                                                   *|
|*    NOTHING                                                                 *|
|*                                                                            *|
\******************************************************************************/

void kludge_input_conversion (char	*str,
			      int	type_idx)
{
   int		i;
   long_type	number[MAX_WORDS_FOR_NUMERIC];

   for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
      number[i] = 0;
   }

   switch (TYP_LINEAR(type_idx)) {
   case Integer_1:
   case Integer_2:
   case Integer_4:
      sscanf(str, "%lu", (long *)number);
      break;

   case Integer_8:
      sscanf(str, "%lld", (long long *)number);
      break;

   case Real_4:
      sscanf(str, "%f", (float *)number);
      break;

   case Real_8:
      sscanf(str, "%lf", (double *)number);
      break;

   case Real_16:
      sscanf(str, "%Lf", (long double *)number);
      break;

   default:
      PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
               "Integer or Real type", "kludge_input_conversion");
      break;
   }

   TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(type_idx,
                                              FALSE, 
                                              number);
}
#endif /* KEY Bug 5554 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ONLY FOR THE LINUX COMPILER. USE UNTIL ARITH IS READY.                *|
|*                                                                            *|
|*      Use sprintf to convert host format constants to strings for integer   *|
|*      and real types.  No error detection.                                  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void kludge_output_conversion (long_type *the_constant,
                               int       type_idx,
			       char	 *str)
{

   switch (TYP_LINEAR(type_idx)) {
   case Integer_1:
   case Integer_2:
   case Integer_4:
      sprintf(str, "%ld", *(long *)the_constant);
      break;

   case Integer_8:
      sprintf(str, "%lld", *(long long *)the_constant);
      break;

   case Real_4:
      sprintf(str, "%f", *(float *)the_constant);
      break;

   case Real_8:
      sprintf(str, "%f", *(double *)the_constant);
      break;

   case Real_16:
      sprintf(str, "%Lf", *(long double *)the_constant);
      break;

   default:
      PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
               "Integer or Real type", "kludge_output_conversion");
      break;
   }
}

# endif 
