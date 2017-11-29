/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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



static char USMID[] = "\n@(#)5.0_pl/sources/sytb.c	5.25	10/27/99 16:59:36\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# ifdef _ARITH_H
# include "arith.h"
# endif
#ifdef KEY /* Mac port */
#include <math.h> /* For "pow" */
#endif /* KEY Mac port */
# include "globals.m"
# include "tokens.m"
# include "sytb.m"  
# include "p_globals.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"

# ifdef _WHIRL_HOST64_TARGET64
int double_stride = 0;
# endif /* _WHIRL_HOST64_TARGET64 */
#ifdef KEY /* Bug 6204 */
#include "../sgi/decorate_utils.h"
#endif /* KEY Bug 6204 */

/******************************************************************\
|* Function prototypes of static functions declared in this file. *|
\******************************************************************/

static	void	calculate_pad(size_offset_type	*, size_offset_type *, int);
static	int	ntr_global_bounds_tbl(int);


/******************************************************************\
|* Other static stuff needed locally in this file.		  *|
\******************************************************************/

/* "pvp_isnormal" mimics the "isnormal" IEEE macro but is used to detect      */
/* an abnormal floating point value on a PVP machine.			      */

static boolean pvp_isnormal(int, long_type *);


/* ntr_abnormal_ieee_const is only needed for IEEE machines but since we have */
/* no ifdef macros that control in/exclusion of code for IEEE machines, it    */
/* exists on all machines.  It will only ever be called on IEEE machines.     */

static int ntr_abnormal_ieee_const(int, long_type *);


/* "is_normal" mimics the "isnormal" macro we put into our C compiler.	      */
/* "is_normal" evaluates to a nonzero int expression if the value is "normal";*/
/* that is, not zero, subnormal, infinite, or NaN.  This is done by testing   */
/* to see if its exponent is not All-1's or zero.  			      */

static boolean is_normal(int, long_type *);
static int     is_normal_32(long_type *);
static int     is_normal_64(int, long_type *);
static int     is_normal_128(int, long_type *);


/* "sign_bit" mimics the "signbit" macro we put into our C compiler.	      */
/* "sign_bit" evaluates to a nonzero int expression if its argument value is  */
/* negative. 								      */

static int     sign_bit(int, long_type *);
static int     sign_bit_32(long_type *);
static int     sign_bit_64(long_type *);
static int     sign_bit_128(long_type *);


/* "fp_classify" mimics the "fpclassify" macro we put into our C compiler.    */
/* "fp_classify" evaluates to an int value that indicates the class of the    */
/* argument.  				   		                      */

static int     fp_classify(int, long_type *);
static int     fp_classify_32(long_type *);
static int     fp_classify_64(int, long_type *);
static int     fp_classify_128(int, long_type *);


static int insert_constant(int, long_type *, int);
static int insert_unordered_constant(int, long_type *, int, int);
static void dump_cn_tree(int, int, int);

/* The following #define constants are likewise only needed for IEEE machines */
/* and are only referenced on IEEE machines.				      */

/* Values representing 32-bit real.					      */

#define IEEE_32_EXPO_BITS           8
#define IEEE_32_MANT_BITS          23
#define IEEE_32_EXPONENT           0XFF
#define IEEE_32_EXPO_ALLONES(X)  ((X) == IEEE_32_EXPONENT)

/* Values representing 64-bit real.					      */

#define IEEE_64_EXPO_BITS          11
#define IEEE_64_MANTU_BITS         20
#define IEEE_64_MANTL_BITS         32
#define IEEE_64_EXPONENT           0X7FF
#define IEEE_64_EXPO_ALLONES(X)  ((X) == IEEE_64_EXPONENT)

/* Values representing the leftmost 64 bits of a 128-bit real.		      */

#define IEEE_128_EXPO_BITS      15
#define IEEE_128_MANTTU_BITS    16
#define IEEE_128_MANTTL_BITS    32
#define IEEE_128_EXPO           0X7FFF
#define IEEE_128_EXPO_ALLONES(X)  ((X) == IEEE_128_EXPO)


/* Values representing the different classes of IEEE values.		      */

#define FP_SGI_NAN           0
#define FP_SGI_INFINITE      1
#define FP_SGI_NORMAL        2
#define FP_SGI_SUBNORMAL     3
#define FP_SGI_ZERO          4

union  ieee_real_4 { 
                        long_type integer_form;
                        struct {
# ifdef _TARGET64
                                 Uint  UNUSED     : 32;
# endif
                                 Uint  sign       : 1;
                                 Uint  exponent   : IEEE_32_EXPO_BITS;
                                 Uint  mantissa   : IEEE_32_MANT_BITS;
                               } parts;
                      };

typedef union ieee_real_4 ieee_real_4_type;

union  ieee_real_8 { 
                         long_type  integer_array[MAX_WORDS_FOR_INTEGER];
                         struct { Uint  sign         : 1;
                                  Uint  exponent     : IEEE_64_EXPO_BITS;
                                  Uint  mantissa_u   : IEEE_64_MANTU_BITS;
                                  Uint  mantissa_l   : IEEE_64_MANTL_BITS;
                                } parts;
                    };

typedef union ieee_real_8 ieee_real_8_type;

union  ieee_real_16 { 
# ifdef _TARGET64
                          long_type  integer_array[2];
# else
                          long_type  integer_array[4];
# endif
                          struct { Uint  sign         : 1;
                                   Uint  exponent     : IEEE_128_EXPO_BITS;
                                   Uint  mantissa_u1  : IEEE_128_MANTTU_BITS;
                                   Uint  mantissa_u2  : IEEE_128_MANTTL_BITS;
                                   Uint  mantissa_l1  : IEEE_128_MANTTL_BITS;
                                   Uint  mantissa_l2  : IEEE_128_MANTTL_BITS;
                                 } parts;
                        };

typedef union ieee_real_16 ieee_real_16_type;


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Compare an integer or real value to the value in a Constant table     *|
|*      entry according to the relational operator "opr".                     *|
|*      The incoming value and the Constant table entry are assumed to be of  *|
|*      the same type.                                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      value    : the incoming integer or real value                         *|
|*      cn_idx   : Constant table index                                       *|
|*      opr      : the comparison to be done                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      The result of the comparison.                                         *|
|*                                                                            *|
\******************************************************************************/

boolean	compare_value_to_cn(long_type	*value,
			    int		 cn_idx,
			    int	  	 opr)

{
   long_type		result[MAX_WORDS_FOR_NUMERIC];
   int			i;
   boolean		is_true 	= FALSE;
   boolean		tested_not_equal;
   int			type_idx;
   int			word_len;


   TRACE (Func_Entry,"compare_value_to_cn" , NULL);


   /* Don't use folder_driver to do EQ/NE comparisons because it's too        */
   /* expensive.                   					      */

   if (opr == Eq_Opr  ||  opr == Ne_Opr) {
      tested_not_equal = FALSE;

      word_len = TARGET_BITS_TO_WORDS(
                    storage_bit_size_tbl[TYP_LINEAR(CN_TYPE_IDX(cn_idx))]);

      for (i = 0;  i < word_len;  i++) {

         if (const_pool[CN_POOL_IDX(cn_idx) + i] != value[i]) {
            tested_not_equal = TRUE;
            break;
         }
      }

      if (opr == Eq_Opr  &&  ! tested_not_equal) {
         is_true = TRUE;
      }
      else if (opr == Ne_Opr  &&  tested_not_equal) {
         is_true = TRUE;
      }
   }
   else {
      type_idx = CG_LOGICAL_DEFAULT_TYPE;

      if (folder_driver( (char *) value,
                         CN_TYPE_IDX(cn_idx),
			 (char *) &CN_CONST(cn_idx),
                         CN_TYPE_IDX(cn_idx),
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
   }

   TRACE (Func_Exit, "compare_value_to_cn", NULL);

   return(is_true);

}  /* compare_value_to_cn */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      srch_sym_tbl searches the local name table for the identifier or      *|
|*      label contained in the identifier field of token.                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      token			token containing identifier or label to       *|
|*                              search for and length in chars of name        *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      name_idx   		local name table index where match occured    *|
|*                              or where entry should be inserted             *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      attribute table index   if found                   		      *|
|*      NULL_IDX                if not found				      *|
|*                                                                            *|
\******************************************************************************/

int srch_sym_tbl (char	*name_str,
		  int	 name_len,
                  int	*name_idx)

{
   int		idx;
   long 	tst_val;       /* result of name comparison */
      

   TRACE (Func_Entry, "srch_sym_tbl", name_str);

   /* This is a name table search utility routine */

  tst_val = srch_name_tbl(name_str, 
                          name_len,
                          &idx,
                          loc_name_tbl,
                          name_pool,
                          SCP_LN_FW_IDX(curr_scp_idx),
                          SCP_LN_LW_IDX(curr_scp_idx));
   *name_idx = idx;

   if (tst_val != 0) {
      idx = NULL_IDX;
      TRACE (Func_Exit, "srch_sym_tbl", NULL);
   }  
   else {
      TRACE (Func_Exit, "srch_sym_tbl", LN_NAME_PTR(*name_idx));
      idx = LN_ATTR_IDX(*name_idx);
   }
   return (idx);
 
}  /* srch_sym_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ntr_sym_tbl adds the token name to the the name pool, links it        *|
|*      to an attribute table entry through the local name table, and         *|
|*      reserves an attribute table entry for the identifier or label.        *|
|*      The attribute table entry field name_idx is linked to the name in     *|
|*      the name pool.                                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      token                   token containing identifier or label and      *|
|*                              length of name to be added to symbol table    *|
|*                                                                            *|
|*      name_idx                local name table index where entry is to      *|
|*                              be inserted                                   *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      attribute table index of reserved entry                               *|
|*                                                                            *|
\******************************************************************************/

int ntr_sym_tbl(token_type *token,
                int         name_idx)

{
   register int 	 attr_idx;
   register int          i;
   register int	         np_idx;  
   register int		 scp_idx;

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   register long        *name_tbl_base; /* name table base address */
# endif


   TRACE (Func_Entry, "ntr_sym_tbl", TOKEN_STR(*token));

# if defined(_DEBUG)

   if (TOKEN_LEN(*token) == 0 || TOKEN_STR(*token) == NULL) {
      PRINTMSG(stmt_start_line, 1200, Internal, stmt_start_col);
   }

# endif

   TBL_REALLOC_CK(loc_name_tbl, 1);		/* add local name table entry */

   NTR_NAME_POOL((long *) TOKEN_STR(*token), TOKEN_LEN(*token), np_idx);

   /* reserve attribute table entry and fill in common definition fields */

   NTR_ATTR_TBL(attr_idx);
   AT_DEF_LINE(attr_idx)	= TOKEN_LINE(*token);
   AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(*token);
   AT_NAME_LEN(attr_idx)	= TOKEN_LEN(*token);
   AT_NAME_IDX(attr_idx)	= np_idx;

   if ((loc_name_tbl_idx - 1) != SCP_LN_LW_IDX(curr_scp_idx)) {

      /* Attempting to enter name into a scope that does not reside at the    */
      /* end of the local name table.  Make room for this entry in that scope */
      /* and then adjust the other scopes name table LW and FW values.        */

      for (scp_idx = 1; scp_idx <= scp_tbl_idx; scp_idx++) {

         if (SCP_LN_FW_IDX(scp_idx) > SCP_LN_LW_IDX(curr_scp_idx)) {
            SCP_LN_FW_IDX(scp_idx) = SCP_LN_FW_IDX(scp_idx) + 1;
            SCP_LN_LW_IDX(scp_idx) = SCP_LN_LW_IDX(scp_idx) + 1;
         }
      }
      SCP_LN_LW_IDX(curr_scp_idx)++;
   }
   else {
   
      /* Adding to local name table for last (most recent) scope.  No        */
      /* adjusting of other scope local name table entries is necessary.     */

      SCP_LN_LW_IDX(curr_scp_idx)	= loc_name_tbl_idx;
   }

   /* Enter name in correct position.  Link name pool and attribute table */

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   name_tbl_base = (long *) loc_name_tbl;
# endif

#  pragma _CRI ivdep
   for (i = loc_name_tbl_idx; i >= name_idx; i--) {
# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
      name_tbl_base [i] = name_tbl_base [i-1];
# else
      loc_name_tbl [i]  = loc_name_tbl [i-1];
# endif
   }

   CLEAR_TBL_NTRY(loc_name_tbl, name_idx);
   LN_ATTR_IDX(name_idx)	= attr_idx;
   LN_NAME_IDX(name_idx)	= np_idx;
   LN_NAME_LEN(name_idx)	= TOKEN_LEN(*token);

   TRACE (Func_Exit, "ntr_sym_tbl", TOKEN_STR(*token));

   return (attr_idx);

}  /* ntr_sym_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      srch_host_sym_tbl searches the name tables of all hosts for the       *|
|*	identifier or label contained in the identifier field of token.       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      token			token containing identifier or label to       *|
|*                              search for and length in chars of name        *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      name_idx   		local name table index where match occured    *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      attribute table index   if found                   		      *|
|*      NULL_IDX                if not found				      *|
|*      -1                      if found but name not visible in this scope   *|
|*                              due to multiple USE association               *|
|*                                                                            *|
|*									      *|
\******************************************************************************/

int srch_host_sym_tbl (char	*name_str,
		       int	 name_len,
                       int	*name_idx,
                       boolean   search_intrin_scp)

{

   int		 idx		= NULL_IDX;
   int		 save_scp_idx;
   int		 search_range;
   
   TRACE (Func_Entry, "srch_host_sym_tbl", NULL);

   /* DO NOT search the host when processing an interface block */

   save_scp_idx	= curr_scp_idx;

   if (search_intrin_scp) { 
      search_range = 0;
   }
   else {
      search_range = 1;
   } 

   if (SCP_IS_INTERFACE(curr_scp_idx)
#ifdef KEY /* Bug 11741 */
     /* Do search the host when processing an interface body which contains
      * an IMPORT statement without an identifier list */
     && ! SCP_IMPORT(curr_scp_idx)
#endif /* KEY Bug 11741 */
   ) {
      curr_scp_idx = 1;  /* search intrinsics */
   }

   while (idx == NULL_IDX && curr_scp_idx != search_range) {

      /* Set current scope to parent, for searching. */

      curr_scp_idx = SCP_PARENT_IDX(curr_scp_idx);
      idx = srch_sym_tbl (name_str, name_len, name_idx);
   }

   curr_scp_idx	= save_scp_idx;

   TRACE (Func_Exit, "srch_host_sym_tbl", NULL);

    return (idx);

}  /* srch_host_sym_tbl */
#ifdef KEY /* Bug 11741 */
/* Like srch_host_sym_tbl, but suitable for use by "IMPORT <id-list>" stmt;
 * and name_idx is allowed to be null. */
int
srch_host_sym_tbl_for_import(char *name_str, int name_len, int *name_idx)
{
   int save_scp_idx = curr_scp_idx;
   int idx = NULL_IDX;
   int dummy_name_idx;
   int *dummy_name_idx_p = name_idx ? name_idx : &dummy_name_idx;
   while (idx == NULL_IDX && curr_scp_idx != 1) {
      curr_scp_idx = SCP_PARENT_IDX(curr_scp_idx);
      idx = srch_sym_tbl (name_str, name_len, dummy_name_idx_p);
   }
   curr_scp_idx = save_scp_idx;
   return idx;
}

/*
 * Try to import from the host an attribute to take the place of a local
 * attribute which has been created but is not yet defined. This is useful
 * when processing:
 *
 *   type(t) function()
 *     import [ t ]
 *
 * If possible, we bash the local attribute so its AT_ATTR_LINK points to
 * the host's attribute.
 *
 * name		name of entity, padded suitably for sym_tbl searching (e.g.
 *		it's good if the name lies inside a token_type)
 * name_len	length of name
 * host_name_idx	if actual arg is not null, it is set to the host name
 *		index
 * local_attr_idx	AT_Tbl_Idx for local attribute
 * return	AT_Tbl_Idx for host attribute corresponding to local attribute,
 *		or NULL_IDX if not found
 */
int
import_from_host(char *name, int name_len, int *host_name_idx,
  int local_attr_idx) {
  int host_attr_idx = srch_host_sym_tbl_for_import(name, name_len,
    host_name_idx);
  if (host_attr_idx) {
    AT_ATTR_LINK(local_attr_idx) = host_attr_idx;
    AT_DEFINED(local_attr_idx) = AT_DEFINED(host_attr_idx);
    AT_LOCKED_IN(local_attr_idx) = TRUE;
  }
  return host_attr_idx;
}
#endif /* KEY Bug 11741 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ntr_host_in_sym_tbl adds an existing name to the local scope.         *|
|*      It makes a local name table entry, and links it to a new attr.        *|
|*      The name must already be in the name pool.                            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      token                   token containing identifier or label and      *|
|*                              length of name to be added to symbol table    *|
|*                                                                            *|
|*      name_idx                local name table index where entry is to      *|
|*                              be inserted                                   *|
|*                                                                            *|
|*      host_attr_idx           Index to attr entry in the host.              *|
|*                                                                            *|
|*      host_ln_idx             Index to local name entry in the host.        *|
|*                                                                            *|
|*      make_new_attr_and_link  FALSE means link new local entry directly to  *|
|*         			the new local name entry.  TRUE means make a  *|
|*         			new entry and use AT_ATTR_LINK to connect     *|
|*         			the old and new attr entries.  new -> old     *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      attribute table index of reserved entry                               *|
|*                                                                            *|
\******************************************************************************/

int ntr_host_in_sym_tbl(token_type     *token,
                	int		name_idx,
			int		host_attr_idx,
			int		host_ln_idx,
			boolean		make_new_attr_and_link)

{
   register int 	 attr_idx;
   register int		 i;
   register int		 scp_idx;

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   register long        *name_tbl_base; /* name table base address */
# endif


   TRACE (Func_Entry, "ntr_host_in_sym_tbl", TOKEN_STR(*token));


   /* If we need a new attr - reserve it - and link them together. */

   if (make_new_attr_and_link) {
      NTR_ATTR_TBL(attr_idx);
      AT_DEF_LINE(attr_idx)	= TOKEN_LINE(*token);
      AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(*token);
      AT_NAME_LEN(attr_idx)	= AT_NAME_LEN(host_attr_idx);
      AT_NAME_IDX(attr_idx)	= AT_NAME_IDX(host_attr_idx);
      AT_ATTR_LINK(attr_idx)	= host_attr_idx;
   }
   else {
      attr_idx			= host_attr_idx;
   }

   TBL_REALLOC_CK(loc_name_tbl, 1);		/* add local name table entry */

   if ((loc_name_tbl_idx - 1) != SCP_LN_LW_IDX(curr_scp_idx)) {

      /* Attempting to enter name into a scope that does not reside at the    */
      /* end of the local name table.  Make room for this entry in that scope */
      /* and then adjust the other scopes name table LW and FW values.        */

      for (scp_idx = 1; scp_idx <= scp_tbl_idx; scp_idx++) {

         if (SCP_LN_FW_IDX(scp_idx) > SCP_LN_LW_IDX(curr_scp_idx)) {
            SCP_LN_FW_IDX(scp_idx) = SCP_LN_FW_IDX(scp_idx) + 1;
            SCP_LN_LW_IDX(scp_idx) = SCP_LN_LW_IDX(scp_idx) + 1;
         }
      }
      SCP_LN_LW_IDX(curr_scp_idx)++;
   }
   else {
   
      /* Adding to local name table for last (most recent) scope.  No        */
      /* adjusting of other scope local name table entries is necessary.     */

      SCP_LN_LW_IDX(curr_scp_idx)	= loc_name_tbl_idx;
   }

   /* Enter name in correct position.  Link name pool and attribute table.  */

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   name_tbl_base = (long *) loc_name_tbl;
# endif

#  pragma _CRI ivdep
   for (i = loc_name_tbl_idx; i >= name_idx; i--) {
# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
      name_tbl_base [i] = name_tbl_base [i-1];
# else
      loc_name_tbl [i]  = loc_name_tbl [i-1];
# endif
   }

   CLEAR_TBL_NTRY(loc_name_tbl, name_idx);
   LN_ATTR_IDX(name_idx)	= attr_idx;
   LN_NAME_IDX(name_idx)	= LN_NAME_IDX(host_ln_idx);
   LN_NAME_LEN(name_idx)	= LN_NAME_LEN(host_ln_idx);

   TRACE (Func_Exit, "ntr_host_in_sym_tbl", TOKEN_STR(*token));

   return (attr_idx);

}  /* ntr_host_in_sym_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      name_idx                local name table index to remove.             *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void remove_ln_ntry(int         name_idx)

{
   register int          i;

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   register long        *name_tbl_base; /* name table base address */
# endif


   TRACE (Func_Entry, "remove_ln_ntry", NULL);

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   name_tbl_base = (long *) loc_name_tbl;
# endif

   /* Remove name */

#  pragma _CRI ivdep
   for (i = name_idx; i < SCP_LN_LW_IDX(curr_scp_idx); i++) {
# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
      name_tbl_base [i] = name_tbl_base [i+1];
# else
      loc_name_tbl [i]  = loc_name_tbl [i+1];
# endif
   }

   if (loc_name_tbl_idx == SCP_LN_LW_IDX(curr_scp_idx)) {
      loc_name_tbl_idx--;
   }

   SCP_LN_LW_IDX(curr_scp_idx)--;

   TRACE (Func_Exit, "remove_ln_ntry", NULL);

   return;

}  /* remove_ln_ntry */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      srch_kwd_name searches the secondary name table entries for the darg  *|
|*      names of an explicit interface.                                       *|
|*                                                                            *|
|*      THIS ROUTINE IS ONLY FOR USE WITH DUMMY ARGUMENT LISTS.               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*	name			Char pointer of name to look for.             *|
|*      length			Length of name to look for.                   *|
|*      attr_idx                index of the proc with the dargs to search.   *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      sn_idx                  secondary name table index if found           *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      attribute table index of member if found                              *|
|*      NULL_IDX		        if not found                          *|
|*                                                                            *|
\******************************************************************************/
int srch_kwd_name(char		*name,
		  int		 length,
		  int		 attr_idx,
		  int		*sn_idx)

{
   register int          i;
   register int          id_char_len;   /* character length of identifier */
   register int          id_wd_len;     /* word length of identifier */
#ifdef KEY /* Bug 10177 */
   register int          num_dargs = 0;
#else /* KEY Bug 10177 */
   register int          num_dargs;
#endif /* KEY Bug 10177 */
   register int          np_idx;
   register long        *id;
   register long         tst_val;
   register long        *sn_tbl_base;



   TRACE (Func_Entry, "srch_kwd_name", name);

   if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
       num_dargs	= ATP_NUM_DARGS(attr_idx);
      *sn_idx		= ATP_FIRST_IDX(attr_idx);
   }
   else if (AT_OBJ_CLASS(attr_idx) == Stmt_Func) {
       num_dargs	= ATP_NUM_DARGS(attr_idx);
      *sn_idx		= ATP_FIRST_IDX(attr_idx);
   }
   else {
      PRINTMSG(stmt_start_line, 136, Internal, stmt_start_col, "srch_kwd_name");
   }
   
   id		= (long *) name;
   id_char_len	= length;
   id_wd_len	= WORD_LEN(id_char_len);      

   /* don't forward sub else no vector */

   tst_val	= -1;
   sn_tbl_base  = (long *) sec_name_tbl;

# if defined(_HOST_LITTLE_ENDIAN)

   for (i = 0; i < num_dargs; i++) {
      np_idx = SN_NP_IDX(*sn_idx + i);  

      if (SN_LEN(*sn_idx + i) == id_char_len) {
         tst_val = compare_names(&id[0],
                                 id_wd_len*HOST_BYTES_PER_WORD-1,
                                 &name_pool[np_idx].name_long,
                                 id_wd_len*HOST_BYTES_PER_WORD-1);
         if (tst_val == 0) {
            break;
         }
      }
   }

# else

   switch (id_wd_len) {
      case 1:
#        pragma _CRI ivdep
         for (i = 0; i < num_dargs; i++) {
            np_idx = SN_NP_IDX(*sn_idx + i);  

            if (SN_LEN(*sn_idx + i) == id_char_len) {
               tst_val = id[0] - name_pool[np_idx].name_long;

               if (tst_val == 0) {
                  break;
               } 
            }
         }
         break;

      case 2:
#        pragma _CRI ivdep

         for (i = 0; i < num_dargs; i++) {
            np_idx = SN_NP_IDX(*sn_idx + i);  

            if (SN_LEN(*sn_idx + i) == id_char_len) {
               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long);

               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;

      case 3:
#        pragma _CRI ivdep

         for (i = 0; i < num_dargs; i++) {
            np_idx = SN_NP_IDX(*sn_idx + i);  

            if (SN_LEN(*sn_idx + i) == id_char_len) {
               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long);

               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;

      case 4: 
#        pragma _CRI ivdep
         for (i = 0; i < num_dargs; i++) {
            np_idx = SN_NP_IDX(*sn_idx + i);  
            if (SN_LEN(*sn_idx + i) == id_char_len) {
               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long) |
                         (id[3] - name_pool[np_idx + 3].name_long);
               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;

# ifdef _HOST32
      case 5:
         for (i = 0; i < num_dargs; i++) {
            np_idx = SN_NP_IDX(*sn_idx + i);  
            if (SN_LEN(*sn_idx + i) == id_char_len) {
               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long) |
                         (id[3] - name_pool[np_idx + 3].name_long) |
                         (id[4] - name_pool[np_idx + 4].name_long);
               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;

      case 6:
         for (i = 0; i < num_dargs; i++) {
            np_idx = SN_NP_IDX(*sn_idx + i);  
            if (SN_LEN(*sn_idx + i) == id_char_len) {
               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long) |
                         (id[3] - name_pool[np_idx + 3].name_long) |
                         (id[4] - name_pool[np_idx + 4].name_long) |
                         (id[5] - name_pool[np_idx + 5].name_long);
               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;

      case 7:
         for (i = 0; i < num_dargs; i++) {
            np_idx = SN_NP_IDX(*sn_idx + i);  
            if (SN_LEN(*sn_idx + i) == id_char_len) {
               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long) |
                         (id[3] - name_pool[np_idx + 3].name_long) |
                         (id[4] - name_pool[np_idx + 4].name_long) |
                         (id[5] - name_pool[np_idx + 5].name_long) |
                         (id[6] - name_pool[np_idx + 6].name_long);
               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;

      case 8:
         for (i = 0; i < num_dargs; i++) {
            np_idx = SN_NP_IDX(*sn_idx + i);  
            if (SN_LEN(*sn_idx + i) == id_char_len) {
               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long) |
                         (id[3] - name_pool[np_idx + 3].name_long) |
                         (id[4] - name_pool[np_idx + 4].name_long) |
                         (id[5] - name_pool[np_idx + 5].name_long) |
                         (id[6] - name_pool[np_idx + 6].name_long) |
                         (id[7] - name_pool[np_idx + 7].name_long);
               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;

# endif

      default:
         PRINTMSG(stmt_start_line, 196, Internal, stmt_start_col, 
                  "srch_kwd_name",
                  NUM_ID_WDS * TARGET_CHARS_PER_WORD);
         break;
   }  /* switch (id_wd_len) */

# endif

   if (tst_val == 0) {
      TRACE (Func_Exit, "srch_kwd_name", name);
      *sn_idx = *sn_idx + i;
      i       = SN_ATTR_IDX(*sn_idx);
   }
   else {
      TRACE (Func_Exit, "srch_kwd_name", NULL);
      i = NULL_IDX;
   }

   return (i); 

}  /*  srch_kwd_name  */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      srch_stor_blk_tbl searches the local common/module table entries      *|
|*      for an entry of the same name.                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      token                   token containing common or module name        *|
|*                              and length of name                            *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      common/module table index   if found                                  *|
|*      NULL_IDX                    if not found                              *|
|*                                                                            *|
\******************************************************************************/
int srch_stor_blk_tbl (char		*name_str,
                       int		 name_len,
		       int		 scp_idx)

{
   register int          i;
   register long        *id;
   register int          id_char_len;   /* character length of identifier */
   register int          id_wd_len;     /* word length of identifier */
   register int          j;
   register int          np_idx;
   register long         tst_val;


   TRACE (Func_Entry, "srch_stor_blk_tbl", name_str);

   id          = (long *) name_str;
   id_char_len = name_len;
   id_wd_len   = WORD_LEN(id_char_len);      
   tst_val     = -1;

# if defined(_HOST_LITTLE_ENDIAN)

   for (i = 1; i <= stor_blk_tbl_idx; i++) {
      np_idx = SB_NAME_IDX(i);  

      if (SB_NAME_LEN(i) == id_char_len &&
          SB_SCP_IDX(i) == scp_idx && !SB_HIDDEN(i)) {

         tst_val = compare_names(&id[0],
                                 id_wd_len*HOST_BYTES_PER_WORD-1,
                                 &name_pool[np_idx].name_long,
                                 id_wd_len*HOST_BYTES_PER_WORD-1);

         if (tst_val == 0) {
            break;
         }
      }
   }
# else

   switch (id_wd_len) {
      case 1:
#        pragma _CRI ivdep
         for (i = 1; i <= stor_blk_tbl_idx; i++) {
            np_idx = SB_NAME_IDX(i);  

            if (SB_NAME_LEN(i) == id_char_len &&
                SB_SCP_IDX(i) == scp_idx && !SB_HIDDEN(i)) {

               tst_val = id[0] - name_pool[np_idx].name_long;

               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;

      case 2:
#        pragma _CRI ivdep
         for (i = 1; i <= stor_blk_tbl_idx; i++) {
            np_idx = SB_NAME_IDX(i);  

            if (SB_NAME_LEN(i) == id_char_len &&
                SB_SCP_IDX(i) == scp_idx && !SB_HIDDEN(i)) {

               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long);

               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;

      case 3:
#        pragma _CRI ivdep
         for (i = 1; i <= stor_blk_tbl_idx; i++) {
            np_idx = SB_NAME_IDX(i);  

            if (SB_NAME_LEN(i) == id_char_len &&
                SB_SCP_IDX(i) == scp_idx && !SB_HIDDEN(i)) {

               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long);

               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;

      case 4: 
#        pragma _CRI ivdep
         for (i = 1; i <= stor_blk_tbl_idx; i++) {
            np_idx = SB_NAME_IDX(i);  

            if (SB_NAME_LEN(i) == id_char_len &&
                SB_SCP_IDX(i) == scp_idx && !SB_HIDDEN(i)) {

               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long) |
                         (id[3] - name_pool[np_idx + 3].name_long);

               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;

# ifdef _HOST32
      case 5:
#        pragma _CRI ivdep
         for (i = 1; i <= stor_blk_tbl_idx; i++) {
            np_idx = SB_NAME_IDX(i);

            if (SB_NAME_LEN(i) == id_char_len &&
                SB_SCP_IDX(i) == scp_idx && !SB_HIDDEN(i)) {

               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long) |
			 (id[3] - name_pool[np_idx + 3].name_long) |
                         (id[4] - name_pool[np_idx + 4].name_long);

               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;
      case 6:
#        pragma _CRI ivdep
         for (i = 1; i <= stor_blk_tbl_idx; i++) {
            np_idx = SB_NAME_IDX(i);

            if (SB_NAME_LEN(i) == id_char_len &&
                SB_SCP_IDX(i) == scp_idx && !SB_HIDDEN(i)) {

               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long) |
                         (id[3] - name_pool[np_idx + 3].name_long) |
                         (id[4] - name_pool[np_idx + 4].name_long) |
                         (id[5] - name_pool[np_idx + 5].name_long);

               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;
      case 7:
#        pragma _CRI ivdep
         for (i = 1; i <= stor_blk_tbl_idx; i++) {
            np_idx = SB_NAME_IDX(i);

            if (SB_NAME_LEN(i) == id_char_len &&
                SB_SCP_IDX(i) == scp_idx && !SB_HIDDEN(i)) {

               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long) |
                         (id[3] - name_pool[np_idx + 3].name_long) |
                         (id[4] - name_pool[np_idx + 4].name_long) |
                         (id[5] - name_pool[np_idx + 5].name_long) |
                         (id[6] - name_pool[np_idx + 6].name_long);

               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;
      case 8:
#        pragma _CRI ivdep
         for (i = 1; i <= stor_blk_tbl_idx; i++) {
            np_idx = SB_NAME_IDX(i);

            if (SB_NAME_LEN(i) == id_char_len &&
                SB_SCP_IDX(i) == scp_idx && !SB_HIDDEN(i)) {

               tst_val = (id[0] - name_pool[np_idx    ].name_long) |
                         (id[1] - name_pool[np_idx + 1].name_long) |
                         (id[2] - name_pool[np_idx + 2].name_long) |
                         (id[3] - name_pool[np_idx + 3].name_long) |
                         (id[4] - name_pool[np_idx + 4].name_long) |
                         (id[5] - name_pool[np_idx + 5].name_long) |
                         (id[6] - name_pool[np_idx + 6].name_long) |
                         (id[7] - name_pool[np_idx + 7].name_long);

               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;
# endif

      default:

         for (i = 1; i <= stor_blk_tbl_idx; i++) {
            np_idx = SB_NAME_IDX(i);  

            if (SB_NAME_LEN(i) == id_char_len &&
                SB_SCP_IDX(i) == scp_idx && !SB_HIDDEN(i)) {
               tst_val = 0;

#        pragma _CRI ivdep
               for (j = 0; j < id_wd_len; j++) {
                  tst_val = tst_val | (id[j] - name_pool[np_idx+j].name_long);
               }

               if (tst_val == 0) {
                  break;
               }
            }
         }
         break;
   }  /* switch (id_wd_len) */

# endif

   if (tst_val != 0) {
      i = NULL_IDX;
   }

   TRACE (Func_Exit, "srch_stor_blk_tbl", NULL);

   return (i); 

}  /* srch_stor_blk_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ntr_stor_blk_tbl makes a storage block table entry and name pool      *|
|*      entry, filling in the name index and name length fields of the        *|
|*      storage block table entry.                                            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      storage block name                                                    *|
|*      length of name                                                        *|
|*      defining line of storage block name                                   *|
|*      defining column of storage block name                                 *|
|*      type of block for SB_BLK_TYPE                                         *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      common/module table index where entry is made                         *|
|*                                                                            *|
\******************************************************************************/

int ntr_stor_blk_tbl (char *name_str,
                      int   name_len,
		      int   def_line,
		      int   def_column,
		      int   blk_type)
 
{
   register int	         np_idx;  


   TRACE (Func_Entry, "ntr_stor_blk_tbl", name_str);

   NTR_NAME_POOL((long *) name_str, name_len, np_idx);

   TBL_REALLOC_CK(stor_blk_tbl, 1);

   CLEAR_TBL_NTRY(stor_blk_tbl, stor_blk_tbl_idx);

   SB_NAME_LEN(stor_blk_tbl_idx)	= name_len;
   SB_NAME_IDX(stor_blk_tbl_idx)	= np_idx;
   SB_DEF_LINE(stor_blk_tbl_idx)	= def_line;
   SB_DEF_COLUMN(stor_blk_tbl_idx)	= def_column;
   SB_SCP_IDX(stor_blk_tbl_idx)		= curr_scp_idx;
   SB_ORIG_SCP_IDX(stor_blk_tbl_idx)	= curr_scp_idx;
#ifdef KEY /* Bug 4630 */
   /* Because the length of a common block is expressed in bits, Integer_4 is
    * not adequate in -m32 mode. Not clear whether other entities besides
    * common block may have the same problem, but we want a safe limited fix.
    */
   if (Common == (sb_type_type) blk_type) {
     SB_LEN_IDX(stor_blk_tbl_idx)		= C_INT_TO_CN(Integer_8, 0);
   } else
#endif /* KEY Bug 4630 */
   SB_LEN_IDX(stor_blk_tbl_idx)		= CN_INTEGER_ZERO_IDX;
   SB_LEN_FLD(stor_blk_tbl_idx)		= CN_Tbl_Idx;
   SB_BLK_TYPE(stor_blk_tbl_idx)	= (sb_type_type) blk_type;

   switch (blk_type) {
      case Common:
      case Task_Common:
      case Threadprivate:
         SB_IS_COMMON(stor_blk_tbl_idx)		= TRUE;
         SB_RUNTIME_INIT(stor_blk_tbl_idx)	= FALSE;
         break;

      case Coment:
      case Static:
      case Static_Named:
      case Static_Local:
         SB_RUNTIME_INIT(stor_blk_tbl_idx)	= FALSE;
         break;

      case Stack:
      case Formal:
      case Based:
      case Equivalenced:
      case Non_Local_Stack:
      case Non_Local_Formal:
      case Hosted_Stack:
      case Auxiliary:
         SB_RUNTIME_INIT(stor_blk_tbl_idx)	= TRUE;
         break;

# if defined(_DEBUG)
      case Unknown_Seg:
      case Extern:
      case Exported:
      case Soft_External:
      case Global_Breg:
      case Global_Treg:
      case Restricted:
      case Distributed:
      case LM_Static:
      case LM_Common:
      case LM_Extern:

         /* Intentional fall through */

      default:
         PRINTMSG(def_line, 1592, Internal, def_column);
         break;
# endif
   }  /* End switch */

   TRACE (Func_Exit, "ntr_stor_blk_tbl", NULL);

   return (stor_blk_tbl_idx); 

}  /* ntr_stor_blk_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ntr_array_in_bd_tbl makes an array table entry.   It is copied from   *|
|*      the array table work area.  (Entries 0-7 of array table)              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      array_idx       Index to start of new array entry.     		      *|
|*                                                                            *|
\******************************************************************************/

int ntr_array_in_bd_tbl(int	bd_idx)
 
{
   int	free_idx;
   int	free_size;
   int	size;


   TRACE (Func_Entry, "ntr_array_in_bd_tbl", NULL);

   /* NOTE:  Deferred shape arrays share entries.  Also, array definitions  */
   /*        that are in the same statement can share bounds.  For example: */
   /*             INTEGER, DIMENSION(100) : A,B,C                           */
   /*        If there is nothing that changes these declarations, the same  */
   /*        bounds table entry will be used for A, B and C.                */

   if (BD_ARRAY_CLASS(bd_idx) == Deferred_Shape) {

      if (!BD_DCL_ERR(bd_idx)) {
         BD_LINE_NUM(BD_RANK(bd_idx))	= BD_LINE_NUM(bd_idx);
         BD_COLUMN_NUM(BD_RANK(bd_idx))	= BD_COLUMN_NUM(bd_idx);
         free_idx			= bd_idx;
         free_size			= BD_NTRY_SIZE(bd_idx);
         bd_idx				= BD_RANK(bd_idx);
      }
      else {
         free_size			= BD_NTRY_SIZE(bd_idx) - 1;
         free_idx			= bd_idx + 1;
         BD_USED_NTRY(bd_idx)		= TRUE;
         BD_NTRY_SIZE(bd_idx)		= 1;
      }
   }
   else {
      size				= BD_RANK(bd_idx) + 1;	
      free_size				= BD_NTRY_SIZE(bd_idx) - size;
      free_idx				= bd_idx + size;
      BD_USED_NTRY(bd_idx)		= TRUE;
      BD_NTRY_SIZE(bd_idx)		= size;
   }

   if (free_size > 0) {

      if ((free_idx + free_size - 1) == bounds_tbl_idx) {
         bounds_tbl_idx -= free_size;
      }
      else {
         BD_NEXT_FREE_NTRY(free_idx) = BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX);
         BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX)	= free_idx;
         BD_NTRY_SIZE(free_idx)			= free_size;
         BD_USED_NTRY(free_idx)			= FALSE;
      }
   }

   TRACE (Func_Exit, "ntr_array_in_bd_tbl", NULL);

   return(bd_idx);

}  /* ntr_array_in_bd_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      reserve_array_ntry finds an unused entry in the bounds table to hold  *|
|*              an array of the specified rank.                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      size	 -> Rank of array.                                            *|
|*      deferred -> True if this is definitely a deferred shape array.        *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      bd_idx	-> Index to start of new array entry.     		      *|
|*                                                                            *|
\******************************************************************************/

int reserve_array_ntry (int	rank)
 
{
   int		 bd_idx;
   int		 i;
   int		 size;
   long		*tbl_idx;


   TRACE (Func_Entry, "reserve_array_ntry", NULL);

   size		= ++rank;
   bd_idx	= BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX);

   while (bd_idx != NULL_IDX && size > BD_NTRY_SIZE(bd_idx)) {
      bd_idx	= BD_NEXT_FREE_NTRY(bd_idx);
   }

   if (bd_idx == NULL_IDX) {
      bd_idx = bounds_tbl_idx + 1;
      TBL_REALLOC_CK(bounds_tbl, size);     /* Get space for whole thing */
   }
   else if (BD_NTRY_SIZE(bd_idx) > size) {
      BD_NTRY_SIZE(bd_idx)	= BD_NTRY_SIZE(bd_idx) - size;
      bd_idx			= size + bd_idx;
   }
   else {
      BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX) = BD_NEXT_FREE_NTRY(bd_idx);
   }

   tbl_idx = ((long *) (&bounds_tbl[bd_idx]));

   for (i = 0; i < NUM_BD_WDS * size; i++) {
      *(tbl_idx) = 0;
      tbl_idx++;
   }

   BD_NTRY_SIZE(bd_idx)	= size;
   BD_USED_NTRY(bd_idx)	= TRUE;

   TRACE (Func_Exit, "reserve_array_ntry", NULL);

   return(bd_idx);

}  /* reserve_array_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Initialize symbol table.  Called once per compilation.                *|
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

void	init_sytb()

{

   TRACE (Func_Entry, "init_sytb", NULL);

/*    Check to make sure that the following table definitions are correct */

# ifdef _DEBUG
   if (sizeof(attr_list_tbl_type) != (NUM_AL_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "Attribute list table");
   }

   if (sizeof(attr_tbl_type) != (NUM_AT_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "Attribute table");
   }

   if (sizeof(bounds_tbl_type) != (NUM_BD_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "Bounds table");
   }

   if (sizeof(file_path_tbl_type) != (NUM_FP_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "File path table");
   }

   if (sizeof(loc_name_tbl_type) != (NUM_LN_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "Local name table");
   }

   if (sizeof(mod_link_tbl_type) != (NUM_ML_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "Module link table");
   }


   if (sizeof(scp_tbl_type) != (NUM_SCP_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "Scope table");
   }

   if (sizeof(pdg_link_tbl_type) != (NUM_PDG_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "Pdg link table");
   }

   if (sizeof(stor_blk_tbl_type) != (NUM_SB_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "Storage block table");
   }

   if (sizeof(sec_name_tbl_type) != (NUM_SN_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "Secondary name table");
   }

   if (sizeof(ir_tbl_type) != (NUM_IR_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "IR table");
   }

   if (sizeof(ir_list_tbl_type) != (NUM_IL_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "IR list table");
   }

   if (sizeof(sh_tbl_type) != (NUM_SH_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "statement header table");
   }

   if (sizeof(rename_only_tbl_type) != (NUM_RO_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "rename only table");
   }

   if (sizeof(type_tbl_type) != (NUM_TYP_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "type table");
   }

   if (sizeof(global_line_tbl_type) != (NUM_GL_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "global line table");
   }

   if (sizeof(global_name_tbl_type) != (NUM_GN_WDS * HOST_BYTES_PER_WORD)) {
      PRINTMSG(1, 138, Internal, 0, "global name table");
   }
# endif

   /* Create this token.  It's used in case of error in the program, module   */
   /* blockdata, function, or subroutine statement.  At the beginning of the  */
   /* parser, $MAIN is put into the name pool, but not into the local name tbl*/
   /* It only gets entered into the local name table if it's needed because   */
   /* of a missing program statement.  This is done in the parse driver.      */

   CREATE_ID(TOKEN_ID(main_token), 
             UNNAMED_PROGRAM_NAME, 
             UNNAMED_PROGRAM_NAME_LEN);

   TOKEN_LEN(main_token)		= UNNAMED_PROGRAM_NAME_LEN;
   TOKEN_LINE(main_token)		= 1;
   TOKEN_COLUMN(main_token)		= 1;
   TOKEN_VALUE(main_token)		= Tok_Id;
   TOKEN_KIND_STR(main_token)[0]	= EOS;
   TOKEN_KIND_LEN(main_token)		= 0;

   /* Initialize here for debug variant checking */

   stmt_start_line	= 1;
   stmt_start_col	= 1;

   TRACE (Func_Exit, "init_sytb", NULL);

   return;

}  /* init_sytb */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ntr_const_tbl add non-character constants to the constant table.  It  *|
|*	searches for a match and returns the matching index if found.  If not *|
|*      found it adds the constant and returns the new index.                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      type_idx	data type.                                            *|
|*      extra_zero_word add an extra word to constant and set flag.           *|
|*      constant	The address of the constant to be entered.            *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      constant table index of entry                                         *|
|*                                                                            *|
\******************************************************************************/

int ntr_const_tbl (int		 type_idx,
		   boolean	 extra_zero_word,
                   long_type	*constant)

{
   register	int			const_idx;
#ifdef KEY /* Bug 10177 */
   		long64			const_word_len = 0;
   register	int			i;
   		long64			input_word_len = 0;
#else /* KEY Bug 10177 */
   		long64			const_word_len;
   register	int			i;
   		long64			input_word_len;
#endif /* KEY Bug 10177 */
   		size_offset_type	length;
   register	int			pool_idx;
   		int			num_long_types;


/* NOTE:  Although it is impossible to have a native compiler under the MPP   */
/*        MAX operating system, we use the perhaps unfortunate name	      */
/*        _HOST_OS_MAX also on UNICOS/mk MPP systems to indicate the host is  */
/*        an MPP.							      */

#if (defined(_HOST_OS_UNICOS)   &&  defined(_TARGET_OS_UNICOS))   ||           \
    (defined(_HOST_OS_MAX)      &&  defined(_TARGET_OS_MAX))      ||           \
    (defined(_HOST_OS_SOLARIS)  &&  defined(_TARGET_OS_SOLARIS))  ||           \
    ((defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))      &&  (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)))

   /* ASSUMPTION:  long and float occupy the same number of bits on PVP and   */
   /*              Solaris machines.  					      */
   /*              long and double occupy the same number of bits (64) on     */
   /*              MPPs.						      */

   union  integer_and_real	{ long		integer_form;

#ifdef _TARGET_OS_MAX
   		  	  	  double	real_form;
#else
   		  	  	  float		real_form;
#endif
                	};

   union  integer_and_real  value;   /* BRIANJ - never used */
   union  integer_and_real  high_cn;   /* BRIANJ - never used */
   union  integer_and_real  low_cn;   /* BRIANJ - never used */
   union  integer_and_real  mid_cn;  /* BRIANJ - never used */

#endif


   TRACE (Func_Entry, "ntr_const_tbl", NULL);

   switch(TYP_TYPE(type_idx)) {

   case Typeless:

      input_word_len = STORAGE_WORD_SIZE(TYP_BIT_LEN(type_idx));

      if (extra_zero_word || (input_word_len == 0)) {
         const_word_len		= input_word_len + 1;
         extra_zero_word	= TRUE;
      }
      else {
         const_word_len		= input_word_len;
      }
      break;


   case Character:

      input_word_len = TARGET_BYTES_TO_WORDS(((long)
                               CN_INT_TO_C(TYP_IDX(type_idx))));

      if (extra_zero_word || (input_word_len == 0)) {
         const_word_len		= input_word_len + 1;
         extra_zero_word	= TRUE;
      }
      else {
         const_word_len		= input_word_len;
      }
      break;


   case Integer: 
   case Real:
   case Logical:

      const_word_len =
         TARGET_BITS_TO_WORDS(storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
      input_word_len = const_word_len;
      break;


   case Complex:

      const_word_len =
         TARGET_BITS_TO_WORDS(storage_bit_size_tbl[TYP_LINEAR(type_idx)]);

# if defined(_TARGET_OS_MAX) || defined(_WHIRL_HOST64_TARGET64)

      if (TYP_LINEAR(type_idx) == Complex_4) {
         const_word_len = 2;
      }

# endif

      input_word_len = const_word_len;
      break;


   /* Issue error - constant too big??? */

   case Structure:

      length.fld = ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));;
      length.idx = ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));;

      BITS_TO_WORDS(length, TARGET_BITS_PER_WORD);

      /* Potential range problem here */

      const_word_len = F_INT_TO_C(length.constant, TYP_LINEAR(length.type_idx));

      if (length.fld == CN_Tbl_Idx) {
         const_word_len = CN_INT_TO_C(length.idx);

         if (const_word_len == 0) {
            const_word_len	= 1;
            extra_zero_word	= TRUE;
         }
      }
      else {
         PRINTMSG(AT_DEF_LINE(TYP_IDX(type_idx)), 1201, Internal,
                  AT_DEF_COLUMN(TYP_IDX(type_idx)),
                  AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
      }

      input_word_len = const_word_len;
      break;
   }


   if (constant != NULL_IDX) {

      if (TYP_TYPE(type_idx) == Integer  ||  TYP_TYPE(type_idx) == Real) {

         /* the insert_constant routine will support comparisons */
         /* using local 'c' integer compares for both types if   */
         /* the constant's type can be viewed as a long or a     */
         /* long long on the host machine. So, if num_long_types */
         /* is a 1 or a 2, arith is not used, even for weird real*/
         /* constants (abnormals). Anything that requires a      */
         /* call to arith comparison should set num_long_types   */
         /* to 0.                                                */

         num_long_types = num_host_wds[TYP_LINEAR(type_idx)];

         /* this is where long_type is already a long long */
         /* or else long long is the same as long */

         if (sizeof(long_type) == sizeof(long long) &&
             num_long_types != 1) {
            num_long_types = 0;
         }

         if (TYP_TYPE(type_idx) == Real && 
             num_long_types != 1 &&
             num_long_types != 2) {

            if (target_ieee) {

               if (! is_normal(type_idx, constant)) {
                  const_idx = ntr_abnormal_ieee_const(type_idx,
                                                      constant);
                  goto FOUND;
               }
            }
            else {

               if (! pvp_isnormal(type_idx, constant)) {
                  const_idx = ntr_unshared_const_tbl(type_idx,
                                                     FALSE,
                                                     constant);
                  goto FOUND;
               }
            }
         }

# ifdef _DEBUG
         if (dump_flags.constant_bits) {
            long neg_one = -1;
            write(1,constant, 
                  sizeof(long_type)*num_host_wds[TYP_LINEAR(type_idx)]);
            write(1,&neg_one, 4);
         }
# endif
         const_idx = insert_constant(type_idx,
                                     constant,
                                     num_long_types);

         if (CN_POOL_IDX(const_idx) != NULL_IDX) {
            goto FOUND;
         }
         else {
            goto ATTACH_POOL_IDX;
         }
      }
      else {
         const_idx = insert_unordered_constant(type_idx,
                                               constant,
                                               input_word_len,
                                               const_word_len);

         if (CN_POOL_IDX(const_idx) != NULL_IDX) {
            goto FOUND;
         }
         else {
            goto ATTACH_POOL_IDX;
         }
      }
   }


   /* ----------------------------------------------------------------------- */
   /* Enter the incoming value into the Constant table.			      */
   /* ----------------------------------------------------------------------- */

   TBL_REALLOC_CK(const_tbl, 1);
   CLEAR_TBL_NTRY(const_tbl, const_tbl_idx);
   const_idx = const_tbl_idx;

ATTACH_POOL_IDX:

   pool_idx = const_pool_idx + 1;

#if defined(_HOST32) 

   if (DALIGN_TEST_CONDITION(type_idx)) {

      while ((((long)&const_pool[pool_idx]) % 8) != 0) {
         pool_idx++;
         const_pool_idx++;
      }
   }

#endif


   CN_POOL_IDX(const_idx) = pool_idx;

   if ((const_pool_idx += const_word_len) >= const_pool_size) {
      const_pool_size = const_pool_size + 
                        ( ( ( (const_pool_idx - const_pool_size + 1) /
                              const_pool_inc) + 1) * const_pool_inc);
      MEM_REALLOC (const_pool, const_pool_type, const_pool_size);
   }

   CN_TYPE_IDX(const_idx)	 = type_idx;
   CN_EXTRA_ZERO_WORD(const_idx) = extra_zero_word;


   /* If constant does not point to anything, then the caller will put the    */
   /* constant into the constant pool.  Otherwise copy the constant in.       */

   if (const_word_len == 0) {

      /* We don't want to write to any const_pool words for zero length.      */
      /* Intentionally blank.						      */

   }
   else if (constant != NULL_IDX) {
      const_pool[const_pool_idx] = 0L;

      if (TYP_TYPE(type_idx) == Character) {

         if (extra_zero_word) {
            const_pool[const_pool_idx - 1] = 0L;
         }

         strncpy((char *) &CN_CONST(const_idx), 
                 (char *)  constant,
                 (long)    CN_INT_TO_C(TYP_IDX(type_idx)));
      }
      else {
         for (i = 0; i < input_word_len; i++) {
            const_pool[pool_idx + i] = constant[i];
         }
      }
   }
   else {

      for (i = pool_idx;  i <= const_pool_idx;  i++) {
         const_pool[i] = 0L;
      }
   }


FOUND:



   TRACE (Func_Exit, "ntr_const_tbl", NULL);

   return (const_idx);

}  /* ntr_const_tbl */

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

static int insert_constant(int		type_idx,
			   long_type	*constant,
			   int		num_long_types)

{

   int		balance_factor;			/* d */
   int		cn_idx = NULL_IDX;		/* Y */
   int		idx = NULL_IDX;			/* P */
   int		idx_B;				/* B */
   int		idx_C;				/* C */
   int		last_unbalanced_idx;		/* A */
   int		unbalanced_parent_idx = NULL_IDX; /* F */
   int		previous_idx = NULL_IDX;	/* Q */
   int		root;
   int		matched_cn_idx = NULL_IDX;

   TRACE (Func_Entry, "insert_constant", NULL);

   root = cn_root_idx[TYP_LINEAR(type_idx)];

   if (root == NULL_IDX) {
      /* nothing in the tree yet */

      TBL_REALLOC_CK(const_tbl, 1);
      CLEAR_TBL_NTRY(const_tbl, const_tbl_idx);
      cn_idx = const_tbl_idx;

      cn_root_idx[TYP_LINEAR(type_idx)] = cn_idx;
      goto EXIT;
   }

   last_unbalanced_idx = root;
   idx = root;

   switch (num_long_types) {
      case 1:
         while (idx) {
            if (CN_BALANCE_FACTOR(idx) != 0) {
               last_unbalanced_idx = idx;
               unbalanced_parent_idx = previous_idx;
            }

            if (*constant < CN_CONST(idx)) {
               previous_idx = idx;
               idx = CN_LEFT_CHILD(idx);
            }
            else if (*constant > CN_CONST(idx)) {
               previous_idx = idx;
               idx = CN_RIGHT_CHILD(idx);
            }
            else if (type_idx < CN_TYPE_IDX(idx)) {
               /* look to LEFT */
               matched_cn_idx = idx;
               previous_idx = idx;
               idx = CN_LEFT_CHILD(idx);
            }
            else if (type_idx > CN_TYPE_IDX(idx)) {
               /* look to RIGHT */
               matched_cn_idx = idx;
               previous_idx = idx;
               idx = CN_RIGHT_CHILD(idx);
            }
            else {
               /* found it. */
               cn_idx = idx;
               goto EXIT;
            }
         }
         break;

      case 2:
         while (idx) {
            if (CN_BALANCE_FACTOR(idx) != 0) {
               last_unbalanced_idx = idx;
               unbalanced_parent_idx = previous_idx;
            }

            if (*(long long *)constant < *(long long *)&CN_CONST(idx)) {
               previous_idx = idx;
               idx = CN_LEFT_CHILD(idx);
            }
            else if (*(long long *)constant > *(long long *)&CN_CONST(idx)) {
               previous_idx = idx;
               idx = CN_RIGHT_CHILD(idx);
            }
            else if (type_idx < CN_TYPE_IDX(idx)) {
               /* always look to LEFT */
               matched_cn_idx = idx;
               previous_idx = idx;
               idx = CN_LEFT_CHILD(idx);
            }
            else if (type_idx > CN_TYPE_IDX(idx)) {
               /* always look to RIGHT */
               matched_cn_idx = idx;
               previous_idx = idx;
               idx = CN_RIGHT_CHILD(idx);
            }
            else {
               /* found it. */
               cn_idx = idx;
               goto EXIT;
            }
         }
         break;

      default:
         while (idx) {
            if (CN_BALANCE_FACTOR(idx) != 0) {
               last_unbalanced_idx = idx;
               unbalanced_parent_idx = previous_idx;
            }

            if (compare_value_to_cn(constant, idx, Lt_Opr)) {
               previous_idx = idx;
               idx = CN_LEFT_CHILD(idx);
            }
            else if (compare_value_to_cn(constant, idx, Gt_Opr)) {
               previous_idx = idx;
               idx = CN_RIGHT_CHILD(idx);
            }
            else if (type_idx < CN_TYPE_IDX(idx)) {
               /* always look to LEFT */
               matched_cn_idx = idx;
               previous_idx = idx;
               idx = CN_LEFT_CHILD(idx);
            }
            else if (type_idx > CN_TYPE_IDX(idx)) {
               /* always look to RIGHT */
               matched_cn_idx = idx;
               previous_idx = idx;
               idx = CN_RIGHT_CHILD(idx);
            }
            else {
               /* found it. */
               cn_idx = idx;
               goto EXIT;
            }
         }
         break;
   } /* switch (num_long_types) */

   /* must insert */
   TBL_REALLOC_CK(const_tbl, 1);
   CLEAR_TBL_NTRY(const_tbl, const_tbl_idx);
   cn_idx = const_tbl_idx;

   if (matched_cn_idx != NULL_IDX) {
      COPY_TBL_NTRY(const_tbl, cn_idx, matched_cn_idx);
      CN_LEFT_CHILD(cn_idx) = NULL_IDX;
      CN_RIGHT_CHILD(cn_idx) = NULL_IDX;
      CN_TYPE_IDX(cn_idx) = type_idx;
   }

   switch (num_long_types) {
      case 1:
         if (*constant > CN_CONST(previous_idx)) {
            /* insert as right child */
            CN_RIGHT_CHILD(previous_idx) = cn_idx;
         }
         else if (*constant < CN_CONST(previous_idx)) {
            /* insert as left child */
            CN_LEFT_CHILD(previous_idx) = cn_idx;
         }
         else if (type_idx > CN_TYPE_IDX(previous_idx)) {
            /* insert as right child */
            CN_RIGHT_CHILD(previous_idx) = cn_idx;
         }
         else {
            /* insert as left child */
            CN_LEFT_CHILD(previous_idx) = cn_idx;
         }

         if (*constant > CN_CONST(last_unbalanced_idx)) {
            idx = CN_RIGHT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = -1;
         }
         else if (*constant < CN_CONST(last_unbalanced_idx)) {
            idx = CN_LEFT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = 1;
         }
         else if (type_idx > CN_TYPE_IDX(last_unbalanced_idx)) {
            idx = CN_RIGHT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = -1;
         }
         else {
            idx = CN_LEFT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = 1;
         }

         while (idx != cn_idx) {
            if (*constant > CN_CONST(idx)) {
               CN_BALANCE_FACTOR(idx) = -1;
               idx = CN_RIGHT_CHILD(idx);
            }
            else if (*constant < CN_CONST(idx)) {
               CN_BALANCE_FACTOR(idx) = 1;
               idx = CN_LEFT_CHILD(idx);
            }
            else if (type_idx > CN_TYPE_IDX(idx)) {
               CN_BALANCE_FACTOR(idx) = -1;
               idx = CN_RIGHT_CHILD(idx);
            }
            else {
               CN_BALANCE_FACTOR(idx) = 1;
               idx = CN_LEFT_CHILD(idx);
            }
         }
         break;

      case 2:
         if (*(long long *)constant > *(long long *)&CN_CONST(previous_idx)) {
            /* insert as right child */
            CN_RIGHT_CHILD(previous_idx) = cn_idx;
         }
         else if (*(long long *)constant < 
                    *(long long *)&CN_CONST(previous_idx)) {
            /* insert as left child */
            CN_LEFT_CHILD(previous_idx) = cn_idx;
         }
         else if (type_idx > CN_TYPE_IDX(previous_idx)) {
            /* insert as right child */
            CN_RIGHT_CHILD(previous_idx) = cn_idx;
         }
         else {
            /* insert as left child */
            CN_LEFT_CHILD(previous_idx) = cn_idx;
         }

         if (*(long long *)constant > 
                    *(long long *)&CN_CONST(last_unbalanced_idx)) {
            idx = CN_RIGHT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = -1;
         }
         else if (*(long long *)constant < 
                           *(long long *)&CN_CONST(last_unbalanced_idx)) {
            idx = CN_LEFT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = 1;
         }
         else if (type_idx > CN_TYPE_IDX(last_unbalanced_idx)) {
            idx = CN_RIGHT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = -1;
         }
         else {
            idx = CN_LEFT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = 1;
         }

         while (idx != cn_idx) {
            if (*(long long *)constant > *(long long *)&CN_CONST(idx)) {
               CN_BALANCE_FACTOR(idx) = -1;
               idx = CN_RIGHT_CHILD(idx);
            }
            else if (*(long long *)constant < *(long long *)&CN_CONST(idx)) {
               CN_BALANCE_FACTOR(idx) = 1;
               idx = CN_LEFT_CHILD(idx);
            }
            else if (type_idx > CN_TYPE_IDX(idx)) {
               CN_BALANCE_FACTOR(idx) = -1;
               idx = CN_RIGHT_CHILD(idx);
            }
            else {
               CN_BALANCE_FACTOR(idx) = 1;
               idx = CN_LEFT_CHILD(idx);
            }
         }
         break;

      default:
         if (compare_value_to_cn(constant, previous_idx, Gt_Opr)) {
            /* insert as right child */
            CN_RIGHT_CHILD(previous_idx) = cn_idx;
         }
         else if (compare_value_to_cn(constant, previous_idx, Lt_Opr)) {
            /* insert as left child */
            CN_LEFT_CHILD(previous_idx) = cn_idx;
         }
         else if (type_idx > CN_TYPE_IDX(previous_idx)) {
            /* insert as right child */
            CN_RIGHT_CHILD(previous_idx) = cn_idx;
         }
         else {
            /* insert as left child */
            CN_LEFT_CHILD(previous_idx) = cn_idx;
         }


         if (compare_value_to_cn(constant, last_unbalanced_idx, Gt_Opr)) {
            idx = CN_RIGHT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = -1;  
         }
         else if (compare_value_to_cn(constant, last_unbalanced_idx, Lt_Opr)) {
            idx = CN_LEFT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = 1;
         }
         else if (type_idx > CN_TYPE_IDX(last_unbalanced_idx)) {
            idx = CN_RIGHT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = -1;
         }
         else {
            idx = CN_LEFT_CHILD(last_unbalanced_idx);
            idx_B = idx;
            balance_factor = 1;
         }

         while (idx != cn_idx) {
            if (compare_value_to_cn(constant, idx, Gt_Opr)) {
               CN_BALANCE_FACTOR(idx) = -1;
               idx = CN_RIGHT_CHILD(idx);
            }
            else if (compare_value_to_cn(constant, idx, Lt_Opr)) {
               CN_BALANCE_FACTOR(idx) = 1;
               idx = CN_LEFT_CHILD(idx);
            }
            else if (type_idx > CN_TYPE_IDX(idx)) {
               CN_BALANCE_FACTOR(idx) = -1;
               idx = CN_RIGHT_CHILD(idx);
            }
            else {
               CN_BALANCE_FACTOR(idx) = 1;
               idx = CN_LEFT_CHILD(idx);
            }
         }
         break;
   }
   
   if (CN_BALANCE_FACTOR(last_unbalanced_idx) == 0) {
      CN_BALANCE_FACTOR(last_unbalanced_idx) = balance_factor;
      goto EXIT;
   }
   
   if (CN_BALANCE_FACTOR(last_unbalanced_idx) + balance_factor == 0) {
      CN_BALANCE_FACTOR(last_unbalanced_idx) = 0;
      goto EXIT;
   }

   /* tree unbalanced */

   if (balance_factor == 1) {
      /* left imbalance */
      if (CN_BALANCE_FACTOR(idx_B) == 1) {
         /* LL rotation */
         CN_LEFT_CHILD(last_unbalanced_idx) = CN_RIGHT_CHILD(idx_B);
         CN_RIGHT_CHILD(idx_B) = last_unbalanced_idx;
         CN_BALANCE_FACTOR(last_unbalanced_idx) = 0;
         CN_BALANCE_FACTOR(idx_B) = 0;
      }
      else {
         /* LR rotation */
         idx_C = CN_RIGHT_CHILD(idx_B);
         CN_RIGHT_CHILD(idx_B) = CN_LEFT_CHILD(idx_C);
         CN_LEFT_CHILD(last_unbalanced_idx) = CN_RIGHT_CHILD(idx_C);
         CN_LEFT_CHILD(idx_C) = idx_B;
         CN_RIGHT_CHILD(idx_C) = last_unbalanced_idx;

         if (CN_BALANCE_FACTOR(idx_C) == 1) {
            /* LR (idx_B) */
            CN_BALANCE_FACTOR(last_unbalanced_idx) = -1;
            CN_BALANCE_FACTOR(idx_B) = 0;
         }
         else if (CN_BALANCE_FACTOR(idx_C) == -1) {
            /* LR (idx_C) */
            CN_BALANCE_FACTOR(idx_B) = 1;
            CN_BALANCE_FACTOR(last_unbalanced_idx) = 0;
         }
         else {
            /* LR (last_unbalanced_idx) */
            CN_BALANCE_FACTOR(idx_B) = 0;
            CN_BALANCE_FACTOR(last_unbalanced_idx) = 0;
         }

         CN_BALANCE_FACTOR(idx_C) = 0;
         idx_B = idx_C;
      }
   }
   else {
      /* right imbalance */
      if (CN_BALANCE_FACTOR(idx_B) == -1) {
         /* RR rotation */
         CN_RIGHT_CHILD(last_unbalanced_idx) = CN_LEFT_CHILD(idx_B);
         CN_LEFT_CHILD(idx_B) = last_unbalanced_idx;
         CN_BALANCE_FACTOR(last_unbalanced_idx) = 0;
         CN_BALANCE_FACTOR(idx_B) = 0;
      }
      else {
         /* RL rotation */
         idx_C = CN_LEFT_CHILD(idx_B);
         CN_LEFT_CHILD(idx_B) = CN_RIGHT_CHILD(idx_C);
         CN_RIGHT_CHILD(last_unbalanced_idx) = CN_LEFT_CHILD(idx_C);
         CN_RIGHT_CHILD(idx_C) = idx_B;
         CN_LEFT_CHILD(idx_C) = last_unbalanced_idx;

         if (CN_BALANCE_FACTOR(idx_C) == -1) {
            /* RL (idx_B) */
            CN_BALANCE_FACTOR(last_unbalanced_idx) = 1;
            CN_BALANCE_FACTOR(idx_B) = 0;
         }
         else if (CN_BALANCE_FACTOR(idx_C) == 1) {
            /* RL (idx_C) */
            CN_BALANCE_FACTOR(idx_B) = -1;
            CN_BALANCE_FACTOR(last_unbalanced_idx) = 0;
         }
         else {
            /* RL (last_unbalanced_idx) */
            CN_BALANCE_FACTOR(idx_B) = 0;
            CN_BALANCE_FACTOR(last_unbalanced_idx) = 0;
         }

         CN_BALANCE_FACTOR(idx_C) = 0;
         idx_B = idx_C;
      }
   }

   if (unbalanced_parent_idx == 0) {
      cn_root_idx[TYP_LINEAR(type_idx)] = idx_B;
   }
   else if (last_unbalanced_idx == CN_LEFT_CHILD(unbalanced_parent_idx)) {
      CN_LEFT_CHILD(unbalanced_parent_idx) = idx_B;
   }
   else if (last_unbalanced_idx == CN_RIGHT_CHILD(unbalanced_parent_idx)) {
      CN_RIGHT_CHILD(unbalanced_parent_idx) = idx_B;
   }

EXIT:

   TRACE (Func_Exit, "insert_constant", NULL);

   return(cn_idx);

}  /* insert_constant */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Types that are not stored ordered are kept in a skewed tree (left).   *|
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

static int insert_unordered_constant(int	type_idx,
				     long_type	*constant,
				     int	input_word_len,
				     int	const_word_len)

{
#ifdef KEY /* Bug 10177 */
   int		cn_idx = 0;
#else /* KEY Bug 10177 */
   int		cn_idx;
#endif /* KEY Bug 10177 */
   int		i;
   int		idx;
   int		pool_idx;
   int		prev_idx;
   int		root;

   TRACE (Func_Entry, "insert_unordered_constant", NULL);

   root = cn_root_idx[TYP_LINEAR(type_idx)];

   if (root == NULL_IDX) {
      /* nothing in the tree yet */

      TBL_REALLOC_CK(const_tbl, 1);
      CLEAR_TBL_NTRY(const_tbl, const_tbl_idx);
      cn_idx = const_tbl_idx;

      cn_root_idx[TYP_LINEAR(type_idx)] = cn_idx;
      goto EXIT;
   }

   prev_idx = 0;
   idx = root;

   if (TYP_TYPE(type_idx) == Typeless) {
      while (idx) {
         if (type_idx == CN_TYPE_IDX(idx)) {
            if (CN_BOZ_CONSTANT(idx) ||
                CN_BOOLEAN_CONSTANT(idx) ||
                CN_HOLLERITH_TYPE(idx) != Not_Hollerith) {
               continue;
            }
            pool_idx = CN_POOL_IDX(idx);
            for (i = 0; i < input_word_len; i++) {
               if (const_pool[pool_idx + i] != constant[i]) {
                  break;
               }
            }
            if (i == input_word_len &&
               (input_word_len == const_word_len ||
                const_pool[pool_idx + i] == 0)) {
               cn_idx = idx;
               goto EXIT;
            }
         }

         prev_idx = idx;
         idx = CN_LEFT_CHILD(idx);
      }
   }
   else {
      while (idx) {
         if (type_idx == CN_TYPE_IDX(idx)) {
            pool_idx = CN_POOL_IDX(idx);

            for (i = 0; i < input_word_len; i++) {

               if (const_pool[pool_idx + i] != constant[i]) {
                  break;
               }
            }

            if (i == input_word_len &&
               (input_word_len == const_word_len ||
                const_pool[pool_idx + i] == 0)) {
               cn_idx = idx;
               goto EXIT;
            }
         }

         prev_idx = idx;
         idx = CN_LEFT_CHILD(idx);
      }
   }

   if (idx == NULL_IDX) {
      TBL_REALLOC_CK(const_tbl, 1);
      CLEAR_TBL_NTRY(const_tbl, const_tbl_idx);
      cn_idx = const_tbl_idx;
      CN_LEFT_CHILD(prev_idx) = cn_idx;
   }

EXIT:

   TRACE (Func_Exit, "insert_unordered_constant", NULL);

   return(cn_idx);

}  /* insert_unordered_constant */

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

static void dump_cn_tree(int		root,
		         int		type_idx,
                         int		indent)

{
   int		i;
   char		shift[80];
   char		str[80];

   TRACE (Func_Entry, "dump_cn_tree", NULL);

   if (root != NULL_IDX) {
      for (i = 0; i < 3 * indent; i++) {
         shift[i] = ' ';
         if (i == 79)
            break;
      }
      shift[i] = '\0';

      printf("%s%s %c\n", shift, convert_to_string(&CN_CONST(root),
                                                    type_idx,
                                                    str),
                          TYP_DESC(CN_TYPE_IDX(root)) == Default_Typed ?
                                      'D' : 'K');

      if (CN_LEFT_CHILD(root) != NULL_IDX || 
          CN_RIGHT_CHILD(root) != NULL_IDX) {
         dump_cn_tree(CN_LEFT_CHILD(root), type_idx, indent+1);
         dump_cn_tree(CN_RIGHT_CHILD(root), type_idx, indent+1);
      }
   }
   else {
      printf("\n");
   }

   TRACE (Func_Exit, "dump_cn_tree", NULL);

   return;

}  /* dump_cn_tree */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ntr_boz_const_tbl adds BOZ constants to constant table. It            *|
|*      searches for a match and returns the matching index if found.  If not *|
|*      found it adds the constant and returns the new index.                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      constant      The constant to be entered.                             *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      constant table index of entry                                         *|
|*                                                                            *|
\******************************************************************************/

int	ntr_boz_const_tbl(int		type_idx,
			  long_type	*constant)

{
   register int          const_idx;
   register int		 i;
   register int		 pool_idx;
   register int		 word_len;


   TRACE (Func_Entry, "ntr_boz_const_tbl", NULL);

   word_len = STORAGE_WORD_SIZE(TYP_BIT_LEN(type_idx));

   for (const_idx = 1; const_idx <= const_tbl_idx; const_idx++) {

      if (CN_BOZ_CONSTANT(const_idx) &&
          CN_TYPE_IDX(const_idx) == type_idx) {

         pool_idx = CN_POOL_IDX(const_idx);

         for (i = 0; i < word_len; i++) {

            if (const_pool[pool_idx + i] != constant[i]) {
               break;
            }
         }

         if (i == word_len) {
            goto FOUND;
         }
      }
   }

   TBL_REALLOC_CK(const_tbl, 1);
   CLEAR_TBL_NTRY(const_tbl, const_tbl_idx);
   pool_idx = const_pool_idx + 1;

   CN_POOL_IDX(const_tbl_idx)   = pool_idx;

   if ((const_pool_idx += word_len) >= const_pool_size) {
      const_pool_size = const_pool_size +
                        ( ( ( (const_pool_idx - const_pool_size + 1) /
                             const_pool_inc) + 1) * const_pool_inc);
      MEM_REALLOC (const_pool, const_pool_type, const_pool_size);
   }

   const_idx			= const_tbl_idx;
   CN_TYPE_IDX(const_idx)	= type_idx;
   CN_BOZ_CONSTANT(const_idx)	= TRUE;

   for (i = 0; i < word_len; i++) {
      const_pool[pool_idx + i] = constant[i];
   }

FOUND:

   TRACE (Func_Exit, "ntr_boz_const_tbl", NULL);

   return (const_idx);

}  /* ntr_boz_const_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ntr_boolean_const_tbl adds boolean constants to constant table. It    *|
|*      searches for a match and returns the matching index if found.  If not *|
|*      found it adds the constant and returns the new index.                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      constant      The constant to be entered.                             *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      constant table index of entry                                         *|
|*                                                                            *|
\******************************************************************************/
int     ntr_boolean_const_tbl(int		type_idx,
			      long_type 	*constant)

{
   register int          const_idx;
   register int          i;
   register int          pool_idx;
   register int          word_len;


   TRACE (Func_Entry, "ntr_boolean_const_tbl", NULL);

   word_len = STORAGE_WORD_SIZE(TYP_BIT_LEN(type_idx));

   for (const_idx = 1; const_idx <= const_tbl_idx; const_idx++) {

      if (CN_BOOLEAN_CONSTANT(const_idx) &&
          CN_TYPE_IDX(const_idx) == type_idx) {

         pool_idx = CN_POOL_IDX(const_idx);

         for (i = 0; i < word_len; i++) {

            if (const_pool[pool_idx + i] != constant[i]) {
               break;
            }
         }

         if (i == word_len) {
            goto FOUND;
         }
      }
   }

   TBL_REALLOC_CK(const_tbl, 1);
   CLEAR_TBL_NTRY(const_tbl, const_tbl_idx);
   pool_idx = const_pool_idx + 1;

   CN_POOL_IDX(const_tbl_idx)   = pool_idx;

   if ((const_pool_idx += word_len) >= const_pool_size) {
      const_pool_size = const_pool_size +
                        ( ( ( (const_pool_idx - const_pool_size + 1) /
                             const_pool_inc) + 1) * const_pool_inc);
      MEM_REALLOC (const_pool, const_pool_type, const_pool_size);
   }

   const_idx                    = const_tbl_idx;
   CN_TYPE_IDX(const_idx)       = type_idx;
   CN_BOOLEAN_CONSTANT(const_idx)   = TRUE;

   for (i = 0; i < word_len; i++) {
      const_pool[pool_idx + i] = constant[i];
   }

FOUND:

   TRACE (Func_Exit, "ntr_boolean_const_tbl", NULL);

   return (const_idx);

}  /* ntr_boolean_const_tbl */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ntr_unshared_const_tbl just slams the constant into the Constant      *|
|*      table without doing any searches to see if the constant already       *|
|*      exists in the table.  It is also used to add typeless constants that  *|
|*      are used in numeric contexts to the Constant table because the normal *|
|*      ntr_const_tbl can't be used because the bit pattern in the typeless   *|
|*      entity might not be a valid floating-point bit pattern for example.   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      type_idx         The data type of the incoming constant.              *|
|*      extra_zero_word  Add an extra word to constant and set flag.          *|
|*      constant         The address of the constant to be entered.           *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      Constant table index of the new entry.                                *|
|*                                                                            *|
\******************************************************************************/

int ntr_unshared_const_tbl (int           type_idx,
                            boolean       extra_zero_word,
                            long_type    *constant)

{
   register     int                     const_idx;
#ifdef KEY /* Bug 10177 */
                long64                  const_word_len = 0;
   register     int                     i;
                long64                  input_word_len = 0;
#else /* KEY Bug 10177 */
                long64                  const_word_len;
   register     int                     i;
                long64                  input_word_len;
#endif /* KEY Bug 10177 */
                size_offset_type        length;
   register     int                     pool_idx;


   TRACE (Func_Entry, "ntr_unshared_const_tbl", NULL);

   switch(TYP_TYPE(type_idx)) {

      case Typeless:

         input_word_len = STORAGE_WORD_SIZE(TYP_BIT_LEN(type_idx));
         const_word_len =
            (extra_zero_word) ? input_word_len + 1 : input_word_len;
         break;


      case Character:

         input_word_len = TARGET_BYTES_TO_WORDS(CN_INT_TO_C(TYP_IDX(type_idx)));
         const_word_len =
            (extra_zero_word) ? input_word_len + 1 : input_word_len;
         break;


      case Integer:
      case Real:
      case Logical:

         const_word_len =
            TARGET_BITS_TO_WORDS(storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
         input_word_len = const_word_len;
         break;


      case Complex:

         const_word_len =
            TARGET_BITS_TO_WORDS(storage_bit_size_tbl[TYP_LINEAR(type_idx)]);

#if defined(_TARGET_OS_MAX) || defined(_WHIRL_HOST64_TARGET64)

         if (TYP_LINEAR(type_idx) == Complex_4) {
            const_word_len = 2;
         }

#endif
         input_word_len = const_word_len;
         break;


      case Structure:

         /* Issue error - constant too big??? */
         /* This cannot ever be executed, because it will not work. KAY */

         length.fld = ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));;
         length.idx = ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));;

         BITS_TO_WORDS(length, TARGET_BITS_PER_WORD);

         if (length.fld == CN_Tbl_Idx) {
            const_word_len =  CN_INT_TO_C(length.idx);
         }
         else {
            PRINTMSG(AT_DEF_LINE(TYP_IDX(type_idx)), 1201, Internal,
                     AT_DEF_COLUMN(TYP_IDX(type_idx)),
                     AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
         }
   
         input_word_len = const_word_len;
         break;
   }


   TBL_REALLOC_CK(const_tbl, 1);
   CLEAR_TBL_NTRY(const_tbl, const_tbl_idx);
   pool_idx = const_pool_idx + 1;


#if defined(_HOST32)

   if (DALIGN_TEST_CONDITION(type_idx)) {

      while ((((long)&const_pool[pool_idx]) % 8) != 0) {
         pool_idx++;
         const_pool_idx++;
      }
   }

#endif


   CN_POOL_IDX(const_tbl_idx) = pool_idx;

   if ((const_pool_idx += const_word_len) >= const_pool_size) {
      const_pool_size = const_pool_size +
                        ( ( ( (const_pool_idx - const_pool_size + 1) /
                             const_pool_inc) + 1) * const_pool_inc);
      MEM_REALLOC (const_pool, const_pool_type, const_pool_size);
   }

   const_idx                     = const_tbl_idx;
   CN_TYPE_IDX(const_idx)        = type_idx;
   CN_EXTRA_ZERO_WORD(const_idx) = extra_zero_word;


   /* If constant does not point to anything, then the caller will put the */
   /* constant into the constant pool.  Otherwise copy the constant in.    */

   if (const_word_len == 0) {

      /* Intentionally blank because we don't want to write to any const_pool */
      /* words for a zero-length constant.				      */

   }
   else if (constant != NULL_IDX) {
      const_pool[const_pool_idx] = 0L;

      if (TYP_TYPE(type_idx) == Character) {

         if (extra_zero_word) {
            const_pool[const_pool_idx - 1] = 0L;
         }
         strncpy((char *) &CN_CONST(const_idx),
                 (char *) constant,
                 (long) CN_INT_TO_C(TYP_IDX(type_idx)));
      }
      else {

         for (i = 0;  i < input_word_len;  i++) {
            const_pool[pool_idx + i] = constant[i];
         }
      }
   }
   else {

      for (i = pool_idx;  i <= const_pool_idx;  i++) {
         const_pool[i] = 0L;
      }
   }

   TRACE (Func_Exit, "ntr_unshared_const_tbl", NULL);

   return (const_idx);

}  /* ntr_unshared_const_tbl */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure adds an abnormal IEEE constant to the Constant table   *|
|*      or finds such a constant if it already exists in the Constant table.  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      type_idx         data type of the incoming constant                   *|
|*      constant         the address of the constant to be entered or located *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      Constant table index of the incoming constant.			      *|
|*                                                                            *|
\******************************************************************************/

static int ntr_abnormal_ieee_const(int           type_idx,
                                   long_type    *constant)

{
   int  const_idx;
#ifdef KEY /* Bug 10177 */
   int  idx = 0;
#else /* KEY Bug 10177 */
   int  idx;
#endif /* KEY Bug 10177 */

   enum   abnormal_value    { Real_4_Nan,
			      Real_8_Nan,
			      Real_16_Nan,
			      Real_4_Pos_Inf,
			      Real_8_Pos_Inf,
			      Real_16_Pos_Inf,
			      Real_4_Neg_Inf,
			      Real_8_Neg_Inf,
    			      Real_16_Neg_Inf,
                              Real_4_Subnormal,
			      Real_8_Subnormal,
                	      Real_16_Subnormal,
                              Real_4_Pos_Zero,
			      Real_8_Pos_Zero,
                	      Real_16_Pos_Zero,
                              Real_4_Neg_Zero,
			      Real_8_Neg_Zero,
                	      Real_16_Neg_Zero
   		            };


   TRACE (Func_Entry, "ntr_abnormal_ieee_const", NULL);


   switch (TYP_LINEAR(type_idx)) {

      case Real_4:

         switch (fp_classify(type_idx, constant)) {

            case FP_SGI_NAN:
               idx = (int) Real_4_Nan;
               break;

            case FP_SGI_INFINITE:
               idx = (sign_bit(type_idx, constant) == 0) ?
                        (int) Real_4_Pos_Inf : (int) Real_4_Neg_Inf;
               break;

            case FP_SGI_SUBNORMAL:
               idx = (int) Real_4_Subnormal;
               break;

            case FP_SGI_ZERO:
               idx = (sign_bit(type_idx, constant) == 0) ?
                        (int) Real_4_Pos_Zero : (int) Real_4_Neg_Zero;
               break;

            default:                                /* FP_SGI_NORMAL */
               PRINTMSG(stmt_start_line, 179, Internal, stmt_start_col,
                     "ntr_abnormal_ieee_const");
         }

         break;


      case Real_8:

         switch (fp_classify(type_idx, constant)) {

            case FP_SGI_NAN:
               idx = (int) Real_8_Nan;
               break;

            case FP_SGI_INFINITE:
               idx = (sign_bit(type_idx, constant) == 0) ?
                        (int) Real_8_Pos_Inf : (int) Real_8_Neg_Inf;
               break;

            case FP_SGI_SUBNORMAL:
               idx = (int) Real_8_Subnormal;
               break;

            case FP_SGI_ZERO:
               idx = (sign_bit(type_idx, constant) == 0) ?
                        (int) Real_8_Pos_Zero : (int) Real_8_Neg_Zero;
               break;

            default:                                /* FP_SGI_NORMAL */
               PRINTMSG(stmt_start_line, 179, Internal, stmt_start_col,
                     "ntr_abnormal_ieee_const");
         }

         break;


      case Real_16:

         switch (fp_classify(type_idx, constant)) {

            case FP_SGI_NAN:
               idx = (int) Real_16_Nan;
               break;

            case FP_SGI_INFINITE:
               idx = (sign_bit(type_idx, constant) == 0) ?
                        (int) Real_16_Pos_Inf : (int) Real_16_Neg_Inf;
               break;

            case FP_SGI_SUBNORMAL:
               idx = (int) Real_16_Subnormal;
               break;

            case FP_SGI_ZERO:
               idx = (sign_bit(type_idx, constant) == 0) ?
                        (int) Real_16_Pos_Zero : (int) Real_16_Neg_Zero;
               break;

            default:                                /* FP_SGI_NORMAL */
               PRINTMSG(stmt_start_line, 179, Internal, stmt_start_col,
                     "ntr_abnormal_ieee_const");
         }
   }

   if (ieee_const_tbl_idx[idx] == NULL_IDX) {
      const_idx = ntr_unshared_const_tbl(type_idx, FALSE, constant);
      ieee_const_tbl_idx[idx] = const_idx;
   }
   else {
      const_idx = ieee_const_tbl_idx[idx];
   }

   TRACE (Func_Exit, "ntr_abnormal_ieee_const", NULL);

   return(const_idx);

}  /* ntr_abnormal_ieee_const */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      srch_host_stor_blk_tbl searches the stor_blk tables of all hosts for  *|
|*	the identifier contained in the identifier field of token.     	      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      token			token containing identifier to  	      *|
|*                              search for and length in chars of name        *|
|*                                                                            *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      stor_blk table index   if found                   		      *|
|*      NULL_IDX                if not found				      *|
|*                                                                            *|
|*									      *|
\******************************************************************************/

int srch_host_stor_blk_tbl (token_type *token)

{

   int		 idx		= NULL_IDX;
   token_type	 nme_token;
   int		 save_scp_idx;
   
   TRACE (Func_Entry, "srch_host_stor_blk_tbl", NULL);

   /* DO NOT search the host when processing an interface block */

   if (SCP_IS_INTERFACE(curr_scp_idx)) {
      return (NULL_IDX);
   }

   save_scp_idx	= curr_scp_idx;

   while (idx == NULL_IDX && SCP_PARENT_IDX(curr_scp_idx) != NULL_IDX) {

      /* Set current scope to parent, for searching. */

      curr_scp_idx	= SCP_PARENT_IDX(curr_scp_idx);

      nme_token		= *token;
      idx		= srch_stor_blk_tbl(TOKEN_STR(nme_token),
                                            TOKEN_LEN(nme_token),
                                            curr_scp_idx);
   }  /* while */

   curr_scp_idx	= save_scp_idx;

   TRACE (Func_Exit, "srch_host_stor_blk_tbl", NULL);

    return (idx);

}  /* srch_host_stor_blk_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Compare two derived types, to see if they are the same.               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      dt_idx1 ->  Index to first derived type to compare.                   *|
|*      dt_idx2 ->  Index to second derived type to compare.                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if they are the same, else FALSE.                                *|
|*                                                                            *|
\******************************************************************************/
boolean	compare_derived_types(int	dt_idx1,
			      int	dt_idx2)

{
   int		 at_idx1;
   int		 at_idx2;
   int		 bit_idx1;
#ifdef KEY /* Bug 10177 */
   int		 bit_idx2 = 0;
#else /* KEY Bug 10177 */
   int		 bit_idx2;
#endif /* KEY Bug 10177 */
   boolean	 check;
   int		 entry_idx1;
#ifdef KEY /* Bug 10177 */
   int		 entry_idx2 = 0;
#else /* KEY Bug 10177 */
   int		 entry_idx2;
#endif /* KEY Bug 10177 */
   int		 id1;
   int		 id2;
   int		 idx;
   boolean	 keep_compare;
   int		 len1;
   int		 len2;
   int		 mod_idx1;
   int		 mod_idx2;
   long		*name1;
   long		*name2;
   int		 s_idx1;
   int		 s_idx2;
   boolean	 same;

   static	 long	dt_cmp_tbl_size;
   static	 int	entry_size;
   static	 long	num_of_entries;
   static	 long	unique_dt_number;

   /*
    *  For the stmt below, GCC gets different results on X8664 and IA64.
    *    long a = -2; int b = 32;
    *    a |= (1 << b);    // GCC on IA64, a == -2; GCC on X8664, a == -1
    *  On X8664, the compiler may regard 1 and (1 << b) as signed integer 
    *  (same type as b) so that it gets the result -1.
    *  To avoid the ambiguous stmt, we should force the number 1 be 
    *  long integer in all this function, i.e. 1L. 
    */

   TRACE (Func_Entry, "compare_derived_types", NULL);

   /* first check to see if they resolve to the same attr */

   dt_idx1 = TYP_IDX(dt_idx1);
   dt_idx2 = TYP_IDX(dt_idx2);

   while (AT_ATTR_LINK(dt_idx1) != NULL_IDX) {
      dt_idx1 = AT_ATTR_LINK(dt_idx1);
   }

   while (AT_ATTR_LINK(dt_idx2) != NULL_IDX) {
      dt_idx2 = AT_ATTR_LINK(dt_idx2);
   }

   if (dt_idx1 == dt_idx2) {
      same = TRUE;
      return(TRUE);
   }

   /* Check to see if this attr has already been compared */

   /* This routine uses a bit table to keep track of whether two derived  */
   /* types have been compared and if they are the same or not.  This     */
   /* prevents checking over and over if a derived type is the same.      */
   /* Keeping the bit format, keeps storage space small.                  */
   /* The number of derived types is counted during pass 1.  All derived  */
   /* types in a program unit and all containing program units get unique */
   /* id's.  This id is used to check the type in the dt_cmp_tbl.         */
   /* The table is a 2-D table, where each derived type gets an entry.    */
   /* Each entry is a group of longs, containing a bit for each derived   */
   /* type.  Thus if a program unit has 100 derived types, the table will */
   /* have 100 entries and each entry will be 128 bits long (100 rounded  */
   /* up to the next full word).  Since this is 2-D, each combination of  */
   /* derived types exists in two places.  The two places hold separate   */
   /* information.  If you access [lower id][higher id] the bit says if   */
   /* the types have been compared.  TRUE = compared. If you access       */
   /* [higher id][lower id] the bit says if the types are the same or     */
   /* not.  TRUE = same.  For example, you have two derived types, #8 and */
   /* #33.  [entry #8][bit #33] tells whether these two derived types     */
   /* have been compared.  If that bit is set, then [entry #33][bit #8]   */
   /* tells you if they are the same or not.  It's compact and fast.      */


   if (dt_cmp_tbl == NULL) {  /* Need to allocate table. */

      if (comp_phase >= Decl_Semantics) {
         num_of_entries = num_of_derived_types;
      }
      else {
         num_of_entries = (num_of_derived_types > 500) ? num_of_derived_types :
                                                         500;
      }

      entry_size	= ((num_of_entries-1) / HOST_BITS_PER_WORD) + 1;
      dt_cmp_tbl_size	= (1 + num_of_entries) * entry_size;
      unique_dt_number	= 0;

      /* must do original malloc */

      MEM_ALLOC(dt_cmp_tbl, long, dt_cmp_tbl_size);

      for (idx = 0; idx < dt_cmp_tbl_size; idx++) dt_cmp_tbl[idx] = 0;
   }
   id1	= ATT_UNIQUE_ID(dt_idx1);
   id2	= ATT_UNIQUE_ID(dt_idx2);

   if (id1 == 0) {
      id1			= ++unique_dt_number;
      ATT_UNIQUE_ID(dt_idx1)	= id1;
   }
   
   if (id2 == 0) {
      id2			= ++unique_dt_number;
      ATT_UNIQUE_ID(dt_idx2)	= id2;
   }


   if (id1 > num_of_entries || id2 > num_of_entries) {

      /* We cannot keep track of these.  Have them use the last extra entry. */

      keep_compare	= FALSE;
   }
   else {
      keep_compare	= TRUE;

      /* The lower of the two becomes id1.  The larger becomes id2. */

      if (id2 < id1) {
         entry_idx1	= id2;	/* Temp holder. */
         id2		= id1;
         id1		= entry_idx1;
      }

      entry_idx1	= ((id1-1)*entry_size) + ((id2-1) / HOST_BITS_PER_WORD);
      entry_idx2	= ((id2-1)*entry_size) + ((id1-1) / HOST_BITS_PER_WORD);
      bit_idx1		= ((id2-1) % HOST_BITS_PER_WORD);
      bit_idx2		= ((id1-1) % HOST_BITS_PER_WORD);

      check		= (1L << bit_idx1) & dt_cmp_tbl[entry_idx1];

      if (check) {
         same = (1L << bit_idx2) & dt_cmp_tbl[entry_idx2]; 
         goto DONE;
      }
   
      /* Didn't find this attr.  Set the check bit and compare bit */
      /* to same in case a recursive call happens.  Same  will get */
      /* set correctly at the end of this routine.                 */

      dt_cmp_tbl[entry_idx1]	|= (1L << bit_idx1);	/* Check */
      dt_cmp_tbl[entry_idx2]	|= (1L << bit_idx2);	/* Same  */

   }

   if (AT_USE_ASSOCIATED(dt_idx1)) {
      name1	= AT_ORIG_NAME_LONG(dt_idx1);
      len1	= AT_ORIG_NAME_LEN(dt_idx1);
      mod_idx1	= AT_MODULE_IDX(dt_idx1);
   }
   else {
      name1	= AT_OBJ_NAME_LONG(dt_idx1);
      len1	= AT_NAME_LEN(dt_idx1);
      mod_idx1	= NULL_IDX;
   }

   if (AT_USE_ASSOCIATED(dt_idx2)) {
      name2	= AT_ORIG_NAME_LONG(dt_idx2);
      len2	= AT_ORIG_NAME_LEN(dt_idx2);
      mod_idx2	= AT_MODULE_IDX(dt_idx2);
   }
   else {
      name2	= AT_OBJ_NAME_LONG(dt_idx2);
      len2	= AT_NAME_LEN(dt_idx2);
      mod_idx2	= NULL_IDX;
   }

   if (compare_names(name1, len1, name2, len2) != 0) {
      same = FALSE;
      goto DONE;
   }

   if (mod_idx1 != NULL_IDX && mod_idx2 != NULL_IDX &&
       compare_names(AT_OBJ_NAME_LONG(mod_idx1),
                     AT_NAME_LEN(mod_idx1),
                     AT_OBJ_NAME_LONG(mod_idx2),
                     AT_NAME_LEN(mod_idx2)) == 0) {
      same = TRUE;
      goto DONE;
   }

   same = (!ATT_PRIVATE_CPNT(dt_idx1) && 
           !ATT_PRIVATE_CPNT(dt_idx2) &&
           (!AT_PRIVATE(dt_idx1) || AT_USE_ASSOCIATED(dt_idx1)) &&
           (!AT_PRIVATE(dt_idx2) || AT_USE_ASSOCIATED(dt_idx1)) &&
#ifdef KEY /* Bug 14150 */
	   ((ATT_SEQUENCE_SET(dt_idx1) && ATT_SEQUENCE_SET(dt_idx2)) ||
	    (AT_BIND_ATTR(dt_idx1) && AT_BIND_ATTR(dt_idx2))) &&
#else /* KEY Bug 14150 */
            ATT_SEQUENCE_SET(dt_idx1) &&
            ATT_SEQUENCE_SET(dt_idx2) &&
#endif /* KEY Bug 14150 */
            ATT_NUM_CPNTS(dt_idx1) == ATT_NUM_CPNTS(dt_idx2));

   if (!same) {
      goto DONE;
   }

   s_idx1 = ATT_FIRST_CPNT_IDX(dt_idx1);
   s_idx2 = ATT_FIRST_CPNT_IDX(dt_idx2);

   while (s_idx1 != NULL_IDX) {
      at_idx1 = SN_ATTR_IDX(s_idx1);
      at_idx2 = SN_ATTR_IDX(s_idx2);

      same = same &&
             ATD_POINTER(at_idx1) == ATD_POINTER(at_idx2) &&
             TYP_TYPE(ATD_TYPE_IDX(at_idx1)) == 
                      TYP_TYPE(ATD_TYPE_IDX(at_idx2)) &&
             compare_array_entries(ATD_ARRAY_IDX(at_idx1), 
                                   ATD_ARRAY_IDX(at_idx2)) &&
             (compare_names(AT_OBJ_NAME_LONG(at_idx1),
                            AT_NAME_LEN(at_idx1),
                            AT_OBJ_NAME_LONG(at_idx2),
                            AT_NAME_LEN(at_idx2)) == 0);

      /* Components, so they must be constants */

      if (TYP_TYPE(ATD_TYPE_IDX(at_idx1)) == Character) {
         same = same && fold_relationals(TYP_IDX(ATD_TYPE_IDX(at_idx1)),
                                         TYP_IDX(ATD_TYPE_IDX(at_idx2)),
                                         Eq_Opr);
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(at_idx1)) == Structure) {

         if (TYP_IDX(ATD_TYPE_IDX(at_idx1)) == dt_idx1 &&
             TYP_IDX(ATD_TYPE_IDX(at_idx2)) == dt_idx2) {

            /* intentionally blank */
         }
         else if (TYP_IDX(ATD_TYPE_IDX(at_idx1)) == dt_idx1 &&
                  TYP_IDX(ATD_TYPE_IDX(at_idx2)) != dt_idx2) {
            same = FALSE;
            goto DONE;
         }
         else if (TYP_IDX(ATD_TYPE_IDX(at_idx1)) != dt_idx1 &&
                  TYP_IDX(ATD_TYPE_IDX(at_idx2)) == dt_idx2) {
            same = FALSE;
            goto DONE;
         }
         else {
            same=same && compare_derived_types(ATD_TYPE_IDX(at_idx1),
                                               ATD_TYPE_IDX(at_idx2));
         }
      }
      else {
         same = same && TYP_LINEAR(ATD_TYPE_IDX(at_idx1)) ==
                        TYP_LINEAR(ATD_TYPE_IDX(at_idx2));
      }

      s_idx1 = SN_SIBLING_LINK(s_idx1);
      s_idx2 = SN_SIBLING_LINK(s_idx2);
   }  

DONE: 

   if (keep_compare) {

      if (same) {
         dt_cmp_tbl[entry_idx2]	|= (1L << bit_idx2);	/* Same bit */
      }
      else {
         dt_cmp_tbl[entry_idx2]	&= ~(1L << bit_idx2);	/* Same bit */
      }
   }

   TRACE (Func_Exit, "compare_derived_types", NULL);

   return(same);

}  /* compare_derived_types */

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

boolean compare_array_entries(int	bd_idx1,
			      int	bd_idx2)
{
   long_type    folded_const[MAX_WORDS_FOR_NUMERIC];
   int		i;
   boolean 	same;
   int		type_idx;


   TRACE (Func_Entry, "compare_array_entries", NULL);

   if (bd_idx1 == bd_idx2) {
      same = TRUE;
   }
   else if (bd_idx1 == NULL_IDX || bd_idx2 == NULL_IDX) {
      same = FALSE;
   }
   else {  /* Compare rank, size and class */

      same = (BD_RANK(bd_idx1) == BD_RANK(bd_idx2)) &&
             (BD_ARRAY_SIZE(bd_idx1) == BD_ARRAY_SIZE(bd_idx2)) &&
             (BD_ARRAY_CLASS(bd_idx1) == BD_ARRAY_CLASS(bd_idx2));

      if (same && BD_ARRAY_CLASS(bd_idx1) != Deferred_Shape) {
         type_idx = CG_LOGICAL_DEFAULT_TYPE;

         for (i = 1; i <= BD_RANK(bd_idx1); i++) {

            if (BD_LB_FLD(bd_idx1, i) == CN_Tbl_Idx && 
                BD_LB_FLD(bd_idx2, i) == CN_Tbl_Idx) {

               if (folder_driver((char *)&CN_CONST(BD_LB_IDX(bd_idx1, i)),
                                 CN_TYPE_IDX(BD_LB_IDX(bd_idx1, i)),
                                 (char *)&CN_CONST(BD_LB_IDX(bd_idx2, i)),
                                 CN_TYPE_IDX(BD_LB_IDX(bd_idx2, i)),
                                 folded_const,
                                 &type_idx,
                                 BD_LINE_NUM(bd_idx1),
                                 BD_COLUMN_NUM(bd_idx1),
                                 2,
                                 Ne_Opr)) {
               }

               if (THIS_IS_TRUE(folded_const, type_idx)) {
                  same = FALSE;
               }
            }

            if (BD_UB_FLD(bd_idx1, i) == CN_Tbl_Idx && 
                BD_UB_FLD(bd_idx2, i) == CN_Tbl_Idx) {

               if (folder_driver((char *)&CN_CONST(BD_UB_IDX(bd_idx1, i)),
                                 CN_TYPE_IDX(BD_UB_IDX(bd_idx1, i)),
                                 (char *)&CN_CONST(BD_UB_IDX(bd_idx2, i)),
                                 CN_TYPE_IDX(BD_UB_IDX(bd_idx2, i)),
                                 folded_const,
                                 &type_idx,
                                 BD_LINE_NUM(bd_idx1),
                                 BD_COLUMN_NUM(bd_idx1),
                                 2,
                                 Ne_Opr)) {
               }

               if (THIS_IS_TRUE(folded_const, type_idx)) {
                  same = FALSE;
               }
            }
         }
      }
   }

   TRACE (Func_Exit, "compare_array_entries", NULL);

   return(same);

}  /* compare_array_entries */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Initialize next 2 entries in local name table to point to the all     *|
|*      zero word and to the all one word, to act as guards for the name      *|
|*      table search.  Initialize next 2 enteries in the stor block table     *|
|*      to stack and static blocks.                                           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
void init_name_and_stor_tbls(int	scp_idx,
			     boolean	create_full_scp)
{
   int			ln_idx;
   id_str_type		name;
   int			new_idx;


   TRACE (Func_Entry, "init_name_and_stor_tbls", NULL);

   ln_idx				= loc_name_tbl_idx + 1;

   TBL_REALLOC_CK(loc_name_tbl, 2);
   CLEAR_TBL_NTRY(loc_name_tbl, ln_idx);
   LN_NAME_IDX(ln_idx)        		= NAME_POOL_ZERO_IDX; /* Zero word */
   LN_NAME_LEN(ln_idx)			= HOST_BYTES_PER_WORD;
   SCP_LN_FW_IDX(scp_idx)		= ln_idx;

   CLEAR_TBL_NTRY(loc_name_tbl, loc_name_tbl_idx);
   LN_NAME_IDX(loc_name_tbl_idx)        = NAME_POOL_ONES_IDX; /* Ones word   */
   LN_NAME_LEN(loc_name_tbl_idx)	= HOST_BYTES_PER_WORD;
   SCP_LN_LW_IDX(scp_idx)		= loc_name_tbl_idx;

   if (create_full_scp) {

      create_hidden_name_tbl(scp_idx);

      /* Create storage blocks for static, stack, and darg storage        */
      /* segments.  Fields in the scope table point to these stor blocks. */

      /* Create an entry for local data block.                            */

      CREATE_ID(name, sb_name[Data_Blk], sb_len[Data_Blk]);
      new_idx			= ntr_stor_blk_tbl(name.string,
                                                   sb_len[Data_Blk],
                                                   stmt_start_line,
                                                   stmt_start_col,
                                                   Static_Local);
      SCP_SB_STATIC_IDX(scp_idx)	= new_idx;
      SB_PAD_BLK(new_idx)               = cmd_line_flags.pad;

# if defined(_SPLIT_STATIC_STORAGE_2) || defined(_SPLIT_STATIC_STORAGE_3)

      /* Create an entry for local data block for data initialized vars.  */

      CREATE_ID(name, sb_name[Data_Init_Blk], sb_len[Data_Init_Blk]);
      new_idx                           = ntr_stor_blk_tbl(name.string,
                                                          sb_len[Data_Init_Blk],
                                                          stmt_start_line,
                                                          stmt_start_col,
                                                          Static_Named);
      SCP_SB_STATIC_INIT_IDX(scp_idx)   = new_idx;
      SB_PAD_BLK(new_idx)               = cmd_line_flags.pad;

# if defined(_SPLIT_STATIC_STORAGE_3)
      CREATE_ID(name, sb_name[Data_Uninit_Blk], sb_len[Data_Uninit_Blk]);
      new_idx                           = ntr_stor_blk_tbl(name.string,
                                                        sb_len[Data_Uninit_Blk],
                                                        stmt_start_line,
                                                        stmt_start_col,
                                                        Static_Named);
      SCP_SB_STATIC_UNINIT_IDX(scp_idx) = new_idx;
      SB_PAD_BLK(new_idx)               = cmd_line_flags.pad;
# endif

# else
      SCP_SB_STATIC_INIT_IDX(scp_idx)   = SCP_SB_STATIC_IDX(scp_idx);
      SCP_SB_STATIC_UNINIT_IDX(scp_idx) = SCP_SB_STATIC_IDX(scp_idx);
# endif

      if (cmd_line_flags.pad_amount != 0) {

# if defined(_SPLIT_STATIC_STORAGE_3)
         SB_PAD_AMOUNT(SCP_SB_STATIC_UNINIT_IDX(scp_idx)) =
                                              cmd_line_flags.pad_amount;
         SB_PAD_AMOUNT_SET(SCP_SB_STATIC_UNINIT_IDX(scp_idx))     = TRUE;
# endif

# if defined(_SPLIT_STATIC_STORAGE_2)

         SB_PAD_AMOUNT(SCP_SB_STATIC_INIT_IDX(scp_idx)) = 
                                              cmd_line_flags.pad_amount;
         SB_PAD_AMOUNT_SET(SCP_SB_STATIC_INIT_IDX(scp_idx)) = TRUE;
# endif
         SB_PAD_AMOUNT(SCP_SB_STATIC_IDX(scp_idx))= cmd_line_flags.pad_amount;
         SB_PAD_AMOUNT_SET(SCP_SB_STATIC_IDX(scp_idx))  = TRUE;
      }

      /* Create an entry for a local stack block                          */

      CREATE_ID(name, sb_name[Stack_Blk], sb_len[Stack_Blk]);
      new_idx			= ntr_stor_blk_tbl(name.string,
                                                   sb_len[Stack_Blk],
                                                   stmt_start_line,
                                                   stmt_start_col,
                                                   Stack);
      SCP_SB_STACK_IDX(scp_idx)	= new_idx;

      /* Create an entry for a local darg block.                          */

      CREATE_ID(name, sb_name[Dargs_Blk], sb_len[Dargs_Blk]);
      new_idx			= ntr_stor_blk_tbl(name.string,
                                                   sb_len[Dargs_Blk],
                                                   stmt_start_line,
                                                   stmt_start_col,
                                                   Formal);
      SCP_SB_DARG_IDX(scp_idx)	= new_idx;

      CREATE_ID(name, sb_name[Based_Blk], sb_len[Based_Blk]);
      new_idx			= ntr_stor_blk_tbl(name.string, 
                                                   sb_len[Based_Blk],
                                                   stmt_start_line,
                                                   stmt_start_col,
                                                   Based);
      SCP_SB_BASED_IDX(scp_idx)	= new_idx;
   }
   
   TRACE (Func_Exit, "init_name_and_stor_tbls", NULL);

   return;

}  /* init_name_and_stor_tbls */

# ifdef _DEBUG

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Issue internal err msg, if there is an attr table variant problem.    *|
|*      NOTE:  been_here_before is a way of stopping a nasty infinite loop    *|
|*             if you get an error in a field, and you try to print it out    *|
|*             as part of the error output.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      err_str  -> A string ptr to a description of the field in trouble.    *|
|*      attr_idx -> Index of the attr entry in trouble.                       *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
attr_tbl_type	*sytb_var_error(char	*err_str,
				int	 attr_idx)
{
static   int	been_here_before;

   if (been_here_before == 0) {
      been_here_before = 1;
      print_at_all(attr_idx);
      PRINTMSG(stmt_start_line, 42, Internal,stmt_start_col, attr_idx, err_str);
   }
   return(attr_tbl);
}
# endif


# ifdef _DEBUG
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Issue internal err msg, if there is an attr aux table variant problem.*|
|*      NOTE:  been_here_before is a way of stopping a nasty infinite loop    *|
|*             if you get an error in a field, and you try to print it out    *|
|*             as part of the error output.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      err_str  -> A string ptr to a description of the field in trouble.    *|
|*      attr_idx -> Index of the attr aux entry in trouble.                   *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
attr_aux_tbl_type	*attr_aux_var_error(char	*err_str,
					    int	 attr_idx)
{
static   int	been_here_before;

   if (been_here_before == 0) {
      been_here_before = 1;
      print_at_all(attr_idx);
      PRINTMSG(stmt_start_line, 42, Internal,stmt_start_col, attr_idx, err_str);
   }
   return(attr_aux_tbl);
}
# endif

# ifdef _DEBUG
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Issue internal err msg, if there is a bounds tbl variant problem.     *|
|*      NOTE:  been_here_before is a way of stopping a nasty infinite loop    *|
|*             if you get an error in a field, and you try to print it out    *|
|*             as part of the error output.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      err_str  -> A string ptr to a description of the field in trouble.    *|
|*      bd_idx   -> Index of the bounds entry in trouble.                     *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
bounds_tbl_type   *bd_var_error(char    *err_str,
                                int      bd_idx)
{
static   int    been_here_before;

   if (been_here_before == 0) {
      been_here_before = 1;
      print_bd(bd_idx);
      PRINTMSG(stmt_start_line, 1367, Internal,stmt_start_col, bd_idx, err_str);
   }
   return(bounds_tbl);
}
# endif

# ifdef _DEBUG
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Issue internal err msg, if there is an ir list tbl variant problem.   *|
|*      NOTE:  been_here_before is a way of stopping a nasty infinite loop    *|
|*             if you get an error in a field, and you try to print it out    *|
|*             as part of the error output.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      err_str  -> A string ptr to a description of the field in trouble.    *|
|*      attr_idx -> Index of the attr entry in trouble.                       *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
ir_list_tbl_type   *ir_list_var_error(char    *err_str,
                                      int      il_idx)
{
static   int    been_here_before;

   if (been_here_before == 0) {
      been_here_before = 1;
      print_il(il_idx);
      PRINTMSG(stmt_start_line, 782, Internal,stmt_start_col, il_idx, err_str);
   }
   return(ir_list_tbl);
}
# endif

# ifdef _DEBUG

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Issue internal err msg, if there is a global attr table variant	      *|
|*	problem.							      *|
|*      NOTE:  been_here_before is a way of stopping a nasty infinite loop    *|
|*             if you get an error in a field, and you try to print it out    *|
|*             as part of the error output.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      err_str  -> A string ptr to a description of the field in trouble.    *|
|*      attr_idx -> Index of the global attr entry in trouble.                *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
global_attr_tbl_type	*ga_var_error(char	*err_str,
				      int	 ga_idx)
{
static   int	been_here_before;

   if (been_here_before == 0) {
      been_here_before = 1;
      print_ga(ga_idx);
      PRINTMSG(stmt_start_line, 42, Internal,stmt_start_col, ga_idx, err_str);
   }
   return(global_attr_tbl);
}
# endif

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Internal and debug labels start at @00001.			      *|
|*	Internal labels are numbered @00001 and debug labels are @D00001.     *|
|*	This routine creates an internal label, enters the label into the     *|
|*	symbol table and sets the attribute fields to reflect an internal lbl.*|
|*									      *|
|* Input parameters:							      *|
|*	label_type	The type of label - Lbl_Internal or Lbl_Debug.        *|
|*	label_line	The global line number of the label definition.       *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	lbl_attr_idx	Index to symbol table attribute entry for this lable. *|
|*									      *|
\******************************************************************************/

int gen_internal_lbl (int		label_line)

{
   int			attr_idx;
   int			length;
   id_str_type		name;


   TRACE (Func_Entry, "gen_internal_lbl", NULL);

   curr_internal_lbl++;

   CREATE_ID(name, " ", 1);

# if defined(_NO_AT_SIGN_IN_NAMES)
   length = (int) sprintf(name.string, "l.%05d", curr_internal_lbl);
# else
   length = (int) sprintf(name.string, "l@%05d", curr_internal_lbl);
# endif

# ifdef _HOST32
   length = strlen(name.string);
# endif


# ifdef _DEBUG  /* Have reached the maximum label - make it and issue message */
   if (curr_internal_lbl > MAX_GENERATED_LABELS) {
      PRINTMSG(label_line, 364, Limit, 0, MAX_GENERATED_LABELS);
   }
# endif

   attr_idx			= ntr_local_attr_list(name.string,
                                                      length,
                                                      label_line,
                                                      0);
   AT_OBJ_CLASS(attr_idx)	= Label;
   AT_COMPILER_GEND(attr_idx)	= TRUE;
   AT_REFERENCED(attr_idx)	= Referenced;
   ATL_CLASS(attr_idx)		= Lbl_Internal;

   if (! cdir_switches.vector) {
      ATL_NOVECTOR(attr_idx)  = TRUE;
   }

   /* Debug class is set to Ldbg_None by default for all internal labels */

   TRACE (Func_Exit, "gen_internal_lbl", NULL);

   return (attr_idx);

}  /* gen_internal_lbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	 Calculates the storage size in bits for the object represented by    *|
|*       the attr.  This means the total size of the array or structure or    *|
|*       pointer or whatever.  If a total size is requested, it is checked    *|
|*       against maximum memory size available on the machine.                *|
|*									      *|
|* Input parameters:							      *|
|*	 attr_idx	  -> Attr index of item to find size of.              *|
|*	 all_elements	  -> TRUE if a total size is requested.  If this is   *|
|*	                     FALSE, just return the size of one element.      *|
|*       check_array_size -> TRUE if check all constant sized things for max  *|
|*                           memory violations.                               *|
|*                           FALSE, just check non explicit shape constant    *|
|*                           size arrays for memory violations.  Need this    *|
|*                           check because we call this more than once for    *|
|*                           explicit shape constant arrays.                  *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	 A constant table index of the bit size.                              *|
|*									      *|
\******************************************************************************/
size_offset_type	stor_bit_size_of(int		 attr_idx,
				         boolean	 all_elements,
				         boolean	 check_array_size)
{
   int			bd_idx;
   size_offset_type	constant;
   boolean		issue_msg;
   size_offset_type	length;
   size_offset_type	max_storage_size;
   long			num;
   size_offset_type	num_chars;
   size_offset_type	result;
   int			type_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   long64		max_size;
# endif


   TRACE (Func_Entry, "stor_bit_size_of", NULL);

#ifdef KEY /* Bug 12553 */
   /* Because sizes and offsets are expressed in bits, we need more than
    * Integer_4 even in -m32 mode. */
   constant.type_idx	= Integer_8;
#else /* Bug 12553 */
   constant.type_idx	= CG_INTEGER_DEFAULT_TYPE;
#endif /* Bug 12553 */
   constant.fld		= NO_Tbl_Idx;
   C_TO_F_INT(constant.constant, 0, CG_INTEGER_DEFAULT_TYPE);

   if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {

      if (ATD_IM_A_DOPE(attr_idx)) {
#ifdef KEY /* Bug 6845 */
	 boolean is_array = (ATD_ARRAY_IDX(attr_idx) != NULL_IDX);
	 num = DV_HD_WORD_SIZE;
         if (is_array) {
	   int n_allocatable_cpnt = do_count_allocatable_cpnt(attr_idx,
	     is_array);
	   num +=
	     (DV_DIM_WORD_SIZE * (long) BD_RANK(ATD_ARRAY_IDX(attr_idx))) +
	     (n_allocatable_cpnt ?
	       ((n_allocatable_cpnt + 1) * DV_ALLOC_CPNT_OFFSET_WORD_SIZE) :
	       0);
	 }
	 /* OSP_467, #4, dope vector bit size */
	 num *= DV_BITS_PER_WORD;
#else /* KEY Bug 6845 */
         num =  (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) ?
                (TARGET_BITS_PER_WORD * (DV_HD_WORD_SIZE +
                    (DV_DIM_WORD_SIZE * 
                          (long) BD_RANK(ATD_ARRAY_IDX(attr_idx))))) :
                (DV_HD_WORD_SIZE * TARGET_BITS_PER_WORD);
#endif /* KEY Bug 6845 */
         C_TO_F_INT(constant.constant, num, CG_INTEGER_DEFAULT_TYPE);
      }
      else {

         type_idx = ATD_TYPE_IDX(attr_idx);

         switch (TYP_TYPE(type_idx)) {
         case Character:

            if (TYP_FLD(type_idx) == CN_Tbl_Idx) {
               constant.fld		= CN_Tbl_Idx;
               constant.idx		= CN_INTEGER_CHAR_BIT_IDX;
               num_chars.fld		= TYP_FLD(type_idx);
               num_chars.idx		= TYP_IDX(type_idx);

               /* Assumption is that this will always be ok.  Char  */
               /* length is checked before we get to this point.    */
 
               size_offset_binary_calc(&num_chars, 
                                       &constant,
                                        Mult_Opr,
                                       &constant);
            }

            break;

         case Structure:

            constant.fld	= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
            constant.idx	= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));
            break;

         case Typeless :
            C_TO_F_INT(constant.constant, TYP_BIT_LEN(type_idx), Integer_8);
            constant.type_idx	= Integer_8;

            align_bit_length(&constant, TARGET_BITS_PER_WORD);
            break;

         default:

# ifdef _DEBUG
            if (TYP_LINEAR(type_idx) == Err_Res) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 810, Internal,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
# endif
            C_TO_F_INT(constant.constant, 
                       storage_bit_size_tbl[TYP_LINEAR(type_idx)], 
                       CG_INTEGER_DEFAULT_TYPE);
         }

         bd_idx = ATD_ARRAY_IDX(attr_idx);

         if (all_elements) {

            if (bd_idx != NULL_IDX) {

               /* If this isn't an explicit shape, constant bound array, */
               /* num_array_elements becomes 0.                          */

               if (BD_ARRAY_SIZE(bd_idx) == Constant_Size ||
                   BD_ARRAY_SIZE(bd_idx) == Symbolic_Constant_Size) {
                  length.fld	= BD_LEN_FLD(bd_idx);
                  length.idx	= BD_LEN_IDX(bd_idx);
#ifdef KEY /* Bug 2242, 12553 */
		  /* Fix to 2242 blithely set this to CG_INTEGER_DEFAULT_TYPE
		   * always. It's safer to fetch the correct type from the
		   * constant, e.g. in case the default is i*4 but the correct
		   * type is i*8. Presumably we always have a CN_Tbl_Idx in
		   * this case, but if, we revert to the (perhaps incorrect,
		   * but no worse than before) fix for 2242. */
		  length.type_idx = (length.fld == CN_Tbl_Idx) ?
		     CN_TYPE_IDX(length.idx) :
		     CG_INTEGER_DEFAULT_TYPE;
#endif /* KEY Bug 2242, 12553 */

                  if (!size_offset_binary_calc(&length,
                                               &constant,
                                                Mult_Opr,
                                               &constant)) {

                     AT_DCL_ERR(attr_idx)	= TRUE;
                  }
               }
               else {
                  constant.fld	= CN_Tbl_Idx;
                  constant.idx	= CN_INTEGER_ZERO_IDX;
               }
            }

# if defined(_CHECK_MAX_MEMORY)

            if (!ATD_AUXILIARY(attr_idx) && 
                constant.fld == NO_Tbl_Idx &&
                (check_array_size ||
                 bd_idx == NULL_IDX ||
                 BD_ARRAY_CLASS(bd_idx) != Explicit_Shape ||
                 BD_ARRAY_SIZE(bd_idx) != Constant_Size)) {

               /* We cannot check arrays based on N$PE.              */
               /* Also, all explicit shape constant size arrays were */
               /* checked in array_dim_resolution.                   */

               issue_msg		= FALSE;
               max_storage_size.fld	= NO_Tbl_Idx;

#              if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
                  max_storage_size.type_idx	= Integer_8;

                  if (cmd_line_flags.s_pointer8) {
                     max_size = 0400000000000000000LL;
                     C_TO_F_INT(max_storage_size.constant,
                     	        max_size,
                     	        Integer_8);
 
                  }
                  else {  /* 2 ** 32 currently */
                     C_TO_F_INT(max_storage_size.constant, pow(2,32),Integer_8);
                  }

#              else
                  max_storage_size.type_idx	= Integer_8;

#                 if defined(_TARGET32)
                     C_TO_F_INT(max_storage_size.constant,
                                2147483616,
                                Integer_8);
# 	          else
                     C_TO_F_INT(max_storage_size.constant,
                               (MAX_STORAGE_SIZE_IN_WORDS*TARGET_BITS_PER_WORD),
                                Integer_8);
#                 endif
#              endif

               size_offset_logical_calc(&constant, 
                                        &max_storage_size, 
                                         Gt_Opr,
                                        &result);

               issue_msg = THIS_IS_TRUE(result.constant, result.type_idx);

               if (issue_msg) {

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

                  if (cmd_line_flags.s_pointer8) {
                     constant	= max_storage_size;
 
                     if (!AT_DCL_ERR(attr_idx)) {
                        AT_DCL_ERR(attr_idx) 	= TRUE;

                        if (AT_COMPILER_GEND(attr_idx)) {
                           ISSUE_EXPR_SIZE_EXCEEDED_MSG(AT_DEF_LINE(attr_idx),
                                                        AT_DEF_COLUMN(attr_idx),
                                                        Error);
                        }
                        else {
                           ISSUE_STORAGE_SIZE_EXCEEDED_MSG(attr_idx, Error);
                        }
                     }
                  }
                  else {
                     ATD_TOO_BIG_FOR_DV(attr_idx) = TRUE;
                  }
# else

                  if (target_t3e) {

                     if (AT_COMPILER_GEND(attr_idx)) {
                        ISSUE_EXPR_SIZE_EXCEEDED_MSG(AT_DEF_LINE(attr_idx),
                                                     AT_DEF_COLUMN(attr_idx),
                                                     Warning);
                     }
                     else {
                        ISSUE_STORAGE_SIZE_EXCEEDED_MSG(attr_idx, Warning);
                     }
                  }
                  else {
                     constant = max_storage_size;

                     if (!AT_DCL_ERR(attr_idx)) {
                        AT_DCL_ERR(attr_idx)       = TRUE;

                        if (AT_COMPILER_GEND(attr_idx)) {
                           ISSUE_EXPR_SIZE_EXCEEDED_MSG(AT_DEF_LINE(attr_idx),
                                                        AT_DEF_COLUMN(attr_idx),
                                                        Error);
                        }
                        else {
                           ISSUE_STORAGE_SIZE_EXCEEDED_MSG(attr_idx, Error);
                        }
                     }
                  }
# endif
               }
            }
# endif
         }
      }
   }

   TRACE (Func_Exit, "stor_bit_size_of", NULL);

   return(constant);

}  /* stor_bit_size_of */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Compiler temps start at $T1 and go on indefinitely.  (If we start     *|
|*	generating millions of temps, table sizes are going to blow.)         *|
|*      This does NOT link tmps into the tmp list, the caller must do this    *|
|*	if they need to be in the list.  SET YOUR OWN STORAGE BLOCK AND       *|
|*	TYPE.  This routine does not set them.                                *|
|*									      *|
|* Input parameters:							      *|
|*	tmp_line	 The global line number for the tmp definition.       *|
|*	tmp_column	 The column number for the tmp definition.            *|
|*      scope            If in a parallel region, set scope based on this     *|
|*                       Enum values are {Priv, Shared}                       *|
|*	add_to_attr_list TRUE means add this to the local SCP_ATTR_LIST       *|
|*	                 FALSE means don't add.  This means that the tmp      *|
|*	                 will not go through attr_semantics, be written out   *|
|*	                 to a module or go through the interface unless       *|
|*	                 the temp is special cased.                           *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	attr_idx	Index to symbol table attribute entry for this tmp.   *|
|*									      *|
\******************************************************************************/


char compiler_tmp_prefix[] = "t$";

int gen_compiler_tmp (int		tmp_line,
		      int		tmp_column,
		      task_scope_type	scope,
		      boolean		add_to_attr_list)

{
   		int		attr_idx;
   		int		length;
   static	int		curr_tmp	= 0;
   		id_str_type	name;
		int		np_idx;


   TRACE (Func_Entry, "gen_compiler_tmp", NULL);

   curr_tmp++;

   CREATE_ID(name, " ", 1);

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
   length = sprintf(name.string, "%s%d", compiler_tmp_prefix, curr_tmp);
# else
   sprintf(name.string, "t$%d", curr_tmp);
   length = strlen(name.string);
# endif

   if (add_to_attr_list) {
      attr_idx			= ntr_local_attr_list(name.string,
                                                      length,
                                                      tmp_line,
                                                      tmp_column);
   }
   else {
      NTR_NAME_POOL(&(name.words[0]), length, np_idx);

      NTR_ATTR_TBL(attr_idx);
      AT_DEF_LINE(attr_idx)	= tmp_line;
      AT_DEF_COLUMN(attr_idx)	= tmp_column;
      AT_NAME_LEN(attr_idx)	= length;
      AT_NAME_IDX(attr_idx)	= np_idx;
   }

   ATD_CLASS(attr_idx)		= Compiler_Tmp;
   AT_REFERENCED(attr_idx)	= Referenced;
   AT_COMPILER_GEND(attr_idx)	= TRUE;
   AT_TYPED(attr_idx)		= TRUE;            /* Prevent implicit errors */

   if (scope == Priv) {
      ADD_TMP_TO_PRIVATE_LIST(attr_idx);
   }
   else {
      ADD_TMP_TO_SHARED_LIST(attr_idx);
   }

   TRACE (Func_Exit, "gen_compiler_tmp", NULL);

   return (attr_idx);

}  /* gen_compiler_tmp */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Change a data object to a program unit.  If the input is Function, it *|
|*      creates a function result with the data object becoming the function  *|
|*	result of the program unit.  If input is Subroutine it creates a      *|
|*	a program unit marked Pgm_Unknown.  If input is Pgm_Unknown,  it will *|
|*      switch the attr to a Function, if there is anything set on the attr   *|
|*      which would trigger a Function set.  It creates a dummy_proc, if the  *|
|*	data object is a dummy argument.  This routine assumes that all       *|
|*	semantic errors are issued before the change occurs and that the attr *|
|*	is semantically correct to become a function result, a Pgm_Unit, or   *|
|*	a Subroutine.                                                         *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx - index of attribute entry to change.			      *|
|*	pgm_unit - Set to Function, Subroutine or Pgm_Unknown		      *|
|*	proc_type- Set to type procedure type for this item.  If it is set to *|
|*	           Extern_Proc, but this is a dummy argument, it becomes      *|
|*                 Dummy_Proc.						      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void chg_data_obj_to_pgm_unit(int 		attr_idx,
			      pgm_unit_type	pgm_unit,
			      atp_proc_type	proc_type)

{
#ifdef KEY /* Bug 10177 */
   int		new_at_idx = 0;
#else /* KEY Bug 10177 */
   int		new_at_idx;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "chg_data_obj_to_pgm_unit", NULL);

   /* If intent is set, an error should have been issued.  The intentness is */
   /* lost, but the fact that it is a dummy argument is retained.  If it is  */
   /* a dummy argument, it has to become a dummy proc.  The input proc_type  */
   /* would always be extern_proc in this case.  The Extern_Proc should be   */
   /* switched to Dummy_Proc.                                                */

   if (ATD_CLASS(attr_idx) == Dummy_Argument) {
      proc_type	= Dummy_Proc;
   }

  /* Check to see if this should be a Function */

   if (pgm_unit == Function ||
       (pgm_unit == Pgm_Unknown && (AT_TYPED(attr_idx) ||
                                    ATD_TARGET(attr_idx) ||
                                    ATD_POINTER(attr_idx) ||
                                    ATD_ARRAY_IDX(attr_idx) != NULL_IDX))) {

      NTR_ATTR_TBL(new_at_idx);			/* Create func result entry */
      COPY_ATTR_NTRY(new_at_idx, attr_idx);/* Copy data to func rslt */
      AT_CIF_SYMBOL_ID(new_at_idx)	= 0;
      ATD_CLASS(new_at_idx)		= Function_Result;
      ATD_FUNC_IDX(new_at_idx)		= attr_idx;
      pgm_unit				= Function;
   }

   CLEAR_VARIANT_ATTR_INFO(attr_idx, Pgm_Unit);	/* Clear to make pgm unit   */
   ATP_PGM_UNIT(attr_idx)	= pgm_unit;
   MAKE_EXTERNAL_NAME(attr_idx, AT_NAME_IDX(attr_idx), AT_NAME_LEN(attr_idx));
   ATP_PROC(attr_idx)		= proc_type;

  
   /* Set scope to current scope for now.  attr_link_resolution will reset */
   /* the scope if it ends up being host associated.                       */

   ATP_SCP_IDX(attr_idx)	= curr_scp_idx;

   if (pgm_unit == Function) {
      ATP_RSLT_IDX(attr_idx)	= new_at_idx;
   }

   TRACE (Func_Exit, "chg_data_obj_to_pgm_unit", NULL);

   return;

}  /* chg_data_obj_to_pgm_unit */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine returns a type string suitable for printing in error msgs*|
|*	WARNING:  If either of the input types can be structures, you cannot  *|
|*	          call this routine twice with the same PRINTMSG.  strcat the *|
|*	          result to a character array and then call it again.         *|
|*									      *|
|* Input parameters:							      *|
|*	type     - The type                           	 		      *|
|*	type_idx - the type idx union	                                      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	a pointer to a character string, describing the type 		      *|
|*									      *|
\******************************************************************************/
char *get_basic_type_str(int			type_idx)
{
   		char	*str;
   static	char	 str1[45];

#ifdef KEY /* Bug 5040 */
   static char *type_strings[Num_Linear_Types] = {
     /* Err_Res */		"[Internal Error0]",
     /* Short_Char_Const */	"CHARACTER",
     /* Short_Typeless_Const */	"BOOLEAN",
     /* Typeless_1 */		"TYPELESS/HOLLERITH",
     /* Typeless_2 */		"TYPELESS/HOLLERITH",
     /* Typeless_4 */		"BOOLEAN",
     /* Typeless_8 */		"BOOLEAN",
     /* Long_Typeless */	"TYPELESS/HOLLERITH",
     /* Integer_1 */		"INTEGER(KIND=1)",
     /* Integer_2 */		"INTEGER(KIND=2)",
     /* Integer_4 */		"INTEGER(KIND=4)",
     /* Integer_8 */		"INTEGER(KIND=8)",
     /* Real_4 */		"REAL(KIND=4)",
     /* Real_8 */		"REAL(KIND=8)",
     /* Real_16 */		"REAL(KIND=16)",
     /* Complex_4 */		"COMPLEX(KIND=4)",
     /* Complex_8 */		"COMPLEX(KIND=8)",
     /* Complex_16 */		"COMPLEX(KIND=16)",
     /* CRI_Ptr_8 */		"Cray pointer",
     /* Logical_1 */		"LOGICAL(KIND=1)",
     /* Logical_2 */		"LOGICAL(KIND=2)",
     /* Logical_4 */		"LOGICAL(KIND=4)",
     /* Logical_8 */		"LOGICAL(KIND=8)",
     /* Character_1 */		"CHARACTER",
     /* Character_2 */		"CHARACTER",
     /* Character_4 */		"CHARACTER",
     /* CRI_Ch_Ptr_8 */		"Cray character pointer",
     /* Structure_Type */	"[Internal Error1]",
     /* CRI_Parcel_Ptr_8 */	"Cray parcel pointer"
     };
# ifdef _DEBUG
   /* Make sure the initializer isn't missing any elements */
   if (0 == type_strings[Num_Linear_Types - 1]) { abort(); }
# endif /* _DEBUG */
#endif /* KEY Bug 5040 */

   TRACE (Func_Entry, "get_basic_type_str", NULL);

   switch (TYP_TYPE(type_idx)) {

#ifdef KEY /* Bug 5040 */
#else
      case Typeless:
         if (TYP_LINEAR(type_idx) == Typeless_4 ||
             TYP_LINEAR(type_idx) == Typeless_8 ||
             TYP_LINEAR(type_idx) == Short_Typeless_Const) {
            str = "BOOLEAN";
         }
         else {
            str = "TYPELESS";
         }
         break;

      case Integer:
         str = "INTEGER";
         break;

      case Logical:
         str = "LOGICAL";
         break;

      case Real:
         str = (TYP_LINEAR(type_idx) <= REAL_DEFAULT_TYPE) ? "REAL" :
                                                             "DOUBLE PRECISION";
         break;

      case Complex:
         str = (TYP_LINEAR(type_idx) <= COMPLEX_DEFAULT_TYPE) ? "COMPLEX":
                                                               "DOUBLE COMPLEX";
         break;

      case Character:
         str =  "CHARACTER";
         break;
#endif /* KEY Bug 5040 */

      case Structure:
         str1[0] =  '\0';
         strcat(str1, "type(");
         strcat(str1, AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
         strcat(str1, ")");
         str  =  str1;
         break;

#ifdef KEY /* Bug 5040 */
      default:
	 str = type_strings[TYP_LINEAR(type_idx)];
	 break;
#else /* KEY Bug 5040 */
      case CRI_Ptr:
         str = "Cray pointer";
         break;

      case CRI_Ch_Ptr:
         str = "Cray character pointer";
         break;

      case CRI_Parcel_Ptr:
         str = "Cray parcel pointer";
         break;
#endif /* KEY Bug 5040 */

   }  /* End switch */

   TRACE (Func_Exit, "get_basic_type_str", NULL);

   return(str);

}  /* get_basic_type_str */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Verify that the kind type is valid, and return the corresponding      *|
|*	aux type.  (If it's character or err, return the input aux type.)     *|
|*									      *|
|* Input parameters:							      *|
|*	opnd	  - An operand holding the kind type.  This operand	      *|
|*		       will not be changed, so you can pass the IR 	      *|
|*	attr_idx  - Attr index to get the updated type index.                 *|
|*	kind0seen							      *|
|*	kind0E0seen							      *|
|*	kind0D0seen							      *|
|*	kindconstseen							      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
boolean	kind_to_linear_type(opnd_type		*opnd,
			    int			 attr_idx,
			    boolean		 kind0seen,
			    boolean		 kind0E0seen,
			    boolean		 kind0D0seen,
			    boolean		 kindconstseen)


{
   int			column;
   boolean		error		= FALSE;
   long			kind;
   int			line;
   linear_type_type	linear_type	= Err_Res;
   basic_type_type	type;
   int			type_idx;


   TRACE (Func_Entry, "kind_to_linear_type", NULL);

   type	= TYP_TYPE(ATD_TYPE_IDX(attr_idx));

   if (OPND_FLD((*opnd)) != CN_Tbl_Idx ||
       TYP_TYPE(CN_TYPE_IDX(OPND_IDX((*opnd)))) != Integer) {

      find_opnd_line_and_column(opnd, &line, &column);
      PRINTMSG(line, 770, Error, column);
      error = TRUE;

      /* For error recovery use the default type */

      switch (type) {
      case Integer:
         type_idx	= INTEGER_DEFAULT_TYPE;
         break;

      case Logical:
         type_idx	= LOGICAL_DEFAULT_TYPE;
         break;

      case Real:
         type_idx	= REAL_DEFAULT_TYPE;
         break;

      case Complex:
         type_idx	= COMPLEX_DEFAULT_TYPE;
         break;

      default:          /* Includes Character */
         type_idx	= ATD_TYPE_IDX(attr_idx);
         break;
      }
   }
   else {
      kind	= (long) CN_INT_TO_C(OPND_IDX((*opnd)));

      error	= validate_kind(type,
                                OPND_LINE_NUM((*opnd)),
                                OPND_COL_NUM((*opnd)),
                                &kind,
                                &linear_type);

      if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
         type_tbl[TYP_WORK_IDX]		= type_tbl[ATD_TYPE_IDX(attr_idx)];
      }
      else {
         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)		= type;
         TYP_LINEAR(TYP_WORK_IDX)	= linear_type;
      }

      TYP_DCL_VALUE(TYP_WORK_IDX)	= kind;
      TYP_DESC(TYP_WORK_IDX)		= Kind_Typed;

      if ((kind0seen &&
           (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Logical ||
            TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Integer)) ||
          (kind0E0seen && TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Real)) {

         /* change to a default type idx */

         TYP_DESC(TYP_WORK_IDX) = Default_Typed;
      }
      else if (kind0D0seen && TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Real) {
         TYP_KIND_DOUBLE(TYP_WORK_IDX) = TRUE;
      }
      else if (kindconstseen) {
         TYP_KIND_CONST(TYP_WORK_IDX) = TRUE;
      }

      type_idx				= ntr_type_tbl();
   }

   ATD_TYPE_IDX(attr_idx)		= type_idx;

   TRACE (Func_Exit, "kind_to_linear_type", NULL);

   return(error);

}   /* kind_to_linear_type */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This generates a debug label before the input statement.	      *|
|*	Debug labels are numbered z@00001.                                    *|
|*	This routine creates the debug label and the compiler generated       *|
|*	continue statement, before the input statement.  It sets              *|
|*	ATL_DEBUG_CLASS to the input debug class.                             *|
|*									      *|
|* Input parameters:							      *|
|*	stmt_idx 	The SH idx which needs a label before it.             *|
|*	label_type	The debug label class.                                *|
|*	attr_idx	If NULL - make new attr, else use this one for label  *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	 NONE								      *|
|*									      *|
\******************************************************************************/
int	gen_debug_lbl_stmt(int			stmt_idx,
			   atl_debug_class_type	label_type,
			   int			attr_idx)

{
   int			ir_idx;
   int			length;
   id_str_type		name;
   int			save_curr_stmt_sh_idx;

# if defined(_NO_AT_SIGN_IN_NAMES)
   char			label_name[7] = "z.%05d";
# else
   char			label_name[7] = "z@%05d";
# endif


   TRACE (Func_Entry, "gen_debug_lbl_stmt", NULL);
   
   if (attr_idx == NULL_IDX) {
      curr_debug_lbl++;

      CREATE_ID(name, " ", 1);

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
      length = sprintf (name.string, label_name, curr_debug_lbl);
# else
      sprintf(name.string, label_name, curr_debug_lbl);
      length = strlen(name.string);
# endif

# ifdef _DEBUG
      /* Have reached the maximum label - make it and issue message */

      if (curr_debug_lbl > MAX_GENERATED_LABELS) {
         PRINTMSG(SH_GLB_LINE(stmt_idx), 364, Limit, 0, MAX_GENERATED_LABELS);
      }
# endif

      attr_idx	= ntr_local_attr_list(name.string,
                                      length,
                                      SH_GLB_LINE(stmt_idx),
                                      0);

      AT_OBJ_CLASS(attr_idx)		= Label;
      AT_COMPILER_GEND(attr_idx)	= TRUE;
      ATL_CLASS(attr_idx)		= Lbl_Debug;
      ATL_DEBUG_CLASS(attr_idx)		= label_type;
      AT_DEFINED(attr_idx)		= TRUE;
      ATL_DEF_STMT_IDX(attr_idx)	= curr_stmt_sh_idx;
   }

   save_curr_stmt_sh_idx	= curr_stmt_sh_idx;

   if (SH_LABELED(stmt_idx)) {
      stmt_idx = SH_PREV_IDX(stmt_idx);
   }

   curr_stmt_sh_idx		= stmt_idx;

   gen_sh(Before,
          Continue_Stmt,
          SH_GLB_LINE(stmt_idx),
          SH_COL_NUM(stmt_idx),
          FALSE,			/* No errors */
          TRUE,				/* Labeled */
          TRUE);			/* Compiler generated */

   stmt_idx			= SH_PREV_IDX(curr_stmt_sh_idx);
   curr_stmt_sh_idx		= save_curr_stmt_sh_idx;
   SH_P2_SKIP_ME(stmt_idx)	= TRUE;
   
   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(stmt_idx)		= ir_idx;
   IR_OPR(ir_idx)		= Label_Opr;
   IR_TYPE_IDX(ir_idx)          = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)		= SH_GLB_LINE(stmt_idx);
   IR_COL_NUM(ir_idx)		= SH_COL_NUM(stmt_idx);
   IR_LINE_NUM_L(ir_idx)	= SH_GLB_LINE(stmt_idx);
   IR_COL_NUM_L(ir_idx)		= SH_COL_NUM(stmt_idx);
   IR_FLD_L(ir_idx)		= AT_Tbl_Idx;
   IR_IDX_L(ir_idx)		= attr_idx;

   TRACE (Func_Exit, "gen_debug_lbl_stmt", NULL);

   return(stmt_idx);

}  /* gen_debug_lbl_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This takes as input a string and sticks _in_PROC after it for each    *|
|*	parent procedure above this child.                                    *|
|*									      *|
|* Input parameters:							      *|
|*	name_str_idx	A name_pool index to the beginning string.            *|
|*	name_str_len	A character length of the beginning string.           *|
|*	scp_idx		The first scope to use for appending.                 *|
|*									      *|
|* Output parameters:							      *|
|*	name_len	A pointer to the length of the new string.            *|
|*									      *|
|* Returns:								      *|
|*	name_pool_idx for the new string				      *|
|*									      *|
\******************************************************************************/
int	make_in_parent_string(int	 name_str_idx,
			      int	 name_str_len,
			      int	 scp_idx,
			      int	*name_len)
{
   int		idx;
   int		length;
   int		new_name_idx;


   TRACE (Func_Entry, "make_in_parent_string", NULL);

   new_name_idx = name_pool_idx + 1;
   length	= name_str_len;

   TBL_REALLOC_CK(name_pool, HOST_BYTES_TO_WORDS(MAX_EXTERNAL_ID_LEN));

   for (idx = new_name_idx; idx <= name_pool_idx; idx++) {
      name_pool[idx].name_long = 0;
   }


   strcat((char *)&name_pool[new_name_idx], (char *)&name_pool[name_str_idx]);

   while (scp_idx != NULL_IDX) {
      strcat((char *)&name_pool[new_name_idx], UNIQUE_PROC_CONNECTOR);
#ifdef KEY /* Bug 5089 */
      int attr_idx = SCP_ATTR_IDX(scp_idx);
      char *appendage;
      int appendage_len;
      /* Because two modules might contain identically-named procedures,
       * we append the module name to the procedure name to avoid conflict.
       * Until F2003, this was sufficient.
       *
       * But an F2003 intrinsic module may have the same name as a nonintrinsic
       * (user-coded) module. To avoid collisions on the module names
       * themselves, we elsewhere generate different external (linker) names
       * for the modules. Both modules may still contain identically named
       * procedures, so we must avoid collisions of these as well. We could
       * just append the external name of the module in both cases, but for
       * backward compatibility with .o files generated prior to the addition
       * of intrinsic modules, we continue to append the non-external name in
       * the case of a nonintrinsic module. */
      if (AT_IS_INTRIN(attr_idx) &&
	Pgm_Unit == AT_OBJ_CLASS(attr_idx) &&
	Module == ATP_PGM_UNIT(attr_idx)) {
        appendage = ATP_EXT_NAME_PTR(attr_idx);
	appendage_len = ATP_EXT_NAME_LEN(attr_idx);
      } else {
        appendage = AT_OBJ_NAME_PTR(attr_idx);
	appendage_len = AT_NAME_LEN(attr_idx);
      }
      strcat((char *)&name_pool[new_name_idx], appendage);
      length += appendage_len + UNIQUE_PROC_LEN;
#else /* KEY Bug 5089 */
      strcat((char *)&name_pool[new_name_idx],
             AT_OBJ_NAME_PTR(SCP_ATTR_IDX(scp_idx)));

      length	= length + AT_NAME_LEN(SCP_ATTR_IDX(scp_idx)) + UNIQUE_PROC_LEN;
#endif /* KEY Bug 5089 */
      scp_idx	= SCP_PARENT_IDX(scp_idx);
   }

   name_pool_idx = name_pool_idx - (HOST_BYTES_TO_WORDS(MAX_EXTERNAL_ID_LEN) -
                   WORD_LEN(length));
   *name_len	 = length;

   TRACE (Func_Exit, "make_in_parent_string", NULL);

   return(new_name_idx);

}  /* make_in_parent_string */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
int	compare_names(long	*id1,
		      int	 id1_len,
		      long	*id2,
		      int	 id2_len)
				
{
   int		i;
   long		matched		= -1;


   TRACE (Func_Entry, "compare_names", NULL);

   matched = strncmp((char *) id1, (char *) id2,
		     WORD_LEN((id1_len > id2_len) ? id1_len : id2_len) *
			sizeof(long));
  if (matched)
    matched = matched > 0 ? 1 : -1;

  TRACE (Func_Exit, "compare_names", NULL);

   return(matched);

}   /* compare_names */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      length of name                                                        *|
|*      defining line                                                         *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      attr tbl index where entry is put                                     *|
|*                                                                            *|
\******************************************************************************/

int ntr_local_attr_list(char *name_str,
                        int   name_len,
		        int   def_line,
		        int   def_column)
 
{
   int		 attr_idx;
   long		*id;
   int		 np_idx;


   TRACE (Func_Entry, "ntr_local_attr_list", NULL);

   id = (long *) name_str;

   NTR_NAME_POOL(id, name_len, np_idx);

   NTR_ATTR_TBL(attr_idx);
   AT_DEF_LINE(attr_idx)	= def_line;
   AT_DEF_COLUMN(attr_idx)	= def_column;
   AT_NAME_LEN(attr_idx)	= name_len;
   AT_NAME_IDX(attr_idx)	= np_idx;

   ADD_ATTR_TO_LOCAL_LIST(attr_idx);

   TRACE (Func_Exit, "ntr_local_attr_list", NULL);

   return(attr_idx);

}   /* ntr_local_attr_list */

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
|*                                                                            *|
\******************************************************************************/

int	create_lib_entry_attr(char *name_str,
			      int   name_len,
			      int   def_line,
			      int   def_column)
 
{
   int		attr_idx;
   id_str_type	name;
   int		np_idx;


   TRACE (Func_Entry, "create_lib_entry_attr", NULL);

   CREATE_ID(name, name_str, name_len);
   NTR_NAME_POOL(&(name.words[0]), name_len, np_idx);
   NTR_ATTR_TBL(attr_idx);
   AT_OBJ_CLASS(attr_idx)	= Pgm_Unit;
   AT_REFERENCED(attr_idx)	= Referenced;
   AT_COMPILER_GEND(attr_idx)	= TRUE;
   ATP_PGM_UNIT(attr_idx)	= Subroutine;
   ATP_SCP_IDX(attr_idx)	= curr_scp_idx;
   ATP_PROC(attr_idx)		= Extern_Proc;
   AT_NAME_IDX(attr_idx)	= np_idx;
   AT_NAME_LEN(attr_idx)	= name_len;
   ATP_EXT_NAME_IDX(attr_idx)	= np_idx;
   ATP_EXT_NAME_LEN(attr_idx)	= name_len;
   AT_DEF_LINE(attr_idx)	= def_line;
   AT_DEF_COLUMN(attr_idx)	= def_column;

   TRACE (Func_Exit, "create_lib_entry_attr", NULL);

   return(attr_idx);

}   /* create_lib_entry_attr */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void	set_stride_for_first_dim(int			 type_idx,
				 size_offset_type	*stride)
{
   long64		length;
   size_offset_type	result;


   TRACE (Func_Entry, "set_stride_for_first_dim", NULL);

# ifdef _SM_UNIT_IS_ELEMENT

   (*stride).fld	= CN_Tbl_Idx;
   (*stride).idx	= CN_INTEGER_ONE_IDX;

# else

   switch (TYP_TYPE(type_idx)) {

   case Typeless:
      length		= STORAGE_WORD_SIZE(TYP_BIT_LEN(type_idx));
      (*stride).fld	= CN_Tbl_Idx;
      (*stride).idx	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, length);
      break;

   case Integer:
   case Logical:
   case CRI_Ptr:
   case CRI_Ch_Ptr:
   case Real:
   case Complex:
      /* OSP_467, #2, use the multiple of INTEGER_SIZE as the stride */
      length            = BITS_TO_INTEGER_DEFAULT_WORDS(
                              storage_bit_size_tbl[TYP_LINEAR(type_idx)],
                              storage_bit_size_tbl[CG_INTEGER_DEFAULT_TYPE] );

      (*stride).fld	= CN_Tbl_Idx;
      (*stride).idx	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, length);
      break;

   case Character:  /* This is really number of bytes */
# if defined(_EXTENDED_CRI_CHAR_POINTER)
      if (TYP_FLD(type_idx) == AT_Tbl_Idx &&
          AT_OBJ_CLASS(TYP_IDX(type_idx)) == Data_Obj &&
          (TYP_TYPE(ATD_TYPE_IDX(TYP_IDX(type_idx))) == CRI_Ch_Ptr ||
           TYP_TYPE(ATD_TYPE_IDX(TYP_IDX(type_idx))) == CRI_Ptr)) {

         /* This is a character pointee with assumed length. */
         /* Set the stride multiplier to one.                */

         (*stride).fld	= CN_Tbl_Idx;
         (*stride).idx	= CN_INTEGER_ONE_IDX;
      }
      else {
         (*stride).fld	= TYP_FLD(type_idx);
         (*stride).idx	= TYP_IDX(type_idx);
      }
# else
      (*stride).fld	= TYP_FLD(type_idx);
      (*stride).idx	= TYP_IDX(type_idx);
# endif
      break;

   case Structure:

      if (ATT_CHAR_SEQ(TYP_IDX(type_idx))) {

         /* stride is in bytes, just like character */

         result.idx		= CN_INTEGER_THREE_IDX;
         result.fld		= CN_Tbl_Idx;
         (*stride).fld		= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
         (*stride).idx		= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));

         if (!size_offset_binary_calc(&(*stride),
                                      &result,
                                       Shiftr_Opr,
                                      &(*stride))) {

            (*stride).fld	= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
            (*stride).idx	= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));
         }
      }
      else {
         (*stride).fld	= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
         (*stride).idx	= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));

# if defined(_TARGET64) && defined(_WHIRL_HOST64_TARGET64)
         BITS_TO_WORDS((*stride), TARGET_BITS_PER_WORD/2);
# else
         BITS_TO_WORDS((*stride), TARGET_BITS_PER_WORD);
# endif /* defined(_TARGET64) && defined(_WHIRL_HOST64_TARGET64) */
      }

      if ((*stride).fld == NO_Tbl_Idx) {
         (*stride).fld	= CN_Tbl_Idx;
         (*stride).idx	= ntr_const_tbl((*stride).type_idx,
                                        FALSE,
                                        (*stride).constant);
      }

      break;

   }  /* end switch */
# endif

   TRACE (Func_Exit, "set_stride_for_first_dim", NULL);

   return;

}   /* set_stride_for_first_dim */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*	This routine adds new types to the type table.  It attempts to share  *|
|*	them all.  If you are entering Typeless, pass Err_Res                 *|
|*      for the lin_type, and this routine will set it correctly.)            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
int	ntr_type_tbl(void)

{
   boolean	 found;
   int		 i;
   int		 new_type_idx;
   long		*null_base;
   long		*type_tbl_base;


   TRACE (Func_Entry, "ntr_type_tbl", NULL);

   switch (TYP_TYPE(TYP_WORK_IDX)) {
   case Integer:
   case Logical:
   case Real:
   case Complex:

      if (TYP_DESC(TYP_WORK_IDX) == Default_Typed && 
          TYP_LINEAR(TYP_WORK_IDX) != Err_Res) {
         new_type_idx = TYP_LINEAR(TYP_WORK_IDX);
         goto EXIT;
      }
      break;

   case CRI_Ptr:

      if (TYP_PTR_INCREMENT(TYP_WORK_IDX) != 0 && 
          TYP_PTR_INCREMENT(TYP_WORK_IDX) != TARGET_BITS_PER_WORD) {
         break;
      }

   case CRI_Parcel_Ptr:
   case CRI_Ch_Ptr:
      new_type_idx = TYP_LINEAR(TYP_WORK_IDX);
      goto EXIT;

   case Typeless:
      
      if (TYP_LINEAR(TYP_WORK_IDX) == Err_Res) {

         switch (TYP_BIT_LEN(TYP_WORK_IDX)) {
         case 32:
            TYP_LINEAR(TYP_WORK_IDX) = Typeless_4;
            break;

         case 64:
            TYP_LINEAR(TYP_WORK_IDX) = Typeless_8;
            break;

         default: 
            TYP_LINEAR(TYP_WORK_IDX) = Long_Typeless;
            break;
         }
      }
      break;

   case Character:
      TYP_LINEAR(TYP_WORK_IDX)	= (TYP_LINEAR(TYP_WORK_IDX) == Err_Res)? 
                                              CHARACTER_DEFAULT_TYPE : 
                                              TYP_LINEAR(TYP_WORK_IDX);
      break;

   case Structure:
      break;
   }

   null_base = (long *) type_tbl;

   for (new_type_idx = 1; new_type_idx  <= type_tbl_idx; new_type_idx++) {
      found		= TRUE;
      type_tbl_base	= (long *) &(type_tbl[new_type_idx]);

      for (i = 0; i < NUM_TYP_WDS; i++) {

         if (null_base[i] != type_tbl_base[i]) {
             found = FALSE;
          }
      }

      if (found) {
         goto EXIT;
      }
   }

   TBL_REALLOC_CK(type_tbl, 1);
   new_type_idx			= type_tbl_idx;
   type_tbl[new_type_idx]	= type_tbl[TYP_WORK_IDX];

EXIT: 

   TRACE (Func_Exit, "ntr_type_tbl", NULL);

   return(new_type_idx);

}   /* ntr_type_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      srch_linked_sn searches linked lists in the secondary name table.     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*	name		Char pointer of name to look for.	              *|
|*      length		Length of name to look for.	                      *|
|*      sn_idx		Secondary name table index to start search.   	      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      sn_idx          Secondary name table index if found.                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      attribute table index of member if found                              *|
|*      NULL_IDX		        if not found                          *|
|*                                                                            *|
\******************************************************************************/
int srch_linked_sn(char		*name,
		  int		 length,
		  int		*sn_idx)

{
   		int		 attr_idx;
   register	int		 i;
   register	int		 id_wd_len; 
   register	long		*id;
   register	long		*id1;
   register	long		 matched;


   TRACE (Func_Entry, "srch_linked_sn", name);

   id		= (long *) name;
   id_wd_len	= WORD_LEN(length);      
   matched	= -1;
   attr_idx	= NULL_IDX;

   while (*sn_idx != NULL_IDX) {

      if (SN_NAME_LEN(*sn_idx) == length) {
         id1 = (long *) &(name_pool[SN_NAME_IDX(*sn_idx)]);

# if !(defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
#        pragma _CRI shortloop
# endif

         for (i = 0; i < id_wd_len; i++) {
            matched = id[i] - id1[i];

            if (matched != 0) {
               break;
            }
         }

         if (matched == 0) {
            attr_idx = SN_ATTR_IDX(*sn_idx);
            break;
         }
      }

      *sn_idx = SN_SIBLING_LINK(*sn_idx);
   }

   TRACE (Func_Exit, "srch_linked_sn", NULL);

   return (attr_idx); 

}  /*  srch_linked_sn  */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Free memory before calling backends                                   *|
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

void free_tables()

{
   TRACE (Func_Entry, "free_tables", NULL);

   /* program_unit_name is used by messages.c and cif.c, after the tables  */
   /* are gone.   PDGCS issues messages and needs a program name to list.  */
   /* Can always copy 1 char more, because the namepool is zero filled and */
   /* a zero always has to end the name.                                   */

   strncpy(program_unit_name,
           AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)),
           AT_NAME_LEN(SCP_ATTR_IDX(curr_scp_idx))+1);

   /* Clear because there is no table left. */

   curr_stmt_sh_idx		= NULL_IDX;
   curr_scp_idx			= NULL_IDX;
   expanded_intrinsic_list	= NULL_IDX;

   TBL_FREE (pdg_link_tbl);
   TBL_FREE (attr_list_tbl);
   TBL_FREE (attr_tbl);
   TBL_FREE (attr_aux_tbl);
   TBL_FREE (bounds_tbl);
   TBL_FREE (const_tbl);
   TBL_FREE (const_pool);
   TBL_FREE (sec_name_tbl);
   TBL_FREE (stor_blk_tbl);
   TBL_FREE (loc_name_tbl);
   TBL_FREE (name_pool);
   TBL_FREE (scp_tbl);
   TBL_FREE (type_tbl);
   TBL_FREE (ir_tbl);
   TBL_FREE (sh_tbl);
   TBL_FREE (ir_list_tbl);
   TBL_FREE (hidden_name_tbl);

   TRACE (Func_Exit, "free_tables", NULL);

   return;

}  /* free_tables */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Verify that this is a valid kind for this type and machine.           *|
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

boolean	validate_kind(basic_type_type	 type,
		      int		 line,
		      int		 column,
		      long		*kind,
		      linear_type_type	*linear_type)

{
   boolean		ok			= TRUE;
   char			kind_str[32];


   TRACE (Func_Entry, "validate_kind", NULL);

   switch (type) {

   case Integer:

      switch(*kind) {
         case 1:
            *linear_type = Integer_1;
            break;

         case 2:
            *linear_type = Integer_2;
            break;

         case 4:
            *linear_type = Integer_4;
            break;

         case 8:
            *linear_type = Integer_8;
            break;

         default:
            *linear_type = INTEGER_DEFAULT_TYPE;
            ok		 = FALSE;
            break;
      }
      break;


   case Logical:

      switch(*kind) {
         case 1:
            *linear_type = Logical_1;
            break;

         case 2:
            *linear_type = Logical_2;
            break;

         case 4:
            *linear_type = Logical_4;
            break;

         case 8:
            *linear_type = Logical_8;
            break;

         default:
            *linear_type = LOGICAL_DEFAULT_TYPE;
            ok		 = FALSE;
            break;
      }
      break;


   case Real:

      switch(*kind) {
         case 4:
            *linear_type = Real_4;
            break;

         case 8:
            *linear_type = Real_8;
            break;

         case 16:
            *linear_type = Real_16;

# if defined(_TARGET_OS_MAX)
            PRINTMSG(line, 543, Warning, column, 16, 8);
            *linear_type = Real_8;
# elif defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)
            PRINTMSG(line, 541, Error, column);
# endif
            break;

         default:
            ok		 = FALSE;
            break;
      }
      break;


   case Complex:

      switch(*kind) {
         case 4:
            *linear_type = Complex_4;
            break;

         case 8:
            *linear_type = Complex_8;
            break;

         case 16:
            *linear_type = Complex_16;

# if defined(_TARGET_OS_MAX)
            PRINTMSG(line, 543, Warning, column, 16, 8);
            *linear_type = Complex_8;
# elif defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)
            PRINTMSG(line, 541, Error, column);
# endif
            break;

         default:
            *linear_type = COMPLEX_DEFAULT_TYPE;
            ok		= FALSE;
            break;
      }
      break;


   case Character:

      switch(*kind) {
         case 1:
            *linear_type = Character_1;
            break;

         default:
            *linear_type = CHARACTER_DEFAULT_TYPE;
            ok		 = FALSE;
            break;
      }
      break;


   default:
      *linear_type	= Err_Res;
      ok		= FALSE;
      break;

   }   /* End switch */

   if (!ok) {
      sprintf(kind_str,"%ld", *kind);
      PRINTMSG(line, 130, Error, column,
               kind_str,
               basic_type_str[type]);
      *kind = 0;
   }

   TRACE (Func_Exit, "validate_kind", NULL);

   return(ok);

}  /* validate_kind */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Assign the offset to an item in a storage block.                      *|
|*      Offsets are assigned as follows for MIPS.                             *|
|*      Complex*32 and complex*(kind=16) is aligned on a 4 word boundary if   *|
|*                                      -align32 is not specified.            *|
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
|* Info:								      *|
|*									      *|
|* Commandline control:   -a dalign					      *|
|*									      *|
|* SGI: This option cannot be specified by the user.  The option is on by     *|
|*      default, as the SGI commandline processor (in whirl) sets it on.      *|
|*      The only way it can be disabled is by user option -align32	      *|
|*      User option -align64 causes -a dalign ON.			      *|
|*      (Se FE_align global variable in sgi_cmd_line.cxx)		      *|
|*									      *|
|*      DEFAULT:  -a dalign ON						      *|
|*									      *|
|* SUN: -a dalign is off by default.  The user may specify the option.        *|
|*									      *|
|*      DEFAULT:  -a dalign OFF						      *|
|*									      *|
|* The following are the classes of items that may be daligned:		      *|
|*      Common block members						      *|
|*      Numeric sequence components					      *|
|*      All other components						      *|
|*      Static local, module and stack data				      *|
|*									      *|
|* If -a dalign is ON these items are double aligned:			      *|
|*      Common block members						      *|
|*      Numeric sequence components					      *|
|*      All other components						      *|
|*      Static local, module and stack data				      *|
|*									      *|
|* If -a dalign is OFF, these items are double aligned:			      *|
|*      All other components						      *|
|*      Static local, module and stack data				      *|
|*									      *|
|* Types affected by dalign:						      *|
|*      Integer(8) 							      *|
|*      Logical(8) 							      *|
|*      Real(8)								      *|
|*      Real(16)							      *|
|*      Complex(4)    (SUN only - NOT on SGI)				      *|
|*      Complex(8)							      *|
|*      Complex(16)							      *|
|*      Typeless_8							      *|
|*      Long_Typeless							      *|
|*									      *|
|* Platforms where dalign is supported:					      *|
|*      SGI 								      *|
|*      SUN 								      *|
|*      SV2								      *|
|*									      *|
\******************************************************************************/
void assign_offset(int	attr_idx)

{
   size_offset_type	offset;
   boolean		pack;
   size_offset_type	pad;
   size_offset_type	storage_size;
   int			type_idx;

# if defined(_TARGET_DOUBLE_ALIGN)
   size_offset_type	result;
# endif


   TRACE (Func_Entry, "assign_offset", NULL);

   if (ATD_SYMBOLIC_CONSTANT(attr_idx)) {

      /* This is a placeholder so it doesn't really need storage or an offset.*/

      ATD_OFFSET_ASSIGNED(attr_idx)	= TRUE;
      ATD_OFFSET_IDX(attr_idx)		=  CN_INTEGER_ZERO_IDX;
      ATD_OFFSET_FLD(attr_idx)		=  CN_Tbl_Idx;
      return;
   }

   if (ATD_CLASS(attr_idx) == Struct_Component) {
      offset.fld	= ATT_STRUCT_BIT_LEN_FLD(CURR_BLK_NAME);
      offset.idx	= ATT_STRUCT_BIT_LEN_IDX(CURR_BLK_NAME);
      pack		= TRUE;
   }
   else {
      offset.fld	= SB_LEN_FLD(ATD_STOR_BLK_IDX(attr_idx));
      offset.idx	= SB_LEN_IDX(ATD_STOR_BLK_IDX(attr_idx));
      pack		= SB_IS_COMMON(ATD_STOR_BLK_IDX(attr_idx));
   }

   storage_size		= stor_bit_size_of(attr_idx, 
                                           TRUE,      /* All elements */
                                           FALSE);

   type_idx		= ATD_TYPE_IDX(attr_idx);

   if (ATD_IM_A_DOPE(attr_idx)) {

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      align_bit_length(&offset, storage_bit_size_tbl[CRI_Ptr_8]);

      if (ATD_CLASS(attr_idx) == Struct_Component) {

         if (cmd_line_flags.s_pointer8 && !cmd_line_flags.align32) {
            ATT_DALIGN_ME(CURR_BLK_NAME) = TRUE;
            ATD_ALIGNMENT(attr_idx)	 = Align_64;
         }
      }
# else
      align_bit_length(&offset, TARGET_BITS_PER_WORD);
      ATD_ALIGNMENT(attr_idx)	 = WORD_ALIGN;
# endif

      if (offset.fld == NO_Tbl_Idx) {
         offset.idx = ntr_const_tbl(offset.type_idx, FALSE, offset.constant);
         offset.fld = CN_Tbl_Idx;
      }
   }
   else if (pack && 
            (TYP_TYPE(type_idx) == Character ||
            (TYP_TYPE(type_idx) == Structure && 
             ATT_CHAR_SEQ(TYP_IDX(type_idx))))) {

      /* Intentionally blank - offset_idx is okay. */

      if (TYP_TYPE(type_idx) == Character) {

# if defined(_CHAR_IS_ALIGN_8)
         ATD_ALIGNMENT(attr_idx)	= Align_8;
# else
         ATD_ALIGNMENT(attr_idx)	= Align_Bit;
# endif
      }
      else {
         ATD_ALIGNMENT(attr_idx)	= Align_Bit;
      }
   }

# if defined(_TARGET_PACK_HALF_WORD_TYPES)

   /* Complex_4 does not go here because we want it aligned on a 64 bit  */
   /* boundary for speed.                                                */

   else if (PACK_HALF_WORD_TEST_CONDITION(type_idx)) {

      /* This item is a 32 bit item or this structure has all 32 bit  */
      /* components (or components that are structures made up of 32  */
      /* bit components).  They can be packed up.                     */

      /* This option is only allowed on 64 bit machines.              */

      align_bit_length(&offset, TARGET_BITS_PER_WORD / 2);
      ATD_ALIGNMENT(attr_idx)	= Align_32;

      if (offset.fld == NO_Tbl_Idx) {
         offset.fld = CN_Tbl_Idx;
         offset.idx = ntr_const_tbl(offset.type_idx, FALSE, offset.constant);
      }
   }
# endif

# if defined(_INTEGER_1_AND_2)

   else if (on_off_flags.integer_1_and_2 && 
            PACK_8_BIT_TEST_CONDITION(type_idx)) {
      align_bit_length(&offset, 8);
      ATD_ALIGNMENT(attr_idx)	= Align_8;

      if (offset.fld == NO_Tbl_Idx) {
         offset.fld = CN_Tbl_Idx;
         offset.idx = ntr_const_tbl(offset.type_idx, FALSE, offset.constant);
      }
   }
   else if (on_off_flags.integer_1_and_2 &&
            PACK_16_BIT_TEST_CONDITION(type_idx)){
      align_bit_length(&offset, 16);
      ATD_ALIGNMENT(attr_idx)	= Align_16;

      if (offset.fld == NO_Tbl_Idx) {
         offset.fld = CN_Tbl_Idx;
         offset.idx = ntr_const_tbl(offset.type_idx, FALSE, offset.constant);
      }
   }

# endif

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   else if (cmd_line_flags.align32) {
      align_bit_length(&offset, 32);
      ATD_ALIGNMENT(attr_idx)	= Align_32;

      if (offset.fld == NO_Tbl_Idx) {
         offset.fld = CN_Tbl_Idx;
         offset.idx = ntr_const_tbl(offset.type_idx, FALSE, offset.constant);
      }
   }
# endif

# if defined(_ALIGN_REAL16_TO_16_BYTES)

   else if (TYP_LINEAR(type_idx) == Complex_16 ||
            TYP_LINEAR(type_idx) == Real_16) {
#if defined(_TARGET64) && defined(_WHIRL_HOST64_TARGET64)
      align_bit_length(&offset, TARGET_BITS_PER_WORD*2);
#else
      align_bit_length(&offset, TARGET_BITS_PER_WORD*4);
#endif 
      ATD_ALIGNMENT(attr_idx)	= Align_128;

      if (offset.fld == NO_Tbl_Idx) {
         offset.fld = CN_Tbl_Idx;
         offset.idx = ntr_const_tbl(offset.type_idx, FALSE, offset.constant);
      }
   }
# endif

# if defined(_TARGET_DOUBLE_ALIGN)

   else if (DALIGN_TEST_CONDITION(type_idx)) {

      /* Equivalence is handled in normalize_offsets */

      if (cmd_line_flags.dalign) { 

         if (ATD_STOR_BLK_IDX(attr_idx) != NULL_IDX &&
             SB_IS_COMMON(ATD_STOR_BLK_IDX(attr_idx)) ) {

            align_bit_length(&offset, TARGET_BITS_PER_WORD);

            if (offset.fld == NO_Tbl_Idx) {
               offset.fld = CN_Tbl_Idx;
               offset.idx = ntr_const_tbl(offset.type_idx, 
                                          FALSE,
                                          offset.constant);
            }
                                 
            C_TO_F_INT(result.constant,
                       TARGET_BITS_PER_WORD * 2,
                       CG_INTEGER_DEFAULT_TYPE);
            result.type_idx	= CG_INTEGER_DEFAULT_TYPE;
            result.fld		= NO_Tbl_Idx;

            if (!size_offset_binary_calc(&offset, &result, Mod_Opr, &result)) {
               AT_DCL_ERR(attr_idx)	= TRUE;
            }

            if (result.fld == NO_Tbl_Idx) {
               result.fld = CN_Tbl_Idx;
               result.idx = ntr_const_tbl(result.type_idx, 
                                          FALSE,
                                          result.constant);
            }

# if ! (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

            /* -a dalign is always on for IRIX and there is no way to shut */
            /* it off, so we do not need to issue this warning for IRIX.   */

            /* KAY - N$PES */

            if (fold_relationals(result.idx,
                                 CN_INTEGER_ZERO_IDX,
                                 Ne_Opr)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1013, Warning, 
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        SB_BLANK_COMMON(ATD_STOR_BLK_IDX(attr_idx)) ?
                        "" : SB_NAME_PTR(ATD_STOR_BLK_IDX(attr_idx)));
            }
# endif
         }
         else if (ATD_CLASS(attr_idx) == Struct_Component) {
            ATT_DALIGN_ME(CURR_BLK_NAME) = TRUE;
         }

         align_bit_length(&offset, TARGET_BITS_PER_WORD * 2);
         ATD_ALIGNMENT(attr_idx) = Align_64;

         if (offset.fld == NO_Tbl_Idx) {
            offset.fld = CN_Tbl_Idx;
            offset.idx = ntr_const_tbl(offset.type_idx, FALSE, offset.constant);
         }
      }
      else if (ATD_CLASS(attr_idx) == Struct_Component &&
               !ATT_DCL_NUMERIC_SEQ(CURR_BLK_NAME)) {

         /* We cannot dalign numeric sequence derived types */

         align_bit_length(&offset, TARGET_BITS_PER_WORD * 2);

         if (offset.fld == NO_Tbl_Idx) {
            offset.fld = CN_Tbl_Idx;
            offset.idx = ntr_const_tbl(offset.type_idx, FALSE, offset.constant);
         }

         ATT_DALIGN_ME(CURR_BLK_NAME) = TRUE;
         ATD_ALIGNMENT(attr_idx) = Align_64;
      }
      else if (ATD_STOR_BLK_IDX(attr_idx) != NULL_IDX) {

         if (SB_IS_COMMON(ATD_STOR_BLK_IDX(attr_idx))) {

            align_bit_length(&offset, TARGET_BITS_PER_WORD);
            ATD_ALIGNMENT(attr_idx) = WORD_ALIGN;

            if (offset.fld == NO_Tbl_Idx) {
               offset.fld = CN_Tbl_Idx;
               offset.idx = ntr_const_tbl(offset.type_idx, 
                                          FALSE,
                                          offset.constant);
            }

            C_TO_F_INT(result.constant,
                       TARGET_BITS_PER_WORD * 2,
                       CG_INTEGER_DEFAULT_TYPE);
            result.type_idx	= CG_INTEGER_DEFAULT_TYPE;
            result.fld		= NO_Tbl_Idx;

            if (!size_offset_binary_calc(&offset, &result, Mod_Opr, &result)) {
               AT_DCL_ERR(attr_idx)	= TRUE;
            }

            /* KAY N$PES */

            if (result.fld == NO_Tbl_Idx) {
               result.fld = CN_Tbl_Idx;
               result.idx = ntr_const_tbl(result.type_idx, 
                                          FALSE,
                                          result.constant);
            }

            if (fold_relationals(result.idx,
                                 CN_INTEGER_ZERO_IDX,
                                 Ne_Opr)) {

               /* Warning - This double is not on a double word boundary. */
               /*           Can only double align these if -a dalign is   */
               /*           specified on the commandline.                 */

               PRINTMSG(AT_DEF_LINE(attr_idx), 1161, Caution, 
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        SB_BLANK_COMMON(ATD_STOR_BLK_IDX(attr_idx)) ?
                        "" : SB_NAME_PTR(ATD_STOR_BLK_IDX(attr_idx)));
            }
         }
         else {

            align_bit_length(&offset, TARGET_BITS_PER_WORD * 2);
            ATD_ALIGNMENT(attr_idx)	= Align_64;

            if (offset.fld == NO_Tbl_Idx) {
               offset.fld = CN_Tbl_Idx;
               offset.idx = ntr_const_tbl(offset.type_idx, 
                                          FALSE,
                                          offset.constant);
            }
         }
      }
      else {
         align_bit_length(&offset, TARGET_BITS_PER_WORD);
            ATD_ALIGNMENT(attr_idx)	= WORD_ALIGN;

         if (offset.fld == NO_Tbl_Idx) {
            offset.fld = CN_Tbl_Idx;
            offset.idx = ntr_const_tbl(offset.type_idx, 
                                       FALSE,
                                       offset.constant);
         }

         if (ATD_CLASS(attr_idx) == Struct_Component) {
            C_TO_F_INT(result.constant,
                       TARGET_BITS_PER_WORD * 2,
                       CG_INTEGER_DEFAULT_TYPE);
            result.fld		= NO_Tbl_Idx;
            result.type_idx	= CG_INTEGER_DEFAULT_TYPE;

            if (!size_offset_binary_calc(&offset, &result, Mod_Opr, &result)) {
               AT_DCL_ERR(attr_idx)	= TRUE;
            }

            if (result.fld == NO_Tbl_Idx) {
               result.fld = CN_Tbl_Idx;
               result.idx = ntr_const_tbl(result.type_idx, 
                                          FALSE,
                                          result.constant);
            }

            /* KAY N$PES */

            if (fold_relationals(result.idx,
                                 CN_INTEGER_ZERO_IDX,
                                 Ne_Opr)) {

               /* Caution - This component is not on a double word boundary */
               /*           It is numeric sequence so we cannot pad it.     */

               PRINTMSG(AT_DEF_LINE(attr_idx), 1198, Caution, 
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(CURR_BLK_NAME));
            }
         }
      }
   }

# endif  /* DALIGN_TEST_CONDTION */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   else if (TYP_TYPE(type_idx) == Structure &&
            ATT_ALIGNMENT(TYP_IDX(type_idx)) > WORD_ALIGN) {

      switch (ATT_ALIGNMENT(TYP_IDX(type_idx))) {
      case Align_Double:
      case Align_128:
         align_bit_length(&offset, 128);
         ATD_ALIGNMENT(attr_idx) = ATT_ALIGNMENT(TYP_IDX(type_idx));
         break;
      }

      if (offset.fld == NO_Tbl_Idx) {
         offset.fld = CN_Tbl_Idx;
         offset.idx = ntr_const_tbl(offset.type_idx, FALSE, offset.constant);
      }
   }
# endif
   else {
      align_bit_length(&offset, TARGET_BITS_PER_WORD);
      ATD_ALIGNMENT(attr_idx) = WORD_ALIGN;

      if (offset.fld == NO_Tbl_Idx) {
         offset.fld = CN_Tbl_Idx;
         offset.idx = ntr_const_tbl(offset.type_idx, FALSE, offset.constant);
      }
   }

   if (ATD_CLASS(attr_idx) == Struct_Component) {
      ATD_OFFSET_FLD(attr_idx)		= offset.fld;
      ATD_CPNT_OFFSET_IDX(attr_idx)	= offset.idx;

      if (!size_offset_binary_calc(&offset, 
                                   &storage_size,
                                    Plus_Opr,
                                   &storage_size)) {
         AT_DCL_ERR(attr_idx)	= TRUE;
      }

      if (storage_size.fld == NO_Tbl_Idx) {
         ATT_STRUCT_BIT_LEN_FLD(CURR_BLK_NAME) = CN_Tbl_Idx;
         ATT_STRUCT_BIT_LEN_IDX(CURR_BLK_NAME) = ntr_const_tbl(
                                                   storage_size.type_idx,
                                                   FALSE,
                                                   storage_size.constant);
      }
      else {
         ATT_STRUCT_BIT_LEN_FLD(CURR_BLK_NAME) = storage_size.fld;
         ATT_STRUCT_BIT_LEN_IDX(CURR_BLK_NAME) = storage_size.idx;
      }
   }
   else {

      /* Do not set ATD_OFFSET_ASSIGNED here, because this routine is used */
      /* by equivalence processing and should not have that flag set.      */

      ATD_OFFSET_IDX(attr_idx)	= offset.idx;
      ATD_OFFSET_FLD(attr_idx)	= offset.fld;

      if (SB_PAD_BLK(ATD_STOR_BLK_IDX(attr_idx))) { 
         calculate_pad(&pad, &storage_size, attr_idx);

         if (!size_offset_binary_calc(&offset,
                                      &storage_size,
                                       Plus_Opr,
                                      &storage_size)) {
            AT_DCL_ERR(attr_idx)	= TRUE;
         }

         if (!size_offset_binary_calc(&pad,
                                      &storage_size,
                                       Plus_Opr,
                                      &storage_size)) {
            AT_DCL_ERR(attr_idx)	= TRUE;
         }
      }
      else {

         if (!size_offset_binary_calc(&offset,
                                      &storage_size,
                                       Plus_Opr,
                                      &storage_size)) {
            AT_DCL_ERR(attr_idx)	= TRUE;
         }
      }
                
      if (storage_size.fld == NO_Tbl_Idx) {
         storage_size.fld = CN_Tbl_Idx;
         storage_size.idx = ntr_const_tbl(storage_size.type_idx,
                                          FALSE,
                                          storage_size.constant);
      }

      SB_LEN_FLD(ATD_STOR_BLK_IDX(attr_idx)) = storage_size.fld;
      SB_LEN_IDX(ATD_STOR_BLK_IDX(attr_idx)) = storage_size.idx;
   }

   TRACE (Func_Exit, "assign_offset", NULL);

   return;

}  /* assign_offset */
#ifdef KEY /* Bug 14150 */
/*
 * Like assign_offset(), but uses C alignment rules (which differ from Fortran
 * alignment rules for X8664 -m32)
 *
 * attr_idx	AT_Tbl_Idx for Structure_Componeent or for Data_Obj in common
 * bind_c	TRUE if the structure or common block has the bind(c) attr
 */
void
assign_bind_c_offset(int attr_idx, boolean bind_c) {
  boolean save_align32 = cmd_line_flags.align32;
  boolean save_align64 = cmd_line_flags.align64;
  if (is_x8664_n32() && bind_c) {
    /* Not pretty, but apt to be bug-free */
    cmd_line_flags.align32 = TRUE;
    cmd_line_flags.align64 = FALSE;
  }
  assign_offset(attr_idx);
  cmd_line_flags.align32 = save_align32;
  cmd_line_flags.align64 = save_align64;
}
#endif /* KEY Bug 14150 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Allocate storage.                                                     *|
|*									      *|
|* Input parameters:							      *|
|*      storage_size :							      *|
|*      attr_idx     :							      *|
|*									      *|
|* Output parameters:							      *|
|*	pad :								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static	void	calculate_pad(size_offset_type	*pad,
			      size_offset_type	*storage_size,
			      int		 attr_idx)

{
   size_offset_type	constant;
   size_offset_type	min_result;
   int			sb_idx;
   size_offset_type	temp_1;
   size_offset_type	temp_2;
   size_offset_type	wd_storage_size;


   TRACE (Func_Entry, "calculate_pad", NULL);

   sb_idx	= ATD_STOR_BLK_IDX(attr_idx);

   if (! SB_PAD_AMOUNT_SET(sb_idx)) {

      /* Storage size must be in words to calculate pad. */

      wd_storage_size	= (*storage_size);

      BITS_TO_WORDS(wd_storage_size, TARGET_BITS_PER_WORD);

      /* If the user has not specified a pad amount use the following        */
      /* formula to calculate the pad amount:                                */
      /* 								     */
      /*  (MIN(256,(MIN(1, size/1K) * (((((256 * size) / 4K) + 7) / 8) * 8)) */
      /*  + (MIN(1, size/128) * 8)                                           */
      /*  + MOD(( 8 - mod(size, 8)), 8)                                      */

      /* t$1 = (size/1024) 						     */

      constant.fld		= NO_Tbl_Idx;
      constant.type_idx		= CG_INTEGER_DEFAULT_TYPE;

      C_TO_F_INT(constant.constant, 1024, CG_INTEGER_DEFAULT_TYPE);

      if (! size_offset_binary_calc(&wd_storage_size,
                                    &constant,
                                     Div_Opr,
                                    &temp_1)) {
         goto ERROR;   /* (size/1024) */
      }


      /* min_result = MIN(1, t$1)					     */

      C_TO_F_INT(constant.constant, 1, CG_INTEGER_DEFAULT_TYPE);

      if (! size_offset_min_max_calc(&constant,
                                     &temp_1,
                                      Min_Opr,
                                     &min_result)) {
         goto ERROR;  /* MIN(1, size/1024) */
      }


      /* t$1 = (size * 256) 						     */

      C_TO_F_INT(constant.constant, 256, CG_INTEGER_DEFAULT_TYPE);

      if (! size_offset_binary_calc(&wd_storage_size,
                                    &constant,
                                     Mult_Opr,
                                   &temp_1)) {
         goto ERROR;   /* (size * 256) */
      }


       /* t$2 = t$1 / 4096) 						     */

      C_TO_F_INT(constant.constant, 4096, CG_INTEGER_DEFAULT_TYPE);

      if (! size_offset_binary_calc(&temp_1, &constant, Div_Opr, &temp_2)) {
         goto ERROR;  /* (256 * size) / 4096) */
      }


      /* t$1 = t$2 + 7 							     */

      C_TO_F_INT(constant.constant, 7, CG_INTEGER_DEFAULT_TYPE);

      if (! size_offset_binary_calc(&temp_2, &constant, Plus_Opr, &temp_1)) {
         goto ERROR;  /* ((256 * size) / 4096) + 7) */
      }


      /* t$2 = t$1 / 8 							     */

      C_TO_F_INT(constant.constant, 8, CG_INTEGER_DEFAULT_TYPE);

      if (! size_offset_binary_calc(&temp_1, &constant, Div_Opr, &temp_2)) {
         goto ERROR;  /* (((256 * size) / 4096) + 7) / 8 ) */
      }


      /* t$1 = t$2 * 8 							     */

      if (! size_offset_binary_calc(&temp_2, &constant, Mult_Opr, &temp_1)) {
         goto ERROR;  /* (((((256 * size) / 4096) + 7) / 8 ) * 8) */
      }


      /* temp_2 = min_result * t$1 					     */

      if (!size_offset_binary_calc(&min_result, &temp_1, Mult_Opr, &temp_2)) {
         goto ERROR;  /* (MIN(1,size/1024) * (((((256*size/4096))+7)/8)*8)  */
      }


      /* pad = MIN(256, temp_2)						     */

      C_TO_F_INT(constant.constant, 256, CG_INTEGER_DEFAULT_TYPE);

      if (! size_offset_min_max_calc(&constant, &temp_2, Min_Opr, pad)) {

         /* (MAX(256,(MIN(1,size/1024)*(((((256*size/4096))+7)/8)*8)))) */

         goto ERROR;
      }


      /* t$1 = size / 128						     */

      C_TO_F_INT(constant.constant, 128, CG_INTEGER_DEFAULT_TYPE);

      if (! size_offset_binary_calc(&wd_storage_size,
                                    &constant,
                                     Div_Opr, 
                                   &temp_1)) {
         goto ERROR;  /* size/128 */
      }


      /* min_result = MIN(1, t$1)					     */

      C_TO_F_INT(constant.constant, 1, CG_INTEGER_DEFAULT_TYPE);

      if (! size_offset_min_max_calc(&constant,
                                     &temp_1,
                                      Min_Opr,
                                     &min_result)) {
         goto ERROR;  /* MIN(1, size/128) */
      }


      /* t$1 = min_result * 8						     */

      C_TO_F_INT(constant.constant, 8, CG_INTEGER_DEFAULT_TYPE);

      if (! size_offset_binary_calc(&min_result,
                                    &constant,
                                     Mult_Opr,
                                    &temp_1)) {
         goto ERROR;   /* MIN(1, size/128) * 8  */
      }


      /* pad = pad + t$1						     */

      if (! size_offset_binary_calc(pad, &temp_1, Plus_Opr, pad)) {
         goto ERROR;   /* first term + second term */
      }


      /* t$1 = MOD(size, 8)						     */

      C_TO_F_INT(constant.constant, 8, CG_INTEGER_DEFAULT_TYPE);

      if (! size_offset_binary_calc(&wd_storage_size,
                                    &constant,
                                     Mod_Opr,
                                    &temp_1)) {
         goto ERROR;   /* mod(size, 8) */
      }


      /* t$2 = 8 - t$1							     */

      if (! size_offset_binary_calc(&constant, &temp_1, Minus_Opr, &temp_2)) {
         goto ERROR;   /* (8 - mod(size, 8))  */
      }


      /* t$1 = MOD(t$2, 8)						     */

      if (! size_offset_binary_calc(&temp_2, &constant, Mod_Opr, &temp_1)) {
         goto ERROR;   /* mod((8 - mod(size, 8)), 8)  */
      }


      /* pad = pad + t$1						     */

      if (! size_offset_binary_calc(pad, &temp_1, Plus_Opr, pad)) {
         goto ERROR;   /* Add third term to accumulated first two terms. */
      }
   }
   else {
      (*pad).fld		= NO_Tbl_Idx;
      (*pad).type_idx		= CG_INTEGER_DEFAULT_TYPE;
      C_TO_F_INT((*pad).constant,
                 SB_PAD_AMOUNT(sb_idx),
                 CG_INTEGER_DEFAULT_TYPE);
   }

   constant.fld			= NO_Tbl_Idx;
   constant.type_idx		= CG_INTEGER_DEFAULT_TYPE;
   C_TO_F_INT(constant.constant, TARGET_BITS_PER_WORD, CG_INTEGER_DEFAULT_TYPE);

   if (!size_offset_binary_calc(pad, &constant, Mult_Opr, pad)) {
      goto ERROR;
   }

   goto DONE;

ERROR:
   (*pad).fld		= CN_Tbl_Idx;
   (*pad).idx		= CN_INTEGER_ZERO_IDX;

DONE:

   TRACE (Func_Exit, "calculate_pad", NULL);

   return;

}  /* calculate_pad */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Allocate storage.                                                     *|
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
boolean	srch_global_name_tbl(char	*name_str,
			     int	 name_len,
			     int	*name_idx)

{
   boolean	found;
   int 	 	idx;
   long 	tst_val;


   TRACE (Func_Entry, "srch_global_name_tbl", name_str);

   tst_val = srch_name_tbl(name_str, 
                           name_len,
                           &idx,
                           global_name_tbl,
                           str_pool,
                           1,
                           global_name_tbl_idx);
   *name_idx = idx;

   if (tst_val != 0) {
      found	= FALSE;
      TRACE (Func_Exit, "srch_global_name_tbl", NULL);
   }  
   else {
      found	= TRUE;
      TRACE (Func_Exit, "srch_global_name_tbl", GN_NAME_PTR(idx));
   }
   return (found);
 
}  /* srch_global_name_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      attr_idx	NULL if this is a common block, otherwise attr entry  *|
|*              	of global program unit to enter.                      *|
|*      sb_idx          NULL if this is a common block inserted during        *|
|*                      commandline processing.  The caller is expected to    *|
|*                      get name and length inserted correctly.  Otherwise if *|
|*                      this is a common block, this is the sb_idx for the blk*|
|*      name_idx        string table index where entry is to be inserted      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void	ntr_global_name_tbl(int		attr_idx,
			    int		sb_idx,
			    int		name_idx)


{
   	    int		 ga_idx;
   register int          i;
   register long	*id;
   register int		 length;     

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   register long        *global_tbl_base;
# endif


   TRACE (Func_Entry, "ntr_global_name_tbl", NULL);

   TBL_REALLOC_CK(global_name_tbl, 1);

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   global_tbl_base	= (long *) global_name_tbl;

#  pragma _CRI ivdep
   for (i = global_name_tbl_idx; i >= name_idx; i--) {
      global_tbl_base[i]	= global_tbl_base[i-1];
   }

# else
   for (i = global_name_tbl_idx; i >= name_idx; i--) {
      global_name_tbl[i]	= global_name_tbl[i-1];
   }
# endif

   CLEAR_TBL_NTRY(global_name_tbl, name_idx);

   if (sb_idx != NULL_IDX) {
      id			= SB_NAME_LONG(sb_idx);
      length			= SB_NAME_LEN(sb_idx);
      GN_NAME_IDX(name_idx)	= str_pool_idx + 1;
      GN_NAME_LEN(name_idx)	= length;
      length			= WORD_LEN(length);

      /* add identifier to string pool */

      TBL_REALLOC_CK (str_pool, length);

      for (i = 0; i < length; i++) {
         str_pool[GN_NAME_IDX(name_idx) + i].name_long = id[i];
      }

      ga_idx	= ntr_common_in_global_attr_tbl(sb_idx, name_idx);

      GN_ATTR_IDX(name_idx)	= ga_idx;
   }
   else if (attr_idx != NULL_IDX) {
      ga_idx			= ntr_global_attr_tbl(attr_idx, NULL_IDX);
      GN_ATTR_IDX(name_idx)	= ga_idx;
      GN_NAME_IDX(name_idx)	= GA_NAME_IDX(ga_idx);
      GN_NAME_LEN(name_idx)	= GA_NAME_LEN(ga_idx);

      fill_in_global_attr_ntry(ga_idx, attr_idx, NULL_IDX);

   }

   TRACE (Func_Exit, "ntr_global_name_tbl", NULL);

   return;

}  /* ntr_global_name_tbl */

#ifdef KEY /* Bug 14150 */
/*
 * Format source file name and line for insertion in an error message
 * def_line	global line number
 * return 	dynamically allocated string containing formatted file name
 *		and line; caller should free this
 */
char *
file_and_line(int def_line) {
  int gl_idx;
  uint act_file_line;
  GLOBAL_LINE_TO_FILE_LINE(def_line, gl_idx, act_file_line);
  const char *file_name = GL_FILE_NAME_PTR(gl_idx);
  char *alloc_str = malloc(strlen(file_name) + 32);
  sprintf(alloc_str, "%d (%s)", act_file_line, file_name);
  return alloc_str;
  }

/*
 * Given the external name of a program unit or common block, add it to
 * the global_attr_tbl entry as the binding label
 * ga_idx	Index into global_attr_tbl
 * name		ATP_EXT_NAME_PTR or SB_EXT_NAME_PTR
 * name_len	ATP_EXT_NAME_LEN or SB_EXT_NAME_LEN
 */
static void
make_ga_binding_label(int ga_idx, const char *name, int name_len) {
  if (GA_BIND_ATTR(ga_idx)) {
    char *result = memcpy(malloc(name_len + 1), name, name_len);
    result[name_len] = 0;
    GA_BINDING_LABEL(ga_idx) = result;
  }
  else {
    GA_BINDING_LABEL(ga_idx) = 0;
  }
}
#endif /* KEY Bug 14150 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Fills in the variant part of a global attr entry.  Assumes that       *|
|*      ntr_global_attr_tbl or some other mechanism has been used to set up   *|
|*      the common fields in this global attr entry.                          *|
|*                                                                            *|
|*      NOTE:  ntr_global_attr_tbl and fill_in_global_attr_ntry are two       *|
|*             separate routines, because there are not linked lists for      *|
|*             components or dargs in the global attr table.  They are        *|
|*             assumed to be consecutive, so we need to create space for      *|
|*             the correct number of components or dargs and then fill them   *|
|*             in later.                                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      ga_idx     -> global attr entry that needs to be filled in.           *|
|*      attr_idx   -> attr entry of attr to enter in global attr table.       *|
|*      ga_pgm_idx -> If this is a darg or func result, this is its pgm unit. *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void	fill_in_global_attr_ntry(int	ga_idx,
				 int	attr_idx,
				 int	ga_pgm_idx)

{
   int		cn_idx;
   int		first_sn_idx;
   int		ga_darg_idx;
   int		i;
   int		module_idx;
   int		name_idx;
   int		new_idx;
   int		num_dargs;
   int		rslt_idx;
   int		sn_idx;


   TRACE (Func_Entry, "fill_in_global_attr_ntry", NULL);

   module_idx	= AT_MODULE_IDX(attr_idx);

   if (module_idx != NULL_IDX) {

      if (srch_global_name_tbl(AT_OBJ_NAME_PTR(module_idx),
                               AT_NAME_LEN(module_idx),	
                               &name_idx)) {

         /* Found - Make sure it is a module and not something else */

         /* It should be in here already - KAY - internal ??? */
   
         if (GA_OBJ_CLASS(GN_ATTR_IDX(name_idx)) == Common_Block) {
            GA_MODULE_IDX(ga_idx) = GAC_PGM_UNIT_IDX(GN_ATTR_IDX(name_idx));
         }
         else {
            GA_MODULE_IDX(ga_idx) = GN_ATTR_IDX(name_idx);
         }
      }
      else {
         ntr_global_name_tbl(module_idx, NULL_IDX, name_idx);
            GA_MODULE_IDX(ga_idx)	= GN_ATTR_IDX(name_idx);
      }
   }

   switch (AT_OBJ_CLASS(attr_idx)) {
   case Data_Obj:

      GAD_CLASS(ga_idx)		= ATD_CLASS(attr_idx);
      GAD_POINTER(ga_idx)	= ATD_POINTER(attr_idx);
#ifdef KEY /* Bug 14110 */
      GAD_VOLATILE(ga_idx)	= ATD_VOLATILE(attr_idx);
#endif /* KEY Bug 14110 */
      GAD_TARGET(ga_idx)	= ATD_TARGET(attr_idx);

      if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {

         /* We actually only need an array entry if this is a component or */
         /* if this is a member of the common block.  Those are the only   */
         /* two places where we need to check the lower and upper bounds.  */

         if (ATD_CLASS(attr_idx) == Struct_Component || 
             ATD_IN_COMMON(attr_idx)) {
            new_idx	       = ntr_global_bounds_tbl(ATD_ARRAY_IDX(attr_idx));
            GAD_ARRAY_IDX(ga_idx) = new_idx;
         }
         GAD_RANK(ga_idx)      = BD_RANK(ATD_ARRAY_IDX(attr_idx));
         GAD_ASSUMED_SHAPE_ARRAY(ga_idx) = 
             (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Shape);
      }

      switch (ATD_CLASS(attr_idx)) {
      case Dummy_Argument:
         GAD_INTENT(ga_idx)	= ATD_INTENT(attr_idx);
         new_idx		= ntr_global_type_tbl(ATD_TYPE_IDX(attr_idx));
         GAD_TYPE_IDX(ga_idx)	= new_idx;

         if (GAD_ASSUMED_SHAPE_ARRAY(ga_idx) ||
             GA_OPTIONAL(ga_idx) ||
             GAD_POINTER(ga_idx) ||
#ifdef KEY /* Bug 14110 */
             GAD_VOLATILE(ga_idx) ||
#endif /* KEY Bug 14110 */
             GAD_TARGET(ga_idx)) {
            GAP_NEEDS_EXPL_ITRFC(ga_pgm_idx) = TRUE;
         }
         break;

      case Function_Result:
         new_idx		= ntr_global_type_tbl(ATD_TYPE_IDX(attr_idx));
         GAD_TYPE_IDX(ga_idx)	= new_idx;

         if (GAD_RANK(ga_idx) != 0 || 
             GAD_POINTER(ga_idx) ||
#ifdef KEY /* Bug 14110 */
             /* Standard doesn't forbid volatile on fcn result, which is
	      * strange, but also doesn't say that volatile fcn result
	      * requires explicit interface, which is fortunate. */
#endif /* KEY Bug 14110 */
             (GT_TYPE(GAD_TYPE_IDX(ga_idx)) == Character &&
              GT_CHAR_CLASS(GAD_TYPE_IDX(ga_idx)) == Var_Len_Char)) {
            GAP_NEEDS_EXPL_ITRFC(ga_pgm_idx) = TRUE;
         }
         break;

      case CRI__Pointee:
         new_idx		= ntr_global_type_tbl(ATD_TYPE_IDX(attr_idx));
         GAD_TYPE_IDX(ga_idx)	= new_idx;
         break;

      case Struct_Component:

         if (ATD_POINTER(attr_idx) &&
             TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Derived_Type && 
             attr_idx == TYP_IDX(ATD_TYPE_IDX(attr_idx))) {

            /* Pointing to itself - type must be set in self before call here */

            GAD_TYPE_IDX(ga_idx) = ATT_GLOBAL_TYPE_IDX(attr_idx);
         }
         else {
            new_idx		 = ntr_global_type_tbl(ATD_TYPE_IDX(attr_idx));
            GAD_TYPE_IDX(ga_idx) = new_idx;
         }
         break;

      case Variable:
         new_idx		= ntr_global_type_tbl(ATD_TYPE_IDX(attr_idx));
         GAD_TYPE_IDX(ga_idx)	= new_idx;
         break;

      default:
         new_idx		= ntr_global_type_tbl(ATD_TYPE_IDX(attr_idx));
         GAD_TYPE_IDX(ga_idx)	= new_idx;
         break;
      }
      break;

   case Pgm_Unit:

      GAP_PGM_UNIT(ga_idx)		= ATP_PGM_UNIT(attr_idx);
      GAP_ELEMENTAL(ga_idx)		= ATP_ELEMENTAL(attr_idx);
      GAP_NOSIDE_EFFECTS(ga_idx)	= ATP_NOSIDE_EFFECTS(attr_idx);
      GAP_PURE(ga_idx)			= ATP_PURE(attr_idx);
      GAP_RECURSIVE(ga_idx)		= ATP_RECURSIVE(attr_idx);
      GAP_VFUNCTION(ga_idx)		= ATP_VFUNCTION(attr_idx);
      ATP_GLOBAL_ATTR_IDX(attr_idx)	= ga_idx;
#ifdef KEY /* Bug 14150 */
      GA_BIND_ATTR(ga_idx)		= AT_BIND_ATTR(attr_idx);
      if (GA_BIND_ATTR(ga_idx)) {
         GAP_NEEDS_EXPL_ITRFC(ga_idx)	= TRUE;
      }
      make_ga_binding_label(ga_idx, ATP_EXT_NAME_PTR(attr_idx),
	ATP_EXT_NAME_LEN(attr_idx));
#endif /* KEY Bug 14150 */

      if (GAP_ELEMENTAL(ga_idx)) {
         GAP_NEEDS_EXPL_ITRFC(ga_idx)	= TRUE;
      }

      if (ATP_EXPL_ITRFC(attr_idx)) {
         GA_DEFINED(ga_idx)	= TRUE;

         if (SCP_IS_INTERFACE(curr_scp_idx)) {
            GAP_IN_INTERFACE_BLK(ga_idx)	= TRUE;
         }
         else {
            GAP_PGM_UNIT_DEFINED(ga_idx)	= TRUE;
         }
      }
      else if (AT_REFERENCED(attr_idx) > Not_Referenced) {
         GA_REFERENCED(ga_idx)			= TRUE;
      }
      else {  /* Declared via EXTERNAL, VFUNCTION, NOSIDEFFECTS ect.. */
      }

      if (ATP_PGM_UNIT(attr_idx) == Function ||
          ATP_PGM_UNIT(attr_idx) == Subroutine) {

         /* If there is an extra darg - never put it in here.  We've got the */
         /* function result information.  That would be duplicating plus we  */
         /* ensure we've always got the same thing when we do compares.      */

         if (ATP_EXTRA_DARG(attr_idx) && ATP_EXPL_ITRFC(attr_idx)) {
            first_sn_idx	= ATP_FIRST_IDX(attr_idx) + 1;
            num_dargs		= ATP_NUM_DARGS(attr_idx) - 1;
         }
         else {
            first_sn_idx	= ATP_FIRST_IDX(attr_idx);
            num_dargs		= ATP_NUM_DARGS(attr_idx);
         }

         GAP_NUM_DARGS(ga_idx)	= num_dargs;

         if (num_dargs > 0) {
            ga_darg_idx			= global_attr_tbl_idx + 1;
            GAP_FIRST_IDX(ga_idx)	= ga_darg_idx;
            sn_idx			= first_sn_idx;

            /* Reserve space for the dummy arguments so they are in */
            /* consecutive order.  Then return and fill them in.    */

            for (i = 0; i < num_dargs; i++ ) {
               ntr_global_attr_tbl(SN_ATTR_IDX(sn_idx), NULL_IDX);
               sn_idx++;
            }

            sn_idx	= first_sn_idx;

            for (i = 0; i < num_dargs; i++) {
               fill_in_global_attr_ntry(ga_darg_idx,
                                        SN_ATTR_IDX(sn_idx),
                                        ga_idx);
               if (SN_LINE_NUM(sn_idx) != 0) {
                  GA_DEF_LINE(ga_darg_idx)	= SN_LINE_NUM(sn_idx);
                  GA_DEF_COLUMN(ga_darg_idx)	= SN_COLUMN_NUM(sn_idx);
               }
               ga_darg_idx++;
               sn_idx++;
            }
         }

         if (ATP_RSLT_IDX(attr_idx) != NULL_IDX) {
            rslt_idx  = ntr_global_attr_tbl(ATP_RSLT_IDX(attr_idx), NULL_IDX);
            fill_in_global_attr_ntry(rslt_idx, ATP_RSLT_IDX(attr_idx), ga_idx);
            GAP_RSLT_IDX(ga_idx)	= rslt_idx;
         }

      }
      break;

   case Derived_Type:
      GAT_NUM_CPNTS(ga_idx)		= ATT_NUM_CPNTS(attr_idx);
      GAT_PRIVATE_CPNT(ga_idx)		= ATT_PRIVATE_CPNT(attr_idx);
      GAT_SEQUENCE_SET(ga_idx)		= ATT_SEQUENCE_SET(attr_idx);
      cn_idx 				= ATT_STRUCT_BIT_LEN_IDX(attr_idx);
      GAT_STRUCT_LIN_TYPE(ga_idx)	= TYP_LINEAR(CN_TYPE_IDX(cn_idx));

      for (i = 0; i < num_host_wds[TYP_LINEAR(CN_TYPE_IDX(cn_idx))]; i++) {
         GAT_STRUCT_BIT_LEN(ga_idx)[i] = CP_CONSTANT(CN_POOL_IDX(cn_idx) + i);
      }

      break;
   }

   TRACE (Func_Exit, "fill_in_global_attr_ntry", NULL);

   return;

}  /* fill_in_global_attr_ntry */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Enters an attr entry into the global attr table.  This just sets up   *|
|*      the common global attr fields, such as line number and names.         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      attr_idx	attr entry of attr to enter in global attr table.     *|
|*      name_idx	This is used to get the string pool idx for the name. *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      ga_idx		New global attr tbl index.                            *|
|*                                                                            *|
\******************************************************************************/

int	ntr_global_attr_tbl(int		attr_idx,
			    int		name_idx)

{
   int		 ga_idx;
   int           i;
   long		*id;
   int		 length;     


   TRACE (Func_Entry, "ntr_global_attr_tbl", NULL);

   TBL_REALLOC_CK(global_attr_tbl, 1);
   CLEAR_TBL_NTRY(global_attr_tbl, global_attr_tbl_idx);
   ga_idx		= global_attr_tbl_idx;

#ifdef KEY /* Bug 14150 */
   /* Set these right away so that error messages can use them */
   GA_DEF_LINE(ga_idx)		= AT_DEF_LINE(attr_idx);
   GA_DEF_COLUMN(ga_idx)	= AT_DEF_COLUMN(attr_idx);	
#endif /* KEY Bug 14150 */

   if (name_idx == NULL_IDX) {

      if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
          ATP_PGM_UNIT(attr_idx) == Module &&
          ATP_MODULE_STR_IDX(attr_idx) != NULL_IDX) {
         GN_NAME_IDX(ga_idx)	= ATP_MODULE_STR_IDX(attr_idx);
         GA_NAME_LEN(ga_idx)	= AT_NAME_LEN(attr_idx);
      }
      else {
         id			= AT_OBJ_NAME_LONG(attr_idx);
         length			= AT_NAME_LEN(attr_idx);
         GA_NAME_IDX(ga_idx)	= str_pool_idx + 1;
         GA_NAME_LEN(ga_idx)	= length;
         length			= WORD_LEN(length);

         /* add identifier to string pool */

         TBL_REALLOC_CK (str_pool, length);

         for (i = 0; i < length; i++) {
            str_pool[GA_NAME_IDX(ga_idx) + i].name_long = id[i];
         }
      }
   }
   else {
      GA_NAME_IDX(ga_idx)	= GN_NAME_IDX(name_idx);
      GA_NAME_LEN(ga_idx)	= GN_NAME_LEN(name_idx);;
   }

   if (AT_ORIG_NAME_IDX(attr_idx) == AT_NAME_IDX(attr_idx)) {
      GA_ORIG_NAME_IDX(ga_idx)	= GA_NAME_IDX(ga_idx);
      GA_ORIG_NAME_LEN(ga_idx)	= GA_NAME_LEN(ga_idx);
   }
   else if (AT_ORIG_NAME_IDX(attr_idx) != NULL_IDX) {
      id			= AT_ORIG_NAME_LONG(attr_idx);
      length			= AT_ORIG_NAME_LEN(attr_idx);
      GA_ORIG_NAME_IDX(ga_idx)	= str_pool_idx + 1;
      GA_ORIG_NAME_LEN(ga_idx)	= length;
      length			= WORD_LEN(length);

      /* add identifier to string pool */

      TBL_REALLOC_CK (str_pool, length);

      for (i = 0; i < length; i++) {
         str_pool[GA_ORIG_NAME_IDX(ga_idx) + i].name_long = id[i];
      }
   }

#if ! defined(KEY) /* Bug 14150 */
   GA_DEF_LINE(ga_idx)		= AT_DEF_LINE(attr_idx);
   GA_DEF_COLUMN(ga_idx)	= AT_DEF_COLUMN(attr_idx);	
#endif /* KEY Bug 14150 */
   GA_OBJ_CLASS(ga_idx)		= AT_OBJ_CLASS(attr_idx);
   GA_OPTIONAL(ga_idx)		= AT_OPTIONAL(attr_idx);
   GA_COMPILER_GEND(ga_idx)	= AT_COMPILER_GEND(attr_idx);
   GA_USE_ASSOCIATED(ga_idx)	= AT_USE_ASSOCIATED(attr_idx);

   TRACE (Func_Exit, "ntr_global_attr_tbl", NULL);

   return(ga_idx);

}  /* ntr_global_attr_tbl */

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
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

int	ntr_common_in_global_attr_tbl(int	sb_idx,
				      int	name_idx)

{
   int		attr_idx;
   int		ga_idx;
   int		new_idx;
   int		prev_idx;


   TRACE (Func_Entry, "ntr_common_in_global_attr_tbl", NULL);

   TBL_REALLOC_CK(global_attr_tbl, 1);
   CLEAR_TBL_NTRY(global_attr_tbl, global_attr_tbl_idx);
   ga_idx			= global_attr_tbl_idx;
   GA_NAME_IDX(ga_idx)		= GN_NAME_IDX(name_idx);
   GA_NAME_LEN(ga_idx)		= GN_NAME_LEN(name_idx);
   GA_DEF_LINE(ga_idx)		= SB_DEF_LINE(sb_idx);
   GA_DEF_COLUMN(ga_idx)	= SB_DEF_COLUMN(sb_idx);	
   GA_OBJ_CLASS(ga_idx)		= Common_Block;
   GA_USE_ASSOCIATED(ga_idx)	= SB_USE_ASSOCIATED(sb_idx);
   GAC_AUXILIARY(ga_idx)	= SB_AUXILIARY(sb_idx);
   GAC_TASK_COMMON(ga_idx)	= SB_BLK_TYPE(sb_idx) == Task_Common;
   GAC_EQUIVALENCED(ga_idx)	= SB_EQUIVALENCED(sb_idx);
   GAC_ALIGN_SYMBOL(ga_idx)	= SB_ALIGN_SYMBOL(sb_idx);
   GAC_FILL_SYMBOL(ga_idx)	= SB_FILL_SYMBOL(sb_idx);
   GAC_SECTION_GP(ga_idx)	= SB_SECTION_GP(sb_idx);
   GAC_SECTION_NON_GP(ga_idx)	= SB_SECTION_NON_GP(sb_idx);
   GAC_CACHE_ALIGN(ga_idx)	= SB_CACHE_ALIGN(sb_idx);
#ifdef KEY /* Bug 14150 */
   GA_BIND_ATTR(ga_idx)		= SB_BIND_ATTR(sb_idx);
   make_ga_binding_label(ga_idx, SB_EXT_NAME_PTR(sb_idx),
     SB_EXT_NAME_LEN(sb_idx));
#endif /* KEY Bug 14150 */

   /* Need to keep the common entries. */

   attr_idx	= SB_FIRST_ATTR_IDX(sb_idx);
   prev_idx	= NULL_IDX;

   while (attr_idx != NULL_IDX) {
      new_idx  = ntr_global_attr_tbl(attr_idx, NULL_IDX);
      fill_in_global_attr_ntry(new_idx, attr_idx, NULL_IDX);

      if (prev_idx != NULL_IDX) {
         GAD_NEXT_IDX(prev_idx) = new_idx;
      }
      else {
         GAC_FIRST_MEMBER_IDX(ga_idx)	   = new_idx;
      }
      prev_idx	= new_idx;
      attr_idx	= ATD_NEXT_MEMBER_IDX(attr_idx);
   }

   if (SB_MODULE_IDX(sb_idx) != NULL_IDX) {

      if (srch_global_name_tbl(AT_OBJ_NAME_PTR(SB_MODULE_IDX(sb_idx)),
                                AT_NAME_LEN(SB_MODULE_IDX(sb_idx)),	
                                &name_idx)) {

         /* Found - Make sure it is a module and not something else */

         /* It should be in here already - KAY - internal ??? */

         if (GA_OBJ_CLASS(GN_ATTR_IDX(name_idx)) == Common_Block) {
            GA_MODULE_IDX(ga_idx) = GAC_PGM_UNIT_IDX(GN_ATTR_IDX(name_idx));
         }
         else {
            GA_MODULE_IDX(ga_idx) = GN_ATTR_IDX(name_idx);
         }
      }
      else {
         ntr_global_name_tbl(SB_MODULE_IDX(sb_idx), NULL_IDX, name_idx);
         GA_MODULE_IDX(ga_idx)	= GN_ATTR_IDX(name_idx);
      }
   }

   TRACE (Func_Exit, "ntr_common_in_global_attr_tbl", NULL);

   return(ga_idx);

}  /* ntr_common_in_global_attr_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*	This routine adds new types to the global type table.  It attempts    *|
|*	to share them all.  If you are entering Typeless, pass Err_Res        *|
|*      for the lin_type, and this routine will set it correctly.)            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
int	ntr_global_type_tbl(int	type_idx)

{
   int		 attr_idx;
   int		 cn_idx;
   boolean	 found;
   int		 ga_idx;
   int		 ga_cpnt_idx;
   int		 i;
   int		 new_type_idx;
   long		*null_base;
   int		 sn_idx;
   long		*type_tbl_base;


   TRACE (Func_Entry, "ntr_global_type_tbl", NULL);

   if (TYP_TYPE(type_idx) == Character) {
      GT_TYPE(TYP_WORK_IDX)		= TYP_TYPE(type_idx);
      GT_DCL_VALUE(TYP_WORK_IDX)	= TYP_DCL_VALUE(type_idx);
      GT_DESC(TYP_WORK_IDX)		= TYP_DESC(type_idx);
      GT_LINEAR_TYPE(TYP_WORK_IDX)	= TYP_LINEAR(type_idx);
      GT_CHAR_CLASS(TYP_WORK_IDX)	= TYP_CHAR_CLASS(type_idx);
      GT_STRUCT_IDX(TYP_WORK_IDX)	= TYP_IDX(type_idx);

      if (GT_CHAR_CLASS(TYP_WORK_IDX) == Const_Len_Char) {
         cn_idx 			  = GT_STRUCT_IDX(TYP_WORK_IDX);
         GT_LENGTH_LIN_TYPE(TYP_WORK_IDX) = TYP_LINEAR(CN_TYPE_IDX(cn_idx));

         for (i = 0; i < num_host_wds[TYP_LINEAR(CN_TYPE_IDX(cn_idx))]; i++) {
            GT_LENGTH(TYP_WORK_IDX)[i] = CP_CONSTANT(CN_POOL_IDX(cn_idx) + i);
         }
      }
      GT_STRUCT_IDX(TYP_WORK_IDX)	= NULL_IDX;
   }
   else if (TYP_TYPE(type_idx) == Structure) {

      if (ATT_GLOBAL_TYPE_IDX(TYP_IDX(type_idx)) != NULL_IDX) { 

         /* This derived type exists already.  Just return the index. */

         new_type_idx	= ATT_GLOBAL_TYPE_IDX(TYP_IDX(type_idx));
         goto EXIT;
      }

      GT_TYPE(TYP_WORK_IDX)		= TYP_TYPE(type_idx);
      GT_DCL_VALUE(TYP_WORK_IDX)	= TYP_DCL_VALUE(type_idx);
      GT_DESC(TYP_WORK_IDX)		= TYP_DESC(type_idx);
      GT_LINEAR_TYPE(TYP_WORK_IDX)	= TYP_LINEAR(type_idx);
      GT_CHAR_CLASS(TYP_WORK_IDX)	= TYP_CHAR_CLASS(type_idx);
      GT_STRUCT_IDX(TYP_WORK_IDX)	= TYP_IDX(type_idx);

      attr_idx	= GT_STRUCT_IDX(TYP_WORK_IDX);
      ga_idx	= ntr_global_attr_tbl(attr_idx, NULL_IDX);

      TBL_REALLOC_CK(global_type_tbl, 1);
      new_type_idx				= global_type_tbl_idx;
      global_type_tbl[new_type_idx]		= global_type_tbl[TYP_WORK_IDX];
      GT_STRUCT_IDX(new_type_idx)			= ga_idx;
      ATT_GLOBAL_TYPE_IDX(attr_idx)		= new_type_idx;

      fill_in_global_attr_ntry(ga_idx, attr_idx, NULL_IDX);

      ga_cpnt_idx			= global_attr_tbl_idx + 1;
      GAT_FIRST_CPNT_IDX(ga_idx)	= ga_cpnt_idx;

      sn_idx				= ATT_FIRST_CPNT_IDX(attr_idx);

      /* Make space for components, then fill in to handle    */
      /* case of derived type pointing to itself.             */

      for (i = 0; i < ATT_NUM_CPNTS(attr_idx); i++ ) {
         ntr_global_attr_tbl(SN_ATTR_IDX(sn_idx), NULL_IDX);
         sn_idx	= SN_SIBLING_LINK(sn_idx);
      }

      sn_idx				= ATT_FIRST_CPNT_IDX(attr_idx);

      for (i = 0; i < ATT_NUM_CPNTS(attr_idx); i++ ) {
         fill_in_global_attr_ntry(ga_cpnt_idx, SN_ATTR_IDX(sn_idx), NULL_IDX);
         sn_idx	= SN_SIBLING_LINK(sn_idx);
         ga_cpnt_idx++;
      }

      goto EXIT;
   }
   else {
      GT_TYPE(TYP_WORK_IDX)		= TYP_TYPE(type_idx);
      GT_DCL_VALUE(TYP_WORK_IDX)	= TYP_DCL_VALUE(type_idx);
      GT_DESC(TYP_WORK_IDX)		= TYP_DESC(type_idx);
      GT_LINEAR_TYPE(TYP_WORK_IDX)	= TYP_LINEAR(type_idx);
      GT_CHAR_CLASS(TYP_WORK_IDX)	= TYP_CHAR_CLASS(type_idx);
      GT_STRUCT_IDX(TYP_WORK_IDX)	= TYP_IDX(type_idx);
   }

   null_base		= (long *) global_type_tbl;

   for (new_type_idx = 1; new_type_idx  <= global_type_tbl_idx; new_type_idx++){
      found		= TRUE;
      type_tbl_base	= (long *) &(global_type_tbl[new_type_idx]);

      for (i = 0; i < NUM_TYP_WDS; i++) {

         if (null_base[i] != type_tbl_base[i]) {
             found = FALSE;
          }
      }

      if (found) {
         goto EXIT;
      }
   }

   TBL_REALLOC_CK(global_type_tbl, 1);
   new_type_idx	= global_type_tbl_idx;
   global_type_tbl[new_type_idx]	= global_type_tbl[TYP_WORK_IDX];

EXIT: 

   TRACE (Func_Exit, "ntr_global_type_tbl", NULL);

   return(new_type_idx);

}   /* ntr_global_type_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*	This routine adds new bound entries to the global bounds table.  It   *|
|*	attempts to share them all.                                           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
static	int	ntr_global_bounds_tbl(int	bd_idx)

{
   int		 cn_idx;
   int		 dim;
   boolean	 found;
   int		 gb_idx;
   long		*gb_tbl_base;
   int		 i;
   long		*new_base;
   int		 new_gb_idx;
   int		 size;
   int		 type_idx;


   TRACE (Func_Entry, "ntr_global_bounds_tbl", NULL);

   if (BD_GLOBAL_IDX(bd_idx) != NULL_IDX) {
      return(BD_GLOBAL_IDX(bd_idx));
   }

   /* Only keep upper and lower bounds for constant size explicit shape arrays*/

   size	= (BD_ARRAY_CLASS(bd_idx) != Explicit_Shape ||
           BD_ARRAY_SIZE(bd_idx) != Constant_Size) ? 1 : 1+(BD_RANK(bd_idx)*3);

   gb_idx = global_bounds_tbl_idx + 1;

   TBL_REALLOC_CK(global_bounds_tbl, size);

   GB_RANK(gb_idx)		= BD_RANK(bd_idx);
   GB_ARRAY_SIZE(gb_idx)	= BD_ARRAY_SIZE(bd_idx);
   GB_ARRAY_CLASS(gb_idx)	= BD_ARRAY_CLASS(bd_idx);

   if (size > 1) {

      for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

         if (BD_LB_FLD(bd_idx,dim) == CN_Tbl_Idx) {
            cn_idx		 	   = BD_LB_IDX(bd_idx, dim);

            for (i = 0; i < num_host_wds[TYP_LINEAR(CN_TYPE_IDX(cn_idx))]; i++){
               GB_LOWER_BOUND(gb_idx, dim)[i] = 
                                    CP_CONSTANT(CN_POOL_IDX(cn_idx) + i);
            }
            type_idx		    = ntr_global_type_tbl(CN_TYPE_IDX(cn_idx));
            GB_LB_TYPE(gb_idx, dim) = type_idx;
         }

         if (BD_UB_FLD(bd_idx,dim) == CN_Tbl_Idx) {
            cn_idx	  		   = BD_UB_IDX(bd_idx, dim);

            for (i = 0; i < num_host_wds[TYP_LINEAR(CN_TYPE_IDX(cn_idx))]; i++){
               GB_UPPER_BOUND(gb_idx, dim)[i] = 
                                    CP_CONSTANT(CN_POOL_IDX(cn_idx) + i);
            }
            type_idx		    = ntr_global_type_tbl(CN_TYPE_IDX(cn_idx));
            GB_UB_TYPE(gb_idx, dim) = type_idx;
         }
      }
   }

   new_base	= (long *) &(global_bounds_tbl[gb_idx]);
   new_gb_idx	= 1;

   while (new_gb_idx <= (gb_idx - 1)) {
      found		= TRUE;
      gb_tbl_base	= (long *) &(global_bounds_tbl[new_gb_idx]);

      /* Check header information */

      for (i = 0; i < NUM_GB_WDS; i++) {

         if (new_base[i] != gb_tbl_base[i]) {
             found = FALSE;
         }
      }

      if (found && (size > 1)) {  /* Matched header.  Now check dimensions */

         for (i = 0; i < (GB_RANK(gb_idx) * 3); i++) {

            if (new_base[i] != gb_tbl_base[i]) {
                found = FALSE;
            }
         }
      }

      if (found) {
         global_bounds_tbl_idx	= gb_idx - 1;   /* Reset */
         gb_idx			= new_gb_idx;
         goto EXIT;
      }
      new_gb_idx += NUM_GB_WDS;

      if (GB_ARRAY_SIZE(new_gb_idx) == Constant_Size && 
          GB_ARRAY_CLASS(new_gb_idx) == Explicit_Shape) {
         new_gb_idx += (3 * GB_RANK(new_gb_idx));
      }
   }

EXIT: 
   BD_GLOBAL_IDX(bd_idx)	= gb_idx;

   TRACE (Func_Exit, "ntr_global_bounds_tbl", NULL);

   return(gb_idx);

}   /* ntr_global_bounds_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      new ir idx                                                            *|
|*                                                                            *|
\******************************************************************************/
int	ntr_ir_tbl(void)

{
   int		ir_idx;


   TRACE (Func_Entry, "ntr_ir_tbl", NULL);

   if (IR_NEXT_IDX(NULL_IDX) != NULL_IDX) {
      ir_idx			= IR_NEXT_IDX(NULL_IDX);
      IR_NEXT_IDX(NULL_IDX)	= IR_NEXT_IDX(ir_idx);
   }
   else {
      TBL_REALLOC_CK(ir_tbl,1);
      ir_idx = ir_tbl_idx;
   }

   CLEAR_TBL_NTRY(ir_tbl, ir_idx);

   TRACE (Func_Exit, "ntr_ir_tbl", NULL);

   return(ir_idx);

}  /* ntr_ir_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      new il idx                                                            *|
|*                                                                            *|
\******************************************************************************/
int	ntr_ir_list_tbl(void)

{
   int		il_idx;


   TRACE (Func_Entry, "ntr_ir_list_tbl", NULL);

   if (IL_NEXT_LIST_IDX(NULL_IDX) != NULL_IDX) {
      il_idx				= IL_NEXT_LIST_IDX(NULL_IDX);
      IL_NEXT_LIST_IDX(NULL_IDX)	= IL_NEXT_LIST_IDX(il_idx);
   }
   else {
      TBL_REALLOC_CK (ir_list_tbl,1);
      il_idx = ir_list_tbl_idx;
   }

   CLEAR_TBL_NTRY(ir_list_tbl, il_idx);

   TRACE (Func_Exit, "ntr_ir_list_tbl", NULL);

   return(il_idx);

}  /* ntr_ir_list_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      new ir idx                                                            *|
|*                                                                            *|
\******************************************************************************/
int     ntr_gl_ir_tbl(void)

{
   int          ir_idx;


   TRACE (Func_Entry, "ntr_gl_ir_tbl", NULL);

   TBL_REALLOC_CK(global_ir_tbl,1);
   ir_idx = global_ir_tbl_idx;

   CLEAR_TBL_NTRY(global_ir_tbl, ir_idx);

   TRACE (Func_Exit, "ntr_gl_ir_tbl", NULL);

   return(ir_idx);

}  /* ntr_gl_ir_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      new il idx                                                            *|
|*                                                                            *|
\******************************************************************************/
int     ntr_gl_ir_list_tbl(void)

{
   int          il_idx;


   TRACE (Func_Entry, "ntr_gl_ir_list_tbl", NULL);

   TBL_REALLOC_CK (global_ir_list_tbl,1);
   il_idx = global_ir_list_tbl_idx;

   CLEAR_TBL_NTRY(global_ir_list_tbl, il_idx);

   TRACE (Func_Exit, "ntr_gl_ir_list_tbl", NULL);

   return(il_idx);

}  /* ntr_gl_ir_list_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
int     ntr_gl_sh_tbl(void)

{
   int          sh_idx;


   TRACE (Func_Entry, "ntr_gl_sh_tbl", NULL);

   TBL_REALLOC_CK(global_sh_tbl,1);
   sh_idx = global_sh_tbl_idx;

   CLEAR_TBL_NTRY(global_sh_tbl, sh_idx);

   TRACE (Func_Exit, "ntr_gl_sh_tbl", NULL);

   return(sh_idx);

}  /* ntr_gl_sh_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
void	add_attr_to_local_list(int	attr_idx)

{
   int		al_idx;


   TRACE (Func_Entry, "add_attr_to_local_list", NULL);

   NTR_ATTR_LIST_TBL(al_idx);
   AL_ATTR_IDX(al_idx) = attr_idx;

   if (SCP_ATTR_LIST(curr_scp_idx) == NULL_IDX) {
      SCP_ATTR_LIST(curr_scp_idx) = al_idx;
   }
   else {
      AL_NEXT_IDX(SCP_ATTR_LIST_END(curr_scp_idx)) = al_idx;
   }

   SCP_ATTR_LIST_END(curr_scp_idx) = al_idx;

   TRACE (Func_Exit, "add_attr_to_local_list", NULL);

   return;

}  /* add_attr_to_local_list */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
int	ntr_sh_tbl(void)

{
   int		sh_idx;


   TRACE (Func_Entry, "ntr_sh_tbl", NULL);

   if (SH_NEXT_IDX(NULL_IDX) != NULL_IDX) {
      sh_idx			= SH_NEXT_IDX(NULL_IDX);
      SH_NEXT_IDX(NULL_IDX)	= SH_NEXT_IDX(sh_idx);
   }
   else {
      TBL_REALLOC_CK(sh_tbl,1);
      sh_idx = sh_tbl_idx;
   }

   CLEAR_TBL_NTRY(sh_tbl, sh_idx);

   TRACE (Func_Exit, "ntr_sh_tbl", NULL);

   return(sh_idx);

}  /* ntr_sh_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
void	find_opnd_line_and_column(opnd_type	*opnd,
				  int		*line,
				  int		*column)

{
   opnd_type	tmp_opnd;

   TRACE (Func_Entry, "find_opnd_line_and_column", NULL);

   switch (OPND_FLD((*opnd))) {
   case CN_Tbl_Idx:
   case AT_Tbl_Idx:
   case SB_Tbl_Idx:
      *line	= OPND_LINE_NUM((*opnd));
      *column	= OPND_COL_NUM((*opnd));
      break;

   case IR_Tbl_Idx:
      *line	= IR_LINE_NUM(OPND_IDX((*opnd)));
      *column	= IR_COL_NUM(OPND_IDX((*opnd)));
      break;

   case IL_Tbl_Idx:
      COPY_OPND(tmp_opnd, IL_OPND(OPND_IDX((*opnd))));
      find_opnd_line_and_column(&tmp_opnd, line, column);
      break;

   case SH_Tbl_Idx:
      *line     = SH_GLB_LINE(OPND_IDX((*opnd)));
      *column   = SH_COL_NUM(OPND_IDX((*opnd)));
      break;

   default:
      *line	= 0;
      *column	= 0;
      break;
   }

   TRACE (Func_Exit, "find_opnd_line_and_column", NULL);

   return;

}  /* find_opnd_line_and_column */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      srch_hidden_name_tbl searches the local name table for the specified  *|
|*      character string.                                                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      token			token containing identifier or label to       *|
|*                              search for and length in chars of name        *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      name_idx   		local name table index where match occured    *|
|*                              or where entry should be inserted             *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      attribute table index   if found                   		      *|
|*      NULL_IDX                if not found				      *|
|*                                                                            *|
\******************************************************************************/

int srch_hidden_name_tbl(char	*name_str,
		  	 int	 name_len,
		  	 int	 attr_idx,
		  	 int	*np_idx,
                  	 int	*name_idx)

{
  int		first;
  int		idx;
  long		tst_val;
      

  TRACE (Func_Entry, "srch_hidden_name_tbl", name_str);

  first	  = SCP_HN_FW_IDX(curr_scp_idx);

  tst_val = srch_name_tbl(name_str, 
                          name_len,
                          &idx,
                          hidden_name_tbl,
                          name_pool,
                          first,
                          SCP_HN_LW_IDX(curr_scp_idx));


   *name_idx = idx;

   if (tst_val != 0) {  /* No match */
      idx	= NULL_IDX;
      *np_idx	= NULL_IDX;
   }  
   else {

      /* The name exists.  Find the start of this name group. */

      while (HN_NAME_IDX(*name_idx) == HN_NAME_IDX((*name_idx) - 1)) {
         (*name_idx)--;
      }

      *np_idx	= HN_NAME_IDX(*name_idx);

      if (attr_idx != NULL_IDX) {
         first = *name_idx;
      
         while (HN_ATTR_IDX(*name_idx) != attr_idx) {

            if (HN_NAME_IDX((*name_idx)++) != *np_idx) {
               *name_idx = first;
               break;
            }
         }
      }
      idx = HN_ATTR_IDX(*name_idx);
   }

   TRACE (Func_Exit, "srch_hidden_name_tbl", NULL);

   return (idx);
 
}  /* srch_hidden_name_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ntr_sym_tbl adds the token name to the the name pool, links it        *|
|*      to an attribute table entry through the local name table, and         *|
|*      reserves an attribute table entry for the identifier or label.        *|
|*      The attribute table entry field name_idx is linked to the name in     *|
|*      the name pool.                                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      token                   token containing identifier or label and      *|
|*                              length of name to be added to symbol table    *|
|*                                                                            *|
|*      name_idx                local name table index where entry is to      *|
|*                              be inserted                                   *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      attribute table index of reserved entry                               *|
|*                                                                            *|
\******************************************************************************/

void ntr_hidden_name_tbl(int    attr_idx,
			 int	np_idx,
	                 int    name_idx)

{
   register int          i;
   register int		 scp_idx;

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   register long        *name_tbl_base; /* name table base address */
# endif


   TRACE (Func_Entry, "ntr_hidden_name_tbl", NULL);

   if (np_idx == NULL_IDX) {
      np_idx = AT_ORIG_NAME_IDX(attr_idx);

      if (np_idx == NULL_IDX) {
         np_idx = AT_NAME_IDX(attr_idx);
      }
   }

   TBL_REALLOC_CK(hidden_name_tbl, 1);

   if ((hidden_name_tbl_idx - 1) != SCP_HN_LW_IDX(curr_scp_idx)) {

      /* Attempting to enter name into a scope that does not reside at the    */
      /* end of the local name table.  Make room for this entry in that scope */
      /* and then adjust the other scopes name table LW and FW values.        */

      for (scp_idx = 1; scp_idx <= scp_tbl_idx; scp_idx++) {

         if (SCP_HN_FW_IDX(scp_idx) > SCP_HN_LW_IDX(curr_scp_idx)) {
            SCP_HN_FW_IDX(scp_idx) = SCP_HN_FW_IDX(scp_idx) + 1;
            SCP_HN_LW_IDX(scp_idx) = SCP_HN_LW_IDX(scp_idx) + 1;
         }
      }
      SCP_HN_LW_IDX(curr_scp_idx)++;
   }
   else {
   
      /* Adding to local name table for last (most recent) scope.  No        */
      /* adjusting of other scope local name table entries is necessary.     */

      SCP_HN_LW_IDX(curr_scp_idx)	= hidden_name_tbl_idx;
   }

   /* Enter name in correct position.  Link name pool and attribute table */

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   name_tbl_base = (long *) hidden_name_tbl;
# endif

#  pragma _CRI ivdep
   for (i = hidden_name_tbl_idx; i >= name_idx; i--) {
# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
      name_tbl_base [i] = name_tbl_base [i-1];
# else
      hidden_name_tbl [i]  = hidden_name_tbl [i-1];
# endif
   }

   CLEAR_TBL_NTRY(hidden_name_tbl, name_idx);
   HN_ATTR_IDX(name_idx)	= attr_idx;
   HN_NAME_IDX(name_idx)	= np_idx;
   HN_NAME_LEN(name_idx)	= AT_ORIG_NAME_LEN(attr_idx);

   TRACE (Func_Exit, "ntr_hidden_name_tbl", NULL);

   return;

}  /* ntr_hidden_name_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      name_idx                hidden name table index to remove.            *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void remove_hidden_name_ntry(int         name_idx)

{
   register int          i;

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   register long        *name_tbl_base; /* name table base address */
# endif


   TRACE (Func_Entry, "remove_hidden_name_ntry", NULL);

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   name_tbl_base = (long *) hidden_name_tbl;
# endif

   /* Remove name */

#  pragma _CRI ivdep
   for (i = name_idx; i < SCP_HN_LW_IDX(curr_scp_idx); i++) {
# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
      name_tbl_base [i] = name_tbl_base [i+1];
# else
      hidden_name_tbl [i]  = hidden_name_tbl [i+1];
# endif
   }

   if (hidden_name_tbl_idx == SCP_HN_LW_IDX(curr_scp_idx)) {
      hidden_name_tbl_idx--;
   }

   SCP_HN_LW_IDX(curr_scp_idx)--;

   TRACE (Func_Exit, "remove_hidden_name_ntry", NULL);

   return;

}  /* remove_hidden_name_ntry */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      attr_idx                Attr with needs ATD_STOR_BLK_IDX set.         *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void assign_storage_blk(int         attr_idx)

{
   int			pgm_attr_idx;
   boolean		pointer;
   int			sb_idx;
   id_str_type		stor_name;

# if defined(_TARGET_OS_SOLARIS)
   size_offset_type	num;
   size_offset_type	size;
# endif


   TRACE (Func_Entry, "assign_storage_blk", NULL);

#ifdef KEY /* Bug 14150 */
   /* Don't overwrite value if already set by set_binding_label() */
   if (ATD_STOR_BLK_IDX(attr_idx)) {
     return;
   }
#endif /* KEY Bug 14150 */

   pgm_attr_idx		= SCP_ATTR_IDX(curr_scp_idx);
#ifdef KEY /* Bug 6845 */
   int typ_idx = TYP_IDX(ATD_TYPE_IDX(attr_idx));
#endif /* KEY Bug 6845 */
   pointer		= ATD_IM_A_DOPE(attr_idx) ||
                         (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure &&
#ifdef KEY /* Bug 6845 */
			  (ATT_ALLOCATABLE_CPNT(typ_idx) ||
			  ATT_POINTER_CPNT(typ_idx))
#else /* KEY Bug 6845 */
                          ATT_POINTER_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx)))
#endif /* KEY Bug 6845 */
			  );

   if (ATD_AUTOMATIC(attr_idx)) {
      ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_BASED_IDX(curr_scp_idx);
   }
   else if (ATD_DATA_INIT(attr_idx)) {
      ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
   }
   else if (ATD_SAVED(attr_idx)) {

      if (pointer) {
         ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
      }
      else if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX &&
               TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Structure) {
         ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_IDX(curr_scp_idx);
      }
      else {
         ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx);
      }
   }
   else if (ATD_SYMBOLIC_CONSTANT(attr_idx)) {

      /* This is a placeholder so it doesn't really need storage or an offset.*/

      ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
   }

   /* The symmetric check needs to follow the save all check. */

   else if (ATD_SYMMETRIC(attr_idx)) { 

      if (SCP_SB_SYMMETRIC_IDX(curr_scp_idx) == NULL_IDX) {
         CREATE_ID(stor_name, sb_name[Sym_Blk], sb_len[Sym_Blk]);
         sb_idx	= ntr_stor_blk_tbl(stor_name.string, sb_len[Sym_Blk],
                                   AT_DEF_LINE(attr_idx),
                                   AT_DEF_COLUMN(attr_idx),
                                   Stack);
         SB_SYMMETRIC(sb_idx)			= TRUE;
         SCP_SB_SYMMETRIC_IDX(curr_scp_idx)	= sb_idx;
      }
      ATD_STOR_BLK_IDX(attr_idx)	= SCP_SB_SYMMETRIC_IDX(curr_scp_idx);
   }
   else if (ATD_STACK(attr_idx) || ATP_STACK_DIR(pgm_attr_idx)) {
      ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
   }
   else if (cmd_line_flags.co_array_fortran &&
            ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX &&
            !ATD_IM_A_DOPE(attr_idx)) {

      /* Non dope vector Co arrays go in static storage. */

      if (pointer) {
         ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_IDX(curr_scp_idx);
      }
      else if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX && 
               TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Structure) {
         ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_IDX(curr_scp_idx);
      }
      else {
         ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx);
      }
   }

# if defined(_TARGET_OS_SOLARIS)

   /* On solaris, all data in the main program is static data. */
   /* Can be overridden by the STACK directive or AUTOMATIC.   */

   else if (ATP_PGM_UNIT(pgm_attr_idx) == Program &&
            (ATD_ARRAY_IDX(attr_idx) != NULL_IDX ||
             TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character ||
             TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure)) {

           /* Could possibly be bigger than 256 bits. */

      size		= stor_bit_size_of(attr_idx, TRUE, FALSE);
      C_TO_F_INT(num.constant, 256, CG_INTEGER_DEFAULT_TYPE);
      num.fld		= NO_Tbl_Idx;
      num.type_idx	= CG_INTEGER_DEFAULT_TYPE;

      size_offset_logical_calc(&size, &num, Gt_Opr, &num);

      if (THIS_IS_TRUE(num.constant, num.type_idx)) {

         if (pointer) {
            ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
         }
         else if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX &&
                  TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Structure) {
            ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_IDX(curr_scp_idx);
         }
         else {
            ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx);
         }
      }
      else if (SCP_DEFAULT_STORAGE(curr_scp_idx) == Stack) {
         ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
      }
      else if (pointer) {
         ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
      }
      else if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX &&
               TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Structure) {
         ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_IDX(curr_scp_idx);
      }
      else {
         ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx);
      }
   }
# endif

   else if (SCP_DEFAULT_STORAGE(curr_scp_idx) == Stack) {
      ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
   }
   else if (pointer) {
      ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
   }
   else if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX &&
            TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Structure) {
      ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_IDX(curr_scp_idx);
   }
   else {
      ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx);
   }

   TRACE (Func_Exit, "assign_storage_blk", NULL);

   return;

}  /* assign_storage_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      bit_len  -> The bit or byte length to be aligned.                     *|
|*      align_to -> This routine will align to a 32 bit boundary or a 64 bit  *|
|*                  boundary.  If you want to double align on a 32 bit target *|
|*                  then set align_to to 64.  If you want to pack on a 64 bit *|
|*                  target, then set align_to to 32.                          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      bit_len  -> The newly aligned bit or byte length.                     *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void	align_bit_length(size_offset_type	*bit_len,
		         int		 	 align_to)

{
   boolean	 arith_ok;
   int		 column;
   long_type	*constant;
   int		 line;
   opnd_type	 opnd;
#ifdef KEY /* Bug 10177 */
   long		 plus_val = 0;
#else /* KEY Bug 10177 */
   long		 plus_val;
#endif /* KEY Bug 10177 */
   int		 plus_ir_idx;
   int		 result_type;
# if defined(KEY)
   int           saved_result_type;
# endif
   int		 shiftr_ir_idx;
   int		 shiftl_ir_idx;
#ifdef KEY /* Bug 10177 */
   long		 shift_val = 0;
#else /* KEY Bug 10177 */
   long		 shift_val;
#endif /* KEY Bug 10177 */
   operator_type shiftl_opr;
   operator_type shiftr_opr;
   boolean	 symbolic_constant;
   int		 type_idx;
   long		 value;

# if !defined(_TARGET64) || !defined(_HOST64) || defined(_WHIRL_HOST64_TARGET64)
   long_type	 plus_target_val[MAX_WORDS_FOR_INTEGER];
   long_type	 shift_target_val[MAX_WORDS_FOR_INTEGER];
# endif


   TRACE (Func_Entry, "align_bit_length", NULL);

   switch (align_to) {
   case 128:
      plus_val	=	127;
      shift_val	=	7;
      break;

   case 64:
      plus_val	=	63;
      shift_val	=	6;
      break;

   case 32:
      plus_val	=	31;
      shift_val	=	5;
      break;

   case 16:
      plus_val	=	15;
      shift_val	=	4;
      break;

   case 8:
      plus_val	=	7;
      shift_val	=	3;
      break;

   default:             /* Input must be 8, 16, 32, 64 or 128 */
      PRINTMSG(stmt_start_line, 1173, Internal, stmt_start_col);
      break;
   }

   if ((*bit_len).fld == NO_Tbl_Idx || (*bit_len).fld == CN_Tbl_Idx) {

      if ((*bit_len).fld == NO_Tbl_Idx) {
         result_type	= (*bit_len).type_idx;
         constant	= (*bit_len).constant;
      }
      else {
         result_type	= CN_TYPE_IDX((*bit_len).idx);
         constant	= &(CN_CONST((*bit_len).idx));
      }
# if defined(KEY)
      saved_result_type = result_type;
      if (result_type == Integer_4 && *constant < 0)
        result_type = Integer_8;
# endif

#     if defined(_TARGET64) && defined(_HOST64) &&  \
      !defined(_TARGET_LITTLE_ENDIAN)

         (*bit_len).constant[0] = ((((constant[0]) + plus_val) >> shift_val)
                                                               << shift_val);
#     else

# if defined(_USE_FOLD_DOT_f)
         shiftl_opr 	= Mult_Opr;
         shiftr_opr 	= Div_Opr;
         value		= plus_val+1;
# else
         shiftl_opr 	= Shiftl_Opr;
         shiftr_opr 	= Shiftr_Opr;
         value		= shift_val;
# endif

         C_TO_F_INT(plus_target_val, plus_val, CG_INTEGER_DEFAULT_TYPE);
         C_TO_F_INT(shift_target_val, value, CG_INTEGER_DEFAULT_TYPE);

         arith_ok = folder_driver((char *) constant,
                                  result_type,
                                  (char *) &plus_target_val,
                                  CG_INTEGER_DEFAULT_TYPE,
                                  (*bit_len).constant,
                                  &result_type,
                                  stmt_start_line,
                                  stmt_start_col,
                                  2,
                                  Plus_Opr);
   

         arith_ok |= folder_driver((char *) (*bit_len).constant,
                                   result_type,
                                   (char *) &shift_target_val,
                                   CG_INTEGER_DEFAULT_TYPE,
                                   (*bit_len).constant,
                                   &result_type,
                                   stmt_start_line,
                                   stmt_start_col,
                                   2,
                                   shiftr_opr);

         arith_ok |= folder_driver((char *) (*bit_len).constant,
                                   result_type,
                                   (char *) &shift_target_val,
                                   CG_INTEGER_DEFAULT_TYPE,
                                   (*bit_len).constant,
                                   &result_type,
                                   stmt_start_line,
                                   stmt_start_col,
                                   2,
                                   shiftl_opr);
#     endif

      /* If we overflow - folder_driver will issue the error */
# if defined(KEY)
      (*bit_len).type_idx	= saved_result_type;
# else 
      (*bit_len).type_idx	= result_type;
# endif
      (*bit_len).fld		= NO_Tbl_Idx;
   }
   else {  /* This contains the IR for the value to be bit aligned. */
      symbolic_constant		= FALSE;
      NTR_IR_TBL(plus_ir_idx);
      NTR_IR_TBL(shiftr_ir_idx);
      NTR_IR_TBL(shiftl_ir_idx);

      if ((*bit_len).fld == IR_Tbl_Idx) {
         COPY_TBL_NTRY(ir_tbl, plus_ir_idx, (*bit_len).idx);
         line		= IR_LINE_NUM(plus_ir_idx);
         column		= IR_COL_NUM(plus_ir_idx);
      }
      else {

         if ((*bit_len).fld == AT_Tbl_Idx) {
            line		= AT_DEF_LINE((*bit_len).idx);
            column 		= AT_DEF_COLUMN((*bit_len).idx);
            symbolic_constant	= (AT_OBJ_CLASS((*bit_len).idx) == Data_Obj) &&
                                  ATD_SYMBOLIC_CONSTANT((*bit_len).idx);
         }
         else {  /* This case shouldn't happen - but just in case. */
            line	= stmt_start_line;
            column	= stmt_start_line;
         }

         IR_LINE_NUM_L(plus_ir_idx)	= line;
         IR_LINE_NUM_R(plus_ir_idx)	= line;
         IR_LINE_NUM(plus_ir_idx)	= line;
         IR_COL_NUM_L(plus_ir_idx)	= column;
         IR_COL_NUM_R(plus_ir_idx)	= column;
         IR_COL_NUM(plus_ir_idx)	= column;
      }

      OPND_FLD(opnd)		= (*bit_len).fld;
      OPND_IDX(opnd)		= (*bit_len).idx;
      OPND_LINE_NUM(opnd)	= line;
      OPND_COL_NUM(opnd)	= column;

      COPY_TBL_NTRY(ir_tbl, shiftr_ir_idx, plus_ir_idx);
      COPY_TBL_NTRY(ir_tbl, shiftl_ir_idx, plus_ir_idx);

      if (symbolic_constant) {  /* Always an attr idx. */
         IR_OPR(plus_ir_idx)	= Symbolic_Plus_Opr;
         IR_OPR(shiftr_ir_idx)	= Symbolic_Shiftr_Opr;
         IR_OPR(shiftl_ir_idx)	= Symbolic_Shiftl_Opr;
         type_idx		= ATD_TYPE_IDX((*bit_len).idx);
      }
      else {
         type_idx		= check_type_for_size_address(&opnd);
         IR_OPR(plus_ir_idx)	= Plus_Opr;
         IR_OPR(shiftr_ir_idx)	= Shiftr_Opr;
         IR_OPR(shiftl_ir_idx)	= Shiftl_Opr;
      }

      IR_TYPE_IDX(plus_ir_idx)		= type_idx;
      IR_TYPE_IDX(shiftr_ir_idx)	= type_idx;
      IR_TYPE_IDX(shiftl_ir_idx)	= type_idx;

      IR_FLD_L(plus_ir_idx)	= OPND_FLD(opnd);
      IR_IDX_L(plus_ir_idx)	= OPND_IDX(opnd);

      IR_FLD_R(plus_ir_idx)	= CN_Tbl_Idx;
      IR_IDX_R(plus_ir_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,plus_val);

      IR_FLD_L(shiftr_ir_idx)	= IR_Tbl_Idx;
      IR_IDX_L(shiftr_ir_idx)	= plus_ir_idx;

      IR_FLD_R(shiftr_ir_idx)	= CN_Tbl_Idx;
      IR_IDX_R(shiftr_ir_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              shift_val);

      IR_FLD_L(shiftl_ir_idx)	= IR_Tbl_Idx;
      IR_IDX_L(shiftl_ir_idx)	= shiftr_ir_idx;

      IR_FLD_R(shiftl_ir_idx)	= CN_Tbl_Idx;
      IR_IDX_R(shiftl_ir_idx)	= IR_IDX_R(shiftr_ir_idx);

      if (symbolic_constant) {
         (*bit_len).fld		= AT_Tbl_Idx;
         (*bit_len).idx		= gen_compiler_tmp(IR_LINE_NUM(shiftl_ir_idx),
                                                   IR_COL_NUM(shiftl_ir_idx),
                                                   Shared, TRUE);
         ATD_TYPE_IDX((*bit_len).idx)		= SA_INTEGER_DEFAULT_TYPE;
         ATD_FLD((*bit_len).idx)		= IR_Tbl_Idx;
         ATD_TMP_IDX((*bit_len).idx)		= shiftl_ir_idx;
         ATD_SYMBOLIC_CONSTANT((*bit_len).idx)	= TRUE;
      }
      else {
         (*bit_len).idx		= shiftl_ir_idx;
         (*bit_len).fld		= IR_Tbl_Idx;
      }
   }

   TRACE (Func_Exit, "align_bit_length", NULL);

   return;

}  /* align_bit_length */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      opnd -> The operand whose size to check and convert if necessary.     *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      opnd -> The original operand with Cvrt_Opr wrapped around if necessary*|
|*                                                                            *|
|* Returns:                                                                   *|
|*      type_idx -> The type index for the type of the operand now.           *|
|*                                                                            *|
\******************************************************************************/

int	check_type_for_size_address(opnd_type	*opnd)

{
   int		 col;
   boolean	 cvrt;
   int		 cvrt_idx;
   int		 line;
   int		 type_idx;



   TRACE (Func_Entry, "check_type_for_size_address", NULL);

   switch (OPND_FLD((*opnd))) {
   case IR_Tbl_Idx:
      line	= IR_LINE_NUM(OPND_IDX((*opnd)));
      col	= IR_COL_NUM(OPND_IDX((*opnd)));
      type_idx	= IR_TYPE_IDX(OPND_IDX((*opnd)));
      cvrt	= (SA_INTEGER_DEFAULT_TYPE > TYP_LINEAR(type_idx));
      break;

   case AT_Tbl_Idx:
      line	= OPND_LINE_NUM((*opnd));
      col	= OPND_COL_NUM((*opnd));
      type_idx	= ATD_TYPE_IDX(OPND_IDX((*opnd)));
      cvrt	= (SA_INTEGER_DEFAULT_TYPE > TYP_LINEAR(type_idx));
      break;

   case CN_Tbl_Idx:
      line	= OPND_LINE_NUM((*opnd));
      col	= OPND_COL_NUM((*opnd));
      type_idx	= CN_TYPE_IDX(OPND_IDX((*opnd)));
      cvrt	= (SA_INTEGER_DEFAULT_TYPE > TYP_LINEAR(type_idx));
      break;

   default:
      /* BHJ - this should be an internal error here */
      line	= OPND_LINE_NUM((*opnd));
      col	= OPND_COL_NUM((*opnd));
      type_idx	= SA_INTEGER_DEFAULT_TYPE;
      cvrt	= TRUE;
      break;
   }

   if (cvrt) {
      NTR_IR_TBL(cvrt_idx);
      IR_OPR(cvrt_idx)		= Cvrt_Opr;
      IR_TYPE_IDX(cvrt_idx)	= SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(cvrt_idx)	= line;
      IR_COL_NUM(cvrt_idx)	= col;
      type_idx			= SA_INTEGER_DEFAULT_TYPE;

      if (OPND_FLD((*opnd)) == IR_Tbl_Idx &&
          IR_OPR(OPND_IDX((*opnd))) == Asg_Opr) {

         /* Cvrt_Opr goes to the right of the Asg_Opr, not on top */

         COPY_OPND(IR_OPND_L(cvrt_idx), IR_OPND_R(OPND_IDX((*opnd))));
         IR_FLD_R(OPND_IDX((*opnd))) = IR_Tbl_Idx;
         IR_IDX_R(OPND_IDX((*opnd))) = cvrt_idx;
      }
      else {
         COPY_OPND(IR_OPND_L(cvrt_idx), (*opnd));
         OPND_FLD((*opnd))		= IR_Tbl_Idx;
         OPND_IDX((*opnd))		= cvrt_idx;
      }
   }

   TRACE (Func_Exit, "check_type_for_size_address", NULL);

   return(type_idx);

}  /* check_type_for_size_address */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      bit_len  -> The bit or byte length to be aligned.                     *|
|*      plus_val -> The value to add when calculating bits or bytes to words. *|
|*      shift_val-> The value to shift by when calculating bits or bytes to   *|
|*		    words.						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      bit_len  -> The newly aligned bit or byte length.                     *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void	bits_and_bytes_to_words(size_offset_type *bit_len,
		      		int		  the_plus_val,
				int		  the_shift_val)

{
   boolean	 arith_ok;
#ifdef KEY /* Bug 10177 */
   int		 column = 0;
   long_type	*constant;
   int		 line = 0;
#else /* KEY Bug 10177 */
   int		 column;
   long_type	*constant;
   int		 line;
#endif /* KEY Bug 10177 */
   opnd_type	 opnd;
   int		 plus_ir_idx;
   int		 result_type;
   int		 shiftr_ir_idx;
   boolean	 symbolic_constant;
   long		 plus_val;
   long		 shift_val;
   operator_type shiftr_opr;
   int		 type_idx;
   long		 value; 	

# if !defined(_TARGET64) || !defined(_HOST64) || defined(_WHIRL_HOST64_TARGET64)
   long_type	 plus_target_val[MAX_WORDS_FOR_INTEGER];
   long_type	 shift_target_val[MAX_WORDS_FOR_INTEGER];
# endif


   TRACE (Func_Entry, "bits_and_bytes_to_words", NULL);

   plus_val	= the_plus_val;
   shift_val	= the_shift_val;

   if ((*bit_len).fld == NO_Tbl_Idx || (*bit_len).fld == CN_Tbl_Idx) {

      if ((*bit_len).fld == NO_Tbl_Idx) {
         result_type	= (*bit_len).type_idx;
         constant	= (*bit_len).constant;
      }
      else {
         result_type	= CN_TYPE_IDX((*bit_len).idx);
         constant	= &(CN_CONST((*bit_len).idx));
      }

#     if defined(_TARGET64) && defined(_HOST64) && \
         !defined(_TARGET_LITTLE_ENDIAN)

         (*bit_len).constant[0] = (((constant[0]) + plus_val) >> shift_val);
#     else

# if defined(_USE_FOLD_DOT_f)
         shiftr_opr 	= Div_Opr;
         value		= plus_val + 1;
# else
         shiftr_opr 	= Shiftr_Opr;
         value		= shift_val;
# endif

         C_TO_F_INT(plus_target_val, plus_val, CG_INTEGER_DEFAULT_TYPE);
         C_TO_F_INT(shift_target_val, value, CG_INTEGER_DEFAULT_TYPE);

         arith_ok = folder_driver((char *) constant,
                                  result_type,
                                  (char *) &plus_target_val,
                                  CG_INTEGER_DEFAULT_TYPE,
                                  (*bit_len).constant,
                                  &result_type,
                                  stmt_start_line,
                                  stmt_start_col,
                                  2,
                                  Plus_Opr);
   

         arith_ok != folder_driver((char *) (*bit_len).constant,
                                   result_type,
                                   (char *) &shift_target_val,
                                   CG_INTEGER_DEFAULT_TYPE,
                                   (*bit_len).constant,
                                   &result_type,
                                   stmt_start_line,
                                   stmt_start_col,
                                   2,
                                   shiftr_opr);

#     endif

      /* If we overflow - folder_driver will issue the error */
 
      (*bit_len).type_idx	= result_type;
      (*bit_len).fld		= NO_Tbl_Idx;
   }
   else {  /* This contains the IR for the value to be bit aligned. */
      symbolic_constant		= FALSE;
      NTR_IR_TBL(plus_ir_idx);
      NTR_IR_TBL(shiftr_ir_idx);

      if ((*bit_len).fld == IR_Tbl_Idx) {
         COPY_TBL_NTRY(ir_tbl, plus_ir_idx, (*bit_len).idx);
      }
      else {

         if ((*bit_len).fld == AT_Tbl_Idx) {
            line		= AT_DEF_LINE((*bit_len).idx);
            column		= AT_DEF_COLUMN((*bit_len).idx);
            symbolic_constant	= (AT_OBJ_CLASS((*bit_len).idx) == Data_Obj) &&
                                  ATD_SYMBOLIC_CONSTANT((*bit_len).idx);
         }
         else {  /* This case shouldn't happen - but just in case. */
            line		= stmt_start_line;
            column		= stmt_start_col;
         }
         IR_LINE_NUM_L(plus_ir_idx)	= line;
         IR_LINE_NUM_R(plus_ir_idx)	= line;
         IR_LINE_NUM(plus_ir_idx)	= line;
         IR_COL_NUM_L(plus_ir_idx)	= column;
         IR_COL_NUM_R(plus_ir_idx)	= column;
         IR_COL_NUM(plus_ir_idx)	= column;
      }

      OPND_FLD(opnd)			= (*bit_len).fld;
      OPND_IDX(opnd)			= (*bit_len).idx;
      OPND_LINE_NUM(opnd)		= line;
      OPND_COL_NUM(opnd)		= column;

      COPY_TBL_NTRY(ir_tbl, shiftr_ir_idx, plus_ir_idx);

      if (symbolic_constant) {
         IR_OPR(plus_ir_idx)	= Symbolic_Plus_Opr;
         IR_OPR(shiftr_ir_idx)	= Symbolic_Shiftr_Opr;
         type_idx		= ATD_TYPE_IDX((*bit_len).idx);
      }
      else {
         IR_OPR(plus_ir_idx)	= Plus_Opr;
         IR_OPR(shiftr_ir_idx)	= Shiftr_Opr;
         type_idx		= check_type_for_size_address(&opnd);
      }

      IR_TYPE_IDX(plus_ir_idx)	= type_idx;
      IR_TYPE_IDX(shiftr_ir_idx)= type_idx;

      IR_FLD_L(plus_ir_idx)	= OPND_FLD(opnd);
      IR_IDX_L(plus_ir_idx)	= OPND_IDX(opnd);

      IR_FLD_R(plus_ir_idx)	= CN_Tbl_Idx;
      IR_IDX_R(plus_ir_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,plus_val);

      IR_FLD_L(shiftr_ir_idx)	= IR_Tbl_Idx;
      IR_IDX_L(shiftr_ir_idx)	= plus_ir_idx;

      IR_FLD_R(shiftr_ir_idx)	= CN_Tbl_Idx;
      IR_IDX_R(shiftr_ir_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              shift_val);

      if (symbolic_constant) {
         (*bit_len).fld		= AT_Tbl_Idx;
         (*bit_len).idx		= gen_compiler_tmp(IR_LINE_NUM(shiftr_ir_idx),
                                                   IR_COL_NUM(shiftr_ir_idx),
                                                   Shared, TRUE);
         ATD_TYPE_IDX((*bit_len).idx)		= SA_INTEGER_DEFAULT_TYPE;
         ATD_FLD((*bit_len).idx)		= IR_Tbl_Idx;
         ATD_TMP_IDX((*bit_len).idx)		= shiftr_ir_idx;
         ATD_SYMBOLIC_CONSTANT((*bit_len).idx)	= TRUE;
      }
      else {
         (*bit_len).idx		= shiftr_ir_idx;
         (*bit_len).fld		= IR_Tbl_Idx;
      }
   }

   TRACE (Func_Exit, "bits_and_bytes_to_words", NULL);

   return;

}  /* bits_and_bytes_to_words */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      new attr list idx                                                     *|
|*                                                                            *|
\******************************************************************************/
int	ntr_attr_list_tbl(void)

{
   int		al_idx;


   TRACE (Func_Entry, "ntr_attr_list_tbl", NULL);

   if (AL_NEXT_IDX(NULL_IDX) != NULL_IDX) {
      al_idx			= AL_NEXT_IDX(NULL_IDX);
      AL_NEXT_IDX(NULL_IDX)	= AL_NEXT_IDX(al_idx);
   }
   else {
      TBL_REALLOC_CK(attr_list_tbl, 1);
      al_idx = attr_list_tbl_idx;
   }

   CLEAR_TBL_NTRY(attr_list_tbl, al_idx);

   TRACE (Func_Exit, "ntr_attr_list_tbl", NULL);

   return(al_idx);

}  /* ntr_attr_list_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
void	free_attr_list(int	al_idx)

{
   int		free_list_start;
   int		old_free_list_start;
   int		prev_idx;


   TRACE (Func_Entry, "free_attr_list", NULL);

   if (al_idx != NULL_IDX) {
      old_free_list_start	= AL_NEXT_IDX(NULL_IDX);
      free_list_start		= al_idx;
      prev_idx			= NULL_IDX;

      while (al_idx != NULL_IDX) {

         if (AL_FREE(al_idx)) {

            /* This is already on the free list.  Do not add.  */

            if (al_idx == free_list_start) {
               free_list_start	= AL_NEXT_IDX(al_idx);
            }
            else {
               AL_NEXT_IDX(prev_idx)	= AL_NEXT_IDX(al_idx);
            }
         }
         else {
            AL_FREE(al_idx)	= TRUE;
            prev_idx		= al_idx;
         }
         al_idx			= AL_NEXT_IDX(al_idx);
      }

      AL_NEXT_IDX(NULL_IDX)	= free_list_start;
      AL_NEXT_IDX(prev_idx)	= old_free_list_start;
   }

   TRACE (Func_Exit, "free_attr_list", NULL);

   return;

}  /* free_attr_list */
#ifdef KEY /* Bug 6204 */

/******************************************************************************\
|*                                                                            *|
|* Description: Decorate external symbol with trailing underscores, or by     *|
|* replacing it with the symbol from the "-fdecorate" machinery    .          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      identifier: Identifier to be decorated (buffer assumed to have room)  *|
|*      underscores: Number of underscores in identifier                      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
int decorate(char *identifier, int name_len, int underscores)
{
   identifier[name_len] = 0;
   const char *decoration = get_symbol_decoration(identifier);
   if (decoration) {
     strcpy(identifier, decoration);
     return strlen(identifier);
     }
   if (on_off_flags.underscoring) {
      identifier[name_len++] = '_';
      if (on_off_flags.second_underscore && (underscores > 0)) {
	 identifier[name_len++] = '_';
      }
   }
   return name_len;
}
#endif /* KEY Bug 6204 */
#ifdef KEY /* Bug 14150 */
/*
 * Implement BIND(C, NAME=X)
 * fld			AT_Tbl_Idx or SB_Tbl_Idx
 * idx			Index of program, data object, or storage block
 * new_binding_label	Binding label to use for that entity
 */
void set_binding_label(fld_type fld, int idx, token_type *new_binding_label) {
  TRACE (Func_Entry, "set_binding_label", NULL);
  int sb = (fld == SB_Tbl_Idx);
  char *name = 0;
  int previous = 0;
  if (sb) {
    if (SB_BIND_ATTR(idx)) {
      name = SB_NAME_PTR(idx);
      previous = SB_DEF_LINE(idx);
    }
    SB_BIND_ATTR(idx) = TRUE;
  }
  else {
    if (AT_BIND_ATTR(idx)) {
      name = AT_OBJ_NAME_PTR(idx);
      previous = AT_DEF_LINE(idx);
    }
    AT_BIND_ATTR(idx) = TRUE;
  }
  if (previous) {
    PRINTMSG(TOKEN_LINE(*new_binding_label), 1696, Error,
      TOKEN_COLUMN(*new_binding_label), name, previous);
  }

  int len = 0;
  id_str_type id;

  /* The BIND(C) attribute had the optional NAME= clause */
  if (BIND_SPECIFIES_NAME(*new_binding_label)) {
    len = TOKEN_LEN(*new_binding_label);
    /* Trim leading/trailing blanks but preserve case */
    char *nonblank_p = TOKEN_STR(*new_binding_label);
    for (; len > 0 && *nonblank_p == ' '; len -= 1, nonblank_p += 1) { }
    for (; len > 0 && nonblank_p[len - 1] == ' '; len -= 1) { }
    /* If NAME= appears but is empty, no binding label */
    if (!len) {
      return;
    }
    CREATE_ID(id, nonblank_p, len);
  }

  /* Optional NAME= clause missing, so variables and non-dummy procedures
   * derive the binding label from the identifier */
  else if (AT_OBJ_CLASS(idx) != Derived_Type &&
    (AT_OBJ_CLASS(idx) != Pgm_Unit || ATP_PROC(idx) != Dummy_Proc)) {
    /* Explicitly set the external name to match the lowercase internal name;
     * otherwise, underscoring would take place */
    len = sb ? SB_NAME_LEN(idx) : AT_NAME_LEN(idx);
    CREATE_ID(id, (sb ? SB_NAME_PTR(idx) : AT_OBJ_NAME_PTR(idx)), len);
    for (int j = 0; j < len; j += 1) {
      id.string[j] = tolower(id.string[j]);
    }
  }

  int name_idx;
  NTR_NAME_POOL(id.words, len, name_idx);
  if (sb) {
    SB_EXT_NAME_IDX(idx) = name_idx;
    SB_EXT_NAME_LEN(idx) = len;
  }
  else if (AT_OBJ_CLASS(idx) == Pgm_Unit) {
    ATP_EXT_NAME_IDX(idx) = name_idx;
    ATP_EXT_NAME_LEN(idx) = len;
  }
  else if (AT_OBJ_CLASS(idx) == Data_Obj) {
    int sb_idx = ATD_STOR_BLK_IDX(idx) = ntr_stor_blk_tbl(id.string, len,
      AT_DEF_LINE(idx), AT_DEF_COLUMN(idx), Static);
    SB_FIRST_ATTR_IDX(sb_idx) = idx;
    SB_MODULE(sb_idx) = TRUE;
    SB_EXT_NAME_IDX(sb_idx) = SB_NAME_IDX(sb_idx); /* Defeat underscoring */
    SB_EXT_NAME_LEN(sb_idx) = SB_NAME_LEN(sb_idx);
  }
#ifdef _DEBUG
  else {
    /* Not exactly what this procedure was meant for, but good enough */
    sytb_var_error("binding label", idx);
  }
#endif /* _DEBUG */
  TRACE (Func_Exit, "set_binding_label", NULL);
}
#endif /* KEY Bug 14150 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

void	make_external_name(int	attr_idx,
			   int	name_idx,
			   int	name_len)

{
   token_type	 ext_token;
   int		 i;
   char		*name_ptr;
   int		underscores = 0;

   TRACE (Func_Entry, "make_external_name", NULL);

   if (!AT_IS_INTRIN(attr_idx)) {
      name_ptr = (char *)&name_pool[name_idx];

      if (!on_off_flags.upper_case_names) {
         for (i = 0;  i < name_len;  i++) {
            TOKEN_STR(ext_token)[i] = tolower(name_ptr[i]);
	    if (name_ptr[i] == '_') {
	      underscores++;
	    }
         }

#ifdef KEY /* Bug 6204 */
         i = name_len = decorate(TOKEN_STR(ext_token), name_len, underscores);
#else /* KEY Bug 6204 */
         if (on_off_flags.underscoring) {
	    TOKEN_STR(ext_token)[i++] = '_';
	    name_len++;
            if (on_off_flags.second_underscore && (underscores > 0)) {
	       TOKEN_STR(ext_token)[i++] = '_';
	       name_len++;
            }
         }
#endif /* KEY Bug 6204 */

         TOKEN_STR(ext_token)[i] = '\0';
         NTR_NAME_POOL(TOKEN_ID(ext_token).words, name_len, name_idx);
      }
   }
#ifdef KEY /* Bug 5089 */
   /* F2003 allows an intrinsic module and a non-intrinsic module to have
    * the same user-visible ID, and to be used in the same compilation (though
    * not to be used in the same program unit.) Thus an intrinsic module must
    * have a different linker-visible ID to avoid collision. We choose to
    * prepend an underscore and uppercase the first letter, thus staying out
    * of the normal Fortran-generated namespace and the user-level C
    * namespace as well. */
   else if (Pgm_Unit == AT_OBJ_CLASS(attr_idx) &&
     Module == ATP_PGM_UNIT(attr_idx)) {
     name_ptr = (char *)&name_pool[name_idx];
     TOKEN_STR(ext_token)[0] = '_';
     TOKEN_STR(ext_token)[1] = toupper(name_ptr[0]);
     for (i = 1; i < name_len; i += 1) {
       TOKEN_STR(ext_token)[i + 1] = tolower(name_ptr[i]);
     }
     TOKEN_STR(ext_token)[name_len += 1] = '\0';
     NTR_NAME_POOL(TOKEN_ID(ext_token).words, name_len, name_idx);
   }
#endif /* KEY Bug 5089 */

   ATP_EXT_NAME_IDX(attr_idx) = name_idx;
   ATP_EXT_NAME_LEN(attr_idx) = name_len;


   TRACE (Func_Exit, "make_external_name", NULL);

   return;

}  /* make_external_name */
# endif

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create the hidden name table for this scope.			      *|
|*									      *|
|* Input parameters:							      *|
|*	scp_idx		-> Scope that needs hidden name table created in.     *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void	create_hidden_name_tbl(int	scp_idx)
{
   int		hn_idx;


   TRACE (Func_Entry, "create_hidden_name_tbl", NULL);

   /* Need to create a hidden name table for compressing out attr entries */

   hn_idx				= hidden_name_tbl_idx + 1;

   TBL_REALLOC_CK(hidden_name_tbl, 2);
   CLEAR_TBL_NTRY(hidden_name_tbl, hn_idx);

   HN_NAME_IDX(hn_idx)      		= NAME_POOL_ZERO_IDX; /* Zero word */
   HN_NAME_LEN(hn_idx)			= HOST_BYTES_PER_WORD;
   SCP_HN_FW_IDX(scp_idx)		= hn_idx;

   CLEAR_TBL_NTRY(hidden_name_tbl, hidden_name_tbl_idx);
   HN_NAME_IDX(hidden_name_tbl_idx)	= NAME_POOL_ONES_IDX; /* Ones word   */
   HN_NAME_LEN(hidden_name_tbl_idx)	= HOST_BYTES_PER_WORD;
   SCP_HN_LW_IDX(scp_idx)		= hidden_name_tbl_idx;

   TRACE (Func_Exit, "create_hidden_name_tbl", NULL);

   return;

}   /* create_hidden_name_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Remove the hidden name table for this scope.			      *|
|*									      *|
|* Input parameters:							      *|
|*	scp_idx		-> Scope that needs hidden name table create in.      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void	remove_hidden_name_tbl(int	scp_idx)
{

   TRACE (Func_Entry, "remove_hidden_name_tbl", NULL);

   /* Remove hidden name table - We're done with it.  It should        */
   /* always be at the end.  This is a safety check.  If for some      */
   /* reason it is not, everything will work, but will compile slower. */

   if (hidden_name_tbl_idx == SCP_HN_LW_IDX(scp_idx)) {
      hidden_name_tbl_idx	= SCP_HN_FW_IDX(scp_idx) - 1;
   }

   SCP_HN_FW_IDX(scp_idx)	= NULL_IDX;
   SCP_HN_LW_IDX(scp_idx)	= NULL_IDX;

   TRACE (Func_Exit, "remove_hidden_name_tbl", NULL);

   return;

}   /* remove_hidden_name_tbl */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on PVP machines.  It determines     *|
|*      whether or not the bit pattern passed to it in "constant" is a valid  *|
|*      form for a non-IEEE PVP machine.				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      type_idx : the data type index for the constant			      *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if the argument is "normal".				      *|
|*                                                                            *|
\******************************************************************************/

static boolean pvp_isnormal(int		 type_idx,
             	            long_type	*constant)
{
   long_type	mask;
   boolean	result;


   TRACE (Func_Entry, "pvp_isnormal", NULL);

   mask = AR_status((AR_DATA *)       constant,
                    (const AR_TYPE *) &linear_to_arith[TYP_LINEAR(type_idx)]);

   if (mask & AR_STAT_UNDERFLOW != 0  ||  mask & AR_STAT_OVERFLOW != 0) {
      result = FALSE;
   }
   else {
      result = TRUE;
   }

   TRACE (Func_Exit, "pvp_isnormal", NULL);
         
   return(result);

}  /* pvp_isnormal */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the driver *|
|*      for the C "isnormal" macro imitation.				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      type_idx : the data type index for the constant			      *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE (1) if the argument is "normal".				      *|
|*                                                                            *|
\******************************************************************************/

static boolean is_normal(int		 type_idx,
             	         long_type	*constant)
{
   int		const_bit_len;
#ifdef KEY /* Bug 10177 */
   boolean	result = FALSE;
#else /* KEY Bug 10177 */
   boolean	result; 
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "is_normal", NULL);

   const_bit_len = storage_bit_size_tbl[TYP_LINEAR(type_idx)];

   switch (const_bit_len) {

      case 32:
         result = (is_normal_32(constant) == 1) ? TRUE : FALSE;
         break;

      case 64:
         result = (is_normal_64(type_idx, constant) == 1) ? TRUE : FALSE;
         break;

      case 128:
         result = (is_normal_128(type_idx, constant) == 1) ? TRUE : FALSE;
   }

   TRACE (Func_Exit, "is_normal", NULL);
         
   return(result);

}  /* is_normal */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the 32-bit *|
|*      implementation of the C "isnormal" macro.			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE (1) if the argument is "normal".				      *|
|*                                                                            *|
\******************************************************************************/

static int is_normal_32(long_type	*constant)
{
   ieee_real_4_type	f;


   f.integer_form = constant[0];

   return( ! IEEE_32_EXPO_ALLONES(f.parts.exponent)  &&
           (f.parts.exponent & IEEE_32_EXPONENT) != 0);

}  /* is_normal_32 */




/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the 64-bit *|
|*      implementation of the C "isnormal" macro.			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      type_idx : the data type index for the constant			      *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE (1) if the argument is "normal".				      *|
|*                                                                            *|
\******************************************************************************/

static int is_normal_64(int	 	 type_idx,
		        long_type	*constant)
{
   int  		const_word_len;
   ieee_real_8_type	f;
   int  		i;

   const_word_len =
         TARGET_BITS_TO_WORDS(storage_bit_size_tbl[TYP_LINEAR(type_idx)]);

   for (i = 0;  i < const_word_len;  ++i) {
      f.integer_array[i] = constant[i];
   }

   return( ! IEEE_64_EXPO_ALLONES(f.parts.exponent)  &&
           (f.parts.exponent & IEEE_64_EXPONENT) != 0);

}  /* is_normal_64 */




/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the 128-bit*|
|*      implementation of the C "isnormal" macro.			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      type_idx : the data type index for the constant			      *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE (1) if the argument is "normal".				      *|
|*                                                                            *|
\******************************************************************************/

static int is_normal_128(int		 type_idx,
			 long_type	*constant)
{
   int  		const_word_len;
   ieee_real_16_type	f;
   int  		i;

   const_word_len =
         TARGET_BITS_TO_WORDS(storage_bit_size_tbl[TYP_LINEAR(type_idx)]);

   for (i = 0;  i < const_word_len;  ++i) {
      f.integer_array[i] = constant[i];
   }

   return( ! IEEE_128_EXPO_ALLONES(f.parts.exponent)  &&
           (f.parts.exponent & IEEE_128_EXPO) != 0 );

}  /* is_normal_128 */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the driver *|
|*      for the C "signbit" macro imitation.				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      type_idx : the data type index for the constant			      *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      The sign bit.							      *|
|*                                                                            *|
\******************************************************************************/

static int sign_bit(int          type_idx,
                    long_type   *constant)
{
   int          const_bit_len;
#ifdef KEY /* Bug 10177 */
   int		result = FALSE;
#else /* KEY Bug 10177 */
   int		result;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "sign_bit", NULL);

   const_bit_len = storage_bit_size_tbl[TYP_LINEAR(type_idx)];

   switch (const_bit_len) {

      case 32:
         result = sign_bit_32(constant); 
         break;

      case 64:
         result = sign_bit_64(constant);
         break;

      case 128:
         result = sign_bit_128(constant);
   }

   TRACE (Func_Exit, "sign_bit", NULL);

   return(result);

}  /* sign_bit */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the 32-bit *|
|*      implementation of the C "signbit" macro.                              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      The sign bit (1 or 0).						      *|
|*                                                                            *|
\******************************************************************************/

static int sign_bit_32(long_type	*constant)
{
   ieee_real_4_type	f;


   f.integer_form = constant[0];

   return(f.parts.sign);

}  /* sign_bit_32 */




/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the 64-bit *|
|*      implementation of the C "signbit" macro.                              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      The sign bit (1 or 0).						      *|
|*                                                                            *|
\******************************************************************************/

static int sign_bit_64(long_type	*constant)
{
   ieee_real_8_type	f;


   f.integer_array[0] = constant[0];

   return(f.parts.sign);

}  /* sign_bit_64 */




/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the 128-bit*|
|*      implementation of the C "signbit" macro.                              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      The sign bit (1 or 0).						      *|
|*                                                                            *|
\******************************************************************************/

static int sign_bit_128(long_type	*constant) 
{
   ieee_real_16_type	f;


   f.integer_array[0] = constant[0];

   return(f.parts.sign);

}  /* sign_bit_128 */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the driver *|
|*      for the C "fpclassify" macro imitation.                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      type_idx : the data type index for the constant			      *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      An int indicating the class of the constant.			      *|
|*                                                                            *|
\******************************************************************************/

static int fp_classify(int	 	 type_idx,
 		      long_type 	*constant)
{
#ifdef KEY /* Bug 10177 */
   int  class = 0;
#else /* KEY Bug 10177 */
   int  class;
#endif /* KEY Bug 10177 */
   int  const_bit_len;


   TRACE (Func_Entry, "fp_classify", NULL);

   const_bit_len = storage_bit_size_tbl[TYP_LINEAR(type_idx)];

   switch (const_bit_len) {

      case 32:
         class = fp_classify_32(constant);
         break;

      case 64:
         class = fp_classify_64(type_idx, constant);
         break;

      case 128:
         class = fp_classify_128(type_idx, constant);
   }

   TRACE (Func_Exit, "fp_classify", NULL);

   return(class);

}  /* fp_classify */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the 32-bit *|
|*      implementation of the C "fpclassify" macro.                           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      An int indicating the class of the constant.			      *|
|*                                                                            *|
\******************************************************************************/

static int fp_classify_32(long_type	*constant)
{

   ieee_real_4_type	f;


   f.integer_form = constant[0];

   if (f.parts.exponent == 0) {

      if (f.parts.mantissa == 0) {
         return(FP_SGI_ZERO);
      }
      else {
         return(FP_SGI_SUBNORMAL);
      }
   }
   else if (f.parts.exponent == IEEE_32_EXPONENT) {

      if (f.parts.mantissa == 0) {
         return(FP_SGI_INFINITE);
      }
      else {
         return(FP_SGI_NAN);
      }
   }
   else {
      return(FP_SGI_NORMAL);
   }

}  /* fp_classify_32 */




/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the 64-bit *|
|*      implementation of the C "fpclassify" macro.                           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      type_idx : the data type index for the constant			      *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      An int indicating the class of the constant.			      *|
|*                                                                            *|
\******************************************************************************/

static int fp_classify_64(int		 type_idx,
			 long_type	*constant)
{
   int  		const_word_len;
   ieee_real_8_type	f;
   int  		i;

   const_word_len =
         TARGET_BITS_TO_WORDS(storage_bit_size_tbl[TYP_LINEAR(type_idx)]);

   for (i = 0;  i < const_word_len;  ++i) {
      f.integer_array[i] = constant[i];
   }

   if (f.parts.exponent == 0) {

      if (f.parts.mantissa_u == 0  &&  f.parts.mantissa_l == 0) {
         return(FP_SGI_ZERO);
      }
      else {
         return (FP_SGI_SUBNORMAL);
      }
   }
   else if (f.parts.exponent == IEEE_64_EXPONENT) {

      if (f.parts.mantissa_u == 0  &&  f.parts.mantissa_l == 0) {
         return(FP_SGI_INFINITE);
      }
      else {
         return(FP_SGI_NAN);
      }
   }
   else {
      return (FP_SGI_NORMAL);
   }

}  /* fp_classify_64 */




/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is only referenced on IEEE machines.  It is the 128-bit*|
|*      implementation of the C "fpclassify" macro.                           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      type_idx : the data type index for the constant			      *|
|*      constant : the constant						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      An int indicating the class of the constant.			      *|
|*                                                                            *|
\******************************************************************************/

static int fp_classify_128(int		 type_idx,
	 		   long_type	*constant)
{
   int  		const_word_len;
   ieee_real_16_type	f;
   int  		i;

   const_word_len =
         TARGET_BITS_TO_WORDS(storage_bit_size_tbl[TYP_LINEAR(type_idx)]);

   for (i = 0;  i < const_word_len;  ++i) {
      f.integer_array[i] = constant[i];
   }

   if (f.parts.exponent == 0) {

      if (f.parts.mantissa_u1 == 0  &&  f.parts.mantissa_u2 == 0  &&
          f.parts.mantissa_l1 == 0  &&  f.parts.mantissa_l2 == 0) {
         return(FP_SGI_ZERO);
      }
      else {
         return (FP_SGI_SUBNORMAL);
      }
   }
   else if (f.parts.exponent == IEEE_128_EXPO) {

      if (f.parts.mantissa_u1 == 0  &&  f.parts.mantissa_u2 == 0  &&
          f.parts.mantissa_l1 == 0  &&  f.parts.mantissa_l2 == 0) {
         return(FP_SGI_INFINITE);
      }
      else {
         return(FP_SGI_NAN);
      }
   }
   else {
      return (FP_SGI_NORMAL);
   }

}  /* fp_classify_128 */
