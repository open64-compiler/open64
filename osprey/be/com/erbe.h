/*
 * Copyright 2005-2007 NVIDIA Corporation.  All rights reserved.
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


#ifndef erbe_INCLUDED
#define erbe_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: erbe.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/com/erbe.h,v $
 *
 * Revision history:
 *  02-Nov-89 - Original Version
 *  01-Feb-91 - Copied for TP/Muse
 *
 * Description:
 *
 * Define the Muse back end error codes for use with the error message
 * handler errors.c.  The associated error descriptors may be found in
 * the file erbe.desc.
 *
 * See also ercg.h and ercg.desc for code generator error codes.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *erbe_rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/com/SCCS/s.erbe.h $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include "errors.h"	/* Always needed */

/* The error codes in each erxxx.h file should start from some multiple
 * of 1000, which is the phase number.
 */
#define EC_BASE_BE	EP_BE*1000

/* Global Live Range Analysis: */
#define EC_Alias_Interfere	EC_BASE_BE	/* stab, stab */
#define EC_Alias_Kill	EC_BASE_BE+1	/* stab, stab */
#define EC_Alias_Def	EC_BASE_BE+2	/* stab, stab, stab */
#define EC_Alias_Ref	EC_BASE_BE+3	/* stab, stab, stab */
#define EC_Glob_Exposed	EC_BASE_BE+4	/* sym */
#define EC_Not_Live_Out	EC_BASE_BE+5	/* sym */
#define EC_Not_Avail	EC_BASE_BE+6	/* sym */
#define EC_Part_Avail	EC_BASE_BE+7	/* sym */
#define EC_Not_Live_In	EC_BASE_BE+8	/* sym */

#define EC_Rtn_Conflict	EC_BASE_BE+10	/* str */
#define EC_Rtn_Uninit	EC_BASE_BE+11	/* str */
#define EC_Formal_Ref	EC_BASE_BE+12	/* str */
#define EC_Uninit_Ref	EC_BASE_BE+13	/* str, str */

/* Call processing: */
#define EC_Invalid_Call	EC_BASE_BE+30	/* str, str */
#define EC_Callee_TN	EC_BASE_BE+31	/* str, int */

/* FE input (btom, sgiops): */
#define EC_B_Synch	EC_BASE_BE+40	/* int, int, str */
#define EC_Inv_Offset	EC_BASE_BE+41	/* int, str */
#define EC_Inv_Flist	EC_BASE_BE+43	/* str, str, str */

/* Alias analysis: */
#define EC_Tree_Class	EC_BASE_BE+50	/* tn, int, node */
#define EC_Xar_Sclass	EC_BASE_BE+51	/* str, int, stab, str */
#define EC_Bad_MOS_Tree	EC_BASE_BE+52	/* node, int, str */
#define EC_Store_Const  EC_BASE_BE+53   /* none */


/* Control Flow Analysis and Optimization: */
#define EC_TO_Missed	EC_BASE_BE+60	/* none */
#define EC_Expanded	EC_BASE_BE+61	/* int, str */

/* RB support */
#define EC_Mult_RB_Reg  EC_BASE_BE+70	/* tn, int, int */
#define EC_Mult_RB_Tn   EC_BASE_BE+71	/* tn */

#define EC_Fold_Arith_Too_Big EC_BASE_BE+80  /* str */
#define EC_Fold_Arith_Too_Big2 EC_BASE_BE+81  /* int */
#define EC_Const_Copy_Too_Big EC_BASE_BE+82  /* str */
#define EC_Const_Copy_Too_Big2 EC_BASE_BE+83  /* int */
#define EC_Gopt_Too_Big EC_BASE_BE+84	/* str */
#define EC_Gopt_Too_Big2 EC_BASE_BE+85	/* int */
#define EC_Gopt_Space 	EC_BASE_BE+86	/* str */
#define EC_Gopt_Space_Skip EC_BASE_BE+87 /* str */

/* LNO error messages */
#define EC_LNO_Bad_Pragma_Int               EC_BASE_BE+90 /* str, int */
#define EC_LNO_Bad_Pragma_String            EC_BASE_BE+91 /* str, str */
#define EC_LNO_Bad_Pragma_Int_Advisory      EC_BASE_BE+92 /* str, int */
#define EC_LNO_Bad_Pragma_String_Advisory   EC_BASE_BE+93 /* str, str */
#define EC_LNO_Generic                      EC_BASE_BE+94 /* str, str */
#define EC_LNO_Generic_Error                EC_BASE_BE+95 /* str, str */
#define EC_LNO_Generic_Advisory             EC_BASE_BE+96 /* str, str */
#define EC_LNO_Generic2String               EC_BASE_BE+97 /* str, str, str */

#define EC_Bad_Pragma_Abort                 EC_BASE_BE+98 /* str, str, str */

/* MP lowerer error messages */
#define EC_MPLOWER_red_mult_use  EC_BASE_BE+110
#define EC_MPLOWER_red_not_found EC_BASE_BE+111
#define EC_MPLOWER_red_conflict  EC_BASE_BE+112
#define EC_MPLOWER_red_misuse    EC_BASE_BE+113
#define EC_MPLOWER_red_badop     EC_BASE_BE+114
#define EC_MPLOWER_shared_store  EC_BASE_BE+115
#define EC_MPLOWER_local_nosize  EC_BASE_BE+116
#define EC_MPLOWER_used_noscope  EC_BASE_BE+117
#define EC_MPLOWER_priv_equiv    EC_BASE_BE+118
#define EC_MPLOWER_defpriv_equiv EC_BASE_BE+119
#define EC_MPLOWER_first_last_priv EC_BASE_BE+120
#define EC_MPLOWER_reprivatization EC_BASE_BE+121
#define EC_MPLOWER_red_of_private  EC_BASE_BE+122
#define EC_MPLOWER_thrpriv_scope   EC_BASE_BE+123
#define EC_MPLOWER_Generic_Error EC_BASE_BE+124  /* str,str */
#define EC_MPLOWER_Generic_Warning EC_BASE_BE+125  /* str,str */

/* Data layout error messages */
#define EC_LAY_stack_limit	EC_BASE_BE+126
#define EC_LAY_section_name	EC_BASE_BE+127

/* Miscellaneous */
#define EC_Skip_PU		EC_BASE_BE+130	/* str, int, str */
#define EC_Uninitialized	EC_BASE_BE+131  /* str, str */

/* DRA cloning error messages */
#define EC_DRA_rii_file_io       EC_BASE_BE+140  /* str, err */
#define EC_DRA_rii_file_format   EC_BASE_BE+141  /* str, str */
#define EC_DRA_unsupported_type  EC_BASE_BE+142  /* str, str, str */
#define EC_DRA_all_stars         EC_BASE_BE+143  /* str */
#define EC_DRA_indirect_call     EC_BASE_BE+144  /* */
#define EC_DRA_clone_altentry    EC_BASE_BE+145  /* */
#define EC_DRA_bad_clone_request EC_BASE_BE+146  /* str, str */
#ifdef KEY
#define EC_MPLOWER_copyin_st     EC_BASE_BE+147
#endif
#ifdef TARG_NVISA
#define EC_No_Calls		 EC_BASE_BE+148
#define EC_Too_Many_Args	 EC_BASE_BE+149
#define EC_Const_Space_Overflow	 EC_BASE_BE+150
#define EC_Unaligned_Memory	 EC_BASE_BE+151
#define EC_Ptr_Assumed_Global	 EC_BASE_BE+152
#define EC_Load_Past_Struct	 EC_BASE_BE+153
#define EC_Shared_Atomic64_Opnd  EC_BASE_BE+154
#define EC_Memset_Too_Large      EC_BASE_BE+155 /* int */
#endif /* TARG_NVISA */

#define EC_Invalid_Asm_Constrain EC_BASE_BE+156 /* str */

#ifdef __cplusplus
}
#endif
#endif /* erbe_INCLUDED */
