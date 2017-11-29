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


/* ====================================================================
 * ====================================================================
 *
 * Module: cwh_expr.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description:
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_EXPR_INCLUDED
#define CWH_EXPR_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

typedef enum {
  f_NONE = 0,
  f_T_PASSED=1,
  f_T_SAVED=2
} FLAG;

extern WN * cwh_expr_operand(WN **arrexp) ;
extern WN * cwh_expr_address(FLAG flag) ;
extern WN * cwh_expr_bincalc(OPERATOR op, WN * wn1, WN * wn2) ;
extern WN  *cwh_get_typed_operand(TYPE_ID ty, WN **arrexp);
extern OPCODE cwh_make_typed_opcode(OPERATOR op, TYPE_ID ty1, TYPE_ID ty2);
extern WN * cwh_convert_to_ty(WN * wn, TYPE_ID ty);
extern WN * cwh_wrap_cvtl(WN * wn, TYPE_ID ty);
extern TYPE_ID cwh_get_highest_type(WN *lhs, WN *rhs);
extern WN * cwh_expr_temp(TY_IDX ty, WN *sz, FLAG flag);
extern void cwh_expr_compare(OPERATOR op,TY_IDX  ty);
extern void cwh_expr_str_operand(W_node expr[2]);
extern void cwh_expr_set_flags(ST *st, FLAG flag);
extern WN * cwh_generate_bitmask(WN *len, TYPE_ID ty);
extern WN * cwh_expr_extract_arrayexp(WN *node, WN **arrayexp);
extern WN * cwh_expr_restore_arrayexp(WN *node, WN *arrayexp);
extern void cwh_expr_temp_set_pragma(ST *st) ;
extern WN * cwh_expr_dispose_of_char(WN * wn);


/* This causes extract_arrayexp to delete all arrayexps it sees if passed
 * to the arrayexp arg.
 */
#define DELETE_ARRAYEXP_WN ((WN **) 1)

    
#endif /* CWH_EXPR_INCLUDED */

