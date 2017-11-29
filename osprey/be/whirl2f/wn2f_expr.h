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


#ifndef wn2f_expr_INCLUDED
#define wn2f_expr_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: wn2f_expr.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:43 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/wn2f_expr.h,v $
 *
 * Revision history:
 *    13-Apr-95 - Original Version
 *
 * Description:
 *
 * ====================================================================
 * ====================================================================
 */

extern void WN2F_Expr_initialize(void);
extern void WN2F_Expr_finalize(void);

extern WN2F_STATUS 
   WN2F_binaryop(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS
   WN2F_unaryop(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_intrinsic_op(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_tas(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_select(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_cvt(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_cvtl(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_realpart(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_imagpart(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_paren(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_complex(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS
   WN2F_ceil(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS
   WN2F_floor(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS
   WN2F_ashr(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_lshr(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_bnor(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_recip(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_rsqrt(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_madd(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_msub(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_nmadd(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_nmsub(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_const(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_intconst(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_eq(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_ne(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_parm(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
WN2F_alloca(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context) ;

extern WN2F_STATUS 
WN2F_dealloca(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context) ;

#endif /* wn2f_expr_INCLUDED */

