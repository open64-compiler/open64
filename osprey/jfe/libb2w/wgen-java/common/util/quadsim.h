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


#ifndef quadsim_INCLUDED
#define quadsim_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* =======================================================================
 * =======================================================================
 *
 *  Module: quadsim.h
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/quadsim.h,v $
 *
 * =======================================================================
 * =======================================================================
 */


extern INT32 __c_fp_class_q(QUAD x);
extern INT __c_q_to_a(char*, QUAD, INT*);
extern double __c_dble_q(QUAD, INT*);
extern float __c_sngl_q(QUAD, INT*);
extern INT32 __c_ji_qint(QUAD, INT*);
extern UINT32 __c_ji_quint(QUAD, INT*);
extern INT64 __c_ki_qint(QUAD, INT*);
extern UINT64 __c_ki_quint(QUAD, INT*);
extern QUAD __c_a_to_q(char*, INT*);
extern QUAD __c_q_extd(double, INT*);
extern QUAD __c_q_ext(float, INT *);
extern QUAD __c_q_flotj(INT32, INT*);
extern QUAD __c_q_flotju(UINT32, INT*);
extern QUAD __c_q_flotk(INT64, INT*);
extern QUAD __c_q_flotku(UINT64, INT*);
extern QUAD __c_q_add(QUAD, QUAD, INT*);
extern QUAD __c_q_sub(QUAD, QUAD, INT*);
extern QUAD __c_q_mul(QUAD, QUAD, INT*);
extern QUAD __c_q_div(QUAD, QUAD, INT*);
extern QUAD __c_q_sqrt(QUAD, INT*);
extern QUAD __c_q_neg(QUAD, INT*);
extern QUAD __c_q_trunc(QUAD, INT*);
extern INT __c_q_eq(QUAD, QUAD, INT*);
extern INT __c_q_ne(QUAD, QUAD, INT*);
extern INT __c_q_lt(QUAD, QUAD, INT*);
extern INT __c_q_le(QUAD, QUAD, INT*);
extern INT __c_q_gt(QUAD, QUAD, INT*);
extern INT __c_q_ge(QUAD, QUAD, INT*);

#ifdef __cplusplus
}
#endif
#endif /* quadsim_INCLUDED */
