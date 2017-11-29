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


/* =======================================================================
 * =======================================================================
 *
 *  Module: cgprep.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:23-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cgprep.h $
 *
 *  Revision comments:
 *
 *  4-Apr-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Utilities for loop and dep-graph code.
 *
 *  Interface:
 *
 *	void CGPREP_Init_Op(OP *op)
 *	  Initialize CGPREP data structures for newly-created <op>.
 *
 *	INT16 CGPREP_Same_Res_Opnd(OP *op)
 *	  Requires: OPER_same_res(Operator(OP_code(op))) 
 *		    (currently means <op> is select or unaligned ld)
 *	  Return the operand number for the operand of <op> that can
 *	  be the same as the result, or -1 if no operand qualifies.
 *
 *	TN *CGPREP_Dup_TN(TN *old_tn)
 *	  Return a TN equivalent to <old_tn>, except that it has a new
 *	  TN_number.  This works even for dedicated TNs, and differs from
 *	  Dup_TN_Even_If_Dedicated because it doesn't reset the flag
 *	  for TN_is_dedicated or the register assignment for dedicated TNs.
 *
 *	void CGPREP_Copy_TN(TN *dest, TN *src, OP *point, UINT8 omega,
 *			    BOOL before)
 *	  Insert a copy from <src>[<omega>] to <dest>, either just
 *	  before <point> if <before> is TRUE, or just after <point>
 *	  otherwise.
 *
 *	void CGPREP_Copy_TN_Into_BB(TN *dest, TN *src, BB *bb, OP *point,
 *				    UINT8 omega, BOOL before)
 *	  Insert a copy from <src>[<omega>] to <dest>.
 *        Arguments are the same as in CGPREP_Copy_TN with the addition of 
 *        BB *bb. This can be used in cases where there may be no OPs
 *        in the BB yet.
 *
 * =======================================================================
 * =======================================================================
 */

#ifndef CGPREP_INCLUDED
#define CGPREP_INCLUDED

#include "tn.h"
#include "op.h"

void CGPREP_Copy_TN(TN *dest, TN *src, OP *point, UINT8 omega, BOOL before);
void CGPREP_Copy_TN_Into_BB(TN *dest, TN *src, BB *bb, OP *point, UINT8 omega, BOOL before);

INT16 CGPREP_Same_Res_Opnd(OP *op);

void CGPREP_Init_Op(OP *op);

TN * CGPREP_Dup_TN(TN *old_tn);

#endif /* CGPREP_INCLUDED */
