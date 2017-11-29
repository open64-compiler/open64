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
 * Module: w2cf_parentize.c
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:42:00-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.w2cf_parentize.cxx $
 *
 * Revision history:
 *  10-Sept-96 - Original Version
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.w2cf_parentize.cxx $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */


#include "common_include.h" /* For defs.h, config.h, erglob.h, etc. */
#include "w2cf_parentize.h"


WN_MAP W2CF_Parent_Map = WN_MAP_UNDEFINED;


void 
W2CF_Parentize(const WN* wn)
{
   /* Given a tree, initialize its parent pointers.
    * Override what was there, if anything.
    * Do not update parent pointer of the root node 'wn'.
    * This is copied from be/lno/lwn_util.h!
    */
   if (!OPCODE_is_leaf (WN_opcode (wn)))
   { 
      if (WN_opcode(wn) == OPC_BLOCK)
      {
	 WN *kid = WN_first(wn);
	 while (kid) {
	    W2CF_Set_Parent(kid, wn);
	    W2CF_Parentize(kid);
	    kid = WN_next(kid);
	 }
      }
      else {
	 INT kidno;
	 WN *kid;
	 for (kidno=0; kidno < WN_kid_count(wn); kidno++) {
	    kid = WN_kid (wn, kidno);
	    if (kid) { 
	       W2CF_Set_Parent(kid, wn);
	       W2CF_Parentize(kid);
	    }
	 }
      }
   }
} /* W2FC_Parentize */
