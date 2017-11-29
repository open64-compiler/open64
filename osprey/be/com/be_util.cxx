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
 * Module: be_util.c
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:34-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.be_util.cxx $
 *
 * Revision history:
 *  06-Dec -95 - Original Version
 *
 * Description:
 *    Utilities for all backend components.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "wn.h"
#include "be_util.h"

static INT32 current_pu_count;

void
Reset_Current_PU_Count(void)
{
  current_pu_count = 0;
}

void
Advance_Current_PU_Count(void)
{
  current_pu_count++;
}

INT32
Current_PU_Count(void)
{
  return current_pu_count;
}

//-----------------------------------------------------------------------
// NAME: St_Idx_Is_Intconst
// FUNCTION: Returns TRUE if 'st_idx' is an integer constant and, when this
//   is the case, sets the value in 'val'.  Otherwise, returns FALSE.
//-----------------------------------------------------------------------

extern BOOL St_Idx_Is_Intconst(ST_IDX st_idx, INT64 *val)
{
  ST* st = &St_Table[st_idx]; 
  if (ST_class(st)==CLASS_CONST) {
    TCON t = STC_val(st);
    switch(TCON_ty(t)) {
      case MTYPE_I1: case MTYPE_I2 : case MTYPE_I4: case MTYPE_I8 :
        *val = t.vals.i0;
        return TRUE;
      case MTYPE_U1: case MTYPE_U2 : case MTYPE_U4: case MTYPE_U8 :
        *val = t.vals.k0;
        return TRUE;
      default:
        return FALSE;
    }
  } else {
    return FALSE;
  }
}

//-----------------------------------------------------------------------
// NAME: Wn_Is_Intconst
// FUNCTION: Returns TRUE if 'ldid' is an integer constant and, when this
//   is the case, sets the value in 'val'.  Otherwise, returns FALSE.
//-----------------------------------------------------------------------

extern BOOL Wn_Is_Intconst(WN *ldid, INT64 *val)
{
  return St_Idx_Is_Intconst(ST_st_idx(WN_st(ldid)), val);
} 

// ----------------------------------------------------------------------
// symbols defined in be.so but used in cg.so
/* official builds will link with identfile that defines _Release_ID */
extern const char *__Release_ID;
const char *Default_Release_ID = "none";
#if defined(SHARED_BUILD)
#if defined(BUILD_OS_DARWIN)
#pragma weak __Release_ID
const char *__Release_ID = "none";
#else /* defined(BUILD_OS_DARWIN) */
#pragma weak __Release_ID = Default_Release_ID
#endif /* defined(BUILD_OS_DARWIN) */
#endif

