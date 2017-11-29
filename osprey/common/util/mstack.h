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


#ifndef stack_INCLUDED
#define stack_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: stack.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:17:57 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/mstack.h,v $
 *
 * Revision history:
 *  14-Jun-93 - Original Version
 *
 * Description:  function prototypes for stack.c
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *stack_rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/mstack.h,v $ $Revision: 1.1 $";
#endif /* _KEEP_RCS_ID */


#if mips

extern int trace_stack ( int prfunc, int prfile );

#else
#if A_UX

extern int stack_lev ( int b );
extern int trace_stack ( int a, int b );

#else

extern int trace_stack ( int a, int b );

#endif /* A_UX */
#endif /* mips */

#ifdef __cplusplus
}
#endif
#endif /* stack_INCLUDED */
