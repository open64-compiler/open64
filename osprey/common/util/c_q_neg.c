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
 *  Module: c_q_neg.c
 *  $Revision$
 *  $Date$
 *  $Author$
 *  $Source$
 *
 * =======================================================================
 * =======================================================================
 */


#include "defs.h"
#include "quad.h"

/* quad unary minus */

#if defined(BUILD_OS_DARWIN)
/* Can't use "pragma weak" to create aliases in Mach-O */
QUAD c_q_neg(QUAD x, INT *p_err );
QUAD __c_q_neg(QUAD x, INT *p_err ) { return c_q_neg(x, p_err); }
#else /* defined(BUILD_OS_DARWIN) */
extern QUAD c_q_neg(QUAD, INT *);
#pragma weak c_q_neg = __c_q_neg
#define	c_q_neg __c_q_neg
#endif /* defined(BUILD_OS_DARWIN) */

QUAD
c_q_neg(QUAD x, INT *p_err )
{
QUAD	result;

	*p_err = 0;

	result.hi = -(x.hi);
	result.lo = -(x.lo);

	return ( result );
}

