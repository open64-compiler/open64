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
 *  Module: c_q_rel.c
 *  $Revision$
 *  $Date$
 *  $Author$
 *  $Source$
 *
 * =======================================================================
 * =======================================================================
 */

static char *source_file = __FILE__;
static char *rcs_id = "$Source$ $Revision$";


#include "defs.h"
#include "quad.h"

extern INT c_q_lt(QUAD, QUAD, INT *);
#if defined(BUILD_OS_DARWIN)
/* Can't use "pragma weak" to create aliases in Mach-O */
extern INT __c_q_lt(QUAD x, QUAD y, INT *p_err) { return c_q_lt(x, y, p_err); }
#else /* defined(BUILD_OS_DARWIN) */
#pragma weak c_q_lt = __c_q_lt
#define	c_q_lt __c_q_lt
#endif /* defined(BUILD_OS_DARWIN) */

	/* implements the relational operator < for long doubles */

INT
c_q_lt(QUAD x, QUAD y, INT *p_err )
{
	*p_err = 0;

	if ( x.hi < y.hi )
		return ( 1 );

	if ( (x.hi == y.hi) && (x.lo < y.lo) )
		return ( 1 );

	return ( 0 );
}

extern INT c_q_le(QUAD, QUAD, INT *);
#if defined(BUILD_OS_DARWIN)
/* Can't use "pragma weak" to create aliases in Mach-O */
extern INT __c_q_le(QUAD x, QUAD y, INT *p_err) { return c_q_le(x, y, p_err); }
#else /* defined(BUILD_OS_DARWIN) */
#pragma weak c_q_le = __c_q_le
#define	c_q_le __c_q_le
#endif /* defined(BUILD_OS_DARWIN) */

	/* implements the relational operator <= for long doubles */

INT
c_q_le(QUAD x, QUAD y, INT *p_err )
{
	*p_err = 0;

	if ( x.hi < y.hi )
		return ( 1 );

	if ( (x.hi == y.hi) && (x.lo <= y.lo) )
		return ( 1 );

	return ( 0 );
}

extern INT c_q_eq(QUAD, QUAD, INT *);
#if defined(BUILD_OS_DARWIN)
/* Can't use "pragma weak" to create aliases in Mach-O */
extern INT __c_q_eq(QUAD x, QUAD y, INT *p_err) { return c_q_eq(x, y, p_err); }
#else /* defined(BUILD_OS_DARWIN) */
#pragma weak c_q_eq = __c_q_eq
#define	c_q_eq __c_q_eq
#endif /* defined(BUILD_OS_DARWIN) */

	/* implements the relational operator == for long doubles */

INT
c_q_eq(QUAD x, QUAD y, INT *p_err )
{
	*p_err = 0;

	if ( (x.hi == y.hi) && (x.lo == y.lo) )
		return ( 1 );

	return ( 0 );
}

extern INT c_q_ne(QUAD, QUAD, INT *);
#if defined(BUILD_OS_DARWIN)
/* Can't use "pragma weak" to create aliases in Mach-O */
extern INT __c_q_ne(QUAD x, QUAD y, INT *p_err) { return c_q_ne(x, y, p_err); }
#else /* defined(BUILD_OS_DARWIN) */
#pragma weak c_q_ne = __c_q_ne
#define	c_q_ne __c_q_ne
#endif /* defined(BUILD_OS_DARWIN) */

	/* implements the relational operator != for long doubles */

INT
c_q_ne(QUAD x, QUAD y, INT *p_err )
{
	*p_err = 0;

	if ( (x.hi != y.hi) || (x.lo != y.lo) )
		return ( 1 );

	return ( 0 );
}

extern INT c_q_gt(QUAD, QUAD, INT *);
#if defined(BUILD_OS_DARWIN)
/* Can't use "pragma weak" to create aliases in Mach-O */
extern INT __c_q_gt(QUAD x, QUAD y, INT *p_err) { return c_q_gt(x, y, p_err); }
#else /* defined(BUILD_OS_DARWIN) */
#pragma weak c_q_gt = __c_q_gt
#define	c_q_gt __c_q_gt
#endif /* defined(BUILD_OS_DARWIN) */

	/* implements the relational operator > for long doubles */

INT
c_q_gt(QUAD x, QUAD y, INT *p_err )
{
	*p_err = 0;

	if ( x.hi > y.hi )
		return ( 1 );

	if ( (x.hi == y.hi) && (x.lo > y.lo) )
		return ( 1 );

	return ( 0 );
}

extern INT c_q_ge(QUAD, QUAD, INT *);
#if defined(BUILD_OS_DARWIN)
/* Can't use "pragma weak" to create aliases in Mach-O */
extern INT __c_q_ge(QUAD x, QUAD y, INT *p_err) { return c_q_ge(x, y, p_err); }
#else /* defined(BUILD_OS_DARWIN) */
#pragma weak c_q_ge = __c_q_ge
#define	c_q_ge __c_q_ge
#endif /* defined(BUILD_OS_DARWIN) */

	/* implements the relational operator >= for long doubles */

INT
c_q_ge(QUAD x, QUAD y, INT *p_err )
{
	*p_err = 0;

	if ( x.hi > y.hi )
		return ( 1 );

	if ( (x.hi == y.hi) && (x.lo >= y.lo) )
		return ( 1 );

	return ( 0 );
}

