/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libF77/mvbits.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	actual or intended publication of such source code.	*/

#include "cmplrs/host.h"
#include "bit.h"

/* Move len bits from position i through i+len-1 of argument m to
 *		      position j through j+len-1 of argument n.
 * The right most bit is bit 0.
 */

void
mvbits_long_long(int64 *m,int64 *i,int64 *len,int64 *n,int64 *j)
{
        uint64 b;

	if ( (*i + *len > NBLL) || (*j + *len > NBLL) ||
	     (*len <= 0ll) || (*i < 0ll) || (*j < 0ll) ) return;

        b = (*m >> *i) & F77llmask[*len];         /* extract bits from src */
        *n &= ~(F77llmask[*j + *len] ^ F77llmask[*j]);      /* clear dest field */
	*n |= (b << *j);                        /* position bits and insert */
}
void
mvbits_long(int32 *m,int32 *i,int32 *len,int32 *n,int32 *j)
{
	uint32 b;

	if ( (*i + *len > NBI) || (*j + *len > NBI) ||
	     (*len <= 0) || (*i < 0) || (*j < 0) ) return;

	b = (*m >> *i) & F77mask[*len];		/* extract bits from src */
	*n &= ~(F77mask[*j + *len] ^ F77mask[*j]);	/* clear dest field */
	*n |= (b << *j);			/* position bits and insert */
}
void
mvbits_short(int16 *m,int16 *i,int16 *len,int16 *n,int16 *j)
{
	uint16 b;

	/* The following test is correct (NBI versus NBSI) for VAX compatibility PV130932 */
	if ( (*i + *len > NBI) || (*j + *len > NBI) ||
	     (*len <= 0) || (*i < 0) || (*j < 0) ) return;

	b = (*m >> *i) & F77mask[*len];		/* extract bits from src */
	*n &= ~(F77mask[*j + *len] ^ F77mask[*j]);	/* clear dest field */
	*n |= (b << *j);			/* position bits and insert */
}
void
mvbits_byte(int8 *m,int8 *i,int8 *len,int8 *n,int8 *j)
{
	uint8 b;

	/* The following test is correct (NBI versus NBB) for VAX compatibility PV130932 */
	if ( (*i + *len > NBI) || (*j + *len > NBI) ||
	     (*len <= 0) || (*i < 0) || (*j < 0) ) return;

	b = (*m >> *i) & F77mask[*len];		/* extract bits from src */
	*n &= ~(F77mask[*j + *len] ^ F77mask[*j]);	/* clear dest field */
	*n |= (b << *j);			/* position bits and insert */
}
