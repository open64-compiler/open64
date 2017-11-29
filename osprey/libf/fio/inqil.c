/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/inqil.c	92.1	06/21/99 10:37:55"

#include <fortran.h>
#include <liberrno.h>
#include "fio.h"
#include "f90io.h"

/*
 *	Temporarily define infoflags structue.  This will be removed when
 *	it is added to f90io.h.
 */

typedef	struct InfoFlags {
    unsigned int	version :8;	/* contains InfoList Version */
    unsigned int		:24;	/* reserved for future development  */
    unsigned int		:8;	/* reserved for future development  */
    unsigned int	stksize	:8;	/* size in words of stack space */
					/* passed as 3rd arg to _INQIL */
    unsigned int		:8;	/* reserved for future development */
    unsigned int	icount	:8;	/* size of struct control list in */
					/* words */
} InfoFlagsType;

_f_int
_INQIL (InfoFlagsType	*info,
	iolist_header	*iolist,
	void		*stck)
{
	int		num_ioentries;
	int		f90_type;
	int		int_len;
	ioentry_header	*nextioh;
	void		*nexte;
	int		**indarray;
	long		elsize;
	int		i;
	int		*iptr;

/*	If first call, clear running total word in stack (word 1)	*/

	iptr = (int *) stck;
	if (iolist->iolfirst) {
	    iptr[0] = 0;
	}

	num_ioentries	= iolist->icount;
	nextioh		= (ioentry_header *) (iolist + 1);

/*
 *	Run through loop until all items have been processed.  A running
 *	total of bytes will be maintained during the loop.  This value will
 *	be the result returned to the calling program.
 */

	while (num_ioentries--) {
	    nexte = nextioh + 1;
	    switch (nextioh->valtype) {
		case IO_SCALAR :
		{
		    ioscalar_entry	*se;

/*
 *	All that is required for a scalar entry is to determine its length,
 *	and increment the running total.
 */
		    se = nexte;
		    f90_type = se->tinfo.type;
		    int_len = se->tinfo.int_len;

		    if (f90_type == DVTYPE_ASCII) {
			const int bytesperchar = 1;
			/*
			 * When 2-byte character is supported:
			 *
			 *  int bytesperchar = (int_len >> 3);
			 */
			elsize = _fcdlen (se->iovar_address.fcd);
			elsize *= bytesperchar;
		    } else {
			elsize = int_len >> 3;
#if	defined(_UNICOS)
/*
 *	Account for padding in PVP and MPP systems.  Padding will occur
 *	if the element size of the current element is greater than or equal
 *	to the word size of the machine, and the current count of bytes is
 *	not on a word boundary.
 */
			if (elsize >= sizeof (long)) {
			    i = iptr[0] & (sizeof(long) - 1);
			    if (i)
				iptr[0] += sizeof(long) - i;
			}
#endif
		    }
		    iptr[0] += elsize;
		    break;
		}

		case IO_DOPEVEC :
		{
		    ioarray_entry	*ae;

		    ae = nexte;
		    if (ae->dv->type_lens.type == DVTYPE_ASCII)
			elsize = _fcdlen (ae->dv->base_addr.charptr);
		    else {
			elsize = ae->dv->type_lens.int_len >> 3;

#if	defined(_UNICOS)
/*
 *	Account for padding in PVP and MPP systems.  Padding will occur
 *	if the element size of the current element is greater than or equal
 *	to the word size of the machine, and the current count of bytes is
 *	not on a word boundary.
 */
			if (elsize >= sizeof (long)) {
			    i = iptr[0] & (sizeof(long) - 1);
			    if (i)
				iptr[0] += sizeof(long) - i;
			}
#endif
		    }

/*
 *	If indflag is not set, multiply the extents together to determine
 *	the number of entries, and then multiply that number by the element
 *	size to get the total number of bytes.
 */
		    if (!ae->indflag) {
			for (i = 0; i < ae->dv->n_dim; i++)
			    elsize *= ae->dv->dimension[i].extent;

/*
 *	If indflag is set, multiply the strides of all indices whose
 *	corresponding entry in dovar is null.  Assume that all entries whose
 *	dovar equivalent is non_null are either 1, or will be calculated
 *	elsewhere (IO_LOOP).
 */
		    } else {
			indarray = ae->dovar;
			for (i = 0; i < ae->dv->n_dim; i++) {
			    if (indarray[i] == NULL)
				elsize *= ae->dv->dimension[i].extent;
			}
		    }
		    iptr[0] += elsize;
		    break;
		}

		case IO_LOOP :
		{
		    long		inc;
		    long		beg;
		    long		end;
		    long		ret;
		    long		tripcnt;
		    ioimplieddo_entry	*ie;
		    long		cntsave;

		    ie = nexte;
		    inc = *ie->ioinccnt;
		    beg = *ie->iobegcnt;
		    end = *ie->ioendcnt;

/*
 *	Determine number of times through loop will actually be done
 *	This number will be multiplied by the number of iterations for
 *	the remaining indices to get the total count.  The remaining
 *	indices will be determined by a recursive call to inqil.
 */

		    if (inc < 0) {
			beg = -beg;
			end = -end;
			inc = -inc;
		    }
		    tripcnt = (end + inc - beg) / inc;
		    if (tripcnt < 0)
			tripcnt = 0;
	
		    cntsave = iptr[0];
		    ret = _INQIL (info, (void *) (ie+1), stck);

/*
 *	Since the calculated size is added to the total by the _INQIL call,
 *	we need to multiply the returned count by the tripcnt - 1.
 */
		    iptr[0] += (ret - cntsave) * (tripcnt - 1);
		    break;
		}

		default :
		    _lerror (_LELVL_ABORT, FEINTUNK);
	    }

	    nextioh = (ioentry_header *) ((long *)nextioh + nextioh->ioentsize);
	}

	return (iptr[0]);
}
