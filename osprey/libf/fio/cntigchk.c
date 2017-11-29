/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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



#pragma ident "@(#) libf/fio/cntigchk.c	92.1	06/18/99 10:21:14"

#include <liberrno.h>
#include <stdlib.h>
#include <string.h>
#include <cray/nassert.h>
#include <cray/dopevec.h>
#include "fio.h"

/*
 *      _cntig_chk - called by unit and format library code to:
 *                   1) check for a contiguous array,
 *                   2) calculate the extent of the array (number of elts),
 *                   3) calculate the number of bytes of the array,
 *                   4) allocate a new array if input array is not
 *                      contiguous and move the noncontiguous array to
 *                      the contiguous array.
 *      Synopsis
 *              int _cntig_chk( DopeVectorType *dv,
 *                              void **newar,
 *                              int *nocontig,
 *                              long *extent,
 *                              long *nbytes);
 *              Where
 *              	dv	- pointer to input dope vector
 *              	newar	- output pointer to new array
 *              	nocontig- address of flag for contiguous array
 *              	extent	- address of extent of array
 *              	nbytes	- address of array size in bytes
 *      Return value
 *              zero	- no errors
 *              nonzero	- error code
 */

int
_cntig_chk(DopeVectorType *dv,
	   void **newar,
	   int *nocontig,
	   long *extent,
	   long *nbytes)
{
	long	*base;			/* pointer to new area */
	char	*cbase;			/* pointer to new area */
	long	extnt = 1;		/* extent of input array */
	int	i;
	long	k;
	int	type;
	long	nbyts;			/* element size in bytes */
	int	notcontig = 0;		/* contiguous flag */
	int	nd;
	long	extente;
	long	elsize;			/* element size */
	long	id2, id3,id4;
	long	id5, id6, id7;
	int	badjust;
	long	*addr;			/* for word-oriented data */
	char	*baddr;			/* for byte-oriented data */
	void	*addr2, *addr3, *addr4;
	void	*addr5, *addr6;

	type = dv->type_lens.type;
	nd = dv->n_dim;

	/* set element byte length and stride length */
	if (dv->type_lens.type == DVTYPE_ASCII) {
		nbyts = _fcdlen (dv->base_addr.charptr);
		elsize = nbyts;
	}
	else {
		nbyts = dv->type_lens.int_len >> 3;
		elsize = nbyts >> 3;
	}

	/*
	 * check for contiguous array
	 */
	for (i=0; i < nd; i++) {
		long st;
		long ex;
		extnt *= dv->dimension[i].extent;
		st = dv->dimension[i].stride_mult;
		ex = dv->dimension[i].extent;
		/* ensure all but last dim are contiguous */
		if ((i < nd-1) && ((st * ex) !=
		    dv->dimension[i+1].stride_mult))
			notcontig = 1;
		/* ensure last dim is contiguous */
		else if ((i == nd-1) && (st != elsize))
			notcontig = 1;
	}

	*nocontig = notcontig;
	*extent = extnt;

	/* calculate size of array in bytes */
	nbyts *= extnt;
	*nbytes = nbyts;

	if (notcontig == 0) {
		*newar = NULL;
		return(0);
	}

	/* clear base address */
	base = (void *) NULL;
	*newar = base;

	/* Allocate size in bytes if not zero.  Zero size is
	 * legal and should not cause an error.
	 */
	if (nbyts != 0) {
		base = (void *) malloc (nbyts);

		/* if no memory assigned, error */
		if (base == NULL) {
			return(FENOMEMY);
		/* set fields for null array as well */
		}
	}
	*newar = base;

	/* if extent is zero, return */
	if (nbyts == 0)
		return(0);

	/* Move noncontiguous array to contiguous array. */
	badjust = 0;
	extente = dv->dimension[0].extent;
	if (type == DVTYPE_ASCII) {
		char 	*ba;
		cbase = (char *) base;
		baddr = _fcdtocp(dv->base_addr.charptr) +
			badjust * (dv->type_lens.int_len >> 3);

		switch(nd) {
		case 7:
			for (id7=0; id7<dv->dimension[6].extent; id7++) {
				addr6 = baddr;
		case 6:
			 for (id6=0; id6<dv->dimension[5].extent; id6++) {
				addr5 = baddr;
		case 5:
			  for (id5=0; id5<dv->dimension[4].extent; id5++) {
				addr4 = baddr;
		case 4:
			   for (id4=0; id4<dv->dimension[3].extent; id4++) {
				addr3 = baddr;
		case 3:
			    for (id3=0; id3<dv->dimension[2].extent; id3++) {
				addr2 = baddr;
		case 2:
			     for (id2=0; id2<dv->dimension[1].extent; id2++) {
		case 1:
				ba = baddr;
				for (i=0; i<extente; i++) {
					(void *) memcpy (cbase,ba,elsize);
					cbase += elsize;
					ba += dv->dimension[0].stride_mult;
				}
				if (nd == 1) goto done;
				baddr += dv->dimension[1].stride_mult;
			     }
			    if (nd == 2) goto done;
			    baddr  = addr2;
			    baddr += dv->dimension[2].stride_mult;
			    }
			   if (nd == 3) goto done;
			   baddr  = addr3;
			   baddr += dv->dimension[3].stride_mult;
			   }
			  if (nd == 4) goto done;
			  baddr  = addr4;
			  baddr += dv->dimension[4].stride_mult;
		          }
		         if (nd == 5) goto done;
		         baddr  = addr5;
		         baddr += dv->dimension[5].stride_mult;
		         }
		        if (nd == 6) goto done;
		        baddr  = addr6;
		        baddr += dv->dimension[6].stride_mult;
		        }
		}
	}
	else {                          /* word-oriented data */
		long 	*ba;
		k	= 0;
		addr = (long*)dv->base_addr.a.ptr + badjust;

		switch(nd) {
		case 7:
			for (id7=0; id7<dv->dimension[6].extent; id7++) {
				addr6 = addr;
		case 6:
			 for (id6=0; id6<dv->dimension[5].extent; id6++) {
				addr5 = addr;
		case 5:
			  for (id5=0; id5<dv->dimension[4].extent; id5++) {
				addr4 = addr;
		case 4:
			   for (id4=0; id4<dv->dimension[3].extent; id4++) {
				addr3 = addr;
		case 3:
			    for (id3=0; id3<dv->dimension[2].extent; id3++) {
				addr2 = addr;
		case 2:
			     for (id2=0; id2<dv->dimension[1].extent; id2++) {
		case 1:
				ba = addr;
				for (i=0; i<extente; i++) {
				/* noncharacter single word entities assumed */
					base[k] = ba[0];
					k++;
					ba += dv->dimension[0].stride_mult;
				}
				if (nd == 1) goto done;
				addr += dv->dimension[1].stride_mult;
		 	     }
			    if (nd == 2) goto done;
			    addr  = addr2;
			    addr += dv->dimension[2].stride_mult;
			    }
			   if (nd == 3) goto done;
			   addr  = addr3;
			   addr += dv->dimension[3].stride_mult;
			   }
			  if (nd == 4) goto done;
			  addr  = addr4;
			  addr += dv->dimension[4].stride_mult;
			  }
			 if (nd == 5) goto done;
			 addr  = addr5;
			 addr += dv->dimension[5].stride_mult;
			 }
			if (nd == 6) goto done;
			addr  = addr6;
			addr += dv->dimension[6].stride_mult;
			}
		}
	}
done:	return(0);
}
/*
 *      _unpack_arry  - called by library routines to move a contiguous
 *                      array to a noncontiguous array.
 *      Synopsis
 *              int _unpak_arry(DopeVectorType *dvc,
 *                              DopeVectorType *dvnc);
 *              Where
 *                      dvc	- pointer to the noncontiguous dope vector.
 *                      dvnc	- Pointer to the contiguous dope vector.
 */
int
_unpack_arry(void *dvc, DopeVectorType *dvnc)
{
	long	* restrict base;	/* pointer to new area */
	char 	* restrict cbase;	/* pointer to new area */
	long	extnt = 1;		/* extent of noncontig array */
	int	i;
	long	k;
	int	type;
	long	elsize;			/* element size of data type */
	int	nd;
	long	extente;
	long	id2, id3,id4;
	long	id5, id6, id7;
	int	badjust;
	long	*addr;			/* for word-oriented data */
	char	*baddr;			/* for byte-oriented data */
	void	*addr2, *addr3, *addr4;
	void	*addr5, *addr6;

	type = dvnc->type_lens.type;
	nd = dvnc->n_dim;

	/* set bytalign flag, element byte length, and stride length */
	if (dvnc->type_lens.type == DVTYPE_ASCII)
		elsize = _fcdlen (dvnc->base_addr.charptr);
	else
		elsize = dvnc->type_lens.int_len >> 6;

	/* determine extent of array */
	for (i=0; i < nd; i++)
		extnt *= dvnc->dimension[i].extent;

	/* Move noncontiguous array to contiguous array. */
	extente = dvnc->dimension[0].extent;
	base = dvc;
	cbase = (char *) base;
	badjust = 0;
	if (type == DVTYPE_ASCII) {
		char *ba;
		baddr = _fcdtocp(dvnc->base_addr.charptr) +
			badjust * (dvnc->type_lens.int_len >> 3);

		switch(nd) {
		case 7:
			for (id7=0; id7<dvnc->dimension[6].extent; id7++) {
				addr6 = baddr;
		case 6:
			 for (id6=0; id6<dvnc->dimension[5].extent; id6++) {
				addr5 = baddr;
		case 5:
			  for (id5=0; id5<dvnc->dimension[4].extent; id5++) {
				addr4 = baddr;
		case 4:
			   for (id4=0; id4<dvnc->dimension[3].extent; id4++) {
				addr3 = baddr;
		case 3:
			    for (id3=0; id3<dvnc->dimension[2].extent; id3++) {
				addr2 = baddr;
		case 2:
			     for (id2=0; id2<dvnc->dimension[1].extent; id2++) {
		case 1:
				ba = baddr;
				for (i=0; i<extente; i++) {
					(void *) memcpy (ba,cbase,elsize);
					cbase += elsize;
					ba += dvnc->dimension[0].stride_mult;
					}
				if (nd == 1) goto done;
				baddr += dvnc->dimension[1].stride_mult;
			     }
			    if (nd == 2) goto done;
			    baddr  = addr2;
			    baddr += dvnc->dimension[2].stride_mult;
			    }
			   if (nd == 3) goto done;
			   baddr  = addr3;
			   baddr += dvnc->dimension[3].stride_mult;
			   }
			  if (nd == 4) goto done;
			  baddr  = addr4;
			  baddr += dvnc->dimension[4].stride_mult;
		          }
		         if (nd == 5) goto done;
		         baddr  = addr5;
		         baddr += dvnc->dimension[5].stride_mult;
		         }
		        if (nd == 6) goto done;
		        baddr  = addr6;
		        baddr += dvnc->dimension[6].stride_mult;
		        }
		}
	}
	else {                          /* word-oriented data */
		long *ba;
		k	= 0;
		addr = (long*)dvnc->base_addr.a.ptr + badjust;

		switch(nd) {
		case 7:
			for (id7=0; id7<dvnc->dimension[6].extent; id7++) {
				addr6 = addr;
		case 6:
			 for (id6=0; id6<dvnc->dimension[5].extent; id6++) {
				addr5 = addr;
		case 5:
			  for (id5=0; id5<dvnc->dimension[4].extent; id5++) {
				addr4 = addr;
		case 4:
			   for (id4=0; id4<dvnc->dimension[3].extent; id4++) {
				addr3 = addr;
		case 3:
			    for (id3=0; id3<dvnc->dimension[2].extent; id3++) {
				addr2 = addr;
		case 2:
			     for (id2=0; id2<dvnc->dimension[1].extent; id2++) {
		case 1:
				ba = addr;
				for (i=0; i<extente; i++) {
					ba[0] = base[k];
					k++;
					ba += dvnc->dimension[0].stride_mult;
				}
				if (nd == 1) goto done;
				addr += dvnc->dimension[1].stride_mult;
		 	     }
			    if (nd == 2) goto done;
			    addr  = addr2;
			    addr += dvnc->dimension[2].stride_mult;
			    }
			   if (nd == 3) goto done;
			   addr  = addr3;
			   addr += dvnc->dimension[3].stride_mult;
			   }
			  if (nd == 4) goto done;
			  addr  = addr4;
			  addr += dvnc->dimension[4].stride_mult;
			  }
			 if (nd == 5) goto done;
			 addr  = addr5;
			 addr += dvnc->dimension[5].stride_mult;
			 }
			if (nd == 6) goto done;
			addr  = addr6;
			addr += dvnc->dimension[6].stride_mult;
			}
		}
	}
done:	return(0);
}
