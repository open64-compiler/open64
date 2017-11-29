/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/iochunk.c	92.2	06/21/99 10:37:55"

 
#include <liberrno.h>
#include <stdlib.h>
#include <cray/nassert.h>
#include "fio.h"
#include "f90io.h"

/*
 *	_iochunk
 *
 *		Perform I/O on all items in an I/O list.  func may be
 *		_ld_read, _rdfmt, _rdunf, _ld_write, _wrfmt, or _wrunf.
 *	
 *	Return Value:
 *
 *		0		normal return
 *		FEEORCND	if end of record condition (with ADVANCE='NO')
 *		other <0	if end of file condition
 *		>0		if error condition
 */


int
_iochunk(
	FIOSPTR		css,
	unit		*cup,
	xfer_func	*func,
	struct DvDimen	*dim,	
	type_packet	*tip,	/* Type information packet */
	short		nd,	/* number of dimensions (must be >= 1) */
	long		extent,	/* number of elements in first dimension */
	int		bshft,	/* see _stride_dv for details about bshft */
	bcont		*addr)	/* data pointer */

{
	register int	errn;
	register long	binc;	/* stride (in bytes) passed to gather/scatter */
	register long	dim2_ex;
	register long	dim1_pb;
	register long	dim1_sz;
	register long	i;
	register long	id3, id4, id5, id6, id7;
#ifdef KEY
	/* align lbuf on 64-bit boundary */
	long long	lbuf[CHBUFSIZE / sizeof(long long)];
#else
	long		lbuf[CHBUFSIZE / sizeof(long)];
#endif
	char		*lptr;
	bcont		*addr2, *addr3, *addr4, *addr5, *addr6;

	/* Do not call this if dim1 does not fit in chunking buffer */

	assert ( (extent * tip->elsize) <= CHBUFSIZE );

	binc		= tip->elsize * tip->stride;	/* stride in bytes */
	dim1_sz		= extent * tip->elsize;	/* size (bytes) of dim 1 */
	tip->stride	= 1;		/* linear, from here on down */

	if (nd == 1)
		dim2_ex	= 1;
	else
		dim2_ex	= dim[1].extent;

	dim1_pb	= CHBUFSIZE / dim1_sz;	/* dim 1 extents per buffer */

	if (dim1_pb > dim2_ex)
		dim1_pb	= dim2_ex;	/* reduce to dim 2 extent */

	switch (nd) {

	case 7:
	    for (id7 = 0; id7 < dim[6].extent; id7++) {
	      addr6	= addr;
	case 6:
	      for (id6 = 0; id6 < dim[5].extent; id6++) {
	        addr5	= addr;
	case 5:
		for (id5 = 0; id5 < dim[4].extent; id5++) {
	          addr4	= addr;
	case 4:
		  for (id4 = 0; id4 < dim[3].extent; id4++) {
		    addr3	= addr;
	case 3:
		    for (id3 = 0; id3 < dim[2].extent; id3++) {
		      addr2	= addr;

	case 2:
		      dim2_ex	= dim[1].extent;

		      while (dim2_ex > 0) {

			if (dim1_pb > dim2_ex)
				dim1_pb	= dim2_ex;
		
	case 1:
			lptr		= (char *) lbuf;
			tip->count	= dim1_pb * extent;

			if (cup->uwrt) {	/* If writing */

			  /*
			   * This loop transfers 1 or more passes through
			   * dimension 1 to cup->ucbuf buffer.
			   */

			  for (i = 0; i < dim1_pb; i++) {
			    _gather_data(lptr, extent, binc, tip->elsize, addr);

	      	            addr	= addr + (dim[1].stride_mult << bshft);
	      	            lptr	= lptr + dim1_sz;
			  }

			  errn	= func(css, cup, lbuf, tip, PARTIAL);
			}
			else {			/* If reading */

			  errn	= func(css, cup, lbuf, tip, PARTIAL);

			  /*
			   * This loop transfers 1 or more passes through
			   * dimension 1 from the lbuf[] buffer.
			   */

			  for (i = 0; i < dim1_pb; i++) {
			    _scatter_data(addr, extent, binc, tip->elsize, lptr);

	      	            addr	= addr + (dim[1].stride_mult << bshft);
	      	            lptr	= lptr + dim1_sz;
			  }
			}

			if (errn != 0) goto done;

			dim2_ex	= dim2_ex - dim1_pb;

		        if (nd == 1) goto done;
		      }

		      if (nd == 2) goto done;
	      	      addr	= addr2 + (dim[2].stride_mult << bshft);
		    }
		    if (nd == 3) goto done;
	      	    addr	= addr3 + (dim[3].stride_mult << bshft);
		  }
		  if (nd == 4) goto done;
	      	  addr	= addr4 + (dim[4].stride_mult << bshft);
		}
		if (nd == 5) goto done;
	      	addr	= addr5 + (dim[5].stride_mult << bshft);
	      }
	      if (nd == 6) goto done;
	      addr	= addr6 + (dim[6].stride_mult << bshft);
	    }
	}

done:
	return(errn);
}
