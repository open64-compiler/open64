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



#ifndef INLINE
#pragma ident "@(#) libf/fio/dopexfer.c	92.3	06/18/99 10:21:14"
#endif

#include <liberrno.h>
#include <stdlib.h>
#include <string.h>
#include <cray/nassert.h>
#include "fio.h"
#include "f90io.h"

/*
 *	If compiled with -D_ASSERT_ON, dopexfer will print the I/O list
 *	items encountered when DEBUG_90IO is set.
 */

static FILE *_df;
static int _ddope_nest	= 0;
static int _ddope	= -1;
static unit *_ddcup;		/* for debug output */

#ifdef	_ASSERT_ON

#define DEBUG_90IO \
	((_ddope == -1) ? ( \
		(getenv("DEBUG_90IO") != NULL) ? ( \
			(_df = fopen(getenv("DEBUG_90IO"), "w")), \
			(_ddope = (_df != NULL)) \
		) \
		: (\
			(_df = fopen("/dev/null", "w")), \
			(_ddope = 0) \
		) \
	) \
	: \
		_ddope \
	)

#define DD { \
	if (DEBUG_90IO) { \
		register int ix; \
		fprintf(_df, "unit %lld ", _ddcup->uid); \
		for (ix = 0; ix < _ddope_nest; ix++) \
			putc(' ', _df); \
	} \
}

#else
#define DEBUG_90IO 	0
#define DD	
#endif

/*
 *	The dovarlist structure stores a list of dovariable addresses.
 *	It is used by _map_to_dv().
 */

#define MAXDOVAR 7	/* we can't map to dv an implied do deeper than this */
struct dovarlist {
	int	nvar;
	int	*dov[MAXDOVAR];
};


static int _stride_dv(FIOSPTR css, unit *cup, DopeVectorType *dv,
	int **dovar, xfer_func *func);

int _map_to_dv(ioimplieddo_entry *impdo, DopeVectorType *dvptr,
	int **iarr, struct dovarlist *dovlp);

int _strip_mine(FIOSPTR css, unit *cup, xfer_func *func, ioimplieddo_entry *ie,
	int *retp);

/*
 *	_xfer_iolist
 *
 *		Perform I/O on all items in an I/O list.  func may be
 *		_ld_read, _rdfmt, _rdunf, _ld_write, _wrfmt, or _wrunf.
 *	
 *	Arguments
 *
 *		iolist	- structure which describes I/O list items.
 *		func	- the Fortran 77 I/O interface function to call.
 *		cup	- the unit pointer.
 *
 *	Return Value:
 *
 *		0		normal return
 *		FEEORCND	if end of record condition (with ADVANCE='NO')
 *		other <0	if end of file consition
 *		>0		if error condition
 */

#ifdef	INLINE
static int
_inline_xfer_iolist(
#else
int
_xfer_iolist(
#endif
	FIOSPTR		css,
	unit		*cup,
	iolist_header	*iolist,
	xfer_func	*func)
{
	register short	termrec;
	int	 	ret;
	type_packet	tip;
	register int	ioitems;
	ioentry_header	*nextioh;
	void		*nexte;
	int		**indarray;
	register int	mode;
	
	ioitems	= iolist->icount;
	nextioh	= (ioentry_header *)(iolist + 1);
	termrec	= 0;

	if (DEBUG_90IO) {
		_ddcup	= cup;		/* set this up for debug output */
		_ddope_nest++;
		if (_ddope_nest == 1) {
			char	*fnm = "";
			if (func == _rdfmt)		fnm = "_rdfmt";
			else if (func == _wrfmt)	fnm = "_wrfmt";
			else if (func == _ld_read)	fnm = "_ld_read";
			else if (func == _ld_write)	fnm = "_ld_write";
			else if (func == _rdunf)	fnm = "_rdunf";
			else if (func == _wrunf)	fnm = "_wrunf"; 
				
			DD; putc('\n', _df);
			DD; fprintf(_df,"----------------------------------\n");
			DD; fprintf(_df,
				"Begin iolist for unit %lld   func=%s\n",
				cup->uid, fnm);

			DD; fprintf(_df,"iolist->icount   = %d\n",iolist->icount);
		}
	}

	ret	= 0;			/* in case 0 entries were passed */

	while (ioitems--) {
			
		if (cup->f_lastiolist) {
			if ((void *)((long *)nextioh + nextioh->ioentsize) == cup->f_lastiolist)
				termrec	= 1;
		}

		nexte	= nextioh + 1;

		/* Initialize type information packet */

		tip.type77	= -1;
		tip.cnvindx	= 0;
		tip.count	= 1;
		tip.stride	= 1;

		switch (nextioh->valtype) {

		case IO_SCALAR:

		{
			ioscalar_entry	*se;
			void		*vaddr;

			se	= nexte;

			/* Assertion */
			assert ( se->tinfo.type == DVTYPE_ASCII ||
				 se->tinfo.int_len > 0 );

			tip.type90	= se->tinfo.type;
			tip.intlen	= se->tinfo.int_len;
			tip.extlen	= tip.intlen;
			tip.elsize	= tip.intlen >> 3;

			if (DEBUG_90IO) {
				DD; putc('\n',_df);
				DD; fprintf(_df,"IO_SCALAR,  type90=%d\n",
						tip.type90);
				DD; fprintf(_df,"address = 0%lo\n",
						(long)se->iovar_address.v);
				if (tip.type90 == DVTYPE_ASCII) {
				  DD; fprintf(_df,"character len = %ld\n",
						_fcdlen(se->iovar_address.fcd));
				}

				DD; fprintf(_df,"dpflag        = %d\n",
						se->tinfo.dpflag);
				DD; fprintf(_df,"kind_or_star  = %d\n",
						se->tinfo.kind_or_star);
				DD; fprintf(_df,"int_len       = %d\n",
						se->tinfo.int_len);
				DD; fprintf(_df,"dec_len       = %d\n",
						se->tinfo.dec_len);
			}

			if (tip.type90 == DVTYPE_ASCII) {

				vaddr		= _fcdtocp(se->iovar_address.fcd);
				tip.elsize	= tip.elsize *
					_fcdlen(se->iovar_address.fcd);
			}
			else
				vaddr		= se->iovar_address.v;

			tip.count	= 1;
			tip.stride	= 1;

#if	NUMERIC_DATA_CONVERSION_ENABLED
			if ( !(cup->ufmt) &&	/* If unformatted */
			     (cup->unumcvrt || cup->ucharset) ) {

				ret	= _get_dc_param(css, cup, se->tinfo,
							&tip);
				if (ret != 0)
					goto done;
			}
#endif

			mode	= termrec ? FULL : PARTIAL;
			ret	= func(css, cup, vaddr, &tip, mode);

			break;
		}

		case IO_DOPEVEC:
		{
			ioarray_entry	*ae;
			DopeVectorType	*dv;

			ae		= nexte;
			dv		= ae->dv;
			tip.type90	= dv->type_lens.type;

			if (DEBUG_90IO) {
				DD; putc('\n',_df);
				DD; fprintf(_df,"IO_DOPEVEC,  type90=%d\n",
				    tip.type90);
			}

			/* Assertions */
			assert ( ! (ae->indflag && ae->dovar == NULL) );

			indarray	= NULL;

			if (ae->indflag)
				indarray	= ae->dovar;

			/*
			 * Call the xfer_func directly for some special
			 * cased non-indexed dopevectors.
			 */
			if (indarray == NULL && tip.type90 != DVTYPE_ASCII) {

				register short	n_dim = dv->n_dim;
				register long	extent = dv->dimension[0].extent;
				register long	inc;
				struct DvDimen	*dimen = dv->dimension;

				tip.intlen	= dv->type_lens.int_len;
				tip.extlen	= tip.intlen;
				tip.elsize	= tip.intlen >> 3;

			    /*
			     * Special case any rank 1 dopevector, or a rank > 1
			     * dopevector whose storage sequence is strided
			     * consistently throughout.
			     */
			    if (n_dim != 1) {
				register short	nc;

				if (n_dim == 2) {
				    if (dimen[0].stride_mult * extent !=
					dimen[1].stride_mult)
					    goto general_dv_processing;
				    extent	*= dimen[1].extent;
				}
				else if (n_dim == 0) {
				    extent	= 1;
				}
				else {
				    for (nc = 0; nc < (n_dim-1); nc++) {
				        register long	st = dimen[nc].stride_mult;
				        register long	ex = dimen[nc].extent;
				        if ( (st * ex) !=
					    dimen[nc+1].stride_mult)
					    goto general_dv_processing;
				        extent	*= dimen[nc+1].extent;
				    }
				}
			    }

			    if (extent > 1) {
				register long	sm;

			    	sm	= dv->dimension[0].stride_mult;

				if (DEBUG_90IO) {
				  DD;fprintf(_df,"elsize=%ld sm=%ld SMSCALE=%d\n",
					tip.elsize, sm, SMSCALE(dv));
				  DD;fprintf(_df,"int_len=%d kind_or_star=%d ext_len=%d\n",
					dv->type_lens.int_len,
					dv->type_lens.kind_or_star,
					dv->type_lens.dec_len);
				}
				if (sm * (signed)SMSCALE(dv) == tip.elsize)
				    inc	= 1;
				else {
				    register long  bpsm;

				    bpsm = sm * (signed)SMSCALE(dv);
				    inc	 = bpsm / tip.elsize;

				    /* if stride not a multiple of size ...*/

				    if (tip.elsize * inc != bpsm) 
					goto general_dv_processing;
			        }
			    }
			    else
				inc	= 1;

			    tip.count	= extent;
			    tip.stride	= inc;

#if	NUMERIC_DATA_CONVERSION_ENABLED
			    if ( !(cup->ufmt) &&	/* If unformatted */
				 (cup->unumcvrt || cup->ucharset) ) {
				ret	= _get_dc_param(css, cup, dv->type_lens,
						&tip);
				if (ret != 0)
				    return(ret);
			    }
#endif
			    if (DEBUG_90IO) {
				DD; fprintf(_df,"Fold DV to 1-dim, extent=%ld inc=%ld\n",
				    extent, inc);
			    }

			    mode	= termrec ? FULL : PARTIAL;
			    ret		= func(css, cup, dv->base_addr.a.ptr,
					  	&tip, mode);
			}
			else {
general_dv_processing:
				ret	= _stride_dv(css, cup, ae->dv, indarray,
						func);
			}

			break;
		}

		case IO_LOOP:
		{
			register long	loopinc;
			register long	begcnt;
			register long	endcnt; 
			int		*loopvar;
			int		*locia[MAXDIM];
			DopeVectorType		locdv;
			ioimplieddo_entry	*ie;
			struct dovarlist	dovl;

			ie	= nexte;

			if (DEBUG_90IO) {
				DD; putc('\n',_df);
				DD; fprintf(_df,
					"IO_LOOP  start=%d  inc=%d  end=%d\n",
					*ie->iobegcnt, *ie->ioinccnt,
					*ie->ioendcnt);
			}

			dovl.nvar	= 0;

			if (_map_to_dv(ie, &locdv, locia, &dovl)) {
				if (DEBUG_90IO) {
					DD; fprintf(_df,"Mapped to dopevect\n");
				}
				ret	= _stride_dv(css, cup, &locdv, locia,
						func);
				break;
			}

			/*
			 * If all iolist entries inside an implied do loop
			 * are the same type, we can strip mine the loop
			 * as an optimization.
			 */
			if (_strip_mine(css, cup, func, ie, &ret))
				break;

			if (DEBUG_90IO) {
				DD; fprintf(_df,
				"Could not map to dopevector or strip mine \n");
			}

			/* Assertions */

			assert ( ie->ioinccnt  != NULL );
			assert ( ie->ioloopvar != NULL );
			assert ( ie->iobegcnt  != NULL );
			assert ( ie->ioendcnt  != NULL );

			loopinc	= *ie->ioinccnt;
			loopvar	= ie->ioloopvar;
			begcnt	= *ie->iobegcnt;
			endcnt	= *ie->ioendcnt;

			if (loopinc == 0) {
				ret	= FEINCZER;
				goto done;
			}

			*loopvar	= begcnt;

			/* If recursive implied DO LOOP, clear pointer so
			 * too many records are not written in seq unf IO
			 */
			if (cup->f_lastiolist != NULL)
				cup->f_lastiolist = NULL;

			for (;;) {

			    if (DEBUG_90IO) {
				DD; fprintf(_df,"loopvar = %d\n",*loopvar);
			    }

			    if (loopinc > 0) {
				if (*loopvar > endcnt) break;
			    }
			    else {
				if (*loopvar < endcnt) break;
			    }

			    ret	= _xfer_iolist(css, cup, (void *)(ie + 1), func);

			    if (ret != 0)
				goto done;

			    *loopvar	+= loopinc;
			}
			break;
		}

		default:
			_ferr(css, FEINTUNK);
		}

		if (ret != 0)
			goto done;

		nextioh	= (ioentry_header*)((long *)nextioh +
				nextioh->ioentsize);
	}
done:
	if (DEBUG_90IO) {
		if (_ddope_nest == 1) {
			DD; fprintf(_df,"End iolist for unit %lld\n",cup->uid);
		}
		_ddope_nest--;
	}
		
	return(ret);
}

/*
 *	_stride_dv
 *
 *		Call a specified function to transfer a data area defined by 
 *		a dopevector.  This corresponds to an array section or a
 *		sequence of array elements expressible by use of an implied
 *		do loop in an iolist.
 *
 *	Arguments
 *
 *		dv	- dope vector which describes the array section.
 *		func	- the function to call with each segment.
 *
 *	Return Value
 *
 *		0		normal return
 *		FEEORCND	if end of record condition (with ADVANCE='NO')
 *		other <0	if end of file consition
 *		>0		if error condition
 */

static int
_stride_dv(
	FIOSPTR		css,
	unit		*cup,
	DopeVectorType	*dv,
	int		**dovar,
	xfer_func	*func)
{
	register short	element_stride;		/* 1 iff elsize divides stride*/
	register short	i;
	register short	nd;
	register short	newi;
	register short	nc;
	register int	id1, id2, id3, id4, id5, id6, id7;
	register int	ret;
	register long	badjust;		/* offset for collapsed dims */
	register long	extent;			/* extent of first dimension */
	struct DvDimen	*dvdimen;
	bcont		*addr; 			/* for numeric data */
	char		*baddr;			/* for byte-oriented data */
	void		*addr2, *addr3, *addr4;
	void		*addr5, *addr6;
	type_packet	tip;
	struct DvDimen	dimen[MAXDIM];

/*
 *	Decide whether the f90 or f77 type code will be used.
 */
	if (DEBUG_90IO) {
		register short	itmp;
		DD; fprintf(_df,"\n");
		DD; fprintf(_df,"Enter _stride_dv\n");
		DD; fprintf(_df,"dv->base_addr        = 0%lo\n",
				 (long)dv->base_addr.a.ptr);
		if (dv->type_lens.type == DVTYPE_ASCII) {
		  DD; fprintf(_df,"character len          = %ld\n",
				_fcdlen(dv->base_addr.charptr));
		}
		DD; fprintf(_df,"dv->base_addr.a.el_len = %ld\n",
			dv->base_addr.a.el_len);
		DD; fprintf(_df,"dv->assoc              = %d\n",dv->assoc);
		DD; fprintf(_df,"dv->ptr_alloc          = %d\n",dv->ptr_alloc);
		DD; fprintf(_df,"dv->p_or_a             = %d\n",dv->p_or_a);
		DD; fprintf(_df,"dv->n_dim              = %d\n",dv->n_dim);

		DD; fprintf(_df,"dv->type_lens.dpflag        = %d\n",
				 dv->type_lens.dpflag);
		DD; fprintf(_df,"dv->type_lens.kind_or_star  = %d\n",
				 dv->type_lens.kind_or_star);
		DD; fprintf(_df,"dv->type_lens.int_len       = %d\n",
				 dv->type_lens.int_len);
		DD; fprintf(_df,"dv->type_lens.dec_len       = %d\n",
				 dv->type_lens.dec_len);

		DD; fprintf(_df,"dv->orig_base     = %p\n",dv->orig_base);
		DD; fprintf(_df,"dv->orig_size     = %ld\n",dv->orig_size);

		for (itmp = 0; itmp < dv->n_dim; itmp++) {
			DD; fprintf(_df,"  Dim %d ", itmp);
			fprintf(_df," low=%2ld  extent=%2ld stride_mult=%2ld\n",
				dv->dimension[itmp].low_bound,
				dv->dimension[itmp].extent,
				dv->dimension[itmp].stride_mult);
		}
		if (dovar != NULL) {
			DD; fprintf(_df,"Indexes into dopevector:\n");
			DD; fprintf(_df,"  Index Addresses: ");
			for (itmp = 0; itmp < dv->n_dim; itmp++) {
				if (dovar[itmp] == NULL)
					fprintf(_df," NULL");
				else
					fprintf(_df," 0%lo", (long)dovar[itmp]);
			}
			fprintf(_df,"\n");
			DD; fprintf(_df,"  Index Values: ");
			for (itmp = 0; itmp < dv->n_dim ; itmp++) {
				if (dovar[itmp] == NULL)
					fprintf(_df," -");
				else
					fprintf(_df," 0%o", *dovar[itmp]);
			}
			fprintf(_df,"\n");
		}
	}	/* end if (DEBUG_90IO) */

	/* Assertions */

	assert ( dv != NULL );
	assert ( dv->type_lens.int_len > 0 );

	if (dv->p_or_a && (dv->assoc == 0))
		return(FEPTRNAS);		/* pointer not associated */

	tip.type77	= -1;
	tip.type90	= dv->type_lens.type;
	tip.intlen	= dv->type_lens.int_len;
	tip.extlen	= tip.intlen;
	tip.elsize	= tip.intlen >> 3;
	tip.cnvindx	= 0;
	tip.count	= 1;
	tip.stride	= 1;

/*
 *	Set up implicit data conversion parameters.
 */

#if	NUMERIC_DATA_CONVERSION_ENABLED
	if ( !(cup->ufmt) &&	/* If unformatted */
	     (cup->unumcvrt || cup->ucharset) ) {

		ret	= _get_dc_param(css, cup, dv->type_lens, &tip);

		if (ret != 0)
			goto done;
	}
#endif

	nd	= dv->n_dim;
	badjust	= 0;

/*
 *	Make a local copy of the dimension information so we may optimize it.
 */
	for (i = 0; i < nd; i++)
		dimen[i]	= dv->dimension[i];

/*
 *	Fold any indexes into the new dimension structure.  The
 *	result is that we can ignore the low_bound field in the
 *	nested loops. 
 *
 *	We also collapse (remove) indexed dimensions and 
 *	unindexed dimensions with extents of one.
 */
	newi	= 0;
	dvdimen	= dv->dimension;

	for (i = 0; i < nd; i++) {
		if (dovar == NULL || dovar[i] == NULL) {

			/* bail out here if any extent is 0 */

			if (dvdimen[i].extent == 0)
				return(0);	

			/* use this unindexed dimension if extent > 1 */

			if (dvdimen[i].extent > 1)
				dimen[newi++]	= dvdimen[i];
		}
		else	/* collapse this indexed dimension */
			badjust	+= (*dovar[i] - dvdimen[i].low_bound) * 
					dvdimen[i].stride_mult;
	}

	if (DEBUG_90IO) {
		DD; fprintf(_df, "%d indexed or extent-1 dims collapsed\n",
			    nd - newi);
	}

	nd	= newi;

	if (DEBUG_90IO) {
		register int	i_dim;
		DD; fprintf(_df, "%d dimension(s) are left\n", nd);
		for (i_dim = 0; i_dim < nd ; i_dim++) {
			DD; fprintf(_df,"  Dim %d ",i_dim);
			fprintf(_df," low=%2ld  extent=%2ld stride_mult=%2ld\n",
				dimen[i_dim].low_bound,
				dimen[i_dim].extent,
				dimen[i_dim].stride_mult);
		}
	}

/*
 *	When two or more initial dimensions are stored and strided 
 *	contiguously (often the case when an entire array is 
 *	being processed), collapse the adjacent compatible dimensions.
 *
 *	Someday, the compiler might do compile-time and/or run-time checking
 *	to eliminate the need to optimize this here in the library.
 *
 *	The first loop sets nc to the number of dimensions which could
 *	be removed by collapsing them into an adjacent dimension
 *	while preserving constant striding.
 */
	for (nc = 0; nc < (nd-1); nc++) {
		register long	st = dimen[nc].stride_mult;
		register long	ex = dimen[nc].extent;
		if ((st * ex) != dimen[nc+1].stride_mult)
			break;
	}

	if (DEBUG_90IO) {
		DD; fprintf(_df, "%d dimensions removed by collapsing compatibile adjacent dimension(s)\n", nc);
	}

/*
 *	Collapse nc adjacent dimensions.   These dimensions are
 *	replaced by one dimension with stride equal to the stride
 *	of the earliest dimension and extent equal to the product
 *	of the extents of the dimensions being replaced.
 */ 
	if (nc > 0) {
		register short	j;

		for (j = 1; j <= nc; j++)
			dimen[0].extent	*= dimen[j].extent;

		nd	= nd - nc;	/* decrease the number of dimensions */

		assert (nd > 0);/* must leave at least one dimension */

		/*
		 * Move the other dimensions down to delete the
		 * collapsed dimensions.
		 */

		for (j = 1; j < nd; j++)
			dimen[j]	= dimen[j+nc];
	}

	if (DEBUG_90IO) {
		register int	i_dim;
		DD; fprintf(_df, "%d dimension(s) are left\n", nd);
		for (i_dim = 0; i_dim < nd ; i_dim++) {
			DD; fprintf(_df,"  Dim %d ",i_dim);
			fprintf(_df," low=%2ld  extent=%2ld stride_mult=%2ld\n",
				dimen[i_dim].low_bound,
				dimen[i_dim].extent,
				dimen[i_dim].stride_mult);
		}
	}
		
	/*
	 * Special case a single indexed array element or pointer to scalar as 
	 * a rank 1 array of shape 1.
	 */
	if (nd == 0) {
		nd			= 1;
		dimen[0].extent		= 1;
		dimen[0].stride_mult	= 0;
	}

	if (tip.type90 == DVTYPE_ASCII) {

		tip.elsize	= tip.elsize * _fcdlen(dv->base_addr.charptr);
		extent		= dimen[0].extent;
		element_stride	= 1;

		if (extent > 1) {
		    register long stm;

		    stm		= dimen[0].stride_mult;
		    tip.stride	= stm / tip.elsize;

		    if (tip.stride * tip.elsize != stm)
			element_stride	= 0;	/* it's a section of substrings */
		}
	
		/* For character arrays in an implied DO loop, badjust
		 * contains the elsize offset from the stridemult:
		 *      badjust += (*dovar[i] - lowbound[i]) * stridemult[i]
		 */
		baddr	= _fcdtocp(dv->base_addr.charptr) + badjust;
		
		switch (nd) {

		case 7:
		    for (id7 = 0; id7 < dimen[6].extent; id7++) {
		      addr6	= baddr;
		case 6:
		      for (id6 = 0; id6 < dimen[5].extent; id6++) {
		        addr5	= baddr;
		case 5:
			for (id5 = 0; id5 < dimen[4].extent; id5++) {
		          addr4	= baddr;
		case 4:
			  for (id4 = 0; id4 < dimen[3].extent; id4++) {
			    addr3	= baddr;
		case 3:
			    for (id3 = 0; id3 < dimen[2].extent; id3++) {
			      addr2	= baddr;
		case 2:
			      for (id2 = 0; id2 < dimen[1].extent; id2++) {
		case 1:
				if (element_stride) {
				  tip.count	= extent;
				  ret	= func(css, cup, baddr, &tip, PARTIAL);
				  if (ret != 0) goto done;
				}
				else {
				  char	*ba;
				  ba	= baddr;
				  for (id1 = 0; id1 < extent; id1++) {
				    tip.count	= 1;
				    ret	= func(css, cup, ba, &tip, PARTIAL);
				    if (ret != 0) goto done;
				    ba	= ba + dimen[0].stride_mult;
				  }
				}
	
				if (nd == 1) goto done;
		      	        baddr	+= dimen[1].stride_mult;
			      }
			      if (nd == 2) goto done;
		      	      baddr	= addr2;
		      	      baddr	+= dimen[2].stride_mult;
			    }
			    if (nd == 3) goto done;
		      	    baddr	= addr3;
		      	    baddr	+= dimen[3].stride_mult;
			  }
			  if (nd == 4) goto done;
		      	  baddr	= addr4;
		      	  baddr	+= dimen[4].stride_mult;
			}
			if (nd == 5) goto done;
		      	baddr	= addr5;
		      	baddr	+= dimen[5].stride_mult;
		      }
		      if (nd == 6) goto done;
		      baddr	= addr6;
		      baddr	+= dimen[6].stride_mult;
		    }
		}
	}
	else {				/* numeric data */

		register int	bshft;	/* 0 or 1; shift count for ratio of */
					/* stride_mult units to basic storage */
					/* unit size. */
		/*
		 *	We only support dopevector stride mults with units
		 *	scaled by sizeof(long) or sizeof(bcont).
 		 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
		assert( SMSCALE(dv) == sizeof(bcont) ||
			SMSCALE(dv) == sizeof(_f_int2) ||
			SMSCALE(dv) == sizeof(_f_int4) ||
			SMSCALE(dv) == sizeof(long)	);
#else
		assert( SMSCALE(dv) == sizeof(bcont) ||
			SMSCALE(dv) == sizeof(long)	);
#endif

		/* The -1 should not be possible but check for it */

		assert( SMSHIFT(dv) != -1);

		element_stride	= 1;
		extent		= dimen[0].extent;
		bshft		= SMSHIFT(dv);

		if (extent > 1) {
		    register long	bpsm;

		    bpsm	= dimen[0].stride_mult * (signed)SMSCALE(dv);
		    tip.stride	= bpsm / tip.elsize;

		    if (tip.stride * tip.elsize != bpsm)
			element_stride	= 0;	/* section across derived type */
		}

		addr	= (bcont *)dv->base_addr.a.ptr + (badjust << bshft);
		
		/*
		 *	Decide if we should copy one or more dimensions 
		 *	contiguously before writing (or read a contigous
 		 *	chunk before distributing it to one or more possibley
		 *	strided dimensions.
 		 *	
		 */

		if (nd > 1 && element_stride && (extent * tip.elsize) <= CHBUFSIZE){

			if (DEBUG_90IO) {
			    DD; fprintf(_df, "calling chunk routine\n");
			}

			ret	= _iochunk(css, cup, func, dimen, &tip, nd,
					extent, bshft, addr); 
			return(ret);
		}
		
		switch (nd) {

		case 7:
		    for (id7 = 0; id7 < dimen[6].extent; id7++) {
		      addr6	= addr;
		case 6:
		      for (id6 = 0; id6 < dimen[5].extent; id6++) {
		        addr5	= addr;
		case 5:
			for (id5 = 0; id5 < dimen[4].extent; id5++) {
		          addr4	= addr;
		case 4:
			  for (id4 = 0; id4 < dimen[3].extent; id4++) {
			    addr3	= addr;
		case 3:
			    for (id3 = 0; id3 < dimen[2].extent; id3++) {
			      addr2	= addr;
		case 2:
			      for (id2 = 0; id2 < dimen[1].extent; id2++) {
		case 1:
				if (element_stride)  {
				  tip.count	= extent;
				  ret	= func(css, cup, addr, &tip, PARTIAL);
				}
				else {
				  bcont	*ad;
				  ad	= addr;
				  /* 
				   * If derived type foo contains two fields,
				   * real a and double precision d,  then
				   * foo(1:2)%d generates this type of 
				   * dopevector with a stride which is not
				   * a multiple of the element size.
				   */
				  for (id1 = 0; id1 < extent; id1++) {
				    tip.count	= 1;
				    ret	= func(css, cup, ad, &tip, PARTIAL);
				    if (ret != 0) goto done;
				    ad	= ad + dimen[0].stride_mult;
				  }
				}

				if (ret != 0) goto done;
	
				if (nd == 1) goto done;
		      	        addr	+= dimen[1].stride_mult << bshft;
			      }
			      if (nd == 2) goto done;
		      	      addr	= addr2;
		      	      addr	+= dimen[2].stride_mult << bshft;
			    }
			    if (nd == 3) goto done;
		      	    addr	= addr3;
		      	    addr	+= dimen[3].stride_mult << bshft;
			  }
			  if (nd == 4) goto done;
		      	  addr	= addr4;
		      	  addr	+= dimen[4].stride_mult << bshft;
			}
			if (nd == 5) goto done;
		      	addr	= addr5;
		      	addr	+= dimen[5].stride_mult << bshft;
		      }
		      if (nd == 6) goto done;
		      addr	= addr6;
		      addr	+= dimen[6].stride_mult << bshft;
		    }
		}
	}

done:
	return(ret);
}

/*
 *	_map_to_dv 
 *
 *	Checks if a (possibly nested) implied do construct may be mapped 
 *	into a dopevector with optional array of dimension indices (one per
 *	dimension).  This function operates recursively.
 *
 *	An implied do construct must meet all of the following criteria
 *	to be mappable into an indexed dopevector:
 *
 *		1) It must contain only one nested iolist item which is a
 *		   dopevector or a nested implied do which maps to a 
 *		   dopevector.
 *		2) A nested dopevector must be indexed by the loop variable
 *		   for the implied do in exactly one dimension.
 *		3) All dimensions of the nested dopevector after the 
 *		   indexed dimension must be indexed or have an extent <= 1. 
 *		4) The do variable for this implied do construct is not also
 *		   serving as the beginning count, ending count, or increment 
 *		   for any nested implied do construct.
 *
 *	Return Value
 *		0 if not successfully mapped to a dopevector.
 *		1 if successfully mapped to a dopevector.
 */
int
_map_to_dv(
	ioimplieddo_entry	*impdo,	/* input impled do construct */
	DopeVectorType		*dvptr,	/* output dopevector */
	int			**iarr,	/* output list of array indexes */
	struct dovarlist	*dovlp)	/* lists do variables for any higher */						/* level implied do constructs. */
{
	register short	need_to_shift;
	register int	i;
	register int	ind_dim;
	register int	ret;
	register long	doinc;
	register long	extent;
	register long	adjust;
	struct DvDimen	*dvdimen;
	struct DvDimen	dvdim_tmp;
	iolist_header	*nested_iolist;
	ioentry_header	*nextioh;
	ioarray_entry	*ae;

	long	_tripcnt(long beg, long end, long inc);

	nested_iolist	= (iolist_header *)(impdo + 1);

	if (nested_iolist->icount != 1)
		return(0);		/* must have exactly 1 iolist item */

	for (i = 0; i < dovlp->nvar; i++) {

		if (DEBUG_90IO) {
			DD; fprintf(_df,"compare loopvar addr %lo to %lo %lo %lo\n",
			(long)dovlp->dov[i], (long)impdo->iobegcnt,
			(long)impdo->ioendcnt, (long)impdo->ioinccnt);
		}

		if ( (impdo->iobegcnt == dovlp->dov[i]) ||
		     (impdo->ioendcnt == dovlp->dov[i]) ||
		     (impdo->ioinccnt == dovlp->dov[i]) )
			return(0);	/* do var is also a beg, end, or inc */
	}

	if (DEBUG_90IO) {
		DD; fprintf(_df,"Making recursive _map_to_dv check\n");
	}

	nextioh	= (ioentry_header *)(nested_iolist + 1);

	switch (nextioh->valtype) {

	default:
		return(0);

	case IO_LOOP:

		/*
		 * Store the addr of this nest level's implied do variable
		 * in the list.  The recursive calls will compare the
	 	 * begin, end, and increment to this variable to ensure
		 * that they are not the same.
		 */
		dovlp->nvar++;
		if (dovlp->nvar > MAXDOVAR) 
			return(0);	/* nested too deeply, give up */
		dovlp->dov[ dovlp->nvar-1 ]	= impdo->ioloopvar;

		/* recursive call to check nested implied do */
		ret	= _map_to_dv(	(ioimplieddo_entry *)(nextioh + 1),
					dvptr,
					iarr,
					dovlp);
		if (ret == 0)
			return(0);	/* nested implied do not mappable */
		break;

	case IO_DOPEVEC:
		ae	= (ioarray_entry *)(nextioh + 1);

		/* copy the dopevector */
		*dvptr	= *(ae->dv);

		if (ae->indflag == 0)
			return(0);	/* dopevector is not indexed */

		/* copy the index array */
		for (i = 0; i < dvptr->n_dim; i++)
			iarr[i]	= ae->dovar[i];

		break;
	}

	dvdimen	= dvptr->dimension;
/*
 *	Search for dimensions indexed by the do variable.
 */
	ind_dim	= -1;	/* indicate no dimension indexed by do variable */

	for (i = 0; i < dvptr->n_dim; i++)
		if (iarr[i] == impdo->ioloopvar) {
			ind_dim	= i;
			break;
		}

	if (ind_dim == -1)
		return(0);		/* not indexed by loop variable */

	if (DEBUG_90IO) {
		DD; fprintf(_df,"Dim %d indexed by impdo\n", ind_dim);
	}

/*
 *	Check that the dimensions after the indexed dimension satisfy necessary
 *	condition that the dopevector storage order be the same as the 
 *	implied do storage order.
 */
	need_to_shift	= 0;

	for (i = ind_dim+1; i < dvptr->n_dim; i++) {
		/*
		 * If the do variable indexes more than one dimension, we
		 * can sometimes handle it by adding together the stride 
		 * mults for all indexed dimensions, essentially merging the 
		 * two dimensions.
		 */
		if (iarr[i] == impdo->ioloopvar) {

		    if (DEBUG_90IO) {
			DD; fprintf(_df,"Dim %d also indexed by impdo\n",i);
		    }

		    if (dvdimen[i].low_bound != dvdimen[ind_dim].low_bound)
			return(0);	/* cannot fold if low_bound's differ */

		    dvdimen[i].stride_mult	+= dvdimen[ind_dim].stride_mult;

		    /*
		     * Setting extent to 1 allows stride_dv to later 
		     * collapse away this dimension.
		     */ 
		    dvdimen[ind_dim].extent	= 1;
		    iarr[ind_dim]		= NULL;

		    ind_dim		= i;	/* replace ind_dim */
		    need_to_shift	= 0;
		}
		else if (iarr[i] == NULL && dvdimen[i].extent > 1)
		    need_to_shift	= 1;
	}

/* 
 *	Ensure that the implied do construct and the dopevector to 
 *	which it is mapped have the same storage order.   This is done 
 *	by ensuring that "ind_dim" is after any unindexed dimension of 
 *	size > 1.  The indexed dimension must be the slowest moving.
 */
	if (need_to_shift) {
		dvdim_tmp	= dvdimen[ind_dim];
		for (i = ind_dim; i < dvptr->n_dim-1; i++) {
			dvdimen[i]	= dvdimen[i+1];
			iarr[i]		= iarr[i+1];
		}
		ind_dim	= i;

		/* slowest moving dimentsion moves with the implied do */
		dvdimen[ind_dim]	= dvdim_tmp;
	}

/*
 *	Remove the indexing of the do-loop-indexed dimension.
 */
	iarr[ind_dim]	= NULL;

	doinc	= *impdo->ioinccnt;

	adjust	= (*impdo->iobegcnt - dvdimen[ind_dim].low_bound) *
		 dvdimen[ind_dim].stride_mult;
	
	extent	= _tripcnt(*impdo->iobegcnt, *impdo->ioendcnt, doinc);
	
/*
 *	Fold the do loop info into the corresponding dimension information.
 */
	dvdimen[ind_dim].extent		= extent;
	dvdimen[ind_dim].stride_mult	*= doinc;

	if (dvptr->type_lens.type == DVTYPE_ASCII) {
		_fcd	f;
		int	flen;
		
		f	= dvptr->base_addr.charptr;
		flen	= _fcdlen(f);
		dvptr->base_addr.charptr	= _cptofcd(_fcdtocp(f) + adjust, flen);
	}
	else {

		/* bshft is 0 iff element size is sizeof(bcont) */
		int	bshft =	SMSHIFT(dvptr);
		dvptr->base_addr.a.ptr	= (bcont *)dvptr->base_addr.a.ptr +
						(adjust << bshft);

		/* The -1 is not be possible but check for it */
		assert( SMSHIFT(dvptr) != -1);
	}

/*
 *	Update the loop variable.
 */
	*impdo->ioloopvar	= *impdo->iobegcnt + extent * doinc;

	return(1);		/* indicate that the mapping to dv succeeded */
}

/*
 *	_tripcnt
 *
 *		Returns the do loop trip count.
 */
long
_tripcnt(long beg, long end, long inc)
{
	register long	tc;

	if (inc < 0) {		/* must negate for ANSI C to divide correctly */
		beg	= -beg;
		end	= -end;
		inc	= -inc;
	}

	tc	= (end - beg + inc) / inc;

	if (tc < 0)
		tc	= 0;

	return(tc);
}

typedef struct strideloop {

	void	*saddr;		/* starting address */

	long 	binc;		/* stride in bytes. This is the stride
				 * between elements of the current 
				 * array between two iterations of the
				 * implied do loop. */

	long	inc;		/* stride in elements (invalid if elstr == 0) */

	int	elstr;		/* 1 iff (binc % elsize == 0) */

} strideloop_t;

/*
 *	_strip_mine
 *
 *	Tries to handle an implied do construct by strip mining.  This is done
 *	only if:
 *
 *		1) The implied do loop contains MAXITEMS or less.
 *		2) Each item in the implied do loop is a scalar or
 *		   a dopevector which represents one array element or
 *		   one contiguous array element.
 *		3) Each item is the same noncharacter type and kind.
 *
 *	Return Value
 *		0 if we could not strip mine this implied do loop
 *		1 if we strip mined it; *retp contains err/end/ok status.
 */

#define	MAXITEMS	32	/* max items allowed inside implied do loop */

int
_strip_mine(
	FIOSPTR			css,
	unit			*cup,
	xfer_func		*func,
	ioimplieddo_entry	*impdo,
	int			*retp)
{
	register short	reading;
	register short	sametp;
	register int	bshft;
	register int	ioitems;
	register int	item;
	register long	badjust;
	register long	begcnt;
	register long	bytes_per_trip; 
	register long	endcnt;
	register long	dotrips;
	register long	ib;
	register long	loopinc;
	register long	trips_per_buf;
	long		stride;
	long		inc;
	int		*loopvar;
	int		**indarray;
	void		*nexte;
	bcont		*addr;
	long		locbuf[CHBUFSIZE/sizeof(long)];
	char		*lbptr;		/* points into locbuf */
	type_packet	tip;			/* Type information packet */
	iolist_header	*iolist;
	strideloop_t	slt[MAXITEMS];
	struct DvDimen	*dimen;
	ioentry_header	*nextioh;
	f90_type_t	ts, curts;

	*retp	= 0;
	reading	= !(cup->uwrt);

	if (DEBUG_90IO) {
		DD; putc('\n',_df);
		DD; fprintf(_df,"Enter _strip_mine\n");
	}

/*
 *	We don't strip mine list-directed reads because they might not
 *	deliver all the data requested if a record is terminated by a /.
 */
	if (func == _ld_read)
		return(0);			
	
	loopinc	= *impdo->ioinccnt;
	loopvar	= impdo->ioloopvar;
	begcnt	= *impdo->iobegcnt;
	endcnt	= *impdo->ioendcnt;

	*loopvar	= begcnt;

	if (loopinc == 0) {
		*retp	= FEINCZER;		/* infinite loop detected */
		return(1);
	}

	iolist	= (iolist_header *) (impdo + 1);
	ioitems	= iolist->icount;

	if (ioitems > MAXITEMS)
		return(0);			/* exceeds arbitrary limit */

	sametp	= 1;
	nextioh	= (ioentry_header *)(iolist + 1);

	tip.type77	= -1;
	tip.cnvindx	= 0;
	tip.count	= 1;
	tip.stride	= 1;
/*
 *	This loop builds up the "stp" array of structures which record the
 *	small amount of relevant information (striding, size, address) for 
 *	each iolist item within the implied do loop.
 */
	for (item = 0; item < ioitems; item++) {
		register int	i;
		register short	n_dim;
		ioscalar_entry	*se;
		ioarray_entry	*ae;
		DopeVectorType	*dv;

		nexte	= nextioh + 1;

		if (nextioh->valtype == IO_SCALAR) {
			se	= nexte;
			curts	= se->tinfo;
		}
		else if (nextioh->valtype == IO_DOPEVEC) {
			ae	= nexte;
			dv	= ae->dv;
			curts	= dv->type_lens;
		}
		else
			return(0);

		tip.type90	= curts.type;

		if (DEBUG_90IO) {
			DD; putc('\n',_df);
			DD; fprintf(_df,"%s,  type90=%d\n",
				((nextioh->valtype == IO_SCALAR) ?
				"IO_SCALAR" : "IO_DOPEVEC"), tip.type90);
		}

		if (tip.type90 == DVTYPE_ASCII)
			return(0);	/* We don't strip mine characters */

		if (item == 0) {
			ts		= curts;
			tip.intlen	= ts.int_len;
			tip.elsize	= ts.int_len >> 3;
		}
		else {
			if (curts.int_len != ts.int_len)
				return(0);	/* not all same size */
			if (memcmp(&curts, &ts, sizeof(ts)))
				sametp	= 0;	/* not all same type */
		}

		/*
		 * Do different stuff for scalars or dopevectors.
		 */
		switch (nextioh->valtype) {

		case IO_SCALAR:

			slt[item].saddr	= se->iovar_address.v;
			slt[item].binc	= 0;
			slt[item].inc	= 0;
			slt[item].elstr	= 1;
			
			break;

		case IO_DOPEVEC:

			indarray	= NULL;

			if (ae->indflag)
				indarray	= ae->dovar;

			n_dim	= dv->n_dim;
			dimen	= dv->dimension;

			badjust	= 0;
			stride	= 0;

			for (i = 0; i < n_dim; i++) {
			    if (indarray != NULL && indarray[i] != NULL) {
				badjust	+= (*indarray[i] - dimen[i].low_bound) *
						dimen[i].stride_mult;
				if (indarray[i] == loopvar) {
				    stride	+= loopinc*dimen[i].stride_mult;
			    	}
			    }
			    else {
				if (dimen[i].extent > 1)
					return(0);	/* no sections allowed*/
			    }
			}

			stride	= stride * (signed)SMSCALE(dv);

			/* The -1 should not be possible but check for it */

			assert( SMSHIFT(dv) != -1);

			bshft	= SMSHIFT(dv);
			addr	= (bcont *)dv->base_addr.a.ptr + (badjust << bshft);

			slt[item].saddr	= addr;
			slt[item].binc	= stride;
			slt[item].inc	= stride / tip.elsize;
			slt[item].elstr	= (stride % tip.elsize == 0);
			
			break;

		default:

			return(0);	/* nested impdo's not supported */
			
		} /* switch */

		nextioh	= (ioentry_header *)((long *)nextioh +
				nextioh->ioentsize);
	} /* for */

	dotrips		= _tripcnt(begcnt, endcnt, loopinc);
	bytes_per_trip	= tip.elsize * ioitems;	/* bytes */
	trips_per_buf	= CHBUFSIZE/bytes_per_trip;	/* trips per buffer */

	if (trips_per_buf > dotrips)
		trips_per_buf	= dotrips;

	if (trips_per_buf == 0)
		return(0);		/* too many items inside implied do */

	if ( !(cup->ufmt) ) {	/* If unformatted */
#if	NUMERIC_DATA_CONVERSION_ENABLED
		if (cup->unumcvrt || cup->ucharset) {

			/*
			 * All items must be the same type if doing data 
			 * conversion.
			 */
			if (!sametp)
				return(0);

			*retp	= _get_dc_param(css, cup, ts, &tip);

			if (*retp != 0)
				return(1);
		}
#endif
	}
	else {
		/*
		 * All items must be the same type if doing formatted
		 * or list-directed I/O.
		 */
		if (!sametp)
			return(0);
	}

/*
 *	This loop iterates through the "stp" array of iolist items, transfering
 *	data between the iolist items and the packing buffer and issuing
 *	lower level I/O requests with buffers full of contiguous data.
 */
	for (ib = 0; ib < dotrips; ib += trips_per_buf) {
		register long	t;

		if (trips_per_buf > dotrips - ib)
			trips_per_buf	= dotrips - ib;

		tip.count	= ioitems * trips_per_buf;

		if (reading) {	/* If reading */

			/*
			 * Fill up locbuf with data from a call to the func
			 * read routine.  Then distribute the data to all the
			 * iolist items in the implied do.
			 */

			*retp	= func(css, cup, locbuf, &tip, PARTIAL);
		}

		/*
		 * Assume that loopvar is one word on all supported 
		 * architecurs.
		 */
		assert ( sizeof(*loopvar) == sizeof(_f_int) );

		for (item = 0; item < ioitems; item++) {

		    if (slt[item].elstr && tip.elsize == sizeof(int)) {

			int	*wptr;
			int	*wbuf = (int *)locbuf;

			inc	= slt[item].inc;
			wptr	= ((int *)slt[item].saddr) + ib * inc;

			if (reading) {	/* If reading */
#ifdef	_CRAY1
#pragma _CRI ivdep
#endif
			    for (t = 0; t < trips_per_buf; t++)
				wptr[t * inc]	= wbuf[item + t*ioitems];
			}
			else {
			    /*
			     * Special case when the loop variable is being
			     * printed (this cannot occur on input).
			     */
			    if (wptr == (int *)loopvar) {
			        for (t = 0; t < trips_per_buf; t++) {
				  wbuf[item + t * ioitems] =
					*loopvar + loopinc * t;
			        }
			    }
			    else {
#ifdef	_CRAY1
#pragma _CRI ivdep
#endif
			        for (t = 0; t < trips_per_buf; t++)
				  wbuf[item + t * ioitems]	= wptr[t * inc];
			    }
			}
		    }
		    else {
			char		*dptr;
			register long	binc; 

			binc	= slt[item].binc;
			lbptr	= (char *)locbuf + tip.elsize * item;
			dptr	= ((char *)slt[item].saddr) + ib * slt[item].binc;

			for (t = 0; t < trips_per_buf; t++) {

			    if (reading)
				(void) memcpy(dptr, lbptr, tip.elsize);
			    else
				(void) memcpy(lbptr, dptr, tip.elsize);

			    dptr 	+= binc;
			    lbptr	+= ioitems * tip.elsize;
			}
		    }
		}

		if (!reading) {		/* If writing */

			/*
			 * Now write out the data buffered in locbuf.
			 */

			*retp	= func(css, cup, locbuf, &tip, PARTIAL);
		}

		*loopvar	+= loopinc * trips_per_buf;

		if (*retp != 0)
			return(1);		/* error/end/eor condition */
	}

	return(1);
}
