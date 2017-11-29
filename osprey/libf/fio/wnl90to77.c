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



#pragma ident "@(#) libf/fio/wnl90to77.c	92.2	06/21/99 10:37:55"

#include <stdio.h>
#include <errno.h>
#include <cray/nassert.h>
#include <liberrno.h>
#include "fio.h"
#include "namelist.h"
#include "wnl90def.h"

/*
 *	_wnl90to77 - called by library routine wnl90.c to write a cf77
 *			namelist output file.
 *
 *	Synopsis
 *		int _wnl90to77( css,
 *				unit *cup,
 *				nmlist_group *namlist,
 *				void *stck,
 *				int errf);
 *
 *		Where
 *			css	pointer to css
 *			cup	pointer to the unit information
 *			namlist pointer to the namelist table.
 *			stck	pointer to stack space which is passed
 *				to each call to _FWN for a particular
 *				statement.  This is used by the library.
 *			errf	Error processing flag.
 *
 *	Return value
 *		errn
 */

int
_wnl90to77(
	FIOSPTR		css,
	unit		*cup,
	nmlist_group	*namlist,
	void		*stck,
	int		errf)
{
	register int	errn;		/* Error number			*/
	char		*wptr;		/* pointer to group name	*/
	unsigned long	wlen;		/* group name length		*/
	unsigned 	wcount;		/* count of namelist items	*/
	int		icnt;
	char		*varptr;	/* ptr to group_obj_list_item	*/
	unsigned long	varlen;		/* len to group_obj_list_item	*/
	nmlist_goli_t	*nlvar;		/* ptr to next variable entry 	*/
	long		eqlchr;		/* hold nl equal character	*/
	long		sepchr;		/* hold nl delimiter character	*/
	long		nlchr;		/* hold nl group character	*/
	long		trmchr;		/* hold nl terminator character	*/
	int		trmsize;	/* size of terminator character */

/****************************************************************************
 *	Data Transfer Section
 ***************************************************************************/

	errn	= 0;
	wcount	= namlist->icount;		/* count of list items 	*/

	/* set up one set of variables to use where f77 mode */

	eqlchr	= OUT_EQ;
	sepchr	= OUT_SEP;
	nlchr	= OUT_CHAR;
	trmchr	= OUT_CHAR; 
	trmsize	= 6;
	NLCHAR(OUT_ECHO);		/* blank or echo char	*/
	NLCHAR(nlchr);			/* ampersand or WNLDELM	*/

	wptr	= _fcdtocp(namlist->group_name); /* ptr to groupname	*/
	wlen	= _fcdlen(namlist->group_name);	/* len of groupname	*/

	/* If length of groupname exceeds recl, put out an error */

	if ((wlen + 4) > cup->unmlsize) {
		errn	= FENLNMSZ;
		ERROR0(errf, css, errn);
	}

	/* Move namelist group name to output buffer	*/

	for (icnt = 0; icnt < wlen; icnt++) {
		*cup->ulineptr++	= *wptr++;
		cup->ulinemax++;
	}

	NLCHAR(' ');			/* write blank		*/
	NLCHAR(' ');			/* write blank		*/

	/* WNLLINE specifies one variable per record		*/

	NLINE();			/* new line		*/

	nlvar	= namlist->goli;		/* group object pointer */

	while (wcount-- && (errn == 0)) {
		varptr	= _fcdtocp(nlvar->goli_name);
		varlen	= _fcdlen(nlvar->goli_name);

		/* If length of variable name exceeds recl, issue an err */

		if (varlen > cup->unmlsize) {
			/* error: group object name too big for rec size */
			errn	= FENLNMSZ;
			ERROR0(errf, css, errn);
		}
		else
			if (varlen > (cup->unmlsize - cup->ulinemax)) {
				NLWFLUSH();
				NLCHAR(' ');		/* write blank	*/
				NLCHAR(' ');		/* write blank	*/
			}

		/* Write namelist group object name to output buffer */

		for (icnt = 0; icnt < varlen; icnt++) {
			*cup->ulineptr++	= varptr[icnt];
			cup->ulinemax++;
		}

		/* Flush output buffer if blank=blank will not fit */

		if ((cup->unmlsize - cup->ulinemax) < 3) {
			NLWFLUSH();
			NLCHAR(' ');			/* write blank	*/
		}

		/* Write equal size or replacement character after name */

		NLCHAR(' ');			/* write blank		*/
		NLCHAR(eqlchr);			/* write equal sign	*/
		NLCHAR(' ');			/* write blank		*/

		/* Setting ldwinit is needed before first call to _ld_write */

		css->u.fmt.u.le.ldwinit = 1;

		/* Write the value of the namelist group object */

		switch (nlvar->valtype) {

		case IO_SCALAR:
		{
			void		*vaddr;
			type_packet	tip;	/* Type information packet */
			nmlist_scalar_t *nlscalar; /* nmlist scalar entry */

			nlscalar	= nlvar->goli_addr.ptr;
			tip.type90	= nlscalar->tinfo.type;
			tip.type77	= -1;
			tip.intlen	= nlscalar->tinfo.int_len;
			tip.extlen	= tip.intlen;
			tip.elsize	= tip.intlen >> 3;
			tip.cnvindx	= 0;
			tip.count	= 1;
			tip.stride	= 1;

			/* Assertions */

			assert (tip.type90 >= DVTYPE_TYPELESS &&
				tip.type90 <= DVTYPE_ASCII);
			assert (tip.intlen > 0);

			if (tip.type90 == DVTYPE_ASCII) {
				vaddr		= _fcdtocp(nlscalar->scal_addr.charptr);

				tip.elsize	= tip.elsize *
						_fcdlen(nlscalar->scal_addr.charptr);
			}
			else
				vaddr	= nlscalar->scal_addr.ptr;

			/* Do not allow double complex for 77 mode */

			if ((tip.type90 == DVTYPE_COMPLEX &&
			     tip.elsize == (sizeof(_f_dble) * 2)))
				errn	= FENLDBCP;
			else /* Use list-directed write */
				errn	= _ld_write(css, cup, vaddr, &tip, 0);

			break;
		}

		case IO_DOPEVEC:
		{
			register short	nc;
			register long	extent;
			void		*vaddr;
			type_packet	tip;
			DopeVectorType	*nldv;

			nldv	= nlvar->goli_addr.dv; /* ptr to dope vec */

			/* Assertions */

			assert (nldv != NULL);
			assert (nldv->type_lens.int_len > 0);

			tip.type90	= nldv->type_lens.type;
			tip.type77	= -1;
			tip.intlen	= nldv->type_lens.int_len;
			tip.extlen	= tip.intlen;
			tip.elsize	= tip.intlen >> 3;
			tip.cnvindx	= 0;
			tip.stride	= 1;

			if (tip.type90 == DVTYPE_ASCII) {
				vaddr		= _fcdtocp(nldv->base_addr.charptr);
				tip.elsize	= tip.elsize *
						_fcdlen(nldv->base_addr.charptr);
			}
			else
				vaddr	= nldv->base_addr.a.ptr;

			extent	= 1;

			for (nc = 0; nc < nldv->n_dim; nc++)
				extent	= extent * nldv->dimension[nc].extent;

			tip.count	= extent;

			/* Assertions */

			assert (tip.elsize > 0 && extent > 0);

			/* Do not allow double complex for 77 mode */

			if ((tip.type90 == DVTYPE_COMPLEX &&
			     tip.elsize == (sizeof(_f_dble) * 2)))
				errn	= FENLDBCP;
			else /* Use list-directed write */
				errn	= _ld_write(css, cup, vaddr, &tip, 0);

			break;
		}

		case IO_STRUC_A:
		case IO_STRUC_S:
		{
			/* Do not allow structures for 77 mode */
			errn	= FENLSTCT;
		}

		default:
			errn	= FEINTUNK;	/* Internal error */
		} /* switch */

		if (errn != 0) {
			ERROR0(errf, css, errn);
		}

		/* Flush out last item to record buffer */

		errn	= _ld_write(css, cup, (void *) NULL, &__tip_null, 0);

		if (errn != 0) {
			ERROR0(errf, css, errn);
		}

		if (wcount > 0) {
			if ((cup->unmlsize - cup->ulinemax) < 2) {
				NLWFLUSH();
				NLCHAR(' ');	/* write delimiter */
				NLCHAR(' ');	/* write delimiter */
				css->u.fmt.u.le.ldwinit = 1;/* suppress comma */
			}
			else {
				NLCHAR(sepchr);	/* write comma */
				NLCHAR(' ');	/* write delimiter */
				css->u.fmt.u.le.ldwinit = 1;/* suppress comma */
				NLINE();	 /* new line	*/
			}
		}

#if	defined(__mips) && (_MIPS_SZLONG == 32)
		nlvar	= (nmlist_goli_t*)((long *)nlvar + 3 +
				(sizeof(_fcd))/(sizeof(long)));
#else
		nlvar	= (nmlist_goli_t*)((long *)nlvar + 2 +
				(sizeof(_fcd))/(sizeof(long)));
#endif
	}

	if (cup->ulinemax > 2) {
		NLINE();	 /* new line	*/
	}

	if ((cup->unmlsize - cup->ulinemax) < trmsize) {
		NLWFLUSH();
		NLCHAR(' ');			/* write blank	*/
	}

	/* CF77 ends namelist with compat character (does accept slash) */

	NLCHAR(' ');			/* write blank	*/
	NLCHAR(trmchr);			/* write ending slash	*/
	NLCHAR('E');			/* write END		*/
	NLCHAR('N');
	NLCHAR('D');
	NLWFLUSH();

	if (errn != 0)
		cup->uflag	= cup->uflag | _UERRC;	/* Set error status */

/****************************************************************************
 *
 *	Statement Finalization Section
 *
 ***************************************************************************/
finalization:
	return(errn);
}
