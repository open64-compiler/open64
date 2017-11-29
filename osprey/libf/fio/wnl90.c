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



#pragma ident "@(#) libf/fio/wnl90.c	92.3	10/12/99 13:16:22"

#include <stdio.h>
#include <errno.h>
#include <cray/nassert.h>
#include <liberrno.h>
#include "fio.h"
#include "namelist.h"
#include "wnl90def.h"

int	_nlstrent(FIOSPTR css, unit *cup, nmlist_goli_t *nalist,
		int count, int errf, int bytofset);

int	_wnl90to77(FIOSPTR css, unit *cup, nmlist_group *namlist,
		void *stck, int errf);

/*
 *	_FWN	- called by compiled Fortran programs to process a namelist 
 *		  WRITE statement.
 *
 *	Synopsis
 *
 *		int _FWN(	ControlListType *cilist,
 *				nmlist_group *namlist,
 *				void *stck);
 *
 *		Where
 *
 *			cilist	- pointer to the control information list
 *				  information.  This describes the specifiers
 *				  for the current I/O statement.
 *			iolist	- pointer to the namelist table.
 *			stck	- pointer to stack space which is passed
 *				  to each call to _FRU for a particular
 *				  statement.  This is used by the library.
 *
 *	Return value
 *
 *		IO_OKAY, IO_END, or IO_ERR
 */

int
_FWN(ControlListType *cilist, nmlist_group *namlist, void *stck)
{
	int		errf;		/* Error processing flag	*/
	int		errn;		/* Error number			*/
	unum_t		unum;		/* Actual unit number		*/
	unit		*cup;		/* Pointer to unit table entry	*/
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
	FIOSPTR		css;

/*
 *	Assertions
 */
	/* Validate that the size of *stck is large enough */

	assert (cilist->stksize >= sizeof(struct fiostate)/sizeof(long));

	/* The compiler flags namelist with fmt flag */

	assert ((cilist->fmt == CI_NAMELIST));

	/* The compiler disallows namelist with internal files */

	assert(!(cilist->internal && cilist->fmt == CI_NAMELIST));

	/* The compiler disallows namelist with direct files */

	assert(!(cilist->dflag && cilist->fmt == CI_NAMELIST));

	css	= stck;
	errn	= 0;

/****************************************************************************
 *
 *	Statement Initialization Section
 *
 ***************************************************************************/

	/* Establish error processing options */

	errf	= (cilist->errflag || cilist->iostatflg);

	if (cilist->uflag == CI_UNITASTERK)
		unum	= STDOUT_U;
	else
		unum	= *cilist->unit.wa;

	STMT_BEGIN(unum, 0, T_WNL, NULL, css, cup);

	if (cup == NULL) {	/* If not connected */

		cup	= _imp_open(css, SEQ, FMT, unum, errf, &errn);

		/*
		 * If the open failed, cup is NULL and errn contains
		 * the error number.
		 */
		if (cup == NULL)
			goto finalization;
	}

	/* All paths which lead here have set cup to a non-null value */

	assert ((cup != NULL));		/* cup assumed non-NULL */

/*
 *	Copy the user's error processing options into the unit table
 */
	cup->uflag	= (cilist->errflag		?  _UERRF : 0) |
			  (cilist->iostat_spec != NULL	? _UIOSTF : 0);

	css->u.fmt.nonadv	= 0;

	/* If trying to write a file without write permission */

	if ((cup->uaction & OS_WRITE) == 0) {
		errn	= FENOWRIT;	/* No write permission */
		ERROR0(errf, css, errn);
	}

	/* If attempting formatted I/O on an unformatted file */

	if (!cup->ufmt) {
		errn	= FEFMTTIV;	/* Formatted not allowed */
		ERROR0(errf, css, errn);
	}

	/* Initialize fields in the Fortran statement state structure */

	css->u.fmt.icp		= NULL;
	css->u.fmt.nonl		= 0;

	if (cup->useq == 0) {	/* seq-io attempted on direct file */
		errn	= FESEQTIV;	/* Sequential not allowed */
		ERROR0(errf, css, errn);
	}

	/* external sequential formatted I/O */

	if (cup->uend != BEFORE_ENDFILE) {
		/*
		 * If positioned after an endfile, and the file does not
		 * support multiple endfiles, a write is invalid.
		 */
		if (!cup->umultfil) {
			errn	= FEWRAFEN;
			ERROR0(errf, css, errn);
		}
		/*
		 * If a logical endfile record had just been read, replace
		 * it with a physical endfile record before starting the
		 * current data record.
		 */
		if (cup->uend == LOGICAL_ENDFILE) {
			if (XRCALL(cup->ufp.fdc, weofrtn)cup->ufp.fdc,
				&cup->uffsw) < 0) {
					errn	= cup->uffsw.sw_error;
					ERROR0(errf, css, errn);
			}
		}
		cup->uend	= BEFORE_ENDFILE;
	}

	/*
	 * Set up record size.  The hierarchy for determining Namelist
	 * output record size is as follows:
	 *	1) RECL, if specified
	 *	2) WNLLONG(), if set and does not exceed cup->urecsize
	 *	3) list-directed output record size (cup->uldwsize)
	 *
	 * Note that while (1) and (3) are established at OPEN time, (2)
	 * can be changed ``on the fly''; therefore, this check has to
	 * be performed here.
	 */

	cup->unmlsize	= cup->uldwsize;

	if (cup->urecl == 0 && _wnlrecsiz > 0) {
		/* RECL is not present but WNLLONG() set */
		if (cup->uft90)
			cup->unmlsize	= cup->urecsize;
		else {
			cup->unmlsize	= MIN(cup->urecsize, _wnlrecsiz);
		}
	}

	if (cup->pnonadv && cup->uwrt == 0) {
		/*
		 * A formatted or list-directed write statement
		 * follows a nonadvancing read.  Switch the 
		 * current line (record) from read to write 
		 * mode.  Then backspace the file so the 
		 * current record gets written back in place.
		 */

		int cur_offset;
		cur_offset = cup->ulineptr - cup->ulinebuf;

		cup->ulinemax	= cur_offset + cup->ulinecnt;
		cup->ulinecnt	= cur_offset;
		cup->uflshptr	= cup->ulinebuf;

		errn	= _unit_bksp(cup);

		if (errn != 0) {
			ERROR0(errf, css, errn);
		}
	}
	else if (cup->pnonadv == 0) {
		/* 
		 * There is no current record (due to a prior
		 * nonadvancing read or write).  Initialize
		 * the empty line buffer.
		 */ 
		cup->ulinemax	= 0;	/* Highwater mark */
		cup->ulineptr	= cup->ulinebuf;
		cup->uflshptr	= cup->ulinebuf;
	}

	/*
	 * If there is a current record then truncate the current record at 
	 * the current position and flush it if the current record 
	 * is already beyond unmlsize.
	 */
	if (cup->pnonadv) {
		errn = _lw_after_nonadv(css, cup, cup->unmlsize, 1);
		if (errn != 0)
			ERROR0(errf, css, errn);
	}

	if (errn != 0) {
		ERROR0(errf, css, errn);
	}

	css->u.fmt.endrec	= _sw_endrec;
	cup->pnonadv 		= 0;
	cup->uwrt		= 1;		/* set write mode */

/****************************************************************************
 *
 *	Data Transfer Section
 *
 ***************************************************************************/

	assert ((cup != NULL));		/* cup assumed non-NULL */
	wcount	= namlist->icount;		/* count of list items 	*/

	/* set up one set of variables to use where f90 or f77 mode */

	if (!(cup->uft90)) {
		errn = _wnl90to77(css,cup,namlist,stck,errf);
		goto finalization;
	}
	eqlchr	= (long) '=';
	sepchr	= (long) ',';
	nlchr	= (long) '&';
	trmchr	= (long) '/';
	trmsize	= 3;
	/*
	 * WNLFLAG for echo not accepted for Fortran 90,
	 * use blank as first char
	 */
	NLCHAR(' ');			/* write blank		*/
	NLCHAR(nlchr);			/* write ampersand	*/

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

	nlvar	= namlist->goli;		/* group object pointer */

	while (wcount--) {
		varptr	= _fcdtocp(nlvar->goli_name);
		varlen	= _fcdlen(nlvar->goli_name);

		/* If length of variable name exceeds recl, put out an error */

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

		/* Flag needed for first call to _ld_write per variable */

		css->u.fmt.u.le.ldwinit = 1;

		/* Write the value of the namelist group object */

		switch (nlvar->valtype) {

		case IO_SCALAR:
		{
			nmlist_scalar_t *nlscalar; /* nmlist scalar entry */
			void		*vaddr;
			type_packet	tip;

			nlscalar	= nlvar->goli_addr.ptr; /* ptr to scalar */
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

			/* Use list-directed write */

			errn	= _ld_write(css, cup, vaddr, &tip, 0);

			if (errn != 0) {
				ERROR0(errf, css, errn);
			}

			/* Flush the list-directed output to the line buffer */

			errn	= _ld_write(css, cup, (void *) NULL,
					&__tip_null, 0);

			if (errn != 0) {
				ERROR0(errf, css, errn);
			}

			break;
		}

		case IO_DOPEVEC:
		{
			register short	nc;
			register long	extent;
			DopeVectorType	*nldv;
			void		*vaddr;
			type_packet	tip;

			nldv	= nlvar->goli_addr.dv; /* ptr to dope vector */

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

			/* Assertions */

			assert (tip.elsize > 0 && extent >= 0);

			/* Use list-directed write */

			tip.count	= extent;

			errn	= _ld_write(css, cup, vaddr, &tip, 0);

			if (errn != 0) {
				ERROR0(errf, css, errn);
			}

			/* Flush the list-directed output to the line buffer */

			errn	= _ld_write(css, cup, (void *) NULL,
					&__tip_null, 0);

			if (errn != 0) {
				ERROR0(errf, css, errn);
			}

			break;
		}

		case IO_STRUC_A:
		{
			register int	bytofset;
			register long	scount;
			nmlist_goli_t	*vaddr;
			nmlist_struclist_t *nlstruc; /* nmlist struc entry */

			nlstruc	= nlvar->goli_addr.sptr; /* ptr to struc */
			vaddr	= nlstruc->goli;	/* ptr to list	*/
			scount	= nlstruc->structlen;	/* number entries */

			/* Bytofset is zero for a scalar structure */

			bytofset	= 0;

			errn	= _nlstrent(css, cup, vaddr, scount, errf,
					bytofset);

			if (errn != 0) {
				ERROR0(errf, css, errn);
			}

			break;
		}

		case IO_STRUC_S:
		{
			register short	nc;
			register int	scount;
			register long	elsize;
			register long	extent;
			register long	ic;
			nmlist_goli_t	*vaddr;
			DopeVectorType	*nlsdv;
			nmlist_struclist_t *nlstruc; /* nmlist struc entry */

			nlstruc	= nlvar->goli_addr.sptr; /* ptr to struc */
			scount	= nlstruc->structlen;	/* number entries */
			vaddr	= nlstruc->goli;	/* ptr to list	*/
			nlsdv	= nlstruc->struc_addr.dv; /* ptr to dopevec */
			elsize	= nlsdv->base_addr.a.el_len;
			extent	= 1;

			for (nc = 0; nc < nlsdv->n_dim; nc++)
				extent	= extent * nlsdv->dimension[nc].extent;

			for (ic = 0; ic < extent; ic++) {
				register int	bytofset;
				/*
				 * bytofset is used when a structure is an
			 	 * array of structures.  Each component must
			 	 * add an offset to its base address after
			 	 * the first array element.  Must change
			 	 * bits to bytes
				 */
				bytofset	= (elsize >> 3) * ic;
				errn		= _nlstrent(css, cup, vaddr,
							scount, errf, bytofset);

				if (errn != 0) {
					ERROR0(errf, css, errn);
				}
			}
			break;
		}

		default:
			errn	= FEINTUNK;
			ERROR0(errf, css, errn);
		}

		/* Flush the list-directed output to the line buffer */

		errn	= _ld_write(css, cup, (void *) NULL, &__tip_null, 0);

		if (errn != 0) {
			ERROR0(errf, css, errn);
		}

                if (OUT_LINE) {
                        NLINE();
                        css->u.fmt.u.le.ldwinit = 1; /* suppress comma*/
                }
                else
		if (wcount > 0) {
			if ((cup->unmlsize - cup->ulinemax) < 2) {
				NLWFLUSH();
				NLCHAR(' ');	/* write delimiter */
				NLCHAR(' ');	/* write delimiter */
				css->u.fmt.u.le.ldwinit = 1; /* suppress comma*/
			}
			else {
				if (cup->ufcomsep == 0) {
					NLCHAR(sepchr);	/* write comma */
				}
				NLCHAR(' ');	/* write delimiter */
				css->u.fmt.u.le.ldwinit = 1; /* suppress comma*/
			}
		}
#if (defined(__mips) && (_MIPS_SZLONG == 32)) || (defined(_LITTLE_ENDIAN) && !defined(_LP64))
		nlvar	= (nmlist_goli_t*)((long *)nlvar + 3 +
				(sizeof(_fcd))/(sizeof(long)));
#else
		nlvar	= (nmlist_goli_t*)((long *)nlvar + 2 +
				(sizeof(_fcd))/(sizeof(long)));
#endif
	}

	if ((cup->unmlsize - cup->ulinemax) < trmsize) {
		NLWFLUSH();
		NLCHAR(' ');			/* write blank	*/
	}

	/*
	 * Fortran 90 ends namelist with slash or compat character
	 */

	NLCHAR(' ');			/* write blank	*/
	NLCHAR(trmchr);			/* write ending slash	*/

	NLWFLUSH();

	if (errn != 0)
		cup->uflag	= cup->uflag | _UERRC;	/* Set error status */

/****************************************************************************
 *
 *	Statement Finalization Section
 *
 ***************************************************************************/
finalization:

	/* Set IOSTAT variable to 0 if no error, >0 error code otherwise */

	if (cilist->iostat_spec != NULL)
		*cilist->iostat_spec	= errn;

	/* End the Beguine */

	STMT_END(cup, TF_WRITE, NULL, css);	/* Unlock unit */

	/* Return proper status */

	if (errn == 0)
		return(IO_OKAY);
	else
		return(IO_ERR);
}

/*
 *	_nlstrent - namelist output of structure entries
 *		Recursive call to handle structure table entries for
 *		namelist.  This code is not used for a file that has
 *		cf77 compatibility mode turned on.
 *	Return value:
 *		0 on success.
 *		>0 error code if error encountered
 */
int
_nlstrent(
	FIOSPTR	css,
 	unit		*cup,
 	nmlist_goli_t	*nalist,
 	int		count,
 	int		errf,
 	int		bytofset)
{
	register int	errn;		/* error number			*/
	register int 	scnt;		/* count of namelist struc items */
	nmlist_goli_t	*nlvar;		/* ptr to next var entry	*/

	scnt	= count;
	errn	= 0;
	nlvar	= nalist;		/* group object pointer */

	while (scnt-- && (errn == 0)) {

		switch (nlvar->valtype) {

		case IO_SCALAR:
		{
			nmlist_scalar_t *nlscalar; /* nmlist scalar entry */
			void		*vaddr;
			type_packet	tip;

			nlscalar	= nlvar->goli_addr.ptr; /* ptr to scalar */
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
				vaddr		= _fcdtocp(nlscalar->scal_addr.charptr) +
							bytofset;
				tip.elsize	= tip.elsize *
						_fcdlen(nlscalar->scal_addr.charptr);
			}
			else {
				register int	adj;

				if (bytofset > 0)
					adj	= bytofset / (sizeof(_f_int));
				else
					adj	= 0;

				vaddr	= (_f_int *) nlscalar->scal_addr.ptr +
						adj;
			}

			/* Use list-directed write */

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

			nldv	= nlvar->goli_addr.dv; /* ptr to dope vector */

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
				vaddr		= _fcdtocp(nldv->base_addr.charptr) +
						bytofset;
				tip.elsize	= tip.elsize *
						_fcdlen(nldv->base_addr.charptr);
			}
			else {
				register int	adj;

				if (bytofset > 0)
					adj	= bytofset/(sizeof(_f_int));
				else
					adj	= 0;

				vaddr	= (_f_int *) nldv->base_addr.a.ptr +
						adj;
			}

			extent	= 1;

			for (nc = 0; nc < nldv->n_dim; nc++)
				extent	= extent * nldv->dimension[nc].extent;

			/* Assertions */

			assert (tip.elsize > 0 && extent > 0);

			/* Use list-directed write */

			tip.count	= extent;

			errn	= _ld_write(css, cup, vaddr, &tip, 0);

			break;
		}

		case IO_STRUC_A:
		{
			register int	scount;
			nmlist_struclist_t *nlstruc; /* nmlist struc entry */
			nmlist_goli_t	*vaddr;

			nlstruc	= nlvar->goli_addr.sptr; /* ptr to struc */
			scount	= nlstruc->structlen;	/* number entries */
			vaddr	= nlstruc->goli;	/* ptr to list	*/

			/*
			 * No additional offset needed, pass current offset
			 * on to next version.
			 */

			errn	= _nlstrent(css, cup, vaddr, scount, errf,
					bytofset);
			break;
		}

		case IO_STRUC_S:
		{
			register short	nc;
			register int	scount;
			register long	elsize;
			register long	extent;
			register long	ic;
			nmlist_struclist_t *nlstruc; /* nmlist struc entry */
			nmlist_goli_t	*vaddr;
			DopeVectorType	*nlsdv;

			nlstruc	= nlvar->goli_addr.sptr; /* ptr to struc */
			scount	= nlstruc->structlen;	/* number entries */
			vaddr	= nlstruc->goli;	/* ptr to list	*/
			nlsdv	= nlstruc->struc_addr.dv; /* ptr to dopevec */

			/*
			 * bytofset is used when the structure is an array
			 * of structures.  Each element must add an offset
			 * to its address after the first array element.
			 */

			elsize	= nlsdv->base_addr.a.el_len;
			extent	= 1;

			for (nc = 0; nc < nlsdv->n_dim; nc++)
				extent	= extent * nlsdv->dimension[nc].extent;

			for (ic = 0; ic < extent; ic++) {
				register int	bytoff;
				/*
				 * create another byte offset for this
				 * nesting of a structure of arrays.  Must
				 * change elsize from bits to bytes.
				 */
				bytoff	= bytofset + ((elsize >> 3) * ic);

				errn	= _nlstrent(css, cup, vaddr, scount,
						errf, bytoff);
			}
			break;
		}

		default:
			errn	= FEINTUNK;
		} /* switch */

		if (errn !=0)
			return(errn);

		/* Flush the list-directed output to line buffer */

		errn	= _ld_write(css, cup, (void *) NULL, &__tip_null, 0);

		if (errn != 0)
			return(errn);

		if (scnt > 0) {
			if ((cup->unmlsize - cup->ulinemax) < 2) {
				NLWFLUSH();
				NLCHAR(' ');		/* write delimiter */
				NLCHAR(' ');		/* write delimiter */
				css->u.fmt.u.le.ldwinit = 1; /* suppress comma*/
			}
			else {
				NLCHAR(',');		/* write delimiter */
				NLCHAR(' ');		/* write delimiter */
				css->u.fmt.u.le.ldwinit = 1; /* suppress comma*/
			}
		}
#if (defined(__mips) && (_MIPS_SZLONG == 32)) || (defined(_LITTLE_ENDIAN) && !defined(_LP64))
		nlvar	= (nmlist_goli_t *)((long *)nlvar + 3 +
			(sizeof(_fcd))/(sizeof(long)));
#else
		nlvar	= (nmlist_goli_t *)((long *)nlvar + 2 +
			(sizeof(_fcd))/(sizeof(long)));
#endif
	}

finalization:
	return(errn);
}
