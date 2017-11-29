/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/ffio/_ffopen.c	92.4	10/29/99 21:40:31"

#include <errno.h>
#include <fcntl.h>
#include <ffio.h>
#ifdef	_UNICOS
#include <infoblk.h>
#endif

#include <stdlib.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <cray/assign.h>

#if defined(_UNICOS) || defined(__mips) || defined(_LITTLE_ENDIAN)
#include "fflock.h"
#endif

#include <cray/mtlock.h>

#ifdef _ABSOFT
#include "ac_sysdep.h"
#endif


void _czero_asninfo( assign_info *aip, assign_info *attrused, int depth);
void _fold_asninfo( assign_info *aip, assign_info *attrused, int depth);
int _ffclass_sys_interface(int class);
int _ffclass_unsup_oflags(int class, int flags, int *errn);

#if defined(_CRAY1) || defined(__mips) || defined(_LITTLE_ENDIAN)
static int
setup_lock_lyr(struct fdinfo *fio, struct fdinfo **nfio, struct ffsw *stat);
#endif

/*
 *	_ffopen
 *
 *	Indirectly recursive routine which opens each FFIO layer in an
 *	FFIO specification list.
 *
 *	A linked chain of fdinfo blocks, one per FFIO layer,  is created for 
 *	the FFIO file being opened.  This routine (as well as ffopen and 
 *	ffopens) returns a pointer to the top level fdinfo block.
 *
 *	As an example, the call sequence for the following layer list:
 *
 *		cos,cache,syscall
 *
 *	is:
 *		_ffopen	calls _cos_open
 *			_cos_open calls _ffopen
 *				_ffopen calls _cch_open
 *					_cch_open calls _ffopen
 *						_ffopen calls _sys_open
 *
 */

#ifdef _UNICOS
#pragma _CRI soft ffiolock
extern int ffiolock();
#endif

/*	
 *	This duplicate name is used only by CRI applications which define
 *	user or site FFIO layers.
 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#pragma _CRI duplicate _ffopen as __ffopen
#elif defined(__mips)
#pragma weak _ffopen=__ffopen
#elif defined(_LITTLE_ENDIAN)
extern _ffopen_t __ffopen(const char *name, int flags, mode_t mode, union spec_u *spek, struct ffsw *stat, long cbits, int cblks, struct fdinfo *pa_fio, struct gl_o_inf *oinf);
# if defined(BUILD_OS_DARWIN)
/* Mach-O doesn't support aliases */
_ffopen_t _ffopen(const char *name, int flags, mode_t mode, union spec_u *spek,
  struct ffsw *stat, long cbits, int cblks, struct fdinfo *pa_fio,
  struct gl_o_inf *oinf) {
  return __ffopen(name, flags, mode, spek, stat, cbits, cblks, pa_fio, oinf);
}
# else /* defined(BUILD_OS_DARWIN) */
_ffopen_t _ffopen() __attribute__ ((weak, alias ("__ffopen")));
# endif /* defined(BUILD_OS_DARWIN) */
#endif

_ffopen_t
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
__ffopen(
#else
_ffopen(
#endif
const char	*name,		/* file name */
int		flags,		/* flags for open(2) */
mode_t		mode,		/* mode for open(2) */
union spec_u	*spek,		/* pointer to current FFIO layer in the chain */
struct ffsw	*stat,		/* return status */
long		cbits,		/* cbits for open(2) */
int		cblks,		/* cblks for open(2) */
struct fdinfo	*pa_fio,	/* preallocated fdinfo structure, or NULL */
struct gl_o_inf	*oinf)		/* global open information for this file */
{
	extern struct xtr_s *_recfm_tab[NUM_CLASSES];
	struct fdinfo	*fio;
	union spec_u	*nxtspec;
	union spec_u	*fdspec;
	int		narg;
	_ffopen_t	nextfio;
	int		errn;
	int		lcblks;
	int		reuse_fio;
	union spec_u	zero;
	struct gl_o_inf	*loinf;
	long		acopy[(AFLAGSIZE + sizeof(long)) / sizeof(long)];
	struct ffsw	locstat;
	int		ret;
	struct ffc_info_s ffci;

#ifdef	_UNICOS
	NUMARG(narg);
#else
	narg = 9;
#endif

	lcblks = 0;
	if (narg >= 7)		/* for compatibility w/ 7.0 user/site layers */
		lcblks = cblks;

	fio = NULL;
	if (narg >= 8)		/* for compatibility w/ 7.0 user/site layers */
		fio = pa_fio;

	loinf = NULL;
	if (narg >= 9)		/* for compatibility w/ 7.0 user/site layers */
		loinf = oinf;

/*
 *	Increment the current FFIO layer nesting depth.
 */
	if (loinf != NULL)
		loinf->depth++;

/*
 *	If a NULL pointer is passed in as the fdspec, make it point
 *	to a NULL spec instead.
 */
	fdspec = spek;
	if (fdspec == (union spec_u *)NULL) {
		fdspec = (union spec_u *)&zero;
		fdspec->wword = 0;
	}
/*
 *	Skip any NULL layers... 
 */
	while (fdspec->fld.class == CLASS_NULL) {
		NEXT_SPEC(fdspec);
	}
/*
 *	Check to see if the layer is loaded, if not, issue error.
 */
	if (!(LOADED_DATA(_recfm_tab[fdspec->fld.class])))
		_FFOPEN_ERETURN(stat, FDC_ERR_DISABL, 0);

/*
 *	Check that the open flags are supported by the current layer.
 */
	if (_ffclass_unsup_oflags(fdspec->fld.class, flags, &errn) != 0)
		_FFOPEN_ERETURN(stat, errn, 0);


	if (fio != NULL) {
		reuse_fio = 1;
	}
	else {
		reuse_fio = 0;
		fio = malloc(sizeof(*fio));
		if (fio == NULL) {
			/* OUT OF MEMORY */
			_FFOPEN_ERETURN(stat, FDC_ERR_NOMEM, 0);
		}
	}
	memset(fio, 0, sizeof(*fio));

/*
 *	Initialize the fdinfo structure.  Put in as much information as
 *	possible without knowing anything about the specific layer.
 */
	fio->magic	= MAGIC_ID;
	fio->class	= fdspec->fld.class;
	fio->rtype	= fdspec->fld.recfmt;
	fio->subtype	= fdspec->fld.subtype;
	fio->opn_flags	= flags;
	fio->xr		= *_recfm_tab[fdspec->fld.class];
	fio->lock	= 1;	/* assume the layer is not multi-task safe */
	fio->lock_word	= NULL;	/* If the layer wants us to use its lock, */
				/* then it will store a value here */

	if (fdspec->wword == 0)
		nextfio = 0;
	else if (_ffclass_sys_interface(fdspec->fld.class)) {
/*
 *		The current layer must be the last layer in the chain.
 *
 *		If the next layer is non zero (other than 'null', 'fd' or 'end')
 *		then error.
 */
		nxtspec =fdspec;
		NEXT_SPEC(nxtspec);

		if (nxtspec->wword == 0) {
			/* OK, this is real end. All is well.  Fall through. */
		}
		else if (nxtspec->fld.class == CLASS_FD) {
/*
 *			If ownopen was CLASS_FD and nxtspec is also CLASS_FD,
 *			or if nxtspec is not END, then error.
 */
			if (nxtspec->wword != 0 &&
			    fdspec->fld.class == CLASS_FD) {
				free((char *)fio);
				_FFOPEN_ERETURN(stat, FDC_ERR_CHAIN, 0);
			}
		}
		else {
			free((char *)fio);
			_FFOPEN_ERETURN(stat, FDC_ERR_CHAIN, 0);
		}
	}

/*
 *	Watch some special assign options which must be handled by all layers
 *	in the chain.
 */
	if (loinf != NULL){
		memset(acopy, 0, sizeof(acopy));
		_czero_asninfo(loinf->aip, (assign_info *)&acopy, loinf->depth);
	}

/*
 *	Do layer specific opening tasks.
 */
	nextfio = XRCALL(fio,openrtn) name, flags, mode, fio, fdspec, stat,
				      cbits, lcblks, loinf);
	if (nextfio == _FFOPEN_ERR) {
		free((char *)fio);
		return(_FFOPEN_ERR);
	}

/*
 *	Fold together info about assign options which were handled by 
 *	layers below with info about assign options handled by layers above
 *	the current position in the chain.
 */
	if (loinf != NULL)
		_fold_asninfo(loinf->aip, (assign_info *)&acopy, loinf->depth);

	fio->fioptr = (struct fdinfo *)nextfio;
	if (nextfio != 0)
		((struct fdinfo *)nextfio)->parptr = fio;
	ret = XRCALL(fio, fcntlrtn) fio, FC_GETINFO, &ffci, &locstat);
	fio->can_listio = 0;	
	if (ret >= 0) 
		{
		if (ffci.ffc_flags & FFC_CANLISTIO)
			fio->can_listio = 1;	
		}
#if defined(_CRAY1) || defined(__mips)
	if (MULTI_ON)
		{
		struct fdinfo	*nfio;
		ret = setup_lock_lyr(fio, &nfio, stat);
		if (ret < 0)
			{
			(void)XRCALL(fio, closertn) fio, &locstat);
			return(_FFOPEN_ERR);
			}
		if (nfio != NULL)
			{
			if (reuse_fio)
				{
				(void)XRCALL(fio, closertn) fio, &locstat);
				_FFOPEN_ERETURN(stat, FDC_ERR_MTLOCK, 0);
				}
			fio->parptr = nfio;
			fio = nfio;
			}
		}
	else
#endif
		fio->lock = 0;

/*
 *	Decrement the current FFIO layer nesting depth before normal return.
 */
	if (loinf != NULL)
		loinf->depth--;

	if (reuse_fio)
		return (nextfio);
	else
		return ((_ffopen_t)fio);
}

/*
 *	_next_spec
 *
 *	Finds the start of the spec for the underlying FFIO layer.  This 
 *	routine is typically called by system and user layer open routines 
 *	before calling the underlying _ffopen().
 *
 *	NOTE:
 *
 *	Changing the basic structure of the spec words would introduce
 *	an incompatibility with this same logic in the NEXT_SPEC
 *	macro from ffio.h in:
 *
 *		UNICOS 8.2 and previous on the YMP/C90
 *		PE 1.1.1 and previous on the T3D
 *
 *	This is because the guts of the _next_spec function used to live in 
 *	the NEXT_SPEC macro.
 */

union spec_u *
_next_spec(union spec_u *sp)
{
	do {
		while (sp->info.ext)
			sp++;
		sp++;
	} while(sp->fld.class == CLASS_NULL);
	return(sp);
}



struct class_info {
	int	unsup_oflags;		/* mask of open(2) flags never 
					 * supported by this layer. */
	unsigned int sys_interface :1;	/* 1 if this layer has its own
					 * system interface. */
};


#if !defined (_CRAY1) && !defined(_CRAYMPP)
#define O_SSD	0
#endif

/*
 *	The _ff_class_info does not include entries for the userX layers.
 *	This is OK, because the class number is range-checked before being
 *	used to index into this table.
 */
struct class_info _ff_class_info[] = {
        { 0			, _TRUE  }, 	/* end */
        { 0			, _TRUE  }, 	/* syscall */
        { 0			, _FALSE },	/* NULL */
        { 0			, _TRUE  }, 	/* system */
        { O_APPEND | O_SSD	, _FALSE },	/* cos */
        { O_APPEND | O_SSD	, _TRUE  }, 	/* tape */
        { O_APPEND | O_SSD	, _FALSE },	/* F */
        { O_APPEND | O_SSD	, _FALSE },	/* V */
        { O_APPEND | O_SSD	, _FALSE },	/* text */
        { O_APPEND | O_SSD	, _FALSE },	/* X */
        { O_APPEND | O_SSD	, _FALSE },	/* cdc */
        { O_APPEND | O_SSD	, _FALSE },	/* SDS */
        { O_APPEND | O_SSD	, _FALSE },	/* MR */
        { 0			, _FALSE },	/* trace */
        { 0			, _FALSE },	/* user */
        { 0			, _FALSE },	/* site */
        { 0			, _FALSE },	/* error */
        { O_APPEND		, _TRUE  }, 	/* FD */
        { 	     O_SSD	, _FALSE },	/* blankx */
        { O_APPEND | O_SSD	, _FALSE },	/* cache */
        { O_APPEND | O_SSD	, _TRUE  },	/* ER90 */
        { 	     O_SSD	, _FALSE },	/* buffering */
        { O_APPEND | O_SSD	, _FALSE },	/* cachea */
        { 0			, _FALSE },	/* event */
        { 0			, _FALSE },	/* lock */
	/* The global layer currently will not call any lower layers */
	/* and that's why its sysinterface is TRUE. */
        { 0			, _TRUE },	/* global */
        { O_APPEND | O_SSD	, _FALSE },	/* F77 */
};


/*
 *	Check for a system interface.
 *
 *	Return value:
 *		1	If the layer is a layer with its own system interface.
 *		0	otherwise.
 */

int
_ffclass_sys_interface(int class)
{
	if (class >= sizeof(_ff_class_info)/sizeof(struct class_info) ||
	    class < 0)
		/* unknown class;  may be a site or user layer */
		return(0);
	else
		return (_ff_class_info[class].sys_interface);
}

/*
 *	Check for unsuppported open flags.
 *
 *	Return value:
 *		A mask containing those requested flags which are not supported
 *		by the layer.   0 is returned if all flags may be supported
 *		by the layer.   
 *
 *		If errn is not NULL, then *errn is assigned an appropriate
 *		error code. 
 */
int
_ffclass_unsup_oflags(int class, int flags, int *errn)
{
	int unsup;

	if (errn != NULL)
		*errn = 0;

	if (class >= sizeof(_ff_class_info)/sizeof(struct class_info) ||
	    class < 0)
		/* unknown class */
		unsup = 0;
	else
		unsup = _ff_class_info[class].unsup_oflags & flags;

	if (unsup != 0 && errn != NULL) {
		if (unsup & O_APPEND)
			*errn = FDC_ERR_OAPPEND;
		else if (unsup & O_SSD)
			*errn = FDC_ERR_OSSD;
		else 
			*errn = FEINTUNK;
	}

	return (unsup);
}

/*
 *	_czero_asninfo and _fold_asninfo handle those assign options which
 *	must be honored by all FFIO layers in a chain. 
 *
 *	There are two ways an assign attribute might be handled by FFIO
 *	layers:
 *
 * 	  	1) must be used/handled by at least one layer	(typical case)
 *	  or	2) must be used/handled by all of the layers	(atypical case)
 *
 *	The caller of _ffopen validates that an FFIO chain handled a specified
 *	assign option by checking for the ATTR_USED bits in 
 *	loinf->aip->xxxx_flg fields.  In the typical case (case 1), the 
 *	ATTR_USED bit might have been set by any layer in the chain.
 *
 *	Case 2 is a bit trickier.  _ffopen calls _czero_asninfo and 
 *	_fold_asninfo to copy and then "and" the ATTR_USED bit going in with
 *	that going out of each layer's open routine.   It assures that the
 *	ATTR_USED is set only if all layers in the chain honor it.
 */
void
_czero_asninfo(
assign_info	*aip,			/* assign attributes, or NULL */
assign_info	*acopy,			/* copy of assign attribute flags */
int		depth)			/* 1 if toplevel FFIO layer */
{
	if (aip != NULL && depth > 1) {
		if (aip->m_multup_flg & ATTR_SET) {
			acopy->m_multup_flg = aip->m_multup_flg;  /* copy it */
			aip->m_multup_flg &= ~ATTR_USED;	  /* clear it */
		}
	}
}
void
_fold_asninfo(
assign_info	*aip,			/* assign attributes, or NULL */
assign_info	*acopy,			/* copy of assign attribute flags */
int		depth)			/* 1 if toplevel FFIO layer */
{
	if (aip != NULL && depth > 1) {
		/*
		 * The ATTR_SET bit for "-m" stays cleared if a previously
		 * invoked layer did not support (or "use') the attribute.
		 */
		if ( (acopy->m_multup_flg & ATTR_USED) == 0 )
			aip->m_multup_flg &= ~ATTR_USED;
	}
}

#if !defined(_UNICOS) && !defined(__mips) && !defined(_LITTLE_ENDIAN)
__ffopen(
const char    *name,          /* file name */
long          flags,          /* flags for open(2) */
long          mode,           /* mode for open(2) */
union spec_u  *spek,          /* pointer to current FFIO layer in the chain */
struct ffsw   *stat,          /* return status */
long          cbits)          /* cbits for open(2) */
{
	return _ffopen(name,flags,mode,spek,stat,cbits,
		/* cblks  = */ 0,
		/* pa_fio = */ NULL,
		/* oinf   = */ NULL);
}
#endif	/* ! _UNICOS */

#if defined(_CRAY1) || defined(__mips) || defined(_LITTLE_ENDIAN)
/*
 * This routine determines whether the fxxx routines need to lock.
 * If they do, it opens up a lock layer and inserts it as the top layer.
 */
int
_ff_top_lock(_ffopen_t fd, struct fdinfo **nfioptr, struct ffsw *stat)
{
	struct fdinfo *fio;
	struct fdinfo *nfio;
	struct ffsw dummy;
	int reglock = 0;
	char *lock;

	fio = (struct fdinfo *)fd;
	
	*nfioptr = NULL;
#ifdef _UNICOS
	if ((fio->lock == 0) && !LOADED(ffiolock))
#else
	if (fio->lock == 0)
#endif
		return(0);	/* No locking is necessary */
#ifdef _UNICOS
	if (LOADED(ffiolock)) 
		{
		/* If ffiolock is used, then we use counting locks. */
		/* Even if the current top layer is multitasking safe, we */
		/* need a lock layer with a counting lock preceeding it */

		struct fdinfo *lfioptr;

		lock = calloc(2, sizeof(int));	/* counting lock */
		if (lock == NULL)
			{
			errno = NULL;
			goto err;	
			}

		
		}
	else
#endif
		{
		lock = malloc(sizeof(plock_t));
		if (lock == NULL)
			{
			errno = 0;
			goto err;	
			}
		INITIALIZE_LOCK(*((plock_t *)lock));
		reglock = 1;
		}
	nfio = _open_lock_lyr(fio, (plock_t *)lock);
	if (nfio == NULL)
		goto err;
	*nfioptr = nfio;
	nfio->reg_lock	= reglock;
	nfio->free_lock	= 1; 
	fio->parptr = nfio;
	return(0);
err:
	_SETERROR(stat, errno, 0);
	__ffclose(fio, &dummy);
	return(-1);
}

/*
 * This routine sets up a lock layer if one is needed by the lower layer.
 * The layer associated with "fio" is asked whether it has a lock that
 * should be used. If it does, then a lock layer is added before "fio"
 * in the chain of layers.
 * On return, nfio will point to the new lock.
 */

static int
setup_lock_lyr(struct fdinfo *fio, struct fdinfo **nfioptr,
 struct ffsw *stat)
{
	int ret;
	struct ffsw locstat;
	extern struct xtr_s *_recfm_tab[NUM_CLASSES];
	struct fdinfo *nfio;

	*nfioptr = NULL;

	if (fio->lock == 0)
		{
		/* the layer thinks it is MTSAFE. */
		/* a layer can be MTSAFE only if it CANLISTIO */
		if (!(fio->can_listio))
			ERETURN(stat, FDC_ERR_MTLSTIO, 0);
		}
	else if (fio->lock_word != NULL)
		{
		/* The layer is supplying a lock */
/*
		if (!fio->can_listio)
			{
			ERETURN(stat, FDC_ERR_MTLSTIO, 0);
			}
*/
				
		/* Set up a locking layer that will use the supplied */
		/* lock. The locking layer will be inserted in the */
		/* chain of layers before the current (fio) layer  */

		nfio = _open_lock_lyr(fio, fio->lock_word);
		if (nfio == NULL)
			{
			ERETURN(stat, errno, 0);
			}
		*nfioptr = nfio;
		nfio->reg_lock = 1;
		}
	return(0);
}
#endif

#if	defined(__mips) || defined(_LITTLE_ENDIAN)

_ffopen_t	*_fdinfo_table_ptr;	
int		_fdinfo_table_max = 0;
_ffopen_t	*_fdinfo_exctable_ptr;
int 		_fdinfo_exctable_size = 0;
int		_firstexceptno = 4096;
#define FDINFO_TABLE_ISIZE 64
#define _EXC_INC	    8
#define _TBLE_INC	   64

DECL_LOCK (_fdinfo_lock);

/*
 *	Associate an integer with the fdinfo structure pointed to by fd.
 */
_ff_fdinfo_to_int(_ffopen_t fd, struct ffsw *pstat)
{
	int ret;
	struct fdinfo *fio;
	struct ffc_info_s ffci;
	int i;
	_ffopen_t *ptr;
	struct ffsw locstat;

	/* Get the real file descriptor. We will return this as the integer */
	/* if we can. */

	fio = (struct fdinfo *)fd;
	CHECK_FIOPTR(fio, &locstat);
	ret = XRCALL(fio, fcntlrtn)fio, FC_GETINFO, &ffci, pstat);
	if (ret < 0) {
		ffci.ffc_fd = -1;
	}
        MEM_LOCK(&_fdinfo_lock);
	if (_fdinfo_table_max == 0) {
		if (_fdinfo_table_ptr == NULL) {
			_fdinfo_table_ptr = calloc(FDINFO_TABLE_ISIZE,sizeof(_ffopen_t ));
			if (_fdinfo_table_ptr == NULL) {
				_SETERROR(pstat, FDC_ERR_NOMEM, 0);		
				ret = ERR;
				goto done;
			}
		}
		_fdinfo_table_max = FDINFO_TABLE_ISIZE;
	}
	if (ffci.ffc_fd != -1) {
		
		if (ffci.ffc_fd < _fdinfo_table_max) {
			if (*(_fdinfo_table_ptr+ffci.ffc_fd) == NULL) {
				/* We can use this slot */
				*(_fdinfo_table_ptr+ffci.ffc_fd) = fd;
				ret=ffci.ffc_fd;
				goto done;
			}
			/* Can't use this slot, assign an exceptional one */
		}
		else if ((ffci.ffc_fd < _fdinfo_table_max + _TBLE_INC) &&
			(_fdinfo_table_max + _TBLE_INC < _firstexceptno)) {
			ptr = realloc(_fdinfo_table_ptr,_fdinfo_table_max +
				(_fdinfo_table_max+_TBLE_INC)*sizeof(_ffopen_t));
			if (ptr == NULL){
				_SETERROR(pstat, FDC_ERR_NOMEM, 0);		
				ret = ERR;
				goto done;
			}
			_fdinfo_table_ptr = ptr;
			memset(_fdinfo_table_ptr+_fdinfo_table_max,0,_TBLE_INC*sizeof(_ffopen_t));
			_fdinfo_table_max += _TBLE_INC;
			*(_fdinfo_table_ptr+ffci.ffc_fd) = fd;
                        ret=ffci.ffc_fd;
			goto done;	
		}
	}
	/* If we get here, either the file doesn't have a real file */
	/* descriptor, or its real file descriptor already has something */
	/* associated with it, or its real file descriptor is outside the */
	/* range of our table.  We assign an exceptional integer */
	if (_fdinfo_exctable_ptr == NULL) {
		_fdinfo_exctable_ptr = calloc(_EXC_INC,sizeof(_ffopen_t));
		_fdinfo_exctable_size = _EXC_INC;
	}
	ptr = _fdinfo_exctable_ptr;
	i = 0;
	while (*ptr != NULL && i < _fdinfo_exctable_size) {
		ptr++;
		i++;
	}
	if (i < _fdinfo_exctable_size)  {
		*ptr = fd;
		ret=_firstexceptno + i;
	}
	else {
		ptr = realloc(_fdinfo_exctable_ptr,
			(_fdinfo_exctable_size + _EXC_INC)*sizeof(_ffopen_t));
		if (ptr == NULL) {
			_SETERROR(pstat, FDC_ERR_NOMEM, 0);		
			ret = ERR;
			goto done;
		}
		_fdinfo_exctable_ptr = ptr;
		_fdinfo_exctable_size += _EXC_INC;
		*(_fdinfo_exctable_ptr + i) = fd;	
		/* zero out the remainder */
		memset(_fdinfo_exctable_ptr+i+1,0,sizeof(_ffopen_t)*(_EXC_INC-1));
		ret=_firstexceptno + i;
	}
done:	
	MEM_UNLOCK(&_fdinfo_lock);
	return(ret);
}

/* 
 * Return the struct fdinfo * associated with this integer
 * This routine is the opposite of _ff_fdinfo_to_int()
 */

struct fdinfo 	*
_cnvrt2fdinfo(int fd)
{
	if (fd >= 0 && fd < _fdinfo_table_max) {
		return(*(struct fdinfo **)(_fdinfo_table_ptr + fd));	
	}
	else if (fd >= _firstexceptno &&
		fd <= _firstexceptno + _fdinfo_exctable_size) {
		if (_fdinfo_exctable_ptr == NULL)
			return(NULL);
		return(*(struct fdinfo **)(_fdinfo_exctable_ptr + fd - _firstexceptno));	
	}
	else
		return(NULL);
}
void
_zerocnvrttbl(int fd)
{
        MEM_LOCK(&_fdinfo_lock);
        if (fd >= 0 && fd < _fdinfo_table_max) {
                *(struct fdinfo **)(_fdinfo_table_ptr + fd) = NULL;
        }
        else if (fd >= _firstexceptno &&
                fd <= _firstexceptno + _fdinfo_exctable_size) {
                if (_fdinfo_exctable_ptr != NULL)
                *(struct fdinfo **)(_fdinfo_exctable_ptr + fd - _firstexceptno) = NULL;
        }
        MEM_UNLOCK(&_fdinfo_lock);
	return;
}

#endif
