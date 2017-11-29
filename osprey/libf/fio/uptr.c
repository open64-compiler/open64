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



#pragma ident "@(#) libf/fio/uptr.c	92.1	06/18/99 18:38:26"
   
#include <string.h>
#include <malloc.h>
#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <unistd.h>
#include <cray/assign.h>
#include <sys/stat.h>
#include "fio.h"

/*
 *	_setuptr() - This routine is called by library routines that
 *		     do auxiliary types of i/o (e.g., aqio and waio). 
 *		     It finds the appropriate entry in the unit table
 *		     corresponding to the file being opened and
 *		     initializes values in the unit table.
 *
 *	returns: NULL on error (errno is set)
 *		 pointer into the unit table if successful.
 */

unit *
_setuptr(
	unum_t		uid,		/* unit number */
	int		rfd,		/* real file descriptor */
	char		*uname,		/* file name from open */
	char		*realname,	/* file name after resolving aliases */
	assign_info	*aip)		/* assign info, or NULL */
{
	register int	private;
	unit		*cup;

	MEM_LOCK(&_openlock);

	cup	= _get_cup(uid);

	if (cup != NULL) {	/* Already open! */
		errno	= FEOPOTHR;	
		goto error;
	}

#ifdef  _CRAYMPP
        private	= 1;    /* private units are default on MPP */
#else
        private	= 0;    /* global units are default on other systems */
#endif
        if (aip != NULL && aip->P_ioscop_flg)
                private	= (aip->P_ioscop == 'p');

	/* Allocate, initialize and lock unit */

	cup	= _alloc_unit(uid, private);

	if (cup == NULL)
		goto done;		/* errno has error status */
	
	cup->usysfd	= rfd;		/* file descriptor */
	_set_device_and_inode(rfd, &cup->udevice, &cup->uinode);

	if (_uniqinod(cup, aip) == -1)
		goto error;
/*
 *	Set the "-m on" flag in the unit table entry.
 */
        if (aip != NULL && aip->m_multup_flg && aip->m_multup)
		cup->umultup	= 1;

	cup->ufs	= FS_AUX;
/*
 *	Allocate cup->ufnm and cup->alfnm strings.
 */
	cup->ufnm	= strdup(uname);

	if (cup->ufnm == NULL) {
		errno	= FENOMEMY;
		goto error;
	}

	cup->alfnm	= strdup(realname);

	if (cup->alfnm == NULL) {	
		free(cup->ufnm);
		cup->ufnm	= NULL;
		errno		= FENOMEMY;
		goto error;
	}

	_release_cup(cup);

done:
	MEM_UNLOCK(&_openlock);
	return(cup);

error:
	if (cup != NULL)
		_release_cup(cup);

	cup	= NULL;
	goto done;
}

/*
 *	_clruptr(uptr) - clears a unit table entry
 *	returns: -1 on error (errno is set)
 *		  0 if ok
 */

_clruptr(unit *cup)
{

	MEM_LOCK(&_openlock);
/*
 *	We should really pass the uid in to _clruptr, not the unit pointer.
 */
	cup	= _get_cup(cup->uid);		/* lock the unit if it's open */

	if (cup == NULL) {
		MEM_UNLOCK(&_openlock);
		return(0);			/* probably deep weeds */
	}

	if ( cup->ufs != FS_AUX ) {
		MEM_UNLOCK(&_openlock);
		_release_cup(cup);		/* unlock the unit */
		errno	= FEINTUNK;
		return(-1);			/* definitely deep weeds */
	}

	if ((cup->alfnm != cup->ufnm) && (cup->alfnm != NULL))
		free(cup->alfnm);
	if (cup->ufnm != NULL)
		free(cup->ufnm);
	cup->ufs	= 0;

	_release_cup(cup);			/* unlock the unit */
	MEM_UNLOCK(&_openlock);

	return(0);
}
