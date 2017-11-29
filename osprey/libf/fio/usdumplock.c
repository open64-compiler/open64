/*
 * Copyright 2002, 2003, 2004 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/usdumplock.c	92.1	06/18/99 18:38:26"

#include <sys/types.h>
#ifndef KEY
#include <ulocks.h> 
#endif
#include <stdio.h>
#include <stdlib.h>
#include "fio.h"
#include <string.h>

#ifdef KEY
typedef void *ulock_t;	/* added in place of including ulocks.h */
#endif

extern int32 __usdumplock_f90(ulock_t *l, int32 *u, char *str, int32 len);
extern _f_int usdumplockf90_(ulock_t *l, _f_int *u, char *str, int len);
extern _f_int4 usdumplockf90_4_8_4_(ulock_t *l, _f_int4 *u, char *str, int len);
extern _f_int8 usdumplockf90_8_(ulock_t *l, _f_int8 *u, char *str, int len);

int32
__usdumplock_f90(ulock_t *l, int32 *u, char *str, int32 len)
{
	return usdumplockf90_(l, u, str, len);
}

_f_int
usdumplockf90_(ulock_t *l, _f_int *u, char *str, int len)
{
	char	*buff;
	unit	*cup;
	FILE	*fd;
	unum_t	unum;

	unum	= *u;
	cup	= _get_cup(unum);
	if (unum < 0 || !cup)
		return((errno=FEIVUNIT));

	if (cup->ufmt == NO && cup->useq == NO) {

		/* not formatted and not sequential */
		errno=FEFMTTIV;

		/*
	 	 * A formatted read or write is not allowed on an
		 * unformatted file.  We really want to say operation
		 * only allowed on formatted sequential files
		 */
		return(-1);
	}

/*	check to see if write is allowed on file. */
	if (!cup->ok_wr_dir_unf) {

		/* write not allowed. */
		errno	= FENOWRIT;
		return(-1);
	}
	if (cup->ufs != STD) {
		/* not an "stdio" file */
		errno	= FDC_ERR_NOSTRM; /* NOT the right msg - DLAI */
		return(-1);
	}
/*	get the FILE */
	fd	= cup->ufp.std;
	buff	= (char *) malloc( len + 1 );
	strncpy( buff, str, len );
	buff[len]	= '\0';
	usdumplock( *l, fd, buff );
	free( buff );
	return(0);
}

_f_int4
usdumplockf90_4_8_4_(ulock_t *l, _f_int *u, char *str, int len)
{
	_f_int8		unum = *u;
	return (_f_int4)usdumplockf90_8_(l,&unum,str,len);
}

_f_int8
usdumplockf90_8_(ulock_t *l, _f_int8 *u, char *str, int len)
{
	char	*buff;
	unit	*cup;
	FILE	*fd;
	unum_t	unum;

	unum	= *u;
	cup	= _get_cup(unum);
	if (unum < 0 || !cup)
		return((errno=FEIVUNIT));

	if (cup->ufmt == NO && cup->useq == NO) {

		/* not formatted and not sequential */
		errno=FEFMTTIV;

		/*
	 	 * A formatted read or write is not allowed on an
		 * unformatted file.  We really want to say operation
		 * only allowed on formatted sequential files
		 */
		return(-1);
	}

/*	check to see if write is allowed on file. */
	if (!cup->ok_wr_dir_unf) {

		/* write not allowed. */
		errno	= FENOWRIT;
		return(-1);
	}
	if (cup->ufs != STD) {
		/* not an "stdio" file */
		errno	= FDC_ERR_NOSTRM; /* NOT the right msg - DLAI */
		return(-1);
	}
/*	get the FILE */
	fd	= cup->ufp.std;
	buff	= (char *) malloc( len + 1 );
	strncpy( buff, str, len );
	buff[len]	= '\0';
	usdumplock( *l, fd, buff );
	free( buff );
	return(0);
}
