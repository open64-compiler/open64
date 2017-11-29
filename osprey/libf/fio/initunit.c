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



#pragma ident "@(#) libf/fio/initunit.c	92.1	06/22/99 11:11:33"
#include "fio.h"
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

/*
 *	If the macro DEBUG_MTIO is set, then this hard reference to TSKSTART
 *	will cause linking in of multitasking locking code even when the
 *	program is not multitasked.   This causes the library locking and 
 *	unlocking to be activated for increased exercise/test coverage of the 
 *	library locking paths.
 *
 *	The reference to TSKSTART is placed in this module because this 
 *	module is guaranteed to be loaded when any Fortran I/O routines
 *	are loaded. 
 */
#if	defined(DEBUG_MTIO) && defined(_CRAY1)
extern int TSKSTART();
int _debug_mtio = (int)TSKSTART;
#endif

/*
 * 	_init_unit
 *
 *		Sets default unit table values.  This routine should be called 
 *		some time before every unit open.  Any freeing of memory 
 *		pointed to by a unit table entry should be freed at close 
 *		time, not in this routine.
 */
void
_init_unit(unit *cup)
{
	/*
	 * Clear the unit table, except for the leading few fields.  The
	 * leading fields are initialized once when allocating new units in a 
	 * hash chain or are used for locking.
	 */

	(void) memset ((char*)cup + UNIT_HEADER, 0, sizeof(unit) - UNIT_HEADER);

	/*
	 * Now, set parameterized or nonzero fields
	 */

	cup->ufs	= FS_DEFAULT;	/* File structure */
	cup->unitchk	= 1;		/* Unit was checked */
	cup->utrunc	= 1;		/* Truncation after write is default */
	cup->usysfd	= -1;	 	/* Default no associated system file */

	return;
}

/*
 *	_init_internal_unit 
 *
 *		Is called by the first call to _get_int_cup() to set up the 
 *		unit structure used by internal file I/O.
 */
extern void _initialize_i_fortran_io(void);

unit *
_init_internal_unit(void)
{
	unit		*cup;
/*
 *	Lock _openlock to ensure that _fort_internal_unit is allocated only 
 *	once.
 */
	OPENLOCK();

        if (! _i_fortran_io_is_init)
                _initialize_i_fortran_io();

	if (_fort_internal_unit == NULL) {
		cup = malloc(sizeof(unit));
		if (cup == NULL) {
			OPENUNLOCK();
			_lerror(_LELVL_ABORT, FENOMEMY);
		}

		cup->hashlink	= NULL;		/* not used */
		cup->uid	= -1;		/* not used */
		INITIALIZE_LOCK(cup->uiolock);

		_init_unit(cup);

		cup->ufmt	= 1;		/* FORM='FORMATTED'	*/
		cup->useq	= 1;		/* ACCESS='SEQUENTIAL'	*/
		cup->ublnk	= 0;		/* BLANK='NULL'		*/
		cup->upad	= OS_YES;	/* PAD='YES'		*/
		cup->udelim	= OS_NONE;	/* DELIM='NONE'		*/
		cup->uaction	= OS_READWRITE;	/* ACTION='READWRITE'	*/

		_set_ok_flags(cup);		/* set up the cup->ok_* flags */

		/*
		 * Ensure that the previous memory stores complete before any 
		 * other task uses the internal unit.
		 */
		FLSH_MEM();

		_fort_internal_unit = cup; 
	}

	OPENUNLOCK();

	return(_fort_internal_unit);
}

