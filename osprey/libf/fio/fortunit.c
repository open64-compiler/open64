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



#pragma ident "@(#) libf/fio/fortunit.c	92.1	06/22/99 11:11:33"

#include <stdlib.h>
#include <cray/nassert.h>
#include "fio.h"

/*
 *	_alloc_unit
 *
 *		Find an unopened unit for an external file.  If necessary, 
 *		allocate the unit and initialize it first, and add the unit 
 *		into the global unit table.  This function locks the unit 
 *		before returning.
 *
 *		This function assumes that _openlock is locked, that
 *		unum is a valid unit number/identfier, and that unit unum
 *		is not currently connected.		
 *
 *	Algorithm
 *
 *		The hash chain associated with "unum" is searched for
 *		any unconnected unit.   This unit is set up to contain
 *		the specified unit number before returning.   
 *
 *	Return value:
 *
 *		Pointer to the allocated unit on success.  NULL on error,
 *		with errno set to the specific error code.
 */

extern void _initialize_e_fortran_io(void);

unit *
_alloc_unit(
	unum_t	unum, 		/* unit number */
	int	private)	/* 1 if private, 0 if global unit */
{
	register int	hval;
	register int	mytask;
	unit		*prcup;
	unit		*cup;

	if (! _e_fortran_io_is_init)
		_initialize_e_fortran_io();
	
	hval	= UHASH(unum);
	prcup	= NULL;
	cup	= _fort_unit[hval].ulist;
	
/*
 *	Skip all units which do not match the unit number sought.  Also
 *	skip any ulready-open units with the same unit number.  In that case
 *	we must be opening a task-private unit.
 */
	mytask	= private ? MYTASK : 0;

	while (cup != NULL) {
 		if (cup->uid == unum && 
		    cup->private == private && cup->utid == mytask)
			break;

		prcup	= cup;
		cup	= cup->hashlink;
	}
	
/*
 *	If a unit structure for the current unit number has not yet been
 *	allocated, then add one to the hash table.
 */
	if (cup == NULL) {

		cup	= malloc(sizeof(unit));

		if (cup == NULL) {
			errno	= FENOMEMY;
			return(NULL);
		}

		/*
		 * Initialize the unit, and then lock it before adding it to 
		 * the hash table to prevent losing the race with another task 
		 * for the lock.
		 *
		 * The FLSH_MEM call assures that the unit initialization
		 * reaches memory prior to the time the unit gets linked into
		 * the hash table.   This ensures that other tasks searching
		 * the unit hash table do not get a false match on this 
		 * unit.
		 */

		cup->hashlink	= NULL;
		cup->uid	= unum;
		cup->private	= private;
		cup->utid	= mytask;
		INITIALIZE_LOCK(cup->uiolock);
		_init_unit(cup);

		MEM_LOCK(&cup->uiolock);

		FLSH_MEM();

		if (prcup == NULL)
			_fort_unit[hval].ulist	= cup;
		else
			prcup->hashlink		= cup;
	}
	else {
		/* We're in deep weeds if the unit is open. */
		assert( ! OPEN_UPTR(cup));	

		MEM_LOCK(&cup->uiolock);

		_init_unit(cup);
	}

	return(cup);
}

/*
 *	_search_unit_list
 *
 *		Perform a linear search of the hash chain starting with
 *		the unit pointed to by ulist.
 */

unit *
_search_unit_list(unit *cup, unum_t unum)
{
	while (cup != NULL) {
		if (cup->uid == unum &&
		    (cup->private == 0 || cup->utid == MYTASK))
			return(cup);

		cup	= cup->hashlink;
	}

	return(cup);
}

/*
 *	_get_next_unit 
 *
 *		This function returns the next open unit in the unit table(s)
 *		after optionally locking the unit.  NULL is returned if there 
 *		are no more open units.
 *
 *		Note that if multiple tasks are active, and the caller of
 *		_get_next_unit wants to be assured of finding all currently
 *		open units at a given instant,   _openlock must be locked 
 *		prior to calling _get_next_unit().
 *
 *	Argument
 *
 *		prcup 	Pointer to previous unit returned by _get_next_unit.
 *			NULL indicates that _get_next_unit should return the
 *			first open unit in the unit table.
 *
 *		iflock	!=0 iff units should be locked by _get_next_unit() 
 *			before returning.
 *
 *		iftask	!=0 iff the search for the next unit should be 
 *			restricted to units visible on the current task.
 *
 *	Return value:
 *		Pointer to the next connected unit.  If there are no more
 *		connected units, then NULL is returned.
 */

unit *
_get_next_unit(unit *prcup, int iflock, int iftask)
{
	register int	hval;
	unit		*cup;

	if (prcup != NULL) {
		cup	= prcup->hashlink;
		hval	= UHASH(prcup->uid);

		if (iflock)
			_release_cup(prcup);	/* unlock previous unit */
	}
	else {
		cup	= _fort_unit[0].ulist;
		hval	= 0;
	}

	while (hval < HASH_SIZE) {

		while (cup != NULL) {
 			if ( OPEN_UPTR(cup) &&  (iftask == 0 ||
				cup->private == 0 || cup->utid == MYTASK) ) {

				if (iflock) {
					MEM_LOCK(&cup->uiolock);
					if (cup->auxlockp != NULL)
						MEM_LOCK(cup->auxlockp);
				}
				goto done;
			}

			cup	= cup->hashlink;
		}

		hval	= hval + 1;

		if (hval < HASH_SIZE)
			cup	= _fort_unit[hval].ulist;
	}

done:
	return(cup);
}
