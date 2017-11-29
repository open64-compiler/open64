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



#pragma ident "@(#) libf/fio/globals.c	92.2	06/23/99 16:08:16"

/*
 *	globals.c	This module contains global data and tables with
 *			dynamic data.
 */

#include <fortran.h>
#include "fio.h"
#include <cray/mtlock.h>

/************************************************************************
 *
 *	Shared global data.
 *
 ***********************************************************************/

plock_t	_openlock;			/* Unit tables lock.  Lock this 
					 * whenever connecting or disconnecting
					 * a unit.   This lock must be obtained
					 * before the unit lock to prevent 
					 * deadlock. */
plock_t	_ioblock;			/* __iob table lock	*/
plock_t	_parselock;			/* Format parsing lock	*/
plock_t _stdin_lock;			/* Stdin lock			*/
plock_t _stdout_lock;			/* Stdout lock			*/
plock_t _stderr_lock;			/* Stderr lock			*/
int	_f_rcsz		= RECMAX;	/* Default sequential formatted RECL */
int	_f_ldsz		= RECMAXLDO;	/* Default list-directed output RECL */

unit_htable _fort_unit[HASH_SIZE];	/* hash table of unit pointers */

unit	*_fort_internal_unit;		/* pointer to unit for internal files */

#ifdef	_F_REAL4
int	_dreal4;			/* decimal digits for real (KIND=4) */
#endif
int	_dreal8;			/* decimal digits for real (KIND=8) */
int	_dreal16;			/* decimal digits for real (KIND=16) */

long	_zero_entity	= 0;		/* Used as addr for zero length	*/
					/* entity for PRESENT function	*/


/************************************************************************
 *
 *	Private global data.
 *
 *	The _tsk_fiostate structure is taskcommon, so the C compiler kindly
 *	replicates it for every active task.  
 *
 ***********************************************************************/
#ifdef	_CRAY1
#pragma _CRI taskcommon _tsk_fiostate
#endif
struct fiostate _tsk_fiostate;

SHARED_CC_BUG_WORKAROUND(_globals_kludge_func)
