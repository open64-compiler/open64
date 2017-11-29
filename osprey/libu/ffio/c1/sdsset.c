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


static char USMID[] = "@(#) libu/ffio/c1/sdsset.c	92.0	10/08/98 14:57:41";


#include <ffio.h>
#include <cray/mtlock.h>
#include "../fssio.h"
#include <memory.h>

/*
 * _sds_locsw_set(sds_info, usersw, count) finds an unused entry in the
 *	structure describing outstanding asynchronous I/O
 *	requests, and initializes its values.
 * Parameters:  usersw - address of the status word passed in from the
 *			upper level. 
 *		count - number of BITS already completed for this request.
 *		stat  - status is returned here.
 * 
 * Returns: pointer to the _lociosw structure to be used.
 *          NULL if error occurred
 */
_lociosw *
_sds_locsw_set(
	struct sds_f *sds_info, 
	struct ffsw *usersw, 
	int count) 
{
_lociosw *locptr;
int i;
struct _loclink *loclink;

	MEM_LOCK(&(sds_info->locsw_lock)); 
	if (sds_info->loclist == NULL) {
		sds_info->loclist = (struct _loclink *)calloc(1, sizeof(struct _loclink)); 
		if (sds_info->loclist == NULL)   {
			goto nomem;
		} 
	} 
	loclink = sds_info->loclist;
	locptr = loclink->loc_first;
	for (;;) {
		for (i = 0; i < _FSSASYNUM ; i++) {
       			if (locptr->user_sw == NULL) {
				goto found ;
			}
			else
				locptr++;
		}
		if (loclink->loc_nxt == NULL) {
			loclink->loc_nxt =  (struct _loclink *)calloc(1, sizeof(struct _loclink));
			if (loclink->loc_nxt == NULL)   {
				goto nomem;
			}
		}
		loclink = loclink->loc_nxt;
		locptr = loclink->loc_first;
	}
found:
	locptr->user_sw = usersw;
	locptr->sw_count = count;
	memset(&locptr->local_sw , 0, sizeof(struct ffsw));
	CLRFFSTAT(*usersw);
#if     defined(_ADDR64) || defined(__mips)
	usersw->sw_sptr = (void *)locptr;
#else
	usersw->sw_sptr = (int)locptr;
#endif
	MEM_UNLOCK(&(sds_info->locsw_lock));
	return(locptr);
nomem:
	MEM_UNLOCK(&(sds_info->locsw_lock));
	return(NULL);
}

/*
 * Clear the entry pointed to by locptr.
 * The calling routine is responsible for locking.
 */
void
_sds_locsw_clear(
	struct sds_f *sds_info,
	 _lociosw *locptr)
{
	locptr->user_sw = 0;
	locptr->sw_count = 0;
	memset(&locptr->local_sw, 0, sizeof(struct ffsw));
	return;
}
