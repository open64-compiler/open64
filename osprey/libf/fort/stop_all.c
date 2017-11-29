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



#pragma ident "@(#) libf/fort/stop_all.c	92.1	06/24/99 10:18:36"

/*
 * This routine is provided on MPP systems, to allow one PE in a
 * parallel region to interrupt the other PEs and exit.
 * (STOP is defined to stop all PEs in a master region, but only
 * the calling PE in a parallel region).
 *
 * We just #include the STOP code here, after setting the STOPALL
 * symbol (this alters how STOP builds itself slightly).  We then
 * call STOP from STOP_ALL (we inline it so we get a good traceback).
 */
#include <signal.h>
#define STOPALL
#include "stop.c"

void
STOP_ALL(s)
_fcd	s;
{
#pragma inline _STOP
	_STOP(s);
}
void
STOP_ALL_DISABLE()
{
	sigset_t bufiomask = sigmask(SIGBUFIO);
	(void)sigprocmask(SIG_BLOCK, &bufiomask, NULL);
}
void
STOP_ALL_ENABLE()
{
	sigset_t bufiomask = sigmask(SIGBUFIO);
	(void)sigprocmask(SIG_UNBLOCK, &bufiomask, NULL);
}
