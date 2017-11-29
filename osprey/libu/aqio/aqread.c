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


#pragma ident "@(#) libu/aqio/aqread.c	92.1	07/01/99 13:50:36"
 
#include <liberrno.h>
#include <fortran.h>
#include <sys/types.h>
#include <sys/iosw.h>
#include <sys/listio.h>
 
/*
 *	AQREAD - queue an asynchronous read request
 */

void
AQREAD(aqp,cpuadd,blknum,blocks,reqid,queue,status)
long *aqp, *blknum, *blocks, *reqid, *queue, *status;
void *cpuadd;
{
	long incs = 0;
	long mstride = 0;
	long dstride = 0;

	if (_issddptr(cpuadd)) {
		*status  = -FESHRSUP;
		return;
	}
	_aqenq(LO_READ,aqp,cpuadd,&mstride,blknum,blocks,&dstride,&incs,reqid,queue,status);
	return;
}

/*
 *	AQREADC - queue an asynchronous compound read request
 */

void
AQREADC(aqp,cpuadd,mstride,blknum,blocks,dstride,incs,reqid,queue,status)
long *aqp, *mstride, *blknum, *blocks, *dstride, *incs,
		*reqid, *queue, *status;
void *cpuadd;
{
	if (_issddptr(cpuadd)) {
		*status  = -FESHRSUP;
		return;
	}
	_aqenq(LO_READ,aqp,cpuadd,mstride,blknum,blocks,dstride,incs,reqid,queue,status);
	return;
}
