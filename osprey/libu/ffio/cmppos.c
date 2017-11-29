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


#pragma ident "@(#) libu/ffio/cmppos.c	92.2	06/29/99 13:16:47"

#include <ffio.h>
#include "cmpio.h"
 
/*
 *  trace positioning requests
 */
 
int
_cmp_pos(fio, cmd, arg, stat)
struct fdinfo *fio;
int cmd;
long *arg;
struct ffsw *stat;
{
   	int 	 ret;
	long	 bytes;
	cmp_lyr	*cinfo;
	char	*rtnName = "_cmp_pos";
 
	cinfo = fio->lyr_info;

	TRC_ENTER;
 
	switch (cmd) {
#ifndef __mips
		case FP_BSEEK:
		    /*
		     * Before supporting this on MIPS, we need to look at
		     * what the arguments should be. A long (for *arg) is
		     * not big enough to hold the total file offset on 
		     * 32-bit machines.
		     */
		    if ((*arg & 7) != 0) {
			    bytes = (-(*arg))>>3;
			    bytes = -bytes;
		    } else {
			    bytes = *arg>>3;
		    }

		    ret = XRCALL(fio, seekrtn) fio, bytes, *(arg+1), stat);

		    return(_dshiftl(ret, ret, 3));
#endif
		default:
		    ERETURN(stat, FDC_ERR_NOSUP, 0);
	}
}
