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


#pragma ident "@(#) libu/multi/atx@ckstrm.c	92.1	05/19/99 14:19:43"


/*
 *      Multi-streaming MSPs are not supported.
 */

extern void ATX@CHECKSTRM(int *arg1);

/*
 * Note that asm entry point $STREAM$ON is set to zero by libc
 * in lib/libc/multi to indicate the presence of the streaming
 * library.  It may mean that streaming is being done or could
 * be done.
 *
 * This is for PVP SV1 systems only.
 */

#pragma _CRI soft $STREAM$ON
extern int $STREAM$ON(void);

void
ATX@CHECKSTRM(int *stream_flag)
{
	*stream_flag = 1;
	if ($STREAM$ON == 0)
		*stream_flag = 0;
}
