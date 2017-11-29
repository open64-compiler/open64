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


#pragma ident "@(#) libu/ffio/evnt_get_parent_child.c	92.1	06/29/99 13:16:47"


#include <ffio.h>
#include "evntio.h"

/*
 * _evnt_get_parent_child
 *
 * Pick up names of layer above and below 
 *
 * Input:
 *	fio		- file descriptor
 *	parent		- pointer to name of parent layer (above)
 *	child		- pointer to name of child layer (below)
 *
 * Output:
 *
 */
void
_evnt_get_parent_child(struct fdinfo *fio, char  **parent, char **child)
{
	static char PROGRAM[] = "program";

	char   *LAYER_NAMES[NUM_CLASSES] =
	{
		"system ", "syscall ", "null ", "system ", "cos ", "bmx ", "f ",
		"v ", "text ", "x ", "cdc ", "sds ", "mr ", "trace ",
		"user ", "site ", "error ", "fd ", "blx ",
		"cache ",
		"er90 ",
		"bufa ",
		"cachea ",
		"event ",
		"lock ",
		"global ",
		"f77 ",
		"resv_27 ",
		"resv_28 ",
		"resv_29 ",
		"user_0",
		"user_1",
		"user_2 ",
		"user_3 ",
		"user_4 ",
		"user_5 ",
		"user_6 ",
		"user_7 ",
		"user_8 ",
		"user_9 "
	};

	if (parent) {
		if (fio->parptr == 0)
			*parent = PROGRAM;
		else
			*parent = LAYER_NAMES[fio->parptr->class];
	}
	if (child)
		*child = LAYER_NAMES[fio->fioptr->class];
}
