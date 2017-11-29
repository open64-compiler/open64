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


/* USMID @(#) libu/ffio/gfio.h	92.0	10/08/98 14:57:41 */

#ifndef _GFIO_H
#define _GFIO_H
/* CLASS_F */
#define TR_IBM_F	1	/* IBM F format */
#define TR_IBM_FB	2	/* IBM FB format */
#define TR_VMS_F_DSK	3	/* VMS F.DISK format */
#define TR_VMS_F_TP	4	/* VMS F.TAPE format */
#define TR_VMS_F_TR	5	/* VMS F.TR (transparent) format */

#define NUM_F_TYPES	6

struct gen_ff			/* generic F format */
	{
	int padd;		/* is padding required? */
				/* padding is accepted on input.  It is */
				/* not produced on output */
	char pchar;		/* padd character */
	};

/*
 *	Define a structure that is to be filled with limits and defaults
 *	for record lengths and block sizes.
 *	The data associated with this structure is in fxrmisc.c
 */
struct f_rec_limit_s
	{
	int min_rsz;	/* minimum record size */
	int max_rsz;	/* maximum record size */
	int def_rsz;	/* default record size */
	int min_mbs;	/* minimum block size */
	int max_mbs;	/* maximum block size */
	int def_mbs;	/* default block size */
	};
#endif
