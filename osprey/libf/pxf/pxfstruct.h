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


/* USMID @(#) libf/pxf/pxfstruct.h	92.0	10/08/98 14:30:10 */

#ifndef _PXFSTRUCT_H
#define _PXFSTRUCT_H

struct pxfhandle {
	void *pxfstructptr;
	int pxftype;
};

#define PXF_FLOCK 1
#define PXF_UTIMBUF 2
#define PXF_UNAMBUF 3
#define PXF_STATBUF 4
#define PXF_TMSBUF 5
#define PXF_GROUP 6
#define PXF_PASSWD 7
#define PXF_DIRENT 8
#define PXF_SIGSET 9
#define PXF_SIGACTION 10
#define PXF_TERMIOS 11
#define PXF_SUBHAND 12

#endif
