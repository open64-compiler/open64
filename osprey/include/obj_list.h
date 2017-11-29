/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* $Header$ */

#ifndef __OBJ_LIST_H__
#define __OBJ_LIST_H__

#ifdef __cplusplus
extern "C" {
#endif

typedef struct obj_list {
	unsigned long	data;
	struct obj_list	*next;
	struct obj_list	*prev;			/* back link */
} objList;

#define LIST_BEGINNING	0
#define LIST_END	1
#define LIST_ADD_BEFORE         2
#define LIST_ADD_AFTER          3
#define LIST_DELETE             4
#define LIST_REPLACE            5

#ifdef __cplusplus
}
#endif

#endif	/* __OBJ_LIST_H__ */
