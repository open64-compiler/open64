/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


/* USMID @(#) libu/ffio/locklyr.h	92.1	10/07/99 22:14:06 */

#include <cray/portdefs.h>
#include <cray/mtlock.h>
#include "fflock.h"


#ifdef KEY /* Bug 6003 */
#define LYR_LOCK(_x)	MEM_LOCK((_x)->lyr_info);
#define LYR_UNLOCK(_x)	MEM_UNLOCK((_x)->lyr_info);
#define COND_LYR_UNLOCK(_x)	{if ((_x)->lyr_info != NULL) LYR_UNLOCK(_x) }
#else
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	/* mips only handles reg_lock now */
#define LYR_LOCK(_x)	MEM_LOCK((unsigned long *)_x->lyr_info);
#define LYR_UNLOCK(_x)	MEM_UNLOCK((unsigned long *)_x->lyr_info);
#define COND_LYR_UNLOCK(_x)	{if (_x->lyr_info != NULL) LYR_UNLOCK(_x) }
#else
#define LYR_LOCK(_x)	{if (_x->reg_lock) \
				{LMEM_LOCK(_x->lyr_info)}\
			 else\
				_nlockon(_x->lyr_info);\
			}

#define LYR_UNLOCK(_x)	{if (_x->reg_lock) \
				{LMEM_UNLOCK(_x->lyr_info)}\
			else\
				_nlockoff(_x->lyr_info);\
			}
#define COND_LYR_UNLOCK(_x)	{if (_x->lyr_info != NULL) LYR_UNLOCK(_x) }
#endif
#endif /* Key Bug 6003 */
