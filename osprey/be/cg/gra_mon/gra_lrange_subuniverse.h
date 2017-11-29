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

/*                      Subuniverses of Live Ranges
 *                      ===========================
 *
 *  Description:
 *
 *      We use LRANGE_SETs to represent interference.  Live ranges can
 *      only conflict if they are (a) in the same region and (b) need a
 *      register from the same register class.  We'll take advantage of
 *      this fact to make our interference sets denser.  This module
 *      implements multiple dense LRANGE<=>INT32 mappings to be used as
 *      support for LRANGE subuniverses.  These subuniverses are
 *      NON-INTERSECTING in the current implementation.
 *
 *      Count returns the number of elements in the subuniverse.  
 *      Since the numbering is guaranteed to be dense, this can be used in
 *      conjunction with Nth_Lrange to iteration over the elements:
 *
 *              for ( i = sub->Count() - 1; i >= 0; --i) {
 *                  LRANGE *lrange = sub->Nth_Lrange(i);
 *                  .. lrange is now a live range in subuniverse
 *              }
 */


/*
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:29-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lrange_subuniverse.h $
 */


#ifndef LRANGE_SUBUNIVERSE_INCLUDED
#define LRANGE_SUBUNIVERSE_INCLUDED

#ifndef LRANGE_SUBUNIVERSE_RCS_ID
#define LRANGE_SUBUNIVERSE_RCS_ID
#ifdef _KEEP_RCS_ID
static char *lrange_subuniverse_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lrange_subuniverse.h $ $Revision: 1.2 $";
#endif
#endif

#include "defs.h"
#include "errors.h"
#include "gra_lrange.h"

class LRANGE_SET_SUBUNIVERSE {
friend class LRANGE_SUB_MGR;
  LRANGE** lranges;	// Maps integers to LRANGEs
  INT32 count;		// Number of elements currently in the subuniverse
  INT32 alloc_size;     // How many it can hold before it must be reallocated
public:
  LRANGE_SET_SUBUNIVERSE(void) {}
  ~LRANGE_SET_SUBUNIVERSE(void) {}

  LRANGE** Lranges(void)	{ return lranges; }
  INT32 Count(void)		{ return count; }
  INT32 Alloc_Size(void)	{ return alloc_size; }
  INT32 Member_Count(void)	{ return alloc_size; }

  void Add(LRANGE* lrange);
  LRANGE *Nth_Lrange(INT i);
};

// use shorter name in gra's code
class LRANGE_SUBUNIVERSE: public LRANGE_SET_SUBUNIVERSE {
};

class LRANGE_SUB_MGR {
friend class LRANGE_SET_SUBUNIVERSE;
  MEM_POOL pool;		// where to allocate stuff
  BOOL pool_initialized;
public:
  LRANGE_SUB_MGR(void) { pool_initialized = FALSE; }
  ~LRANGE_SUB_MGR(void) {}

  void Initialize(void);
  void Finalize(void);
  LRANGE_SUBUNIVERSE *Create(INT32 initial_size);
};

extern LRANGE_SUB_MGR lrange_sub_mgr;

#endif
