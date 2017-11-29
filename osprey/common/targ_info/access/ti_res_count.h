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


/* ====================================================================
 * ====================================================================
 *
 *  Module: ti_res_count.h
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/access/ti_res_count.h,v $
 *
 *  Revision history:
 *   16-Oct-91 - Original Version
 *
 *  Synopsis:
 *
 *	Utility functions for determining scheduling resource constraints.
 *
 *  Interface Description:
 *
 *	Exported types:
 *
 *	    TI_RES_COUNT
 *
 *		Opaque type to maintain context for the resource
 *		counting functions.
 *
 *      Exported functions:
 *
 *	    TI_RES_COUNT *TI_RES_COUNT_Alloc(
 *		MEM_POOL *pool
 *	    )
 *
 *		Allocate a new TI_RES_COUNT context from 'pool' with the 
 *		resource counters initialized to zero.
 *
 *	    TI_RES_COUNT *TI_RES_COUNT_Print(
 *		FILE *fp,
 *		TI_RES_COUNT *res
 *	    )
 *
 *		Print a representation of <res> to <fp>.
 *
 *	    void TI_RES_COUNT_Add_Op_Resources(
 *		TI_RES_COUNT *counts
 *		TOP           opcode
 *	    )
 *
 *              Add the resource usage counts of 'opcode' into the
 *		appropriate elements of 'counts'.
 *
 *	    void TI_RES_COUNT_Add_Op_Resources_Scaled(
 *		TI_RES_COUNT *counts
 *		TOP           opcode,
 *		double	      factor
 *	    )
 *
 *              Add the resource usage counts of 'opcode' multiplied by
 *		'factor' into the appropriate elements of 'counts'.
 *
 *	    void TI_RES_COUNT_Subtract_Op_Resources(
 *		TI_RES_COUNT *counts
 *		TOP           opcode
 *	    )
 *
 *              Subtract the resource usage counts of 'opcode' from the 
 *              appropriate elements of 'counts'.
 *
 *	    void TI_RES_COUNT_Subtract_Op_Resources_Scaled(
 *		TI_RES_COUNT *counts
 *		TOP           opcode,
 *		double	      factor
 *	    )
 *
 *              Subtract the resource usage counts of 'opcode' multiplied
 *		by 'factor' from the appropriate elements of 'counts'.
 *
 *	    void TI_RES_COUNT_Add(
 *		TI_RES_COUNT *sum
 *		TI_RES_COUNT *addend1
 *		TI_RES_COUNT *addend2
 *	    )
 *
 *		Add the individual resource counts of 'addend1' to 'addend2',
 *		storing the result in 'sum'.
 *
 *	    void TI_RES_COUNT_Subtract(
 *		TI_RES_COUNT *difference
 *		TI_RES_COUNT *minuend
 *		TI_RES_COUNT *subtrahend
 *	    )
 *
 *		Subtract the individual resource counts of 'subtrahend' from
 *		'minuend' storing the result in 'difference'.
 *
 *	    double TI_RES_COUNT_Min_Cycles(
 *		TI_RES_COUNT *counts
 *          )
 *
 *              Compute and return a minimum cycle count based on the
 *		resource counts in 'counts'.
 *
 *	    INT32 TI_RES_COUNT_Min_II(
 *		TI_RES_COUNT *counts
 *          )
 *
 *              Compute and return a minimum II based on the resource 
 *		counts in 'counts'. This is the first II at or above
 *		the minimum cycle count that is a possible II.  (Some 
 *		operations have resource requirements that prevent from 
 *		from being scheduled in the cycle count, e.g., something
 *		that wants a resource in its 1st and 3rd cycles cannot 
 *		be scheduled in 3 cycles if there is only one of these 
 *		resources.)
 *
 *         void
 *         TI_RES_COUNT_Emit_Note(
 *              const char *prefix,		       
 *              FILE *fp,
 *              TI_RES_COUNT *res,
 *              INT ii
 *         )
 *              Emit resource usage to asm note.
 * 
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef ti_res_count_INCLUDED
#define ti_res_count_INCLUDED

#include "topcode.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _KEEP_RCS_ID
static const char ti_res_count_rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/access/ti_res_count.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

typedef struct ti_res_count TI_RES_COUNT;

extern TI_RES_COUNT *
TI_RES_COUNT_Alloc(
  MEM_POOL *pool
);

extern void
TI_RES_COUNT_Add_Op_Resources(
  TI_RES_COUNT *counts,
  TOP           opcode
);

extern void
TI_RES_COUNT_Add_Op_Resources_Scaled(
  TI_RES_COUNT *counts,
  TOP           opcode,
  double	factor
);

extern void
TI_RES_COUNT_Subtract_Op_Resources(
  TI_RES_COUNT *counts,
  TOP           opcode
);

extern void
TI_RES_COUNT_Subtract_Op_Resources_Scaled(
  TI_RES_COUNT *counts,
  TOP           opcode,
  double	factor
);

extern void
TI_RES_COUNT_Add(
  TI_RES_COUNT *sum,
  TI_RES_COUNT *addend1,
  TI_RES_COUNT *addend2
);

extern void
TI_RES_COUNT_Subtract(
  TI_RES_COUNT *difference,
  TI_RES_COUNT *minuend,
  TI_RES_COUNT *subtrahend
);

extern double
TI_RES_COUNT_Min_Cycles(
  TI_RES_COUNT *counts
);

extern INT32
TI_RES_COUNT_Min_II(
  TI_RES_COUNT *counts
);

extern void
TI_RES_COUNT_Print(
  FILE *fp,
  TI_RES_COUNT *res
);

extern void
TI_RES_COUNT_Emit_Note(
  const char *prefix,		       
  FILE *fp,
  TI_RES_COUNT *res,
  INT ii
);

#ifdef __cplusplus
}
#endif
#endif /* ti_res_count_INCLUDED */
