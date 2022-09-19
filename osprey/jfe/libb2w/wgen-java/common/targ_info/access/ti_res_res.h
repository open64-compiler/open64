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
 *  Module: ti_res_res.h
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/access/ti_res_res.h,v $
 *
 *  Synopsis:
 *
 *      Resource accounting package for various clients including
 *	the software pipeliner and the local (BB) scheduler. The pipeliner
 *      has different needs than the BB scheduler.  In particular,
 *	it needs to be able to unreserve as well as reserve resources.
 *	It also needs to be able compare resource requests for relevance 
 *	and equivalence during backtracking.
 *
 *  Interface Description:
 *
 *	Exported types:
 *
 *	    TI_RES_RES
 *
 *		Opaque type to maintain context for resource reservation
 *		and resource related inquiries.
 *
 *      Exported functions:
 *
 *	    TI_RES_RES *TI_RES_RES_Alloc(
 *		BOOL      cyclic
 *		MEM_POOL *pool
 *	    )
 *
 *		Allocate a TI_RES_RES structure for managing resources for a
 *		basic block. 'cyclic' indicates if the BB is being scheduled 
 *		as a loop. 'pool' is used to allocate dynamic memory during 
 *		creation of the TI_RES_RES structure and for future 
 *		maintenence of the structure. 
 *
 *		After this call, the client must make the cycle count
 *		of the BB known via TI_RES_RES_Set_BB_Cycle_Count.
 *		And for cyclic scheduling, TI_RES_RES_Has_TOP should
 *		be called for each instruction in the BB.
 *
 *		Note that there is no corresponding "free" call, deallocating
 *		the pool memory is the only cleanup necessary.
 *
 *	    void TI_RES_RES_Has_TOP(
 *		TI_RES_RES *res
 *		TOP         opcode
 *	    )
 *
 *		Before software pipelining using 'res', this must be called at
 *		least once for each unique 'opcode' to be scheduled in the
 *		loop.
 *
 *	    void TI_RES_RES_Set_BB_Cycle_Count(
 *		TI_RES_RES  *res
 *		INT          length
 *	    )
 *
 *		Sets the cycle count to 'length', adjusting internal
 *		data structures as necessary, including resetting
 *		the resource reservation table to "empty".
 *
 *	    BOOL TI_RES_RES_Resources_Available(
 *		TI_RES_RES  *res
 *		TOP          opcode
 *		INT          cycle
 *	    )
 *
 *              Check to see if resources for 'opcode' are available at
 *		the given 'cycle'. Returns TRUE for available; FALSE 
 *		otherwise.
 *
 *	    void TI_RES_RES_Reserve_Resources(
 *		TI_RES_RES  *res
 *		TOP          opcode
 *		INT          cycle
 *	    )
 *
 *              Reserve resources for 'opcode' at the given 'cycle'. It is
 *		an error, resulting in undefined behavior, if the 
 *		resources are not available.
 *
 *	    void TI_RES_RES_Unreserve_Resources(
 *		TI_RES_RES  *res
 *		TOP          opcode
 *		INT          cycle
 *	    )
 *
 *              Unreserve resources for 'opcode' at the given 'cycle'. It 
 *		is an error, resulting in undefined behavior, if the 
 *		resources have not been reserved.
 *
 *	    BOOL TI_RES_RES_Is_Bad_II(
 *		TI_RES_RES  *res
 *		INT          ii
 *          )
 *
 *		For cyclic scheduling, return TRUE if the given 'ii' is not 
 *		possible based on resource usage, FALSE otherwise.
 *		The function always returns FALSE for non-cyclic scheduling.
 *
 *
 *	    BOOL TI_RES_RES_Resources_Relevant(
 *		TI_RES_RES  *res
 *		TOP          opcode1
 *		TOP          opcode2
 *		INT          offset
 *	    )
 *
 *              Are the resource requirements of 'opcode1' issued 'offset'
 *              cycles before 'opcode2' relevant to 'opcode2'?  The question
 *              really is: "Is there a resource class of which 'opcode1'
 *              requires a member, and which 'opcode2' also requires a
 *              member in the same cycle when 'opcode2' is issued 'offset'
 *              cycles after 'opcode1'?  If there are some resources
 *              common to all the OPs in the loop (such as issue
 *              slot), these do not count.
 *
 *		NOTE: only supported for cyclic scheduling.
 *
 *	    BOOL TI_RES_RES_Resources_Equivalent(
 *		TI_RES_RES  *res
 *		TOP          opcode1
 *		TOP          opcode2
 *	    )
 *
 *              Do 'opcode1' and 'opcode2' have exactly the same resource
 *              requirements?
 *
 *	    BOOL TI_RES_RES_Resources_Grainy(
 *		TI_RES_RES  *res
 *		TOP          opcode
 *	    )
 *
 *              Does 'opcode' use non-common resources in any cycle other
 *              than its first.  (TODO: This is only a second cousin
 *              of the right definition.  It will probably work pretty
 *              well for the r4k, but fails to capture the TFP LD, IST
 *              interaction, for example.)
 *
 *		NOTE: only supported for cyclic scheduling.
 *
 *          INT TI_RES_RES_Resources_Length(TI_RES_RES *res)
 *              Returns length of the resources
 *
 *          void TI_RES_RES_Print(FILE *fp, TI_RES_RES *)
 *              Prints the resources reservation table for debugging.
 *
 *          BOOL TI_RES_RES_Equal(TI_RES_RES *res1, TI_RES_RES *res2)
 *              Returns TRUE if res1 and res1 are identical.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef ti_res_res_INCLUDED
#define ti_res_res_INCLUDED

#include "topcode.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _KEEP_RCS_ID
static const char ti_res_res_rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/access/ti_res_res.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

typedef struct ti_res_res TI_RES_RES;

extern TI_RES_RES *TI_RES_RES_Alloc(
  BOOL      cyclic,
  MEM_POOL *pool
);

extern void TI_RES_RES_Has_TOP(
  TI_RES_RES *res,
  TOP         opcode
);

extern void TI_RES_RES_Set_BB_Cycle_Count(
  TI_RES_RES  *res,
  INT          cycles
);

extern BOOL TI_RES_RES_Resources_Available(
  TI_RES_RES  *res,
  TOP          opcode,
  INT          cycle
);

extern void TI_RES_RES_Reserve_Resources(
  TI_RES_RES  *res,
  TOP          opcode,
  INT          cycle
);

extern void TI_RES_RES_Unreserve_Resources(
  TI_RES_RES  *res,
  TOP          opcode,
  INT          cycle
);

extern BOOL TI_RES_RES_Is_Bad_II(
  TI_RES_RES  *res,
  INT          ii
);

extern BOOL TI_RES_RES_Resources_Relevant(
  TI_RES_RES  *res,
  TOP          opcode1,
  TOP          opcode2,
  INT          offset
);

extern BOOL TI_RES_RES_Resources_Equivalent(
  TI_RES_RES  *res,
  TOP          opcode1,
  TOP          opcode2
);

extern BOOL TI_RES_RES_Resources_Grainy(
  TI_RES_RES  *res,
  TOP          opcode
);

extern BOOL TI_RES_RES_Resources_Length(
  TI_RES_RES  *res,
  TOP          opcode
);

extern void TI_RES_RES_Print(FILE *fp, TI_RES_RES *res);

extern BOOL TI_RES_RES_Equal(TI_RES_RES *res1, TI_RES_RES *res2);

#ifdef __cplusplus
}
#endif
#endif /* ti_res_res_INCLUDED */
