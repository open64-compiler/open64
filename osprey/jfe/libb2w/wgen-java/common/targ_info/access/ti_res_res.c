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


static const char source_file[] = __FILE__;
static const char rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/access/ti_res_res.c,v $ $Revision: 1.1.1.1 $";

#include <alloca.h>
#include <limits.h>

#include "defs.h"
#include "erglob.h"
#include "mempool.h"
#include "bitset.h"
#include "ti_si.h"

#include "ti_res_res.h"

/* Declare the TI_RES_RES opaque type (a context for resource reservation 
 * actitivies):
 */
struct ti_res_res {
  MEM_POOL *pool;		/* For our dynamic memory needs		     */
  SI_RRW   *rrtab;		/* First cycle (word) of table		     */
  INT32     length;		/* Current length of the table		     */
  INT32     alloc_size;		/* Its allocated size (in cycles)	     */
  BOOL      cyclic;		/* Is this a schedule for a loop, e.g.	     */
				/* modulo scheduling for software pipelining */

  /* Fields used only for cyclic scheduling:
   */
  BS       *si_ids;		/* Set of the scheduling information IDs     */
				/* being scheduled                           */
  UINT      min_rr_length;	/* minimum length of any or the RRs being    */
				/* scheduled.				     */
  SI_RESOURCE_ID_SET *uncommon_res_ids;
  				/* ith element is a set of resource IDs used */
				/* in the ith cycle by some, but not all the */
				/* opcodes being scheduled.                  */
  SI_BAD_II_SET bad_iis;	/* Impossible IIs given the opcodes being    */
				/* scheduled				     */
};

/* TI_RES_RES accessors:
 */
#define TI_RES_RES_pool(t)		((t)->pool)
#define TI_RES_RES_rrtab(t)		((t)->rrtab)
#define TI_RES_RES_length(t)		((t)->length)
#define TI_RES_RES_alloc_size(t)	((t)->alloc_size)
#define TI_RES_RES_cyclic(t)		((t)->cyclic)
#define TI_RES_RES_si_ids(t)		((t)->si_ids)
#define TI_RES_RES_min_rr_length(t)	((t)->min_rr_length)
#define TI_RES_RES_uncommon_res_ids(t)	((t)->uncommon_res_ids)
#define TI_RES_RES_bad_iis(t)		((t)->bad_iis)


/* ====================================================================
 *
 *  Cycle_Mod_II
 *
 *  Return the cycle number modulo the II. See be/cg/swp_ii_funcs.[ch]
 *  for a discussion of why we just don't use the remainder operator
 *  (performance).
 *
 *  TODO: in general the cycle should be close, so the simple loop is 
 *  probably good enough. Verify that this is true.
 *
 * ====================================================================
 */
static INT32
Cycle_Mod_II(
  INT32 cyc,
  INT32 ii
)
{
  if ( cyc < 0 ) {
    do {
      cyc += ii;
    } while (cyc < 0);
  } else if (cyc >= ii) {
    do {
      cyc -= ii;
    } while (cyc >= ii);
  }

  return cyc;
}


/* ====================================================================
 *
 *  TI_RES_RES_Has_TOP
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_RES_RES_Has_TOP( 
  TI_RES_RES *res,
  TOP         opcode
)
{
  if ( !BS_MemberP(TI_RES_RES_si_ids(res), TSI_Id(opcode) ) ) {
    UINT rr_length;

    TI_RES_RES_si_ids(res) = BS_Union1D(TI_RES_RES_si_ids(res),
					TSI_Id(opcode),
					TI_RES_RES_pool(res));
    TI_RES_RES_bad_iis(res) = SI_BAD_II_SET_Union(TI_RES_RES_bad_iis(res),
						  TSI_Bad_IIs(opcode));

    rr_length = SI_RR_Length(TSI_Resource_Requirement(opcode));
    if ( rr_length < TI_RES_RES_min_rr_length(res) ) {
      TI_RES_RES_min_rr_length(res) = rr_length;
    }
  }
}


/* Set up some constrol variables common to Resource_Available,
 * Reserve_Resources, and Unreserve_Resources.  The idea is to break the
 * work into two loops and obviate the need to do a Mod operation for every
 * cycle checked.  The first should check <length1> cycles of <rr>, starting
 * with cycle 0 of <rr> and cycle <cycle_mod_ii> of the schedule.  The
 * second loop should theck <length1> cycles of <rr>, starting with cycle
 * <length1> of <rr> and cycle 0 of the schedule.
 */
static void Check_Reserve_Loop_Control(
  TI_RES_RES *res,
  TOP         opcode, 
  INT         cycle,
  SI_RR      *rr,
  INT        *length1, 
  INT        *length2,
  INT        *cycle_mod_ii
)
{
  INT32 rr_length;
  INT32 length = TI_RES_RES_length(res);

  if ( TI_RES_RES_cyclic(res) ) {
    *rr = TSI_II_Resource_Requirement(opcode,length);
  }
  else {
    *rr = TSI_Resource_Requirement(opcode);
  }
  *cycle_mod_ii = Cycle_Mod_II(cycle,length);

  rr_length = SI_RR_Length(*rr);
  if ( *cycle_mod_ii + rr_length <= length ) {
    *length1 = rr_length;
    *length2 = 0;
  }
  else {
    *length1 = length - *cycle_mod_ii;
    *length2 = rr_length - *length1;
  }
}


/* ====================================================================
 *
 *  TI_RES_RES_Set_BB_Cycle_Count
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_RES_RES_Set_BB_Cycle_Count(
  TI_RES_RES  *res,
  INT          length
)
{
  INT  i;
  BOOL cyclic = TI_RES_RES_cyclic(res);

  if ( length > TI_RES_RES_alloc_size(res) ) {
    INT new_alloc_size = length * 2;
    TI_RES_RES_alloc_size(res) = new_alloc_size;

    TI_RES_RES_rrtab(res) = TYPE_MEM_POOL_ALLOC_N(SI_RRW,
					TI_RES_RES_pool(res),
					new_alloc_size);
    if ( cyclic ) {
      TI_RES_RES_uncommon_res_ids(res)
	= TYPE_MEM_POOL_ALLOC_N(SI_RESOURCE_ID_SET,
				TI_RES_RES_pool(res),
				new_alloc_size);
    }
  }

  TI_RES_RES_length(res) = length;

  /* Initialize the part of the table we will use
   */
  for ( i = 0; i < length; ++i ) {
    TI_RES_RES_rrtab(res)[i] = SI_RRW_Initial();
  }

  if ( cyclic ) {
    INT id;
    BS *si_ids = TI_RES_RES_si_ids(res);
    SI_RESOURCE_ID_SET *uncommon_res_ids = TI_RES_RES_uncommon_res_ids(res);
    INT common_length = MIN(TI_RES_RES_min_rr_length(res), length);

    /* For each cycle, compute the set of resources that not all the OPs in
     * the loop use in that cycle.  We do this by computing its complement --
     * the set of resources that all the OPs use in the cycle -- and then
     * complementing it in place.
     */

    /* Compute common resources into "uncommon_res_ids"
     *
     * NOTE: the following loop also initializes the "res_ids"
     * from common_length to the end of the vector. These
     * are by definition not common to all OPs, and we will leave
     * the setting unchanged in the following loops.
     */
    for ( i = 0; i < length; ++i ) {
      uncommon_res_ids[i] = SI_RESOURCE_ID_SET_Universe();
    }

    for ( id = BS_Choose(si_ids); id != BS_CHOOSE_FAILURE;
                                  id = BS_Choose_Next(si_ids,id)
    ) {
      const SI_RESOURCE_ID_SET* resource_ids_used
        = SI_ID_II_Cycle_Resource_Ids_Used(id,length);

      for ( i = 0; i < common_length; ++i ) {
        uncommon_res_ids[i]
          = SI_RESOURCE_ID_SET_Intersection(uncommon_res_ids[i],
                                            resource_ids_used[i]);
      }
    }

    /* Complement in place
     */
    for ( i = 0; i < common_length; ++i ) {
      uncommon_res_ids[i] =
        SI_RESOURCE_ID_SET_Complement(uncommon_res_ids[i]);
    }
  }
}


/* ====================================================================
 *
 *  TI_RES_RES_Alloc
 *
 *  See interface description
 *
 * ====================================================================
 */
TI_RES_RES *TI_RES_RES_Alloc(
  BOOL      cyclic,
  MEM_POOL *pool
)
{
  TI_RES_RES *res = TYPE_MEM_POOL_ALLOC(TI_RES_RES, pool);

  TI_RES_RES_pool(res) = pool;
  TI_RES_RES_cyclic(res) = cyclic;
  TI_RES_RES_bad_iis(res) = SI_BAD_II_SET_Empty();
  TI_RES_RES_length(res) = 0;
  TI_RES_RES_alloc_size(res) = 0;
  TI_RES_RES_min_rr_length(res) = UINT_MAX;

  if ( cyclic ) {
    TI_RES_RES_si_ids(res) = BS_Create_Empty(SI_ID_Count(), pool);
  }

  return res;
}


/* ====================================================================
 *
 *  TI_RES_RES_Resources_Available
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_RES_RES_Resources_Available(
  TI_RES_RES  *res,
  TOP          opcode,
  INT          cycle
)
{
  INT     cycle_mod_ii;
  INT     length1;
  INT     length2;
  INT     i;
  SI_RR   rr;
  SI_RRW *rrtab = TI_RES_RES_rrtab(res);

  Check_Reserve_Loop_Control(res,opcode,cycle,
			     &rr,&length1,&length2,&cycle_mod_ii);

  for ( i = 0; i < length1; ++i ) {
    SI_RRW reserved = SI_RRW_Reserve(rrtab[cycle_mod_ii+i],
                                     SI_RR_Cycle_RRW(rr,i));
    if ( SI_RRW_Has_Overuse(reserved) ) return FALSE;
  }

  for ( i = 0; i < length2; ++i ) {
    SI_RRW reserved = SI_RRW_Reserve(rrtab[i],SI_RR_Cycle_RRW(rr,i+length1));
    if ( SI_RRW_Has_Overuse(reserved) ) return FALSE;
  }

  return TRUE;
}


/* ====================================================================
 *
 *  TI_RES_RES_Reserve_Resources
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_RES_RES_Reserve_Resources(
  TI_RES_RES  *res,
  TOP          opcode,
  INT          cycle
)
{
  INT     cycle_mod_ii;
  INT     length1;
  INT     length2;
  INT     i;
  SI_RR   rr;
  SI_RRW *rrtab = TI_RES_RES_rrtab(res);

  Check_Reserve_Loop_Control(res,opcode,cycle,
			     &rr,&length1,&length2,&cycle_mod_ii);

  for ( i = 0; i < length1; ++i ) {
    rrtab[cycle_mod_ii+i]
      = SI_RRW_Reserve(rrtab[cycle_mod_ii+i],SI_RR_Cycle_RRW(rr,i));
  }

  for ( i = 0; i < length2; ++i ) {
    rrtab[i] = SI_RRW_Reserve(rrtab[i],SI_RR_Cycle_RRW(rr,i+length1));
  }
}


/* ====================================================================
 *
 *  TI_RES_RES_Unreserve_Resources
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_RES_RES_Unreserve_Resources(
  TI_RES_RES  *res,
  TOP          opcode,
  INT          cycle
)
{
  INT     cycle_mod_ii;
  INT     length1;
  INT     length2;
  INT     i;
  SI_RR   rr;
  SI_RRW *rrtab = TI_RES_RES_rrtab(res);

  Check_Reserve_Loop_Control(res,opcode,cycle,
			     &rr,&length1,&length2,&cycle_mod_ii);

  for ( i = 0; i < length1; ++i ) {
    rrtab[cycle_mod_ii+i]
      = SI_RRW_Unreserve(rrtab[cycle_mod_ii+i],SI_RR_Cycle_RRW(rr,i));
  }

  for ( i = 0; i < length2; ++i ) {
    rrtab[i] = SI_RRW_Unreserve(rrtab[i],SI_RR_Cycle_RRW(rr,i+length1));
  }
}


/* ====================================================================
 *
 *  TI_RES_RES_Is_Bad_II
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_RES_RES_Is_Bad_II(
  TI_RES_RES  *res,
  INT          ii
)
{
  return SI_BAD_II_SET_MemberP(TI_RES_RES_bad_iis(res),ii);
}


/* ====================================================================
 *
 *  TI_RES_RES_Resources_Relevant
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_RES_RES_Resources_Relevant(
  TI_RES_RES  *res,
  TOP          opcode1,
  TOP          opcode2,
  INT          offset
)
{
  INT length1, length2, i;
  const INT32 length = TI_RES_RES_length(res);
  const SI_RESOURCE_ID_SET *const res_ids1
    = TSI_II_Cycle_Resource_Ids_Used(opcode1,length);
  const SI_RESOURCE_ID_SET *const res_ids2
    = TSI_II_Cycle_Resource_Ids_Used(opcode2,length);
  const INT rr1_length
    = SI_RR_Length(TSI_II_Resource_Requirement(opcode1,length));
  const INT rr2_length
    = SI_RR_Length(TSI_II_Resource_Requirement(opcode2,length));
  const INT offset_mod_ii = Cycle_Mod_II(offset,length);
  const SI_RESOURCE_ID_SET *const uncommon_res_ids 
    = TI_RES_RES_uncommon_res_ids(res);

  FmtAssert (TI_RES_RES_cyclic(res),
  	("TI_RES_RES_Resources_Relevant not applicable to non-cyclic schedules"));

  /* Check from the start of rr2 until either the end of rr2 or the end of
   * rr1 + offset (which cannot be greater than II-1.)
   */
  length1 = rr1_length - offset_mod_ii;
  if ( rr2_length < length1 ) length1 = rr2_length;

  for ( i = 0; i < length1; ++i ) {
    if ( SI_RESOURCE_ID_SET_Intersection4_Non_Empty(
           res_ids1[i + offset_mod_ii],
           uncommon_res_ids[i + offset_mod_ii],
           res_ids2[i],
           uncommon_res_ids[i] )
    ) {
      return TRUE;
    }
  }

  /* The resource requirements for opcode1 (rr1) and opcode2 (rr2)
   * are already modulo the II. But we are comparing rr2 at an offset
   * from rr1, therefore we may have some cycles and the end of rr2 
   * that wrap around to the beginning of rr1. If that is the case,
   * check those cycles. Note that rr2 can only wrap once (because
   * we have use the offset mod II), and some cycles in the middle 
   * of rr2 may not need to be checked against rr1 (because rr1 might
   * consume no resource in those cycles).
   */
  length2 = (rr2_length + offset_mod_ii) - length;
  if ( length > 0 ) {
    if ( rr1_length < length2 ) length2 = rr1_length;

    for ( i = 0; i < length2; ++i ) {
      if ( SI_RESOURCE_ID_SET_Intersection4_Non_Empty(
             res_ids1[i],
             uncommon_res_ids[i],
             res_ids2[i + length - offset_mod_ii],
             uncommon_res_ids[i + length - offset_mod_ii] )
      ) {
        return TRUE;
      }
    }
  }

  return FALSE;
}


/* ====================================================================
 *
 *  TI_RES_RES_Resources_Equivalent
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_RES_RES_Resources_Equivalent(
  TI_RES_RES  *res,
  TOP          opcode1,
  TOP          opcode2
)
{
  INT i;
  const INT32 length = TI_RES_RES_length(res);
  SI_RR rr1 = TSI_II_Resource_Requirement(opcode1,length);
  SI_RR rr2 = TSI_II_Resource_Requirement(opcode2,length);

  if ( rr1 == rr2 ) return TRUE;

  if ( SI_RR_Length(rr1) != SI_RR_Length(rr2) ) return FALSE;

  for (i = 0; i < SI_RR_Length(rr1); ++i) {
    if ( SI_RR_Cycle_RRW(rr1,i) != SI_RR_Cycle_RRW(rr2,i) )
      return FALSE;
  }

  return TRUE;
}


/* ====================================================================
 *
 *  TI_RES_RES_Resource_Grainy
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_RES_RES_Resources_Grainy(
  TI_RES_RES  *res,
  TOP          opcode
)
{
  INT i;
  const INT32 length = TI_RES_RES_length(res);
  SI_RESOURCE_ID_SET *uncommon_res_ids = TI_RES_RES_uncommon_res_ids(res);
  UINT min_rr_length = TI_RES_RES_min_rr_length(res);
  const SI_RESOURCE_ID_SET* res_used
    = TSI_II_Cycle_Resource_Ids_Used(opcode,length);
  INT res_used_length
    = SI_RR_Length(TSI_II_Resource_Requirement(opcode,length));

  if ( min_rr_length < res_used_length ) return TRUE;

  for ( i = 0; i < min_rr_length; ++i ) {
    if ( SI_RESOURCE_ID_SET_Intersection_Non_Empty(res_used[i],
                                                   uncommon_res_ids[i])
    ) {
      return TRUE;
    }
  }

  return FALSE;
}


INT TI_RES_RES_Resources_Length(
  TI_RES_RES  *res,
  TOP          opcode
)
{
  return SI_RR_Length(TSI_Resource_Requirement(opcode));
}


void TI_RES_RES_Print(FILE *fp, TI_RES_RES *res)
{
  INT i;
  for (i = 0; i < TI_RES_RES_length(res); i++) 
    fprintf(fp, "%d --> 0x%0llx\n", i,  TI_RES_RES_rrtab(res)[i]);
}


BOOL TI_RES_RES_Equal(TI_RES_RES *res1, TI_RES_RES *res2)
{
  INT i;
  if (TI_RES_RES_length(res1) != TI_RES_RES_length(res2)) return FALSE;
  for (i = 0; i < TI_RES_RES_length(res1); i++) 
    if (TI_RES_RES_rrtab(res1)[i] != TI_RES_RES_rrtab(res2)[i])
      return FALSE;
  return TRUE;
}

