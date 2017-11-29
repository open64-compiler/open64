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


/* =======================================================================
 * =======================================================================
 *
 *  Module: gtn_universe.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:25-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.gtn_universe.h $
 *
 *  Revision comments:
 *
 *  13-Oct-1994 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Impliments the universe of global TNs over which we define GTN_SETs.
 *  It is very advantageous to do this as there are far fewer global TNs
 *  than local TNs.
 *
 *  Reserved prefix:
 *  ================
 *
 *      GTN_UNIVERSE
 *
 *  Exported functions:
 *  ===================
 *
 *      void GTN_UNIVERSE_Pu_Begin(void)
 *
 *          Initialize GTN for the current program unit.
 *
 *
 *      void GTN_UNIVERSE_Pu_End(void)
 *
 *          All done with GTN sets in current PU.  Release any memory and
 *          destruct ay data structures.
 *
 *
 *      void GTN_UNIVERSE_Add_TN(
 *          TN *tn
 *      )
 *
 *          Register 'tn' as a global.  There is no going back.
 *
 *
 *  Exported variables:
 *  ===================
 *
 *      const INT32 GTN_UNIVERSE_size
 *
 *          Number of TNs in the GTN universe.
 *
 *
 *  Macros exported only to GTN_SET functions:
 *  ==========================================
 *
 *      INT32 GTN_UNIVERSE_TN_int(
 *          TN *tn
 *      )
 *
 *          Returns the global TN number of 'tn'.  If it has not been
 *          added to the universe, return 0.
 *
 *
 *      TN *GTN_UNIVERSE_INT_tn(
 *          INT32 i
 *      )
 *
 *          Returns the global TN whose number in the GTN universe is
 *          'i'.
 *
 *  Iteration over the members of the GTN universe:
 *  ===============================================
 *
 *      Iterator type GTN_UNIVERSE_ITER
 *
 *          Iterates over all the GTNs in the current REGION.
 *
 *          Supports:
 *
 *              void GTN_UNIVERSE_ITER_Init( GTN_UNIVERSE_ITER *iter )
 *              BOOL GTN_UNIVERSE_ITER_Done( const GTN_UNIVERSE_ITER *iter )
 *              TN* GTN_UNIVERSE_ITER_Current( GTN_UNIVERSE_ITER *iter )
 *              void GTN_UNIVERSE_ITER_Step( GTN_UNIVERSE_ITER *iter )
 *
 *  
 * =======================================================================
 * =======================================================================
 */

#ifndef GTN_UNIVERSE_INCLUDED
#define GTN_UNIVERSE_INCLUDED


#ifdef GTN_IMPLEMENTATION
#define GTN_CONST
#else
#define GTN_CONST const
#endif

extern GTN_CONST INT32  GTN_UNIVERSE_size;
extern INT32  First_REGION_GTN;
extern GTN_CONST INT32 *GTN_CONST GTN_UNIVERSE_tn_int_map;
extern TN   *GTN_CONST *GTN_CONST GTN_UNIVERSE_int_tn_map;
extern INT32  tn_int_map_allocated_size;
extern INT32  int_tn_map_allocated_size;


extern void GTN_UNIVERSE_Pu_Begin(void);
extern void GTN_UNIVERSE_REGION_Begin(void);
extern void GTN_UNIVERSE_Pu_End(void);
extern void GTN_UNIVERSE_Add_TN( TN *tn );

inline INT32
GTN_UNIVERSE_TN_int( TN *tn)
{
  if (!(TN_is_register(tn) && 
	TN_is_global_reg(tn) &&
	(TN_number(tn) < tn_int_map_allocated_size)))
    return 0;

  return GTN_UNIVERSE_tn_int_map[TN_number(tn)];
}


inline TN *
GTN_UNIVERSE_INT_tn (INT32 i) 
{
  Is_True (i < GTN_UNIVERSE_size, ("GTN_UNIVERSE_INT_tn: %d out of range", i));
  return GTN_UNIVERSE_int_tn_map[i];
}


typedef INT32 GTN_UNIVERSE_ITER;    /* Well, it's a simple iterator... */

/* =======================================================================
 *
 *  GTN_UNIVERSE_ITER_Init
 *  GTN_UNIVERSE_ITER_Done
 *  GTN_UNIVERSE_ITER_Current
 *  GTN_UNIVERSE_ITER_Step
 *
 *      Iterator functions.  See interface description.
 *
 * =======================================================================
 */

inline void
GTN_UNIVERSE_ITER_Init( GTN_UNIVERSE_ITER* iter )
{
  *iter = GTN_UNIVERSE_size - 1;
}

inline BOOL
GTN_UNIVERSE_ITER_Done( const GTN_UNIVERSE_ITER* iter )
{
  return *iter < First_REGION_GTN;
}

inline TN*
GTN_UNIVERSE_ITER_Current( const GTN_UNIVERSE_ITER* iter )
{
  return GTN_UNIVERSE_INT_tn(*iter);
}

inline void
GTN_UNIVERSE_ITER_Step( GTN_UNIVERSE_ITER* iter )
{
  --*iter;
}
#endif
