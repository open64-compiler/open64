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


//-*-c++-*-
//=====================================================================
//
// Module: rbi.h
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:58-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.rbi.h $
//
// Revision history:
//  7-SEP-95 dahl - Original Version
//
// Description:
//	Region Boundary Info (RBI)
//
//======================================================================
//======================================================================

#ifndef rbi_INCLUDED
#define rbi_INCLUDED "rbi.h"
#ifdef _KEEP_RCS_ID
static char *rbircs_id = rbi_INCLUDED"$ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "region_util.h"

const INT32 WN_PROP_USED_IN  = 0x01,
	    WN_PROP_DEF_IN   = 0x02,
	    WN_PROP_LIVE_OUT = 0x04;

class RBI {

private:
  INT32	    _trace_level;		// -ttTP_REGION(41), -ttRGN
  MEM_POOL *_pool;
  WN_MAP    _prop_map;
  WN_MAP    _tag_map;			// WN map, mark nodes with region id

public:
  RBI(MEM_POOL *);			// pool must be supplied to constructor
  ~RBI(void);

  // 0 - no trace
  // 1 - major function trace only (TT_REGION_RBI_DEBUG and other debug bits)
  // 2 - detailed trace (only RBI bit set TT_REGION_RBI_DEBUG)
  BOOL Trace( void ) const		{ return _trace_level > 0; }
  BOOL Trace2( void ) const		{ return _trace_level == 2; }

  MEM_POOL *Mem_Pool( void )            { return _pool; }

  void RBI_Calc_Kids(RID *root, const DU_MANAGER *du_mgr,
		     const ALIAS_MANAGER *am);

  void RBI_Calc_Rgn(RID *const rid_node, const DU_MANAGER *const du_mgr,
		    const ALIAS_MANAGER *const am);

  INT32 Rgn_WN_Tag( WN *const wn ) const
    {
      return WN_MAP32_Get( _tag_map, wn );
    }

  void Set_WN_Cur_Rgn( WN* wn, INT32 rgn_id )
    {
      WN_MAP32_Set( _tag_map, wn, rgn_id );
    }

  BOOL Outside_Cur_Rgn( WN* wn, INT32 rgn_id ) const
    {
      // RETURNs are always considered to lie outside since they
      // amount to exits from the region and anything that could be
      // used by the exited-to code is found on the RETURN's
      // def-list.
      if (WN_opcode(wn) == OPC_RETURN)
	return TRUE;
      else
	return (Rgn_WN_Tag(wn) != rgn_id);
    }

  INT32 Get_WN_Prop( WN *const wn ) const
    {
      return WN_MAP32_Get( _prop_map, wn );
    }

  void Set_WN_Prop( WN *const, const INT32 );

  void Set_PT_SET_All_Aliasable(POINTS_TO_SET *);

  void Add_To_PT_SET(POINTS_TO_SET *, POINTS_TO *);

};	// end of class RBI

extern "C" void Region_Bound_Info(WN *tree,
				  DU_MANAGER *du_mgr,
				  ALIAS_MANAGER *am);

#endif /* ifdef rbi_INCLUDED */
