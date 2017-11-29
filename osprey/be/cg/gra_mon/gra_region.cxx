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

//
//  GRA_REGION -- Region encapsulation
/////////////////////////////////////
//
//  Description:
//
//      Implementation details for GRA_REGIONs.
//
/////////////////////////////////////


//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:30-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_region.cxx $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_region.cxx $ $Revision: 1.2 $";
#endif

#include "defs.h"
#include "cg.h"
#include "register.h"
#include "gra_bb.h"
#include "gra_lrange.h"
#include "gra_region.h"

GRA_REGION_MGR gra_region_mgr;

/////////////////////////////////////
// Add <gbb> to the list of BBs associated with <region>.
void 
GRA_REGION::Add_GBB( GRA_BB* gbb )
{
  if ( first_gbb != NULL )
    last_gbb->Region_Next_Set(gbb);
  else first_gbb = gbb;
    
  gbb->Region_Next_Set(NULL);
  last_gbb = gbb;

  if (BB_call(gbb->Bb()))
    flags = (REGION_FLAG) (flags | REGION_FLAGS_has_call);
}

/////////////////////////////////////
// allocate the lrange subuniverses for a region.
void 
GRA_REGION::Alloc_Subuniverses(void)
{
  ISA_REGISTER_CLASS rc;
  INT subuniverse_size;

  //  Using the GTN universe size is probably way too conservative
  //  Most likely, there will some in each class, and the ones in the
  //  regions will be transparent to the complement.  divide total
  //  universe size by region count for each region.
  if (Rid() == Current_Rid) {
    subuniverse_size = GTN_UNIVERSE_size;
  } else {
    subuniverse_size = GTN_UNIVERSE_size / gra_region_mgr.Alloc_Count();
  }
  if (subuniverse_size == 0) ++subuniverse_size;	// don't want 0
  FOR_ALL_ISA_REGISTER_CLASS( rc ) {
    subuniverse[rc] = lrange_sub_mgr.Create(subuniverse_size);
  }
}

/////////////////////////////////////
// Add <lrange> to the appropriate subuniverse of <region>.
void
GRA_REGION::Add_LRANGE( LRANGE*     lrange)
{
  if ( lrange->Type() == LRANGE_TYPE_LOCAL )
    local_subuniverse->Add(lrange);
  else subuniverse[lrange->Rc()]->Add(lrange);
  ++lrange_count[lrange->Rc()];
}

/////////////////////////////////////
// Returns total number of cycles in all the scheduled BBs in the region
INT32
GRA_REGION::Cycle_Count(void)
{
  GRA_REGION_GBB_ITER iter;
  INT32 result = 0;

  for (iter.Init(this); ! iter.Done(); iter.Step() ) {
    GRA_BB* gbb = iter.Current();
    result += OP_scycle(BB_last_op(gbb->Bb()));
  }
  return result;
}

/////////////////////////////////////
// initialize the fields in GRA_REGION_MGR; called at start of GRA
void
GRA_REGION_MGR::Initialize(void)
{
  _first_prev_alloc_region = NULL;
  _complement_region_created = FALSE;
  _map = TYPE_MEM_POOL_ALLOC_N(GRA_REGION*, GRA_pool, Last_Region_Id() + 1);
  bzero(_map, sizeof(GRA_REGION*) * (Last_Region_Id() + 1));
  _alloc_count = 0;
}

/////////////////////////////////////
//  Region creation common to complement and previously allocate regions.
GRA_REGION*
GRA_REGION_MGR::Create( RID* rid)
{
  ISA_REGISTER_CLASS rc;
  GRA_REGION* region = TYPE_MEM_POOL_ALLOC(GRA_REGION,GRA_pool);

  _alloc_count++;

  region->rid = rid;
  FOR_ALL_ISA_REGISTER_CLASS( rc ) {
    region->registers_used[rc] = REGISTER_SET_EMPTY_SET;
    region->registers_available[rc] = REGISTER_CLASS_allocatable(rc);
    region->lrange_count[rc] = 0;
  }

  region->first_gbb = NULL;
  region->flags = (REGION_FLAG) 0;
  return region;
}

/////////////////////////////////////
GRA_REGION*
GRA_REGION_MGR::Create_Complement( RID* rid )
{
  GRA_REGION* region = Create(rid);

  //  This is a totally wild guess of the number of local live ranges
  //  we'll need.  But an average of 10/BB doesn't seem too bad.  It's
  //  probably high because we are counting the BBs in the regions as well
  //  and there are probably less than 10 on average.  However,
  //  it just has to prevent some worst case behavior.
  region->local_subuniverse = lrange_sub_mgr.Create(PU_BB_Count * 10);

  _complement_region_created = TRUE;
  _complement_region = region;

  region->Alloc_Subuniverses();
  return region;
}

/////////////////////////////////////
GRA_REGION*
GRA_REGION_MGR::Create_Prev_Allocated(  RID* rid )
{
  GRA_REGION* region = Create(rid);

  region->next = _first_prev_alloc_region;
  _first_prev_alloc_region = region;
  return region;
}

/////////////////////////////////////
// Get GRA's encapsulation for the region identified by <rid>.  Creates it 
// if not already.  The region is allocated in the GRA_pool.  <rid> is the 
// RID associated with the blocks in the region.  For previously allocated
// regions there will be exactly one of this for all the blocks.  For the 
// complement, there may be more than one.  I don't know that we care.
GRA_REGION*
GRA_REGION_MGR::Get( RID* rid )
{
  if ( rid == NULL || ! RID_has_reg_alloc(rid) ) {
    if ( _complement_region_created )
      return _complement_region;
    else
      return Create_Complement(rid);
  }
  else {
    Is_True(RID_id(rid) <= Last_Region_Id(), ("Region out of bounds"));
    //
    // only want outermost region contained within this unit (which itself
    // may be a region).  all inner regions are to be treated as a black
    // box.  this is primarily to allow splitting to stop at region boundries,
    // but we don't recolor regions anyway, so who cares.  if we ever want
    // to recolor them, though, we'll have to undo this and treat regions as
    // we do loops (i.e. set the regs_used bit vectors by walking the trees
    // and such ... blech).
    //
    RID *rid_tmp;
    for (rid_tmp = rid;
	 RID_type(rid_tmp) != RID_TYPE_swp && RID_parent(rid_tmp) != Current_Rid;
	 rid_tmp = RID_parent(rid_tmp));
    if ( _map[RID_id(rid_tmp)] != NULL )
      return _map[RID_id(rid_tmp)];
    else
      return _map[RID_id(rid_tmp)] = Create_Prev_Allocated(rid);
  }
}


/////////////////////////////////////
// second state: stepping the locals
void
GRA_REGION_RC_LRANGE_ITER::Step_Locals(void)
{
  INT32 j;
  LRANGE_SUBUNIVERSE* l_su  = _region->Local_Subuniverse();

  for ( j = _i - 1; j >= 0; --j ) {
    LRANGE* current = l_su->Nth_Lrange(j);
    if ( current->Rc() == _rc ) {
      _i = j;
      _current = current;
      return;
    }
  }
  _state = done;
}

/////////////////////////////////////
// first state: stepping the non_locals
void
GRA_REGION_RC_LRANGE_ITER::Step_Non_Locals(void)
{
  if ( --_i >= 0 ) {
    _current = _region->Subuniverse(_rc)->Nth_Lrange(_i);
  }
  else if ( _region == gra_region_mgr.Complement_Region() ) {
    //  Switch states.  Set things up as if we had just stepped the nth+1
    //  local so we can just call Step_Locals.
    _i = _region->Local_Subuniverse()->Count();
    _state = in_locals;
    Step_Locals();
  }
  else _state = done;
}

/////////////////////////////////////
void
GRA_REGION_RC_LRANGE_ITER::Init(GRA_REGION*                region,
                                ISA_REGISTER_CLASS         rc )
{
  LRANGE_SUBUNIVERSE* nl_su = region->Subuniverse(rc);

  _region = region;
  _rc = rc;

  if (nl_su->Count() > 0 ) {
    _i = nl_su->Count() - 1;
    _current = nl_su->Nth_Lrange(_i);
    _state = in_non_locals;
  }
  else if ( _region == gra_region_mgr.Complement_Region() ) {
    //  Set things up as if we had just stepped the nth+1 local so we can just
    //  call Step_Locals.
    _i = _region->Local_Subuniverse()->Count();
    _state = in_locals;
    Step_Locals();
  }
  else _state = done;
}
