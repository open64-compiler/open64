/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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

//  GRA -> LRA grant implementation
/////////////////////////////////////
//  
//  Description:
//
//      Not much more than a map from BBs to vectors of REGISTER_SETs.
//
/////////////////////////////////////


//  $Revision: 1.7 $
//  $Date: 05/12/05 08:59:10-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_grant.cxx $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_grant.cxx $ $Revision: 1.7 $";
#endif

#include "tracing.h"
#include "defs.h"
#include "mempool.h"
#include "bb.h"
#include "bb_map.h"
#include "gra_grant.h"
#include "register.h"
#include "gra_bb.h"
#include "gra_trace.h"
#ifdef TARG_SL2 //minor_reg_alloc
#include "gra_para_region.h"
#endif 

MEM_POOL grant_pool;    // Just for grants
BB_MAP grant_map;

#ifndef Is_True_On
#define GRANT_INLINE inline
#else
#define GRANT_INLINE static
#endif

/////////////////////////////////////
typedef REGISTER_SET GRANT;
/////////////////////////////////////
//
//  Not a very impressive type really.
//
/////////////////////////////////////

/////////////////////////////////////
GRANT_INLINE void
GRANT_REGISTER_SET_Set( GRANT* grant, ISA_REGISTER_CLASS rc,  REGISTER_SET  set )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  grant[rc-ISA_REGISTER_CLASS_MIN] = set;
}

/////////////////////////////////////
GRANT_INLINE REGISTER_SET
GRANT_REGISTER_SET_Get( GRANT* grant, ISA_REGISTER_CLASS rc )
/////////////////////////////////////
//
//  Get the REGISTER_SET for <rc> from <grant>.
//
/////////////////////////////////////
{
  return grant[rc-ISA_REGISTER_CLASS_MIN];
}

/////////////////////////////////////
GRANT_INLINE void
GRANT_Union1D( GRANT* grant, ISA_REGISTER_CLASS rc, REGISTER reg )
/////////////////////////////////////
//
//  Add a register to <grant>...
//
/////////////////////////////////////
{
  GRANT_REGISTER_SET_Set(
    grant,
    rc,
    REGISTER_SET_Union1(GRANT_REGISTER_SET_Get(grant,rc),reg)
  );
}

/////////////////////////////////////
GRANT_INLINE void
GRANT_UnionD( GRANT* grant, ISA_REGISTER_CLASS rc, REGISTER_SET register_set )
/////////////////////////////////////
//
//  Add the registers in <register_set> to <grant>...
//
/////////////////////////////////////
{
  GRANT_REGISTER_SET_Set(
    grant,
    rc,
    REGISTER_SET_Union(GRANT_REGISTER_SET_Get(grant,rc),register_set)
  );
}

/////////////////////////////////////
void
GRA_GRANT_Initialize(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  BB *bb;
  GRANT* grant_mem;
  static BOOL grant_pool_initialized = FALSE;

  if ( ! grant_pool_initialized ) {
    grant_pool_initialized = TRUE;
    MEM_POOL_Initialize(&grant_pool,"GRA register to LRA",FALSE);
    MEM_POOL_Push(&grant_pool);
  }

  grant_mem =
    TYPE_MEM_POOL_ALLOC_N(REGISTER_SET,&grant_pool,
                                       ISA_REGISTER_CLASS_COUNT*PU_BB_Count + 2);
  bzero(grant_mem,sizeof(GRANT)*ISA_REGISTER_CLASS_COUNT*PU_BB_Count + 2);
  grant_map = BB_MAP_Create();

  for ( bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ) {
    BB_MAP_Set(grant_map,bb,(void*) grant_mem);
    grant_mem += ISA_REGISTER_CLASS_COUNT;
  }
}

/////////////////////////////////////
void
GRA_GRANT_Transfer( BB* from_bb, BB* to_bb )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_REGISTER_CLASS rc;
  GRANT* from_grant = (GRANT*)BB_MAP_Get(grant_map,from_bb);
  GRANT* to_grant   = (GRANT*)BB_MAP_Get(grant_map,to_bb);

  FOR_ALL_ISA_REGISTER_CLASS( rc )
    GRANT_UnionD(to_grant,rc,GRANT_REGISTER_SET_Get(from_grant,rc));
}

/////////////////////////////////////
void
GRA_GRANT_Local_Register( GRA_BB* gbb, ISA_REGISTER_CLASS rc, REGISTER reg )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  GRANT* gr = (GRANT*)BB_MAP_Get(grant_map,gbb->Bb());

  /*
     Reconsider the decision to let LRA use a particular register.

     Some registers may be needed to carry implicit values between blocks.
     We must make sure that LRA does not destroy registers that may
     contain valid information, and we will do this by keeping those
     registers out of the grant that is given to LRA.

     The only implicitly defined registers that we are concerned about
     are the ones that contain the results of a function call.  Since
     calls terminate a block, the result registers are always used in
     successor blocks.  Initially, this would always be the immediate
     successor, but hyperblock optimization may move code between blocks
     and cause the use of the implicitly defined result to be several
     blocks away from the call.

     Note: this may not be the cleanest way of preventing the problem.
     In the long run, it may not even be safe if changes are made to
     how Build_Dedicated_TN works. But it works for now!
  */
#if !defined (TARG_X8664)
  if (!REGISTER_SET_MemberP(REGISTER_CLASS_function_value(rc), reg) ||
      !BB_call(gbb->Bb()) &&
      !GTN_SET_MemberP(BB_live_in (gbb->Bb()),Build_Dedicated_TN(rc, reg, 8)) ||
      !GTN_SET_MemberP(BB_live_out(gbb->Bb()),Build_Dedicated_TN(rc, reg, 8)))
#else
#if defined (TARG_SL)
    if ((REGISTER_SET_MemberP(REGISTER_CLASS_function_value(rc), reg))) {
      if (BB_call(gbb->Bb()))
        if (GTN_SET_MemberP(BB_live_out(gbb->Bb()),Build_Dedicated_TN(rc, reg, 8)))
          return;
        else if (GTN_SET_MemberP(BB_live_in (gbb->Bb()),Build_Dedicated_TN(rc, reg, 8)))
          return;
    }
    else
#endif
#endif
    {
	 GRA_Trace_Grant(gbb,rc,reg);
         GRANT_Union1D(gr,rc,reg);
    }
}


#ifdef TARG_SL2 //minor_reg_alloc
/////////////////////////////////////
void
GRA_GRANT_REGISTER_SET_Set_For_BB( BB* bb, ISA_REGISTER_CLASS rc, REGISTER_SET reg_set )
{
  GRANT* gr = (GRANT*)BB_MAP_Get(grant_map, bb);
  GRANT_REGISTER_SET_Set(gr,  rc,  reg_set);
}
#endif 

/////////////////////////////////////
REGISTER_SET
GRA_GRANT_Get_Local_Registers( BB* bb, ISA_REGISTER_CLASS rc )
/////////////////////////////////////
//  See interface description.
//  Description
/////////////////////////////////////
{
  return GRANT_REGISTER_SET_Get((GRANT*) BB_MAP_Get(grant_map,bb),rc);
}

/////////////////////////////////////
void
GRA_GRANT_Finalize(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  MEM_POOL_Push(&grant_pool);
  BB_MAP_Delete(grant_map);
}

/////////////////////////////////////
void
GRA_GRANT_Unused_Caller_Saved(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_REGISTER_CLASS rc;

  GRA_Trace_Grant_Unused_Caller_Saved();
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ) {
    GRA_BB* gbb = gbb_mgr.Get(bb);
    FOR_ALL_ISA_REGISTER_CLASS( rc ) {
      REGISTER_SET free_regs;
      REGISTER reg;

      free_regs = REGISTER_SET_Difference(REGISTER_CLASS_caller_saves(rc),
					  gbb->Registers_Used(rc));
      FOR_ALL_REGISTER_SET_members(free_regs, reg) {
#ifdef TARG_SL2 //minor_reg_alloc
        if(BB_rid(gbb->Bb()) && RID_TYPE_minor(BB_rid(gbb->Bb()))) 
        {
          GRA_PARA_REGION* region  = gra_para_region_mgr.Get(BB_rid(gbb->Bb()));
          if( !REGISTER_SET_Intersection1(region->Registers_Exclude(rc), reg)) 
            GRA_GRANT_Local_Register(gbb, rc, reg);
        }
        else 
#endif 
          GRA_GRANT_Local_Register(gbb, rc, reg);
      }
    }
  }
}




