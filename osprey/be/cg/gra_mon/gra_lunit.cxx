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

// LUNIT implemetation

//  $Revision: 1.6 $
//  $Date: 05/12/05 08:59:10-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lunit.cxx $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lunit.cxx $ $Revision: 1.6 $";
#endif

#include "defs.h"
#include "errors.h"
#include "mempool.h"
#include "register.h"
#include "bb.h"
#include "gra_bb.h"
#include "gra_lunit.h"
#include "gra_lrange.h"

#ifdef TARG_X8664
#include "targ_sim.h"   // For RAX, RCX and RDX
#endif


/////////////////////////////////////
// Create and return a new LUNIT associated with <lrange> and
// <gbb>.  The new LUNIT is also added to the collections of
// LUNITs associated with <lrange> and <gbb>.  See gra_bb.h for
// the definition of GRA_BB_RC_LUNIT_ITER and gra_lrange.h for
// the definition of LRANGE_LUNIT_ITER.
extern LUNIT*
LUNIT_Create( LRANGE* lrange, GRA_BB* gbb )
{
  LUNIT* result = TYPE_MEM_POOL_ALLOC(LUNIT,GRA_pool);

  result->lrange = lrange;
  result->gbb = gbb;
  result->pref_priority = 0.0;
  result->allowed_preferences = REGISTER_SET_EMPTY_SET;
  result->flags = 0;
  result->def_count = 0;
  result->last_def = -1;
  result->global_pref = NULL;
#ifdef TARG_IA64
  result->has_use = FALSE;
#endif
  gbb->Add_LUNIT(result);
  lrange->Add_LUNIT(result);
  return result;
}

/////////////////////////////////////
// Call once for each preferencing copy seen that copies into or
// out of <lunit>'s LRANGE in its BB.  <lrange> is the other side
// of the copy -- where we are copying lunit to or from.
void 
LUNIT::Preference_Copy(LRANGE *lr) 
{
  pref_priority += gbb->Freq();
  if (lr->Type() == LRANGE_TYPE_LOCAL && lr->Has_Wired_Register()) {
#ifdef TARG_X8664
    /* Relax me!!!
       The following condition is necessary when the curent lrange does not
       across an operation which uses RAX, RCX or RDX implicitly.
    */
    if( lr->Reg() == RAX ||
	lr->Reg() == RCX ||
	lr->Reg() == RDX )
      return;
#endif
    allowed_preferences = REGISTER_SET_Union1(allowed_preferences,lr->Reg());
  }
}

/////////////////////////////////////
// Is <lunit>'s LRANGE live-out of its BB?
BOOL 
LUNIT::Live_Out(void) 
{ 
  return GTN_SET_MemberP(BB_live_out(gbb->Bb()), lrange->Tn()); 
}

/////////////////////////////////////
// Is <lunit>'s LRANGE live-in to its BB?
BOOL 
LUNIT::Live_In(void) 
{ 
  return GTN_SET_MemberP(BB_live_in(gbb->Bb()), lrange->Tn()); 
}

/////////////////////////////////////
// Return the priority of the given <lunit>.
float 
LUNIT::Priority(void) {
  float result;
  result = Live_In() ? gbb->Freq() : 0.0;
  if (Live_Out() && Has_Def())
    result += gbb->Freq();
  return result;
}
