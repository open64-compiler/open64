//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_region_emit.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_region_emit.h,v $
//
// Revision history:
//  27-DEC-94 shin - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
//    The supporting class for Emitters.
//
// ====================================================================
// ====================================================================


#ifndef opt_region_emit_INCLUDED
#define opt_region_emit_INCLUDED	"opt_region_emit.h"
#ifdef _KEEP_RCS_ID
static char *opt_region_emitrcs_id = 	opt_region_emit_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

class ALIAS_MANAGER;
class BB_NODE;
class CHI_LIST;
class MU_LIST;
class OPT_STAB;
class CFG;

typedef struct region_id RID;

#ifndef defs_INCLUDED
#include "defs.h"
#endif

#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif

#ifndef wn_INCLUDED
#include "wn.h"
#endif

// class used only by emitter class
//
class E_REGION {
  friend class EMITTER;

  BB_NODE *_region_start;	// the start of the region
  BB_NODE *_region_end;		// last block in the region
  WN      *_prev_wn;		// previous WN that we must follow

  // private constructor so it cannot be used
  E_REGION(void);
  E_REGION(const E_REGION&);
  E_REGION& operator = (const E_REGION&);

public:
  E_REGION( BB_NODE *region_start, BB_NODE *region_end, WN *prev_wn=NULL ) :
    _region_start(region_start),
    _region_end(region_end),
    _prev_wn(prev_wn)  { }

  BB_NODE *Region_start(void) const { return _region_start; }
  BB_NODE *Region_end(void) const   { return _region_end; }
  WN      *Prev_wn(void) const      { return _prev_wn; }
  void     Set_prev_wn(WN *wn)      { _prev_wn = wn; }
}; // end E_REGION class

//============================================================================
// Prune the region's boundary sets (called from the main emitter)
// Look through all loads and stores of PREGs in the region and
// update the pregs in and out sets accordingly.
class PRUNE_BOUND {
private:
  CFG *_cfg;
  OPT_STAB *_opt_stab;
  RID *_rid;
  BS *_modset;
  BS *_useset;
  BOOL _trace;
  MEM_POOL _lpool;
  
  PRUNE_BOUND& operator = (const PRUNE_BOUND&);

  BOOL Trace(void)	{ return _trace; }
  void Prune_boundary_sets(void); // main entry point, called by constructor

  // traverse entire region and set bit in mod/use sets
  void Collect_mod_use_sets(BB_NODE *bb);
  void Useset_stmt(STMTREP *stmt);	// STMTREP level
  void Useset_expr(CODEREP *cr);	// CODEREP level

  // remove an aux_id from the appropriate boundary set
  void REGION_remove_from_bound(AUX_ID aux_id, BOOL outset);

public:
  PRUNE_BOUND(CFG *cfg, OPT_STAB *opt_stab) : _cfg(cfg), _opt_stab(opt_stab)
    {	_trace = Get_Trace(TP_REGION, TT_REGION_WOPT_DEBUG) ||
	  	 Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG) ||
	         Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG);
	_rid = _cfg->Rid();

	OPT_POOL_Initialize(&_lpool, "REGION prune boundary set", FALSE, -1);
	OPT_POOL_Push(&_lpool, -1);
	INT32 size = opt_stab->Lastidx() + 1;

	_modset = BS_Create_Empty(size, &_lpool);
	_useset = BS_Create_Empty(size, &_lpool);

	_modset = BS_ClearD(_modset);
	_useset = BS_ClearD(_useset);

	Prune_boundary_sets(); // actually do the work
    }

  ~PRUNE_BOUND(void)
    {
      OPT_POOL_Pop(&_lpool, -1);
      OPT_POOL_Delete(&_lpool, -1);
    }
};

//============================================================================

extern void REGION_live_in_from_chi(RID *rid,
                                    CHI_LIST *clist,
                                    OPT_STAB *opt_stab,
                                    ALIAS_MANAGER *alias_mgr);

extern void REGION_live_out_from_mu(RID *rid, 
                                    MU_LIST *mlist,
                                    OPT_STAB *opt_stab,
                                    ALIAS_MANAGER *alias_mgr);

extern void Push_region(STACK<E_REGION *> *stk,
                        BB_NODE *start_region,
                        MEM_POOL *pool);

extern WN  *Pop_region(STACK<E_REGION *> *stk, WN *first_wn, WN *last_wn,
		       REGION_LEVEL rgn_level, OPT_STAB *opt_stab);


#endif  // opt_region_emit_INCLUDED
