//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_emit.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_emit.h,v $
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
//    The emitter class for the Global Optimizer.  The algorithm for
//    emitter is described in doc/Mongoose/Preopt-emitter.doc.
//
//   Exported type:
//
//    EMITTER: the class that carries the global resource we need
//             when we emit the WHIRL tree from the CODEREP.
//
// ====================================================================
// ====================================================================


#ifndef opt_emit_INCLUDED
#define opt_emit_INCLUDED	"opt_emit.h"
#ifdef _KEEP_RCS_ID
static char *opt_emitrcs_id = 	opt_emit_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

#ifndef cxx_base_INCLUDED
#include "cxx_base.h"
#endif
#ifndef opt_htable_INCLUDED
#include "opt_htable.h"
#endif
#ifndef optimizer_INCLUDED
#include "optimizer.h"
#endif
#include "region_util.h"	// RID structure

class ALIAS_MANAGER;
class ALIAS_RULE;
class CFG;
class CODEREP;
class DU_MANAGER;
class E_REGION;
class OPT_STAB;
class RVI;
class VER_LIST;
class WHIRL_SSA_EMITTER;

class EMITTER {
private:
  CFG        *_cfg;
  CODEMAP    *_htable;
  OPT_STAB   *_opt_stab;
  ALIAS_MANAGER *_alias_mgr;
  DU_MANAGER    *_du_mgr;
  WHIRL_SSA_EMITTER *_wssa_emitter;  // WSSA emitter
  WN         *_opt_func;
  MEM_POOL   *_loc_pool;
  MEM_POOL   *_mem_pool;
  OPT_PHASE   _opt_phase;
  BOOL	      _has_do_loop;
  BOOL        _trace;

  STMTREP     *_rgn_entry_stmt; // first statement in region (emit time)
  
  CODEREP_LIST_CONTAINER _copy_list;

  WN_MAP       _wn_to_cr_map;	// map from wn -> cr/sr for def-use

  STACK<E_REGION *> _region_stack; // for MP regions

  ID_MAP<IDTYPE, INT32> _preg_renumbering_map;

  EMITTER(const EMITTER&);
  EMITTER& operator = (const EMITTER&);


  void Generate_entry_copy(BB_NODE *);
  void Raise_func_entry(BB_NODE*, BB_NODE*);
  BOOL Raise_altentry(BB_NODE*);
  BOOL Can_raise_to_scf(BB_NODE*);

  // du_manager related routines
  //
  // Add def(s) to use(s)
  void Add_defs_use( DU_MANAGER *, STMTREP *, WN * );

  // various traversal routines to compute use-def
  void Compute_use_def_var( DU_MANAGER *, CODEREP *, WN *, BB_NODE * );
  void Compute_use_def_expr( DU_MANAGER *, WN *, BB_NODE * );
  void Compute_use_def_stmt( DU_MANAGER *, WN *, BB_NODE * );
  void Compute_use_def_zero_ver( DU_MANAGER * );
  void Compute_use_def_zero_version_var( DU_MANAGER *, CODEREP *, WN *, BB_NODE *, BB_NODE * );
  // traversal to set incomplete lists flag
  void Compute_incomplete_defs( DU_MANAGER *du_mgr, CODEREP *cr );
  // after emitting, compute the def-use information
  void Compute_use_def(DU_MANAGER *);
  void Collect_IPA_summary(DU_MANAGER *, WN *);


public:
  EMITTER(MEM_POOL *lpool,
	  MEM_POOL *gpool,
          OPT_PHASE opt_phase);
  ~EMITTER(void);

  OPT_STAB *Opt_stab(void) const            { return _opt_stab; }
  MEM_POOL *Loc_pool(void) const            { return _loc_pool; }
  MEM_POOL *Mem_pool(void) const            { return _mem_pool; }
  CFG	   *Cfg(void) const		    { return _cfg; }
  CODEMAP  *Htable(void) const              { return _htable; }
  ALIAS_MANAGER *Alias_Mgr(void) const      { return _alias_mgr; }
  DU_MANAGER    *Du_Mgr(void) const         { return _du_mgr; }
  WHIRL_SSA_EMITTER *WSSA_Emitter(void) const { return _wssa_emitter;}
  WN_MAP   *Wn_to_cr_map(void)              { return &_wn_to_cr_map; }
  void      Set_has_do_loop(void)	    { _has_do_loop = TRUE; }
  BOOL      Has_do_loop(void) const	    { return _has_do_loop; }
  BOOL      Trace(void) const               { return _trace; }
  BOOL      For_preopt(void) const          { return _opt_phase !=
                                                MAINOPT_PHASE; }
  BOOL      For_mainopt(void) const         { return _opt_phase ==
                                                MAINOPT_PHASE; }
  STACK<E_REGION *> *Region_stack(void)     { return &_region_stack; }

  ID_MAP<IDTYPE, INT32> &Preg_renumbering_map(void)
    { return _preg_renumbering_map; }

  void      Gen_wn(BB_NODE *f, BB_NODE *l);
  BOOL      Verify(WN *wn);
  WN       *Emit(COMP_UNIT *cu,
		 DU_MANAGER *du_mgr,
		 ALIAS_MANAGER *alias_mgr);

  // provide connections between opt's structures and WNs (during Emit)
  void Connect_sr_wn( STMTREP *stmtrep, WN *wn );
  void Connect_cr_wn( CODEREP *coderep, WN *wn );
  void Duplicate_sr_cr_connections( WN *old_wn, WN *new_wn );

  // a few routines used by the template in opt_emit_template.h
  BOOL      Gen_lno_info(void)         { return FALSE; }
  BOOL      Do_rvi(void)               { return FALSE; }
  RVI      *Rvi(void) const            { return NULL; };
  WN       *Build_loop_info(BB_NODE *) { return NULL; }

  // save first stmtrep in region until region wn is created to connect_sr_wn
  void	       Set_region_entry_stmt(STMTREP *s)
    { Is_True(s != NULL,("EMITTER::Set_region_entry_stmt, NULL"));
     _rgn_entry_stmt = s; }
  STMTREP     *Region_entry_stmt(void)
    { Is_True(_rgn_entry_stmt != NULL,("EMITTER::Region_entry_stmt, NULL"));
      return _rgn_entry_stmt; }

};

void Detect_invalid_doloops(COMP_UNIT *);

// WOPT Pragmas
enum {
  WOPT_GOTO_CONVERSION_FINISHED = 0x2,
  WOPT_TAIL_RECUR_FINISHED = 0x4,
};

#endif  // opt_emit_INCLUDED
