/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005-2007 NVIDIA Corporation.  All rights reserved.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_du.cxx
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:53-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/Patch0002-taketwo/kpro64-pending/be/opt/SCCS/s.opt_du.cxx $
//
// Revision history:
//  21-DEC-94 shin - Original Version
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
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_du_CXX	"opt_du.cxx"
static char *rcs_id = 	opt_du_CXX"$Revision$";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "tracing.h"
#include "errors.h"
#include "erglob.h"
#include "wn_util.h"
#include "ir_reader.h"  // For IR_dump_map_info

#include "opt_base.h"
#include "opt_config.h"
#include "cxx_memory.h"
#include "opt_du.h"
#include "opt_emit.h"
#include "opt_htable.h"
#include "opt_ssa.h"
#include "opt_mu_chi.h"
#include "opt_util.h"
#include "opt_cfg.h"
#include "bb_node_set.h"

#ifdef Is_True_On
// for cross-checking with alias analysis
#include "optimizer.h"
#endif

// Egads, a global variable.  Used to keep us from visiting a
// phi-node twice during a use-def traversal.
static mINT16 DU_Phi_Ctr = 0;

// ====================================================================
// Various printing functions
// ====================================================================

void
DU_NODE::Print(FILE *fp) const
{
  // print the Def node mapcat and map_id
  fprintf(fp, "<%d:%d> ", OPCODE_mapcat(WN_opcode(_wn)), WN_map_id(_wn));
}

void
DEF_LIST::Print(FILE *fp)
{
  if ( Incomplete() ) 
    fprintf( fp, "(Incomplete) " );

  // print the Def list
  DEF_LIST_ITER   def_lst_iter;
  const DU_NODE *tmp;
  FOR_ALL_NODE(tmp, def_lst_iter, Init(this))
    tmp->Print(fp);
}

void
USE_LIST::Print(FILE *fp)
{
  if ( Incomplete() ) 
    fprintf( fp, "(Incomplete) " );

  // print the Use list
  USE_LIST_ITER   use_lst_iter;
  const DU_NODE *tmp;
  FOR_ALL_NODE(tmp, use_lst_iter, Init(this))
    tmp->Print(fp);
}

void
DU_MANAGER::Print_Du(WN *def, FILE *fp)
{
  fprintf(fp, "D-U <%d:%d> -> ",
	  OPCODE_mapcat(WN_opcode(def)), WN_map_id(def));
  USE_LIST *use_list = Du_Get_Use(def);
  if ( use_list != NULL ) {
    use_list->Print(fp);
  }
  fprintf( fp, "def:" );
  fdump_wn(fp, def);
}

void
DU_MANAGER::Print_Ud(WN *use, FILE *fp)
{
  fprintf(fp, "U-D <%d:%d> -> ",
	  OPCODE_mapcat(WN_opcode(use)), WN_map_id(use));
  DEF_LIST *def_list = Ud_Get_Def(use);
  if ( def_list != NULL )
    def_list->Print(fp);
  if (! OPERATOR_is_scalar_iload (WN_operator(use))) {
    if ( def_list != NULL && def_list->Loop_stmt() ) {
      fprintf(fp," Loop_stmt:");
      //fdump_wn(fp, def_list->Loop_stmt());
    }
  }
  fprintf(fp, "\n");
}

#ifdef Is_True_On
void
DU_MANAGER::Print_Du_Info(FILE *fp)
{
  BOOL save_IR_dmi = IR_dump_map_info;
  IR_dump_map_info = TRUE;

  // For each node in the WHIRL tree,
  // if this node has a use list, print info for the list and the
  // node.

  for (WN_ITER* wni = WN_WALK_TreeIter(Entry_Wn());
       wni != NULL;
       wni = WN_WALK_TreeNext(wni)) {
    USE_LIST *use_list = Du_Get_Use(WN_ITER_wn(wni));

    if (use_list != NULL) {
      use_list->Print(fp);
      fflush(fp);
      fprintf(fp, "def: ");
      fdump_wn(fp, WN_ITER_wn(wni));
      fflush(fp);
    }
  }

  IR_dump_map_info = save_IR_dmi;
}
#endif

// ====================================================================
// Various Contains functions to see if a list contains a node
// ====================================================================

BOOL
USE_LIST::Contains( const WN *wn )
{
  USE_LIST_ITER use_lst_iter;
  const DU_NODE *tmp;
  FOR_ALL_NODE( tmp, use_lst_iter, Init(this) ) {
    if ( tmp->Wn() == wn ) 
      return TRUE;
  }

  return FALSE;
}

BOOL
DEF_LIST::Contains( const WN *wn )
{
  DEF_LIST_ITER def_lst_iter;
  const DU_NODE *tmp;
  FOR_ALL_NODE( tmp, def_lst_iter, Init(this) ) {
    if ( tmp->Wn() == wn ) 
      return TRUE;
  }

  return FALSE;
}

// ====================================================================
// Provide connections between optimizer's data structures and WNs
// ====================================================================

void
EMITTER::Connect_cr_wn( CODEREP *cr, WN *wn )
{
  // set the WN -> cr relationship (1 to 1 relationship)
  WN_MAP_Set( _wn_to_cr_map, wn, cr );
}

void
EMITTER::Connect_sr_wn( STMTREP *sr, WN *wn )
{
  // set the WN -> sr relationship (1 to 1 relationship)
  WN_MAP_Set( _wn_to_cr_map, wn, sr );

  // set the sr -> WN relationship (1 to possibly many)
  if ( sr->Wn() == NULL ) {
    Is_True ( ! sr->Is_use_list(),
      ("EMITTER::Connect_sr_wn: null Wn() but Is_use_list") );

    sr->Set_wn( wn );
  }
  else {
    // must be 1 to many relationship
    // do we already have a list?
    if ( ! sr->Is_use_list() ) {
      // first save current Wn() value before overwriting with list
      DU_NODE *old_use = CXX_NEW( DU_NODE(sr->Wn()), Mem_pool() );

      // need to create list and then add current value to list
      USE_LIST *use_list = CXX_NEW( USE_LIST(old_use,0), Mem_pool() );
      sr->Set_use_list(use_list);
    }

    // now add the new wn
    DU_NODE *new_use = CXX_NEW( DU_NODE(wn), Mem_pool() );
    sr->Use_list()->Prepend(new_use);
  }
}

void
EMITTER::Duplicate_sr_cr_connections( WN *old_wn, WN *new_wn )
{
  const OPCODE opc = WN_opcode(old_wn);

  Is_True( opc == WN_opcode(new_wn),
    ("EMITTER::Duplicate_sr_cr_connections: trees don't match") );

  void *vp = WN_MAP_Get( _wn_to_cr_map, old_wn );
  if ( vp != NULL ) {
    if ( OPCODE_is_stmt(opc) || OPCODE_is_scf(opc) ) {
      // must be a stmtrep
      Connect_sr_wn( (STMTREP*)vp, new_wn );
    }
    else {
      // must be an expression with a coderep
      Connect_cr_wn( (CODEREP*)vp, new_wn );
    }
  }

  if ( opc == OPC_BLOCK ) {
    WN *old_bwn, *new_bwn;
    for ( old_bwn = WN_first(old_wn), new_bwn = WN_first(new_wn); 
	  old_bwn != NULL; 
	  old_bwn = WN_next(old_bwn), new_bwn = WN_next(new_bwn) )
    {
      Duplicate_sr_cr_connections( old_bwn, new_bwn );
    }
  }
  else {
    for ( INT ikid = 0; ikid < WN_kid_count(old_wn); ikid++ ) {
      Duplicate_sr_cr_connections( WN_kid(old_wn,ikid),
				   WN_kid(new_wn,ikid) );
    }
  }
}

// Mark the assignment to be value_restored.
//
void DU_MANAGER::Set_value_restored(WN *def)
{
  if (_val_restored_map == WN_MAP_UNDEFINED)
    _val_restored_map = WN_MAP32_Create(&_mem_pool);
  Is_True(OPERATOR_is_scalar_store (WN_operator(def)),
	  ("DU_MANAGER::Set_value_restored: not an assignment."));
  WN_MAP32_Set(_val_restored_map, def, TRUE); 
}



// Setup the _alias_mgr for debugging.
//
void
DU_MANAGER::Set_alias_mgr(ALIAS_MANAGER *am)
{ 
  _alias_mgr = am; 
}


void
DU_MANAGER::Ud_Add_Def(WN *use, WN *def)
{
  // Add a def to the U-D chain
  DEF_LIST *deflst = Ud_Get_Def(use);

  // check for uniqueness
  if ( deflst != NULL && deflst->Contains(def) )
    return;

  DU_NODE *defnod = CXX_NEW(DU_NODE(def), &_mem_pool);
  if (deflst == NULL) {
    deflst = CXX_NEW( DEF_LIST(defnod,0), &_mem_pool);
    deflst->Set_loop_stmt(NULL);
    Ud_Put_Def(use, deflst);	// map the deflst to the use
  }
  else {
    deflst->Append(defnod);
  }

  if ( Tracing() )
    Print_Ud(use, TFile);

#ifdef Is_True_On
  // cross-check with alias analysis
  // alias_mgr is global
  const OPCODE def_opc = WN_opcode(def);
  const OPCODE use_opc = WN_opcode(use);
  if ( (OPCODE_is_load(use_opc) || OPCODE_is_store(use_opc)) &&
       (OPCODE_is_load(def_opc) || OPCODE_is_store(def_opc)) )
  {
    //
    // the accuracy of alias analysis is reduced by the use of virtual variable.
    //
    if (! OPERATOR_is_scalar_iload (WN_operator(use)) &&
	! OPERATOR_is_scalar_istore (WN_operator(def)) &&
	WN_operator(use) != OPR_MLOAD && WN_operator(use) != OPR_MSTORE &&
	!OPCODE_is_stmt(WN_opcode(use)))
    {
      if (!Aliased(_alias_mgr, use, def)) {
	DevWarn("DU_MANAGER::Ud_Add_Def: Use %d [%p] and Def %d [%p] are not aliased",
		WN_map_id(use), use, WN_map_id(def), def);
      }
    }
  }
#endif // Is_True_On

}

void
DU_MANAGER::Du_Add_Use(WN *def, WN *use)
{
  // Add an use to the D-U chain
  USE_LIST *uselst = Du_Get_Use(def);

  // check for uniqueness
  if ( uselst != NULL && uselst->Contains(use) )
    return;

  DU_NODE *usenod = CXX_NEW(DU_NODE(use), &_mem_pool);
  if (uselst == NULL) {
    uselst = CXX_NEW(USE_LIST(usenod,0), &_mem_pool);
    Du_Put_Use(def, uselst);	// map the uselst to the def
  }
  else {
    uselst->Append(usenod);
  }

  if ( Tracing() )
    Print_Du(def, TFile);
}

void
DU_MANAGER::Add_Def_Use( WN *def, WN *use )
{
  if ((_opt_phase == PREOPT_PHASE || _opt_phase == PREOPT_LNO_PHASE ||
       _opt_phase == PREOPT_LNO1_PHASE ||
       _opt_phase == PREOPT_DUONLY_PHASE) &&
      OPERATOR_is_scalar_iload (WN_operator(use)) &&
      !OPERATOR_is_scalar_store (WN_operator(def)))
    return;

  // def-use and use-def lists should be symmetric
  Du_Add_Use( def, use );
  Ud_Add_Def( use, def );
}

void
DU_MANAGER::Ud_Delete_Def(WN *use, WN *def)
{
  // Delete a def from the U-D chain
  DEF_LIST *deflist = Ud_Get_Def(use);
  DEF_LIST_ITER d_iter(deflist);
  DU_NODE *du_ptr=(DU_NODE *)d_iter.First();
  DU_NODE *du_ptr1=du_ptr;
  for (; !d_iter.Is_Empty(); du_ptr1=du_ptr,du_ptr=(DU_NODE *)d_iter.Next()) {
    if (du_ptr->Wn()==def)
      break;
  }
  if (d_iter.Is_Empty())
    return;
  else if (deflist->Head()->Wn()==def)
    deflist->Remove_Headnode();
  else
    deflist->Remove(du_ptr1,du_ptr);
  if ( Tracing() )
    Print_Ud(use, TFile);
}

void
DU_MANAGER::Du_Delete_Use(WN *def, WN *use)
{
  // Delete a use from the D-U chain
  USE_LIST *uselist = Du_Get_Use(def);
  USE_LIST_ITER u_iter(uselist);
  DU_NODE *du_ptr=(DU_NODE *)u_iter.First();
  DU_NODE *du_ptr1=du_ptr;
  for (; !u_iter.Is_Empty(); du_ptr1=du_ptr,du_ptr=(DU_NODE *)u_iter.Next()) {
    if (du_ptr->Wn()==use)
      break;
  }
  if (u_iter.Is_Empty())
    return;
  else if (uselist->Head()->Wn()==use)
    uselist->Remove_Headnode();
  else
    uselist->Remove(du_ptr1,du_ptr);
  if ( Tracing() )
    Print_Du(def, TFile);
}

void
DU_MANAGER::Delete_Def_Use( WN *def, WN *use )
{
  // def-use and use-def lists should be symmetric
  Du_Delete_Use( def, use );
  Ud_Delete_Def( use, def );
}
    

// Get rid of all the edges involving use
void DU_MANAGER::Remove_Use_From_System(WN *use)
{
  DEF_LIST *defs = Ud_Get_Def(use);
  if (defs) {
    while (!defs->Is_Empty()) {
      DU_NODE *def_node = defs->Remove_Headnode();
      WN *def = def_node->Wn();

      // look for backedge to remove
      USE_LIST *uses = Du_Get_Use(def);
      BOOL found = FALSE;
      if (uses) {
        USE_LIST_ITER uli(uses);
	DU_NODE *prev = NULL;
        for ( DU_NODE *use_node=uli.First(); !uli.Is_Empty(); 
	      use_node = uli.Next()) 
	{
          if (use_node->Wn() == use) {
	    CXX_DELETE(uses->Remove(prev,use_node),&_mem_pool);
	    found = TRUE;
	    break;
	  }
	  prev = use_node;
        }
      }
      Is_True(found,("Must be a backedge in Remove_Use_From_System\n"));
    }
    CXX_DELETE(defs,&_mem_pool);
  }
  WN_MAP_Set(_ud_map, use, NULL);
}


void DU_MANAGER::Remove_Def_From_System(WN *def)
{
  USE_LIST *uses = Du_Get_Use(def);
  if (uses) {
    while (!uses->Is_Empty()) {
      DU_NODE *use_node = uses->Remove_Headnode();
      WN *use = use_node->Wn();

      // look for backedge to remove
      BOOL found = FALSE;
      DEF_LIST *defs = Ud_Get_Def(use);
      if (defs) {
        DEF_LIST_ITER dli(defs);
	DU_NODE *prev = NULL;
        for (DU_NODE *def_node=dli.First(); !dli.Is_Empty(); 
					def_node = dli.Next()) {
          if (def_node->Wn() == def) {
	    found = TRUE;
	    CXX_DELETE(defs->Remove(prev,def_node),&_mem_pool);
	    break;
	  }
	  prev = def_node;
        }
      }
      Is_True(found,("Must be a backedge in Remove_Def_From_System\n"));
    }
    CXX_DELETE(uses,&_mem_pool);
  }
  WN_MAP_Set(_du_map, def, NULL);
}

// ====================================================================
// Create empty DEF_LIST or USE_LIST if they have no list
// ====================================================================

void 
DU_MANAGER::Create_Def_List(WN *use)
{
  if ( Ud_Get_Def(use) == NULL ) {
    DEF_LIST *def_list = CXX_NEW( DEF_LIST(NULL,0), &_mem_pool);
    Ud_Put_Def(use, def_list);	// map the def_list to the use
  }
}

void 
DU_MANAGER::Create_Use_List(WN *def)
{
  if ( Du_Get_Use(def) == NULL ) {
    USE_LIST *use_list = CXX_NEW( USE_LIST(NULL,0), &_mem_pool);
    Du_Put_Use(def, use_list);	// map the use_list to the def
  }
}

// ====================================================================
// Add use to all of the defs
// ====================================================================

void
EMITTER::Add_defs_use( DU_MANAGER *du_mgr, STMTREP *defstmt, WN *use )
{
  FmtAssert( defstmt->Wn() != NULL,
    ("EMITTER::Du_Add_Uses: no Wn for stmtrep") );

  // do we have just a single defining wn?
  if ( ! defstmt->Is_use_list() ) {
    du_mgr->Add_Def_Use( defstmt->Wn(), use );
  }
  else {
    // have multiple defs for this stmtrep
    USE_LIST_ITER def_lst_iter;
    DU_NODE *du;
    FOR_ALL_NODE(du, def_lst_iter, Init(defstmt->Use_list())) {
      du_mgr->Add_Def_Use( du->Wn(), use );
    }
  }
}

// ====================================================================
// This definition is "incomplete" meaning it does not have all uses
// in its uselist.  This generally occurs if the value defined is used
// in a chi-node, and we're not providing the full use-def information.
//

void
DU_MANAGER::Du_Set_Incomplete( WN *def )
{
  // Add an use to the D-U chain
  USE_LIST *uselst = Du_Get_Use(def);
  if (uselst == NULL) {
    // create an empty list if we didn't have a list already
    uselst = CXX_NEW(USE_LIST(), &_mem_pool);
    uselst->Init();
    Du_Put_Use(def, uselst);	// map the uselst to the def
  }
  uselst->Set_Incomplete();

  if ( Tracing() )
    Print_Du(def, TFile);
}

// ====================================================================
// Mark all of the defs' use-lists as incomplete
// (may have generated more than one def WN for a given stmtrep
// ====================================================================

static void
Set_Incomplete_Uses( DU_MANAGER *du_mgr, STMTREP *defstmt )
{
  FmtAssert( defstmt->Wn() != NULL,
    ("Set_Incomplete_Uses: no Wn for stmtrep") );

  // do we have just a single defining wn?
  if ( ! defstmt->Is_use_list() ) {
    du_mgr->Du_Set_Incomplete( defstmt->Wn() );
  }
  else {
    // have multiple defs for this stmtrep
    USE_LIST_ITER def_lst_iter;
    DU_NODE *du;
    FOR_ALL_NODE(du, def_lst_iter, Init(defstmt->Use_list())) {
      du_mgr->Du_Set_Incomplete( du->Wn() );
    }
  }
}

// ====================================================================
// Update the loop information
// ====================================================================

static
void Update_loop_info( DU_MANAGER *du_mgr, WN *wn, BB_NODE *wn_bb )
{
  // now update the loop information for this variable, if any
  // Note that we change it from a BB_LOOP structure to a WN *.
  DEF_LIST *deflist = du_mgr->Ud_Get_Def(wn);
  if (deflist && deflist->Loop_info() != NULL) {
    if (deflist->Loop_info()->Contains( wn_bb->Innermost())) {
      deflist->Set_loop_stmt(deflist->Loop_info()->Loopstmt());
      if ( du_mgr->Tracing() && deflist->Loop_stmt() != NULL ) {
	fprintf( TFile, "Use WN:<%d:%d> has Loop_stmt WN:<%d:%d> %s\n",
		OPCODE_mapcat(WN_opcode(wn)),
		WN_map_id(wn),
		OPCODE_mapcat(WN_opcode(deflist->Loop_stmt())),
		WN_map_id(deflist->Loop_stmt()),
		OPCODE_name(WN_opcode(deflist->Loop_stmt())) );
      }
    }
    else
      deflist->Set_loop_stmt(NULL);
  }
}

// ====================================================================
// The coderep is an operand of a chi, so see if any of the defs are
// direct defs that we want to say do not have complete use lists.
//

void
EMITTER::Compute_incomplete_defs( DU_MANAGER *du_mgr, CODEREP *cr )
{
  if (cr->Kind() != CK_VAR) 
    return;

  // we really should have handled virtual variables before getting
  // here.
  BOOL is_real_var = du_mgr->Opt_Stab()->Is_real_var(cr->Aux_id());
  FmtAssert( is_real_var,
    ("Compute_incomplete_defs: should not see virtual variable") );

  if (!cr->Is_var_nodef()) {
    if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
      // need to go through the phi operand list
      PHI_NODE *phi = cr->Defphi();

      // have we already visited this phi for this use?
      if ( phi->Live() && phi->Count() != DU_Phi_Ctr) {
	// note that we've visited this phi for this use
	phi->Set_count(DU_Phi_Ctr);

	PHI_OPND_ITER phi_opnd_iter(phi);
	CODEREP *opnd;
	BOOL ignore_pregs = Opt_stab()->Is_virtual(phi->RESULT()->Aux_id());
	FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
          Is_True(opnd != NULL, 
	    ("EMITTER::Compute_incomplete_defs: null phi-opnd"));

	  // recursively follow the use-def chain through the phi's
	  // to find real definitions
	  if ( ! opnd->Is_flag_set(CF_IS_ZERO_VERSION) ) {
	    Compute_incomplete_defs( du_mgr, opnd );
	  }
	}
      }
    } else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
      // don't really care about this case, do we? (where an operand
      // of a chi is also defined by a chi)
    } else {
      // regular definition
      STMTREP *defstmt = cr->Defstmt();
      if ( defstmt->Live_stmt() ) {
	if (OPERATOR_is_scalar_store (defstmt->Opr())) {
	  // direct definition, but at least one use is in a chi, so
	  // say the use list is incomplete
	  Set_Incomplete_Uses( du_mgr, defstmt );
	}
      } // end if live statement
    } // end if regular definition
  }
}

// ====================================================================
// Find the definition(s) for the given coderep, but without the use
// of defining-statements within the coderep.  i.e., follow the dom
// tree up until we find a definition
//
// NOTE: We start at the bottom of the given bb, and work our way up.
// ====================================================================

void
EMITTER::Compute_use_def_zero_version_var( DU_MANAGER *du_mgr, 
  CODEREP *cr, WN *wn, BB_NODE *bb, BB_NODE *wn_bb )
{
  for ( ; bb != NULL; bb = bb->Idom() ) {
    STMTREP *stmt;
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    FOR_ALL_NODE_REVERSE(stmt,stmt_iter,Init()) {
      const OPERATOR stmt_opr = stmt->Opr();

      if (OPERATOR_is_scalar_store (stmt_opr)) {
	if ( stmt->Lhs()->Aux_id() == cr->Aux_id() ) {
	  // found direct definition!
	  Add_defs_use( du_mgr, stmt, wn );
	  return;
	}
      }

      if ( stmt->Has_chi() ) {
	CHI_LIST_ITER chi_iter;
	CHI_NODE *chi;
	FOR_ALL_NODE( chi, chi_iter, Init(stmt->Chi_list()) ) {
	  if ( chi->Live() && chi->RESULT()->Aux_id() == cr->Aux_id() ) {
	    // found it!

	    Add_defs_use( du_mgr, stmt, wn );

	    // def-list is incomplete
	    DEF_LIST *deflist = du_mgr->Ud_Get_Def(wn);
	    deflist->Set_Incomplete();

	    // similarly, the use-list of the defining statement(s) is
	    // (are) incomplete
	    Set_Incomplete_Uses( du_mgr, stmt );

	    if ( du_mgr->Tracing() ) {
	      fprintf( TFile, "Compute_use_def_zero_version_var: "
			      "found defstmt in BB:%d\n", bb->Id() );
	    }

	    return;
	  }
	}
      }
    } // end of stmt loop


    PHI_LIST_ITER phi_iter;
    PHI_NODE     *phi;
    FOR_ALL_ELEM ( phi, phi_iter, Init(bb->Phi_list()) ) {
      if (!phi->Res_is_cr()) continue;
      if ( phi->Count() != DU_Phi_Ctr ) {

	// note that we've visited this phi for this use
	phi->Set_count(DU_Phi_Ctr);

	if ( phi->Live() && phi->RESULT()->Aux_id() == cr->Aux_id() ) {
	  // found a phi that defines this variable, so precede up
	  // the use-def chain

	  PHI_OPND_ITER phi_opnd_iter(phi);
	  CODEREP *opnd;
	  BOOL ignore_pregs = Opt_stab()->Is_virtual(phi->RESULT()->Aux_id());
	  FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
	    if ( ! opnd->Is_flag_set(CF_IS_ZERO_VERSION) ) {
	      // this is a real use, so use normal processing because
	      // it's faster to do so
	      Compute_use_def_var( du_mgr, opnd, wn, wn_bb );
	    }
	    else {
	      // handle zero-version operands
	      Compute_use_def_zero_version_var( du_mgr, opnd, wn,
						phi->Bb()->Nth_pred(phi_opnd_iter.Curidx()), wn_bb );
	    }
	  }

	  // there had better not be any more phi's that define this
	  // coderep, so we'll go ahead and return
	  return;
	}
      }
    } // end of phi loop
  }

  // if we make it here, we must not have found any definition.
  // so must be defined at entry
  if (OPERATOR_is_scalar_load (WN_operator(wn))) {
    du_mgr->Add_Def_Use( du_mgr->Entry_Wn(), wn );

    DEF_LIST *deflist = du_mgr->Ud_Get_Def(wn);
    deflist->Set_Incomplete();
  }
}

// ====================================================================
//  Identify the DEFs for one USE.
//    tracing through the PHI and CHI functions to locate
//    the defining WN statement(s).
// 
void
EMITTER::Compute_use_def_var( DU_MANAGER *du_mgr, CODEREP *cr, WN *wn,
                              BB_NODE *wn_bb)
{
  if (cr->Kind() != CK_VAR) 
    return;

  // We don't need special handling here for dedicated PREGs because
  // alias analysis maintains the invariant that they are in the Chi
  // list of every call that may affect them.

  BOOL is_real_var = du_mgr->Opt_Stab()->Is_real_var(cr->Aux_id());

  if ( !is_real_var && !Opt_stab()->Unique_vsym(cr->Aux_id())) {
    // virtual variables don't get processed, but we will say the 
    // def list of loads are incomplete.
    if (OPERATOR_is_scalar_load (WN_operator(wn))) {
      DEF_LIST *deflist = du_mgr->Ud_Get_Def(wn);
      // do we need to handle the case where there is no deflist
      // already?
      Is_True( deflist != NULL,
	("Compute_use_def_var: null deflist") );
      deflist->Set_Incomplete();
    }
  }
  else if ( cr->Is_flag_set(CF_IS_ZERO_VERSION) ) {
    // zero-version variables have no known definition, and we should
    // avoid seeing them here.
    FmtAssert( FALSE,
      ("EMITTER::Compute_use_def_var: cr is zero-version") );
  }
  else if (!cr->Is_var_nodef()) {
    if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
      // need to go through the phi operand list
      PHI_NODE *phi = cr->Defphi();

      if (! phi->Live())	// can happen due to transition between IRM-
	return;			// generated pregs and unique vsyms

      // have we already visited this phi for this use?
      if (phi->Count() != DU_Phi_Ctr) {

	// note that we've visited this phi for this use
	phi->Set_count(DU_Phi_Ctr);

	PHI_OPND_ITER phi_opnd_iter(phi);
	CODEREP *opnd;
	BOOL ignore_pregs = Opt_stab()->Is_virtual(phi->RESULT()->Aux_id());
	FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
	  Is_True(opnd != NULL, 
	    ("EMITTER::Compute_use_def_var: null phi-opnd"));
	  FmtAssert(opnd->Kind() == CK_VAR,
	    ("CODEREP::Compute_use_def: phi operand not a VAR"));

	  if ( ! opnd->Is_flag_set(CF_IS_ZERO_VERSION) ) {
	    Compute_use_def_var( du_mgr, opnd, wn, wn_bb );
	  }
	  else {
	    // handle zero-version operands, by looking for def in
	    // the appropriate predecessor
	    Compute_use_def_zero_version_var( du_mgr, opnd, wn,
	      phi->Bb()->Nth_pred(phi_opnd_iter.Curidx()), wn_bb );
	  }
	  
	}

	BB_NODE *bb = phi->Bb();
	if ( (bb->Kind() == BB_DOEND || bb->Kind() == BB_WHILEEND) 
	     && bb->Loop() != NULL ) {
	  DEF_LIST *deflst = du_mgr->Ud_Get_Def(wn);
          // deflst could be NULL if wn is ILOAD
	  if (deflst) {
            if (deflst->Loop_info() == NULL) {
              if (wn_bb && bb->Loop()->Contains( wn_bb->Innermost()))
                deflst->Set_loop_info(bb->Loop());
            }
            else {
              BB_LOOP *prev_loop = deflst->Loop_info();
              if ( bb->Loop()->Step()->Loopdepth() <
		   prev_loop->Step()->Loopdepth() )
	      {
                deflst->Set_loop_info(bb->Loop());
	      }
            }
          }
	}
      }
    } else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
      STMTREP *defstmt = cr->Defstmt();
      CHI_NODE *chi = cr->Defchi();
      // get the defstmt of the chi, 
      // chain of mu-chi is also needed for LNO
      Add_defs_use( du_mgr, defstmt, wn );
  
      // entry chi statement, add to the def use chain
      if ( defstmt->Opr() == OPR_OPT_CHI ) {
        Add_defs_use( du_mgr, defstmt, wn );
      }
      // do we want full use-def info for indirectly defined values,
      // or just a flag noting the fact that we're not getting all of
      // the definitions?
      else if ( WOPT_Enable_Zero_Version || ! WOPT_Enable_DU_Full ) {
        // just say this use has at least one indirect def, and we
        // may never see a direct def. (the list is incomplete)
        DEF_LIST *deflist = du_mgr->Ud_Get_Def(wn);
        deflist->Set_Incomplete();
      }
      else {
        if ( chi->Live() ) {
	  Compute_use_def_var( du_mgr, chi->OPND(), wn, wn_bb );
        }
      }
    } else {
      // regular definition
      STMTREP *defstmt = cr->Defstmt();
      if (OPERATOR_is_scalar_load (WN_operator(wn)) ||
	  OPERATOR_is_scalar_store (defstmt->Opr()))
      {
	Add_defs_use( du_mgr, defstmt, wn );
      }
    }
  }
  else {
    // Define at the function entry if the use is a scalar load
    if (OPERATOR_is_scalar_load (WN_operator (wn))) {
      du_mgr->Add_Def_Use( du_mgr->Entry_Wn(), wn );
    }
  }
}


// ====================================================================
// Find the definitions for the given use WN
// ====================================================================
void
EMITTER::Compute_use_def_expr( DU_MANAGER *du_mgr, WN *wn, 
				BB_NODE *wn_bb )
{
  const OPERATOR opr = WN_operator(wn);
  CODEREP *cr = (CODEREP*)WN_MAP_Get( _wn_to_cr_map, wn );

  switch ( opr ) {

    case OPR_LDID:
    case OPR_LDBITS:
      DU_Phi_Ctr++;
      Compute_use_def_var( du_mgr, cr, wn, wn_bb );

      // now update the loop information for this variable, if any
      // Note that we change it from a BB_LOOP structure to a WN *.
      Update_loop_info( du_mgr, wn, wn_bb );
      break;

    default:
      for ( INT ikid = 0; ikid < WN_kid_count(wn); ikid++ ) {
	Compute_use_def_expr( du_mgr, WN_kid(wn,ikid), wn_bb );
      }
      break;
  }

  // also check for mu-list
  if ( cr && cr->Kind() == CK_IVAR ) {
    MU_NODE *mnode = cr->Ivar_mu_node(); 
    if ( mnode && mnode->Is_Valid() ) {
      CODEREP *mu_cr = mnode->OPND();

      if ( ! mu_cr->Is_flag_set(CF_IS_ZERO_VERSION) ) {
	DU_Phi_Ctr++;
	Compute_use_def_var( du_mgr, mu_cr, wn, wn_bb );

	// now update the loop information for this variable, if any
	// Note that we change it from a BB_LOOP structure to a WN *.
	Update_loop_info( du_mgr, wn, wn_bb );
      }
    }
  } // end mu processing
}

void
EMITTER::Compute_use_def_stmt( DU_MANAGER *du_mgr, WN *wn, BB_NODE *wn_bb )
{
  const OPERATOR opr = WN_operator(wn);

  Is_True( OPERATOR_is_stmt(opr) || OPERATOR_is_scf(opr),
    ("EMITTER::Compute_use_def_stmt: non-stmt: %s", OPERATOR_name(opr)) );

  // check for mu-list on statements
  if ( WN_has_mu(wn, Cfg()->Rgn_level()) ) {
    STMTREP *stmt = (STMTREP*)WN_MAP_Get( _wn_to_cr_map, wn );
    Is_True( stmt != NULL,
      ("Compute_use_def_stmt: no stmtrep for wn:<%d:%d>\n",
       OPERATOR_mapcat(opr), WN_map_id(wn)) );

    MU_LIST_ITER mu_iter;
    MU_NODE *mnode;
    FOR_ALL_NODE( mnode, mu_iter, Init(stmt->Mu_list())) {
      if ( mnode->Is_Valid() ) {
	CODEREP *mu_cr = mnode->OPND();

	if ( ! mu_cr->Is_flag_set(CF_IS_ZERO_VERSION) ) {
	  DU_Phi_Ctr++;
	  Compute_use_def_var( du_mgr, mu_cr, wn, wn_bb );

	  // now update the loop information for this variable, if any
	  // Note that we change it from a BB_LOOP structure to a WN *.
	  Update_loop_info( du_mgr, wn, stmt->Bb() );
	}
      }
    }
  } // end mu processing

  // check for chi-list on statements, but only if we're not providing
  // full def-use information.  
  if ( WN_has_chi(wn, Cfg()->Rgn_level()) ) {
    STMTREP *stmt = (STMTREP*)WN_MAP_Get( _wn_to_cr_map, wn );
    Is_True( stmt != NULL,
      ("Compute_use_def_stmt: no stmtrep for wn:<%d:%d>\n",
       OPERATOR_mapcat(WN_operator(wn)), WN_map_id(wn)) );

    CHI_LIST_ITER chi_iter;
    CHI_NODE *chi;
    FOR_ALL_NODE( chi, chi_iter, Init(stmt->Chi_list()) ) {
      if ( chi->Live() ) {
	BOOL real_var_chi_opnd = 
	  du_mgr->Opt_Stab()->Is_real_var(chi->OPND()->Aux_id());

	// if an STID has a live chi that is not zero version then
	// the DU is incomplete (PV 367510)
	if (WOPT_Enable_DU_Union && OPERATOR_is_scalar_store (opr) &&
	    chi->Aux_id() != du_mgr->Opt_Stab()->Default_vsym()) {
	  du_mgr->Du_Set_Incomplete( wn );
	}

	if ( !real_var_chi_opnd ) {
	  // scalar stores have incomplete use lists if they have a
	  // chi of a virtual variable.
          // Need to do a better job here in the future.  We can not
          // tell if the value will be redefined by function call
          // later in the PU or not.  Need better alias analysis
          // to prune down the chi list.  But, as long as the STID
          // has chi associate with it, we cannot do it.
	  if ( OPERATOR_is_scalar_store (opr) ) {
	    du_mgr->Du_Set_Incomplete( wn );
	  }
	}
	else if ( WOPT_Enable_Zero_Version || ! WOPT_Enable_DU_Full ) {
	  if ( ! chi->OPND()->Is_flag_set(CF_IS_ZERO_VERSION) ) {
	    DU_Phi_Ctr++;
	    Compute_incomplete_defs( du_mgr, chi->OPND() );
	  }
	}
      }
    }
  }


  if ( opr == OPR_BLOCK ) {
    for ( WN *bwn = WN_first(wn); bwn != NULL; bwn = WN_next(bwn) ) {
      Compute_use_def_stmt( du_mgr, bwn, wn_bb );
    }
  } else if ( opr == OPR_IO ) {
    // nothing more to do for IO because we treat it like a black box
  } else if (opr == OPR_REGION) {
    RID *rid = REGION_get_rid(wn);
    Is_True(rid != NULL, ("EMITTER::Compute_use_def_stmt, NULL RID"));
    // see PV 457243
#if defined(TARG_SL2)
    if (RID_TYPE_mp(rid) || RID_TYPE_eh(rid) || RID_TYPE_olimit(rid) ||
	RID_TYPE_major(rid) || RID_TYPE_minor(rid) ||
	RID_TYPE_pragma(rid) ||
	RID_level(rid) < Cfg()->Rgn_level()) {
#else
    if (RID_TYPE_mp(rid) || RID_TYPE_eh(rid) || RID_TYPE_olimit(rid) ||
	RID_TYPE_pragma(rid) ||
	RID_level(rid) < Cfg()->Rgn_level()) {
#endif
      Compute_use_def_stmt(du_mgr, WN_region_body(wn), wn_bb);
      // need to do pragmas also because of xpragmas
      if (RID_TYPE_mp(rid))
	Compute_use_def_stmt(du_mgr, WN_region_pragmas(wn), wn_bb);
    }
  } else {
    // there may or may not be a stmtrep for this wn, but we do it
    // here because it's loop-invariant.
    STMTREP *stmt = (STMTREP*)WN_MAP_Get( _wn_to_cr_map, wn );

    // handle stores with zero-version chi nodes
    if ( OPERATOR_is_scalar_store (opr) ) {
      if ( stmt->Has_zver() ) {
	du_mgr->Du_Set_Incomplete( wn );
      }
    }

    for ( INT ikid = 0; ikid < WN_kid_count(wn); ikid++ ) {
      WN *kid = WN_kid(wn,ikid);
      const OPERATOR kid_opr = WN_operator(kid);
      if ( OPERATOR_is_stmt(kid_opr) || OPERATOR_is_scf(kid_opr) )
	Compute_use_def_stmt( du_mgr, kid, wn_bb );
      else
	Compute_use_def_expr( du_mgr, kid, (stmt?stmt->Bb():NULL) );
    }
  }
}

// ====================================================================
// There are some values that are operands of phi nodes whose result
// is a zero-version variable.  Because we do not normally process
// such phi-nodes, we must do so separately.
// ====================================================================

void
EMITTER::Compute_use_def_zero_ver( DU_MANAGER *du_mgr )
{
  CFG_ITER cfg_iter(Cfg());
  BB_NODE *bb;
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    PHI_LIST_ITER phi_iter;
    PHI_NODE     *phi;
    FOR_ALL_ELEM ( phi, phi_iter, Init(bb->Phi_list()) ) {
      if (!phi->Res_is_cr()) continue;
      CODEREP *res = phi->RESULT();
      if ( res != NULL && res->Kind() == CK_VAR &&
	   res->Is_flag_set(CF_IS_ZERO_VERSION) )
      {
	// process any non-zero-version operands of this phi node
	PHI_OPND_ITER phi_opnd_iter(phi);
	CODEREP *opnd;
	BOOL ignore_pregs = Opt_stab()->Is_virtual(phi->RESULT()->Aux_id());
	DU_Phi_Ctr++;
	FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
	  // make sure the operand is not a zero-version
	  // make sure the operand is not a virtual variable
	  if ( ! opnd->Is_flag_set(CF_IS_ZERO_VERSION) &&
	       du_mgr->Opt_Stab()->Is_real_var(opnd->Aux_id()) )
	  {
	    // have non-zero-version operand of this phi, so follow
	    // its use-def chain, and say its use-list is incomplete
	    Compute_incomplete_defs( du_mgr, opnd );
	  }
	}
      }
    }
  }
}

// ====================================================================
// We set all of the phi->Count() fields to 0, and then each time we
// are about to follow the use-def chain through phi's, we increment
// it.  When we visit a phi, we set its Count field to DU_Phi_Ctr
// so we know we've visited it already.
// ====================================================================
static void
Clear_phi_counts( CFG *cfg )
{
  CFG_ITER cfg_iter(cfg);
  BB_NODE *bb;
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    PHI_LIST_ITER phi_iter;
    PHI_NODE     *phi;
    FOR_ALL_ELEM ( phi, phi_iter, Init(bb->Phi_list()) ) {
      phi->Set_count(0);
    }
  }

  // reset the counter so it's unlikely to wrap around
  DU_Phi_Ctr = 0;
}


void
EMITTER::Compute_use_def(DU_MANAGER *du_mgr)
{
  BOOL gen_du = WOPT_Enable_Generate_DU;
  BOOL collect_ipa = TRUE;

  du_mgr->Set_Entry_Wn(_opt_func);
  du_mgr->Set_Opt_Stab(_opt_stab);
  du_mgr->Set_opt_phase(_opt_phase);
  du_mgr->Set_Tracing( Get_Trace(TP_GLOBOPT, DU_DUMP_FLAG) );
  if ( du_mgr->Tracing() ) {
    fprintf( TFile, "%s EMITTER::Compute_use_def\n%s",SBar,SBar );
  }

  // do not generate info for LNO if no loops
  if ((du_mgr->Opt_phase() == PREOPT_LNO_PHASE
       || du_mgr->Opt_phase() == PREOPT_LNO1_PHASE) &&
      !Has_do_loop() &&
      !PU_mp_needs_lno(Get_Current_PU())
      )
  {
    gen_du = FALSE;
    collect_ipa = FALSE;
  }

  if ( gen_du ) {
    // some initialization never hurt
    Clear_phi_counts(_cfg);
    // handle the whole tree
    Compute_use_def_stmt(du_mgr, du_mgr->Entry_Wn(), NULL);
    // go back and handle zero-versions
    Compute_use_def_zero_ver( du_mgr );
    du_mgr->Set_du_built();
  }

  if ( collect_ipa ) {
    // Compute IPA summary info
    Collect_IPA_summary( du_mgr, du_mgr->Entry_Wn() );
  }

#ifdef Is_True_On
  if ( du_mgr->Du_built() && !du_mgr->Verify() ) {
    DevWarn( "DU_MANAGER::Verify() returned FALSE" );
  }
#endif // is_true_on

}

// ====================================================================
// Create and Delete interface
// ====================================================================

DU_MANAGER* Create_Du_Manager(MEM_POOL *pu_pool)
{ 
  return CXX_NEW(DU_MANAGER, pu_pool);
}

void Delete_Du_Manager(DU_MANAGER *du, MEM_POOL *pu_pool)
{
  CXX_DELETE(du, pu_pool);
}

// ====================================================================
// Check if DU info has been built yet
// ====================================================================

extern BOOL
Du_Built( DU_MANAGER *du )
{
  return du->Du_built();
}

// ====================================================================
// ====================================================================
// Verification routines
// ====================================================================
// ====================================================================

// ====================================================================
// Verify that all nodes in the du-chains are actually in the tree
// ====================================================================

BOOL
DU_MANAGER::Verify_add_wn_to_map( WN *wn, WN_MAP *wn_in_tree_map ) const
{
  BOOL verified = TRUE;
#ifdef Is_True_On
  if ( WOPT_Enable_Verify <= 0 )
    return TRUE;

  if ( WN_MAP32_Get( *wn_in_tree_map, wn ) != 0 ) {
    verified = FALSE;

    if ( Tracing() ) {
      fprintf( TFile, "Verify_add_wn_to_map: WN:<%d:%d> in DAG?\n", 
		OPCODE_mapcat(WN_opcode(wn)), WN_map_id(wn) );
    }
  }
  else {
    WN_MAP32_Set( *wn_in_tree_map, wn, 1 );
  }

  if ( WN_opcode(wn) == OPC_BLOCK ) {
    for ( WN *stmt = WN_first(wn); stmt; stmt = WN_next(stmt) )
      if ( ! Verify_add_wn_to_map( stmt, wn_in_tree_map ) )
	verified = FALSE;
  }
  else {
    for ( INT kidnum = 0; kidnum < WN_kid_count(wn); kidnum++ )
      if ( ! Verify_add_wn_to_map( WN_kid(wn,kidnum), wn_in_tree_map ) )
	verified = FALSE;
  }

#endif
  return verified;
}

BOOL
DU_MANAGER::Verify_du_chains_in_tree( WN *wn, WN_MAP *wn_in_tree_map ) const
{
  BOOL verified = TRUE;
#ifdef Is_True_On
  if ( WOPT_Enable_Verify <= 0 )
    return TRUE;

  // is this whirl in our tree?
  if ( WN_MAP32_Get( *wn_in_tree_map, wn ) == 0 ) {
    verified = FALSE;

    if ( Tracing() ) {
      fprintf( TFile, "Verify_du_chains_in_tree: WN:<%d:%d> not in tree\n", 
		OPCODE_mapcat(WN_opcode(wn)), WN_map_id(wn) );
    }
  }
  else {

    // does this whirl have a Use-def list?
    DEF_LIST *ud_list = Ud_Get_Def(wn);
    if ( ud_list != NULL ) {
      DEF_LIST_ITER def_lst_iter;
      DU_NODE *ud_node;
      FOR_ALL_NODE(ud_node, def_lst_iter, Init(ud_list)) {
	WN *ud_wn = ud_node->Wn();
	if ( ud_wn != NULL ) {
	  if ( WN_MAP32_Get( *wn_in_tree_map, ud_wn ) == 0 ) {
	    verified = FALSE;

	    if ( Tracing() ) {
	      fprintf( TFile, 
		"Verify_du_chains_in_tree: def WN:<%d:%d> from use WN:<%d:%d> not in tree\n", 
		OPCODE_mapcat(WN_opcode(ud_wn)), WN_map_id(ud_wn),
		OPCODE_mapcat(WN_opcode(wn)),    WN_map_id(wn) );
	      fdump_tree( TFile, ud_wn );
	    }
	  }
	}
      }

      // also verify loop
      if ( ud_list->Loop_stmt() != NULL ) {
	if ( WN_MAP32_Get(*wn_in_tree_map, ud_list->Loop_stmt()) == 0 ){
	  verified = FALSE;

	  if ( Tracing() ) {
	    fprintf( TFile, 
	      "Verify_du_chains_in_tree: Loop_stmt WN:<%d:%d> from use WN:<%d:%d> not in tree\n", 
	      OPCODE_mapcat(WN_opcode(ud_list->Loop_stmt())),
	      WN_map_id(ud_list->Loop_stmt()),
	      OPCODE_mapcat(WN_opcode(wn)), WN_map_id(wn) );
	  }
	}
      }
    }

    // does this whirl have a Def-use list?
    USE_LIST *du_list = Du_Get_Use(wn);
    if ( du_list != NULL ) {
      USE_LIST_ITER use_lst_iter;
      DU_NODE *du_node;
      FOR_ALL_NODE(du_node, use_lst_iter, Init(du_list)) {
	WN *du_wn = du_node->Wn();
	if ( du_wn != NULL ) {
	  if ( WN_MAP32_Get( *wn_in_tree_map, du_wn ) == 0 ) {
	    verified = FALSE;

	    if ( Tracing() ) {
	      fprintf( TFile, 
		"Verify_du_chains_in_tree: use WN:<%d:%d> from def WN:<%d:%d> not in tree\n", 
		OPCODE_mapcat(WN_opcode(du_wn)), WN_map_id(du_wn),
		OPCODE_mapcat(WN_opcode(wn)),    WN_map_id(wn) );
	      fdump_tree( TFile, du_wn );
	    }
	  }
	}
      }
    }

    // check out the subtree
    if ( WN_opcode(wn) == OPC_BLOCK ) {
      for ( WN *stmt = WN_first(wn); stmt; stmt = WN_next(stmt) )
	if ( ! Verify_du_chains_in_tree( stmt, wn_in_tree_map ) )
	  verified = FALSE;
    }
    else {
      for ( INT kidnum = 0; kidnum < WN_kid_count(wn); kidnum++ )
	if (! Verify_du_chains_in_tree( WN_kid(wn,kidnum), wn_in_tree_map ))
	  verified = FALSE;
    }
  }
#endif
  return verified;
}

BOOL
DU_MANAGER::Verify_wn_in_tree( void ) const
{
  BOOL verified = TRUE;
#ifdef Is_True_On
  if ( WOPT_Enable_Verify <= 0 )
    return TRUE;

  if ( Entry_Wn() == NULL )
    return verified;

  OPT_POOL_Push( &MEM_local_pool, DU_DUMP_FLAG );

  // create a map of all WNs in this tree
  WN_MAP wn_in_tree_map = WN_MAP32_Create(&MEM_local_pool);
  if ( ! Verify_add_wn_to_map( Entry_Wn(), &wn_in_tree_map ) )
    verified = FALSE;

  // check all of the du-chains to see if the nodes they say are in
  // the tree are in the tree
  if ( ! Verify_du_chains_in_tree( Entry_Wn(), &wn_in_tree_map ) )
    verified = FALSE;

  // get rid of our temporary map
  WN_MAP_Delete(wn_in_tree_map);

  OPT_POOL_Pop( &MEM_local_pool, DU_DUMP_FLAG );

#endif
  return verified;
}

BOOL
DU_MANAGER::Verify_scalar_usage( WN *wn ) const
{
  BOOL verified = TRUE;
#ifdef Is_True_On
  if ( WOPT_Enable_Verify <= 0 )
    return TRUE;

  const OPCODE   opc = WN_opcode(wn);
  const OPERATOR opr = OPCODE_operator(opc);
  
  switch ( opr ) {
  case OPR_LDID:
  case OPR_LDBITS:
    {
      DEF_LIST *ud_list = Ud_Get_Def(wn);
      if ( ud_list == NULL ) {
	verified = FALSE;
	if ( Tracing() ) {
	  fprintf( TFile, "Verify_scalar_usage: no def for WN:<%d:%d>:",
		    OPCODE_mapcat(WN_opcode(wn)), WN_map_id(wn) );
	  fdump_wn( TFile, wn );
	}
      }
      else {
	WN *loop = ud_list->Loop_stmt();
	if ( loop ) {
	  if ( WN_opcode(loop) != OPC_DO_LOOP ) {
	    verified = FALSE;
	    if ( Tracing() ) {
	      fprintf( TFile,
		       "Verify_scalar_usage: non-loop node for WN:<%d:%d>\n",
		       OPCODE_mapcat(WN_opcode(wn)), WN_map_id(wn) );
	      fprintf( TFile, "  is %s\n", OPCODE_name(WN_opcode(loop)) );
	    }
	  }
	}
      }
    }
    break;

  case OPR_STID:
  case OPR_STBITS:
    {
      USE_LIST *du_list = Du_Get_Use(wn);
      if ( du_list == NULL ) {
	// check for exceptions:  volatiles
	if ( TY_is_volatile( WN_ty(wn) ) ||
	     ( ST_class(WN_st(wn)) == CLASS_PREG &&
	       Preg_Is_Dedicated(WN_offset(wn)) ) ) {
	  // ok not to have a use-list
	  goto stid_visit_kids;
	}

	verified = FALSE;
	if ( Tracing() ) {
	  fprintf( TFile, "Verify_scalar_usage: no use for WN:<%d:%d>: ",
		    OPCODE_mapcat(WN_opcode(wn)), WN_map_id(wn) );
	  fdump_wn( TFile, wn );
	}
      }
      // now visit kids
    stid_visit_kids:
      for ( INT kidnum = 0; kidnum < WN_kid_count(wn); kidnum++ )
	if ( ! Verify_scalar_usage( WN_kid(wn,kidnum) ) )
	  verified = FALSE;
    }
    break;

  case OPR_FORWARD_BARRIER:
  case OPR_BACKWARD_BARRIER:
  case OPR_IO:
    // do not look any deeper into these
    break;

  case OPR_BLOCK:
    {
      for ( WN *stmt = WN_first(wn); stmt; stmt = WN_next(stmt) )
	if ( ! Verify_scalar_usage( stmt ) )
	  verified = FALSE;
    }
    break;

  default:
    {
      for ( INT kidnum = 0; kidnum < WN_kid_count(wn); kidnum++ )
	if ( ! Verify_scalar_usage( WN_kid(wn,kidnum) ) )
	  verified = FALSE;
    }
    break;
  }

#endif
  return verified;
}

// ====================================================================
// main verification driver
// ====================================================================

BOOL
DU_MANAGER::Verify(void)
{
  BOOL verified = TRUE;

#ifdef Is_True_On
  if ( WOPT_Enable_Verify <= 0 )
    return TRUE;

  BOOL old_tracing = Tracing();
  Set_Tracing( Get_Trace(TP_GLOBOPT, DU_DUMP_FLAG) );

  if ( Tracing() ) {
    fprintf( TFile, "DU_MANAGER::Verify\n" );
  }

  // verify that all WHIRLs seen in DU_CHAINs are in the tree
  if ( ! Verify_wn_in_tree() )
    verified = FALSE;
  
  // verify that all LDIDs have defs
  if ( ! Verify_scalar_usage( Entry_Wn() ) )
    verified = FALSE;

  if ( Tracing() ) {
    fprintf( TFile, "DU_MANAGER::Verify: %s\n",
	     (verified ? "TRUE":"FALSE") );
  }

  Set_Tracing(old_tracing);
#endif

  return verified;
}


// ========================================================================
//    Temp IPA summary info
// ========================================================================

class BB_SUMMARY_INFO {
public:
  enum {
      BR_FALL_THRU = 0,
      BR_TAKEN = 1,
      BR_UNKNOWN = 2
  };

  IDTYPE  *succ;
  IDTYPE   cd;
  INT      which_edge;   
  INT      dom_dfs_id;
  INT      dom_kid_cnt;
  WN      *first_stmt;
  WN      *last_stmt;
};


// Return a NULL terminated vector of BB ids
IDTYPE *
DU_MANAGER::Get_succ_vec(IDTYPE bb_id)
{
  Check_bb_id(bb_id);
  return _bb_summary[bb_id].succ;
}

IDTYPE
DU_MANAGER::Get_cd(IDTYPE bb_id)
{
  Check_bb_id(bb_id);
  return _bb_summary[bb_id].cd;
}


WN *
DU_MANAGER::Get_first_stmt(IDTYPE bb_id)
{
  Check_bb_id(bb_id);
  return _bb_summary[bb_id].first_stmt;
}

WN *
DU_MANAGER::Get_last_stmt(IDTYPE bb_id)
{
  Check_bb_id(bb_id);
  return _bb_summary[bb_id].last_stmt;
}


BOOL
DU_MANAGER::CD_is_fall_thru(IDTYPE bb_id)
{
  Check_bb_id(bb_id);
  return _bb_summary[bb_id].which_edge == BB_SUMMARY_INFO::BR_FALL_THRU;
}

BOOL
DU_MANAGER::CD_is_br_taken(IDTYPE bb_id)
{
  Check_bb_id(bb_id);
  return _bb_summary[bb_id].which_edge == BB_SUMMARY_INFO::BR_TAKEN;
}


BOOL
DU_MANAGER::Dominate(IDTYPE bb1, IDTYPE bb2)
{
  Check_bb_id(bb1);
  Check_bb_id(bb2);
  BB_SUMMARY_INFO *bs1 = &_bb_summary[bb1];
  BB_SUMMARY_INFO *bs2 = &_bb_summary[bb2];
  return (bs1->dom_dfs_id <= bs2->dom_dfs_id &&
	  bs2->dom_dfs_id <= (bs1->dom_dfs_id + bs1->dom_kid_cnt));
}


void
DU_MANAGER::Collect_BB_id(WN_MAP wn_to_cr_map, WN *wn)
{
  const OPERATOR opr = WN_operator(wn);
  const STMTREP *stmt = (STMTREP*)WN_MAP_Get( wn_to_cr_map, wn );

  if (stmt && (WN_operator(wn) != OPR_FUNC_ENTRY || 
               WN_operator(wn) != OPR_ALTENTRY)) {
    BB_NODE *bb = stmt->Bb();
    BB_SUMMARY_INFO *bs = &_bb_summary[bb->Id()];
    Set_bb_id(wn, bb->Id());
    if (bs->first_stmt == NULL) 
      bs->first_stmt = wn;
    bs->last_stmt = wn;
    if (Tracing())
      fprintf(TFile, "stmt (map_id %d) in BB%d\n", WN_map_id(wn), Get_bb_id(wn));
  }

  if ( opr == OPR_BLOCK ) {
    for ( WN *bwn = WN_first(wn); bwn != NULL; bwn = WN_next(bwn) ) {
      Collect_BB_id(wn_to_cr_map, bwn);
    }
  } else if ( opr == OPR_IO ) {
    // nothing more to do for IO because we treat it like a black box
  } else if (opr == OPR_REGION) {
    Collect_BB_id(wn_to_cr_map, WN_region_body(wn));
  } else {
    for ( INT ikid = 0; ikid < WN_kid_count(wn); ikid++ ) {
      WN *kid = WN_kid(wn,ikid);
      const OPERATOR kid_opr = WN_operator(kid);
      if ( OPERATOR_is_stmt(kid_opr) || OPERATOR_is_scf(kid_opr) )
	Collect_BB_id( wn_to_cr_map, kid);
    }
  }
}


void
DU_MANAGER::Collect_CFG(CFG *cfg)
{
  CFG_ITER cfg_iter;
  BB_NODE *bb;
  FOR_ALL_ELEM (bb, cfg_iter, Init(cfg)) {
    IDTYPE *succ = (IDTYPE *) CXX_NEW_ARRAY(IDTYPE *, bb->Succ()->Len()+1, &_mem_pool);
    BB_LIST_ITER bb_iter;
    BB_NODE *bb_succ;
    INT i = 0;
    FOR_ALL_ELEM( bb_succ, bb_iter, Init(bb->Succ()) ) {
      succ[i++] = bb_succ->Id();
    }
    succ[i] = 0;
    _bb_summary[bb->Id()].succ = succ;
    _bb_summary[bb->Id()].dom_dfs_id = bb->Dom_dfs_id();
    _bb_summary[bb->Id()].dom_kid_cnt = bb->Dom_descendant_cnt();

    // Only identify CD for "real" blocks
    if (bb == cfg->Fake_entry_bb() || bb == cfg->Fake_exit_bb())
      continue;

    // Only identify the CD if the CD is unique.
    BB_NODE *cd_bb = NULL;
    BB_NODE *temp_bb;
    BB_NODE_SET_ITER rcfg_iter;
    FOR_ALL_ELEM( temp_bb, rcfg_iter, Init( bb->Rcfg_dom_frontier() ) ) {
      if (temp_bb == bb) continue;
      if (cd_bb != NULL) {
	cd_bb = NULL;
	break;
      }
      cd_bb = temp_bb;
    }
    if (cd_bb == NULL && bb->Postdominates_strictly(cfg->Entry_bb())) {
      cd_bb = cfg->Entry_bb();
    }
    if (cd_bb != NULL) {
      if (cd_bb == bb || ! cd_bb->Dominates_strictly(bb)) {
	cd_bb = NULL;
      }
    }

    if (cd_bb != NULL) {
      BB_LIST_ITER bb_iter;
      BB_NODE *succ;
      BB_NODE *which_edge = NULL;
      FOR_ALL_ELEM( succ, bb_iter, Init(cd_bb->Succ()) ) {
	if (bb->Postdominates(succ)) {
	  which_edge = succ;
	  break;
	}
      }
      if (which_edge != NULL) {
	BB_SUMMARY_INFO *bs = &_bb_summary[bb->Id()];
	bs->cd = cd_bb->Id();
	bs->which_edge = BB_SUMMARY_INFO::BR_UNKNOWN;
	STMTREP *bb_branch = cd_bb->Branch_stmtrep();
	if (bb_branch != NULL) {
	  OPERATOR opr = bb_branch->Opr();
	  if (opr == OPR_TRUEBR || opr == OPR_FALSEBR) {
	    if (which_edge == cd_bb->Next())  // fall thru
	      bs->which_edge = BB_SUMMARY_INFO::BR_FALL_THRU;
	    else
	      bs->which_edge = BB_SUMMARY_INFO::BR_TAKEN;
	  }
	}
      }
    }
  }

  // Deal with entry and exit BBs
  _entry_bb = cfg->Entry_bb()->Id();
  _exit_bb = cfg->Exit_bb()->Id();
  if (cfg->Exit_bb() == cfg->Fake_exit_bb()) {
    BB_NODE *bb = cfg->Exit_bb();
    IDTYPE *pred = (IDTYPE *) CXX_NEW_ARRAY(IDTYPE *, bb->Pred()->Len()+1, &_mem_pool);
    BB_LIST_ITER bb_iter;
    BB_NODE *bb_pred;
    INT i = 0;
    FOR_ALL_ELEM( bb_pred, bb_iter, Init(bb->Pred()) ) {
      pred[i++] = bb_pred->Id();
    }
    pred[i] = 0;
    _bb_summary[bb->Id()].succ = pred;
  }

  if (Tracing()) {
    INT i;
    for (i = 1; i < cfg->Total_bb_count(); i++) {
      fprintf(TFile, "%3d  ", i);
      if (_bb_summary[i].first_stmt)
	fprintf(TFile, "\t\tfirst_stmt %d  last_stmt %d", 
		WN_map_id(_bb_summary[i].first_stmt),
		WN_map_id(_bb_summary[i].last_stmt));
      fprintf(TFile, "\n");
    }

    fprintf(TFile, "entry bb is %d.  exit bb is %d\n", _entry_bb, _exit_bb);
    for (i = 1; i < cfg->Total_bb_count(); i++) {
      fprintf(TFile, "%3d -> ", i);
      if (_bb_summary[i].succ) {
	for (IDTYPE *p = _bb_summary[i].succ; *p != 0; p++)
	  fprintf(TFile, "%d ", *p);
      }
      if (_bb_summary[i].cd) 
	fprintf(TFile, "\t\tCD%d:%d ", _bb_summary[i].cd, _bb_summary[i].which_edge);
      fprintf(TFile, "\t\tdom_dfs: %d  dom_kid: %d", _bb_summary[i].dom_dfs_id, _bb_summary[i].dom_kid_cnt);
      fprintf(TFile, "\n");
    }
  }
}


void
DU_MANAGER::Alloc_IPA_summary(CFG *cfg)
{
  _bb_cnt = cfg->Total_bb_count();
  _bb_summary = (BB_SUMMARY_INFO *) CXX_NEW_ARRAY(BB_SUMMARY_INFO, _bb_cnt, &_mem_pool);
  BZERO (_bb_summary, sizeof(BB_SUMMARY_INFO) * _bb_cnt);
}


void
EMITTER::Collect_IPA_summary(DU_MANAGER *du_mgr, WN *wn)
{
  du_mgr->Alloc_IPA_summary(Cfg());
  du_mgr->Collect_BB_id(_wn_to_cr_map, wn);
  du_mgr->Collect_CFG(Cfg());
}

// ====================================================================

// DU_MANAGER constructor
//
DU_MANAGER::DU_MANAGER(void)
{ 
  OPT_POOL_Initialize(&_mem_pool, "DU_pool", FALSE, DU_DUMP_FLAG);
  OPT_POOL_Push(&_mem_pool, DU_DUMP_FLAG);
  _du_map = WN_MAP_Create(&_mem_pool);
  _ud_map = WN_MAP_Create(&_mem_pool);
  _bb_map = WN_MAP32_Create(&_mem_pool);
  _val_restored_map = WN_MAP_UNDEFINED;
  _opt_stab = NULL;
  _entry_wn = NULL;
  _opt_phase = PREOPT_PHASE;
  _du_built = FALSE;
  _tracing  = FALSE;
}


