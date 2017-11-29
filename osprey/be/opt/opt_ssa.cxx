/*
 * Copyright (C) 2011, Hewlett-Packard Development Company, L.P. All Rights Reserved.
 */
//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_ssa.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_ssa.cxx,v $
//
// Revision history:
//  3-OCT-94 lo - Initial Version
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
//  Construction of SSA form.
//
//  See the paper Efficiently Computing Static Single Assignment Form
//  and the Control Dependence graph, Ron Cytron, Jeanne Ferrante,
//  Barry K. Rose, and Mark N. Wegman,  TOPLAS v13 n4 Oct., 1991.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_ssa_CXX    "opt_ssa.cxx"
static char *rcs_id = 	opt_ssa_CXX"$Revision: 1.24 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "config.h"
#include "errors.h"
#include "erglob.h"
#include "glob.h"	// for Cur_PU_Name
#include "bb_node_set.h"
#include "tracing.h"

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_htable.h"
#include "opt_util.h"
#include "opt_ssa.h"
#include "opt_mu_chi.h"
#include "opt_wn.h"
#include "opt_prop.h"
#include "wn.h"


#ifdef Is_True_On
static BOOL verify_defs_bb(OPT_STAB *opt_stab)
{
  AUX_ID idx;
  AUX_STAB_ITER aux_stab_iter(opt_stab);

  FOR_ALL_NODE (idx, aux_stab_iter, Init()) {
    if (opt_stab->Aux_stab_entry(idx)->Def_bbs() != NULL)
      return FALSE;
  }
  return TRUE;
}
#endif


// Determine the definition points of a variable
void SSA::Collect_defs_bb(MEM_POOL *pool)
{
  WN *wn;
  BB_NODE *bb;
  CFG_ITER cfg_iter;
  STMT_ITER  stmt_iter;
  
  Is_True(verify_defs_bb(Opt_stab()), ("defs_bb not initialized."));

  FOR_ALL_ELEM (bb, cfg_iter, Init(_cfg)) {
    FOR_ALL_ELEM (wn, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
      // Allow multiple occurence of BB on the prepend list.
      if (WN_has_chi(wn, Cfg()->Rgn_level()) ||
	  OPERATOR_is_scalar_store (WN_operator(wn))) {
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	CHI_LIST *chi_list = Opt_stab()->Get_generic_chi_list(wn);
	if (chi_list) {
	  FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	    Opt_stab()->Aux_stab_entry(cnode->Aux_id())->Prepend_def_bbs(bb, pool);
	  }
	}
      } 
      // Process LHS
      if (OPERATOR_is_scalar_store (WN_operator(wn)))
	Opt_stab()->Aux_stab_entry(WN_aux(wn))->Prepend_def_bbs(bb, pool);
    }
  }
}

//  Determine placement of phi nodes.
//     Fred suggested to exchange the for-variable loop 
//     and the for-bb loop for more efficiently processing.
//     Currently use the algorithm from the paper directly for simplicity.
//
void SSA::Place_phi_node(MEM_POOL *def_bb_pool)
{ 
  AUX_STAB_ITER     opt_stab_iter(Opt_stab());
  BB_LIST_ITER      bb_list_iter;
  BB_LIST_CONTAINER worklist;
  BB_NODE_SET_ITER  bns_iter;
  BB_NODE           *bbx, *bby, *mergebb;
  INT32             var;
  BS_ELT            bbs = Cfg()->Last_bb_id()+1;
  MEM_POOL          bbset_pool;

  OPT_POOL_Initialize(&bbset_pool, "SSA bb set pool", FALSE, SSA_DUMP_FLAG);
  OPT_POOL_Push(&bbset_pool, SSA_DUMP_FLAG);

  BB_NODE_SET inserted(bbs, Cfg(), &bbset_pool, BBNS_EMPTY);
  BB_NODE_SET everonlist(bbs, Cfg(), &bbset_pool, BBNS_EMPTY);
  BB_NODE_SET loopstart(bbs, Cfg(), &bbset_pool, BBNS_EMPTY);
  BB_NODE_SET do_not_insert_ident(bbs, Cfg(), &bbset_pool, BBNS_EMPTY);

  // collect all Startbbs (first bb in the loop).
  BB_NODE *bb;
  CFG_ITER cfg_iter;
  FOR_ALL_ELEM(bb, cfg_iter, Init(Cfg())) {
    if (bb->Loop() && 
	bb->Loop()->Well_formed() &&
	bb->Loop()->Header() == bb &&
	bb->Loop()->Merge() != NULL) {  // must have merge block
      BB_NODE *startbb = bb;
      loopstart.Union1D(startbb);

      // we may disallow inserting identity assignments in certain
      // kinds of loops
      if ( !WOPT_Enable_IVR_Outermost_Loop_Parallel_Region ) {
	BB_LOOP *loop = bb->Loop();
	if ( loop != NULL &&
	     Cfg()->Is_outermost_loop_in_parallel_region(loop,WN_PRAGMA_PDO_BEGIN) )
	{
	  do_not_insert_ident.Union1D(startbb);
	}
      }
    }
  }

  AUX_ID default_vsym = Opt_stab()->Default_vsym();
  
  //  Iterate through the aux symbol table.
  FOR_ALL_NODE(var, opt_stab_iter, Init()) {

    if (var == default_vsym) continue;

    AUX_STAB_ENTRY *psym = Opt_stab()->Aux_stab_entry(var);

    // skip the volatiles
    if (psym->Is_volatile()) continue;

    inserted.ClearD();
    everonlist.ClearD();
    worklist.Clear();
    
    //  Iterate through the BBs that var is defined.
    FOR_ALL_ELEM (bbx, bb_list_iter, Init(psym->Def_bbs())) {
      if (everonlist.MemberP(bbx) == FALSE) {
	everonlist.Union1D(bbx);
	worklist.Append(bbx, &bbset_pool);
      }
    }

    //  Go through the work list.
    while (bbx = worklist.Remove_head(&bbset_pool)) {
      //  Go through the dominator frontier of bbx.
      FOR_ALL_ELEM (bby, bns_iter, Init(bbx->Dom_frontier())) {
	if (inserted.MemberP(bby) == FALSE) {
	  bby->Phi_list()->New_phi_node(var, mem_pool, bby);
	  inserted.Union1D(bby);
	  if (everonlist.MemberP(bby) == FALSE) {
	    everonlist.Union1D(bby);
	    worklist.Append(bby, &bbset_pool);
	  }
	  // try to insert i=i at the loopmerge block
	  if (loopstart.MemberP(bby) &&
	      !do_not_insert_ident.MemberP(bby) ) {
	    //  introduce phi functions at the dominator frontiner of the merge block
	    //  if the loop might exit early and this variable might be in the chi
	    //  list of an identity assignment that will be inserted in the future.
	    //
	    //  if the loop does not exit early, the loop header dominates the loop
	    //  merge and the loop merge postdominates the loop header, their dominance
	    //  frontier should be identical.
	    //
	    if (bby->Loop()->Exit_early()) {
	      mergebb = bby->Loop()->Merge();
	      if (everonlist.MemberP(mergebb) == FALSE) {
		everonlist.Union1D(mergebb);
		worklist.Append(mergebb, &bbset_pool);
	      }
	    }
	    // introduce identity assignment
	    mergebb = Insert_identity_assignment_4_loopexit(bby, var, def_bb_pool);
	    if ( mergebb != NULL ) {
	      if (everonlist.MemberP(mergebb) == FALSE) {
		everonlist.Union1D(mergebb);
		worklist.Append(mergebb, &bbset_pool);
	      }
	    }
	  }
	}
      }
    }
  }

  // Process the default vsym last.
  {
    AUX_STAB_ENTRY *psym = Opt_stab()->Aux_stab_entry(default_vsym);
    inserted.ClearD();
    everonlist.ClearD();
    worklist.Clear();
    
    //  Iterate through the BBs that var is defined.
    FOR_ALL_ELEM (bbx, bb_list_iter, Init(psym->Def_bbs())) {
      if (everonlist.MemberP(bbx) == FALSE) {
	everonlist.Union1D(bbx);
	worklist.Append(bbx, &bbset_pool);
      }
    }

    //  Go through the work list.
    while (bbx = worklist.Remove_head(&bbset_pool)) {
      //  Go through the dominator frontier of bbx.
      FOR_ALL_ELEM (bby, bns_iter, Init(bbx->Dom_frontier())) {
	if (inserted.MemberP(bby) == FALSE) {
	  bby->Phi_list()->New_phi_node(default_vsym, mem_pool, bby);
	  inserted.Union1D(bby);
	  if (everonlist.MemberP(bby) == FALSE) {
	    everonlist.Union1D(bby);
	    worklist.Append(bby, &bbset_pool);
	  }
	  if (loopstart.MemberP(bby) &&
	      !do_not_insert_ident.MemberP(bby) ) {
	    if (bby->Loop()->Exit_early()) {
	      mergebb = bby->Loop()->Merge();
	      if (everonlist.MemberP(mergebb) == FALSE) {
		everonlist.Union1D(mergebb);
		worklist.Append(mergebb, &bbset_pool);
	      }
	    }
	  }
	}
      }
    }
  }
  
  OPT_POOL_Pop(&bbset_pool, SSA_DUMP_FLAG);
  OPT_POOL_Delete(&bbset_pool, SSA_DUMP_FLAG);
}


// Insert_identity_assignment_4_loop
//   Introduce a copy of potential induction variables at the loop_merge block.
//   See opt_ivr.h for more details.
//
BB_NODE *
SSA::Insert_identity_assignment_4_loopexit(BB_NODE *start, AUX_ID aux_id, MEM_POOL *def_bb_pool)
{
  if (!WOPT_Enable_IVR) return NULL;

  AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(aux_id);
  ST *st = sym->St();
  INT32 mtype_class = sym->Mclass();  // cannot use ST_btype(st) because of structures

  // include variables that are integral
  if (sym->Is_real_var() &&
      mtype_class != MTYPE_UNKNOWN &&
      mtype_class != MTYPE_V &&
      !Opt_stab()->Dedicated(aux_id) &&
      !sym->Is_flag_const_init())
    {
      INT32 size = sym->Byte_size();

      // do not handle integers <= 16 bits until we can deal
      // with cvtls 
      if ((mtype_class == MTYPE_CLASS_INTEGER) && size <= 2) {
	sym->Set_dont_replace_iv();
	return NULL;
      }
      
      // only handle floats if we can propagate them
      if ( MTYPE_is_float(mtype_class) &&
           WOPT_Enable_LNO_Copy_Propagate ) {
	sym->Set_dont_replace_iv();
	return NULL;
      }

      // make sure we can come up with a valid type for this identity
      // assignment (pv 418175)
      TY_IDX ty = Identity_assignment_type( sym, Htable()->Phase());
      if ( ty == TY_IDX_ZERO ) {
	sym->Set_dont_replace_iv();
	return NULL;
      }

      BB_NODE *merge_bb = start->Loop()->Merge();

      WN *copy = Create_identity_assignment(sym, aux_id, ty);

      if (!sym->Points_to()->No_alias())
	Opt_stab()->Compute_FFA_for_copy( copy, merge_bb, TRUE /* complete chi list */ );

      merge_bb->Prepend_wn_after_labels(copy);
      return merge_bb;
    }

  sym->Set_dont_replace_iv();
  return NULL;
}

//  Rename the RHS of the WHIRL statement
//
void
SSA::Rename_rhs(OPT_STAB *opt_stab, WN *wn)
{
  OPCODE opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);
  VER_ID du;

  if (OPCODE_has_aux(opc)) {
    if (OPERATOR_is_scalar_load (opr)) {
      if ( opt_stab->Is_volatile(WN_aux(wn)) ) {
	// each volatile reference gets its own version
        opt_stab->Gen_name(WN_aux(wn));
	// get the just-generated version
	du = opt_stab->Stack(WN_aux(wn))->Pop();
	// set the ver stab entry to be defined at the entry
	// i.e., it has no known definition
	opt_stab->Ver_stab_entry(du)->Set_type(ENTRY_STMT);
      }
      else {
        du = opt_stab->Get_name(WN_aux(wn));
      }
      WN_set_ver(wn, du);
    }
    else if (OPERATOR_is_scalar_store (opr)) {
      du = opt_stab->Get_name(WN_aux(wn));
      WN_set_ver(wn, du);
    }
  }
  
  if (WN_has_mu(wn, Cfg()->Rgn_level())) {
    OCC_TAB_ENTRY *occ = opt_stab->Get_occ(wn);
    if (occ->Is_stmt()) {
      MU_LIST *mu_list = occ->Stmt_mu_list();
      if (mu_list) {
	MU_LIST_ITER mu_iter;
	MU_NODE *mnode;
	FOR_ALL_NODE( mnode, mu_iter, Init(mu_list)) {
	  VER_ID ver = opt_stab->Get_name(mnode->Aux_id());
	  mnode->Set_opnd(ver);
	}	
      }
    } else {
      MU_NODE *mnode = occ->Mem_mu_node();
      VER_ID ver = opt_stab->Get_name(mnode->Aux_id());
      mnode->Set_opnd(ver);
    }
  }

  for (INT32 i = 0; i < WN_kid_count(wn); i++)
    Rename_rhs(opt_stab, WN_kid(wn,i));
}

// Rename the variables
//
void SSA::Rename(CFG *cfg, OPT_STAB *opt_stab, BB_NODE *bb)
{
  PHI_LIST_ITER phi_iter;
  PHI_NODE *phi;
  BB_LIST_ITER bb_iter;
  STMT_ITER stmt_iter;
  INT32 pos;
  AUX_ID v;
  BB_NODE *succ, *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  WN *wn;
  AUX_ID var;
  VER_ID du;

  //  Iterate through each phi-node 
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    opt_stab->Gen_name_phi(phi);
  }

  //  Iterate through each statement
  FOR_ALL_ELEM (wn, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {

    OPCODE   opc = WN_opcode(wn);
    OPERATOR opr = OPCODE_operator(opc);
    BOOL process_only_mu_chi = FALSE;

    Is_True( opr != OPR_BLOCK, ("cannot rename a block of statements."));

    // any regions in the whirl are black boxes, process only mu and chi
    if (opc == OPC_REGION) {
      RID *rid = REGION_get_rid(wn);
      Is_True(rid != NULL,("SSA::Rename, NULL rid"));
      if (RID_level(rid) >= opt_stab->Rgn_level())
	process_only_mu_chi = TRUE;
    }

    if (!process_only_mu_chi) {
      // Process RHS
      if ( opr == OPR_COMPGOTO ) {
	// process only the index expression, other kids are statements
	Rename_rhs(opt_stab, WN_kid0(wn));
      }
      // do not rename "black-box" operations, other than mu/chi lists
      else if ( ! OPCODE_is_black_box( opc ) ) {
	for (INT32 i = 0; i < WN_kid_count(wn); i++)
	  Rename_rhs(opt_stab, WN_kid(wn,i));
      }
    }

    // Process side effect (mu)
    if (WN_has_mu(wn, Cfg()->Rgn_level())) {
      MU_LIST *mu_list = opt_stab->Get_stmt_mu_list(wn);
      if (mu_list) {
	MU_LIST_ITER mu_iter;
	MU_NODE *mnode;
	FOR_ALL_NODE( mnode, mu_iter, Init(mu_list)) {
	  VER_ID ver = opt_stab->Get_name(mnode->Aux_id());
	  mnode->Set_opnd(ver);
	}	
      }
    }

    // Process side effect (chi)
    if (WN_has_chi(wn, Cfg()->Rgn_level())) {
      CHI_LIST *chi_list = opt_stab->Get_generic_chi_list(wn);
      if (chi_list) {
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  VER_ID ver = opt_stab->Get_name(cnode->Aux_id());
	  cnode->Set_opnd(ver);
	  opt_stab->Gen_name_chi(cnode, wn);
	}
      } 
    }

    // Process LHS
    if (!process_only_mu_chi && OPERATOR_is_scalar_store (opr)) {
      opt_stab->Gen_name(WN_aux(wn));
    }

  }

  FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
    pos = succ->Pred()->Pos(bb);
    FOR_ALL_ELEM (phi, phi_iter, Init(succ->Phi_list())) {
      v = phi->Aux_id();
	phi->Set_opnd(pos, opt_stab->Stack(v)->Top());
    }
  }

  FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Rename(cfg, opt_stab, dom_bb);  /* child */
  }

  //  The statements are processed in reverse order when poping 
  //  rename stack.
  //
  FOR_ALL_ELEM_REVERSE (wn, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {

    OPERATOR opr = WN_operator(wn);

    // Process side effect (chi)
    if (WN_has_chi(wn, Cfg()->Rgn_level())) {
      CHI_LIST *chi_list = opt_stab->Get_generic_chi_list(wn);
      if (chi_list) {
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  var = cnode->Aux_id();
	  du = opt_stab->Stack(var)->Pop();
	  opt_stab->Enter_du(du, cnode, bb);
	}
      } 
    }

    // Process LHS
    if (OPERATOR_is_scalar_store (opr)) {
      var = WN_aux(wn);
      du = opt_stab->Stack(var)->Pop();
      opt_stab->Enter_du(du, wn, bb);
      WN_set_ver(wn, du);
    }
  }

  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    du = opt_stab->Stack(phi->Aux_id())->Pop();
    opt_stab->Enter_du(du, phi, bb);
  }
}


#ifdef Is_True_On
// verify rename stack after renaming.
static BOOL verify_stack(OPT_STAB *opt_stab)
{
  AUX_ID idx, v;
  AUX_STAB_ITER aux_stab_iter(opt_stab);

  FOR_ALL_NODE (idx, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *psym = opt_stab->Aux_stab_entry(idx);
    if (psym->Is_real_var() || psym->Is_virtual()) {
	  v = opt_stab->Ver_stab_entry(opt_stab->Get_name(idx))->Version();
	  FmtAssert(v == 1, ("Verify_stack: stack for var%d is not empty %d\n",
			     idx, v));
	}
  }
  return TRUE;
}
#endif

//   Construct the SSA form.
//
void SSA::Construct(CODEMAP *htable, CFG *cfg, OPT_STAB *opt_stab)
{
  BB_NODE *bb;
  CFG_ITER cfg_iter;
  AUX_ID var;
  VER_ID du;
  AUX_STAB_ITER opt_stab_iter(opt_stab);

  OPT_POOL_Push(loc_pool, SSA_DUMP_FLAG);

  _cfg = cfg;
  _opt_stab = opt_stab;
  _htable = htable;

  MEM_POOL defs_bb_pool;  // only used for collecting defs_bb
  OPT_POOL_Initialize(&defs_bb_pool, "SSA defs bb pool", FALSE, SSA_DUMP_FLAG);
  OPT_POOL_Push(&defs_bb_pool, SSA_DUMP_FLAG);

  Opt_stab()->Create_entry_chi();

  // For every variable, find out where it is defined.
  Collect_defs_bb(&defs_bb_pool);

  if ( Get_Trace(TP_GLOBOPT, SSA_DUMP_FLAG))  {
    opt_stab->Print(TFile);
  }

  // For every BBs, setup the Phi_list.
  FOR_ALL_ELEM (bb, cfg_iter, Init(cfg))
    bb->Set_phi_list(CXX_NEW(PHI_LIST(bb), mem_pool));
    
  // Placement of phi functions.
  Place_phi_node(&defs_bb_pool);

  OPT_POOL_Pop(&defs_bb_pool, SSA_DUMP_FLAG);
  OPT_POOL_Delete(&defs_bb_pool, SSA_DUMP_FLAG);
  _opt_stab->Reset_def_bbs();

  MEM_POOL rename_pool;
  OPT_POOL_Initialize(&rename_pool, "SSA rename pool", FALSE, SSA_DUMP_FLAG);
  OPT_POOL_Push(&rename_pool, SSA_DUMP_FLAG);

  // Generate the default name for undefined or global variables.
  // Init version and allocate rename-stack.
  //
  FOR_ALL_NODE(var, opt_stab_iter, Init()) {
    AUX_STAB_ENTRY *psym = opt_stab->Aux_stab_entry(var);
    if (psym->Is_real_var() || psym->Is_virtual()) {
      psym->Clear_version();
      psym->Set_stack(CXX_NEW(STACK<AUX_ID>(&rename_pool), &rename_pool));
      du = opt_stab->Gen_name(var);
      opt_stab->Enter_du(du);
    }
  } 

  //  Rename into SSA names.
  Rename(cfg, opt_stab, cfg->Entry_bb());

  //  Verify the rename stack is empty
  Is_True(verify_stack(Opt_stab()), ("ssa: stack is non-empty"));

  OPT_POOL_Pop(&rename_pool, SSA_DUMP_FLAG);
  OPT_POOL_Delete(&rename_pool, SSA_DUMP_FLAG);

  if ( Get_Trace(TP_GLOBOPT, SSA_DUMP_FLAG)) {
    // Dump phi functions
    fprintf(TFile, "PHI INSERTION: \n");
    FOR_ALL_ELEM (bb, cfg_iter, Init(cfg)) 
      if (bb->Phi_list()->Len() > 0) {
	fprintf(TFile, "BB%d: \n", bb->Id());
	bb->Phi_list()->PRINT(TFile);
      }
  }
}


// Perform pointer analysis on the ver_stab
void
SSA::Pointer_Alias_Analysis(void)
{
  if (WOPT_Enable_FSA) {
    BB_NODE *bb;
    STMT_ITER stmt_iter;
    WN *wn;
    CFG_ITER cfg_iter;

    Opt_stab()->Compute_FSA();

    // TODO: The version merging should go
    // into opt_htable during value-numbering to reduce compile-time
    FOR_ALL_ELEM (bb, cfg_iter, Init(Cfg())) {
      PHI_NODE *phi;
      PHI_LIST_ITER phi_iter;
      FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
	for (INT i = 0; i < phi->Size(); i++) {
	  VER_ID idx = phi->Opnd(i);
	  if (Opt_stab()->Ver_stab_entry(idx)->Synonym())  {
	    VER_ID s = idx;
	    while (s != 0 && Opt_stab()->Ver_stab_entry(s)->Synonym())
	      s = Opt_stab()->Ver_stab_entry(s)->Synonym();
	    phi->Set_opnd(i,s);
	  }
	}
      }
      FOR_ALL_ELEM (wn, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) 
	Opt_stab()->Remap_ver_synonym(wn);
    }
    if ( Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG)) {
      fprintf( TFile, "%sPOINTS_TO after pointer analysis\n%s", DBar, DBar );
      _opt_stab->Print_alias_info(TFile);
      
      fprintf( TFile, "%sOcc table after flow sensitive alias analysis\n%s", DBar, DBar );
#ifdef KEY
      FOR_ALL_ELEM (bb, cfg_iter, Init(Cfg())) {
        FOR_ALL_ELEM (wn, stmt_iter, Init(bb->Firststmt(), bb->Laststmt()))
         _opt_stab->Print_occ_tab(TFile,wn);
      }
#endif
      //_opt_stab->Print_occ_tab(TFile);
    }
  }
}


// ====================================================================
// Find all the ver_stab elements that should be marked zero version
// ====================================================================
void
SSA::Find_zero_versions(void)
{
  VER_STAB_ENTRY *vse;
  BB_LIST_ITER bb_list_iter;
  BB_NODE *bbpred;
  INT32 pos;
  PHI_NODE *ph;
  VER_STAB_ENTRY *opnd;
  BOOL phi_opnds_all_real_use;

  OPT_POOL_Push(loc_pool, SSA_DUMP_FLAG);

  // pass over all variable versions
  VER_STAB_ITER ver_stab_iter(Opt_stab()->Ver_stab());
  AUX_ID vid;
  FOR_ALL_NODE(vid, ver_stab_iter, Init()) {
    vse = Opt_stab()->Ver_stab_entry(vid);
    if (! vse->Any_use()) {
      vse->Set_Zero_vers();
      continue;	// such are deleted by DSE
      }
    if (vse->Real_use()) continue;
    // do not create zero version for virtual variables that are not unique
    // or not base on LDAs
    if (Opt_stab()->Special_vsym(vse->Aux_id()))
      continue;
    if (vse->Type() == CHI_STMT) {
      if (WN_operator(vse->Chi_wn()) != OPR_OPT_CHI)
        vse->Set_Zero_vers();
      continue;
      }

    Is_True(vse->Type() == PHI_STMT, 
	    ("SSA::Find_zero_versions: version not defined by phi"));
    // loop thru the operands of the phi
    phi_opnds_all_real_use = TRUE;
    pos = 0;
    ph = vse->Phi();
    FOR_ALL_ELEM(bbpred, bb_list_iter, Init(ph->Bb()->Pred())) {
      opnd = Opt_stab()->Ver_stab_entry(ph->Opnd(pos));
      if (opnd->Zero_vers()) {
        vse->Set_Zero_vers();
        break;
	}
      if (! opnd->Real_use())
	phi_opnds_all_real_use = FALSE;	
      pos++;
      }
    if (vse->Zero_vers()) continue;
    if (phi_opnds_all_real_use) 
      vse->Set_Real_use(); 	// cannot be zero version
    else { // candidate for zero version: add to non-zero phi list
      VER_STAB_LIST_NODE *nz = CXX_NEW(VER_STAB_LIST_NODE(vse), loc_pool);
      AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(vse->Aux_id());
      sym->Set_nonzerophis(sym->Nonzerophis()->Prepend(nz));
      }
    }

  // pass over aux_stab
  AUX_ID i;
  AUX_STAB_ITER aux_stab_iter(Opt_stab());
  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *v = Opt_stab()->Aux_stab_entry(i);

    BOOL changed;
    do {
      VER_STAB_LIST_NODE *prev = NULL;
      VER_STAB_LIST_NODE *cur = v->Nonzerophis();
      changed = FALSE;
      // pass thru all nodes in non-zero phi list for this aux_stab entry
      while (cur != NULL) {
	vse = cur->Vers();

	// loop thru the operands of the phi
	phi_opnds_all_real_use = TRUE;
	pos = 0;
	ph = vse->Phi();
	FOR_ALL_ELEM(bbpred, bb_list_iter, Init(ph->Bb()->Pred())) {
	  opnd = Opt_stab()->Ver_stab_entry(ph->Opnd(pos));
	  if (opnd->Zero_vers()) {
	    vse->Set_Zero_vers();
	    // delete the node from the list
	    if (prev != NULL)
	      prev->Set_Next(cur->Next());
	    else v->Set_nonzerophis(cur->Next());
	    cur = cur->Next();
	    changed = TRUE;
	    break;
	    }
	  if (! opnd->Real_use())
	    phi_opnds_all_real_use = FALSE;	
	  pos++;
	  }
	if (vse->Zero_vers()) continue;
	if (phi_opnds_all_real_use) {
	  vse->Set_Real_use();		// cannot be zero version
	  // delete the node from the list
	  if (prev != NULL)
	    prev->Set_Next(cur->Next());
	  else v->Set_nonzerophis(cur->Next());
	  cur = cur->Next();
	  changed = TRUE;
	  break;
	  }

	prev = cur;
	cur = cur->Next();
        }
    } while (changed);

    v->Set_cr_list(NULL);    // initialize this field unioned with nonzerophis
    }
#ifdef KEY
  if ( Get_Trace(TP_GLOBOPT, SSA_DUMP_FLAG) ) {
    fprintf(TFile, "ZERO VERSIONING: \n");
    Print();
  }
#endif
  OPT_POOL_Pop(loc_pool, SSA_DUMP_FLAG);
}

void SSA::Print() {
    CFG_ITER cfg_iter;
    BB_NODE *bb;
    FOR_ALL_ELEM (bb, cfg_iter, Init(_cfg)){
      if (bb->Phi_list()->Len() > 0) {
        fprintf(TFile, "BB%d: \n", bb->Id());
        bb->Phi_list()->PRINT(TFile);
      }
      WN *wn;
      STMT_ITER stmt_iter;
      FOR_ALL_ELEM (wn, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
        Print_ssa_ver_for_wn(wn, 0);
        if (WN_has_mu(wn, _cfg->Rgn_level())) {
          MU_LIST *mu_list = Opt_stab()->Get_stmt_mu_list(wn);
          if (mu_list)
            mu_list->Print(TFile);
        }
        if (WN_has_chi(wn, _cfg->Rgn_level())) {
          CHI_LIST *chi_list = Opt_stab()->Get_generic_chi_list(wn);
          if (chi_list)
            chi_list->Print(TFile);
        }
      }
    }
}

#ifdef KEY
void SSA::Print_ssa_ver_for_wn(WN* wn, INT indent)
{
  for (INT32 i = 0; i < WN_kid_count(wn); i++)
    Print_ssa_ver_for_wn(WN_kid(wn,i), indent + 1);
  OPCODE opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);
  for (INT j = 0; j < indent; j++)
    fprintf(TFile, " ");
  fdump_wn_no_st(TFile,wn);
  if (WN_has_mu(wn, Cfg()->Rgn_level())) {
    OCC_TAB_ENTRY *occ = Opt_stab()->Get_occ(wn);
    if (occ && !occ->Is_stmt()) {
      MU_NODE *mnode = occ -> Mem_mu_node();
      mnode->Print(TFile);
    }
  }
}
#endif
void
SSA::Create_CODEMAP(void)
{
  _opt_stab->New_coderep(loc_pool);
  _cfg->Set_htable(Htable());  // register htable in the CFG data structure
  
  {
    extern BOOL Simp_Canonicalize;
    BOOL save_simp_canon = Simp_Canonicalize;
    COPYPROP copyprop(_htable, _opt_stab, _cfg, loc_pool);
#ifdef TARG_NVISA
    // in first pass, only want to handle loops, not do optimization
    // (which may cause performance issues because propagates into loop,
    // emits whirl, then does mainopt which may not hoist if originates
    // in loop, unless aggcm which we have turned off).
    if (Htable()->Phase() == PREOPT_PHASE)
      copyprop.Set_disabled();
#endif

    //Simp_Canonicalize = FALSE;
    Value_number(_htable, _opt_stab, _cfg->Entry_bb(), &copyprop, _cfg->Exc());
#ifdef Is_True_On
    _opt_stab->Check_stack();
#endif

    if ( Get_Trace(TP_GLOBOPT, CR_DUMP_FLAG)) {
      fprintf( TFile, "%sAfter SSA::Create_CODEMAP\n%s",
	      DBar, DBar );
      _htable->Print(TFile);
      _cfg->Print(TFile);
      if (_opt_stab->Cr_sr_annot_mgr ())
        _opt_stab->Cr_sr_annot_mgr()->Print (TFile);
    }
  
    Opt_tlog( "CODEMAP", 0, "%d iloadfolds, %d istorefolds",
	      _htable->Num_iloadfolds(), _htable->Num_istorefolds() );
    Opt_tlog( "INPUTPROP", 0, "%d copy propagations",
	      Htable()->Num_inputprops() );
#ifdef TARG_NVISA
    if (Htable()->Phase() == PREOPT_PHASE)
      copyprop.Reset_disabled();
#endif
    Simp_Canonicalize = save_simp_canon;
  }
  
  Opt_stab()->Delete_ver_pool();
  OPT_POOL_Pop(loc_pool, SSA_DUMP_FLAG);

  _htable->Init_var_phi_hash();
  
  // Clear WN annotation map lest we accidently use it in preopt/wopt.
  if (WN_MEMOP_ANNOT_MGR::WN_mem_annot_mgr()) 
    WN_MEMOP_ANNOT_MGR::WN_mem_annot_mgr()->Invalidate ();
}

// ====================================================================
// Remove_opnd - remove the i'th operand from the phi-node (0 is
// first operand
// ====================================================================

void
PHI_NODE::Remove_opnd( INT32 i )
{
  if ( size > 0 ) {
    Reset_OPND(i);
    // shift all of the operands one lower
    for ( INT32 n = i+1; n < size; n++ ) {
      Set_opnd( n-1, OPND(n) );    // Set_opnd will increase use count
    }
    size -= 1;
  }
}


//  PHI_LIST constructor
//
PHI_LIST::PHI_LIST(BB_NODE *bb) 
{ 
  in_degree = bb->Pred()->Len();
}

PHI_LIST *
PHI_LIST::Dup_phi_node(MEM_POOL *pool, BB_NODE *bb)
{
  PHI_LIST *newp;
  PHI_NODE *phi, *p;
  PHI_LIST_ITER phi_iter;
 
  newp = (CXX_NEW(PHI_LIST(bb), pool));
  FOR_ALL_ELEM ( phi, phi_iter, Init(bb->Phi_list()) ) {
    p = CXX_NEW(PHI_NODE(newp->In_degree(), pool, bb), pool);
    p->Set_aux_id(phi->Aux_id());
    p->Set_count(phi->Count());
    p->Set_result(phi->RESULT());
    p->Set_flags(phi->Flags());
    if (phi->Live())
      phi->RESULT()->Set_defphi(p);
    for (INT i = 0; i < newp->In_degree(); i++) {
      p->Set_opnd(i, phi->OPND(i));
    }
    newp->Append(p);
  }
  return newp;
}
// duplicates phi list, if pos is non-negative, add one entry
//
PHI_LIST *
PHI_LIST::Dup_phi_node(MEM_POOL *pool, BB_NODE *bb, INT pos)
{
  PHI_LIST *newp;
  PHI_NODE *phi, *p;
  PHI_LIST_ITER phi_iter;
 
  newp = (CXX_NEW(PHI_LIST(bb), pool));
  FOR_ALL_ELEM ( phi, phi_iter, Init(bb->Phi_list()) ) {
    p = CXX_NEW(PHI_NODE(newp->In_degree(), pool, bb), pool);
    p->Set_aux_id(phi->Aux_id());
    p->Set_count(phi->Count());
    p->Set_result(phi->RESULT());
    p->Set_flags(phi->Flags());
    if (phi->Live())
      phi->RESULT()->Set_defphi(p);
    for (INT i = 0; i < newp->In_degree()-1; i++) {
      p->Set_opnd(i, phi->OPND(i));
    }
    if (pos >= 0)  // dup opnd from pos at the end
      p->Set_opnd(newp->In_degree()-1, phi->OPND(pos));
    newp->Append(p);
  }
  return newp;
}

// ====================================================================
// Remove_opnd - remove the i'th operand from each of the phi-nodes
// 0 is first operand
// ====================================================================
void
PHI_LIST::Remove_opnd( INT32 i )
{
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;

  FOR_ALL_ELEM ( phi, phi_iter, Init(this) ) {
    phi->Remove_opnd(i);
  }

  // we've removed one, so our in-degree must have changed
  Set_in_degree( In_degree() - 1 );
}



// If du is 0, skip the part that deal with the verstab
CODEREP *
SSA::Get_zero_version_CR(AUX_ID aux_id, OPT_STAB *opt_stab, VER_ID du)
{
  CODEREP *retv;

  if (du != 0) {
    retv = opt_stab->Du_coderep(du);
    if (retv) return retv;
  }

  ST *st = opt_stab->St(aux_id);
  TY_IDX ty = TY_IDX_ZERO;
  if (opt_stab->Zero_cr(aux_id) == NULL) {
    if (st != NULL) ty = ST_type(st);
    MTYPE dtype, rtype;
    AUX_STAB_ENTRY *sym = opt_stab->Aux_stab_entry(aux_id);
   
    if (sym->Mtype()==MTYPE_M || MTYPE_is_vector(sym->Mtype())
#ifdef KEY // bug 7733: if dedicated-preg, Mtype() is MTYPE_UNKNOWN
        || (sym->Mclass() & MTYPE_CLASS_VECTOR)
#endif
       )
        rtype = sym->Mtype();
    else {
        rtype = Mtype_from_mtype_class_and_size(sym->Mclass(), 
						    sym->Byte_size());
#ifdef KEY // bug 8186
	if (MTYPE_is_unsigned(sym->Mtype()))
	  rtype = Mtype_TransferSign(MTYPE_U4, rtype);
#endif
	}

    dtype = rtype;
    if (rtype != MTYPE_UNKNOWN && rtype != MTYPE_M) {
      if (MTYPE_is_integral(rtype) &&
          sym->Byte_size() < (MTYPE_size_min(MTYPE_I4)/8)) {
        rtype = Mtype_from_mtype_class_and_size(sym->Mclass(),
                                                MTYPE_size_min(MTYPE_I4)/8);
#ifdef KEY // bug 8186
	if (MTYPE_is_unsigned(sym->Mtype()))
	  rtype = Mtype_TransferSign(MTYPE_U4, rtype);
#endif
      }
      ty = MTYPE_To_TY(rtype);
    }
    CODEREP *cr = _htable->Add_def(aux_id, 0, NULL, rtype, dtype,
				   opt_stab->St_ofst(aux_id), ty, 0, TRUE);
    cr->Set_flag(CF_MADEUP_TYPE);
    cr->Set_flag(CF_IS_ZERO_VERSION);
    // begin - fix for bug OSP 194
    if( opt_stab->Is_volatile(aux_id) )
      cr->Set_var_volatile();
    // end - fix for bug OSP 194 
    opt_stab->Set_zero_cr(aux_id, cr);
  }

  retv = opt_stab->Zero_cr(aux_id);

  if ( du != 0 ) {
    if ( opt_stab->Du_is_volatile(du) ) {
      retv->Set_var_volatile();
    }
    else {
      // cache non-volatile coderep nodes
      opt_stab->Du_set_coderep(du, retv);
    }
  }

  return retv;
}


CODEREP *
SSA::Du2cr( CODEMAP *htable, OPT_STAB *opt_stab, VER_ID du, 
	    STMTREP *stmt)
{
  VER_STAB_ENTRY *vse = opt_stab->Ver_stab_entry(du);
  CODEREP *cr = vse->Coderep();
  if (cr == NULL) {
    WN *ref_wn;
    MTYPE dtype, rtype;
    TY_IDX ty;
    if (vse->Type() == PHI_STMT && (ref_wn = vse->Ref_wn()) != NULL) {
      OPCODE opc = WN_opcode(ref_wn);
      rtype = OPCODE_rtype(opc);
      dtype = OPCODE_desc(opc);
      Is_True(!(dtype==MTYPE_I2 && rtype== MTYPE_I2), ("Create illegal coderep i2i2"));

#ifndef TARG_X8664
      // Fix 770676
      // before u64 lowering, I8I4LDID and I4I4LDID are equivalent,
      // use I4I4 to simplify IVR.
      //
      if (dtype == MTYPE_I4 && rtype == MTYPE_I8)
	rtype = MTYPE_I4;
#else
      if (dtype == MTYPE_U4 && rtype == MTYPE_U8)
	rtype = MTYPE_U4;
#endif

      ty = WN_object_ty(ref_wn);
      cr = htable->Add_def(opt_stab->Du_aux_id(du),
			   opt_stab->Du_version(du),
			   stmt,
			   rtype,
			   dtype,
			   opt_stab->Du_st_ofst(du),
			   ty,
			   0,		// WN_object_ty already lowered the 
					// type, so the field id must be 0.
			   TRUE);
    } else {
      AUX_STAB_ENTRY *sym = opt_stab->Aux_stab_entry(vse->Aux_id());

      ty = TY_IDX_ZERO;
      ST *st = opt_stab->St(vse->Aux_id());
      if (st != NULL) ty = ST_type(st);
      
      if (sym->Mtype()==MTYPE_M || MTYPE_is_vector(sym->Mtype()))
        rtype = sym->Mtype();
      else {
        rtype = Mtype_from_mtype_class_and_size(sym->Mclass(), 
						    sym->Byte_size()); 
#ifdef KEY // bug 8186
	if (MTYPE_is_unsigned(sym->Mtype()))
	  rtype = Mtype_TransferSign(MTYPE_U4, rtype);
#endif
	}

      dtype = rtype;

      if (rtype != MTYPE_UNKNOWN && rtype != MTYPE_M) {
        if (MTYPE_is_integral(rtype) &&
            sym->Byte_size() < (MTYPE_size_min(MTYPE_I4)/8)) {
          rtype = Mtype_from_mtype_class_and_size(sym->Mclass(),
                                                  MTYPE_size_min(MTYPE_I4)/8);
#ifdef KEY // bug 8186
	  if (MTYPE_is_unsigned(sym->Mtype()))
	    rtype = Mtype_TransferSign(MTYPE_U4, rtype);
#endif
	}
        ty = MTYPE_To_TY(rtype);
      }

      cr = htable->Add_def(opt_stab->Du_aux_id(du),
			   opt_stab->Du_version(du),
			   stmt,
			   rtype,
			   dtype,
			   opt_stab->Du_st_ofst(du),
			   ty,
			   0,
			   TRUE);
      cr->Set_flag(CF_MADEUP_TYPE);
    }


    if ( opt_stab->Du_is_volatile(du) ) {
      cr->Set_var_volatile();
    }
    else {
      // cache non-volatile coderep nodes
      opt_stab->Du_set_coderep(du, cr);
    }
  }

  return cr;
}

// Enter into the Hash table
// After this process, all nodes in phi-list are replaced with CODEREP*.
void SSA::Value_number(CODEMAP *htable, OPT_STAB *opt_stab, BB_NODE *bb,
		       COPYPROP *copyprop, EXC *exc)
{
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;
  STMT_ITER     stmt_iter;
  WN           *wn;
  VER_ID        du;
  CODEREP      *cr;

  //  Iterate through each phi-node 
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    if (phi->Live()) {
      du = phi->Result();
      FmtAssert(du != 0,
		("SSA::Value_number: Phi result wasn't set correctly"));
      if (opt_stab->Du_zero_vers(du) ) {
	cr = Get_zero_version_CR(phi->Aux_id(), opt_stab, du);
	phi->Set_result(cr);
      } else {
	cr = Du2cr( htable, opt_stab, du, NULL/*stmtrep*/);
	Is_True(cr != NULL, ("SSA::Value_number: Du2cr cannot return NULL"));
        cr->Set_flag(CF_DEF_BY_PHI);
        cr->Set_defphi(phi);
	phi->Set_result(cr);
      }
      phi->Set_res_is_cr();
    } else cr = NULL;
    opt_stab->Push_coderep(phi->Aux_id(), cr);
  }

  copyprop->Unvisit_nodes();

  Is_True(! copyprop->Past_ret_reg_def(),
	  ("SSA::Value_number: _past_ret_reg_def flag set incorrectly"));

  STMTREP *stmt = NULL;
  //  Iterate through each statement
  FOR_ALL_ELEM (wn, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
    if (OPERATOR_is_scalar_store (WN_operator(wn)) &&
	! opt_stab->Du_any_use(WN_ver(wn)))
      continue; // skip the statement that DSE has recongnized dead code
#ifndef KEY // move deletion to emit phase because htable.cxx needs to mark
	    // with DONT_PROP flag
    else if (Htable()->Phase() == MAINOPT_PHASE && 
	     WN_operator(wn) == OPR_XPRAGMA &&
	     WN_pragma(wn) == WN_PRAGMA_COPYIN_BOUND)
      continue; // mainopt needs to delete such xpragmas because remaining
		// phases do not need it and it may cause problem for mainopt
		// (see bug 659146)
#endif
    else if ((OPERATOR_is_scalar_istore (WN_operator(wn)) ||
	      WN_operator(wn) == OPR_MSTORE) &&
	     WOPT_Enable_Dse_Aggressive &&
	     WN_has_chi(wn, Cfg()->Rgn_level())) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = opt_stab->Get_generic_chi_list(wn);
      BOOL istore_live = FALSE;
      TY_IDX ty = WN_ty(wn);
      // I don't understand the following. We check to see if p is
      // volatile, and we check to see if *p is volatile. What about
      // **p? It could be volatile. Or ***p could be volatile. This
      // seems like a totally wrong approach. -- RK 990524
      if (TY_is_volatile(ty))
	istore_live = TRUE;
      else if (TY_kind(ty) == KIND_POINTER && Ilod_TY_is_volatile(ty))
	istore_live = TRUE;
#ifdef KEY
      else if ((WN_operator(WN_kid1(wn)) == OPR_LDA || WN_operator(WN_kid1(wn)) ==OPR_ILDA) &&
               TY_kind(WN_ty(WN_kid1(wn))) == KIND_POINTER && Ilod_TY_is_volatile(WN_ty(WN_kid1(wn))))
	istore_live = TRUE;
#endif
      else {
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list) ) {
	  if (cnode->Live()) {
	    istore_live = TRUE;
	    break;
	  }
	}
      }
      // The following check of Unique_pt() appears to be a vestigial
      // hack by analogy to code that I "#if 0"'ed from
      // DCE::Required_istore for 597561. This check should be removed
      // as well.
      // Also disturbing is the fact that DCE::Required_istore returns
      // TRUE for ISTOREs through __restrict pointers (see the comment
      // there for the reason), and we don't appear to handle those
      // correctly here. -- RK 990524
      if (! istore_live && !opt_stab->Get_occ(wn)->Points_to()->Unique_pt())
	continue;
    }

    // add new stmtrep with Wn set
    stmt = bb->Add_stmtnode(wn, mem_pool);

#ifdef TARG_SL //fork_joint
    stmt->Set_fork_stmt_flags(WN_is_compgoto_para(wn));
    stmt->Set_minor_fork_stmt_flags(WN_is_compgoto_for_minor(wn));
    // mark istore for vbuf automatic expansion 
    if (WN_operator(wn) == OPR_ISTORE && WN_is_internal_mem_ofst(wn))
      stmt->Set_SL2_internal_mem_ofst(TRUE);
    else
      stmt->Set_SL2_internal_mem_ofst(FALSE); 
#endif 

    stmt->Enter_rhs(htable, opt_stab, copyprop, exc);
    stmt->Enter_lhs(htable, opt_stab, copyprop);

#ifdef KEY // bug 3130
    // Simplification of CVT in identity assignments as in:
    //   I8I4LDID sym10
    //  I4I8CVT
    // I4STID sym10
    OPERATOR stmt_opr = stmt->Opr();
    if (stmt_opr == OPR_STID &&
	ST_class(opt_stab->St(stmt->Lhs()->Aux_id())) != CLASS_PREG) {
      CODEREP *rhs_cr = stmt->Rhs();
      CODEREP *lhs = stmt->Lhs();
      if (rhs_cr->Kind() == CK_OP && rhs_cr->Opr() == OPR_CVT && 
	  rhs_cr->Opnd(0)->Kind() == CK_VAR &&
	  lhs->Aux_id() == rhs_cr->Opnd(0)->Aux_id() &&
	  MTYPE_is_integral(rhs_cr->Dsctyp()) &&
	  MTYPE_is_integral(rhs_cr->Dtyp()) && 
	  MTYPE_is_integral(lhs->Dsctyp()) &&
	  MTYPE_byte_size(rhs_cr->Dtyp()) == MTYPE_byte_size(lhs->Dsctyp())) {
	stmt->Set_rhs(rhs_cr->Opnd(0));
	rhs_cr->DecUsecnt();
      }
    }
#endif

    INT32 linenum = Srcpos_To_Line(stmt->Linenum());	// for debugging

    // should no longer need the Wn
    stmt->Set_wn(NULL);

    BOOL set_dont_prop = TRUE;	// control whether to set any dont_prop flag
    if (WN_operator(wn) == OPR_CALL && WN_Call_Does_Mem_Free(wn)) {
      set_dont_prop = FALSE;	// see 649340
      WOPT_Enable_Itself_Prop = FALSE; // can cause incorrectness in itself-prop
    }

    // For a call, set the call site id for the Nystrom alias analyzer
    // so as to restore it during CODEMAP -> WHIRL translation
    if (OPERATOR_is_call(WN_operator(wn))) {
      UINT32 callsite_id = WN_MAP32_Get(WN_MAP_ALIAS_CGNODE, wn);
      if (callsite_id != 0)
        stmt->Set_constraint_graph_callsite_id(callsite_id);
    }

    // statement has mu only in returns and calls
    if (WN_has_mu(wn, Cfg()->Rgn_level())) {
      MU_LIST_ITER mu_iter;
      MU_NODE *mnode;

      stmt->Set_mu_list( opt_stab->Get_stmt_mu_list(wn) );
      if (stmt->Opr() == OPR_RETURN || stmt->Opr() == OPR_RETURN_VAL
#ifdef KEY
	  || stmt->Opr() == OPR_GOTO_OUTER_BLOCK
#endif
	 )
	stmt->Mu_list()->Delete_def_at_entry_mus(opt_stab);
      FOR_ALL_NODE( mnode, mu_iter, Init(stmt->Mu_list()) ) {
	if (mnode->Opnd() != 0) {
          // if the zero version not turned on and the defining
          // statement for the mu operand is deleted by dse, we
          // turn this mu operand into a zero-version coderep
	  if (opt_stab->Du_zero_vers(mnode->Opnd()) ||
	      ! opt_stab->Du_any_use(mnode->Opnd())) {
	    cr = Get_zero_version_CR(mnode->Aux_id(), opt_stab, mnode->Opnd());
	  } else {
            cr = Du2cr( htable, opt_stab, mnode->Opnd(),
                       NULL/*stmt*/);
	  }
	  mnode->Set_OPND(cr, set_dont_prop);
	  if (opt_stab->NULL_coderep(cr->Aux_id()))
	    opt_stab->Push_coderep(cr->Aux_id(), cr);
	}
      }
    }

    if (WN_has_chi(wn, Cfg()->Rgn_level())) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      
      stmt->Set_chi_list( opt_stab->Get_generic_chi_list(wn) );
      FOR_ALL_NODE( cnode, chi_iter, Init( stmt->Chi_list()) ) {
	//  WARNING:
	//  must enter the chi node even though it has no use,
	//  because copy_prop depends on the chi to indicate overlaps
	//  of versions!
	//
        if (cnode->Live()) {
	  if (opt_stab->Du_zero_vers(cnode->Result())) {
	    // set the zero-version attr with the STMTREP, so that DCE
	    // would not delete this node.
	    stmt->Set_has_zver();   
	    CODEREP *tmpcr = Get_zero_version_CR(cnode->Aux_id(), opt_stab,
                                                 cnode->Result());
	    cnode->Set_RESULT(tmpcr);
	    opt_stab->Push_coderep(tmpcr->Aux_id(), tmpcr);

	    if (opt_stab->Du_zero_vers(cnode->Opnd()) ||
		stmt->Op() == OPC_OPT_CHI)
              tmpcr = Get_zero_version_CR(cnode->Aux_id(), opt_stab,
                                       cnode->Opnd());
            else
              tmpcr = Du2cr( htable, opt_stab, cnode->Opnd(), NULL/*stmt*/);
	    cnode->Set_OPND(tmpcr, set_dont_prop);
    
	  } else {
	    
	    cr = Du2cr(htable, opt_stab, cnode->Result(), stmt);
	    if ( cr != NULL ) {
	      cr->Set_flag(CF_DEF_BY_CHI);
	      cr->Set_defchi(cnode);
	      opt_stab->Push_coderep(cr->Aux_id(), cr);
	    }
	    // note that we may end up with a chi with a null result
	    cnode->Set_RESULT(cr);
	    
            // if the zero version not turned on and the defining
            // statement for the chi operand is deleted by dse, we
            // turn this chi operand into a zero-version coderep
            if (opt_stab->Du_zero_vers(cnode->Opnd()) || 
		stmt->Op() == OPC_OPT_CHI) {

	      Is_True(opt_stab->Du_any_use(cnode->Opnd()),
		      ("Du_any_use if chi for aux %d is not true.",cnode->Aux_id()));

              cr = Get_zero_version_CR(cnode->Aux_id(), opt_stab,
                                       cnode->Opnd());
            } else
              cr = Du2cr( htable, opt_stab, cnode->Opnd(), NULL/*stmt*/);
	    // note that we may end up with a chi with a null operand
	    cnode->Set_OPND(cr, set_dont_prop);
	  }
	}
	else opt_stab->Push_coderep(cnode->Aux_id(), NULL);
      }
    } 

    if (OPERATOR_is_scalar_store (stmt->Opr()) &&
	opt_stab->Aux_stab_entry(stmt->Lhs()->Aux_id())->Is_dedicated_preg())
      copyprop->Set_past_ret_reg_def();
    else if (stmt->Opr() == OPR_RETURN || 
	     stmt->Opr() == OPR_RETURN_VAL ||
#ifdef KEY
  	     stmt->Opr() ==  OPR_GOTO_OUTER_BLOCK ||
#endif
	     stmt->Opr() == OPR_REGION)
      copyprop->Reset_past_ret_reg_def();

    if (WN_has_chi(wn, Cfg()->Rgn_level()) || OPCODE_is_store(WN_opcode(wn)))
      copyprop->Unvisit_nodes();
  }

  // mark IO statement that ends BB due to control flow inside it
  if (stmt != NULL && stmt->Opr() == OPR_IO && 
      bb->Kind() == BB_IO) {
    stmt->Set_has_ctrl_flow();
  }
  
  // Update all corresponding element in phi opnd of all succ
  BB_NODE *succ; BB_LIST_ITER bb_iter;
  FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
    INT32 pos = succ->Pred()->Pos(bb);
    Is_True(pos >= 0 || succ->Pred() == NULL, 
    	    ("SSA:Value_number: cannot find BB in predecessor list of successor"));
    FOR_ALL_ELEM (phi, phi_iter, Init(succ->Phi_list())) {
      if (phi->Live()) {
	if (opt_stab->Du_zero_vers(phi->Opnd(pos)))
	  cr = Get_zero_version_CR(phi->Aux_id(), opt_stab, phi->Opnd(pos));
	else {
	  cr = Du2cr( htable, opt_stab, phi->Opnd(pos), NULL/*stmt*/);
        }
        phi->Set_opnd(pos, cr);
        cr->Set_flag(CF_DONT_PROP);
      }
    }
  }

  bb->Set_vn_processed();	// mark this BB as processed by Value_number()

  // Do value numbering for its dominated node
  BB_NODE *dom_bb; BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Value_number(htable, opt_stab, dom_bb, copyprop, exc);
  }

  STMTREP_ITER stmtrep_iter(bb->Stmtlist());
  FOR_ALL_NODE_REVERSE (stmt, stmtrep_iter, Init()) {

    OPERATOR opr = stmt->Opr();

    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
        opt_stab->Pop_coderep(cnode->Aux_id());
      }
    } 

    // Process LHS
    if (OPERATOR_is_scalar_store (opr))
      opt_stab->Pop_coderep(stmt->Lhs()->Aux_id());
  }

  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    opt_stab->Pop_coderep(phi->Aux_id());
  }
  // no longer refer to the WN list in this block
  bb->Init_stmt( NULL );
}


void SSA::Value_number_mu_node(MU_NODE *mnode)
{
  if (mnode && mnode->Opnd() != 0) {
    CODEREP *cr = Du2cr( _htable, _opt_stab, mnode->Opnd(), NULL/*stmt*/);
    mnode->Set_OPND(cr);
    if (_opt_stab->NULL_coderep(cr->Aux_id()))
      _opt_stab->Push_coderep(cr->Aux_id(), cr);
  }
}

void SSA::Value_number_mu_list(MU_LIST *mu_list)
{
  MU_NODE *mnode;
  MU_LIST_ITER mu_iter;
  FOR_ALL_NODE( mnode, mu_iter, Init(mu_list)) {
    Value_number_mu_node(mnode);
  }
}

// =======================================================================
// Find_stmt_containing_chi - Given a chi node, we do not know what statement
// it is attached to; find the statement knowing that the chi occurs in the
// given bb.
// =======================================================================
static STMTREP *
Find_stmt_containing_chi(CHI_NODE *chi, BB_NODE *bb)
{
  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (! stmt->Has_chi())
      continue;
    CHI_LIST_ITER chi_iter;
    CHI_NODE *cnode;
    CHI_LIST *chi_list = stmt->Chi_list();
    FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
      if (cnode == chi)
	return stmt;
    }
  }
  return NULL;
}

// =======================================================================
// Resurrect_phi - A phi node that was dead has to be resurrected
// because ILOAD to LDID folding introduces a real use of the result of the
// phi; need to call recursively for the phi operands to either revive them
// or just make live, depending on whether Value_number() has processed the
// predecessor or not.
// =======================================================================
void
SSA::Resurrect_phi(PHI_NODE *phi)
{
  VER_ID du;
  CODEREP *cr;

  du = phi->Result();
  if (_opt_stab->Du_zero_vers(du) ) {
    cr = Get_zero_version_CR(phi->Aux_id(), _opt_stab, du);
  } else {
    cr = Du2cr(_htable, _opt_stab, du, NULL/*stmtrep*/);
    cr->Set_flag(CF_DEF_BY_PHI);
    cr->Set_defphi(phi);
  }
  phi->Set_result(cr);
  phi->Set_live();
  phi->Set_res_is_cr();

  // now, the phi operands
  BB_LIST_ITER bb_list_iter;
  BB_NODE *bbpred;
  INT32 pos = 0;
  FOR_ALL_ELEM(bbpred, bb_list_iter, Init(phi->Bb()->Pred())) {
    if (bbpred->VN_processed()) {
      phi->Set_opnd(pos, Revive_phi_chi_opnd(phi->Opnd(pos)));
      phi->OPND(pos)->Set_flag(CF_DONT_PROP);
    }
    else Make_live_phi_chi_opnd(phi->Opnd(pos));
    pos++;
  }
}

// =======================================================================
// Resurrect_chi - A phi node that was dead has to be resurrected
// because ILOAD to LDID folding introduces a real use of the result of the
// chi; need to call recursively for the chi operand to revive it.
// =======================================================================
void
SSA::Resurrect_chi(CHI_NODE *chi)
{
  VER_ID du;
  CODEREP *cr;

  du = chi->Result();
  STMTREP *sr = Find_stmt_containing_chi(chi, _opt_stab->Ver_stab_entry(du)->Bb());
  if (_opt_stab->Du_zero_vers(du) ) {
    cr = Get_zero_version_CR(chi->Aux_id(), _opt_stab, du);
    if (sr) sr->Set_has_zver();
  } else {
    cr = Du2cr(_htable, _opt_stab, du, sr);
    cr->Set_flag(CF_DEF_BY_CHI);
    cr->Set_defchi(chi);
  }
  chi->Set_RESULT(cr);
  chi->Set_live(TRUE);
  chi->Set_dse_dead(FALSE);

  if (!sr || sr->Op() == OPC_OPT_CHI)
    chi->Set_OPND( Get_zero_version_CR(chi->Aux_id(), _opt_stab, chi->Opnd()));
  else {
    chi->Set_OPND(Revive_phi_chi_opnd(chi->Opnd()));
    chi->OPND()->Set_flag(CF_DONT_PROP);
  }
}

// =======================================================================
// Revive_phi_chi_opnd - parameter du was the operand of a dead phi or chi
// that is now made live because its result is an LDID formed from iload
// folding; update the operand to coderep node; if the new operand is also
// defined by a dead phi or chi, call Resurrect_phi or Resurrect_chi recursively
// =======================================================================
CODEREP *
SSA::Revive_phi_chi_opnd(VER_ID du)
{
  VER_STAB_ENTRY *vse = _opt_stab->Ver_stab_entry(du);
  // Fix 629115:  after ILOAD LDA x is folded into LDID x.
  // The use-def is not revived.
  // if (_opt_stab->Du_zero_vers(du)) 
  // return Get_zero_version_CR(vse->Aux_id(), _opt_stab, du);
  if (vse->Type() == PHI_STMT) {
    PHI_NODE *phi = _opt_stab->Ver_stab_entry(du)->Phi();
    if (! phi->Live()) 
      Resurrect_phi(phi);
    return phi->RESULT();
  }
  else if (vse->Type() == CHI_STMT) {
    CHI_NODE *chi = _opt_stab->Ver_stab_entry(du)->Chi();
    if (! chi->Live()) {
      Resurrect_chi(chi);
    }
    return chi->RESULT();
  }
  else return Du2cr(_htable, _opt_stab, du, NULL/*stmtrep*/);
}

// =======================================================================
// Make_live_phi_chi_opnd - parameter du was the operand of a dead phi or chi
// that is now made live due to iload folding happening further down the 
// control flow; mark its defining phi or chi live; call recursively;
// if any def has been processed by Value_number() along the way, call
// Revive_phi_chi_opnd().
// =======================================================================
void
SSA::Make_live_phi_chi_opnd(VER_ID du)
{
  VER_STAB_ENTRY *vse = _opt_stab->Ver_stab_entry(du);
  // Fix 477155:  make a dead phi to be live.
  //   The sym for d[0] is dead before there is no directly use of it.
  //   However, there is a use of d[i] with another vsym and i was
  //   found to be 0 later.  With copy propagation, d[i] becomes
  //   d[0] and the result of a dead phi for d[0] need to be resurrected.
  // if (_opt_stab->Du_zero_vers(du)) 
  //  return;
  BB_NODE *defbb = _opt_stab->Ver_stab_entry(du)->Bb();

  // Fix 766322: if a du has no definition, simply return.
  if (defbb == NULL) 
    return; 

  if (defbb->VN_processed()) 
    Revive_phi_chi_opnd(du);
  else {
    if (vse->Type() == PHI_STMT) {
      PHI_NODE *phi = _opt_stab->Ver_stab_entry(du)->Phi();
      if (! phi->Live()) {
	phi->Set_live();
	// now, the phi operands
	BB_LIST_ITER bb_list_iter;
	BB_NODE *bbpred;
	INT32 pos = 0;

	FOR_ALL_ELEM(bbpred, bb_list_iter, Init(phi->Bb()->Pred())) {
	  if (bbpred->VN_processed()) {
	    phi->Set_opnd(pos, Revive_phi_chi_opnd(phi->Opnd(pos)));
	    phi->OPND(pos)->Set_flag(CF_DONT_PROP);
	  }
	  else Make_live_phi_chi_opnd(phi->Opnd(pos));
	  pos++;
	}
      }
    }
    else if (vse->Type() == CHI_STMT) {
      CHI_NODE *chi = _opt_stab->Ver_stab_entry(du)->Chi();
      if (! chi->Live()) {
	chi->Set_live(TRUE);
        chi->Set_dse_dead(FALSE);
	Make_live_phi_chi_opnd(chi->Opnd());
      }
    }
  }
}

void
PHI_NODE::Print(FILE *fp) const
{
  Print(size, fp);
}

// Print the phi node
//
void
PHI_NODE::Print(INT32 in_degree, FILE *fp)  const
{
  if (! Live())
    PRINT(in_degree, fp);
  else {
    CODEREP *cr = RESULT();
    fprintf(fp, "   sym%dv%d<cr%d> <- phi(", cr->Aux_id(),
	    cr->Version(), cr->Coderep_id());
    for (INT32 i = 0; i < Size(); i++) {
      cr = OPND(i);
      if (cr != NULL) {
        fprintf(fp, "sym%dv%d<cr%d>", cr->Aux_id(), cr->Version(),
	        cr->Coderep_id());
      }
      else {
	fprintf(fp, "<null>");
      }

      if ( i == in_degree-1 )
	fprintf(fp, ")\n" );
      else
	fprintf(fp, "," );
    }
  }
}


//  Print the phi node.
//
void
PHI_NODE::PRINT(INT32 in_degree, FILE *fp) const
{
  if (! Live())
    fprintf(fp, "(not live)");
  if (Dse_dead()) 
    fprintf(fp, "(dse-dead)");
  if (Dce_dead()) 
    fprintf(fp, "(dce-dead)");
  fprintf(fp, "   sym%dv%d <- phi(", Aux_id(), Result());
  for (INT32 i = 0; i < in_degree; i++) {
    fprintf(fp, "sym%dv%d", Aux_id(), Opnd(i));
    if ( i == in_degree-1 )
      fprintf(fp, ")\n" );
    else
      fprintf(fp, "," );
  }
}


//  Print the list of phi functions.
//
void
PHI_LIST::Print(FILE *fp) 
{
  PHI_LIST_ITER phi_iter;
  PHI_NODE *pnode;

  FOR_ALL_NODE(pnode, phi_iter, Init(this))
    pnode->Print(in_degree, fp);
}

//  Print the list of phi functions.
//
void
PHI_LIST::PRINT(FILE *fp) 
{
  PHI_LIST_ITER phi_iter;
  PHI_NODE *pnode;

  FOR_ALL_NODE(pnode, phi_iter, Init(this))
    pnode->PRINT(in_degree, fp);
}

