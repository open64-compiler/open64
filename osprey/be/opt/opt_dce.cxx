/*
 * Copyright (C) 2011, Hewlett-Packard Development Company, L.P. All Rights Reserved.  
 */

/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_dce.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_dce.cxx,v $
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
// Perform Dead-Code Elimination (dce) on the CODEREP/STMTREP 
// representation.
//
// This module contains both unreachable-code elimination (when
// conditional branches are based upon constant conditions), and
// dead-store elimination (stores to variables that are not used
// later in the code).
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "glob.h"	// for Cur_PU_Name
#include "tracing.h"
#include "pf_cg.h"

#include "cxx_base.h"
#include "erbe.h"

#include "opt_base.h"
#include "opt_bb.h"
#include "opt_config.h"
#include "config_opt.h"  // OPT_Cyg_Instrument
#include "opt_sys.h"
#include "bb_node_set.h"
#include "idx_32_set.h"
#include "opt_cfg.h"
#include "opt_ssa.h"
#include "opt_sym.h"
#include "opt_mu_chi.h"
#include "opt_htable.h"
#include "opt_main.h"
#include "opt_alias_rule.h"
#include "opt_exc.h"
#include "opt_util.h"
#include "opt_project.h"
#include "opt_dce.h"

// ====================================================================
//
// DCE class - This unexported class holds the (minimal) data and 
// all of the methods for performing dead-code elimination.  Most
// comments are in the prolog of the methods.
//
// ====================================================================

class DCE {
  private:
    OPT_PHASE    _opt_phase;
    BOOL         _is_varargs_func;  // are we processing a vararg function
    BOOL         _enable_dce_global;
    BOOL         _enable_dce_alias; 
    BOOL         _do_unreachable;
    BOOL         _enable_agg_dce;
    BOOL         _enable_identity_removal;
    BOOL         _enable_preg_renumbering;
    CFG         *_cfg;              // handle on the control-flow graph
    OPT_STAB    *_opt_stab;         // handle on the Opt's symbol table
    CODEMAP     *_htable;           // handle on the hash table
    const ALIAS_RULE  *_alias_rule; // handle on the Opt's alias-rules
    BOOL         _tracing;          // is general tracing enabled?
    BB_NODE_SET *_may_need_label;   // set of blocks that may need a
                                    // label statement added 

    BB_NODE_SET *_keep_unreached;   // set of blocks that should be
                                    // kept around even if not reached
                                    // (used during dse phase)

    BB_NODE_SET *_may_need_goto;    // set of blocks whose branch has
                                    // been eliminated, and may need a
                                    // goto inserted

    BB_NODE_SET *_region_start_bbs; // set of blocks that represent
                                    // the start of regions 

    BS          *_dce_visited;      // CR that DCE already visited;
    BS          *_retvsym_visited;  // CR that RETVSYM already visited;

    AUX_ID       _return_vsym;
    AUX_ID       _return_vsym_max;  // local handle on Return_vsym a
                                    // largest variable id that is
                                    // larger than any aliased by vsym
                                    // Used as a marker to cover all
                                    // other uses of the vsym 

    COND_EVAL   *_cond_eval;        // array of eval indexed by bb id
                                    // to hold the condition in
                                    // conditional branches, and
                                    // whether or not it should be
                                    // considered to evaluate to true, etc.

    CODEREP    **_cond_coderep;     // array of the conditions
    IDX_32_SET  *_return_vsym_full_set;  // Set of all AUX_IDs aliased by vsym

    IDX_32_SET  *_return_vsym_reqd_set;  // Set of AUX_IDs aliased by
                                         // vsym, that are still
                                         // required 
    STACK<MU_NODE*> *_mu_stack;
    STACK<POINTS_TO*> *_points_to_stack; // stacks used for alias
                                         // analysis during dce 

    MOD_PHI_BB_CONTAINER *_mod_phis;     // NULL if !Htable()->Phi_hash_valid()
#if defined(TARG_SL)
    vector<STMTREP *> *_injured_aux_intrnop; // stacks used for store aux intrinsic op need repaired to live
#endif

    // If a block belong to neither _may_throw_bbs nor _nothrow_bbs, 
    // it must be a new block created after Collect_may_throw_bbs() is called. 
    //
    std::map<IDTYPE, BOOL> _may_throw_bbs;
    std::map<IDTYPE, BOOL> _nothrow_bbs;

    // which phase of DCE is running
    enum {
      DCE_UNKNOWN,		// default value until set
      DCE_UNREACHABLE,		// unreachable-code elim
      DCE_DEAD_STORE		// dead-store elim
    } _dce_phase;

    // access functions
    OPT_STAB *Opt_stab(void) const
		{ return _opt_stab; }
    CODEMAP *Htable(void) const
		{ return _htable; }
    AUX_ID Return_vsym(void) const
		{ return _return_vsym; }
    AUX_ID Return_vsym_max(void) const
		{ return _return_vsym_max; }
    IDX_32_SET *Return_vsym_full_set(void) const 
		{ return _return_vsym_full_set; }
    IDX_32_SET *Return_vsym_reqd_set(void) const 
		{ return _return_vsym_reqd_set; }
    
    COND_EVAL Cond_eval( const BB_NODE *bb ) const
		{ return _cond_eval[bb->Id()]; }
    void Set_cond_eval( const BB_NODE *bb, COND_EVAL eval ) const
		{ _cond_eval[bb->Id()] = eval; }
    CODEREP *Cond_coderep( const BB_NODE *bb ) const
		{ return _cond_coderep[bb->Id()]; }
    void Set_cond_coderep( const BB_NODE *bb, CODEREP *cond ) const
		{ _cond_coderep[bb->Id()] = cond; }
    
    // private constructor so it cannot be used
    DCE(void);
    DCE(const DCE&);
    DCE& operator = (const DCE&);

    BOOL Is_dce_visited(const CODEREP *cr) const
      { Is_True(cr->Coderep_id()>0,("DCE:Is_dce_visited, illegal CR id"));
	return BS_MemberP(_dce_visited, cr->Coderep_id());
      }
    BOOL Is_retvsym_visited(const CODEREP *cr) const
      { Is_True(cr->Coderep_id()>0,("DCE:is_retvsym_visited, illegal CR id"));
	return BS_MemberP(_retvsym_visited, cr->Coderep_id());
      }
    void Set_dce_visited(const CODEREP *cr)
      { Is_True(cr->Coderep_id()>0,("DCE:Set_dce_visited, illegal CR id"));
	_dce_visited = BS_Union1D(_dce_visited, cr->Coderep_id(),
				  _cfg->Loc_pool());
      }
    void Set_retvsym_visited(const CODEREP *cr)
      { Is_True(cr->Coderep_id()>0,("DCE:Set_retvsym_visited, illegal CR id"));
	_retvsym_visited = BS_Union1D(_retvsym_visited, cr->Coderep_id(),
				      _cfg->Loc_pool());
      }

    //
    // methods for doing unreachable code elimination
    // 

    BB_NODE_SET *May_need_label( void ) const
		{ return _may_need_label; }
    void Reset_reaching_conditions(BB_NODE_SET *) const;
    void Compute_reaching_conditions(BB_NODE *bb, BB_NODE_SET *) const;
    BOOL Check_redundant_cond_br_new( BB_NODE *, CODEREP *, BB_NODE_SET *) const;
    BOOL Check_conditional_branches_dom( BB_NODE *,BB_NODE_SET * ) const;
    BOOL Check_conditional_branches_pred( CFG *cfg ) const;
    BB_NODE *Branch_target_block( const STMTREP *br_stmt ) const;
    void Add_goto_stmt(BB_NODE *, BB_NODE *, SRCPOS, BOOL) const;
    BOOL Check_constant_cond_br( BB_NODE *bb ) const;
    void Replace_condition_with_constant(BB_NODE *bb, INT64 val) const;
    void Check_unreachable_blocks( void ) const;
    void Replace_condbr_with_uncondbr( 
      BB_NODE *bb, STMTREP *cond_br_stmt, BB_NODE *goto_bb ) const;
    BOOL Update_predecessor_lists( BB_NODE *bb ) const;
    void Check_for_label( BB_NODE *bb ) const;
    BOOL Hasexception( BB_NODE *bb ) const;
    void Check_for_unreachable_exceptions( BB_NODE *bb ) const;
    void Remove_path( BB_NODE *pred, BB_NODE *succ ) const;
    void Remove_unreached_statements( BB_NODE *bb ) const;

    //
    // functions for doing dead-store elimination
    //

    BB_NODE_SET *Keep_unreached( void ) const
		{ return _keep_unreached; }
    void Keep_unreached_bb( BB_NODE *bb ) const;
    BB_NODE_SET *May_need_goto( void ) const
		{ return _may_need_goto; }
    BB_NODE_SET *Region_start_bbs( void ) const
		{ return _region_start_bbs; }
    
    STACK<POINTS_TO *> *Points_to_stack( void ) const
		{ return _points_to_stack; }
    STACK<MU_NODE *> *Mu_stack( void ) const
		{ return _mu_stack; }

    // should we allow copy-propagation of mu/chi/etc. during dce?
    // Until we decide if we need to update pvl lists, etc., we will
    // not allow it.
    BOOL Allow_dce_prop( void ) const
		{ return FALSE; }
    CODEREP *Find_current_version( const STMTREP *, const CODEREP *) const;

    BOOL Aliased( const POINTS_TO *pt1, const POINTS_TO *pt2 ) const;
    BOOL Loop_pragma(WN_PRAGMA_ID pragma) const;
    BOOL Required_call( const STMTREP *stmt, OPERATOR oper ) const;
    BOOL Required_asm( const STMTREP *stmt ) const;
    BOOL Required_store( const STMTREP *stmt, OPERATOR oper ) const;
    BOOL Required_istore( const STMTREP *stmt, OPERATOR oper ) const;
    BOOL Required_pragma( const STMTREP *stmt ) const;
    BOOL Required_stmt( const STMTREP *stmt ) const;
    BOOL Required_phi ( const PHI_NODE *phi) const;
    BOOL Required_bb ( const BB_NODE *bb ) const;
    CODEREP *Dce_prop(CODEREP *cr) const;
    void Mark_coderep_live( CODEREP *cr ) const;
    void Mark_phinode_live( PHI_NODE *phi, BOOL visit_opnds ) const;
    void Mark_chinode_live( CHI_NODE *chi, STMTREP *def_stmt ) const;
    void Mark_zero_version_chinode_live( STMTREP *stmt ) const;
    void Mark_cr_munode_live( CODEREP *cr ) const;
    void Mark_sr_munode_live( STMTREP *sr ) const;
    void Mark_block_live( BB_NODE *bb ) const;
    BOOL Need_condbr_target_label(STMTREP *stmt, BB_NODE *target) const;
    void Mark_branch_related_live( STMTREP *stmt ) const;
    void Mark_region_exits_live( STMTREP *stmt ) const;
    void Mark_return_vsym_chi_live( CHI_NODE *chi )const;
    void Mark_return_vsym_mu_ref_live( CODEREP *cr ) const;
    void Mark_return_vsym_phi_live( PHI_NODE *phi ) const;
    CODEREP *Prop_return_vsym_new_result( CODEREP * ) const;
    void Propagate_return_vsym_cr( CODEREP * ) const;
    void Propagate_return_vsym_bb( BB_NODE * ) const;
    void Mark_infinite_loops_live( void ) const;
    void Mark_statement_live( STMTREP *stmt ) const;
    void Mark_statements_dead( void ) const;
    BOOL BB_branch_live( BB_NODE *bb ) const;
    void Get_full_rcfg_dom_frontier( BB_NODE *bb ) const;
    void Add_path_to_ipdom( BB_NODE *bb ) const;
    void Replace_control_dep_succs( BB_NODE *bb ) const;
    void Check_required_blocks( void ) const;
    void Check_required_doend( BB_NODE *bb ) const;
    void Check_required_goto( BB_NODE *bb ) const;
    void Check_required_io( BB_NODE *bb ) const;
    void Check_required_logif( BB_NODE *bb ) const;
    void Check_required_region( BB_NODE *bb ) const;
    void Update_region_information( void ) const;
    void Check_required_repeatend( BB_NODE *bb ) const;
    void Check_required_vargoto( BB_NODE *bb ) const;
    void Check_required_agoto( BB_NODE *bb ) const;
    void Check_required_whileend( BB_NODE *bb ) const;
    void Find_required_statements( void ) const;
    void Find_assumed_goto_blocks( BB_NODE_SET *assumed_goto ) const;
    void Insert_required_gotos( void ) const;
    void Update_branch_to_bb_labels( BB_NODE *bb ) const;
#ifdef KEY
    void Update_branch_to_bbs_labels( BB_LIST *bbs ) const;
#endif
    BOOL Remove_dead_statements( void ) ;
    BOOL Enable_dce_global( void ) const { return _enable_dce_global; }
    BOOL Enable_dce_alias( void ) const  { return _enable_dce_alias; }
    BOOL Enable_aggressive_dce ( void ) const 
                                         { return _enable_agg_dce; }
    BOOL Enable_identity_removal(void) const { return _enable_identity_removal; }
    BOOL Enable_preg_renumbering(void) const { return _enable_preg_renumbering; }

    // the scf/non_scf property on opcodes has been pretty badly
    // polluted, so figure out for our own purposes what is a branch
    // The one exception is IO, which is only a branch if the block
    // is also a BB_IO block.
    BOOL Is_branch( OPERATOR opr ) const
		{ 
		  switch ( opr ) {
		    case OPR_AGOTO:
		    case OPR_COMPGOTO:
		    case OPR_GOTO:
		    case OPR_FALSEBR:
		    case OPR_REGION_EXIT:
		    case OPR_RETURN:
		    case OPR_RETURN_VAL:
		    case OPR_TRUEBR:
#ifdef KEY
		    case OPR_GOTO_OUTER_BLOCK:
#endif
                   case OPR_ZDLBR:
		      return TRUE;
		    default:
		      return FALSE;
		  }
		}

     void Collect_may_throw_bbs (void);
     BOOL BB_may_throw (BB_NODE* b);
     BOOL Strip_try_region_helper (BB_REGION*);
     BOOL Strip_try_region (void);

public:

    DCE(CFG *cfg, OPT_STAB *optstab, ALIAS_RULE *alias_rule, 
	CODEMAP *htable, BOOL tracing, OPT_PHASE opt_phase, 
	BOOL do_unreachable, BOOL do_dce_global, BOOL do_dce_alias, 
        BOOL do_agg_dce, BOOL do_identity_removal, BOOL do_preg_renumbering)
      : _cfg(cfg), _opt_stab(optstab), _alias_rule(alias_rule),
	_htable(htable), _tracing(tracing), _opt_phase(opt_phase),
	_dce_phase(DCE_UNKNOWN), _do_unreachable(do_unreachable),
        _enable_dce_global(do_dce_global && WOPT_Enable_DCE_Global),
        _enable_dce_alias(do_dce_alias && WOPT_Enable_DCE_Alias),
        _enable_agg_dce(do_agg_dce && WOPT_Enable_Aggressive_dce),
        _enable_identity_removal(do_identity_removal),
	_enable_preg_renumbering(do_preg_renumbering)
      {
	_may_need_label = CXX_NEW(
	  BB_NODE_SET(cfg->Total_bb_count(), cfg, cfg->Loc_pool(),
		      BBNS_EMPTY), cfg->Loc_pool());
        _keep_unreached = CXX_NEW(
	  BB_NODE_SET(cfg->Total_bb_count(), cfg, cfg->Loc_pool(),
		      BBNS_EMPTY), cfg->Loc_pool());
        _may_need_goto = CXX_NEW(
	  BB_NODE_SET(cfg->Total_bb_count(), cfg, cfg->Loc_pool(),
		      BBNS_EMPTY), cfg->Loc_pool());
	if ( cfg->Has_regions() ) {
	  _region_start_bbs = CXX_NEW(
	    BB_NODE_SET(cfg->Total_bb_count(), cfg, cfg->Loc_pool(),
			BBNS_EMPTY), cfg->Loc_pool());
	}
	else {
	  _region_start_bbs = NULL;
	}
	
	_is_varargs_func = Opt_stab()->Is_varargs_func();
	_return_vsym = Opt_stab()->Return_vsym();
	_return_vsym_max = 0;	// we'll find the real value later
	_return_vsym_full_set = NULL;// we'll fill in later
	_return_vsym_reqd_set = NULL;// we'll fill in later

	if ( Enable_dce_alias() ) {
	  _points_to_stack = CXX_NEW(
	    STACK<POINTS_TO *>(cfg->Loc_pool()), cfg->Loc_pool());
	  _mu_stack = CXX_NEW(
	    STACK<MU_NODE *>(cfg->Loc_pool()), cfg->Loc_pool());
	}
	else {
	  _points_to_stack = NULL;
	  _mu_stack = NULL;
	}

	// allocate structures to track conditions in branches
	if ( WOPT_Enable_DCE_Branch ) {
	  _cond_eval = TYPE_OPT_POOL_ALLOC_N( COND_EVAL, 
		cfg->Loc_pool(), cfg->Total_bb_count(), DCE_DUMP_FLAG );
	  BZERO( _cond_eval, sizeof(COND_EVAL)*cfg->Total_bb_count());

	  _cond_coderep = TYPE_OPT_POOL_ALLOC_N( CODEREP*, 
		cfg->Loc_pool(), cfg->Total_bb_count(), DCE_DUMP_FLAG );
	  BZERO(_cond_coderep,sizeof(CODEREP*)*cfg->Total_bb_count());
	}
	else {
	  _cond_eval = NULL;
	  _cond_coderep = NULL;
	}
        if (Htable()->Phi_hash_valid())
          _mod_phis = CXX_NEW(MOD_PHI_BB_CONTAINER(cfg->Loc_pool()),
                              cfg->Loc_pool());
        else
          _mod_phis = NULL;
	
	_dce_visited = BS_Create_Empty(Htable()->Coderep_id_cnt() + 1,
				       cfg->Loc_pool()),
	_retvsym_visited = BS_Create_Empty(Htable()->Coderep_id_cnt() + 1,
					   cfg->Loc_pool());
#if defined(TARG_SL)
        _injured_aux_intrnop = CXX_NEW(vector<STMTREP *>, cfg->Loc_pool());
#endif
      }

    ~DCE(void)
      {
        MOD_PHI_BB     *tmp;
        MOD_PHI_BB_ITER mod_iter(_mod_phis);
        FOR_ALL_NODE(tmp, mod_iter, Init()) {
          PHI_LIST_ITER phi_iter;
          PHI_NODE *pnode;

          FOR_ALL_NODE(pnode, phi_iter, Init(tmp->Old_lst())) {
	    Htable()->Remove_var_phi_hash(pnode);
          }
          FOR_ALL_NODE(pnode, phi_iter, Init(tmp->New_lst())) {
            Htable()->Enter_var_phi_hash(pnode);
          }
        }
        if ( _mod_phis != NULL )
          CXX_DELETE(_mod_phis, _cfg->Loc_pool());
      }

    BOOL Enable_dce_unreachable( void ) const { return _do_unreachable; }
    void Set_phase_unreachable( void )
		{ _dce_phase = DCE_UNREACHABLE; }
    void Set_phase_dead_store( void )
		{ _dce_phase = DCE_DEAD_STORE; }
    BOOL Tracing(void) const	{ return _tracing; }
    BOOL Unreachable_code_elim(void);
    BOOL Dead_store_elim(void) ;
    void Init_return_vsym( void );
#if defined(TARG_SL)
    void Append_Injured_AuxIntrnOp (STMTREP *stmt) const {
       _injured_aux_intrnop->insert(_injured_aux_intrnop->begin(), (STMTREP *)stmt);
    };
    void Repair_Injured_AuxIntrnOP() const;
#endif
    
}; // end of class DCE


#if defined(TARG_SL)
void
DCE::Repair_Injured_AuxIntrnOP() const {

  for (INT32 i = 0; i < _injured_aux_intrnop->size(); i++) {
    STMTREP *stmt = (*_injured_aux_intrnop)[i];
    if (stmt->Live_stmt())
      continue;
    CODEREP *rhs = stmt->Rhs();
    if (CR_Intrinsic_Op_Slave(rhs)) {
      CODEREP *parm2cr = rhs->Opnd(0);	// first parameter
      Is_True(parm2cr->Kind() == CK_IVAR, ("Repair_Injured_AuxIntrnOP::kid of intrinsic op must be parameter"));
      CODEREP *op2cr = parm2cr->Ilod_base();
      if (op2cr) {
        switch (op2cr->Kind()) {
          case CK_VAR: 
          {
            if(op2cr->Defstmt()->Live_stmt())	
	      Mark_statement_live(stmt);	
	  }
	  break;	
	  case CK_OP: 
          {
	    Mark_statement_live(stmt);
	  };
     	  break; 	
          case CK_CONST:
            break; // do nothing	
	  default:
	    Is_True (0, ("Repair_Injured_AuxIntrnOP::slave intrinsic op(c3_ptr): first parameter is unsupported kind coderep"));	
        } // end switch
      } else {
        Is_True (0, ("Repair_Injured_AuxIntrnOP::slave intrinsic op(c3_ptr): first parameter is null"));
      }
    } else {
      Is_True(0, ("Repair_Injured_AuxIntrnOP::rhs is not injure AuxIntrn"));
    }
  }
  return;
}
#endif

// ====================================================================
// Keep this only until we figure it out, then re-inline it above
// ====================================================================

void 
DCE::Keep_unreached_bb( BB_NODE *bb ) const
{ 
  if ( ! bb->Reached() )
    Keep_unreached()->Union1D( bb );
}

// ====================================================================
// Find the block branched to from the statement
// ====================================================================

BB_NODE *
DCE::Branch_target_block( const STMTREP *br_stmt ) const
{
  INT32 label_num = br_stmt->Label_number();
  return _cfg->Get_bb_from_label(label_num);
}

// ====================================================================
// Add an unconditional goto statement in "bb" to jump to "goto_bb" and 
// possibly add a label to "goto_bb"
// ====================================================================

void
DCE::Add_goto_stmt( BB_NODE *bb, BB_NODE *goto_bb, SRCPOS ln,
		   BOOL rgn_exit ) const
{
  const STMTREP *bb_branch = bb->Branch_stmtrep();
  if ( bb_branch != NULL ) {
    const OPERATOR opr = bb_branch->Opr();
    // todo note: the opc_io statement should get is_call property,
    // so we don't need to check for it separately
    Warn_todo( "DCE::Add_goto_stmt: OPC_IO should be call" );
    if ( ! (OPERATOR_is_call(opr)||opr==OPR_IO) || _cfg->Calls_break() ) {
      FmtAssert( FALSE,
	("DCE::Add_goto_stmt BB:%d already has branch %s", 
	 bb->Id(), OPERATOR_name(opr)) );
    }
  }

  // Decide if we need to have an unconditional branch
  if ( bb->Next() != goto_bb ) {
    // need to have a goto statement 

    // does the target block have a label associated with it?
    if ( goto_bb->Labnam() == 0 ) {
      _cfg->Append_label_map( _cfg->Alloc_label(), goto_bb );
    }

    STMTREP *new_stmt;
    if (rgn_exit)	// can't add a goto to a region exit block
      new_stmt = CXX_NEW( STMTREP(OPC_REGION_EXIT), _cfg->Mem_pool() );
    else		// normal case
      new_stmt = CXX_NEW( STMTREP(OPC_GOTO), _cfg->Mem_pool() );
    new_stmt->Init_Goto( NULL, goto_bb->Labnam(), ln );
    bb->Append_stmtrep( new_stmt );

    if ( _dce_phase == DCE_UNREACHABLE ) {
      // the goto block may need a label added to it if it is reached
      // so wait until later to add it
      May_need_label()->Union1D( goto_bb );
    }
    else if ( _dce_phase == DCE_DEAD_STORE ) {
      // if we're adding a goto during this phase, we need to also add
      // the label right now.
      Check_for_label( goto_bb );
    }
    else {
      Is_True( FALSE,
	("DCE::Add_goto_stmt: unknown DCE phase") );
    }

    if ( Tracing() ) {
      fprintf ( TFile, "<DCE> Add statement to BB:%d:\n", bb->Id() );
      new_stmt->Print( TFile );
    }
  }
}

// ====================================================================
// Replace_condbr_with_uncondbr - Replace the block's conditional branch
// with an unconditional branch (or no branch if the goto_bb block is
// the fall-through block).
// ====================================================================
void 
DCE::Replace_condbr_with_uncondbr( BB_NODE *bb, STMTREP *cond_br_stmt,
				   BB_NODE *goto_bb ) const
{
  if ( Tracing() ) {
    fprintf( TFile, "DCE::Replace_condbr_with_uncondbr: Updating bb:%d;\n",
	     bb->Id() );
    fprintf( TFile, "                      new sole successor is bb:%d.\n",
	      goto_bb->Id() );
    fflush( TFile );
  }
 
  // Remove the conditional branch from the block
  bb->Remove_stmtrep(cond_br_stmt);

  // this just becomes a goto block
  _cfg->Change_block_kind( bb, BB_GOTO );

  // We only have one successor now, so get rid of others
  BB_LIST *succlist, *nextsucc = NULL;
  for ( succlist = bb->Succ(); succlist != NULL; succlist = nextsucc ) {
    // remember the next one in case we remove this one
    nextsucc = succlist->Next();

    // see if this successor isn't the one we'll be going to
    BB_NODE *succ = succlist->Node();
    if ( succ != goto_bb ) {
      // we can get rid of the path from here to there
      Remove_path( bb, succ );

      // Update feedback
      if ( _cfg->Feedback() )
	_cfg->Feedback()->Delete_edge( bb->Id(), succ->Id() );
    }
  }

  // Decide if we need to have an unconditional branch
  Add_goto_stmt( bb, goto_bb, cond_br_stmt->Linenum(), FALSE );

  if ( Tracing() ) {
    fprintf( TFile, "DCE::Replace_condbr_with_uncondbr: Done with bb:%d\n",
	     bb->Id() );
    fflush( TFile );
  }
}

// ====================================================================
// Check_constant_cond_br - If this block has a conditional branch
// that is based upon a known condition, change it to an unconditional
// branch to the appropriate block.  Return TRUE if a change was made.
// ====================================================================

BOOL
DCE::Check_constant_cond_br( BB_NODE *bb ) const
{
  STMTREP *cond_br_stmt = NULL;
  OPERATOR cond_br_oper;

  switch ( bb->Kind() ) {
    case BB_UNKNOWN:   // invalid kind
      ErrMsg( EC_Unimplemented, 
	"Check_constant_cond_br: Unknown bb Kind()" );
      return ( FALSE );

    case BB_DOSTART:   // init block
    case BB_DOSTEP:    // increment
    case BB_DOHEAD:    // do head block
    case BB_DOTAIL:    // do tail block
    case BB_ENTRY:     // the entry bb
    case BB_EXIT:      // the exit bb
    case BB_REGIONEXIT:// the region exit bb
    case BB_GOTO:      // single target BB
    case BB_IO:        // io statement (mix of goto and vargoto)
    case BB_REGIONSTART://start of region
    case BB_REPEATBODY:// first BB in repeat body
    case BB_SUMMARY:   // summary BB
      return ( FALSE );

    case BB_WHILEEND:  // ending condition for while statement
#ifdef KEY // bug 7905: to enable while (1) to be preserved by ipl's preopt
      if (_opt_phase == PREOPT_IPA0_PHASE)
	return ( FALSE ); 
      // fall thru
#endif
    case BB_LOGIF:     // logical if
    case BB_VARGOTO:   // variable goto
    case BB_DOEND:     // ending condition
    case BB_REPEATEND: // ending condition for repeat statement
      cond_br_stmt = bb->Branch_stmtrep();
      break;

    default:
      ErrMsg( EC_Unimplemented, 
	"Check_constant_cond_br: invalid bb Kind()" );
      return ( FALSE );
  }

  Is_True( cond_br_stmt != NULL,
    ("Check_constant_cond_br: null condition branch statement BB:%d",
     (INT)bb->Id()) );

  // get the operand that represents the condition
  cond_br_oper = cond_br_stmt->Opr();

  // get the block we're going to if we find that the condition is a 
  // constant
  BB_NODE *goto_bb = NULL;
  switch ( cond_br_oper ) {
    case OPR_COMPGOTO: 
      {
	CODEREP *cg_cond = cond_br_stmt->Rhs();
	if ( cg_cond->Kind() != CK_CONST ) {
	  return ( FALSE );
	}
	else {
	  Is_True( bb->Switchinfo() != NULL,
		   ("DCE::Check_constant_cond_br: no switch info") );
	
	  if ( cg_cond->Const_val() >= 0 && 
	       cg_cond->Const_val() < bb->Switchentries() )
	    {
	      // the index lies in the range of valid cases
	      goto_bb = bb->Switchcase( cg_cond->Const_val() );
	    }
	  else {
	    // we'll go to the default case
	    if ( (goto_bb = bb->Switchdefault()) == NULL ) {
	      // we haven't been provided a default, so we can't
	      // do anything.  Essentially, this compgoto should
	      // be unreachable code.
	      return ( FALSE );
	    }
	  }
	}
	break;
      }
   
   case OPR_ZDLBR:
    return FALSE;

    case OPR_FALSEBR:
    case OPR_TRUEBR:
      {
	CODEREP *cb_cond = cond_br_stmt->Rhs();
	if ( cb_cond->Kind() != CK_CONST ) {
	  return ( FALSE );
	}
	else {
	  // we have a constant condition, so figure out if we're going
	  // to the label or to the fall-through block.
	  if ( (cb_cond->Const_val() && cond_br_oper == OPR_TRUEBR) ||
	       (!cb_cond->Const_val() && cond_br_oper == OPR_FALSEBR) )
	    {
	      // non-zero condition (truebr) or zero condition (falsebr), 
	      // which means we will unconditionally jump to the labelled 
	      // block
	      goto_bb = Branch_target_block( cond_br_stmt );
	    }
	  else {
	    // zero condition (truebr) or non-zero condition (falsebr), 
	    // which means we go to the fall-through block
	    goto_bb = bb->Next();
	  }
	}
	break;
      }

    case OPR_AGOTO:
      DevWarn( "DCE::Check_constant_cond_br: AGOTO not handled yet" );
      // note:  if the operand of this is a constant, we should be
      // able to replace this agoto with a direct goto.
      return ( FALSE );

    default:
      ErrMsg( EC_Unimplemented, 
	"Check_constant_cond_br: invalid conditional branch operator" );
      return ( FALSE );
  }

  if ( goto_bb == NULL ) {
    ErrMsg( EC_Unimplemented, 
      "DCE::Check_constant_cond_br: No goto block" );
    return ( FALSE );
  }

  // At this point, we must have had a constant condition that allowed
  // us to figure out which block we will go to.  "goto_bb" contains
  // the new target block.  Any other successors of "bb" will not be
  // successors any more.
  Replace_condbr_with_uncondbr( bb, cond_br_stmt, goto_bb );
  if ( Tracing() ) {
    fprintf( TFile, "DCE::Remove_br in bb:%d (%p)\n",bb->Id(),bb);
  }
  // if we made it this far, we must have done something
  return ( TRUE );
}

// ====================================================================
// The conditional branch needs to have its condition replaced with
// a constant.
// ====================================================================

void
DCE::Replace_condition_with_constant( BB_NODE *bb, INT64 val ) const
{
  STMTREP *condbr = bb->Branch_stmtrep();
  CODEREP *oldrhs = condbr->Rhs();

  // replacing old with new, so decrement the use of old
  oldrhs->DecUsecnt();

  // create new coderep for the value
  CODEREP *newrhs = Htable()->Add_const( oldrhs->Dtyp(), val );

  // and replace the old kid with new
  condbr->Set_rhs(newrhs);

  if ( Tracing() ) {
    fprintf( TFile, "Replaced bb:%d condition with %lld\n",
	     bb->Id(), val );
  }
}

// ====================================================================
// Check for a redundant condition by checking if there is a similar
// condition that dominates this block.
//
// The "path" is the path taken down the dominator tree intersecting
// with the origbb's iterative control dependence BBs.
// ====================================================================

BOOL
DCE::Check_redundant_cond_br_new( BB_NODE *origbb, CODEREP *origcond,
				 BB_NODE_SET *path ) const
{
  BOOL found_redundant = FALSE;

  BB_NODE *cdbb;
  BB_NODE_SET_ITER rcfg_iter;
  FOR_ALL_ELEM( cdbb, rcfg_iter, Init(path) ) {
    if ( Cond_eval( cdbb ) == EVAL_TRUE || 
	Cond_eval( cdbb ) == EVAL_FALSE )
    {
      COND_EVAL eval = Eval_redundant_cond_br( origcond,
			Cond_coderep(cdbb), Cond_eval(cdbb));

      // Replace the condition with a constant  (Use FALSE for EVAL_DEAD)
      if ( eval == EVAL_TRUE || eval == EVAL_FALSE  || eval == EVAL_DEAD ) {
        Replace_condition_with_constant( origbb, (eval == EVAL_TRUE) ? 1 : 0);
	found_redundant = TRUE;
	break;
      }
    }
    else if ( Cond_eval( cdbb ) == EVAL_UNKNOWN ) {
      // nothing to do because we don't know if this cond evals to
      // true or false
    }
    else {
      fprintf( TFile, "origbb:%d path: ", origbb->Id() );
      path->Print(TFile);
      fprintf( TFile, "\n" );
      FmtAssert( FALSE,
	("DCE::Check_redundant_cond_br_new: uninit bb:%d",cdbb->Id()) );
	found_redundant = TRUE;
	break;
    }

  }

  return found_redundant;
}

// ========================================================================
// Reset the reaching condition on the path
// ========================================================================
void
DCE::Reset_reaching_conditions( BB_NODE_SET *path ) const
{
  BB_NODE *bb;
  BB_NODE_SET_ITER cfg_iter;
  FOR_ALL_ELEM ( bb, cfg_iter, Init( path ) ) {
    if ( Cond_coderep( bb ) != NULL)
      Set_cond_eval( bb, EVAL_UNINIT );
    else 
      Set_cond_eval( bb, EVAL_UNKNOWN );
  }
}

// dom dominates all predecessors of bb except for cdbb
static BOOL
dominates_all_but_one_preds( BB_NODE *dom, BB_NODE *bb, BB_NODE *cdbb )
{
  BB_LIST_ITER pred_iter;
  BB_NODE *pred;
  FOR_ALL_ELEM( pred, pred_iter, Init( bb->Pred() ) ) {
    if ( ! dom->Dominates(pred) && pred != cdbb )
      return FALSE;
  }
  return TRUE;
}

// =========================================================================
// For each node 'cdbb' that 'bb' is iteratively control dependent upon and 
// dominated by, set the condition for 'cdbb' to reach 'bb'.
// Set iterative control dependence nodes of the initial 'bb' to 'cd_path'.
// =========================================================================
void
DCE::Compute_reaching_conditions( BB_NODE *bb, BB_NODE_SET *path ) const
{

  BB_NODE *cdbb;
  BB_NODE_SET_ITER rcfg_iter;
  BB_LIST_ITER bb_iter;

  FOR_ALL_ELEM( cdbb, rcfg_iter, Init(path) ) {
    // setup the dominating condition
    if (Cond_eval( cdbb ) != EVAL_UNKNOWN ) {

      Set_cond_eval( cdbb, EVAL_UNKNOWN );
      
      Is_True(cdbb->Dominates(bb),
	      ("DCE::Compute_reaching_conditions: cdbb does not dominate bb"));
      
      Is_True(Cond_coderep( cdbb ) != NULL, 
	      ("DCE::Compute_reaching_conditions: NULL Cond_coderep"));
      
      STMTREP *br = cdbb->Branch_stmtrep();
      Is_True(br != NULL, ("DCE::Compute_reaching_conditions: NULL branch"));

      BB_NODE *true_bb = NULL, *false_bb = NULL;
      if ( br->Opr() == OPR_TRUEBR ) {
	true_bb = Branch_target_block( br );
	false_bb = cdbb->Next();
      } else if ( br->Opr() == OPR_FALSEBR ) {
	false_bb = Branch_target_block( br );
	true_bb = cdbb->Next();
      } else {
	Is_True(0, ("DCE::Compute_reaching_conditions: unexpected branch"));
      }

      // In order to determine if the TRUE branch of block cdbb dominates
      // bb more recently than the false branch, we need to verify that:
      //   (1) cdbb strictly dominates bb, and
      //   (2) every path from false_bb to bb must include cdbb.
      // These conditions are provably equivalent to the conditions:
      //   (1) true_bb != false_bb,
      //   (2) true_bb dominates bb, and
      //   (3) true_bb dominates every predecessor of true_bb other than cdbb.
      // Similar conditions need to hold for EVAL_FALSE.

      if ( true_bb == false_bb ) {
	true_bb = NULL;
	false_bb = NULL;
      }

      if ( true_bb && true_bb->Dominates(bb) &&
           dominates_all_but_one_preds( true_bb, true_bb, cdbb ) )
        Set_cond_eval( cdbb, EVAL_TRUE );

      if ( false_bb && false_bb->Dominates(bb) &&
           dominates_all_but_one_preds( false_bb, false_bb, cdbb ) ) {
	Is_True( Cond_eval(cdbb) == EVAL_UNKNOWN,
		 ("DCE::Compute_reaching_conditions: bb most recently dominated by both true and false branches"));
        Set_cond_eval( cdbb, EVAL_FALSE );
      }
    }
  }
}

// ====================================================================
// This drives the constant/redundant conditions optimization.  It
// goes down the dominator tree, noting the path taken, and along the 
// way, it tracks whether the condition at the end of the block can be
// asserted true, false, or unknown for the particular path we're 
// following.
//
// Before returning, we check to see if the condition is redundant
// (by looking up the control-dependence path, which *should* be in
// the stack of blocks above us), or if it has a constant condition.
//
// Redundant comparisons are replaced with an appropriate constant,
// and then the constant condition code handles the rest.
// ====================================================================

BOOL
DCE::Check_conditional_branches_dom( BB_NODE *bb, BB_NODE_SET *path ) const
{
  BOOL changed_cflow = FALSE;

  if ( WOPT_Enable_DCE_Branch ) {
    STMTREP *br = bb->Branch_stmtrep();
    if ( br != NULL && ( br->Opr() == OPR_TRUEBR || br->Opr() == OPR_FALSEBR ) )
      Set_cond_coderep( bb, br->Rhs() );
    else // unknown or null branch
      Set_cond_coderep( bb, NULL );
  }

  // this block is now part of the path followed
  path->Union1D( bb );

  // go down the dominator tree
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    // check dominated blocks
    if ( Check_conditional_branches_dom(dom_bb, path) ) {
      changed_cflow = TRUE;
    }
  }

  // this block is no longer part of the path followed
  path->Difference1D( bb );

  if ( WOPT_Enable_DCE_Branch ) {
    // for sanity check, uninitialize the value for this block
    CODEREP *condition = Cond_coderep( bb );
    Set_cond_eval( bb, EVAL_UNINIT );
    Set_cond_coderep( bb, NULL );

    // don't bother with null or already-constant conditions
    if ( condition != NULL && condition->Kind() != CK_CONST ) {
      Reset_reaching_conditions( path );
      Compute_reaching_conditions( bb, path );
      Check_redundant_cond_br_new( bb, condition, path );
    }
  }

  // check this block
  if ( Check_constant_cond_br( bb ) ) {
    changed_cflow = TRUE;
  }

  return changed_cflow;
}

// ====================================================================
// Performs a localized breadth-first search backwards from block bb
// through the control flow graph, in order to determine if the
// branching condition in bb is known to always evaluate to TRUE (or
// FALSE).  If so, the branching condition is replaced by a constant.
// The search gives up after examining WOPT_Enable_DCE_Branch_Pred_Limit.
// ====================================================================

BOOL
DCE::Check_conditional_branches_pred( CFG *cfg ) const
{
  if (! WOPT_Enable_DCE_Branch || WOPT_Enable_DCE_Branch_Pred_Limit < 1)
    return FALSE;

  // Identify all branching conditions
  BB_NODE *bb;
  POBB_ITER cfg_iter(cfg);
  FOR_ALL_ELEM(bb, cfg_iter, Init()) {
    STMTREP *br = bb->Branch_stmtrep();
    if ( br != NULL && ( br->Opr() == OPR_TRUEBR || br->Opr() == OPR_FALSEBR ) ) 
      Set_cond_coderep( bb, br->Rhs() );
    else // unknown or null branch
      Set_cond_coderep( bb, NULL );
  }

  BOOL changed_cflow = FALSE;

  if (Tracing()) fprintf(TFile,"DCE::Check_conditional_branches_pred\n");

  // Iterate through branching, non-constant cfg blocks, preferably
  // visiting successors before predecessors.
  FOR_ALL_ELEM(bb, cfg_iter, Init()) {
    CODEREP *condition = Cond_coderep( bb );
    if (condition == NULL || condition->Kind() == CK_CONST ) continue;

    // eval holds the postulated constant value for bb's branching condition
    COND_EVAL eval = EVAL_UNINIT;

    // back_blocks is a list of blocks from which bb can be reached
    // without clearly establishing the value of bb's branching condition.
    // In order to maintain the forward interator current_iter,
    // back_blocks is given sufficent capacity for the entire search, and
    // new blocks are appended only to the end of back_blocks.

    vector<BB_NODE *> back_blocks(1, bb);
    back_blocks.reserve(WOPT_Enable_DCE_Branch_Pred_Limit);

    if (Tracing()) fprintf(TFile,"back_blocks: ");

    for (vector<BB_NODE *>::const_iterator current_iter = back_blocks.begin();
	 current_iter != back_blocks.end(); ++current_iter) {
      BB_NODE *current_bb = *current_iter;

      if (Tracing()) fprintf(TFile," %d", current_bb->Id());

      BB_KIND kind = current_bb->Kind();
      if (kind == BB_ENTRY || kind == BB_REGIONSTART) {
	eval = EVAL_UNKNOWN;
	break;
      }

      // iterate through all predecessors of current_bb
      BB_LIST_ITER pred_iter;
      BB_NODE *pred_bb;
      FOR_ALL_ELEM( pred_bb, pred_iter, Init( current_bb->Pred() ) ) {

	// Skip pred_bb if it is already in the examination queue.
	if ( find( back_blocks.begin(), back_blocks.end(), pred_bb )
	    != back_blocks.end() )
	  continue;

	// If pred_bb is a branch, determine if its condition restricts
	// possible values of bb condition.
	if ( Cond_coderep(pred_bb) != NULL ) {

	  // Identify the true and false branches
	  STMTREP *br = pred_bb->Branch_stmtrep();
	  Is_True(br != NULL,
		  ("DCE::Check_conditional_branches_pred: NULL branch"));
	  BB_NODE *true_bb = NULL, *false_bb = NULL;
	  if ( br->Opr() == OPR_TRUEBR ) {
	    true_bb = Branch_target_block( br );
	    false_bb =  pred_bb->Next();
	  } else if ( br->Opr() == OPR_FALSEBR ) {
	    false_bb = Branch_target_block( br );
	    true_bb = pred_bb->Next();
	  } else
	    Is_True(0, ("DCE::Check_conditional_branches_pred: unexpected branch"));

	  // If traversing the edge pred_bb --> current_bb guarantees that
	  // bb's branching condition is constant (TRUE or FALSE), then
	  // record that constant (in eval) and pass over pred_bb.

	  COND_EVAL eval_redundant = EVAL_UNINIT;
	  if ( false_bb != current_bb )
	    eval_redundant
	      = Eval_redundant_cond_br( Cond_coderep(bb),
					Cond_coderep(pred_bb), EVAL_TRUE );
	  else if ( true_bb != current_bb )
	    eval_redundant
	      = Eval_redundant_cond_br( Cond_coderep(bb),
					Cond_coderep(pred_bb), EVAL_FALSE );

	  if (eval_redundant == EVAL_DEAD) continue;
	  else if (eval_redundant == EVAL_TRUE)
	    if (eval == EVAL_FALSE) {
	      eval = EVAL_UNKNOWN;
	      break;
	    } else {
	      eval = EVAL_TRUE;
	      continue;
	    }
	  else if (eval_redundant == EVAL_FALSE)
	    if (eval == EVAL_TRUE) {
	      eval = EVAL_UNKNOWN;
	      break;
	    } else {
	      eval = EVAL_FALSE;
	      continue;
	    }
	}

        // Otherwise, add pred_bb to the list back_blocks.  If the limit is
	// reached, terminate the search.
	if (back_blocks.size() < WOPT_Enable_DCE_Branch_Pred_Limit)
	  back_blocks.push_back(pred_bb);
	else {
	  eval = EVAL_UNKNOWN;
	  break;
	}
      }
      if (eval == EVAL_UNKNOWN) break;
    }

    if (Tracing())
      switch ( eval ) {
      case EVAL_UNINIT : fprintf(TFile, " UNINIT\n");   break;
      case EVAL_TRUE   : fprintf(TFile, " TRUE\n");     break;
      case EVAL_FALSE  : fprintf(TFile, " FALSE\n");    break;
      case EVAL_UNKNOWN: fprintf(TFile, " UNKNOWN\n");  break;
      case EVAL_DEAD   : fprintf(TFile, " DEAD\n");     break;
      default          : fprintf(TFile, "\n");          break;
      }

    if (eval != EVAL_UNKNOWN) {
      // EVAL_DEAD and EVAL_UNINIT are treated as EVAL_FALSE
      Replace_condition_with_constant( bb, (eval == EVAL_TRUE) ? 1 : 0 );
      Check_constant_cond_br(bb);
      Set_cond_coderep( bb, NULL );
      changed_cflow = TRUE;
    }
  }

  // for sanity check, uninitialize the value for all blocks
  FOR_ALL_ELEM(bb, cfg_iter, Init())
    Set_cond_coderep( bb, NULL );

  return changed_cflow;
}

// ====================================================================
// Evaluate origcond assuming that evalcond evaluates to true or false
// as indicated by eval.
//
//   evalcond + eval -- is the context
//   They determine if origcond can be simplified
// ====================================================================
COND_EVAL
Eval_redundant_cond_br( CODEREP *origcond, CODEREP *evalcond, COND_EVAL eval ) 
{

  // exact match means the evalcond and origcond evaluate the same way
  if ( origcond == evalcond ) {
    return eval;
  }

  // is either condition constant?
  if (evalcond->Kind() == CK_CONST) {
    if (eval == EVAL_TRUE && !evalcond->Const_val()) return EVAL_DEAD;
    if (eval == EVAL_FALSE && evalcond->Const_val()) return EVAL_DEAD;
    if (origcond->Kind() == CK_CONST)
      return ( origcond->Const_val() ? EVAL_TRUE : EVAL_FALSE );
    return EVAL_UNKNOWN;
  }
  if (origcond->Kind() == CK_CONST)
    return ( origcond->Const_val() ? EVAL_TRUE : EVAL_FALSE );

  // are both comparisons?
  if ( origcond->Kind() == CK_OP && OPERATOR_is_compare(origcond->Opr()) &&
       evalcond->Kind() == CK_OP && OPERATOR_is_compare(evalcond->Opr()) )
  {
    CODEREP *origkid0 = origcond->Opnd(0);
    CODEREP *origkid1 = origcond->Opnd(1);
    CODEREP *evalkid0 = evalcond->Opnd(0);
    CODEREP *evalkid1 = evalcond->Opnd(1);

    // for now, pick up the easy cases
    if ( origkid0 == evalkid0 && origkid1 == evalkid1 ) {
      const OPERATOR origcomp = origcond->Opr();
      const OPERATOR evalcomp = evalcond->Opr();

      // floating point comparisons don't act very well because of
      // NaNs.  The only ones that are correct are == and !=.
      if ( MTYPE_is_float(OPCODE_desc(origcond->Op())) ) {
	if ( origcomp != OPR_EQ && origcomp != OPR_NE ) {
	  return EVAL_UNKNOWN;
	}
      }
#ifdef KEY // bug 11685
     if (origcond->Dsctyp() != evalcond->Dsctyp())
       return EVAL_UNKNOWN;
#endif

      // canonicalize so we can always assume comparison is true
      // (dealing with false is not just a matter of negating result
      //  because x>y being true implies x<y is false, but x>y being
      //  false does not imply that x<y is true because x may equal y)
      OPERATOR comparison;
      if ( eval == EVAL_FALSE ) {
	switch ( evalcomp ) {
	  case OPR_LT: comparison = OPR_GE; break;
	  case OPR_LE: comparison = OPR_GT; break;
	  case OPR_EQ: comparison = OPR_NE; break;
	  case OPR_NE: comparison = OPR_EQ; break;
	  case OPR_GE: comparison = OPR_LT; break;
	  case OPR_GT: comparison = OPR_LE; break;
	}
      }
      else {
	comparison = evalcomp;
      }

      switch ( comparison ) {
	case OPR_LT:
	  // if x<y what is x?y
	  switch ( origcomp ) {
	    case OPR_LT: return EVAL_TRUE;
	    case OPR_LE: return EVAL_TRUE;
	    case OPR_EQ: return EVAL_FALSE;
	    case OPR_NE: return EVAL_TRUE;
	    case OPR_GE: return EVAL_FALSE;
	    case OPR_GT: return EVAL_FALSE;
	  }
	break; // end OPR_LT

	case OPR_LE:
	  // if x<=y what is x?y
	  switch ( origcomp ) {
	    case OPR_LT: return EVAL_UNKNOWN;
	    case OPR_LE: return EVAL_TRUE;
	    case OPR_EQ: return EVAL_UNKNOWN;
	    case OPR_NE: return EVAL_UNKNOWN;
	    case OPR_GE: return EVAL_UNKNOWN;
	    case OPR_GT: return EVAL_FALSE;
	  }
	break; // end OPR_LE

	case OPR_EQ:
	  // if x==y what is x?y
	  switch ( origcomp ) {
	    case OPR_LT: return EVAL_FALSE;
	    case OPR_LE: return EVAL_TRUE;
	    case OPR_EQ: return EVAL_TRUE;
	    case OPR_NE: return EVAL_FALSE;
	    case OPR_GE: return EVAL_TRUE;
	    case OPR_GT: return EVAL_FALSE;
	  }
	break; // end OPR_EQ

	case OPR_NE:
	  // if x!=y what is x?y
	  switch ( origcomp ) {
	    case OPR_LT: return EVAL_UNKNOWN;
	    case OPR_LE: return EVAL_UNKNOWN;
	    case OPR_EQ: return EVAL_FALSE;
	    case OPR_NE: return EVAL_TRUE;
	    case OPR_GE: return EVAL_UNKNOWN;
	    case OPR_GT: return EVAL_UNKNOWN;
	  }
	break; // end OPR_NE

	case OPR_GE:
	  // if x>=y what is x?y
	  switch ( origcomp ) {
	    case OPR_LT: return EVAL_FALSE;
	    case OPR_LE: return EVAL_UNKNOWN;
	    case OPR_EQ: return EVAL_UNKNOWN;
	    case OPR_NE: return EVAL_UNKNOWN;
	    case OPR_GE: return EVAL_TRUE;
	    case OPR_GT: return EVAL_UNKNOWN;
	  }
	break; // end OPR_GE

	case OPR_GT:
	  // if x>y what is x?y
	  switch ( origcomp ) {
	    case OPR_LT: return EVAL_FALSE;
	    case OPR_LE: return EVAL_FALSE;
	    case OPR_EQ: return EVAL_FALSE;
	    case OPR_NE: return EVAL_TRUE;
	    case OPR_GE: return EVAL_TRUE;
	    case OPR_GT: return EVAL_TRUE;
	  }
	break; // end OPR_GT

      } // end switch EVAL_TRUEcomp
    } // end kids are equal
  }

  return EVAL_UNKNOWN;
}

// ====================================================================
// Remove_path - update the structures that note that pred can
// immediately reach succ.
// ====================================================================

void 
DCE::Remove_path( BB_NODE *pred, BB_NODE *succ ) const
{ 
  _cfg->Remove_path( pred, succ );
}

// ====================================================================
// Update_predecessor_lists - remove from the predecessor list any 
// paths that are not taken.  Update phi functions to remove values
// from removed paths.
// ====================================================================

BOOL 
DCE::Update_predecessor_lists( BB_NODE *bb ) const
{
  BB_NODE *pred;
  BB_LIST *predlist, *nextpred = NULL;
  BOOL changed_cflow = FALSE;

  if ( Tracing() ) {
    fprintf( TFile, "DCE::Update_predecessor_lists: Updating bb:%d\n",
	     bb->Id() );
    fflush( TFile );
  }

  for ( predlist = bb->Pred(); predlist != NULL; predlist = nextpred ) {
    // remember the next one in case we remove this one
    nextpred = predlist->Next();

    pred = predlist->Node();
    if ( ! pred->Reached() || ! bb->Reached() ) {
      Remove_path( pred, bb );
      changed_cflow = TRUE;

      // Update feedback
      if ( _cfg->Feedback() && _cfg->Removable_bb(bb) )
	_cfg->Feedback()->Delete_edge( pred->Id(), bb->Id() );
    }
  }

  if ( Tracing() ) {
    fprintf( TFile, "DCE::Update_predecessor_lists: Done with bb:%d\n",
	     bb->Id() );
    fprintf( TFile, "DCE::Update_predecessor_lists: changed_cflow == %d\n",
	     (INT) changed_cflow );
    fflush( TFile );
  }
  return ( changed_cflow );
}

// ====================================================================
// check this block to see if it needs a label attached to it.
// ====================================================================

void 
DCE::Check_for_label( BB_NODE *bb ) const
{
  Is_True(bb != NULL, ("DCE::Check_for_label, NULL bb"));
  STMTREP *label_stmt = bb->Label_stmtrep();
  if ( label_stmt == NULL ) {
    
    // Allocate label number
    if (bb->Labnam() == 0) {
      bb->Set_labnam( _cfg->Alloc_label());
#ifdef KEY // bug 3152
      _cfg->Append_label_map( bb->Labnam(), bb );
#endif
    }

    // Generate label STMTREP
    bb->Add_label_stmtrep( _cfg->Mem_pool() );
    label_stmt = bb->Label_stmtrep();

    if ( Tracing() )
      fprintf( TFile, "DCE::Check_for_label: Add label to BB:%d\n", bb->Id() );
  }

  Mark_statement_live( label_stmt );
}

// ====================================================================
// ====================================================================

BOOL 
DCE::Hasexception( BB_NODE *bb ) const
{
  return bb->Haspragma();
}

// ====================================================================
// If an unreachable block contains EXC_SCOPE_BEGIN or EXC_SCOPE_END
// nodes, it cannot simply be deleted, because such a node and its
// matching node may bracket reachable code.  If the block were simply
// deleted, the EH tables could be wrong.  This routine locates such
// nodes and moves them backward (via Prev()) into the nearest
// reachable code.  Since the nodes are processed from beginning to
// end in Succ() order, this guarantees that the order of the EH
// nodes is preserved.  Since they are moved only from unreachable
// nodes to the nearest preceding reachable node, they bracket the
// same executable instructions as before, and therefore the correct
// EH tables are generated.
// ====================================================================

//PPP NOTE: this routine is obsolete !!! remove after 7.2 (pete 5-7-97)

void 
DCE::Check_for_unreachable_exceptions( BB_NODE *bb ) const
{
  Is_True( Hasexception(bb),
    ("Check_for_unreachable_exceptions: called on block with no exc") );

  if (!bb->Reached()) {
    BB_NODE *bb1 = bb;

    // find nearest preceding reachable block
    
    while (!bb1->Reached())
      bb1 = bb1->Prev();

    if (bb1 != NULL) {
      STMTREP * stmt, *next_stmt = NULL;

      for (stmt = bb->First_stmtrep(); stmt; stmt = next_stmt) {
        next_stmt = stmt->Next();
        if (stmt->Opr() == OPR_EXC_SCOPE_BEGIN ||
	    stmt->Opr() == OPR_EXC_SCOPE_END) {
          if (Tracing()) {
	    fprintf(TFile, "Moving OPR_EXC_SCOPE_%s from bb:%d to bb:%d\n",
	    stmt->Opr() == OPR_EXC_SCOPE_BEGIN ? "BEGIN" : "END",
	    bb->Id(), bb1->Id());
	  }

	  // remove from original block
	  bb->Remove_stmtrep(stmt);

	  // and insert before any branch
	  STMTREP * br_stmt = bb1->Branch_stmtrep();
	  if (br_stmt == NULL) {
	    bb1->Append_stmtrep(stmt);
	  }
	  else {
	    bb1->Insert_stmtrep_before(stmt, br_stmt);
          }
	}
      }
    }
  }
}

// ====================================================================
// Remove_unreached_statements - the block is not reached, so need to
// remove each statement
// ====================================================================

void
DCE::Remove_unreached_statements( BB_NODE *bb ) const
{
  // remove all statements
  STMTREP *stmt;

  if (bb->Kind() == BB_REGIONSTART) // need to delete entire region
    Remove_region_entry(bb);
  else {
    // start from last so that use count is consistent
    while ( (stmt = bb->Last_stmtrep()) != NULL ) {
      if (stmt->Opr() == OPR_REGION_EXIT) // need to update RID structure
	Remove_region_exit(bb, FALSE);
      bb->Remove_stmtrep(stmt); // does the actual work
    }
  }

  while ( bb->Phi_list() != NULL && !bb->Phi_list()->Is_Empty() ) {
    PHI_NODE *phi = bb->Phi_list()->Remove_Headnode();
    phi->Reset_live();
  }
}

// ====================================================================
// Remove a region exit, includes updating the regioninfo and RID
// used by CFG::Change_block_kind and DCE::Remove_unreached_statements
// Assumption: all region exits are unique and have an entry in the exit list
// (see Remove_region_entry)
// Returns TRUE if it removed the pointer to the Regioninfo or if the
// region is MP. False otherwise.
// ====================================================================
BOOL Remove_region_exit(BB_NODE *bb, BOOL is_whirl)
{
  Is_True(bb != NULL && bb->Kind() == BB_REGIONEXIT,
	  ("DCE::Remove_region_exit, REGION_EXIT not inside "
	   "BB_REGION_EXIT, BB%d",bb->Id()));

  BB_REGION *bb_region = bb->Regioninfo();
  if (bb_region == NULL) // Remove_region_start already got the Regioninfo
    return TRUE;

  RID *rid = bb_region->Rid();
  // the rid is gone, we have no idea what it was, clean up
  if (rid == NULL) { // Remove_region_entry got rid of the rid
    bb_region->Set_region_end(NULL);
    bb->Set_regioninfo(NULL);
    return TRUE;
  }

  // at this point we know the regioninfo and rid are still there

  // MP and EH regions do not have exits
  if (RID_TYPE_mp(rid)) { // the kind of the exit bb is about to be reset
    bb_region->Set_region_end(NULL);
    return TRUE;
  }
  if (RID_TYPE_eh(rid))  // maintain pointer to end of EH region
    return FALSE;

  // find the label of the regionexit we are deleting
  IDTYPE label_no;
  if (is_whirl) {
    Is_True(WN_operator(bb->Laststmt()) == OPR_REGION_EXIT,
	    ("DCE::Remove_region_exit, last statement is not region exit"));
    label_no = WN_label_number(bb->Laststmt());
  } else {
    Is_True(bb->Last_stmtrep()->Opr() == OPR_REGION_EXIT,
	    ("DCE::Remove_region_exit, last statement is not region exit"));
    label_no = bb->Last_stmtrep()->Label_number();
  }

  // Now update the associated data structures if there are no other
  // region exits to this label, this is the part that does the work.
  REGION_delete_exit(rid, label_no, bb_region->Region_exit_list(), FALSE);
  Is_True(bb_region->Region_num_exits() == RID_num_exits(rid) + 1,
	  ("Remove_region_exits, miscalculated number of exits"));
  bb_region->Set_region_num_exits(rid);
  return FALSE;
}

// ====================================================================
// Remove a region entry and all the exits associated with it
// used by CFG::Change_block_kind and DCE::Remove_unreached_statements
// includes updating regioninfo and RID (see Remove_region_exit)
// ====================================================================
void Remove_region_entry(BB_NODE *bb)
{
  Is_True(bb != NULL && bb->Kind() == BB_REGIONSTART,
	  ("DCE::Remove_region_entry, something wrong with region entry, BB%d",
	   bb->Id()));
  BB_REGION *bb_region = bb->Regioninfo();
  if (bb_region == NULL)
    return;
  Is_True(bb_region->Region_start() == bb,
	  ("DCE::Remove_region_entry, could not find region entry"));

  // delete the rid from the RID tree
  RID *rid = bb_region->Rid();
  Is_True(rid != NULL, ("Remove_region_entry, NULL RID"));
  RID_Delete2(rid);

  // the region entry BB will soon be a BB_GOTO, don't point to it anymore
#ifndef KEY // bug 5714
  bb_region->Set_region_start(NULL);
  bb_region->Set_rid(NULL);
#endif
  bb->Set_regioninfo(NULL);
}

// ====================================================================
// Check if there are unreachable blocks associated with reached blocks
// that we want to keep around
// ====================================================================

void
DCE::Check_unreachable_blocks( void ) const
{
  CFG_ITER cfg_iter(_cfg);
  BB_NODE *bb;

  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    if ( bb->Reached() ) {
      switch ( bb->Kind() ) {
	case BB_DOEND:     // ending condition
	case BB_DOHEAD:    // do head block
	case BB_DOSTART:   // init block
	case BB_DOSTEP:    // increment
	case BB_DOTAIL:    // do tail block
	case BB_ENTRY:     // the entry bb
	case BB_EXIT:      // the exit bb
	case BB_REGIONEXIT:// the exit bb
	case BB_GOTO:      // single target BB
	case BB_IO:        // IO statement with control flow
	case BB_REPEATBODY:// first BB in repeat body
	case BB_REPEATEND: // ending condition for repeat statement
	case BB_SUMMARY:   // summary BB
	case BB_VARGOTO:   // variable goto
	  break;

	case BB_WHILEEND:  // ending condition for while statement
	  // check if the loop-back and merge blocks are reachable
	  if ( ! bb->Loopstep()->Reached() || 
	       ! bb->Loopmerge()->Reached() ) 
	  {
	    _cfg->Change_block_kind( bb, BB_LOGIF );
	  }
	  break;

	case BB_REGIONSTART: // start of region
        {
	  // check if the last block in the region is reachable
          // if not, shorten the region
	  BB_REGION *region = bb->Regioninfo();
	  Is_True(region != NULL,
		  ("DCE::Check_unreachable_blocks, NULL bb_region"));
	  BB_NODE *last_region_bb = region->Region_end();
	  if ( last_region_bb && ! last_region_bb->Reached() ) {
	    // need to find the last block in this region that is
	    // reached.
	    while ( ! last_region_bb->Reached() ) {
	      last_region_bb = last_region_bb->Prev();
	      // if we make it to the region start, then there is
	      // nothing in the region reachable
	      if ( last_region_bb == bb ) {
		last_region_bb = NULL;
		break;
	      }
	    }

	    if ( last_region_bb == NULL ) {
	      // none of region is reachable
	      bb->Set_regioninfo( NULL );
	      bb->Set_kind( BB_GOTO );
	    } else
	      region->Set_region_end( last_region_bb );
	  }
	}
	  break;

	case BB_LOGIF:     // logical if
	  if ( bb->Ifinfo() != NULL ) {
	    Is_True( bb->If_then()->Reached(),
	      ("IF (bb:%d) reached, but THEN (bb:%d) is not",
	       bb->Id(), bb->If_then()->Id()) );
	    Is_True( bb->If_else()->Reached(),
	      ("IF (bb:%d) reached, but ELSE (bb:%d) is not",
	       bb->Id(), bb->If_else()->Id()) );

	    // if a valid "if" is reachable, but the merge block is
	    // not, then the "if" is not well-structured, so don't
	    // treat it as such
	    if ( ! bb->If_merge()->Reached() ) {
	      // by "lowering" this to an unstructured "if" we need
	      // to possibly add a label
	      if ( !_cfg->Lower_fully() )
		Check_for_label( bb->If_then() );
	      bb->Set_ifinfo( NULL );
	    }
	  }
	  break;

	case BB_UNKNOWN:   // invalid kind
	default:
	  ErrMsg( EC_Unimplemented, 
	    "Check_unreachable_blocks: invalid bb Kind()" );
	  break;
      }

    } // end if reached

//    if ( Hasexception( bb ) ) {
//      Check_for_unreachable_exceptions( bb );
//    }
  }
}

// ====================================================================
// Unreachable_code_elim - find conditional branches with known
// conditions and make them unconditional branches.  Return TRUE if
// any changes were made to the control-flow of the program.
// ====================================================================

BOOL
DCE::Unreachable_code_elim( void ) 
{
  // say no blocks are reached initially
  CFG_ITER cfg_iter(_cfg);
  BB_NODE *bb;
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    bb->Reset_reached();
  }

  // Check the blocks for constant conditional branches, or redundant
  // conditions
  BB_NODE_SET cb_path( _cfg->Total_bb_count(), _cfg, 
		       _cfg->Loc_pool(), BBNS_EMPTY );
  BOOL changed_cflow;
  {
    changed_cflow = Check_conditional_branches_dom( _cfg->Entry_bb(), &cb_path );
    changed_cflow |= Check_conditional_branches_pred( _cfg );
  }

  changed_cflow |= Strip_try_region ();

  // find the blocks that are reached
  _cfg->Find_not_reached();

  // find the blocks that may not be reached, but we want to keep
  // them around anyways
  Check_unreachable_blocks();

  BB_NODE *nextbb = NULL;
  for ( bb = _cfg->First_bb(); bb != NULL; bb = nextbb ) {
    // remember the next bb in case we change the internal linkage
    nextbb = bb->Next();

    // update the block's predecessor list and its phi-functions
    // todo note: with the call to this function below when we delete
    // a block, this call may be unnecessary
    Warn_todo( "DCE::Unreachable_code_elim: update preds necessary?");
    changed_cflow |= Update_predecessor_lists( bb );

    // see if we need to add a label to this block
    // (don't bother if we aren't reached)
    if ( bb->Reached() ) {
      if ( May_need_label()->MemberP( bb ) ) {
	Check_for_label( bb );
      }
    }
    else {
      // found to be unreached
      if ( Tracing() ) {
	fprintf( TFile, "DCE: Removing stmts in BB%d (0x%p)\n", 
		 bb->Id(), bb );
      }

      // remove the block and then change the kind of the block
      Remove_unreached_statements( bb );

      // Bug 13821: Don't delete unreachable blocks whose labels are
      // taken and passed to __cyg_instrument_entry/exit
#if !defined(TARG_SL)
      if ( OPT_Cyg_Instrument > 0 && bb->Labnam() != 0 &&
	   LABEL_addr_saved( bb->Labnam() ) ) {
#else
      if ( 0 && bb->Labnam() != 0 &&
	   LABEL_addr_saved( bb->Labnam() ) ) {
#endif
	Keep_unreached_bb( bb );
	// Restore LABEL deleted by Remove_unreached_statements
	Check_for_label( bb );
      }

      // the only kind of block whose type we won't try to change,
      // because it's otherwise illegal, is an exit block and region exit
      if ( bb->Kind() != BB_EXIT && bb->Kind() != BB_REGIONEXIT )
	_cfg->Change_block_kind( bb, BB_GOTO );

      if ( !Keep_unreached()->MemberP(bb) && _cfg->Removable_bb(bb) ) {
	// Can't use standard iterator here because we free list nodes
	// as we go, so the iterator would refer to freed nodes.

	BB_LIST *succ_ref;
	BB_LIST *nextsucc = NULL;
	for ( succ_ref = bb->Succ();
	      succ_ref != NULL;
	      succ_ref = nextsucc ) {
	  nextsucc = succ_ref->Next();
	  changed_cflow |= Update_predecessor_lists( succ_ref->Node() );
	}
        if (bb->Pred() == NULL) {
          // bb from fake entry is removed, cfg is changed
          changed_cflow = TRUE;
        }
	    
	_cfg->Remove_bb(bb);
#ifdef Is_True_On
        if (bb->Kind() == BB_DOSTART &&
            (bb->Loop()->Flags() & LOOP_ORIGINAL_DO))
          DevWarn("DO-loop is demoted to WHILE-loop in preopt.");
#endif
	if ( Tracing() ) {
	  fprintf( TFile, "DCE: Removed BB%d\n",bb->Id());
	  fflush( TFile );
	}
      }
    }
  }

  // if we change any control-flow, assume we messed up our loop
  // structures as well
  if ( changed_cflow )
    _cfg->Invalidate_loops();

  return ( changed_cflow );
}

// ====================================================================
// ====================================================================
//
// DEAD-CODE ELIMINATION METHODS
//
// ====================================================================
// ====================================================================

// ====================================================================
// Need to find the current version of a given variable that is
// referenced in an assignment statement.  Do this by starting before 
// the given statement, and working backwards up through the given 
// block, and then its immediate dominators.  We stop when we find the
// first assignment (left-hand side, or in chi, or in phi) to the
// variable.
//
// NOTE: This is a fairly limited version that is written to support
//       the copy-propagation during DCE.
// ====================================================================

CODEREP *
DCE::Find_current_version( const STMTREP *ref_stmt, const CODEREP *cr ) const
{
  Is_True( cr->Kind() == CK_VAR,
    ("DCE::Find_current_version: non-VAR coderep") );
  Is_True( ref_stmt != NULL,
    ("DCE::Find_current_version: null ref_stmt") );

  const AUX_ID aux_id = cr->Aux_id();
  BB_NODE *ref_bb = ref_stmt->Bb();

  for ( BB_NODE *bb = ref_bb; bb != NULL; bb = bb->Idom() ) {

    // for the first block, we start at the given stmt, otherwise, we
    // start at the last stmt in the block, and work backwards
    const STMTREP *stmt;
    if ( bb != ref_bb )
      stmt = bb->Last_stmtrep();
    else
      stmt = ref_stmt->Prev();

    for ( ; stmt != NULL; stmt = stmt->Prev() ) {

      // check for chi-defs
      if (stmt->Has_chi()) {
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	FOR_ALL_NODE(cnode,chi_iter,Init(stmt->Chi_list())) {
	  if (! cnode->Dse_dead()) {
	    if ( cnode->Aux_id() == aux_id )
	      return cnode->RESULT();
	  }
	}
      }

      // check LHS for DEF of this CR
      CODEREP *lhs = stmt->Lhs();
      if ( lhs && lhs->Kind() == CK_VAR && lhs->Aux_id() == aux_id ) {
	return lhs;
      }
    
    } // stmt loop

    // check PHI node if we didn't start with a reference in a phi
    // (we guess this is the case when there was no stmt passed, and
    //  we're still handling the first block)
    PHI_LIST_ITER phi_iter;
    PHI_NODE *pnode;
    FOR_ALL_ELEM(pnode,phi_iter,Init(bb->Phi_list())) {
      if ( pnode->Aux_id() == aux_id )
        return pnode->RESULT();
    } // phi loop
  } // bb loop

  return NULL;	  // not found
}



// ====================================================================
// Required_store -  Is this direct   store statement required?
// Required_istore - Is this indirect store statement required?
// ====================================================================

BOOL
DCE::Required_store( const STMTREP *stmt, OPERATOR oper ) const
{
  Is_True ( OPERATOR_is_scalar_store (oper),
    ("DCE::Required_store: expecting STID/STBITS operator") );

  const CODEREP *lhs = stmt->Lhs();

  // store to a volatile location?
  if (lhs->Is_var_volatile())
    return ( TRUE );

#if defined(TARG_SL)
  if (CR_Intrinsic_Op_Slave(stmt->Rhs())) {
    Append_Injured_AuxIntrnOp((STMTREP *)stmt);
  }
#endif

  // statement of form i = i are not required
  //  (even it stores a dedicated register)
  if ( Enable_identity_removal() &&
       stmt->Is_identity_assignment_removable( ) ) {
    return ( FALSE );
  }

  //  The node is not connected to all of its uses, assume it is live.
  if (lhs->Is_flag_set(CF_INCOMPLETE_USES))
    return TRUE;

  // store to a dedicated pseudo-register?
  ST *s = Opt_stab()->St(lhs->Aux_id());
  if ( ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(lhs->Offset()) )
    return ( TRUE );

  // the ... part of vararg parameters needs to be always homed at PU entry
  // because we don't know if they are used
  if (_is_varargs_func && ST_sclass(s) == SCLASS_FORMAL) {
    const CODEREP *rhs = stmt->Rhs();
    // make required only if rhs is a dedicated register
    if (rhs->Kind() == CK_VAR && 
	ST_class(Opt_stab()->St(rhs->Aux_id())) == CLASS_PREG &&
	Preg_Is_Dedicated(rhs->Offset())) {
      Warn_todo("better processing of varargs in DCE.");
      return TRUE;
    }
  }

#ifdef KEY // deleting fetch of MTYPE_M return value can cause lowerer to omit
  	   // inserting the fake parm
  if (Opt_stab()->Phase() == PREOPT_IPA0_PHASE && lhs->Dsctyp() == MTYPE_M &&
      stmt->Rhs()->Kind() == CK_VAR &&
      ST_class(Opt_stab()->St(stmt->Rhs()->Aux_id())) == CLASS_PREG &&
      Preg_Is_Dedicated(stmt->Rhs()->Offset())) 
    return TRUE;
#endif

  // maybe it just isn't required
  return ( FALSE );
}

BOOL
DCE::Required_istore( const STMTREP *stmt, OPERATOR oper ) const
{
  Is_True ( ! OPERATOR_is_scalar_store (oper),
    ("DCE::Required_istore: STID/STBITS operator") );

  if (stmt->Lhs()->Is_ivar_volatile())
    return ( TRUE );

  // remove the following as part of the fix for 597561.
  // istore to restrict pt
  // All items aliased with dereferences of __restrict pointers are
  // assumed to be live because of C scoping rules. A __restrict
  // pointer can be copied to another pointer as long as the other
  // pointer isn't dereferenced in the scope of the __restrict
  // pointer. Our alias analysis says that dereferences of the two
  // pointers do not alias (because one has __restrict), but of course
  // they do. Therefore we don't want to delete stores through the
  // __restrict pointer, since they may be live out of the scope of
  // the __restrict pointer.
  if (stmt->Lhs()->Points_to(Opt_stab())->Restricted()) 
    return TRUE;

#ifdef KEY // deleting fetch of MTYPE_M return value can cause lowerer to omit
  	   // inserting the fake parm
  if (Opt_stab()->Phase() == PREOPT_IPA0_PHASE && stmt->Lhs()->Dsctyp() == MTYPE_M &&
      stmt->Rhs()->Kind() == CK_VAR &&
      ST_class(Opt_stab()->St(stmt->Rhs()->Aux_id())) == CLASS_PREG &&
      Preg_Is_Dedicated(stmt->Rhs()->Offset())) 
    return TRUE;
#endif

  return ( FALSE );
}

// ====================================================================
// Required_call - Is this call statement required?
// ====================================================================

BOOL
DCE::Required_call( const STMTREP *stmt, OPERATOR oper ) const
{
  Warn_todo( "DCE::Required_call" );
  return ( TRUE );
}

// ====================================================================
// Required_asm
// ====================================================================

BOOL
DCE::Required_asm( const STMTREP *stmt ) const
{
  if (stmt->Asm_stmt_flags() & WN_ASM_VOLATILE)
    return TRUE;
  else
    return FALSE;
}

// ====================================================================
// Required_phi - Is this phi statement required?
// ====================================================================

BOOL
DCE::Required_phi( const PHI_NODE *phi ) const
{
  if (phi->Dse_dead())
    return FALSE;
  if (phi->RESULT() != NULL && 
      (phi->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION) ||
       phi->RESULT()->Is_flag_set(CF_INCOMPLETE_USES)))
    return ( TRUE );
  return FALSE;
}

inline BOOL
DCE::Loop_pragma(WN_PRAGMA_ID pragma) const
{
  switch (pragma) {
    // the following are loop specific pragma, maybe deleted if the
    // associated loop is deleted.
    case WN_PRAGMA_AGGRESSIVE_INNER_LOOP_FISSION:
    case WN_PRAGMA_BLOCKABLE:
    case WN_PRAGMA_BLOCKING_SIZE:
    case WN_PRAGMA_CRI_CNCALL:
    case WN_PRAGMA_FISSION:
    case WN_PRAGMA_FISSIONABLE:
    case WN_PRAGMA_FUSE:
    case WN_PRAGMA_FUSEABLE:
    case WN_PRAGMA_INTERCHANGE:
    case WN_PRAGMA_IVDEP:
    case WN_PRAGMA_KAP_ASSERT_CONCURRENT_CALL:
    case WN_PRAGMA_KAP_ASSERT_DO:
    case WN_PRAGMA_KAP_ASSERT_DOPREFER:
    case WN_PRAGMA_NEXT_SCALAR:
    case WN_PRAGMA_NORECURRENCE:
    case WN_PRAGMA_NO_BLOCKING:
    case WN_PRAGMA_NO_FISSION:
    case WN_PRAGMA_NO_FUSION:
    case WN_PRAGMA_NO_INTERCHANGE:
    case WN_PRAGMA_UNROLL:
      return TRUE;
  }
  return FALSE;
}

// ====================================================================
// Required_pragma - Is this pragma something we absolutely must keep
// around.  If its block becomes live, we'll make the pragmas all live,
// but should this pragma make the block live as well?
// ====================================================================
inline BOOL
DCE::Required_pragma( const STMTREP *stmt ) const
{
  WN_PRAGMA_ID pragma = (WN_PRAGMA_ID)WN_pragma(stmt->Orig_wn());
#ifdef KEY
  if (pragma == WN_PRAGMA_UNROLL) return TRUE;
#endif
  if (Loop_pragma(pragma)) return FALSE;

  switch ( pragma ) {
    case WN_PRAGMA_INLINE_BODY_START:
    case WN_PRAGMA_INLINE_BODY_END:
      return FALSE;
  }

  return TRUE;
}

// ====================================================================
// Required_stmt - Is this statement required?
// ====================================================================

BOOL
DCE::Required_stmt( const STMTREP *stmt ) const
{
  const OPERATOR opr = stmt->Opr();

  // does this statement have any volatile references?
  if ( stmt->Volatile_stmt() )
    return TRUE;

  if (OPERATOR_is_scalar_store (opr) &&
      Enable_identity_removal() &&
      stmt->Is_identity_assignment_removable()) // if COPYPROP assumes the stmt is deleted)
    return FALSE;

  // statements with zero-version chi nodes are required
  if ( WOPT_Enable_Zero_Version && stmt->Has_zver())
    return TRUE;

#ifdef KEY // bugs 5401 and 5267
  if (_opt_phase != MAINOPT_PHASE &&
      OPERATOR_is_scalar_store(opr) && 
      Opt_stab()->Aux_stab_entry(stmt->Lhs()->Aux_id())->Mp_no_dse())
    return TRUE;
#endif

  CHI_LIST_ITER  chi_iter;
  CHI_NODE      *cnode;
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    // mark statements that has incomplete uses to be live
    if (!cnode->Dse_dead() &&
	cnode->RESULT() != NULL &&
	cnode->RESULT()->Is_flag_set(CF_INCOMPLETE_USES))
      return TRUE;

    // Fix 621039: mark statement affecting a volatile accesses to be live
    if (Opt_stab()->Aux_stab_entry(cnode->Aux_id())->Is_volatile())
      return TRUE;
  }

  // handle those cases we immediately know about
  switch ( opr ) {
  case OPR_AGOTO:
  case OPR_TRUEBR:
  case OPR_FALSEBR:
  case OPR_GOTO:
  case OPR_ZDLBR:
    return ( !Enable_aggressive_dce() );

  case OPR_COMPGOTO:
#ifdef KEY 
    if ((PU_has_mp(Get_Current_PU()) || PU_mp_lower_generated(Get_Current_PU()))
	&& Early_MP_Processing)
      return TRUE; // deleting COMPGOTO related to SECTIONS caused bug 5553
#endif
    return ( !Enable_aggressive_dce() );

  case OPR_LABEL:
    return ( !Enable_aggressive_dce() ||
	     LABEL_addr_saved( stmt->Label_number() ) );

  case OPR_PRAGMA:
  case OPR_XPRAGMA:
    return ( Required_pragma(stmt) );

  case OPR_ASSERT:
  case OPR_BACKWARD_BARRIER:
  case OPR_FORWARD_BARRIER:
  case OPR_DEALLOCA:
  case OPR_EXC_SCOPE_BEGIN:
  case OPR_EXC_SCOPE_END:
  case OPR_IO:
  case OPR_PREFETCH:
  case OPR_PREFETCHX:
  case OPR_REGION:
  case OPR_RETURN:
  case OPR_RETURN_VAL:
  case OPR_REGION_EXIT:
  case OPR_OPT_CHI: // entry chi is required, pv 454154
#ifdef KEY
  case OPR_GOTO_OUTER_BLOCK:
#endif
    return TRUE;

  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
    return ( Required_call( stmt, opr ) );

  case OPR_STID:
  case OPR_STBITS:
    return ( Required_store( stmt, opr ) );

  case OPR_MSTORE:
  case OPR_ISTORE:
  case OPR_ISTBITS:
  case OPR_ISTOREX:
    return ( Required_istore( stmt, opr ) );

  case OPR_ALTENTRY:
  case OPR_FUNC_ENTRY:
    FmtAssert(FALSE, ("DCE::Required_stmt: Unexpected OPR\n"));
    break;

  case OPR_ASM_STMT:
    return ( Required_asm( stmt ));

  default:
    break;
  }

  return FALSE;
}

// ====================================================================
// Required_bb - Is this BB required?
// ====================================================================

BOOL
DCE::Required_bb( const BB_NODE *bb ) const
{
  if ( bb->Kind() == BB_REGIONSTART ) {
    // can never get rid of EH Guard regions
    BB_REGION *bb_region = bb->Regioninfo();
    Is_True(bb_region != NULL, ("DCE::Required_bb, NULL bb_region"));
    Is_True(bb_region->Rid() != NULL, ("DCE::Required_bb, NULL RID"));
    if ( RID_TYPE_guard(bb_region->Rid()) )
      return TRUE;
  }
  return FALSE;
}

// ====================================================================
// Dce_prop: limited form of copy propagation in DCE
// ====================================================================
CODEREP *
DCE::Dce_prop(CODEREP *opnd) const
{
  if (!Allow_dce_prop()) return NULL;
  
  CODEREP *newopnd = opnd;

  // while to skip multiple redundant assignments.
  // e.g.  the virtual variable defined by a i=i;j=j in the loop-exit 
  //       block.
  while ( newopnd != NULL ) {
    // Test the definition of the opnd is copy-propagatable.
    if (newopnd->Is_flag_set(CF_DEF_BY_PHI)) break;
    if (newopnd->Defstmt() == NULL) break;

    // Test if the defstmt is a redundant assignment stateement
    CODEREP *lhs = newopnd->Defstmt()->Lhs();
    if (lhs == NULL) break;   // e.g. it is a call statement
    if (lhs->Kind() != CK_VAR) break;

    CODEREP *rhs = newopnd->Defstmt()->Rhs();
    if (rhs->Kind() != CK_VAR) break;
    if (rhs->Aux_id() != lhs->Aux_id()) break;

    // load of a volatile location?
    if (rhs->Is_var_volatile()) break;

    // make sure we're assigning the current version
    if ( Find_current_version(newopnd->Defstmt(), rhs) != rhs ) break;

    // we have an assignment:  i = i   but if the coderep in question
    // is defined by a chi associated with this assignment, we need
    // to follow the chi's rhs.  We can do this because we've decided
    // that the assignment has no effect, so the chi has no effect.
    if ( ! newopnd->Is_flag_set(CF_DEF_BY_CHI) )
      newopnd = rhs;
    else
      newopnd = newopnd->Defchi()->OPND();

    if ( Tracing() ) {
       fprintf( TFile, "DCE::Dce_prop: revising mu/chi CODEREP from\n" );
       opnd->Print( 0, TFile );
       fprintf( TFile, "  to\n" );
       newopnd->Print( 0, TFile );
    }
  }

  return (newopnd == opnd) ? NULL : newopnd;
}

// ====================================================================
// Mark_phinode_live - Mark this phi-node live as required and mark
// (recursively) any statements that become required because this one
// is.
// ====================================================================

void
DCE::Mark_phinode_live( PHI_NODE *phi, BOOL visit_opnds ) const
{
  if ( phi->Live() )
    return;

  phi->Set_live();

  // and the block in which this phi resides is required
  if ( ! phi->Bb()->Reached() ) {
    Mark_block_live( phi->Bb() );
  }

  // now process the phi's operands
  if ( visit_opnds ) {
    BOOL ignore_pregs = Opt_stab()->Is_virtual(phi->RESULT()->Aux_id());
    for ( INT pkid = 0; pkid < phi->Size(); pkid++ ) {
      CODEREP *cr;
      if ((cr = Dce_prop(phi->OPND(pkid))) != NULL) {
	phi->Set_opnd(pkid,cr);
        cr->Set_flag(CF_DONT_PROP);
      } 
      else cr = phi->OPND(pkid);

      STMTREP *proj_defstmt = Proj_defstmt(cr, Opt_stab());
      if (proj_defstmt != NULL) {
	// Increment twice so projections can never be combined with
	// the RHS of cr->Defstmt(), even if the the RHS is a
	// projectable operation.
	proj_defstmt->Inc_proj_op_uses(2);
      }
      // PREGs represented by CK_VAR codereps that are used as operands
      // of live phi nodes are not to be renumbered.
      cr->Reset_safe_to_renumber_preg();
      Mark_coderep_live(cr);
    }
  }
}

// ====================================================================
// Hook to alias analysis
// ====================================================================

BOOL
DCE::Aliased( const POINTS_TO *pt1, const POINTS_TO *pt2 ) const
{
  if ( pt1 == NULL || pt2 == NULL )
    // assume the worst
    return TRUE;

  return ( _alias_rule->Aliased_Memop( pt1, pt2 ) );
}

// ====================================================================
// Mark_chinode_live - Mark this chi-node live as required and mark
// (recursively) any statements that become required because this one
// is.
// ====================================================================

void
DCE::Mark_chinode_live( CHI_NODE *chi, STMTREP *def_stmt ) const
{
  if (chi->Live() || chi->Dse_dead()) return;

  // do not process the return vsym as an opnd during the regular pass
  if ( Enable_dce_global() &&
       chi->OPND()->Aux_id() == Return_vsym() )
    return;
 

  CODEREP *cr = Dce_prop(chi->OPND());
  if (cr != NULL) {
    AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(cr->Aux_id());
    Is_True(!aux_entry->Is_non_dedicated_preg(),
	    ("DCE: mu/chi of non-dedicated PREG not allowed"));
    // The following is for safety in NODEBUG compilers where the
    // above Is_True would fail if it were switched on.
    if (aux_entry->Is_non_dedicated_preg()) {
      cr->Reset_safe_to_renumber_preg();
    }
    chi->Set_OPND(cr);
    Mark_chinode_live(chi, def_stmt);
  } else {
    AUX_STAB_ENTRY *aux_entry =
      Opt_stab()->Aux_stab_entry(chi->OPND()->Aux_id());
    Is_True((!aux_entry->Is_non_dedicated_preg()) ||
	    (def_stmt->Opr() == OPR_OPT_CHI) ||
	    ((def_stmt->Opr() == OPR_REGION) &&
	     (def_stmt->Black_box())),
	    ("DCE: mu/chi of non-dedicated PREG not allowed"));
    // The following is for safety in NODEBUG compilers where the
    // above Is_True would fail if it were switched on, and for those
    // few instances of entry/region chi that come through here.
    if (aux_entry->Is_non_dedicated_preg()) {
      chi->OPND()->Reset_safe_to_renumber_preg();
    }
    chi->Set_live(TRUE);
    Mark_coderep_live( chi->OPND() );
  }
}

// ====================================================================
// This statement is marked with "Has_zver" meaning it has some chi-
// node that is a zero-version.  Find those, and mark them live, which
// in turn marks the statement live.
// ====================================================================

void
DCE::Mark_zero_version_chinode_live( STMTREP *stmt ) const
{
  Is_True( stmt->Has_zver(),
    ("DCE::Mark_zero_version_chinode_live: stmt does not have zver") );

  BOOL made_chi_live = FALSE;
  CHI_LIST_ITER chi_iter;
  CHI_NODE *chi;
  CHI_LIST *chi_list = stmt->Chi_list();
  FOR_ALL_NODE( chi, chi_iter, Init(chi_list)) {
    if ( ! chi->Dse_dead() ) {
      CODEREP *res = chi->RESULT();
      if ( res->Is_flag_set(CF_IS_ZERO_VERSION) ) {
	Mark_chinode_live(chi, stmt);
	made_chi_live = TRUE;
      }
    }
  }

  if ( made_chi_live ) {
    if ( ! stmt->Live_stmt()) {
      Mark_statement_live( stmt );
    }
  }
  else {
    FmtAssert( FALSE,
      ("DCE::Mark_zero_version_chinode_live: no zero-version chi") );
  }
}

// ====================================================================
// Mark_cr_munode_live - Mark this mu-node live as required and mark
// (recursively) any statements that become required because this one
// is.
// ====================================================================

void
DCE::Mark_cr_munode_live( CODEREP *cr ) const
{
  MU_NODE *mu;

  if ( Enable_dce_alias() && cr->Ivar_mu_node() != NULL ) {
    Points_to_stack()->Push( cr->Points_to(Opt_stab()) );
  }

  mu = cr->Ivar_mu_node();

  if (mu) {

    // handle return vsym specially
    if ( Enable_dce_global() &&
	 mu->OPND()->Aux_id() == Return_vsym() ) {
      Mark_return_vsym_mu_ref_live( mu->OPND() );
    } else {

      if ( Enable_dce_alias() ) {
	Mu_stack()->Push( mu );
      }
    
      if (mu->OPND()->Defstmt() == NULL) {
	// mu's may not have a def (such virtual var.)
	if (mu->OPND()->Is_flag_set(CF_DEF_BY_PHI)) {
	  if ( !(mu->OPND()->Defphi()->Live()) )
	    Mark_coderep_live( mu->OPND() );
	}
      }
      else if (mu->OPND()->Is_flag_set(CF_DEF_BY_CHI)) {
	Mark_coderep_live(mu->OPND());
      }
      else {
	if (!(mu->OPND()->Defstmt()->Live_stmt())) {
	  CODEREP *cr;
	  if ((cr = Dce_prop(mu->OPND())) != NULL) {
	    mu->Set_OPND(cr);
	    Mark_coderep_live(cr);
	  } else
	    Mark_coderep_live( mu->OPND() );
	}
      }

      if ( Enable_dce_alias() ) {
	Mu_stack()->Pop();
      }
    }
  }

  if ( Enable_dce_alias() && cr->Ivar_mu_node() != NULL ) {
    Points_to_stack()->Pop();
  }

}

// ====================================================================
// Mark_sr_munode_live - Mark this mu-node live as required and mark
// (recursively) any statements that become required because this one
// is.
// ====================================================================

void
DCE::Mark_sr_munode_live( STMTREP *sr ) const
{
  MU_NODE *mu;
  
  if ( Enable_dce_alias() && sr->Mu_list() != NULL ) {
    Points_to_stack()->Push( sr->Points_to(Opt_stab()) );
  }

  mu = (sr->Mu_list() ? sr->Mu_list()->Head() : NULL);
  while (mu != NULL) {
    // handle return vsym specially
    if ( Enable_dce_global() && 
	 mu->OPND()->Aux_id() == Return_vsym() )
    {
      Mark_return_vsym_mu_ref_live( mu->OPND() );
      mu = mu->Next();
      continue;
    }

    if ( Enable_dce_alias() ) {
      Mu_stack()->Push( mu );
    }

    CODEREP *cr;
    if ((cr = Dce_prop(mu->OPND())) != NULL) {
      mu->Set_OPND(cr);
      Mark_coderep_live(cr);
    } else
      Mark_coderep_live( mu->OPND() );

    if ( Enable_dce_alias() ) {
      Mu_stack()->Pop();
    }
    mu = mu->Next();
  }

  if ( Enable_dce_alias() && sr->Mu_list() != NULL ) {
    Points_to_stack()->Pop();
  }
}

// ====================================================================
// Mark_coderep_live - Mark this coderep live as required and mark
// (recursively) any statements that become required because this one
// is.
// ====================================================================

void
DCE::Mark_coderep_live( CODEREP *cr ) const
{
  // which kinds do not have definitions
  if ( ! inCODEKIND(cr->Kind(), CK_LDA|CK_CONST|CK_RCONST) ) {

    // phi-statement defines this?
    if ( cr->Is_flag_set(CF_DEF_BY_PHI) ) {
      if ( ! cr->Defphi()->Live() ) {
	Mark_phinode_live( cr->Defphi(), TRUE/*visit opnds*/ );
      }
    } 
    else {
      switch ( cr->Kind() ) {
      case CK_VAR:
	// Don't process Return_vsym() here.  Any of its references
	// in mu statements will trigger other code to handle it.
	if ( Enable_dce_global() && 
	     cr->Aux_id() == Return_vsym() )
	{
	  Is_True( FALSE, 
	    ("Should not see Return_vsym() here") );
	  return;
	}

	// Mark the chi node to be live if it is defined by a chi node.
	// Notice that we do not reach the defining statement from the 
	// CHI node.
	// Need to mark the defining statement live later
	if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
	  Mark_chinode_live( cr->Defchi(), cr->Defstmt() );
	}

	// if there is a defining statement, and it's not already live
	// NOTE: May at some time always have a defining statement, so
	//       we could remove the check for a null defstmt
	//	   (i.e., if live-in values get some dummy def stmt)
	if ( cr->Defstmt() != NULL && !cr->Defstmt()->Live_stmt() ) {
	  // if we're making this statement live just because it's
	  // associated with a chi, see if we can get rid of "i = i"
	  // assignments.  This only happens when zero-version is
	  // enabled because we won't otherwise make the chi live
	  // by default.
	  BOOL make_live = TRUE;
	  if ( cr->Is_flag_set(CF_DEF_BY_CHI) &&
	       WOPT_Enable_Zero_Version )
	    {
	      const STMTREP *defstmt = cr->Defstmt();
	      const OPERATOR defoper = defstmt->Opr();
	      if (OPERATOR_is_scalar_store (defoper) &&
		   Enable_identity_removal() &&
		  defstmt->Is_identity_assignment_removable() )
		make_live = FALSE;
	    }

	  if ( make_live )
	    Mark_statement_live( cr->Defstmt() );
	}
	break;
	
      case CK_IVAR:
	// handle the base
	if ( cr->Istr_base() != NULL ) 
	  Mark_coderep_live( cr->Istr_base() );
	else
          Mark_coderep_live( cr->Ilod_base() );
	
	// Is there a size also?
	if ( cr->Opr() == OPR_MLOAD ) {
	  Mark_coderep_live( cr->Mload_size() );
	}
	else if ( cr->Opr() == OPR_ILOADX ) {
	  Mark_coderep_live( cr->Index() );
	}

	if ( cr->Opr() == OPR_PARM ) {
	  // anything to do?  Already handled the base
	}
	else {
	  // Follow the prefetch info
	  PF_POINTER *pf = cr->Ivar_occ()->Pf_pointer();
	  if (pf != NULL) {
	    if (PF_PTR_wn_pref_1L(pf) != NULL)
	      Mark_statement_live((STMTREP *) PF_PTR_wn_pref_1L(pf));
	    if (PF_PTR_wn_pref_2L(pf) != NULL)
	      Mark_statement_live((STMTREP *) PF_PTR_wn_pref_2L(pf));
	  }
	}
	  
	// if there is a defining statement, and it's not already live
	// NOTE: May at some time always have a defining statement, so
	//       we could remove the check for a null defstmt
	if (cr->Ivar_defstmt() != NULL && !cr->Ivar_defstmt()->Live_stmt()) {
	  Mark_statement_live( cr->Ivar_defstmt() );
	}
	Mark_cr_munode_live( cr );
	break;
	
      case CK_OP:
	{
	  if ( Is_dce_visited(cr) )
	    return;
	  ((DCE *) this)->Set_dce_visited( cr );

	  if (Projection_operation(cr->Opr())) {
	    CODEREP *opnd     = cr->Opnd(0);

	    if (opnd->Kind() == CK_VAR) {
	      // Operand of the projection is a variable. If it has a
	      // real definition, mark the definition as used one more
	      // time. Note that Proj_op_uses() is examined only in the
	      // emitter, and only when the STID statement stores to an
	      // EPRE temporary.

	      STMTREP *opnd_def = Proj_defstmt(opnd, Opt_stab());
	      if (opnd_def != NULL) {
		Is_True(Projectable_operation(opnd_def->Rhs()),
			("Definition of projected var must be projectable"));
		opnd_def->Inc_proj_op_uses();
	      }
	      else {
		// This operation will be ineligible for recombination
		// anyway because one of the following holds:
		//   its operand is defined by phi;
		//   its operand is defined by other than an STID to the
		//     operand;
		//   its operand is defined by an STID of a
		//     non-projectable expression;
	      }
	    }
	    else {
	      // This projection operation will be unconditionally
	      // (re)combined by the emitter if it can be, and will be
	      // left alone if the child of the projection is not
	      // projectable (e.g. REALPART(complex multiply) or
	      // DIVPART(CK_IVAR)).
	    }
	  }

	  for ( INT32 ikid = 0; ikid < cr->Kid_count(); ikid++ ) {
	    Mark_coderep_live( cr->Opnd(ikid) );
	  }
	}
	break;
	
      default:
	FmtAssert( FALSE, ("DCE::Mark_coderep_live: invalid kind 0x%x", cr->Kind()) );
	break;
      }

    }
  }
}


// ====================================================================
// Determine if it is certain that the conditional branch statement
// is part of a high-level statement, and a label is not necessary
// for the target of the branch
// ====================================================================

BOOL
DCE::Need_condbr_target_label( STMTREP *stmt, BB_NODE *target ) const
{
  Is_True( stmt->Opr() == OPR_TRUEBR || stmt->Opr() == OPR_FALSEBR,
    ("DCE::Need_condbr_target_label: invalid branch") );
  // should not call this unless we're in preopt
  Is_True( ! _cfg->Lower_fully(),
    ("DCE::Need_condbr_target_label: lowered fully") );

  BB_NODE *stmt_bb = stmt->Bb();
  switch ( stmt_bb->Kind() ) {
    case BB_LOGIF:
      if ( stmt_bb->Ifinfo() != NULL ) {
	// the only thing needed for a high-level IF is the Ifinfo,
	// since the branch is already being made live
	return FALSE;
      }
      break;

    case BB_REPEATEND:
      if ( stmt_bb->Loop() != NULL ) {
	// the only thing needed for a high-level do-while loop  is
	// to make sure the target of the branch is a repeatbody bb
	if ( stmt_bb->Loopbody() == target && 
	     target->Kind() == BB_REPEATBODY )
	{
	  return FALSE;
	}
      }
      break;
  }

  return TRUE;
}

// ====================================================================
// Mark_branch_related_live - We're marking this branch statement live
// so see if there are related statements that must also be made live.
// ====================================================================

void
DCE::Mark_branch_related_live( STMTREP *stmt ) const
{
  BB_NODE *br_bb = Branch_target_block( stmt );
  BOOL check_target_label = TRUE;

  if ( ! _cfg->Lower_fully() ) {
    // we may need to raise back to high-level constructs, so see if
    // we really need to have a label at the target of the branch.
    if ( ! Need_condbr_target_label( stmt, br_bb ) ) {
      check_target_label = FALSE;
    }
  }

  if ( check_target_label ) {
    Check_for_label( br_bb );
  }

  // also the fall-thru block is required
  BB_NODE *fall_thru = stmt->Bb()->Next();
  if ( ! fall_thru->Reached() )
    Mark_block_live( fall_thru );

  switch ( stmt->Bb()->Kind() ) {
  case BB_DOEND:
  case BB_REPEATEND:
  case BB_WHILEEND:  
    {
      // make the trip-count eval statement live, if there is one
      BB_LOOP *loop = stmt->Bb()->Loop();
      if ( loop != NULL ) {
	if ( _cfg->Lower_fully() ) {
	  // mark any EVALs in the dohead block as required.  These
	  // are inserted by copy-prop to allow values to be moved
	  // back out during PRE
	  // NOTE: this also picks up the Trip_count_stmt().
	  BB_NODE *dohead = loop->Start();
	  if ( (dohead &&  dohead->Kind() == BB_DOHEAD &&
                (loop->Is_flag_set(LOOP_DO) || 
                 loop->Is_flag_set(LOOP_WHILE) || 
                 loop->Is_flag_set(LOOP_REPEAT)))
             ||
               (dohead && loop->Is_flag_set(LOOP_PRE_DO) && dohead->Kind() == BB_DOSTART)) {
	    STMTREP_ITER stmt_iter(dohead->Stmtlist());
	    STMTREP *dohead_stmt;
	    FOR_ALL_NODE( dohead_stmt, stmt_iter, Init() ) {
	      if ( !dohead_stmt->Live_stmt() &&
		   dohead_stmt->Opr() == OPR_EVAL )
	      {
		Mark_statement_live( dohead_stmt );
	      }
	    }
	  }
	}
      }
    }
    break;
  }
}

// ====================================================================
// Mark_region_exits_live - Given a black box region, mark the labels
// that it branches to live.
// ====================================================================

void
DCE::Mark_region_exits_live( STMTREP *stmt ) const
{
  RID *rid = REGION_get_rid(stmt->Black_box_wn());
  WN *wn_exit_list = WN_region_exits(RID_rwn(rid));
  for (WN *wtmp=WN_first(wn_exit_list); wtmp; wtmp=WN_next(wtmp)) {
    INT32 label_num = WN_label_number(wtmp);
    BB_NODE *bb = _cfg->Get_bb_from_label(label_num);
    Is_True(bb != NULL, ("DCE::Mark_region_exits_live, need BB for label"));
    Mark_block_live(bb);
    if (Tracing())
      fprintf(TFile,"Mark_region_exits_live, marking label %d for RGN%d\n",
	      label_num, RID_id(rid));
  }
}

// ====================================================================
// Process a chi definition of the return virtual variable, processing
// a particular variable it is aliased with.
//
// This relies somewhat on there being chi nodes for the real vars
// attached to the statement.  We look for the real variable amongst
// the list of chi nodes, and if we find it, we continue up the use-
// def chain for that variable regularly.
// ====================================================================

void 
DCE::Mark_return_vsym_chi_live( CHI_NODE *chi ) const
{
  if ( chi->Live() )
    return;

  CODEREP *res = chi->RESULT();
  STMTREP *stmt= res->Defstmt();
  const OPERATOR opr = stmt->Opr();

  // assume this is a required statement
  BOOL make_live = TRUE;

  if ( ! stmt->Live_stmt() ) {

    if (OPERATOR_is_scalar_store (opr)) {
      AUX_ID aux_id = stmt->Lhs()->Aux_id();

      // is this a store to the variable that's still required?
      if ( Return_vsym_reqd_set()->MemberP( aux_id ) ) {
	// say this variable is no longer required as we go up use-def
	Return_vsym_reqd_set()->Difference1D( aux_id );
      }
      else if ( Return_vsym_full_set()->MemberP( aux_id ) ) {
	// this is a store to another aliased variable, but it's 
	// apparently not required, so we don't need to make it live.
	make_live = FALSE;

	if ( Tracing() ) {
	  fprintf(TFile, "Mark_return_vsym_chi_live: skip stid def:");
	  stmt->Lhs()->Print(0,TFile);
	}
      }
    }
  }
  
  if ( make_live ) {
    // say this chi is live, but don't call Mark_chinode_live because 
    // we'll handle the operand down below.  Also, this keeps us from
    // getting in a loop
    chi->Set_live(TRUE);

    if ( ! stmt->Live_stmt() ) {
      Mark_statement_live(stmt);
    }
  }

  // no matter what we've done about the statement, say we've
  // processed it for the return vsym
  stmt->Set_dce_retvsym();

  // continue up the use-def chain
  CODEREP *chiopnd = chi->OPND();
  if ( chiopnd->Is_flag_set(CF_DEF_BY_PHI) ) {
    Mark_return_vsym_phi_live( chiopnd->Defphi() );
  }
  else if ( chiopnd->Is_flag_set(CF_DEF_BY_CHI) ) {
    Mark_return_vsym_chi_live( chiopnd->Defchi() );
  }
  else {
    // virtual syms can only be defined by phi,chi, or nothing
    Is_True( chiopnd->Is_var_nodef(),
      ("DCE::Mark_return_vsym_chi_live: vsym has real def?") );
  }

}

// ====================================================================
// Process a reference to the return vsym by treating it as a reference
// to each of the scalars it is aliased to
// ====================================================================

void 
DCE::Mark_return_vsym_mu_ref_live( CODEREP *cr ) const
{
  Is_True( cr->Aux_id() == Return_vsym(),
    ("DCE::Mark_return_vsym_mu_ref_live: not return vsym") );

  // say everything is now required
  Return_vsym_reqd_set()->CopyD( Return_vsym_full_set() );

  if ( cr->Is_flag_set(CF_DEF_BY_PHI) ) {
    Mark_return_vsym_phi_live( cr->Defphi() );
  }
  else if ( cr->Is_flag_set(CF_DEF_BY_CHI) ) {
    Mark_return_vsym_chi_live( cr->Defchi() );
  }
  else {
    // virtual syms can only be defined by phi,chi, or nothing
    Is_True( cr->Is_var_nodef(),
      ("DCE::Mark_return_vsym_mu_ref_live: vsym has real def?") );
  }

}

// ====================================================================
// Process this phi node for the return vsym
// ====================================================================

void 
DCE::Mark_return_vsym_phi_live( PHI_NODE *phi ) const
{
  // have we already visited this phi
  if ( phi->Live() )
    return;

  // say we've visited this phi now.  Unfortunate side effect is that
  // we must always keep phi's of this vsym around.
  Mark_phinode_live( phi, FALSE/*do not visit operands*/ );

  // save a copy of the required set at this point
  // This is wasteful of memory, but shouldn't be too bad
  IDX_32_SET *saved_reqd = Return_vsym_reqd_set()->Copy(_cfg->Loc_pool());
  BOOL need_copy = FALSE;

  for ( INT pkid = 0; pkid < phi->Size(); pkid++ ) {
    // do we need to restore our reqd set?
    if ( need_copy ) 
      Return_vsym_reqd_set()->CopyD( saved_reqd );

    CODEREP *cr = phi->OPND(pkid);
    if ( cr->Is_flag_set(CF_DEF_BY_PHI) ) {
      Mark_return_vsym_phi_live( cr->Defphi() );
    }
    else if ( cr->Is_flag_set(CF_DEF_BY_CHI) ) {
      Mark_return_vsym_chi_live( cr->Defchi() );
    }
    else {
      // virtual syms can only be defined by phi,chi, or nothing
      Is_True( cr->Is_var_nodef(),
	("DCE::Mark_return_vsym_phi_live: vsym has real def?") );
    }

    // next time through this loop, we will need to copy the reqd set
    need_copy = TRUE;
  }
}

// ====================================================================
// Find the last id that the return vsym aliases with.
// ====================================================================

void
DCE::Init_return_vsym( void )
{
  AUX_ID max_id = 0;

  if ( Enable_dce_global() ) {
    IDX_32_SET *vsym_set = CXX_NEW( IDX_32_SET(Opt_stab()->Lastidx()+1, 
			_cfg->Loc_pool(),OPTS_FALSE), _cfg->Loc_pool());
    _return_vsym_reqd_set = CXX_NEW( IDX_32_SET(Opt_stab()->Lastidx()+1,
			_cfg->Loc_pool(),OPTS_FALSE), _cfg->Loc_pool());

    AUX_ID_LIST_ITER id_list_iter;
    AUX_ID_NODE *id_node;
    FOR_ALL_ELEM( id_node, id_list_iter, 
		  Init(Opt_stab()->Aux_id_list(Return_vsym())) )
    {
      AUX_ID id = id_node->Aux_id();

      vsym_set->Union1D( id );
      if ( id > max_id )
        max_id = id;
    }

    _return_vsym_full_set = vsym_set;
  }

  _return_vsym_max = max_id+1;
}

// ====================================================================
// Return the coderep that defines the given cr.
// ====================================================================

CODEREP *
DCE::Prop_return_vsym_new_result( CODEREP *cr ) const
{
  if ( cr->Is_flag_set(CF_DEF_BY_PHI) ) {
    // we assume that the result is correct
    return cr->Defphi()->RESULT();
  }
  else if ( cr->Is_flag_set(CF_DEF_BY_CHI) ) {
    // is this statement live?
    if ( cr->Defstmt()->Live_stmt() ) {
      return cr->Defchi()->RESULT();
    }
    else {
      // dead statements skip over this, so return the chi operand
      // which should have been updated to the correct value by now
//Bug 2659
# ifdef KEY
      if (!cr->Defchi()->Live())
        return Prop_return_vsym_new_result(cr->Defchi()->OPND());
      else
        return cr->Defchi()->OPND();
# else
      return cr->Defchi()->OPND();
# endif
    }
  }
  else {
    // virtual syms can only be defined by phi,chi, or nothing
    Is_True( cr->Is_var_nodef(),
      ("DCE::Prop_return_vsym_new_result: vsym has real def?") );
    return cr;
  }
}

// ====================================================================
// After we've deleted some globals that had chis with a virtual
// variable in them, we need to go back and fix up the use-def chain
// to be correct.
//
// We only need to handle the Return_vsym() virtual variable referenced
// in mu nodes.
// ====================================================================

void
DCE::Propagate_return_vsym_cr( CODEREP *cr ) const
{
  switch ( cr->Kind() ) {
    case CK_LDA:
    case CK_CONST:
    case CK_RCONST:
    case CK_VAR:
      // none of these have mu references, so no need to do anything
      break;
    
    case CK_IVAR:
      {
	if ( cr->Istr_base() != NULL ) 
	  Propagate_return_vsym_cr( cr->Istr_base() );
	else
	  Propagate_return_vsym_cr( cr->Ilod_base() );

	if ( cr->Opr() == OPR_MLOAD )
	  Propagate_return_vsym_cr( cr->Mload_size() );
	else if ( cr->Opr() == OPR_ILOADX )
	  Propagate_return_vsym_cr( cr->Index() );

	MU_NODE *mu = cr->Ivar_mu_node();
	if ( mu && mu->OPND()->Aux_id() == Return_vsym() ) {
	  mu->Set_OPND( Prop_return_vsym_new_result(mu->OPND()) );
	}
      }
      break;

    case CK_OP:
      {
	// have we already visited this cr, which may be a CSE?
	if ( Is_retvsym_visited( cr ) )
	  break;
	((DCE *) this)->Set_retvsym_visited( cr );

	// visit the kids
	for ( INT32 ikid = 0; ikid < cr->Kid_count(); ikid++ ) {
	  Propagate_return_vsym_cr( cr->Opnd(ikid) );
	}
      }
      break;

    case CK_DELETED:
    default:
      FmtAssert( FALSE,
	("DCE::Propagate_return_vsym_cr: bad coderep") );
      break;
  }
}

// ====================================================================
// After we've deleted some globals that had chis with a virtual
// variable in them, we need to go back and fix up the use-def chain
// to be correct.
//
// This involves "skipping" over dead statements and updating the
// references.  The algorithm does update the dead statements as well
// however, so each reference only has to look at its definition to see
// what the propagated value is.
//
// Do this in dominator-tree order, so it must be called for the first
// time with the entry-bb that dominates all blocks.
// ====================================================================

void
DCE::Propagate_return_vsym_bb( BB_NODE *bb ) const
{
  // propagate into the phi-nodes, if any
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;
  FOR_ALL_ELEM ( phi, phi_iter, Init(bb->Phi_list()) ) {
    if ( ! phi->Dse_dead() ) {
      CODEREP *res = phi->RESULT();
      if ( res->Aux_id() == Return_vsym() ) {
	for ( INT pkid = 0; pkid < phi->Size(); pkid++ ) {
	  phi->Set_opnd(pkid, Prop_return_vsym_new_result(phi->OPND(pkid)));
	}
      }
    }
  }

  // handle the block's statements, INCLUDING non-live ones
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    // handle mu references only on live statements
    if ( stmt->Live_stmt() ) {
      // handle the statement's mu list
      if ( stmt->Has_mu() ) {
	MU_LIST_ITER mu_iter;
	MU_NODE *mu;
	FOR_ALL_NODE( mu, mu_iter, Init(stmt->Mu_list()) ) {
	  if ( mu->OPND()->Aux_id() == Return_vsym() ) {
	    mu->Set_OPND( Prop_return_vsym_new_result(mu->OPND()) );
	  }
	}
      }

      // handle the lhs, if any
      if ( stmt->Lhs() != NULL ) {
	Propagate_return_vsym_cr( stmt->Lhs() );
      }

      // handle the rhs, if any
      if ( stmt->Rhs() != NULL ) {
	Propagate_return_vsym_cr( stmt->Rhs() );
      }
    }

    // need to handle all chi statements, dead or not
    // handle the statement's chi list
    if ( stmt->Has_chi() ) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *chi;
      FOR_ALL_NODE( chi, chi_iter, Init(stmt->Chi_list())) {
	if ( ! chi->Dse_dead() && chi->Aux_id() == Return_vsym() ) {
	  // propagate into the chi node's operand
	  chi->Set_OPND( Prop_return_vsym_new_result( chi->OPND() ));

	  // it's possible for the statement to become live for a
	  // reason other than this chi, so check if we just need to
	  // make the chi live as well.
	  // But, if we never processed this statement for the return
	  // vsym, don't make it live because there is no mu ref that
	  // post-dominates it. (see pv#362689)
	  if ( ! chi->Live() && stmt->Live_stmt() &&
	       stmt->Dce_retvsym() ) 
	  {
	    chi->Set_live(TRUE);
	  }
	}
      }
    }

  }

  // do copy propagation for this block's dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Propagate_return_vsym_bb(dom_bb);
  }
}

// ====================================================================
// Mark_statement_live - Mark this statement as required and mark
// (recursively) any statements that become required because this one
// is.
// ====================================================================

void
DCE::Mark_statement_live( STMTREP *stmt ) const
{
  // don't bother if someone was lazy enough to call us twice with
  // the same statement
  if ( stmt->Live_stmt() )
    return;

//   if (Tracing()) {
//     fprintf( TFile, "  DCE::Mark_statement_live:\n" );
//     stmt->Print(TFile);
//   }

  OPERATOR opr = stmt->Opr();

  if (OPERATOR_is_scalar_store (opr) &&
      Enable_identity_removal() &&
      stmt->Is_identity_assignment_removable()) {
    // process the rhs expression, if any
    CODEREP *rhs = stmt->Rhs();
    if ( rhs != NULL ) {
      if (stmt->Opr() == OPR_PREFETCH)
	Mark_coderep_live(rhs->Ilod_base());
      else
	Mark_coderep_live( rhs );
    }
    return;
  }

  // go ahead and say this one's live
  stmt->Set_live_stmt();

  switch (opr) {
    
  case OPR_STID:
  case OPR_STBITS:
    break;
    
  case OPR_ISTORE:
  case OPR_ISTBITS:
    {
      Mark_coderep_live(stmt->Lhs()->Istr_base());
      
      // Follow the prefetch info
      PF_POINTER *pf = stmt->Lhs()->Ivar_occ()->Pf_pointer();
      if (pf != NULL) {
	if (PF_PTR_wn_pref_1L(pf) != NULL)
	  Mark_statement_live((STMTREP *) PF_PTR_wn_pref_1L(pf));
	if (PF_PTR_wn_pref_2L(pf) != NULL)
	  Mark_statement_live((STMTREP *) PF_PTR_wn_pref_2L(pf));
      }
    }
    break;
    
  case OPR_MSTORE:
    Mark_coderep_live(stmt->Lhs()->Istr_base());
    Mark_coderep_live(stmt->Lhs()->Mstore_size());
    break;

  case OPR_REGION: // make exit labels live
    Mark_region_exits_live(stmt);
    break;
    
  case OPR_GOTO:
  case OPR_REGION_EXIT:
    { BB_NODE *goto_bb = Branch_target_block( stmt );
      if (goto_bb) // some region exit labels might be outside current region
	Check_for_label( goto_bb );
    }
    break;

  case OPR_TRUEBR:
  case OPR_FALSEBR:
  case OPR_ZDLBR:
    Mark_branch_related_live( stmt );
    break;

  case OPR_FORWARD_BARRIER:
  case OPR_BACKWARD_BARRIER:
    // Fix 570666 again: does not need to keep chi node live because
    // DCE does not delete dse-dead or dead chi node.  -Raymond 7/24/98.
    //
    // Fix 570666: keep chi node barrier live.
    // 
    // if (stmt->Has_chi()) {
    // CHI_LIST_ITER chi_iter;
    // CHI_NODE *cnode;
    //  CHI_LIST *chi_list = stmt->Chi_list();
    // FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
    //   Mark_chinode_live(cnode, stmt);
    // }
    // }
    break;

  default: 
    // process the lhs expression, if any
    CODEREP *lhs = stmt->Lhs();
    if ( lhs != NULL ) {
      Mark_coderep_live( lhs );
    }
  }

  // process the rhs expression, if any
  CODEREP *rhs = stmt->Rhs();
  if ( rhs != NULL ) {
    if (stmt->Opr() == OPR_PREFETCH)
      Mark_coderep_live(rhs->Ilod_base());
    else
      Mark_coderep_live( rhs );
  }

  // Ignore the CHI nodes because they are not necessarily live, except
  // in the case of zero-version chi nodes, which we mark live.
  if ( WOPT_Enable_Zero_Version && stmt->Has_zver() ) {
    // this stmt is used by some zero version, so mark that
    // zero-version chi-node live.
    Mark_zero_version_chinode_live( stmt );
  }

  // mark chi-node defining incomplete_uses to be live
  CHI_LIST_ITER  chi_iter;
  CHI_NODE      *cnode;
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (!cnode->Dse_dead() &&
	cnode->RESULT() != NULL &&
	cnode->RESULT()->Is_flag_set(CF_INCOMPLETE_USES))
      Mark_chinode_live(cnode, stmt);
  }

  // process the mu list if any
  if ( stmt->Has_mu() ) {
    Mark_sr_munode_live( stmt );
  }

  // make this statement's block required
  if ( ! stmt->Bb()->Reached() ) {
    Mark_block_live( stmt->Bb() );
  }
}

// ====================================================================
// Determine if there are GOTO statements that we must keep around
// because they make create loops that do not appear to terminate.
// ====================================================================

void
DCE::Mark_infinite_loops_live( void ) const
{
  if (Tracing())
    fprintf( TFile, "DCE::Mark_infinite_loops_live\n");
  // only have infinite loops if there are blocks that do not reach
  // the exit block, which causes us to create a fake exit
  if ( _cfg->Exit_bb() == _cfg->Fake_exit_bb() ) {
    BB_LIST_ITER pred_iter;
    BB_NODE *pred;
    FOR_ALL_ELEM( pred, pred_iter, Init(_cfg->Fake_exit_bb()->Pred()) ) {
      // In an infinite loop (or a block leading to it) Willexit is FALSE
      if (! pred->Willexit() && pred->Kind() == BB_GOTO) {
 	STMTREP *goto_stmt = pred->Branch_stmtrep();
 	if ( goto_stmt != NULL )
	  Mark_statement_live( goto_stmt );
      }
      // Old code replaced so that overall/cfg5 and wopt/c429683-00 pass
//    if ( pred->Kind() == BB_GOTO ) {
// 	STMTREP *goto_stmt = pred->Branch_stmtrep();
// 	if ( goto_stmt != NULL ) {
// 	  BB_NODE *tgt_bb = Branch_target_block( goto_stmt );
// 	  Is_True( tgt_bb != NULL,
// 	  	   ("DCE::Mark_infinite_loops_live: tgt_bb == NULL\n"));
// 	  // the target of the goto must be postdominated by this block
// 	  if ( pred->Postdominates( tgt_bb ) ) {
// 	    Mark_statement_live( goto_stmt );
//        }
// 	}
//    }
    }
  }
}

// ====================================================================
// Mark_block_live - Mark this bb's label, pragmas, etc. (if any) as
// live
// ====================================================================

void
DCE::Mark_block_live( BB_NODE *bb ) const
{
  if ( bb->Reached() ) {
    return;
  }

//   if (Tracing())
//     fprintf( TFile, "  DCE::Mark_block_live(BB%u)\n", bb->Id());

  // Say we reach this block (for later unreachable code)
  bb->Set_reached();

  // LOOP: for each bb in CD-1 (reverse cfg-dominator) of bb
  BB_NODE *cdbb;
  BB_NODE_SET_ITER rcfg_iter;
  FOR_ALL_ELEM( cdbb, rcfg_iter, Init( bb->Rcfg_dom_frontier() ) ) {
    // if this block has a branch, mark it as live
    STMTREP *brstmt = cdbb->Branch_stmtrep();
    if ( brstmt != NULL && ! brstmt->Live_stmt() ) {
      Mark_statement_live( brstmt );
    }
  }

  // if this block has a label statement, mark it as live
  STMTREP *labelstmt = bb->Label_stmtrep();
  if ( labelstmt != NULL && ! labelstmt->Live_stmt() ) {
    Mark_statement_live( labelstmt );
  }

  // if this block has any pragma statements, mark them as live
  if ( bb->Haspragma() ) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if ( stmt->Opr() == OPR_PRAGMA || stmt->Opr() == OPR_XPRAGMA ) {
	Mark_statement_live( stmt );
      }
    }
  }

  // In preopt, if this block is a DOEND block and the DOSTART block
  // is not marked live, then mark the DOSTART block as live.  This
  // ensures that every live loop has a (potentially empty) live
  // DOSTART block.  The emitter cannot handle two successive empty
  // blocks, but we here rely on an assumption that this can never
  // occur for a DOSTART block.
  //
  if ( bb->Kind() == BB_DOEND && (bb->Loop()->Flags() & LOOP_PREOPT) ) {
    BB_NODE *loop_start = bb->Loopstart();
    if (loop_start != NULL && !loop_start->Reached())
       Mark_block_live( loop_start );
  }
  
  // In preopt, if this block is a DOSTART block, check the previous
  // BB for loop_pragma and mark them requied.
  if ( bb->Kind() == BB_DOSTART && (bb->Loop()->Flags() & LOOP_PREOPT) ) {
    BB_NODE *prev_bb = bb->Prev();
    STMTREP_ITER stmt_iter(prev_bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if ( stmt->Opr() == OPR_PRAGMA &&
           Loop_pragma( (WN_PRAGMA_ID)WN_pragma(stmt->Orig_wn()) ) ) {
	Mark_block_live( prev_bb );
        break;
      }
    }
  }
}

// ====================================================================
// Mark_statements_dead - Mark all statements dead
// ====================================================================

void
DCE::Mark_statements_dead( void ) const
{

  CFG_ITER cfg_iter(_cfg);
  BB_NODE *bb;

  // visit all blocks (reached or not)
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    // visit all statements
    // NOTE: a "reverse" order is probably better
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      stmt->Reset_live_stmt();
      if (Stores_proj_op_to_temp(stmt, Opt_stab())) {
	stmt->Set_proj_op_uses(0);
      }
      // For PREG renumbering, mark this statement's LHS as safe to
      // renumber if the statement is a store to a non-dedicated PREG.
      if (Enable_preg_renumbering() &&
	  OPERATOR_is_scalar_store (stmt->Opr ())) {
	CODEREP *lhs = stmt->Lhs();
	AUX_STAB_ENTRY *aux_entry =
	  Opt_stab()->Aux_stab_entry(lhs->Aux_id());
	if (aux_entry->Is_non_dedicated_preg()) {
	  lhs->Set_safe_to_renumber_preg();
	  aux_entry->Reset_emitter_flags();
	}
      }
      if (stmt->Has_chi()) {
	bool update_mu = OPERATOR_is_scalar_istore (stmt->Opr()) &&
	  stmt->Lhs()->Ivar_mu_node();
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	FOR_ALL_NODE(cnode,chi_iter,Init(stmt->Chi_list())) {
	  if (cnode->Live()) {
	    cnode->Set_live(FALSE);
	    cnode->Set_dse_dead(FALSE);
	  }
	  else {
	    cnode->Set_dse_dead(TRUE);
	    // satisfy Verify_istore_version requirement.   A dse-dead
	    // chi-node cannot have a live mu-node of the same aux-id.
	    if (update_mu && 
		stmt->Lhs()->Ivar_mu_node() &&
		stmt->Lhs()->Ivar_mu_node()->Aux_id() == cnode->Aux_id())
	      stmt->Lhs()->Set_ivar_mu_node(NULL);
	  }
	}
      }
    }

    // visit all phi nodes for this block
    PHI_LIST_ITER phi_iter;
    PHI_NODE     *phi;
    FOR_ALL_ELEM ( phi, phi_iter, Init(bb->Phi_list()) ) {
      if (phi->Live()) {
        phi->Reset_live();
	phi->Reset_dse_dead();
      }
      else {
	phi->Set_dse_dead();
      }
    }

    // start out assuming this block isn't reached
    bb->Reset_reached();
  } // end loop through blocks
}

// ====================================================================
// Determine if this block has a terminating branch and it's live
// ====================================================================

BOOL
DCE::BB_branch_live( BB_NODE *bb ) const
{
  STMTREP *br = bb->Branch_stmtrep();
  if ( br && br->Live_stmt() ) {
    // it's live, but make sure it's a terminating branch, and not 
    // just a call at the end of a block
    const OPERATOR opr = br->Opr();
    if ( Is_branch(opr) ) {
      return TRUE;
    }

    // exception to branch
    if ( opr == OPR_IO && bb->Kind() == BB_IO ) {
      return TRUE;
    }
  }

  return FALSE;
}

// ====================================================================
// Normally, the rcfg set for a block contains only its "immediate"
// rcfg members, so we sometimes want the recursive set of all things
// that it's control-dependent upon.
// ====================================================================

void
DCE::Get_full_rcfg_dom_frontier( BB_NODE *bb ) const
{
  BB_NODE_SET saved_set( _cfg->Total_bb_count(), _cfg, 
			 _cfg->Loc_pool(), BBNS_EMPTY );
  BOOL changed;

  do {
    changed = FALSE;
    saved_set.CopyD( bb->Rcfg_dom_frontier() );

    // go through blocks that we're control-dependent upon, and see
    // if we already have included all of the blocks that they in
    // turn are c-d upon.
    BB_NODE *rcfgbb;
    BB_NODE_SET_ITER rcfg_iter;
    FOR_ALL_ELEM( rcfgbb, rcfg_iter, Init( &saved_set ) ) {
      bb->Rcfg_dom_frontier()->UnionD( rcfgbb->Rcfg_dom_frontier() );
    }

    // did we change anything?
    if ( ! saved_set.EqualP( bb->Rcfg_dom_frontier() ) )
      changed = TRUE;
  } while ( changed );
}

// ====================================================================
// When we decide a conditional branch isn't required (or the entire 
// block isn't required), we will replace all of the successors with
// a single successor (the immediate post-dominator block).
//
// This function is also responsible for updating the phi nodes of
// the immediate post-dominator.
// ====================================================================

void
DCE::Add_path_to_ipdom( BB_NODE *bb ) const
{
  BB_NODE *ipdom = bb->Ipdom();

  if ( ipdom == _cfg->Fake_exit_bb() ) {
    if ( bb->Willexit() ) {
      FmtAssert( FALSE,
	("DCE::Add_path_to_ipdom: post-dom block is fake exit block") );
    }
    else {
      // supposedly, this block is in an infinite loop, so it probably
      // doesn't matter what we do.  So, don't do anything.
      return;
    }
  }

  // only need to do this if bb is not already a predecessor of ipdom
  if ( ipdom->Pred()->Contains(bb) )
    return;

  // Here's the scary part:  We need to pick out one of the preds of
  // the ipdom as being "representative" of 'bb' for the purpose of
  // updating the phi-nodes.  For any live phi-nodes, any preds that
  // are control-dependent upon 'bb' should have the same value coming
  // from that pred.
  //
  // i.e., if ipdom has three preds, 1,2,3, if both 2 and 3 are control
  // dependent upon 'bb', then the phi operands for 2 and 3 should be
  // the same version.

  BB_LIST_ITER pred_iter;
  BB_NODE *ipdom_pred;
  BB_NODE *rep_bb = NULL;
  INT      rep_pos= -1;
  FOR_ALL_ELEM( ipdom_pred, pred_iter, Init(ipdom->Pred()) ) {
    Get_full_rcfg_dom_frontier( ipdom_pred );
    if ( ipdom_pred->Rcfg_dom_frontier()->MemberP( bb ) ) {
      // this block is control-dependent upon bb
      rep_bb = ipdom_pred;
      rep_pos= ipdom->Pred()->Pos(ipdom_pred);
      break;
    }
  }

  FmtAssert( rep_bb != NULL,
    ("DCE::Add_path_to_ipdom: no representative bb for BB:%d",
     bb->Id()) );

  if ( Tracing() ) {
    fprintf( TFile, "DCE::Add_path_to_ipdom: add bb%d -> ipdom bb%d\n",
      bb->Id(), ipdom->Id() );
  }

  bb->Append_succ( ipdom, _cfg->Mem_pool() );
  ipdom->Append_pred( bb, _cfg->Mem_pool() ); 
  PHI_LIST *new_philist = 
    ipdom->Phi_list()->Dup_phi_node(_cfg->Mem_pool(), ipdom, rep_pos);
  _mod_phis->Add_entry(ipdom, ipdom->Phi_list(), new_philist, _cfg->Mem_pool());
  ipdom->Set_phi_list(new_philist);
}

// ====================================================================
// Replace paths to any successors that are control-dependent upon
// this block with a path to the immediate post-dominator, which is
// where we would end up if this branch were not here.
// ====================================================================

void 
DCE::Replace_control_dep_succs( BB_NODE *bb ) const
{
  BOOL all_cd = TRUE;

#ifdef KEY
  if (bb->Succ() == NULL)
    return;
#endif
  // just remove paths to any control-dep successors
  BB_LIST *succlist, *nextsucc = NULL;
  for ( succlist = bb->Succ(); succlist != NULL; succlist = nextsucc ) {
    // remember the next one in case we remove this one
    nextsucc = succlist->Next();

    // is this successor control-dependent on us?
    BB_NODE *succ = succlist->Node();
    if ( succ->Rcfg_dom_frontier()->MemberP( bb ) ) {
      Remove_path( bb, succ );
    }
    else {
      all_cd = FALSE;
    }
  }

  if ( all_cd ) {
    // Need to add a path to our immedate post-dominator
    Add_path_to_ipdom( bb );
  }

  // Update feedback
  if ( _cfg->Feedback() )
    _cfg->Feedback()->Merge_outgoing_edges( bb->Id(), bb->Ipdom()->Id() );
}

// ====================================================================
// Handle bb_repeatend for Check_required_blocks()
// ====================================================================

void
DCE::Check_required_repeatend( BB_NODE *bb ) const
{
  if ( BB_branch_live( bb ) ) {
    // having a live branch is all that's necessary for a valid loop,
    // but make sure we keep around interesting blocks
    if ( _cfg->Lower_fully() ) {
      Keep_unreached_bb(bb->Loopstart());
      Keep_unreached_bb(bb->Loopbody());
      Keep_unreached_bb(bb->Loopmerge());

      // make sure there's a label at our destination block
      Check_for_label( bb->Loopbody() );
    }
    else {
      Keep_unreached_bb(bb->Loopbody());
      Keep_unreached_bb(bb->Loopmerge());
    }
  }
  else {
    // either block is not reached, or just condition at end is not
    // reached, so it just becomes a goto block
    if ( _cfg->Lower_fully() ) {
      bb->Loopstart()->Set_kind( BB_GOTO ); // dohead becomes goto
					    // no dotail for repeatend
    }
    else {
      Remove_path( bb, bb->Loopbody() );
      bb->Loopbody()->Set_kind( BB_GOTO );
    }

    Replace_control_dep_succs( bb );
    bb->Set_loop( NULL );
    bb->Set_kind( BB_GOTO );
  }
}

// ====================================================================
// Handle bb_whileend for Check_required_blocks()
// ====================================================================

void
DCE::Check_required_whileend( BB_NODE *bb ) const
{
  if ( BB_branch_live( bb ) ) {
    // having a live branch is all that's necessary for a valid loop,
    // but make sure we keep around interesting blocks
    if ( _cfg->Lower_fully() ) {
      if (bb->Loopstart())
        Keep_unreached_bb(bb->Loopstart());
      else { 
        Is_True(bb->Loop()->Is_flag_set(LOOP_PRE_WHILE),("wrong non bottom test loop lowering!\n"));
      }
      Keep_unreached_bb(bb->Loopbody());
      Keep_unreached_bb(bb->Loopmerge());

      // make sure there's a label at our destination block
      Check_for_label( bb->Loopbody() );
    }
    else {
      Keep_unreached_bb(bb->Loopbody());
      Keep_unreached_bb(bb->Loopstep());
      Keep_unreached_bb(bb->Loopmerge());
    }
  }
  else {
    // either block is not reached, or just condition at end is not
    // reached, so it just becomes a goto block, and the loop is
    // broken up
    BB_LOOP *loopinfo = bb->Loop();
    if ( loopinfo != NULL ) {
      if ( _cfg->Lower_fully() ) {
	if ( loopinfo->Start() != NULL &&
	     loopinfo->Start()->Kind() == BB_DOHEAD )
	{
	  // and the dohead becomes a simple fall-through block
	  loopinfo->Start()->Set_kind( BB_GOTO );
	}
	if ( loopinfo->Merge() != NULL &&
	     loopinfo->Merge()->Kind() == BB_DOTAIL )
	{
	  // and the merge block becomes a simple fall-through block
	  loopinfo->Merge()->Set_kind( BB_GOTO );
	}
      }
      // NOTE: if not lowered fully, there are no other blocks to
      // fix up.
    }

    Replace_control_dep_succs( bb );
    bb->Set_loop( NULL );
    bb->Set_kind( BB_GOTO );
  }
}

// ====================================================================
// Handle bb_doend for Check_required_blocks()
// ====================================================================

void
DCE::Check_required_doend( BB_NODE *bb ) const
{
  if ( BB_branch_live( bb ) ) {
    if ( bb->Loop() != NULL ) {
      // is it still a good do-loop?
      BOOL valid_loop = TRUE;
      if ( _cfg->Lower_fully() ) {
	// check for low-level requirements
	// NOTE: there are none other than having a live "end" block
	//       which this one is if we reach this point

	// make sure there's a label at our destination block
	Check_for_label( Branch_target_block( bb->Branch_stmtrep() ) );
      }
      else {
	// check for high-level requirements
	if ( !bb->Loopstart()->Reached() || 
	     bb->Loopstart()->Kind() != BB_DOSTART )
	  valid_loop = FALSE;
	else if ( !bb->Loopstep()->Reached() ||
		  bb->Loopstep()->Kind() != BB_DOSTEP )
	  valid_loop = FALSE;

	if ( ! valid_loop ) {
	  // convert the loop to a while-do loop
	  bb->Loopstart()->Set_kind( BB_GOTO );
	  bb->Loopstep()->Set_kind( BB_GOTO );
	  bb->Loopend()->Set_kind( BB_WHILEEND );

	  Check_required_whileend( bb );

	  return;
	}
      }

      if ( valid_loop ) {
	// keep around the interesting blocks
	Keep_unreached_bb(bb->Loopstart());
	Keep_unreached_bb(bb->Loopbody());
	Keep_unreached_bb(bb->Loopstep());
	Keep_unreached_bb(bb->Loopmerge());
	Keep_unreached_bb(bb->Loopend());

	Is_True( bb->Loop()->Trip_count_stmt() == NULL ||
		bb->Loop()->Trip_count_stmt()->Live_stmt(),
	  ("DCE::Check_required_doend: Trip_count not live") );
      }
    }
    else {
      FmtAssert( FALSE,
	("DCE::Check_required_doend: no loop info for end bb:%d",
	 bb->Id()) );
    }
  }
  else {
    // we no longer conditionally branch to the merge block
    Replace_control_dep_succs( bb );

    // either not reached, or just conditional branch not live
    // And in either case, this is no longer a loop
    bb->Loopstart()->Set_kind( BB_GOTO );
    if ( bb->Loopstep()->Kind() == BB_DOSTEP )
      bb->Loopstep()->Set_kind( BB_GOTO );
    // merge and tail are same thing
    if ( bb->Looptail()->Kind() == BB_DOTAIL )
      bb->Looptail()->Set_kind( BB_GOTO );
    // this block just becomes a goto to some other block because the
    // conditional branch isn't live
    bb->Loopend()->Set_kind( BB_GOTO );

    bb->Set_loop( NULL );
  }
}

// ====================================================================
// Handle goto for Check_required_blocks()
// ====================================================================

void
DCE::Check_required_goto( BB_NODE *bb ) const
{
  // only interested if this block is reached
  if ( bb->Reached() ) {
    STMTREP *br_stmt = bb->Branch_stmtrep();

    if (br_stmt->Opr() == OPR_REGION) { // black box region
      Mark_statement_live( br_stmt );
      BB_LIST_ITER bb_succ_iter;
      BB_NODE *succ;
      FOR_ALL_ELEM(succ, bb_succ_iter, Init(bb->Succ())) {
	Mark_block_live(succ);
      }
    } else {
      // nothing to do if this goto is already live
      if ( br_stmt->Live_stmt() )
	return;

      // see if the target block is reached also
      BB_NODE *goto_bb = Branch_target_block( br_stmt );
      if ( goto_bb->Reached() ) {
	Mark_statement_live( br_stmt );
      }
    }
  }
}

// ====================================================================
// Handle bb_io for Check_required_blocks()
// ====================================================================

void
DCE::Check_required_io( BB_NODE *bb ) const
{
  STMTREP *br_stmt = bb->Branch_stmtrep();

  FmtAssert( br_stmt->Live_stmt(),
    ("DCE::Check_required_io: non-live IO statment bb:%d", bb->Id()) );

  // should keep around all blocks that we may jump to, which means
  // all of our successors
  BB_LIST_ITER succ_iter;
  BB_NODE *succ;
  FOR_ALL_ELEM( succ, succ_iter, Init(bb->Succ()) ) {
    Keep_unreached_bb( succ );
  }

  // need to keep labels around at all of the places we may branch to
  INT32 num_entries = bb->IO_entries();
  for (INT32 num_entry = 0; num_entry < num_entries; num_entry++ ) {
    Check_for_label( bb->IO_bb( num_entry ) );
  }
}

// ====================================================================
// Handle bb_logif for Check_required_blocks()
// ====================================================================

void
DCE::Check_required_logif( BB_NODE *bb ) const
{
  if ( BB_branch_live( bb ) ) {
    if ( bb->Ifinfo() != NULL ) {
      // the "if" is still valid, so keep the associated blocks
      // around
      Keep_unreached_bb(bb->If_then());
      Keep_unreached_bb(bb->If_else());
      Keep_unreached_bb(bb->If_merge());
    }
    else {
      // the condition is live, so we should keep the target block
      // and the fall-through blocks around
      BB_NODE *goto_bb = Branch_target_block( bb->Branch_stmtrep() );
      Keep_unreached_bb( goto_bb );
      Keep_unreached_bb( bb->Next() );

      // make sure there's a label at our goto block
      Check_for_label( goto_bb );
    }
  }
  else {
    // either not reached, or conditional branch not live
    if ( bb->Ifinfo() != NULL ) {
      bb->If_merge()->Reset_ifmerge();
      bb->Set_ifinfo( NULL );
    }

    // this will just become a goto to some successor block
    Replace_control_dep_succs( bb );
    bb->Set_kind( BB_GOTO );
  }
}

// ====================================================================
// Handle bb_regionstart for Check_required_blocks()
// ====================================================================

void
DCE::Check_required_region( BB_NODE *bb ) const
{
  // just save a list of the start blocks, and we'll deal with them
  // all together once we've processed all of the blocks.
  Region_start_bbs()->Union1D( bb );
}

BOOL
DCE::BB_may_throw (BB_NODE* bb) {

  if (_may_throw_bbs.find (bb->Id()) != _may_throw_bbs.end()) {
    return TRUE;
  }

  if (_nothrow_bbs.find (bb->Id ()) != _nothrow_bbs.end ()) {
    return FALSE;
  }

  if (!bb->Hascall ()) {
    return FALSE;
  }

  BOOL may_throw = FALSE;

  // Examine STMTREPs
  //
  if (bb->First_stmtrep ()) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE (stmt, stmt_iter, Init()) {
      OPERATOR opr = stmt->Opr ();
      if (!OPERATOR_is_call (opr)) {
          continue;
      }
  
      if (opr == OPR_CALL) {
        ST* st = stmt->St ();
        PU& pu = Pu_Table [ST_pu(*st)];
        if (!PU_nothrow (pu)) {
          may_throw = TRUE;
          break;
        }
      } else {
        // not able to handle indirect calls, virtual functions calls
        may_throw = TRUE;
        break;
      }
    } // end of FOR_ALL_NODE 
  }

  // Examine WN statements
  //
  if (!may_throw && bb->Firststmt ()) {
    // HINT: calls are not necessarily at the end of the block depending on 
    //   CFG::Calls_break().
    //
    WN* wn;
    STMT_ITER iter;
    FOR_ALL_ELEM (wn, iter, Init (bb->Firststmt(), bb->Laststmt())) {
      OPERATOR opr = WN_operator (wn);  
      if (!OPERATOR_is_call (opr)) {
        continue;
      }
  
      if (opr == OPR_CALL) {
        ST* st = WN_st (wn);
        PU& pu = Pu_Table [ST_pu(*st)];
        if (!PU_nothrow (pu)) {
          may_throw = TRUE;
          break;
        }
      } else {
        // not able to handle indirect calls, virtual functions calls
        may_throw = TRUE;
        break;
      }
    } // end of FOR_ALL_ELEM 
  }
  
  if (may_throw) {
    _may_throw_bbs[bb->Id()] = TRUE;
  } else {
    _nothrow_bbs[bb->Id()] = TRUE;
  }

  return may_throw;
}

void
DCE::Collect_may_throw_bbs (void) {

  if (!_cfg->Has_regions () || Language != LANG_CPLUS) {
    _may_throw_bbs.clear ();
  }

  {
    CFG_ITER iter(_cfg);
    BB_NODE *bb;
    FOR_ALL_NODE (bb, iter, Init()) {

      if (!bb->Hascall () || !BB_may_throw (bb)) {
        _nothrow_bbs[bb->Id()] = TRUE;
      } else {
        _may_throw_bbs[bb->Id()] = TRUE;
      }
    } // end of FOR_ALL_NODE(..) 
  }
}

BOOL
DCE::Strip_try_region_helper (BB_REGION* rgn) {

  BB_NODE* rgn_first_bb = rgn->Region_start();
  BB_NODE* rgn_last_bb = rgn->Region_end(); 
  if (!RID_TYPE_try (rgn->Rid())) {
    // not appliable
    return FALSE;
  }

  // step 1: don't touch this region if it isn't in good shape.
  //
  if (rgn_first_bb->Next() != rgn_last_bb) {
    for (BB_NODE* t = rgn_first_bb->Next(); TRUE; t = t->Next ()) {
      if (t->Kind () == BB_REGIONSTART) {
        // We need to skip inner regions, therefore the region-info
        // should be well maintained.
        //
        BB_REGION* inner_rgn = t->Regioninfo ();
        if (!inner_rgn || !inner_rgn->Region_end () || 
            inner_rgn->Region_end () == rgn_last_bb) {
          if (Tracing ()) {
            fprintf (TFile, 
                     "Strip_try_region_helper: skip region started at BB%d "
                     "because inner region start at BB%d isn't in good shape",
                     rgn_first_bb->Id(), t->Id());
          } 
          return FALSE;
        }
      }

      if (t == rgn_last_bb) {
        break;
      }
    } // end of for-loop
  }

  // step 2 :check the calls in the region
  //
  for (BB_NODE* bb = rgn_first_bb; bb; bb = bb->Next ()) {
      if (BB_may_throw (bb)) return FALSE; 
      if (bb == rgn_last_bb) break;
  }

  if (Tracing ()) {
    fprintf (TFile, 
             "Strip_try_region_helper: identify unnecessary try region"
             " bb%d->bb%d\n", rgn_first_bb->Id(), rgn_last_bb->Id());
  }

  // step 3: remove the region information associated with the 1st block of 
  //   the region.
  //
  Remove_region_entry (rgn_first_bb);
  rgn_first_bb->Set_kind (BB_GOTO);

  // step 4: Convert OPR_REGION_EXIT to goto; change BB_REGIONEXIT to BB_GOTO.
  //
  for (BB_NODE* blk = rgn_first_bb; TRUE; ) {

    // skip innert regions 
    //
    BB_NODE* next_blk = blk->Next ();
    if (blk->Kind () == BB_REGIONSTART) {
      next_blk = blk->Regioninfo()->Region_end ()->Next();
    }

    // Change BB_REGIONEXIT to BB_GOTO, and remove region information associated
    // with it.
    //
    if (blk->Kind() == BB_REGIONEXIT) {
      blk->Set_regioninfo (NULL);
      blk->Set_kind (BB_GOTO);
    }

    if (STMTREP* stmt = blk->Last_stmtrep()) {
      if (stmt->Opr() == OPR_REGION_EXIT) {
        INT32 lab_num = stmt->Label_number ();
        blk->Remove_stmtrep (stmt);
        stmt = CXX_NEW (STMTREP (OPC_GOTO), _cfg->Mem_pool());
        stmt->Set_label_number (lab_num);
        blk->Append_stmtrep (stmt);
      }
    } else if (WN* wn = blk->Laststmt ()) {
      if (WN_operator (wn) == OPR_REGION_EXIT) {
        INT32 lab_num = WN_label_number (wn);
        Is_True (FALSE, ("TODO: replace OPR_REGION_EXIT with OPR_GOTO"));
      }
    }

    // advance to next block
    //
    if (blk == rgn_last_bb) { break; }
    blk = next_blk;
  }
  
  return TRUE;
}

// Get rid of the "try" region if the calls in the region don't throw exception.
//
BOOL
DCE::Strip_try_region (void) {

  if ( ! _cfg->Has_regions() )
    return FALSE;

  if (!WOPT_Enable_Nothrow_Opt)
    return FALSE;

  BOOL change = FALSE;

  // step 1: identify those blocks which contains a call which may throw exception.
  //
  Collect_may_throw_bbs ();

  // step 2: loop over each try-region, examining the code in it. Remove the region 
  //  if the calls in the region won't throw exception.
  //
  CFG_ITER cfg_iter(_cfg);
  BB_NODE *bb;
  FOR_ALL_NODE (bb, cfg_iter, Init() ) {

    if (bb->Kind() != BB_REGIONSTART) {
      continue; 
    }

    BB_REGION* rgn = bb->Regioninfo();
    if (rgn && RID_TYPE_try (rgn->Rid())) {
      if (Strip_try_region_helper (rgn)) {
        change = TRUE;
      }
    }
  }

  return change;
}

// ====================================================================
// Update the region information after all check_required is done.
// For EH Guard Regions we need to call Keep_unreached_bb on all its blocks.
// ====================================================================
void
DCE::Update_region_information( void ) const
{
  if ( ! _cfg->Has_regions() )
    return;

  BB_NODE *regbb;
  BB_NODE_SET_ITER reg_iter;
  FOR_ALL_ELEM( regbb, reg_iter, Init( Region_start_bbs() ) ) {
    // make sure they're still starting the region
    if ( regbb->Kind() == BB_REGIONSTART ) {
      BB_REGION *bb_region = regbb->Regioninfo();
      Is_True(bb_region != NULL,
	      ("DCE::Update_region_information, NULL bb_region"));
      BB_NODE *region_end = bb_region->Region_end();
      // Any EH Guard regions are kept always
      if (RID_TYPE_guard(bb_region->Rid())) {
	Is_Trace(Tracing(),(TFile,"DCE::Update_region_information, keeping "
			    "EH GUARD region\n"));
	BB_NODE *btmp = region_end;
	while (btmp != regbb) {
	  if ( ! btmp->Reached() )
	    Keep_unreached_bb(btmp);
	  btmp = btmp->Prev();
	}
	if ( ! regbb->Reached() )
	  Keep_unreached_bb(regbb);
      }

      BOOL remove_region = FALSE;
      if (region_end) {
	// find last block in region that has code in it.
	while ( ! (region_end->Reached() || 
		   Keep_unreached()->MemberP(region_end)) )
	  {
	    if ( region_end != regbb )
	      region_end = region_end->Prev();
	    else {
	      // we've made it to the beginning of the region, and still
	      // haven't found any useful code (i.e., regbb==region_end,
	      // and this block is neither reached nor kept
	      region_end = NULL;
	      remove_region = TRUE;
	      break;
	    }
	  }
      }

      if (remove_region) {
	// the region is empty, it is no longer a valid region
	Remove_region_entry(regbb);
	regbb->Set_kind( BB_GOTO );
	if ( Tracing() ) {
	  fprintf( TFile, 
		   "Update_region_information: removed region bb%d->bb%d\n",
		   regbb->Id(), bb_region->Region_end()->Id() );
	}
      } else {
	// there are some blocks within the region reached, so keep
	// this region around

	// keep the start of the region around if it isn't already
	if ( ! regbb->Reached() ) {
	  Keep_unreached_bb( regbb );
	}

	// need to keep any buffer blocks that precede the start 
	// There should be either 1 or 0 such blocks
	BB_LIST_ITER pred_iter;
	BB_NODE *pred;
	INT pred_cnt = 0;
	FOR_ALL_ELEM( pred, pred_iter, Init( regbb->Pred() ) ) {
	  Keep_unreached_bb( pred );
	  pred_cnt++;
	  Is_True(pred_cnt == 1, ("DCE::Update_region_information: start "
			   "(bb:%d) has more than 1 pred",regbb->Id()));
	}

	// keep the end of the region around if it isn't already
        region_end = bb_region->Region_end();
	if ( region_end && ! region_end->Reached() ) {
	  Keep_unreached_bb( region_end );
	}

	// now mark following block also as a buffer for any added
	// code.
	// fixes pv 385990
	if ( region_end &&
	     region_end->Succ() != NULL && 
	     region_end->Succ()->Len() > 0 )
	{
	  for (BB_LIST *succlist=region_end->Succ(); succlist!=NULL;
	       succlist=succlist->Next()) {
	    if (!succlist->Node()->Reached())
	      Keep_unreached_bb(succlist->Node());
	  }
	}

      } 

      // if a region starts off with a block that ends in a branch,
      // DCE could try to insert the branch into the region_start BB
      // if it deletes the condition code or if it is an unconditional
      // branch. (547802)
      // first need to check it is still a region start, it might be deleted
      if (regbb->Kind() == BB_REGIONSTART && regbb->Succ() != NULL && regbb->Succ()->Len() == 1) {
#ifndef KEY
	Is_True(regbb->Succ() != NULL && regbb->Succ()->Len() == 1,
		("DCE::Update_region_information, multiple successors "
		 "for region start"));
#endif
	BB_NODE *succ = regbb->Succ()->Node();
	if (!succ->Reached())
	  Keep_unreached_bb(succ);
      }

    } // if (regbb->Kind() == BB_REGIONSTART)
  } // FOR_ALL_ELEM (Region_start_bbs)
}

// ====================================================================
// Handle bb_vargoto for Check_required_blocks()
// ====================================================================

void
DCE::Check_required_vargoto( BB_NODE *bb ) const
{
  if ( BB_branch_live( bb ) ) {
    // should keep around all blocks that we may jump to 
    INT32 num_entries = bb->Switchentries();
    for (INT32 num_entry = 0; num_entry < num_entries; num_entry++ ) {
      Keep_unreached_bb( bb->Switchcase( num_entry ) );
      Check_for_label( bb->Switchcase( num_entry ) );
    }

    if ( bb->Switchdefault() ) {
      Keep_unreached_bb( bb->Switchdefault() );
      Check_for_label( bb->Switchdefault() );
    }
  }
  else {
    // either not reached, or conditional branch not live
    bb->Set_switchinfo( NULL );

    // this will just become a goto to some successor block
    Replace_control_dep_succs( bb );
    bb->Set_kind( BB_GOTO );
  }
}


void
DCE::Check_required_agoto( BB_NODE *bb ) const
{
  if ( BB_branch_live( bb ) ) {
    // should keep around all blocks that we may jump to 
    INT32 num_entries = _cfg->Agoto_succ_entries();
    for (INT32 num_entry = 0; num_entry < num_entries; ++num_entry ) {
      Keep_unreached_bb( _cfg->Agoto_succ_bb( num_entry ) );
      Check_for_label( _cfg->Agoto_succ_bb( num_entry ) );
    }
  }
  else {
    // either not reached, or conditional branch not live
    // this will just become a goto to some successor block
    Replace_control_dep_succs( bb );
    bb->Set_kind( BB_GOTO );
    _cfg->Remove_agoto_pred( bb );
  }
}


// ====================================================================
// Based upon this block's kind, see if any related blocks or statements
// need to be made to remain (even if empty)
// NOTE:  Keep track of which blocks must be kept even if they're not
// reached.  (we could use the "reached" flag, but I prefer to leave
// that alone until we're all done.)
// ====================================================================

void
DCE::Check_required_blocks( void ) const
{
  if ( Tracing() )
    fprintf( TFile, "DCE::Check_required_blocks\n" );

  // visit all blocks, reached or not
  // This time, decide if there are other blocks or statements that
  // should be considered reached
  CFG_ITER cfg_iter(_cfg);
  BB_NODE *bb;
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    switch ( bb->Kind() ) {

      case BB_DOEND:     // ending condition
	Check_required_doend( bb );
	break;

      case BB_DOHEAD:    // do head block
	// let doend or whileend handle it
	break;

      case BB_DOSTART:   // init block
	// let doend handle it
	break;

      case BB_DOSTEP:    // increment
	// let doend handle it
	break;

      case BB_DOTAIL:    // do tail block
	// let doend or whileend handle it
	break;

      case BB_ENTRY:     // the entry bb
	if ( ! bb->Reached() ) {
	  if ( bb == _cfg->Fake_entry_bb() )
	    Keep_unreached_bb( bb );
	  else
	    bb->Set_reached();
	}
	// go ahead and say the lone successor of the entry block must
	// be kept so we don't try inserting a goto statement in the
	// entry block, which we assume is empty except for the entry 
	// statement
	if ( bb != _cfg->Fake_entry_bb() ) {
	  Keep_unreached_bb(bb->Next());
	}
	break;

      case BB_EXIT:      // the exit bb
	if ( ! bb->Reached() ) {
	  // after unreachable code has run, all remaining exits are 
	  // indeed reachable, so keep them around
	  if ( bb != _cfg->Fake_exit_bb() )
	    bb->Set_reached();
	  else
	    Keep_unreached_bb( bb );
	}
	break;

      case BB_GOTO:	// single target BB
	// it's only interesting if it has a goto statement in it.
	if ( bb->Branch_stmtrep() != NULL ) {
	  Check_required_goto( bb );
	}
	break;

      case BB_IO:     	// IO statement (mix of goto and vargoto)
	Check_required_io( bb );
	break;

      case BB_LOGIF:     // logical if
	Check_required_logif( bb );
	break;

      case BB_REGIONEXIT:// end of region -- let regionstart handle
	break;

      case BB_REGIONSTART:// start of region
	Check_required_region( bb );
	break;

      case BB_REPEATBODY:// first BB in repeat body
	// let repeatend handle it
	break;

      case BB_REPEATEND: // ending condition for repeat statement
	Check_required_repeatend( bb );
	break;

      case BB_SUMMARY:   // summary BB
	break;

      case BB_VARGOTO:   // variable goto
	if ( bb->Switchinfo() ) {
	  Check_required_vargoto( bb );  // SWITCH, COMPGOTO, XGOTO
	} else {
	  Check_required_agoto( bb );    // AGOTO
	}
	break;

      case BB_WHILEEND:  // ending condition for while statement
	Check_required_whileend( bb );
	break;

      case BB_UNKNOWN:   // invalid kind
      default:
	ErrMsg( EC_Unimplemented, 
	  "DCE::Check_required_blocks: invalid bb Kind()" );
	break;
    }
  }

  // now go back and check out regions again.
  // if it deletes an entire region, it deletes the RID also.
  Update_region_information();
}


// ====================================================================
// Find_required_statements - Find all of the required statements in
// all of the reached blocks
// ====================================================================

void 
DCE::Find_required_statements( void ) const
{
  CFG_ITER cfg_iter(_cfg);
  BB_NODE *bb;

  if ( Tracing() )
    fprintf( TFile, "DCE::Find_required_statements\n" );

  // find a new set of blocks to keep around even if they're not
  // reached
  Keep_unreached()->ClearD();

  // visit all blocks, reached or not
  // NOTE: a "reverse" order is probably better
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
  
    // required BB?
    if ( Required_bb( bb ) )
      Mark_block_live( bb );

    // visit all statements
    // NOTE: a "reverse" order is probably better
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;

    // visit all phi nodes for this block
    PHI_LIST_ITER phi_iter;
    PHI_NODE     *phi;
    FOR_ALL_ELEM ( phi, phi_iter, Init(bb->Phi_list()) ) {
      if ( ! phi->Live() && Required_phi(  phi ) ) {
	Mark_phinode_live( phi, TRUE/*visit opnds*/ );
      }
    }

    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if ( ! stmt->Live_stmt() && Required_stmt( stmt ) ) {
	// mark this statement as live, as well as any statements
	// that define values for it (recursively)
	Mark_statement_live( stmt );
      }
    }

  } // end loop through blocks
#ifdef TARG_SL
  Repair_Injured_AuxIntrnOP();
#endif

  // see if there are infinite loops that we need to keep around
  Mark_infinite_loops_live();

  if ( Tracing() ) {
    fprintf( TFile, "DCE::Keep_unreached stmts: " );
    Keep_unreached()->Print( TFile ) ;
    fprintf( TFile, "\n" );
  }

  // visit all blocks, reached or not
  // This time, decide if there are other blocks or statements that
  // should be considered reached
  Keep_unreached()->ClearD();
  Check_required_blocks();

  if ( Tracing() ) {
    fprintf( TFile, "DCE::Keep_unreached blocks: " );
    Keep_unreached()->Print( TFile ) ;
    fprintf( TFile, "\n" );
  }

  // do copy-propagation to get return vsym correct
  if ( Enable_dce_global() ) {
    Propagate_return_vsym_bb( _cfg->Entry_bb() );
  }
}

// ====================================================================
// Update_branch_to_bb_labels update labels and branch statements so
// that all branches to this block target its successor instead.
// ====================================================================
#ifdef KEY
void
DCE::Update_branch_to_bbs_labels( BB_LIST *bbs ) const
{
  BB_NODE *bb, *last_bb;
  BB_LIST_ITER bb_iter;
  BOOL label_needed = FALSE;
                                                                                                                                          
  FOR_ALL_ELEM( bb, bb_iter, Init(bbs) )
    last_bb = bb;

  FOR_ALL_ELEM( bb, bb_iter, Init(bbs) ){
    if ( bb->Labnam() != 0 )
      label_needed = TRUE;
  }
  if (label_needed == FALSE)
    return;

                                                                                                                                          
  // Find the unique distinct successor
  BB_LIST_ITER succ_iter;
  BB_NODE *succ, *the_succ = NULL;
  FOR_ALL_ELEM( succ, succ_iter, Init( last_bb->Succ() ) )
    if (succ != last_bb) {
      Is_True(the_succ == NULL,
              ("DCE::Update_branch_to_bb_labels: Multiple successors\n"));
      the_succ = succ;
    }
                                                                                                                                          
  LABEL_IDX label;
  if (the_succ == NULL)
    label = 0;
  else {
    label = the_succ->Labnam();
    if ( label == 0 ) {
      label = _cfg->Alloc_label();
      the_succ->Set_labnam( label );
      _cfg->Append_label_map(label, the_succ);
      if (the_succ->Label_stmtrep() == NULL)
        the_succ->Add_label_stmtrep( _cfg->Mem_pool() );
    }
  }
                                                                                                                                          
  // change all pred branch targets to the new label
  BB_LIST_ITER pred_iter;
  BB_NODE *pred;
  FOR_ALL_ELEM( bb, bb_iter, Init(bbs) ){
    FOR_ALL_ELEM( pred, pred_iter, Init( bb->Pred() ) ) {
      if (bbs->Contains(pred))
        continue;
      STMTREP *branch_stmt = pred->Branch_stmtrep();
      if (branch_stmt != NULL) {
        OPERATOR opr = branch_stmt->Opr();
        if ((opr == OPR_GOTO ||
             opr == OPR_ZDLBR ||
             opr == OPR_TRUEBR ||
             opr == OPR_FALSEBR) && 
             (branch_stmt->Label_number() == bb->Labnam())){
          if (Tracing())
            fprintf( TFile, "  changing label %u to %u in BB%u\n",
                   bb->Labnam(), label, pred->Id() );
          branch_stmt->Set_label_number(label);
        }
      }
    }
  }
}
#endif

void
DCE::Update_branch_to_bb_labels( BB_NODE *bb ) const
{
  if ( bb->Labnam() == 0 ) return;
  if (Tracing())
    fprintf( TFile, "DCE::Update_branch_to_bb_labels for BB%u\n",
	     bb->Id());

  // Find the unique distinct successor
  BB_LIST_ITER succ_iter;
  BB_NODE *succ, *the_succ = NULL;
  FOR_ALL_ELEM( succ, succ_iter, Init( bb->Succ() ) )
    if (succ != bb) {
      Is_True(the_succ == NULL,
	      ("DCE::Update_branch_to_bb_labels: Multiple successors\n"));
      the_succ = succ;
    }

//  CAN'T do the following, because can't change _label_map in cfg
//   // if the_succ has no labels, just move the label statement;
//   if ( the_succ->Labnam() == 0 ) {
//     the_succ->Set_labnam(bb->Labnam());
//     the_succ->Add_label_stmtrep( _cfg->Mem_pool() );
//     bb->Set_labnam(0);
//     bb->Remove_label_stmtrep();  // unnecessary?
//     return;
//   }

  LABEL_IDX label;
  // If no successor (see bug 643920 for an example), use label 0
  if (the_succ == NULL)
    label = 0;
  else {
    label = the_succ->Labnam();
    // if the_succ has no labels, create one for it
    if ( label == 0 ) {
      label = _cfg->Alloc_label();
      the_succ->Set_labnam( label );
      _cfg->Append_label_map(label, the_succ);
      if (the_succ->Label_stmtrep() == NULL)
	the_succ->Add_label_stmtrep( _cfg->Mem_pool() );
    }
  }

  // change all pred branch targets to the new label
  BB_LIST_ITER pred_iter;
  BB_NODE *pred;
  FOR_ALL_ELEM( pred, pred_iter, Init( bb->Pred() ) ) {
    STMTREP *branch_stmt = pred->Branch_stmtrep();
    if (branch_stmt != NULL) {
      OPERATOR opr = branch_stmt->Opr();
      if ((opr == OPR_GOTO ||
           opr == OPR_ZDLBR ||
	   opr == OPR_TRUEBR ||
	   opr == OPR_FALSEBR) &&
	  branch_stmt->Label_number() == bb->Labnam()) {
	if (Tracing())
	  fprintf( TFile, "  changing label %u to %u in BB%u\n",
		   bb->Labnam(), label, pred->Id() );
	branch_stmt->Set_label_number(label);
      }
    }
  }
}

// ====================================================================
// Remove_dead_statements - Remove any statements that are not marked
// as being live/required
// ====================================================================

BOOL
DCE::Remove_dead_statements( void )
{
  BB_NODE *bb;
  BOOL changed_cflow = FALSE;
  BB_LIST *removable_bbs = NULL;
  BB_LIST *removable_bb_chain = NULL; 

  // visit all blocks
  // NOTE: a "reverse" order is probably better
  BB_NODE *nextbb = NULL;
  for ( bb = _cfg->First_bb(); bb != NULL; bb = nextbb ) {
    // remember the next bb in case we change the internal linkage
    nextbb = bb->Next();

    if ( bb->Reached() || Keep_unreached()->MemberP(bb) ) {
      // visit all statements
      STMTREP *stmt, *nextstmt = NULL;

      // trace loop first
      if ( Tracing() ) {
	for (stmt = bb->First_stmtrep(); stmt != NULL; stmt = nextstmt){
	  nextstmt = stmt->Next();
	  if ( ! stmt->Live_stmt() ) {
	    fprintf( TFile, "Remove dead statement from live BB:%d\n",
		     bb->Id() );
	    stmt->Print( TFile );
	  }
	}
      }

      // then real delete loop
      for ( stmt = bb->First_stmtrep(), nextstmt = NULL;
	    stmt != NULL;
	    stmt = nextstmt )
      {
	nextstmt = stmt->Next();
	if ( ! stmt->Live_stmt() ) {
	  bb->Remove_stmtrep(stmt);
	}
        else {
        //  Dead chi node cannot be removed because it is needed
	// to suuppress copy propagation.
        //     Remove_dead_chinodes(stmt);
        }
      }

      // go ahead and say this block is reached, just in case it was
      // just one we kept
      if ( ! bb->Reached() && _cfg->Removable_bb(bb) ) {
	bb->Set_reached();
      }
    }
    else {

      // entire block found to be unreached
      if ( Tracing() ) {
	fprintf( TFile, "DCE_A: BB%d unreached (%p)\n", bb->Id(), bb );
      }

      if ( Enable_aggressive_dce() ) {
      
        Is_True( _cfg->Removable_bb(bb),
	  ("DCE::Remove_dead_statements: BB%d not removable",
	   bb->Id()) );
#ifdef KEY
        if (WOPT_Enable_Aggressive_dce_for_bbs)
          removable_bbs = removable_bbs->Append(bb, _cfg->Loc_pool());
        else{
	  Update_branch_to_bb_labels( bb );
	  _cfg->Delete_bb( bb, _mod_phis );
        }
#endif
	changed_cflow = TRUE;

	if ( Tracing() ) {
	  fprintf( TFile, "DCE_A: Removed BB%d (%p)\n", bb->Id(), bb );
	}
      }
      else {
	bb->Set_reached();
      }
#ifdef KEY
      if ( !removable_bbs->Contains(bb) || !WOPT_Enable_Aggressive_dce_for_bbs ) 
#endif
        Remove_unreached_statements( bb );
    }
  } // end loop through blocks
//Bug# 1278
#ifdef KEY  
  if ( !Enable_aggressive_dce() || !WOPT_Enable_Aggressive_dce_for_bbs) 
    return  ( changed_cflow );

  BB_LIST_ITER bb_iter;
  BB_NODE *cur_bb;
  BOOL last_bb_unique_succ = TRUE;

  for ( ; removable_bbs->Len() ; ){
    bb = removable_bbs->Node();

    if (removable_bb_chain != NULL){
      CXX_DELETE(removable_bb_chain, _cfg->Loc_pool());
      removable_bb_chain = NULL;
    }

    while (bb->Pred()->Len() == 1 && bb->Pred()->Node() != bb && removable_bbs->Contains( bb->Pred()->Node() ))
      bb = bb->Pred()->Node();
    while (bb){
      removable_bb_chain = removable_bb_chain->Append(bb, _cfg->Loc_pool());
      removable_bbs = removable_bbs->Remove(bb, _cfg->Loc_pool());
      if (bb->Succ()->Len() == 1 && bb->Succ()->Node() != bb && removable_bbs->Contains( bb->Succ()->Node() )){
        bb = bb->Succ()->Node();
        if (bb->Succ()->Len() == 1 &&
            bb->Succ()->Node()->Pred()->Len() == 1 &&
            (bb->Succ()->Node()->Phi_list()== NULL || bb->Succ()->Node()->Phi_list()->Is_Empty()) &&
            bb->Phi_list() != NULL)
          bb = NULL;
      }
      else{
        if (bb->Succ()->Len() != 1)
          last_bb_unique_succ = FALSE;
        bb = NULL;
      }
    }
    
    if ( removable_bb_chain && removable_bb_chain->Len() > 1 && last_bb_unique_succ){
//Bug 1507
      Update_branch_to_bbs_labels( removable_bb_chain );
      _cfg->Delete_bbs( removable_bb_chain, _mod_phis ); 
      FOR_ALL_ELEM( bb, bb_iter, Init(removable_bb_chain) ){
        Remove_unreached_statements( bb );
      }
    }
    else if ( removable_bb_chain ){
// If the last basic block has more than one successor, remove basic block one by one. It fixes bug 2379.
      FOR_ALL_ELEM( bb, bb_iter, Init(removable_bb_chain) ){
        Update_branch_to_bb_labels( bb );
        _cfg->Delete_bb( bb, _mod_phis );
        Remove_unreached_statements( bb );
      }
    }
  }
#endif
  return ( changed_cflow );
}

// ====================================================================
// Find all the blocks that have assumed gotos, and therefore do not
// need real ones inserted.  Example: last block in "else" block of
// an "if" has an assumed branch to the if-merge block around the 
// "then" blocks.
// ====================================================================

void
DCE::Find_assumed_goto_blocks( BB_NODE_SET *assumed_goto ) const
{
  Is_True( ! _cfg->Lower_fully(),
    ("DCE::Find_assumed_goto_blocks: do not call if lowered fully") );

  CFG_ITER cfg_iter(_cfg);
  BB_NODE *bb;
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    if ( ! bb->Reached() )
      continue;

    switch ( bb->Kind() ) {

      case BB_DOEND:     // ending condition
	// the loop-step block has an assumed goto
	assumed_goto->Union1D( bb->Loopstep() );
	break;

      case BB_DOSTART:   // init block
	// let doend handle it
	break;

      case BB_DOSTEP:    // increment
	// let doend handle it
	break;

      case BB_ENTRY:     // the entry bb
	// none
	break;

      case BB_EXIT:      // the exit bb
      case BB_REGIONEXIT:// region exit bb
	// none
	break;

      case BB_GOTO:      // single target BB
	// none
	break;

      case BB_IO:	// IO statement (mix of goto/vargoto)
	// none
	break;

      case BB_LOGIF:     // logical if
	if ( bb->Ifinfo() != NULL ) {
	  // there's possibly an assumed branch from the last of the
	  // "then" blocks to the merge block.
	  BB_NODE *last_then = bb->If_else()->Prev();
	  if ( last_then->Succ()->Contains( bb->If_merge() ) ) {
	    assumed_goto->Union1D( last_then );
	  }
	}
	break;

      case BB_REGIONSTART: // start of region has none
	break;

      case BB_REPEATBODY:// first BB in repeat body
	// let repeatend handle it
	break;

      case BB_REPEATEND: // ending condition for repeat statement
	// no assumed branches for this loop
	break;

      case BB_SUMMARY:   // summary BB
	// none
	break;

      case BB_VARGOTO:   // variable goto
	// all branches are pretty explicit in this one
	break;

      case BB_WHILEEND:  // ending condition for while statement
	// the loop-step block has an assumed goto
	assumed_goto->Union1D( bb->Loopstep() );
	break;

      case BB_UNKNOWN:   // invalid kind
      default:
	ErrMsg( EC_Unimplemented, 
	  "DCE::Find_assumed_goto_blocks: invalid bb Kind()" );
	break;
    }
  }

  if ( Tracing() ) {
    fprintf( TFile, "DCE::Find_assumed_goto_blocks: " );
    assumed_goto->Print( TFile ) ;
    fprintf( TFile, "\n" );
  }
}

// ====================================================================
// Need to possibly insert goto statements for blocks that go to non-
// fall-through blocks, and won't be assumed by the emitter
// ====================================================================

void
DCE::Insert_required_gotos( void ) const
{
  // find all blocks that have assumed gotos
  BB_NODE_SET assumed_goto( _cfg->Total_bb_count(), _cfg, 
			    _cfg->Loc_pool(), BBNS_EMPTY );

  // there are only assumed gotos if we haven't lowered fully
  if ( ! _cfg->Lower_fully() ) {
    Find_assumed_goto_blocks( &assumed_goto );
  }

  // visit all reached blocks
  CFG_ITER cfg_iter(_cfg);
  BB_NODE *bb;
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    if ( ! bb->Reached() )
      continue;
      
    // does this block have an assumed goto?
    if ( assumed_goto.MemberP( bb ) )
      continue;

    // does this block already end with a branch to someplace else?
    // (excludes calls, etc. that don't really end the block)
    STMTREP *br_stmt = bb->Branch_stmtrep();
    if ( br_stmt != NULL ) {
      const OPERATOR br_op = br_stmt->Opr();
      if ( Is_branch(br_op) )
	continue;

      if ( br_op == OPR_IO && bb->Kind() == BB_IO )
	continue;
    }

    // if there is a black box region then the CFG may branch quite a bit
    // Two cases:
    //   A) bb->Kind() == BB_GOTO
    //		region is last statement in a BB_GOTO block meaning it has
    //		been processed before.
    //   B) bb->Kind() == BB_REGIONEXIT
    //		region is last statement in another region's BB_REGIONEXIT
    //          block meaning it was processed and the enclosing region is
    //		transparent.
    if (bb->Last_stmtrep() && bb->Last_stmtrep()->Opr() == OPR_REGION )
      continue;

#ifdef KEY // needed due to fix for bug 8690
    if (bb->MP_region() && _opt_phase != MAINOPT_PHASE &&
        bb->Kind() == BB_REGIONSTART) {
      // see if the assumed goto is due to a SINGLE pragma
      BOOL single_pragma_found = FALSE;
      STMTREP_ITER stmt_iter(bb->Stmtlist());
      STMTREP *stmt;
      FOR_ALL_NODE( stmt, stmt_iter, Init() ) {
	if (stmt->Opr() == OPR_PRAGMA &&
	    WN_pragma(stmt->Orig_wn()) == WN_PRAGMA_SINGLE_PROCESS_BEGIN) {
	  single_pragma_found = TRUE;
	  break;
	}
      }
      if (single_pragma_found)
	continue;
    }
#endif

    // at this point, we know we don't have a branch, assumed or
    // otherwise.  Check if our only successor is a fall-through
    // block or not.
    INT succ_cnt = 0;
    BB_LIST_ITER bb_succ_iter;
    BB_NODE *succ;
    FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
      FmtAssert( succ_cnt == 0,
		("DCE::Insert_required_gotos: more than one succ BB%d",
		 bb->Id()) );
      succ_cnt++;
      if ( succ != bb->Next() ) {
	Add_goto_stmt( bb, succ, bb->Linenum(), bb->Kind() == BB_REGIONEXIT );
        br_stmt = bb->Branch_stmtrep();
	if ( br_stmt != NULL )
	  Mark_statement_live( br_stmt );
      }
    }
  }
}

// ====================================================================
// Dead_store_elim - perform dead-code/store elimination
// ====================================================================

BOOL 
DCE::Dead_store_elim(void)
{
  BOOL changed_cflow = FALSE;

  // mark all statements dead
  Mark_statements_dead();

  // and then find those that are necessary
  Find_required_statements();

  // and then remove any that are not found to be necessary
  changed_cflow = Remove_dead_statements();

  Insert_required_gotos();

  // if we change any control-flow, assume we messed up our loop
  // structures as well
  if ( changed_cflow )
    _cfg->Invalidate_loops();

  return ( changed_cflow );
}

// ====================================================================
// ====================================================================
//
// COMP_UNIT::Do_dead_code_elim - invoke the dead-code elimination
// optimization
//
// ====================================================================
// ====================================================================

void
COMP_UNIT::Do_dead_code_elim(BOOL do_unreachable,
			     BOOL do_dce_global,
			     BOOL do_dce_alias,
			     BOOL do_agg_dce,
			     BOOL do_identity_removal,
			     BOOL do_preg_renumbering,
			     BOOL *paths_removed)
{
  // get a local memory area
  OPT_POOL_Push( Cfg()->Loc_pool(), DCE_DUMP_FLAG);

  BOOL unreachable = FALSE;
  if (paths_removed) *paths_removed = unreachable;
  {
    DCE dce(Cfg(), Opt_stab(), Arule(), Htable(),
            Get_Trace( TP_GLOBOPT, DCE_DUMP_FLAG ), Phase(),
	    do_unreachable, do_dce_global, do_dce_alias, do_agg_dce,
	    do_identity_removal, do_preg_renumbering);
    if ( dce.Enable_dce_unreachable() ) {
      if ( dce.Tracing() ) {
	fprintf( TFile, "%sUnreachable_code_elim\n%s", SBar, SBar );
      }

      dce.Set_phase_unreachable();
      unreachable = dce.Unreachable_code_elim();

      if ( unreachable ) {
	Cfg()->Invalidate_and_update_aux_info(TRUE);
      }
    }

    if ( dce.Tracing() ) {
      fprintf( TFile, "%sDead_store_elim\n%s", SBar, SBar );
    }

    dce.Set_phase_dead_store();
    dce.Init_return_vsym();
    unreachable = dce.Dead_store_elim();

    if ( unreachable ) {
      Cfg()->Invalidate_and_update_aux_info(TRUE);
    }

    // fake blocks should not be considered as reached
    if ( Cfg()->Fake_entry_bb() != NULL )
      Cfg()->Fake_entry_bb()->Reset_reached();
    if ( Cfg()->Fake_exit_bb() != NULL )
      Cfg()->Fake_exit_bb()->Reset_reached();

    if ( dce.Tracing() ) {
      fprintf( TFile, "%sAfter COMP_UNIT::Do_dead_code_elim\n%s",
              DBar, DBar );
      Cfg()->Print( TFile );
    }
    if (paths_removed)
      *paths_removed = unreachable;
  }

  // get rid of local memory area
  OPT_POOL_Pop( Cfg()->Loc_pool(), DCE_DUMP_FLAG);
}

#ifdef KEY
#include "../../include/gnu/demangle.h"
extern "C" char *cplus_demangle (const char *, int);

void
COMP_UNIT::Find_uninit_locals_for_entry(BB_NODE *bb) 
{
  if (bb->Kind() != BB_ENTRY)
    return;
  STMTREP *stmt = bb->First_stmtrep();
  Is_True(bb->First_stmtrep()->Op() == OPC_OPT_CHI,
  	  ("Find_uninit_locals_for_entry: cannot find entry chi"));
  CHI_NODE *cnode;
  CHI_LIST_ITER chi_iter;
  AUX_STAB_ENTRY *sym;
  FOR_ALL_NODE(cnode, chi_iter, Init(bb->First_stmtrep()->Chi_list())) {
    if (! cnode->Live())
      continue;
    if (cnode->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION))
      continue;
    sym = Opt_stab()->Aux_stab_entry(cnode->Aux_id());
    if (sym->St() == NULL)
      continue;
    if (ST_sclass(sym->St()) != SCLASS_AUTO)
      continue;
    if (! sym->Is_real_var())
      continue;
    if (ST_is_temp_var(sym->St()))
      continue;
    if (sym->Is_volatile())
      continue;
    if (sym->Mp_shared())
      continue;
    if (sym->Has_nested_ref())
      continue;
    if (! sym->Points_to()->Local())
      continue;
    if (! sym->Points_to()->No_alias())
      continue;
    if (sym->Points_to()->F_param())
      continue;

    char *output_pu_name = Cur_PU_Name;
    char *output_var_name = &Str_Table[sym->St()->u1.name_idx];
    char *p = NULL;
    char *v = NULL;
    if ((PU_src_lang(Get_Current_PU()) & PU_CXX_LANG)) {
      // C++ mangled names begin with "_Z".
      if (output_pu_name[0] == '_' && output_pu_name[1] == 'Z') {
	p = cplus_demangle(output_pu_name, DMGL_PARAMS|DMGL_ANSI|DMGL_TYPES);
	if (p)
	  output_pu_name = p;
      }
      if (output_var_name[0] == '_' && output_var_name[1] == 'Z') {
	v = cplus_demangle(output_var_name, DMGL_PARAMS|DMGL_ANSI|DMGL_TYPES);
	if (v)
	  output_var_name = v;
      }
    }

    ErrMsgLine(EC_Uninitialized, ST_Line(*(sym->St())), output_var_name, output_pu_name);
    if (p != NULL && p != Cur_PU_Name)
      free(p);
    if (v != NULL && v != &Str_Table[sym->St()->u1.name_idx])
      free(v);
  }
}

// ====================================================================
// ====================================================================
//
// COMP_UNIT::Find_uninitialized_locals - find uninitialized local variables
// by going thru the entry CHI list of each entry points to the PU
//
// ====================================================================
// ====================================================================

void
COMP_UNIT::Find_uninitialized_locals(void) 
{
  if (Cfg()->Fake_entry_bb() == NULL)
    Find_uninit_locals_for_entry(Cfg()->Entry_bb());
  else {
    BB_NODE *entry_bb;
    BB_LIST_ITER bb_iter;
    FOR_ALL_ELEM (entry_bb, bb_iter, Init(Cfg()->Fake_entry_bb()->Succ())) {
      Find_uninit_locals_for_entry(entry_bb);
    }
  }
}
#endif
