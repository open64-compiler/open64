/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_rvi.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_rvi.cxx,v $
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
// Perform Register-Variable Identification.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_rvi_CXX	"opt_rvi.cxx"
static char *rcs_id = 	opt_rvi_CXX"$Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "config.h"
#include "tracing.h"
#include "wn.h"
#include "wn_lower.h"

#include "opt_defs.h"
#include "opt_base.h"
#include "opt_config.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_htable.h"
#include "opt_mu_chi.h"
#include "bb_node_set.h"
#include "idx_32_set.h"
#include "opt_alias_mgr.h"
#include "opt_rvi.h"
#include "opt_rvilr.h"
#include "opt_rvitab.h"
#include "opt_rvi_emit.h"
#include "opt_sym.h"
#include "opt_util.h"
#include "opt_alias_interface.h"		// alias_mgr, Verify_alias()

// ====================================================================
// Constructor for the RVI class
// ====================================================================

RVI::RVI( BOOL do_rvi, OPT_STAB *opt_stab, IDX_32 max_varbit, ALIAS_MANAGER *alias_mgr ) :
  _do_rvi(do_rvi),
  _opt_stab(opt_stab)
{
  // do initialization only if we're about to do RVI
  if ( do_rvi ) {
    AUX_ID var;
    AUX_STAB_ITER aux_stab_iter(opt_stab);

    _tracing = Get_Trace( TP_GLOBOPT, RVI_TRACE_FLAG );
    _alias_mgr = alias_mgr;
    // first, get us a memory pool to allocate global things in
    _rvi_gpoolp = &_rvi_gpool;
    OPT_POOL_Initialize( &_rvi_gpool, "Rvi_gpool", FALSE, RVI_TRACE_FLAG+1);
    OPT_POOL_Push( Rvi_gpool(), RVI_TRACE_FLAG+1);
    // get us a memory pool to allocate per-phase things in
    _rvi_ppoolp = &_rvi_ppool;
    OPT_POOL_Initialize( &_rvi_ppool, "Rvi_ppool", FALSE, RVI_TRACE_FLAG+2);
    OPT_POOL_Push( Rvi_ppool() , RVI_TRACE_FLAG+2);
    // get us a memory pool to allocate very local things in
    _rvi_lpoolp = &_rvi_lpool;
    OPT_POOL_Initialize( &_rvi_lpool, "Rvi_lpool", FALSE, RVI_TRACE_FLAG+3);
    OPT_POOL_Push( Rvi_lpool() , RVI_TRACE_FLAG+3);

    WN_MAP_Status();
    // init the mappings
    _mu_map  = WN_MAP_Create( Rvi_gpool() );
    _chi_map = WN_MAP_Create( Rvi_gpool() );
    _bp_map  = WN_MAP32_Create( Rvi_gpool() );

    // do not handle base lda at first
    _do_base_lda = FALSE;
    // use unique pregs for individual live-ranges
    _unique_pregs = TRUE;
    // this is the (currently) largest bit position assigned
    _max_bitpos  = max_varbit;
    // this is the last bitpos of the variables (assigned by mainopt)
    _last_varbit = max_varbit;
    // this is the number of bits we should allocate to bitsets
    // (note that references to Initial_set_size() may change its value)
    _init_set_size = 0;
    // we have no constant table yet
    _rvi_ctab = NULL;
    // this is the control-flow graph we haven't built yet
    _cfg = NULL;
    // no depth-first list of blocks
    _dfs_vec = NULL; _dfs_vec_size = 0;

    // redundancy bit sets
    _redundant._load_top = NULL;
    _redundant._load_bot = NULL;
    _redundant._store_top = NULL;
    _redundant._store_iref = NULL;

    _volatile_set = CXX_NEW( IDX_32_SET(Initial_set_size(), Rvi_gpool(),
					   OPTS_FALSE), Rvi_gpool() );

    FOR_ALL_NODE(var, aux_stab_iter, Init()) {
      AUX_STAB_ENTRY *psym = opt_stab->Aux_stab_entry( var );
      if ( psym->Is_real_var() && psym->Is_volatile()) {
	_volatile_set->Union1D((IDX_32)psym->Itab_bitpos() + 1);
      }
    }
  }
  else {
    // not doing RVI, so don't init much
  }
}

// ====================================================================
// Destructor for the RVI class
// ====================================================================

RVI::~RVI( void )
{
  // do destruction only if we did RVI
  if ( _do_rvi ) {
    // get rid of the mappings
    WN_MAP_Delete( Mu_map() );
    WN_MAP_Delete( Chi_map() );
    WN_MAP_Delete( Bp_map() );

    // last, free up our memory pools
    OPT_POOL_Pop( Rvi_lpool(), RVI_TRACE_FLAG+3);
    OPT_POOL_Pop( Rvi_ppool(), RVI_TRACE_FLAG+2);
    OPT_POOL_Pop( Rvi_gpool(), RVI_TRACE_FLAG+1);
    OPT_POOL_Delete( Rvi_lpool(), RVI_TRACE_FLAG+3);
    OPT_POOL_Delete( Rvi_ppool(), RVI_TRACE_FLAG+2);
    OPT_POOL_Delete( Rvi_gpool(), RVI_TRACE_FLAG+1);
  }
  else {
    // did not do RVI, so anything to do?
  }
}

// ====================================================================
// Track the mu list for this node (used from main-opt's emitter)
// ====================================================================

void
RVI::Map_mu_list ( WN *wn, MU_LIST *mu_list )
{
  if ( mu_list->Is_Empty() ) {
    return;
  }

  // references of pregs cannot have mu lists.  This may occur because
  // the emitter generated a preg reference for a CSE, and the expr 
  // that had the mu is only evaluated once (when stored into the preg)
  // We could check for this case in the main emitter, but it's easier
  // to do it here (there, we have a coderep that has a mu, but instead
  // of generating code for that cr, we generate a load from a preg
  // if it is a cse)
  if ( WN_operator(wn) == OPR_LDID &&
       ST_class(WN_st(wn)) == CLASS_PREG )
  {
    return;
  }

  Warn_todo( "RVI::Map_mu_list: do not adjust bitpos by 1" );

  MU_LIST_ITER mu_iter;
  MU_NODE *mu;
  IDX_32_SET *mu_set = NULL;

  // add all of the members to the set
  FOR_ALL_NODE( mu, mu_iter, Init(mu_list) ) {
    CODEREP *mu_opnd = mu->OPND();
    if ( mu_opnd != NULL ) {
      if ( mu_opnd->Bitpos() != ILLEGAL_BP ) {
	if ( mu_set == NULL )
	  mu_set = CXX_NEW( IDX_32_SET(Initial_set_size(), Rvi_gpool(),
				       OPTS_FALSE), Rvi_gpool() );
	mu_set->Union1D( mu_opnd->Bitpos()+1 );
      }
      else if ( mu_opnd->Kind() == CK_VAR ) {
	AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(mu_opnd->Aux_id());
	if ( sym->Is_real_var() ) {
	  // regular variable
	  if ( sym->Itab_bitpos() != ILLEGAL_BP ) {
	    if ( mu_set == NULL )
	      mu_set = CXX_NEW( IDX_32_SET(Initial_set_size(), Rvi_gpool(),
					   OPTS_FALSE), Rvi_gpool() );
	    mu_set->Union1D( sym->Itab_bitpos()+1 );
	  }
	}
	else {
	  // virtual variable
	  if ( sym->Aux_id_list() != NULL ) {
	    AUX_ID_LIST_ITER id_list_iter;
	    AUX_ID_NODE *id_node;
	    FOR_ALL_ELEM( id_node, id_list_iter, Init(sym->Aux_id_list()) ) {
	      if ( (IDX_32)(id_node->Aux_id()) != ILLEGAL_BP ) {
		if ( mu_set == NULL )
		  mu_set = CXX_NEW(IDX_32_SET(Initial_set_size(),Rvi_gpool(),
					      OPTS_FALSE), Rvi_gpool() );
		mu_set->Union1D( (IDX_32)(id_node->Aux_id()+1) );
	      }
	    }
	  }
	}
      }
    }
  }

  if ( mu_set != NULL ) {
    WN_MAP_Set( Mu_map(), wn, (void*)mu_set );
  }
}



// TODO: Map_mu_list can be simplfied!
//
void
RVI::Map_mu_node ( WN *wn, MU_NODE *mu_node )
{
  // references of pregs cannot have mu lists.  This may occur because
  // the emitter generated a preg reference for a CSE, and the expr 
  // that had the mu is only evaluated once (when stored into the preg)
  // We could check for this case in the main emitter, but it's easier
  // to do it here (there, we have a coderep that has a mu, but instead
  // of generating code for that cr, we generate a load from a preg
  // if it is a cse)
  if ( WN_operator(wn) == OPR_LDID &&
       ST_class(WN_st(wn)) == CLASS_PREG )
  {
    return;
  }

  Warn_todo( "RVI::Map_mu_node: do not adjust bitpos by 1" );

  IDX_32_SET *mu_set = NULL;

  CODEREP *mu_opnd = mu_node->OPND();
  if ( mu_opnd != NULL ) {
    if ( mu_opnd->Bitpos() != ILLEGAL_BP ) {
      if ( mu_set == NULL )
	mu_set = CXX_NEW( IDX_32_SET(Initial_set_size(), Rvi_gpool(),
				     OPTS_FALSE), Rvi_gpool() );
      mu_set->Union1D( mu_opnd->Bitpos()+1 );
    }
    else if ( mu_opnd->Kind() == CK_VAR ) {
      AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(mu_opnd->Aux_id());
      if ( sym->Is_real_var() ) {
	// regular variable
	if ( sym->Itab_bitpos() != ILLEGAL_BP ) {
	  if ( mu_set == NULL )
	    mu_set = CXX_NEW( IDX_32_SET(Initial_set_size(), Rvi_gpool(),
					 OPTS_FALSE), Rvi_gpool() );
	  mu_set->Union1D( sym->Itab_bitpos()+1 );
	}
      }
      else {
	// virtual variable
	if ( sym->Aux_id_list() != NULL ) {
	  AUX_ID_LIST_ITER id_list_iter;
	  AUX_ID_NODE *id_node;
	  FOR_ALL_ELEM( id_node, id_list_iter, Init(sym->Aux_id_list()) ) {
	    if ( (IDX_32)(id_node->Aux_id()) != ILLEGAL_BP ) {
	      if ( mu_set == NULL )
		mu_set = CXX_NEW(IDX_32_SET(Initial_set_size(),Rvi_gpool(),
					    OPTS_FALSE), Rvi_gpool() );
	      mu_set->Union1D( (IDX_32)(id_node->Aux_id()+1) );
	    }
	  }
	}
      }
    }
  }

  if ( mu_set != NULL ) {
    WN_MAP_Set( Mu_map(), wn, (void*)mu_set );
  }
}

// ====================================================================
// Track the chi list for this node (used from main-opt's emitter)
// ====================================================================

void
RVI::Map_chi_list ( WN *wn, CHI_LIST *chi_list )
{
  if ( chi_list->Is_Empty() ) {
    return;
  }

  Warn_todo( "RVI::Map_chi_list: do not adjust bitpos by 1" );

  CHI_LIST_ITER chi_iter;
  CHI_NODE *chi;
  IDX_32_SET *chi_set = NULL;

  const OPERATOR opr = WN_operator(wn);
  const BOOL is_scalar = (opr == OPR_STID);
  const BOOL is_istore = (opr == OPR_ISTORE);

#ifdef DEBUGGING
if ( Tracing() ) {
  fprintf( TFile, "RVI::Map_chi_list: WN <%d:%d>\n",
    OPCODE_mapcat(WN_opcode(wn)), WN_map_id(wn) );
}
#endif

  // add all of the members to the set
  FOR_ALL_NODE( chi, chi_iter, Init(chi_list) ) {
    CODEREP *chi_res = chi->RESULT();
    if ( chi_res != NULL ) {

#ifdef DEBUGGING
if ( Tracing() ) {
  fprintf( TFile, "..chi=" );
  chi->Print(TFile);
}
#endif
      if ( chi_res->Bitpos() != ILLEGAL_BP ) {
	if ( chi_set == NULL )
	  chi_set = CXX_NEW( IDX_32_SET(Initial_set_size(), Rvi_gpool(),
				       OPTS_FALSE), Rvi_gpool() );
	chi_set->Union1D( chi_res->Bitpos()+1 );
#ifdef DEBUGGING
if ( Tracing() ) {
  fprintf( TFile, "  chi_res->Bitpos() != ILLEGAL_BP\n" );
}
#endif
      }
      else if ( chi_res->Kind() == CK_VAR ) {
	AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(chi_res->Aux_id());
	if ( sym->Is_real_var() ) {
	  // regular variable
#ifdef DEBUGGING
if ( Tracing() ) {
  fprintf( TFile, "  regular variable\n" );
}
#endif
	  if ( sym->Itab_bitpos() != ILLEGAL_BP ) {
	    if ( chi_set == NULL )
	      chi_set = CXX_NEW( IDX_32_SET(Initial_set_size(), Rvi_gpool(),
					   OPTS_FALSE), Rvi_gpool() );
	    chi_set->Union1D( sym->Itab_bitpos()+1 );
#ifdef DEBUGGING
if ( Tracing() ) {
  fprintf( TFile, "  sym->Itab_bitpos() != ILLEGAL_BP\n" );
}
#endif
	  }
	}
	else {
	  // virtual variable
#ifdef DEBUGGING
if ( Tracing() ) {
  fprintf( TFile, "  virtual variable\n" );
}
#endif
	  if ( WOPT_Enable_Rvivsym ) {
#ifdef DEBUGGING
if ( Tracing() ) {
  fprintf( TFile, "  skipping vsym\n" );
}
#endif
	    continue;
	  }

	  if ( is_istore && WOPT_Enable_Rviistore ) {
	    // if this is an indirect store, we can skip virtual
	    // variables because each of the scalars it kills is in
	    // the chi list as a real variable.  (Right, Raymond?)
#ifdef DEBUGGING
if ( Tracing() ) {
  fprintf( TFile, "  is_istore, so skip virtual variable\n" );
}
#endif
	    continue;
	  }
	  if ( sym->Aux_id_list() != NULL ) {
#ifdef DEBUGGING
if ( Tracing() ) {
  fprintf( TFile, "  Aux_id_list() != NULL\n" );
}
#endif
	    AUX_ID_LIST_ITER id_list_iter;
	    AUX_ID_NODE *id_node;
	    FOR_ALL_ELEM( id_node, id_list_iter, Init(sym->Aux_id_list()) ) {
#ifdef DEBUGGING
if ( Tracing() ) {
  fprintf( TFile, "  Inside Aux_id_list() iter\n" );
}
#endif
	      if ( (IDX_32)(id_node->Aux_id()) != ILLEGAL_BP ) {
		if ( chi_set == NULL )
		  chi_set = CXX_NEW(IDX_32_SET(Initial_set_size(),Rvi_gpool(),
					      OPTS_FALSE), Rvi_gpool() );

		// if we discover something that we're aliased with,
		// and we're a scalar store, we ignore the alias, and
		// just leave a possibly empty chi-set attached to the
		// store which forces us to store to memory at the
		// point of the store.
		if ( ! is_scalar ) {
		  chi_set->Union1D( (IDX_32)(id_node->Aux_id()+1) );
		}
#ifdef DEBUGGING
if ( Tracing() ) {
  fprintf( TFile, "  id_node->Aux_id() != ILLEGAL_BP\n" );
}
#endif
	      }
	    }
	  }
	} // end is virtual variable
      } // end ck_var
    } // end chi result 
  }

  if ( chi_set != NULL ) {
    WN_MAP_Set( Chi_map(), wn, (void*)chi_set );
  }
}

// ====================================================================
// Get the mu list associated with the statement that terminates this 
// block
// ====================================================================

IDX_32_SET *
RVI::Bb_end_mu_list( const BB_NODE *bb ) const
{
  return bb->Loc_mu_ref();
}

// ====================================================================
// Get the chi list associated with the statement that terminates this 
// block
// ====================================================================

IDX_32_SET *
RVI::Bb_end_chi_list( const BB_NODE *bb ) const
{
  return bb->Loc_chi_def();
}

// ====================================================================
// determine if the block has no successors in the region being compiled
// ====================================================================

BOOL
RVI::Is_exit_block( const BB_NODE *bb ) const
{
  if ( bb->Kind() == BB_EXIT || bb->Kind() == BB_REGIONEXIT ) {
    return TRUE;
  }

  return FALSE;
}

// ====================================================================
// the set of values live-out of "Is_exit_block" blocks.  May return
// null if none.
// ====================================================================

const IDX_32_SET *
RVI::Global_vars( const BB_NODE *bb ) const
{
  WN *last = bb->Laststmt();
  if ( last != NULL ) {
    if ( WN_has_mu(last, Cfg()->Rgn_level()) ) {
      return Get_mu_list( last );
    }
  }

  return NULL;
}

// ====================================================================
// Resize any sets that need to be the same length
// ====================================================================

void
RVI::Resize_sets( BB_NODE *bb, INT32 new_size ) const
{
  if ( bb->Loc_appear() )
    bb->Loc_appear()->ResizeD( new_size );
  if ( bb->Loc_def() )
    bb->Loc_def()->ResizeD( new_size );
  if ( bb->Loc_upwd() )
    bb->Loc_upwd()->ResizeD( new_size );
  if ( bb->Loc_mu_ref() )
    bb->Loc_mu_ref()->ResizeD( new_size );

  // resize end-of-bb indirect reference sets
  if ( Has_end_chi_list(bb) ) {
    IDX_32_SET *chi_set = Bb_end_chi_list( bb );
    if ( chi_set != NULL ) {
      chi_set->ResizeD( new_size );
    }
  }
}

// ====================================================================
// Initialize the sets we use to track redundant code
// ====================================================================

void
RVI::Init_redundant( MEM_POOL *pool )
{
  _redundant._load_top = CXX_NEW(BB_NODE_SET(Cfg()->Total_bb_count(), 
				 Cfg(), pool, BBNS_EMPTY), pool);
  _redundant._load_bot = CXX_NEW(BB_NODE_SET(Cfg()->Total_bb_count(), 
				 Cfg(), pool, BBNS_EMPTY), pool);
  _redundant._store_top = CXX_NEW(BB_NODE_SET(Cfg()->Total_bb_count(), 
				 Cfg(), pool, BBNS_EMPTY), pool);
  _redundant._store_iref = CXX_NEW(BB_NODE_SET(Cfg()->Total_bb_count(), 
				 Cfg(), pool, BBNS_EMPTY), pool);
}

// ====================================================================
// Clear the sets we use to track redundant code
// ====================================================================

void
RVI::Clear_redundant( void ) const
{
  _redundant._load_top->ClearD();
  _redundant._load_bot->ClearD();
  _redundant._store_top->ClearD();
  _redundant._store_iref->ClearD();
}

// ====================================================================
// Temporary hack until we get valid loop information
// Once we have valid information (either CFG provided it, or we need
// to call another function), get rid of this function.  We just want
// valid loopdepth information.
// ====================================================================

void
RVI::Find_loops( void ) const
{
  Warn_todo( "RVI::Find_loops: get valid loops" );

  CFG_ITER cfg_iter(Cfg());
  BB_NODE *bb;
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    // is it a self-loop?
    if ( bb->Succ() != NULL && bb->Succ()->Contains(bb) ) {
      bb->Set_loopdepth( 1 );
    }
  }
}

// ====================================================================
// Phase 1 of the algorithm deals with variables, constants, but not
// LDAs.
// ====================================================================

WN *
RVI::Perform_phase1( WN *entry_wn )
{
  WN *return_wn;
  OPT_POOL_Push( Rvi_ppool(), RVI_TRACE_FLAG+4 );

  { //local scope for things allocated to ppool
  CFG cfg( Rvi_ppool(), Rvi_lpool() );
  _cfg = &cfg;

  // create a constant-table
  RVI_CTAB ctab( Rvi_ppool() );
  _rvi_ctab = &ctab;

  // create a variable table
  RVI_VTAB vtab( Last_varbit(), Rvi_ppool() );
  _rvi_vtab = &vtab;

  // create the control-flow graph (breaking up blocks at calls)
  Warn_todo( "RVI::Perform_phase1: get rid of Set_rvi_break_stmt" );
  cfg.Set_rvi_break_stmt(WOPT_Enable_Rvisplit);
  cfg.Create(entry_wn, TRUE/*lower_fully*/, TRUE/*calls_break*/,
	     RL_RVI1/*this is RVI phase 1*/, NULL/*stab*/,
	     FALSE/*tail-rec*/,
	     NULL);
  // we don't build dom-tree, so we need to do this little fixup
  cfg.Remove_fake_entryexit_arcs();
  // locate the loops
  Find_loops();

  // make sure dedicated pregs defined by function calls are stored
  // to pregs rather than to things RVI may work on.
  Copy_dedicated_regs_to_pregs();

  if ( Tracing() ) {
    fprintf( TFile, "%sBefore Perform_phase1 RVI\n%s", DBar, DBar );
    cfg.Print( TFile );
    fprintf( TFile, "%sPerform_phase1\n%s", SBar, SBar );
  }

  // get the local attributes for all the blocks
  Get_local_attributes( FALSE/*!just_lda*/ );

  // create a depth-first ordering of the cfg blocks
  // Do this after Get_local_attributes because it may create new blocks
  _dfs_vec      = cfg.Dfs_vec();
  _dfs_vec_size = cfg.Dfs_vec_sz();

  // get some bit sets to track the redundant code.  Do this here
  // because the local_attributes may change the number of blocks
  Init_redundant( Rvi_ppool() );

  if ( Tracing() ) {
    Rvi_vtab()->Print( TFile );
    Rvi_ctab()->Print( TFile );
  }

  // perform the dataflow analysis
  Get_dataflow_equations();

  // perform RVI for variables
  Perform_variable_rvi();

  // perform RVI for constants
  Perform_constant_rvi();

  if ( Tracing() ) {
    fprintf( TFile, "%sAfter Perform_phase1 RVI\n%s", DBar, DBar );
    cfg.Print( TFile );
  }

  // convert CFG back to a WHIRL tree
  RVI_EMIT emitter( this, FALSE/*!lda_only*/, Alias_Mgr(), RL_RVI1 );
  return_wn = emitter.Entry_wn();

  if ( Tracing() ) {
    fprintf( TFile, "%sPerform_phase1 return_wn\n%s", SBar, SBar );
    fdump_tree( TFile, return_wn );
  }
  } // end of local scope for ppool-allocated values

  OPT_POOL_Pop( Rvi_ppool(), RVI_TRACE_FLAG+4 );

  return return_wn;
}

// ====================================================================
// Phase 2 of the algorithm deals with LDAs.
// ====================================================================

WN *
RVI::Perform_phase2( WN *entry_wn )
{
  WN *return_wn;
  OPT_POOL_Push( Rvi_ppool(), RVI_TRACE_FLAG+5 );

  { // local scope for values allocated to ppool
  // Reinitialize the current mappings and start again because we're 
  // going to start assigning bit positions starting from 0 for
  // this phase, and all of these mappings currently have values
  // from the previous phase.
  WN_MAP_Delete( Mu_map() );
  WN_MAP_Delete( Chi_map() );
  WN_MAP_Delete( Bp_map() );
  _mu_map  = WN_MAP_Create( Rvi_gpool() );
  _chi_map = WN_MAP_Create( Rvi_gpool() );
  _bp_map  = WN_MAP32_Create( Rvi_gpool() );

  // reset some values
  // this is the (currently) largest bit position assigned
  _max_bitpos  = 0;
  // this is the number of bits we should allocate to bitsets
  // (note that references to Initial_set_size() may change its value)
  _init_set_size = 0;

  CFG cfg( Rvi_ppool(), Rvi_lpool() );
  _cfg = &cfg;

  // create a constant-table
  RVI_CTAB ctab( Rvi_ppool() );
  _rvi_ctab = &ctab;

  // no need for a variable-table
  _rvi_vtab = NULL;

  // create the control-flow graph (breaking up blocks at calls)
  cfg.Set_rvi_break_stmt(FALSE);
  cfg.Create(entry_wn, TRUE/*lower_fully*/, TRUE/*calls_break*/,
	     RL_RVI2/*this is RVI phase 2*/, NULL/*stab*/,
	     FALSE/*tail-rec*/,
	     NULL);

  // we don't build dom-tree, so we need to do this little fixup
  cfg.Remove_fake_entryexit_arcs();
  // create a depth-first ordering of the cfg blocks
  _dfs_vec      = cfg.Dfs_vec();
  _dfs_vec_size = cfg.Dfs_vec_sz();

  // locate the loops
  Find_loops();

  // Since rvi2 only works on LDAs, there is no need to 
  // Copy_dedicated_regs_to_pregs(), but we still need to set the Callrel()
  // attribute correctly for basic blocks.
  //
  Set_callrel();

  if ( Tracing() ) {
    fprintf( TFile, "%sBefore RVI::Perform_phase2\n%s", DBar, DBar );
    cfg.Print( TFile );

    fprintf( TFile, "%sRVI::Perform_phase2\n%s", SBar, SBar );
  }

  // get the local attributes for all the blocks
  Get_local_attributes( TRUE/*just_lda*/ );

  // get some bit sets to track the redundant code.  Do this here
  // because the local_attributes may change the number of blocks
  Init_redundant( Rvi_ppool() );

  if ( Tracing() ) {
    Rvi_ctab()->Print( TFile );
  }

  // perform the dataflow analysis
  Get_lda_dataflow_equations();

  // perform RVI for constants
  Perform_constant_rvi();

  if ( Tracing() ) {
    fprintf( TFile, "%sAfter RVI::Perform_phase2\n%s", DBar, DBar );
    cfg.Print( TFile );
  }

  // convert CFG back to a WHIRL tree
  RVI_EMIT emitter( this, TRUE/*lda_only*/, Alias_Mgr(), RL_RVI2 );
  return_wn = emitter.Entry_wn();
  if ( Tracing() ) {
    fprintf( TFile, "%sPerform_phase2 return_wn\n%s", SBar, SBar );
    fdump_tree( TFile, return_wn );
  }
  } // end of local scope for values allocated to ppool

  OPT_POOL_Pop( Rvi_ppool(), RVI_TRACE_FLAG+5 );

  return return_wn;
}

// ====================================================================
// debugging function to print homing maps
// ====================================================================

static void
PRINT_HOMING_INFO( WN *wn, ALIAS_MANAGER *am, BOOL first_time )
{
  if ( first_time ) {
    fprintf( TFile, "%sPRINT_HOMING_INFO\n%s", DBar, DBar );
  }

  const OPCODE opc = WN_opcode(wn);

  if ( opc == OPC_FUNC_ENTRY ) {
    PRINT_HOMING_INFO( WN_func_body(wn), am, FALSE );
  }
  else if ( opc == OPC_BLOCK ) {
    for ( WN *stmt = WN_first(wn); stmt; stmt = WN_next(stmt) ) {
      PRINT_HOMING_INFO( stmt, am, FALSE );
    }
  }
  else {
    // note that we can just use one of the "Homing" functions because
    // they both trigger off of the same map.
    if ( am->Homing_load(wn) ) {
      fprintf( TFile, "Homing: " );
      fdump_wn( TFile, wn );
    }

    for ( INT i = 0; i < WN_kid_count(wn); i++ ) {
      PRINT_HOMING_INFO( WN_kid(wn,i), am, FALSE );
    }
  }
}

// ====================================================================
// ====================================================================
// Main entry point into the RVI algorithm
// ====================================================================
// ====================================================================

WN *
RVI::Perform_RVI( WN *entry_wn, ALIAS_MANAGER *alias_mgr )
{
  Is_True(REGION_consistency_check(entry_wn),
	    ("RVI::Perform_RVI, inconsistent region in RVI"));
  Set_Error_Phase("RVI1");
  WN *phase1_wn = WOPT_Enable_RVI1 ? 
		    Perform_phase1( entry_wn ) :
		    entry_wn;
  Is_True(phase1_wn && REGION_consistency_check(phase1_wn),
	    ("RVI::Perform_RVI, inconsistent region from RVI phase 1"));
  Verify_alias(alias_mgr,phase1_wn);

  LOWER_ACTIONS actions = 
    LOWER_SPLIT_CONST_OFFSETS |
    LOWER_SPLIT_SYM_ADDRS |
    LOWER_PICCALL |
    LOWER_MLDID_MSTID |
    LOWER_ALL_MAPS;      

  WN *lda_wn = WN_Lower( phase1_wn, 
    actions,
    alias_mgr, "RVI" );

  REGION_new_wn(lda_wn, phase1_wn);

  if ( Tracing() ) {
    fprintf( TFile, "%sAfter LDA Lowering\n%s", SBar, SBar );
    fdump_tree( TFile, lda_wn );
  }

  Set_Error_Phase("RVI2");
  WN *phase2_wn = WOPT_Enable_RVI2 ?
		    Perform_phase2( lda_wn ) :
		    lda_wn;
  Is_True(REGION_consistency_check(phase2_wn),
	    ("RVI::Perform_RVI, inconsistent region from RVI phase 2"));
  Verify_alias(alias_mgr,phase2_wn);

  return phase2_wn;
}

// ====================================================================
// Add the node to the constant table if it wasn't already added, and
// assign a new bit position
// ====================================================================

RVI_NODE *
RVI::Add_to_const_table( WN *wn )
{
  IDX_32 hash_val = Rvi_ctab()->Hash( wn );
  RVI_NODE *cnode = Rvi_ctab()->Find( wn, hash_val );

  if ( cnode == NULL ) {
    // constant wasn't found in our table, so it must not have a bitpos
    IDX_32 bitpos = Next_bitpos();
    Map_bitpos( wn, bitpos );

    cnode = Rvi_ctab()->Add_unique( wn, bitpos, hash_val );
  }
  else {
    // want to associate this particular wn with this bitpos
    Map_bitpos( wn, cnode->Bitpos() );
  }

  return cnode;
}

// ====================================================================
// Determine if this LDA node is considered a "base" LDA
// ====================================================================

BOOL
RVI::Is_base_lda( const WN *wn ) const
{
  Is_True( WN_operator(wn) == OPR_LDA,
    ("RVI::Is_base_lda (not): %s", OPERATOR_name(WN_operator(wn))) );

  return ( WN_lda_offset(wn) == 0 );
}

// ====================================================================
// Determine if this ldid is a candidate for RVI
// ====================================================================

BOOL
RVI::Is_ldid_candidate( const WN *ldidwn ) const
{
  Warn_todo( "RVI::Is_ldid_candidate: returns true" );
  return TRUE;
}

// ====================================================================
// Determine if this stid is a candidate for RVI
// ====================================================================

BOOL
RVI::Is_stid_candidate( const WN *stidwn ) const
{
  Warn_todo( "RVI::Is_stid_candidate: returns true" );
  return TRUE; 
}

// ====================================================================
// Gather local attributes for the wn assuming we've been processing
// them in forward-statement order
// Also assign bit positions for any candidate constants, as well as
// any additional ldid/stid inserted by first lowering phase (for
// call-related parameter passing)
// ====================================================================

void 
RVI::Get_wn_local_attributes( BB_NODE *bb, WN *wn, BOOL *check_const )
{
  IDX_32 bitpos = ILLEGAL_BP;
  RVI_NODE *rvi_node;

  const OPCODE   opc = WN_opcode(wn);
  const OPERATOR opr = OPCODE_operator(opc);

  // tell our caller not to bother checking for a constant operator
  *check_const = FALSE;

  // process all uses before looking at anything that may kill the
  // upward-exposed use

  // are there any indirect references?
  if ( WN_has_mu(wn, Cfg()->Rgn_level()) ) {
    IDX_32_SET *mu_set = Get_mu_list( wn );
    if ( mu_set != NULL ) {
      // combine all of this statement's mu lists into one attached
      // to the block, and we'll look at this set during dataflow.
      if ( bb->Loc_mu_ref() == NULL ) {
	// steal this wn's mu-list as one to use for the whole block
	bb->Set_loc_mu_ref(mu_set);
      }
      else {
	// combine this set with the block's set
	bb->Loc_mu_ref()->UnionD( mu_set );
      }

      if ( Tracing() ) {
	fprintf( TFile, "<wn_local_attr> Mu refs: " );
	mu_set->Print(TFile);
	fprintf( TFile, "\n" );
	fdump_wn( TFile, wn );
      }
    }
  }

  // don't delve any more than necessary into "black boxes" because
  // their mu/chi lists should be enough to indicate the variable uses
  // and defs.
  if ( ! Black_box(opc) ) {
    // handle kids if necessary, deal with case of constant child
    for ( INT ikid = 0; ikid < WN_kid_count(wn); ikid++ ) {
      WN *kid = WN_kid(wn,ikid);
      BOOL is_const;
#ifdef KEY // bug 12471: __builtin_expect's first kid must be constant
      if (WN_operator(wn) == OPR_INTRINSIC_OP &&
	  ((INTRINSIC) WN_intrinsic(wn)) == INTRN_EXPECT &&
	  ikid == 1)
	continue;
#endif
      Get_wn_local_attributes( bb, kid, &is_const );

      // if this kid was constant, need to do some more checks
      if ( is_const ) {
	if ( WN_operator(kid) == OPR_INTCONST ) {
	  if ( Is_const_candidate( wn, kid, ikid ) ) {
	    rvi_node = Add_to_const_table( kid );
	    rvi_node->Add_reference( bb, TRUE/*is_load*/, Rvi_ppool() );
	    bitpos = rvi_node->Bitpos();
	    bb->Loc_appear()->Union1D( bitpos );
	    bb->Loc_upwd()->Union1D( bitpos );
	  }
	}
	else if ( WN_operator(kid) == OPR_LDA ) {
	  // handle non-Base lda's during phase1
	  if ( Is_lda_candidate( wn, kid, ikid ) ) {
	    rvi_node = Add_to_const_table( kid );
	    rvi_node->Add_reference( bb, TRUE/*is_load*/, Rvi_ppool() );
	    bitpos = rvi_node->Bitpos();
	    bb->Loc_appear()->Union1D( bitpos );
	    bb->Loc_upwd()->Union1D( bitpos );
	  }
	}
	else {
	  FmtAssert( FALSE,
	    ("Unknown is_const operator: %s",
	     OPCODE_name(WN_opcode(kid))) );
	}
      } // end if is_const
    }
  }

  switch ( opr ) {
    case OPR_CONST:
      // these constants are always candidates for our expected targets
      rvi_node = Add_to_const_table( wn );
      rvi_node->Add_reference( bb, TRUE/*is_load*/, Rvi_ppool() );
      bitpos = rvi_node->Bitpos();
      bb->Loc_appear()->Union1D( bitpos );
      bb->Loc_upwd()->Union1D( bitpos );
      break;

    case OPR_INTCONST:
      // don't do much with this node now because we don't know if
      // this integer can be used as an immediate operand in the parent
      // node.
      *check_const = TRUE;
      break;

    case OPR_LDA:
      // see if we want to handle this lda at this time
      if ( ! Is_base_lda( wn ) ) {
	*check_const = TRUE;
      }
      break;

    case OPR_LDID:
      if ( ST_class(WN_st(wn)) == CLASS_PREG ) {
	// we just don't care about pseudo-registers
	break;
      }
      bitpos = Get_bitpos( wn );
      if ( bitpos == ILLEGAL_BP ) {
	// deal with these later
	FmtAssert( FALSE,
	  ("RVI::Get_wn_local_attributes: ldid has no bitpos") );
	break;
      }
      if ( ! Is_ldid_candidate( wn ) ) {
	// warntodo note: should we make this reference an iref, so
	// it stops the live-range, or what?
	Warn_todo( "RVI::Get_wn_local_attributes: not candidate ldid" );
	// map this wn to an illegal bp so we don't handle it again
	Map_bitpos( wn, ILLEGAL_BP );
	break;
      }

      FmtAssert( bitpos != ILLEGAL_BP && bitpos <= Last_varbit(),
	("RVI::Get_wn_local_attributes: LDID bitpos invalid %d",
	 bitpos) );

      rvi_node = Rvi_vtab()->Add_load( wn, bitpos );
      rvi_node->Add_reference( bb, TRUE/*isload*/, Rvi_ppool() );

      bb->Loc_appear()->Union1D( bitpos );

      // can only be upward exposed if we haven't already seen a 
      // local definition of it during our forward scan of the stmts
      if ( ! bb->Loc_def()->MemberP( bitpos ) ) {
	bb->Loc_upwd()->Union1D( bitpos );
      }
      break;

    case OPR_STID:
      if ( ST_class(WN_st(wn)) == CLASS_PREG ) {
	// we just don't care about pseudo-registers
	break;
      }
      bitpos = Get_bitpos( wn );
      if ( bitpos == ILLEGAL_BP ) {
	// deal with these later
	FmtAssert( FALSE,
	  ("RVI::Get_wn_local_attributes: stid has no bitpos") );
	break;
      }

      if ( ! Is_stid_candidate( wn ) ) {
	// warntodo note: should we make this reference an iref, so
	// it stops the live-range, or what?
	Warn_todo( "RVI::Get_wn_local_attributes: not candidate stid" );
	// map this wn to an illegal bp so we don't handle it again
	Map_bitpos( wn, ILLEGAL_BP );
	break;
      }

      FmtAssert( bitpos != ILLEGAL_BP && bitpos <= Last_varbit(),
	("RVI::Get_wn_local_attributes: STID bitpos invalid %d",
	 bitpos) );

      rvi_node = Rvi_vtab()->Add_store( wn, bitpos );
      rvi_node->Add_reference( bb, FALSE/*!isload*/, Rvi_ppool() );
      bb->Loc_appear()->Union1D( bitpos );
      bb->Loc_def()->Union1D( bitpos );

      break;

    default:
      break;
  }

  // are there any indirect definitions?
  if ( WN_has_chi(wn, Cfg()->Rgn_level()) ) {
    IDX_32_SET *chi_set = Get_chi_list( wn );
    if ( chi_set != NULL ) {
      // also if this was a stid, we remove it from the chi set, so
      // its definition can reach other blocks.
      if ( opr == OPR_STID && bitpos != ILLEGAL_BP ) {
	chi_set->Difference1D( bitpos );

	// track stid's that have chi's on them.  By definition, any
	// operation with a chi on it is the last statement in the
	// block.
	bb->Set_last_stid_has_chi( TRUE );
      }

      Is_True( bb->Loc_chi_def() == NULL,
	("RVI::Get_wn_local_attributes: Loc_chi_def already set") );
      bb->Set_loc_chi_def( chi_set );

      if ( Tracing() ) {
	fprintf( TFile, "<wn_local_attr>: " );
	fdump_wn( TFile, wn );
	fprintf( TFile, "  Chi defs: " );
	chi_set->Print(TFile);
	fprintf( TFile, "\n" );
      }
    }
  }

  // track info about the stid that terminates the block
  if ( opr == OPR_STID && bitpos != ILLEGAL_BP ) {
    // determine if this is the last statement, or will become the
    // last statement if the block will be split after we return
    if ( wn == bb->Laststmt() ||
         bb->Loc_mu_ref() != NULL ||
	 bb->Loc_chi_def() != NULL )
    {
      bb->Set_last_stid_bitpos( bitpos );
    }
    else {
      // not the last store, so see if it is considered an "unstored"
      // definition, which means one without a chi attached
      if ( Get_chi_list( wn ) == NULL ) {
	bb->Unstored_defs()->Union1D( bitpos );
      }
    }
  }

}

// ====================================================================
// Gather local attributes for the bb
// ====================================================================

void
RVI::Get_bb_local_attributes( BB_NODE *bb )
{
  // assumed that nobody has allocated these sets yet
  bb->Set_loc_appear( CXX_NEW(IDX_32_SET(Initial_set_size(), 
		      Rvi_ppool(), OPTS_FALSE), Rvi_ppool()));
  bb->Set_loc_def( CXX_NEW(IDX_32_SET(Initial_set_size(), 
		      Rvi_ppool(), OPTS_FALSE), Rvi_ppool()));
  bb->Set_loc_upwd( CXX_NEW(IDX_32_SET(Initial_set_size(), 
		      Rvi_ppool(), OPTS_FALSE), Rvi_ppool()));
  bb->Set_unstored_defs( CXX_NEW(IDX_32_SET(Initial_set_size(), 
		      Rvi_ppool(), OPTS_FALSE), Rvi_ppool()));

  // there are no known indirect references (yet)
  bb->Set_loc_chi_def( NULL );
  bb->Set_loc_mu_ref( NULL );
  bb->Set_loc_mu_wn( NULL );
  bb->Set_last_stid_bitpos( ILLEGAL_BP );
  bb->Set_last_stid_has_chi( FALSE );

  // no annotations yet
  bb->Set_rvi_anns( NULL );

  WN *first = bb->Firststmt();

  if ( first == NULL ) {
    // empty block
  }
  else {
    STMT_ITER stmt_iter;
    WN *wn;
    FOR_ALL_ELEM( wn, stmt_iter, Init(bb->Firststmt(),bb->Laststmt()) ){
      BOOL is_const;
      BOOL need_to_split = FALSE;
      Get_wn_local_attributes( bb, wn, &is_const );
      Is_True( !is_const,
	("RVI::Get_bb_local_attributes: statement can't be constant") );

      if ( bb->Loc_mu_ref() != NULL ) {
	// need to track the statement that had indirect refs, in case
	// it does not remain the last statement in the block
	Is_True( bb->Loc_mu_wn() == NULL,
	  ("RVI::Get_bb_local_attributes: Loc_mu_wn set") );
	bb->Set_loc_mu_wn( wn );
	need_to_split = TRUE;
      }

      if ( bb->Loc_chi_def() != NULL ) {
	// the mu_wn doubles as the chi_wn (((CHANGE NAME)))
	bb->Set_loc_mu_wn( wn );
	need_to_split = TRUE;
      }

      if ( need_to_split && wn != bb->Laststmt() ) {
        Is_True( !WOPT_Enable_Rvisplit,
	  ("RVI::Get_bb_local_attributes: need to split") );

	BB_NODE *newbb = Cfg()->Split_bb_with_wns( bb, wn );
	// the old block doesn't have a call unless the wn is call
	if ( ! OPCODE_is_call(WN_opcode(wn)) )
	    newbb->Pred()->Node()->Reset_hascall();

	if ( Tracing() ) {
	  fprintf( TFile, 
	    "Get_bb_local_attributes: split BB:%d into BB:%d/BB:%d\n",
	    bb->Id(), bb->Id(), newbb->Id() );
	}

	// and we're done looping through this block
	break;
      }
    }

    // also track calls
    if ( bb->Hascall() && bb->Loc_mu_wn() == NULL ) {
      bb->Set_loc_mu_wn( bb->Laststmt() );
    }

  }

#ifdef Is_True_On
  if ( Tracing() ) {
    fprintf( TFile, "BB:%d Loc_appear: ", bb->Id() );
    bb->Loc_appear()->Print(TFile);
    fprintf( TFile, "\n" );
    fprintf( TFile, "BB:%d Loc_def: ", bb->Id() );
    bb->Loc_def()->Print(TFile);
    fprintf( TFile, "\n" );
    fprintf( TFile, "BB:%d Loc_upwd: ", bb->Id() );
    bb->Loc_upwd()->Print(TFile);
    fprintf( TFile, "\n" );
    fprintf( TFile, "BB:%d Unstored_defs: ", bb->Id() );
    bb->Unstored_defs()->Print(TFile);
    fprintf( TFile, "\n" );
    fprintf( TFile, "BB:%d Loc_mu_ref: ", bb->Id() );
    if ( bb->Loc_mu_ref() )
      bb->Loc_mu_ref()->Print(TFile);
    else
      fprintf( TFile, "NULL" );
    fprintf( TFile, "\n" );
    fprintf( TFile, "BB:%d Loc_chi_def: ", bb->Id() );
    if ( bb->Loc_chi_def() )
      bb->Loc_chi_def()->Print(TFile);
    else
      fprintf( TFile, "NULL" );
    fprintf( TFile, "\n" );
    if ( bb->Last_stid_bitpos() != ILLEGAL_BP ) {
      fprintf( TFile, "BB:%d Last_stid_bitpos: %d %s\n", 
		bb->Id(), bb->Last_stid_bitpos(),
		(bb->Last_stid_has_chi() ? "(has chi)" : "") );
    }
  }
#endif

}

// ====================================================================
// this function is used to decide if nth parameter in following intrinsic function need 
// to do RVI optimization. These parameter is address  expression and is offset from
// internal buffer start address.
// ====================================================================

#if defined(TARG_SL)
BOOL 
RVI::Is_Intrncall_Nth_Parm_Need_RVI(INTRINSIC id,  INT nth_parm ) {
  switch(id) {
  case INTRN_C2_LD_C_IMM:
  case INTRN_C2_ST_C_IMM:				
    if(nth_parm == 1) return TRUE;
    return FALSE;
  case INTRN_C2_LD_V2G_IMM:
  case INTRN_C2_ST_G2V_IMM:		
  case INTRN_C2_LD_G_IMM:
  case INTRN_C2_ST_G_IMM:			
    if(nth_parm == 2) return TRUE;
    return FALSE;
  case INTRN_C2_ST_V_IMM:
    if(nth_parm == 3) return TRUE;
    return FALSE;
  case INTRN_C2_LD_V_IMM:
    if(nth_parm == 4) return TRUE;
    return FALSE;
  default:
    return FALSE;
  }
  return FALSE;
}
#endif

// ====================================================================
// Gather local lda attributes for the wn assuming we've been processing
// them in forward-statement order
// Also assign bit positions for any candidate constants
// If the WN is an LDA, set "*check_lda=true"
// ====================================================================

void 
RVI::Get_wn_local_lda_attributes( BB_NODE *bb, WN *wn, BOOL *check_lda )
{
  IDX_32 bitpos;
  RVI_NODE *rvi_node;

  // tell our caller not to bother checking for an LDA
  *check_lda = FALSE;

  const OPCODE   opc = WN_opcode(wn);
  const OPERATOR opr = OPCODE_operator(opc);

  // don't delve any more than necessary into "black boxes" because
  // their mu/chi lists should be enough to indicate the variable uses
  // and defs.
  if ( ! Black_box(opc) ) {
    // handle kids if necessary, deal with case of constant child
    for ( INT ikid = 0; ikid < WN_kid_count(wn); ikid++ ) {
      BOOL is_lda;

#ifdef TARG_SL
      // the parameter one in the two intrinsic functions are used as offset relative to 
      // vbuf start address. We don't want to these two parameter to be screening out
      // since we need allocate special handling when expanding the intrinsic call 
     if( (opr==OPR_INTRINSIC_CALL || opr == OPR_INTRINSIC_OP)  && 
	 Is_Intrncall_Nth_Parm_Need_RVI(WN_intrinsic(wn), ikid)) 
       continue; 
#endif 
      Get_wn_local_lda_attributes( bb, WN_kid(wn,ikid), &is_lda );

      // if this kid was constant, need to do some more checks
      if ( is_lda ) {
	if ( Is_lda_candidate( wn, WN_kid(wn,ikid), ikid ) ) {
	  rvi_node = Add_to_const_table( WN_kid(wn,ikid) );
	  rvi_node->Add_reference( bb, TRUE/*is_load*/, Rvi_ppool() );
	  bitpos = rvi_node->Bitpos();
	  bb->Loc_appear()->Union1D( bitpos );
	}
      }
    }
  }

  if ( opr == OPR_LDA ) {
    // handle Base lda's during phase2
    if ( Is_base_lda( wn ) ) {
      *check_lda = TRUE;
    }
  }

}

// ====================================================================
// Gather local LDA attributes for the bb
// ====================================================================

void
RVI::Get_bb_local_lda_attributes( BB_NODE *bb )
{
  // assumed that nobody has allocated these sets yet
  bb->Set_loc_appear( CXX_NEW(IDX_32_SET(Initial_set_size(), 
		      Rvi_ppool(), OPTS_FALSE), Rvi_ppool()));
  bb->Set_loc_def( NULL );
  bb->Set_loc_upwd( NULL );
  bb->Set_unstored_defs( NULL );

  bb->Set_loc_chi_def( NULL );
  bb->Set_loc_mu_ref( NULL );
  bb->Set_loc_mu_wn( NULL );
  bb->Set_last_stid_bitpos( ILLEGAL_BP );
  bb->Set_last_stid_has_chi( FALSE );

  // no annotations yet
  bb->Set_rvi_anns( NULL );

  WN *first = bb->Firststmt();

  if ( first == NULL ) {
    // empty block
  }
  else {
    STMT_ITER stmt_iter;
    WN *wn;
    FOR_ALL_ELEM( wn, stmt_iter, Init(bb->Firststmt(),bb->Laststmt()) ){
      BOOL is_lda;
      Get_wn_local_lda_attributes( bb, wn, &is_lda );
      Is_True( !is_lda,
	("RVI::Get_bb_local_lda_attributes: statement can't be LDA") );
    }

    // also track calls
    if ( bb->Hascall() && bb->Loc_mu_wn() == NULL ) {
      bb->Set_loc_mu_wn( bb->Laststmt() );
    }
  }


  if ( Tracing() ) {
    fprintf( TFile, "BB:%d Loc_appear: ", bb->Id() );
    bb->Loc_appear()->Print(TFile);
    fprintf( TFile, "\n" );
  }
}

// ====================================================================
// Gather local attributes for the cfg
// ====================================================================

void
RVI::Get_local_attributes( BOOL just_lda )
{
  CFG_ITER cfg_iter(Cfg());

  if ( ! just_lda ) {
    BB_NODE *bb;
    FOR_ALL_NODE( bb, cfg_iter, Init() ) {
      Get_bb_local_attributes( bb );
    }
  }
  else {
    // handle only LDAs
    BB_NODE *bb;
    FOR_ALL_NODE( bb, cfg_iter, Init() ) {
      Get_bb_local_lda_attributes( bb );
    }
  }
}

// ====================================================================
// Gather the forward dataflow information given the local attributes
// NOTE: 
// Defreach() for each block should have been initialized to that 
// block's Loc_appear()
// Unstored_defs() for each block should have been initialized to the
// set of STIDs that do not terminate the block (during local attr).
// ====================================================================

void
RVI::Get_forward_dataflow( void )
{
  IDX_32_SET save_set(Initial_set_size(), Rvi_lpool(), OPTS_FALSE);

  BOOL changed;
  do {
    changed = FALSE;

    for ( INT bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
      BB_NODE *bb = Dfs_vec(bbi);
      BB_LIST_ITER bb_pred_iter;
      BB_NODE *pred;

      if ( ! changed )
	save_set.CopyD( bb->Defreach() );

      FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) {
	IDX_32_SET *chi_set = Has_end_chi_list(pred) ? 
				 Bb_end_chi_list(pred) : NULL;
	if ( chi_set != NULL ) {
	  // defreach += defreach(preds) - chi_set(pred)
	  bb->Defreach()->Bs_2_3_Minus_1_Or_D( pred->Defreach(), chi_set );
	}
	else {
	  // defreach += defreach(preds)
	  bb->Defreach()->UnionD( pred->Defreach() );
	}
      }

      if ( ! changed )
	changed = ! save_set.EqualP( bb->Defreach() );
    }
  } while ( changed );

  // propagate the Unstored_defs() set, which contains all definitions
  // that reach the final reference in the block without having been
  // stored previously.  We decide that all defs are stored when they
  // are indirectly referenced in a mu, or are attached to stores with
  // chi's.
  IDX_32_SET temp_set(Initial_set_size(), Rvi_lpool(), OPTS_FALSE);
  do {
    changed = FALSE;

    for ( INT bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
      BB_NODE *bb = Dfs_vec(bbi);
      BB_LIST_ITER bb_pred_iter;
      BB_NODE *pred;

      if ( ! changed )
	save_set.CopyD( bb->Unstored_defs() );

      // unstored += unstored(pred) - mu(pred) - chi(pred) +
      //             last_stid_with_no_chi(pred)
      FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) {
	temp_set.CopyD( pred->Unstored_defs() );
	
	IDX_32_SET *pred_mu = Bb_end_mu_list(pred);
	if ( pred_mu != NULL ) 
	  temp_set.DifferenceD( pred_mu );

	IDX_32_SET *pred_chi = Bb_end_chi_list(pred);
	if ( pred_chi != NULL ) 
	  temp_set.DifferenceD( pred_chi );

	if ( pred->Last_stid_bitpos() != ILLEGAL_BP &&
	     ! pred->Last_stid_has_chi() )
	  temp_set.Union1D( pred->Last_stid_bitpos() );

	bb->Unstored_defs()->UnionD( &temp_set );
      }

      if ( ! changed )
	changed = ! save_set.EqualP( bb->Unstored_defs() );
    }
  } while ( changed );
}

// ====================================================================
// Gather the backward dataflow information given the local attributes
// NOTE: Assumed that live_at_exit() is initialized to "Loc_appear"
//       Assumed that live_out() is initialized to empty
//       Assumed that bbs_reached() is initialized to include only bb
// ====================================================================

void
RVI::Get_backward_dataflow( void )
{
  IDX_32_SET save_set(Initial_set_size(), Rvi_lpool(), OPTS_FALSE);
  BB_NODE_SET bb_set(Cfg()->Total_bb_count(), Cfg(), Rvi_lpool(), BBNS_EMPTY);

  // initialize live-at-exit += loc_upwd(succ) - chi_def(bb)
  // initialize live-out = (mu_ref(succ)-loc_def(succ))+
  //			   loc_upwd(succ)+mu_ref(bb)
  INT bbi;
  for (bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
    BB_NODE *bb = Dfs_vec(bbi);
    BB_LIST_ITER bb_succ_iter;
    BB_NODE *succ;

    // does this bb have a chi that blocks things coming from succ?
    IDX_32_SET *chi_set = Has_end_chi_list(bb) ? 
				  Bb_end_chi_list(bb) : NULL;
    // does this bb have a mu that indirectly references values?
    IDX_32_SET *mu_set = Has_end_mu_list(bb) ? 
				  Bb_end_mu_list(bb) : NULL;

    if ( mu_set != NULL ) {
      FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {

        // initialize live-at-exit += loc_upwd(succ) - chi_def(bb)
	if ( chi_set != NULL ) {
	  bb->Live_at_exit()->Bs_2_3_Minus_1_Or_D( succ->Loc_upwd(), 
						   chi_set );
	}
	else {
	  bb->Live_at_exit()->UnionD( succ->Loc_upwd() );
	}

	IDX_32_SET *succ_mu_set = Has_end_mu_list(succ) ? 
					Bb_end_mu_list(succ) : NULL;
	if ( succ_mu_set != NULL ) {
	  // both this block and succ have mu's
	  // lo += (mu_ref(succ)-def(succ))+loc_upwd(succ)+mu_ref(bb)
	  bb->Live_out()->Bs_3_2_Minus_4_Or_5_Or_1_Or_D(succ->Loc_def(),
	    succ_mu_set, mu_set, succ->Loc_upwd() );
	}
	else {
	  // just this block has a mu
	  // lo += loc_upwd(succ)+mu_ref(bb)
	  bb->Live_out()->Bs_2_3_Or_1_Or_D(succ->Loc_upwd(),mu_set);
	}
      }
    }
    else {
      // no mu-refs for this block, so just pick up some things from
      // successors
      FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
	bb->Live_at_exit()->UnionD( succ->Loc_upwd() );

	IDX_32_SET *succ_mu_set = Has_end_mu_list(succ) ? 
					Bb_end_mu_list(succ) : NULL;
	if ( succ_mu_set != NULL ) {
	  // just succ has mu_ref
	  // lo += (mu_ref(succ)-def(succ))+loc_upwd(succ)
	  bb->Live_out()->Bs_3_2_Minus_4_Or_1_Or_D( succ->Loc_def(),
	    succ_mu_set, succ->Loc_upwd() );
	}
	else {
	  // succ has no mu-ref either, so just pick up upward-exposed
	  // lo += loc_upwd(succ)
	  bb->Live_out()->UnionD( succ->Loc_upwd() );
	}
      }
    }

    // blocks that leave the region need to note which values are
    // Live_out.
    if ( Is_exit_block(bb) ) {
      const IDX_32_SET *global_vars = Global_vars(bb);
      if ( global_vars != NULL ) {
	bb->Live_out()->UnionD( global_vars );
      }
    }
  }

  if ( Tracing() ) {
    for ( INT bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
      BB_NODE *bb = Dfs_vec(bbi);
      fprintf( TFile, "<RVI::Get_backward_dataflow: bb:%d init l-a-e:   ",
		bb->Id() );
      bb->Live_at_exit()->Print( TFile );
      fprintf( TFile, "\n" );
      fprintf( TFile, "<RVI::Get_backward_dataflow: bb:%d init live-out:",
		bb->Id() );
      bb->Live_out()->Print( TFile );
      fprintf( TFile, "\n" );
    }
  }


  // propagate the live-at-exit sets backwards
  //
  BOOL changed;
  do {
    changed = FALSE;

    for ( bbi = Dfs_vec_size()-1; bbi >= 0; bbi-- ) {
      BB_NODE *bb = Dfs_vec(bbi);
      BB_LIST_ITER bb_succ_iter;
      BB_NODE *succ;

      if ( ! changed )
	save_set.CopyD( bb->Live_at_exit() );

      // does this bb have a chi that blocks things coming from succs
      IDX_32_SET *chi_set = Has_end_chi_list(bb) ? 
				Bb_end_chi_list(bb) : NULL;

      if ( chi_set == NULL ) {
	FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
	  // live-at-exit |= l-a-e(succs) - loc_def(succ)
	  bb->Live_at_exit()->Bs_2_3_Minus_1_Or_D( succ->Live_at_exit(),
						   succ->Loc_def() );
	}
      }
      else {
	FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
	  // chi's block values from successors
	  // live-at-exit |= l-a-e(succs) - loc_def(succ) - chi_def(bb)
	  bb->Live_at_exit()->Bs_2_3_Minus_4_Minus_1_Or_D(
				succ->Live_at_exit(),
				succ->Loc_def(), chi_set );
	}
      }

      if ( ! changed )
	changed = ! save_set.EqualP( bb->Live_at_exit() );
    }
  } while ( changed );


  // propagate the live-out sets backwards
  //
  do {
    changed = FALSE;

    for ( bbi = Dfs_vec_size()-1; bbi >= 0; bbi-- ) {
      BB_NODE *bb = Dfs_vec(bbi);
      BB_LIST_ITER bb_succ_iter;
      BB_NODE *succ;

      if ( ! changed )
	save_set.CopyD( bb->Live_out() );

      FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
	// live-out += live-out(succs) - loc_def(succ)
	bb->Live_out()->Bs_2_3_Minus_1_Or_D( succ->Live_out(), 
					     succ->Loc_def() );
      }

      if ( ! changed )
	changed = ! save_set.EqualP( bb->Live_out() );
    }
  } while ( changed );

}

// ====================================================================
// Gather the dataflow information given the local attributes
// ====================================================================

void
RVI::Get_dataflow_equations( void )
{
  INT bbi;
  for ( bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
    BB_NODE *bb = Dfs_vec(bbi);

    // resize the sets that may not be large enough to match the
    // latest size.
    Resize_sets( bb, Initial_set_size() );

    // create dataflow sets with largest necessary size, and then
    // initialize them as appropriate

    // reaching definitions start out with "appear"
    bb->Set_defreach( CXX_NEW(IDX_32_SET(Initial_set_size(), 
		      Rvi_ppool(), OPTS_DONT_CARE), Rvi_ppool()));
    bb->Defreach()->CopyD( bb->Loc_appear() );

    // live-at-exit starts out with "appear"
    bb->Set_live_at_exit( CXX_NEW(IDX_32_SET(Initial_set_size(), 
		      Rvi_ppool(), OPTS_DONT_CARE), Rvi_ppool()));
    bb->Live_at_exit()->CopyD( bb->Loc_appear() );

    // live-out starts out empty
    bb->Set_live_out( CXX_NEW(IDX_32_SET(Initial_set_size(), 
		      Rvi_ppool(), OPTS_FALSE), Rvi_ppool()));
  }

  Get_forward_dataflow();
  Get_backward_dataflow();

  if ( Tracing() ) {
    for ( INT bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
      BB_NODE *bb = Dfs_vec(bbi);

      fprintf ( TFile, "BB:%d Defreach: ", bb->Id() );
      bb->Defreach()->Print(TFile);
      fprintf ( TFile, "\n" );
      fprintf ( TFile, "BB:%d Live_at_exit: ", bb->Id() );
      bb->Live_at_exit()->Print(TFile);
      fprintf ( TFile, "\n" );
      fprintf ( TFile, "BB:%d Live_out: ", bb->Id() );
      bb->Live_out()->Print(TFile);
      fprintf ( TFile, "\n" );
      fprintf ( TFile, "BB:%d Unstored_defs: ", bb->Id() );
      bb->Unstored_defs()->Print(TFile);
      fprintf ( TFile, "\n" );
    }
  }

  // save ourselves time later by intersecting defreach and live
  // leaving result in defreach 
  for ( bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
    BB_NODE *bb = Dfs_vec(bbi);
    bb->Defreach()->IntersectionD( bb->Live_at_exit() );

    if ( Tracing() ) {
      fprintf ( TFile, "BB:%d Live-range (defreach): ", bb->Id() );
      bb->Defreach()->Print(TFile);
      fprintf ( TFile, "\n" );
    }
  }

}

// ====================================================================
// Gather the forward dataflow information given the local lda 
// attributes
// NOTE: Defreach() for each block should have been initialized to
// that block's Loc_appear().
// ====================================================================

void
RVI::Get_forward_lda_dataflow( void )
{
  IDX_32_SET save_set(Initial_set_size(), Rvi_lpool(), OPTS_FALSE);

  BOOL changed;
  do {
    changed = FALSE;

    for ( INT bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
      BB_NODE *bb = Dfs_vec(bbi);
      BB_LIST_ITER bb_pred_iter;
      BB_NODE *pred;

      if ( ! changed )
	save_set.CopyD( bb->Defreach() );

      FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) ) {
	// defreach = defreach(preds)
	bb->Defreach()->UnionD( pred->Defreach() );
      }

      if ( ! changed )
	changed = ! save_set.EqualP( bb->Defreach() );
    }
  } while ( changed );
}

// ====================================================================
// Gather the backward dataflow information given the local lda
// attributes
// NOTE: Assumed that live_at_exit() is initialized to "Loc_appear"
// ====================================================================

void
RVI::Get_backward_lda_dataflow( void )
{
  IDX_32_SET save_set(Initial_set_size(), Rvi_lpool(), OPTS_FALSE);

  // initialize live-at-exit += loc_appear(succ)
  INT bbi;
  for ( bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
    BB_NODE *bb = Dfs_vec(bbi);
    BB_LIST_ITER bb_succ_iter;
    BB_NODE *succ;

    FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
      bb->Live_at_exit()->UnionD( succ->Loc_appear() );
    }
  }

  if ( Tracing() ) {
    for ( INT bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
      BB_NODE *bb = Dfs_vec(bbi);
      fprintf( TFile, "<RVI::Get_backward_dataflow: init l-a-e:bb:%d:",
		bb->Id() );
      bb->Live_at_exit()->Print( TFile );
      fprintf( TFile, "\n" );
    }
  }

  // propagate the live-at-exit sets backwards
  BOOL changed;
  do {
    changed = FALSE;

    for ( bbi = Dfs_vec_size()-1; bbi >= 0; bbi-- ) {
      BB_NODE *bb = Dfs_vec(bbi);
      BB_LIST_ITER bb_succ_iter;
      BB_NODE *succ;

      if ( ! changed )
	save_set.CopyD( bb->Live_at_exit() );

      FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) ) {
	// live-at-exit |= l-a-e(succs) - loc_def(succ)
	bb->Live_at_exit()->UnionD( succ->Live_at_exit() );
      }

      if ( ! changed )
	changed = ! save_set.EqualP( bb->Live_at_exit() );
    }
  } while ( changed );

}

// ====================================================================
// Gather the dataflow information given the local lda attributes
// ====================================================================

void
RVI::Get_lda_dataflow_equations( void )
{
  INT bbi;
  for ( bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
    BB_NODE *bb = Dfs_vec(bbi);

    // create dataflow sets with largest necessary size, and then
    // initialize them as appropriate

    // reaching definitions start out with "appear"
    bb->Set_defreach( CXX_NEW(IDX_32_SET(Initial_set_size(), 
		      Rvi_ppool(), OPTS_DONT_CARE), Rvi_ppool()));
    bb->Defreach()->CopyD( bb->Loc_appear() );

    // live-at-exit starts out with "appear"
    bb->Set_live_at_exit( CXX_NEW(IDX_32_SET(Initial_set_size(), 
		      Rvi_ppool(), OPTS_DONT_CARE), Rvi_ppool()));
    bb->Live_at_exit()->CopyD( bb->Loc_appear() );

    bb->Set_live_out( NULL );
  }

  Get_forward_lda_dataflow();
  Get_backward_lda_dataflow();

  if ( Tracing() ) {
    for ( INT bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
      BB_NODE *bb = Dfs_vec(bbi);

      fprintf ( TFile, "BB:%d Defreach: ", bb->Id() );
      bb->Defreach()->Print(TFile);
      fprintf ( TFile, "\n" );
      fprintf ( TFile, "BB:%d Live_at_exit: ", bb->Id() );
      bb->Live_at_exit()->Print(TFile);
      fprintf ( TFile, "\n" );
    }
  }

  // save ourselves time later by intersecting defreach and live
  // leaving result in defreach 
  for ( bbi = 0; bbi < Dfs_vec_size(); bbi++ ) {
    BB_NODE *bb = Dfs_vec(bbi);
    bb->Defreach()->IntersectionD( bb->Live_at_exit() );

    if ( Tracing() ) {
      fprintf ( TFile, "BB:%d Live-range (defreach): ", bb->Id() );
      bb->Defreach()->Print(TFile);
      fprintf ( TFile, "\n" );
    }
  }

}


// ====================================================================
// Do RVI for variables or constants (common code for other routines)
// ====================================================================

void
RVI::Perform_variable_constant_rvi( RVI_NODE *rvi_node )
{
  OPT_POOL_Push( Rvi_lpool(), RVI_TRACE_FLAG+6 );
  Build_live_ranges( rvi_node, Rvi_lpool() );

  INT32 home_preg = 0;

  RVI_LR_ITER lr_iter;
  RVI_LR *lr;
  FOR_ALL_NODE( lr, lr_iter, Init( rvi_node->Live_ranges() ) ) {
    Analyze_live_range( lr );

    if ( Tracing() ) {
      fprintf( TFile, "After Analyze_live_range for rvi_node\n" );
      lr->Print( TFile );
    }

    // are we going to do anything?
    if ( lr->Replace_anything() ) {

      // assign a preg
      if ( lr->Need_home() || !Unique_pregs() ) {
	// give this live-range the same one as others
	if ( home_preg == 0 ) {
	  home_preg = Create_Preg(rvi_node->Mtype(), rvi_node->Name(), 
				  rvi_node->New_home_wn( Alias_Mgr() ) );
	}

	lr->Set_preg( home_preg );
      }
      else {
	// this live-range gets a unique preg
	lr->Set_preg( Create_Preg(rvi_node->Mtype(), rvi_node->Name(),
				  rvi_node->New_home_wn( Alias_Mgr() ) ) );
      }

      Insert_loads_stores( lr, rvi_node );

      if ( Tracing() ) {
	fprintf( TFile, "After Insert_loads_stores for rvi_node\n" );
	lr->Print( TFile );
      }
    }

  }

  OPT_POOL_Pop( Rvi_lpool(), RVI_TRACE_FLAG+6 );
}

// ====================================================================
// Do RVI for variables
// ====================================================================

void
RVI::Perform_variable_rvi( void )
{
  RVI_VTAB_ITER vtab_iter;
  RVI_NODE *rvi_node;
  FOR_ALL_NODE( rvi_node, vtab_iter, Init( Rvi_vtab() ) ) {
    if ( Tracing() ) {
      fprintf( TFile, "Perform_variable_rvi: " );
      rvi_node->Print( TFile );
    }

    // don't process skipped variables
    if ( WOPT_Enable_Rviskip != NULL ) {
      char *name = NULL;
      if ( rvi_node->Loadwn() ) {
	name = ST_name(WN_st(rvi_node->Loadwn()));
      }
      else if ( rvi_node->Storewn() ) {
	name = ST_name(WN_st(rvi_node->Storewn()));
      }

      if ( name && strcmp(WOPT_Enable_Rviskip,name) == 0 ) {
	DevWarn( "RVI skip variable %s", name );
	if ( Tracing() ) {
	  fprintf( TFile, "  SKIP: rvskip'd variable\n" );
	}
	continue;
      }
    }

    // don't mess with volatile variables
    if ( Volatile_set()->MemberP(rvi_node->Bitpos()) ) {
      if ( Tracing() ) {
	fprintf( TFile, "  SKIP: Is_volatile\n" );
      }
      continue;
    }

    // don't mess with parameters in multi-entry point functions
    if (Cfg()->Fake_entry_bb()) {
      if (ST_sclass(rvi_node->St()) == SCLASS_FORMAL_REF) {
	if ( Tracing() ) {
	  fprintf( TFile, "  SKIP: multi-entry point parameter %s\n", rvi_node->Name() );
        }
	continue;
      }
    }

    Is_True(!rvi_node->Is_volatile(), ("RVI::Perform_variable_rvi: Find volatile rvi_node."));

    Perform_variable_constant_rvi( rvi_node );
  }
}

// ====================================================================
// Do RVI for constants
// ====================================================================

void
RVI::Perform_constant_rvi( void )
{
  RVI_CTAB_ITER ctab_iter;
  RVI_NODE *rvi_node;
  
  FOR_ALL_NODE( rvi_node, ctab_iter, Init(Rvi_ctab()) ) {

    if ( Tracing() ) {
      fprintf( TFile, "Perform_constant_rvi: " );
      rvi_node->Print( TFile );
    }

    Perform_variable_constant_rvi( rvi_node );
  }
}

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Description:
// Simulate itable bit positions for variables for use by RVI.
//
// ====================================================================
// ====================================================================

void
PRE_RVI_HOOKS::Setup_bitpos(OPT_STAB *opt_stab,
			    CODEREP *cr)
{
  if (opt_stab->Aux_stab_entry(cr->Aux_id())->Itab_bitpos() ==
      ILLEGAL_BP) {
    Is_Trace(Tracing(), (TFile, "Setup_bitpos: assigning bitpos %d "
			 "to aux_id %d\n", Nbits(),
			 cr->Aux_id()));

    opt_stab->Set_itab_bitpos(cr->Aux_id(), Nbits());
    Inc_nbits();
  }
  else {
    Is_Trace(Tracing(),
	     (TFile, "Setup_bitpos: Aux id %d already has bitpos %d\n",
	      cr->Aux_id(),
	      opt_stab->Aux_stab_entry(cr->Aux_id())->Itab_bitpos()));
  }
  Is_Trace_cmd(Tracing(), cr->Print(1, TFile));
  Is_Trace(Tracing(),
	   (TFile, "     ^^^ coderep assigned bitpos %d\n",
	    opt_stab->Aux_stab_entry(cr->Aux_id())->Itab_bitpos()));
  
  cr->Set_Bitpos(opt_stab->Aux_stab_entry(cr->Aux_id())->Itab_bitpos());
}

// Go through the whole PU one time and map each CK_VAR coderep we
// encounter to a bit position index that corresponds to its Aux_id().

PRE_RVI_HOOKS::PRE_RVI_HOOKS(OPT_STAB *opt_stab,
			     CFG      *cfg,
			     MEM_POOL *pool,
			     BOOL      tracing) : _tracing(tracing)
{
  CFG_ITER  bb_iter;
  BB_NODE  *bb;

  opt_stab->Clear_itab_bitpos();

  _nbits = 0;

  OPT_POOL_Push(pool, RVI_TRACE_FLAG+7);

  FOR_ALL_ELEM(bb, bb_iter, Init(cfg)) {
    STMTREP_ITER  stmt_iter(bb->Stmtlist());
    STMTREP      *stmt;

    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      LEAF_ITER<CODEREP> leaf_iter(pool, Tracing());
      CODEREP           *leaf;

      if (stmt->Lhs() != NULL) {
	FOR_ALL_NODE(leaf, leaf_iter, Init(stmt->Lhs())) {
	  if (leaf->Kind() == CK_VAR) {
	    // Need a bit position for this leaf.
	    Setup_bitpos(opt_stab, leaf);
	  }
	}
      }
      if (stmt->Rhs() != NULL) {
	FOR_ALL_NODE(leaf, leaf_iter, Init(stmt->Rhs())) {
	  if (leaf->Kind() == CK_VAR) {
	    // Need a bit position for this leaf.
	    Setup_bitpos(opt_stab, leaf);
	  }
	}
      }
    }
  }

  OPT_POOL_Pop(pool, RVI_TRACE_FLAG+7);

  opt_stab->Rename_aux_id_list_to_bitpos();
}
