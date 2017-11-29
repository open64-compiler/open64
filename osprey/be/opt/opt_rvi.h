//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_rvi.h
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:55-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/opt/SCCS/s.opt_rvi.h $
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
// (RVI) Register-Variable Identification
//
// Perform analysis of program to promote memory-based scalar variables
// to register-based variables.
//
// ====================================================================
// ====================================================================


#ifndef opt_rvi_INCLUDED
#define opt_rvi_INCLUDED "opt_rvi.h"
#ifdef _KEEP_RCS_ID
static char *opt_rvircs_id = opt_rvi_INCLUDED"$ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#ifndef wn_INCLUDED
#include "wn.h"
#endif // wn_INCLUDED
#ifndef mempool_INCLUDED
#include "mempool.h"
#endif // mempool_INCLUDED

// class declarations
class ALIAS_MANAGER;
class BB_NODE_SET;
class CFG;
class CHI_LIST;
class IDX_32_SET;
class MU_NODE;
class MU_LIST;
class OPT_STAB;
class RVI_CTAB;
class RVI_LR;
class RVI_LR_LIST;
class RVI_LRBB;
class RVI_LRBB_LIST;
class RVI_NODE;
class RVI_NOMAP_LIST;
class RVI_VTAB;

// Main class for Register Variable Identification
class RVI {
friend class RVI_EMIT;
friend class RVI_LR;
private:
  static const INT32 SET_SIZE_INCREMENT = 32; // extra set space for growth
  WN_MAP	 _mu_map;	// map of WN -> mu set
  WN_MAP	 _chi_map;	// map of WN -> chi set
  WN_MAP	 _bp_map;	// map of WN -> bit position
  MEM_POOL	 _rvi_gpool;	// used for global allocation
  MEM_POOL	*_rvi_gpoolp;	// used for global allocation
  MEM_POOL	 _rvi_ppool;	// used for per-phase allocation
  MEM_POOL	*_rvi_ppoolp;	// used for per-phase allocation
  MEM_POOL	 _rvi_lpool;	// used for local allocation
  MEM_POOL	*_rvi_lpoolp;	// used for local allocation
  BOOL		 _do_rvi;	// did we perform RVI?
  BOOL		 _do_base_lda;	// are we now handling base ldas
  BOOL		 _unique_pregs;	// do we want unique pregs for LRs
  BOOL		 _tracing;	// are we debugging
  IDX_32	 _max_bitpos;	// maximum "bitpos" of rvi candidates
  IDX_32	 _last_varbit;	// last "bitpos" assigned to vars
  IDX_32	 _init_set_size;// number of bits to init bitsets with
  RVI_CTAB	*_rvi_ctab;	// internal constant-table
  RVI_VTAB	*_rvi_vtab;	// internal variable-table
  CFG		*_cfg;		// control-flow graph
  BB_NODE      **_dfs_vec;	// depth-first ordering of cfg bbs
  INT		 _dfs_vec_size;	// number of blocks in dfs_vec
  OPT_STAB	*_opt_stab;	// symbol table
  ALIAS_MANAGER *_alias_mgr;    // alias manager
  IDX_32_SET    *_volatile_set; // bitset of volatile vars

  // track where we've already inserted code, so we don't do it
  // multiple times.
  struct {
    BB_NODE_SET *_load_top;	// load inserted at top
    BB_NODE_SET *_load_bot;	// load inserted at bottom
    BB_NODE_SET *_store_top;	// store inserted at top
    BB_NODE_SET *_store_iref;	// store inserted at bottom, before iref
  } _redundant;

  //
  // private access methods
  //

  // track bit positions assigned either by mainopt or rvi
  IDX_32 Max_bitpos(void) const
		{ return _max_bitpos; }
  IDX_32 Next_bitpos(void)
		{ return ++_max_bitpos; }
  IDX_32 Last_varbit(void) const
		{ return _last_varbit; }
  IDX_32 Initial_set_size(void)
		{ if ( _init_set_size <= _max_bitpos )
		    _init_set_size = _max_bitpos+SET_SIZE_INCREMENT;
		  return _init_set_size;
		}
  // are we now processing base ldas?
  BOOL Do_base_lda(void) const
		{ return _do_base_lda; }
  // do we want to use unique pregs for individual live-ranges?
  BOOL Unique_pregs(void) const
		{ return _unique_pregs; }
  // are we debugging
  BOOL Tracing(void) const
		{ return _tracing; }

  // map of WN -> mu set
  WN_MAP Mu_map(void) const
		{ return _mu_map; }
  IDX_32_SET *Get_mu_list( const WN *wn ) const
		{ return (IDX_32_SET*)WN_MAP_Get( Mu_map(), wn ); } 
  // mu list attached to statement at end of block
  BOOL Has_end_mu_list( const BB_NODE *bb ) const
		{ return bb->Loc_mu_ref() != NULL; }
  IDX_32_SET *Bb_end_mu_list( const BB_NODE *bb ) const;

  // map of WN -> chi set
  WN_MAP Chi_map(void) const
		{ return _chi_map; }
  IDX_32_SET *Get_chi_list( const WN *wn ) const
		{ return (IDX_32_SET*)WN_MAP_Get( Chi_map(), wn ); } 
  // chi list attached to statement at end of block
  BOOL Has_end_chi_list( const BB_NODE *bb ) const
		{ return bb->Loc_chi_def() != NULL; }
  IDX_32_SET *Bb_end_chi_list( const BB_NODE *bb ) const;

  // resize sets so they all have same length
  void Resize_sets( BB_NODE *bb, INT32 new_size ) const;

  // map of WN -> bit position
  WN_MAP Bp_map(void) const
		{ return _bp_map; }
  // internal constant-table
  RVI_CTAB *Rvi_ctab(void) const
		{ return _rvi_ctab; }
  // internal variable-table
  RVI_VTAB *Rvi_vtab(void) const
		{ return _rvi_vtab; }
  // control-flow graph
  CFG *Cfg(void) const
		{ return _cfg; }
  // depth-first ordering of blocks
  BB_NODE *Dfs_vec( INT i ) const
		{ return _dfs_vec[i]; }
  INT Dfs_vec_size( void ) const
		{ return _dfs_vec_size; }

  // symbol table
  OPT_STAB *Opt_stab( void ) const
		{ return _opt_stab; }

  // alias manager
  ALIAS_MANAGER *Alias_Mgr( void ) const
                { return _alias_mgr; }

  // volatile bitset
  IDX_32_SET    *Volatile_set( void ) const
                { return _volatile_set; } 

  // track where we've already inserted code, so we don't do it
  // multiple times.
  BB_NODE_SET *Redundant_load_top( void ) const
		{ return _redundant._load_top; }
  BB_NODE_SET *Redundant_load_bot( void ) const
		{ return _redundant._load_bot; }
  BB_NODE_SET *Redundant_store_top( void ) const
		{ return _redundant._store_top; }
  BB_NODE_SET *Redundant_store_iref( void ) const
		{ return _redundant._store_iref; }
  // clear all of the redundant sets
  void Init_redundant( MEM_POOL *pool );
  void Clear_redundant( void ) const;

  //
  // private functions
  //

  // calculate the local attributes for each block in the cfg
  // The flag indicates whether we just find attrs for lda's
  void Get_local_attributes( BOOL just_lda );
  // calculate the local attributes for the given block
  void Get_bb_local_attributes( BB_NODE *bb );
  // calculate the local attributes for the given wn statement
  void Get_wn_local_attributes( BB_NODE *bb, WN *wn, BOOL *check_const);

  // calculate the local lda attributes for the given block
  void Get_bb_local_lda_attributes( BB_NODE *bb );
  // calculate the local lda attributes for the given wn statement
  void Get_wn_local_lda_attributes( BB_NODE *bb, WN *wn, BOOL *check_lda );

  // calculate the data-flow equations
  void Get_dataflow_equations( void );
  void Get_forward_dataflow( void );
  void Get_backward_dataflow( void );

  // calculate the data-flow equations for just LDAs
  void Get_lda_dataflow_equations( void );
  void Get_forward_lda_dataflow( void );
  void Get_backward_lda_dataflow( void );

  // determine if the block has no successors in the region being
  // compiled
  BOOL Is_exit_block( const BB_NODE *bb ) const;

  // the set of values live-out of "Is_exit_block" blocks.  May return
  // null if none.
  const IDX_32_SET *Global_vars( const BB_NODE *bb ) const;

  // add the constant node (const,intconst,lda) to constant table,
  // and return it
  RVI_NODE *Add_to_const_table( WN *wn );

  // determine if this lda is a "base" lda
  BOOL Is_base_lda( const WN *wn ) const;

  // determine if the constant should be a candidate for RVI
  BOOL Is_const_candidate( const WN *parent, const WN *constant, INT whichkid ) const;

  // determine if the lda should be a candidate for RVI
  BOOL Is_lda_candidate( const WN *parent, const WN *lda, INT whichkid ) const;

  // determine if the ldid/stid should be candidate for RVI
  BOOL Is_ldid_candidate( const WN *ldidwn ) const;
  BOOL Is_stid_candidate( const WN *stidwn ) const;

  // find cycles in the cfg and set the loop-nest level for each bb
  // NOTE: this should go away once we get valid loop info from CFG
  void Find_loops( void ) const;

  // make sure dedicated pregs defined by function calls are stored
  // to pregs rather than to things RVI may work on, and set Callrel()
  // correctly.
  void Copy_dedicated_regs_to_pregs( void ) const;

  // make sure Callrel() is set correctly when we do not call 
  // Copy_dedicated_regs_to_pregs().
  void Set_callrel() const;

  // build a list of live-ranges for a given variable or constant
  void Build_live_ranges(RVI_NODE *rvi_node,MEM_POOL *pool ) const;
  // function called only by above
  void Build_up_live_range( RVI_LR *live_range, BB_NODE *bb, 
		     RVI_LRBB_LIST *appearances,
		     BB_NODE_SET *visited_bbs, MEM_POOL *pool ) const;

  // analyze the live-range for the given bitpos
  void Analyze_live_range( RVI_LR *live_range ) const;

  // insert the loads and stores (or annotate)
  enum RVI_INSERT {
    RVI_INS_TOP,	// insert at top of block
    RVI_INS_BEFORE_IREF,// insert before iref at bottom of block
    RVI_INS_PRECALL,	// insert at bottom (before branch or call)
    RVI_INS_POSTCHI };	// insert at bottom (before branch/after chi)
  void Insert_statement( BB_NODE *bb, WN *wn, RVI_INSERT insert ) const;
  void Insert_loads_stores( RVI_LR *live_range, RVI_NODE *node);
  void Annotate_load_store( BB_NODE *bb, RVI_NODE *node, INT32 preg );
  void Insert_load( RVI_LRBB *lrbb, WN *loadwn, RVI_LR *live_range ) const;
  void Insert_store( RVI_LRBB *lrbb, WN *storewn, RVI_LR *live_range ) const;

  // return a WN that performs the appropriate conversion to store
  // the value to the preg
  void Store_to_preg_cvtl(WN *store_wn, ST* preg_st,
			  TY_IDX preg_ty, PREG_NUM preg_num) const;

  // return a WN that performs the appropriate conversion to store
  // the value to the preg, and then it stores to memory as well.
  void Store_to_preg_and_mem(BB_NODE *bb, WN *store_wn, 
			     ST* preg_st, TY_IDX preg_ty,
			     PREG_NUM preg_num) const;

  // return a WN that performs appropriate conversion to load the
  // value from a preg, which was loaded with 'load_opc'
  WN *Load_from_preg_cvtl(WN *wn, OPCODE load_opc) const;

  // perform RVI for variables
  void Perform_variable_rvi( void );
  // perform RVI for constants
  void Perform_constant_rvi( void );
  // perform RVI for variables and constants (common code for above)
  void Perform_variable_constant_rvi( RVI_NODE * );
#if defined(TARG_SL)
  BOOL Is_Intrncall_Nth_Parm_Need_RVI(INTRINSIC, INT);
#endif
  // The two main phases of RVI
  WN *Perform_phase1( WN *wn );
  WN *Perform_phase2( WN *wn );

  // convert the CFG back to a WHIRL tree
  WN *Emit( void ) const;

  RVI(void);			// constructor (never used)
  RVI(const RVI&);		// (never used)
  RVI& operator = (const RVI&);	// (never used)

public:
  // constructors and destructors
  RVI( BOOL do_rvi, OPT_STAB *opt_stab, IDX_32 max_varbit, ALIAS_MANAGER *alias_mgr );
  ~RVI(void);

  // public access methods

  // Access the memory pool for all allocation
  MEM_POOL *Rvi_gpool(void) const
		{ return _rvi_gpoolp; }
  MEM_POOL *Rvi_ppool(void) const
		{ return _rvi_ppoolp; }
  MEM_POOL *Rvi_lpool(void) const
		{ return _rvi_lpoolp; }

  // Have we enabled RVI?
  BOOL Do_rvi(void) const
		{ return _do_rvi; }

  // Track mu/chi lists
  void Map_mu_list ( WN *wn, MU_LIST  *mu_list  );
  void Map_mu_node ( WN *wn, MU_NODE  *mu_node  );
  void Map_chi_list( WN *wn, CHI_LIST *chi_list );

  // Track the itable's bit-position associated with the node
  void Map_bitpos( WN *wn, IDX_32 bitpos ) const
		{ WN_MAP32_Set( Bp_map(), wn, bitpos ); }

  // get the bit-position associated with the node, if any.
  // returns ILLEGAL_BP if not set
  IDX_32 Get_bitpos( const WN *wn ) const
		{ IDX_32 bp = WN_MAP32_Get( Bp_map(), wn );
		  return ( bp != 0 ? bp : ILLEGAL_BP );
		}

  // Does RVI consider the op a black box that it won't look in?
  BOOL Black_box( const OPCODE opc ) const
		{ return OPCODE_is_black_box(opc) ||
			 // don't want pregs for what may be local
			 // const table that asm refers to
			 // because then symbol will be moved outside
			 // of asm scope.
			 opc == OPC_ASM_STMT ||
			 opc == OPC_EXC_SCOPE_BEGIN;
		}

  // Perform the RVI algorithm (main entry point)
  WN *Perform_RVI( WN *wn, ALIAS_MANAGER *alias );
};

class PRE_RVI_HOOKS {
private:
  IDX_32 _nbits;
  BOOL   _tracing;

public:
  PRE_RVI_HOOKS(OPT_STAB *,
		CFG      *,
		MEM_POOL *,
		BOOL);

  BOOL Tracing(void) { return _tracing; }

  void Setup_bitpos(OPT_STAB *, CODEREP *);

  IDX_32 Nbits(void) { return _nbits; }

  void   Inc_nbits(void) { ++_nbits; }
};

#endif  // opt_rvi_INCLUDED
