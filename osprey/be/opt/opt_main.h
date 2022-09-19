/*
 *  Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_main.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_main.h,v $
//
// Revision history:
//  8-SEP-94 shin - Original Version
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


#ifndef opt_main_INCLUDED
#define opt_main_INCLUDED     "opt_main.h"
#ifdef _KEEP_RCS_ID
static char *opt_mainrcs_id = opt_main_INCLUDED"$Revision: 1.4 $";
#endif /* _KEEP_RCS_ID */

// forward declaration
class WN;
typedef struct mem_pool MEM_POOL;

class ALIAS_MANAGER;
class ALIAS_RULE;
class CFG;
class DNA_NODE;
class EXC;
class CODEMAP;
class DF_LOCAL;
class EMITTER;
class ITABLE;
class IPSA;
class MAIN_EMITTER;
class OPT_STAB;
class RVI;
class SSA;
class FENCEPOSTING;
class PRE_RVI_HOOKS;
class VN;
class CDA;
class VRA;
class VSA;
class VSA_ACCESS_INFO;
class VSA_ADDRESS_INFO;
class EH_TABLE;

#include "opt_config.h"
#include "optimizer.h"
#include "region_util.h"

class COMP_UNIT {
private:
  OPT_PHASE      _phase;
  WN            *_input_tree;
  CFG           *_cfg;
  EXC           *_exc;
  RID	        *_rid;
  OPT_STAB      *_opt_stab;
  SSA           *_ssa;
  ALIAS_MANAGER *_alias_mgr;
  ALIAS_RULE    *_arule;
  CODEMAP       *_htable;
#if defined(BUILD_MASTIFF)
  DNA_NODE      *_dna;
  CDA           *_cda;
  VRA           *_vra;    // value range analyzer
  VSA           *_vsa;
  EH_TABLE      *_eh_table;
#endif

  union {
    PRE_RVI_HOOKS *_pre_rvi_hooks;
    ITABLE        *_itable;
  };

  DF_LOCAL      *_df_local;
  EMITTER       *_emitter;
  FENCEPOSTING  *_fenceposting;
  MAIN_EMITTER  *_main_emitter;
  MEM_POOL      *_mem_pool;
  MEM_POOL      *_loc_pool;
  
                COMP_UNIT(void);
                COMP_UNIT(const COMP_UNIT&);
                COMP_UNIT& operator = (const COMP_UNIT&);

public:
  
                COMP_UNIT(WN *t, ALIAS_MANAGER *, OPT_PHASE phase,
                          MEM_POOL *gpool, MEM_POOL *lpool
                          );
               ~COMP_UNIT(void);

  void         Build_df_local(CFG *, OPT_STAB *, ITABLE *, ALIAS_RULE *);
  void	       Build_itable(ITABLE *, CFG *, CODEMAP *);    // create itable
  void         Collect_statistics(void);    // collect statistics
  void         Compute_PRE_saves(void);     // compute source, sink, region
  void         Create_fenceposting(void);   // create fenceposting class
  void         Create_mainopt_res(void);
  void         Create_main_emitter(RVI *);  // create main emitter
  void         Delete_main_emitter(void);   // deallocate main emitter    
  void         Delete_fenceposting(void);   // deallocate fenceposting
  void         Do_vnfre(BOOL before_epre);  // Value numbering redundancy elim.
  void	       Do_bitwise_dce(BOOL copy_propagate);
					    // bitwise dead code elimination
  void         Do_code_motion(void);        // Take PRE approach
  void	       Do_copy_propagate(void);     // copy propagation for whole PU
  void	       Do_dead_code_elim(BOOL do_unreachable,
				 BOOL do_dce_global,
				 BOOL do_dce_alias,
				 BOOL do_agg_dce,
				 BOOL do_identity_removal,
				 BOOL do_preg_renumbering,
				 BOOL *paths_removed);
                                            // dead-code elim for whole PU
  void	       Do_iv_recognition(void);     // IV recognition for whole PU
  void         Do_induction_var_elim(void); // IV elimination
  void         Do_ldx_optimization(void);   // Indexed load optimization
  void         Do_load_pre(BOOL do_consts, 
			   BOOL do_loads);  // LOAD PRE based on SSA
  void         Do_local_rvi(void);          // Fast rvi of local variables
  void         Do_reasso(void);             // Redundancy elimination with reassociation
  void         Do_new_pre(void);            // PRE based on SSA
  void         Do_store_pre(void);          // STORE PRE based on SSA
  void         Do_update_dead_sources(void);// IV elimination (part 2)
  void         Do_zdl(RVI *);               // Do zdl transformation
  WN          *Emit_ML_WHIRL(RVI *);        // Emit the mid-low level whirl
  void         Find_lr_shrink_cand(void);   // if Do_local_rvi is not called
#ifdef KEY
  void	       Find_uninitialized_locals(void); // find uninitialized local vars
  void	       Find_uninit_locals_for_entry(BB_NODE*); // find uninitialized local vars for an entry to the PU
#endif
  void	       Fold_lda_iload_istore(void); // lda-iload/istore folding on cr
  void         Find_iv(void);               // Find IVs for strength reduction
  void         Init_df_sets(void);          // Initialize all bitsets
  void	       Introduce_mtype_bool(void);  // introduce MTYPE_B 
  void	       Lower_to_extract_compose(void); // lower {I,}{LD,ST}BITS
  WN          *Normalize_loop(WN *wn);
  void	       U64_lower_cr(BOOL leave_CVTL_at_leaf); // coderep U64 lowerer 
  BOOL	       Verify_IR(CFG *, CODEMAP *, INT);// consistency check
  BOOL         Verify_CODEMAP(void);
  void         Verify_version(void);
  void         Pro_loop_trans();         // The driver for proactive loop transformations.

#if defined(BUILD_MASTIFF)
  void         Do_cda(IPSA *ipsa_mgr);      // control dependency analysis
  void         Do_vra(IPSA *ipsa_mgr);      // Value Range Analysis
  void         Do_vsa(IPSA *ipsa_mgr);      // Vulnerability Static Analysis
  void         Build_eh_table();            // Build EH table
  CODEREP     *Analyze_base_info(STMTREP* use,
                                 CODEREP* cr,
                                 BOOL check_ud); // analyze address expression to get base
  BOOL         Analyze_pointer_info(STMTREP* use,
                                    CODEREP* cr,
                                    VSA_ADDRESS_INFO* info,
                                    BOOL check_ud); // analyze address expression
  BOOL         Analyze_address_info(STMTREP* use,
                                    CODEREP* cr,
                                    VSA_ADDRESS_INFO* info,
                                    BOOL store,
                                    BOOL check_ud);  // Analyze IVAR cr to get address information
  BOOL         Analyze_access_info(STMTREP* use,
                                   CODEREP* cr,
                                   VSA_ACCESS_INFO* info); // Analyze IVAR cr to get access information
  BOOL         Is_stmt_in_loop(STMTREP *sr);
  CODEREP     *Find_return_value(STMTREP *call_stmt);
  CODEREP     *Find_return_value(STMTREP *call_stmt, CODEREP *lda_node);
#endif

  // member access functions
  WN          *Input_tree(void)         { return _input_tree; }
  MEM_POOL    *Mem_pool(void)           { return _mem_pool; }
  MEM_POOL    *Loc_pool(void)           { return _loc_pool; }
  CFG         *Cfg(void)                { return _cfg; }
  EXC         *Exc(void)                { return _exc; }
  RID	      *Rid(void)		{ return _rid; }
  OPT_STAB    *Opt_stab(void)           { return _opt_stab; }
  SSA 	      *Ssa(void)                { return _ssa; }
  ALIAS_MANAGER *Alias_mgr(void)        { return _alias_mgr; }
  ALIAS_RULE  *Arule(void)              { return _arule; }
  CODEMAP     *Htable(void)             { return _htable; }
  ITABLE      *Itable(void)             { return _itable; }
  EMITTER     *Emitter(void)            { return _emitter; }
  FENCEPOSTING *Fenceposting(void)      { return _fenceposting; }
  MAIN_EMITTER *Main_emitter(void)      { return _main_emitter; }
  OPT_PHASE   Phase(void)               { return _phase; }
  PRE_RVI_HOOKS *Pre_rvi_hooks(void)    { return _pre_rvi_hooks; }
  void           Set_pre_rvi_hooks(PRE_RVI_HOOKS *pre_rvi_hooks)
    { _pre_rvi_hooks = pre_rvi_hooks; }
#if defined(BUILD_MASTIFF)
  DNA_NODE    *Dna(void)                { return _dna; }
  CDA         *Cda(void)                { return _cda; }
  VRA         *Vra(void)                { return _vra; }
  VSA         *Vsa(void)                { return _vsa; }
  EH_TABLE    *EH_table(void)           { return _eh_table; }
  void        Set_dna(DNA_NODE *dna)    { _dna =  dna; }
  void        Set_vsa(VSA *vsa)         { _vsa =  vsa; }
  SRCPOS         End_srcpos(void)
    { WN *blk = WN_func_varrefs(_input_tree);
      return WN_Get_Linenum(blk);     }
  SRCPOS         Get_end_srcpos(void);
#endif
};


extern PREOPT_PHASES Preopt_Current_Phase;

#endif  // opt_main_INCLUDED
