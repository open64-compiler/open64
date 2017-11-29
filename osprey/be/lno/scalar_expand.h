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



// -*-C++-*-

/**
***                     Scalar Expansion
***                     ----------------
***
*** Description:
***
***         This file contains procedures to test the condition for
***         scalar expansion in LNO and to perform scalar expansion.
***
***	NOTE: 
***
***	    In the functions below, the argument 'full_dist' is TRUE 
***	    if the scalar expansion is being provided to transform a 
***	    SNL into a perfect nest via distribution, FALSE if the 
***	    scalar expansion is being provided only to enable the 
***	    permutation. 
***
***
*** Exported functions:
***
***	SE_STATUS SNL_Is_Scalar_Expandable(WN* wn_outer,
***                                   INT permutation[],
***                                   INT nloops,
***                                   SX_INFO* sx_info,
***				      BOOL full_dist)
***
***	    Returns an SE_STATUS value describing if the SNL 'wn_outer' to 
***	    which we are applying a 'permutation' of length 'nloops' can be 
***	    scalar expanded to enable that permutation.  Its scalar expandable 
***	    variables are summarized in the 'sx_info'.
***
***	void SNL_GEN_Scalar_Expand(WN* wn_outer,
***                               IMAT* unimodular,
***                               SNL_TILE_INFO* ti,
***                               INT nloops,
***                               SX_PLIST* plist,
***				  INT split_depth=-1,
***				  SD_PLIST* sd_plist, 
***				  BOOL ignore_illegal=FALSE,
***				  BOOL full_dist=TRUE)
***
***	    Perform unlimited tile size scalar expansion on a general
***	    SNL 'wn_outer' with 'nloops' loops.  The 'unimodular' matrix and
***	    the tile info 'ti' are used to guide the selection of subscripts.
***	    The 'plist' is a list of scalar expanded variables created by 
***	    calling SX_INFO::Make_Sx_Info(). The 'sd_plist' is a list of 
***         possibly removal scalars.  If 'ignore_illegal' is TRUE, skip 
***	    over SX_PNODE::ILLEGAL scalars on the 'plist'. 
***
***	void SNL_GEN_Scalar_Expand(WN* wn_outer,
***                                INT permutation[],
***                                INT nloops,
***                                SX_PLIST* plist,
***				   INT split_depth=-1,
***				   SD_PLIST* sd_plist, 
***				   BOOL ignore_illegal=FALSE,
***				   BOOL full_dist=TRUE) 
***
***	    Perform unlimited tile size scalar expansion on a general 
***	    SNL 'wn_outer' with 'nloops' loops.  The 'permutation' is the final
***	    permutation of the SNL loops and is used to guide the selection of
***	    subscripts. The 'plist' is a list of scalar expanded variables 
***	    created by calling SX_INFO::Make_Sx_Info(). The 'sd_plist' is a
***         list of possibly removal scalars.  If 'ignore_illegal' is 
***	    TRUE, skip over SX_PNODE::ILLEGAL scalars on the 'plist'.
***
***	void SNL_INV_Scalar_Expand(WN* wn_outer,
***                                INT permutation[],
***                                INT nloops,
***                                SX_PLIST* plist,
***				   INT split_depth=-1, 
***				   SD_PLIST* sd_plist, 
***				   BOOL ignore_illegal=FALSE,
***				   BOOL full_dist=TRUE)
***
***	    Perform unlimited tile size scalar expansion on an invariant
***	    SNL 'wn_outer' with 'nloops' loops.  The 'permutation' is the final
***	    permutation of the SNL loops and is used to guide the selection of
***	    subscripts. The 'plist' is a list of scalar expanded variables 
***	    created by calling SX_INFO::Make_Sx_Info().  The 'sd_plist' is a 
***	    list of possibly removal scalars.  If 'ignore_illegal' 
***	    is TRUE, skip over SX_PNODE::ILLEGAL scalars on the 'plist'.
***
***	INT Split_Sx_Depth(WN* wn_outer, INT nloops, SX_PLIST* plist, 
***	    INT split_depth)
***
***	    Return the depth of the outermost loop which for which we
*** 	    must apply scalar expansion because the SNL with outermost loop
***	    'wn_outer' containing 'nloops' loops whose scalars are described
***	    by 'plist' must be split into possibly three distributends: one
***	    above and below the loop at depth 'split_depth' and the main loop.
***
***	void SNL_Scalar_Expand(WN* wn_outer,
***                           WN* wn_inner,
***                           INT permutation[],
***                           INT nloops,
***                           SX_INFO* sx_info,
***                           BOOL invariant, 
***			      BOOL ignore_illegal=FALSE,
***			      BOOL full_dist=TRUE)
***
***	    For the SNL with outermost loop 'wn_outer' and innermost loop
***	    'wn_inner' to which we are applying the 'permutation' of length 
***	    'nloops', scalar expand the required variables in the 'sx_info'.
***	    If 'invariant' is TRUE, use invariant scalar expansion, otherwise  
***	    use general scalar expansion.  If 'ignore_illegal' is TRUE, 
***	    skip over SX_PNODE::ILLEGAL scalars in the 'sx_info'.
***
***	void SNL_Scalar_Expand_For_Splitting(WN* wn_outer,
***                                          WN* wn_inner,
***                                          INT split_depth,
***                                          SX_PLIST* plist,
***					     SD_PLIST* sd_plist, 
***                                          BOOL invariant,
***					     BOOL ignore_illegals=FALSE,
***					     BOOL full_dist=TRUE)
***
***	    For the SNL with outermost loop 'wn_outer' and innermost loop
***	    'wn_inner', scalar expand the required variables in 'plist' and 
***	    'sd_plist' so that the kernel of loops from depth 'split_depth' 
***	    inward can be distributed into its own distributend.  If 
***	    'ignore_illegal' is TRUE, skip over SX_PNODE::ILLEGAL scalars 
***	    on the 'plist'.  Returns a BOOL indicating whether a scalar 
***	    expanded variable had LCDs.
***
***     STACK<WN*>* Scalar_Equivalence_Class(WN* ref, DU_MANAGER* Du_Mgr,
***                      MEM_POOL *pool)
***
***         Returns a stack of WNs which can be reached through DEF or
***         USE chains from ref. It is used by the scalar renaming and
***         scalar expansion.  We only rename or expand scalars whose
***         equivalence class is entirely in the loop that we are
***         interested. We use a depth-first search along the DU chains
***         to collect all the references. 'pool' is the memoery pool
***         used to allocate storage for the returned stack. 'Du_Mgr'
***         provides the DU info.
***
***	STACK<WN*>* Scalar_Equivalence_Class(WN* ref, DU_MANAGER* Du_Mgr,
***         MEM_POOL *pool, BOOL find_restrict, WN** wn_outer_loop)
***
***	    Identical to the above when 'find_restrict' is FALSE.  When it 
***	    is TRUE, we return in 'wn_outer_loop' a subset of the scalar 
***	    equivalence class restricted to this loop.  The belief is that 
***	    if we finalize SYMBOL(ref) on exit to this loop, that we will 
***	    still be able to scalar expand SYMBOL(ref) within the loop 
***	    'wn_outer_loop'. 
***
***	STACK<WN*>* Scalar_Equivalence_Class(WN* ref, DU_MANAGER* du, 
***	    MEM_POOL* pool, WN* wn_loop)
***
***	    Find the scalar equivalence class for 'ref' within 'wn_loop'.
***	    Assumes that only STIDs and LDIDs will be found within this loop.
***	    and that all testing has been done to ensure that this is a valid
***	    equivalence class.
***
***     SE_RESULT Scalar_Expandable(
***         WN*                 ref,
***         WN*                 loop,
***         DU_MANAGER*         du_mgr
***     )
***
***	    Returns: 
***	      SE_NONE: Cannot be scalar expanded in the loop 
***	      SE_EASY: Can be scalar expanded without finalization 
***	      SE_HARD: Can be scalar expanded, but requires finalization
***
***         A scalar is expandable if all of the references
***         to this variable connected through some DU chains
***         (represented by Scalar_Equivalence_Class()) are all in the
***         given loop, and the loop_stmt (see 'Du_Mgr' for all connected
***         uses) are inside the given loop.  Uses LNO_local_pool for
***         temporary storage of the scalar_equivalence_class.
***
***     SE_RESULT Scalar_Expandable(
***         STACK<WN*>*         equivalence_class;
***         WN*                 ref,
***         WN*                 loop,
***         DU_MANAGER*         du_mgr,
***	    WN* 		wn_outer_loop,
***	    WN* 		wn_eq_loop
***     )
***
***         As above, but equivalence class not recalculated or destroyed.
***
***     BOOL Scalar_Expansion_Not_Necessary(WN* ref, DU_MANAGER* Du_Mgr)
***
***         If all the refs and uses associated with this ref occur on the
***         same body (or within on the same body) and if all occur either
***         above or below a DO in that body, then TRUE.  Also TRUE if there
***         is no DO in the body.  Otherwise, FALSE.
***
***	void Scalar_Expand(
***	    WN* allocregion,
***	    WN* region,
***	    WN* wn_sym, 
***	    const SYMBOL& symbol,
***	    WN** loops,
***	    INT* order,
***	    INT dimcount,
***	    BOOL invariant,    
***	    BOOL finalize, 
***	    BOOL has_lcd, 
***	    WN* guard_tests[], 
***	    BIT_VECTOR *used_loops=NULL,
***         WN** tile_loops=NULL,
***         INT* stripsizes=NULL, 
***         INT nstrips=0)
***	);
***
***	    Apply scalar expansion to region.  Allocregion a loop before
***	    which malloc/alloc is called and after which free is called
***	    if necessary.  The sym (which is the symbol of wn_sym) is ex-
***	    panded to have dimcount dimensions.
***
***	    loops are the loops, in order from allocregion on in, and
***	    order shows the order in which the loops will occur after further
***	    transformation, so that we know which dimension to make stride
***	    one when scalar expanding.  The dependence graph is updated,
***	    as are access vectors.
***
***	    If nstrips > 0, create small scalar expanded arrays of compile-
***	    time constant length.  Additional parameters include "tile_loops",
***	    which are the scalar expansion tile loops created before this 
***	    function is called, and "ti_se", the tile info for scalar 
***	    expansion.  If used_loops isn't NULL, only expand the scalar  
***	    over the loops for which used_loops[i] is set. 
***
***	    If 'invariant' is TRUE, scalar expansion is being performed 
***	    on an invarinat nest.  If 'finalize' is TRUE, we need to        
***	    compute a final value for the scalar expanded variable.  
***
***	INT SE_Guard_Depth(WN* wn_outer, INT permutation[], INT nloops, 
***	   SX_PLIST* plist, INT split_depth, SD_PLIST* sd_plist, 
***	   BOOL ignore_illegal, BOOL full_dist)
***
***	    For the SNL with outermost loop 'wn_outer', lists of scalar
***	    expandable references 'plist' and 'sd_plist', returns the depth 
*** 	    of the innermost loop which we must guard to finalize scalar 
***	    expandable variables.  The 'split_depth' for the nest is 
***	    'split_depth'.  If 'ignore_illegal' is TRUE, then ignore 
***	    unexpandable scalars, otherwise assert.
***
***	void SE_Guard_Tests(WN* wn_outer, INT nloops, WN* guard_tests[], 
***	    INT guard_depth)
***
***	    For the SNL with outermost loop 'wn_outer' consisting of 
***	    'nloops' loops, place a nest of guard tests after 'wn_outer', one
***	    for each loop up to and including the loop of depth 'guard_depth'.
***	    Return pointers to these guard tests in 'guard_tests'.
***
***     struct WN_REFERENCE_INFO {
***         WN*	                         wn;
***         INT	                         lexcount;
***     }
***          Used in the def_list and use_list to rebuild dependence
***          information. used in a template, so can't be declared locally.
***
***
***     extern void SE_Symbols_For_SE(
***         SYMBOL*                      ptr_array, 
***         const char*                  pref, 
***         INT                          unique_id, 
***         TYPE_ID                      mtype)
***
***          Create a new symbol name: 'pref+unique_id' of type 'mtype', 
***          and return the new symbol in (*prt_array)
***
***     extern WN* Get_Expansion_Space(
***         SYMBOL                       se_ptrsym, 
***         WN*                          bsz,
***         const char*                  pref,        
***	    INT                          unique_id,     
***         TYPE_ID                      wtype,     
***         WN*                          allocregion,   
***         WN*                          useregion,
***         WN*                          deallocregion)
*** 
***
***          Get space for expanded scalar.  Insert allocation code 
***          before 'allocregion', and deallocation code after 
***          'deallocregion'.  The line number for the allocation/
***          deallocation codes are set to 'useregion'.  
***          'se_prtsym' is the name of the space.
***          pref+unique_id form an unique name of the expanded array.
***          Return a WN* that contains the starting location of the 
***          allocated space.
***
***     extern void SE_Fix_Dependence(
***         DYN_ARRAY<WN_REFERENCE_INFO>& deflist,
***         DYN_ARRAY<WN_REFERENCE_INFO>& uselist,
***         INT no_self_depend = 1)
***
***          Given a 'deflist' and a 'uselist' of references to an array,
***          build the corresponding dependence information to the dependence
***	     graph. 
***
***          For normal scalar expansion, no_self_depend=1 meaning that the
***          stores in the first loop will not self-interfere.  For gather/
***          scatter, no_self_depend=0 because the index may not always
***          get increased.
**/

/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#include <sys/types.h>
#include "defs.h"
#include "wn.h"
#include "cxx_memory.h"
#include "cxx_template.h"
#include "opt_du.h"

#ifndef scalar_expand_INCLUDED
#define scalar_expand_INCLUDED "scalar_expand.h"

#ifdef _KEEP_RCS_ID
static char *scalar_expand_rcs_id = scalar_expand_INCLUDED "$Revision$";
#endif /* _KEEP_RCS_ID */

#ifndef mat_INCLUDED
#include "mat.h"
#endif 
#ifndef snl_INCLUDED
#include "snl.h"
#endif 
#ifndef snl_utils_INCLUDED
#include "snl_utils.h"
#endif 
#ifndef snl_trans_INCLUDED
#include "snl_trans.h"
#endif 
#ifndef _lno_bv_INCLUDED
#include "lno_bv.h"
#endif
#ifndef sxlist_INCLUDED
#include "sxlist.h"
#endif 
#ifndef sdlist_INCLUDED
#include "sdlist.h"
#endif 

enum SE_STATUS {SE_TRUE, SE_FALSE, SE_MAYBE}; 

enum SE_RESULT {SE_NONE, SE_EASY, SE_HARD, SE_EASY_LCD, SE_HARD_LCD}; 

extern SE_STATUS SNL_Is_Scalar_Expandable(WN* wn_outer, INT permutation[],
  INT nloops, SX_INFO* sx_info, BOOL full_dist);

extern INT SNL_Bad_Scalars_Are_Distributable(WN* wn_outer, INT permutation[],
  INT nloops, SX_INFO* sx_info, SD_INFO* sd_info);

extern void SNL_GEN_Scalar_Expand(WN* wn_outer, IMAT* unimodular,
  SNL_TILE_INFO* ti, INT nloops, SX_PLIST* plist, INT split_depth=-1,
  SD_PLIST* sd_plist=NULL, BOOL ignore_illegal=FALSE, BOOL full_dist=TRUE);

extern void SNL_GEN_Scalar_Expand(WN* wn_outer, INT permutation[], 
  INT nloops, SX_PLIST* plist, INT split_depth=-1, SD_PLIST* sd_plist=NULL,
  BOOL ignore_illegal=FALSE, BOOL full_dist=TRUE);

extern void SNL_INV_Scalar_Expand(WN* wn_outer, INT permutation[],
  INT nloops, SX_PLIST* plist, INT split_depth=-1, SD_PLIST* sd_plist=NULL, 
  BOOL ignore_illegal=FALSE, BOOL full_dist=TRUE);

extern INT Split_Sx_Depth(WN* wn_outer,
                          INT nloops,
                          SX_PLIST* plist,
                          INT split_depth);

extern void SNL_Scalar_Expand(WN* wn_outer,
                              WN* wn_inner,
                              INT permutation[],
                              INT nloops,
                              SX_INFO* sx_info,
                              BOOL invariant,
			      BOOL ignore_illegal=FALSE,
			      BOOL full_dist=TRUE);

extern void SNL_Scalar_Expand_For_Splitting(WN* wn_outer,
					    WN* wn_inner,
					    INT split_depth,
					    SX_PLIST* plist,
					    SD_PLIST* sd_plist, 
					    BOOL invariant,
					    BOOL ignore_illegals,
					    BOOL full_dist=TRUE);

extern STACK<WN*>* Scalar_Equivalence_Class(
       WN* ref, DU_MANAGER* Du_Mgr, MEM_POOL* pool);

extern STACK<WN*>* Scalar_Equivalence_Class(
       WN* ref, DU_MANAGER* Du_Mgr, MEM_POOL* pool,
       BOOL find_restrict, WN** wn_outer_loop);

extern STACK<WN*>* Scalar_Equivalence_Class(WN* ref, DU_MANAGER* du,
  MEM_POOL* pool, WN* wn_loop); 

extern SE_RESULT Scalar_Expandable(WN* ref, WN* loop, DU_MANAGER* du);
extern SE_RESULT Scalar_Expandable(STACK<WN*>* equivalence_class, 
  WN* ref, WN* loop, DU_MANAGER* du, WN* wn_outer_loop, WN* wn_eq_loop=NULL);
extern BOOL Scalar_Expansion_Not_Necessary(WN* ref, DU_MANAGER* Du_Mgr);

extern void Scalar_Expand(WN* allocregion, WN* region,
                          WN* wn_sym, const SYMBOL& symbol, WN** loops, 
			  const INT* order, INT dimcount, BOOL invariant, 
			  BOOL finalize, BOOL has_lcd, WN* guard_tests[], 
			  BIT_VECTOR *used_loops=NULL,
                          WN** tile_loops=NULL,
                          INT* stripsizes=NULL, 
                          INT nstrips=0);

extern INT SE_Guard_Depth(WN* wn_outer,
			  INT permutation[], 
			  INT nloops, 
                          SX_PLIST* plist,
                          INT split_depth,
			  SD_PLIST* sd_plist, 
                          BOOL ignore_illegal,
			  BOOL full_dist);

extern void SE_Guard_Tests(WN* wn_outer,
                           INT nloops,
                           WN* guard_tests[],
                           INT guard_depth);

struct WN_REFERENCE_INFO {
  WN*	wn;
  INT	lexcount;
};

extern void SE_Symbols_For_SE(SYMBOL* ptr_array, 
			      const char* pref, INT unique_id, TYPE_ID mtype);

extern WN* Get_Expansion_Space(SYMBOL se_ptrsym, 
			       WN* bsz,          
			       const char* pref,       
			       INT unique_id,     
			       TYPE_ID wtype,     
			       WN* allocregion,   
			       WN* useregion,
			       WN* deallocregion);

extern void SE_Fix_Dependence(DYN_ARRAY<WN_REFERENCE_INFO>& deflist,
			      DYN_ARRAY<WN_REFERENCE_INFO>& uselist);
#endif

