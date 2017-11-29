/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

/*
 *  Copyright (C) 2000-2003, Intel Corporation
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without modification,
 *  are permitted provided that the following conditions are met:
 *  
 *  Redistributions of source code must retain the above copyright notice, this list
 *  of conditions and the following disclaimer. 
 *  
 *  Redistributions in binary form must reproduce the above copyright notice, this list
 *  of conditions and the following disclaimer in the documentation and/or other materials
 *  provided with the distribution. 
 *
 *  Neither the name of the owner nor the names of its contributors may be used to endorse or
 *  promote products derived from this software without specific prior written permission. 
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
 *  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 *  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

    /* ======================================================================
     * ======================================================================
     *
     *  Module sched_cand.cxx
     *
     *  $Revision: 1.1 $
     *  $Date: 2005/12/30 01:50:23 $
     *  $Author: weitang $
     *  $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/sched_cand.cxx,v $
     * 
     *  Description:
     *  =============
     *
     *  This module implement 3 classes, namely CAND_LIST,CAND_LIST_ITER and
     *  CAND_LIST_RITER.
     *  
     *  See sched_cand.h for the description.
     * 
     * ======================================================================
     * ======================================================================
     */
#include "cg.h"
#include "cg_dep_graph.h"

#include "ipfec_options.h"

#include "sched_rgn_info.h"
#include "sched_util.h"
#include "sched_cand.h"
#include "sched_heur.h"


UNRESOLVED_DEP_LST& 
UNRESOLVED_DEP_LST :: operator = (const UNRESOLVED_DEP_LST& urds) {

    FmtAssert (FALSE, ("Copy is prohibit"));
    return *this;
}


void
UNRESOLVED_DEP_LST :: Delete_Item (UNRESOLVED_DEP* dep) {
    
    Is_True (dep->Get_List () == this, 
             ("item is not in this list"));
    
    Is_True (dep->Is_Alloc_By_Cand_Mgr (), 
             ("UNRESOLVED_DEP_LST is not allocated by CAND_MGR"));

    UNRESOLVED_DEP* prev = dep->_prev;
    UNRESOLVED_DEP* next = dep->_next;

    if (prev) {
        prev->_next = next;
    } else {
        _header = next;
    }

    if (next) {
        next->_prev = prev;   
    } else {
        _tail = prev;
    }

    _cand_mgr->Free_Unresolved_Dep (dep);
}

void
UNRESOLVED_DEP_LST :: Prepend (UNRESOLVED_DEP* dep) {
    
    dep->_next = _header;
    dep->_prev = NULL;

    if (_header) { _header -> _prev = dep; } 
    _header = dep; 

    if (!_tail) { _tail = dep; }
    dep->_lst = this;
}

void
UNRESOLVED_DEP_LST :: Append  (UNRESOLVED_DEP* dep) {

    dep->_prev = _tail;
    dep->_next = NULL;

    if (_tail) { _tail -> _next = dep; }
    _tail = dep;

    if (!_header) { _header = dep; }
    
    dep->_lst = this;
}


void
BOOKEEPING_LST :: Delete_Item (BOOKEEPING* item) {

    Is_True (item->Get_List () == this, 
             ("item is not in this list"));
    
    Is_True (item->Is_Alloc_By_Cand_Mgr (), 
             ("BOOKEEPING structure is not allocated by CAND_MGR"));

    BOOKEEPING* prev = item->_prev;
    BOOKEEPING* next = item->_next;

    if (prev) {
        prev->_next = next;
    } else {
        _header = next;
    }

    if (next) {
        next->_prev = prev;   
    } else {
        _tail = prev;
    }

    _cand_mgr->Free_Bookeeping (item);
}

void
BOOKEEPING_LST :: Append (BOOKEEPING* item) {

    item->_prev = _tail;
    if (_tail) { _tail->_next = item; }
    _tail = item;

    if (!_header) { _header = item; }
    item->_bklst = this;
}

void
BOOKEEPING_LST :: Prepend (BOOKEEPING* item) {
    
    item->_next = _header;
    if (_header) { _header->_prev = item; }
    _header = item;
    
    if (!_tail) { _tail = item; }
    item->_bklst = this;
}


 /* =====================================================================
  * =====================================================================
  *
  *         Implementation of CANDIDATE 
  *
  * =====================================================================
  * =====================================================================
  */

    /* constructor and destructor 
     */
void
CANDIDATE :: Init (void) {

    _try_ver = 0 ; /* any arbitrary value works, now */ 
    _prev = _next = NULL ;
    _op   = NULL ; 
    _spec_type = SPEC_NONE ;
    _flags = 0;
    _move_against_paths.Clear ();
    _move_across_rgns.clear ();
    _match_model = FALSE;

    Free_Unresolved_Dep_Lst ();
    Free_Bookeeping_Lst ();
}

CANDIDATE :: CANDIDATE (CAND_MGR* cand_mgr, MEM_POOL* mp) 
    : _mp(mp),
      _cand_mgr (cand_mgr), 
      _violated_deps (cand_mgr),
      _bookeepings (cand_mgr),
      _move_across_rgns (mp),
      _move_against_paths(mp),
      _pready_bookeeping_paths(mp) {

      Init (); 
}


CANDIDATE :: ~CANDIDATE (void) {
    Free_Unresolved_Dep_Lst ();
    Free_Bookeeping_Lst ();
}

    /* =========================================================
     * 
     *  Get_Up_to_Date_Spec_Type 
     *   
     *  Derive speculation type associated with candidate from 
     *  CURRENT context. The spec-type may vary during the corse
     *  of scheduling. Take as an example: 
     * 
     *  BBx: 
     *     op1: st8 [addr1], v1
     *     op2: ld8 v2, [addr2]
     * 
     *  Assume op2 alias with op1. At the time when scheduling 
     *  begins, both OPs can be deemed as candidates, scheduler 
     *  marks op2 as "data speculation" since OPs can be 
     *  potentially scheduled prior to OP1. 
     * 
     *  However,OP1 can be actually scheduled before OP2. If that
     *  occurs, the speculation type corresponding to OP2 is become 
     *  stale. 
     * 
     *  Hence, we need to examine the _*CURRENT*_ speculation type 
     *  for an instruction right before its schedule.   
     *
     * =========================================================
     */
SPEC_TYPE 
CANDIDATE :: Get_Up_to_Date_Spec_Type (void) {

    if (_violated_deps.Is_Empty () || 
        Is_P_Ready ()) {
            
        return _spec_type ;
    }

    SPEC_TYPE spec_type = SPEC_NONE;
    for (UNRESOLVED_DEP* dep = _violated_deps.First_Item ();
         dep != NULL;
         dep = _violated_deps.Next_Item (dep)) {

        /* Why OP_xfer() clause? -- the UNRESOLVED_DEP corresponding to the 
         * <pred> that is OP_xfer() is used to carray the information that 
         * this candidate's home block is not post-dominate the OP_bb(<pred>).
         * If we encount this kind of UNRESOLVED_DEP, we should keep
         * the speculation-type (control-spec) derive from this UNRESOLVED_DEP
         */
        if (OP_Scheduled(dep->Pred ()) &&
            !OP_xfer(dep->Pred ())) {
            continue ; 
        }
        
        spec_type = SPEC_TYPE(spec_type|dep->Spec_Type ());
    }

    return _spec_type = spec_type ;
}

    /* =====================================================
     *
     *  Shadowed_By_P_Ready_Bookeeping 
     *
     * ref the header file for details.
     *
     * =====================================================
     */
BOOL
CANDIDATE :: Shadowed_By_P_Ready_Bookeeping 
    (BB* b, RGN_CFLOW_MGR* cflow_info) {
    
    if (Is_M_Ready ()) { return FALSE; }

    EXEC_PATH_SET* eps = cflow_info->Get_Path_Flow_Thru (b);
    return eps->Is_Subset_Of (&_pready_bookeeping_paths);
}

BOOL
CANDIDATE :: Shadowed_By_P_Ready_Bookeeping 
    (REGION* r, RGN_CFLOW_MGR* cflow_info) {

    if (Is_M_Ready ()) { return FALSE; }

    EXEC_PATH_SET* eps = cflow_info->Get_Path_Flow_Thru (r);
    return eps->Is_Subset_Of (&_pready_bookeeping_paths);
}


 /* =====================================================================
  * =====================================================================
  * 
  *             IMPLEMENTATION OF CAND_LIST
  *
  * =====================================================================
  * =====================================================================
  */ 
void
CAND_LIST :: Init (void) {
        
    _total          = 0 ;
    _tried_cand_num = 0 ;
    _try_version    = MIN_TRY_VERSION ;

    /* initialize CANDIDATE's internal list 
     */
    _cand_lst_head._prev = _cand_lst_tail._next = NULL ;   
    _cand_lst_head._next = &_cand_lst_tail ;
    _cand_lst_tail._prev = &_cand_lst_head ;


    _op_map_idx = BS_Create_Empty (DEF_BBLENGTH, 
                                   _mp);
}

CAND_LIST :: CAND_LIST 
    (MEM_POOL *mp, CAND_MGR* cand_mgr,BOOL host_m_ready_cand):
     _mp(mp),
     _cand_lst_head (cand_mgr,mp),
     _cand_lst_tail (cand_mgr,mp) {

    _host_m_ready_cand = host_m_ready_cand;
    _cand_mgr = cand_mgr;

    Init () ;    
}


 /* ============================================================
  *
  *     Set_Cand_Has_Been_Tried 
  *  
  *  mark the status that this candidate has been tried to be
  *  scheduled but fails.
  *
  * ============================================================
  */
void 
CAND_LIST :: Set_Cand_Has_Been_Tried (CANDIDATE* cand) {

    if (!Cand_Has_Been_Tried (cand)) {

        cand->_try_ver = _try_version + 1 ;
        ++ _tried_cand_num ;
        Is_True (_tried_cand_num <= _total,
            ("tried candidate exceed total number!"));

    }
}

    /* =======================================================
     *
     *  Erase_All_Cand (void)
     *
     *  Make candidate list empty.
     * 
     * =======================================================
     */
void
CAND_LIST :: Erase_All_Cand (void) {
    while (_total > 0) {
        Erase_Cand (_cand_lst_head._next);
    }
            
    Is_True (_cand_lst_head._next == &_cand_lst_tail, 
        ("candidate list should be empty!"));
}

    /* ====================================================
     *
     *  Clear_Cand_Tried_Mark 
     * 
     *  Undo what <Set_Cand_Has_Been_Tried> has done
     *
     * ====================================================
     */
void
CAND_LIST :: Clear_Cand_Tried_Mark (CANDIDATE *cand) {

    if (Cand_Has_Been_Tried (cand)) {

        cand->_try_ver = _try_version ;
        -- _tried_cand_num ;

        Is_True (_tried_cand_num >= 0, 
            ("tried candidate can not be a negative"));
                        
    }
}


    /* =====================================================
     *
     *   return TRUE if <op> is added into this candidate 
     *   list, FALSE otherwise
     *
     * =====================================================
     */
BOOL
CAND_LIST :: OP_Is_In_Cand_List (OP* op) {
    
    if (!BS_MemberP (_op_map_idx, OP_map_idx(op))) {
        return FALSE;
    }

    for (CAND_LIST_ITER iter(this); ! iter.done () ; iter.step ()) {
        if (iter.cur()->Op () == op) return TRUE; 
    }
    return FALSE;
}

    /* ==============================================================
     * 
     *  Get_Candidate 
     *
     *  return CANDIDATE structure associated with candidate <op>. 
     *  NULL if <op> is not candidate or is not under the control 
     *  of this list.
     *
     * ==============================================================
     */
CANDIDATE *
CAND_LIST :: Get_Candidate (OP* op) {
    
    for (CAND_LIST_ITER iter(this); ! iter.done() ; iter.step ()) {
        CANDIDATE * cand = iter.cur();
        if (cand->Op () == op) return cand ; 
    }
    return NULL;
}

    /* ======================================================
     * 
     *  Add_Candidate
     *
     *  make <cand> under the control of CAND_LIST 
     *
     * ======================================================
     */
void
CAND_LIST :: Add_Candidate (CANDIDATE * cand) {
  
    /* TODO : add some cheap mechanism to prevent duplicated
     *        candidtes added into candidate list.
     */

    Append (cand) ;
    ++ _total ;
    cand->_try_ver = _try_version ; 

    _op_map_idx = BS_Union1D (_op_map_idx, 
                              OP_map_idx(cand->_op),_mp);

    cand->Set_Cand_Attached ();
}

    /* ===========================================================
     *
     *  Erase_Cand (OP*)
     *  Erase_Cand (CANDIDATE *cand)
     *
     *  Evict <op> or <cand> from candidate list and free its 
     *  corresponding CANDIDATE structure as well.
     *
     * ===========================================================
     */
void
CAND_LIST :: Erase_Cand (CANDIDATE* cand) {
    
    Is_True (cand->Is_Attached_To_Cand_Lst() &&
             ((cand->Is_M_Ready () && _host_m_ready_cand) ||
              (cand->Is_P_Ready () && !_host_m_ready_cand)),
            ("canidate list does not take control of this cand"));

    if (Cand_Has_Been_Tried (cand)) { -- _tried_cand_num ; }
    -- _total ;

    cand->Set_Cand_Detached ();
    Detach (cand);

    _cand_mgr->Reclaim_Free_Cand (cand);
}

void
CAND_LIST :: Erase_Cand (OP *op, BOOL abort_if_not_belong_lst) {
    
    for (CAND_LIST_ITER iter(this) ; !iter.done() ; ) {

        CANDIDATE * cand = iter.cur();

        if (cand->Op() == op) {
            iter.erase_cur_and_advance ();
            return ;
        } 

        iter.step ();
    }

    if (abort_if_not_belong_lst) {
        FmtAssert (FALSE, 
            ("OP[%d] of BB:%d is not in %c-Ready-Candidate list!\n", 
              OP_map_idx(op), 
              BB_id(OP_bb(op)),
              _host_m_ready_cand ? 'M' : 'P'));
    }
}



    /* ===========================================================
     *
     *  Create_Empty_Cand 
     *
     *  allocate an empty candidate structure
     *
     * ===========================================================
     */
CANDIDATE *
CAND_LIST :: Create_Empty_Cand (void) {

    return _cand_mgr->Alloc_Cand () ;
}

    /* ===============================================================
     *
     *   Clear_All_Cands_Tried_Mark 
     * 
     *   make all candidates managed by this CAND_LIST being un-"tried". 
     *   so they arc potentially scheduled in current cycle.
     *
     * ===============================================================
     */
void
CAND_LIST :: Clear_All_Cands_Tried_Mark (void) {
    
    _tried_cand_num = 0 ;

    if (_try_version < MAX_TRY_VERSION) {

       ++ _try_version ;
       return ;

    } else {
        
        _try_version = MIN_TRY_VERSION ;

        for (CAND_LIST_ITER iter(this) ; ! iter.done(); iter.step ()) {
            iter.cur ()->_try_ver = MIN_TRY_VERSION; 
        }
    }
}

    /* ===========================================================
     * 
     *  Evict_Unqualified_Cands 
     * 
     *  drove all "unqualified" candidate out of CAND_LIST. 
     *  func <quad_func> define the qualification.
     * 
     * ===========================================================
     */

INT16
CAND_LIST :: Evict_Unqualified_Cands (
    CAND_QUALIFICATION_CHECK * quaf_func, void* parm) 
{
    INT16 evict_num = 0 ;

    for (CANDIDATE * tmp = _cand_lst_head._next ; 
         tmp != &_cand_lst_tail ; ) {

        if (!(*quaf_func)(tmp, parm)) {
            CANDIDATE * cand = tmp ;
            tmp = tmp -> _next ;
            Erase_Cand (cand); 
            evict_num  ++ ;
        } else {
            tmp = tmp -> _next ;
        }
    }

    return evict_num ;
}




    /* ==========================================================
     * ==========================================================
     *
     *  Implementation of CAND_MGR
     *
     * =========================================================
     * =========================================================
     */
CAND_MGR :: CAND_MGR (MEM_POOL* mp) :
    _mp(mp), 
    _m_ready_cands(mp,this,TRUE),
    _p_ready_cands(mp,this,FALSE) {

    _free_cand = NULL ;
    _free_unresolved_deps = NULL;
    _free_bookeepings = NULL;
}


    /* ======================================================
     *
     *   New_Unresolved_Dep,Free_Unresolved_Dep 
     *  
     *  Allocate and free UNRESOLVED_DEP structure.
     *
     * ======================================================
     */
#define UNRESOLVED_DEP_INC (64)
UNRESOLVED_DEP* 
CAND_MGR :: New_Unresolved_Dep (void) {

    UNRESOLVED_DEP* t;
    if (!_free_unresolved_deps) {
        t = TYPE_MEM_POOL_ALLOC_N (UNRESOLVED_DEP, _mp, UNRESOLVED_DEP_INC);

        for (INT i = 0; i < UNRESOLVED_DEP_INC; i++) {
            t[i].Set_Alloc_By_Cand_Mgr ();
            t[i]._next = &t[i+1];
        }
        t[UNRESOLVED_DEP_INC-1]._next = NULL;
        _free_unresolved_deps = t;
    }
    
    t = _free_unresolved_deps;
    _free_unresolved_deps = t->_next ;

    t->Init (); 
    t->Set_Alloc_By_Cand_Mgr ();

    return t;
}

void
CAND_MGR :: Free_Unresolved_Dep (UNRESOLVED_DEP* item) {

    if (item && item->Is_Alloc_By_Cand_Mgr ()) {
        item->_next = _free_unresolved_deps;
        _free_unresolved_deps = item ;
    }
}

    /* ====================================================
     *
     *  New_Empty_Bookeeping, Free_Bookeeping 
     *
     *  allocate and free BOOKEEPING
     *
     * ===================================================
     */
#define BOOKEEPING_ALLOC_INC (64)
BOOKEEPING*
CAND_MGR :: New_Empty_Bookeeping (void) {

    BOOKEEPING* t;
    if (_free_bookeepings) {

        t = _free_bookeepings;
        _free_bookeepings = _free_bookeepings->Next (); 

        t->Init ();
        t->Set_Alloc_By_Cand_Mgr ();

        return t;
    }

    t = TYPE_MEM_POOL_ALLOC_N 
            (BOOKEEPING, _mp,BOOKEEPING_ALLOC_INC);

    _free_bookeepings = t;
    for (INT i = 0; i < BOOKEEPING_ALLOC_INC; i++) {
        t[i]._next = &t[i+1];    
        t[i].Set_Alloc_By_Cand_Mgr ();
    }
    t[BOOKEEPING_ALLOC_INC-1]._next = NULL;
        
    return New_Empty_Bookeeping ();
}

    /* =======================================================
     *
     *  Calc_Useful_Exec_Prob 
     *
     *  ref the header file for details.
     *
     * ======================================================
     */
void
CANDIDATE :: Calc_Useful_Exec_Prob 
    (BB* targ, RGN_CFLOW_MGR* cflow_info) {
    
    BB* home = OP_bb(Op ());

    _useful_exec_prob = 
        cflow_info->Reachable_Prob (targ, home);

    if (Is_M_Ready ()) return ;

    float prob = 0.0f;
    for (BOOKEEPING* bk = _bookeepings.First_Item ();
         bk;
         bk = _bookeepings.Next_Item (bk)) {
        

        if (bk->Is_P_Ready_Bookeeping ()) {

            BB* b = bk->Get_Placement ();
            prob += cflow_info->Reachable_Prob (targ,b) *
                 cflow_info->Reachable_Prob (b, home) /
                 (float)REACH_PROB_SCALE;
        }
    }

    _useful_exec_prob -= (PROBABILITY)prob;
}

    /* =====================================================
     * =====================================================
     *
     *      class CAND_MGR's implementation comes here.
     *
     * =====================================================
     * =====================================================
     */
     
void
CAND_MGR :: Free_Bookeeping (BOOKEEPING* bk) {
        
    if (!bk->Is_Alloc_By_Cand_Mgr ()) {
        DevWarn ("BOOKEEPING struct is not allocated by CAND_MGR");
        return;
    }

    bk->_next =  _free_bookeepings;
    _free_bookeepings = bk;
}


    /* ====================================================
     *
     *  Alloc_Cand, Reclaim_Free_Cand 
     * 
     * allocate and free CANDIDATE
     *
     * ====================================================
     */

void
CAND_MGR :: Reclaim_Free_Cand (CANDIDATE* cand) {

    if (cand->Alloc_By_Cand_Mgr ()) {
        cand->_next = _free_cand ;
        _free_cand = cand ;
    }

    cand->Free_Unresolved_Dep_Lst ();
    cand->Free_Bookeeping_Lst ();
}

#define CAND_ALLOC_INC (8)
CANDIDATE*
CAND_MGR :: Alloc_Cand (void) {

    CANDIDATE* t; 

    if (!_free_cand) {

        CANDIDATE* t = CXX_NEW (CANDIDATE(this,_mp), _mp) ;
        t->Set_Alloc_By_Cand_Mgr ();
        Reclaim_Free_Cand (t);
    }
    
    t = _free_cand;
    _free_cand = t->_next;
    
    t->Init ();
    t->Set_Alloc_By_Cand_Mgr ();

    return t; 
}

void
CAND_MGR :: Erase_Cand (CANDIDATE* cand) {

    if (cand->Is_Attached_To_Cand_Lst ()) {
        if (cand->Is_M_Ready ()) { 
            _m_ready_cands.Erase_Cand(cand); 
        } else { 
            _p_ready_cands.Erase_Cand (cand); 
        }
    } else {
        Reclaim_Free_Cand (cand);
    }
}

    /* check to see whether <src> can potentially donate P-ready
     * candidate to <targ>
     */
BOOL
Can_BB_Potentially_Donate_P_Ready_Cands 
    (BB * src, BB * targ, RGN_CFLOW_MGR * cflow_info) {
                            
    return FALSE;
}

    /* =======================================================================
     * =======================================================================
     *
     *              implementation of SRC_BB_MGR
     *
     * =======================================================================
     * =======================================================================
     */

SRC_BB_MGR::SRC_BB_MGR (MEM_POOL *mp) : 
    _mp(mp), 
    _src_bbs_vect(BB_ALLOC(mp)),
    _src_info_vect (SRC_BB_INFO_ALLOC(mp)) {

    _src_bbs_set = BB_SET_Create (PU_BB_Count+2, _mp);
    _targ = NULL;
    _scope = NULL;
    _prepass = FALSE;

    _find_src_bbs_access_bbs  = BB_SET_Create_Empty (PU_BB_Count + 2, _mp);
    _find_src_bbs_access_rgns = BS_Create_Empty (64/* estimated */, _mp);
}

SRC_BB_MGR :: ~SRC_BB_MGR (void) {
}

  /* ===================================================================
   * 
   *  _find_src_bbs & Find_Src_BBs :
   * 
   *  find out all BBs which potentially donate candidates to 
   *  <_targ>(member of SRC_BB_MGR) .
   *
   * ===================================================================
   */
BOOL
SRC_BB_MGR :: _src_bb_is_qualified (BB* src, SRC_BB_INFO* its_info, 
                                   RGN_CFLOW_MGR* cflow_info) {
    if (src != _targ) {
        
        BB_VECTOR* bb_vect ; 

        bb_vect = its_info->Get_Cutting_Set ();
        for (BB_VECTOR_ITER iter = bb_vect->begin() ; 
             iter != bb_vect->end (); iter++) {

            if (!IPFEC_Glos_Motion_Across_Calls && 
                 BB_call(*iter)) {
                return FALSE ;
            }
        }

        bb_vect = its_info->Move_Across_Or_Around_BBs ();
        for (BB_VECTOR_ITER iter = bb_vect->begin () ;
             iter != bb_vect->end () ; iter ++) {
            
            if (!IPFEC_Glos_Motion_Across_Calls && 
                BB_call(*iter) &&
                    /* p-ready candidate can by-pass call rather than 
                     * being moved across it 
                     */
                !IPFEC_Glos_Enable_P_Ready_Code_Motion) {
                return FALSE ;
            }
        }

            /* this fragment is not fit the purpose of this routine
             * very much. But it may prevent some low-reach-prob 
             * block from becoming an candidate-bb and hence save
             * some compilation time.
             */
        REACH_PROB rp = cflow_info->Reachable_Prob (_targ,src);
        if (rp < SAFE_CNTL_SPEC_PROB   &&
            rp < UNSAFE_CNTL_SPEC_PROB) {
            return FALSE;
        }

        /* fall-thru and routine returns TRUE */
    }

    return TRUE ;
}

    /* ===================================================================
     *
     *  ::_find_src_bbs
     *
     *  Support routine for Find_Src_BBs. _find_src_bbs try to find out
     *  all BBs that are reachable from nested-<rgn> and potentially donate 
     *  candidates to <_targ>.
     *
     *  <n> is the REGIONAL_CFG_NODE of the REGION which immediatly nests 
     *  <rgn>. 
     *
     * ===================================================================
     */
void
SRC_BB_MGR :: _find_src_bbs (REGION* rgn, 
                             REGIONAL_CFG_NODE* n,
                             RGN_CFLOW_MGR* cflow_info) {

    Is_True (n->Region_Node () == rgn,
             ("n->Region_Node and rgn(%d) does not match !",rgn->Id()));
    
    if (BS_MemberP (_find_src_bbs_access_rgns, rgn->Id())) {
        return ;
    }

    _find_src_bbs_access_rgns = 
        BS_Union1D (_find_src_bbs_access_rgns, rgn->Id(), _mp);

    for (CFG_SUCC_NODE_ITER succ_iter(n);
         succ_iter != 0; 
         ++succ_iter) {
        
        if ((*succ_iter)->Is_Region ()) {
            REGION* r = (*succ_iter)->Region_Node ();
            _find_src_bbs(r, *succ_iter, cflow_info);
        } else {
           _find_src_bbs ((*succ_iter)->BB_Node (),cflow_info);
        }
    }
}

    /* ===================================================================
     *
     *  ::_find_src_bbs
     *
     *  Support routine for Find_Src_BBs. _find_src_bbs try to find out
     *  all BBs that are reachable from <src> and potentially donate 
     *  candidates to <_targ>.
     *
     * ===================================================================
     */
void
SRC_BB_MGR :: _find_src_bbs (BB* src, RGN_CFLOW_MGR* cflow_info) {

    _find_src_bbs_access_bbs = 
        BB_SET_Union1D (_find_src_bbs_access_bbs, src,_mp);


    if (_prepass) {
        if (BB_entry (src) || BB_exit(src)) {
            return ;
        }
    }

        /* <src> is "isolated" from schedule scope, (mostly for the 
         * debugging purpose), Do *NOT* schedule any OP out of it. 
         */
    if (BB_Is_Isolated_From_Sched (src)) return ;

    SRC_BB_INFO* bb_info = CXX_NEW (SRC_BB_INFO(_mp), _mp);
    bb_info->Set_Src_BB (src);
    bb_info->Set_Target_BB (_targ);

    if (!_compute_cutting_set (src, bb_info,cflow_info) ||
        !_src_bb_is_qualified (src, bb_info,cflow_info)) {
        return ;
    }

        /* now, <src> are qualifed being an "source" BB.
         * we determine whether <src> qualified to donate
         * P-ready candidates.
         */
    if (src != _targ) {

        Determine_BB_Can_Donate_P_Ready_Cand_Or_Not 
            (bb_info,cflow_info) ;

    }

    _src_bbs_set = BB_SET_Union1D (_src_bbs_set, src,_mp);
    _src_bbs_vect.push_back (src);
    _src_info_vect.push_back (bb_info);

    if (!_scope) {
        /* we does not specify global-scope (regional-cfg),
         * hence, current schedule scope is confined within
         * an BB, namely <_targ>, which should has *ONLY* one
         * "source" BB -- <_targ> itself.
         */ 
        
        Is_True (src == _targ, 
                 ("BB:%d should not donate candidte to BB:%d!",
                   BB_id(src), BB_id(_targ)));
        return ;
    }


    /* Check whether <src>'s desendants are also qualified as 
     * "source" BB (to <_targ> BB)
     */
    for (CFG_SUCC_NODE_ITER succ_iter(Regional_Cfg_Node(src));
         succ_iter != 0; 
         ++succ_iter) 
    {
        if ((*succ_iter)->Is_Region()) {

            REGION * r = (*succ_iter)->Region_Node ();
            _find_src_bbs(r, *succ_iter, cflow_info);
            _find_src_bbs_access_rgns = 
                BS_Union1D (_find_src_bbs_access_rgns, r->Id(), _mp);

        } else {

            BB * succ_bb = (*succ_iter)->BB_Node();
            if (BB_SET_MemberP (_find_src_bbs_access_bbs, succ_bb)) {
                continue ;
            }

            _find_src_bbs_access_bbs = 
                BB_SET_Union1D (_find_src_bbs_access_bbs, succ_bb,_mp);

            if (BB_exit (succ_bb) || BB_Is_Isolated_From_Sched (succ_bb)) {
                continue ;
            }

            _find_src_bbs (succ_bb, cflow_info) ;
        }
    }
}


    /* ===============================================================
     *
     *  Find_Src_BBs 
     *
     *  Find out all BBs (in the REGION <scope>) that can potentially
     *  donate candidates to <targ>.
     *
     * ===============================================================
     */
const BB_VECTOR * 
SRC_BB_MGR::Find_Src_BBs (REGION * scope , BB* targ,
                          RGN_CFLOW_MGR * cflow_info, 
                          BOOL prepass) 
{
    Is_True (!BB_Is_Isolated_From_Sched (targ), 
             ("BB:%d is isolated from schedule", BB_id(targ)));

    _src_bbs_vect.clear ();
    _src_info_vect.clear ();
    _src_bbs_set = BB_SET_ClearD (_src_bbs_set);

    _targ = targ ;
    _scope = scope ;
    _prepass = prepass; 

    _find_src_bbs_access_bbs  = BB_SET_ClearD (_find_src_bbs_access_bbs);
    _find_src_bbs_access_rgns = BS_ClearD (_find_src_bbs_access_rgns);

    _find_src_bbs (targ, cflow_info);
    
    return &_src_bbs_vect;
}


    /* ====================================================================
     *
     *  _ubs_union1d, _ubs_union1d,  _ubs_memberp, _ubs_memberp
     *  _ubs_memberp, _ubs_union1d etc
     *  
     *  ref the header file for details.
     *
     *  !!! NOTE: THESE 6 ROUTINES ARE CALLED ONLY BY <_compute_cutting_set>.
     *            THEY ARE *NOT* GENERAL PURPOSE FUNCS!
     * 
     * =====================================================================
     */  
inline BS *
SRC_BB_MGR :: _ubs_union1d (BS * Bitset, BB * bb) {
    return BS_Union1D (Bitset, BB_id(bb),_mp);
}

inline BS * 
SRC_BB_MGR :: _ubs_union1d (BS * Bitset, REGION *r, INT32 rgn_id_base) {
   return BS_Union1D (Bitset, r->Id () + rgn_id_base,_mp);
}

inline BS *
SRC_BB_MGR :: _ubs_union1d 
    (BS *Bitset, REGIONAL_CFG_NODE *n, INT32 rgn_base_id) {

    if (n->Is_Region ()) {
        return _ubs_union1d (Bitset, n->Region_Node(), 
                             rgn_base_id);
    }

    return _ubs_union1d (Bitset, n->BB_Node()) ;
}


inline BS *
SRC_BB_MGR :: _ubs_diff1d  (BS * Bitset, BB * bb) {
    return BS_Difference1D (Bitset, BB_id(bb)) ;
}

inline BS *
SRC_BB_MGR :: _ubs_diff1d  (BS * Bitset, REGION *r, 
                            INT32 rgn_id_base) {
    return BS_Difference1D (Bitset, r->Id () + rgn_id_base);
}

inline BS *
SRC_BB_MGR :: _ubs_diff1d  (BS * Bitset, REGIONAL_CFG_NODE *n,
                            INT32 rgn_id_base) {

    if (n->Is_Region ()) {
        return _ubs_diff1d (Bitset, n->Region_Node (), rgn_id_base);   
    } 

    return _ubs_diff1d (Bitset, n->BB_Node());
}


inline BOOL
SRC_BB_MGR :: _ubs_memberp (BS * Bitset, BB *b) {
   return BS_MemberP (Bitset, BB_id(b));
}

inline BOOL
SRC_BB_MGR :: _ubs_memberp (BS * Bitset, REGION *r, INT32 rgn_id_base) {
    return BS_MemberP (Bitset, r->Id() + rgn_id_base);
}

inline BOOL
SRC_BB_MGR :: _ubs_memberp (BS *Bitset, REGIONAL_CFG_NODE *n, 
                            INT32 rgn_base_id) {

    if (n->Is_Region ()) {
        return _ubs_memberp (Bitset, n->Region_Node(), rgn_base_id);
    }

    return _ubs_memberp (Bitset, n->BB_Node());
}


    /* ==============================================================
     *
     *  _compute_cutting_set 
     *
     *  Compute the cutting set for the code motion from <src>
     *  to <_targ> and keep the cutting set in src_info if at 
     *  least one cutting-set is found.
     *
     *  return TRUE iff we find one cutting-set, FALSE otherwise.
     *
     *  TODO: divide the large routine into some small ones.   
     *
     * ==============================================================
     */
BOOL
SRC_BB_MGR::_compute_cutting_set (BB * src, SRC_BB_INFO *src_info,
                                  RGN_CFLOW_MGR * cflow_info) {
    src_info->Get_Cutting_Set () -> clear () ;
    src_info->Move_Across_Or_Around_BBs () -> clear ();
    src_info->Move_Across_Or_Around_Nested_Rgns ()->clear ();

    if (_targ == src) {

        src_info->Set_Src_BB (src) ;
        src_info->Get_Cutting_Set ()->push_back (src);

        return TRUE;
    }


    const INT32 rgn_id_base = cflow_info->Max_BB_Id () + 1;

        /* keep track of the nodes we have accessed
         */
    NODE_VECTOR visited_nodes_v(_mp); 
    BS * visited_nodes_bs = BS_Create_Empty (
                                cflow_info->Max_BB_Id  () + 
                                cflow_info->Max_Rgn_Id () + 2,
                                _mp);
        
        /* keep track of the nodes we need moving across 
         */
    BB_VECTOR       across_bbs  (_mp);
    REGION_VECTOR   across_rgns (_mp);


        /* cutting-set BBs 
         */
    BS* siss_node_set = BS_Create_Empty (
                            cflow_info->Max_BB_Id  () + 
                            cflow_info->Max_Rgn_Id () + 2,
                            _mp);


    visited_nodes_v.push_back (::Regional_Cfg_Node(src));
    visited_nodes_bs = _ubs_union1d (visited_nodes_bs, src);


    siss_node_set = _ubs_union1d (siss_node_set, src);
     
    while (!_ubs_memberp (siss_node_set, _targ)) {

        BOOL changed = TRUE;
        while (changed) {

                /* stepping over every node we have accessed 
                 */
            changed = FALSE ;

            for (INT32 vect_idx = 0 ; 
                vect_idx < visited_nodes_v.size() ; 
                vect_idx ++) {

                REGIONAL_CFG_NODE * member = visited_nodes_v[vect_idx]; 
                if (!_ubs_memberp(siss_node_set, member, rgn_id_base)) {

                    /* member->BB_Node() is *OBVIOUSLY* not an 
                     * element of cutting-set, this fact has already 
                     * been identified in the previous iterations
                     * of the outer while-statement, but it remains
                     * undeleted since deleting an element of an 
                     * vector is quite expensive.
                     */
                    continue ;
                }

                if (!member->Is_Region ()) {

                    BB * b = member->BB_Node ();

                    if (b == _targ) { continue ; }
                
                    if (!cflow_info->BB1_Reachable_From_BB2(b,_targ) && 
                        !BB_Is_Isolated_From_Sched (b)) {
                        continue ;
                    }
                }

                    /* Now, we can draw the conclusion that member is not 
                     * qualified being an element of cutting-set.
                     */
                siss_node_set = _ubs_diff1d(siss_node_set,
                                            member,
                                            rgn_id_base);
                changed = TRUE;


                for (CFG_PRED_NODE_ITER pred_iter(member); 
                     pred_iter != 0; 
                     ++pred_iter) {
                        
                    if (_ubs_memberp (visited_nodes_bs, 
                                      *pred_iter,
                                      rgn_id_base)) {
                        continue ;
                    }

                    visited_nodes_bs = 
                        _ubs_union1d (visited_nodes_bs, *pred_iter,rgn_id_base);

                    visited_nodes_v.push_back (*pred_iter);
                    siss_node_set = _ubs_union1d (siss_node_set, 
                                                  *pred_iter,
                                                  rgn_id_base);

                    if ((*pred_iter)->Is_Region ()) {

                        /* pred is nested REGION 
                         */
                        REGION * r = (*pred_iter)->Region_Node () ;

                        if (!IPFEC_Glos_Code_Motion_Across_Nested_Rgn ||
                             No_OP_Can_be_Moved_Across_Region (r)) {
                            return FALSE;
                        }

                        /* workaround for Edge_Splitting which currently does not 
                         * split critical edge leading from an nested region 
                         */
                        if (cflow_info->BB_Reachable_From_RGN (_targ, r)) {
                            return FALSE ;
                        }

                        across_rgns.push_back ((*pred_iter)->Region_Node());

                    } else {

                        BB * pred_bb = (*pred_iter)->BB_Node () ;
                        if (BB_Is_Isolated_From_Sched (pred_bb)) {
                            return FALSE;
                        }

                        across_bbs.push_back ((*pred_iter)->BB_Node());
                    }

                } /* end of for(CFG_PRED_NODE_ITER ...(member) */

            } /* end of for (INT32 vect_idx = 0; ...) */

        } /* end of nested while (change) */

    } /* while (_ubs...) */



    BB_VECTOR * siss_p = src_info->Get_Cutting_Set ();
    BB_VECTOR * across_bbs_p= src_info->Move_Across_Or_Around_BBs ();

    for (BB_VECTOR_ITER iter = across_bbs.begin() ; 
         iter != across_bbs.end () ; 
         iter ++) {

        BB * b = *iter ;
        if (_ubs_memberp(siss_node_set, b)) {
            siss_p->push_back (b);
        } else if (b != src) {
            across_bbs_p->push_back (b);
        }
    }

    *(src_info->Move_Across_Or_Around_Nested_Rgns ()) = across_rgns ;

    return TRUE;

}


    /* ==================================================================
     *
     * ::Cutting_Set 
     * 
     * return the cutting-set for the code motion from <src> to <_targ>
     * (a data member of class SRC_BB_MGR).
     *
     * ==================================================================
     */
const BB_VECTOR * 
SRC_BB_MGR :: Cutting_Set (BB * src) {

    Is_True (BB_SET_MemberP (_src_bbs_set, src), 
             ("BB:%d is not candidate BB of BB:%d", BB_id(src), BB_id(_targ)));
                
    for (SRC_BB_INFO_ITER iter = _src_info_vect.begin () ; 
         iter != _src_info_vect.end () ; iter ++) {
        SRC_BB_INFO  * info = *iter ;
        if (info->Source_BB () == src)  {
            return info->Get_Cutting_Set ();
        }
    }

    Is_True (FALSE, ("Fail to find BB-info for BB:%d", BB_id(src))); 

}

    /* ===================================================================
     *
     *  ::BBs_Between_Cutting_Set_and_Src 
     * 
     *  return the cutting-set for the code motion from <src> to <_targ>.
     *
     * ===================================================================
     */
const BB_VECTOR * 
SRC_BB_MGR :: BBs_Between_Cutting_Set_and_Src (BB * src) {

    Is_True (BB_SET_MemberP (_src_bbs_set, src), 
             ("BB:%d is not candidate BB of BB:%d", BB_id(src), BB_id(_targ)));
                
    for (SRC_BB_INFO_ITER iter = _src_info_vect.begin () ; 
         iter != _src_info_vect.end () ; iter ++) {

        SRC_BB_INFO* info = *iter ;
        if (info->Source_BB () == src) {
            return info->Move_Across_Or_Around_BBs ();
        }

    }

    Is_True (FALSE, ("Fail to find BB-info for BB:%d", BB_id(src))); 

}

    /* =================================================================
     *
     *  ::Moved_Across_Nested_Rgns 
     *
     *  Returns all nested REGIONs (in REGION_VECTOR) that when code 
     *  motion from <src> to <_targ> occurs, we need move instruction
     *  across these REGIONS.
     *
     * =================================================================
     */
const REGION_VECTOR*
SRC_BB_MGR :: Move_Across_Or_Around_Rgns (BB *src) {
    
    Is_True (BB_SET_MemberP (_src_bbs_set, src),
                ("BB:%d is not candidate BB of BB:%d", BB_id(src), BB_id(_targ)));
    
    for (SRC_BB_INFO_ITER iter = _src_info_vect.begin () ;
         iter != _src_info_vect.end () ; iter++) {
        
        SRC_BB_INFO * info = *iter ;
        if (info->Source_BB () == src) {
            return info->Move_Across_Or_Around_Nested_Rgns ();
        }
    }

    Is_True (FALSE, ("Fail to find BB-info for BB:%d", BB_id(src))); 
}

    /* =================================================================
     *
     *  ::Get_Src_Info 
     *
     *  returns SRC_BB_INFO associated with <bb>
     *
     * =================================================================
     */
SRC_BB_INFO * 
SRC_BB_MGR::Get_Src_Info (BB * bb) {

    Is_True (Is_Src_BB (bb), ("BB:%d is not Soruce-BB", BB_id(bb)));
   
    for (SRC_BB_INFO_ITER iter = _src_info_vect.begin() ;
         iter != _src_info_vect.end() ; iter++) {
        if ((*iter)->Source_BB () == bb)  {
            return *iter; 
        }
    }

    Is_True (FALSE, ("fail to find SRC_BB_INFO for BB:%d", BB_id(bb)));
    return NULL;

}

    /* ==========================================================
     *
     * Calc_Cutting_Set_Between_Src_And_Targ 
     *
     * compute the lowest cutting set between src block and 
     * target block
     *
     * ==========================================================
     */
BOOL
SRC_BB_MGR::Calc_Cutting_Set_Between_Src_And_Targ 
    (BB_VECTOR* bbv, BB* src,RGN_CFLOW_MGR* cflow_info) {

    bbv->clear ();

    if (src == _targ) { return FALSE ;}
    Is_True (cflow_info->BB1_Reachable_From_BB2 (src, _targ),
             ("BB:%d is not reachable from BB:%d", 
              BB_id(src), BB_id(_targ)));

    REGIONAL_CFG_NODE* node      = Regional_Cfg_Node (src);
    REGIONAL_CFG_NODE* targ_node = Regional_Cfg_Node (_targ);

    while (TRUE) {

        REGIONAL_CFG_NODE* t = node->Unique_Pred ();
        if (!t) {
            for (CFG_PRED_NODE_ITER iter(node); iter != 0; ++iter) {
                if ((*iter)->Is_Region ()) {
                    return FALSE;
                }

                BB* b = (*iter)->BB_Node ();
                if (BB_call(b) || BB_Is_Isolated_From_Sched (b)) {
                    return FALSE;
                }

                bbv->push_back (b);
            }

            return bbv->size() ? TRUE : FALSE;

        } else {

            node = t; 
            if (node == targ_node) { return FALSE; }

        }

    } /* end of while(TRUE) */

    FmtAssert (FALSE, ("We should not reach here"));
    return FALSE; /* just to make compiler happy */
}

void
SRC_BB_MGR :: Determine_BB_Can_Donate_P_Ready_Cand_Or_Not 
    (SRC_BB_INFO* bb_info,
     RGN_CFLOW_MGR* cflow_info) {
    
    BB* dest = bb_info->Target_BB ();
    BB* src  = bb_info->Source_BB ();

    bb_info->Set_Cannot_Donate_P_Ready_Cand (); 

    if (cflow_info->Reachable_Prob (dest, src) < 
        DONATE_P_READY_CAND_BB_REACH_PROB || 
        dest == src) {
        return; 
    }

    BB_VECTOR* bv = bb_info->Get_P_Ready_Bookeeping_Blks (); 
    if (!Calc_Cutting_Set_Between_Src_And_Targ 
            (bv,bb_info->Source_BB (), cflow_info)) {

        return;
    }

    if (!cflow_info->Path_Info_Is_Valid ()) {
        return;
    }

    if (!IPFEC_Glos_Enable_P_Ready_Code_Motion) {
        return;
    }

    bb_info->Set_Can_Donate_P_Ready_Cand ();
}

