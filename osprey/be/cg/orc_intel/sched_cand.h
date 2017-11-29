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

#ifndef sched_cand_INCLUDED
#define sched_cand_INCLUDED

 /* ======================================================================
  * ======================================================================
  * 
  *  Module sched_cand.h
  *
  *  $Revision: 1.1.1.1 $
  *  $Date: 2005/10/21 19:00:00 $
  *  $Author: marcel $
  *  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/sched_cand.h,v $
  *
  *  Description:
  *  ============
  *  this module implements candidates management. We put all candidates 
  *  of same type (M-ready or P-ready) into an candidate list. different
  *  type of candidates are managed by separately, but this is out of 
  *  scope of this module.
  * 
  *
  *
  *  Exported interface:
  *  
  *  o. CANDIDATE 
  *         data structure used to describe an candidate.
  *
  *  o. class CAND_LIST 
  *
  *    - CAND_LIST (MEM_POOL * mp,BOOL m_ready_cand) 
  *    - ~CAND_LIST (void) 
  *         constructor & destructor 
  * 
  *    - BOOL OP_Is_In_Cand_List (OP *op); 
  *         return TRUE iff candidate-list has already take control of <op> 
  *
  *    - CANDIDATE * Get_Candidate (OP* op);
  *         return CANDIDATE structure associated with <op>, NULL if 
  *         <op> is not managed by cand-list.
  *
  *    - void Erase_Cand (OP *op) 
  *    - void Erase_Cand (CANDIDATE * cand)
  *         evict candidate <op> or <cand> from list.
  *
  *    - void Erase_All_Cand (void) 
  *         make candidate list empty
  * 
  *    - CANDIDATE *Create_Empty_Cand (void)
  *         create a vacant CANDIDATE structure, 
  *         NOTE: all CANDIDATE structures should be allocated by 
  *         CAND_LIST.
  *
  *    - void Add_Candidate (CANDIDATE * cand)
  *         add <cand> to CAND_LIST
  *
  *    - BOOL Cand_Has_Been_Tried (CANDIDATE * cand) 
  *         check to see whether <cand> has already been tried to be
  *         scheduled but failed.
  * 
  *    - void Set_Cand_Has_Been_Tried (CANDIDATE* cand) ;
  *         mark <cand> has been "tried".
  *
  *    - void Clear_Cand_Tried_Mark (CANDIDATE *cand) 
  *    - void Clear_All_Cands_Tried_Mark (void);
  *         clear <cand>(or all candidates in CAND_LIST)'s "tried" flag
  *
  *    - BOOL All_Cands_Have_Been_Tried (void) 
  *         return TRUE iff all candidates in cand-list has been "tried".
  *
  *    - INT16 Cand_In_Total(void) 
  *         return the number of candidates this list hosts
  * 
  *    - INT16 Tried_Cand_Num (void)
  *         how many candidates in this list have been "tried" to be scheduled
  *         and yet failed. 
  *
  *    - BOOL Cand_Lst_Is_Empty(void)
  *         return TRUE iff list host no candidates.
  *
  *    - INT16 Evict_Unqualified_Cands (CAND_QUALIFICATION_CHECK * quaf_func, 
  *                                     void * parm)
  *         evict all unqualified candidates from list. the qualification is 
  *         checked by <quaf_func> with <parm> being its parameter which 
  *         make sense only to <quaf_func>.
  *
  *    - void Dump (FILE * f=stderr, BOOL verbose=FALSE, 
  *                 FAVOR_DELAY_HEUR * heur=NULL)
  *         Dump list's current status, we may obtains more info when <verbose>
  *         set to non-FALSE. If <heur> is passed, this routine will print out
  *         heuristic-related data associated with each candidate.
  *
  *
  *
  *  o. class CAND_LIST_ITER  
  *  o. class CAND_LIST_RITER 
  * 
  *         iterator used to step over each CANDIDATE managed by CAND_LIST.
  *         CAND_LIST_ITER step candidate list from front to end, 
  *         CAND_LIST_RITER works in reverse order.
  *
  *         NOTE: Do not erase CANDIDATE when iterates thru cand-list unless
  *               this is performed by an integrated func 
  *               'erase_cur_and_advance()'
  *
  * ======================================================================
  * ======================================================================
  */ 

#include "ipfec_defs.h"
#include "bb.h"

#include "mempool.h"
#include "mempool_allocator.h"

#include "sched_path.h"
   
class FAVOR_DELAY_HEUR;
class UNRESOLVED_DEP_LST;
class BOOKEEPING_LST;
class CAND_MGR;

    /* structure to describe the unresolved dependency 
     */
class UNRESOLVED_DEP {
friend class UNRESOLVED_DEP_LST;
friend class CAND_MGR;

private:

    ARC*            _arc;
    UNRESOLVED_DEP  *_next,*_prev;
    UNRESOLVED_DEP_LST* _lst;
    
    UNRESOLVED_DEP* Next (void) const { return _next; }
    UNRESOLVED_DEP* Prev (void) const { return _prev; }
    UNRESOLVED_DEP_LST* Get_List (void) const { return _lst; }

    enum {
        URD_ALLOC_BY_CAND_MGR = 0x1,
    };
    SPEC_TYPE _spec_type;
    UINT16    _flags;  

    void Set_Alloc_By_Cand_Mgr (void) { _flags |= URD_ALLOC_BY_CAND_MGR;}
    BOOL Is_Alloc_By_Cand_Mgr (void) const 
        { return URD_ALLOC_BY_CAND_MGR & _flags; }

    void Init (void) {_arc = NULL; _spec_type = SPEC_NONE; 
                      _flags = 0;_next = NULL;}

public:

    UNRESOLVED_DEP (void)  { Init (); }
    ~UNRESOLVED_DEP (void) { /* do nothing */ }

        /* accessors */
    ARC*     Arc (void) const { return _arc ; }
    void Set_Arc (ARC* Arc)   { _arc = Arc  ; }

    SPEC_TYPE Spec_Type (void) const   { return _spec_type; }
    void  Set_Spec_Type (SPEC_TYPE st) { _spec_type = st ;  }

    OP* Pred (void) const { return ARC_pred(_arc); }
    OP* Succ (void) const { return ARC_succ(_arc); }

}; 

    /* UNRESOLVED_DEP's doublely-linked list.
     */
class UNRESOLVED_DEP_LST {

friend class CAND_MGR;

private:
    UNRESOLVED_DEP *_header,*_tail;
    CAND_MGR*  _cand_mgr;

public:

    UNRESOLVED_DEP_LST (CAND_MGR* cand_mgr):_cand_mgr(cand_mgr) 
        { _header = _tail = NULL; }
    UNRESOLVED_DEP_LST (CAND_MGR* cand_mgr,INT size);

    UNRESOLVED_DEP* Next_Item (UNRESOLVED_DEP* item) {
                        Is_True (item->Get_List () == this, 
                                 ("item is not in this list"));
                        return item ? item->Next () : NULL;
                    }

    UNRESOLVED_DEP* Prev_Item (UNRESOLVED_DEP* item) const {
                        Is_True (item->Get_List () == this, 
                                 ("item is not in this list"));
                        return item ? item->Next () : NULL;
                    }

    UNRESOLVED_DEP* First_Item (void) const { return _header ; }
    UNRESOLVED_DEP* Last_Item (void)  const { return _tail;    }

    void Delete_Item (UNRESOLVED_DEP* dep);

    void Prepend (UNRESOLVED_DEP* item); 
    void Append  (UNRESOLVED_DEP* item);

    void Free_Whole_Lst (void) { while(_header){ Delete_Item (_header);}}

    BOOL Is_Empty (void) const { return _header == NULL; }

    UNRESOLVED_DEP_LST& operator=(const UNRESOLVED_DEP_LST& urds);

    ~UNRESOLVED_DEP_LST (void) { Free_Whole_Lst (); }
};

    /* structure describing compensation.
     */
class BOOKEEPING {

friend class BOOKEEPING_LST;
friend class CAND_MGR;

private:
    
    BOOKEEPING *_prev, *_next;
    BOOKEEPING_LST* _bklst;

    enum {
        P_READY_BOOKEEPING = 0x1,
        DUP_BOOKEEPING     = 0x2,
        ALLOC_BY_CAND_MGR  = 0x4,
    };

    UINT _flags;
    BB* _placement;

    BOOKEEPING* Prev (void) const { return _prev; }
    BOOKEEPING* Next (void) const { return _next; }

    BOOKEEPING_LST* Get_List (void) { return _bklst; }

    void Set_Alloc_By_Cand_Mgr (void) { _flags |= ALLOC_BY_CAND_MGR;}
    BOOL Is_Alloc_By_Cand_Mgr (void) { return _flags & ALLOC_BY_CAND_MGR;}

    void Init (void) { _flags = 0; _placement = NULL; 
                       _prev = _next = NULL; _bklst = NULL; }

public:

    BOOKEEPING  (void) { Init (); }
    ~BOOKEEPING (void);

    BOOL Is_P_Ready_Bookeeping (void) const 
        { return _flags & P_READY_BOOKEEPING; }
    void Set_P_Ready_Bookeeping (void) 
        { _flags |= P_READY_BOOKEEPING; }

    BOOL Is_Dup_Bookeeping (void) const 
        { return _flags & DUP_BOOKEEPING; }
    void Set_Dup_Bookeeping (void) 
        { _flags |= DUP_BOOKEEPING; }
        

    BB* Get_Placement (void) const { return _placement; }
    void Set_Placement (BB* place) { _placement = place;}
};


class BOOKEEPING_LST {
friend class CAND_MGR;

private:
    BOOKEEPING *_header, *_tail;
    CAND_MGR*  _cand_mgr;

public:

    BOOKEEPING_LST (CAND_MGR* cand_mgr):_cand_mgr(cand_mgr) 
        { _header = _tail = NULL; }
    BOOKEEPING_LST  (INT size);
    ~BOOKEEPING_LST (void) { }

    BOOKEEPING* Next_Item (BOOKEEPING* item) {
                    Is_True (item->Get_List () == this, 
                             ("item is not in the list"));
                    return item ? item->Next () : NULL;
                }

    BOOKEEPING* Prev_Item (BOOKEEPING* item) {
                    Is_True (item->Get_List () == this, 
                             ("item is not in the list"));
                    return item ? item->Prev () : NULL;
                }

    BOOKEEPING* First_Item (void) const { return _header; }
    BOOKEEPING* Last_Item  (void) const { return _tail;  }

    void Delete_Item (BOOKEEPING* item);

    BOOKEEPING* Retrieve (BB* place) {
                    for (BOOKEEPING* bk = _header ; bk; bk = bk->_next) {
                        if (bk->Get_Placement () == place) return bk;
                    }
                    return NULL;
                }

    void Append (BOOKEEPING* item);
    void Prepend (BOOKEEPING* item);

    void Free_Whole_Lst (void) { while(_header){Delete_Item(_header);}}
};


    /* =============================================================
     * ============================================================= 
     *
     *       Structure used to describe candidate
     *
     * =============================================================
     * =============================================================
     */
class CANDIDATE {

friend class CAND_LIST ;
friend class CAND_LIST_ITER ;
friend class CAND_LIST_RITER ;
friend class CAND_MGR;

private:
    MEM_POOL* _mp; 
    CANDIDATE* _prev, *_next ; /* internal doublely linked list */

    INT16  _try_ver;    /* time stamp used to keep the latest 
                         * trial of being scheduled.
                         * this field make sense only to  
                         * CAND_LIST.
                         */
    OP*  _op;  /* associated OP */
    SPEC_TYPE  _spec_type; 
    BOOL _if_converted;
    
    enum {
        CAND_IS_P_READY = 0x1,
        CAND_ATTACH_TO_CAND_LST = 0x2,
        CAND_SAFE_LOAD  = 0x4,
        CAND_ALLOC_BY_CAND_MGR = 0x8,
    };

    UINT16 _flags;
    PORT_SET _mandatory_ip;
    CAND_MGR* _cand_mgr; /* its manager */

    EXEC_PATH_SET _move_against_paths;
    REGION_VECTOR _move_across_rgns;

    UNRESOLVED_DEP_LST _violated_deps;  /* violated dependencies */
    BOOKEEPING_LST _bookeepings; 
    EXEC_PATH_SET  _pready_bookeeping_paths; 
    PROBABILITY _useful_exec_prob; 

    void Init (void) ; /* initialization */

    CANDIDATE (CAND_MGR* cand_mgr, MEM_POOL* mp);

    void Set_Cand_Attached (void) { _flags |= CAND_ATTACH_TO_CAND_LST; }
    void Set_Cand_Detached (void) { _flags &= CAND_ATTACH_TO_CAND_LST; }

    void Set_Alloc_By_Cand_Mgr (void) { _flags |= CAND_ALLOC_BY_CAND_MGR;}
    BOOL Alloc_By_Cand_Mgr (void) { return _flags & CAND_ALLOC_BY_CAND_MGR;}    

public : 

    ~CANDIDATE (void);

        /* about speculation type
         */
    BOOL Is_Spec (void)        {  return _spec_type != SPEC_NONE ; } 
    BOOL Is_If_Converted()     {  return _if_converted; }
    void Set_If_Converted(BOOL b)	{  _if_converted = b; }
    SPEC_TYPE Spec_Type (void) {  return _spec_type ;              }
    SPEC_TYPE Get_Up_to_Date_Spec_Type (void) ;
    void Set_Spec(SPEC_TYPE t) { _spec_type = t ; }
    void Add_Spec(SPEC_TYPE t) { _spec_type = SPEC_TYPE(_spec_type | t); }
    void Del_Spec(SPEC_TYPE t) { _spec_type = SPEC_TYPE(_spec_type & ~t);}

    BOOL Is_Load      (void) const { return OP_load (_op); }
    BOOL Is_Safe_Load (void) const { return _flags & CAND_SAFE_LOAD; }

    BOOL Is_P_Ready  (void) const { return _flags & CAND_IS_P_READY;   } 
    BOOL Is_M_Ready  (void) const { return !(_flags & CAND_IS_P_READY);}
    void Set_P_ready (void) { _flags |= CAND_IS_P_READY;  }
    void Set_M_ready (void) { _flags &= ~CAND_IS_P_READY; }

        /* Move-against execution path set & P-ready bookeeping path set.
         */
    EXEC_PATH_SET* Move_Against_Path_Set(void) {
                return &_move_against_paths;
            }

    EXEC_PATH_SET* P_Ready_Bookeeping_Path_Set (void) {
                return &_pready_bookeeping_paths;
            }

    PROBABILITY Useful_Exec_Prob (void) { return _useful_exec_prob; }
    void Calc_Useful_Exec_Prob (BB* targ, RGN_CFLOW_MGR* cflow_info);

    OP* Op (void) const { return _op ; }
    void Set_OP (OP *op) { _op = op ; 
                if (OP_load(op) && Load_Has_Valid_Vaddr(op)) {
                    _flags |= CAND_SAFE_LOAD;
                } else {
                    _flags &= ~CAND_SAFE_LOAD;
                }
                _mandatory_ip = TSI_Issue_Ports (OP_code(op));
            }
    
    OP* Get_Cmp_OP_Of_If_Converted_Code(void);
    	
    PORT_SET* Mandatory_Issue_Ports (void) const { 
              } 
    BOOL Is_Attached_To_Cand_Lst (void) const { 
                return _flags & CAND_ATTACH_TO_CAND_LST;
            }

    UNRESOLVED_DEP_LST* Unresolved_Dep_List(void){ return &_violated_deps;}

    void Free_Unresolved_Dep_Lst (void) {
                _violated_deps.Free_Whole_Lst (); 
            }

    BOOKEEPING_LST* Bookeeping_Lst (void) { return &_bookeepings; }
    void Free_Bookeeping_Lst (void) { _bookeepings.Free_Whole_Lst (); }

    BOOL Shadowed_By_P_Ready_Bookeeping 
            (BB* bb, RGN_CFLOW_MGR* cflow_info);
    BOOL Shadowed_By_P_Ready_Bookeeping 
            (REGION* r, RGN_CFLOW_MGR* cflow_info);
    BOOL Shadowed_By_P_Ready_Bookeeping 
            (REGIONAL_CFG_NODE* n, RGN_CFLOW_MGR* cflow_info) {

                return n->Is_Region () ? 
                    Shadowed_By_P_Ready_Bookeeping 
                        (n->Region_Node (), cflow_info) :
                    Shadowed_By_P_Ready_Bookeeping 
                        (n->BB_Node (), cflow_info);
            }

    REGION_VECTOR* Move_Across_Rgns (void) { return &_move_across_rgns;} 

    void Dump (FILE *f=stderr,BOOL verbose=FALSE,
               FAVOR_DELAY_HEUR * heur=NULL); 
};

INT32 Determine_Cand_Init_Etime (BB* targ, OP* cand);


   /* =========================================================
    * =========================================================
    * 
    *      CAND_LIST : hold M-ready or P-ready candidates 
    * 
    *  M-ready and P-ready should be put in distinct list. 
    * 
    * ==========================================================
    * ==========================================================
    */
typedef BOOL (*CAND_QUALIFICATION_CHECK) (CANDIDATE * cand,void * parm);

class CAND_LIST {

friend class CAND_LIST_ITER ;
friend class CAND_LIST_RITER ;
friend class CAND_MGR;

private:
    MEM_POOL* _mp ;  /* underlying memory pool */
    CAND_MGR* _cand_mgr;

    INT16  _total ;   /* candidate in total */
    INT16  _tried_cand_num ;   /* this many candidates have been 
                                * tried to be scheduled but failed */
    mBOOL  _host_m_ready_cand ; /* indicate whether CAND_LIST_MGR host 
                                 * M-ready or P-ready cand */

      /* candidate try version stuff : 
       *        
       *   type version is used as a time-stamp. It is used to mark
       *   time when a candidate is tried to scheduled but fails. 
       *   
       *   when candidate's try-version <= <_try_version>, it indidates
       *   the candidate in question has not been tried yet. 
       * 
       *   the variable <_try_version> keep the latest time-stamp, 
       *   each time a candidate is tried, it is time-stamp or try-version
       *   is set to <_try_version> plus one. 
       *  
       *   to reset all candidate has not been tried, we can simply 
       *   advance _try_version by 1. 
       *
       */ 

    #define MAX_TRY_VERSION (0x7fffffff)
    #define MIN_TRY_VERSION 0 
    INT32  _try_version ;


    BS  *  _op_map_idx; /* used to quickly answer the question 
                         * "Is OP in candidate list?" with a 
                         * resounding NO. 
                         */
        /* candidate list led by <_cand_lst_head> and end at 
         * <_cand_lst_tail>, both of them are not valid 
         * canidate, them only serve as sentinal to ease 
         * list manipulation.
         */
    CANDIDATE _cand_lst_head, _cand_lst_tail ;   



        /* CANDIDATE internal doublely-linked-lst operation 
         */
    void Insert  (CANDIDATE * prev, CANDIDATE * next, CANDIDATE *item) {
            item -> _next = next ; item -> _prev = prev ;
            prev -> _next = item ; next -> _prev = item ;
         }

    void Prepend (CANDIDATE *item) {
            Insert (&_cand_lst_head, _cand_lst_head._next, item);
         }

    void Append  (CANDIDATE *item) {
            Insert (_cand_lst_tail._prev, &_cand_lst_tail, item);
         }

    void Detach (CANDIDATE * item) {
            Is_True (item != &_cand_lst_tail && item != &_cand_lst_head, 
                     ("can not delete list head or tail"));
            item -> _next -> _prev = item -> _prev ;
            item -> _prev -> _next = item -> _next ;
            item -> _next = item -> _prev = NULL ;
        }

    /* allocate new candidate and reclaim free-candidte
     */

     void Init (void);   /* Initialize CAND_LIST */


public:
    
    CAND_LIST (MEM_POOL* mp,CAND_MGR* cand_mgr, BOOL m_ready_cand) ;
    ~CAND_LIST (void) {} ;

    BOOL OP_Is_In_Cand_List (OP *op); 
    CANDIDATE * Get_Candidate (OP* op);

    void Erase_Cand (OP*op, BOOL abort_if_not_belong_lst = TRUE) ;
    void Erase_Cand (CANDIDATE* cand);

    void Erase_All_Cand (void) ;

    CANDIDATE *Create_Empty_Cand (void);

    void Add_Candidate (CANDIDATE * cand);

    inline BOOL Cand_Has_Been_Tried (CANDIDATE * cand) {
            return cand->_try_ver > _try_version ;
         }

    void Set_Cand_Has_Been_Tried (CANDIDATE* cand) ;
    void Clear_Cand_Tried_Mark (CANDIDATE *cand) ;
    void Clear_All_Cands_Tried_Mark (void);
    
    inline BOOL All_Cands_Have_Been_Tried (void) {
                    return _tried_cand_num == _total ;
                }

    inline INT16 Cand_In_Total     (void) { return _total ; } ;
    inline INT16 Tried_Cand_Num    (void) { return _tried_cand_num ; } ;
    inline BOOL  Cand_Lst_Is_Empty (void) { return Cand_In_Total () == 0 ; } 

    INT16  Evict_Unqualified_Cands (CAND_QUALIFICATION_CHECK * quaf_func, void * parm);

    void   Dump (FILE * f=stderr, BOOL verbose=FALSE, 
                 FAVOR_DELAY_HEUR * heur=NULL); 

    #ifdef Is_True_On
    void   gdb_dump (FAVOR_DELAY_HEUR * heur) ;
    #endif 
};

class CAND_MGR {

friend class CANDIDATE;
friend class CAND_LIST;

private:
    MEM_POOL* _mp;
    CAND_LIST _m_ready_cands, _p_ready_cands;
    
    UNRESOLVED_DEP* _free_unresolved_deps;
    BOOKEEPING*     _free_bookeepings;

        /* allocate and reclaim free CANDIDATE
         */
    CANDIDATE* _free_cand;
    void Reclaim_Free_Cand (CANDIDATE* cand) ;
    CANDIDATE* Alloc_Cand (void);


public:

    CAND_MGR (MEM_POOL* mp);
    ~CAND_MGR (void) { };

    CAND_LIST* M_Ready_Cand_List (void) { return &_m_ready_cands; }
    CAND_LIST* P_Ready_Cand_List (void) { return &_p_ready_cands; }

    BOOL OP_Is_In_Cand_List (OP* op) {
            return _m_ready_cands.OP_Is_In_Cand_List (op) || 
                   _p_ready_cands.OP_Is_In_Cand_List (op);
         }

        /* Allocate and reclaim free CADIDATE
         */
    CANDIDATE* Create_Empty_Cand (void) { return Alloc_Cand () ; }
    void Erase_Cand (CANDIDATE* cand);

        /* UNRESOLVED_DEP stuff
         */
    UNRESOLVED_DEP* New_Unresolved_Dep (void)  ;
    void  Free_Unresolved_Dep (UNRESOLVED_DEP* dep) ;

        /* BOOKEEPING stuff 
         */
    BOOKEEPING* New_Empty_Bookeeping (void);
    void Free_Bookeeping (BOOKEEPING* bk);

        /* convenient interface 
         */
    CANDIDATE* Get_Candidate (OP* op) {
                  CANDIDATE* cand = _m_ready_cands.Get_Candidate (op);
                  return cand ? cand : _p_ready_cands.Get_Candidate (op);
               }

    CAND_LIST* Get_Cand_List (CANDIDATE* cand) {
                 return cand->Is_M_Ready () ? &_m_ready_cands :
                                              &_p_ready_cands ;
               }

    void Clear_All_Cands_Tried_Mark (void) {
                    _m_ready_cands.Clear_All_Cands_Tried_Mark ();
                    _p_ready_cands.Clear_All_Cands_Tried_Mark ();
               }

    BOOL Cand_Lst_Is_Empty (void) {
                   return _m_ready_cands.Cand_Lst_Is_Empty () &&
                          _p_ready_cands.Cand_Lst_Is_Empty () ;
               }

    BOOL All_Cands_Have_Been_Tried (void) {
                   return _m_ready_cands.All_Cands_Have_Been_Tried () &&
                          _p_ready_cands.All_Cands_Have_Been_Tried () ;
               }
};

    
    /* ===========================================================

     * ===========================================================
     *
     * An iterator type for looping over the elemnts of all 
     * candidates of a paticular candidate list
     *
     * ===========================================================
     * ===========================================================
     */
class CAND_LIST_ITER {
    
private :
    
    CAND_LIST * _cand_lst;
    CANDIDATE * _cur ;

public :

    CAND_LIST_ITER (CAND_LIST * cand_lst) : 
        _cand_lst (cand_lst),
        _cur(cand_lst->_cand_lst_head._next) { } 

    ~CAND_LIST_ITER (void) { /* do nothing */ } 

    CAND_LIST_ITER& begin (void) {
        _cur = _cand_lst->_cand_lst_head._next ;
        return *this ;
    }

    void  step (void) { _cur = _cur->_next; };
    CANDIDATE* cur (void) const { return _cur; };
    BOOL  done (void) const { return _cur == &_cand_lst->_cand_lst_tail ; }
    void  erase_cur_and_advance (void) {
            Is_True (!done(), ("could not erase list tail!"));
                CANDIDATE * tmp = _cur ;
            step ();
            _cand_lst->Erase_Cand (tmp); 
          }
};

/* ===========================================================
 *
 * Looping over the elemnts of all candidates of a paticular 
 * candidate list in reverse order.
 *
 * ===========================================================
 */

class CAND_LIST_RITER {
    
private :
    
    CAND_LIST * _cand_lst;
    CANDIDATE * _cur ;

public :

    CAND_LIST_RITER (CAND_LIST * cand_lst) : 
        _cand_lst (cand_lst),
        _cur(cand_lst->_cand_lst_tail._prev) {} 

    ~CAND_LIST_RITER (void) {} ;

    CANDIDATE * begin (void) {
        return _cur = _cand_lst->_cand_lst_tail._prev;
    }

    CANDIDATE * cur (void)  { return _cur; };
    void  step (void) { _cur = _cur->_prev; };
    BOOL  done (void) { return _cur == &_cand_lst->_cand_lst_head; }
    void  erase_cur_and_advance (void) {
                            Is_True (!done(), ("could not erase list tail!"));
                            CANDIDATE * tmp = _cur ;
                            step ();

                            _cand_lst->Erase_Cand (tmp); 
           }
};


    /* ======================================================================
     * ======================================================================
     * 
     *  SRC_BB_INFO, SRC_BB_MGR
     *  ============================================
     *
     *  BBs that potentially donate candidates are call source BB.
     *  Each SRC_BB_INFO describe each src source BB and all of them
     *  are under the control of SRC_BB_MGR.
     *
     *  DONATE_P_READY_INFO are used when src block can donate P-ready 
     *  candidate.
     *
     * ======================================================================
     * =====================================================================
     */

class SRC_BB_INFO {

friend class SRC_BB_MGR;

private:
    BB* _src ;    
    REGIONAL_CFG_NODE* _src_node;
    BB* _targ ;
    BB_VECTOR _siss ;  /* cutting set for code motion from 
                        * <src> to <targ> 
                        */
                       
                       /* P-ready compensation code should be
                        * appended to some of (NOT all) these
                        * blocks.
                        */
    BB_VECTOR _pready_bookeeping;

    BB_VECTOR _middle_bbs;  /* BBs that OP of <src> should move across 
                             * or around(for P_ready candidate) when 
                             * schedule occurs.
                             */
    REGION_VECTOR _middle_nested_rgns; 

    mBOOL _donate_p_ready_cand ; /* permit <src> donate p-ready candidate 
                                  * or not */

    mBOOL _cntl_equiv;  /* flags indidate whether code motion from 
                         * <src> to <targ> is control equivalent.
                         */


    void Set_Cannot_Donate_P_Ready_Cand (void) {_donate_p_ready_cand=FALSE;}
    void Set_Can_Donate_P_Ready_Cand (void) {_donate_p_ready_cand=TRUE;}

    void Set_Src_BB (BB* b) { _src = b; }
    void Set_Target_BB (BB* b) { _targ = b; }

public:

        /* Accessors */
    BB* Source_BB (void) const { return _src ; }
    BB* Target_BB (void) const { return _targ; }
    REGIONAL_CFG_NODE* Src_Node (void) { return _src_node; }
    
        /* the control flow relation of target block and src block:
         * there are control equvalent or not.
         */
    BOOL Is_Cntl_Equiv (void) const { return _cntl_equiv ; }
    void Set_Cntl_Equiv (void) { _cntl_equiv = TRUE ; }

    
        /* blocks candidates should be moved across or around if 
         * schedule is actually performed.
         */
    BB_VECTOR* Move_Across_Or_Around_BBs (void) { return &_middle_bbs; }
    void Add_Across_BBs (BB* b) { _middle_bbs.push_back (b); }

    REGION_VECTOR* Move_Across_Or_Around_Nested_Rgns (void) 
            { return &_middle_nested_rgns; }

    BB_VECTOR* Get_Cutting_Set (void) { return &_siss; }

        /* P-ready candidate related stuff.
         */
    BOOL Can_Donate_P_Ready_Cand (void) const 
            { return _donate_p_ready_cand; }

    BB_VECTOR* Get_P_Ready_Bookeeping_Blks (void) 
            { return &_pready_bookeeping;   }

        /* constructor and destructor
         */
    SRC_BB_INFO (MEM_POOL *mp) : 
        _siss(mp), _middle_bbs(mp),
        _middle_nested_rgns(mp) {

        _src = NULL ;
        _cntl_equiv = FALSE;
        _donate_p_ready_cand = FALSE;
    }

    ~SRC_BB_INFO (void) {} ;

};

class SRC_BB_MGR {
private :

    typedef mempool_allocator<SRC_BB_INFO * > SRC_BB_INFO_ALLOC;
    typedef std::vector<SRC_BB_INFO *, SRC_BB_INFO_ALLOC>  SRC_BB_INFO_VECT; 
    typedef SRC_BB_INFO_VECT::iterator       SRC_BB_INFO_ITER;

    MEM_POOL*  _mp;           /* underlying MEM_POOL */

    BB_VECTOR  _src_bbs_vect;   /* all source BBs */
    SRC_BB_INFO_VECT _src_info_vect ;  

    BB_SET*  _src_bbs_set ;  /* store all source BBs into a set for 
                                 * fast query an check */
    BB*      _targ ;         /* the BBs that SCHEDULER is not now engaging
                                 * with */
    REGION*  _scope ; 
    BOOL     _prepass;


            /* compute the cutting-set for code motion from <src> to <_targ>
             */
    BOOL _compute_cutting_set (BB *src, SRC_BB_INFO* src_info,
                               RGN_CFLOW_MGR *cflow_info) ;
    BOOL Calc_Cutting_Set_Between_Src_And_Targ 
        (BB_VECTOR* bbv, BB* src,RGN_CFLOW_MGR* cflow_info) ;

            /* this routines is called only by Find_Src_BBs */
    void _find_src_bbs (BB * src,    RGN_CFLOW_MGR * cflow_info) ;
    void _find_src_bbs (REGION *rgn, REGIONAL_CFG_NODE * n,
                                     RGN_CFLOW_MGR *cflow_info);

           /* keep track of the nodes Find_Src_BBs accesss 
            */
    BB_SET * _find_src_bbs_access_bbs ;
    BS     * _find_src_bbs_access_rgns ;

            /* check to see whether <src> is qualified to donate candidate
             * to <_targ>
             */
    BOOL _src_bb_is_qualified (BB *src, SRC_BB_INFO* its_info, 
                               RGN_CFLOW_MGR * cflow_info);

             /* following 9 inline routines are used only by 
              * _compute_cutting_set. we try to use a single bitset to 
              * keep track both BB and nested REGION. the prefix
              * _ubs stands for "uniform bitset".
              */
    inline BS * _ubs_union1d (BS * Bitset, BB * bb) ;
    inline BS * _ubs_union1d (BS * Bitset, REGION *r, INT32 rgn_id_base);
    inline BS * _ubs_union1d (BS * Bitset, REGIONAL_CFG_NODE *n,
                                       INT32 rgn_id_base);
    inline BS * _ubs_diff1d  (BS * Bitset, BB * bb) ;
    inline BS * _ubs_diff1d  (BS * Bitset, REGION *r,
                                       INT32 rgn_id_base);
    inline BS * _ubs_diff1d  (BS * Bitset, REGIONAL_CFG_NODE *n,
                                       INT32 rgn_id_base);

    inline BOOL _ubs_memberp (BS * Bitset, BB *b) ;
    inline BOOL _ubs_memberp (BS * Bitset, REGION *r, 
                                INT32 rgn_id_base);
    inline BOOL _ubs_memberp (BS *Bitset, REGIONAL_CFG_NODE *n, 
                                INT32 rgn_id_base);
    

    void Determine_BB_Can_Donate_P_Ready_Cand_Or_Not 
            (SRC_BB_INFO* bb_info, RGN_CFLOW_MGR* cflow_info);


        /* EXPORTED INTERFACES 
         */
public:

    SRC_BB_MGR (MEM_POOL *mp) ;
    ~SRC_BB_MGR (void) ;

            /* find all BBs that can potentially donate candidates to <targ>
             */ 
    const BB_VECTOR* Find_Src_BBs (REGION * scope, BB * targ,
                                    RGN_CFLOW_MGR *cflow_info, 
                                    BOOL prepass) ;

            /* return source BBs of <_targ> (data member of this class) 
             */
    const BB_VECTOR* Src_BBs (void) { return &_src_bbs_vect ; } 

            /* return the cutting-set for code motion from <src> to <_targ>
             * with which scheduler is now dealing.*/
    const BB_VECTOR* Cutting_Set (BB * src) ;     

            /* check to see whether <bb> is one of the source-BB of <_targ> 
             */
    BOOL Is_Src_BB (BB * bb) { return  BB_SET_MemberP (_src_bbs_set, bb); }

            /* return the block scheduler is now scheduling */
    BB* Targ_BB (void) { return _targ ; }   
            
            /* return all BBs that code motion from <src> to <_targ> need
             * moving across 
             */
    const BB_VECTOR* BBs_Between_Cutting_Set_and_Src (BB *src);
            
            /* return all REGIONs that code motion from <src> to <_targ> need
             * moving across
             */
    const REGION_VECTOR* Move_Across_Or_Around_Rgns (BB *src) ;
    
            /* return SRC_BB_INFO associated with <bb> 
             */
    SRC_BB_INFO* Get_Src_Info (BB * bb);

            /* dump the status of this class 
             */
    void Dump (FILE *f=stderr) ;

#ifdef Is_True_On
    void gdb_dump (void);    
#endif 

};



    /* ============================================================
     * ============================================================
     *
     *          Miscellaneous 
     *
     * ============================================================
     * ============================================================
     */

    /* check to see whether <src> can potentially donate P-ready 
     * candidate to <targ>.
     */
class RGN_CFLOW_INFO ;
BOOL Can_BB_Potentially_Donate_P_Ready_Cands (BB * src, BB* targ, 
                                              RGN_CFLOW_MGR *cflow_info);

#endif /*sched_cand_INCLUDED*/ 
