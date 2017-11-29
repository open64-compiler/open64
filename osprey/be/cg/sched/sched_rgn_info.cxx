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

 /* =========================================================================
  * =========================================================================
  * 
  * Module: sched_rgn_info.cxx
  * $Revision: 1.1 $
  * $Date: 2005/12/30 01:50:22 $
  * $Author: weitang $
  * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/sched_rgn_info.cxx,v $
  *
  * Revision comments:
  *
  * 4-January-2002 - Initial version
  *
  * Description:
  * ============
  *
  *    Implementation of Region summary 
  *
  * =========================================================================
  * =========================================================================
  */
#include "defs.h"
#include "ipfec_defs.h"
#include "ipfec_options.h"

#include "tracing.h"
#include "errors.h"

#include "be_util.h"

#include "register.h"

#include "sched_rgn_info.h"
#include "tn_set.h"

void
RGN_CFG_PATH_INFO :: Derive_RGN_CFG_PATH_INFO_from_REGION_Node
        (RGN_SUMMARY * sum) {

    if (!sum->Summary_Is_Valid ()) {

        /*  Region's summary is not avaiable. Under this circumstance,
         *  we do not guess how the nested region looks like, rather,
         *  We set each field of RGN_CFG_PATH_INFO "conservative" enough
         *  to prevent scheduler from moving any instruction across
         *  this region. Since (<summary_unknown> != FALSE) invalidate
         *  any fields in RGN_CFG_PATH_INFO, we simplily set this field
         *  "TRUE".  
         */
        _summary_unknown  = TRUE ;         
        return ;
    }

    _rgn_num         = 1 ; /* we now encounter a nested region */
    _has_call        = sum->Has_Call  ();
    _has_load        = sum->Has_Load  ();
    _has_store       = sum->Has_Store ();
    _has_rotating_kernel = sum->Has_Rotating_Kernel ();

    _summary_unknown = FALSE ;

    _killed_def = TN_SET_CopyD (_killed_def, sum->Killed_Def(), _mp);
    _tn_used    = TN_SET_CopyD (_tn_used,    sum->TN_Used   (), _mp);
}

void
RGN_CFG_PATH_INFO :: Derive_RGN_CFG_PATH_INFO_from_BB_Node (BB * bb) {
    
    _killed_def = TN_SET_ClearD (_killed_def);
    _tn_used    = TN_SET_ClearD (_tn_used);

    if (!(_has_call = BB_call(bb))) {

            /* currently we do not allow code motion across region
             * which contiains a call. so, any other fields of 
             * RGN_CFG_PATH_INFO is invalid if <has_call> field
             * is set.
             */
        OP * op ;
        FOR_ALL_BB_OPs (bb, op) {
            
            if (OP_load (op)) { _has_load = TRUE ; }
            if (!_has_store && OP_like_store(op)) {
                _has_store = TRUE ;
            }

            for (INT i = OP_results(op) - 1 ; i >= 0 ; --i) {
                TN * result_tn = OP_result(op,i);

                if (TN_is_register(result_tn) && 
                    !TN_is_const_reg(result_tn)) {
                    _killed_def = 
                        TN_SET_Union1D (_killed_def, result_tn, _mp);
                }

            } /* end of for (INT i...--i) */

            for (INT i = OP_opnds(op) - 1 ; i >= 0 ; --i) {
                TN * opnd = OP_opnd(op,i);

                if (TN_is_register(opnd) && !TN_is_const_reg(opnd)) {
                   _tn_used = 
                        TN_SET_Union1D (_tn_used, opnd, _mp);
                }
            } /* end of for (INT i..--i) */

        } /* end of FOR_ALL_BB_OPs */

        _has_rotating_kernel = BB_rotating_kernel (bb) ? TRUE : FALSE;

    } /* end of if(!has_call) */

    _bb_num     = 1;
    _rgn_num    = 0;
    _summary_unknown = FALSE ;
}
     


class RGN_SUM_LEAF_NODE {
private :
    BB * _bb ; /* associated BB */
    RGN_SUM_LEAF_NODE * _next ; /* next leaf */
    MEM_POOL * _mp ;
    
public:
    RGN_SUM_LEAF_NODE (MEM_POOL * mp);
    ~RGN_SUM_LEAF_NODE (void);
    
    BB * Associated_BB (void) { return _bb ; }
    void Set_Associated_BB (BB * bb) { _bb = bb ;} 
};

class RGN_SUM_PATH {
private :
    RGN_SUM_ROOT_NODE * _from ;  /* this path leads from <_from> */
    RGN_SUM_LEAF_NODE * _to   ;  /* and extends toward <_to> */ 

    MEM_POOL * _mp;

    mINT16   _bb_num ;        /* how many BBs along this path */
    TN_SET*  _killed_defs ;   /* keep track of the killed defs along 
                               * this path */
    BOOL    _has_call ;
    BOOL    _has_cannot_move_across_RGN ;

public :

    RGN_SUM_PATH  (MEM_POOL *mp) ;
    ~RGN_SUM_PATH (void) ;

    RGN_SUM_ROOT_NODE * From (void) { return _from ; }
    RGN_SUM_LEAF_NODE * To   (void) { return _to   ; }
    
    BOOL Path_Has_Call (void) { return _has_call ;   }
    BOOL Path_Has_Cannot_Move_Across_Rgn (void) { 
            return _has_cannot_move_across_RGN ;
         }

    BOOL Can_OP_be_Hoisted_Along_Path (OP *op) ;
};


class RGN_SUM_ROOT_NODE {
private :
    RGN_SUM_ROOT_NODE * _next ;
    RGN_SUM_LEAF_NODE * _leaf_nodes ;
    BB  * _bb ;  /* associated block */
    MEM_POOL * _mp ;

public:
    RGN_SUM_ROOT_NODE (MEM_POOL *mp) ;
    ~RGN_SUM_ROOT_NODE (void) ;

    RGN_SUM_ROOT_NODE * Next_Root_Node (void)   { return _next ; }
    RGN_SUM_LEAF_NODE * First_Leaf_Node (void)  { _leaf_nodes ;  }

    BB * Associated_BB (void) { return _bb ; }
    void Set_Associated_BB (BB * bb) { _bb = bb ;} 
};



    /* ====================================================================
     * ====================================================================
     *
     *      Implementation of RGN_SUM_LEAF_NODE 
     *
     * ====================================================================
     * ====================================================================
     */
RGN_SUM_LEAF_NODE :: RGN_SUM_LEAF_NODE (MEM_POOL *mp) {
    _bb     = NULL;
    _next   = NULL; 
    _mp     = mp ;
}

RGN_SUM_LEAF_NODE :: ~RGN_SUM_LEAF_NODE (void) {
}
    
    /* ====================================================================
     * ====================================================================
     *
     *      Implementation of RGN_SUM_PATH
     *
     * ====================================================================
     * ====================================================================
     */
RGN_SUM_PATH :: RGN_SUM_PATH (MEM_POOL *mp) {

    _from       = NULL ; 
    _to         = NULL ;
    _bb_num     = 0 ; 
    _mp         = mp;
    _killed_defs = TN_SET_Create_Empty (Last_TN,mp);
    _has_call   = TRUE;
   _has_cannot_move_across_RGN = TRUE;
}

RGN_SUM_PATH :: ~RGN_SUM_PATH (void) {}

    /* ====================================================================
     * ====================================================================
     *
     *      Implementation of RGN_SUM_PATH
     *
     * ====================================================================
     * ====================================================================
     */
RGN_SUM_ROOT_NODE :: RGN_SUM_ROOT_NODE (MEM_POOL *mp) {
    _next  = NULL;
    _leaf_nodes = NULL;
    _bb = NULL;
    _mp = mp ;
}

RGN_SUM_ROOT_NODE :: ~RGN_SUM_ROOT_NODE (void) {}


    /* ====================================================================
     * ====================================================================
     *
     *      Implementation of RGN_SUM_PATH
     *
     * ====================================================================
     * ====================================================================
     */

RGN_SUMMARY :: RGN_SUMMARY (REGION *rgn,MEM_POOL *mp) {

    _rgn  = rgn ; _mp   = mp ; _forest = NULL;
    _rgn_type = rgn->Region_Type ();

    _tree_num = 0;
    _summary_is_valid = FALSE ;

    _killed_def = NULL;
    _tn_used    = NULL;
    _has_call   = TRUE;
    _has_nested_rgn = TRUE;
    _has_cannot_move_across_RGN = TRUE ;
}

RGN_SUMMARY :: ~RGN_SUMMARY (void) {} 


    /* ==================================================================
     * ==================================================================
     *
     *      REGION_TREE_INFO impelmentation 
     * 
     * ==================================================================
     * ==================================================================
     */

REGION_INFO_MGR * _rgn_info_mgr = NULL; 


    /* ==============================================================
     *
     *  ::New_RGN_CFG_PATH_INFO 
     * 
     *  Allocate RGN_CFG_PATH_INFO structure ;
     * 
     * ==============================================================
     */
RGN_CFG_PATH_INFO *
REGION_INFO_MGR :: New_RGN_CFG_PATH_INFO (void) {

    if (_free_rgn_cfg_path_info_lnk) {
        RGN_CFG_PATH_INFO * t = _free_rgn_cfg_path_info_lnk;

        _free_rgn_cfg_path_info_lnk = t->Next ();
        t->Set_Next (NULL);

        t->Clear () ;

        return t ;
    }

    return CXX_NEW(RGN_CFG_PATH_INFO(&_mem_pool), &_mem_pool);
}


    /* ==============================================================
     *
     *  ::Free_RGN_CFG_PATH_INFO 
     * 
     * ==============================================================
     */
void
REGION_INFO_MGR :: Free_RGN_CFG_PATH_INFO (RGN_CFG_PATH_INFO * info) {
    
    Is_True (!info->Next (), ("the next field is non-null"));

    info->Set_Next (_free_rgn_cfg_path_info_lnk) ;
    _free_rgn_cfg_path_info_lnk = info ;
}


     /* constructor */
REGION_INFO_MGR :: REGION_INFO_MGR (REGION_TREE *rgn_tree):
     _rgn_tree (rgn_tree) {

    INT32 SIZE = 128; /* arbitrary value. REGION_TREE currently 
                       * does not privoide a interface to query
                       * its nodes number. however, even a large
                       * PU rarely hosts inner regions exceed 
                       * this number.
                       */
    _rgn_2_rgn_info = REGION_MAP_Create (SIZE); 

    /* pre-allocate as many as <PATH_INFO_ELEM_NUM> PATH_INFO
     */
    _free_rgn_cfg_path_info_lnk = NULL;
    #define NODE_INFO_NUM  (64)
    for (INT i = 0 ; i < NODE_INFO_NUM ; i++) {
        Free_RGN_CFG_PATH_INFO (
            CXX_NEW(RGN_CFG_PATH_INFO(&_mem_pool), &_mem_pool));
    }

    Acquire_Rgn_Info ();
}


    /* destructor */
REGION_INFO_MGR :: ~REGION_INFO_MGR (void) {
    _rgn_tree       = NULL;

    if (_rgn_2_rgn_info) {
        REGION_MAP_Delete (_rgn_2_rgn_info);
        _rgn_2_rgn_info = NULL;
    }
}

    /* ===============================================================
     *
     * ::Acquire_Rgn_Info 
     *
     * 1. Collect some attribute of region: including 
     *
     *   o. Is region fragment of "abnormal" loop. ref the comment of
     *      "Is_Abnormal_Loop" for the definition of "abnormal loop" 
     *
     *   o. Determine can we apply our global-code-motion upon 
     *      a region.
     *
     * 2. Allocate a RGN_SUMMARY instance for each REGION in current 
     *    REGION_TREE. At this time(calling Acquire_Rgn_Info), 
     *    RGN_SUMMARY serve only as a placeholder. For the sake of 
     *    correctness, we mark each RGN_SUMMARY as "invalid" so that 
     *    any instruction is prevented from being moved across region.
     *   
     *      After a REGION is (global) scheduled, the summary(RGN_SUMMARY)
     *    of the REGION in question is *ACTUALLY* calculated. 
     *    
     * ================================================================
     */
void
REGION_INFO_MGR :: Acquire_Rgn_Info (void) {

    RGN_INFO * rgn_info     = CXX_NEW (RGN_INFO, &_mem_pool) ;
    rgn_info->skip_reason   = SKIP_RGN_NONE ;
    rgn_info->rgn           = _rgn_tree->Root ();
    rgn_info->summary       = NULL;

    extern BOOL Is_Abnormal_Loop (REGION* region) ;
    rgn_info->in_abnormal_loop = Is_Abnormal_Loop (rgn_info->rgn);
  
    std::list<RGN_INFO *,mempool_allocator<RGN_INFO *> > queue(&_mem_pool) ;
    queue.push_back (rgn_info);

    while (!queue.empty ()) {

        RGN_INFO * tmp = *(queue.begin());
        queue.erase (queue.begin());

            /* emit <rgn> (to <rgn_info_lnk>) : determine wether the 
             * region in question is suitable for global scheduling.
             */
        REGION * rgn = tmp -> rgn ;
        SKIP_SCHED_RGN_REASON skip_reason = SKIP_RGN_NONE ;

        if (IPFEC_Query_Skiplist(glos_skip_rgn, rgn->Id(), Current_PU_Count())) {
            skip_reason = SKIP_RGN_DEBUG ;
            fprintf (stdout, "skip rgn %d\n", rgn->Id ());
        } else if (rgn->Region_Type() == IMPROPER) {
            skip_reason = SKIP_RGN_IMPROPER;
        } else if (rgn->Is_No_Further_Opt()) {
            skip_reason = SKIP_RGN_NO_FURTHER_OPT;
        } else if (rgn->Entries().size() != 1) {
            skip_reason = SKIP_RGN_NON_SEME;
        }

        tmp->Set_Skip_Sched_Reason (skip_reason);

        REGION_MAP_Set (_rgn_2_rgn_info, rgn, tmp);
        
                /* Push all immediate kids into queue and propagate 
                 * "in-abnormal-loop" to kids.
                 */
        for (REGION_KID_ITER kids_iter(rgn) ; kids_iter != 0 ; ++kids_iter) {

            RGN_INFO  * kid_info = CXX_NEW(RGN_INFO, &MEM_phase_pool) ;

            kid_info->Set_Region (*kids_iter);

                 /* Propagate "in abnormal loop" attribute to nested region 
                  */
            kid_info->Set_In_Abnormal_Loop (tmp->in_abnormal_loop ? 
                                            TRUE : 
                                            Is_Abnormal_Loop (*kids_iter));

                /* do not care other fields of RGN_INFO */
            queue.push_back (kid_info);
        }

            /* allocate an RGN_SUMMARY for each REGION in current REGION_TREE 
             */
        RGN_SUMMARY * summary = CXX_NEW (RGN_SUMMARY(rgn,&_mem_pool), 
                                         &_mem_pool);
        summary -> Set_Summary_Is_Invalid ();
        tmp->Set_Summary (summary);
    }
}


    /* ====================================================================
     *
     *  ::Build_Rgn_Summary 
     *
     * ====================================================================
     */
void
REGION_INFO_MGR :: Build_Rgn_Summary (REGION *rgn, 
                                      RGN_CFLOW_MGR * rgn_cflow_info) {
    
    RGN_INFO * rgn_info = Get_Rgn_Info (rgn) ;
    Is_True (rgn_info, ("RGN_INFO associated with <rgn> does not exist!"));

    BOOL can_not_build_summary = 
            !rgn_cflow_info                 || 
            !rgn_cflow_info->Valid ()       ||
            (rgn->Region_Type () == MEME)   ||
            (rgn->Region_Type () == IMPROPER);

    RGN_SUMMARY * summary = rgn_info->Summary ();
    if (!summary) {
        summary = CXX_NEW(RGN_SUMMARY(rgn,&_mem_pool), &_mem_pool);
        rgn_info->Set_Summary (summary);
    }
        
    if (can_not_build_summary) {
        summary->Set_Summary_Is_Invalid () ;
        return ;
    } else {
        
        summary->Set_Summary_Is_Valid () ; /* the summary may be invalidate
                                            * in the following code.
                                            */
    }

    /* 1. bind each cfg node in rgn with an empty RGN_CFG_PATH_INFO 
     */ 
    CFG_NODE_MAP node_info_map (&_mem_pool);
    node_info_map.Initialize_Map (rgn);

    for (TOPOLOGICAL_REGIONAL_CFG_ITER 
         iter (rgn->Regional_Cfg()) ; iter != 0 ; ++ iter) {

        RGN_CFG_PATH_INFO * info = New_RGN_CFG_PATH_INFO ();

        if ((*iter)->Is_Region ()) {
            info -> Derive_RGN_CFG_PATH_INFO_from_REGION_Node
                      (Get_Rgn_Info((*iter)->Region_Node())->Summary ());
        } else {
            info -> Derive_RGN_CFG_PATH_INFO_from_BB_Node 
                      ((*iter)->BB_Node()); 
        }

        node_info_map.Set_Map (*iter, (void*)info);
    }


        /* 2. Topologcially traverse the regional cfg to obtain 
         *    region path info 
         */
    BB * bb_node = NULL ;
    REGION * nested_rgn = NULL; 

    summary -> Init_Def_Kill_Set    () ;
    summary -> Init_Use_TN_Set      () ;
    summary -> Clear_Has_Call       () ;
    summary -> Clear_Has_Load       () ;
    summary -> Clear_Has_Store      () ;
    summary -> Clear_Has_Nested_Rgn () ;
    summary -> Clear_Has_Cannot_Move_Across_Rgn () ;

    INT16 bb_num = 0, rgn_num = 0 ;
    for (TOPOLOGICAL_REGIONAL_CFG_ITER 
         iter (rgn->Regional_Cfg()) ; iter != 0 ; ++ iter) {
       
        RGN_CFG_PATH_INFO * info = 
            (RGN_CFG_PATH_INFO *)node_info_map.Get_Map (*iter);
        
        if (info->Rgn_Summary_Unknown ()) {
           summary->Set_Summary_Is_Invalid () ; 
           break ;
        }
            
        bb_num  += info->BB_Num  (); 
        rgn_num += info->Rgn_Num ();

        if (info->Has_Call ()) {
            summary->Set_Has_Call ();
        }

        if (info->Has_Load ()) {
            summary->Set_Has_Load () ;
        }

        if (info->Has_Store ()) {
            summary->Set_Has_Store () ;
        }

        if (info->Has_has_rotating_kernel()) {
            summary->Set_Has_Rotating_Kernel ();
        }

        summary->Union_Killed_Def (info->Killed_Def ());
        summary->Union_Use_TNs    (info->TN_Used());

    } /* end of for (TOPO...ITER */

    summary->Set_BB_Total_Num (bb_num);

        /* 3. free the memory block occupied by RGN_CFG_PATH_INFO
         */
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(rgn->Regional_Cfg()) ; 
         iter != 0 ; ++iter) {
        
        RGN_CFG_PATH_INFO * info = 
            (RGN_CFG_PATH_INFO *)node_info_map.Get_Map (*iter);
        Free_RGN_CFG_PATH_INFO (info);
    }
}


    /* ====================================================================
     *
     *  Adjust_Liveness_After_OP_Being_Moved_Across_Rgn 
     * 
     *  ref the header file for more details
     *
     * ====================================================================
     */
void
RGN_SUMMARY :: Adjust_Liveness_After_OP_Being_Moved_Across_Rgn (OP * op) {
    
}

    /* ==============================================================
     *
     *  No_OP_Can_be_Moved_Across_Region 
     * 
     *  check to see whether there exist some OPs that can be 
     *  moved across <rgn>.
     *
     *  TODO:
     *      control register presure in more quantiatively approach.
     *
     * ==============================================================
     */
#define LARGE_REGION_MUST_HOST_THIS_MANY_BB  (3)

BOOL
No_OP_Can_be_Moved_Across_Region (REGION *rgn) {
    
    RGN_SUMMARY * sum = Get_Region_Summary (rgn) ;

    Is_True (sum, ("fail to get region's summary for REGION%d", rgn->Id()));
    if (!sum->Summary_Is_Valid ()          ||  
        sum->Has_Cannot_Move_Across_RGN () ||
        sum->Has_Call ()) {
        return TRUE;
    }
    

    switch (rgn->Region_Type ()) {
    case UNKNOWN:
    case MEME:
    case IMPROPER:
        return TRUE;

    case ROOT:
    case SEME:
    case LOOP:
        return FALSE ; 

    default:
        FmtAssert (FALSE, ("Unknown region(RGN%d) type(%d)", 
             rgn->Id(), rgn->Region_Type ()));

    } /* end of switch (rgn->...Type ()) */

    return FALSE;
}

BOOL
Is_Large_Region (REGION *rgn) {

    RGN_SUMMARY * sum = Get_Region_Summary (rgn) ;
    Is_True (sum, ("Fail to get REGION%d's summary", sum->Region()->Id()));
                
    return sum->Get_BB_Total_num () >= 
            LARGE_REGION_MUST_HOST_THIS_MANY_BB;
}

#undef LARGE_REGION_MUST_HOST_THIS_MANY_BB
