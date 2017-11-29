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
  * Module: sched_cflow.h
  * $Revision: 1.1 $
  * $Date: 2005/12/30 01:50:23 $
  * $Author: weitang $
  * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/sched_cflow.h,v $
  *
  * Revision comments:
  *
  * 4-January-2002 - Initial version
  *
  * Description:
  * ============
  *
  * This module implements all control-flow query and manipulation
  * functionality within scheduler. the exported class/routines are:
  * 
  * o. RGN_CFLOW_MGR
  *
  *     provide regional CFG information as well as manipulate cflow on 
  *     behalf of scheduler. see the comment right before RGN_CFLOW_MGR
  *     declaration for more detail.
  * 
  *
  * o. DOMINATOR AND POST-DOMINATOR STUFF
  *   
  *    The presence of "abnormal" loop make the dominator and post-dominator
  *    calculation (by calling Calculate_Dominators) into total mess. 
  *    (ref the if-convertion package for definition of "abnormal" loop).
  *    these routines are provide as workaround to this defect. they 
  *    answer dominator and postdominator in an conservative style.
  * 
  *         - void Calculate_Dominator_Info (REGION_TREE *rgn_tree)
  *             calculate dominator/postdominator inforation, out 
  *             workaround comes into play at this time. 
  *
  *         - BOOL BB1_Postdominate_BB2 (BB * bb1, BB* bb2) 
  *             return TRUE iff <bb1> postdominate <bb2>, FALSE otherwise.
  *
  *         - BOOL BB1_Dominate_BB2 (BB* bb1, BB * bb2) 
  *             return TRUE iff <bb1> dominate <bb2>
  *
  *         - BOOL BB1_BB2_Cntl_Equiv (BB* bb_a, BB * bb_b) 
  *             return TRUE iff <bb_a> control-equivalent with <bb_b>
  * 
  *         - BOOL Free_Dominator_Info_Memory (void) 
  *             it is self-descriptive. 
  *
  *
  * o. PU'S ENTRY & EXIT BLOCK SPLITTING AND MERGING 
  *
  *     When code motion is performed before register allocation phase,
  *     we should not move these kinds of OPs out the exit and entry 
  *     block:
  *         - stack pointer or SP adjustment instruction
  *         - OPs marked with OP_glue flags.
  *         - OPs that are "no_move_before_gra"(these OPs are used to 
  *           restore callee saved register).
  *  
  *     Our approach is: before register allocation, we carve each entry
  *     and exit block into two. Those OPs that has attribute mentioned
  *     above still reside in its home block, and the remaining OPs are 
  *     moved into temp block. these newly created tmp block are added
  *     to global code motion scope. After code motion, we merge those
  *     tmp block back. 
  *
  *         - void Init_Split_PU_Entry_Or_Exit_BB (void)
  *               Initilize the internal data structure. this routine
  *               should be called before splitting occurs. 
  * 
  *         - BB * Split_PU_Entry_BB (BB * entry)
  *               split PU's <entry> BB into two. return the new entry 
  *               block. 
  * 
  *         - BB * Split_PU_Entry_BB (REGION * rgn)
  *               split PU's entry block in <rgn> if any. this routine
  *               appliable only to SEME region and return new entry 
  *               block if splitting really occurs.
  *
  *         - BB * Split_PU_Exit_BB  (BB * exit)
  *               split PU's exit block specified by <exit>, and return
  *               the new exit block.
  *
  *         - void Split_PU_Exit_BB  (REGION * rgn) 
  *               split PU's exit blocks in <rgn> if any. 
  *
  *         - void Merge_Splitted_PU_Entry_BB (BB * splitted_entry) ;
  *               merge temp block specified by <splitted_entry> back 
  *               to where it is carved from. 
  * 
  *         - void Merge_Splitted_PU_Exit_BB (BB * splitted_exit)  ;
  *               merge temp block specified <splitted_exit> back 
  *               to where it is carved from.
  * 
  *         - void Merge_All_Splitted_Entry_and_Exit_BB (void)
  *               merge all splitted block back to where there are 
  *               carved from. 
  * 
  *
  *
  * o. MISCELLANEOUS
  * 
  *         - BB_POS BB_Pos_Analysis (BB * src, BB_VECTOR * cutting_set, 
  *                            RGN_CFLOW_MGR * _cflow_info);
  * 
  *             analyse the control flow relationship between block specified
  *             by <src> and the <cutting_set>. the relationship are describe
  *             the numeration BB_POS
  * 
  *        -  BB_POS (enumeration)
  *
  *             IN_SISS           : <src> is an element of <cutting_set>
  *             ABOVE_SISS        : at least one of element in <cutting_set> 
  *                                 is reachable from <cutting_set>
  *             BELOW_SISS        : o. <src> is not an element of <cutting_set>, and
  *                                 o. it is reachable from at least one
  *                                    element of <cutting_set>
  *             OTHER             : other case that scheduler does not 
  *                                 show any interest.
  * 
  * =========================================================================
  * =========================================================================
  */

#ifndef sched_cflow_INCLUDED  
#define sched_cflow_INCLUDED 

//#if (__GNUC__ == 3)
#include <ext/slist>
//#else
//#include <slist> 
//#endif // __GNUC__ == 3
#include <list>
#include <vector> 
using std::vector;
#include <map>

/* definition used globally in backend */
#include "ipfec_defs.h"

#include "region.h"
#include "region_map.h"

/* tracing facility and commomly used data structure and 
 * utilities
 */
#include "tracing.h"
#include "bb.h"
#include "dominate.h"
#include "sched_util.h"
#include "sched_path.h"

    /* ===============================================================
     * ===============================================================
     *
     *  class RGN_CFLOW_MGR 
     *
     *  exported interface: 
     *  ===================
     *
     *  1. constructor, destructor and initialization
     * 
     *      - RGN_CFLOW_MGR (void)  : constructor
     *      - ~RGN_CFLOW_MGR (void) : destructor
     *
     *      - void Init (REGION * rgn)
     *      - void Init (BB * bb)
     *
     *          acquire control flow information of <rgn>'s regional cfg  
     *          to make local-scheduling share some code with 
     *          global-scheduling in an uniformly flavor, we need 
     *          acquire "control flow information of single BB"
     * 
     *      - BOOL Valid (void) 
     *          check to see whether this class is initialized properly
     *
     *      -INT32 BB_Node_Num (void) 
     *          answer the question "how many BB in current schedule scope?"
     * 
     *      -INT32 RGN_Node_Num (void)
     *          answer the question "how many nested region in current 
     *          schedule scope?"
     *
     *      - INT32 Max_Level (REGIONAL_CFG_NODE * node) ;
     *      - INT32 Max_Level (BB * bb);
     *      - INT32 Min_Level (REGIONAL_CFG_NODE * node) ;
     *      - INT32 Min_Level (BB * bb);
     *
     *          each CFG node are assign a pair of level, namely, 
     *          max-level and min-level. these routines query CFG node's
     *          min/max-level.
     *
     *        
     *      - BOOL BB1_Reachable_From_BB2 (BB * bb1, BB* bb2) 
     *          return TRUE iff <bb1> is reachable from <bb2> in current
     *          *ACYCLIC* CFG fragment. 
     *
     *      - BOOL BB_Reachable_From_RGN  (BB * bb, REGION * rgn) 
     *          return TRUE iff <bb> is reachable from nested <rgn> in 
     *          current *ACYCLIC" CFG fragment
     *
     *      - BOOL BB_Is_Reachable (BB * bb, BB_VECTOR * bb_vect) 
     *          return TRUE iff <bb> is an element of reachable-bb-vector
     *          specified by <bb_vect>
     *
     *      - REACH_PROB Reachable_Prob (BB * from, BB * to) 
     *          return the reachable probability from <from> to <to>
     *
     *      - static BOOL Critical_Edge_Present (REGION *rgn)
     *          check the presence of critical edge.
     *
     *      - void Add_Ficticious_BB_to_Remove_Critical_Edge (void) 
     *      - void Remove_Ficticious_Empty_BB (void);
     *          add a ficticious block to remove critical edge. and 
     *          remove them afterward.
     *
     *      and other miscellaneous functions
     * 
     *      - BOOL Has_Scheduled_Preds (BB* bb) 
     *          return TRUE iff bb has at least one immediate predecessor
     *          that has been scheduled by scheduler.
     *
     *      - INT32 Across_Node_Num (BB * from , BB * to) ;
     *          the node # of longest path from <from> toward <to>, including
     *          <from> & <to> as well. 
     * 
     *      - void Dump (FILE *f=stderr,BOOL verbose=TRUE) 
     *          dump this class's status
     *
     * ===============================================================
     * ==============================================================
     */

class RGN_CFLOW_MGR : public SCHED_UTIL_MEM_POOL {

private :

    BOOL  _cflow_info_valid;    /* flag indidate whether this class
                                 * is initialized properly or not
                                 */

    /* --------------------------------------------
     *          basic control flow info 
     *--------------------------------------------*/
    UINT32 _bb_num ;     /* this many BBs in current scheduling scope */ 
    UINT32 _rgn_num ;    /* this many inner-region in current scheduling 
                          * scope */

    UINT32 _max_bb_id ;  /* they are self-descriptive */
    UINT32 _min_bb_id ;
    UINT32 _max_rgn_id ;
    UINT32 _min_rgn_id ;

    REGION * _scope ;    /* the CFG fragment this class deals with */ 
    BB * _bb_scope;

    void    _init_data_member (void) ;
    void    _acquire_basic_cflow_info (void);

    /* -------------------------------------------------------------
     * map discrete node ids to continguous range of integer 
     * -------------------------------------------------------------
     */
    enum { ID_MAP_BASE = 1, };  /* the map-idx starts with this number */
    enum { INVALID_MAP_IDX = ID_MAP_BASE - 1,};

    UINT32 * _bb_id_2_map_idx_vect ;      /* fb: BB_id(bb) -> bb-map-idx */
    UINT32 * _rgn_id_2_map_idx_vect ;     /* fr: region->Id() -> rgn-map-idx */ 
    BB**      _map_idx_2_bb_vect ;        /* fb': bb-map-idx -> (BB *) */
    REGION ** _map_idx_2_rgn_vect ;       /* fr': rgn-map-idx -> (REGION *) */ 

    inline void _setup_map_array (void) ;
    inline void _setup_node_cflow_info_array (void) ;

    INT32   _bb_2_map_idx (BB *bb) const { 
                return _bb_id_2_map_idx_vect[BB_id(bb)] ; 
            }

    INT32   _rgn_2_map_idx (REGION *rgn) const { 
                return _rgn_id_2_map_idx_vect[rgn->Id()];
            }

    INT32   _rgn_2_map_idx (REGIONAL_CFG_NODE * node) const {
                return node->Is_Region () ? 
                       _bb_2_map_idx(node->BB_Node()) :
                       _rgn_2_map_idx(node->Region_Node()) ;
            }

    BB *    _map_idx_2_bb (INT32 map_idx) const { 
                return _map_idx_2_bb_vect[map_idx] ; 
            }

    REGION * _map_idx_2_rgn (INT32 map_idx) const {
                return _map_idx_2_rgn_vect[map_idx] ;
            }

    INT32   _max_bb_map_idx (void) const { return ID_MAP_BASE + _bb_num - 1; } 
    INT32   _min_bb_map_idx (void) const { return ID_MAP_BASE ; } 

    INT32   _max_rgn_map_idx (void) const { return ID_MAP_BASE + _rgn_num - 1;}
    INT32   _min_rgn_map_idx (void) const { return ID_MAP_BASE; }  

    /* ----------------------------------------------------------------
     * 
     *      reachable-info, reach-prob, node-level, etc
     * 
     * ----------------------------------------------------------------
     */
    typedef BS  REACH_INFO_VECT ; 

    /* reachable vector */
    typedef struct tagREACH_PROB_VECT{
        INT32 elem_num ;
        INT32 vect_size;
        REACH_PROB * reach_prob_vect; /* indiced by map-idx */

        tagREACH_PROB_VECT (void) {
            elem_num = 0;
            vect_size = 0;
            reach_prob_vect = NULL;
        };
    } REACH_PROB_VECT;

    typedef struct tagNODE_CFLOW_INFO {
        union {
            BB * bb ;
            REGION * rgn ;
        } node ;
        REACH_INFO_VECT * reach_bb ;

        INT32  min_level ;
        INT32  max_level ;
        REACH_PROB_VECT   reach_prob;

        tagNODE_CFLOW_INFO () {
            node.bb     = NULL ;
            node.rgn    = NULL ;
            reach_bb    = NULL ;
            min_level   = max_level = 1 ;
        }
    } _NODE_CFLOW_INFO ; 


    typedef mempool_allocator<_NODE_CFLOW_INFO >  _NODE_CFLOW_INFO_ALLOC;
    typedef vector<_NODE_CFLOW_INFO, _NODE_CFLOW_INFO_ALLOC> 
                                                  _NODE_CFLOW_VECT;                              
    typedef _NODE_CFLOW_VECT::iterator      	  _NODE_CFLOW_VECT_ITER;

    _NODE_CFLOW_VECT _bb_node_cflow_info ;
    _NODE_CFLOW_VECT _rgn_node_cflow_info ;

    _NODE_CFLOW_INFO& _node_cflow_info (BB *bb) ;
    _NODE_CFLOW_INFO& _node_cflow_info (REGION *rgn) ;
    _NODE_CFLOW_INFO& _node_cflow_info (REGIONAL_CFG_NODE *node);

    BS* _reach_info_vect (BB *bb) ;
    BS* _reach_info_vect (REGION *rgn);
    BS* _reach_info_vect (REGIONAL_CFG_NODE *node);

    REACH_PROB_VECT * _reach_prob_vect (BB *bb);
    REACH_PROB_VECT * _reach_prob_vect (REGION *rgn);
    REACH_PROB_VECT * _reach_prob_vect (REGIONAL_CFG_NODE * node);


    /* ---------------------------------------------------------
     *
     *      reachable-{info|prob}-vector util 
     *
     * ---------------------------------------------------------
     */
    BS * _create_empty_reach_bb_vect () ;
    BS * _add_reachable_bb (BB *from, BB* to);
    BS * _add_reachable_bb (REGION* rgn, BB* to);
    BS * _add_reachable_bb (BS * vect, BB* bb,MEM_POOL * mp);

    /*
    void _del_reachable_bb (BB *from, BB* to);
    void _del_reachable_bb (REGION *rgn, BB* to);
    BS * _del_reachable_bb (REACH_INFO_VECT* vect, BB* to, MEM_POOL *mp);
    */

    BS* _add_reachable_bbs (BB *from, BS * reach_bbs);
    BS* _add_reachable_bbs (REGION* rgn, BS * reach_bbs);
    BS* _add_reachable_bbs (REGIONAL_CFG_NODE * node, BS * reach_bbs);

    BS* _set_bb_is_reachable (BS* reach_vect, BB * bb, MEM_POOL *mp);
    BOOL _is_bb_reachable (BS *reach_vect, BB *bb);

    void _set_bb_reach_prob (REACH_PROB_VECT* prob_vect, 
                             BB* src_bb,REACH_PROB prob) ;
    void _set_bb_reach_prob (BB *from, BB* to,REACH_PROB prob) ;
    void _set_bb_reach_prob (REGION *from , BB * to, REACH_PROB prob); 
    void _set_bb_reach_prob (REGIONAL_CFG_NODE *node, 
                             BB * to, REACH_PROB prob); 

    REACH_PROB _bb_reach_prob (REGION * from,BB *to); 
    REACH_PROB _bb_reach_prob (BB * from,BB *to); 
    REACH_PROB _bb_reach_prob (REGIONAL_CFG_NODE * node,BB *to); 


    void  _fused_mult_add (REACH_PROB_VECT * dest, 
                           REACH_PROB_VECT * src, float scalor) ; 


    /* miscellaneous  */

    void *  _alloc_array (INT32 elem_num , INT32 unit_size) ;
    void    _acquire_cflow_info (void) ; 

    void    _acquire_reachable_info (void) ;
    void    _acquire_reach_prob_info (void) ;

    static UINT16  bb_node_succ_num (REGIONAL_CFG_NODE * node) ;
    static UINT16  bb_node_pred_num (REGIONAL_CFG_NODE * node) ;

    void    _compute_node_level (void) ;

    _NODE_CFLOW_INFO  _bb_cflow_info ;

    static char * _invalid_prompt_msg ;

    EXEC_PATH_MGR  _exec_path_mgr;

    /* ====================================================
     *
     *      EXPORTED INTERFACE
     *
     * ====================================================
     */

public :

        /* constructor and destructor */
    RGN_CFLOW_MGR (void) : 
        _bb_node_cflow_info (_NODE_CFLOW_INFO_ALLOC(&_mem_pool)),
        _rgn_node_cflow_info (_NODE_CFLOW_INFO_ALLOC(&_mem_pool)),
        _exec_path_mgr (&_mem_pool) {

        _init_data_member () ; 
    }

    ~RGN_CFLOW_MGR (void) {}
    
        /* acquire control flow info */
    void Init (REGION * rgn);
    void Init (BB * bb) ;

        /* check to see whether this class has been initialized properly */
    BOOL Path_Info_Is_Valid (void) const 
            { return !_exec_path_mgr.Path_Info_Is_Invalid (); }
    BOOL Valid (void) const { return  _cflow_info_valid; }
    REGION* Scope (void) const { return _scope; }
        /* has many cfg node in this CFG fragment with which 
         * this class deals */

    INT32   BB_Node_Num (void) const {
                Is_True (Valid (), (_invalid_prompt_msg));
                return _bb_num ;
            }

    INT32   RGN_Node_Num (void) const {
                Is_True (Valid (), (_invalid_prompt_msg));
                return _rgn_num ;
            }

    INT32   Max_BB_Id (void) const {
                Is_True (Valid (), (_invalid_prompt_msg));
                return _max_bb_id;
            }

    INT32   Max_Rgn_Id (void) const {
                Is_True (Valid (), (_invalid_prompt_msg));
                return _max_rgn_id ;
            }

        /* query the min/max level of cfg node 
         */
    INT32 Max_Level (REGIONAL_CFG_NODE * node) ;
    INT32 Max_Level (BB * bb);

    INT32 Min_Level (REGIONAL_CFG_NODE * node) ;
    INT32 Min_Level (BB * bb);


        /* reachable info 
         */
    BOOL BB1_Reachable_From_BB2 (BB* bb1, BB* bb2) ;
    BOOL BB_Reachable_From_RGN  (BB* bb, REGION* rgn) ;
    BOOL BB_Reachable_From_Node (BB* bb, REGIONAL_CFG_NODE* n) {
            return n->Is_Region () ? 
                   BB_Reachable_From_RGN  (bb, n->Region_Node()) :
                   BB1_Reachable_From_BB2 (bb, n->BB_Node ());
         }


    BOOL BB_Is_Reachable (BB * bb, BB_VECTOR * bb_vect) {
            for (BB_VECTOR_ITER iter = bb_vect->begin () ; 
                 iter != bb_vect->end() ; iter++) {
                if (BB1_Reachable_From_BB2 (bb, *iter)) return TRUE;
            }
            return FALSE;
         }

    REACH_PROB Reachable_Prob (BB* from, BB* to) {

            if (from != to) {
                Is_True (Valid (), (_invalid_prompt_msg));
                return _bb_reach_prob (from, to);                 
            } 

            return (REACH_PROB)(1.0f * REACH_PROB_SCALE);
        }


        /* critical-edge stuff 
         */
    static BOOL Critical_Edge_Present (REGION *rgn);

    void Add_Ficticious_BB_to_Remove_Critical_Edge (void) ;
    void Remove_Ficticious_Empty_BB (void);

        /* execution path info */
    EXEC_PATH_MGR* Get_Exec_Path_Mgr (void) { return &_exec_path_mgr; }
    EXEC_PATH_SET* Get_Path_Flow_Thru (BB* b) {
                        return _exec_path_mgr.Get_Path_Flow_Thru (b);
                   }
    EXEC_PATH_SET* Get_Path_Flow_Thru (REGION* r) {
                        return _exec_path_mgr.Get_Path_Flow_Thru (r);
                   }
    EXEC_PATH_SET* Get_Path_Flow_Thru (REGIONAL_CFG_NODE* n) {
                        return _exec_path_mgr.Get_Path_Flow_Thru (n);
                   }

        /* misc */
    BOOL  Has_Scheduled_Preds (BB* bb) ;
    INT32 Across_Node_Num (BB* from , BB* to) ;
   
    void Dump (FILE *f=stderr,BOOL verbose=TRUE) ;

    #ifdef Is_True_On
    void gdb_dump (void) ;
    #endif 
};




  /* =======================================================================
   * =======================================================================
   *      
   *          Dominator and Post-dominator stuff 
   * 
   * =======================================================================
   * =======================================================================
   */

void   Calculate_Dominator_Info (REGION_TREE *rgn_tree);
BOOL BB1_Postdominate_BB2 (BB * bb1, BB* bb2) ;

inline BOOL BB1_Dominate_BB2 (BB* bb1, BB * bb2) {
        return BB_SET_MemberP (BB_dom_set(bb2), bb1);
     }

inline BOOL BB1_BB2_Cntl_Equiv (BB* bb_a, BB * bb_b) {
        return BB1_Postdominate_BB2 (bb_a,bb_b) && 
               BB1_Dominate_BB2 (bb_b, bb_a);
    }

inline BOOL Free_Dominator_Info_Memory (void) {
        Free_Dominators_Memory () ;
     }

void Workaround_Dom_Info_For_In_Abnormal_Loop_Rgn (REGION * r); 



    /* ==============================================================
     * ==============================================================
     * 
     *              PU entry- exit-BB splitting and merging 
     * 
     * ==============================================================
     * ==============================================================
     */
void Init_Split_PU_Entry_Or_Exit_BB (void);

BB * Split_PU_Entry_BB (BB * entry) ;
BB * Split_PU_Entry_BB (REGION * rgn) ;
BB * Split_PU_Exit_BB  (BB * exit);
void Split_PU_Exit_BB  (REGION * rgn) ;

void Merge_Splitted_PU_Entry_BB (BB * splitted_entry) ;
void Merge_Splitted_PU_Exit_BB  (BB * splitted_exit)  ;
void Merge_All_Splitted_Entry_and_Exit_BB (void);


    /* analyze the control flow relationship between <src> and 
     * <cutting_set>
     */
typedef enum { IN_SISS, ABOVE_SISS, BELOW_SISS, OTHER } BB_POS ;
BB_POS BB_Pos_Analysis (BB* block, BB_VECTOR* cutting_set, 
                            RGN_CFLOW_MGR* _cflow_info);
    
BB_POS BB_Pos_Analysis (BB* block, BB_VECTOR* cutting_set, 
                            RGN_CFLOW_MGR* _cflow_info);

#endif /*sched_cflow_INCLUDED */
