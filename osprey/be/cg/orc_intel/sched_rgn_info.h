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
  * Module: sched_rgn_info.h
  * $Revision: 1.1.1.1 $
  * $Date: 2005/10/21 19:00:00 $
  * $Author: marcel $
  * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/sched_rgn_info.h,v $
  *
  * Revision comments:
  *
  * 4-January-2002 - Initial version
  *
  * Description:
  * ============
  *
  *   TODO: finish this comments.
  * 
  * =========================================================================
  * =========================================================================
  */

#ifndef sched_rgn_info_INCLUDED
#define sched_rgn_info_INCLUDED

#include "sched_cflow.h"

    /* the reason for which we do not global-schedule particular scheduler 
     */
typedef enum {

    SKIP_RGN_NONE,      /* We need schedule corresponding region */
    SKIP_RGN_IMPROPER,  /* corresponding region is of "IMPROPER" type */
    SKIP_RGN_NON_SEME,  /* corresponding region is of non-"SEME" type */
    SKIP_RGN_CRITICAL_EDGE, /* critical edge presents */
    SKIP_RGN_DEBUG,     /* do not global-schedule region for debugging 
                         * purpose */
    SKIP_RGN_NO_FURTHER_OPT 

} SKIP_SCHED_RGN_REASON ;
     

    /* ===============================================================
     * ===============================================================
     * 
     *  RGN_SUMMARY 
     *
     *  regional Cfg is represented bing    
     *
     * ===============================================================
     * ===============================================================
     */
class RGN_SUM_ROOT_NODE;
typedef RGN_SUM_ROOT_NODE RGN_SUM_TREE ;

class RGN_SUMMARY {
private:
    REGION       * _rgn;       /* associated nested rgn */
    MEM_POOL     * _mp ;       /* underlying memory pool */
    RGN_SUM_TREE * _forest; /* the tree number is equ to the number 
                               * of entry node of region */

    REGION_TYPE _rgn_type ;   /* cache of _rgn->Region_Type () */
    INT32 _tree_num ;         /* how many tree in <_forest> */
    BOOL  _summary_is_valid;  /* indicate whether the summary is built up
                               * properly */


        /* the following data member are actually unions of each 
         * RGN_SUM_PATH in <_forest>.
         */
    TN_SET * _killed_def ;   /* these defs are killed in this region or 
                               * its nested rgn */
    TN_SET * _tn_used ;      /* these TNs are used within this region 
                              * and all nested region */
    mBOOL     _has_call ;     /* there are at least one function call 
                               * in this region or its nested region */
    mBOOL     _has_nested_rgn; /* indicate whether this region has 
                                * nested region or not */
    mBOOL     _has_cannot_move_across_RGN ; /* indicate whether this region
                               * has a nested region and any OP can't be moved
                               * across it */

    mBOOL     _has_load ;     /* there are at least one load in rgn */
    mBOOL     _has_store ;    /* there are at least one OP that is like store*/
    mBOOL     _has_rotating_kernel ; /* there are at least one 
                                      * rotating-kernel BB */
    mINT16    _bb_in_total ;  /* has many basic block in this region */

public:

    RGN_SUMMARY (REGION * rgn,MEM_POOL *mp) ;
    ~RGN_SUMMARY (void) ;

    INT32 Tree_Num (void)            { return _tree_num ; }
    RGN_SUM_TREE * First_Tree (void) { return _forest ;   }    

    REGION * Region (void)           { return _rgn ;      } 
    void Set_Region (REGION *rgn)    { _rgn = rgn  ;      }

    BOOL Summary_Is_Valid (void) const { return _summary_is_valid;   }
    void Set_Summary_Is_Valid (void)   { _summary_is_valid = TRUE;   }
    void Set_Summary_Is_Invalid (void) { _summary_is_valid = FALSE;  }

    TN_SET* Killed_Def (void) const { return _killed_def ; }
    TN_SET* TN_Used    (void) const { return _tn_used    ; }

    void Clear_Def_Kill_Set (void) {  TN_SET_ClearD (_killed_def) ; }
    void Init_Def_Kill_Set (void) {
            if (!_killed_def) {
                _killed_def = TN_SET_Create_Empty ((INT32)Last_TN, _mp);
            }
            Clear_Def_Kill_Set ();
         }

    void Init_Use_TN_Set (void) {

            if (!_tn_used) {
                _tn_used = TN_SET_Create_Empty ((INT32)Last_TN, _mp);
            }
            
            _tn_used = TN_SET_ClearD (_tn_used);
         }

    void Union_Killed_Def (TN_SET* s) {  _killed_def = TN_SET_UnionD(
                                                        _killed_def,s, _mp); }
    void Union_Use_TNs    (TN_SET* s) {  _tn_used    = TN_SET_UnionD(
                                                        _tn_used, s, _mp); }

    BOOL       Has_Call (void) const   { return _has_call  ; }
    void Clear_Has_Call (void)         { _has_call = FALSE ; }
    void   Set_Has_Call (void)         { _has_call = TRUE  ; }

    BOOL       Has_Load (void) const   { return _has_load  ; }
    void Clear_Has_Load (void)         { _has_load = FALSE ; }
    void   Set_Has_Load (void)         { _has_load = TRUE  ; }

    BOOL       Has_Rotating_Kernel (void) { return _has_rotating_kernel  ; }
    void Clear_Has_Rotating_Kernel (void) { _has_rotating_kernel = FALSE ; }
    void   Set_Has_Rotating_Kernel (void) { _has_rotating_kernel = TRUE  ; }

    BOOL       Has_Store (void) const  { return _has_store ; }
    void Clear_Has_Store (void)        { _has_store = FALSE; }
    void   Set_Has_Store (void)        { _has_store = TRUE ; }

    BOOL       Has_Mem_OP (void) const { return Has_Load () || Has_Store () ; }

    BOOL       Has_Nested_Rgn (void) const { return _has_nested_rgn ; }
    void Clear_Has_Nested_Rgn (void)       { _has_nested_rgn = FALSE; }
    void   Set_Has_Nested_Rgn (void)       { _has_nested_rgn = TRUE ; }

    BOOL  Has_Cannot_Move_Across_RGN (void) const {
                return _has_cannot_move_across_RGN ; 
            }

    void  Clear_Has_Cannot_Move_Across_Rgn (void) { 
                    _has_cannot_move_across_RGN = FALSE; 
               }

    void   Set_Has_Cannot_Move_Across_Rgn (void) {
                    _has_cannot_move_across_RGN = TRUE ; 
               }

    MEM_POOL * Underlying_MEM_POOL (void) const { return _mp ; }

    void Adjust_Liveness_After_OP_Being_Moved_Across_Rgn (OP * op);

        /* the following two routines are just for the time being 
         */
    void Set_BB_Total_Num (INT32 bb_num)  { _bb_in_total = bb_num ; }
    INT16 Get_BB_Total_num (void)  const  { return _bb_in_total ;   }

        /* tracing & debugging */
    void Dump (FILE * f=stderr, BOOL verbose=TRUE);
#ifdef Is_True_On 
    void gdb_dump (void);
#endif 

};



    /* Data structure used to hold some information about particular
     * region, and SCHEDULER shows great interest on this info.
     */
typedef struct tagRGN_INFO RGN_INFO ;
struct tagRGN_INFO {

    REGION      * rgn ;             /* associated REGION */
    RGN_SUMMARY * summary ;         /* <rgn>'s summary info */
    SKIP_SCHED_RGN_REASON skip_reason ; /* Need we global-schedule <rgn>?
                                         * if not, for what reason */
    BOOL    in_abnormal_loop;       /* <rgn> is a fragment of "abnormal" loop. 
                                     * ref the comment of Is_Abnormal_Loop
                                     * for the definition of "abnormal loop"
                                     * (in if_conv.cxx) */
    tagRGN_INFO (void) {
        rgn     = NULL;
        summary = NULL;
        skip_reason = SKIP_RGN_DEBUG;
        in_abnormal_loop = TRUE;
    }
    
        /* accessors */
    REGION*   Region (void) const { return rgn; }
    void  Set_Region (REGION *r)  { rgn = r ;   }
    
    RGN_SUMMARY* Summary (void) const     { return summary; }
    void     Set_Summary (RGN_SUMMARY* s) { summary = s ;   }

    SKIP_SCHED_RGN_REASON Skip_Sched_Reason(void) const  {return skip_reason;}
    void Set_Skip_Sched_Reason (SKIP_SCHED_RGN_REASON r) { skip_reason = r  ;}

    BOOL In_Abnormal_Loop (void) const { return in_abnormal_loop; }
    void Set_In_Abnormal_Loop (BOOL b) { in_abnormal_loop = b ;   }

    void Dump (FILE * f=stderr) ;
};

    /* ========================================================================
     * ========================================================================
     *
     *  class  REGION_INFO_MGR 
     * 
     *  ref the header of this file for details.
     *
     * ========================================================================
     * ========================================================================
     */

    /* _tagRGN_CFG_PATH_INFO : keep track of some info of control *PATH*
     *                         that is from REGION's entry node toward  
     *                         the node in question.
     *
     * NOTE: this structure is only used with Build_Region_Summary. 
     */
typedef struct _tagRGN_CFG_PATH_INFO RGN_CFG_PATH_INFO ;
struct _tagRGN_CFG_PATH_INFO {
private:
    TN_SET* _killed_def ;  /* these defs are killed from REGION's
                            * entry node toward this node */ 
    TN_SET* _tn_used    ;  /* these TNs are used from REGION's 
                            * entry node toward this node */
    MEM_POOL* _mp ; 
    RGN_CFG_PATH_INFO* _next ;

    INT32     _bb_num ;      /* this many bb along the path */
    INT32     _rgn_num ;     /* this many nested region along the path */ 
    mBOOL     _has_call ;    /* there is at least one call along the path */
    mBOOL     _has_load ;    /* there is at least one load along the path */
    mBOOL     _has_store ;   /* there is at least store (or OP like st) 
                             * along the path */
    mBOOL     _has_rotating_kernel; /* there is at least one rotating kernel
                                     * along the path */
    mBOOL     _summary_unknown; /* there are at least one nested region 
                                 * (along the path) whose summary is 
                                 * unknown */
public:

    void Clear (void) {

        _killed_def = TN_SET_ClearD (_killed_def);
        _bb_num     = 0;
        _rgn_num    = 0;
        _has_call   = FALSE;
        _summary_unknown = FALSE ;
        _next       = NULL;

    }

    void Union_Def_Kill (BB * bb) {
        _killed_def = TN_SET_UnionD (_killed_def, 
                         BBREGS_live_def(BB_bbregs(bb)), _mp);
    }

    void Derive_RGN_CFG_PATH_INFO_from_RGN_SUMMARY (RGN_SUMMARY * sum);

    _tagRGN_CFG_PATH_INFO (MEM_POOL *m) : _mp(m) {
        _killed_def = TN_SET_Create_Empty (Last_TN,_mp);
        _tn_used    = TN_SET_Create_Empty (Last_TN,_mp);
        _bb_num     = 0;
        _rgn_num    = 0;
        _has_call   = FALSE;
        _summary_unknown = FALSE ;
        _next       = NULL;
    }

    ~_tagRGN_CFG_PATH_INFO (void) { /* do nothing */ };

    void Derive_RGN_CFG_PATH_INFO_from_REGION_Node (RGN_SUMMARY * sum) ;
    void Derive_RGN_CFG_PATH_INFO_from_BB_Node     (BB * bb) ;

    /* accessors 
     */
    TN_SET* Killed_Def (void) const     {   return _killed_def;   }
    void    Set_Killed_Def (TN_SET *kd) {   _killed_def = kd ;    } 

    TN_SET* TN_Used (void) const { return _tn_used ; }
    
    INT32   BB_Num  (void) const  { return _bb_num ;  } 
    INT32   Rgn_Num (void) const  { return _rgn_num ; }

    BOOL    Has_Call (void) const { return _has_call; }
    BOOL    Has_Load (void) const { return _has_load; }
    BOOL    Has_Store(void) const { return _has_store;} 
    BOOL    Has_has_rotating_kernel (void) const { 
                    return  _has_rotating_kernel;
            }
    BOOL    Rgn_Summary_Unknown (void) const { return _summary_unknown; }

    RGN_CFG_PATH_INFO* Next (void) const     { return _next ; }
    void  Set_Next (RGN_CFG_PATH_INFO* n)    { _next = n ;    }
};


class REGION_INFO_MGR : public SCHED_UTIL_MEM_POOL {

protected:

    REGION_TREE * _rgn_tree ;  /* We collect info of each region in 
                                * this "tree" */

    /* NOTE: do not use CFG_NODE_MAP to map inner region 
     *       to <info>, it is too expensive under this 
     *       situation.
     */
    REGION_MAP _rgn_2_rgn_info; 
    void Acquire_Rgn_Info (void); 

    RGN_CFG_PATH_INFO * _free_rgn_cfg_path_info_lnk ;
    RGN_CFG_PATH_INFO * New_RGN_CFG_PATH_INFO (void);
    void                Free_RGN_CFG_PATH_INFO (RGN_CFG_PATH_INFO * info) ;


public :

                /* constructor & destructor */
    REGION_INFO_MGR (REGION_TREE *rgn_tree);
    ~REGION_INFO_MGR (void) ;
    
                /* returns the REGION_TREE we now deal with 
                 */
    REGION_TREE * Region_Tree (void)       { return _rgn_tree ; }

                /* returns RGN_INFO associated with <rgn>
                 */
    RGN_INFO * Get_Rgn_Info (REGION * rgn) {
                    return (RGN_INFO*)REGION_MAP_Get (_rgn_2_rgn_info,rgn);
               }
    
                /* returns the RGN_SUMMARY associated with <rgn> 
                 */
    RGN_SUMMARY * Get_Rgn_Summary (REGION *rgn) {
                        RGN_INFO * info = Get_Rgn_Info (rgn) ;
                        return info ? info-> Summary () : NULL;
                  }


        /* ===============================================================
         *
         *  Build_Rgn_Summary 
         *
         *  Acquire <rgn>'s summary info, this should be done after control
         *  flow of <rgn> become stable and scheduler does not perform any 
         *  transformation upon <rgn> before register allocation phase.
         *  phase. if <rgn_cflow_info> is NULL or its data is obsolete or 
         *  invalid (<rgn_cflow_info->Valid() == FALSE), Build_Rgn_Summary
         *  simplily mark the RGN_SUMMARY(associated with <rgn> as 
         *  invalid, if this occurs, any instruction shold not be moved 
         *  across <rgn>.
         *
         * =============================================================== 
         */
    void Build_Rgn_Summary (REGION *rgn, 
                            RGN_CFLOW_MGR* rgn_cflow_info=NULL);

        /* tracing & debugging */
    void Dump (FILE * f=stderr, BOOL verbose=TRUE);
};

extern REGION_INFO_MGR * _rgn_info_mgr; 



    /* =================================================================
     *
     * Acquire_Region_Info 
     *
     * acquire the region tree hierarchy info and init <_rgn_tree_info>
     * properly 
     *
     * =================================================================
     */
inline void 
Acquire_Region_Info (REGION_TREE *rgn_tree) {

    Is_True (!_rgn_info_mgr, 
             ("<_rgn_info_mgr> has not been destructed properly!"));

    _rgn_info_mgr = CXX_NEW(REGION_INFO_MGR(rgn_tree), &MEM_phase_pool);
}

    /* ================================================================
     *
     * Free_Region_Info_Memory 
     * 
     * free the memory used by region-tree-info and destruct 
     * <_rgn_info_mgr>.
     *
     * ================================================================
     */
inline void 
Free_Region_Info_Memory (void) {

    Is_True (_rgn_info_mgr, ("_rgn_info_mgr can not be NULL!"));

    _rgn_info_mgr->~REGION_INFO_MGR ();
    _rgn_info_mgr = NULL;

}



    /* ===============================================================
     * ===============================================================
     *
     *  Get_Region_Info, Get_Region_Summary, etc:
     * 
     *  Wrapper routines for _rgn_info_mgr->Get_Rgn_Info (), 
     *  _rgn_info_mgr->Get_Rgn_Summary () ... respectively.
     *
     * ===============================================================
     * ===============================================================
     */
#ifdef Is_True_On 

    inline void
    _rgn_info_mgr_validity_check (REGION *rgn) {
        Is_True (_rgn_info_mgr, ("_rgn_info_mgr can not be NULL!"));
        Is_True (rgn->Tree () == _rgn_info_mgr->Region_Tree(),
                    ("ambiguous region tree!"));
    }

#else 

    #define  _rgn_info_mgr_validity_check(x) ((void)0)

#endif 


inline RGN_INFO *
Get_Region_Info (REGION *rgn) {

    _rgn_info_mgr_validity_check(rgn);
    return _rgn_info_mgr->Get_Rgn_Info (rgn);

}

inline RGN_SUMMARY *
Get_Region_Summary (REGION *rgn) {
    _rgn_info_mgr_validity_check(rgn);
    return _rgn_info_mgr->Get_Rgn_Info(rgn)->Summary ();
}

inline void
Build_Region_Summary (REGION *rgn, RGN_CFLOW_MGR * rgn_cflow_info=NULL) {

    _rgn_info_mgr_validity_check(rgn);
    return _rgn_info_mgr->Build_Rgn_Summary (rgn, rgn_cflow_info);

}

    /* ==============================================================
     * 
     *  No_OP_Can_be_Moved_Across_Region 
     *
     *  check to see whether there exist some OPs that can be moved 
     *  across <rgn>
     *
     * ===============================================================
     */
BOOL No_OP_Can_be_Moved_Across_Region (REGION *rgn) ;
BOOL Is_Large_Region (REGION *rgn) ;
         
#endif /*sched_rgn_info_INCLUDED*/
