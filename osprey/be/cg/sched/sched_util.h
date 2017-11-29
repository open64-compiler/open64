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
#ifndef sched_util_INCLUDED
#define sched_util_INCLUDED

#include "defs.h"

#include <list>
#include <vector> 
#if (__GNUC__ == 3)
using std::vector;
#endif // __GNUC__ == 3
#include <queue>

/* memory management */
#include "mempool.h"
#include "mempool_allocator.h"

/* definition used globally in backend */
#include "ipfec_defs.h"
#include "cg_flags.h"

/* tracing facility and commomly used data structure and utilities */
#include "tracing.h"
#include "bb.h"
#include "op.h"

/* control flow related header file */
#include "region.h"
#include "region_verify.h"
#include "dominate.h"

#include "reg_live.h"

#ifdef TARG_IA64
#include "targ_issue_port.h" /* for ISSUE_PORT */
#endif

#include "dag.h"

#ifndef	min
    #define min(a,b) (((a)<(b))?(a):(b))
#endif

#ifndef	max
    #define max(a,b) (((a)>(b))?(a):(b))
#endif


    /* ==============================================================
     * ==============================================================
     *
     *          speculation and dependence related stuff 
     * 
     * ==============================================================
     * ==============================================================
     */
typedef enum {
    SPEC_NONE   = 0,    /* no speculation */
    SPEC_CNTL   = 1,    /* control speculation */ 
    SPEC_DATA   = 2,    /* data speculation */ 
    SPEC_COMB   = 3,    /* both data and control speculation */
    SPEC_DISABLED = 4,  /* could not speculate */
} SPEC_TYPE ;

typedef enum {
    LOAD_FORM_NORMAL = 0,
    LOAD_FORM_DOT_S  = 1,  /* ld[1,2,4,8].s form */
    LOAD_FORM_DOT_A  = 2,  /* ld[1,2,4,8].a form */
    LOAD_FORM_DOT_SA = 3,  /* ld[1,2,4,8].sa form */ 
    LOAD_FORM_LAST   = 4, 
} LOAD_FORM ;

extern const char* spec_text[] ;

extern SPEC_TYPE Derive_Spec_Type_If_Violate_Dep (ARC* arc);


class RGN_CFLOW_MGR ;

/* =================================================================
 * =================================================================
 * 
 *          Flags and options 
 *
 * =================================================================
 * =================================================================
 */
extern BOOL SCHED_TF_DUMP_IR ; 
extern BOOL SCHED_TF_DUMP_DAG ;
extern BOOL SCHED_TF_DUMP_CAND ;
extern BOOL SCHED_TF_SUMMARY_DUMP;
extern BOOL SCHED_TF_VERBOSE_DUMP;
extern BOOL SCHED_TF_DRAW_GLBL_CFG;
extern BOOL SCHED_TF_DRAW_RGNL_CFG;
extern BOOL SCHED_TF_DRAW_LOCAL_DAG;
extern BOOL SCHED_TF_DRAW_RGNL_DAG;
extern BOOL SCHED_TF_TEST_SPEC;
extern BOOL SCHED_TF_CANDSEL_DUMP;
#if defined(TARG_SL)
extern BOOL SCHED_TF_DUMP_CS_PROCESS;
#endif
#if defined(TARG_SL2)
extern BOOL SCHED_TF_DUMP_MACRO_INSN_COMBINATION; 
extern BOOL SCHED_TF_DUMP_OP_MOVE_IN_BB;
extern BOOL SCHED_TF_DUMP_CAND_SELECTION_PROCESS; 
extern BOOL SCHED_TF_DUMP_SATD_COMB; 
extern BOOL SCHED_TF_DUMP_COND_MV_COMBINE; 
#endif 

extern INT32 SAFE_CNTL_SPEC_PROB ;
extern INT32 UNSAFE_CNTL_SPEC_PROB ;
extern INT32 SPEC_SAFE_LOAD_WITHOUT_TRANSFORM_REACH_PROB;

extern void Get_Sched_Opts (BOOL prepass) ;

/* ================================================================
 * ================================================================
 *
 *     memory pool used for some data structure
 * 
 * ================================================================
 * ================================================================
 */ 
class SCHED_UTIL_MEM_POOL {
protected:
    MEM_POOL _mem_pool ;

public:
    SCHED_UTIL_MEM_POOL () {
        MEM_POOL_Initialize (&_mem_pool,"SCHED_UTIL", true);
        MEM_POOL_Push(&_mem_pool);
    }

    ~SCHED_UTIL_MEM_POOL () {
        MEM_POOL_Pop (&_mem_pool);
        MEM_POOL_Delete (&_mem_pool);
    }
};

    /* =================================================================
     * =================================================================
     *
     *   CFG_NODE_MAP 
     *
     *  We have alreay many maps, such as BB_MAP, OP_MAP etc. 
     *  From the viewpoint of REGIONAL-CONTROL-FLOW, we need mapping 
     *  a regional-cfg-node to any arbitrary value. CFG_NODE_MAP 
     *  serve this purpose. 
     *  
     *  Instance of this class is free at choosing the underlying 
     *  MEM_POOL. 
     * 
     *  NOTE : o. before use this mapping mechanism, 
     *            CFG_NODE_MAP::Initialize_Map should be called. 
     *            otherwise, CFG_NODE_MAP will constantly change
     *            its internal mapping data structure.
     *
     * =================================================================
     * =================================================================
     */
class CFG_NODE_MAP {
private:
    MEM_POOL * _mem_pool ;

    typedef struct tagNODE_ID_VAL_PAIR{
        void * value ;
        INT32  node_id ;
        tagNODE_ID_VAL_PAIR (void* Value, INT32 Node_id) {
            value = Value ; node_id = Node_id ;
        }
    } NODE_ID_VAL_PAIR ;

    typedef mempool_allocator<NODE_ID_VAL_PAIR >    _PAIR_ALLOC ;
    typedef vector<NODE_ID_VAL_PAIR , _PAIR_ALLOC>  _PAIR_VECT ; 
    typedef _PAIR_VECT::iterator      	            _PAIR_VECT_ITER ;

    _PAIR_VECT _bb_map_vect, _rgn_map_vect ;

    void    _bubble_sort (_PAIR_VECT& vect, INT32 low_idx, INT32 hign_idx); 
    void    _set_map (_PAIR_VECT& vect, INT32 id, void *value);
    INT32   _find_elem (_PAIR_VECT& vect, INT32 id);

public :

    CFG_NODE_MAP (MEM_POOL * mp) : 
        _mem_pool(mp), _bb_map_vect(mp), _rgn_map_vect(mp) { }

    ~CFG_NODE_MAP () {} ;

    void Initialize_Map (REGION* rgn) ;
    void Initialize_Map (BB *bb) ;

    inline void Set_Map (BB * bb, void* value) { 
        _set_map (_bb_map_vect,BB_id(bb), value); 
    }

    inline void Set_Map (REGION *rgn, void * value) {
        _set_map (_rgn_map_vect,rgn->Id(), value) ;
    }

    inline void Set_Map (REGIONAL_CFG_NODE * nd, void* value) {
        if (nd->Is_Region()) Set_Map (nd->Region_Node(), value); 
        else  Set_Map (nd->BB_Node(), value);
    }

    inline void * Get_Map (BB * bb) {
        INT32 pos = _find_elem (_bb_map_vect,BB_id(bb));
        return pos >= 0 ? _bb_map_vect[pos].value : NULL ;
    }

    inline void * Get_Map (REGION * rgn) {
        INT32 pos = _find_elem (_rgn_map_vect,rgn->Id()) ;
        return pos >= 0 ? _rgn_map_vect[pos].value : NULL;
    }

    inline void * Get_Map (REGIONAL_CFG_NODE *nd) {
        return nd->Is_Region() ? Get_Map (nd->Region_Node()) : 
                                 Get_Map (nd->BB_Node()) ;
    }
    
    void Dump (FILE *f=stderr) ;

    #ifdef Is_True_On
    void gdb_dump (void);
    #endif
};




typedef INT32 REACH_PROB;
typedef REACH_PROB PROBABILITY;
#define REACH_PROB_SCALE (100)

    /* ======================================================================
     * 
     *   SCHED_{OP|BB|RGN}_ANNOT and SCHED_ANNOT
     * 
     *
     *  Basic data structure which is used to hold some,from scheduler's point 
     *  of view, essential information of object (OP, BB and inner-region). the
     *  object itself, however, does not provide.
     *
     *  SCHED_ANNOT neatly organize these data through its one (and only one)
     *  global instance 'sched_annot'.
     *
     *  These "annotation" can be viewed as extended attribute of object, so 
     *  they should be initialized properly before any code motion occurs and 
     *  updated timely to reflect object's up-to-date status. 
     *
     * ======================================================================
     */
typedef INT16 CYCLE ;
#define CYCLE_MAX  0x7fff

typedef UINT32 OP_EXT_FLAG;

class SCHED_OP_ANNOT {

public:

    OP_EXT_FLAG _ext_flags ;

    OP *        _op;
    BB *        _org_home_bb ; 
    
    SCHED_OP_ANNOT (OP *associated_op) {
        _ext_flags = (OP_EXT_FLAG)0 ;

        _op = associated_op; _org_home_bb = OP_bb(_op);
        Is_True (_org_home_bb, ("OP has no home BB"));

    }

    void Add_OP_Ext_Flag (OP_EXT_FLAG e_flags) {
        _ext_flags |= e_flags ; 
    }

    void Del_OP_Ext_Flag (OP_EXT_FLAG e_flags) {
        _ext_flags &= ~e_flags;
    }

    ~SCHED_OP_ANNOT () {} ;

    SCHED_OP_ANNOT& operator = (SCHED_OP_ANNOT& opnd) {
        _ext_flags = opnd._ext_flags ;
        _op        = opnd._op;
        _org_home_bb = opnd._org_home_bb;
    }

    void Dump (FILE *f=stderr) ;
    
} ;


class SCHED_BB_ANNOT {

private:

    MEM_POOL * _mem_pool ;  /* underlying memory pool */

    BB *  _bb ;             /* associated BB */
    
    OP *  _1st_append_op;   /* before <_bb> being scheduled, <_1st_append_op> 
                             * through BB_last_op(_bb) inclusively are the OPs 
                             * appended to <_bb> as compensation code during
                             * code hoisting
                             */
    OP *  _last_prepend_op ; /* before <_bb> being scheduled, BB_first_op(_bb) 
                              * through _last_prepend_op are the OPs prepended 
                              * to <_bb> during code sinking 
                              */
    OP * _xfer_op ;         /* control-transfer OP */

    BB_OP_MAP       _ops_annot_map ;
    BS *            _annot_inited_ops; 

    void  _init_ops_annot (void) ;

    inline void  _set_op_annot (OP * op, SCHED_OP_ANNOT * annot) ;


public :

    SCHED_OP_ANNOT * Get_OP_Annot (OP * op) {
        if (BS_MemberP (_annot_inited_ops, OP_map_idx(op))) {
            return (SCHED_OP_ANNOT * )BB_OP_MAP_Get (_ops_annot_map, op); 
        }
        
        return NULL;
    }
        
    void Set_OP_Annot (OP* op, SCHED_OP_ANNOT *annot) {
        BB_OP_MAP_Set (_ops_annot_map, op, annot); 
    }

    SCHED_OP_ANNOT * Init_New_OP_Annot (OP* op) ;

    SCHED_OP_ANNOT * Detach_OP_Annot (OP * op);
    void       Attach_OP_Annot (OP * op, SCHED_OP_ANNOT * annot) ;

    OP * First_Append_OP (void) { return _1st_append_op ; } 
    void Set_First_Append_OP(OP *op) { _1st_append_op = op; } ;
    OP * Last_Prepend_OP (void) { return _last_prepend_op ; } 

    SCHED_BB_ANNOT (BB *bb, MEM_POOL *mp) ;
    ~SCHED_BB_ANNOT () {} ;

    void Dump (FILE *f=stderr) ;

};


/* =================================================================
 * =================================================================
 *
 *      SCHED_RGN_ANNOT class
 * 
 * =================================================================
 * =================================================================
 */

class SCHED_RGN_ANNOT {
private:
    MEM_POOL * _mem_pool;
    REGION * _rgn;

public:
    SCHED_RGN_ANNOT (REGION *rgn, MEM_POOL *mp) : 
        _mem_pool(mp) , _rgn(rgn) {} 
    ~SCHED_RGN_ANNOT () {} ;
    
    void Dump (FILE *f) ;
};

/* ================================================================
 * ================================================================
 *
 *          SCHED_ANNOT 
 *
 * ================================================================
 * ================================================================
 */
class SCHED_ANNOT : public SCHED_UTIL_MEM_POOL {
private:
    CFG_NODE_MAP  * _node_map ; 
    static INT32   _inst_num ; 

    REGION * _rgn_scope ;
    BB     * _local_scope;
    BB_SET * _isolated_bbs;

public:

    SCHED_ANNOT (void);  
    ~SCHED_ANNOT (void) { -- _inst_num ; } 

    void Init (REGION *scope) ;
    void Init (BB *scope) ;

    inline SCHED_BB_ANNOT * Get_BB_Annot (BB *bb) {
        return (SCHED_BB_ANNOT*)_node_map->Get_Map (bb);
    }

    inline SCHED_RGN_ANNOT * Get_Rgn_Annot (REGION *rgn) {
        return (SCHED_RGN_ANNOT*)_node_map->Get_Map (rgn);
    }

    inline SCHED_OP_ANNOT * Get_OP_Annot (OP *op) {
        SCHED_BB_ANNOT * bb_annot = Get_BB_Annot (OP_bb(op));
        return bb_annot ? bb_annot->Get_OP_Annot (op) : NULL;
    }
    
    inline UINT32 Get_OP_Ext_Flags (OP *op) {
        return Get_OP_Annot(op)->_ext_flags ;
    }

    inline UINT32 * Get_OP_Ext_Flags_P (OP *op) {
        return &Get_OP_Annot(op)->_ext_flags ;
    }


    void Isolate_BB_From_Sched_Scope (BB * bb) {
            _isolated_bbs = BB_SET_Union1D (    
                                _isolated_bbs,bb, &_mem_pool);
         }

    BOOL BB_Is_Isolated (BB *bb) { return BB_SET_MemberP(_isolated_bbs,bb); }
    void Delete_BB_From_Isolated_BB_Lst (BB* bb) {
            _isolated_bbs = BB_SET_Difference1D (_isolated_bbs,bb);
         }

    /* tracing facility 
     */

    void Dump (FILE * f=stderr) ;
    
};

extern SCHED_ANNOT sched_annot ;

    /* ===============================================================
     * ===============================================================
     *
     *              OP and BB's extended FLAGS
     *
     * ===============================================================
     * ===============================================================
     */

#define OP_EXT_MASK_ACTUAL (0x00000001) /* OP defines autual argument */
#define OP_EXT_MASK_NO_CNTL_SPEC (0x00000002) /* OP cannot be control-speculated */
#define OP_EXT_MASK_NO_DATA_SPEC (0x00000004) /* OP cannot be data-speculated */
#define OP_EXT_MASK_COMPENSATION (0x00000008) /* OP is a compenstation code */

#define Set_OP_Ext_Flag(x,y)  \
    { sched_annot.Get_OP_Annot ((x))->Add_OP_Ext_Flag (OP_EXT_MASK_ ## y); }
#define OP_Ext_Flag(x,y) (sched_annot.Get_OP_Ext_Flags ((x)) & OP_EXT_MASK_ ## y)
#define Reset_OP_Ext_Flag(x,y) \
    { sched_annot.Get_OP_Annot ((x))->Del_OP_Ext_Flag (OP_EXT_MASK_ ## y); }


inline BOOL
OP_ANNOT_Cannot_Data_Spec (OP* op) { return OP_Ext_Flag (op, NO_DATA_SPEC); }

inline void 
OP_ANNOT_Set_Cannot_Data_Spec(OP *op) { Set_OP_Ext_Flag (op, NO_DATA_SPEC); }

inline void
OP_ANNOT_Reset_Cannot_Data_Spec(OP *op) { Reset_OP_Ext_Flag (op, NO_DATA_SPEC); }

inline BOOL 
OP_ANNOT_Cannot_Cntl_Spec (OP * op) { return OP_Ext_Flag(op, NO_CNTL_SPEC); }

inline void
OP_ANNOT_Set_Cannot_Cntl_Spec (OP* op) { Set_OP_Ext_Flag(op, NO_CNTL_SPEC); }

inline void
OP_ANNOT_Reset_Cannot_Cntl_Spec (OP* op) { Reset_OP_Ext_Flag(op,NO_CNTL_SPEC);}

inline BOOL 
OP_ANNOT_Is_Compensation (OP * op) { return OP_Ext_Flag(op, COMPENSATION); }

inline void
OP_ANNOT_Set_Compenstation (OP* op) { Set_OP_Ext_Flag(op, COMPENSATION); }

inline void
OP_ANNOT_Reset_Compensation (OP* op) { Reset_OP_Ext_Flag(op, COMPENSATION);}

inline BOOL
OP_ANNOT_Cannot_Spec (OP * op) {
    return OP_ANNOT_Cannot_Cntl_Spec (op) || 
           OP_ANNOT_Cannot_Data_Spec (op) ;
}

inline void 
OP_ANNOT_Set_Cannot_Spec (OP* op) {
    OP_ANNOT_Set_Cannot_Data_Spec (op) ;
    OP_ANNOT_Set_Cannot_Cntl_Spec (op) ;
}

inline void
OP_ANNOT_Reset_Cannot_Spec (OP* op) {
    OP_ANNOT_Reset_Cannot_Data_Spec (op) ;
    OP_ANNOT_Reset_Cannot_Cntl_Spec (op) ;
}

inline BOOL
OP_ANNOT_OP_Def_Actual_Para (OP *op) { return OP_Ext_Flag(op, ACTUAL); }

inline void
OP_ANNOT_Set_OP_Def_Actual_Para (OP *op) { Set_OP_Ext_Flag (op, ACTUAL); }

inline void
OP_ANNOT_Reset_OP_Def_Actual_Para (OP* op) { Reset_OP_Ext_Flag(op,ACTUAL) ;}

inline void
Isolate_BB_From_Sched_Scope (BB * bb) {
    sched_annot.Isolate_BB_From_Sched_Scope (bb);
}

inline BOOL
BB_Is_Isolated_From_Sched (BB* bb) { 
    return sched_annot.BB_Is_Isolated (bb); 
}

inline void 
Delete_BB_From_Isolated_BB_Lst (BB* bb) {
    sched_annot.Delete_BB_From_Isolated_BB_Lst (bb);
}

extern BOOL
OP1_Defs_Are_Used_By_OP2(OP* op1, OP* op2);

extern BOOL
OP1_Defs_Are_Killed_By_OP2(OP* op1, OP* op2);






    /* ======================================================
     * ======================================================
     *
     *              Miscellaneous 
     * 
     * ======================================================
     * ======================================================
     */
BOOL Load_Has_Valid_Vaddr(OP* ld); 

typedef mempool_allocator<OP*> OP_ALLOC;
typedef vector<OP*,OP_ALLOC>   OP_Vector;
typedef OP_Vector::iterator    OP_Vector_Iter;
typedef OP_Vector::reverse_iterator OP_Vector_Riter ;

#ifdef TARG_IA64
/* these funcs put here just for the time-being */
/* CGGRP_OP_Issue_Port put to microscheduling package 
 */
inline ISSUE_PORT CGGRP_OP_Issue_Port (OP *op) { return ip_invalid ; } 
INT32 CGTARG_adjust_latency (ARC* arc, ISSUE_PORT pred_port, 
                             ISSUE_PORT succ_port);
#endif /* TARG_IA64 */

#if !defined(TARG_IA64)
    #ifdef OP_Scheduled
        #undef OP_Scheduled
        #undef Set_OP_Scheduled
        #undef Reset_OP_Scheduled
    #endif

    /* We need not to pass the state of being scheduled (or not) 
     * to phases other-than global/local instruction scheduling.
     */
    #define OP_Scheduled(o) OP_flag1(o)
    #define Set_OP_Scheduled(o) Set_OP_flag1(o)
    #define Reset_OP_Scheduled(o) Reset_OP_flag1(o)

    #ifndef OP_chk
        #define OP_chk(o)  (FALSE)
    #endif

    #if !defined(Set_OP_data_spec) || !defined(Set_OP_cntl_spec)
        #define Set_OP_data_spec(o) ((void)0) 
        #define Set_OP_cntl_spec(o) ((void)0)
    #endif

    #ifndef Set_OP_orig_bb_id 
        #define Set_OP_orig_bb_id(o, bid) ((void)0)
    #endif

#endif /* !defined(TARG_IA64) */

#endif  /* sched_util_INCLUDED */
