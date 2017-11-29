/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/* Placeholder for license */

#ifndef opt_peel_unroll_helper_INCLUDED
   #error header file opt_peel_unroll_helper.h is for internal use only
#endif

///////////////////////////////////////////////////////////////////////
//
// 1. Motivatation:
// =================
//   TODO: fill in this section 
//
// 2. Class Hierarchy
// ======================
//   o. LOOP_PEEL_UNROLL_DRIVER : Thin class with simple interface exposed to
//        +      its callers
//        |
//        +- PEEL_UNROLL_DRIVER_IMPL : the real implementaion of 
//               LOOP_PEEL_UNROLL_DRIVER 
//
//   o. PEEL_UNROLL_DRIVER_IMPL relies on couple of "plug-ins", each dedicated 
//      to one unrolling related optimization. "plug-in" is interfaced by 
//      LPU_PLUGIN. As of I write this note, the hierachy is following, where
//      the "MULTIVER_PLUGIN" is for 473.astar@SPEC2006int hack, and DUMMY_PLUGIN 
//      is just used to illustrate how to extend PEEL_UNROLL_DRIVER_IMPL by 
//      adding a plug-in.
//
//      LPU_PLUGIN
//          +
//          |
//          +---- DUMMY_PLUGIN 
//          |
//          +---- MULTIVER_PLUGIN 
//
//   o. There are bunch of utilities:
//
//      - LOOP_CONSTRUCT   : describe loop construct.
//      - LOOP_UNROLL_UTIL : loop unrolling transformation utilities.
//
///////////////////////////////////////////////////////////////////////
//

// foward decl
//
class LMV_CFG_ADAPTOR;
class LOOP_CONSTRUCT;
class LOOP_UNROLL_UTIL;
class PEEL_UNROLL_DRIVER_IMPL;
class LPU_PLUGIN; 
class MULTIVER_PLUGIN; 
class DUMMY_PLUGIN;

// LOOP_CONSTRUCT can be considered as BB_LOOP's extension
//
class LOOP_CONSTRUCT {
public:
    LOOP_CONSTRUCT (CFG* cfg, BB_LOOP* loop);
    ~LOOP_CONSTRUCT (void) {}

    BB_LOOP* Get_loop (void) const { return _loop; }
    CFG* Get_cfg (void) const { return _cfg; }
    STMTREP* Get_unique_exit_br (void) const { return _unique_exit_br; }

    BOOL Is_do_loop (void) const { return _is_do_loop != 0; }
    BOOL Is_in_do_loop_shape (void) const { return _is_in_do_loop_shape; }

    BOOL Is_normalized (void) const { return _is_normalized != 0; }

    BOOL Is_const_strided (void) const { return _has_const_stride != 0; }
    INT32 Get_const_stride (void) const { return _stride; }
    BOOL Is_unit_strided (void) const { return _stride == 1; }

    CODEREP* Get_iv_lower_bound (void) const { return _iv_lower_bound; }
    CODEREP* Get_iv_upper_bound (void) const { return _iv_upper_bound; }
    INT32 Get_upbound_adjustment (void) const { return _upbound_adj; }

    BOOL Is_simple_nesting (UINT32 depth) const;

    // check to see if the loop is clonable. NOTE: it examine all 
    //  blocks every time this function is called. 
    //
    BOOL Check_if_clonable (void);

    void Print (FILE*) const;

private:
    CFG* _cfg;
    BB_LOOP* _loop;
    
    struct {
        UINT32 _is_do_loop : 1;
        UINT32 _is_in_do_loop_shape: 1;
        UINT32 _is_normalized: 1;
        UINT32 _has_const_stride : 1;
    };

    STMTREP* _unique_exit_br; // the *UNIQUE* branch that leave the loop

    // The closed interval of IV, i.e. IV is in the interval:
    //  [_iv_lower_bound, _iv_upper_bound + _upbound_adj * stride]
    // <_upbound_adj> is useful if loop-breaking condition is "iv < upbound" 
    // instead of "iv <= upbound". Currently, they are valid only for DO-loop.
    //
    CODEREP* _iv_lower_bound;
    CODEREP* _iv_upper_bound;
    INT32  _upbound_adj;

    PHI_NODE* _iv_def_phi;
    STMTREP* _iv_init_stmt;
    STMTREP* _iv_incr_stmt;

    INT32 _stride;

    void Init_all_flags (void);

    void Check_if_in_do_loop_shape (void);
    BOOL Check_if_in_do_loop_shape_helper (void);

    PHI_NODE* Lookup_iv_phi (void);
    STMTREP*  Lookup_iv_init_stmt (void);
    STMTREP*  Lookup_iv_incr_stmt (void);

    // Misc

    // TODO: implement this func.
    CODEREP* Strip_cvt (CODEREP* cr) const { return cr; }
};

///////////////////////////////////////////////////////////////////////
//
// This class is to physically perform unrolling transformation.
//
///////////////////////////////////////////////////////////////////////
//
class LOOP_UNROLL_UTIL {
public:
    LOOP_UNROLL_UTIL (COMP_UNIT*, BB_LOOP*, BOOL trace=false);
    ~LOOP_UNROLL_UTIL () {};

    void Unroll (UINT32 factor);

    // Caller should specify the trip count of the given loop. Sometimes 
    // it is not straightfoward figure out trip-count directly from given  
    // loop. But the caller is supposed to be aware of if when it calls 
    // this func.
    //
    void Fully_unroll (UINT32 trip_cnt);

private:
    COMP_UNIT* _cu;
    BB_LOOP* _loop;
    BB_NODE* _backedge_src; // i.e. loop-back block
    
    typedef std::list<BB_NODE*> BB_NODE_LIST; 
    BB_NODE_LIST _all_blks; // all blks in unrolled loop, inc orig blks
    BB_NODE_LIST _bbs_of_orig_loop; // body + merge-blk of orig loop
    BB_NODE_LIST _merge_blks; // merge-block of original loop

    UINT32 _factor;     // unroll factor
    BOOL _is_fully_unroll;
    BOOL _unrolled;     // prevent from being unrolled twice
    BOOL _trace;

    // Mapping between original loop body and cloned loop bodies. 
    //
    typedef INT32 LAB_ID;
    typedef std::map<LAB_ID, LAB_ID> LAB_ID_MAP;
    typedef std::map<const BB_NODE*, const BB_NODE*> BB_NODE_MAP;
    typedef std::vector<LAB_ID_MAP>  LAB_ID_MAP_VECT;
    typedef std::vector<BB_NODE_MAP> BB_NODE_MAP_VECT;
    
    LAB_ID_MAP_VECT  _lab_id_map_vect;
    BB_NODE_MAP_VECT _bb_map_vect;

    typedef UINT32 UNROLL_IDX;

    inline void Map_cloned_label (LAB_ID, LAB_ID, UNROLL_IDX);
    inline LAB_ID Get_cloned_label (LAB_ID, UNROLL_IDX) const ;

    inline void Map_cloned_bb (const BB_NODE* src, const BB_NODE*, UNROLL_IDX);
    inline BB_NODE* Get_cloned_bb (const BB_NODE*, UNROLL_IDX) const ;

    void Verify_body_copy_idx (UNROLL_IDX idx) const {
        Is_True (idx < _factor, ("invalid loop body clone index"));
    }

    // Blocks of cloned loop body are linked by a doubly-linked list.
    // BB_NODE_PAIR contains the header/tail of such list.
    //
    typedef std::pair<BB_NODE*, BB_NODE*> BB_NODE_PAIR;
    typedef std::vector<BB_NODE_PAIR> BB_NODE_PAIR_VECT; 
    BB_NODE_PAIR_VECT _bb_nodes_pair_vect;

    // accessor for symtab, cfg etc
    //
    COMP_UNIT* Get_comp_unit (void) const { return _cu; }
    CFG* Get_cfg (void) const { return _cu->Cfg (); }
    CODEMAP* Get_htab (void) const { return _cu->Htable (); }
    BB_NODE* Get_backedge_src (void) const { return _backedge_src; }

    // cloning helper functions
    //
    void Split_backedge ();
    BB_NODE* Clone_block (const BB_NODE*, UNROLL_IDX);
    void Clone_edges (const BB_NODE_LIST& cloned_bbs, UNROLL_IDX);
    void Copy_layout (const BB_NODE_LIST& cloned_bbs, UNROLL_IDX, 
                      BB_NODE** first, BB_NODE** last);
    void Clone_BB_IFINFO (UNROLL_IDX );
    void Update_branch_target (UNROLL_IDX);
    void Clone_loop_body (UNROLL_IDX);
    void Adjust_bb_frequncy (void);
    void Destroy_loop_construct (void); // helper func of fully-unroll
    void Hookup_cloned_bodies (void);

    // Misc
    //
    void Remove_flags_about_loop (BB_NODE*);

    inline void Connect_predsucc (BB_NODE* pred, BB_NODE* succ,
                                  BOOL change_pred_br_target=FALSE); 

    STMTREP* Append_goto_stmt (BB_NODE*);
    STMTREP* Prepend_label_stmt (BB_NODE*);
    LAB_ID Alloc_lab (BB_NODE*);

#ifdef Is_True_On
    inline void Verify_merge_blk (const BB_NODE*);
    void Verify (void);
#else
    void Verify (void) {};
    void Verify_merge_blk (const BB_NODE*) {};
#endif
};

//////////////////////////////////////////////////////////////////////////
//
//          The driver for loop peeling and unrolling
//
//////////////////////////////////////////////////////////////////////////
//
class PEEL_UNROLL_DRIVER_IMPL : public LOOP_PEEL_UNROLL_DRIVER {
public:
    PEEL_UNROLL_DRIVER_IMPL (COMP_UNIT* cu, LPU_OPTS opts, 
                             LPU_TRACE_LEVEL tr=LPU_NONE);
    ~PEEL_UNROLL_DRIVER_IMPL (void);

    void Set_processed (BB_LOOP* l) { _processed[l] = TRUE; }
    BOOL is_processed (BB_LOOP* l) const 
        { return _processed.find(l) != _processed.end ();}

    CFG* Get_cfg (void) const { return _comp_unit->Cfg(); } 

    // return FALSE iff no change is made
    //
    BOOL Perform_peeling_or_unroll (void);

    // Misc
    //
    BOOL Is_enabled (LPU_OPTS opt) const { return (_enable_opts & opt) != 0; }
    const char Get_trace_leading_char (void) { return _trace_leading_char;}

private:
    // leading char to make important message distinct from surrounding text.
    //
    static const char _trace_leading_char;

    MEM_POOL _mp;
    std::map<BB_LOOP*, BOOL> _processed;

    // helper func of Perform_peeling_or_unroll().
    BOOL Perform_peeling_or_unroll_rec (BB_LOOP*);
    BOOL Process_loop (BB_LOOP*);
};

//////////////////////////////////////////////////////////////////////////
//
// LPU_PLUGIN stands for "loop peeling and unrolling plug-in".
// As its name suggest, it is the base class for loop peeling/unrolling 
// related optimizer, which we call "plug-in". 
//
// PEEL_UNROLL_DRIVER_IMPL may have multiple of "plug-in"s. It is possible 
// that more than one plug-ins declare it's beneficial to perform particular 
// transformation. In this case, PEEL_UNROLL_DRIVER_IMPL arbitrate the winner
// by picking up the one having largest estimanted benifit.
// 
//////////////////////////////////////////////////////////////////////////
//
class LPU_PLUGIN {
public:
    // The benefit of peeling/unrolling in terms of percentage of run time 
    // improvement over original loop. E.g. If it is estimated 75% run-time 
    // gain, then PEEL_UNROLL_BENEFIT is 75.
    //
    typedef UINT32 PEEL_UNROLL_BENEFIT;
    static const PEEL_UNROLL_BENEFIT NO_BENEFIT;
    static const PEEL_UNROLL_BENEFIT MAX_BENEFIT;

    LPU_PLUGIN (PEEL_UNROLL_DRIVER_IMPL& dr, MEM_POOL* mp, BB_LOOP* loop): 
        _driver(dr), _mp(mp), _loop(loop), _benefit(NO_BENEFIT) {}

    // return TRUE iff this opt is applicable to given loop, and is supposed 
    // to gain some improvement.
    //
    BOOL Exam_applicability (void) 
        { Is_True (FALSE, ("call dervied class's implementation")); return FALSE;}

    PEEL_UNROLL_BENEFIT Get_estimated_benefit (void) const { return _benefit; }

    virtual void Perform_transformantion (void) = 0;

    CFG* Get_cfg (void) const { return _driver.Get_comp_unit()->Cfg (); } 
    COMP_UNIT* Get_comp_unit (void) const { return _driver.Get_comp_unit (); }
    BOOL Is_trace_on (void) const {
        return _driver.Get_trace_level () != LOOP_PEEL_UNROLL_DRIVER::LPU_NONE;
    }

protected:
    PEEL_UNROLL_DRIVER_IMPL& _driver;
    MEM_POOL* _mp; 
    BB_LOOP* _loop;
    PEEL_UNROLL_BENEFIT _benefit;
};

//////////////////////////////////////////////////////////////////////////
//
// MULTIVER_PLUGIN is for the hack of astar in CPU2006int suite. It is to 
// catch this pattern bellow:
//    x1=px-1;
//    y1=py-1;
//    x2=px+1;
//    y2=py+1;
//  
//    if (x1<0) x1=0;
//    if (y1<0) y1=0;
//    if (x2>mapmaxx) x2=mapmaxx;
//    if (y2>mapmaxy) y2=mapmaxy;
//  
//    for (y=0; y<=y2-y1; y++)
//      for (x=0; x<=x2-x1; x++) {
//           ... loop body
//      }
//
//  After pattern is detected, multi-ver the double-loop into a 3*3 double-loop 
//  and fully-unroll them.  
//
//    x1=px-1;
//    y1=py-1;
//    x2=px+1;
//    y2=py+1;
//
//    t1=px-1;   *
//    t2=px-1;   *
//    t3=py+1;   *
//    t4=py+1;   *
//
//    bool multi_ver = true; *
//    if (x1<0) x1=0;
//    if (y1<0) y1=0;
//    if (x2>mapmaxx) {x2=mapmaxx; multi_ver=false; *}
//    if (y2>mapmaxy) {y2=mapmaxy; multi_ver=false; *}
//  
//    if (multi_ver) {
//       x1 = t1; * // We actually want to propagate "px-1" here
//       x2 = t2; *
//       y1 = t3; *
//       y4 = t4; *
//       for (y=0; y<=2, y++)
//         for (x=0; x<=2; x++) 
//           ... body ...
//    } else {
//          ... original double-loop
//    }
//
//////////////////////////////////////////////////////////////////////////
//
class MULTIVER_PLUGIN : public LPU_PLUGIN {
public:
    MULTIVER_PLUGIN (PEEL_UNROLL_DRIVER_IMPL&, MEM_POOL*, BB_LOOP*);
    ~MULTIVER_PLUGIN (void);

    BOOL Exam_applicability (void);
    virtual void Perform_transformantion (void);

private:
    BB_NODE* _loop_header;
    UINT32  _unrool_factor;
    BOOL _allow_clone_call_bb_saved; // save CFG::Allow_clone_calls() 
    static const INT32 _small_trip_count_limit;
    INT32 _inner_trip_count;
    INT32 _outer_trip_count;

    typedef struct {
        CODEREP* nonconst_addend;
        INT32 const_addend;
        STMTREP* dom_def;
        STMTREP* cond_def;
    } DEF_PATTERN;

    DEF_PATTERN _x1_def_pattern;
    DEF_PATTERN _x2_def_pattern;
    DEF_PATTERN _y1_def_pattern;
    DEF_PATTERN _y2_def_pattern;

    // helper functions of pattern detection
    //
    BOOL Is_scalar_ld (CODEREP*) const;
    void Get_def_pattern (CODEREP*, DEF_PATTERN&) const;
    BOOL Trip_count_could_be_small 
            (LOOP_CONSTRUCT&, DEF_PATTERN&, DEF_PATTERN&, INT32& tc) const;
    BOOL Unrolling_will_bring_benefit 
            (LOOP_CONSTRUCT& outer, LOOP_CONSTRUCT&) const;

    // helper functions of transformation
    //
    STMTREP* Gen_stid_stmt (AUX_ID lhs_var, UINT32 rhs_val, VER_ID); 
    void Insert_before (STMTREP*, STMTREP* before);

    CODEREP* Gen_multi_ver_predicate (void);
    void Propagate_original_val (LMV_CFG_ADAPTOR&);
};

/////////////////////////////////////////////////////////////////////////////
//
// The dummy derived class is just used to demonstate how to plug in another
// peel/unroll optimizations/hacks.
//
/////////////////////////////////////////////////////////////////////////////
//
class DUMMY_PLUGIN : public LPU_PLUGIN {
public:
    DUMMY_PLUGIN 
        (PEEL_UNROLL_DRIVER_IMPL& dr, MEM_POOL* mp, BB_LOOP* loop) : 
         LPU_PLUGIN (dr, mp, loop) {};

    void Perform_transformantion (void) {};
};
