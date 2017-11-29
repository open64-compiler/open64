/*
  Copyright (C) 2000-2003, Institute of Computing Technology, Chinese Academy of Sciences
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer. 

  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
 
//-*-c++-*-

 /* =========================================================================
  * =========================================================================
  * 
  * Module: loop_invar_hoist.cxx
  *
  * $Revision: 1.1.1.1 $
  * $Date: 2005/10/21 19:00:00 $ 
  * $Author: marcel $
  * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/loop_invar_hoist.cxx,v $
  *
  * Revision comments:
  * 
  *    14-April-2003 - Initial version
  * 
  * Description:
  * ============
  *  
  *    This module is designed to implement loop invariant code motion as 
  *    files' name suggest. It is appllied upon CGIR level and right before
  *    Perform_Loop_Optimization () is invoked. 
  *    
  *    The motivation of this moddule is to pick the chances that can not 
  *    be caught by the earlier loop-invar-code-motion-er which is a 
  *    by-product of PRE (Roy told me so if I have not misconstrued what
  *    he said. If things turn out to be a totally different story, do not
  *    make fuss, since I kown nothing about WOPT and I likely misunderstand
  *    what other people told me).
  *     
  *    The "biggest chances" that earlier LICMer cannt caught is the 
  *    address calculation, e.g "addl r11=@ltoff(seedi#),gp". Those address
  *    calc instructions do not exist before lowering to CGIR, hence 
  *    earlier LICMer has no idea of them.   
  *     
  *    One week ago, I was unfortunately asked by Roy to implement this 
  *    LICMer.  Life is so hard!
  * 
  *    Our algorithm is based on "Steven S.Muchnick, Advanced Compiler 
  *    Design and Implementation,p397-407". For the purpose of clarity, this
  *    algorithm understate alias issues -- it treats all instructions 
  *    as simple ALU calculation. However, a pragmatic LICMer should take 
  *    alias into account. What is more, it should also take an eye on 
  *    architecture specific dependences beteen instructions which could 
  *    not easily observed by bare eye via comparing the TNs explicitly 
  *    defined and used between instructions. 
  *  
  * =========================================================================
  * =========================================================================
  */
#include <vector>  /* for STL vector and list */
#include <list>

#include "tracing.h"
#include "error.h"

#include "mempool_allocator.h" /* for class mempool_allocator */
#include "cxx_memory.h"        /* for CXX_NEW() */

#include "op.h"
#include "bb.h"
#include "bb_set.h"

#include "gtn_universe.h"    /* for GTN_SET_xxx() */
#include "gtn_set.h"
#include "register.h"
#include "gra_live.h"

#include "dominate.h"   /* for BB_dom_set() */
#include "cgtarget.h"   /* for CGTARG_Can_Be_Speculative() */  
#include "cg_dep_graph.h" /* for dependence info */
#include "cg_loop.h"    /* for LOOP_DESCR */

#include "gcm_licm.h"
#include "gcm.h"
#include "tag.h"

    /* STL OP vector */
typedef mempool_allocator<OP*> OP_ALLOC;
typedef std::vector<OP*,OP_ALLOC>   OP_Vector;
typedef OP_Vector::iterator    OP_Vector_Iter;

    /* STL BB list */
typedef mempool_allocator<BB*> BB_ALLOC;
typedef std::list<BB*,OP_ALLOC>     BB_Lst;
typedef BB_Lst::iterator       BB_Lst_Iter;

     /* class LI_TN_INFO :
      *     keep track of definition and use of TNs.
      */
class LI_TN_INFO {
private:
    OP_Vector _def_list;    /* {op|op define the corresponding TN } */
    OP_Vector _use_list;    /* {op|op use the corresponding TN}     */
    INT32     _inloop_def_num; /* # of in-loop-OPs that def this TN */    
    INT32     _inloop_use_num; /* # of in-loop-OPs that use this TN */  
    mBOOL     _loop_invar;  /* TRUE iff corrsponding TN is loop invar */

public:
    
    LI_TN_INFO (MEM_POOL* mp): 
        _def_list(mp), _use_list(mp), _loop_invar(FALSE), 
        _inloop_def_num(0), _inloop_use_num(0) {};
    ~LI_TN_INFO (void) {} ;

    void Add_Def_OP (OP* o) { _def_list.push_back (o); }
    void Add_Use_OP (OP* o) { _use_list.push_back (o); }

    const OP_Vector* Defs (void) const { return &_def_list; }   
    const OP_Vector* Uses (void) const { return &_use_list; }

    BOOL Is_Loop_Invar  (void) const { return _loop_invar; } 
    void Set_Loop_Invar (void)       { _loop_invar = TRUE; }
    void Reset_Loop_Invar (void)     { _loop_invar = FALSE;}

    INT32 Inloop_Def_Num (void) const { return _inloop_def_num;   }
    INT32 Dec_Inloop_Def_Num (void)   { return --_inloop_def_num; }
    INT32 Inc_Inloop_Def_Num (void)   { return ++_inloop_def_num; }

    INT32 Inloop_Use_Num (void) const { return _inloop_use_num;   }
    INT32 Dec_Inloop_Use_Num (void)   { return --_inloop_use_num; }
    INT32 Inc_Inloop_Use_Num (void)   { return ++_inloop_use_num; }
};

    /* =============================================================
     * =============================================================
     * 
     *  class LOOP_TOPO_ITER
     * 
     *    iterator used to topologically traverse loop. 
     *
     * =============================================================
     * =============================================================
     */ 
class LOOP_TOPO_ITER {
private:
    MEM_POOL*   _mp;
    BB_SET*     _visit; /* _visit: BB -> boolean */
    LOOP_DESCR* _loop;  /* the loop in question */  
    BB*         _cur;   /* currently visited node */
    BB_Lst      _seq;   /* visit sequence */

public:
    LOOP_TOPO_ITER (LOOP_DESCR* l, MEM_POOL* mp);
    ~LOOP_TOPO_ITER (void) {};
    BOOL end   (void) const { return _seq.empty (); }
    
    LOOP_TOPO_ITER& operator++ (void);  // prefix inc 
    // LOOP_TOPO_ITER& operator++ (int);   // postfix inc

    BB*  operator*(void) const { return _cur; }
};

    /* =============================================================
     * =============================================================
     *
     * class LI_OP_INFO and LI_OP_INFO_MGR 
     *
     *  Bind OP within loop with some essential info as described by 
     *  LI_OP_INFO which is managed under LI_OP_INFO_MGR.
     *
     * =============================================================
     * =============================================================
     */
class LI_OP_INFO_MGR;
class LI_OP_INFO {
friend class LI_OP_INFO_MGR;
private:
    mBOOL  _def_loop_invar;
    INT16  _level;  /* level(op) = +-- 0 : op is loop entry point
                     *             |-- max{level(pred)} + 1:otherwise.
                     *             +-- -1: invalid
                     */                
    void Set_Level (INT32 l) { _level = (INT16)l; }

public:
    LI_OP_INFO  (void): _def_loop_invar(FALSE), _level(0) {};
    ~LI_OP_INFO (void) {};
    BOOL Def_Loop_Invar (void) const { return _def_loop_invar; }
    void Set_Def_Loop_Invar   (void) { _def_loop_invar = TRUE; }
    void Reset_Def_Loop_Invar (void) { _def_loop_invar = FALSE;}

    INT32 Level         (void) const { return (INT32)_level; }
    BOOL  Invalid_Level (void) const { return _level == -1; } 
};

class LI_OP_INFO_MGR {
private:
    MEM_POOL*   _mp;
    LOOP_DESCR* _loop;

    INT16       _bb_num;    // this many blocks in loop  
    INT16       _op_num;    // this many ops in loop.  
    INT16       _max_level; //

    typedef struct bb_map_bb_pair{
        BB_OP_MAP op_info; 
        BB* bb; 
    } BB_MAP_BB_PAIR;
    BB_MAP_BB_PAIR* _info;     // (_info[i]) : OP -> LI_OP_INFO 

            /* get BB_MAP_BB_PAIR associated with <b> */
    inline const BB_MAP_BB_PAIR* Idx (BB* b) const; 

    void Init_OP_Level (void);

public:
    LI_OP_INFO_MGR (LOOP_DESCR* l, MEM_POOL* mp);
    ~LI_OP_INFO_MGR (void) {} ;

    LI_OP_INFO* Get (OP*) const;
    void        Set (OP*, LI_OP_INFO*);

    INT32 BB_Num    (void) const { return (INT32)_bb_num;  } 
    INT32 OP_Num    (void) const { return (INT32)_op_num;  }
    INT32 Max_Level (void) const { return (INT32)_max_level;}

    BOOL It_is_Tiny_Loop  (void) const {return _op_num <= 8;}
    BOOL It_is_Small_Loop (void) const {return _op_num<16 && _op_num >=8;}
    BOOL It_is_Mediate_Sized_Loop (void) const 
                    { return _op_num < 32 && _op_num >= 16; }
    BOOL It_is_Large_Loop (void) const { return _op_num >= 32; }
    BOOL It_is_Huge_Loop (void) const  { return _op_num >= 128; }
    BOOL Very_Hot_Loop (void) const { 
                    BB* b = LOOP_DESCR_loophead(_loop); 
                    return BB_freq(b) > 1000000;
         }
};

    /* ==============================================================
     *
     * Init_OP_Level()
     *   level-func is such termed:
     *     level(op) = +-- 0 : op is loop entry point
     *                 |-- max{level(pred)} + 1:otherwise.
     *                 +-- -1: invalid
     *   we use level() to "guesstimate" register pressure.
     * 
     * =============================================================
     */
void
LI_OP_INFO_MGR :: Init_OP_Level (void) {

        /* Loop each BB in topolocal order 
         */
    INT32 start_level = 0;
    BB* header = LOOP_DESCR_loophead (_loop);

    for (LOOP_TOPO_ITER iter(_loop, _mp); !iter.end (); ++iter) {
        BB* cur_bb = *iter;

            /* determine level(BB_first_op(cur_bb)) 
             */
        if (cur_bb != header) {
            BBLIST* P;
            FOR_ALL_BB_PREDS (cur_bb,P) {
                BB* pred = BBLIST_item(P); 
                if (BB_loop_head_bb (pred) != header) {
                    /* it is not in this loop */
                    continue; 
                }

                OP* last = BB_last_op (pred);
                if (last) {
                    INT32 l = Get(last)->Level ();
                    start_level = MAX(start_level, l);
                }
            }
        }

        OP* op;
        FOR_ALL_BB_OPs (cur_bb, op) {
            LI_OP_INFO* oi = Get(op);
            oi->Set_Level (start_level++);
        }

    }
}

LI_OP_INFO_MGR :: LI_OP_INFO_MGR (LOOP_DESCR* l, MEM_POOL* mp): 
    _loop (l), _mp(mp) {

    BB_SET* bbs = LOOP_DESCR_bbset (l);
    BB* bb; 
    _bb_num = 0 ; 
    _op_num = 0;

        /* 1. get the loop body size 
         */
    FOR_ALL_BB_SET_members (bbs, bb) {
        _bb_num ++;  _op_num += BB_length(bb); 
    }
    
        /* 2. Initialize mapping data structure. 
         */
    _info = TYPE_MEM_POOL_ALLOC_N (BB_MAP_BB_PAIR, _mp, _bb_num); 
    INT i = 0;
    FOR_ALL_BB_SET_members (bbs, bb) {
        _info[i].op_info = BB_OP_MAP_Create (bb, _mp);
        _info[i].bb = bb; 

        OP* o;
        FOR_ALL_BB_OPs (bb, o) {
            LI_OP_INFO* oi = CXX_NEW (LI_OP_INFO, _mp);
            BB_OP_MAP_Set (_info[i].op_info, o, oi); 
        }

        i ++;
    }

        /* bubble sort BB_OP_MAPs so that those smaller corresponding
         * BB-id is permuted before the larger one. This sort is 
         * actually leads into NOP because "FOR_ALL_BB_SET_members"
         * visit BB of smaller ID before that of larger one. 
         */
    for (INT i1 = _bb_num - 2; i1 >= 0; i1 --) {
        for (INT i2 = i1; i2 >= 0 ; i2 --) {
            if (BB_id(_info[i2+1].bb) < BB_id(_info[i2].bb)) {
                    /* swap them */
                BB_MAP_BB_PAIR t = _info[i2];
                _info[i2] = _info[i2+1]; _info[i2] = t;
            }
        }
    }

    Init_OP_Level ();
}
      
    /* ========================================================
     *
     * Idx (BB* b):
     *      return  BB_MAP_BB_PAIR associated with <b>
     *
     * ========================================================
     */
inline const LI_OP_INFO_MGR::BB_MAP_BB_PAIR* 
LI_OP_INFO_MGR :: Idx (BB* b) const {
    
    mBB_NUM id = BB_id (b);
    INT32 low=1, high=_bb_num, mid;

    do {
        mid = (low + high)/2;
        mBB_NUM t = BB_id (_info[mid-1].bb);
        
        if (t == id) return _info+mid-1;
        if (t < id)  { low = mid + 1; continue; }
        high = mid - 1;
    } while (low <= high);

    Is_True (FALSE, ("No BB_MAP_BB_PAIR associated with BB:%d", id));
    return NULL; // make compiler happy. 
}

    /* ==========================================================
     * 
     * Return <op> corresponding LI_OP_INFO
     *
     * =========================================================
     */
LI_OP_INFO* 
LI_OP_INFO_MGR :: Get (OP* op) const {

    const BB_MAP_BB_PAIR* t = Idx (OP_bb(op));
    return (LI_OP_INFO*) BB_OP_MAP_Get (t->op_info, op);
}

    /* =========================================================
     * 
     * Bind <info> with <op>  
     * 
     * =========================================================
     */
void
LI_OP_INFO_MGR :: Set (OP* op, LI_OP_INFO* info) {
    const BB_MAP_BB_PAIR* t = Idx (OP_bb(op));
    BB_OP_MAP_Set (t->op_info, op, info);
}

LOOP_TOPO_ITER :: LOOP_TOPO_ITER (LOOP_DESCR* l, MEM_POOL* mp) : _mp(mp) {
    _loop = l;

    _cur = LOOP_DESCR_loophead(l);
    _seq.push_back (_cur);
    _visit = BB_SET_Create_Empty (252, _mp);
}

    /* advance */
LOOP_TOPO_ITER&
LOOP_TOPO_ITER :: operator++ (void) {

    if (end ()) { _cur = NULL; return *this; }

    Is_True (_cur == *_seq.begin (), 
("currently visited (BB:%d) is not the first element of visit-sequence", 
             BB_id(_cur)));

    BB_SET* body = LOOP_DESCR_bbset(_loop);
    BB* head =  LOOP_DESCR_loophead(_loop);

    _visit = BB_SET_Union1D (_visit, _cur, _mp);
    _seq.pop_front (); 

    BBLIST *s,*p;
    FOR_ALL_BB_SUCCS(_cur, s) {

        BB* succ = BBLIST_item(s);
        if (!BB_SET_MemberP (body,succ)) { continue; }
        if (BB_SET_MemberP (_visit,succ)){ continue; }

        BOOL all_pred_visited = TRUE;
        if (succ != head) {

            FOR_ALL_BB_PREDS (succ, p) {
                BB* pred = BBLIST_item (p);
                if (!BB_SET_MemberP (_visit, pred)) {
                    all_pred_visited = FALSE; break;
                }
            }
        }
        
        if (all_pred_visited) { _seq.push_back (succ); }
    }

    _cur = *_seq.begin ();
    
    return *this;
}


    /* ===========================================================
     * ===========================================================
     * 
     * class LOOP_INVAR_CODE_MOTION  
     *   
     *    Loop invariant identification and code motion.
     *
     * ===========================================================
     * ===========================================================
     */
class LOOP_INVAR_CODE_MOTION : public CXX_MEM_POOL {
    /* 
     * Hint: LI and LICM is acronym for "loop invariant" and 
     *       "Loop invariant code motion" respectively. 
     */
private:
    LOOP_DESCR* _loop;  /* currently handled loop */
    BB* _prolog;      /* corresponding prologue */
    MEM_POOL* _mp;    /* underlying MEM_POOL*/

    TN_SET*  _def_within_loop; /* all TNs that are defined within loop */
    GTN_SET* _liveout_defs;    /* TNs that live out of loop. */
    
    hTN_MAP _tn_info;          /* f: tn -> class LI_TN_INFO */
    LI_OP_INFO_MGR _op_info;  
    OP_Vector _loop_invar_defs;/* all loop invariants definitions in 
                                * in topological order. */

    BB_Lst  _loop_exits;
    
    OP_Vector _ld_ops,   /* all loads in this loop */ 
              _store_ops,/* all stores in this loop*/ 
              _call_ops; /* all calls in this loop */

    BOOL _ugly_loop;     /* whether this loop is ugly or not */
    BOOL _ignore_tn_dep; /* */
    BOOL _trace;         /* whether dump the trace */

    BOOL It_Is_Constant_TN (TN* t) const {
        return TN_is_const_reg(t) || 
               TN_is_constant (t) || 
               t == GP_TN ||
               t == Zero_TN; }

    BOOL It_Is_Loop_Invar_TN (TN* t) const {
            LI_TN_INFO* i = (LI_TN_INFO*)hTN_MAP_Get (_tn_info,t);
            return i->Is_Loop_Invar ();
        }

    LI_OP_INFO* Get_OP_Info (OP* o) const { return _op_info.Get (o); }

    void Sanity_Check (void); /* Abort if this routine does not happy */

    void Calc_Defs_Within_Loop_Body (void);
    void Calc_Liveout_Defs (void);
    void Collect_TN_Def_Use_Info (void);
    void Collect_Ld_St_Call (void);
    void Find_Out_All_Exits (void);
    void Misc_Init (void);
    BOOL Init (void);
    BB*  Loop_Is_Zdl();
    BB*  Find_Prolog();

    void Mark_Dep_OPs_As_Non_Loop_Invar (OP* op);

    void Identify_Loop_Invariants (void);

        /* Functions that are used to identify loop invariants:
         *
         *  o. TN_is_Loop_Constant (TN* tn)
         *     determine whether given <tn> is loop constant
         *  
         *  o. All_Reaching_Def_are_Outside_Of_Loop(op, opnd_idx)
         *     return TRUE iff all definitions of OP_opnd(op,opnd_idx) that 
         *     reach op is defined outside of loop. 
         * 
         *  o. Unique_Reaching_Def_Inside_Loop (op, opnd_idx)
         *     return TRUE Iff 
         *      - there is exactly one definition of OP_opnd(op,opnd_idx) 
         *        reach <op> 
         *      - the definition in quesion reside within loop body. 
         *
         *  o. Register_Para_Passing(OP*)
         *     determine whether <op> define a register which serve as 
         *     passing parameter to call.
         */ 
    BOOL TN_is_Loop_Constant (TN* tn);
    BOOL All_Reaching_Def_are_Outside_Of_Loop (OP *, INT opnd_idx);
    BOOL Unique_Reaching_Def_Inside_Loop (OP*, INT opdn_idx);
    BOOL Register_Para_Passing (OP* op);
    BOOL Alias_With_Call(OP*, OP* call);
    BOOL It_is_Fake_Loop_Invar (OP*);
    
        /* Functions that are used to perform L.I code motion. 
         *
         * o. Def_Dominate_All_Use (OP* op) 
         *      return TRUE iff loop invariants defined by <op> dominate 
         *      all use of these invariants. 
         *
         * o. Live_Out_Of_Loop (OP* op)
         *      determine whether left-hand-side variables of <op> live out
         *      loop. This function is useful when:
         *            !Def_Dominate_All_Use () && !Dom_All_Loop_Exits ()
         *      at that circumstance, moving <op>, which satisfy 
         *      Live_Out_Of_Loop(), to loop-prologue will produce 
         *      incorrect results. 
         * 
         * o. Code_Motion_Is_Profitable (OP* op)
         *      Determine whether perform L.I.C.M of <op> is profitable or not.   
         */
    BOOL Def_Dominate_All_Use    (OP*);
    BOOL Dom_All_Loop_Exits      (BB* );
    inline BOOL Live_Out_Of_Loop (OP* ) const;
    BOOL Illegal_Or_Nonprofitable (OP* );
    void Perform_Code_Motion     (OP* );

        /* Heuristic
         * 
         *  o Ignore_Loop_With_Few_Interation 
         *    discard some non-interesting loop at very early stage.
         *  
         *  o.Ignore_Ugly_Loop
         *    discard some ugly loop, produced by CFlow. e.g., prolog is
         *    not prev of loophead
         * 
         *  o.Code_Motion_Is_Profitable 
         *    called rigth refore performing code motion.
         */
    BOOL Ignore_Ugly_Loop();
    BOOL Ignore_Loop_With_Few_Interation (void);
    BOOL Code_Motion_Is_Profitable (OP*);

public:
    LOOP_INVAR_CODE_MOTION  (LOOP_DESCR* l, BOOL ign_tn_dep);
    ~LOOP_INVAR_CODE_MOTION (void) {} 

    INT32 Perform_Code_Motion (void);

        /* Auxillary functions
         */
    BOOL Loop_Has_Call (void) const  { return Is_Call_Loop(_loop);}
    const LOOP_DESCR* Loop (void) const { return _loop;}

    void Dump (FILE* f=stderr);
    #ifdef Is_True_On    
        /* gdb (v5.2) is clumsy at calling a func with parameter being 
         * assigned a default value. Hence "gdb_dump()".
         */
        void gdb_dump (void);  
    #endif 
};

//==================================================================
// Ignore_Ugly_Loop
//
// Some loops are ugly, e.g., (1)cannot find profitable prolog; (2) prolog
// is not fall throught to loop head; (3) not a real loop; ....
//
//==================================================================
BOOL LOOP_INVAR_CODE_MOTION::Ignore_Ugly_Loop()
{
  if( _ugly_loop )
    return TRUE;

  if( !_prolog )
    return TRUE;

  BB *head = LOOP_DESCR_loophead( _loop );
  if( !head )
    return TRUE;

  return FALSE;
}

    /* ================================================================= 
     * Calc_Defs_Within_Loop_Body 
     * 
     * Collect all TNs that are defined within loop body. We think all 
     * TNs on the right-hand-side of OP are "defined" are by that OP
     * no matter it is constant or not.
     * =================================================================
     */
void
LOOP_INVAR_CODE_MOTION :: Calc_Defs_Within_Loop_Body (void) {

    BB_SET* bbs = LOOP_DESCR_bbset (_loop);
    BB* bb; 

    FOR_ALL_BB_SET_members (bbs, bb) {
        OP* op;

        FOR_ALL_BB_OPs (bb, op) {
            for (INT i = OP_results (op) - 1; i >= 0; i--) {
                TN* res = OP_result(op, i);
                _def_within_loop = TN_SET_Union1D (_def_within_loop,res, _mp);

            }
        }
    }
}

    /* =================================================================
     * Calc_Liveout_Defs 
     * 
     *  Collect all TNs that are live out of loop. 
     *
     *  CONSTRAINTS:
     *    Loop exits should be identified prior to calling this function. 
     * 
     * =================================================================
     */
void
LOOP_INVAR_CODE_MOTION :: Calc_Liveout_Defs (void) {

    BB_SET* body = LOOP_DESCR_bbset (_loop);

    for (BB_Lst_Iter iter = _loop_exits.begin (); 
         iter != _loop_exits.end (); iter++) {

        BB* exit_blk = *iter;
        BBLIST* s;

        FOR_ALL_BB_SUCCS (exit_blk, s) {
            BB* succ = BBLIST_item (s);
            if (BB_SET_MemberP (body, succ)) { continue ; }
        
            _liveout_defs = 
                GTN_SET_UnionD (_liveout_defs, BB_live_in (succ), _mp);

        } // end of FOR...(exit_blk,s)
    } // end of for(BB_Lst_Iter...)
}


    /* =================================================================
     * Collect_TN_Def_Use_Info 
     *
     *   Find out all def and use of each TN. these info are kept track 
     *   of by LOOP_INVAR_CODE_MOTION::_tn_info which is indiced by 
     *   TN. 
     * =================================================================
     */
void
LOOP_INVAR_CODE_MOTION :: Collect_TN_Def_Use_Info (void) {

    BB_SET* bbs = LOOP_DESCR_bbset(_loop);
    BB* bb; 

    FOR_ALL_BB_SET_members (bbs, bb) {

        OP* op;
        FOR_ALL_BB_OPs (bb, op) {
                /* Acquire left-hand-side TN's information
                 */
            for (INT i = 0 ; i < OP_results(op); i++) {

                TN* res = OP_result(op,i);
                if (It_Is_Constant_TN (res)) { continue; }

                LI_TN_INFO* info;
                info = (LI_TN_INFO*)hTN_MAP_Get (_tn_info, res);

                if (!info) { 
                    info = CXX_NEW(LI_TN_INFO(_mp), _mp); 
                    hTN_MAP_Set (_tn_info, res, (void*)info);
                }

                info->Add_Def_OP (op);
                info->Inc_Inloop_Def_Num ();

            } // end of for(...OP_Results...)

                /* Acquire right-hand-side TN's information
                 */
            for (INT i = 0; i < OP_opnds(op); i++) {
                
                TN* opnd = OP_opnd(op,i);
                if (It_Is_Constant_TN (opnd)) { continue; }

                LI_TN_INFO* info;
                info = (LI_TN_INFO*)hTN_MAP_Get (_tn_info, opnd);

                if (!info) {
                    info = CXX_NEW (LI_TN_INFO(_mp), _mp); 
                    hTN_MAP_Set (_tn_info, opnd, (void*)info);
                }

                info->Add_Use_OP (op);
                info->Inc_Inloop_Use_Num ();

            } // end of for(...OP_opnd...)
        } // end of for ALL_BB_OPs
    } // end of FOR_ALL_BB_SET_members
}

    /* =============================================================
     *
     * Collect_Ld_St_Call 
     * 
     *   use 3 separated list to collect all loads stores and calls.
     *
     * =============================================================
     */
void
LOOP_INVAR_CODE_MOTION :: Collect_Ld_St_Call (void) {

    BB_SET* bbs = LOOP_DESCR_bbset(_loop);
    BB* bb; 

    FOR_ALL_BB_SET_members (bbs, bb) {
        OP* op;
        FOR_ALL_BB_OPs (bb,op) {

            extern BOOL OP_like_store(OP *op);

            if (OP_load (op)) { _ld_ops.push_back (op) ; continue; }
            if (OP_like_store (op)) { _store_ops.push_back(op); continue; }
            if (OP_call (op)) {
                _call_ops.push_back (op); 
                    /* findloops.cxx does not set this flag, so we set it 
                     * each time we encount a loop.
                     */
                Set_Call_Loop (_loop);     
            }
        }
    } 
}

    /* ==============================================================
     * Find_Out_All_Exits 
     * 
     *   Find out loop exits and store them <_loop_exits>. function  
     *   create great difficulty in identifing a loop-exit which 
     *   will be ampled later. 
     *   
     *   A sub-routine may break a loop (such as longjmp()) or terminate
     *   program's execution. We conservatively treat any call as a 
     *   loop-exit point. 
     *
     * ==============================================================
     */
void
LOOP_INVAR_CODE_MOTION :: Find_Out_All_Exits (void) {

    BB_SET* bbs = LOOP_DESCR_bbset(_loop);
    BB* bb;

    FOR_ALL_BB_SET_members (bbs, bb) {

        BBLIST *succs;
        FOR_ALL_BB_SUCCS(bb, succs) {
            BB *succ = BBLIST_item(succs);
            if (!BB_SET_MemberP(bbs, succ) && 
                find (_loop_exits.begin(), 
                      _loop_exits.end(), succ) == _loop_exits.end()) {
                _loop_exits.push_back (bb);
            }
        }
    }
}
    
void
LOOP_INVAR_CODE_MOTION :: Misc_Init (void) {
}

    /* ==============================================================
     *
     * Sanity_Check 
     *
     *     this function will abort execution via Is_True () if 
     *     he does not feel happy. 
     *
     * ==============================================================
     */
void
LOOP_INVAR_CODE_MOTION :: Sanity_Check (void) {

#ifdef Is_True_On
        /* 1. Since some code in this package cound upon OP->order field,
         * we need to double check this field are managed well. 
         */
    BB_SET* bbs = LOOP_DESCR_bbset (_loop);
    BB* bb; 

    FOR_ALL_BB_SET_members (bbs, bb) {
        BB_Verify_OP_Order (bb); 
    }
#endif /*Is_True_On*/
}

BOOL
LOOP_INVAR_CODE_MOTION :: Init (void) {

    _tn_info       = hTN_MAP_Create (_mp);

    _def_within_loop = TN_SET_Create_Empty (Last_TN + 1, _mp);
    _liveout_defs    = TN_SET_Create_Empty (Last_TN + 1, _mp);

    Find_Out_All_Exits (); /*should be prior to Calc_Liveout_Defs()*/
    Calc_Defs_Within_Loop_Body ();
    Calc_Liveout_Defs  (); 
    Collect_TN_Def_Use_Info ();
    Collect_Ld_St_Call ();

    Misc_Init ();

    _trace = Get_Trace (TP_GCM, GCM_TRACE_LICM );

    return TRUE;
}

// ========================================================================
// Loop_Is_Zdl
//
// return the prolog if it's a zdl, else return NULL
// ========================================================================
BB* LOOP_INVAR_CODE_MOTION::Loop_Is_Zdl()
{
  BB *head = LOOP_DESCR_loophead( _loop );
  BB *prolog = BB_prev( head );
  if( !prolog )
    return NULL;

  while( Is_BB_Empty(prolog) )
    prolog = BB_prev(prolog);

  // Sometimes CFLow will make the CFG ugly,
  // There are one problems in CFlow, it may creates BB like:
  // BB:1
  //   ...
  //   loop 0, tag_xxx
  //   jp .BB:3
  // BB:2
  //   [other BBs not related to this loop]
  // BB:3
  //   ...loop body..
  // .tag_xxx
  //
  // So if I cannot find the prolog through OP_prev(), then have to
  // go create prolog before BB:3, and move 'loop' into that prolog.
  // Only then, can I move code from loop body BEFORE 'loop'
  OP *loop_op = BB_last_op( prolog );
  if( !OP_is_loop(loop_op) ){
    BBLIST *preds_list;
    prolog = NULL;
    FOR_ALL_BB_PREDS( head, preds_list ){
      if( BB_loop_op(BBLIST_item(preds_list)) ){
        if( prolog )
          Is_True( 0, ("multi loop insn meet on a single loop head") );
        prolog = BBLIST_item( preds_list );
      }
    }
    // If we find 'loop' using FOR_ALL_BB_PREDS, but not BB_prev, then
    // we got ugly loop, produced by cflow
    if( prolog )
      _ugly_loop = TRUE;
  }

  return prolog;
}

//=================================================================
// Find_Prolog
//
// If it's a zero delay loop, then find its prolog using Loop_Is_Zdl;
// If it's a non-zdl loop, then try to find it in normal way. 
// If the above two fails, create one. 
//
//=================================================================
BB* LOOP_INVAR_CODE_MOTION::Find_Prolog() 
{
    BB* lhead = LOOP_DESCR_loophead(_loop);
    BB_SET* body = LOOP_DESCR_bbset(_loop);
    BB* prolog = NULL;

    prolog = Loop_Is_Zdl();
    if( prolog ){
      if( _trace )
        fprintf( TFile, "this is zero delay loop\n" );
      return prolog;
    }

    BBLIST* p;
    FOR_ALL_BB_PREDS (lhead, p) {
        BB* pred = BBLIST_item (p);
        if (BB_SET_MemberP (body, pred)) { continue; }
        
        if (prolog) {
            prolog = NULL; break; 
        } else {
            prolog = pred;
        }
    }

    if( prolog ){
      if (BB_kind (prolog) != BBKIND_GOTO || 
        BB_branch_op (prolog)) {
        prolog = NULL;
      }
    }

    if (prolog) {
      Set_BB_gra_spill(prolog);
    }

    if( prolog && BB_prev(lhead) != prolog ){
      DevWarn( "prolog BB:%d not fall through to BB:%d. Ugly due to cflow", 
               BB_id(prolog), BB_id(lhead) );
      DevWarn( "I don't do LICM for it" );
      _ugly_loop = TRUE;
      return NULL;
    }else{ 
      if( !prolog ){
        if( _trace )
          fprintf( TFile, "Create a new :\n" );
        prolog = CG_LOOP_Gen_And_Prepend_To_Prolog( lhead,_loop );

        Set_BB_dom_set( prolog, BS_Create_Empty( 2+PU_BB_Count+1, _mp ) );
        BS_UnionD( BB_dom_set(prolog), BB_dom_set(lhead), _mp );
        BS_Union1D( BB_dom_set(prolog), BB_id(prolog), _mp );
        BS_Difference1D( BB_dom_set(prolog), BB_id(lhead) );

        Set_BB_pdom_set( prolog, BS_Create_Empty( 2+PU_BB_Count+1, _mp ) );
        BS_UnionD( BB_pdom_set(prolog), BB_pdom_set(lhead), _mp );
        BS_Union1D( BB_pdom_set(prolog), BB_id(prolog), _mp );
      }
      if( _trace ){
        fprintf( TFile, "Prolog is :\n" );
        Print_BB_No_Srclines( prolog );
      }
     
      return prolog;
    }
}


LOOP_INVAR_CODE_MOTION::LOOP_INVAR_CODE_MOTION( LOOP_DESCR* l, BOOL ign_tn_dep ):
    CXX_MEM_POOL("Loop Invariant Code Motion", FALSE),
    _mp((*this)()), _loop_exits (_mp), _loop_invar_defs(_mp), 
    _ld_ops (_mp), _store_ops(_mp), _call_ops(_mp),
    _loop(l), _op_info(l, _mp), _ugly_loop(FALSE),
    _ignore_tn_dep(ign_tn_dep) {
    Init ();
    _prolog   = Find_Prolog(); 
}

    /* ================================================================ 
     * TN_is_Loop_Constant (TN*)
     *
     *  return TRUE if <tn> is loop constant. 
     * 
     * NOTE: 
     *   for simplicity, only constant is identified as "loop constant". 
     * =================================================================
     */
BOOL
LOOP_INVAR_CODE_MOTION :: TN_is_Loop_Constant (TN* t) {

    if (It_Is_Constant_TN (t)) { return TRUE; }

    LI_TN_INFO* info;
    info = (LI_TN_INFO*)hTN_MAP_Get (_tn_info, t);

    return info->Is_Loop_Invar(); 
}


    /* ===============================================================
     * All_Reaching_Def_are_Outside_Of_Loop (OP* op, INT opnd_idx)
     *
     *     return TRUE iff all definitions of OP_opnd(op,opnd_idx) that 
     *     reach op is defined outside of loop. 
     * ===============================================================
     */ 
BOOL
LOOP_INVAR_CODE_MOTION :: All_Reaching_Def_are_Outside_Of_Loop 
    (OP* op, INT opnd_idx) {

    TN* opnd = OP_opnd (op, opnd_idx); 
    
    Is_True (opnd, 
        ("OP[%d] (of BB:%d)'s No.%d operand does not exist!", 
          OP_map_idx(op), BB_id(OP_bb(op)), opnd_idx));

        /* Constant literal/reg is deemed as "defined" outside of loop 
         */
    if (It_Is_Constant_TN (opnd)) { return TRUE; }

    return !TN_SET_MemberP (_def_within_loop, opnd);
}

    /* =================================================================
     * Unique_Reaching_Def_Inside_Loop 
     *
     * return TRUE Iff 
     *   o there is exactly one definition of OP_opnd(op,opnd_idx) reach <op> 
     *   o the definition in quesion reside within loop body. 
     * 
     * NOTE: 
     *   Without immediate-dominance information, it is quite hard to 
     *   kown whether a def is acutally reach given point. Hence, in 
     *   this function think any def of variable v can reach any other 
     *   point inside the loop(either through a path within one iteration
     *   of a path across ajacent iternations).
     *
     * TODO:
     *   Current identification of "Uniqueness" is too conservative. 
     * 
     * =================================================================
     */
BOOL
LOOP_INVAR_CODE_MOTION :: Unique_Reaching_Def_Inside_Loop 
    (OP* op, INT opnd_idx) {
    
    BB* home = OP_bb (op);

    TN* opnd = OP_opnd (op, opnd_idx);
    LI_TN_INFO* ti = (LI_TN_INFO*)hTN_MAP_Get (_tn_info, opnd);
    OP_Vector* ops = (OP_Vector*)ti->Defs ();

    if (ops->size () != 1) {
        /* TODO: some def may be killed and hence could not reach 
         *    <op>, we should take this case into account.
         */
        return FALSE; 
    }

    OP* def = *ops->begin (); 

        /* def should dominate <opnd>
         */
    if (OP_bb(op) != OP_bb(def)) {
        if (BB_SET_MemberP (BB_dom_set(home), OP_bb(def))) { 
            return FALSE; 
        }
    } else {
        if (OP_Precedes (op,def) || op == def) { return FALSE; }
    }

    if (OP_has_predicate(op) && OP_opnd(op, OP_PREDICATE_OPND) != True_TN) {
        /* TODO: utilize PQS/PRDB to determine whether this statement 
         *   is true or not:
         *     <use>'s guarding predicate being 1 imply that <def>'s
         *     guarding predicate is 1.
         */
        return FALSE;
    }

    return TRUE;
}


    /* =========================================================
     * 
     * Register_Para_Passing 
     * 
     *   return true iff <op> define a regiter which serve as  
     *   passing parameter to sub-call.
     *
     * ===========================================================
     */
BOOL
LOOP_INVAR_CODE_MOTION :: Register_Para_Passing (OP* op) {

    if (!BB_call(OP_bb(op))) { return FALSE; }

    for (INT i = OP_results(op) - 1; i >= 0; i--) {
        TN* res = OP_result(op,i); 
        if (!TN_register(res)) { continue; }
        
        if (REGISTER_Is_Stacked_Output (TN_register_class(res), 
                                        TN_register(res))) {
            return TRUE;
        }
    }

    return FALSE;
}

    /* ===============================================================
     *
     *  Mark_Dep_OPs_As_Non_Loop_Invar 
     *    mark <op> does not compute loop invariants
     *
     * ===============================================================
     */
void
LOOP_INVAR_CODE_MOTION :: Mark_Dep_OPs_As_Non_Loop_Invar (OP* op) {

    for (INT i = OP_results(op) - 1; i >= 0; i--) {

        TN* res = OP_result(op, i);
        if (It_Is_Constant_TN (res)) { continue; }

        LI_TN_INFO* tninfo = (LI_TN_INFO*)hTN_MAP_Get (_tn_info, res);
        OP_Vector* ops = (OP_Vector*)tninfo->Uses ();

        for (OP_Vector_Iter iter = ops->begin (); 
             iter != ops->end (); iter++) {

            OP* use = *iter;
            LI_OP_INFO* opinfo = Get_OP_Info (use);
            opinfo->Reset_Def_Loop_Invar ();
        }
    }
}

//=============================================================
// In_DMA_Region
//
// in SL, we have following situations:
// Example-1:
//   .peripheral.rw.begin
// .label
//    ...
//    TN2 :- ldw 0x4000
//    br.nez TN2 label
//   .peripheral.rw.end
//
// the 'ldw' CANNOT be moved out of loop body, or above .label. since 
// DMA accesses must be done exactly that number of times.
//
// Need to check if .peripheral.rw.begin and .peripheral.rw.end only
// figure out a sub-range of code, not including 'op'.
// 
// Example-2:
// .label
//   .peripheral.rw.begin
//    ...
//   .peripheral.rw.end
//    TN2 :- ldw 0x4000
//    br.nez TN2 label
// This 'ldw' is not a DMA access, so it can be moved above.
//
// What's more complicated is nested DMA region.
// Example-3:
// .label
//   .peripheral.rw.begin
//   .peripheral.rw.begin
//    ...
//   .peripheral.rw.end
//    TN2 :- ldw 0x4000
//   .peripheral.rw.end
//    br.nez TN2 label
// This 'ldw' is still in DMA region. 
//
// In theory, it's very hard to check whether we are in DMA region,
// since there are many situations need to concern. 
// 
// There are also many barriers that forbid code motion, and this 
// needs a completely searching in the loop body.
//
// For now, I only want to fix that bug, and check only host bb and
// prolog of loop
//=============================================================
#define OP_is_dma_begin(o) ( OP_code(o) == TOP_peripheral_rw_begin )
#define OP_is_dma_end(o)   ( OP_code(o) == TOP_peripheral_rw_end )

static inline BOOL 
BB_has_barrier( BB *bb, OP *position )
{
  OP *op = position;
  if( !op )
    op = BB_last_op(bb);
  while( op ){
    if( CGTARG_Is_OP_Barrier(op) )
      return TRUE;
    op = OP_prev(op);
  }

  return FALSE;
}

static inline BOOL
BB_has_unpaired_dma_begin( BB *bb )
{
  BOOL unpaired_begin = FALSE;
  BOOL unpaired_end = FALSE;

  OP *op;
  FOR_ALL_BB_OPs_REV( bb, op ){
    if( OP_is_dma_begin(op) ){
      if( !unpaired_end )
        return TRUE;
      else
        unpaired_end = FALSE;
    }
    if( OP_is_dma_end(op) ){
      if( !unpaired_end )
        unpaired_end = TRUE;
      else
        Is_True( 0, ("more than 1 DMA end") );
    }
  }

  return FALSE;
}

static BOOL Is_Barriered( LOOP_DESCR *loop, BB *bb, OP *mem_op )
{
  // First check if bb contains .peripheral.rw.begin 
  OP *op = OP_prev(mem_op);
  if( BB_has_barrier(bb, mem_op) )
    return TRUE;

  // need to go through pred BBs until prolog 
  BB *head = LOOP_DESCR_loophead(loop);
  if( head ){
    BB *prolog = BB_Fall_Thru_Predecessor( head );
    if( prolog ){
      return BB_has_unpaired_dma_begin( prolog ); 
    }else{
      //This is a complicated situation, just forbid code motion.
      //If I do loop_struct_normalization, this shoud not happen.
      return TRUE;
    }
  }else{
    Is_True( 0, ("no head for loop") );
  }
}

static inline BOOL OP_Not_Suitable( LOOP_DESCR *loop, BB *bb, OP *op )
{
  if( CGTARG_Is_OP_Barrier(op) )
    return TRUE;

  if( OP_xfer (op) || OP_call (op) || OP_is_loop(op) ||
      ((OP_load(op) || OP_store(op)) && Is_Barriered(loop, bb, op)) )
    return TRUE;

  if( CGTARG_Is_OP_Intrinsic(op) ) 
    return TRUE;

  if( OP_volatile(op) )
    return TRUE;

  return FALSE;
}
    /* ===============================================================
     *
     * Identify_Loop_Invariants 
     * 
     *   This routine traverse loop body in topological order. It mark
     *   currently encount OP as "loop invariants definition" if it 
     *   satisfy all following conditions:
     *
     *    1. any src operand V must fall into one of the following category
     *          - V is Loop constant
     *          - all reaching definition of V reside out of loop. 
     *          - There is exactly one definiton of V reach this OP and 
     *            this def locates within loop.
     *    2. it is not a control-transfer (e.g call) instruction. 
     *
     * *IMPORTANT NOTE*:
     *
     *   We omit the issues of alias and the architecture specific dep. 
     *   Attacking these issues is postponed until right before the time 
     *   when we try to peform actual code motion by calling 
     *   It_is_Fake_Loop_Invar(). 
     * 
     *   The motivation of this two phase identifcation of loop-invar is 
     *   that: many fake "loop invariant"s found by this routines are 
     *   either illegal or non-profitable to perform code motion, hence 
     *   we save the time of determining the alias and arch-spec-dep between
     *   these fake loop-invar and other instructions. it seem to be 
     *   a big saving! 
     *
     * ===============================================================
     */
void
LOOP_INVAR_CODE_MOTION :: Identify_Loop_Invariants (void) {
         
    for (LOOP_TOPO_ITER iter(_loop, _mp); !iter.end (); ++iter) {

        BB* b = *iter;
        OP* op;

        FOR_ALL_BB_OPs (b, op) {
            BOOL it_is; // indidate whether <op> define loop-invar
            it_is = TRUE;

            for (INT i = OP_opnds (op) - 1; i >= 0 && it_is; i--) {

                TN* opnd = OP_opnd(op,i);
                it_is = 
                    TN_is_Loop_Constant (opnd)  ||
                    All_Reaching_Def_are_Outside_Of_Loop (op, i) || 
                    It_Is_Loop_Invar_TN (opnd) && 
                    Unique_Reaching_Def_Inside_Loop (op, i);
            }

            if( OP_Not_Suitable(_loop, b, op) ){
                it_is = TRUE;
                continue; 
            }

                /* Do not move those instructions that serves as paramater
                 * passing.
                 */
            if (it_is && Register_Para_Passing (op)) {
                it_is = FALSE;
            }

                /* mark all non-constant results as loop-invariant or 
                 * non-loop-invariant accordingly
                 */
            for (INT i = OP_results (op) - 1; i >= 0; i--) {
                TN* res = OP_result (op, i);

                if (It_Is_Constant_TN (res)) continue; 

                LI_TN_INFO* tninfo;
                tninfo = (LI_TN_INFO*) hTN_MAP_Get (_tn_info, res);
                if (tninfo->Inloop_Def_Num () != 1) { it_is = FALSE; }

                if (it_is) {
                    tninfo->Set_Loop_Invar ();
                } else {
                    tninfo->Reset_Loop_Invar ();
                    break;
                }

             }/* end of for(INT...) */

             if (it_is) {
                /* Memorize this loop-invar definition. Since we traverse
                 * the CFG in topolocal order, and each loop-invar-def we 
                 * encount is append to _loop_invar_defs, hence the OPs 
                 * in _loop_invar_defs are also topologically permuted. 
                 */
                _loop_invar_defs.push_back (op);
                _op_info.Get(op)->Set_Def_Loop_Invar ();
             } /* end of if(it_is) */

        } /* end of FOR_ALL_BB_OPs(...) */

    } /* end of for (LOOP_TOPO_ITER...) */
}

BOOL
LOOP_INVAR_CODE_MOTION :: Alias_With_Call (OP* op, OP* call) {

    BOOL alias =  
            !CG_DEP_Can_OP_Move_Across_Call (op, call, TRUE, _ignore_tn_dep) || 
            !CG_DEP_Can_OP_Move_Across_Call (op, call, FALSE, _ignore_tn_dep);

    if (!alias) { return FALSE; } 
    
    return TRUE;
}

BOOL
LOOP_INVAR_CODE_MOTION :: It_is_Fake_Loop_Invar (OP* op) {

    UINT8 omega = 0;
    BOOL  definite=FALSE;

    if (OP_store (op)) { return TRUE; }

    if (Loop_Has_Call ()) {
        for (OP_Vector_Iter iter = _call_ops.begin (); 
             iter != _call_ops.end (); iter++) {

            if (Alias_With_Call (op, *iter)) { return TRUE; }
        }
    }

    if (OP_load (op)) {
        for (OP_Vector_Iter st_iter = _store_ops.begin ();
             st_iter != _store_ops.end (); st_iter ++) {
            
            OP* op_like_st = *st_iter;
            if (!OP_store (op_like_st)) { 
                    /* e.g CGTARG_Is_OP_Intrinsic(). ref OP_like_store() 
                     * for details 
                     */
                return TRUE; 
            }

            if (CG_DEP_Mem_Ops_Alias (op, op_like_st, NULL)) {
                return TRUE;
            }
        }
    }

    return FALSE;
}

   /* ===============================================================
    * Def_Dominate_All_Use 
    * 
    *   return TRUE iff all uses of <linvar>'s left-hand-side might 
    *   be reached only be <linvar>. Put in other word <linvar> 
    *   dominate all use the its left-hand-side variables. 
    *
    * ==============================================================
    */
BOOL
LOOP_INVAR_CODE_MOTION :: Def_Dominate_All_Use (OP* linvar) {
    
    BB* home = OP_bb (linvar);
    
    for (INT i=0 ; i < OP_results(linvar); i++) {

        TN* res = OP_result(linvar, i);
        if (It_Is_Constant_TN (res)) { continue; }

        LI_TN_INFO* info;
        info = (LI_TN_INFO*)hTN_MAP_Get (_tn_info, res);
        Is_True (info, 
            ("LI_TN_INFO for OP:%d of BB:%d does not exist", 
              OP_map_idx(linvar), BB_id (OP_bb(linvar))));
       
        OP_Vector* ops = (OP_Vector*)info->Uses ();

            /* loop over each use of OP_result(linvar, i)
             */
        for (OP_Vector_Iter iter = ops->begin (); 
             iter != ops->end (); iter++) {

            OP* o = *iter;
            BB* t = OP_bb (o);

                /* If def and use locate in diff blocks, we can count on 
                 * BB level dorminance info, otherwise we resort to comparing
                 * the "order" fields of these 2 OPs.
                 */
            if (!BB_SET_MemberP (BB_dom_set(t), home)) {
                return FALSE; 
            } else if (t == home) {
                if (!OP_Precedes (linvar, o)) { return FALSE; }
            }
        }
    }
}

    /* ============================================================
     *  
     * Dom_All_Exits 
     *   Determine whether <b> dominate all loop exits. 
     * 
     * ============================================================
     */
BOOL
LOOP_INVAR_CODE_MOTION :: Dom_All_Loop_Exits (BB* b) {

    BOOL no_exit = TRUE;

    for (BB_Lst_Iter iter = _loop_exits.begin (); 
         iter != _loop_exits.end (); iter++) {

        BB* exit_bb = *iter;
        if (!BB_SET_MemberP (BB_dom_set(exit_bb), b)) { return FALSE; }

        no_exit = FALSE;
    }

        /* If this loop SHOULD had no loop exit, we conservatively 
         * think b does not dominate all exits.  
         */
    return  !no_exit;
}

    /* =============================================================
     *
     * Live_Out_Of_Loop 
     *
     *   Determine whether <op>'s left-hand-side variables are live
     *   out of loop.
     *
     * =============================================================
     */
inline BOOL
LOOP_INVAR_CODE_MOTION :: Live_Out_Of_Loop (OP* op) const {

    for (INT i = OP_results (op) - 1; i >= 0; i--) {
        TN* res = OP_result(op, i);
        
        if (!TN_is_constant(res) && TN_is_global_reg(res) &&
            GTN_SET_MemberP (_liveout_defs, res)) {
            return TRUE;
        }
    }
    return FALSE;
}

    /* ================================================================
     * 
     * Code_Motion_Is_Profitable 
     * 
     *   Determine whether move loop-invar def <op> to loop-header
     *   is profitable or not. Our concern are roughly two folds:
     *      -  register pressure
     *      -  penalty inherient in speculation: some L.I.C.Ms are 
     *         speculation (i.e OP is not always executed in every 
     *         interation)
     *
     * =================================================================
     */
BOOL
LOOP_INVAR_CODE_MOTION :: Code_Motion_Is_Profitable (OP* op) {
    
    BB* loop_ent = LOOP_DESCR_loophead(_loop);
    BB* home = OP_bb (op);

        /* rule 1: rule out those candidate of low reach probability 
         *    from loop entry. 
         */
      /* the larger prob-thresold the small latency
       */ 
    #define PROG_SCALE          (100)
    #define LOW_PROB_FOR_ALU_OP (70)
    #define LOW_PROB_FOR_LD_OP  (40)
    #define LOW_PROB_FOR_LONG_LATENCY (40)

    float freq1 = BB_freq (loop_ent), freq2 = BB_freq (home);
    freq1 = (freq1 < 1.0f) ? 1.0f : freq1;
    INT32 div = (INT32)(freq2 * PROG_SCALE/ freq1);

    if (CGTARG_Is_Long_Latency (OP_code (op))) {
        if (div < LOW_PROB_FOR_LONG_LATENCY) { return FALSE; }
    } else if (OP_load (op)) {
        if (div < LOW_PROB_FOR_LD_OP) { return FALSE; }
    } else {
        if (div < LOW_PROB_FOR_ALU_OP) { return FALSE; }
    }

        /* rule 2: prevent those code motion that deteriorate reg pressure.
         */
    if (Live_Out_Of_Loop (op)) {
        /* it does not change the reg-pressure */  
        return TRUE;
    }

    if (_op_info.It_is_Tiny_Loop ())  { return TRUE; }
    if (_op_info.It_is_Huge_Loop ())  { return FALSE;}
    if (_op_info.It_is_Large_Loop () && !_op_info.Very_Hot_Loop ()) {
        return FALSE;
    }


        /* for mediate sized loop 
         */
    /*
    INT32 max_use_level = 0;
    for (INT i = OP_results (op) - 1; i >= 0; i--) {
        TN* res = OP_result (op, i);
        
        LI_TN_INFO* ti = (LI_TN_INFO*)hTN_MAP_Get (_tn_info, res);
        OP_Vector* ops = (OP_Vector*)ti->Uses ();

        for (OP_Vector_Iter iter = ops->begin (); iter != ops->end (); iter++) {
            OP* use = *iter; 
            LI_OP_INFO* oi = _op_info.Get (use);  
            if (max_use_level > oi->Level ()) {
                max_use_level = oi->Level (); 
            }
        }
    } 

    LI_OP_INFO* oi = _op_info.Get (op);
    INT32 potion = (oi->Level() + 1 - max_use_level) * 100 / 
                        (_op_info.Max_Level () + 1);
    return potion >= 20;
    */
    return TRUE;
}

    /* ================================================================
     *
     * Ignore_Loop_With_Few_Interation 
     * 
     *    Detemine whether the current loop has few interations. If so, 
     *    performing loop invariant code motion will degrade performance.
     *
     *    This function should be called at very early stage to rule 
     *    out some non-interesting loop.
     *
     * ==================================================================
     */
BOOL
LOOP_INVAR_CODE_MOTION :: Ignore_Loop_With_Few_Interation (void) {

    BB* loop_ent = LOOP_DESCR_loophead(_loop);
    float f = 0.0f;

    BBLIST* pred;
    FOR_ALL_BB_PREDS (loop_ent,pred) {
        BB* b = BBLIST_item(pred);
        f += BB_freq (b);
    }

    f = (f < 1.0f) ? 1.0f : f;
    return (BB_freq (loop_ent) - f) < 10^5;
}

    /* =================================================================
     *
     * Can_be_Moved_to_Prologue 
     * 
     *   Determine whether loop invariant definition,<loopinvar>, can be 
     *   moved the end of prologue. 
     *
     *   NOTE: this routine assume all the left-hand-side variables of 
     *         <op> are loop invariants. 
     *
     * =================================================================
     */
BOOL
LOOP_INVAR_CODE_MOTION :: Illegal_Or_Nonprofitable (OP* loopinvar) {

    BB* home = OP_bb (loopinvar);

    if (!Def_Dominate_All_Use (loopinvar)) {
        return TRUE;
    }

        /* "<spec> == TURE" indicate the code motion is speculation rather 
         *  than useful code motion.
         */
    BOOL spec = !Dom_All_Loop_Exits (home);

    if (spec) {
            /* It will raise exception and hence erase your disk and 
             * hence spread epidemic such as severe acute respiratory
             * syndrome...!!! */
        if (!CGTARG_Can_Be_Speculative (loopinvar)) {
            return TRUE;
        }
            /* To make life easier, we do not perform code motion for 
             * speculated loads(ld.s, ld.a and ld.sa).
             */
        if (CGTARG_Is_OP_Speculative (loopinvar)) { 
            return TRUE; 
        }
            /* It will kill variables that are live extend the loop. 
             */
        if (Live_Out_Of_Loop (loopinvar)) { 
            return TRUE; 
        }
    }

    if (!Code_Motion_Is_Profitable (loopinvar)) { 
        return TRUE; 
    }

        /* Rule out the *FAKE* loop invariant definition, ref details 
         * of the comment right before the the definition of  
         * LOOP_INVAR_CODE_MOTION::Identify_Loop_Invariants ().
         */
    if (It_is_Fake_Loop_Invar (loopinvar)) {
        return TRUE;
    }

    return FALSE; 
}

    /* =============================================================
     *
     *  Perform_Loop_Invar_Code_Motion 
     * 
     *    This routine topogically visit each loop-invar definition.
     *    When progress, it will move some loop-invar def to prologue
     *    if it think this moving is both benefial and legal.
     *
     *    the return value is the # of code motion that is actully 
     *    performed. 
     * 
     * ==============================================================
     */
extern BOOL Should_Skip( int, int, int, int );
static BOOL GCM_LICM_Skip_Op_Binary_Search( INT32 op_id )
{
  BOOL skip = Should_Skip( CG_GCM_LICM_op_skip_before,
                           CG_GCM_LICM_op_skip_after,
                           CG_GCM_LICM_op_skip_equal,
                           op_id );
  return skip;
}

INT32 
LOOP_INVAR_CODE_MOTION :: Perform_Code_Motion (void) 
{
    if( _trace ){
      fprintf( TFile, "======= Loop Invariant Code Motion ====\n" );

      BB_SET* loop_bbs = LOOP_DESCR_bbset( _loop );
      fprintf( TFile, "GCM_For_Loop: list of bbs:" );
      BB_SET_Print( loop_bbs, TFile );
      fprintf( TFile, "\n" );

      /* It's better to dump the pred/succ of all BBs */
      BB* temp = NULL;
      BBLIST * list;
      FOR_ALL_BB_SET_members( loop_bbs, temp ){
        fprintf( TFile, "PREDs of BB[%i] are:", BB_id(temp) );
        FOR_ALL_BB_PREDS(temp, list) {
          BB *pred = BBLIST_item(list);
          fprintf( TFile, " %i, ",BB_id(pred) );
        }

        fprintf( TFile, "\nSUCCs of BB[%i] are:", BB_id(temp) );
        FOR_ALL_BB_SUCCS(temp, list) {
          BB *succ = BBLIST_item(list);
          fprintf( TFile, " %i, ",BB_id(succ) );
        }
        fprintf( TFile, "\n" );
      }
    }

    if( Ignore_Ugly_Loop() ){
      if( _trace ){
        fprintf( TFile, " -_- ignored since it's ugly loop, or not real loop\n");
      }
      fprintf( TFile, " Number of LICM: 0\n" );
      fprintf( TFile, "======= Finished Loop Invariant Code Motion ====\n" );
      return 0;
    }
 

    Identify_Loop_Invariants ();

        /* Looping over loop-invarants in topological order, NOTE:
         * it should be in topological order.
         */
    INT32 code_motion_num = 0;
    for (OP_Vector_Iter iter = _loop_invar_defs.begin ();
         iter != _loop_invar_defs.end (); iter++) {
        
        OP* linvar = *iter;
        LI_OP_INFO* opinfo = Get_OP_Info (linvar);
        
        BOOL perform = TRUE;

        if (!opinfo->Def_Loop_Invar ()) {
                /* This test is _*NOT*_ NOP, since some source operands 
                 * of <linvar> is defined by 
                 *   - some "fake" loop-invar identified by 
                 *     Identify_Loop_Invariants (), or 
                 *   - genuine loop-invar defs which is illegal or unprofitabl     
                 *     to be moved to the prologue. 
                 */ 
            perform = FALSE;
        }
            
        if (perform && Illegal_Or_Nonprofitable (linvar)) {
            perform = FALSE;
        }

        if (!perform) {
                /* mark those OPs that immediately flow dependents upon 
                 * this OPs as non-loop-invariant definition.
                 */
            Mark_Dep_OPs_As_Non_Loop_Invar (linvar);
        } else {
            if( !GCM_LICM_Skip_Op_Binary_Search(code_motion_num) ){
                Perform_Code_Motion (linvar);
                code_motion_num ++;
            }
        }
    }

    if( _trace ){
      fprintf( TFile, " Number of LICM: %i\n", code_motion_num );
      fprintf( TFile, "======= Finished Loop Invariant Code Motion ====\n" );
    }

    return code_motion_num;
}

    /* ==============================================================
     * 
     * Perform_Code_Motion 
     *
     *   move <linvar> to the end of prologue.
     * 
     * =============================================================
     */
static void Append_Op_To_BB(OP * cand_op, BB *cand_bb )
{
  OP *limit_op;

  // Due to CFlow problem, we may got a BB with 'loop' instruction followed
  // by 'jp'
  if( BB_loop_op(cand_bb) )
    limit_op = BB_loop_op(cand_bb);
  else
    limit_op = BB_xfer_op(cand_bb);

  if( !limit_op && BB_zdl_body(cand_bb) ) {
    OP* last_op = BB_last_op(cand_bb);
    Is_True( OP_has_tag(last_op), ("zdl tail bb should have tag") );

    /* Append the cand_op to cand_bb, and re-assign the tag */
    BB_Append_Op (cand_bb, cand_op);
    LABEL_IDX tag_idx = 0;
    tag_idx = Get_OP_Tag( last_op );
    Is_True( tag_idx > 0, ("incorrect tag index") );
    Reset_OP_has_tag( last_op );
    Set_OP_Tag( cand_op, tag_idx );
  }else{
    if (limit_op)
      BB_Insert_Op_Before (cand_bb, limit_op, cand_op);
    else
      BB_Append_Op (cand_bb, cand_op);
  }
}

void LOOP_INVAR_CODE_MOTION :: Perform_Code_Motion (OP* linvar) 
{
    if( _trace ){
      fprintf( TFile, " -- before moving following OP, from BB:%i to BB:%i\n",
               BB_id(OP_bb(linvar)), BB_id(_prolog) );
      Print_OP_No_SrcLine( linvar );
      Print_BB_No_Srclines( OP_bb(linvar) );
      Print_BB_No_Srclines( _prolog );
    }

    BB_Remove_Op( OP_bb(linvar), linvar );
    Append_Op_To_BB( linvar, _prolog );

    if( _trace ){
      fprintf( TFile, " -- after moving following OP, from BB:%i to BB:%i\n",
               BB_id(OP_bb(linvar)), BB_id(_prolog) );
      Print_OP_No_SrcLine( linvar );
      Print_BB_No_Srclines( OP_bb(linvar) );
      Print_BB_No_Srclines( _prolog );
    }

    // we need to change the result TNs to be global version since 
    // later on we will recaculate liveness from scratch.
}
        
    /* =============================================================
     *
     * Count_Loop_Interation 
     * 
     *  return the number of itertions loop will go. if loop has 
     *  constant trip-count, use that value, otherwise, derive 
     *  from frequency from backedge.
     *
     * ============================================================
     */
static float
Count_Loop_Interation (LOOP_DESCR* l) {

    LOOPINFO* li LOOP_DESCR_loopinfo(l);

    TN* trip_count = NULL; 
    if (li) {
        trip_count = LOOPINFO_trip_count_tn(li); 
    }
    
    if (trip_count && TN_is_constant(trip_count) && 
        TN_has_value(trip_count)) {
        return (float)TN_value(trip_count);
    }

    BBLIST* p;
    BB_SET* body = LOOP_DESCR_bbset(l);
    BB* head = LOOP_DESCR_loophead(l); 
    float f = 0.0f;
    FOR_ALL_BB_PREDS (head, p) {
        BB* succ = BBLIST_item(p);
        if (!BB_SET_MemberP (body, succ)) {
            continue;
        }

        BBLIST* bl = BB_Find_Succ (succ,head);
        f += BBLIST_freq(bl);
    }

    return f;
}


    /* ==============================================================
     * 
     * Skip_Loop_Invar_Code_Motion 
     *
     *   This function detect those loops that are not approciate for 
     *   loop invariant code motion. It is pre-cursor to actual Loop 
     *   Invariant Code Motion. 
     * 
     * ===============================================================
     */
static BOOL
Skip_Loop_Invar_Code_Motion (LOOP_DESCR* loop) {
    
    INT bb_num = 0 , op_num = 0;
    BB* bb;
    BB_SET* body = LOOP_DESCR_bbset(loop);

    #define HOT_LOOP_FREQ  (1000000)
    INT32 MAX_BB = 8, MAX_OP = 64;  
    
        /* relax the constraints for very hot loop 
         */
    bb = LOOP_DESCR_loophead(loop);
    if (BB_freq(bb) > HOT_LOOP_FREQ) {
        MAX_BB = 10; MAX_OP = 100; 
    }

    FOR_ALL_BB_SET_members (body, bb) {

        if (BB_rotating_kernel (bb)) {
               /* rule 1: It is hard for us to determine whether variable
                *   is defined or used within rotating-kernel-ed block. so
                *   we give up.
                */
            return TRUE;
        }

            /* rule 2: Do not perform LICM for very large loop 
             */
        if (BB_length(bb)) { ++ bb_num; }

        if (bb_num > MAX_BB) { return TRUE; }
        op_num += BB_length(bb);
        if (op_num > MAX_OP) { return TRUE; }
    }

        /* rule 3: ignore unimportant loops.
         */
    BB *head = LOOP_DESCR_loophead(loop);
    LOOPINFO *info = LOOP_DESCR_loopinfo(loop);

    if (info && WN_Loop_Unimportant_Misc(LOOPINFO_wn(info))) {
        return TRUE;
    }

    float f1 = Count_Loop_Interation (loop);
    float f2 = BB_freq(head);

    return !(f1 > 1000 || f2 > 10000);
}


    /* =================================================================
     *
     * Maintain_Dominator_Info
     * 
     *   Maintain the dominator-info in incremental manner  
     *   when prologue/epilogue is inserted
     *
     * =================================================================
     */
static void
Maintain_Dominator_Info (LOOP_DESCR* l, BB* prolog, MEM_POOL* mp) {

        /* set initial value of BB_pdom_set(prolog)
         */
    BB* head = LOOP_DESCR_loophead(l);

    if (prolog) {
        BS* dom_pro = BS_Copy (BB_dom_set(head), mp);
        dom_pro = BS_Union1D (dom_pro, (BS_ELT)BB_id(prolog),mp);
        dom_pro = BS_Difference1D (dom_pro, (BS_ELT)BB_id(head));
        Set_BB_dom_set (prolog, dom_pro);
    }
     
        /* propagate the changes
         */
    for (BB* b = REGION_First_BB; b; b = BB_next(b)) {

            /* <prolog> dominate <b> iff (1)<prolog> == <b> 
             *    or (2)- <head> dominate <b>.
             */
        BS* dom_set = BB_dom_set (b);
        if (BS_MemberP (BB_dom_set(b), BB_id(head))) {
            dom_set = BS_Union1D (dom_set, BB_id(prolog), mp);
            Set_BB_dom_set (b, dom_set);
        }
    }
}


    /* ==================================================================
     * 
     * Perform_Loop_Invariant_Code_Motion 
     *      The wrapper of LOOP_INVAR_CODE_MOTION::Perform_Loop_Motion (). 
     *      This function is exported to outside.
     *
     * ==================================================================
     */
void
Perform_Loop_Invariant_Code_Motion( LOOP_DESCR *loop, 
                                    MEM_POOL *pool,
                                    BOOL ignore_tn_dep )
{
    GRA_LIVE_Init (NULL);

    LOOP_INVAR_CODE_MOTION licm( loop, ignore_tn_dep );
    licm.Perform_Code_Motion();

    GRA_LIVE_Init (NULL);
}

