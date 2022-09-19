//-*-c++-*-

/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_vsa_checker.h
//
// ====================================================================
//


#ifndef opt_vsa_checker_INCLUDED
#define opt_vsa_checker_INCLUDED        "opt_vsa_checker.h"

#include "config_vsa.h"
#include "opt_vsa_eh.h"
#include "opt_vra.h"
#include "opt_vsa.h"

extern "C" void be_debug();

// ====================================================================
// VSA CHECKER
//   Traverse the CFG & CG by the U-D of sym and/or vsym.
//
// Components:
//   The whole checking process are implemented by three different
//   - The checker implementation which implements the call backs to
//     do real checking work.
//   - The infra-function and inter-function U-D traverser
//     do traverse U-D for sym and vsym on CFG and CG
//   - The driver to traverse the IR and initiatate the U-D traversal
//
// ====================================================================
enum TRAV_ENTITY {
  TE_STMTREP,        // statement should be visited
  TE_CODEREP,        // coderep should be visited
};

enum TRAV_DIRECTION {
  TD_NONE,           // for initial frame
  TD_UP,             // from callee to caller
  TD_DOWN,           // from caller to callee
};

enum CHECKER_SUSPECT {
  CS_NONE         = 0x0000,
  CS_DEREFERENCE  = 0x0001,   // dereference, CK_IVAR && ! OPR_PARM
  CS_DIVISOR      = 0x0002,   // divided by ZERO, second kid of OPR_DIV/REM/MOD/DIVREM
  CS_CALL         = 0x0004,   // call, for double free
  CS_ICALL        = 0x0008,   // icall, for icall promotion and devirtualization
  CS_VAR_DEF      = 0x0010,   // for var define
  CS_VAR_USE      = 0x0020,   // for var use
  CS_HEAP_OBJ     = 0x0040,   // for heap object
  CS_VSYM_OBJ     = 0x0080,   // for vsym object
  CS_INIT         = 0x0100,   // for value init
  CS_VPTR         = 0x0200,   // for vptr
  CS_ILOD_VPTR    = 0x0400,   // for iload vptr
  CS_RETURN       = 0x0800,   // return, for MSF
  CS_CONTAINER_UD = 0x1000,   // container U-D
};

enum RBC_RET_TYPE {
  UND_T          = 0,
  BOOL_T         = 1,
  CODEREP_PTR    = 2,
  LIST_PTR       = 3,
  MAP_PTR        = 4,
  CHAR_T         = 5,
};

enum COLL_POSITION {
  COLL_POS_FRONT,
  COLL_POS_BACK,
  COLL_POS_IDX,
};

typedef std::pair<RBC_RET_TYPE, INT64> EVAL_RET;
#define EVAL_RET_INVALID std::make_pair(UND_T, 0LL)
#define TRAV_DIR_BIT 2
#define MAX_ARG_BIT  (32 - TRAV_DIR_BIT)
typedef union {
  UINT64 key;                      // combined key
  struct {
    IDTYPE rna_idx;                // Rna index
    UINT32 td     : TRAV_DIR_BIT;  // traverse direction
    UINT32 arg_idx: MAX_ARG_BIT;   // traverse arg/parameter index
  } fld;
} RNA_VISIT_KEY;

class JNI_CHECKER_HELPER;
// ====================================================================
// TRAV_FRAME
//  Frame information for traversal
// ====================================================================
class TRAV_FRAME {
private:
  IDTYPE_SET     _visited;     // BB (id) visited in this frame
  UINT64_SET     _rna_visited; // rna visited with direction
  RNA_NODE      *_rna;         // rna reaches to this frame
  COMP_UNIT     *_cu;          // COMP_UNIT to this frame
  BB_NODE       *_bb;          // start BB in this frame
  UINT64         _key;         // rna visited key
  BOOL           _recursion;   // visited back edge rna

  TRAV_FRAME(const TRAV_FRAME&);            // disable copy constructor
  TRAV_FRAME& operator=(const TRAV_FRAME&); // disable assign operator

public:
  // TRAV_FRAME
  // constructor
  TRAV_FRAME(RNA_NODE *rna, COMP_UNIT *cu, UINT64 key, MEM_POOL *pool)
   : _visited(47, IDTYPE_HASHER(), IDTYPE_EQUAL(), IDTYPE_ALLOCATOR(pool)),
     _rna_visited(13, UINT64_HASHER(), UINT64_EQUAL(), UINT64_ALLOCATOR(pool)),
     _rna(rna), _cu(cu), _bb(NULL), _key(key), _recursion(FALSE) { }

  // Visited
  // check if the BB is visited
  //  return TRUE if BB is already visited
  //  return FALSE if BB is not visited and BB is marked to be visited
  BOOL Visited(IDTYPE id)
  {
    if (_visited.find(id) == _visited.end()) {
      _visited.insert(id);
      return FALSE;
    }
    else {
      return TRUE;
    }
  }

  void Set_recursion(BOOL v)        { _recursion = v; }
  BOOL Recursion()                  { return _recursion; }

  UINT64_SET *Rna_visited()         { return &_rna_visited; }
  void Set_visited(UINT64 key)      { _rna_visited.insert(key); }

  // Rna
  // return rna node
  RNA_NODE *Rna() const { return _rna; }

  // Comp_unit
  // return cu
  COMP_UNIT *Comp_unit() const { return _cu; }

  // BB_NODE
  // Set and get start BB in this frame
  void Set_bb(BB_NODE* bb) { _bb = bb;   }
  BB_NODE* Bb() const      { return _bb; }

  // Direction
  // return trav direction
  UINT64 Visit_key() const {return _key; }
};

// ====================================================================
// VPTR_TRACKER
// VPTR infor for virtual call visited during the travesal. For example:
//  class A {};
//  class B : public A {};
//  class C : public A {};
//  X::foo(A* p) {
//    p->f1();  // can be B::f1(), or C::f1()
//    p->f2();  // can be B::f2(), or C::f2()
//  }
// The combination if B::f1()/C::f2() or C::f1()/B::f2() is invalid
// ====================================================================
class VPTR_TRACKER {
public:
  struct VPTR_INFO {
  private:
    CODEREP    *_this_cr;
    const char *_cls_name;

  public:
    VPTR_INFO(CODEREP *cr, const char *cls_name)
      : _this_cr(cr), _cls_name(cls_name) { }

    CODEREP    *Cr() const       { return _this_cr;  }
    const char *Cls_name() const { return _cls_name; }
    void        Set_cls_name(const char *name) {
      _cls_name = name;
    }

    void        Dump(FILE *fp) const;
  };
  typedef std::vector<VPTR_INFO> INFO_ARRAY;

  struct VPTR_FRAME {
  private:
    DNA_NODE   *_dna;
    INFO_ARRAY  _vptr_array;

  private:
    const INFO_ARRAY &Array() const { return _vptr_array; }

  public:
    VPTR_FRAME(DNA_NODE *dna, CODEREP *cr, const char *cls_name)
      : _dna(dna) {
      if (cr != NULL)
        _vptr_array.push_back(VPTR_INFO(cr, cls_name));
    }

    void Update(CODEREP *cr, const char *cls_name) {
      INFO_ARRAY::iterator end = _vptr_array.end();
      for (INFO_ARRAY::iterator it = _vptr_array.begin();
           it != end; ++it) {
        if (it->Cr() == cr) {
          it->Set_cls_name(cls_name);
          return;
        }
      }
      // not found, add it here
      _vptr_array.push_back(VPTR_INFO(cr, cls_name));
    }

    const char *Find_class(CODEREP *cr) const {
      INFO_ARRAY::const_iterator end = _vptr_array.end();
      for (INFO_ARRAY::const_iterator it = _vptr_array.begin();
           it != end; ++it) {
        if (it->Cr() == cr)
          return it->Cls_name();
      }
      return NULL;
    }

    DNA_NODE *Dna() const { return _dna; }

    void Dump(FILE *fp) const;
  };
  typedef std::deque<VPTR_FRAME> VPTR_STACK;

private:
  VPTR_STACK       _vptr_stack;
  CLASS_HIERARCHY *_ch;
  BOOL             _tracing;

  const VPTR_STACK &Stack() const { return _vptr_stack;        }
  const VPTR_FRAME &Top() const   { return _vptr_stack.back(); }

public:
  VPTR_TRACKER(CLASS_HIERARCHY *ch, BOOL trc)
    : _ch(ch), _tracing(trc) { }

  BOOL Can_function_taken(CODEREP *cr, const char *cr_cls, const char *cls, const char *func);

  void Push(DNA_NODE *dna, CODEREP *cr, const char *cls_name);

  void Pop(DNA_NODE *dna);

public:
  BOOL Tracing() const       { return _tracing; }
  void Set_tracing(BOOL trc) { _tracing = trc;  }

  void Dump_top(FILE *fp) const;
  void Dump(FILE *fp) const;
};

// ====================================================================
// TRAV_CONTEXT
// Context information for traversal, include
//  - root COMP_UNIT/STMTREP/CODEREP
//  - current COMP_UNIT/STMTREP/CODEREP
//  - call stack, stack of TRAV_FRAME
//  Notice:
//  Need to call de-constructor to free CXX_MEM_POOL defined in the class
//  
// ====================================================================
class TRAV_CONTEXT {
  typedef std::stack<TRAV_FRAME *> TRAV_STACK;
private:
  TRAV_STACK  _call_stack;        // all call stack
  // we should keep down stack, when we enter callee, back to caller, should check down call stack
  TRAV_STACK  _down_call_stack;   // down call stack
  TRAV_STACK  _tmp_call_stack;    // tempory call stack
  TRAV_STACK  _cs;          // call stack
  VSYM_TRACKER _tracker;    // track var/vsym U-D transition
  VPTR_TRACKER _vptrack;    // track vptr for virtual call
  CXX_MEM_POOL _spos_pool;  // pool used to store srcpos handle
  CXX_MEM_POOL _mpool;      // local temporary mem_pool
  UINT64_SET *_rna_visited; // rna visited with direction
  TRAV_FRAME *_root_frame;  // root frame
  TRAV_FRAME *_cur_frame;   // current frame
  COMP_UNIT  *_root_cu;     // root vsa where the traversal starts
  STMTREP    *_root_sr;     // root sr where the traversal starts
  CODEREP    *_root_cr;     // root cr where the traversal starts
  COMP_UNIT  *_cu;          // current vsa where the traversal is in
  BOOL        _tracing;     // tracing flag

  TRAV_CONTEXT(const TRAV_CONTEXT&);            // disable copy constructor
  TRAV_CONTEXT& operator=(const TRAV_CONTEXT&); // disable assign operator

public:
  // TRAV_CONTEXT
  // Constructor
  TRAV_CONTEXT(COMP_UNIT* cu, STMTREP* sr, CODEREP* cr, BOOL tracing = TRUE)
   : _spos_pool("CHECKER srcpos pool", FALSE),
     _mpool("CHECKER local pool", FALSE),
     _vptrack(cu->Vsa()->Ipsa()->Glob_cha(), FALSE),
     _root_cu(cu), _root_sr(sr), _root_cr(cr),
     _cu(cu), _tracing(tracing)
  {
    _rna_visited = CXX_NEW(UINT64_SET(23, UINT64_HASHER(), UINT64_EQUAL(),
                           UINT64_ALLOCATOR(Mem_pool())), Mem_pool());
    // push an initial frame for current CU
    _root_frame = CXX_NEW(TRAV_FRAME(NULL, cu, 0, Mem_pool()), Mem_pool());;
    _cur_frame = _root_frame;
  }

  void Clone_context(const TRAV_CONTEXT* ctx, BOOL cs, BOOL vsym, BOOL visited)
  {
    // clone call stack
    if (cs) {
      _down_call_stack = ctx->_down_call_stack;
      _tmp_call_stack = ctx->_tmp_call_stack;
      _cs = ctx->_cs;
      _root_frame = ctx->_root_frame;
      _cur_frame = ctx->_cur_frame;
    }

    // clone vsym stack
    if (vsym) {
      _tracker = ctx->_tracker;
      _vptrack = ctx->_vptrack;
    }

    // clone visited flags
    if (visited) {
      _rna_visited->insert(ctx->_rna_visited->begin(),
                           ctx->_rna_visited->end());
    }
  }

public:
  COMP_UNIT *Comp_unit() const { return _cu; }
  CODEREP   *Root_cr() const   { return _root_cr; }
  STMTREP   *Root_sr() const   { return _root_sr; }
  MEM_POOL  *Mem_pool()        { return _mpool(); }
  MEM_POOL  *Spos_pool()       { return _spos_pool(); }
  OPT_STAB  *Opt_stab() const  { return _cu->Opt_stab(); }
  CODEMAP   *Htable() const    { return _cu->Htable(); }
  VSA       *Vsa() const       { return _cu->Vsa(); }
  VSA       *Root_vsa() const  { return _root_cu->Vsa(); }
  DNA_NODE  *Dna() const       { return _cu->Dna(); }
  IPSA      *Ipsa() const      { return Vsa()->Ipsa(); }
  BOOL       Tracing() const   { return _tracing; }
  VSYM_TRACKER *Tracker()      { return &_tracker; }
  VPTR_TRACKER *Vptrack()      { return &_vptrack; }

  void       Set_context(COMP_UNIT* cu) { _cu = cu; }
  UINT32     Frame_depth()              { return _call_stack.size(); }

  void       Set_Tracing(BOOL v) { _tracing = v; 
                                   _vptrack.Set_tracing(v);
                                   _tracker.Set_tracing(v);
                                 }
public:
  // will enter to callee, push current dna to down stack
  TRAV_FRAME* Push_callee_frame(RNA_NODE *rna, COMP_UNIT* cu, UINT64 key)
  {
    Is_Trace(_tracing, (TFile, "Push callee frame, caller %s -> callee %s\n", _cu->Dna()->Fname(), cu->Dna()->Fname()));
    TRAV_FRAME* frame = CXX_NEW(TRAV_FRAME(rna, cu, key, Mem_pool()), Mem_pool());
    _down_call_stack.push(frame);
    _call_stack.push(frame);
    if(_cur_frame->Recursion() || (rna && rna->Is_back_edge())) {
      frame->Set_recursion(TRUE);
    }
    _cur_frame = frame;
    _cu = cu;
    return frame;
  }

  // return from callee
  TRAV_FRAME* Pop_callee_frame()
  {
    Is_True(!_down_call_stack.empty(), ("Down call stack is empty."));
    Is_True(!_call_stack.empty(), ("Call stack is empty."));
    TRAV_FRAME *top = _call_stack.top();
    Reset_visited(top);
    _down_call_stack.pop();
    _call_stack.pop();
    if (!_call_stack.empty()) {
      _cur_frame = _call_stack.top();
    } else {
      _cur_frame = _root_frame;
    }
    _cu = _cur_frame->Comp_unit();
    Is_Trace(_tracing, (TFile, "Pop callee frame, caller %s -> callee %s\n", _cu->Dna()->Fname(), top->Comp_unit()->Dna()->Fname()));
    return top;
  }

  // will enter to caller, if we have dna in down stack, move the top node of down stack to tmporary stack
  // will recover down stack in pop procedure
  TRAV_FRAME* Push_caller_frame(RNA_NODE *rna, COMP_UNIT* cu, UINT64 key)
  {
    Is_Trace(_tracing, (TFile, "Push caller frame, callee %s -> caller %s\n", _cu->Dna()->Fname(), cu->Dna()->Fname()));
    TRAV_FRAME *frame = CXX_NEW(TRAV_FRAME(rna, cu, key, Mem_pool()), Mem_pool());
    _call_stack.push(frame);
    // rna is back edge or current frame is in recursion
    // mark new frame in recursion too
    if(_cur_frame->Recursion() || (rna && rna->Is_back_edge())) {
      frame->Set_recursion(TRUE);
    }
    _cur_frame = frame;
    _cu = cu;
    if (!_down_call_stack.empty()) {
      _tmp_call_stack.push(_down_call_stack.top());
      _down_call_stack.pop();
    }
    return frame;
  }

  // recover down stack, if temporary stack is not empty
  TRAV_FRAME* Pop_caller_frame()
  {
    Is_True(!_call_stack.empty(), ("Up call stack is empty."));
    TRAV_FRAME *top = _call_stack.top();
    Reset_visited(top);
    _call_stack.pop();
    if (!_tmp_call_stack.empty()) {
      _down_call_stack.push(_tmp_call_stack.top());
      _tmp_call_stack.pop();
    }
    if (!_call_stack.empty()) {
      _cur_frame = _call_stack.top();
    } else {
      _cur_frame = _root_frame;
    }
    _cu = _cur_frame->Comp_unit();
    Is_Trace(_tracing, (TFile, "Pop caller frame, callee %s -> caller %s\n", _cu->Dna()->Fname(), top->Comp_unit()->Dna()->Fname()));
    return top;
  }

  TRAV_FRAME* Top_down_frame()
  {
    if (!_down_call_stack.empty()) {
      return _down_call_stack.top();
    }
    return NULL;
  }

  // Frame_empty
  // check if the stack is really empty. the initial frame is excluded
  BOOL Frame_empty()
  {
    return _cur_frame == _root_frame;
  }

  // Visited
  // check if BB is visited in current frame
  BOOL Visited(BB_NODE* bb)
  {
    return _cur_frame->Visited(bb->Id());
  }

  // reset rna visited
  // if force is true, reset visited
  // if check_aggr is on and frame not in recursion, reset visited
  // do not reset visited if the frame is marked recursion to avoid infinite loop
  void Reset_visited(UINT64 visited_key, BOOL force = FALSE) {
    if(force || (VSA_Checker_Aggr && !_cur_frame->Recursion())) {
      _rna_visited->erase(visited_key);
    }
  }

  // clear rna visited in the frame, called in pop frame
  void Reset_visited(TRAV_FRAME *frame) {
    if(VSA_Checker_Aggr && !frame->Recursion()) {
      UINT64_SET *frame_visited = frame->Rna_visited();
      for (UINT64_SET::iterator iter = frame_visited->begin();
          iter != frame_visited->end(); iter++) {
        _rna_visited->erase(*iter);
      }
    }
  }

  UINT64 Rna_visited_key(RNA_NODE *rna, TRAV_DIRECTION dir, IDTYPE arg_idx = 0)
  {
    Is_True(arg_idx <= ((UINT32)1<<MAX_ARG_BIT) -1, ("arg_idx outof key range"));
    RNA_VISIT_KEY visited_key;
    visited_key.fld.rna_idx = rna ? rna->Rna_idx() : 0;
    visited_key.fld.td = dir;
    visited_key.fld.arg_idx = arg_idx;
    return visited_key.key;
  }

  // Visited
  // check if the rna is already visited
  //  - return TRUE if rna is already visited
  //  - return FALSE if rna is not visited and rna is marked to be visited
  // temporary use
  BOOL Visited(RNA_NODE *rna, UINT64 key)
  {
    if(_rna_visited->find(key) == _rna_visited->end()) {
      _rna_visited->insert(key);
      if(_cur_frame) {
        _cur_frame->Set_visited(key);
        if(rna->Is_back_edge()) {
          _cur_frame->Set_recursion(TRUE);
        }
      }
      return FALSE;
    }
    Is_Trace(_tracing, (TFile, "rna %s visited\n", Td_name(Rna_visited_dir(key))));
    return TRUE;
  }

  TRAV_DIRECTION Rna_visited_dir(UINT64 k) {
    RNA_VISIT_KEY visited_key;
    visited_key.key = k;
    return (TRAV_DIRECTION)(visited_key.fld.td);
  }

  const char *Td_name(TRAV_DIRECTION dir) {
    switch (dir) {
      case TD_NONE:
        return "None";
      case TD_UP:
        return "Up";
      case TD_DOWN:
        return "Down";
      default:
        return "Invalid";
    }
  }

  // Recursive iterate through call stack to check if a frame with cu/rna/dir visited
  BOOL Visited_frame(TRAV_STACK *stack, COMP_UNIT *cu, UINT64 visit_key)
  {
    BOOL ret = FALSE;
    if(!stack || stack->empty()) {
      return FALSE;
    }
    TRAV_FRAME *frame = stack->top();
    if(frame && frame->Comp_unit() == cu && frame->Visit_key() == visit_key) {
      return TRUE;
    } else {
      stack->pop();
      ret = Visited_frame(stack, cu, visit_key);
      stack->push(frame);
    }
    return ret;
  }

  // Visited
  // check if given cu/rna with direction is already visited
  //  - return TRUE if already visited
  //  - return FALSE if not visited
  // avoid loop in check_global_init
  BOOL Visited(COMP_UNIT *cu, RNA_NODE *rna, TRAV_DIRECTION dir, IDTYPE arg_idx)
  {
    return Visited_frame(&_call_stack, cu, Rna_visited_key(rna, dir, arg_idx));
  }

public:
  // Is_dereference
  // check if the CR is do dereference. this is for CS_DEREFERENCE
  static BOOL Is_dereference(CODEREP* cr)
  {
    return (cr->Kind() == CK_IVAR && cr->Opr() != OPR_PARM) ? TRUE
                                                            : FALSE;
  }

  // Is_divisor
  // check if the CR is do division. this is for CS_DIVISOR
  static BOOL Is_divisor(CODEREP* cr)
  {
    return (cr->Kind() == CK_OP &&
            (cr->Opr() == OPR_DIV || cr->Opr() == OPR_DIVREM ||
             cr->Opr() == OPR_MOD || cr->Opr() == OPR_DIV)) ? TRUE
                                                            : FALSE;
  }

  // Is_call
  // check if the CR is call
  static BOOL
  Is_call(CODEREP* cr) { return cr->Kind() == CK_OP && cr->Opr() == OPR_CALL; }
  // check if the SR is call
  static BOOL
  Is_call(STMTREP* sr) { return sr->Opr() == OPR_CALL; }

  // Is_icall
  // check if the CR is icall
  static BOOL
  Is_icall(CODEREP* cr) { return cr->Kind() == CK_OP && cr->Opr() == OPR_ICALL; }
  // check if the SR is icall
  static BOOL
  Is_icall(STMTREP* sr) { return sr->Opr() == OPR_ICALL; }

  // check if cr is vptr
  static BOOL
  Is_vptr(CODEREP *cr, STMTREP *sr)
  {
    if(cr->Kind() != CK_IVAR) {
      return FALSE;
    }
    CODEREP *rhs = sr->Rhs();
    if(sr->Opr() == OPR_ICALL) {
      // Get the function ptr from icall's last param:
      // For java virtual call, get func_ptr from intrinsic op LOOKUP_VIRT_FUNC
      CODEREP *func_ptr = rhs->Opnd(rhs->Kid_count()-1);
      if (PU_java_lang(Get_Current_PU()) &&
          Is_valid_lookup_virt_op(func_ptr)) {
        func_ptr = Get_lookup_virt_original_target_info(func_ptr);
      }

      if (Is_stmt_virtual_call(sr) &&
          func_ptr->Kind() == CK_IVAR &&
          cr == func_ptr->Ilod_base())
        return TRUE;
    } else if(sr->Opr() == OPR_INTRINSIC_CALL) {
      // java interface call, check if cr is the base of _JV_VTable
      if(rhs->Intrinsic() == INTRN_LOOKUP_IF) {
        CODEREP *arg0 = rhs->Opnd(0)->Ilod_base();
        if(arg0->Kind() == CK_IVAR &&
           cr == arg0->Ilod_base() &&
           cr->I_field_id() == 1) {
          return TRUE;
        }
      }
    }
    return FALSE;
  }

  // check if bb in EH handler
  BOOL
  In_handler_bb(BB_NODE *bb)
  {
    CFG *cfg = Comp_unit()->Cfg();
    VRA *vra = Comp_unit()->Vra();
    if(vra == NULL) {
      return FALSE;
    }
    if (cfg->Fake_entry_bb() != NULL) {
      BB_NODE *entry_bb;
      BB_LIST_ITER bb_iter;
      FOR_ALL_ELEM (entry_bb, bb_iter, Init(cfg->Fake_entry_bb()->Succ())) {
        if (entry_bb->Kind() == BB_ENTRY && entry_bb->Labnam() > 0) {
          if (bb == entry_bb) {
            return TRUE;
          } else {
            vector<UINT32> visited;
            visited.resize(cfg->Total_bb_count());
            if (vra->Cfg_has_path(entry_bb, bb, visited, 1)) {
              return TRUE;
            }
          }
        }
      }
    }
    return FALSE;
  }

  TYPE_ID
  Get_init_mtype(const ST *st)
  {
    TY_IDX ty = ST_type(st);
    TYPE_ID mtype = TY_mtype(ty);
  #ifdef TARG_X8664
    switch (mtype) {
    case MTYPE_M8I1:
      mtype = MTYPE_I1;
      break;
    case MTYPE_M8I2:
      mtype = MTYPE_I2;
      break;
    case MTYPE_M8I4:
      mtype = MTYPE_I4;
      break;
    default:
      break;
    }
  #endif
    if (TY_kind(ty) == KIND_ARRAY) {
      mtype = TY_mtype(TY_etype(ty));
    }
    return mtype;
  }

  BOOL
  Try_st_value_zero(TYPE_ID mtype, ST *st, TCON *init_tcon)
  {
    if (ST_init_value_zero(st)) {
      if (MTYPE_is_integral(mtype)) {
        *init_tcon = Host_To_Targ(mtype, 0L);
      } else if (MTYPE_is_float(mtype)) {
        *init_tcon = Host_To_Targ_Float(mtype, 0.0);
      } else {
        // for struct or array, just pick up a int mtype
        // struct or array may set value zero
        *init_tcon = Host_To_Targ(MTYPE_I4, 0L);
      }
      return TRUE;
    }
    return FALSE;
  }

  // =============================================================================
  //
  // Given a tcon and a mtype, if mtype is legal, convert tcon to that mtype,
  // and return converted tcon
  // =============================================================================
  TCON
  Convert_tcon(TYPE_ID mtype, TCON init_tcon)
  {
    if (MTYPE_is_integral(mtype) || MTYPE_is_float(mtype)) {
      if (mtype != TCON_ty(init_tcon)) {
        init_tcon = Targ_Conv(mtype, init_tcon);
      }
    }
    Is_True(MTYPE_is_integral(TCON_ty(init_tcon)) || MTYPE_is_float(TCON_ty(init_tcon)),
      ("Init tcon mtype is not correct, mtype: %s.", MTYPE_name(TCON_ty(init_tcon))));
    return init_tcon;
  }

  // =============================================================================
  //
  // Replace CK_VAR or CK_IVAR with their constant initialized value.
  // we can only return const cr, can't return lda, st may initialized in other
  // file, we can't use that st index, must keep current file context
  // x is  CK_VAR st  or
  //       CK_IVAR CK_LDA st
  // and var_aux_id is corr opt_stab entry for the st.
  //
  // =============================================================================
  BOOL
  Initialized_const_value(CODEREP *cr, CODEREP **value_cr, pair<UINT32, ST_IDX> &symbol_st)
  {
    AUX_ID aux_id = ILLEGAL_AUX_ID;
    if (cr->Kind() == CK_VAR) {
      aux_id = cr->Aux_id();
    } else {
      Is_True(FALSE, ("VSA::Initialized_const_value: wrong tree."));
    }
    AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(aux_id);
    // ignore vector type
    if (MTYPE_is_vector(cr->Dsctyp()) ||
        (sym->St() && MTYPE_is_vector(ST_mtype(sym->St())))) {
      return FALSE;
    }
    if (sym->Is_volatile()) {
      return FALSE;
    }
    ST *st = sym->St();
    vector<INT> offset_vec;
    if (Tracker()->Empty() && !sym->Is_virtual()) {
      offset_vec.push_back(sym->St_ofst());
    }
    if (!Tracker()->Empty()) {
      // check tracker first, if all vsym fild rep have offset info
      // then we have the chance to find the final initv
      VSYM_TRACKER *tracker = Tracker();
      while (!tracker->Empty()) {
        VSYM_FLD_REP *vfr = tracker->Fld_rep();
        // can't handle this kind of vfr, no offset nor field id
        if (!vfr->Is_uniq_id()) {
          return FALSE;
        }
        offset_vec.push_back(vfr->Ofst());
        tracker->Pop();
      }
    }
    // we need to switch file context in for loop, for vsym tracker
    // maype some level of field id is initialized with symbol,
    // and that symbol is initialized in other file
    // so must guarantee the file context switched to the original one,
    // and deallocator will help us to achieve the goal
    FILE_CONTEXT_SWITCH file_context(File_Index);
    UINT32 prev_file_idx = File_Index;
    WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
    TCON init_tcon;
    BOOL have_tcon = FALSE;
    for (vector<INT>::iterator iter = offset_vec.begin(); iter != offset_vec.end(); iter++) {
      INT offset = *iter;
      Is_Trace(Tracing(), (TFile, "Trying get initv at offset %d:\n", offset));
      BOOL initialized = ST_is_initialized_cross(st);
      if (!initialized) {
        return FALSE;
      }
      // const symbol don't have inito, just get tcon val
      if (ST_class(st) == CLASS_CONST) {
        TCON tcon = ST_tcon_val(st);
        if (MTYPE_is_integral(TCON_ty(tcon)) || MTYPE_is_float(TCON_ty(tcon))) {
          init_tcon = Convert_tcon(TY_mtype(ST_type(st)), tcon);
          have_tcon = TRUE;
        }
        // st is const string, should not return this tcon as cr
        break;
      }
      pair<UINT32, pair<ST_IDX, INITV_IDX> > initv_info = ST_has_initv_cross(st);
      UINT32 file_idx = initv_info.first;
      ST_IDX st_idx = initv_info.second.first;
      INITV_IDX initv_idx = initv_info.second.second;
      // st is not initialized
      Is_True_Ret(st_idx != (ST_IDX) 0, ("ST is initialized, but can't resolve."), FALSE);
      // switch file context, should switch manually
      if (mgr && prev_file_idx != file_idx) {
        mgr->Save_be_scope_tab(prev_file_idx);
        mgr->Set_file_context(file_idx);
        File_Index = file_idx;
        prev_file_idx = file_idx;
      }
      Is_Trace(Tracing(), (TFile, "Initv for symbol %s\n", ST_name(st_idx)));
      Is_Trace_cmd(Tracing(), Print_INITVs(initv_idx));
      BOOL value_symbol = FALSE;
      ST *nst = ST_ptr(st_idx);
      TYPE_ID nst_mtype = Get_init_mtype(nst);
      if (Try_st_value_zero(nst_mtype, nst, &init_tcon)) {
        init_tcon = Convert_tcon(TY_mtype(ST_type(nst)), init_tcon);
        have_tcon = TRUE;
      } else {
        // st is initialized
        Is_True_Ret(initv_idx != (INITV_IDX) 0, ("ST is initialized, and is not zero, but can't find initv."), FALSE);
        INITV_IDX n_initv_idx;
        BOOL is_arr_elem = (TY_kind(ST_type(nst)) == KIND_ARRAY) ? TRUE : FALSE;
        BOOL found = FALSE;
        /* if (offset == 0 && sym->Is_virtual()) {
          n_initv_idx = initv_idx;
          found = TRUE;
        } else */ {
          found = Get_initv_from_offset(file_idx, initv_idx, offset, &n_initv_idx);
        }
        // can't find initv field
        if (!found) {
          return FALSE;
        }
        INITV n_initv = Initv_Table[n_initv_idx];
        switch (n_initv.kind) {
        case INITVKIND_PAD:
        case INITVKIND_ZERO:
          have_tcon = TRUE;
          // just pick one int mtype
          init_tcon = Host_To_Targ(MTYPE_I8, 0L);
          break;
        case INITVKIND_ONE:
          have_tcon = TRUE;
          // just pick one int mtype
          init_tcon = Host_To_Targ(MTYPE_I8, 1L);
          break;
        case INITVKIND_VAL:
          have_tcon = TRUE;
          init_tcon = Tcon_Table[INITV_tc(n_initv)];
          break;
        case INITVKIND_SYMOFF:
          value_symbol = TRUE;
          st = ST_ptr(INITV_st(n_initv));
          symbol_st.first = file_idx;
          symbol_st.second = ST_st_idx(st);
          break;
        default:
          break;
        }
      }

      // TODO: how to handle MTYPE_STR?
      if (TCON_ty(init_tcon) == MTYPE_STR) {
        have_tcon = FALSE;
        break;
      }

      // ATTENTION: must switch back to prev file context, then we can insert to tcon table
      if (have_tcon) {
        init_tcon = Convert_tcon(cr->Dtyp(), init_tcon);
        break;
      } else if (!value_symbol) {
        // initialized with other kind of tcon, don't need to handle them now
        break;
      }
      // offset reference to a symbol, go on checking this symbol
    }
    if (have_tcon) {
      TCON_IDX tcon_idx = Enter_tcon(init_tcon);
      *value_cr = Htable()->Add_tcon(tcon_idx);
    }
    return TRUE;
  }
};

// ====================================================================
// SPOS_HELPER
// Helper function to track spos information. Nothing to do by default
// ====================================================================
template<typename _CHECKER, BOOL _SPOS>
class SPOS_HELPER {
private:
  _CHECKER& _checker;    // checker who really owns the SRCPOS_HANDLE
  INT       _child_cnt;  // accumulated children cnt

public:
  // Constructor
  // SPOS_HELPER
  SPOS_HELPER(_CHECKER& checker) : _checker(_checker), _child_cnt(0) { }

  // Append_data
  // Append sr's srcpos to sp_h. Do nothing
  void
  Append_data(STMTREP* sr, DNA_NODE* dna, PATH_INFO pi) { }

  // Append_data
  // Append sr's srcpos and cr's stpath to sp_h. Do nothing
  void
  Append_data(STMTREP* sr, CODEREP* cr, DNA_NODE* dna, PATH_INFO pi) { }

  // Append_data
  // Append bb's srcpos to sp_h. Do nothing
  void
  Append_data(BB_NODE* bb, DNA_NODE* dna, PATH_INFO pi) { }

  void
  Append_data(ST* st, BB_NODE* bb, DNA_NODE* dna, PATH_INFO pi) { }

  // Append_stpath
  // Append cr's stpath to sp_b. Do nothing
  void
  Append_stpath(STMTREP* sr, CODEREP* cr, DNA_NODE* dna, BOOL stname) { }

  // Add_children
  // Add children to current node and return current node. Do nothing
  SRCPOS_TREENODE*
  Add_children(INT cnt) { _child_cnt += cnt; return NULL; }

  // Children count
  UINT32
  Children_count() { return _child_cnt; }

  // Set_cur_node
  // Set current node to ith child of node. Do nothing
  void
  Set_cur_node(SRCPOS_TREENODE* node, INT idx) { }

  // Reset_cur_node
  // Reset current node with idx
  void
  Reset_cur_node(SRCPOS_TREENODE* node, INT idx) { }

  // Set_flag
  // Set flag to current node
  void
  Set_flag(SRCPOS_FLAG flag) { }

  // Get_cur_node
  SRCPOS_TREENODE*
  Cur_node() { return NULL; }

  // Get current idx
  IDTYPE
  Cur_idx() { return 0; }

  // Push_mark
  // Push a mark to path
  template<typename _T> void
  Push_mark(_T* mark) { }

  // Pop_mark
  // Pop a mark from path
  template<typename _T> void
  Pop_mark(_T* mark, BOOL pop_mark) { }

  // Add_bb
  // Add a bb to path
  void
  Add_bb(BB_NODE* bb) { }

  // Push_value_graph
  // Push a mark to value graph
  INT
  Push_value_graph() { return 0; }

  // Pop_value_graph
  // Pop from value graph till previous mark
  void
  Pop_value_graph(INT level) { }

  // Add_assign
  // Add an assignment to value graph
  template<typename _T> BOOL
  Add_assign(VSA* lvsa, _T *lhs, VSA *rvsa, _T *rhs) { return TRUE; }

  // Add_control_dependency
  // Add control dependency to value graph
  BOOL
  Add_control_dependency(DNA_NODE *dna, BB_NODE *pred, BB_NODE *succ) { return TRUE; }

  BOOL
  Add_phi_opnd(DNA_NODE *dna, PHI_NODE *phi, INT32 idx) { return TRUE; }

  BOOL
  Is_def_reachable(BB_NODE *use_bb, STMTREP *def_sr) { return TRUE; }

  // Is_path_possible
  // Check if the path is possible consider the value range on conditions
  BOOL
  Is_path_possible(PHI_NODE* phi, INT opnd, SRCPOS_TREENODE *cur_node) { return TRUE; }

  BOOL
  Check_skip() 
  {  
    if(VSA_Checker_Max_Srcpos_Child >=0 &&
       Children_count() > VSA_Checker_Max_Srcpos_Child) {
      return TRUE;
    }
    return FALSE; 
  }
};

// ====================================================================
// SPOS_HELPER<_CHECKER, TRUE>
// Specialization of SPOS_HELPER to track spos information
// ====================================================================
template<typename _CHECKER>
class SPOS_HELPER<_CHECKER, TRUE> {
private:
  _CHECKER& _checker;    // checker who really owns the SRCPOS_HANDLE

public:
  // Constructor
  // SPOS_HELPER
  SPOS_HELPER(_CHECKER& checker) : _checker(checker) { }

  // Append_data
  // Append sr's srcpos to sp_h
  void
  Append_data(STMTREP* sr, DNA_NODE* dna, PATH_INFO pi)
  {
    _checker.Append_data(sr, dna, pi);
  }

  // Append_data
  // Append sr's srcpos and cr's stpath to sp_h
  void
  Append_data(STMTREP* sr, CODEREP* cr, DNA_NODE* dna, PATH_INFO pi)
  {
    _checker.Append_data(sr, cr, dna, pi);
  }

  // Append_data
  // Append bb's srcpos to sp_h
  void
  Append_data(BB_NODE* bb, DNA_NODE* dna, PATH_INFO pi)
  {
    _checker.Append_data(bb, dna, pi);
  }

  void
  Append_data(ST* st, BB_NODE* bb, DNA_NODE* dna, PATH_INFO pi)
  {
    _checker.Append_data(st, bb, dna, pi);
  }

  // Append_stpath
  // Append cr's stpath to sp_b
  void
  Append_stpath(STMTREP* sr, CODEREP* cr, DNA_NODE* dna, BOOL stname)
  {
    _checker.Append_stpath(sr, cr, dna, stname);
  }

  // Add_children
  // Add children to current node and return current node
  SRCPOS_TREENODE*
  Add_children(INT cnt)
  {
    return _checker.Add_children(cnt);
  }

  // Children count
  UINT32
  Children_count() { return _checker.Children_count(); }

  // Set_cur_node
  // Set current node to ith child of node
  void
  Set_cur_node(SRCPOS_TREENODE* node, INT idx)
  {
    _checker.Set_cur_node(node, idx);
  }

  // Reset_cur_node
  // Reset current node with idx
  void
  Reset_cur_node(SRCPOS_TREENODE* node, INT idx)
  {
    _checker.Reset_cur_node(node, idx);
  }

  // Set_flag
  // Set flag to current node
  void
  Set_flag(SRCPOS_FLAG flag)
  {
    _checker.Set_flag(flag);
  }

  // Get_cur_node
  SRCPOS_TREENODE *
  Cur_node()
  {
    return _checker.Cur_node();
  }

  // Get_cur_idx
  IDTYPE
  Cur_idx()
  {
    return _checker.Cur_idx();
  }

  // Push_mark
  // Push a mark to path
  template<typename _T> void
  Push_mark(_T* mark)
  {
    _checker.Push_mark(mark);
  }

  // Pop_mark
  // Pop a mark from path
  template<typename _T> void
  Pop_mark(_T* mark, BOOL pop_mark)
  {
    _checker.Pop_mark(mark, pop_mark);
  }

  // Add_bb
  // Add a bb to path
  void
  Add_bb(BB_NODE* bb)
  {
    _checker.Add_bb(bb);
  }

  // Push_value_graph
  // Push a mark to value graph
  INT
  Push_value_graph()
  {
    return _checker.Push_value_graph();
  }

  // Pop_value_graph
  // Pop from value graph till previous mark
  void
  Pop_value_graph(INT level)
  {
    _checker.Pop_value_graph(level);
  }

  // Add_assign
  // Add an assignment to value graph
  template<typename _T> BOOL
  Add_assign(VSA* lvsa, _T *lhs, VSA *rvsa, _T *rhs)
  {
    return _checker.Add_assign(lvsa, lhs, rvsa, rhs);
  }

  // Add_control_dependency
  // Add control dependency to value graph
  BOOL
  Add_control_dependency(DNA_NODE *dna, BB_NODE *pred, BB_NODE *succ) {
    return _checker.Add_control_dependency(dna, pred, succ);
  }

  BOOL
  Add_phi_opnd(DNA_NODE *dna, PHI_NODE *phi, INT32 idx) {
    return _checker.Add_phi_opnd(dna, phi, idx);
  }

  BOOL
  Is_def_reachable(BB_NODE *use_bb, STMTREP *def_sr)
  {
    return _checker.Is_def_reachable(use_bb, def_sr);
  }

  // Is_path_possible
  // Check if the path is possible condiering the value range on conditions
  BOOL
  Is_path_possible(PHI_NODE* phi, INT opnd, SRCPOS_TREENODE *cur_node)
  {
    return _checker.Is_path_possible(phi, opnd, cur_node);
  }

  BOOL
  Check_skip()
  {
    return _checker.Check_skip();
  }
};

// ====================================================================
// CHECKER_LIST
// combine two checkers into a list and do two checks in one scan
// ====================================================================
template <typename _FIRST, typename _SECOND>
class CHECKER_LIST {
private:
  _FIRST  _first;
  _SECOND _second;

  CHECKER_LIST(const CHECKER_LIST&);             // disable copy constructor
  CHECKER_LIST& operator=(const CHECKER_LIST&);  // disable assign operator

public:
  enum { SUSPECT = _FIRST::SUSPECT | _SECOND::SUSPECT };
  enum { ENTITY = _FIRST::ENTITY | _SECOND::ENTITY };
  enum { USE_SRCPOS = _FIRST::USE_SRCPOS | _SECOND::USE_SRCPOS };

public:
  // CHECKER_LIST
  // constructor
  CHECKER_LIST(TRAV_CONTEXT* ctx) : _first(ctx), _second(ctx) { }

public:
  // Check_coderep
  // invoke first and second's Check_coderep respectively
  template<CODEKIND _KIND> CHECKER_STATUS
  Check_coderep(CODEREP* cr, STMTREP* sr, TRAV_CONTEXT* ctx)
  {
    if (_FIRST::ENTITY & TE_CODEREP)
      _first.Check_coderep<_KIND>(cr, sr, ctx);
    if (_SECOND::ENTITY & TE_CODEREP)
      _second.Check_coderep<_KIND>(cr, sr, ctx);
    return CS_DONE;
  }

  // Check_stmtrep
  // invoke first and second's Check_stmtrep respectively
  CHECKER_STATUS
  Check_stmtrep(STMTREP* sr, TRAV_CONTEXT* ctx)
  {
    if (_FIRST::ENTITY & TE_STMTREP)
      _first.Check_stmtrep(sr, ctx);
    if (_SECOND::ENTITY & TE_STMTREP)
      _second.Check_stmtrep(sr, ctx);
    return CS_DONE;
  }
};

// ====================================================================
// UNI_PARAMETER_HELPER
// helper class to check the vsym tracker and find the actual coderep
// and its VOR at callsite from formal param index
// ====================================================================
class UNI_PARAMETER_HELPER {
private:
  TRAV_CONTEXT &_ctx;      // context
  VSYM_TRACKER *_tracker;  // tracker
  IDTYPE _parm_idx;        // formal parameter index

  UNI_PARAMETER_HELPER(const UNI_PARAMETER_HELPER&);             // disable copy ctor
  UNI_PARAMETER_HELPER& operator=(const UNI_PARAMETER_HELPER&);  // disable assignment

public:
  UNI_PARAMETER_HELPER(TRAV_CONTEXT& ctx, IDTYPE parm_idx)
    : _ctx(ctx), _tracker(ctx.Tracker()), _parm_idx(parm_idx) { }


  IDTYPE Visited_arg_idx() const { return _parm_idx; }
  // Check_obj
  // Update CHECK_OBJ with actual coderep/vor with given rna and parameter index
  // Return TRUE if success
  BOOL Check_obj(DNA_NODE* caller, RNA_NODE* rna, CHECK_OBJ &caller_obj, BOOL &maybe)
  {
    Is_True(rna != NULL, ("bad call context"));
    Is_True(rna->Has_callee(_ctx.Dna()->Dna_idx()), ("bad callee index"));
    if (_parm_idx > rna->Arg_cnt()) {
      Is_Trace(_ctx.Tracing(), (TFile, "    parm_idx %d out of bound %ld\n",
                                       _parm_idx, rna->Arg_list()->size()));
      return FALSE;
    }
    Is_True(_parm_idx < rna->Arg_list()->size(), ("bad parm index"));
    CODEREP* cr = rna->Get_arg(_parm_idx);
    Is_True(cr != NULL, ("bad argument"));
    if (cr == NULL)
      return FALSE;

    STMTREP *stmt = rna->Callstmt();
    if (!_tracker->Empty()) {
      Is_Trace(_ctx.Tracing(), (TFile, "Match vsym tracker for parameter cr%d:\n",
                                cr->Coderep_id()));
      VSA *vsa = caller->Comp_unit()->Vsa();
      HEAP_OBJ_REP* hor = vsa->Cr_2_heap_obj(cr);
      VSYM_OBJ_REP* vor = _tracker->Compress(vsa, hor, stmt, cr, TRUE, maybe);
      if (vor) {
        caller_obj.Update_vsym(vor, stmt, cr);
        return TRUE;
      } else if (!VSA_Checker_Vfr_Exact_Match) {
        // LDA - no vor attached to stmt mu, match fld id 0
        if (cr->Kind() == CK_LDA && !_tracker->Empty()) {
          VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
          MU_NODE *mu = vsa->Find_stmt_var_mu(stmt, cr->Lda_base_st(), &zero_fld);
          if (mu) {
            cr = mu->OPND();
            Is_Trace(_ctx.Tracing(), (TFile, "  LDA matched mu cr%d\n",
                                      cr->Coderep_id()));
          } else {
            Is_Trace(_ctx.Tracing(), (TFile, "  LDA no mu matched, continue with param cr%d\n",
                                      cr->Coderep_id()));
          }
        } else {
          Is_Trace(_ctx.Tracing(), (TFile, "  No vsym matched, switch back to var UD\n"));
        }
      } else {
        Is_Trace(_ctx.Tracing(), (TFile, "  No vsym exact match\n"));
        return FALSE;
      }
    }
    // for var UD or no vsym matched, switch back to var UD
    caller_obj.Update_var(cr, stmt);
    return TRUE;
  }

  // Check_obj
  // Update CHECK_OBJ corresponding to the formal associated with given
  // callee and return statememt
  // Return TRUE if matched, Return FALSE if not matched
  BOOL Check_obj(DNA_NODE *callee, STMTREP *sr, CHECK_OBJ &callee_obj, BOOL &maybe)
  {
    Is_True(sr->Opr() == OPR_RETURN, ("bad return stmt"));
    if (_parm_idx >= callee->Parm_list()->size()) {
      Is_Trace(_ctx.Tracing(), (TFile, "    parm_idx %d out of bound %ld\n",
                                       _parm_idx, callee->Parm_list()->size()));
      return FALSE;
    }
    Is_True(_parm_idx < callee->Parm_list()->size(), ("bad arg index"));
    CHECKER_STATUS state = _tracker->Empty() ? CS_VAR_UD : CS_VSYM_UD;
    CODEREP* parm = (*callee->Parm_list())[_parm_idx]->Cr();
    // parm can be null if it's never used in this function
    if (parm == NULL) {
      return FALSE;
    }
    if (!_tracker->Empty()) {
      // for vsym UD
      VSA* vsa = callee->Comp_unit()->Vsa();
      HEAP_OBJ_REP* hor = vsa->Cr_2_heap_obj(parm);
      // Do we need to first match vor by Find_vor_mu_vor?
      // Do we need to match field id 0?
      VSYM_OBJ_REP *vor = _tracker->Compress(vsa, hor, sr, parm, TRUE, maybe);
      if (vor) {
        callee_obj.Update_vsym(vor, sr, parm);
        return TRUE;
      }
      if (VSA_Checker_Vfr_Exact_Match) {
        Is_Trace(_ctx.Tracing(), (TFile, "  No vsym exact match\n"));
        return FALSE;
      }
    }
    // for var UD or no vor matched
    callee_obj.Update_var(parm, sr);
    return TRUE;
  }

}; // UNI_PARAMETER_HELPER

// ====================================================================
// UNI_GLOBAL_VAR_HELPER
// Helper class to check the vsym checker and find the coderep and its
// VOR for global variables
// ====================================================================
class UNI_GLOBAL_VAR_HELPER {
private:
  TRAV_CONTEXT  &_ctx;      // context
  VSYM_TRACKER  *_tracker;  // tracker
  ST_IDX         _st_idx;   // global variable
  UINT32         _file_idx; // file index where the st is defined
  ST_IDX         _res_st;   // resolved st_idx if it's extern
  UINT32         _res_file; // resolved file_idx if it's extern

  UNI_GLOBAL_VAR_HELPER(const UNI_GLOBAL_VAR_HELPER&);            // disable copy ctor
  UNI_GLOBAL_VAR_HELPER& operator=(const UNI_GLOBAL_VAR_HELPER&); // disable assignment

  // Resolve
  // Resolve extern symbol
  void Resolve()
  {
    if (_res_st == 0) {
      Is_True(ST_sclass(St_ptr(_file_idx, _st_idx)) == SCLASS_EXTERN, ("not extern var"));
      WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
      Is_True(mgr != NULL, ("not in xfa mode"));
      mgr->Resolve(_file_idx, _st_idx, _res_file, _res_st);
    }
  }

  // Get_file_st
  // Get definition file and st
  FILE_ST_IDX Get_file_st(UINT32 file_idx)
  {
    if (file_idx == _file_idx) {
      return File_st_idx(_file_idx, _st_idx);
    }
    else {
      Resolve();
      return File_st_idx(_res_file, _res_st);
    }
  }

public:
  // UNI_GLOBAL_VAR_HELPER
  // Constructor
  UNI_GLOBAL_VAR_HELPER(TRAV_CONTEXT& ctx, ST_IDX st_idx)
    : _ctx(ctx), _tracker(ctx.Tracker()), _st_idx(st_idx)
  {
    _file_idx = _ctx.Dna()->File_idx();
    if (ST_sclass(st_idx) != SCLASS_EXTERN) {
      _res_st = _st_idx;
      _res_file = _file_idx;
    }
    else {
      _res_st = 0;
      _res_file = 0;
    }
  }

  IDTYPE Visited_arg_idx() const { return 0; }

  DNA_NODE* Get_global_clinit_dna()
  {
    FILE_ST_IDX fst = Get_file_st(_file_idx);
    if(FST_st_idx(fst) != 0) {
      return _ctx.Ipsa()->Get_global_clinit_dna(FST_file_idx(fst), FST_st_idx(fst));
    } else {
      return NULL;
    }
  }

  // Check_obj
  // Update CHECK_OBJ with actual coderep or vor with given rna and st_idx
  // return TRUE if success
  // called during callee->caller traversal
  BOOL Check_obj(DNA_NODE* caller, RNA_NODE* rna, CHECK_OBJ &caller_obj, BOOL &maybe)
  {
    Is_True(rna != NULL, ("bad call context"));
    Is_True(rna->Has_callee(_ctx.Dna()->Dna_idx()), ("bad callee index"));
    CHECKER_STATUS sts = _tracker->Empty() ? CS_VAR_UD : CS_VSYM_UD;

    VSA *vsa = caller->Comp_unit()->Vsa();
    STMTREP* stmt = rna->Callstmt();
    Is_True(stmt != NULL && OPERATOR_is_call(stmt->Opr()), ("bad call stmt"));
    FILE_ST_IDX fst = Get_file_st(caller->File_idx());
    VSYM_FLD_REP* vfr = _tracker->Empty() ? NULL : _tracker->Fld_rep();
    if (!_tracker->Empty()) {
      // for vsym UD, first check vor chi, if not found check vor mu
      CHI_NODE* vor_chi = vsa->Find_vor_chi(stmt,
                                            FST_file_idx(fst),
                                            FST_st_idx(fst),
                                            vfr);
      if (vor_chi) {
        CVOR* cvor = (CVOR*)vor_chi->OPND();
        VSYM_OBJ_REP *vor = cvor->first;
        Is_True(vor->Vsym_obj()->Fld_rep().Match(vfr),
                ("field id mismatch"));
        VSYM_OBJ_REP *fold_vor = _tracker->Compress(vsa, vor->Hor(), stmt, cvor->second, FALSE, maybe);
        if (!fold_vor && VSA_Checker_Vfr_Exact_Match) {
          return FALSE;
        }
        fold_vor = fold_vor ? fold_vor : vor;
        caller_obj.Update_vsym(fold_vor, stmt, cvor->second);
        return TRUE;
      } else {
        // check vor mu
        MU_NODE* vor_mu = vsa->Find_vor_mu(stmt,
                                           FST_file_idx(fst),
                                           FST_st_idx(fst),
                                           vfr);
        if (vor_mu) {
          CVOR* cvor = (CVOR*)vor_mu->OPND();
          VSYM_OBJ_REP *vor = cvor->first;
          Is_True(vor->Vsym_obj()->Fld_rep().Match(vfr),
                  ("field id mismatch"));

          VSYM_OBJ_REP *fold_vor = _tracker->Compress(vsa, vor->Hor(), stmt, cvor->second, TRUE, maybe);
          if (!fold_vor && VSA_Checker_Vfr_Exact_Match) {
            return FALSE;
          }
          fold_vor = fold_vor ? fold_vor : vor;
          caller_obj.Update_vsym(fold_vor, stmt, cvor->second);
          return TRUE;
        } else if (VSA_Checker_Vfr_Exact_Match) {
          return FALSE;
        }
      }
    }
    // for var UD or no vor mu/chi matched, switch back to var UD
    // check chi
    CHI_NODE * chi = vsa->Find_stmt_var_chi(stmt, FST_file_idx(fst), FST_st_idx(fst), vfr);
    if (chi) {
      caller_obj.Update_var(chi->OPND(), stmt);
      return TRUE;
    } else {
      // check mu
      MU_NODE *mu = vsa->Find_stmt_var_mu(stmt, FST_file_idx(fst), FST_st_idx(fst), vfr);
      if (mu) {
        caller_obj.Update_var(mu->OPND(), stmt);
        return TRUE;
      } else {
        VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
        mu = vsa->Find_stmt_var_mu(stmt, FST_file_idx(fst), FST_st_idx(fst), &zero_fld);
        if (mu) {
          caller_obj.Update_var(mu->OPND(), stmt);
          return TRUE;
        }
      }
    }
    return FALSE;
  }

  // Check_obj
  // Update callee CHECK_OBJ with given return stmt and st_idx
  // return TRUE if success
  // called during caller->callee traversal
  BOOL Check_obj(DNA_NODE* callee, STMTREP* sr, CHECK_OBJ &callee_obj, BOOL &maybe)
  {
    Is_True(sr->Opr() == OPR_RETURN, ("bad return stmt"));
    FILE_ST_IDX fst = Get_file_st(callee->File_idx());
    VSYM_FLD_REP* fld = _tracker->Empty() ? NULL : _tracker->Fld_rep();

    VSA* vsa = callee->Comp_unit()->Vsa();
    if (!_tracker->Empty()) {
      // for vsym UD, check vor mu
      MU_NODE* vor_mu = vsa->Find_vor_mu(sr,
                                         FST_file_idx(fst),
                                         FST_st_idx(fst),
                                         fld);
      if (vor_mu) {
        CVOR *cvor = (CVOR*)vor_mu->OPND();
        VSYM_OBJ_REP *fold_vor = _tracker->Compress(vsa, cvor->first->Hor(), sr, cvor->second, TRUE, maybe);
        if (!fold_vor && VSA_Checker_Vfr_Exact_Match) {
          return FALSE;
        }
        fold_vor = fold_vor ? fold_vor : cvor->first;
        callee_obj.Update_vsym(cvor->first, sr, cvor->second);
        return TRUE;
      } else if (VSA_Checker_Vfr_Exact_Match) {
        return FALSE;
      }
    }
    // for var UD, or no vor mu matched, check mu list
    MU_NODE* mu = vsa->Find_stmt_var_mu(sr,
                                        FST_file_idx(fst),
                                        FST_st_idx(fst),
                                        fld);
    if (mu) {
      callee_obj.Update_var(mu->OPND(), sr);
      return TRUE;
    }
    return FALSE;
  }
};  // UNI_GLOBAL_VAR_HELPER

// ====================================================================
// UNI_RETURN_VAR_HELPER
// Helper class to check vsym tracker and get coderep for output var
// and its associated vor at return point
// ====================================================================
class UNI_RETURN_VAR_HELPER {
private:
  TRAV_CONTEXT  &_ctx;     // context
  VSYM_TRACKER  *_tracker; // tracker
  CODEREP       *_retv_cr; // cr for return value

  UNI_RETURN_VAR_HELPER(const UNI_RETURN_VAR_HELPER&);             // disable copy ctor
  UNI_RETURN_VAR_HELPER& operator=(const UNI_RETURN_VAR_HELPER&);  // disable assignment

public:
  UNI_RETURN_VAR_HELPER(TRAV_CONTEXT& ctx, CODEREP *retv_cr)
    : _ctx(ctx), _tracker(ctx.Tracker()), _retv_cr(retv_cr) { }

  IDTYPE Visited_arg_idx() const { return 0; }
  CODEREP *Retv_cr() const       { return _retv_cr; }

  // Check_obj
  // Update callee_obj for return value from given PDV and ofst
  // return TRUE if success
  BOOL Check_obj(DNA_NODE* callee, PDV_NODE* pdv, CHECK_OBJ &callee_obj, BOOL &maybe)
  {
    CHECKER_STATUS sts = _tracker->Empty() ? CS_VAR_UD : CS_VSYM_UD;
    STMTREP* stmt = pdv->Stmt();
    if (pdv->Kind() == BY_RETURNSTMT && pdv->Oparam() == 0 &&
        stmt->Lhs()->Offset() == _retv_cr->Offset()) {
      Is_True(stmt->Opr() == OPR_STID, ("bad stmt"));
      Is_True(stmt->Next() != NULL && stmt->Next()->Opr() == OPR_RETURN,
              ("bad return stmt"));
      if (sts == CS_VAR_UD) {
        callee_obj.Update_var(stmt->Rhs(), stmt);
        return TRUE;
      }

      VSA* vsa = callee->Comp_unit()->Vsa();
      CODEREP* cr = stmt->Rhs();
      STMTREP* next = stmt->Next();
      Is_Trace(_ctx.Tracing(), (TFile,
                                 "  @UNI_RETURN_VAR_HELPER: Try matching vsym for cr%d in ret stmt:\n", cr->Coderep_id()));
      Is_Trace_cmd(_ctx.Tracing(), vsa->Print_sr(next, TFile));
      CODEREP *base = Find_ilod_base(cr);
      if (base == NULL)
        return FALSE;
      HEAP_OBJ_REP* hor = vsa->Cr_2_heap_obj(base);
      VSYM_OBJ_REP* vor = _tracker->Compress(vsa, hor, next, base, TRUE, maybe);
      if (vor) {
        callee_obj.Update_vsym(vor, stmt, cr);
        return TRUE;
      } else if (!VSA_Checker_Vfr_Exact_Match) {
        maybe = TRUE;
        callee_obj.Update_var(cr, stmt);
        return TRUE;
      } else {
        return FALSE;
      }
    }
    return FALSE;
  }
};

// ====================================================================
// UNI_OUTPUT_VAR_HELPER
// Helper class to get coderep and its VOR for output variable at
// return point
// ====================================================================
class UNI_OUTPUT_VAR_HELPER {
private:
  TRAV_CONTEXT  &_ctx;      // context
  VSYM_TRACKER  *_tracker;  // tracker
  IDTYPE         _arg_idx;  // argument index

  UNI_OUTPUT_VAR_HELPER(const UNI_OUTPUT_VAR_HELPER&);            // disable copy ctor
  UNI_OUTPUT_VAR_HELPER& operator=(const UNI_OUTPUT_VAR_HELPER&); // disable assignment

public:
  // UNI_OUTPUT_VAR_HELPER
  // Constructor
  UNI_OUTPUT_VAR_HELPER(TRAV_CONTEXT& ctx, INT32 arg_idx)
    : _ctx(ctx), _tracker(ctx.Tracker()), _arg_idx(arg_idx) { }

  IDTYPE Visited_arg_idx() const { return _arg_idx; }
  // Check_obj
  // Update callee CHECK_OBJ for output parameter from given PDV and arg index
  // return TRUE if success
  BOOL Check_obj(DNA_NODE* callee, PDV_NODE* pdv, CHECK_OBJ &callee_obj, BOOL &maybe)
  {
    STMTREP* stmt = pdv->Stmt();
    if (pdv->Kind() == BY_PARAMETER && pdv->Oparam() == _arg_idx) {
      // TODO: MSTORE, need to re-consider RETV/RETS. better to add mu on return
      if (stmt->Opr() == OPR_MSTORE)
        return FALSE;
      Is_True(stmt->Opr() == OPR_ISTORE, ("bad stmt"));
      callee_obj.Update_var(stmt->Rhs(), stmt);
      return TRUE;
    }
    return FALSE;
  }

  // Check_obj
  // Update callee CHECK_OBJ for output parameter from given return stmt and arg idx
  // return TRUE if success
  // 
  BOOL Check_obj(DNA_NODE* callee, STMTREP* stmt, CHECK_OBJ &callee_obj, BOOL &maybe)
  {
    Is_True(stmt->Opr() == OPR_RETURN, ("bad return stmt"));
    if (_arg_idx >= callee->Parm_list()->size()) {
      //Is_True(TY_is_varargs(ST_type(callee->St())), ("callee is not varargs"));
      return FALSE;
    }
    Is_True(_arg_idx < callee->Parm_list()->size(), ("bad arg index"));
    CODEREP* parm = (*callee->Parm_list())[_arg_idx]->Cr();
    // parm can be null if it's never used in this function
    if (parm == NULL)
      return FALSE;

    VSA* vsa = callee->Comp_unit()->Vsa();
    HEAP_OBJ_REP* hor = vsa->Cr_2_heap_obj(parm);
    VSYM_OBJ_REP *vor = _tracker->Compress(vsa, hor, stmt, parm, TRUE, maybe);
    if (vor) {
      callee_obj.Update_vsym(vor, stmt, parm);
      return TRUE;
    } else if (!VSA_Checker_Vfr_Exact_Match) {
      maybe = TRUE;
      callee_obj.Update_var(parm ,stmt);
      return TRUE;
    } else {
      return FALSE;
    }
    return FALSE;
  }
};


// ====================================================================
// CONTAINER_UD_HELPER
// Connect the U-D for container elements
// ====================================================================
template<typename _CHECKER>
class UD_TRAVELER;

class CONTAINER_UD_HELPER {
private:

  DEF_OBJS* Eval_container_get(RNA_NODE *rna, CHECK_OBJ &obj,
                               TRAV_CONTEXT* ctx, MEM_POOL *pool);

  template <typename _CHECKER, typename _T> void
  Check_value_objs(_CHECKER &chk, VALUE_OBJS<_T> &cands, STMTREP *sr, TRAV_CONTEXT *ctx)
  {
    SPOS_HELPER<_CHECKER, _CHECKER::USE_SRCPOS> _spos(chk);
    SRCPOS_TREENODE* cur_node = _spos.Add_children(cands.Size());
    IDTYPE p_idx = _spos.Cur_idx();
    _spos.Append_data(sr, ctx->Dna(), PATHINFO_DUMMY);
    VSYM_TRACKER::VSYM_VECTOR vsym_stack;
    ctx->Tracker()->Save_stack(&vsym_stack);
    COMP_UNIT *old_cu = ctx->Comp_unit();
    for(int i = 0; i < cands.Size(); i++) {
      _spos.Set_cur_node(cur_node, i);
      COMP_UNIT *def_cu = cands.Value_cu(i);
      DNA_NODE *def_dna = def_cu->Dna();
      STMTREP *def_sr = cands.Value_sr(i);
      CODEREP *def_cr = cands.Value_cr(i);
      CHECK_OBJ def_obj(def_cr, def_sr);
      ctx->Set_context(def_cu);
      CONTEXT_SWITCH def_ctx(def_dna);
      Is_Trace(ctx->Tracing(), (TFile, "--%s: container ud check: [%d] ",
                                chk.Checker_name(), i + 1));
      Is_Trace_cmd(ctx->Tracing(), cands.Value_obj(i)->Print(TFile));
      _spos.Append_data(def_sr, def_dna, PATHINFO_CALL_CHI);
      UD_TRAVELER<_CHECKER> chk_helper(chk, *ctx);
      chk_helper.Continue_trav(def_obj, CS_OP);
      ctx->Tracker()->Restore_stack(&vsym_stack);
    }
    ctx->Set_context(old_cu);
    _spos.Reset_cur_node(cur_node, p_idx);
  }

public:
  template<typename _CHECKER>
  CHECKER_STATUS Check_call(_CHECKER &chk, CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
  {
    STMTREP *sr = obj.Stmtrep();
    Is_True(sr && OPERATOR_is_call(sr->Opr()), ("not call stmt"));
    RNA_NODE *rna = ctx->Vsa()->Sr_2_rna(sr);
    RBC_OP get_ops[] = RBC_OP_CONTAINER_GET;
    if (!rna->Is_container_op() ||
        !ctx->Ipsa()->Rna_has_rbc_ops(rna, get_ops,RBC_OPS_CNT(get_ops)))
      return CS_CONT;

    if (!VSA_Checker_SE) {
      Is_Trace(ctx->Tracing(), (TFile, "--%s: container ud stop check sr%d at line %d.\n",
                                chk.Checker_name(),
                                sr->Stmtrep_id(), Srcpos_To_Line(sr->Linenum())));
      return CS_DONE;
    }

    CXX_MEM_POOL pool("local container se pool", 0);
    DEF_OBJS *value_objs = Eval_container_get(rna, obj, ctx, pool());
    if (value_objs) {
      switch (value_objs->Type()) {
        case LIST_TYPE:
        {
          LIST_OBJS *cands = (LIST_OBJS *) value_objs;
          Check_value_objs(chk, *cands, sr, ctx);
          break;
        }
        case MAP_TYPE:
        {
          MAP_OBJS *cands = (MAP_OBJS *) value_objs;
          Check_value_objs(chk, *cands, sr, ctx);
          break;
        }
        default:
          Is_True_Ret(FALSE, ("unknown value type found"), CS_DONE);
      }
    }
    else {
      // TODO: not find element U-D
    }
    return CS_DONE;
  }
};

// ====================================================================
// JNI_CHECKER_HELPER
// Connect the U-D for JNI call
// ====================================================================
typedef CHECKER_STATUS (JNI_CHECKER_HELPER::*JNI_FUNC) (CHECK_OBJ &);
typedef pair<const char*, JNI_FUNC> JNI_PAIR;
typedef mempool_allocator<JNI_PAIR> JNI_ALLOCATOR;
struct funeq
{
  BOOL operator()(const char *s1, const char *s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};
typedef hash_map<const char*, JNI_FUNC, __gnu_cxx::hash<const char*>,
                     funeq, JNI_ALLOCATOR> JNI_FUNC_MAP;

class JNI_CHECKER_HELPER
{
  private:
    static JNI_FUNC_MAP  _jni_func_map;
    TRAV_CONTEXT        &_ctx;
    VSYM_TRACKER        *_tracker;

    JNI_CHECKER_HELPER(void);                                   // REQUIRED UNDEFINED UNWANTED methods
    JNI_CHECKER_HELPER(const JNI_CHECKER_HELPER&);              // REQUIRED UNDEFINED UNWANTED methods
    JNI_CHECKER_HELPER& operator = (const JNI_CHECKER_HELPER&); // REQUIRED UNDEFINED UNWANTED methods

  public:
    JNI_CHECKER_HELPER(TRAV_CONTEXT &ctx) : _ctx(ctx), _tracker(ctx.Tracker()) {}
    static void     Init_func_map();
    CHECKER_STATUS  Check_jni(CHECK_OBJ &obj);
  
  private:
    CHECKER_STATUS  Check_jni_get_object_field(CHECK_OBJ &obj);
    CHECKER_STATUS  Check_jni_set_object_field(CHECK_OBJ &obj);
    CHECKER_STATUS  Check_jni_get_field_id(CHECK_OBJ &obj);
    CHECKER_STATUS  Check_jni_get_object_class(CHECK_OBJ &obj);
    // helper functions
    JNI_FUNC        Jni_fun(char *fun_name);
    TRAV_CONTEXT&   Ctx()       { return _ctx; }
};

// ====================================================================
// UD_TRAVELER
// Traverse the U-D for sym and vsym on CFG and CG
// ====================================================================
template<typename _CHECKER>
class UD_TRAVELER {
private:
  _CHECKER     &_checker;    // the checker
  TRAV_CONTEXT &_ctx;        // the traversal context
  SPOS_HELPER<_CHECKER, _CHECKER::USE_SRCPOS> _spos;  // spos helper

  UD_TRAVELER(const UD_TRAVELER&);                // disable copy constructor
  UD_TRAVELER& operator=(const UD_TRAVELER&);     // disable assign operator

  BOOL Check_skip()
  {
    if((VSA_Checker_Max_Frame >= 0) && _ctx.Frame_depth() >= VSA_Checker_Max_Frame ||
       _spos.Check_skip()) {
      Is_Trace(_ctx.Tracing(), (TFile, "%s: skip U-D srcpos exceed max limit, frame=%d, child_cnt=%d, data_cnt=%ld\n",
                                _checker.Checker_name(), _ctx.Frame_depth(), _spos.Children_count(),
                                _spos.Cur_node() ? _spos.Cur_node()->Data()->size() : 0));
      return TRUE;
    }
    return FALSE;
  }

public:
  // UD_TRAVELER
  // constructor
  UD_TRAVELER(_CHECKER& checker, TRAV_CONTEXT& ctx)
   : _checker(checker), _ctx(ctx), _spos(checker) { }

private:
  // Check_caller_helper
  // Helper function to traverse call-by list and step into caller
  template<typename _VAR_HELPER>
  CHECKER_STATUS Check_caller_helper(_VAR_HELPER& hlp)
  {
    if(Check_skip()) {
      return CS_DONE;
    }
    // get current downwards frame
    Is_Trace(_ctx.Tracing(), (TFile, "Check caller helper:\n"));
    TRAV_FRAME* frame = _ctx.Top_down_frame();
    Is_True(frame == NULL || frame->Rna() != NULL, ("bad frame"));
    Is_Trace(_ctx.Tracing(), (TFile, "top down frame rna %d\n",
                              frame ? frame->Rna()->Rna_idx() : -1));
    DNA_NODE* me = _ctx.Dna();
    VSA *my_vsa = _ctx.Vsa();
    // TODO: global variable
    CODEREP *my_cr = hlp.Visited_arg_idx()
                       ? me->Get_param_cr(hlp.Visited_arg_idx())
                       : NULL;
    Is_True(hlp.Visited_arg_idx() == 0 ||
            (my_cr != NULL && my_cr->Kind() == CK_VAR),
            ("bad formal cr"));
    // save stack before visit caller
    VSYM_TRACKER::VSYM_VECTOR vsym_stack;
    _ctx.Tracker()->Save_stack(&vsym_stack);
    _spos.Push_mark(me->Comp_unit());
    INT32 callby_cnt = me->Caller_cnt();
    INT32 check_cnt = callby_cnt;
    if (frame != NULL) {
      // if frame not null, only visit the callby which entered current dna
      check_cnt = 1;
    } else {
      // limit the callby cnt by VSA_Checker_Max_Callby(if negative, no limit)
      check_cnt = (VSA_Checker_Max_Callby < 0) ? callby_cnt :
                   (callby_cnt > VSA_Checker_Max_Callby ? VSA_Checker_Max_Callby : callby_cnt);
    }
    Is_Trace(_ctx.Tracing(), (TFile, "Real callby=%d, check callby=%d\n", callby_cnt, check_cnt));
    SRCPOS_TREENODE *cur_treenode = _spos.Add_children(check_cnt);
    IDTYPE parent_idx = _spos.Cur_idx();
    INT check_idx = VAR_INIT_ID;
    // check all callers in call-by list
    for (INT i = VAR_INIT_ID; (i <= callby_cnt && check_idx <= check_cnt) && !Check_skip(); i++) {
      RNA_NODE *rna = (*me->Clby_list())[i];
      SRCPOS line = rna->Linenum();
      Is_Trace(_ctx.Tracing(), (TFile, "Visit %s caller[%d/%d] at line %d:",
                                me->Fname(), i, callby_cnt , SRCPOS_linenum(line)));
      // not the caller where the callee entered
      if (frame != NULL && frame->Rna() != rna) {
        Is_Trace(_ctx.Tracing(), (TFile, "rna not equal to caller entered\n"));
        continue;
      }
      // get callsite information
      DNA_NODE *caller = _ctx.Ipsa()->Get_dna(rna->Caller_idx());
      Is_True(caller != NULL, ("bad caller"));
      // TODO: refine this for sec01 & sec02
      //if (caller->Non_functional())
      //  continue;
      // already visited
      UINT64 visited_key = _ctx.Rna_visited_key(rna, TD_UP, hlp.Visited_arg_idx());
      if (_ctx.Visited(rna, visited_key))
        continue;
      Is_Trace(_ctx.Tracing(), (TFile, "\n"));

      if (caller != NULL) {
        // get callee this cr in current context
        TY_IDX   callee_ty = TY_IDX_ZERO;
        CODEREP *callee_this = me->Get_this_param(&callee_ty);

        // switch to caller context and push a new frame
        CONTEXT_SWITCH context(caller);
        CHECK_OBJ caller_obj(rna->Callstmt());
        BOOL maybe = FALSE;
        BOOL succ = hlp.Check_obj(caller, rna, caller_obj, maybe);
        if (!succ || !caller_obj.Is_valid()) {
          Is_Trace(_ctx.Tracing(), (TFile, "Ignore caller %s: caller_obj is not var/vor\n",
                                    caller->Fname()));
          continue;
        }
        // check if caller can be taken in vtable
        CODEREP *rna_this = rna->Get_this_arg();
        if (frame == NULL && rna_this && callee_this) {
          Is_True(callee_ty != TY_IDX_ZERO, ("fail to get this obj type"));
          TY_IDX caller_ty = TY_IDX_ZERO;
          CODEREP *caller_this = caller->Get_this_param(&caller_ty);
          // rna is caller class's member function
          if (caller_ty != TY_IDX_ZERO && caller_this == rna_this) {
            const char *cls_name = TY_name(caller->File_idx(), caller_ty);
            const char *callee_cls = TY_name(me->File_idx(), callee_ty);
            if (!_ctx.Vptrack()->Can_function_taken(caller_this, cls_name, callee_cls, me->Fname())) {
              Is_Trace(_ctx.Tracing(), (TFile, "%s: skip caller %s in class %s. vtable entry mismatch.\n",
                                        _checker.Checker_name(), caller->Fname(), cls_name));
              continue;
            }
          }
        }
        if (rna_this && callee_this) {
          Is_True(callee_ty != TY_IDX_ZERO, ("fail to get this obj type"));
          const char *cls_name = TY_name(me->File_idx(), callee_ty);
          _ctx.Vptrack()->Push(caller, rna_this, cls_name);
        }

        TRAV_FRAME* par_frame = _ctx.Push_caller_frame(rna, caller->Comp_unit(), visited_key);
        STMTREP* stmt = rna->Callstmt();

        // trace stmt after context switch (frame push)
        Is_Trace(_ctx.Tracing(), (TFile, " -%s: from callee %s to caller %s by call stmt:\n",
                                  _checker.Checker_name(),
                                  me->Fname(), caller->Fname()));
        Is_Trace_cmd(_ctx.Tracing(), _ctx.Vsa()->Print_sr(stmt, TFile));

        _spos.Set_cur_node(cur_treenode, check_idx-1);
        if (maybe) {
          _spos.Set_flag(SRCPOS_FLAG_MAYBE);
        }
        _spos.Append_data(stmt, caller, PATHINFO_DNA_CALLSITE);
        _spos.Push_mark(caller->Comp_unit());
        _spos.Add_bb(stmt->Bb());
        check_idx++;

        INT level = _spos.Push_value_graph();
        Is_Trace(_ctx.Tracing(),
                 (TFile, " -%s-VG0: push %s call %s parm %d at line %d, level=%d\n",
                         _checker.Checker_name(),
                         caller->Fname(), me->Fname(),
                         hlp.Visited_arg_idx(),
                         Srcpos_To_Line(stmt->Linenum()),
                         level));

        BOOL vg_ret = TRUE;
        if (my_cr != NULL) {
          // TODO: global variable
          CODEREP *arg_cr = rna->Get_arg(hlp.Visited_arg_idx());
          Is_True(arg_cr != NULL, ("arg cr invalid"));
          vg_ret = _spos.Add_assign(my_vsa, my_cr, _ctx.Vsa(), arg_cr);
          Is_Trace(_ctx.Tracing(),
                   (TFile, " -%s-VG0: %s cr%d <- %s cr%d on %s %s parm %d line %d ==> %d\n",
                           _checker.Checker_name(),
                           caller->Fname(), arg_cr->Coderep_id(),
                           me->Fname(), my_cr->Coderep_id(),
                           OPERATOR_name(stmt->Opr()) + 4,
                           stmt->Opr() == OPR_CALL ? ST_name(stmt->St()) : "",
                           hlp.Visited_arg_idx(),
                           Srcpos_To_Line(stmt->Linenum()),
                           vg_ret));
        }

        if (vg_ret) {
          // update using bb, for checking value range
          CHECKER_STATUS sts = CS_DONE;
          if (caller_obj.Is_var()) {
            sts = Check_coderep(caller_obj);
          } else {
            sts = _checker.Check_heap_obj(caller_obj, &_ctx);
            if (sts == CS_CONT) {
              sts = Check_vsym_ud(caller_obj);
            }
          }
          if (sts != CS_DONE)
            Continue_trav(caller_obj, sts);
        }

        // restart stack after visiting the caller
        _ctx.Tracker()->Restore_stack(&vsym_stack);
        if (rna_this && callee_this)
          _ctx.Vptrack()->Pop(caller);

        Is_Trace(_ctx.Tracing(),
                 (TFile, " -%s-VG0: pop %s call %s parm %d at line %d, level=%d\n",
                         _checker.Checker_name(),
                         caller->Fname(), me->Fname(),
                         hlp.Visited_arg_idx(),
                         Srcpos_To_Line(stmt->Linenum()),
                         level));
        _spos.Pop_value_graph(level);

        _spos.Pop_mark(caller->Comp_unit(), TRUE);
        // pop-up the frame
        TRAV_FRAME* tmp = _ctx.Pop_caller_frame();
        Is_True(tmp == par_frame, ("bad call stack"));
        // break if go back to previous call site
        if (frame != NULL)
          break;
      }
      _ctx.Reset_visited(visited_key);
    }
    _spos.Reset_cur_node(cur_treenode, parent_idx);
    _spos.Pop_mark(me->Comp_unit(), TRUE);
    return CS_DONE;
  }

  // Check_global_init
  // Check static global init value
  CHECKER_STATUS Check_global_init(UNI_GLOBAL_VAR_HELPER& hlp, CHECK_OBJ &obj, CODEREP *cr)
  {
    if (Check_skip()) {
      return CS_DONE;
    }
    Is_Trace(_ctx.Tracing(), (TFile, "Check global initialize:\n"));
    if (PU_java_lang(Get_Current_PU())) {
      // for java, global data means static variable, inited in clinit method
      // this method will be called when class is loaded into jvm
      DNA_NODE *clinit = hlp.Get_global_clinit_dna();
      if(clinit && !_ctx.Visited(clinit->Comp_unit(), NULL, TD_DOWN, hlp.Visited_arg_idx())) {
        _spos.Push_mark(_ctx.Comp_unit());
        Check_callee_rets(NULL, clinit, hlp);
        _spos.Pop_mark(_ctx.Comp_unit(), TRUE);
      }
    }
    // c/c++ init value is saved in inito data
    else if (PU_cxx_lang(Get_Current_PU()) || PU_c_lang(Get_Current_PU())) {
      CODEREP *value_const = NULL;
      std::pair<UINT32, ST_IDX> symbol_st(0, ST_IDX_ZERO);
      BOOL found = _ctx.Initialized_const_value(cr, &value_const, symbol_st);
      Is_Trace(_ctx.Tracing(), (TFile, "%s initialized const value.\n", found ? "Found" : "Not found"));
      // can't found initialized value, can report may here
      if (!found) {
        return CS_DONE;
      }
      if (value_const != NULL) {
        Is_True(value_const->Kind() == CK_CONST || value_const->Kind() == CK_RCONST,
                ("Can only handle const cr here."));
        Is_Trace(_ctx.Tracing(), (TFile, "Const value cr: "));
        Is_Trace_cmdn(_ctx.Tracing(), value_const->Print(TFile), TFile);
        // set global coderep, and check this coderep
        obj.Update_var(value_const);
        return CS_OP;
      } else if ((_CHECKER::SUSPECT & CS_ICALL) && symbol_st.second != ST_IDX_ZERO) {
        ST *fun_st = St_ptr(symbol_st.first, symbol_st.second);
        if (ST_class(fun_st) == CLASS_FUNC) {
          Is_Trace(_ctx.Tracing(), (TFile, "Add target %s\n", ST_name(symbol_st.first, symbol_st.second)));
          _checker.Add_target(symbol_st.first, symbol_st.second);
        } else {
          Is_Trace(_ctx.Tracing(), (TFile, "Skip target non-function symbol %s\n", ST_name(symbol_st.first, symbol_st.second)));
        }
      }
      // may initialized with symbol, done for now
      return CS_DONE;
    } else {
      Is_True(FALSE, ("Unsupported pu language: %d.", PU_src_lang(Get_Current_PU())));
    }
    return CS_DONE;
  }

  // Check_caller_param_var
  // check the U-D of parameter var from callee into caller
  CHECKER_STATUS Check_caller_param_var(IDTYPE parm_idx)
  {

    Is_Trace(_ctx.Tracing(), (TFile, "Check_caller_param_var:\n"));
    //PARAMETER_HELPER hlp(_ctx, parm_idx);
    UNI_PARAMETER_HELPER hlp(_ctx, parm_idx);
    return Check_caller_helper(hlp);
  }

  // Check_caller_param_vsym
  // Check the U-D of parameter's vsym from callee to caller
  CHECKER_STATUS Check_caller_param_vsym(STMTREP *chi, IDTYPE parm_idx, CODEREP *parm_cr, VSYM_OBJ_REP* vor)
  {
    Is_Trace(_ctx.Tracing(), (TFile, "Check_caller_param_vsym:\n"));
    // Expand the vfrs from vor to entry_chi
    // Ex: foo(A *p) { int v = p->x->y->z; }
    // check vor is p->x->y->z, need to Expand below vfrs to Vsym tracker
    // L1: vfr for (y->z) L2: vfr for (x->y) L3: vfr for (p->x)
    BOOL matched = _ctx.Tracker()->Expand(_ctx.Vsa(), chi, vor, parm_cr);
    if (!matched) {
      Is_Trace(_ctx.Tracing(), (TFile, "CS_DONE: vsym not matched\n"));
      return CS_DONE;
    }
    UNI_PARAMETER_HELPER hlp(_ctx, parm_idx);
    return Check_caller_helper(hlp);
  }

  // Check_caller_global_var
  // check the U-D of global var from callee into caller
  CHECKER_STATUS Check_caller_global_var(CHECK_OBJ &obj, CODEREP* cr)
  {
    Is_Trace(_ctx.Tracing(), (TFile, "Check_caller_global_var:\n"));
    Is_True(cr->Kind() == CK_VAR, ("not var"));
    AUX_STAB_ENTRY *sym = _ctx.Opt_stab()->Aux_stab_entry(cr->Aux_id());
    // skip checking java constant data (begin with _CD_XXX) for now
    // TODO: may need special handle for different checker
    if (!sym->Is_global() || ST_is_class_const_data(sym->St())) {
      // Assertion or for RVSA
      return CS_DONE;
    }

    UNI_GLOBAL_VAR_HELPER hlp(_ctx, ST_st_idx(sym->St()));
    if (_ctx.Dna()->Clby_list()->size() <= VAR_INIT_ID) {
      return Check_global_init(hlp, obj, cr);
    } else {
      // for icall resolver, always read inito first
      if (_CHECKER::SUSPECT & CS_ICALL) {
        Check_global_init(hlp, obj, cr);
      }
      return Check_caller_helper(hlp);
    }
    return CS_DONE;
  }

  // Check_caller_global_vsym
  // Check the U-D of global var's vsym from callee to caller
  CHECKER_STATUS Check_caller_global_vsym(CHECK_OBJ &obj, CODEREP* cr, VSYM_OBJ_REP* vor)
  {
    Is_Trace(_ctx.Tracing(), (TFile, "Check_caller_global_vsym:\n"));
    Is_True(cr->Kind() == CK_VAR, ("not var"));
    AUX_STAB_ENTRY *sym = _ctx.Opt_stab()->Aux_stab_entry(cr->Aux_id());
    if (!sym->Is_global()) {
      // Assertion or for RVSA
      return CS_DONE;
    }

    // Expand the vfrs from vor to entry_chi
    // Ex: foo() { int v = p->x->y->z; } // p is global
    // check vor is p->x->y->z, need to Expand below vfrs to Vsym tracker
    // L1: vfr for (y->z) L2: vfr for (x->y) L3: vfr for (p->x)
    BOOL matched = _ctx.Tracker()->Expand(_ctx.Vsa(), obj.Stmtrep(), vor, cr);
    if (!matched) {
      Is_Trace(_ctx.Tracing(), (TFile, "CS_DONE: vsym not matched\n"));
      return CS_DONE;
    }

    if (!sym->Is_virtual()) {
      VSYM_FLD_REP vfr = _ctx.Vsa()->Cr_vfr(cr);
      VSYM_FLD_REP *check_vfr = CXX_NEW(VSYM_FLD_REP(vfr.Kind(), vfr.Fld_id(), vfr.Ofst()), _ctx.Mem_pool());
      _ctx.Tracker()->Push(check_vfr);
    }

    UNI_GLOBAL_VAR_HELPER hlp(_ctx, ST_st_idx(sym->St()));
    if (_ctx.Dna()->Clby_list()->size() <= VAR_INIT_ID) {
      return Check_global_init(hlp, obj, cr);
    } else {
      // for icall resolver, always read inito first
      if (_CHECKER::SUSPECT & CS_ICALL) {
        Check_global_init(hlp, obj, cr);
      }
      return Check_caller_helper(hlp);
    }
    return CS_DONE;
  }

  // Check_callee_retv_helper
  // Helper function to traverse callee retv list and step into callee
  template<typename _VAR_HELPER>
  CHECKER_STATUS Check_callee_retv_helper(RNA_NODE* rna, _VAR_HELPER& hlp)
  {
    UINT64 visited_key = _ctx.Rna_visited_key(rna, TD_DOWN, hlp.Visited_arg_idx());
    if (Check_skip() || _ctx.Visited(rna, visited_key))
      return CS_DONE;

    DNA_NODE *caller = _ctx.Dna();
    VSA *caller_vsa = _ctx.Vsa();
    CODEREP *retv_cr = hlp.Retv_cr();
    Is_True(retv_cr != NULL, ("retv coderep invalid"));

    // save stack before visiting the callee
    VSYM_TRACKER::VSYM_VECTOR vsym_stack;
    _ctx.Tracker()->Save_stack(&vsym_stack);
    _spos.Push_mark(_ctx.Comp_unit());
    _spos.Append_data(rna->Callstmt(), _ctx.Dna(), PATHINFO_CALL_CHI);
    IDTYPE parent_idx = _spos.Cur_idx();
    INT32 callee_cnt = rna->Callee_cnt();
    INT32 check_cnt = (VSA_Checker_Max_Callee < 0) ? callee_cnt :
                       (callee_cnt > VSA_Checker_Max_Callee ? VSA_Checker_Max_Callee : callee_cnt);

    Is_Trace(_ctx.Tracing(), (TFile, "Real callee=%d, check callee=%d\n", callee_cnt, check_cnt));
    SRCPOS_TREENODE* callee_node = _spos.Add_children(check_cnt);
    for (INT32 i = 0; (i < check_cnt) && !Check_skip(); i++) {
      _spos.Set_cur_node(callee_node, i);
      DNA_NODE* callee = _ctx.Ipsa()->Get_dna(rna->Callee_info(i)->Callee());
      Is_True(callee != NULL, ("bad callee"));
      if (callee == NULL)
        continue;

      Is_Trace(_ctx.Tracing(), (TFile, " -%s: from caller %s to callee %s by call stmt:\n",
                                       _checker.Checker_name(),
                                       _ctx.Dna()->Fname(), callee->Fname()));
      Is_Trace_cmd(_ctx.Tracing(), _ctx.Vsa()->Print_sr(rna->Callstmt(), TFile));
      if(callee->Non_functional()) {
        Is_Trace(_ctx.Tracing(), (TFile, 
                                  " -%s: skip callee %s as it is rbc model function\n",
                                  _checker.Checker_name(),
                                  callee->Fname()));
        continue;
      }

      CODEREP *rna_this = rna->Get_this_arg();
      CODEREP *callee_this = callee->Get_param_cr(VAR_INIT_ID);
      if (rna_this && callee_this) {
        TY_IDX callee_ty = callee->Get_this_obj_ty(callee_this);
        Is_True(callee_ty != TY_IDX_ZERO, ("invalid callee this type"));
        const char *cls_name = TY_name(callee->File_idx(), callee_ty);
        if (!_ctx.Vptrack()->Can_function_taken(rna_this, NULL, cls_name, callee->Fname())) {
          Is_Trace(_ctx.Tracing(), (TFile, "%s: skip callee %s in class %s. vtable entry mismatch.\n",
                                    _checker.Checker_name(), callee->Fname(), cls_name));
          continue;
        }

        TY_IDX caller_ty = rna->Get_this_obj_ty(_ctx.Dna()->File_idx(), rna_this);
        Is_True(caller_ty != TY_IDX_ZERO, ("invalid caller this type"));
        cls_name = TY_name(_ctx.Dna()->File_idx(), caller_ty);
        _ctx.Vptrack()->Push(callee, callee_this, cls_name);
      }

      _spos.Append_data(rna->Callstmt(), _ctx.Dna(), PATHINFO_DUMMY);

      // switch to callee context and push a new frame
      CONTEXT_SWITCH context(callee);
      TRAV_FRAME* frame = _ctx.Push_callee_frame(rna, callee->Comp_unit(), visited_key);
      _spos.Push_mark(callee->Comp_unit());
      SRCPOS_TREENODE* cur_node = _spos.Add_children(callee->Retv_list()->size() - 1);
      IDTYPE inner_parent_idx = _spos.Cur_idx();
      for (INT i = VAR_INIT_ID; i < callee->Retv_list()->size() && !Check_skip(); ++ i) {
        PDV_NODE* pdv = (*callee->Retv_list())[i];
        if (pdv->Kind() != BY_RETURNSTMT)
          continue;
        _spos.Set_cur_node(cur_node, i-1);
        STMTREP* stmt = pdv->Stmt();
        Is_Trace(_ctx.Tracing(), (TFile, " -%s: Check return stmt:\n",
                                         _checker.Checker_name()));
        Is_Trace_cmd(_ctx.Tracing(), _ctx.Vsa()->Print_sr(pdv->Stmt(), TFile));
        Is_True(pdv->Oparam() == 0, ("not return value"));
        INT level = _spos.Push_value_graph();
        Is_Trace(_ctx.Tracing(),
                 (TFile, " -%s-VG1: push %s call %s retv line %d, level=%d\n",
                         _checker.Checker_name(),
                         caller->Fname(), callee->Fname(),
                         Srcpos_To_Line(stmt->Linenum()),
                         level));
        // TODO: lhs or rhs here?
        BOOL vg_ret = _spos.Add_assign(caller_vsa, retv_cr, _ctx.Vsa(), stmt->Rhs());
        Is_Trace(_ctx.Tracing(),
                 (TFile, " -%s-VG1:  %s cr%d <- %s cr%d call line %d retv line %d ==> %d\n",
                         _checker.Checker_name(),
                         caller->Fname(), retv_cr->Coderep_id(),
                         callee->Fname(), stmt->Rhs()->Coderep_id(),
                         Srcpos_To_Line(rna->Callstmt()->Linenum()),
                         Srcpos_To_Line(stmt->Linenum()),
                         vg_ret));

        if (vg_ret) {
          // get new callee_obj from helper
          CHECK_OBJ callee_obj(stmt);
          BOOL maybe = FALSE;
          BOOL succ = hlp.Check_obj(callee, pdv, callee_obj, maybe);
          if (succ && callee_obj.Is_valid()) {
            if (maybe) {
              _spos.Set_flag(SRCPOS_FLAG_MAYBE);
            }
            _spos.Add_bb(stmt->Bb());
            _spos.Append_data(stmt, callee, PATHINFO_COPY);
            CHECKER_STATUS sts;
            if (callee_obj.Is_var()) {
              sts = Check_coderep(callee_obj);
            } else {
              sts = Check_vsym_ud(callee_obj);
            }
            if (sts != CS_DONE) {
              Continue_trav(callee_obj, sts);
            }
          } else {
            Is_Trace(_ctx.Tracing(), (TFile, "  ignored. callee_obj is not var/vsym.\n"));
          }
        }

        // restore stack after visiting callee
        Is_Trace(_ctx.Tracing(),
                 (TFile, " -%s-VG1: pop %s call %s retv at line %d, level=%d\n",
                         _checker.Checker_name(),
                         caller->Fname(), callee->Fname(),
                         Srcpos_To_Line(stmt->Linenum()),
                         level));
        _spos.Pop_value_graph(level);

        _spos.Pop_mark(callee->Comp_unit(), FALSE);
        _ctx.Tracker()->Restore_stack(&vsym_stack);
      }
      _spos.Reset_cur_node(cur_node, inner_parent_idx);
      // pop-up the frame
      if (rna_this && callee_this)
        _ctx.Vptrack()->Pop(callee);
      _spos.Pop_mark(callee->Comp_unit(), TRUE);
      TRAV_FRAME* tmp = _ctx.Pop_callee_frame();
      Is_True(tmp == frame, ("bad call stack"));
    }
    _ctx.Reset_visited(visited_key);
    _spos.Reset_cur_node(callee_node, parent_idx);
    _spos.Pop_mark(_ctx.Comp_unit(), TRUE);
    return CS_DONE;
  }

  // Check_callee_rets
  // traverse callee rets list
  // Note: rna can be NUL, ex: Check_global_init
  template<typename _VAR_HELPER>
  void Check_callee_rets(RNA_NODE * rna, DNA_NODE* callee, _VAR_HELPER& hlp)
  {
    Is_True(callee != NULL, ("bad callee"));
    if (callee == NULL)
      return;
    if(callee->Non_functional()) {
      Is_Trace(_ctx.Tracing(), (TFile,
                                " -%s: skip callee %s as it is rbc model function\n",
                                _checker.Checker_name(),
                                callee->Fname()));
      return;
    }
    // if rna is NULL, it is a fake edge from top dna to global init dna
    // set call_stmt to entry chi
    STMTREP *call_stmt = rna ? rna->Callstmt() : _ctx.Vsa()->Get_entry_chi_stmt();
    if (call_stmt == NULL) {
      return;
    }
    Is_Trace(_ctx.Tracing(), (TFile, " -%s: from caller %s to callee %s by call stmt:\n",
                                      _checker.Checker_name(),
                                      _ctx.Dna()->Fname(), callee->Fname()));
    Is_Trace_cmd(_ctx.Tracing(), _ctx.Vsa()->Print_sr(call_stmt, TFile));

    DNA_NODE *caller = _ctx.Dna();
    VSA *caller_vsa = _ctx.Vsa();
    if (caller_vsa == callee->Comp_unit()->Vsa()) {
      Is_Trace(_ctx.Tracing(), (TFile,
                                " -%s: skip recursive call %s \n",
                                _checker.Checker_name(),
                                callee->Fname()));
      return;
    }
    CODEREP *retv_cr = rna ? _ctx.Comp_unit()->Find_return_value(call_stmt)
                           : NULL;

    CODEREP *rna_this = rna ? rna->Get_this_arg() : NULL;
    CODEREP *callee_this = callee->Get_param_cr(VAR_INIT_ID);
    if (rna_this && callee_this) {
      TY_IDX callee_ty = callee->Get_this_obj_ty(callee_this);
      Is_True(callee_ty != TY_IDX_ZERO, ("invalid callee this type"));
      const char *cls_name = TY_name(callee->File_idx(), callee_ty);
      if (!_ctx.Vptrack()->Can_function_taken(rna_this, NULL, cls_name, callee->Fname())) {
        Is_Trace(_ctx.Tracing(), (TFile, "%s: skip callee %s in class %s. vtable entry mismatch.\n",
                                  _checker.Checker_name(), callee->Fname(), cls_name));
        return;
      }

      TY_IDX caller_ty = rna->Get_this_obj_ty(_ctx.Dna()->File_idx(), rna_this);
      Is_True(caller_ty != TY_IDX_ZERO, ("invalid caller this type"));
      cls_name = TY_name(_ctx.Dna()->File_idx(), caller_ty);
      _ctx.Vptrack()->Push(callee, callee_this, cls_name);
    }

    VSYM_TRACKER::VSYM_VECTOR vsym_stack;
    _ctx.Tracker()->Save_stack(&vsym_stack);
    _spos.Append_data(call_stmt, _ctx.Dna(), PATHINFO_DUMMY);
    CONTEXT_SWITCH context(callee);
    UINT64 visited_key = _ctx.Rna_visited_key(rna, TD_DOWN, hlp.Visited_arg_idx());
    TRAV_FRAME* frame = _ctx.Push_callee_frame(rna, callee->Comp_unit(), visited_key);
    _spos.Push_mark(callee->Comp_unit());
    IDTYPE inner_idx = _spos.Cur_idx();
    int ret_idx = 0;
    SRCPOS_TREENODE *retv_node = _spos.Add_children(callee->Rets_list()->size());
    for (STMTR_VECTOR::const_iterator it = callee->Rets_list()->begin();
          it != callee->Rets_list()->end() && !Check_skip(); ++ it) {
      _spos.Set_cur_node(retv_node, ret_idx++);
      STMTREP* stmt = *it;
      Is_Trace(_ctx.Tracing(), (TFile, " -%s: Check return stmt[%d]:\n",
                                        _checker.Checker_name(), ret_idx));
      Is_Trace_cmd(_ctx.Tracing(), _ctx.Vsa()->Print_sr(stmt, TFile));

      INT level = _spos.Push_value_graph();
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VG2: push %s call %s outparm %d line %d, level=%d\n",
                       _checker.Checker_name(),
                       caller->Fname(), callee->Fname(),
                       hlp.Visited_arg_idx(),
                       Srcpos_To_Line(stmt->Linenum()),
                       level));
      Is_True(stmt->Opr() == OPR_RETURN, ("not return stmt"));
      STMTREP* prev = stmt->Prev();
      AUX_STAB_ENTRY* aux;

      BOOL vg_ret = TRUE;
      if (retv_cr && prev && prev->Opr() == OPR_STID &&
          prev->Lhs()->Kind() == CK_VAR &&
          (aux = _ctx.Opt_stab()->Aux_stab_entry(prev->Lhs()->Aux_id())) != NULL &&
           aux->Is_return_preg()) {
        // TODO: lhs or rhs?
        vg_ret = _spos.Add_assign(caller_vsa, retv_cr, _ctx.Vsa(), prev->Rhs());
        Is_Trace(_ctx.Tracing(),
                 (TFile, " -%s-VG2: %s cr%d <- %s cr%d outparm %d line %d ==> %d\n",
                         _checker.Checker_name(),
                         caller->Fname(), retv_cr->Coderep_id(),
                         callee->Fname(), prev->Rhs()->Coderep_id(),
                         hlp.Visited_arg_idx(),
                         Srcpos_To_Line(stmt->Linenum()),
                         vg_ret));
      }

      if (vg_ret) {
        CHECK_OBJ callee_obj(stmt);
        BOOL maybe = FALSE;
        BOOL succ = hlp.Check_obj(callee, stmt, callee_obj, maybe);
        if (succ && callee_obj.Is_valid()) {
          if (maybe) {
            _spos.Set_flag(SRCPOS_FLAG_MAYBE);
          }
          _spos.Add_bb(stmt->Bb());
          _spos.Append_data(stmt, callee, PATHINFO_COPY);
          CHECKER_STATUS sts;
          if (callee_obj.Is_var()) {
            sts = Check_coderep(callee_obj);
          } else {
            sts = _checker.Check_heap_obj(callee_obj, &_ctx);
            if (sts == CS_CONT)
              sts = Check_vsym_ud(callee_obj);
          }
          if (sts != CS_DONE) {
            Continue_trav(callee_obj, sts);
          }
        }
        else {
          Is_Trace(_ctx.Tracing(), (TFile, "  ignored. callee_obj is not var/vsym.\n"));
        }
      }

      // restore stack after visiting callee
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VG2: pop %s call %s out-parm %d at line %d, level=%d\n",
                       _checker.Checker_name(),
                       caller->Fname(), callee->Fname(),
                       hlp.Visited_arg_idx(),
                       Srcpos_To_Line(stmt->Linenum()),
                       level));
      _spos.Pop_value_graph(level);

      _spos.Pop_mark(callee->Comp_unit(), FALSE);
      _ctx.Tracker()->Restore_stack(&vsym_stack);
    }
    _spos.Reset_cur_node(retv_node, inner_idx);
    if (rna_this && callee_this)
      _ctx.Vptrack()->Pop(callee);
    _spos.Pop_mark(callee->Comp_unit(), TRUE);
    TRAV_FRAME* tmp = _ctx.Pop_callee_frame();
    Is_True(tmp == frame, ("bad call stack"));
  }
  // Check_callee_rets_helper
  // Helper function to traverse callee rets list and step into callee
  template<typename _VAR_HELPER>
  CHECKER_STATUS Check_callee_rets_helper(RNA_NODE* rna, _VAR_HELPER& hlp)
  {
    UINT64 visited_key = _ctx.Rna_visited_key(rna, TD_DOWN, hlp.Visited_arg_idx());
    if (Check_skip() || _ctx.Visited(rna, visited_key))
      return CS_DONE;

    _spos.Push_mark(_ctx.Comp_unit());
    IDTYPE parent_idx = _spos.Cur_idx();
    _spos.Append_data(rna->Callstmt(), _ctx.Dna(), PATHINFO_CALL_CHI);
    INT32 callee_cnt = rna->Callee_cnt();
    INT32 check_cnt = (VSA_Checker_Max_Callee < 0) ? callee_cnt :
                       (callee_cnt > VSA_Checker_Max_Callee ? VSA_Checker_Max_Callee : callee_cnt);
    Is_Trace(_ctx.Tracing(), (TFile, "Real callee=%d, check callee=%d\n", callee_cnt, check_cnt));
    SRCPOS_TREENODE* callee_node = _spos.Add_children(check_cnt);

    for (INT32 i = 0; (i < check_cnt) && !Check_skip(); i++) {
      _spos.Set_cur_node(callee_node, i);
      DNA_NODE* callee = _ctx.Ipsa()->Get_dna(rna->Callee_info(i)->Callee());
      Check_callee_rets(rna, callee, hlp);
    }
    _ctx.Reset_visited(visited_key);
    _spos.Reset_cur_node(callee_node, parent_idx);
    _spos.Pop_mark(_ctx.Comp_unit(), TRUE);
    return CS_DONE;
  }

  // Check_callee_return_var
  // Check the U-D of return value from caller into callee
  CHECKER_STATUS Check_callee_return_var(RNA_NODE* rna, CODEREP* ret_cr)
  {
    Is_Trace(_ctx.Tracing(), (TFile, "Check_callee_return_var:\n"));
    UNI_RETURN_VAR_HELPER hlp(_ctx, ret_cr);
    return Check_callee_retv_helper(rna, hlp);
  }

  // Check_callee_return_vsym
  // Check the U-D of return value's vsym from caller into callee
  CHECKER_STATUS Check_callee_return_vsym(RNA_NODE* rna, VSYM_OBJ_REP* vor, CODEREP *ret_cr)
  {
    Is_Trace(_ctx.Tracing(), (TFile, "Check_callee_return_vsym:\n"));
    BOOL matched = _ctx.Tracker()->Expand(_ctx.Vsa(), rna->Callstmt(), vor, ret_cr);
    if (matched == FALSE) {
      Is_Trace(_ctx.Tracing(), (TFile, "  callee vsym unmatched: CS_DONE\n"));
      return CS_DONE;
    }
    UNI_RETURN_VAR_HELPER hlp(_ctx, ret_cr);
    return Check_callee_retv_helper(rna, hlp);
  }

  // Check_callee_output_var
  // Check the U-D of output value from caller into callee (LDA)
  CHECKER_STATUS Check_callee_output_var(RNA_NODE* rna, IDTYPE arg_idx, CHECK_OBJ &obj)
  {
    Is_Trace(_ctx.Tracing(), (TFile, "Check_callee_output_var: "));
    CODEREP *arg_cr = rna->Get_arg(arg_idx);
    if (arg_cr == NULL) {
      Is_Trace(_ctx.Tracing(), (TFile, " CS_DONE: no arg_cr for arg_idx%d\n", arg_idx));
      return CS_DONE;
    }
    CODEREP *check_cr = obj.Coderep();
    HEAP_OBJ_REP* hor = _ctx.Vsa()->Cr_2_heap_obj(arg_cr);
    Is_True_Ret(hor, ("lda arg hor is null"), CS_DONE);
    Is_True_Ret(check_cr->Kind() == CK_VAR, ("Check_callee_output_var check cr is not VAR"), CS_DONE);
    UINT32 ofst = _ctx.Vsa()->Cr_ofst(check_cr);
    UINT32 fld_id = check_cr->Field_id();
    AUX_STAB_ENTRY* sym = _ctx.Opt_stab()->Aux_stab_entry(check_cr->Aux_id());
    if (sym->St()) {
      TY_IDX st_type = ST_type(sym->St());
      if (TY_kind(st_type) == KIND_STRUCT) {
        UINT cur_field_id = 0;
        FLD_HANDLE fld = FLD_get_from_offset(st_type, ofst, cur_field_id);
        Is_True_Ret(!fld.Is_Null(),
                    ("Check_callee_output_var unable to get fld from ofst %d", ofst),
                    CS_DONE);
        fld_id = cur_field_id;
      }
    }
    VSYM_FLD_REP vfr(FLD_K_ID, fld_id, ofst);
    VSYM_OBJ_REP *vor =  _ctx.Vsa()->Find_hor_chi_vor_any(rna->Callstmt(), hor, &vfr, arg_cr, !VSA_Checker_Vfr_Exact_Match);
    if (vor) {
      Is_Trace(_ctx.Tracing(), (TFile, "Matched vsym \n"));
      VSYM_FLD_REP *check_vfr = CXX_NEW(VSYM_FLD_REP(FLD_K_ID, fld_id, ofst), _ctx.Mem_pool());
      _ctx.Tracker()->Push(check_vfr);
    } else if (rna->Is_flag_set(RNA_HAS_FUNCTIONAL)) {
      CHI_NODE* chi = check_cr->Defchi();
      obj.Update_var(chi->OPND(), rna->Callstmt());
      Is_Trace(_ctx.Tracing(), (TFile, "No vsym chi in callee, continue with chi opnd cr%d:\n", chi->OPND()->Coderep_id()));
      return CS_VAR_UD;
    }
    UNI_OUTPUT_VAR_HELPER hlp(_ctx, arg_idx);
    return Check_callee_rets_helper(rna, hlp);
  }

  // Check_callee_input_var
  // Check the U-D of input parameter from caller into callee
  // VAR_VSYM only
  CHECKER_STATUS Check_callee_input_var(RNA_NODE* rna, IDTYPE arg_idx)
  {
    Is_True(!_ctx.Tracker()->Empty(), ("no vsym in stack"));
    Is_Trace(_ctx.Tracing(), (TFile, "Check_callee_input_var:\n"));
    UNI_PARAMETER_HELPER hlp(_ctx, arg_idx);
    return Check_callee_rets_helper(rna, hlp);
  }

  // Check_callee_output_vsym
  // Check the U-D of output value's vor from caller into callee (LDA)
  CHECKER_STATUS Check_callee_output_vsym(RNA_NODE* rna, INT32 arg, CODEREP *cr, VSYM_OBJ_REP* vor)
  {
    // check return statement because vor is annotated on return stmt
    Is_Trace(_ctx.Tracing(), (TFile, "Check_callee_output_vsym:\n"));
    CODEREP *arg_cr = rna->Get_arg(arg);
    if (arg_cr == NULL) {
      return CS_DONE;
    }
    BOOL matched = _ctx.Tracker()->Expand(_ctx.Vsa(), rna->Callstmt(), vor, arg_cr);
    if (matched == FALSE) {
      Is_Trace(_ctx.Tracing(), (TFile, "  callee vsym unmatched: CS_DONE\n"));
      return CS_DONE;
    }
    UNI_OUTPUT_VAR_HELPER hlp(_ctx, arg);
    return Check_callee_rets_helper(rna, hlp);
  }

  // Check_callee_global_var
  // Check the U-D of global variable from caller to callee
  CHECKER_STATUS Check_callee_global_var(RNA_NODE* rna, ST* st, CHECK_OBJ &obj, CHI_NODE *chi)
  {
    Is_True(ST_sclass(st) == SCLASS_FSTATIC ||
            ST_sclass(st) == SCLASS_UGLOBAL ||
            ST_sclass(st) == SCLASS_DGLOBAL ||
            ST_sclass(st) == SCLASS_COMMON ||
            ST_sclass(st) == SCLASS_EXTERN, ("invalid st"));

    Is_Trace(_ctx.Tracing(), (TFile, "Check_callee_global_var:"));
    BOOL callee_def_global = FALSE;

    for (INT32 i = 0; i < rna->Callee_cnt() && !callee_def_global; i++) {
      DNA_NODE* callee = _ctx.Ipsa()->Get_dna(rna->Callee_info(i)->Callee());
      if (callee && !callee->Non_functional() && callee->Rgvl_list()->size() > PDV_INIT_ID) {
        UINT32 file_idx = _ctx.Dna()->File_idx();
        UINT32 st_idx = ST_st_idx(st);
        CONTEXT_SWITCH callee_ctx(callee);
        WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
        if (file_idx != callee->File_idx() &&
            ST_sclass(st) == SCLASS_EXTERN) {
          Is_True_Ret(mgr != NULL, ("not in xfa mode"), CS_DONE);
          mgr->Resolve(file_idx, st_idx, file_idx, st_idx);
        }
        for (INT j = VAR_INIT_ID; j < callee->Rgvl_list()->size() && !callee_def_global; ++j) {
          PDV_NODE* pdv = callee->Rgvl_list()->at(j);
          if (!pdv->Is_value())  // ignore resource PDV_NODE
            continue;
          Is_True(pdv->Stmt() != NULL && pdv->Stmt()->Opr() == OPR_STID,
                  ("invalid assignment to global"));
          CODEREP* lhs = pdv->Stmt()->Lhs();
          Is_True(callee->Is_aux_global(lhs->Aux_id()),
                  ("invalid aux in return global value list"));

          AUX_STAB_ENTRY* lhs_aux = callee->Comp_unit()->Opt_stab()->Aux_stab_entry(lhs->Aux_id());
          UINT32 pdv_file = callee->File_idx();
          UINT32 pdv_st = ST_st_idx(lhs_aux->St());

          if (file_idx != pdv_file &&
              ST_sclass(lhs_aux->St()) == SCLASS_EXTERN) {
            Is_True_Ret(mgr != NULL, ("not in xfa mode"), CS_DONE);
            mgr->Resolve(pdv_file, pdv_st, pdv_file, pdv_st);
          }
          if (pdv_file == file_idx && pdv_st == st_idx) {
            callee_def_global = TRUE;
            break;
          }
        }
      }
    }
    if (callee_def_global) {
      Is_Trace(_ctx.Tracing(), (TFile, "  check callee rets\n"));
      UNI_GLOBAL_VAR_HELPER hlp(_ctx, ST_st_idx(st));
      return Check_callee_rets_helper(rna, hlp);
    } else {
      // if callee has no global def, continue traverse by chi opnd
      Is_Trace(_ctx.Tracing(), 
               (TFile, "  callee has no global def, cont with chi opnd cr%d\n",
                chi->OPND()->Coderep_id()));
      obj.Update_var(chi->OPND(), rna->Callstmt());
      return CS_VAR_UD;
    }
  }

  // Check_callee_global_vsym
  // Check the U-D of global variable's vor from caller to callee
  CHECKER_STATUS Check_callee_global_vsym(RNA_NODE* rna, ST* st, VSYM_OBJ_REP* vor)
  {
    Is_True(ST_sclass(st) == SCLASS_FSTATIC ||
            ST_sclass(st) == SCLASS_UGLOBAL ||
            ST_sclass(st) == SCLASS_DGLOBAL ||
            ST_sclass(st) == SCLASS_COMMON ||
            ST_sclass(st) == SCLASS_EXTERN, ("invalid st"));
    
    Is_Trace(_ctx.Tracing(), (TFile, "Check_callee_global_vsym:\n"));
    UNI_GLOBAL_VAR_HELPER hlp(_ctx, ST_st_idx(st));
    return Check_callee_rets_helper(rna, hlp);
  }

  
  CHECKER_STATUS Check_callee_input_vsym(RNA_NODE *rna, IDTYPE parm, CODEREP *parm_cr, VSYM_OBJ_REP *vor)
  {
    Is_True(!_ctx.Tracker()->Empty(), ("no vsym in stack"));
    Is_Trace(_ctx.Tracing(), (TFile, "Check_callee_input_vsym:\n"));

    BOOL matched = _ctx.Tracker()->Expand(_ctx.Vsa(), rna->Callstmt(), vor, parm_cr);
    if (matched == FALSE) {
      Is_Trace(_ctx.Tracing(), (TFile, "  callee vsym unmatched: CS_DONE\n"));
      return CS_DONE;
    }
    UNI_PARAMETER_HELPER hlp(_ctx, parm);
    return Check_callee_rets_helper(rna, hlp);
  }

  // Check_eh_path
  // Check the U-D of var on EH path
  CHECKER_STATUS Check_eh_path(CHECK_OBJ& obj, STMTREP *sr)
  {
    VSYM_OBJ_REP *vor = NULL;
    CODEREP *cr = NULL;
    if (obj.Is_var()) {
      cr = obj.Coderep();
      Is_True_Ret(cr->Kind() == CK_VAR &&
                  cr->Is_flag_set(CF_DEF_BY_CHI),
                  ("Check_eh_path: bad cr"), CS_DONE);
    } else {
      vor = obj.Vor();
      Is_True_Ret(vor, ("Check_eh_path: bad vor"), CS_DONE);
    }
    Is_True(sr->Opr() == OPR_OPT_CHI, ("bad sr"));
    Is_True(obj.Stmtrep() == NULL || obj.Stmtrep() != sr, ("bad orig sr"));

    // check if eh table is available
    if (_ctx.Comp_unit()->EH_table() == NULL) {
      Is_Trace(_ctx.Tracing(), (TFile, "Not find eh_table for %s.\n", _ctx.Dna()->Fname()));
      return CS_DONE;
    }

    // check if this handler is visited before
    if (_ctx.Visited(sr->Bb())) {
      Is_Trace(_ctx.Tracing(), (TFile, "BB%d visited", sr->Bb()->Id()));
      return CS_DONE;
    }
    BB_NODE* orig_bb = obj.Bb();
    // check EH filter
    INT filter = -1;
    {
      // get control dependency between orig_bb and chi bb
      std::vector< std::pair<BB_NODE*, BB_NODE*> > cds;
      _ctx.Comp_unit()->Vra()->Collect_control_dependencies(orig_bb, sr->Bb(), cds);
      // find out filter used in comparison on CDs
      std::vector< std::pair<BB_NODE*, BB_NODE*> >::iterator it;
      for (it = cds.begin(); it != cds.end(); ++it) {
        STMTREP* stmt = it->first->Last_stmtrep();
        if (stmt == NULL)
          continue;
        if (stmt->Rhs()->Kind() != CK_OP ||
            (stmt->Rhs()->Opr() != OPR_EQ &&
             stmt->Rhs()->Opr() != OPR_NE))
          continue;
        if (stmt->Rhs()->Opnd(0)->Kind() != CK_VAR)
          continue;
        AUX_STAB_ENTRY *sym = _ctx.Opt_stab()->Aux_stab_entry(stmt->Rhs()->Opnd(0)->Aux_id());
        ST* st = sym->St();
        if (st != NULL &&
            ST_one_per_pu(st) &&
            strcmp(ST_name(st), "__Exc_Filter__") == 0) {
          Is_True(stmt->Rhs()->Opnd(1)->Kind() == CK_CONST,
                  ("opnd(1) is not a int const"));
          filter = stmt->Rhs()->Opnd(1)->Const_val();
          break;
        }
      }
    }

    // find out a possible paths from call(throw) to this handler
    std::vector<EH_PATH*> paths;
    EH_TABLE* eh_table = _ctx.Comp_unit()->EH_table();
    EH_PATH_ITER iter(eh_table, sr);
    while (!iter.Is_empty()) {
      EH_PATH* path = iter.Next();
      // get throw stmt
      STMTREP* stmt = path->Throw_stmt();
      STR_IDX eh_type = _ctx.Dna()->Get_eh_throw_type(stmt);
      if (eh_type != 0) {
        if (eh_table->Is_eh_type_match(_ctx.Dna()->File_idx(), eh_type, filter))
          paths.push_back(path);
      }
      else {
        // eh_type = 0 here, this is not a direct 'throw' stmt with a valid object
        // maybe this stmt is 'throw null' or
        // a call stmt to a function that may throw some exception
        RNA_NODE* rna = _ctx.Vsa()->Sr_2_rna(stmt);
        if (rna != NULL && rna->Uniq_callee() != INVALID_RNA_PU_IDX) {
          // If the stmt is a call (Not direct 'throw') which may throw exceptions
          DNA_NODE *callee = _ctx.Vsa()->Ipsa()->Get_dna(rna->Uniq_callee());
          if (filter >= 0) {
            FS_PAIR eh_type = eh_table->Type_info(filter);
            if (callee->EH_table()->Throw_eh_type(eh_type.first, eh_type.second))
              paths.push_back(path);
          }
          else {
            EH_TYPE_VECTOR* tv = callee->EH_table()->Type_list();
            if (VSA_EH_Spec_Default == 1 && tv->size() == 0 &&
                ! PU_nothrow(Pu_Table[callee->Pu_idx()])) {
              paths.push_back(path);
              break;
            }
            EH_TYPE_VECTOR::iterator it;
            for (it = tv->begin(); it != tv->end(); ++it) {
              if (eh_table->Get_type_filter(it->first, it->second) == filter) {
                paths.push_back(path);
                break;
              }
            }
          }
        }
        else if (filter == -1) {
          // this is an external call and catch all, so add to path
          paths.push_back(path);
        }
      }
    }

    if (paths.size() == 0)
      return CS_DONE;

    // now check all possible paths
    // TODO: find out the right srcpos for catch
    _spos.Append_data(sr, _ctx.Dna(), PATHINFO_EH_CATCH);
    SRCPOS_TREENODE *cur_node = _spos.Add_children(paths.size());
    _spos.Push_mark(_ctx.Comp_unit());

    HEAP_OBJ_REP* hor = vor ? vor->Vsym_obj()->Base_hor() : NULL;
    VSYM_FLD_REP* fld = vor ? vor->Vsym_obj()->Fld_rep_ptr() : NULL;
    Is_True(vor == NULL ||
            (hor != NULL && fld != NULL), ("invalid vor, hor and fld"));

    INT32 i = 0;
    std::vector<EH_PATH*>::iterator it;
    for (it = paths.begin(); it != paths.end(); ++it) {
      EH_PATH* path = *it;
      STMTREP* call = path->Throw_stmt();
      CHECK_OBJ new_obj(call);
      // check var or vor
      if (hor != NULL) {
        // search vor chi list
        vor = _ctx.Vsa()->Find_hor_chi_vor(call, hor, fld, cr);
        CODEREP* opnd = _ctx.Vsa()->Find_vor_chi_cr(call, vor);
        // search vor mu list
        if (vor == NULL) {
          vor = _ctx.Vsa()->Find_hor_mu_vor(call, hor, fld, cr);
          opnd = _ctx.Vsa()->Find_vor_mu_cr(call, vor);
        }
        // not defined or referenced at this call site
        if (vor == NULL)
          continue;
        // pop original vor from vsym tracker
        _ctx.Tracker()->Pop();
        // push new vor to vsym tracker
        _ctx.Tracker()->Push(fld);
        new_obj.Update_vsym(vor, call, opnd);
      }
      else {
        CODEREP* opnd = NULL;
        // search chi list
        if (call->Chi_list()) {
          CHI_NODE *chi = call->Chi_list()->Search_chi_node(cr->Aux_id());
          if (chi && chi->Live())
            opnd = chi->RESULT();
        }
        // search mu list
        if (opnd == NULL && call->Mu_list()) {
          MU_NODE *mu = call->Mu_list()->Search_mu_node(cr->Aux_id());
          if (mu)
            opnd = mu->OPND();
        }
        // variable is defined and used in handler locally
        if (opnd == NULL)
          continue;
        new_obj.Update_var(opnd, call);
      }

      _spos.Set_cur_node(cur_node, i);
      _spos.Append_data(call, _ctx.Dna(), PATHINFO_EH_THROW);
      _spos.Add_bb(call->Bb());
      INT level = _spos.Push_value_graph();
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VG3: push EH path %d from %s %s at line %d to line %d in %s\n",
                       _checker.Checker_name(),
                       i,
                       OPERATOR_name(call->Opr()) + 4,
                       call->Opr() == OPR_CALL ? ST_name(call->St()) : "",
                       Srcpos_To_Line(call->Linenum()),
                       Srcpos_To_Line(sr->Linenum()),
                       _ctx.Dna()->Fname()));

      CHECKER_STATUS sts = new_obj.Is_vsym() ? Check_vsym_ud(new_obj) : Check_var_ud(new_obj);
      if (sts != CS_DONE)
        Continue_trav(new_obj, sts);

      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VG3: pop EH path %d from %s %s at line %d to line %d in %s\n",
                       _checker.Checker_name(),
                       i,
                       OPERATOR_name(call->Opr()) + 4,
                       call->Opr() == OPR_CALL ? ST_name(call->St()) : "",
                       Srcpos_To_Line(call->Linenum()),
                       Srcpos_To_Line(sr->Linenum()),
                       _ctx.Dna()->Fname()));
      _spos.Pop_value_graph(level);

      _spos.Pop_mark(_ctx.Comp_unit(), FALSE);
    }
    _spos.Pop_mark(_ctx.Comp_unit(), TRUE);
    return CS_DONE;
  }

  // Check_var_chi_ud
  // Check the U-D of var chi
  CHECKER_STATUS Check_var_chi_ud(CHECK_OBJ &obj)
  {
    CODEREP *cr = obj.Coderep();
    Is_True(cr->Kind() == CK_VAR && cr->Is_flag_set(CF_DEF_BY_CHI), ("bad cr"));
    STMTREP* sr = cr->Defstmt();
    Is_True(sr != NULL, ("bad stmt"));
    Is_Trace(_ctx.Tracing(), (TFile, " -%s: cr%d sym%dv%d def by chi:\n",
                              _checker.Checker_name(),
                              cr->Coderep_id(), cr->Aux_id(), cr->Version()));
    Is_Trace_cmd(_ctx.Tracing(), _ctx.Vsa()->Print_sr(sr, TFile));

    // ignore istore at first because it's not handled so far
    if (OPERATOR_is_store(sr->Opr()))
      return CS_DONE;

    // check control dependency to entry
    BB_NODE *prev_bb = obj.Stmtrep() ? obj.Stmtrep()->Bb() : obj.Bb();
    if (prev_bb != NULL) {
      BOOL vg_ret = _spos.Add_control_dependency(_ctx.Dna(), sr->Bb(), prev_bb);
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VGCD0: sr%d %s BB%d line %d ->  BB%d ==> %d\n",
                       _checker.Checker_name(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4,
                       sr->Bb()->Id(), Srcpos_To_Line(sr->Linenum()),
                       prev_bb->Id(),
                       vg_ret));
      if (vg_ret == FALSE)
        return CS_DONE;

      if (prev_bb != sr->Bb()) {
        if (sr->Opr() == OPR_OPT_CHI) {
          AUX_STAB_ENTRY *sym = _ctx.Opt_stab()->Aux_stab_entry(cr->Aux_id());
          // check may reachable if obj def by caller
          if (_ctx.Dna()->Is_param(cr) || sym->Is_global()) {
            if (!_spos.Is_def_reachable(prev_bb, sr)) {
              Is_Trace(_ctx.Tracing(),
                (TFile, "sr%d is unreachable from bb%d\n",
                        sr->Stmtrep_id(), prev_bb->Id()));
              return CS_DONE;
            }
          }
        } else if (OPERATOR_is_call(sr->Opr())) {
          if (!_spos.Is_def_reachable(prev_bb, sr)) {
            Is_Trace(_ctx.Tracing(),
              (TFile, "sr%d is unreachable from bb%d\n",
                      sr->Stmtrep_id(), prev_bb->Id()));
            return CS_DONE;
          }
        }
        Is_Trace(_ctx.Tracing(), (TFile, " -possible:%s\n",
                                  _spos.Cur_node()->Is_flag_set(SRCPOS_FLAG_MAYBE) ?
                                  "maybe" : "yes"));
      }
    }

    CHI_NODE* chi = cr->Defchi();
    if (sr->Opr() == OPR_OPT_CHI) {
      _spos.Append_stpath(sr, chi->OPND(), _ctx.Dna(), FALSE);
      // check if sr is handler entry
      Is_True(sr->Bb()->Kind() == BB_ENTRY, ("bb is not entry"));
      AUX_STAB_ENTRY *aux = _ctx.Opt_stab()->Aux_stab_entry(chi->Aux_id());
      // check exception handler if checker needs
      if (_CHECKER::FOLLOW_EH &&
          sr->Bb()->Labnam() > 0) {
        WN* ent = sr->Bb()->Entrywn();
        Is_True(ent != NULL &&
                WN_operator(ent) == OPR_LABEL &&
                WN_Label_Is_Handler_Begin(ent),
                ("not a eh label"));
        // done checking if the entry chi is return preg (for ex __Exc_Ptr__)
        BOOL is_return_preg = aux->Is_return_preg();
        if(is_return_preg) {
          Is_Trace(_ctx.Tracing(), (TFile, (" -cr is __Exc_Ptr__ Done.\n")));
          return CS_DONE;
        }
        return Check_eh_path(obj, sr);
      }

      obj.Set_stmtrep(sr);
      CHECKER_STATUS sts = _checker.template Check_stmtrep<OPR_OPT_CHI>(obj, &_ctx);
      if (sts != CS_CONT) {
        return sts;
      }
      IDTYPE parm_id = _ctx.Dna()->Is_param(cr);
      if (parm_id != INVALID_VAR_IDX) {
        // input parameter
        _spos.Append_data(sr, _ctx.Dna(), PATHINFO_PARM);
        return Check_caller_param_var(parm_id);
      }
      else if (aux->Is_global()) {
        // global var
        _spos.Append_data(sr, _ctx.Dna(), PATHINFO_CHI);
        return Check_caller_global_var(obj, cr);
      }
    }
    else if (sr->Opr() == OPR_INTRINSIC_CALL) {
      // special handling for intrinsic call
      _spos.Append_data(sr, _ctx.Dna(), PATHINFO_CALL_CHI);
      obj.Set_stmtrep(sr);
      CHECKER_STATUS ret_sts = _checker.template Check_stmtrep<OPR_INTRINSIC_CALL>(obj, &_ctx);
      if (ret_sts != CS_CONT) {
        return ret_sts;
      }
      CONTAINER_UD_HELPER helper;
      if (_CHECKER::SUSPECT & CS_CONTAINER_UD) {
          helper.Check_call(_checker, obj, &_ctx);
      }
      return CS_DONE;
    }
    else if (OPERATOR_is_call(sr->Opr())) {
      obj.Set_stmtrep(sr);
      if((_CHECKER::SUSPECT & CS_ICALL) && 
         sr->Callee_returns_new_heap_memory()) {
        // special handling for call
        _spos.Append_data(sr, _ctx.Dna(), PATHINFO_CALL_CHI);
        return _checker.template Check_stmtrep<OPR_CALL>(obj, &_ctx);
      }
      // return value or callee's side effect
      RNA_NODE *rna = _ctx.Vsa()->Sr_2_rna(sr);

      if (rna != NULL) {
        _spos.Append_data(sr, _ctx.Dna(), PATHINFO_CALL_CHI);
        if(_ctx.Ipsa()->Is_jni_call(rna)) {
          return Check_jni(obj);
        }
        else {
          if (_CHECKER::SUSPECT & CS_VAR_DEF) {
            CHECKER_STATUS ret = CS_OP;
            if(sr->Opr() == OPR_ICALL) {
              ret = _checker.template Check_stmtrep<OPR_ICALL>(obj, &_ctx);
            } else if(sr->Opr() == OPR_CALL) {
              ret = _checker.template Check_stmtrep<OPR_CALL>(obj, &_ctx);
            }
            if(ret != CS_CONT) {
              return ret;
            }
            // else fall through
          }
          AUX_STAB_ENTRY* aux = _ctx.Opt_stab()->Aux_stab_entry(cr->Aux_id());
          // check if return value
          if (aux->Is_return_preg()) {
            CONTAINER_UD_HELPER helper;
            if ((_CHECKER::SUSPECT & CS_CONTAINER_UD) &&
                helper.Check_call(_checker, obj, &_ctx) == CS_DONE)
              return CS_DONE;
            return Check_callee_return_var(rna, cr);
          }
          if (!aux->St())
            return CS_DONE;
          // check if output argument
          pair<IDTYPE, BOOL> arg = rna->Get_arg(chi->OPND(), _ctx.Vsa());
          if (arg.first != INVALID_VAR_IDX) {
            if (arg.second) {
              return Check_callee_output_var(rna, arg.first, obj);
            }
            else if (_ctx.Tracker()->Empty()) {
              obj.Update_var(chi->OPND(), sr);
              return CS_VAR_UD;
            }
            else {
              return Check_callee_input_var(rna, arg.first);
            }
          }
          // check if global variable
          else if (aux->Is_global()) {
            return Check_callee_global_var(rna, aux->St(), obj, chi);
          } else {
            Is_Trace(_ctx.Tracing(), (TFile, "--%s: Done, cr%d not arg or global\n", _checker.Checker_name(), cr->Coderep_id()));
          }
        }
      }
      else {
        // Assertion or for RVSA
        Is_Trace(_ctx.Tracing(), (TFile, "--%s: Done. def by unknown %s %s.\n",
                                         _checker.Checker_name(),
                                         OPERATOR_name(sr->Opr()) + 4,
                                         sr->Opr() == OPR_CALL ? ST_name(sr->St()) : ""));
      }
    }
    else {
      // TODO: istore, etc
    }
    return CS_DONE;
  }

  // Check_var_phi_ud
  // Check the U-D of var phi
  CHECKER_STATUS Check_var_phi_ud(CHECK_OBJ &obj)
  {
    if(Check_skip()) {
      return CS_DONE;
    }
    CODEREP *cr = obj.Coderep();
    Is_True(cr->Kind() == CK_VAR && cr->Is_flag_set(CF_DEF_BY_PHI), ("bad cr"));
    PHI_NODE *phi = cr->Defphi();
    Is_True(phi != NULL && phi->Live(), ("phi is dead"));
    Is_Trace(_ctx.Tracing(), (TFile, " -%s: cr%d sym%dv%d def by phi:\n",
                                     _checker.Checker_name(),
                                     cr->Coderep_id(), cr->Aux_id(), cr->Version()));
    Is_Trace_cmd(_ctx.Tracing(), phi->Print(TFile));

    if (_ctx.Visited(phi->Bb())) {
      Is_Trace(_ctx.Tracing(), (TFile, ("phi bb is visited, ret CS_DONE\n")));
      return CS_DONE;
    }

    if (obj.Stmtrep() != NULL) {
      STMTREP *prev = obj.Stmtrep();
      BOOL vg_ret = _spos.Add_control_dependency(_ctx.Dna(), phi->Bb(), prev->Bb());
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VGCD1: phi BB%d line %d -> sr%d %s BB%d line %d ==> %d\n",
                       _checker.Checker_name(),
                       phi->Bb()->Id(), Srcpos_To_Line(Get_bb_first_linenum(phi->Bb())),
                       prev->Stmtrep_id(), OPERATOR_name(prev->Opr()) + 4,
                       prev->Bb()->Id(), Srcpos_To_Line(prev->Linenum()),
                       vg_ret));
      if (vg_ret == FALSE)
        return CS_DONE;
    }

    _spos.Append_data(phi->Bb(), _ctx.Dna(), PATHINFO_PHI);
    SRCPOS_TREENODE* cur_node = _spos.Add_children(phi->Size());
    IDTYPE p_idx = _spos.Cur_idx();
    _spos.Push_mark(phi);
    // save stack before visiting phi operands
    VSYM_TRACKER::VSYM_VECTOR vsym_stack;
    _ctx.Tracker()->Save_stack(&vsym_stack);

    BB_NODE* bb_pred;
    BB_LIST_ITER bb_iter;
    INT i = 0;
    FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi->Bb()->Pred())) {
      CODEREP* opnd = phi->OPND(i);
      Is_Trace(_ctx.Tracing(), (TFile, "  Check phi opnd[%d/%d] cr%d ",
                                i, phi->Bb()->Pred()->Len(), opnd->Coderep_id()));
      if (!_spos.Is_path_possible(phi, i, cur_node)) {
        Is_Trace(_ctx.Tracing(), (TFile, " - skip as path is not possible\n"));
        ++ i;
        continue;
      }
      if (opnd->Is_flag_set(CF_IS_ZERO_VERSION)) {
        Is_Trace(_ctx.Tracing(), (TFile, " - skip as opnd is zero version\n"));
        ++ i;
        continue;
      }
      // value defined
      if (_CHECKER::SUSPECT & CS_INIT && opnd->Value_def()) {
        Is_Trace(_ctx.Tracing(), (TFile, " - skip as opnd value is defined\n"));
        ++i;
        continue;
      }
      _spos.Set_cur_node(cur_node, i);
      _spos.Add_bb(bb_pred);
      ++ i;

      Is_Trace(_ctx.Tracing(), (TFile, " - possible:%s\n",
                                _spos.Cur_node()->Is_flag_set(SRCPOS_FLAG_MAYBE) ?
                                "maybe" : "yes"));

      INT level = _spos.Push_value_graph();
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VG4: push cr%d = phi cr%d parm %d from BB%d:%d to BB%d:%d in %s\n",
                       _checker.Checker_name(),
                       cr->Coderep_id(), opnd->Coderep_id(),
                       i,
                       bb_pred->Id(), Srcpos_To_Line(Get_bb_last_linenum(bb_pred)),
                       phi->Bb()->Id(), Srcpos_To_Line(Get_bb_first_linenum(phi->Bb())),
                       _ctx.Dna()->Fname()));
      BOOL vg_ret = TRUE;
      vg_ret = _spos.Add_phi_opnd(_ctx.Dna(), phi, i - 1);
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VGPHI: cr%d = phi cr%d opnd %d ==> %d\n",
                        _checker.Checker_name(),
                        cr->Coderep_id(), opnd->Coderep_id(),
                        i - 1,
                        vg_ret));
#if 0  // added above
      if (cr != opnd && vg_ret) {
        vg_ret = _spos.Add_assign(_ctx.Vsa(), cr, _ctx.Vsa(), opnd);
        Is_Trace(_ctx.Tracing(),
                 (TFile, " -%s-VG4: cr%d = phi cr%d parm %d ==> %d\n",
                         _checker.Checker_name(),
                         cr->Coderep_id(), opnd->Coderep_id(),
                         i,
                         vg_ret));
      }
#endif

      if (vg_ret) {
        _spos.Append_data(bb_pred, _ctx.Dna(), PATHINFO_BRANCH);
        CHECK_OBJ opnd_obj(opnd, phi, bb_pred);
        CHECKER_STATUS sts = Check_coderep(opnd_obj);
        if (sts != CS_DONE)
          Continue_trav(opnd_obj, sts);
      }

      // restore stack after visiting phi operands
      _ctx.Tracker()->Restore_stack(&vsym_stack);

      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VG4: pop cr%d = phi cr%d parm %d from BB%d:%d to BB%d:%d in %s\n",
                       _checker.Checker_name(),
                       cr->Coderep_id(), opnd->Coderep_id(),
                       i,
                       bb_pred->Id(), Srcpos_To_Line(Get_bb_last_linenum(bb_pred)),
                       phi->Bb()->Id(), Srcpos_To_Line(Get_bb_first_linenum(phi->Bb())),
                       _ctx.Dna()->Fname()));
      _spos.Pop_value_graph(level);

      _spos.Pop_mark(phi, FALSE);
    }
    _spos.Reset_cur_node(cur_node, p_idx);
    _spos.Pop_mark(phi, TRUE);
    return CS_DONE;
  }

  // Check_var_def_ud
  // Check the U-D of stmt refined var
  CHECKER_STATUS Check_var_def_ud(CHECK_OBJ &obj)
  {
    CODEREP *cr = obj.Coderep();
    Is_True(cr->Kind() == CK_VAR &&
            !cr->Is_flag_set(CF_DEF_BY_PHI) && !cr->Is_flag_set(CF_DEF_BY_CHI),
            ("bad cr"));
    if (cr->Is_var_volatile() || cr->Is_flag_set(CF_IS_ZERO_VERSION)) {
      Is_Trace(_ctx.Tracing(), (TFile, " -%s: DONE. cr%d sym%dv%d is volatile or zero version.\n",
                                       _checker.Checker_name(),
                                       cr->Coderep_id(), cr->Aux_id(), cr->Version()));
      return CS_DONE;
    }

    STMTREP* sr = cr->Defstmt();
    Is_True(sr != NULL, ("bad stmt"));
    Is_Trace(_ctx.Tracing(), (TFile, " -%s: cr%d sym%dv%d def by stmt.\n",
                                     _checker.Checker_name(),
                                     cr->Coderep_id(), cr->Aux_id(), cr->Version()));
    Is_Trace_cmd(_ctx.Tracing(), sr->Print(TFile));

    // check control dependency
    if (obj.Stmtrep() != NULL) {
      STMTREP *prev = obj.Stmtrep();
      BOOL vg_ret = _spos.Add_control_dependency(_ctx.Dna(), sr->Bb(), prev->Bb());
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VGCD2: sr%d %s BB%d line %d -> sr%d %s BB%d line %d ==> %d\n",
                       _checker.Checker_name(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4,
                       sr->Bb()->Id(), Srcpos_To_Line(sr->Linenum()),
                       prev->Stmtrep_id(), OPERATOR_name(prev->Opr()) + 4,
                       prev->Bb()->Id(), Srcpos_To_Line(prev->Linenum()),
                       vg_ret));
      if (vg_ret == FALSE)
        return CS_DONE;
    }

    _spos.Append_data(sr, _ctx.Dna(), PATHINFO_COPY);
    _spos.Append_stpath(sr, sr->Rhs(), _ctx.Dna(), FALSE);
    obj.Update_var(sr->Rhs(), sr);
    return CS_OP;
  }

  // Check_vsym_chi_ud
  // Check the U-D of vsym which is defined by CHI
  CHECKER_STATUS Check_vsym_chi_ud(CHECK_OBJ &obj)
  {
    VSYM_OBJ_REP *vor = obj.Vor();
    Is_True(vor->Attr() == ROR_DEF_BY_CHI, ("invalid vor attr"));
    STMTREP *sr = vor->Stmt_def();
    // do not assert, as vor may not have define, report UIV?
    Is_Trace(_ctx.Tracing(), (TFile, " -%s: vo%dv%d def by chi:\n",
                                     _checker.Checker_name(),
                                     vor->Vsym_obj()->Id(), vor->Version()));
    if (sr != NULL) {
      Is_Trace_cmd(_ctx.Tracing(), _ctx.Vsa()->Print_sr(sr, TFile));
    }
    else {
      Is_Trace(_ctx.Tracing(), (TFile, "  CS_DONE:sr is NULL.\n"));
      return CS_DONE;
    }

    // check control dependency
    if (obj.Stmtrep() != NULL) {
      STMTREP *prev = obj.Stmtrep();
      BB_NODE *prev_bb = prev->Bb();
      BOOL vg_ret = _spos.Add_control_dependency(_ctx.Dna(), sr->Bb(), prev_bb);
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VGCD3: sr%d %s BB%d line %d -> sr%d %s BB%d line %d ==> %d\n",
                       _checker.Checker_name(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4,
                       sr->Bb()->Id(), Srcpos_To_Line(sr->Linenum()),
                       prev->Stmtrep_id(), OPERATOR_name(prev->Opr()) + 4,
                       prev_bb->Id(), Srcpos_To_Line(prev->Linenum()),
                       vg_ret));
      if (vg_ret == FALSE)
        return CS_DONE;

      if (prev_bb != sr->Bb()) {
        if (sr->Opr() == OPR_OPT_CHI) {
          CODEREP *base = _ctx.Vsa()->Find_vor_chi_cr(sr, vor);
          if (base) {
            AUX_STAB_ENTRY *sym = _ctx.Opt_stab()->Aux_stab_entry(base->Aux_id());
            // check may reachable if obj def by caller
            if (_ctx.Dna()->Is_param(base) || sym->Is_global()) {
              if (!_spos.Is_def_reachable(prev_bb, sr)) {
                Is_Trace(_ctx.Tracing(),
                  (TFile, "sr%d is unreachable from bb%d\n",
                          sr->Stmtrep_id(), prev_bb->Id()));
                return CS_DONE;
              }
            }
          }
        }
        else if (OPERATOR_is_call(sr->Opr())) {
          if (!_spos.Is_def_reachable(prev_bb, sr)) {
            Is_Trace(_ctx.Tracing(),
              (TFile, "  sr%d is unreachable from bb%d\n",
                      sr->Stmtrep_id(), prev_bb->Id()));
            return CS_DONE;
          }
        }
        Is_Trace(_ctx.Tracing(), (TFile, " -possible:%s\n",
                                  _spos.Cur_node()->Is_flag_set(SRCPOS_FLAG_MAYBE) ?
                                  "maybe" : "yes"));
      }
    }

    if (vor->Is_entry_chi() ||
        (sr && sr->Opr() == OPR_OPT_CHI)) {
      // check if sr is handler entry
      Is_True(sr->Bb()->Kind() == BB_ENTRY, ("bb is not entry"));
      // check exception handler if checker needs
      if (_CHECKER::FOLLOW_EH &&
          sr->Bb()->Labnam() > 0) {
        WN* ent = sr->Bb()->Entrywn();
        Is_True(ent != NULL &&
                WN_operator(ent) == OPR_LABEL &&
                WN_Label_Is_Handler_Begin(ent),
                ("not a eh label"));
        return Check_eh_path(obj, sr);
      }
      // parameter, global from caller
      CODEREP* base = _ctx.Vsa()->Find_vor_chi_cr(sr, vor);
      if (base == NULL)
        return CS_DONE;

      _spos.Append_data(sr, base, _ctx.Dna(), PATHINFO_CHI);
      obj.Update_vsym(vor, sr, base);
      CHECKER_STATUS sts = _checker.template Check_stmtrep<OPR_OPT_CHI>(obj, &_ctx);
      if (sts != CS_CONT) {
        return sts;
      }

      IDTYPE parm = _ctx.Dna()->Is_param(base);
      if (parm != INVALID_VAR_IDX)
        return Check_caller_param_vsym(sr, parm, base, vor);
      else
        return Check_caller_global_vsym(obj, base, vor);
    }
    else if (OPERATOR_is_call(sr->Opr())) {
      obj.Set_stmtrep(sr);
      RNA_NODE *rna = _ctx.Vsa()->Sr_2_rna(sr);
      if (rna != NULL) {
        _spos.Append_data(sr, _ctx.Dna(), PATHINFO_CALL_CHI);
        if(_ctx.Ipsa()->Is_jni_call(rna)) {
          return Check_jni(obj);
        } else {
          CODEREP* cr = _ctx.Vsa()->Find_vor_chi_cr(sr, vor);
          if (cr == NULL) {
            Is_Trace(_ctx.Tracing(), (TFile, " -%s: not find cr for vor ",
                                             _checker.Checker_name()));
            Is_Trace_cmd(_ctx.Tracing(), vor->Print(TFile));
            Is_Trace(_ctx.Tracing(), (TFile, " on OPT_CHI.\n"));
            Is_Trace_cmd(_ctx.Tracing(), _ctx.Vsa()->Print_sr(sr, TFile));
            return CS_DONE;
          }
          obj.Update_vsym(vor, sr, cr);
          if (_CHECKER::SUSPECT & CS_VAR_DEF) {
            CHECKER_STATUS ret = CS_OP;
            if(sr->Opr() == OPR_ICALL) {
              ret = _checker.template Check_stmtrep<OPR_ICALL>(obj, &_ctx);
            } else if(sr->Opr() == OPR_CALL) {
              ret = _checker.template Check_stmtrep<OPR_CALL>(obj, &_ctx);
            } else if(sr->Opr() == OPR_INTRINSIC_CALL) {
              ret = _checker.template Check_stmtrep<OPR_INTRINSIC_CALL>(obj, &_ctx);
            }
            if(ret != CS_CONT) {
              return ret;
            }
            // else fall through
          }

          AUX_STAB_ENTRY* aux = NULL;
          if (cr->Kind() == CK_VAR) {
            aux = _ctx.Opt_stab()->Aux_stab_entry(cr->Aux_id());
            // check if return value
            if (aux->Is_return_preg()) {
              CONTAINER_UD_HELPER helper;
              if ((_CHECKER::SUSPECT & CS_CONTAINER_UD) &&
                  helper.Check_call(_checker, obj, &_ctx) == CS_DONE)
                return CS_DONE;
              return Check_callee_return_vsym(rna, vor, cr);
            }
            if (!aux->St())
              return CS_DONE;
          }
          // check if parameter
          pair<IDTYPE, BOOL> arg = rna->Get_arg(cr, _ctx.Vsa());
          if (arg.first != INVALID_VAR_IDX) {
            if (arg.second)
              return Check_callee_output_vsym(rna, arg.first, cr, vor);
            else
              return Check_callee_input_vsym(rna, arg.first, cr, vor);
          }
          if (aux && aux->Is_global())
            return Check_callee_global_vsym(rna, aux->St(), vor);
        }
      }
    }
    else if (OPERATOR_is_store(sr->Opr())) {
      // aliased store, try follow the D-U of RHS
      _spos.Add_bb(sr->Bb());
      _spos.Append_data(sr, sr->Rhs(), _ctx.Dna(), PATHINFO_ISTORE);
      _spos.Set_flag(SRCPOS_FLAG_MAYBE);
      HEAP_OBJ_REP *base_hor = vor->Vsym_obj()->Base_hor();
      // if the vor def stmt is the base hor stmt def, can't pop vfr
      if (!(base_hor && base_hor->Has_defstmt() && base_hor->Defstmt() == sr)) {
        _ctx.Tracker()->Pop();
      }
      obj.Update_var(sr->Rhs(), sr);
      return CS_OP;
    }
    else {
      Is_True(FALSE, ("TODO: Handle %s for vsym chi def", OPERATOR_name(sr->Opr()) + 4));
    }
    return CS_DONE;
  }

  // Check_vsym_phi_ud
  // Check the U-D of vsym which is defined by PHI
  CHECKER_STATUS Check_vsym_phi_ud(CHECK_OBJ &obj)
  {
    if(Check_skip()) {
      return CS_DONE;
    }
    VSYM_OBJ_REP *vor = obj.Vor();
    Is_True(vor->Attr() == ROR_DEF_BY_PHI ||
            vor->Attr() == ROR_DEF_BY_HORPHI, ("invalid vor attr"));
    VSYM_TRACKER *tracker = _ctx.Tracker();
    PHI_NODE* phi = vor->Phi_def();
    Is_True(phi != NULL, ("bad vor phi"));
    Is_Trace(_ctx.Tracing(), (TFile, " -%s: vo%dv%d def by phi:\n",
                                     _checker.Checker_name(),
                                     vor->Vsym_obj()->Id(), vor->Version()));
    Is_Trace_cmd(_ctx.Tracing(), _ctx.Vsa()->Print_vor_phi(phi, TFile));

    if (_ctx.Visited(phi->Bb())) {
      Is_Trace(_ctx.Tracing(), (TFile, "  ignored. phi already visited.\n"));
      return CS_DONE;
    }

    if (obj.Stmtrep() != NULL) {
      STMTREP *prev = obj.Stmtrep();
      BOOL vg_ret = _spos.Add_control_dependency(_ctx.Dna(), phi->Bb(), prev->Bb());
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VGCD4: phi BB%d line %d -> sr%d %s BB%d line %d ==> %d\n",
                       _checker.Checker_name(),
                       phi->Bb()->Id(), Srcpos_To_Line(Get_bb_first_linenum(phi->Bb())),
                       prev->Stmtrep_id(), OPERATOR_name(prev->Opr()) + 4,
                       prev->Bb()->Id(), Srcpos_To_Line(prev->Linenum()),
                       vg_ret));
      if (vg_ret == FALSE)
        return CS_DONE;
    }

    _spos.Append_data(phi->Bb(), _ctx.Dna(), PATHINFO_PHI);
    IDTYPE p_idx = _spos.Cur_idx();
    SRCPOS_TREENODE* cur_node = _spos.Add_children(phi->Size());
    _spos.Push_mark(phi);
    // save stack before visiting phi operands
    VSYM_TRACKER::VSYM_VECTOR vsym_stack;
    tracker->Save_stack(&vsym_stack);

    BB_NODE* bb_pred;
    BB_LIST_ITER bb_iter;
    INT32 i = 0;
    FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi->Bb()->Pred())) {
      VSYM_OBJ_REP *vor_opnd = (VSYM_OBJ_REP*) phi->OPND(i);
      Is_True(vor_opnd != NULL, ("vor phi opnd is null"));
      Is_Trace(_ctx.Tracing(), (TFile, "  Check phi opnd[%d/%d]: vo%dv%d",
                                i, phi->Bb()->Pred()->Len(),
                                vor_opnd->Vsym_obj()->Id(), vor_opnd->Version()));
      if (!_spos.Is_path_possible(phi, i, cur_node)) {
        ++ i;
        Is_Trace(_ctx.Tracing(), (TFile, " - skip as path not possible\n"));
        continue;
      }
      if (_ctx.Vsa()->Is_special_vor(vor_opnd)) {
        ++ i;
        Is_Trace(_ctx.Tracing(), (TFile, " - skip as opnd is special vor\n"));
        continue;
      }

      _spos.Set_cur_node(cur_node, i);
      Is_Trace(_ctx.Tracing(), (TFile, " - possible:%s\n",
                                _spos.Cur_node()->Is_flag_set(SRCPOS_FLAG_MAYBE) ?
                                "maybe" : "yes"));
      _spos.Add_bb(bb_pred);
      ++ i;

      // the vor phi is caused by alias, find the current version of vor_opnd on current check sr
      if (obj.Stmtrep() &&
          vor_opnd->Vsym_obj()->Base_hor() != vor->Vsym_obj()->Base_hor()) {
        VSYM_OBJ_REP *cur_opnd = _ctx.Vsa()->Find_stmt_cur_vor(obj.Stmtrep(), vor_opnd->Vsym_obj());
        if (cur_opnd) {
          BOOL is_phi = cur_opnd->Attr() == ROR_DEF_BY_HORPHI || cur_opnd->Attr() == ROR_DEF_BY_PHI;
          // do not adjust if cur_opnd def by same phi
          if (!(is_phi && cur_opnd->Phi_def()->Bb() == phi->Bb())) {
            vor_opnd = cur_opnd;
            Is_Trace(_ctx.Tracing(), (TFile, " adjust to aliased vor \n"));
          }
        }
      }
      INT level = _spos.Push_value_graph();
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VG5: push vo%dv%d = phi vo%dv%d parm %d from BB%d:%d to BB%d:%d in %s\n",
                       _checker.Checker_name(),
                       vor->Vsym_obj()->Id(), vor->Version(),
                       vor_opnd->Vsym_obj()->Id(), vor_opnd->Version(),
                       i,
                       bb_pred->Id(), Srcpos_To_Line(Get_bb_last_linenum(bb_pred)),
                       phi->Bb()->Id(), Srcpos_To_Line(Get_bb_first_linenum(phi->Bb())),
                       _ctx.Dna()->Fname()));

      BOOL vg_ret = _spos.Add_phi_opnd(_ctx.Dna(), phi, i - 1);
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VGPHI: vo%dv%d = phi vo%dv%d OPND %d ==> %d\n",
                        _checker.Checker_name(),
                        vor->Vsym_obj()->Id(), vor->Version(),
                        vor_opnd->Vsym_obj()->Id(), vor_opnd->Version(),
                        i - 1,
                        vg_ret));
#if 0 // added above
      if (vg_ret && vor != vor_opnd && !_ctx.Vsa()->Is_special_vor(vor_opnd)) {
        vg_ret = _spos.Add_assign(_ctx.Vsa(), vor, _ctx.Vsa(), vor_opnd);
        Is_Trace(_ctx.Tracing(),
                 (TFile, " -%s-VG5: vo%dv%d = phi vo%dv%d parm %d ==> %d\n",
                         _checker.Checker_name(),
                         vor->Vsym_obj()->Id(), vor->Version(),
                         vor_opnd->Vsym_obj()->Id(), vor_opnd->Version(),
                         i,
                         vg_ret));
      }
#endif

      if (vg_ret) {
        _spos.Append_data(bb_pred, _ctx.Dna(), PATHINFO_BRANCH);
        if (vor_opnd->Vsym_obj()->Fld_rep().Is_any()) {
          // we have a phi def on any vor. since we don't track the range of
          // use and def vor, mark the issue maybe
          _spos.Set_flag(SRCPOS_FLAG_MAYBE);
        }
        CHECK_OBJ opnd_obj(vor_opnd, phi, bb_pred);
        CHECKER_STATUS sts = Check_vsym_ud(opnd_obj);
        if (sts != CS_DONE)
          Continue_trav(opnd_obj, sts);
      }

      // restore stack after visiting phi operands
      tracker->Restore_stack(&vsym_stack);

      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VG5: pop vo%dv%d = phi vo%dv%d parm %d from BB%d:%d to BB%d:%d in %s\n",
                       _checker.Checker_name(),
                       vor->Vsym_obj()->Id(), vor->Version(),
                       vor_opnd->Vsym_obj()->Id(), vor_opnd->Version(),
                       i,
                       bb_pred->Id(), Srcpos_To_Line(Get_bb_last_linenum(bb_pred)),
                       phi->Bb()->Id(), Srcpos_To_Line(Get_bb_first_linenum(phi->Bb())),
                       _ctx.Dna()->Fname()));
      _spos.Pop_value_graph(level);
      _spos.Pop_mark(phi, FALSE);
    }
    _spos.Reset_cur_node(cur_node, p_idx);
    _spos.Pop_mark(phi, TRUE);
    return CS_DONE;
  }

  // Check_vsym_istore_ud
  // Check the U-D of vsym which is defined by ISTORE
  CHECKER_STATUS Check_vsym_istore_ud(CHECK_OBJ& obj)
  {
    VSYM_OBJ_REP *vor = obj.Vor();
    Is_True(vor->Attr() == ROR_DEF_BY_ISTORE, ("invalid vor attr"));
    STMTREP* sr = vor->Stmt_def();
    Is_True(sr != NULL && sr->Rhs() != NULL && OPERATOR_is_store(sr->Opr()),
            ("invalid vor def stmt"));

    Is_Trace(_ctx.Tracing(), (TFile, " -%s: vo%dv%d def by istore:\n",
                                     _checker.Checker_name(),
                                     vor->Vsym_obj()->Id(), vor->Version()));
    Is_Trace_cmd(_ctx.Tracing(), sr->Print(TFile));

    // check control dependency
    if (obj.Stmtrep() != NULL) {
      STMTREP *prev = obj.Stmtrep();
      BOOL vg_ret = _spos.Add_control_dependency(_ctx.Dna(), sr->Bb(), prev->Bb());
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VGCD5: sr%d %s BB%d line %d -> sr%d %s BB%d line %d ==> %d\n",
                       _checker.Checker_name(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4,
                       sr->Bb()->Id(), Srcpos_To_Line(sr->Linenum()),
                       prev->Stmtrep_id(), OPERATOR_name(prev->Opr()) + 4,
                       prev->Bb()->Id(), Srcpos_To_Line(prev->Linenum()),
                       vg_ret));
      if (vg_ret == FALSE)
        return CS_DONE;
    }

    _ctx.Tracker()->Pop();
    _spos.Append_data(sr, sr->Rhs(), _ctx.Dna(), PATHINFO_ISTORE);
    // follow vor mu to traverse field vor
    if (!_ctx.Tracker()->Empty()) {
      HEAP_OBJ_REP *rhs_hor = _ctx.Vsa()->Find_stmt_hor_mu(sr, sr->Rhs());
      if (rhs_hor) {
        BOOL maybe = FALSE;
        VSYM_OBJ_REP *vor = _ctx.Tracker()->Compress(_ctx.Vsa(), rhs_hor, sr, sr->Rhs(), TRUE, maybe);
        if (vor) {
          obj.Update_vsym(vor, sr);
          if (maybe) {
            _spos.Set_flag(SRCPOS_FLAG_MAYBE);
          }
          return CS_VSYM_UD;
        }
      }
    }
    // check if sr modified vor partially
    VSA_ACCESS_INFO info;
    if (vor->Vsym_obj()->Fld_rep().Is_any() &&
        _ctx.Comp_unit()->Analyze_access_info(sr, sr->Lhs(), &info) == TRUE) {
      if (obj.Access_info().Subtract(info) > 0) {
        MU_NODE *mu = _ctx.Vsa()->Find_vor_mu(sr, vor->Vsym_obj());
        if (mu != NULL && _ctx.Vsa()->Vor_access_whole_ho(sr, sr->Lhs(), vor) == 0) {
          VSYM_OBJ_REP *opnd = ((CVOR*)mu->OPND())->first;
          Is_True(opnd->Vsym_obj() == vor->Vsym_obj(), ("vsym_obj mismatch"));
          Is_Trace(_ctx.Tracing(), (TFile, " -%s: vo%dv%d <- vo%dv%d partial def by istore:\n",
                                           _checker.Checker_name(),
                                           vor->Vsym_obj()->Id(), opnd->Version(),
                                           opnd->Vsym_obj()->Id(), opnd->Version()));
          Is_Trace_cmd(_ctx.Tracing(), sr->Print(TFile));

          CHECK_OBJ opnd_obj(obj);
          VSYM_TRACKER::VSYM_VECTOR vsym_stack;
          _ctx.Tracker()->Save_stack(&vsym_stack);
          _ctx.Tracker()->Push(opnd->Vsym_obj()->Fld_rep_ptr());
          opnd_obj.Update_vsym(opnd, sr, sr->Lhs());
          Continue_trav(opnd_obj, CS_VSYM_UD);
          _ctx.Tracker()->Restore_stack(&vsym_stack);
        }
      }
    }
    obj.Update_var(sr->Rhs(), sr);
    return CS_OP;
  }

  // Check_vsym_copy
  // Check the U-D of vsym which is defined by STID, lhs is struct field
  CHECKER_STATUS Check_vsym_copy(CHECK_OBJ& obj)
  {
    VSYM_OBJ_REP *vor = obj.Vor();
    Is_True_Ret(vor->Attr() == ROR_DEF_BY_COPY, ("invalid vor attr"), CS_DONE);
    STMTREP* sr = vor->Stmt_def();
    Is_True(sr != NULL && sr->Rhs() != NULL && OPERATOR_is_store(sr->Opr()),
            ("invalid vor def stmt"));

    Is_Trace(_ctx.Tracing(), (TFile, " -%s: vo%dv%d def by istore:\n",
                                     _checker.Checker_name(),
                                     vor->Vsym_obj()->Id(), vor->Version()));
    Is_Trace_cmd(_ctx.Tracing(), sr->Print(TFile));

    // check control dependency
    if (obj.Stmtrep() != NULL) {
      STMTREP *prev = obj.Stmtrep();
      BOOL vg_ret = _spos.Add_control_dependency(_ctx.Dna(), sr->Bb(), prev->Bb());
      Is_Trace(_ctx.Tracing(),
               (TFile, " -%s-VGCD6: sr%d %s BB%d line %d -> sr%d %s BB%d line %d ==> %d\n",
                       _checker.Checker_name(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4,
                       sr->Bb()->Id(), Srcpos_To_Line(sr->Linenum()),
                       prev->Stmtrep_id(), OPERATOR_name(prev->Opr()) + 4,
                       prev->Bb()->Id(), Srcpos_To_Line(prev->Linenum()),
                       vg_ret));
      if (vg_ret == FALSE)
        return CS_DONE;
    }

    _ctx.Tracker()->Pop();
    _spos.Append_data(sr, sr->Rhs(), _ctx.Dna(), PATHINFO_ISTORE);
    obj.Update_var(sr->Rhs(), sr);
    return CS_OP;
  }

  // Check_coderep
  // fetch current cr & sr from context and invoke checker's Check_coderep method
  CHECKER_STATUS Check_coderep(CHECK_OBJ &obj)
  {
    CODEREP *cr = obj.Coderep();
    Is_True(cr != NULL, ("invalid cr"));
    Is_Trace(_ctx.Tracing(), (TFile, " -%s: Checking cr:\n",
                                     _checker.Checker_name()));
    Is_Trace_cmd(_ctx.Tracing(), cr->Print(TFile));
    switch (obj.Coderep()->Kind()) {
    case CK_LDA:
      return _checker.template Check_coderep<CK_LDA>(obj, &_ctx);
    case CK_CONST:
      return _checker.template Check_coderep<CK_CONST>(obj, &_ctx);
    case CK_RCONST:
      return _checker.template Check_coderep<CK_RCONST>(obj, &_ctx);
    case CK_OP:
      return _checker.template Check_coderep<CK_OP>(obj, &_ctx);
    case CK_VAR:
      if (cr->Is_var_volatile())
        return CS_DONE;
      return _checker.template Check_coderep<CK_VAR>(obj, &_ctx);
    case CK_IVAR:
      if (cr->Is_ivar_volatile())
        return CS_DONE;
      return _checker.template Check_coderep<CK_IVAR>(obj, &_ctx);
    default:
      Is_True(FALSE, ("unknown cr kind"));
      return CS_DONE;
    }
  }

  // Check_var_ud
  // fetch VAR from context and check the U-D of the var
  CHECKER_STATUS Check_var_ud(CHECK_OBJ &obj)
  {
    CODEREP* cr = obj.Coderep();
    Is_True(cr != NULL && cr->Kind() == CK_VAR, ("not var"));
    Is_Trace(_ctx.Tracing(), (TFile, " -%s: Checking var U-D for cr:\n",
                                     _checker.Checker_name()));
    Is_Trace_cmd(_ctx.Tracing(), cr->Print(TFile));

    if (cr->Is_flag_set(CF_DEF_BY_CHI))
      return Check_var_chi_ud(obj);
    else if (cr->Is_flag_set(CF_DEF_BY_PHI))
      return Check_var_phi_ud(obj);
    else
      return Check_var_def_ud(obj);
  }

  // Check_ivar_ud
  // fetch IVAR from context and check the U-D of the ivar
  CHECKER_STATUS Check_ivar_ud(CHECK_OBJ &obj)
  {
    CODEREP* cr = obj.Coderep();
    STMTREP* sr = obj.Stmtrep();
    Is_True(cr->Kind() == CK_IVAR && cr->Opr() != OPR_PARM, ("invalid ivar"));
    Is_Trace(_ctx.Tracing(), (TFile, " -%s: Checking ivar U-D for cr%d:\n",
                                     _checker.Checker_name(), cr->Coderep_id()));
    Is_Trace_cmd(_ctx.Tracing(), cr->Print(TFile));

    VSYM_OBJ_REP* vor = _ctx.Vsa()->Cr_2_vor(cr);
    if (vor == NULL || _ctx.Vsa()->Is_special_vor(vor)) {
      Is_Trace(_ctx.Tracing(), (TFile, " -%s: Done. invalid vor %s for cr%d:\n",
                                       _checker.Checker_name(),
                                        vor ? "special" : "null",
                                       cr->Coderep_id()));
      Is_Trace_cmd(_ctx.Tracing(), cr->Print(TFile));
      return CS_DONE;
    }
    // New hovo model create vor for ivar
    if (VSA_New_HVA) {
      VSYM_FLD_REP vfr = _ctx.Vsa()->Cr_vfr(cr);
      Is_True_Ret(vor != NULL &&
                  (vor->Vsym_obj()->Fld_rep().Is_any() ||
                   vor->Vsym_obj()->Fld_rep().Match(&vfr)),
                  ("null vor or field id mismatch"), CS_DONE);
      _ctx.Tracker()->Push(vor->Vsym_obj()->Fld_rep_ptr());
    } else {
      IDTYPE fldid = (vor)?
        _ctx.Vsa()->Synthesize_fldid(vor->Vsym_obj()->Base_hor(), cr->I_field_id(), cr->Offset()):
        cr->I_field_id();
      VSYM_FLD_REP *vfr = CXX_NEW(VSYM_FLD_REP(FLD_K_ID, fldid, _ctx.Vsa()->Cr_ofst(cr)), _ctx.Mem_pool());
      while (vor == NULL) {
        Is_True(cr->Kind() == CK_IVAR && cr->Opr() != OPR_PARM,
                ("invalid cr"));
        _ctx.Tracker()->Push(vfr);
        CODEREP* base = (cr == sr->Lhs()) ? cr->Istr_base() : cr->Ilod_base();
        CODEREP* base_cr = Find_ilod_base(base);
        if (base_cr == NULL) {
          Is_Trace(_ctx.Tracing(), (TFile, " -%s: Done. missing base for cr%d:\n",
                                          _checker.Checker_name(), cr->Coderep_id()));
          Is_Trace_cmd(_ctx.Tracing(), cr->Print(TFile));
          return CS_DONE;
        }
        Is_True(base_cr != NULL, ("invalid base cr"));
        if (base_cr->Kind() == CK_LDA) {
          MU_NODE* mu = cr->Ivar_mu_node();
          if (mu == NULL || mu->Aux_id() != base_cr->Lda_aux_id()) {
            Is_Trace(_ctx.Tracing(), (TFile, " -%s: Done. missing mu for cr%d:\n",
                                            _checker.Checker_name(), cr->Coderep_id()));
            Is_Trace_cmd(_ctx.Tracing(), cr->Print(TFile));
            return CS_DONE;
          }
          Is_True(mu->OPND() != NULL, ("invalid mu node"));
          base_cr = mu->OPND();
        }
        if (base_cr->Kind() == CK_VAR) {
          //Is_True(FALSE, ("should not happen"));
          obj.Update_var(base_cr, sr);
          return CS_VAR_UD;
        }
        cr = base_cr;
        Is_True(cr->Kind() == CK_IVAR, ("not ivar"));
        vor = _ctx.Vsa()->Cr_2_vor(cr);
        fldid = (vor)?
          _ctx.Vsa()->Synthesize_fldid(vor->Vsym_obj()->Base_hor(), cr->I_field_id(), cr->Offset()) :
          cr->I_field_id();
        vfr = CXX_NEW(VSYM_FLD_REP(FLD_K_ID, fldid, _ctx.Vsa()->Cr_ofst(cr)), _ctx.Mem_pool());
      }
      Is_True(vor != NULL &&
              (vor == _ctx.Vsa()->Null_vor() ||
              vor->Vsym_obj()->Fld_rep().Match(vfr)),
              ("field id mismatch"));
      _ctx.Tracker()->Push(vfr);
    }
    obj.Update_vsym(vor, sr, cr);
    return CS_VSYM_UD;
  }

  // Check_vsym_ud
  // fetch VOR from context and check the U-D of the vsym
  CHECKER_STATUS Check_vsym_ud(CHECK_OBJ &obj)
  {
    VSYM_OBJ_REP *vor = obj.Vor();
    Is_True_Ret(vor && !_ctx.Vsa()->Is_special_vor(vor),
                ("CHECK_OBJ vor is null"), CS_DONE);
    Is_Trace(_ctx.Tracing(), (TFile, " -%s: Checking vsym U-D for ",
                                     _checker.Checker_name()));
    Is_Trace_cmd(_ctx.Tracing(), vor->Print(TFile));
    Is_Trace(_ctx.Tracing(), (TFile, " :\n"));

    if(_CHECKER::SUSPECT & CS_VSYM_OBJ) {
      CHECKER_STATUS sts = _checker.Check_vsym_obj(obj, &_ctx);
      // fall through if CS_CONT
      if(sts != CS_CONT) {
        return sts;
      }
    }

    switch (vor->Attr()) {
    case ROR_DEF_BY_CHI:
      return Check_vsym_chi_ud(obj);
    case ROR_DEF_BY_PHI:
    case ROR_DEF_BY_HORPHI:
      return Check_vsym_phi_ud(obj);
    case ROR_DEF_BY_ISTORE:
      return Check_vsym_istore_ud(obj);
    case ROR_DEF_BY_COPY:
      return Check_vsym_copy(obj);
    case ROR_DEF_BY_NONE:
    default:
      // Assertion or for RVSA
      Is_Trace(_ctx.Tracing(), (TFile, " -Done: unsupported vor Attr:%d\n", vor->Attr()));
      return CS_DONE;
    }
  }

  // Check_jni
  // Evaluate JNI side effect during U-D traversal
  CHECKER_STATUS Check_jni(CHECK_OBJ &obj)
  {
    STMTREP *sr = obj.Stmtrep();
    if((_CHECKER::SUSPECT & CS_VAR_DEF) &&
        sr->Opr() == OPR_CALL && 
       strcmp(ST_name(sr->St()), "GetFieldID") == 0) {
      obj.Update_var(sr->Rhs(), sr);
      return _checker.template Check_stmtrep<OPR_CALL>(obj, &_ctx);
    }
    JNI_CHECKER_HELPER jni_helper(_ctx);
    return jni_helper.Check_jni(obj);
  }

public:
  // Continue_trav
  // Continue the traversal according to CHECKER_STATUS
  CHECKER_STATUS Continue_trav(CHECK_OBJ &obj, CHECKER_STATUS sts)
  {
    do {
      switch (sts) {
      case CS_DONE:
        break;
      case CS_OP:
        sts = Check_coderep(obj);
        break;
      case CS_VAR_UD:
        sts = Check_var_ud(obj);
        break;
      case CS_IVAR_UD:
        sts = Check_ivar_ud(obj);
        break;
      case CS_VSYM_UD:
        sts = Check_vsym_ud(obj);
        break;
      case CS_CONT:
      default:
        Is_True(FALSE, ("unknown status"));
        sts = CS_DONE;
      }
    } while (sts != CS_DONE);
    return sts;
  }
};

// ====================================================================
// CHECKER_TRAVELER
// A temporary traveler to traverse all STMTREP and CODEREP to find
// candidates and start U-D traversal
// ====================================================================
template<typename _CHECKER>
class CHECKER_TRAVELER {
  friend class VSA;
  // record how many times the pointer is reported and skip the issue
  // if the counter exceeds the threshold
  typedef pair<uintptr_t, INT> PTR_COUNTER_PAIR;
  typedef hash_map<uintptr_t, INT,
                   __gnu_cxx::hash<uintptr_t>,
                   std::equal_to<uintptr_t>,
                   mempool_allocator<PTR_COUNTER_PAIR> > PTR_COUNTER_MAP;
  typedef hash_set<uintptr_t,
                   __gnu_cxx::hash<uintptr_t>,
                   std::equal_to<uintptr_t>,
                   mempool_allocator<uintptr_t> >        PTR_VISITED_SET;

private:
  CXX_MEM_POOL             _trav_pool;   // traveler local pool
  COMP_UNIT               *_cu;          // compile unit
  PTR_COUNTER_MAP         *_ptr_counter; // ptr checked counter
  PTR_VISITED_SET         *_ptr_checked; // ptr checked set

public:
  CHECKER_TRAVELER(COMP_UNIT *cu)
    : _trav_pool("CHECKER_TRAVELER local pool", FALSE), _cu(cu)
  {
    _ptr_counter = CXX_NEW(PTR_COUNTER_MAP(7,
                                           __gnu_cxx::hash<uintptr_t>(),
                                           std::equal_to<uintptr_t>(),
                                           mempool_allocator<PTR_COUNTER_PAIR>(_trav_pool())),
                           _trav_pool());
    Is_True(_ptr_counter != NULL, ("CHECKER_TRAVELER out of memory"));

    _ptr_checked = CXX_NEW(PTR_VISITED_SET(7,
                                           __gnu_cxx::hash<uintptr_t>(),
                                           std::equal_to<uintptr_t>(),
                                           mempool_allocator<uintptr_t>(_trav_pool())),
                           _trav_pool());
  }

  VSA* Vsa() { return _cu->Vsa(); }

  // check if cr has been checked before
  BOOL Coderep_checked(CODEREP *cr) {
    return !_ptr_checked->insert((uintptr_t)cr).second;
  }

  // check if sr is from inlined lib function
  BOOL Is_inlined_lib_func(STMTREP *sr) const {
    INLCXT *inlcxt = sr->Bb()->Inlinecxt();
    return inlcxt && Is_lib_func(ST_name(inlcxt->Inlcxt_call_st()));
  }

  // check if cr/sr should be reported (not exceed the threshold)
  BOOL Issue_reported(STMTREP *sr, CODEREP *cr, INT kind) {
    if (VSA_Checker_Max_Path <= 0)
      return FALSE;
    uintptr_t key = (uintptr_t)sr << 32 | (uint32_t)(uintptr_t)cr | kind;
    INT &cnt = (*_ptr_counter)[key];
    ++cnt;
    return cnt > VSA_Checker_Max_Path ? TRUE : FALSE;
  }

  // check if file/st should be reported (not exceed the threshold)
  BOOL Issue_reported(UINT32 file_idx, ST *st, INT kind) {
    if (VSA_Checker_Max_Path <= 0)
      return FALSE;
    uintptr_t key = (uintptr_t)file_idx << 24 | (uintptr_t)st | kind;
    INT &cnt = (*_ptr_counter)[key];
    ++cnt;
    return cnt > VSA_Checker_Max_Path ? TRUE : FALSE;
  }

private:

  void Trace_start(_CHECKER *checker, CHECK_OBJ &obj, TRAV_CONTEXT *ctx, FILE *fp)
  {
    STMTREP *sr = obj.Stmtrep();
    SRCPOS cur_line = sr ? sr->Linenum() : 0 ;
    if (obj.Is_var()) {
      fprintf(fp, "\n##%s: Find candidates for cr%d at line %d ",
                   checker->Checker_name(), obj.Coderep()->Coderep_id(),
                   SRCPOS_linenum(cur_line));
    } else if (obj.Is_vsym()) {
      VSYM_OBJ_REP *vor = obj.Vor();
      fprintf(fp, "\n##%s: Find candidates for vor at line %d :",
                   checker->Checker_name(), SRCPOS_linenum(cur_line));
      vor->Print(fp);
    } else {
      // check by stmt
      fprintf(fp, "\n##%s: Find candidates for stmt at line %d ",
                   checker->Checker_name(),
                   SRCPOS_linenum(cur_line));
    }
    fprintf(fp, "in stmt:\n");
    ctx->Vsa()->Print_sr(sr, fp);
  }

  bool Ignore_Stmt(STMTREP *sr) const {
    if (VSA_Ignore_Asm && sr->Opr() == OPR_ASM_STMT) {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
      (TFile, "##TRAVERLER: skip asm stmt sr%d.\n",
              sr->Stmtrep_id()));
      return true;
    }
    if (!VSA_Enable_Lib_Check && Is_inlined_lib_func(sr)) {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "##TRAVERLER: skip sr%d in inlined lib function.\n",
                       sr->Stmtrep_id()));
      return true;
    }
    return false;
  }

  template<CODEKIND _KIND> void
  Start_check(CODEREP* cr, STMTREP* sr)
  {
    // check if stmt is ignored
    if (Ignore_Stmt(sr)) {
      return;
    }
    // for those checker who want to check init value,
    // need to ignore duplicate result from same cr, same cr lead to same init value
    if (_CHECKER::SUSPECT & CS_INIT) {
      if (Coderep_checked(cr) == TRUE) {
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                (TFile, "##TRAVERLER: Ignored, already checked, cr%d\n", cr->Coderep_id()));
        return;
      }
    }
    OPT_POOL_Push(_cu->Loc_pool(), -1);
    {
      TRAV_CONTEXT ctx(_cu, sr, cr);
      CHECK_OBJ obj(cr, sr);
      _CHECKER checker(ctx, this);
      Is_Trace_cmd(ctx.Tracing(), Trace_start(&checker, obj, &ctx, TFile));

      VSYM_OBJ_REP *vor = NULL;
      if (_KIND == CK_IVAR &&
          cr->Opr() != OPR_PARM &&
          (vor = Vsa()->Cr_2_vor(cr)) != NULL &&
          vor->Vsym_obj()->Fld_rep().Is_any()) {
        _cu->Analyze_access_info(sr, cr, &obj.Access_info());
      }

      CHECKER_STATUS sts = checker.template Check_coderep<_KIND>(obj, &ctx);
      if (sts != CS_DONE) {
        UD_TRAVELER<_CHECKER> helper(checker, ctx);
        helper.Continue_trav(obj, sts);
      }
      Is_True(ctx.Frame_empty(), ("call stack corrupted"));
    }
    OPT_POOL_Pop(_cu->Loc_pool(), -1);
  }

  void
  Start_check(CODEREP* cr, STMTREP* sr, CHECKER_SUSPECT suspect)
  {
    // check if stmt is ignored
    if (Ignore_Stmt(sr)) {
      return;
    }
    OPT_POOL_Push(_cu->Loc_pool(), -1);
    {
      IDTYPE aux_id = ILLEGAL_AUX_ID;
      if (cr->Kind() == CK_VAR) {
        aux_id = cr->Aux_id();
      } else if (cr->Kind() == CK_LDA) {
        aux_id = cr->Lda_aux_id();
      }
      if (aux_id != ILLEGAL_AUX_ID) {
        ST *st = _cu->Opt_stab()->Aux_stab_entry(aux_id)->St();
        if (st && Vsa_check_sym_ignore(ST_name(st))) {
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
            (TFile, "##TRAVERLER: Ignore checking cr, symbol is ignored, symbol name: %s.\n", ST_name(st)));
          return;
        }
      }
      TRAV_CONTEXT ctx(_cu, sr, cr);
      CHECK_OBJ obj(cr, sr);
      _CHECKER checker(ctx, this);
      if (checker.Set_check_kind(suspect)) {
        Is_Trace(ctx.Tracing(), (TFile, "\n##Start check with suspect %d:", suspect));
        Is_Trace_cmd(ctx.Tracing(), Trace_start(&checker, obj, &ctx, TFile));
        UD_TRAVELER<_CHECKER> helper(checker, ctx);
        checker.Set_orig_stname(obj, &ctx);
        helper.Continue_trav(obj, CS_OP);
      }
      Is_True(ctx.Frame_empty(), ("call stack corrupted"));
    }
    OPT_POOL_Pop(_cu->Loc_pool(), -1);
  }

  template<OPERATOR opr> void
  Start_check(STMTREP *sr, CHECKER_SUSPECT suspect)
  {
    // check if stmt is ignored
    if (Ignore_Stmt(sr)) {
      return;
    }
    OPT_POOL_Push(_cu->Loc_pool(), -1);
    {
      TRAV_CONTEXT ctx(_cu, sr, NULL);
      CHECK_OBJ obj(sr);
      _CHECKER checker(ctx, this);
      if (checker.Set_check_kind(suspect)) {
        Is_Trace(ctx.Tracing(), (TFile, "\n##Start check sr with suspect %d:", suspect));
        Is_Trace_cmd(ctx.Tracing(), Trace_start(&checker, obj, &ctx, TFile));
        UD_TRAVELER<_CHECKER> helper(checker, ctx);
        CHECKER_STATUS sts = checker.template Check_stmtrep<opr>(obj, &ctx);
        if (sts != CS_DONE) {
          UD_TRAVELER<_CHECKER> helper(checker, ctx);
          helper.Continue_trav(obj, sts);
        }
      }
      Is_True(ctx.Frame_empty(), ("call stack corrupted"));
    }
    OPT_POOL_Pop(_cu->Loc_pool(), -1);
  }

  void Process_coderep(CODEREP* cr, STMTREP* sr)
  {
    Is_True(cr != NULL, ("invalid cr"));
    switch (cr->Kind()) {
    case CK_LDA:
      if ((_CHECKER::SUSPECT & CS_VAR_USE) && VSA_Extern_Uiv)
        Start_check<CK_LDA>(cr, sr);
      break;
    case CK_CONST:
    case CK_RCONST:
      break;

    case CK_VAR:
      if ((_CHECKER::SUSPECT & CS_VAR_USE) && !cr->Is_var_volatile())
        Start_check<CK_VAR>(cr, sr);
      break;

    case CK_IVAR:
      // check istr/ilod base
      if (OPERATOR_is_store(sr->Opr()) && cr == sr->Lhs()) {
        Process_coderep(cr->Istr_base(), sr);
        if (sr->Opr() == OPR_MSTORE) {
          Process_coderep(cr->Mstore_size(), sr);
        }
      }
      else {
        Process_coderep(cr->Ilod_base(), sr);
      }
      // check if checker suspects dereference
      if (_CHECKER::SUSPECT & CS_DEREFERENCE) {
        if (!(_CHECKER::SUSPECT & CS_VPTR) && TRAV_CONTEXT::Is_vptr(cr, sr)) {
          // skip vptr
          break;
        }
        CODEREP *base = (cr == sr->Lhs()) ? cr->Istr_base() : cr->Ilod_base();
        if (!(_CHECKER::SUSPECT & CS_ILOD_VPTR) &&
            base && TRAV_CONTEXT::Is_vptr(base, sr)) {
          // skip iload vptr
          break;
        }
        if (TRAV_CONTEXT::Is_dereference(cr) && !cr->Is_ivar_volatile()) {
          Start_check<CK_IVAR>(cr, sr);
        }
        else if (cr->Opr() == OPR_PARM && OPERATOR_is_call(sr->Opr())) {
          // for parameter annotated as dereferenced, start a check
          DNA_NODE *dna = _cu->Dna();
          RNA_NODE *rna = dna->Get_callsite_rna(sr);
          if (rna != NULL) {
            VSA *vsa = dna->Comp_unit()->Vsa();
            IDTYPE parm_idx = rna->Get_arg_with_cr(cr->Ilod_base());
            TY_IDX ty_idx = cr->Ilod_base()->object_ty();
            if (parm_idx != INVALID_VAR_IDX &&
                ty_idx != TY_IDX_ZERO && TY_kind(ty_idx) == KIND_POINTER &&
                vsa->Ipsa()->Rna_has_rbc_parm_flag(rna, parm_idx, REF_ILOAD|REF_ISTORE)) {
              Start_check(cr->Ilod_base(), sr, CS_DEREFERENCE);
            }
          }
        }
      }
      break;

    case CK_OP:
      for (INT i = 0; i < cr->Kid_count(); ++i) {
        CODEREP* opnd = cr->Opnd(i);
        Process_coderep(opnd, sr);
      }
      if (((_CHECKER::SUSPECT & CS_CALL) && TRAV_CONTEXT::Is_call(cr)) ||
          ((_CHECKER::SUSPECT & CS_ICALL) && TRAV_CONTEXT::Is_icall(cr))) {
        Start_check<CK_OP>(cr, sr);
      }
      if (_CHECKER::SUSPECT & CS_DIVISOR) {
        Start_check<CK_OP>(cr, sr);
      }
      break;
    default:
      Is_True(FALSE, ("unknown cr kind"));
      break;
    }
  }

  void Process_stmtrep(STMTREP* sr) {
    // don't check prefetch/eval
    if (sr->Opr() == OPR_PREFETCH ||
        sr->Opr() == OPR_EVAL)
      return;
    if (sr->Rhs()) {
      if (_CHECKER::SUSPECT & CS_INIT) {
        // if stmt is alerady a assgned stmt, or the rhs is pu copyin, ignore
        if (sr->Is_identity_asgn() || _cu->Dna()->Is_param_copyin(sr)) {
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
            (TFile, "##TRAVERLER: Ignored, sr is assign stmt or sr is parameter copy in, sr%d, cr%d.\n",
              sr->Stmtrep_id(), sr->Rhs()->Coderep_id()));
          return;
        }
      }
      Process_coderep(sr->Rhs(), sr);
    }
    BOOL is_call = OPERATOR_is_call(sr->Opr());
    if (!is_call && sr->Lhs())
      Process_coderep(sr->Lhs(), sr);

    switch (sr->Opr()) {
      case OPR_CALL:
      case OPR_ICALL:
        Process_call(sr);
      break;
      case OPR_RETURN:
        Process_ret(sr);
      break;
      default:
      break;
    }
  }

  void Process_call(STMTREP *sr) {
    Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
    if (_CHECKER::SUSPECT & CS_DEREFERENCE) {
      DNA_NODE *dna = _cu->Dna();
      RNA_NODE *rna = dna->Get_callsite_rna(sr);
      if (rna == NULL)
        return;

      // check if icall target is NULL
      // skip java: java icall target contains iload, covered by Process_coderep
      if (sr->Opr() == OPR_ICALL && !PU_java_lang(Get_Current_PU())) {
        CODEREP *rhs = sr->Rhs();
        Is_True(rhs->Kind() == CK_OP && rhs->Kid_count() > 0,
                ("invalid icall stmtrep"));
        CODEREP *check_cr = rhs->Opnd(rhs->Kid_count() - 1);
        // skip ivar: already covered by Process_coderep
        if (check_cr->Kind() != CK_IVAR) {
          Start_check(check_cr, sr, CS_DEREFERENCE);
        }
      }
    }
  }

  void Process_ret(STMTREP *sr) {
    Is_True(sr && sr->Opr() == OPR_RETURN, ("invalid return sr"));
    if (_CHECKER::SUSPECT & CS_RETURN) {
      Start_check<OPR_RETURN>(sr, CS_RETURN);
    }
  }

  void Process_bb(BB_NODE* bb) {
    STMTREP *sr;
    STMTREP_ITER sr_iter(bb->Stmtlist());
    FOR_ALL_NODE(sr, sr_iter, Init()) {
      Process_stmtrep(sr);
    }

    // traverse DOM so far
    BB_NODE *dom_bb;
    BB_LIST_ITER dom_bb_iter;
    FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
      Process_bb(dom_bb);
  }

public:
  void Process() {
    Process_bb(_cu->Cfg()->Entry_bb());
  }
};

// ====================================================================
// SPOS_BASE
// Base class for check which needs spos information
// ====================================================================
class SPOS_BASE {
private:
  SRCPOS_HANDLE *_sp_h;
  BOOL           _copied;

public:
  SPOS_BASE(TRAV_CONTEXT& ctx) : _copied(FALSE) {
    _sp_h = CXX_NEW(SRCPOS_HANDLE(ctx.Root_cr(), ctx.Root_sr(), ctx.Dna(), ctx.Spos_pool()),
                    ctx.Spos_pool());
  }

  SPOS_BASE(SRCPOS_HANDLE *sp_h) : _sp_h(sp_h), _copied(TRUE) {}

  ~SPOS_BASE() {
    if(!_copied) {
      CXX_DELETE(_sp_h, _sp_h->Mem_pool());
    }
  }

  SRCPOS_HANDLE* Sp_h()
  {
    return _sp_h;
  }

  void
  Set_orig_stname(CHECK_OBJ &obj, TRAV_CONTEXT* ctx, BOOL force=FALSE)
  {
    CODEREP *cr = obj.Is_var() ? obj.Coderep() : obj.Vor_cr();
    if (cr) {
      _sp_h->Set_orig_stname(
        _sp_h->Find_orig_stname(cr, obj.Stmtrep(), ctx->Dna(), force));
    }
  }

  void
  Append_data(STMTREP* sr, DNA_NODE* dna, PATH_INFO pi)
  {
    _sp_h->Append_data(sr, dna, pi);
  }

  void
  Append_data(STMTREP* sr, CODEREP* cr, DNA_NODE* dna, PATH_INFO pi)
  {
    _sp_h->Append_data(sr, cr, dna, pi);
  }

  void
  Append_data(BB_NODE* bb, DNA_NODE* dna, PATH_INFO pi)
  {
    _sp_h->Append_data(bb, dna, pi);
  }

  void
  Append_data(ST* st, BB_NODE* bb, DNA_NODE* dna, PATH_INFO pi)
  {
    _sp_h->Append_data(st, bb, dna, pi);
  }

  void
  Append_stpath(STMTREP* sr, CODEREP* cr, DNA_NODE* dna, BOOL stname)
  {
    _sp_h->Append_stpath(sr, cr, dna, stname);
  }

  SRCPOS_TREENODE*
  Add_children(INT cnt)
  {
    _sp_h->Add_children(cnt);
    return _sp_h->Cur_node();
  }

  // Children count
  UINT32
  Children_count() { return _sp_h->Children_count(); }

  void
  Set_cur_node(SRCPOS_TREENODE* node, INT idx)
  {
    _sp_h->Set_cur_node(node, idx);
  }

  void
  Reset_cur_node(SRCPOS_TREENODE* node, INT idx)
  {
    _sp_h->Reset_cur_node(node, idx);
  }

  void
  Set_flag(SRCPOS_FLAG flag)
  {
    _sp_h->Set_flag(flag);
  }

  SRCPOS_TREENODE*
  Cur_node()
  {
    return _sp_h->Cur_node();
  }

  IDTYPE
  Cur_idx()
  {
    return _sp_h->Cur_idx();
  }

  template<typename _T> void
  Push_mark(_T* mark)
  {
    _sp_h->Path()->Push_mark(mark);
  }

  template<typename _T> void
  Pop_mark(_T* mark, BOOL pop_mark)
  {
    _sp_h->Path()->Pop_mark(mark, pop_mark);
  }

  void
  Add_bb(BB_NODE* bb)
  {
    _sp_h->Path()->Add_bb(bb);
  }

  INT
  Push_value_graph()
  {
    return _sp_h->Push_value_graph();
  }

  void
  Pop_value_graph(INT level)
  {
    _sp_h->Pop_value_graph(level);
  }

  template<typename _T> BOOL
  Add_assign(VSA* lvsa, _T *lhs, VSA *rvsa, _T *rhs)
  {
    return _sp_h->Add_assign(lvsa, lhs, rvsa, rhs);
  }

  BOOL
  Add_control_dependency(DNA_NODE *dna, BB_NODE *pred, BB_NODE *succ)
  {
    return _sp_h->Add_control_dependency(dna, pred, succ);
  }

  BOOL
  Add_phi_opnd(DNA_NODE *dna, PHI_NODE *phi, INT32 idx)
  {
    return _sp_h->Add_phi_opnd(dna, phi, idx);
  }

  BOOL
  Is_def_reachable(BB_NODE *use_bb, STMTREP *def_sr)
  {
    PATH_POSSIBLE_RES res = _sp_h->Path()->Is_def_reachable(use_bb, def_sr);
    if (res == PATH_MAY_REACHABLE) {
      SRCPOS_TREENODE *cur_node = Cur_node();
      if (cur_node) {
        cur_node->Set_flag(SRCPOS_FLAG_MAYBE);
      }
    }
    return res == PATH_NOT_REACHABLE ? FALSE : TRUE;
  }

  BOOL
  Is_path_possible(PHI_NODE* phi, INT opnd, SRCPOS_TREENODE *cur_node)
  {
    PATH_POSSIBLE_RES res = _sp_h->Path()->Is_path_possible(phi, opnd);
    if (res == PATH_MAY_REACHABLE) {
      if (cur_node && cur_node->Get_child_cnt() > opnd) {
        SRCPOS_TREENODE *opnd_node = cur_node->Get_child(opnd);
        opnd_node->Set_flag(SRCPOS_FLAG_MAYBE);
      }
    }
    return res == PATH_NOT_REACHABLE ? FALSE : TRUE;
  }

  BOOL
  Check_skip()
  {
    return _sp_h->Reach_check_limit();
  }
};

#endif /* opt_vsa_checker_INCLUDED */

