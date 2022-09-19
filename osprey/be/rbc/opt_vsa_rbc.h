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
// Module: opt_vsa_rbc.h
//
// ====================================================================
// ====================================================================

#ifndef opt_vsa_rbc_INCLUDED
#define opt_vsa_rbc_INCLUDED        "opt_vsa_rbc.h"

#include "opt_vsa_checker.h"
#include "opt_vsa_var_def.h"
#include "rbc_base.h"
#include <regex.h>  // for regcomp, regexec

#define RBC_EVAL_SKIP()                            \
  do {                                             \
    if (Rbc_eval_certainty()->back() == REC_SKIP)  \
      return 0;                                    \
  } while(0)

#define RBC_EVAL_SKIP_RET_INVALID()                \
  do {                                             \
    if (Rbc_eval_certainty()->back() == REC_SKIP)  \
      return EVAL_RET_INVALID;                     \
  } while(0)

#ifdef Is_True_On
# define Is_True_Rbc Is_True
# define Is_True_Ret_Rbc_Invalid Is_True
#else
# define Is_True_Rbc(a, b)                        \
  do {                                            \
    if (!(a)) {                                   \
      Rbc_eval_certainty()->push_back(REC_SKIP);  \
      return 0;                                   \
    }                                             \
  } while(0)

# define Is_True_Ret_Rbc_Invalid(a, b)            \
  do {                                            \
    if (!(a)) {                                   \
      Rbc_eval_certainty()->push_back(REC_SKIP);  \
      return EVAL_RET_INVALID;                    \
    }                                             \
  } while(0)
#endif

enum RBC_EVAL_CERTAINTY {
  REC_DEFINITE  = 0,     // evaluation information is enough to determine the result.
  REC_MAYBE     = 1,     // evaluation information is not enough to determine the result.
  REC_UNKNOWN   = 2,     // evaluation information is missing, if we guess a value, it's MAYBE.
  REC_SKIP      = 3,     // evaluation failed, pop up TRUE always avoid reporting issues.
};

typedef enum _rbc_phase {
  RBC_DEFAULT_PHASE            = 0,
  RBC_DNA_INIT                 = 1,
  RBC_BOTTOM_UP_VSA_BB         = 2,
  RBC_BOTTOM_UP_CREATE_HO      = 3,
  RBC_BOTTOM_UP_CREATE_VO      = 4,
  RBC_BOTTOM_UP_TAG_PROP       = 5,
  RBC_BOTTOM_UP_FSM_ANALYSIS   = 6,
  RBC_FSM_CHECK                = 7,
  RBC_VSA_CHECK                = 8,
} RBC_PHASE;

enum FSM_ERR_KIND {
  FSM_ERR_KIND_TRANSIT   = 0,
  FSM_ERR_KIND_DEFAULT   = 1,
  FSM_ERR_KIND_DANGLING  = 2,
};

enum PATH_EVAL_KIND {
  PATH_EVAL_KIND_NONE = 0,
  PATH_MAX_CALL_DEPTH = 1,
  PATH_MAX_STACK_SIZE = 2,
};


typedef enum {
  CHECK_BY_TAG,         // Ex: check by "tainted.*"
  CHECK_BY_ATTR,        // Ex: check by "*.sqli"
  CHECK_BY_TAG_ATTR,    // Ex: check by "tainted.sqli"
} TAG_CHECK_TYPE;

class RULE_BODY {
private:
  IDTYPE       _dna_idx;      // dna that holds the rbc expr
  CODEREP     *_rbc_expr;     // conditions

  RULE_BODY(void);                          // REQUIRED UNDEFINED UNWANTED methods
  RULE_BODY(const RULE_BODY&);              // REQUIRED UNDEFINED UNWANTED methods
  RULE_BODY& operator = (const RULE_BODY&); // REQUIRED UNDEFINED UNWANTED methods

public:
  RULE_BODY (IDTYPE dna_idx, CODEREP *expr) {
    _dna_idx  = dna_idx;
    _rbc_expr = expr;
  }

  IDTYPE       Get_dna_idx(void) const       { return _dna_idx; }
  DNA_NODE    *Get_rbc_dna(IPSA *ipsa) const { return ipsa->Get_dna(_dna_idx); }
  CODEREP     *Get_rbc_expr(void) const      { return _rbc_expr; }
  STMTREP     *Get_rbc_sr(void) const        { return (_rbc_expr && _rbc_expr->Kind() == CK_VAR) ?
                                                       _rbc_expr->Defstmt() : NULL; }
};

class RULE {

private:
  IDTYPE     _id;
  IDTYPE     _dna_idx;
  CODEREP   *_bool_expr;
  STRING     _name;
  int        _languages;
  BOOL       _scanned;
  BOOL       _tracing;
  BOOL       _enable;

  RULE(void);                     // REQUIRED UNDEFINED UNWANTED methods
  RULE(const RULE&);              // REQUIRED UNDEFINED UNWANTED methods
  RULE& operator = (const RULE&); // REQUIRED UNDEFINED UNWANTED methods

public:
  RULE (IDTYPE id, IDTYPE dna_idx, CODEREP *expr, STRING name, int lang) {
    _id        = id;
    _dna_idx   = dna_idx;
    _bool_expr = expr;
    _name      = name;
    _languages = lang;
    _scanned   = FALSE;
    _tracing   = FALSE;
    _enable    = TRUE;
  }

  IDTYPE   Get_id(void) const          { return _id;        }
  IDTYPE   Get_dna_idx(void) const     { return _dna_idx;   }
  CODEREP *Get_bool_expr(void) const   { return _bool_expr; }
  STRING   Get_rule_name(void) const   { return _name;      }
  int      Get_language(void) const    { return _languages; }
  BOOL     Is_scanned(void) const      { return _scanned;   }
  BOOL     Is_tracing(void) const      { return _tracing;   }
  BOOL     Is_enable(void) const       { return _enable;    }
  void     Set_scanned(BOOL s)         { _scanned = s;      }
  void     Set_tracing(BOOL t)         { _tracing = t;      }
  void     Set_enable(BOOL e)          { _enable = e;       }

  void     Print(FILE* fp)
  {
    fprintf(fp, "Rule(\"%s\"): Id(%d), Dna(%d), Bool(cr%d), S(%d), T(%d), E(%d)\n",
            _name, _id, _dna_idx, _bool_expr == NULL ? -1 : _bool_expr->Coderep_id(),
            _scanned, _tracing, _enable);
  }
};

class FSM_TRAV_FRAME {
private:
  IDTYPE_SET     *_visited;     // BB(id) visited in this frame
  UINT64_SET     *_rna_visited; // rna with direction visited by this frame
  RNA_NODE       *_rna;         // rna reaches to this frame
  BB_NODE        *_bb;          // BB to continue in previous frame
  STMTREP        *_stmt;        // stmt to continue in previous frame
  CODEREP        *_key;         // key to continue in previous frame
  const char     *_key_str;     // key str to continue in previous frame
  COMP_UNIT      *_cu;          // COMP_UNIT to this frame
  IDTYPE          _fsm_state;   // start fsm state to this frame
  TRAV_DIRECTION  _trav_dir;    // direction
  BOOL            _tracing;     // depends on VSA_DUMP_FLAG

  FSM_TRAV_FRAME(void);                             // REQUIRED UNDEFINED UNWANTED methods
  FSM_TRAV_FRAME(const FSM_TRAV_FRAME&);            // REQUIRED UNDEFINED UNWANTED methods
  FSM_TRAV_FRAME& operator=(const FSM_TRAV_FRAME&); // REQUIRED UNDEFINED UNWANTED methods

public:
  FSM_TRAV_FRAME(RNA_NODE *rna, BB_NODE *bb, STMTREP *stmt, CODEREP *key, const char *key_str,
                 COMP_UNIT *cu, IDTYPE state, TRAV_DIRECTION dir, MEM_POOL *pool)
    : _rna(rna), _bb(bb), _stmt(stmt), _key(key), _key_str(key_str),
      _cu(cu), _fsm_state(state), _trav_dir(dir)
    { _tracing = Get_Trace(TP_WOPT2, VSA_DUMP_FLAG) || Get_Trace(TP_WOPT2, FSM_RBC_DUMP_FLAG);
      _visited = CXX_NEW(IDTYPE_SET(47, IDTYPE_HASHER(), IDTYPE_EQUAL(), IDTYPE_ALLOCATOR(pool)), pool);
      _rna_visited = CXX_NEW(UINT64_SET(8, UINT64_HASHER(), UINT64_EQUAL(), UINT64_ALLOCATOR(pool)), pool);
    }

  BOOL Tracing(void)
  {
    return _tracing;
  }

  uint64_t Rna_visited_key(IDTYPE idx, TRAV_DIRECTION dir)
  {
    return (uint64_t)idx << 32 | (uint64_t)dir;
  }

  void Set_rna_visited(uint64_t key)
  {
    if (_rna_visited->find(key) == _rna_visited->end())
      _rna_visited->insert(key);
  }

  void Set_bb_visited(IDTYPE idx)
  {
    if (_visited->find(idx) == _visited->end())
      _visited->insert(idx);
  }

  BOOL Visited(RNA_NODE *rna, TRAV_DIRECTION dir)
  {
    if (rna != NULL) {
      uint64_t key = Rna_visited_key(rna->Rna_idx(), dir);
      if (_rna_visited->find(key) == _rna_visited->end()) {
        _rna_visited->insert(key);
        Is_Trace(Tracing(), (TFile, "RBC: ******** visiting RNA_NODE(%d)DIR(%d)\n",
                             rna->Rna_idx(), dir));
        return FALSE;
      }
    }
    Is_Trace(Tracing(), (TFile, "RBC: ******** already visited RNA_NODE(%d)DIR(%d)\n",
                         rna == NULL ? -1 : rna->Rna_idx(), dir));
    return TRUE;
  }

  void Reset_visited(RNA_NODE *rna, TRAV_DIRECTION dir)
  {
    if (rna != NULL) {
      uint64_t key = Rna_visited_key(rna->Rna_idx(), dir);
      if (_rna_visited->find(key) != _rna_visited->end()) {
        _rna_visited->erase(key);
        Is_Trace(Tracing(), (TFile, "RBC: ******** reset visited RNA_NODE(%d)DIR(%d)\n",
                             rna->Rna_idx(), dir));
      }
    }
    return;
  }

  BOOL Visited(BB_NODE *bb)
  {
    if (bb != NULL && _visited->find(bb->Id()) == _visited->end()) {
      _visited->insert(bb->Id());
      Is_Trace(Tracing(), (TFile, "RBC: @@@@@@@@ visiting BB(%d)\n", bb->Id()));
      return FALSE;
    }
    Is_Trace(Tracing(), (TFile, "RBC: @@@@@@@@ already visited BB(%d)\n", bb->Id()));
    return TRUE;
  }

  void Reset_visited(BB_NODE *bb)
  {
    if (bb != NULL && _visited->find(bb->Id()) != _visited->end()) {
      Is_Trace(Tracing(), (TFile, "RBC: @@@@@@@@ reset visited BB(%d)\n", bb->Id()));
      _visited->erase(bb->Id());
    }
    return;
  }

  IDTYPE_SET     *Bb_visited(void) const { return _visited; }
  UINT64_SET     *Rna_visited(void) const{ return _rna_visited; }
  RNA_NODE       *Rna(void) const        { return _rna; }
  BB_NODE        *Bb(void) const         { return _bb; }
  STMTREP        *Stmt(void) const       { return _stmt; }
  CODEREP        *Key(void) const        { return _key; }
  const char     *Key_string(void) const { return _key_str; }
  COMP_UNIT      *Comp_unit(void) const  { return _cu; }
  IDTYPE          State(void) const      { return _fsm_state; }
  TRAV_DIRECTION  Direction(void) const  { return _trav_dir; }

  void            Print(FILE *fp = stdout) const
  {
    fprintf(fp, "FRAME: BB(%d), STMT(%d), KEY(cr%d: %s), RNA_NODE(%d), DIRECTION(%d), FUNC(%s), STATE(%d)\n",
            _bb == NULL ? -1 : _bb->Id(), _stmt == NULL ? -1 : _stmt->Stmtrep_id(),
            _key == NULL ? -1 : _key->Coderep_id(), _key_str == NULL ? "null" : _key_str,
            _rna == NULL ? -1 : _rna->Rna_idx(), _trav_dir, _cu->Dna()->Fname(), _fsm_state);
    fprintf(fp, "       %ldRNA_VISITED(", _rna_visited->size());
    for (UINT64_SET::iterator iter = _rna_visited->begin();
         iter != _rna_visited->end(); iter++) {
      uint64_t key = *iter;
      IDTYPE rna_idx = (IDTYPE)((key & 0xffffffff00000000) >> 32);
      uint32_t dir = (uint32_t)(key & 0xffffffff);
      fprintf(fp, "%d:%d ", rna_idx, dir);
    }
    fprintf(fp, ")\n");
    fprintf(fp, "       %ldBB_VISITED(", _visited->size());
    for (IDTYPE_SET::iterator iter = _visited->begin();
         iter != _visited->end(); iter++) {
      fprintf(fp, "%d ", *iter);
    }
    fprintf(fp, ")\n");
  }
};

#define SIZE_MAX_STR 32
#define MAX_ERROR_IN_ONE_CHECK 64

class FSM_TRAV_CONTEXT {
  typedef std::stack<FSM_TRAV_FRAME*> FSM_TRAV_STACK;
private:
  FSM_TRAV_STACK   _fts;         // call stack
  MEM_POOL        *_mpool;       // local mem_pool
  UINT64_SET      *_rna_visited; // rna visited in current context
  FSM             *_fsm;         // FSM in evaluation
  STRING           _fsm_name;    // the name of the FSM in evaluation
  CODEREP         *_cur_key;     // current key of the FSM in evaluation
  BOOL             _tracing;     // depends on VSA_DUMP_FLAG
  BOOL             _skip;        // skip current FSM traversal due to stack too deep

  FSM_TRAV_CONTEXT(void);                               // REQUIRED UNDEFINED UNWANTED methods
  FSM_TRAV_CONTEXT(const FSM_TRAV_CONTEXT&);            // REQUIRED UNDEFINED UNWANTED methods
  FSM_TRAV_CONTEXT& operator=(const FSM_TRAV_CONTEXT&); // REQUIRED UNDEFINED UNWANTED methods

public:
  FSM_TRAV_CONTEXT(STRING name, FSM* fsm, MEM_POOL *pool)
    : _mpool(pool), _fsm(fsm), _fsm_name(name), _cur_key(NULL), _skip(FALSE)
  {
    _tracing = Get_Trace(TP_WOPT2, VSA_DUMP_FLAG) || Get_Trace(TP_WOPT2, FSM_RBC_DUMP_FLAG);
    _rna_visited = CXX_NEW(UINT64_SET(47, UINT64_HASHER(), UINT64_EQUAL(), UINT64_ALLOCATOR(pool)), pool);
  }

  BOOL      Tracing(void) const  { return _tracing; }
  FSM      *Fsm(void) const      { return _fsm; }
  STRING    Fsm_name(void) const { return _fsm_name; }
  CODEREP  *Cur_key(void) const  { return _cur_key; }
  MEM_POOL *Mem_pool(void) const { return _mpool; }
  BOOL      Skip(void) const     { return _skip; }
  void      Set_skip(void)       { _skip = TRUE; }
  UINT32    Stack_size(void) const { return _fts.size(); }

  void Set_cur_key(CODEREP *key)
  {
    _cur_key = key;
  }

  uint64_t Rna_visited_key(IDTYPE idx, TRAV_DIRECTION dir)
  {
    return (uint64_t)idx << 32 | (uint64_t)dir;
  }

  BOOL Visited(RNA_NODE *rna, TRAV_DIRECTION dir)
  {
    if (rna != NULL) {
      uint64_t key = Rna_visited_key(rna->Rna_idx(), dir);
      if (_rna_visited->find(key) == _rna_visited->end()) {
        _rna_visited->insert(key);
        if (!_fts.empty()) {
          _fts.top()->Visited(rna, dir);
        }
        return FALSE;
      }
    }
    return TRUE;
  }

  void Reset_visited(RNA_NODE *rna, TRAV_DIRECTION dir)
  {
    if (rna != NULL) {
      uint64_t key = Rna_visited_key(rna->Rna_idx(), dir);
      if (_rna_visited->find(key) != _rna_visited->end()) {
        _rna_visited->erase(key);
        if (!_fts.empty()) {
          _fts.top()->Reset_visited(rna, dir);
        }
      }
    }
    return;
  }

  FSM_TRAV_FRAME *Duplicate(FSM_TRAV_FRAME *frame, TRAV_DIRECTION dir)
  {
    FSM_TRAV_FRAME *ret = CXX_NEW(FSM_TRAV_FRAME(frame->Rna(), frame->Bb(), frame->Stmt(), frame->Key(),
                                                 frame->Key_string(), frame->Comp_unit(), frame->State(),
                                                 dir, Mem_pool()), Mem_pool());
    for (UINT64_SET::iterator iter = frame->Rna_visited()->begin();
         iter != frame->Rna_visited()->end(); iter++) {
      ret->Set_rna_visited(*iter);
    }
    for (IDTYPE_SET::iterator iter = frame->Bb_visited()->begin();
         iter != frame->Bb_visited()->end(); iter++) {
      ret->Set_bb_visited(*iter);
    }
    return ret;
  }

  FSM_TRAV_FRAME *Push_frame(FSM_TRAV_FRAME *frame)
  {
    if (frame != NULL) {
      Is_Trace(Tracing(), (TFile, "%s@@@@@@@@ Push Frame:\n", SBar));
      Is_Trace_cmd(Tracing(), Print(TFile));
      _fts.push(frame);
      for (UINT64_SET::iterator iter = frame->Rna_visited()->begin();
           iter != frame->Rna_visited()->end(); iter++) {
        _rna_visited->insert(*iter);
      }
      if (_fts.size() > VSA_Checker_Max_Frame)
        _skip = TRUE;
      Is_Trace(Tracing(), (TFile, ">>>>>>>>\n"));
      Is_Trace_cmd(Tracing(), Print(TFile));
      Is_Trace(Tracing(), (TFile, "%s", SBar));
    }
    return frame;
  }

  FSM_TRAV_FRAME *Push_frame(RNA_NODE *rna, BB_NODE *bb, STMTREP *stmt, CODEREP *key,
                             const char *key_str, COMP_UNIT *cu, IDTYPE state, TRAV_DIRECTION dir)
  {
    FSM_TRAV_FRAME *frame = CXX_NEW(FSM_TRAV_FRAME(rna, bb, stmt, key, key_str, cu, state, dir, Mem_pool()),
                                    Mem_pool());
    Is_Trace(Tracing(), (TFile, "%s@@@@@@@@ Push Frame:\n", SBar));
    Is_Trace_cmd(Tracing(), Print(TFile));
    _fts.push(frame);
    if (_fts.size() > VSA_Checker_Max_Frame)
      _skip = TRUE;
    Is_Trace(Tracing(), (TFile, ">>>>>>>>\n"));
    Is_Trace_cmd(Tracing(), Print(TFile));
    Is_Trace(Tracing(), (TFile, "%s", SBar));
    return frame;
  }

  FSM_TRAV_FRAME *Pop_frame(TRAV_DIRECTION dir, BOOL reset = TRUE)
  {
    Is_True(!_fts.empty(), ("stack is empty"));
    if (_fts.empty()) {
      return NULL;
    }
    FSM_TRAV_FRAME *top = _fts.top();
    if (top->Direction() == dir) {
      Is_Trace(Tracing(), (TFile, "%s@@@@@@@@ Pop Frame:\n", SBar));
      Is_Trace_cmd(Tracing(), Print(TFile));
      _fts.pop();
      if (reset) {
        UINT64_SET *rna_visited = top->Rna_visited();
        for (UINT64_SET::iterator iter = rna_visited->begin();
             iter != rna_visited->end(); iter++) {
          _rna_visited->erase(*iter);
        }
      }
      Is_Trace(Tracing(), (TFile, "<<<<<<<<\n"));
      Is_Trace_cmd(Tracing(), Print(TFile));
      Is_Trace(Tracing(), (TFile, "%s", SBar));
      return top;
    }
    else {
      return NULL;
    }
  }

  FSM_TRAV_FRAME *Top_frame(TRAV_DIRECTION dir)
  {
    Is_True(!_fts.empty(), ("stack is empty"));
    if (_fts.empty()) {
      return NULL;
    }
    FSM_TRAV_FRAME *top = _fts.top();
    return top->Direction() == dir ? top : NULL;
  }

  BOOL Visited(BB_NODE *bb)
  {
    Is_True(!_fts.empty(), ("stack is empy"));
    if (_fts.empty()) {
      return TRUE;
    }
    FSM_TRAV_FRAME *top = _fts.top();
    return top->Visited(bb);
  }

  void Reset_visited(BB_NODE *bb)
  {
    Is_True(!_fts.empty(), ("stack is empty"));
    if (_fts.empty()) {
      return;
    }
    FSM_TRAV_FRAME *top = _fts.top();
    top->Reset_visited(bb);
  }

  COMP_UNIT *Cur_comp_unit(void) const
  {
    Is_True(!_fts.empty(), ("stack is empty"));
    if (_fts.empty()) {
      return NULL;
    }
    FSM_TRAV_FRAME *top = _fts.top();
    return top->Comp_unit();
  }

  VSA *Cur_vsa(void) const
  {
    COMP_UNIT *cu = Cur_comp_unit();
    return cu == NULL ? NULL : cu->Vsa();
  }

  DNA_NODE *Cur_dna(void) const
  {
    COMP_UNIT *cu = Cur_comp_unit();
    return cu == NULL ? NULL : cu->Dna();
  }

  void Print(FILE *fp = stdout) const
  {
    fprintf(fp, "CONTEXT: FSM(%s), KEY(cr%d), SKIP(%d), STACK(%ld), %ldRNA_VISITED(",
            _fsm_name, _cur_key == NULL ? -1 : _cur_key->Coderep_id(),
            _skip, _fts.size(), _rna_visited->size());
    for (UINT64_SET::iterator iter = _rna_visited->begin();
         iter != _rna_visited->end(); iter++) {
      uint64_t key = *iter;
      IDTYPE rna_idx = (IDTYPE)((key & 0xffffffff00000000) >> 32);
      uint32_t dir = (uint32_t)(key & 0xffffffff);
      fprintf(fp, "%d:%d ", rna_idx, dir);
    }
    fprintf(fp, ")\n");
    if (!_fts.empty()) {
      fprintf(fp, "========\n");
      _fts.top()->Print(fp);
    }
  }
};

class INV_USED_INFO {
private:
  CODEREP    *_cr;      // CODEREP to check
  COMP_UNIT  *_cu;      // COMP_UNIT that holds the _cr
  const char *_fname;   // invalid func name
  BOOL        _invalid; // if _cr is invalid
  BOOL        _result;  // check result

  INV_USED_INFO(void);                            // REQUIRED UNDEFINED UNWANTED methods
  INV_USED_INFO(const INV_USED_INFO&);            // REQUIRED UNDEFINED UNWANTED methods
  INV_USED_INFO& operator=(const INV_USED_INFO&); // REQUIRED UNDEFINED UNWANTED methods

public:
  INV_USED_INFO(CODEREP *cr, COMP_UNIT *cu, const char *fname)
    : _cr(cr), _cu(cu), _fname(fname), _invalid(FALSE), _result(FALSE) {}

  CODEREP    *Cr(void) const        { return _cr;      }
  COMP_UNIT  *Comp_unit(void) const { return _cu;      }
  const char *Fname(void) const     { return _fname;   }
  BOOL        Invalid(void) const   { return _invalid; }
  BOOL        Result(void) const    { return _result;  }

  void Set_cr(CODEREP *cr)   { _cr = cr;       }
  void Set_cu(COMP_UNIT *cu) { _cu = cu;       }
  void Set_invalid(BOOL inv) { _invalid = inv; }
  void Set_result(BOOL res)  { _result = res;  }

  BOOL Is_good(void) const
  {
    if (_cr == NULL || _cu == NULL || _fname == NULL)
      return FALSE;
    return TRUE;
  }
};

class FULL_TRAV_FRAME {
private:
  BB_NODE        *_bb;          // BB to continue in previous frame
  STMTREP        *_stmt;        // stmt to continue in previous frame
  RNA_NODE       *_rna;         // rna reaches to this frame
  TRAV_DIRECTION  _trav_dir;    // direction
  COMP_UNIT      *_cu;          // COMP_UNIT of this frame
  IDTYPE_SET     *_bb_visited;  // BB visited in this frame
  UINT64_SET     *_rna_visited; // rna with direction visited by this frame
  BOOL            _tracing;     // debug info depends on VSA_DUMP_FLAG

  FULL_TRAV_FRAME(void);                              // REQUIRED UNDEFINED UNWANTED methods
  FULL_TRAV_FRAME(const FULL_TRAV_FRAME&);            // REQUIRED UNDEFINED UNWANTED methods
  FULL_TRAV_FRAME& operator=(const FULL_TRAV_FRAME&); // REQUIRED UNDEFINED UNWANTED methods

public:
  FULL_TRAV_FRAME(BB_NODE *bb, STMTREP *stmt, RNA_NODE *rna, TRAV_DIRECTION dir,
                  COMP_UNIT *cu, MEM_POOL *pool)
    : _bb(bb), _stmt(stmt), _rna(rna), _trav_dir(dir), _cu(cu)
  {
    _bb_visited = CXX_NEW(IDTYPE_SET(47, IDTYPE_HASHER(), IDTYPE_EQUAL(), IDTYPE_ALLOCATOR(pool)), pool);
    _rna_visited = CXX_NEW(UINT64_SET(8, UINT64_HASHER(), UINT64_EQUAL(), UINT64_ALLOCATOR(pool)), pool);
    _tracing = Get_Trace(TP_WOPT2, VSA_DUMP_FLAG) || Get_Trace(TP_WOPT2, FSM_RBC_DUMP_FLAG);
  }

  uint64_t Rna_visited_key(IDTYPE idx, TRAV_DIRECTION dir)
  {
    return (uint64_t)idx << 32 | (uint64_t)dir;
  }

  void Set_rna_visited(uint64_t key)
  {
    if (_rna_visited->find(key) == _rna_visited->end())
      _rna_visited->insert(key);
  }

  void Set_bb_visited(IDTYPE idx)
  {
    if (_bb_visited->find(idx) == _bb_visited->end())
      _bb_visited->insert(idx);
  }

  BOOL Visited(RNA_NODE *rna, TRAV_DIRECTION dir)
  {
    if (rna != NULL) {
      uint64_t key = Rna_visited_key(rna->Rna_idx(), dir);
      if (_rna_visited->find(key) == _rna_visited->end()) {
        _rna_visited->insert(key);
        Is_Trace(Tracing(),
                 (TFile, "FRAME TRAV: visiting RNA_NODE(%d):%s\n", rna->Rna_idx(),
                  dir == TD_NONE ? "NONE" : dir == TD_DOWN ? "DOWN" : dir == TD_UP ? "UP" : "?"));
        return FALSE;
      }
    }
    Is_Trace(Tracing(),
             (TFile, "FRAME TRAV: already visited RNA_NODE(%d):%s\n", rna->Rna_idx(),
              dir == TD_NONE ? "NONE" : dir == TD_DOWN ? "DOWN" : dir == TD_UP ? "UP" : "?"));
    return TRUE;
  }

  void Reset_visited(RNA_NODE *rna, TRAV_DIRECTION dir)
  {
    if (rna != NULL) {
      uint64_t key = Rna_visited_key(rna->Rna_idx(), dir);
      if (_rna_visited->find(key) != _rna_visited->end()) {
        _rna_visited->erase(key);
        Is_Trace(Tracing(),
                 (TFile, "FRAME TRAV: reset visited RNA_NODE(%d):%s\n", rna->Rna_idx(),
                  dir == TD_NONE ? "NONE" : dir == TD_DOWN ? "DOWN" : dir == TD_UP ? "UP" : "?"));
      }
    }
    return;
  }

  BOOL Visited(BB_NODE *bb)
  {
    if (bb != NULL && _bb_visited->find(bb->Id()) == _bb_visited->end()) {
      _bb_visited->insert(bb->Id());
      Is_Trace(Tracing(), (TFile, "FRAME TRAV: visiting BB(%d)\n", bb->Id()));
      return FALSE;
    }
    return TRUE;
  }

  void Reset_visited(BB_NODE *bb)
  {
    if (bb != NULL && _bb_visited->find(bb->Id()) != _bb_visited->end()) {
      Is_Trace(Tracing(), (TFile, "FRAME TRAV: reset visited BB(%d)\n", bb->Id()));
      _bb_visited->erase(bb->Id());
    }
    return;
  }

  BB_NODE        *Bb(void) const          { return _bb;          }
  STMTREP        *Stmt(void) const        { return _stmt;        }
  RNA_NODE       *Rna(void) const         { return _rna;         }
  TRAV_DIRECTION  Direction(void) const   { return _trav_dir;    }
  COMP_UNIT      *Comp_unit(void) const   { return _cu;          }
  IDTYPE_SET     *Bb_visited(void) const  { return _bb_visited;  }
  UINT64_SET     *Rna_visited(void) const { return _rna_visited; }
  BOOL            Tracing(void) const     { return _tracing;     }

  void            Print(FILE *fp = stdout) const
  {
    fprintf(fp, "FRAME: %s\n", _cu->Dna()->Fname());
    fprintf(fp, "-- PREV: BB(%d), STMT(%d), FROM: RNA_NODE(%d), DIRECTION(%s)\n",
            _bb == NULL ? -1 : _bb->Id(),
            _stmt == NULL ? -1 : _stmt->Stmtrep_id(),
            _rna == NULL ? -1 : _rna->Rna_idx(),
            _trav_dir == TD_NONE ? "NONE" : _trav_dir == TD_DOWN ? "DOWN" : _trav_dir == TD_UP ? "UP" : "?");
    fprintf(fp, "-- %ld RNA visited( ", _rna_visited->size());
    for (UINT64_SET::iterator iter = _rna_visited->begin();
         iter != _rna_visited->end(); iter++) {
      uint64_t key = *iter;
      IDTYPE rna_idx = (IDTYPE)((key & 0xffffffff00000000) >> 32);
      uint32_t dir = (uint32_t)(key & 0xffffffff);
      fprintf(fp, "%d:%s ", rna_idx,
              dir == TD_NONE ? "NONE" : dir == TD_DOWN ? "DOWN" : dir == TD_UP ? "UP" : "?");
    }
    fprintf(fp, ")\n");
    fprintf(fp, "-- %ld BB visited( ", _bb_visited->size());
    for (IDTYPE_SET::iterator iter = _bb_visited->begin();
         iter != _bb_visited->end(); iter++) {
      fprintf(fp, "%d ", *iter);
    }
    fprintf(fp, ")\n");
  }
};

class FULL_TRAV_CONTEXT {
  typedef std::stack<FULL_TRAV_FRAME*> FULL_TRAV_STACK;
private:
  FULL_TRAV_STACK  _fts;         // call stack
  MEM_POOL        *_mpool;       // local mem_pool
  UINT64_SET      *_rna_visited; // RNA visited in current context
  BOOL             _skip;        // skip if stack too deep
  BOOL             _tracing;     // debug info depends on VSA_DUMP_FLAG

  FULL_TRAV_CONTEXT(void);                                // REQUIRED UNDEFINED UNWANTED methods
  FULL_TRAV_CONTEXT(const FULL_TRAV_CONTEXT&);            // REQUIRED UNDEFINED UNWANTED methods
  FULL_TRAV_CONTEXT& operator=(const FULL_TRAV_CONTEXT&); // REQUIRED UNDEFINED UNWANTED methods

public:
  FULL_TRAV_CONTEXT(MEM_POOL *pool)
    : _mpool(pool), _skip(FALSE)
  {
    _rna_visited = CXX_NEW(UINT64_SET(8, UINT64_HASHER(), UINT64_EQUAL(), UINT64_ALLOCATOR(pool)), pool);
    _tracing = Get_Trace(TP_WOPT2, VSA_DUMP_FLAG) || Get_Trace(TP_WOPT2, FSM_RBC_DUMP_FLAG);
  }

  MEM_POOL *Mem_pool(void) const   { return _mpool;      }
  BOOL      Skip(void) const       { return _skip;       }
  void      Set_skip(void)         { _skip = TRUE;       }
  BOOL      Tracing(void) const    { return _tracing;    }
  UINT32    Stack_size(void) const { return _fts.size(); }

  uint64_t Rna_visited_key(IDTYPE idx, TRAV_DIRECTION dir)
  {
    return (uint64_t)idx << 32 | (uint64_t)dir;
  }

  BOOL Visited(RNA_NODE *rna, TRAV_DIRECTION dir)
  {
    if (rna != NULL) {
      uint64_t key = Rna_visited_key(rna->Rna_idx(), dir);
      if (_rna_visited->find(key) == _rna_visited->end()) {
        _rna_visited->insert(key);
        if (!_fts.empty()) {
          _fts.top()->Visited(rna, dir);
        }
        return FALSE;
      }
    }
    return TRUE;
  }

  void Reset_visited(RNA_NODE *rna, TRAV_DIRECTION dir)
  {
    if (rna != NULL) {
      uint64_t key = Rna_visited_key(rna->Rna_idx(), dir);
      if (_rna_visited->find(key) != _rna_visited->end()) {
        _rna_visited->erase(key);
        if (!_fts.empty()) {
          _fts.top()->Reset_visited(rna, dir);
        }
      }
    }
    return;
  }

  FULL_TRAV_FRAME *Duplicate(FULL_TRAV_FRAME *frame, TRAV_DIRECTION dir)
  {
    FULL_TRAV_FRAME *ret = CXX_NEW(FULL_TRAV_FRAME(frame->Bb(), frame->Stmt(),
                                                   frame->Rna(), dir, frame->Comp_unit(),
                                                   Mem_pool()), Mem_pool());
    for (UINT64_SET::iterator iter = frame->Rna_visited()->begin();
         iter != frame->Rna_visited()->end(); iter++) {
      ret->Set_rna_visited(*iter);
    }
    for (IDTYPE_SET::iterator iter = frame->Bb_visited()->begin();
         iter != frame->Bb_visited()->end(); iter++) {
      ret->Set_bb_visited(*iter);
    }
    return ret;
  }

  FULL_TRAV_FRAME *Push_frame(FULL_TRAV_FRAME *frame)
  {
    if (frame != NULL) {
      Is_Trace(Tracing(), (TFile, "%sCONTEXT TRAV:\n", SBar));
      Is_Trace_cmd(Tracing(), Print(TFile));
      _fts.push(frame);
      for (UINT64_SET::iterator iter = frame->Rna_visited()->begin();
           iter != frame->Rna_visited()->end(); iter++) {
        _rna_visited->insert(*iter);
      }
      if (_fts.size() > VSA_Checker_Max_Frame)
        _skip = TRUE;
      Is_Trace(Tracing(), (TFile, "------ After Pushed\n"));
      Is_Trace_cmd(Tracing(), Print(TFile));
      Is_Trace(Tracing(), (TFile, "%s", SBar));
    }
    return frame;
  }

  FULL_TRAV_FRAME *Push_frame(BB_NODE *bb, STMTREP *stmt, RNA_NODE *rna,
                              TRAV_DIRECTION dir, COMP_UNIT *cu)
  {
    FULL_TRAV_FRAME *frame = CXX_NEW(FULL_TRAV_FRAME(bb, stmt, rna, dir, cu, Mem_pool()),
                                     Mem_pool());
    Is_Trace(Tracing(), (TFile, "%sCONTEXT TRAV:\n", SBar));
    Is_Trace_cmd(Tracing(), Print(TFile));
    _fts.push(frame);
    if (_fts.size() > VSA_Checker_Max_Frame)
      _skip = TRUE;
    Is_Trace(Tracing(), (TFile, "------ After Pushed\n"));
    Is_Trace_cmd(Tracing(), Print(TFile));
    Is_Trace(Tracing(), (TFile, "%s", SBar));
    return frame;
  }

  FULL_TRAV_FRAME *Pop_frame(TRAV_DIRECTION dir, BOOL reset = TRUE)
  {
    if (_fts.empty())
      return NULL;
    FULL_TRAV_FRAME *top = _fts.top();
    if (top->Direction() == dir) {
      Is_Trace(Tracing(), (TFile, "%sCONTEXT TRAV:\n", SBar));
      Is_Trace_cmd(Tracing(), Print(TFile));
      _fts.pop();
      if (reset) {
        UINT64_SET *rna_visited = top->Rna_visited();
        for (UINT64_SET::iterator iter = rna_visited->begin();
             iter != rna_visited->end(); iter++) {
          _rna_visited->erase(*iter);
        }
      }
      Is_Trace(Tracing(), (TFile, "------ After Popped\n"));
      Is_Trace_cmd(Tracing(), Print(TFile));
      Is_Trace(Tracing(), (TFile, "%s", SBar));
      return top;
    }
    return NULL;
  }

  FULL_TRAV_FRAME *Top_frame(TRAV_DIRECTION dir)
  {
    if (_fts.empty())
      return NULL;
    FULL_TRAV_FRAME *top = _fts.top();
    return top->Direction() == dir ? top : NULL;
  }

  BOOL Visited(BB_NODE *bb)
  {
    if (_fts.empty())
      return TRUE;
    FULL_TRAV_FRAME *top = _fts.top();
    return top->Visited(bb);
  }

  void Reset_visited(BB_NODE *bb)
  {
    if (_fts.empty())
      return;
    FULL_TRAV_FRAME *top = _fts.top();
    top->Reset_visited(bb);
  }

  COMP_UNIT *Cur_comp_unit(void) const
  {
    if (_fts.empty())
      return NULL;
    FULL_TRAV_FRAME *top = _fts.top();
    return top->Comp_unit();
  }

  VSA *Cur_vsa(void) const
  {
    COMP_UNIT *cu = Cur_comp_unit();
    return cu == NULL ? NULL : cu->Vsa();
  }

  DNA_NODE *Cur_dna(void) const
  {
    COMP_UNIT *cu = Cur_comp_unit();
    return cu == NULL ? NULL : cu->Dna();
  }

  void Print(FILE *fp = stdout) const
  {
    fprintf(fp, "CONTEXT: STACK(%ld), SKIP(%d), %ld RNA visited( ",
            _fts.size(), _skip, _rna_visited->size());
    for (UINT64_SET::iterator iter = _rna_visited->begin();
         iter != _rna_visited->end(); iter++) {
      uint64_t key = *iter;
      IDTYPE rna_idx = (IDTYPE)((key & 0xffffffff00000000) >> 32);
      uint32_t dir = (uint32_t)(key & 0xffffffff);
      fprintf(fp, "%d:%s ", rna_idx,
              dir == TD_NONE ? "NONE" : dir == TD_DOWN ? "DOWN" : dir == TD_UP ? "UP" : "?");
    }
    fprintf(fp, ")\n");
    if (!_fts.empty()) {
      fprintf(fp, "-----> Stack Top\n");
      _fts.top()->Print(fp);
    }
  }
};

enum RBC_BUILT_CHECK_TYPE {
  RBCT_NONE         = 0x0, // nothing
  RBCT_RECURSION    = 0x1, // global recursion checks
};

typedef RBC_ENGINE::TAG_DEF_VAL TAG_VAL;
class TAG_CONF_INFO {
private:
  TAG_VAL _cst_val;
  TAG_VAL _input_val;
  
public:
  TAG_CONF_INFO(): _cst_val(RBC_ENGINE::TAG_KEEP),
                    _input_val(RBC_ENGINE::TAG_KEEP) {}
  
  TAG_CONF_INFO(const TAG_CONF_INFO& o) { _cst_val = o._cst_val; _input_val = o._input_val; }
  TAG_CONF_INFO& operator=(const TAG_CONF_INFO& o) 
  {
    _cst_val = o._cst_val; _input_val = o._input_val;
    return *this; 
  }
  void Set_cst_val(TAG_VAL v)           { _cst_val = v; }
  void Set_input_val(TAG_VAL v)         { _input_val = v; }
  TAG_VAL Cst_val()               const { return _cst_val; }
  TAG_VAL Input_val()             const { return _input_val; }
  BOOL Is_cst_unset()                   { return _cst_val == RBC_ENGINE::TAG_UNSET; }
  BOOL Is_input_set()                   { return _input_val == RBC_ENGINE::TAG_SET; }
};


template <typename T>
struct FUNC_PTR_ENTRY {
  const char    *_fname;
  T              _ptr;
};

class GLOB_REF_MGR;
class GLOB_REF_INFO;

typedef vector<CODEREP *, mempool_allocator<CODEREP *> > TAG_OBJS;
class TAG_INFO {
public:
  IDTYPE             _model_dna_idx;
  const char*        _tag_name;
  TAG_OBJS          *_tag_objs;
  TAG_INFO(IDTYPE dna_idx, const char *name, TAG_OBJS *lists) :
    _model_dna_idx(dna_idx), _tag_name(name), _tag_objs(lists) {}
  IDTYPE      Dna_idx(void)    { return _model_dna_idx; }
  TAG_OBJS   *Tag_objs(void)   { return _tag_objs;      }
  const char *Tag_name(void)   { return _tag_name;      }
};

class RBC_CONTEXT {
private:
  DNA_NODE     *_caller;    // real source code caller
  DNA_NODE     *_callee;    // real source code callee, can be the same as _rbc_node if not defined
  DNA_NODE     *_rbc_node;  // dna that holds the symbolic evaluation expression
  TAG_INFO     *_tag_info;  // tag_info the symbolic eval referenced
  STMTREP      *_stmt;      // statement that triggers the evaluation
  MEM_POOL     *_pool;
  CXX_MEM_POOL  _spos_pool; // pool used to store srcpos info, SRCPOS_HANDLE constructor calls
                            // MEM_POOL_Push to mark memory spot, to not misuse of the memory
                            // use standalone pool to store srcpos handle

  RBC_CONTEXT(void);                           // REQUIRED UNDEFINED UNWANTED methods
  RBC_CONTEXT(const RBC_CONTEXT&);             // REQUIRED UNDEFINED UNWANTED methods
  RBC_CONTEXT& operator =(const RBC_CONTEXT&); // REQUIRED UNDEFINED UNWANTED methods

public:

  RBC_CONTEXT(DNA_NODE *caller, DNA_NODE *callee, DNA_NODE *rbc_node, RNA_NODE *rna, MEM_POOL *pool)
    : _caller(caller), _callee(callee), _rbc_node(rbc_node), _tag_info(NULL), _pool(pool),
      _spos_pool("RBC_CONTEXT spos pool", FALSE)
  { _stmt = rna == NULL ? NULL : rna->Callstmt(); }
  RBC_CONTEXT(DNA_NODE *caller, DNA_NODE *rbc_node, STMTREP *stmt, MEM_POOL *pool)
    : _caller(caller), _callee(NULL), _rbc_node(rbc_node), _stmt(stmt), _tag_info(NULL), _pool(pool),
      _spos_pool("RBC_CONTEXT spos pool", FALSE)
  {}

  DNA_NODE *Caller(void)   const { return _caller;   }
  DNA_NODE *Callee(void)   const { return _callee;   }
  DNA_NODE *Rbc_node(void) const { return _rbc_node; }
  TAG_INFO *Tag_info(void) const { return _tag_info; }
  STMTREP  *Stmt(void)     const { return _stmt;     }
  MEM_POOL *Mem_pool(void) const { return _pool;     }
  MEM_POOL *Spos_pool(void)      { return _spos_pool(); }
  void      Set_tag_info(TAG_INFO *tag_info) { _tag_info = tag_info; }
  void      Set_rbc_node(DNA_NODE *rbc_node) { _rbc_node = rbc_node; }

  STMTREP  *Callstmt(void) const
  {
    if (_stmt == NULL)
      return NULL;
    if (OPERATOR_is_call(_stmt->Opr()))
      return _stmt;
    return NULL;
  }

  RNA_NODE *Rna(void) const
  {
    if (_caller == NULL || _stmt == NULL)
      return NULL;
    if (OPERATOR_is_call(_stmt->Opr()))
      return _caller->Get_callsite_rna(_stmt);
    return NULL;
  }

  VSA      *Caller_vsa(void) const
  {
    if (_caller == NULL)
      return NULL;
    return _caller->Comp_unit()->Vsa();
  }

  IPSA     *Ipsa(void) const
  {
    if (_caller == NULL)
      return NULL;
    return _caller->Comp_unit()->Vsa()->Ipsa();
  }
};


class RBC_BASE {
  friend class VSA;
  friend class RNA_NODE;
private:
  struct streq
  {
    BOOL operator()(const char *s1, const char *s2) const
    {
      return strcmp(s1, s2) == 0;
    }
  };
  typedef void (RBC_BASE::*INIT_FUNC) (IPSA*, DNA_NODE*, RNA_NODE*, STMTREP*);
  typedef pair<const char*, INIT_FUNC> INIT_FUNC_PAIR;
  typedef mempool_allocator<INIT_FUNC_PAIR> INIT_FUNC_ALLOCATOR;
  typedef hash_map<const char*, INIT_FUNC, __gnu_cxx::hash<const char*>,
                   streq, INIT_FUNC_ALLOCATOR> INIT_FUNC_MAP;

  typedef UINT64 (RBC_BASE::*BUILTIN_FUNC) (RBC_CONTEXT&, STMTREP*);
  typedef pair<const char*, BUILTIN_FUNC> BUILTIN_PAIR;
  typedef mempool_allocator<BUILTIN_PAIR> BUILTIN_ALLOCATOR;

  typedef hash_map<const char*, BUILTIN_FUNC, __gnu_cxx::hash<const char*>,
                   streq, BUILTIN_ALLOCATOR> BUILTIN_FUNC_MAP;
  typedef mempool_allocator<SRCPOS_HANDLE*> SH_ALLOCATOR;
  typedef vector<SRCPOS_HANDLE*, SH_ALLOCATOR> SRCPOS_HANDLE_VEC;
  typedef mempool_allocator<RBC_EVAL_CERTAINTY> REC_ALLOCATOR;
  typedef vector<RBC_EVAL_CERTAINTY, REC_ALLOCATOR> RBC_EVAL_CERTAINTY_VEC;

  typedef pair<UINT32, BUILTIN_FUNC> NEW_BUILTIN_PAIR;
  typedef mempool_allocator<NEW_BUILTIN_PAIR> NEW_BUILTIN_ALLOCATOR;
  typedef hash_map<UINT32, BUILTIN_FUNC, __gnu_cxx::hash<UINT32>, __gnu_cxx::equal_to<UINT32>,
                   NEW_BUILTIN_ALLOCATOR> NEW_BUILTIN_FUNC_MAP;

  typedef FUNC_PTR_ENTRY<BUILTIN_FUNC> BUILTIN_FPTR_ENTRY;
  typedef FUNC_PTR_ENTRY<INIT_FUNC>    INIT_FPTR_ENTRY;

  typedef pair<const char*, RBC_OP>  EVAL_NAME_OP_PAIR;
  typedef mempool_allocator<EVAL_NAME_OP_PAIR> EVAL_NAME_OP_ALLOCATOR;
  typedef hash_map<const char*, RBC_OP, __gnu_cxx::hash<const char*>,
                   streq, EVAL_NAME_OP_ALLOCATOR> EVAL_NAME_OP_MAP;
  typedef struct rbc_op_desc {
    RBC_OP        _eval_op;       // evaluation operator
    INIT_FUNC     _init_ptr;      // evaluation fptr in init phase
    BUILTIN_FUNC  _eval_ptr;      // evaluation fptr in model/assert phase
    const char *  _name;          // rbc interface name
  } RBC_OP_DESC;

  typedef mempool_allocator<RULE_BODY*> RB_ALLOCATOR;
  typedef vector<RULE_BODY*, RB_ALLOCATOR> RULE_BODY_VEC;
  typedef pair<const char*, RULE_BODY_VEC*> NAME_RB_PAIR;
  typedef mempool_allocator<NAME_RB_PAIR> NAME_RB_ALLOCATOR;
  typedef hash_map<const char*, RULE_BODY_VEC*, __gnu_cxx::hash<const char*>,
                   streq, NAME_RB_ALLOCATOR> RULE_BODY_MAP;

  typedef pair<const char*, IDTYPE> NAME_ID_PAIR;
  typedef mempool_allocator<NAME_ID_PAIR> NAME_ID_ALLOCATOR;
  typedef hash_map<const char*, IDTYPE, __gnu_cxx::hash<const char*>,
                   streq, NAME_ID_ALLOCATOR> NAME_ID_MAP;
  
  typedef mempool_allocator<const char *> NAME_ALLOCATOR;
  typedef vector<const char *, NAME_ALLOCATOR> NAME_VEC;

  typedef pair<IDTYPE, TAG_CONF_INFO> TAG_ID_CONF_PAIR;
  typedef mempool_allocator<TAG_ID_CONF_PAIR> TAG_ID_CONF_ALLOCATOR;
  typedef hash_map<IDTYPE, TAG_CONF_INFO, __gnu_cxx::hash<IDTYPE>, __gnu_cxx::equal_to<IDTYPE>,
                   TAG_ID_CONF_ALLOCATOR> TAG_ID_CONF_MAP;

  typedef pair<IDTYPE, DNODE_VECTOR*> IDX_DNODES_PAIR;
  typedef mempool_allocator<IDX_DNODES_PAIR> IDX_DNODES_ALLOCATOR;
  typedef hash_map<IDTYPE, DNODE_VECTOR*, __gnu_cxx::hash<IDTYPE>, __gnu_cxx::equal_to<IDTYPE>,
                   IDX_DNODES_ALLOCATOR> IDX_DNODES_MAP;

  typedef pair<IDTYPE, RBC_OP_SET*> IDX_RBC_OPS_PAIR;
  typedef mempool_allocator<IDX_RBC_OPS_PAIR> IDX_RBC_OPS_ALLOCATOR;
  typedef hash_map<IDTYPE, RBC_OP_SET*, __gnu_cxx::hash<IDTYPE>, __gnu_cxx::equal_to<IDTYPE>,
                   IDX_RBC_OPS_ALLOCATOR > IDX_RBC_OPS_MAP;

  typedef pair<IDTYPE, char *> ID_STR_PAIR;
  typedef mempool_allocator<ID_STR_PAIR > ID_STR_ALLOCATOR;
  typedef hash_map<IDTYPE, char *, __gnu_cxx::hash<IDTYPE>, __gnu_cxx::equal_to<IDTYPE>,
                    ID_STR_ALLOCATOR > ID_STR_MAP;

  typedef mempool_allocator<TAG_INFO*> TAG_INFO_PTR_ALLOCATOR;
  typedef vector<TAG_INFO*, TAG_INFO_PTR_ALLOCATOR> TAG_INFOS;
  typedef pair<IDTYPE, TAG_INFOS*> DAN_TAG_INFOS_PAIR;
  typedef mempool_allocator<DAN_TAG_INFOS_PAIR> DAN_TAG_INFOS_ALLOCATOR;
  typedef hash_map<IDTYPE, TAG_INFOS*, __gnu_cxx::hash<IDTYPE>,
                   __gnu_cxx::equal_to<IDTYPE>, DAN_TAG_INFOS_ALLOCATOR > DNA_TAG_MAP;

  typedef pair<const char*, const char*> RULE_NAME_SET_PAIR;
  typedef mempool_allocator<RULE_NAME_SET_PAIR> RULE_NAME_SET_ALLOCATOR;
  typedef hash_map<const char*, const char*, __gnu_cxx::hash<const char *>,
                   streq, RULE_NAME_SET_ALLOCATOR> RULE_NAME_SET_MAP;

  typedef hash_set<char *, __gnu_cxx::hash<char *>, streq, mempool_allocator<char *> > STR_SET;

  MEM_POOL                   *_mem_pool;          // memory pool for rbc
  MEM_POOL                   *_loc_pool;          // local memory pool for rbc
  NEW_BUILTIN_FUNC_MAP        _builtin_func_map_new; // enumerator => builtin function pointer
  EVAL_NAME_OP_MAP            _eval_name_op_map;  // map func name to eval opcode
  RBC_EVAL_CERTAINTY_VEC     *_eval_cert;         // evaluation certainty
  SRCPOS_HANDLE_VEC          *_true_plist;        // paths that RBC evaluate to be true;
  SRCPOS_HANDLE_VEC          *_false_plist;       // paths that RBC evaluate to be false;
  BOOL                        _tracing;           // depends on VSA_DUMP_FLAG
  ID_MAP<RULE*, IDTYPE>       _rules_map;         // rules loaded
  RULE_BODY_MAP               _rule_except_map;   // rule exceptions
  IDTYPE                      _last_rule_id;

  IDTYPE                      _last_fsm_base_id;  // track the fsm_base at IPSA level
  FB_LIST                    *_fsm_base_list;     // the list of fsm obj identified
  FSM_BASE                   *_current_fsm_base;  // manage the FSM currently under construction
  BOOL                        _adjust_ofst;       // adjust offset for exec_eval call to get right opnd

  IDTYPE                      _last_tag_base_id;  // track the tag_base at IPSA level
  IDTYPE                      _last_tag_attr_id;  // track the tag attr id
  TB_LIST                    *_tag_base_list;     // the list of tag obj identified
  UINT32                      _builtin_checks;    // builtin checks
  RBC_PHASE                   _rbc_phase;         // the current phase
  NAME_ID_MAP                 _tag_attr_map;      // tag attr name => tag attr idx
  NAME_VEC                   *_tag_attr_vec;      // tag attr name vector, quary by attr id
  TAG_ID_CONF_MAP             _tag_conf_map;      // tag id = > tag config info
  NAME_ID_MAP                 _rbc_fix_cost_map;  // COMPLEXITY hack: rule name => fix cost map
  NAME_ID_MAP                 _func_dna_map;      // function name => dna id
  IDX_DNODES_MAP              _func_rbc_map;      // function => rbc DNODE_VECTOR
  IDX_RBC_OPS_MAP             _func_rbc_ops_map;  // function => RBC_OP SET 
  RULE_BODY_MAP               _annot_rbc_map;     // annot string => rbc expressions
  RULE_NAME_SET_MAP           _rule_set_map;      // rule name => rule_set
  DNA_TAG_MAP                 _dna_tag_map;       // function name => same tagged function info

  static RBC_OP_DESC          _eval_op_table[];   // eval op descriptors

  RBC_BASE(void);                                 // REQUIRED UNDEFINED UNWANTED methods
  RBC_BASE(const RBC_BASE&);                      // REQUIRED UNDEFINED UNWANTED methods
  RBC_BASE& operator = (const RBC_BASE&);         // REQUIRED UNDEFINED UNWANTED methods

  // COMPLEXITY hack
  void         Rule_fix_cost_init(void);
  UINT         Rule_fix_cost(const char *rule)
  {
    NAME_ID_MAP::iterator it = _rbc_fix_cost_map.find(rule);
    return it != _rbc_fix_cost_map.end() ? it->second : 1;
  }  // end COMPLEXITY hack

  const char *Rbc_op_name(RBC_OP op);
  RBC_OP  Get_rbc_op(const char *fname)
  {
    RBC_OP ret_op = RBC_OP_NONE;
    Is_True_Ret(fname, ("null func name"), RBC_OP_NONE);
    char *prune_name = Prune_func_name(fname);
    Is_True_Ret(prune_name, ("null prune func name for %s", fname), RBC_OP_NONE);
    EVAL_NAME_OP_MAP::iterator iter = _eval_name_op_map.find(prune_name);
    if (iter != _eval_name_op_map.end()) {
      ret_op = iter->second;
    }
    if (prune_name) {
      free(prune_name);
    }
    return ret_op;
  }

  BUILTIN_FUNC Builtin_func_new(UINT32 idx)
  {
    return _builtin_func_map_new[idx];
  }

  BUILTIN_FUNC Builtin_func(RBC_OP rbc_op)
  {
    return _eval_op_table[rbc_op]._eval_ptr;
  }

  INIT_FUNC    Init_func(RBC_OP rbc_op)
  {
    return _eval_op_table[rbc_op]._init_ptr;
  }

  void         Enter_rna_rule_map(IDTYPE rna_idx,  RULE* rule)
  {
    _rules_map.Insert(rna_idx, rule);
    Is_Trace(Tracing(), (TFile, "RBC: add RULE(%d): (rna%d) => \"%s\"\n",
                         rule->Get_id(), rna_idx, rule->Get_rule_name()));
  }

  void         Enter_rule_except_map(const char *name, RULE_BODY *except)
  {
    RULE_BODY_VEC *tmp = _rule_except_map[name];
    if (tmp == NULL) {
      tmp = CXX_NEW(RULE_BODY_VEC(RULE_BODY_VEC::allocator_type(_mem_pool)), _mem_pool);
      _rule_except_map[name] = tmp;
    }
    tmp->push_back(except);
    Is_Trace(Tracing(), (TFile, "RBC: add exception for rule(\"%s\") in DNA_NODE(%d)\n",
                         name, except->Get_dna_idx()));
  }

  void         Enter_annot_rbc_map(const char *aname, RULE_BODY *expr)
  {
    RULE_BODY_VEC *tmp = _annot_rbc_map[aname];
    if (tmp == NULL) {
      tmp = CXX_NEW(RULE_BODY_VEC(RULE_BODY_VEC::allocator_type(_mem_pool)), _mem_pool);
      _annot_rbc_map[aname] = tmp;
    }
    tmp->push_back(expr);
    Is_Trace(Tracing(), (TFile, "RBC: add annotation(\"%s\") for DNA_NODE(%d)\n",
                         aname, expr->Get_dna_idx()));
  }

  void         Enter_tag_conf_map(IDTYPE tag_id, TAG_CONF_INFO &info)
  {
    if(_tag_conf_map.find(tag_id) == _tag_conf_map.end()) {
      _tag_conf_map[tag_id] = info;
    }
  }

  const char*  Get_rule_set(const char* rule_code)
  {
    RULE_NAME_SET_MAP::const_iterator rule_it = _rule_set_map.find(rule_code);
    if (rule_it != _rule_set_map.end()) {
      return rule_it->second;
    }
    if (_rbc_fix_cost_map.find(rule_code) != _rbc_fix_cost_map.end()) {
      return "CERT";
    }
    // hard code rule_code with "CD-" prefix's rule set to "CMR"
    if (strncmp(rule_code, "CD-", 3) == 0) {
      return "CMR";
    }
    if (strncmp(rule_code, "MISRA", 5) == 0) {
      return "SML";
    }
    return "BUILTIN";
  }

  void     Set_adjust_ofst(BOOL val) { _adjust_ofst = val; }
  BOOL     Get_adjust_ofst() { return _adjust_ofst; }
  void     Switch_plist(void);
  void     Builtin_func_map_init(void);
  void     Init_dna_fuzzy_name_map(IPSA *ipsa, ID_STR_MAP &dna_fuzzy_name_map);
  BOOL     Use_fuzzy_name(char *demangled_name);
  char    *Convert_to_fuzzy_name(char *demangled_name, MEM_POOL *pool);
  IDTYPE   Extract_get_arg_idx(DNA_NODE *enclosing_dna, STMTREP *model_call, UINT32 model_idx);
  void     Set_obj_parm_flag(DNA_NODE *enclosing_dna, STMTREP *model_call, UINT32 model_idx, UINT32 parm_flag);
  TAG_BASE *Create_tag_base(DNA_NODE *enclosing_dna, STMTREP *model_call, UINT32 tag_idx);

  // Init dna phase APIs

  // FSM builder APIs
  void     Init__fsm_use(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__fsm_build_begin(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__fsm_new_start_state(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__fsm_new_final_state(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__fsm_add_transition(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__fsm_set_default_action(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__fsm_build_end(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  // TAG Init
  void     Init__if(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__not(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__is_tag_set(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__is_tag_attr_set(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__set_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__or_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__merge_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__copy_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__eval_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__set_tag_const_defval(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__set_tag_input_defval(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__set_tag_attr(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__set_tag_for_all_parm(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  // Others
  void     Init__set_class_sensitive(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  // Type Init
  void     Init__set_ty_is_mutex(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__set_ty_is_atomic(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__set_ty_is_thread(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  // Function Init
  void     Init__set_func_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);

  // Assert Init
  void     Init__do_not_call(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__is_parm_type_addr_passed(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);

  // Model Init
  void     Init__set_parm_mod(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__set_parm_deref(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void     Init__set_func_thread(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);

  // Symbolic execution APIs
  BOOL     Eval__bool_exp(RBC_CONTEXT &rbc_ctx, CODEREP *boolexp);
  UINT64   Eval__exp(RBC_CONTEXT &rbc_ctx, CODEREP *x);
  UINT64   Eval__builtin_function(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__not(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__or(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__and(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__if(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__pre_sanitized(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__pre_call(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_errno_cleared_before(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__post_check_var_value(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__post_check_var_func(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__post_call(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_func_exec_successful(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_var_used_after(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_var_defined_after(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_var_invalid_and_used_after(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_errno_checked_after(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__parm_is_def_by_func(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_called_by(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_called_in_thread(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_called_in_isr(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_called_in_loop(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_memory_overlap(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_memory_big_enough(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__func_may_enter_recursion(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__func_may_not_return(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__func_is_asynchronous_safe(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__func_performs_sanitize(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_automatic_variable(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_dynamically_allocated_if_copied(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_compatible_parm_type(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_parm_tainted(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_std_output(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__do_not_get_called(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__do_not_call(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__do_not_access_shared_obj(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__do_not_call_sleep_in_atm(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__declare_malloc_similar(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__declare_free_similar(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_tag_for_all_parm(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__unset_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_tag_attr(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_tag_set(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_tag_attr_set(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_tag_attr_set_for_all_parm(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__or_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__merge_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__copy_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__eval_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_tag_const_defval(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__decl_tag_equal(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__get_this_pointer(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__get_arg(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__get_ret(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__get_argcnt(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__get_mem_size(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__get_value(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__get_strlen(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__get_elem_count(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__get_type_name(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__get_type_kind(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__get_max_stack_size(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_parm_tainted(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_implicit_assign(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_parm_deref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_parm_mod(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_parm_base_and_fld_name(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_may_sleep(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_atomic_region_begin(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_atomic_region_end(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_atomic(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_shutdown(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_errno_setting(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_container_init(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_coll_append(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_coll_insert(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_coll_remove(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_coll_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_coll_back(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_coll_end(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_map_put(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_map_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_str_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_coll_append_ref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_coll_get_ref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_coll_back_ref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_map_put_ref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_map_get_ref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_class_sensitive(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__implicit_call(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__call_super(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__func_invoked_by_subclass(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_obj_meth_override(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__hard_coded_password(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_compression_extraction_safe(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_cst_str_eq(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_str_eq(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_str_sub(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_str_match(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_init_by_const_str(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_parm_constant(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_parm_plain_old_func(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_return_value_checked(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__is_parm_type_addr_passed(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__assume_parm(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__assume_var(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__assume_ret(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_func_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_ty_is_mutex(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_ty_is_atomic(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  UINT64   Eval__set_ty_is_thread(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);

  // assumption helper APIs
  CODEREP *Find_cr_with_name(const char* name, CODEREP *cr, STMTREP *stmt, DNA_NODE *dna);
  CODEREP *Find_cr_with_name_stmt(const char* name, STMTREP *stmt, DNA_NODE *dna);
  CODEREP *Find_cr_with_name_in_line(const char* name, UINT32 line, BB_NODE *bb, DNA_NODE *dna);
  // Evaluation APIs
  UINT64   Eval__exec_eval(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  char    *Eval__java_Property_getProperty(DNA_NODE *dna, STMTREP *call_stmt);
  char    *Eval__java_Property_load(DNA_NODE *dna, STMTREP *call_stmt, CODEREP *cr);

  // JNI APIs
  UINT64       Eval__jni_model_pragma(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);

  // Helper builtin
  UINT64       Eval__valueof(RBC_CONTEXT &rbc_ctx, STMTREP *stmt);
  
  // Symbolic execution helper APIs
  BOOL         Pre_sanitized(VSA *vsa_ctx, STMTREP *stmt, CODEREP *v, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_in_if_stmt(VSA *vsa, CODEREP *v, CODEREP *value, OPERATOR opr, STMTREP *stmt, BOOL vonly);
  BOOL         Check_cr_eq(VSA *vsa, CODEREP *v, CODEREP *value);
  BOOL         Is_called_previously(VSA *vsa_ctx, char *fname, STMTREP *latter, BB_NODE *bb,
                                    vector<IDTYPE> *path, hash_set<IDTYPE> &visited, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_var_errno(CODEREP *var, VSA *vsa);
  BOOL         Is_errno_cleared_before(STMTREP *stmt, BB_NODE *bb, BB_NODE *succ, DNA_NODE *dna,
                                       hash_set<IDTYPE> &visited_bb, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_errno_checked_after(CODEREP *cr, STMTREP *stmt, BB_NODE *bb, DNA_NODE *dna,
                                      hash_set<IDTYPE> &visited_bb, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_var_used_after(CODEREP *v, STMTREP *stmt, DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_var_used_in_stmt(CODEREP *v, STMTREP *stmt, VSA *vsa);
  BOOL         Is_var_used_in_bb(CODEREP *v, STMTREP *stmt, BB_NODE *bb, DNA_NODE *dna,
                                 hash_set<IDTYPE> &visited_bb, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_var_used_in_caller(CODEREP *v, DNA_NODE *dna, hash_set<IDTYPE> &visited_rna,
                                     SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_var_defined_after(CODEREP *v, STMTREP *stmt, DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_var_defined_in_bb(CODEREP *v, STMTREP *stmt, BB_NODE *bb, DNA_NODE *dna,
                                    hash_set<IDTYPE> &visited_bb, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_var_defined_in_caller(CODEREP *v, DNA_NODE *dna, hash_set<IDTYPE> &visited_rna,
                                        SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_var_invalid_and_used_in_bb(INV_USED_INFO &chk_info, STMTREP *stmt, BB_NODE *bb,
                                             FULL_TRAV_CONTEXT &trav_ctx, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_var_invalid_and_used_in_callee(INV_USED_INFO &chk_info, STMTREP *call_stmt,
                                                 FULL_TRAV_CONTEXT &trav_ctx, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_var_invalid_and_used_in_caller(INV_USED_INFO &chk_info, CODEREP *ret_cr,
                                                 FULL_TRAV_CONTEXT &trav_ctx, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_var_retv_of_func(DNA_NODE *dna, CODEREP *cr, STMTREP *stmt, STRING fname);
  BOOL         Is_memory_overlap(CODEREP *tgt, UINT64 size, CODEREP *src, STMTREP *stmt, DNA_NODE *dna);
  BOOL         Is_memory_big_enough(CODEREP *tgt, UINT64 elem_sz, UINT64 elem_cnt, STMTREP *stmt, DNA_NODE *dna);

  BOOL         Post_check_var(STMTREP *stmt, BB_NODE* bb, CODEREP *v, CODEREP *value, OPERATOR opr, INT direction,
                              BOOL vonly, vector<IDTYPE> *path, DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h);
  BOOL         Post_check_func(STMTREP *stmt, CODEREP *v, char *fname, char *value, DNA_NODE *dna,
                               hash_set<IDTYPE> &visited, SRCPOS_HANDLE *srcpos_h);
  BOOL         Will_call_later(VSA *vsa_ctx, char *fname, STMTREP *latter, BB_NODE *bb,
                               hash_set<IDTYPE>& visited, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_retv_def_from_super(const char *sname, DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h);
  BOOL         Func_may_enter_recursion(DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h, vector<DNA_NODE*> *visited);
  BOOL         Print_out_recursion(DNA_NODE *caller, DNA_NODE *dna, STMTREP *stmt, SRCPOS_HANDLE *srcpos_h,
                                   hash_set<IDTYPE> &visited, vector<IDTYPE> &in_stack);
  BOOL         Func_may_not_return(DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h, vector<DNA_NODE*> *visited);
  BOOL         Is_func_asynchronous_safe(DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h, vector<DNA_NODE*> *visited);
  BOOL         Is_dynamically_allocated_if_copied(VSA *vsa_ctx, CODEREP *v, SRCPOS_HANDLE *srcpos_h);
  BOOL         Is_compatible_type(TY *base, TY *v, CODEREP *var);
  BOOL         Do_not_access_shared_obj(VSA *vsa_ctx, BB_NODE *bb, SRCPOS_HANDLE *srcpos_h, vector<IDTYPE> *path);
  BOOL         Contains_shared_obj(VSA *vsa_ctx, STMTREP *stmt, SRCPOS_HANDLE *srcpos_h);
  BOOL         Contains_shared_obj(VSA *vsa_ctx, CODEREP *cr, SRCPOS_HANDLE *srcpos_h);
  BOOL         Do_not_call_sleep_in_atm(VSA *vsa_ctx, STMTREP *from, BB_NODE *bb,
                                        SRCPOS_HANDLE *srcpos_h, vector<UINT64> *path);
  UINT64       Get_elem_count(VSA *vsa_ctx, CODEREP *cr,
                              hash_set<IDTYPE> &visited_bb, hash_set<IDTYPE> &visited_rna);
  UINT64       Set_func_coll_append(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref);
  UINT64       Set_func_coll_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref);
  UINT64       Set_func_coll_back(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref);
  UINT64       Set_func_map_put(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref);
  UINT64       Set_func_map_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref);

  const char  *Generate_var_name(VSA *vsa_ctx, CODEREP *cr);
  CODEREP     *Get_ret(VSA *vsa_ctx, RNA_NODE *rna);
  ST          *Get_this_symbol(DNA_NODE *dna);
  BOOL         Is_cr_ret(VSA *vsa_ctx, RNA_NODE *rna, CODEREP *cr);
  UINT64       Is_parm_tainted(VSA *vsa_ctx, RNA_NODE *caller_rna, DNA_NODE *callee, CODEREP *arg, hash_set<uint64_t> &visited_rna_parm, SRCPOS_HANDLE *srcpos_h);
  BOOL         Set_tag(BOOL value, DNA_NODE *callee, IDTYPE parm_idx, STRING tag_name, BOOL is_ret);

  BOOL         Is_equal(const STRING name, const char * as);
  BOOL         Is_str_match(VSA *vsa_ctx, RNA_NODE *caller_rna, CODEREP *var, char *pattern, MEM_POOL *pool);
  BOOL         Is_rbc_assert(RNA_NODE *rna)            { return rna->Is_rbc_op(RBC_OP_RBC_ASSERT); }
  BOOL         Is_rbc_model_decl(RNA_NODE *rna)        { return rna->Is_rbc_op(RBC_OP_MODEL_DECL); }
  void         Find_var_init_by_const_str(DNA_NODE *dna, STMTREP *sr, CODEREP *cr, vector<SRCPOS_HANDLE *> &sp_vector);
  BOOL         Is_Pdom_bb(BB_NODE* bb, BB_NODE *check_bb);
  BOOL         Is_value_checked(BB_NODE *exit_bb, STMTREP *stmt, DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h);
  BOOL         Do_not_call(DNA_NODE *dna, char *fname, SRCPOS_HANDLE *srcpos_h, hash_set<IDTYPE> * visited_dna);
  BOOL         Is_called_in_dna_with_path(IPSA *ipsa, DNA_NODE *cur_dna, UINT32 dna_flag, SRCPOS_HANDLE *sp_h, hash_set<IDTYPE> * visited_dna);
  BOOL         Is_called_in_loop(RNA_NODE *rna, DNA_NODE *cur_dna, SRCPOS_HANDLE *sp_h, hash_set<IDTYPE> * visited_dna);

  // container op symbolic execution
  EVAL_RET     Eval__container_init(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, void *objs, BOOL is_forward);
  EVAL_RET     Eval__coll_remove(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, void *objs, BOOL is_forward);
  EVAL_RET     Eval__coll_append(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, void *objs, BOOL deref, BOOL is_forward);
  EVAL_RET     Eval__coll_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref, BOOL is_forward);
  EVAL_RET     Eval__coll_back(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref, BOOL is_forward);
  EVAL_RET     Eval__coll_insert(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, void *objs, BOOL deref, BOOL is_forward);
  EVAL_RET     Eval__coll_end(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL is_forward);
  EVAL_RET     Eval__map_put(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, void *objs, BOOL deref, BOOL is_forward);
  EVAL_RET     Eval__map_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref, BOOL is_forward);
  EVAL_RET     Eval__str_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL is_forward);
  template <typename T>
  EVAL_RET     Eval__container_get(VSA *vsa_ctx, RNA_NODE *caller_rna,
                                   MEM_POOL *pool, COLL_POSITION pos, CODEREP *key,
                                   VALUE_OBJS<T> &objs, VALUE_OBJS<T> &value_cands);
  template <typename T>
  T            Get_value_obj(VALUE_OBJS<T> &objs, STMTREP *sr, COLL_POSITION pos, CODEREP *key, COMP_UNIT *cu, MEM_POOL *pool);
  STMTREP     *Get_model_decl_func(RNA_NODE *rna, UINT32 offset);
  STMTREP     *Get_rbc_nth_call(STMTREP *rbc_call, UINT32 offset);
  void         Process_model_decl_func(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call);
  void         Process_rbc_assert_func(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call);
  void         Process_rbc_disable_rule(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call);
  void         Process_rbc_rule_exception(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call);
  void         Process_rbc_same_as_func(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call);
  void         Process_rbc_rule_set(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call);
  void         Process_rbc_annotate(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call);
  void         Process_rbc_instr_range(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call);
  void         Process_rbc_instr_ne(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call);
  void         Process_rbc_instr_compare(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call);
  void         Process_rbc_set_builtin(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call);
  void         Process_rbc_for_all_func(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call);
  void         Propagate_fsm_use(IPSA *ipsa, DNA_NODE *dna);
  void         Propagate_fsm_list(DNA_NODE *tgt, DNA_NODE *src, MEM_POOL *pool);
  BOOL         Find_arg_in_callee(VSA *vsa, CODEREP *arg, RNA_NODE *rna,
                                  DNA_NODE *callee, CODEREP **ret);
  BOOL         Find_fsm_key_in_callee(VSA *vsa, FSM_OBJ_REP *fsm_obj_rep, CODEREP *key,
                                      RNA_NODE *rna, DNA_NODE *callee, CODEREP **ret);
  BOOL         Find_fsm_key_in_caller(VSA *vsa, FSM_OBJ_REP *fo, RNA_NODE *rna,
                                      DNA_NODE *caller, CODEREP **ret);
  CODEREP     *Match_fsm_key(VSA *vsa, FSM_OBJ_REP *fsm_obj_rep, CODEREP *ori_key,
                             CODEREP *new_key, hash_set<IDTYPE> &visited);
  STRING_VEC  *Build_vec_from_string(STRING str, MEM_POOL *pool);

  FB_LIST     *Fsm_base_list(void)const                { return _fsm_base_list; }
  MEM_POOL    *Mem_pool(void)                          { return _mem_pool; }
  MEM_POOL    *Loc_pool(void)                          { return _loc_pool; }

public:
  RBC_BASE(MEM_POOL *gpool, MEM_POOL *loc_pool);
  ~RBC_BASE();

  BOOL         Is_builtin_check(UINT32 c) const        { return (_builtin_checks & c) != 0; }
  void         Set_builtin_check(UINT32 c)             { _builtin_checks |= c; }
  BOOL         Tracing(void) const                     { return _tracing; }
  UINT32       Rbc_parm_offset(DNA_NODE *dna) const    { return dna->Rbc_parm_offset(); }

  void         Set_rbc_phase(RBC_PHASE rbc_phase)      { _rbc_phase = rbc_phase; }
  RBC_PHASE    Rbc_phase()                             { return _rbc_phase; }
  
  // adjust parm offset if evaluate with Eval_exec
  UINT32       Rbc_parm_ofst_adjust(DNA_NODE *dna) const
  { 
    return _adjust_ofst ? dna->Rbc_parm_offset() + 1 : dna->Rbc_parm_offset(); 
  }
  void         Rbc_init(void);
  RBC_EVAL_CERTAINTY_VEC *Rbc_eval_certainty(void) { return _eval_cert;  }
  SRCPOS_HANDLE_VEC      *Plist_false(void)        { return _false_plist; }
  SRCPOS_HANDLE_VEC      *Plist_true(void)         { return _true_plist; }
  BOOL         Rbc_result_maybe(void) const
  {
    return !_eval_cert->empty() && _eval_cert->back() != REC_DEFINITE;
  }
  BOOL         Rbc_eval_skip(void) const
  {
    return !_eval_cert->empty() && _eval_cert->back() == REC_SKIP;
  }
  BOOL         Rbc_result_ignore(void) const
  {
    return (!_eval_cert->empty() &&
            (_eval_cert->back() == REC_SKIP ||
             _eval_cert->back() == REC_UNKNOWN));
  }

  char        *Prune_func_name(const char *name);  // demangle & prune function name to search builtin functions
  RULE        *Get_rule(IDTYPE rna_idx)
  {
    return _rules_map.Lookup(rna_idx);
  }

  RULE_BODY_VEC *Get_rule_exceptions(const char *name)
  {
    return _rule_except_map[name];
  }

  RULE_BODY_VEC *Get_annot_rbc(const char *aname)
  {
    return _annot_rbc_map[aname];
  }

  void          Add_rbc_node(DNA_NODE *dna, DNA_NODE *rbc_dna);
  DNODE_VECTOR *Get_rbc_nodes(DNA_NODE *dna)
  {
    if (dna == NULL)
      return NULL;
    IDX_DNODES_MAP::iterator iter = _func_rbc_map.find(dna->Get_rbc_ref_idx());
    if (iter != _func_rbc_map.end()) {
      return iter->second;
    }
    return NULL;
  }
  void         Add_rbc_ops(DNA_NODE *dna, DNA_NODE *rbc_dna);
  RBC_OP_SET  *Get_rbc_ops(DNA_NODE *dna)
  {
    if (dna == NULL)
      return NULL;
    IDX_RBC_OPS_MAP::iterator iter = _func_rbc_ops_map.find(dna->Get_rbc_ref_idx());
    if (iter != _func_rbc_ops_map.end()) {
      return iter->second;
    }
    return NULL;
  }

  TAG_CONF_INFO* Get_tag_conf_info(IDTYPE tag_id) {
    TAG_ID_CONF_MAP::iterator iter = _tag_conf_map.find(tag_id);
    if(iter == _tag_conf_map.end()) {
      return NULL;
    }
    return &(iter->second);
  }

  ST          *Get_cr_st(VSA *vsa_ctx, CODEREP *cr);
  TY          *Get_cr_ty(VSA *vsa_ctx, CODEREP *cr);
  UINT64       Get_mem_size(VSA *vsa_ctx, CODEREP *cr);
  BOOL         Get_mem_size(VSA *vsa_ctx, CODEREP *cr, UINT64 &size);
  UINT64       Get_value(VSA *vsa_ctx, CODEREP *cr);
  char        *Find_const_char(DNA_NODE *dna, CODEREP *cr);
  BOOL         Find_const_char_cross(VSA* vsa_ctx, STMTREP *, CODEREP *cr, STR_SET& str_set, MEM_POOL *pool);
  UINT64       Get_initv_strlen(DNA_NODE *dna, CODEREP *cr);
  BOOL         Is_const(DNA_NODE *dna, CODEREP *cr);
  void         Register_rules(IPSA *ipsa, DNA_NODE *func, vector<std::pair<char*, IDTYPE> >*);
  void         Get_param_list_types(DNA_NODE *dna, vector<TY*> &tlist);
  void         Link_dna_for_rbc(IPSA *ipsa);   // propogate rbc flags to source dna
  void         Link_rule_for_rbc(IPSA *ipsa);  // rule linking for Rbc_apply_rule
  void         Link_rule_for_annot(IPSA *ipsa, DNA_NODE *dna);
  void         Build_rbc_func_name_map(IPSA *ipsa); // build map for rbc node, key function name, value dna id
  IDTYPE       Get_dna_idx_by_name(char *func_name)
  {
    if (_func_dna_map.find(func_name) != _func_dna_map.end()) {
      return _func_dna_map[func_name];
    }
    // invalid dna idx
    return 0;
  }
  void         Insert_fname_dna_idx(char *fname, IDTYPE idx)
  {
    if (_func_dna_map.find(fname) == _func_dna_map.end()) {
      _func_dna_map[Clone_string(fname, Mem_pool())] = idx;
    }
  }
  TAG_INFO *Find_dna_tag_info(const char *tag_name, DNA_NODE *dna)
  {
    if (!dna->Is_set_rbc_flag(DNA_RBC_FUNC_TAG)) {
      return NULL;
    }
    if (dna->Non_functional()) {
      DNA_TAG_MAP::iterator iter = _dna_tag_map.find(dna->Dna_idx());
      if (iter == _dna_tag_map.end()) {
        return NULL;
      } else {
        TAG_INFOS *tag_infos = iter->second;
        Is_True_Ret(tag_infos, ("null func tag_infos for fun %s with tag %s", dna->Fname(), tag_name), NULL);
        TAG_INFOS::iterator tag_iter;
        for (tag_iter = tag_infos->begin(); tag_iter != tag_infos->end(); tag_iter++) {
          TAG_INFO *tag_info = *tag_iter;
          Is_True_Ret(tag_info && tag_info->_tag_name, ("null tag info"), NULL);
          if (strcmp(tag_info->_tag_name, tag_name) == 0) {
            return tag_info;
          }
        }
      }
    } else {
      DNODE_VECTOR *rbc_nodes = Get_rbc_nodes(dna);
      if (rbc_nodes == NULL)
        return NULL;
      DNODE_VECTOR::const_iterator rbc_iter = rbc_nodes->begin();
      for (; rbc_iter != rbc_nodes->end(); rbc_iter++) {
        DNA_NODE *rbc_callee = *rbc_iter;
        if (rbc_callee != NULL) {
          TAG_INFO *tag_info = Find_dna_tag_info(tag_name, rbc_callee);
          if (tag_info) {
            return tag_info;
          }
        }
      }
    }
    return NULL;
  }

  void Enter_dna_tag_info(const char* tag_name, DNA_NODE *dna, TAG_OBJS *lists)
  {
    Is_True(!Find_dna_tag_info(tag_name, dna),
            ("dna %s with tag %s already added", tag_name, dna->Fname()));

    TAG_INFOS *tag_infos = NULL;
    DNA_TAG_MAP::iterator iter = _dna_tag_map.find(dna->Dna_idx());
    if (iter == _dna_tag_map.end()) {
      tag_infos = CXX_NEW(TAG_INFOS(TAG_INFO_PTR_ALLOCATOR(Mem_pool())), Mem_pool());
      _dna_tag_map[dna->Dna_idx()] = tag_infos;
    } else {
      tag_infos = iter->second;
    }
    Is_True(tag_infos, ("null func tag_infos for fun %s with tag %s", dna->Fname(), tag_name));
    if (tag_infos) {
      TAG_INFO *info = CXX_NEW(TAG_INFO(dna->Dna_idx(), Clone_string((char*)tag_name, Mem_pool()), lists), Mem_pool());
      tag_infos->push_back(info);
    }
  }
  TAG_INFO *Find_rna_tag_info(IPSA *ipsa, const char *tag_name, RNA_NODE *rna);
  CODEREP  *Get_tag_obj(VSA *vsa, const char *tag_name,
                        RNA_NODE *caller_rna, UINT32 index);
  CODEREP  *Get_tag_obj(RBC_CONTEXT &rbc_ctx, TAG_INFO *tag_info, UINT32 index);

  void         Process_rbc_builtin(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, ST *st); // preprocess builtin functions
  void         Process_rbc_exec_path(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, MEM_POOL *pool);
  void         Eval__mvsa_model(DNA_NODE *caller, RNA_NODE *caller_rna, MEM_POOL *pool);
  void         Eval__mvsa_assert(DNA_NODE *caller, RNA_NODE *caller_rna, RNA_FLAGS assert_flag);
  void         Eval__mvsa_assert(DNA_NODE *callee, DNA_NODE *rbc_callee, RNA_NODE *caller_rna, DNA_NODE *caller, RNA_FLAGS assert_flag);
  void         Eval__annot_model(DNA_NODE *caller, RNA_NODE *caller_rna, DNA_NODE *callee, MEM_POOL *pool);
  void         Eval__annot_assert(DNA_NODE *caller, RNA_NODE *caller_rna, DNA_NODE *callee);
  void         Eval__base_assert(DNA_NODE *caller, RNA_NODE *caller_rna, DNA_NODE *callee,
                                 DNA_NODE *rbc_callee, STRING rule_name, CODEREP *boolexp);
  void         Eval__mvsa_tag_op(DNA_NODE *caller, RNA_NODE *caller_rna, MEM_POOL *pool);
  EVAL_RET     Eval__mvsa_container_op(DNA_NODE *caller, RNA_NODE *caller_rna,
                                       MEM_POOL *pool, void *objs, BOOL is_forward);

  void         Report_rbc_error(VSA *vsa_ctx, SRCPOS spos, const char* rule, BOOL maybe, SRCPOS_HANDLE *srcpos_h,
                                const char* cname = NULL);
  void         Report_rbc_error(VSA *vsa_ctx, STMTREP *stmt, const char* rule, BOOL maybe, SRCPOS_HANDLE *srcpos_h,
                                const char* cname = NULL);
  void         Report_rbc_error(DNA_NODE *dna, const char *stname, const char *rule, const char *msg_id, BOOL maybe);
  void         Report_fsm_error(VSA *vsa_ctx, FSM_TRAV_CONTEXT *fsm_ctx, STMTREP *stmt, FSM_OBJ_REP *fo,
                                TRANSIT *ts, SRCPOS_HANDLE *srcpos_h, FSM_ERR_KIND kind);
  
  void          Report_xsca_error(VSA *vsa_ctx, SRCPOS spos, const char* rule,
                                  SRCPOS_HANDLE *srcpos_h);

  // Finite State Machine Management
  STRING       Clone_string(STRING name, MEM_POOL *pool) {
    STRING cloned_name = (STRING) CXX_NEW_ARRAY(BOOL, (strlen(name)+1), pool);
    strcpy(cloned_name, name);
    return cloned_name;
  }
  IDTYPE       New_fsm_base_id(void)                   { return (_last_fsm_base_id == MAX_ID)?
                                                                 MAX_ID:_last_fsm_base_id++;}
  FSM_BASE    *Current_fsm_base(void) const            { return _current_fsm_base; }
  void         Set_current_fsm_base(FSM_BASE *fo)      { _current_fsm_base = fo; }
  void         Reset_current_fsm_base(void)            { _current_fsm_base = NULL; }
  FSM_BASE    *Find_fsm_base(STRING name)              { return _fsm_base_list->Find(name); }
  FSM_BASE    *New_fsm_base(STRING name);
  FSM_BASE    *Find(STRING name)const                  { return _fsm_base_list->Find(name); }

  IDTYPE       New_tag_base_id(void)                   { return (_last_tag_base_id == MAX_ID)?
                                                                 MAX_ID:_last_tag_base_id++;}
  TAG_BASE    *Find_tag_base(STRING name)              { return _tag_base_list->Find(name); }
  TAG_BASE    *Find_tag_base(IDTYPE tag_id)
  {
    TB_LIST_ITER iter;
    TAG_BASE *tb;
    FOR_ALL_ELEM(tb, iter, Init(_tag_base_list)) {
      if (tb->Id() == tag_id)
        return tb;
    }
    return NULL;
  }
  TAG_BASE    *New_tag_base(STRING name);
  TB_LIST     *Tag_base_list()                         { return _tag_base_list; }
  IDTYPE       Add_tag_attr(const char *attr_name)
  {
    NAME_ID_MAP::iterator iter = _tag_attr_map.find(attr_name);
    if (iter == _tag_attr_map.end()) {
      IDTYPE attr_id = _last_tag_attr_id;
      char *dup_name = Clone_string((STRING) attr_name, Mem_pool());
      _tag_attr_map[dup_name] = attr_id;
      _last_tag_attr_id++;
      _tag_attr_vec->push_back(attr_name);
      Is_Trace(Tracing(), (TFile, "RBC: Add tag attr %s:%d\n", attr_name, attr_id));
      return attr_id;
    } else {
      return iter->second;
    }
  }
  IDTYPE       Get_tag_attr_id(const char *attr_name)
  {
    if (_tag_attr_map.find(attr_name) == _tag_attr_map.end()) {
      Is_Trace(Tracing(), (TFile, "RBC: Get_tag_attr_id: attr name not exist, attr name : %s.\n", attr_name));
      return TAG_INVALID_ID;
    }
    return _tag_attr_map[attr_name];
  }
  const char  *Get_tag_attr_name(IDTYPE attr_id)
  {
    if (attr_id == TAG_INVALID_ID) {
      Is_Trace(Tracing(), (TFile, "RBC: Get_tag_attr_name: invalid attr id.\n"));
      return "";
    }
    Is_True(attr_id != TAG_INVALID_ID, ("Tag attr id is not valid."));
    return _tag_attr_vec->at(attr_id);
  }
  UINT32       Get_attr_number() { return _tag_attr_vec->size() - TAG_START_ID; }

  // Tag Checking
  BOOL         Check_tag(CHECK_OBJ &obj, TRAV_CONTEXT &ctx, TAG_CHECK_TYPE type,
                         TAG_BASE *tag_base, SRCPOS_HANDLE *sp_h = NULL, 
                         IDTYPE attr_id = TAG_INVALID_ID);
  BOOL         Check_tag(DNA_NODE *dna, STMTREP *sr, CODEREP *cr,
                         TAG_CHECK_TYPE type, TAG_BASE *tag_base,
                         SRCPOS_HANDLE *sp_h = NULL, IDTYPE attr_id = TAG_INVALID_ID);

  // Finite State Machine Checking
  void         Evaluate_and_check_FSMs(IPSA *ipsa);
  void         Match_start_trans_and_eval(VSA *vsa, BB_NODE *bb, MEM_POOL *pool);
  void         Perform_fsm_check_bb(FSM_OBJ_REP *fsm_obj_rep, STMTREP *stmt, BB_NODE *bb,
                                    FSM_TRAV_CONTEXT &fsm_ctx, SRCPOS_HANDLE *srcpos_h);
  FSM_OBJ_REP *Perform_fsm_check_stmt(FSM_OBJ_REP *fsm_obj_rep, STMTREP *stmt, BB_NODE *bb,
                                      FSM_TRAV_CONTEXT &fsm_ctx, SRCPOS_HANDLE *srcpos_h);
  BOOL         Perform_fsm_check_caller(FSM_OBJ_REP *fsm_obj_rep, FSM_TRAV_CONTEXT &fsm_ctx,
                                        SRCPOS_HANDLE *srcpos_h);
  BOOL         Perform_fsm_check_callee(FSM_OBJ_REP *fsm_obj_rep, STMTREP *stmt,
                                        FSM_TRAV_CONTEXT &fsm_ctx, SRCPOS_HANDLE *srcpos_h);
  STRING       Generate_fsm_error_var(FSM *fsm, TRANSIT *ts, IDTYPE state, IDTYPE nstate);
  const char  *Generate_fsm_key_str(CODEREP *key, STMTREP *stmt, STRING act, DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h);
  BOOL         Match_trans_with_hook(FSM_OBJ_REP *fsm_obj_rep, FSM_TRAV_CONTEXT &fsm_ctx,
                                     SRCPOS_HANDLE *srcpos_h);
  CODEREP     *Find_thread_init_key(BB_NODE *bb, CODEREP *cr, hash_set<IDTYPE>&);
  CODEREP     *Find_thread_run_key(VSA *run_vsa, BB_NODE *bb,
                                   VSA *init_vsa, CODEREP *cr, hash_set<IDTYPE>&);
  STMTREP     *Get_last_stmt(BB_NODE *bb);
  STMTREP     *Get_init_stmt(VSA *vsa, STMTREP *stmt, CODEREP *cr, DNA_NODE** def_dna);
  STMTREP     *Get_ctor_by_alloc(DNA_NODE *dna, STMTREP *alloc_stmt);
  DNA_NODE    *Get_runnable_dna(IPSA *ipsa, STMTREP *init_stmt);
  BB_LOOP     *Find_bb_loop(CFG *cfg, BB_NODE *bb);
  CODEREP     *Find_self_inc_include_cr(OPT_STAB *opt_stab, BB_NODE *bb, CODEREP *cr);

  // Builtin Checks
  void         Global_builtin_certc_check(IPSA *ipsa);
  void         Builtin_recursion_check(IPSA *ipsa, MEM_POOL *pool);
  void         Builtin_certc_msc37(DNA_NODE *dna);
  void         Builtin_certcpp_str50(DNA_NODE *dna);
  BOOL         Builtin_certcpp_str50(STMTREP *stmt, BB_NODE *bb, hash_set<IDTYPE> &visited);
  void         Builtin_cwe390_ewa(DNA_NODE *dna);
  void         Builtin_cwe390_ewa(STMTREP *stmt, BB_NODE *bb, BOOL icb, VSA *vsa, hash_set<IDTYPE> &visited);
  void         Builtin_ctor_init(DNA_NODE *dna);
  CODEREP     *Find_this_cr(VSA *vsa, BB_NODE *bb, DNA_NODE *dna, hash_set<IDTYPE> &visited);
  void         Builtin_ctor_init(VSA *vsa, BB_NODE *bb, hash_set<UINT> &fields, hash_set<IDTYPE> &visited);
  void         Global_builtin_certj_check(IPSA *ipsa);
  void         Builtin_certj_env06(DNA_NODE *dna);
  void         Builtin_certj_met06(IPSA *ipsa, DNA_NODE *dna);
  void         Builtin_certj_msc03(DNA_NODE *dna);
  void         Builtin_certj_msc03_bb(DNA_NODE *dna, BB_NODE *bb, regex_t **reg_exp_arr, INT len);
  void         Builtin_certj_obj07(IPSA *ipsa, DNA_NODE *dna);
  void         Builtin_certj_dcl00(IPSA *ipsa, DNA_NODE *dna);
  // helper functions
  BOOL         Check_class_canbe_copied(IPSA *ipsa, const char *class_name,
                                        const char *rule, const char *msg_prefix);
  BOOL         Check_class_init_cycle(DNA_NODE *check_dna, const char *check_cls,
                                      STMTREP *check_sr, GLOB_REF_INFO *ref_info,
                                      GLOB_REF_MGR *ref_mgr, BOOL cross_class,
                                      hash_set<IDTYPE> *visited_dna,
                                      SRCPOS_HANDLE *sp_h);
  BOOL         Is_func_overridable(UINT32 flag);
  BOOL         Has_arg_address_taken(DNA_NODE *dna);
  BOOL         Has_set_cr(BB_NODE *bb, CODEREP *cr, VSA *vsa, SRCPOS_HANDLE *srcpos_h);
  BOOL         Has_cmp_cr(BB_NODE *bb, VSA *vsa);
  BOOL         Has_cmp_set_race(DNA_NODE *dna);
  void         Dump_FSMs(FILE *fp);
};

class UINT64_COMPARER {
private:
  OPERATOR       _opr;  // compare operator

  UINT64_COMPARER(void);                                // REQUIRED UNDEFINED UNWANTED methods
  UINT64_COMPARER(const UINT64_COMPARER&);              // REQUIRED UNDEFINED UNWANTED methods
  UINT64_COMPARER& operator = (const UINT64_COMPARER&); // REQUIRED UNDEFINED UNWANTED methods

public:
  UINT64_COMPARER (OPERATOR opr): _opr(opr) {}

  BOOL operator()(UINT64 lhs, UINT64 rhs) {
    BOOL ret = FALSE;
    switch(_opr) {
    case OPR_LT:
      ret = (lhs < rhs);
      break;
    case OPR_LE:
      ret = (lhs <= rhs);
      break;
    case OPR_GT:
      ret = (lhs > rhs);
      break;
    case OPR_GE:
      ret = (lhs >= rhs);
      break;
    case OPR_EQ:
      ret = (lhs == rhs);
      break;
    case OPR_NE:
      ret = (lhs != rhs);
      break;
    default:
      Is_True(FALSE, ("RBC ERROR: OPCODE:%d not implemented yet.\n", _opr));
      break;
    }
    return ret;
  }
};

#endif  // opt_vsa_rbc_INCLUD
