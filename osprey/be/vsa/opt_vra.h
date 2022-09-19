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

#ifndef opt_vra_INCLUDED
#define opt_vra_INCLUDED    "opt_vra.h"

#include "opt_main.h"
#include "opt_cfg.h"
#include "opt_fold.h"
#include "const.h"
#include <vector>
#include <list>
#include <ext/hash_set>
#include <ext/hash_map>
using __gnu_cxx::hash_set;
using __gnu_cxx::hash_map;

namespace __gnu_cxx {
  template<>
  struct hash<CODEREP*> {
    size_t operator() (const CODEREP* cr) const { return (size_t)cr; }
  };
}

class DNA_NODE;
class IPSA;
class VSA;

// Allow entry in expr table can be visited 5 times in one value range query
// TODO: configurable??
#define VISIT_MAX_COUNT 2

// complement operator for value range analysis
enum VRA_OPERATOR {
  OPR_CODEREP = OPERATOR_LAST + 1, // the node performs operation according to the CR
                                   // OPERATOR_LAST is defined in enum OPERATOR in opcode_gen_core.h
  OPR_VRA_FIRST = OPR_CODEREP,     // first opr for VRA
  OPR_PLACEHOLDER,                 // similar to OPR_CODEREP, cr is the variable itself
                                   // used for phi's operand whose pred isn't visited yet
  OPR_TOP,                         // the node evaluates to top
  OPR_BOTTOM,                      // the node evaluates to bottom
  OPR_EMPTY,                       // the node means the value set is empty
  OPR_CONJUNCTION,                 // the node performs a conjunction operation
  OPR_DISJUNCTION,                 // the node performs a disjunction operation
  OPR_VRA_LAST,
};

// value range query result. ATTN: order matters!
enum VRA_RESULT {
  VA_IGNORE,   // ignore this result
  VA_UNKNOWN,  // not enough information
  VA_POSSIBLE, // the value is possibly in the range queried
  VA_YES,      // the value is in the range queried
  VA_NO,       // the value is not the range queried
  VA_LAST = VA_NO
};

// value bound
enum VRA_BOUND {
  VA_NB = 0x0, // no bound
  VA_LB = 0x1, // lower bound
  VA_UB = 0x2, // upper bound
  VA_FB = 0x3, // full bound
};

extern const char *VRA_OPERATOR_NAME(VRA_OPERATOR opr);
extern const char *VRA_RESULT_NAME(VRA_RESULT res);

// value range expression
class VRA_EXPR {
private:
  UINT8  _opr;        // VRA_OPERATOR
  IDTYPE _bb_id : 24;
  union {
    CODEREP* _cr;     // pointer to CODEREP for OPR_CODEREP
    struct {
      UINT32 _op0;    // left kid of CONJ/DISJ, index to EXPR_TABLE
      UINT32 _op1;    // right kid of CONJ/DISJ, index to EXPR_TABLE
    } _opnd;
  };

  // disable default constructor
  VRA_EXPR();
  // disable copy constructor
  VRA_EXPR(const VRA_EXPR&);
  // disable assign operator
  VRA_EXPR& operator=(const VRA_EXPR&);

public:
  // init VRA_EXPR for CODEREP
  void Init(CODEREP *cr, UINT32 bb, BOOL placeholder = FALSE)
  {
    _opr = placeholder ? OPR_PLACEHOLDER : OPR_CODEREP;
    _bb_id = bb;
    _cr = cr;
  }

  // Init VRA_EXPR for given opr, bb and two kids
  void Init(VRA_OPERATOR opr, UINT32 bb, UINT32 op0, UINT32 op1)
  {
    Is_True(opr != OPR_CODEREP, ("opr cannot be OPR_CODEREP"));
    _opr = opr;
    _bb_id = bb;
    _opnd._op0 = op0;
    _opnd._op1 = op1;
  }

  // Copr fields except bb from expr
  void Copy(const VRA_EXPR& expr, UINT32 bb)
  {
    _opr = expr._opr;
    _bb_id = bb;
    _cr = expr._cr;
  }

  // Get VRA_OPERATOR
  VRA_OPERATOR Opr() const { return (VRA_OPERATOR)_opr; }

  // Get BB id
  IDTYPE Bb_id() const { return _bb_id; }

  // Get CODEREP only if _opr is OPR_CODEREP
  CODEREP* Cr() const
  {
    Is_True(_opr <= OPR_CODEREP || _opr == OPR_PLACEHOLDER, ("not a CR"));
    return _cr;
  }

  // Get operand 0 for CONJ/DISJ
  UINT32 Op0() const
  {
    Is_True(_opr == OPR_CONJUNCTION || _opr == OPR_DISJUNCTION,
            ("invalid operator for opnd"));
    return _opnd._op0;
  }

  // Set operand 0 for CONJ/DISJ
  void Set_op0(UINT32 id)
  {
    Is_True(_opr == OPR_CONJUNCTION || _opr == OPR_DISJUNCTION,
            ("invalid operator for opnd"));
    _opnd._op0 = id;
  }

  // Get operand 1 for CONJ/DISJ
  UINT32 Op1() const
  {
    Is_True(_opr == OPR_CONJUNCTION || _opr == OPR_DISJUNCTION,
            ("invalid operator for opnd"));
    return _opnd._op1;
  }

  // Set operand 0 for CONJ/DISJ
  void Set_op1(UINT32 id)
  {
    Is_True(_opr == OPR_CONJUNCTION || _opr == OPR_DISJUNCTION,
            ("invalid operator for opnd"));
    _opnd._op1 = id;
  }

  // Get operand i for CONJ/DISJ
  UINT32 Opnd(INT i) const
  {
    Is_True(i == 0 || i == 1,
            ("invalid opnd index"));
    return i == 0 ? Op0() :
             i == 1 ? Op1() : -1;
  }

};

// ============================================================================
// VRA_PATH_CACHE
//  cache path reachability information calculated by VRA
//  cache content: <from, to, reachability>
// ============================================================================
class VRA_PATH_CACHE {
private:
  struct CACHE_ENTRY {
    UINT32 _id;     // from_bb id
    UINT32 _block;  // block index
  };

  enum { ENTRY_COUNT = 4 };           // to_bb count in each block
  enum { RESULT_BIT = 2 };            // result bit
  enum { BB_ID_MASK = 0xFFFFFFFCUL }; // BB id mask
  enum { RESULT_MASK = 0x3 };         // result mask
  struct CACHE_BLOCK {
    UINT32 _next;              // next block in data array
    UINT32 _data[ENTRY_COUNT]; // 30b to_bb id + 2b reachability
  };

  typedef SEGMENTED_ARRAY<CACHE_ENTRY, 16> ENTRY_ARRAY;
  typedef SEGMENTED_ARRAY<CACHE_BLOCK, 16> BLOCK_ARRAY;

  ENTRY_ARRAY _entry_array;  // array of cache entries for from_bb
  BLOCK_ARRAY _block_array;  // array of cache block for to_bb (31b) and reachability (1b)
  UINT32      _last_entry;   // last entry visited
  UINT32      _last_block;   // last block visited

  VRA_PATH_CACHE(const VRA_PATH_CACHE&);             // disable copy ctor
  VRA_PATH_CACHE& operator=(const VRA_PATH_CACHE&);  // disable assignment oper

public:
  // RESULT
  enum RESULT { UNREACHABLE = 0, REACHABLE = 1, MAY_REACHABLE = 2, NO_ENTRY = 3, LAST_RESULT = 3 };

  // ctor
  VRA_PATH_CACHE(MEM_POOL* mp)
    : _entry_array(mp), _block_array(mp),
      _last_entry(UINT32_MAX), _last_block(UINT32_MAX) { }

  // get RESULT description for tracing
  const char* RESULT_desc(RESULT res) {
    const char* desc[] = { "unreachable", "reachable", "may_reachable", "no_entry" };
    return desc[res];
  }
  VRA_RESULT Get_vra_result(RESULT res) {
    // mapping RESULT to VRA_RESULT
    VRA_RESULT mapping[] = { VA_NO,         // UNREACHABLE
                             VA_YES,        // REACHABLE
                             VA_POSSIBLE,   // MAY_REACHABLE
                             VA_UNKNOWN     // NO_ENTRY
                           };
    Is_True_Ret(res <= LAST_RESULT, ("res outof bound"), VA_UNKNOWN);
    return mapping[res];
  }
  RESULT Get_result(VRA_RESULT vra_res) {
    // mapping VRA_RESULT to RESULT
    RESULT mapping[] = { REACHABLE,         // VA_IGNORE
                         REACHABLE,         // VA_UNKNOWN
                         MAY_REACHABLE,     // VA_POSSIBLE
                         REACHABLE,         // VA_YES
                         UNREACHABLE        // VA_NO
                       };
    Is_True_Ret(vra_res <= VA_LAST, ("vra_res outof bound"), NO_ENTRY);
    return mapping[vra_res];
  }

private:
  // Find the entry for from_bb. If not exist, an new entry created
  CACHE_ENTRY& Find_entry(UINT32 from) {
    if (_last_entry != UINT32_MAX &&
        _entry_array[_last_entry]._id == from)
      return _entry_array[_last_entry];

    // reset _last_block
    _last_block = UINT32_MAX;

    if (_entry_array.Size() > 0) {
      ENTRY_ARRAY::iterator it;
      ENTRY_ARRAY::iterator end = _entry_array.end();
      for (it = _entry_array.begin(); it != end; ++it) {
        if (it->_id == from) {
          // set _last_entry to the new entry
          _last_entry = it.Index();
          return *it;
        }
      }
    }
    // create new entry, set _last_entry to the new entry
    CACHE_ENTRY& entry = _entry_array.New_entry(_last_entry);
    entry._id = from;
    entry._block = UINT32_MAX;
    return entry;
  }

  // Find reachability from CACHE_BLOCK. key is (to_bb << RESULT_BIT).
  RESULT Find_in_block(const CACHE_BLOCK& block, UINT32 key) {
    for (INT i = 0; i < ENTRY_COUNT; ++i) {
      UINT32 data = block._data[i];
      if ((data & BB_ID_MASK) == key)
        return (RESULT)(data & RESULT_MASK);
    }
    return NO_ENTRY;
  }

  // Find reachability in CACHE_BLOCK belongs to the CACHE_ENTRY
  RESULT Find_data(CACHE_ENTRY& entry, UINT32 to) {
    UINT32 key = (to << RESULT_BIT);
    if (_last_block != UINT32_MAX) {
      // search _last_block at first
      RESULT res = Find_in_block(_block_array[_last_block], key);
      if (res != NO_ENTRY)
        return res;
    }

    // search blocks belong to the entry
    UINT32 block = entry._block;
    while (block != UINT32_MAX) {
      CACHE_BLOCK& cb = _block_array[block];
      RESULT res = Find_in_block(cb, key);
      if (res != NO_ENTRY) {
        // set _last_block for next query
        _last_block = block;
        return res;
      }
      block = cb._next;
    }
    // set _last_block for inserting
    _last_block = block;
    return NO_ENTRY;
  }

  // Query cache
  RESULT Get_internal(UINT32 from, UINT32 to) {
    CACHE_ENTRY& entry = Find_entry(from);
    if (entry._block == UINT32_MAX)
      return NO_ENTRY;
    return Find_data(entry, to);
  }

  // Put new entry into cache
  void   Put_internal(UINT32 from, UINT32 to, RESULT res) {
    CACHE_ENTRY& entry = Find_entry(from);
    if (entry._block != UINT32_MAX) {
      CACHE_BLOCK& cb = _block_array[entry._block];
      for (INT i = 0; i < ENTRY_COUNT; ++i) {
        // insert into empty slot in the last block
        if (cb._data[i] == 0) {
          cb._data[i] = ((to << RESULT_BIT) | res);
          // set _last_block for query
          _last_block = entry._block;
          return;
        }
      }
    }

    // insert into new block
    UINT32 block_index;
    CACHE_BLOCK& block = _block_array.New_entry(block_index);
    block._next = entry._block;
    entry._block = block_index;
    block._data[0] = ((to << RESULT_BIT) | res);
    for (INT i = 1; i < ENTRY_COUNT; ++i)
      block._data[i] = 0;
    // set _last_block for query
    _last_block = block_index;
  }

public:
  // external interface to query cache
  RESULT Get(UINT32 from, UINT32 to) {
    RESULT res = Get_internal(from, to);
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "PATH_CACHE GET: %d --> %d %s\n", from, to, RESULT_desc(res)));
    return res;
  }

  // external interface to put into cache
  void   Put(UINT32 from, UINT32 to, RESULT res) {
    Is_True(Get_internal(from, to) == NO_ENTRY, ("entry put before"));
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "PATH_CACHE PUT: %d --> %d %s\n", from, to, RESULT_desc(res)));
    Put_internal(from, to, res);
    Is_True(Get_internal(from, to) == res, ("entry put failed"));
  }
};

class PATH_SELECTED;
class DNA_NODE;

// main class for value range analysis
class VRA {
  friend class VRA_BUILDER;
  friend class SRCPOS_PATH;

private:
  enum {
    VR_NOT_FOUND = 0,   // reserved for "id map not found"
    VR_TOP       = 1,   // reserved for "top" which needs PU context
    VR_BOTTOM    = 2,   // reserved for "bottom" which the compiler fails to analyze
    VR_EMPTY     = 3,   // reserved for "empty" which there is no possible value
  };

  struct VALBOUND {
    VRA_BOUND _bound;   // has bound or not
    INT64     _min;     // minimal value (included)
    INT64     _max;     // maximal value (included)

    // constructor
    VALBOUND() : _bound(VA_NB), _min(0), _max(0) { }
    // constructor
    VALBOUND(VRA_BOUND bound, INT64 min, INT64 max)
      : _bound(bound), _min(min), _max(max) { }

    // has bound
    VRA_BOUND Vra_bound() const { return _bound; }
    // minimal value
    INT64     Min() const       { return _min;   }
    // maximal value
    INT64     Max() const       { return _max;   }
  };

private:
  typedef std::vector<unsigned char>     COUNT_ARRAY;
  typedef std::vector< std::pair<CODEREP*, STMTREP*> > COND_ARRAY;
  typedef SEGMENTED_ARRAY<VRA_EXPR, 256> EXPR_TABLE;
  typedef SEGMENTED_ARRAY<UINT64, 256>   VALRANGE_TABLE;
  typedef hash_map<uint64_t, uint32_t>   VALRANGE_MAP;  // (cr_id, bb_id) -> expr_id
  typedef hash_map<UINT32, VALBOUND>     VALBOUND_MAP;  // expr_id -> (has_bound, min, max)
  COMP_UNIT      *_comp_unit;
  CFG            *_cfg;
  CODEMAP        *_htable;
  VRA_PATH_CACHE *_path_cache;
  //MEM_POOL     *_loc_pool;
  //MEM_POOL     *_mem_pool;
  EXPR_TABLE      _expr_table;
  VALRANGE_MAP    _valrange_map;
  COND_ARRAY      _bb_cond;
  COUNT_ARRAY     _trav_array;
  unsigned char   _trav_counter;
  unsigned char   _trav_counter_set;

private:
  DNA_NODE* Dna() const { return _comp_unit->Dna(); }
  VSA*      Vsa() const { return _comp_unit->Vsa(); }
  void      Begin_trav()
  {
    Is_True(_trav_counter_set == 0, ("_trav_counter_set not set"));
    // check _trav_array size
    if (_trav_array.size() < _expr_table.size() + 8)
      _trav_array.resize(_expr_table.size() + 16);
    // set _trav_counter
    if (_trav_counter == 255) {
      _trav_counter = 1;
      std::fill(_trav_array.begin(), _trav_array.end(), 0);
    }
    else {
      ++_trav_counter;
    }
    // set _trav_counter_set
    _trav_counter_set = 1;
  }
  void      End_trav()
  {
    Is_True(_trav_counter_set == 1, ("_trav_counter_set not set"));
    _trav_counter_set = 0;
  }
  BOOL      Visited(UINT32 idx)
  {
    Is_True(_trav_counter_set == 1, ("_trav_counter_set not set"));
    if (idx >= _trav_array.size())
      return TRUE;  // newly created after Begin_trav(), assume visited
    if (_trav_array[idx] < _trav_counter) {
      _trav_array[idx] = _trav_counter;
      return FALSE;
    }
    return TRUE;
  }

private:
  void Set_bb_cond(UINT32 bb_id, CODEREP *cr, STMTREP *sr)
  {
    Is_True(bb_id < _bb_cond.size(), ("wrong bb id"));
    if (_bb_cond[bb_id].second == NULL) { // never set
      _bb_cond[bb_id].first = cr;
      _bb_cond[bb_id].second = sr;
    }
    else if (_bb_cond[bb_id].first != NULL) {
      _bb_cond[bb_id].first = NULL;       // reset cr t o NULL
    }
  }

public:
  std::pair<CODEREP*, STMTREP*> Get_bb_cond(BB_NODE *bb)
  {
    Is_True(bb->Id() < _bb_cond.size(), ("wrong bb id"));
    return _bb_cond[bb->Id()];
  }

  std::pair<CODEREP*, STMTREP*> Get_bb_cond_rec(BB_NODE *bb)
  {
    do {
      std::pair<CODEREP*, STMTREP*> ret = Get_bb_cond(bb);
      if (ret.first)
        return ret;
      bb = bb->Idom();
    } while (bb);
    return std::pair<CODEREP*, STMTREP*>(NULL, NULL);
  }

private:
  // value range simplifier for conjunction, implemented in opt_vra_simp.cxx
  UINT32 Simplify_conj_expr(UINT32 left, CODEREP* lexp, UINT32 right, CODEREP* rexp, UINT32 bb_id);
  UINT32 Simplify_conj_expr(UINT32 left, CODEREP* lexp, UINT32 right, UINT32 bb_id);
  UINT32 Simplify_conj_expr(UINT32 left, UINT32 right, UINT32 bb_id);

  // value range simplifier for disjunction
  UINT32 Simplify_disj_expr(UINT32 left, CODEREP* lexp, UINT32 right, CODEREP* rexp, UINT32 bb_id);
  UINT32 Simplify_disj_expr(UINT32 left, CODEREP* lexp, UINT32 right, UINT32 bb_id);
  UINT32 Simplify_disj_expr(UINT32 left, UINT32 right, UINT32 bb_id);

private:
  // helper routines to create new value range expression
  // create conj/disj expression
  UINT32 New_expr(VRA_OPERATOR opr, UINT bb_id, UINT op0, UINT op1)
  {
    Is_True(opr == OPR_CONJUNCTION || opr == OPR_DISJUNCTION,
            ("invalid operator"));
    UINT32 idx;
    VRA_EXPR& expr = _expr_table.New_entry(idx);
    expr.Init(opr, bb_id, op0, op1);
    return idx;
  }

  // create coderep expression
  UINT32 New_coderep_expr(CODEREP* cr, UINT32 bb_id, BOOL placeholder)
  {
    UINT32 idx;
    VRA_EXPR& expr = _expr_table.New_entry(idx);
    expr.Init(cr, bb_id, placeholder);
    return idx;
  }

  // create special expression
  UINT32 New_special_expr(VRA_OPERATOR opr, UINT32 bb_id)
  {
    Is_True((OPERATOR)opr == OPERATOR_UNKNOWN ||
            opr == OPR_TOP || opr == OPR_BOTTOM || opr == OPR_EMPTY,
            ("invalid operator"));
    if (bb_id != 0) {
      // reuse prebuilt entry for special expr
      return (opr - OPR_TOP) + VR_TOP;
    }
    UINT32 idx;
    VRA_EXPR& expr = _expr_table.New_entry(idx);
    expr.Init(opr, bb_id, 0, 0);
    return idx;
  }

  // create conj expression, if possible, simplify it
  UINT32 New_conj_expr(UINT32 left, UINT32 right, UINT32 bb_id)
  {
    Is_True(!Is_placeholder(right),
            ("right child cannot be placeholder"));
    // if left is pseodu expr, leave it
    if (Is_placeholder(left))
      return New_expr(OPR_CONJUNCTION, bb_id, left, right);

    // try simplify it
    Begin_trav();
    UINT32 ret = Simplify_conj_expr(left, right, bb_id);
    End_trav();
    return ret != VR_NOT_FOUND ? ret :
                      New_expr(OPR_CONJUNCTION, bb_id, left, right);
  }

  // create disj expression, if possible, simplify it
  UINT32 New_disj_expr(UINT32 left, UINT32 right, UINT32 bb_id)
  {
    // try simplify the expr
    Begin_trav();
    UINT32 expr = Simplify_disj_expr(left, right, bb_id);
    End_trav();
    return expr != VR_NOT_FOUND ?
                       expr : New_expr(OPR_DISJUNCTION, bb_id, left, right);
  }

  // clone expression, doesn't copy the bb id
  UINT32 Clone_expr(const VRA_EXPR& expr, UINT32 bb_id)
  {
    UINT32 idx;
    VRA_EXPR& entry = _expr_table.New_entry(idx);
    entry.Copy(expr, bb_id);
    return idx;
  }

  // copy a expression in place and do necessary simplification
  UINT32 Copy_in_place(UINT32 inp, UINT32 opnd, UINT32 bb_id)
  {
    VRA_EXPR& opnd_expr = Expr(opnd);
    // special handling for possible conjunction over itself
    if ((opnd_expr.Opr() == OPR_CONJUNCTION || opnd_expr.Opr() == OPR_DISJUNCTION) &&
        (opnd_expr.Op0() == inp || opnd_expr.Op1() == inp)) {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-MGR: BB%d copy in place to replace expr%d with CONJ%d kid expr%d\n",
                       bb_id, inp, opnd, opnd_expr.Op1()));
      UINT32 opnd_copy = opnd_expr.Op0() == inp ? opnd_expr.Op1() : opnd_expr.Op0();
      return Copy_in_place(inp, opnd_copy, bb_id);
    }
    if (opnd_expr.Opr() == OPR_PLACEHOLDER) {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-MGR: BB%d ignore copy in place to replace expr%d with cr%d:bb%d:expr%d\n",
                       bb_id, inp,
                       opnd_expr.Cr()->Coderep_id(), opnd_expr.Bb_id(), opnd));
      return inp;
    }
    Is_True((opnd_expr.Opr() != OPR_CONJUNCTION &&
             opnd_expr.Opr() != OPR_DISJUNCTION) ||
            (opnd_expr.Op0() != inp && opnd_expr.Op1() != inp &&
             opnd_expr.Op0() != opnd && opnd_expr.Op1() != opnd),
            ("invalid recursive on CONJ/DISJ"));

    VRA_EXPR& inp_expr = Expr(inp);
    Is_True(inp_expr.Opr() == OPR_PLACEHOLDER,
            ("can only copy in place on placeholder"));
    Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
             (TFile, "VRA-MGR: Copy in place cr%d:bb%d:expr%d --> expr%d:bb%d:%s\n",
                     inp_expr.Cr()->Coderep_id(), inp_expr.Bb_id(), inp,
                     opnd, bb_id, VRA_OPERATOR_NAME(opnd_expr.Opr())));
    inp_expr.Copy(opnd_expr, bb_id);
    return inp;
  }


  // make a disjunction in place over inp and opnd
  // inp was copied to a new entry and a disjunction was created in inp
  UINT32 Disjunction_in_place(UINT32 inp, UINT32 opnd, UINT32 bb_id)
  {
    VRA_EXPR& entry_inp = Expr(inp);
    UINT32 inp2;
    if (entry_inp.Opr() == OPR_DISJUNCTION &&
        (entry_inp.Op0() == opnd || entry_inp.Op1() == opnd)) {
      return inp;
    }
    if (entry_inp.Opr() == OPR_PLACEHOLDER) {
      VRA_EXPR& entry_opnd = Expr(opnd);
      if ((entry_opnd.Opr() == OPR_CONJUNCTION || entry_opnd.Opr() == OPR_DISJUNCTION) &&
          (entry_opnd.Op0() == inp || entry_opnd.Op1() == inp))
        opnd = entry_opnd.Op0() == inp ? entry_opnd.Op1() : entry_opnd.Op0();
      return Copy_in_place(inp, opnd, bb_id);
    }
    VRA_EXPR& entry_inp2 = _expr_table.New_entry(inp2);
    entry_inp2.Copy(entry_inp, entry_inp.Bb_id());
    entry_inp.Init(OPR_DISJUNCTION, entry_inp.Bb_id(), inp2, opnd);
    return inp;
  }

  // check if an expr is a placeholder
  BOOL Is_placeholder(UINT32 expr_id) const
  {
    const VRA_EXPR& expr = Expr(expr_id);
    return expr.Opr() == OPR_PLACEHOLDER;
  }

  // get the expression table entry
  VRA_EXPR& Expr(UINT32 expr)
  {
    Is_True(expr < _expr_table.Size(), ("expr %d out of range %d.",
                                        expr, _expr_table.Size()));
    return _expr_table.Entry(expr);
  }

  // get the expression table entry
  const VRA_EXPR& Expr(UINT32 expr) const
  {
    Is_True(expr < _expr_table.Size(), ("expr %d out of range %d.",
                                        expr, _expr_table.Size()));
    return _expr_table.Entry(expr);
  }

private:
  // routines to create and query value range
  // calculate key for cr at given bb
  UINT64 Key(CODEREP* cr, UINT32 bb_id) const;

  // create new value range
  UINT32 New_value_range(CODEREP* cr, UINT32 bb_id, UINT32 expr_id, BOOL replace = FALSE)
  {
    UINT64 key = Key(cr, bb_id);
    VALRANGE_MAP::iterator it = _valrange_map.find(key);
    if (it != _valrange_map.end()) {
      if (it->second == expr_id) {
        // happens with res = phi(v0, v1, v1) where two v1 have the
        // same range
        return expr_id;
      }
      if (replace) {
        _valrange_map.erase(it);
        // insert the new entry into map
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-MGR: BB%d New_value_range cr%d:bb%d --> expr%d\n",
                         bb_id, cr->Coderep_id(), bb_id, expr_id));
        _valrange_map.insert(std::pair<uint64_t, uint32_t>(key, expr_id));
        return expr_id;
      }
      VRA_EXPR& expr = Expr(it->second);
      if (expr.Opr() == OPR_PLACEHOLDER) {
        // the old entry is a placeholder, replace it with the new one
        Is_True(expr.Cr() == cr || cr->Is_flag_set(CF_DEF_BY_PHI),
                ("invalid placeholder expr%d for expr%d", it->second, expr_id));
        UINT32 copy_id = Copy_in_place(it->second, expr_id, bb_id);
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-MGR: BB%d New_value_range cr%d:bb%d --> expr%d = copy(%d, %d)\n",
                         bb_id, cr->Coderep_id(), bb_id,
                         copy_id, it->second, expr_id));
        if (it->second != copy_id)
          it->second = copy_id;
        return copy_id;
      }
      else {
        // make a disjunction over existing expr, make sure this bb
        // have multiple preds
        Is_True(_cfg->Get_bb(bb_id)->Pred() != NULL &&
                _cfg->Get_bb(bb_id)->Pred()->Multiple_bbs(),
                ("bb does not have multiple preds"));
        //return Disjunction_in_place(it->second, expr_id, bb_id);
        UINT32 disj_id = New_disj_expr(it->second, expr_id, bb_id);
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-MGR: BB%d New_value_range cr%d:bb%d --> expr%d = disj(%d, %d)\n",
                         bb_id, cr->Coderep_id(), bb_id,
                         disj_id, it->second, expr_id));
        it->second = disj_id;
        return disj_id;
      }
    }
    else {
      // insert the new entry into map
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-MGR: BB%d New_value_range cr%d:bb%d --> expr%d\n",
                       bb_id, cr->Coderep_id(), bb_id, expr_id));
      _valrange_map.insert(std::pair<uint64_t, uint32_t>(key, expr_id));
      return expr_id;
    }
  }

  // get value range for a cr at given bb
  UINT32 Value_range(CODEREP* cr, UINT32 bb_id) const
  {
    if (cr->Is_flag_set(CF_IS_ZERO_VERSION))
      return VR_BOTTOM;
    UINT64 key = Key(cr, bb_id);
    VALRANGE_MAP::const_iterator it = _valrange_map.find(key);
    if (it != _valrange_map.end())
      return it->second;
    else
      return VR_NOT_FOUND;
  }

  // get value range for a cr at given bb. if no value range found,
  // search its idom node
  UINT32 Value_range_rec(CODEREP* cr, UINT32 bb_id) const
  {
    UINT32 expr;
    do {
      expr = Value_range(cr, bb_id);
      if (expr != VR_NOT_FOUND) {
        break;
      }
      BB_NODE* idom = _cfg->Get_bb(bb_id)->Idom();
      if (idom == NULL)
        break;
      bb_id = idom->Id();
    } while (true); 
    return expr;
  }

public:
  // helper function to simplify coderep
  CODEREP* Enter_cr(CODEREP* cr, BOOL fold) const
  {
    CODEREP* ret = NULL;
    if (fold) {
      FOLD ftmp;
      ret = ftmp.Fold_Expr(cr);
    }
    return ret ? ret : _htable->Rehash(cr);
  }

  // helper function to init const cr
  void Init_const(CODEREP* cr, TYPE_ID dtyp, INT64 val) const
  {
    // CODEREP::Init_const may change I4 to U4 if val is in U4 range
    cr->Init_const(MTYPE_is_signed(dtyp) ? MTYPE_I8 : dtyp, val);
  }

  // helper function to init rconst for float point const
  CODEREP* Init_fconst_cr(CODEREP* cr, TYPE_ID dtyp, double fval) const
  {
    TCON tc = Host_To_Targ_Float(dtyp, fval);
    ST* st = New_Const_Sym(Enter_tcon(tc), MTYPE_To_TY(dtyp));
    cr->Init_rconst(dtyp, st);
    return cr;
  }

  // helper function to create constant coderep
  CODEREP* New_cr(TYPE_ID dtyp, INT64 val) const
  {
    CODEREP* cst = Alloc_stack_cr(0);
    Init_const(cst, dtyp, val);
    return _htable->Rehash(cst);
  }

  // helper function to create binary coderep
  CODEREP* New_cr(OPCODE opc, CODEREP* op0, CODEREP* op1, BOOL fold = FALSE) const
  {
    CODEREP* cr = Alloc_stack_cr(2);
    cr->Init_op(opc, 2);
    cr->Set_opnd(0, op0);
    cr->Set_opnd(1, op1);
    return Enter_cr(cr, fold);
  }

  // helper function to create binary coderep
  CODEREP* New_binary_cr(OPCODE opc, CODEREP* op0, INT64 op1) const
  {
    CODEREP* cst = New_cr(op0->Dtyp(), op1);
    return New_cr(opc, op0, cst);
  }

  // helper function to create unary coderep
  CODEREP* New_unary_cr(OPCODE opc, CODEREP* op0, BOOL fold = FALSE) const
  {
    CODEREP* cr = Alloc_stack_cr(1);
    cr->Init_op(opc, 1);
    cr->Set_opnd(0, op0);
    return Enter_cr(cr, fold);
  }

  // helper function to create cmp coderep
  CODEREP* New_cmp_cr(OPERATOR opr, CODEREP* op0, CODEREP* op1) const
  {
    Is_True(opr == OPR_EQ || opr == OPR_NE || opr == OPR_GT ||
            opr == OPR_GE || opr == OPR_LT || opr == OPR_LE,
            ("invalid opr for cmp cr"));
    return New_cr(OPCODE_make_op(opr, Boolean_type, op0->Dtyp()),
                  op0, op1);
  }

  // helper function to create cmp coderep
  CODEREP* New_cmp_cr(OPERATOR opr, CODEREP* op0, INT64 op1) const
  {
    CODEREP* cst = New_cr(op0->Dtyp(), op1);
    return New_cmp_cr(opr, op0, cst);
  }

  CODEREP* New_div_cr(CODEREP* dividend, INT64 divisor) const;

  // helper function to create complement cr
  CODEREP* Complement_cr(CODEREP* cr) const;

  // helper function to get var list (CK_VAR) in a coderep (CK_OP)
  BOOL Analyze_coderep_vars(CODEREP* cr, hash_set<CODEREP*>& st) const;

  // helper function to canonicalize the cond expr cr according to the var cr
  BOOL Canonicalize_coderep(CODEREP* expr, CODEREP* var, CODEREP* &out) const;

private:
  // internal implementation for symbolic execution to check if the operation is
  // satisfied. implemented in opt_vra_tmpl.h

  // know x op1 lhs, check if x op2 rhs in bb_id
  template<UINT32 op1, UINT32 op2>
  VRA_RESULT Compare_expr(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const;
  // know abx(x) op1 lhs, check if x op2 rhs in bb_id
  template<UINT32 op1, UINT32 op2>
  VRA_RESULT Compare_abs_expr(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const;
  // check if const lhs opr const rhs
  template<UINT32 opr>
  VRA_RESULT Compare_const(CODEREP* lhs, CODEREP* rhs, bool zext) const;
  // check if rconst lhs opr rconst rhs
  template<UINT32 opr>
  VRA_RESULT Compare_rconst(CODEREP* lhs, CODEREP* rhs) const;
  // check if var lhs opr rhs
  template<UINT32 opr>
  VRA_RESULT Compare_var(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const;
  // check if ivar lhs opr rhs
  template<UINT32 opr>
  VRA_RESULT Compare_ivar(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const;
  // check if lda lhs opr rhs
  template<UINT32 opr>
  VRA_RESULT Compare_lda(CODEREP* lhs, CODEREP* rhs) const;

  // check if expr "cmp" val in bb_id
  template<UINT32 cmp>
  VRA_RESULT Compare_vra_expr(UINT32 expr, UINT32 bb_id, CODEREP* val, BOOL zext, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const;
  // check if op cr "cmp" val in bb_id
  template<UINT32 cmp>
  VRA_RESULT Compare_op_cr(CODEREP* cr, UINT32 bb_id, CODEREP* val, BOOL zext, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const;
  // check if general cr "cmp" val in bb_id
  template<UINT32 cmp>
  VRA_RESULT Compare_cr(CODEREP* cr, UINT32 bb_id, CODEREP* val, BOOL zext, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const;

  // temporary solution to simplify the coderep
  // TODO: to be enhanced later
  CODEREP* Simplify_coderep(CODEREP* cr, INT64& val) const
  {
    if (cr->Kind() == CK_OP &&
        (cr->Opr() == OPR_ADD || cr->Opr() == OPR_SUB)) {
      CODEREP* op0 = cr->Opnd(0);
      CODEREP* op1 = cr->Opnd(1);
      if (op0->Kind() == CK_CONST) {
        val -= op0->Const_val();
        return op1;
      }
      else if (op1->Kind() == CK_CONST) {
        val -= op1->Const_val();
        return op0;
      }
    }
    return cr;
  }

public:
  // public interface to check if the operation is satisfied.

  // return VA_YES if "var" "cmp" "val" in "bb" is TRUE
  // return VA_POSSIBLE if "var" "cmp" "val" in "bb" is possibly TRUE
  //   out param "paths" contains all possible paths from entry to bb where var equals to val
  // return VA_NO if "var" "cmp" "val" in "bb" is FALSE
  //   value of paths is unspecified
  // return VA_UNKNOWN if not enough information to "cmp" "var" and "val"
  //   value of paths is unspecified
  // "opr" must be one of OPR_EQ, OPR_NE, OPR_GE, OPR_GT, OPR_LE, OPR_LT
  template<OPERATOR opr>
  VRA_RESULT Var_cmp_val(CODEREP* var, UINT32 bb_id, INT64 val, const PATH_SELECTED& paths) const
  {
    CODEREP* v = Alloc_stack_cr(0);
    Init_const(v, MTYPE_I8, val);
    return Var_cmp_val<opr>(var, bb_id, v, paths);
  }

  template<OPERATOR opr>
  VRA_RESULT Var_cmp_val(CODEREP* var, UINT32 bb_id, CODEREP* val, const PATH_SELECTED& paths) const
  {
    UINT32 expr = Value_range_rec(var, bb_id);
    if (expr == VR_NOT_FOUND || expr == VR_BOTTOM)
      return VA_UNKNOWN;
    VRA_RESULT res;
    COUNT_ARRAY visited;
    visited.resize(_expr_table.Size());
    return Compare_vra_expr<opr>(expr, bb_id, val, MTYPE_is_unsigned(var->Dtyp()),
                                 paths, visited);
  }

  // compare an "expr" with "val"
  // return value is the same as Var_cmp_val()
  // "opr" must be one of OPR_EQ, OPR_NE, OPR_GE, OPR_GT, OPR_LE, OPR_LT
  template<OPERATOR opr>
  VRA_RESULT Expr_cmp_val(CODEREP* expr, UINT32 bb_id, INT64 val, const PATH_SELECTED& paths) const
  {
    // temporary hack to simplify the expr at first
    if (expr->Kind() == CK_OP)
      expr = Simplify_coderep(expr, val);

    CODEREP* v = Alloc_stack_cr(0);
    Init_const(v, MTYPE_I8, val);
    return Expr_cmp_val<opr>(expr, bb_id, v, paths);
  }

  template<OPERATOR opr>
  VRA_RESULT Expr_cmp_val(CODEREP* expr, UINT32 bb_id, CODEREP* val, const PATH_SELECTED& paths) const
  {
    if (expr->Kind() == CK_VAR)
      return Var_cmp_val<opr>(expr, bb_id, val, paths);
    else if (expr->Kind() == CK_OP && expr->Opr() == OPR_CVT)
      return Expr_cmp_val<opr>(expr->Opnd(0), bb_id, val, paths);
    else if (expr->Kind() == CK_CONST && val->Kind() == CK_CONST)
      return Compare_const<opr>(expr, val, FALSE);
    else
      return VA_UNKNOWN;
  }

  template<OPERATOR opr>
  VRA_RESULT Compare_cr(CODEREP* lhs, BB_NODE* bb, CODEREP* rhs, const PATH_SELECTED& paths) const
  {
    COUNT_ARRAY visited;
    visited.resize(_expr_table.Size());
    return Compare_cr<opr>(lhs, bb->Id(), rhs, FALSE, paths, visited);
  }

  template<OPERATOR opr>
  VRA_RESULT Compare_cr_xfa(CODEREP* lhs, IDTYPE bb_id, CODEREP* rhs,
                            DNA_NODE* rhs_dna, const PATH_SELECTED& paths) const;

  // return VA_YES if "expr" is in the range of ["lb", "ub") in "bb"
  //   out param "paths" contains all possible paths where expr is in of range of [lb, ub)
  // return VA_NO if expr is out the range of [lb, ub), value of paths is unspecified
  // return VA_UNKNOWN if not enough information, value of paths is unspecified
  //
  VRA_RESULT Is_expr_in_range(CODEREP* cr, BB_NODE* bb, INT64 lb, INT64 ub, const PATH_SELECTED& paths) const;
  VRA_RESULT Is_expr_in_range(CODEREP* cr, BB_NODE* bb, CODEREP* lb, CODEREP* ub, const PATH_SELECTED& paths) const;

  // return VA_YES if "expr" is out the range of ["lb", "ub") in "bb"
  //   out param "paths" contains all possible paths where expr is out of range of [lb, ub)
  // return VA_NO if expr is in the range of [lb, ub), value of paths is unspecified
  // return VA_UNKNOWN if not enough information, value of paths is unspecified
  //
  VRA_RESULT Is_expr_out_range(CODEREP* cr, BB_NODE* bb, INT64 lb, INT64 ub, const PATH_SELECTED& paths) const;
  VRA_RESULT Is_expr_out_range(CODEREP* cr, BB_NODE* bb, CODEREP* lb, CODEREP* ub, const PATH_SELECTED& paths) const;

  VRA_RESULT Is_var_zero(CODEREP* cr, BB_NODE* bb) const;
  VRA_RESULT Is_expr_zero(CODEREP* cr, BB_NODE* bb) const;

private:
  // internal implementation for boundary-style API, implemented in opt_vra_bounds.cxx

  // calculate the bounds in coderep for conjunction
  BOOL Calc_conj_bounds(TYPE_ID dtyp, CODEREP* min1, CODEREP* max1, CODEREP* min2, CODEREP* max2, CODEREP* &min, CODEREP* &max) const;

  // calculate the bounds in coderep for disjunction
  BOOL Calc_disj_bounds(TYPE_ID dtyp, CODEREP* min1, CODEREP* max1, CODEREP* min2, CODEREP* max2, CODEREP* &min, CODEREP* &max) const;

  // get the bounds in coderep for a coderep
  BOOL Get_bounds(TYPE_ID dtyp, CODEREP* cr, BB_NODE* bb, CODEREP* &min, CODEREP* &max, COUNT_ARRAY& visited) const;

  // get the bounds in coderep for an expr
  BOOL Get_bounds(TYPE_ID dtyp, UINT32 expr_id, BB_NODE* bb, CODEREP* &min, CODEREP* &max, COUNT_ARRAY& visited) const;

  // calculate the bounds in constant for conjunction
  BOOL Calc_conj_bounds(TYPE_ID dtyp, INT64 min1, INT64 max1, INT64 min2, INT64 max2, INT64 &min, INT64 &max) const;

  // calculate the bounds in constant for disjunction
  BOOL Calc_disj_bounds(TYPE_ID dtyp, INT64 min1, INT64 max1, INT64 min2, INT64 max2, INT64 &min, INT64 &max) const;

  // get the bounds in constant for a coderep
  VRA_BOUND Get_bounds(TYPE_ID dtyp, CODEREP* cr, BB_NODE* bb, INT64 &min, INT64 &max, VALBOUND_MAP& cache) const;

  // get the bounds in constant for an expr
  VRA_BOUND Get_bounds(TYPE_ID dtyp, UINT32 expr_id, BB_NODE* bb, INT64 &min, INT64 &max, VALBOUND_MAP& cache) const;

  // get the upper bounds for a coderep
  BOOL Get_ub(CODEREP* cr, BB_NODE* bb, CODEREP*& ub, COUNT_ARRAY& visited) const;

  // get upper bounds in coderep for an expr
  BOOL Get_ub(UINT32 expr_id, BB_NODE* bb, CODEREP*& ub, COUNT_ARRAY& visited) const;

public:
  // public interface for boundary-style API, implemented in opt_vra_bounds.cxx
  // get bounds in coderep for coderep in bb
  BOOL Get_bounds(CODEREP* cr, BB_NODE* bb, CODEREP*& min, CODEREP*& max) const;

  // get bounds in constant for coderep in bb
  VRA_BOUND Get_bounds(CODEREP* cr, BB_NODE* bb, INT64& min, INT64& max) const;

  // get upper bounds in coderep for coderep in bb
  BOOL Get_ub(CODEREP* cr, BB_NODE* bb, CODEREP*& ub) const;

private:
  // if expr is evaluated to a constant at bb, return the constant CR
  // otherwise NULL is returned
  CODEREP* Prop_const_coderep(TYPE_ID dtyp, CODEREP* cr, BB_NODE* bb, VALBOUND_MAP& cache) const;
  CODEREP* Prop_const_scalar(TYPE_ID dtyp, UINT32 expr_id, BB_NODE* bb, VALBOUND_MAP& cache) const;

public:
  // if cr is evaluated to a constant at bb, return the constant CR
  // Otherwise NULL is returned
  CODEREP* Prop_const_scalar(CODEREP* cr, BB_NODE* bb) const;

private:
  // Get control variables may impact the reachability of the bb
  BOOL Get_control_variables(BB_NODE* bb, hash_set<CODEREP*>& vars) const;

  // select a control variable easier to analyze the reachability between bb1 and bb2
  CODEREP* Select_control_variable(BB_NODE* bb1, BB_NODE* bb2) const;

  // Find phi node's pred at given BB
  PHI_NODE* Find_phi_pred_at_bb(PHI_NODE* phi, BB_NODE* bb, std::vector<bool>& visited) const;

  // if first bb reached, check if second bb is reachable
  VRA_RESULT Is_bb_reachable(BB_NODE* first, BB_NODE* sencond) const;

  // check if use's value is from def
  BOOL Is_assign(CODEREP* def, CODEREP* use, BOOL follow_phi = FALSE) const;

  // find common cr in two CRs and do canon on two crs
  BOOL Find_and_canon_common_cr(CODEREP* lhs, CODEREP* rhs, CODEREP*& lhs_canon, CODEREP* &rhs_canon) const;

  // check if def matches with use
  VRA_RESULT Check_match(CODEREP* def, CODEREP* use, UINT32 use_bb) const;

  // get condition coderep from BB
  CODEREP* Get_cond_expr(BB_NODE* pred, BB_NODE* succ) const;

  // collect all control dependencies from def_bb to use_bb
  void Collect_control_dependencies(BB_NODE* use_bb, BB_NODE* def_bb,
                                    std::vector< std::pair<BB_NODE*, BB_NODE*> >& cds,
                                    std::vector<bool>& visited,
                                    std::vector<UINT32>& df_visited,
                                    UINT32 df_visit_cnt) const;

  VRA_RESULT Is_path_possible_internal(BB_NODE* use, PHI_NODE* phi, INT opnd) const;

public:
  // collect all control dependencies from def_bb to use_bb
  void Collect_control_dependencies(BB_NODE* use_bb, BB_NODE* def_bb,
                                    std::vector< std::pair<BB_NODE*, BB_NODE*> >& cds) const
  {
    std::vector<bool> visited;
    visited.resize(_cfg->Total_bb_count());
    std::vector<UINT32> df_visited;
    df_visited.resize(_cfg->Total_bb_count());
    Collect_control_dependencies(use_bb, def_bb, cds,
                                 visited, df_visited, 1);
  }

  // check if path possible from def to use according to control dep edge in defs and uses
  VRA_RESULT Is_path_possible(const std::vector< std::pair<BB_NODE*, BB_NODE*> >& defs,
                              const std::vector< std::pair<BB_NODE*, BB_NODE*> >& uses,
                              UINT32 def_bb, UINT32 use_bb) const;

  VRA_RESULT Is_path_possible(BB_NODE* use, PHI_NODE* phi, INT opnd) const
  {
    BB_NODE* from = phi->Bb()->Nth_pred(opnd);
    VRA_PATH_CACHE::RESULT cres = _path_cache->Get(from->Id(), use->Id());
    if (cres != VRA_PATH_CACHE::NO_ENTRY) {
      return _path_cache->Get_vra_result(cres);
    } else {
      VRA_RESULT vres = Is_path_possible_internal(use, phi, opnd);
      cres = _path_cache->Get_result(vres);
      _path_cache->Put(from->Id(), use->Id(), cres);
      return vres;
    }
  }

  VRA_RESULT Is_path_possible(BB_NODE* use, CODEREP *var, CODEREP *val) const;

  // check if bb reachable by checking if cr value range is empty in given BB
  BOOL Is_bb_reachable(CODEREP* cond, BB_NODE* bb) const
  {
    UINT32 expr = Value_range_rec(cond, bb->Id());
    return expr != VR_EMPTY ? TRUE : FALSE;
  }

  // check if there is a path on CFG from first to second
  BOOL Cfg_has_path(BB_NODE* first, BB_NODE* second, std::vector<UINT32>& visited, UINT32 cnt) const;

  // compare two crs
  VRA_RESULT Compare_cr(CODEREP* def, CODEREP* use, UINT32 def_bb, UINT32 use_bb) const;

public:
  // Get the variable name of a CK_VAR coderep
  const char* Var_name(CODEREP* cr) const;

  // print utilities, implemented in opt_vra_util.cxx
  void Print_bb_loop(BB_NODE* bb, FILE* f = stderr) const;
  void Print_coderep(CODEREP* cr, FILE* f = stderr) const;
  void Print_stmtrep(STMTREP* sr, FILE* f = stderr) const;
  void Print_expr(UINT32 idx, FILE* f = stderr) const;
  void Print_path(const PATH_SELECTED& paths, INT ident, FILE* f = stderr) const;
  void Print_valrange(FILE* f = stderr) const;
  void Print(FILE* f = stderr) const;

public:
  VRA(COMP_UNIT* comp_unit)
    : _comp_unit(comp_unit),
      _cfg(comp_unit->Cfg()),
      _htable(comp_unit->Htable()),
      _expr_table(comp_unit->Mem_pool()),
      _path_cache(CXX_NEW(VRA_PATH_CACHE(comp_unit->Mem_pool()), comp_unit->Mem_pool())),
      _trav_counter(0), _trav_counter_set(0)
  {
    // reserve expr table item 0 for "id_map not found" and "top"
    UINT32 idx;
    idx = New_special_expr(OPR_BOTTOM, 0);
    Is_True(idx == VR_NOT_FOUND, ("expr table is not initialized correctly"));
    // reserve expr table item 1 for "top"
    idx = New_special_expr(OPR_TOP, 0);
    // reserve expr table item 1 for "bottom"
    idx = New_special_expr(OPR_BOTTOM, 0);
    // reserve expr table item 1 for "bottom"
    idx = New_special_expr(OPR_EMPTY, 0);

    // initialize bb_cond table
    _bb_cond.resize(_cfg->Total_bb_count());
  }

private:
  // build value range table
  void Build_table(IPSA* ipsa_mgr);

public:
  // Perform value range analysis
  void Perform(IPSA* ipsa_mgr);

  // Test driver for vra API, implemented in opt_vra_demo.cxx
  void Test_api();

  // Test driver for null pointer dereference (NPD), implemented in opt_vra_demo.cxx
  void Test_npd();

  // Test driver for array out-of-bound (AOB), implemented in opt_vra_demo.cxx
  void Test_aob();

  // Test driver for pointer out-of-bound (POB), implemented in opt_vra_demo.cxx
  void Test_pob();
};

#endif /* opt_vra_INCLUDED */

