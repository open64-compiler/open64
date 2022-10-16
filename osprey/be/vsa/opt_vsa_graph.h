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

// ==================================================================
//
// Module: opt_vsa_graph.h
//
// ==================================================================

//
// value graph:
// collect values and dependencies during the U-D or D-U traversal and
// put them in the graph. when there is a circle in the graph or same
// or different values are bound on the same node, that means the 
// assumption we made is confirmed or the path has conflict control
// dependencies.
//

#ifndef opt_vsa_graph_INCLUDED
#define opt_vsa_graph_INCLUDED

#include "opt_cda.h"

// node kind
enum NODE_KIND {
  N_NONE  = 0,         // none, used as sentinel
  N_VALUE = 1,         // integer value
  N_CONST = 2,         // const cr out of value's range
  N_VAR   = 3,         // var cr
  N_VOR   = 4,         // vsym_obj_rep
  N_OP    = 5,         // op cr, +/-, etc
  N_LAST,
};

// edge operator, le/lt are converted intoo ge/gt
enum EDGE_OPER {
  E_NONE  = 0,         // none, used as sentinel
  E_EQ    = 1,         // ==
  E_NE    = 2,         // !=
  E_GE    = 3,         // >=, LE is converted into GE on edge
  E_GT    = 4,         // >, LT is converted into GT on edge
  E_LE    = 5,         // <=
  E_LT    = 6,         // <
  E_LAST,
};

// edge flag
enum EDGE_FLAG {
  EF_NONE   = 0x00,    // no flag
  EF_TARGET = 0x01,    // edge to be proven
  EF_DISJ   = 0x02,    // has disjunction edge(s)
  EF_EVAL   = 0x04,    // edge is evaluated from existing edges
};

// operation result when a new node/edge is added to graph
enum OP_RESULT {
  OP_CONTINUE  = 0,    // no result,continue
  OP_PROVEN    = 1,    // target is proven
  OP_VIOLATION = 2,    // target is violation
  OP_CONFLICT  = 4,    // found conflict, path impossible
  OP_ERROR     = 5,    // internal error
};

typedef mUINT16 NODE_IDX;
typedef mUINT16 EDGE_IDX;
typedef mUINT16 PHI_IDX;

#define VG_NULL_IDX (0)
#define VG_INIT_IDX (1)
#define VG_MAX_IDX  (0xFFFF)

//
// value graph
//
class VALUE_GRAPH {
private:
  // CONST_KIND
  enum CONST_KIND {
    LOCAL_LDA,      // LDA of local variable
    GLOBAL_LDA,     // LDA of global variable
    INT_CONST,      // integer constant
    FLOAT_CONST,    // float constant
  };

  // EDGE_IDX vector
  typedef mempool_allocator<EDGE_IDX>       EIDX_ALLOC;
  typedef std::vector<EDGE_IDX, EIDX_ALLOC> EIDX_VEC;
  typedef EIDX_VEC::const_iterator EIDX_ITER;

  // EDGE in VALUE_GRAPH
  class EDGE {
    friend class VALUE_GRAPH;
  private:
    NODE_IDX _lhs;     // left hand side of the edge
    NODE_IDX _rhs;     // right hand side of the edge
    EDGE_IDX _disj;    // disjunction edge if there is
    uint8_t  _opr;     // _lhs _opr _rhs
    uint8_t  _flag;    // edge flag

    EDGE(void);                      // REQUIRED UNDEFINED UNWANTED methods
    EDGE(const EDGE&);               // REQUIRED UNDEFINED UNWANTED methods
    EDGE& operator = (const EDGE&);  // REQUIRED UNDEFINED UNWANTED methods

  public:
    // constructor
    EDGE(NODE_IDX l) : _lhs(l), _rhs(VG_NULL_IDX), _disj(VG_NULL_IDX),
                       _opr(E_NONE), _flag(EF_NONE) {}

    // constructor
    EDGE(NODE_IDX l, NODE_IDX r, EDGE_IDX e, EDGE_OPER o, EDGE_FLAG f)
      : _lhs(l), _rhs(r), _disj(e), _opr(o), _flag(f) {}

    NODE_IDX  Lhs()  const { return _lhs;  }
    NODE_IDX  Rhs()  const { return _rhs;  }
    EDGE_IDX  Disj() const { return _disj; }
    EDGE_OPER Opr()  const { return (EDGE_OPER)_opr; }
    EDGE_FLAG Flag() const { return (EDGE_FLAG)_flag;}

    BOOL      Is_target() const { return (_flag & EF_TARGET) == EF_TARGET; }
    BOOL      Has_disj() const  { return (_flag & EF_DISJ) == EF_DISJ; }

    // dump function
    void Dump(EDGE_IDX idx, FILE *fp) const;
  }; // EDGE

  // NODE in VALUE_GRAPH
  class NODE {
    friend class VALUE_GRAPH;
  private:
    // NODE_MARK to save the node state
    struct NODE_MARK {
    private:
      NODE_MARK();             // disable default constructor

    public:
      mUINT8  _in_edge;        // last in edge count for mark
      mUINT8  _out_edge;       // last out edge count for mark
      mUINT16 _const_num;      // last const num for mark
      mUINT16 _next_node;      // last next node index for mark
      mUINT16 _par_node;       // last parent node index for mark
      INT64   _const_val;      // last const val for mark
      NODE_MARK(mUINT8 ie, mUINT8 oe, mUINT16 c, mUINT16 n, mUINT16 p, INT64 cv)
        : _in_edge(ie), _out_edge(oe), _const_num(c), _next_node(n), _par_node(p), _const_val(cv) {}
    };
    // stack to maintain node state
    typedef mempool_allocator<NODE_MARK> MARK_ALLOC;
    typedef std::deque<NODE_MARK, MARK_ALLOC> MARK_STACK;

    EIDX_VEC   _in_edges;      // in edges, node is rhs
    EIDX_VEC   _out_edges;     // out edges, node is lhs
    MARK_STACK _mark_stack;    // mark stack
    INT64      _const_val;     // if the node equals to a constant
    mUINT16    _const_num;     // number of constant node
    mUINT16    _next_node;     // next node with the same value like p = q
                               // where p/q has been added to different node
    mUINT16    _par_node;      // common parent node with the same value

    NODE(void);                      // REQUIRED UNDEFINED UNWANTED methods
    NODE(const NODE&);               // REQUIRED UNDEFINED UNWANTED methods
    NODE& operator = (const NODE&);  // REQUIRED UNDEFINED UNWANTED methods

  public:
    // constructor
    NODE(MEM_POOL *pool, NODE_IDX par)
      : _in_edges(EIDX_ALLOC(pool)),
        _out_edges(EIDX_ALLOC(pool)),
        _mark_stack(MARK_ALLOC(pool)),
        _const_val(), _const_num(0),
        _next_node(VG_NULL_IDX), _par_node(par) {}

    // constructor
    NODE(MEM_POOL *pool, NODE_IDX par, INT64 val)
      : _in_edges(EIDX_ALLOC(pool)),
        _out_edges(EIDX_ALLOC(pool)),
        _mark_stack(MARK_ALLOC(pool)),
        _const_val(val), _const_num(1),
        _next_node(VG_NULL_IDX), _par_node(par) {}

    // check if node is constant
    BOOL Is_const() const {
      return _const_num > 0;
    }

    // get const val
    INT64 Const_val() const {
      return _const_val;
    }

    // set const val
    void Set_const_val(INT64 val) {
      Is_True((_const_val == 0 && _const_num == 0) || _const_val == val,
              ("const mismatch"));
      if (_const_val != val)
        _const_val = val;
      ++ _const_num;
    }

    // inc const number
    void Inc_const_num() {
      ++ _const_num;
    }

    // get next
    NODE_IDX Next() const {
      return _next_node;
    }

    // set next
    void Set_next(NODE_IDX n) {
      _next_node = n;
    }

    // get parent
    NODE_IDX Parent() const {
      return _par_node;
    }

    // set parent
    void Set_parent(NODE_IDX p) {
      Is_True(p != VG_NULL_IDX, ("invalid par node"));
      _par_node = p;
    }

    // add in edge
    void Add_in_edge(EDGE_IDX e) {
      Is_True(e != VG_NULL_IDX, ("bad edge idx"));
      _in_edges.push_back(e);
    }

    // add out edge
    void Add_out_edge(EDGE_IDX e) {
      Is_True(e != VG_NULL_IDX, ("bad edge idx"));
      _out_edges.push_back(e);
    }

    // in edge iterator begin
    EIDX_ITER In_edge_begin() const {
      return _in_edges.begin();
    }

    // in edge iterator end
    EIDX_ITER In_edge_end() const {
      return _in_edges.end();
    }

    // out edge iterator begin
    EIDX_ITER Out_edge_begin() const {
      return _out_edges.begin();
    }

    // out edge iterator end
    EIDX_ITER Out_edge_end() const {
      return _out_edges.end();
    }

    // push current state into MARK_STACK
    void Push_mark() {
      _mark_stack.push_front(NODE_MARK(_in_edges.size(), _out_edges.size(),
                                       _const_num, _next_node, _par_node, _const_val));
    }

    // restore state from MARK_STACK
    void Pop_mark() {
      Is_True(!_mark_stack.empty(), ("stack empty"));
      NODE_MARK mark = _mark_stack.front();
      _mark_stack.pop_front();
      Is_True(mark._in_edge <= _in_edges.size(), ("in_edge vec smaller"));
      Is_True(mark._out_edge <= _out_edges.size(), ("out_edge vec smaller"));
      _in_edges.resize(mark._in_edge);
      _out_edges.resize(mark._out_edge);
      _const_num = mark._const_num;
      _const_val = mark._const_val;
      _next_node = mark._next_node;
      _par_node = mark._par_node;
    }

    // dump
    void Dump(NODE_IDX idx, FILE *fp) const;

  }; // NODE

  // Lazy evaluation for phi opnd
  class PHI_OPND {
  private:
    DNA_NODE *_dna;
    PHI_NODE *_phi;
    INT32     _opnd;

  public:
    PHI_OPND() : _dna(NULL), _phi(NULL), _opnd(0) {}
    PHI_OPND(DNA_NODE *dna, PHI_NODE *phi, INT32 opnd)
      : _dna(dna), _phi(phi), _opnd(opnd) {}

    DNA_NODE *Dna()  const { return _dna;  }
    PHI_NODE *Phi()  const { return _phi;  }
    INT32     Opnd() const { return _opnd; }
  }; // PHI_OPND

  // graph state for push/pop operation
  class STATE {
  private:
    NODE_IDX _ncount;  // Node count
    EDGE_IDX _ecount;
    PHI_IDX  _pcount;

  public:
    STATE() : _ncount(0), _ecount(0), _pcount(0) {}
    STATE(NODE_IDX n, EDGE_IDX e, PHI_IDX p)
      : _ncount(n), _ecount(e), _pcount(p) {}

    NODE_IDX Ncount() const { return _ncount; }
    EDGE_IDX Ecount() const { return _ecount; }
    PHI_IDX  Pcount() const { return _pcount; }
  };

  typedef pair<uintptr_t, NODE_IDX>       NMAP_PAIR;
  typedef mempool_allocator<NMAP_PAIR>    NMAP_ALLOC;
  typedef hash_map<uintptr_t, NODE_IDX,
                   __gnu_cxx::hash<uintptr_t>,
                   std::equal_to<uintptr_t>,
                   NMAP_ALLOC>            NODE_MAP;
  typedef mempool_allocator<NODE*>        NODE_ALLOC;
  typedef std::vector<NODE*, NODE_ALLOC>  NODE_VEC;
  typedef mempool_allocator<EDGE*>        EDGE_ALLOC;
  typedef std::vector<EDGE*, EDGE_ALLOC>  EDGE_VEC;
  typedef mempool_allocator<PHI_OPND>     PHI_ALLOC;
  typedef std::vector<PHI_OPND, PHI_ALLOC> PHI_VEC;
  typedef mempool_allocator<STATE>        STATE_ALLOC;
  typedef std::deque<STATE, STATE_ALLOC>  STATE_STACK;

  MEM_POOL    *_mpool;         // memory pool
  BOOL         _tracing;       // turn on/off tracing -xtchk:0x10000
  NODE_MAP     _cst_map;       // const -> node_idx map
  NODE_MAP     _var_map;       // var   -> node_idx map
  NODE_MAP     _vor_map;       // vsym  -> node_idx map
  NODE_MAP     _op_map;        // op    -> node_idx map

  NODE_VEC     _nodes;         // all nodes
  EDGE_VEC     _edges;         // all edges
  PHI_VEC      _phis;          // all phi opnds
  STATE_STACK  _stack;         // state stack
  static NODE  _null_node;     // static default null node
  static EDGE  _null_edge;     // static default null edge

private:
  // stack management
  // when the U-D traversal faces multiple paths, like phi/callers/callees,
  // push current state and chose a path. when the traversal returns from
  // the path, pop state and chose the next path

  // remove newly added map entry
  void Remove_map_entry(NODE_MAP &map, UINT32 index) {
    NODE_MAP::iterator it = map.begin();
    while (it != map.end()) {
      NODE_MAP::iterator p = it++;
      if (p->second >= index)
        map.erase(p);
    }
  }

  NODE *Node(NODE_IDX idx) const {
    Is_True(idx >= VG_INIT_IDX && idx < _nodes.size(),
            ("node idx out of range [1,%d)", idx, _nodes.size()));
    NODE *node = _nodes[idx];
    Is_True(node && node != &_null_node, ("invalid node at idx %d", idx));
    return node;
  }

  EDGE *Edge(EDGE_IDX idx) const {
    Is_True(idx >= VG_INIT_IDX && idx < _edges.size(),
            ("edge idx %d out of range [1,%d)", idx, _edges.size()));
    EDGE *edge = _edges[idx];
    Is_True(edge && edge != &_null_edge, ("invalid edge at idx %d", idx));
    return edge;
  }

public:
  // push current state into stack
  INT Push_mark() {
    Is_True(_nodes.size() < VG_MAX_IDX, ("node index out of range"));
    Is_True(_edges.size() < VG_MAX_IDX, ("edge index out of range"));
    Is_True(_phis.size() < VG_MAX_IDX,  ("phi index out of range"));
    // mark node and edge vector
    _stack.push_front(STATE(_nodes.size(), _edges.size(), _phis.size()));
    // mark each node
    NODE_VEC::iterator end = _nodes.end();
    for (NODE_VEC::iterator it = _nodes.begin(); it != end; ++it) {
      (*it)->Push_mark();
    }
    return _stack.size();
  }

  // restore state from stack
  void Pop_mark(INT level) {
    Is_True(!_stack.empty() && _stack.size() == level,
            ("stack empty or level mismatch"));
    STATE top = _stack.front();
    _stack.pop_front();
    Is_True(_nodes.size() >= top.Ncount(), ("bad node top"));
    Is_True(_edges.size() >= top.Ecount(), ("bad edge top"));
    Is_True(_phis.size()  >= top.Pcount(), ("bad phi top"));

    if (_phis.size() > top.Pcount()) {
      _phis.resize(top.Pcount());
    }

    if (_nodes.size() > top.Ncount()) {
      // restore node vector
      _nodes.resize(top.Ncount(), &_null_node);
      // restore each map
      Remove_map_entry(_cst_map, top.Ncount());
      Remove_map_entry(_var_map, top.Ncount());
      Remove_map_entry(_vor_map, top.Ncount());
      Remove_map_entry(_op_map, top.Ncount());
    }

    if (_edges.size() > top.Ecount()) {
      // restore edge vector
      _edges.resize(top.Ecount(), &_null_edge);
    }
    // restore each node
    NODE_VEC::iterator end = _nodes.end();
    for (NODE_VEC::iterator it = _nodes.begin(); it != end; ++it) {
      (*it)->Pop_mark();
    }
  }

private:
  // node utilities

  // check if two nodes have same value by checking if they are on the
  // same chain
  BOOL Has_same_value(NODE_IDX lhs, NODE_IDX rhs) const {
    Is_True_Ret(lhs != VG_NULL_IDX, ("invalid lhs"), FALSE);
    Is_True_Ret(rhs != VG_NULL_IDX, ("invalid rhs"), FALSE);
    const NODE *node = Node(lhs);
    const NODE *rhs_node = Node(rhs);
    Is_True_Ret(lhs != rhs, ("same node"), TRUE);
    // swap lhs/rhs if rhs is smaller
    if (rhs < lhs) {
      NODE_IDX tmp = rhs;
      rhs = lhs;
      lhs = tmp;
    }
    // check parent
    if (node->Parent() != rhs_node->Parent())
      return FALSE;
    // search next chain
    while (node->Next() != VG_NULL_IDX) {
      if (node->Next() == rhs)
        return TRUE;
      node = Node(node->Next());
    }
    return FALSE;
  }

  // Hash key for OP cr
  uintptr_t Op_hash(VSA *vsa, CODEREP *cr) const {
    Is_True(cr->Kind() == CK_OP, ("not OP"));
    Is_True(cr->Opr() != OPR_CALL &&
             cr->Opr() != OPR_ICALL &&
             cr->Opr() != OPR_INTRINSIC_CALL &&
             cr->Opr() != OPR_INTRINSIC_OP,
             ("invalid op"));
    Is_True(cr->Kid_count() <= 3, ("too many kids"));
    uintptr_t key = cr->Opr();
    for (INT i = 0; i < cr->Kid_count(); ++i) {
      NODE_IDX kid_idx = Find_cr_node(vsa, cr->Opnd(i));
      if (kid_idx == VG_NULL_IDX)
        return 0;
      key = (key << 16) | kid_idx;
    }
    return key;
  }

  // find node_idx from constant value
  NODE_IDX Find_const_node(INT64 val) const {
    NODE_MAP::const_iterator it = _cst_map.find((uintptr_t)val);
    if (it == _cst_map.end())
      return VG_NULL_IDX;
    Is_True(Node(it->second)->Const_val() == val &&
            Node(Node(it->second)->Parent())->Const_val() == val,
            ("invalid const node"));
    return it->second;
  }

  // find node_idx from variable/vsym
  NODE_IDX Find_variable_node(const NODE_MAP &map, uintptr_t ptr) const {
    NODE_MAP::const_iterator it = map.find(ptr);
    if (it == map.end())
      return VG_NULL_IDX;
    NODE *node = Node(it->second);
    Is_True(node->Parent() > VG_NULL_IDX &&
            node->Parent() <= it->second,
            ("invalid var node parent"));
    return node->Parent();
  }

  // find node_idx from op
  // need hash function for op combined with kind's node_idx and opr
  NODE_IDX Find_op_node(VSA *vsa, CODEREP *cr) const {
    uintptr_t key = Op_hash(vsa, cr);
    NODE_MAP::const_iterator it = _op_map.find(key);
    if (it == _op_map.end())
      return VG_NULL_IDX;
    NODE *node = Node(it->second);
    Is_True(node->Parent() > VG_NULL_IDX &&
            node->Parent() <= it->second,
            ("invalid var node parent"));
    return node->Parent();
  }

  // find node_idx for general coderep
  NODE_IDX Find_cr_node(VSA *vsa, CODEREP *cr) const {
    VSYM_OBJ_REP *vor;
    switch (cr->Kind()) {
    case CK_CONST:
    case CK_RCONST:
    case CK_LDA:
      return Find_const_node(Const_val(cr));
    case CK_VAR:
      return Find_variable_node(_var_map, (uintptr_t)cr);
    case CK_IVAR:
      Is_True(cr->Opr() != OPR_PARM, ("wrong PARM"));
      vor = vsa->Cr_2_vor(cr);
      Is_True_Ret(vor != NULL, ("not find the vor"), VG_NULL_IDX);
      if (vsa->Is_special_vor(vor))
        return Find_variable_node(_var_map, (uintptr_t)cr);
      else {
        return Find_variable_node(_vor_map, (uintptr_t)vor);
      }
    case CK_OP:
      return Find_op_node(vsa, cr);
    default:
      Is_True(FALSE, ("bad cr kind"));
      return VG_NULL_IDX;
    }
  }

private:
  // node management

  // add const to graph and return the node_idx
  NODE_IDX Add_const_node(INT64 val) {
    NODE_MAP::iterator it = _cst_map.find((uintptr_t)val);
    if (it != _cst_map.end()) {
      Is_True(Node(it->second)->Const_val() == val &&
              Node(Node(it->second)->Parent())->Const_val() == val,
              ("const val mismatch"));
      return it->second;
    }
    Is_True(_nodes.size() < VG_MAX_IDX, ("max nodes reached"));
    if (_nodes.size() == VG_MAX_IDX)
      return VG_NULL_IDX;
    NODE_IDX idx = _nodes.size();
    NODE *node = CXX_NEW(NODE(_mpool, idx, val), _mpool);
    _nodes.push_back(node);
    _cst_map[val] = idx;
    return idx;
  }

  // add var/vor to graph and return the node_idx
  NODE_IDX Add_variable_node(NODE_MAP &map, uintptr_t ptr) {
    NODE_MAP::iterator it = map.find(ptr);
    if (it != map.end()) {
      NODE *node = Node(it->second);
      Is_True(node->Parent() > VG_NULL_IDX &&
              node->Parent() <= it->second,
              ("var node parent mismatch"));
      return node->Parent();
    }
    Is_True(_nodes.size() < VG_MAX_IDX, ("max nodes reached"));
    if (_nodes.size() == VG_MAX_IDX)
      return VG_NULL_IDX;
    NODE_IDX idx = _nodes.size();
    NODE *node = CXX_NEW(NODE(_mpool, idx), _mpool);
    _nodes.push_back(node);
    map[ptr] = idx;
    return idx;
  }

  // add op to graph and return the node_idx
  NODE_IDX Add_op_node(VSA *vsa, CODEREP *cr) {
    Is_True(cr->Kind() == CK_OP, ("not OP cr"));
    if (cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL)
      return Add_cr_node(vsa, cr->Opnd(0));
    // add op cr by default
    return Add_variable_node(_var_map, (uintptr_t)cr);
  }

  // add a generic coderep
  NODE_IDX Add_cr_node(VSA *vsa, CODEREP *cr) {
    VSYM_OBJ_REP *vor;
    switch (cr->Kind()) {
    case CK_CONST:
    case CK_RCONST:
    case CK_LDA:
      return Add_const_node(Const_val(cr));
    case CK_VAR:
      return Add_variable_node(_var_map, (uintptr_t)cr);
    case CK_IVAR:
      Is_True(cr->Opr() != OPR_PARM, ("wrong PARM"));
      vor = vsa->Cr_2_vor(cr);
      Is_True_Ret(vor != NULL, ("not find the vor"), VG_NULL_IDX);
      if (vsa->Is_special_vor(vor)) {
        return Add_variable_node(_var_map, (uintptr_t)cr);
      } else {
        return Add_variable_node(_vor_map, (uintptr_t)vor);
      }
    case CK_OP:
      return Add_op_node(vsa, cr);
    default:
      Is_True(FALSE, ("bad cr kind"));
      return VG_NULL_IDX;
    }
  }

  // add a generic edge
  EDGE_IDX Add_edge(NODE_IDX l, NODE_IDX r, EDGE_IDX e, EDGE_OPER o, EDGE_FLAG f)
  {
    Is_True(e == VG_NULL_IDX || e < _edges.size(),
            ("wrong disj edge"));
    Is_True(_edges.size() < VG_MAX_IDX,
            ("max edges reached"));
    if (_edges.size() == VG_MAX_IDX)
      return VG_NULL_IDX;
    EDGE_IDX idx = _edges.size();
    _edges.push_back(CXX_NEW(EDGE(l, r, e, o, f), _mpool));
    Node(l)->Add_out_edge(idx);
    Node(r)->Add_in_edge(idx);
    return idx;
  }

private:
  // add node and check graph

  typedef hash_map<NODE_IDX, EDGE_OPER> EVAL_MAP;
  OP_RESULT Eval_node(NODE_IDX idx, EVAL_MAP &map) const;

  // node was assigned with const val, check if the target is satisfied
  // or there is conflict
  OP_RESULT Check_node_const(NODE_IDX idx, NODE_IDX const_val) const;

  // node was connected with another, check if the target is satisfied
  // or there is conflict
  OP_RESULT Check_node_connected(NODE_IDX idx) const;

  // connect two node for assignment and cmp_eq, check if the target
  // is satisfied or there is conflict
  OP_RESULT Connect_nodes(NODE_IDX lhs, NODE_IDX rhs) {
    Is_True(lhs != VG_NULL_IDX, ("invalid lhs"));
    Is_True(rhs != VG_NULL_IDX, ("invalid rhs"));
    Is_True(lhs != rhs, ("same node"));
    NODE *lhs_node = Node(lhs);
    NODE *rhs_node = Node(rhs);
    Is_True(lhs_node->Parent() == lhs, ("lhs is not parent"));
    Is_True(rhs_node->Parent() == rhs, ("rhs is not parent"));
    // choose new common parent from minimal index
    NODE_IDX new_par, old_par, to_merge, to_verify;
    if (lhs < rhs) {
      new_par = lhs;
      old_par = rhs;
      to_merge = rhs;
      to_verify = lhs_node->Next();
    }
    else {
      new_par = rhs;
      old_par = lhs;
      to_merge = lhs;
      to_verify = rhs_node->Next();
    }

    Node(new_par)->Set_next(to_merge);
    while (TRUE) {
      NODE *to_merge_node = Node(to_merge);
      Is_True(to_merge_node->Parent() == old_par, ("parent mismatch"));
      to_merge_node->Set_parent(new_par);
      NODE_IDX next_merge = to_merge_node->Next();
      if (next_merge == 0) {
        to_merge_node->Set_next(to_verify);
        break;
      }
      to_merge = next_merge;
    }

#ifdef Is_True_On
    while (to_verify > 0) {
      NODE *verify_node = Node(to_verify);
      Is_True(verify_node->Parent() == new_par,
              ("parent mismatch"));
      to_verify = verify_node->Next();
    }
#endif

    return OP_CONTINUE;
  }

  // append a const to existing node
  OP_RESULT Append_const(NODE_IDX idx, INT64 val) {
    Is_True(idx != VG_NULL_IDX, ("bad idx"));
    NODE* node = Node(idx);
    Is_True(node->Parent() == idx, ("not parent node"));
    if (node->Is_const()) {
      // TODO: assert their parent is the name
      //Is_True(Find_const_node(node->Const_val()) == idx,
      //        ("connst map mismatch"));
      if (node->Const_val() == val) {
        node->Inc_const_num();
        return OP_CONTINUE;
      }
      else {
        return OP_CONFLICT;
      }
    }
    else {
      // find or create node for constant
      NODE_IDX const_idx = Find_const_node(val);
      if (const_idx == VG_NULL_IDX) {
        const_idx = Add_const_node(val);
      }
      else {
        const_idx = Node(const_idx)->Parent();
      }
      // connect constant node with existing node
      Connect_nodes(idx, const_idx);
      node->Set_const_val(val);
      // eval the node to check if there are conflicts
      EVAL_MAP map;
      map[idx] = E_EQ;
      OP_RESULT vg_ret = Eval_node(idx, map);
      return vg_ret;
    }
  }

  // append a var/vor to existing node
  OP_RESULT Append_variable(NODE_MAP &map, NODE_IDX idx, uintptr_t ptr) {
    Is_True(idx != VG_NULL_IDX && idx < _nodes.size(),
            ("bad idx"));
    NODE_IDX rhs_idx = Find_variable_node(map, ptr);
    // not added before, merge to idx
    if (rhs_idx == VG_NULL_IDX) {
      map[ptr] = idx;
      return OP_CONTINUE;
    }
    if (idx == rhs_idx) {
      // EQ append before
      return OP_CONTINUE;
    }
    Is_True(rhs_idx != VG_NULL_IDX && rhs_idx < _nodes.size(),
            ("bad idx"));

    // check and connect two nodes
    NODE* lhs = Node(idx);
    NODE* rhs = Node(rhs_idx);
    Is_True(lhs->Parent() == idx, ("lhs is not parent"));
    Is_True(rhs->Parent() == rhs_idx, ("rhs is not parent"));
    if (lhs->Is_const() && rhs->Is_const()) {
      Is_True(lhs->Const_val() != rhs->Const_val(),
              ("same value mapped to different node?"));
      if (lhs->Const_val() != rhs->Const_val())
        return OP_CONFLICT;
      Is_True(lhs->Parent() == rhs->Parent(), ("two nodes on diff chain"));
      Is_True(Has_same_value(idx, rhs_idx), ("two nodes on diff chain"));
      // same const val can't be assigned with two nodes
      Is_True(FALSE, ("same const with two nodes"));
      return OP_ERROR;
    }
    // propagate const val
    if (lhs->Is_const()) {
      rhs->Set_const_val(lhs->Const_val());
    }
    else if (rhs->Is_const()) {
      lhs->Set_const_val(rhs->Const_val());
    }
    // connect to same chain
    return Connect_nodes(idx, rhs_idx);
  }

  // append op cr to existing node
  OP_RESULT Append_op(NODE_IDX idx, VSA *vsa, CODEREP *cr);

  // append generic coderep to existing node
  OP_RESULT Append_cr(NODE_IDX idx, VSA *vsa, CODEREP *cr) {
    VSYM_OBJ_REP *vor;
    switch (cr->Kind()) {
    case CK_CONST:
    case CK_RCONST:
    case CK_LDA:
      return Append_const(idx, Const_val(cr));
    case CK_VAR:
      return Append_variable(_var_map, idx, (uintptr_t)cr);
    case CK_IVAR:
      Is_True(cr->Opr() != OPR_PARM, ("wrong PARM"));
      vor = vsa->Cr_2_vor(cr);
      Is_True_Ret(vor != NULL, ("not find the vor"), OP_ERROR);
      if (vsa->Is_special_vor(vor)) {
        return Append_variable(_var_map, idx, (uintptr_t)cr);
      } else {
        return Append_variable(_vor_map, idx, (uintptr_t)vor);
      }
    case CK_OP:
      return Append_op(idx, vsa, cr);
    default:
      Is_True(FALSE, ("bad cr kind"));
      return OP_ERROR;
    }
  }

  // handle compare with two existing nodes, include NE/GE/GT/LE/LT
  template<OPERATOR _CMP_OPR>
  OP_RESULT Handle_compare(NODE_IDX lhs, NODE_IDX rhs);

  // handle compare with node and cr, include NE/GE/LE/LT
  template<OPERATOR _CMP_OPR>
  OP_RESULT Handle_compare(NODE_IDX idx, VSA *vsa, CODEREP *cr);

  // handle compare with two crs, include NE/GE/GT/LE/LT
  template<OPERATOR _CMP_OPR>
  OP_RESULT Handle_compare(VSA *lvsa, CODEREP *lhs, VSA *rvsa, CODEREP *rhs) {
    NODE_IDX lhs_id = Add_cr_node(lvsa, lhs);
    if (lhs_id == VG_NULL_IDX)
      return OP_ERROR;

    NODE_IDX rhs_id = Add_cr_node(rvsa, rhs);
    if (rhs_id == VG_NULL_IDX)
      return OP_ERROR;

    OP_RESULT vg_ret = Handle_compare<_CMP_OPR>(lhs_id, rhs_id);
    if (vg_ret == OP_CONTINUE) {
      Is_Trace(_tracing, (TFile, "@@VG: Add N%d(cr%d) %s N%d(cr%d)\n",
                                  lhs_id, lhs->Coderep_id(), OPERATOR_name(_CMP_OPR),
                                  rhs_id, rhs->Coderep_id()));
    }
    return vg_ret;
  }

  // handle EQ and assign between two crs
  OP_RESULT Handle_eq(VSA *lvsa, CODEREP *lhs, VSA *rvsa, CODEREP *rhs) {
    // swap lhs and rhs if lhs is constant
    if (lhs->Kind() == CK_CONST ||
        lhs->Kind() == CK_LDA ||
        lhs->Kind() == CK_RCONST) {
      CODEREP *tmp = lhs;
      lhs = rhs;
      rhs = tmp;
      if (lvsa != rvsa) {
        VSA *tvsa = lvsa;
        lvsa = rvsa;
        rvsa = tvsa;
      }  
    }
    Is_True(lhs->Kind() != CK_CONST && lhs->Kind() != CK_LDA &&
            lhs->Kind() != CK_RCONST, ("constants eq?"));
    // add lhs
    NODE_IDX lhs_id = Add_cr_node(lvsa, lhs);
    if (lhs_id == VG_NULL_IDX)
      return OP_ERROR;

    return Append_cr(lhs_id, rvsa, rhs);
  }

  // add U-D for non-primary variable which is assigned by constant
  // or another variable
  OP_RESULT Add_var_ud(VSA *vsa, CODEREP *cr);
  OP_RESULT Add_vor_ud(VSA *vsa, VSYM_OBJ_REP *vor);
  OP_RESULT Eval_phi_opnd(DNA_NODE *dna, PHI_NODE *phi, INT32 opnd_idx, BOOL &maybe);

public:
  // constructor
  VALUE_GRAPH(MEM_POOL *mpool)
    : _mpool(mpool),
      _tracing(Get_Trace(TP_CHECKER, CHK_VG_TRACE_FLAG)),
      _cst_map(3, __gnu_cxx::hash<uintptr_t>(),
               std::equal_to<uintptr_t>(), NMAP_ALLOC(mpool)),
      _var_map(3, __gnu_cxx::hash<uintptr_t>(),
               std::equal_to<uintptr_t>(), NMAP_ALLOC(mpool)),
      _vor_map(3, __gnu_cxx::hash<uintptr_t>(),
               std::equal_to<uintptr_t>(), NMAP_ALLOC(mpool)),
      _op_map(3, __gnu_cxx::hash<uintptr_t>(),
              std::equal_to<uintptr_t>(), NMAP_ALLOC(mpool)),
      _nodes(NODE_ALLOC(mpool)),
      _edges(EDGE_ALLOC(mpool)),
      _phis(PHI_ALLOC(mpool)),
      _stack(STATE_ALLOC(mpool)) {
    _nodes.reserve(7);
    _edges.reserve(7);
    _phis.reserve(7);
    // reserve VG_NULL_IDX
    _nodes.push_back(&_null_node);
    // reserve VG_NULL_IDX
    _edges.push_back(&_null_edge);
  }

public:
  // handle assignment like lhs = rhs
  OP_RESULT Add_assign(VSA *lvsa, CODEREP *lhs, VSA *rvsa, CODEREP *rhs) {
    Is_True((lhs->Kind() == CK_VAR && !lhs->Is_flag_set(CF_IS_ZERO_VERSION)) ||
            (lhs->Kind() == CK_IVAR && lhs->Opr() != OPR_PARM), ("bad lhs"));
    Is_True(rhs->Kind() != CK_VAR ||
            !rhs->Is_flag_set(CF_IS_ZERO_VERSION), ("bad rhs"));
    // treat it as lhs == rhs
    return Handle_eq(lvsa, lhs, rvsa, rhs);
  }

  // handle assignment like lhs_vor = rhs_vor
  OP_RESULT Add_assign(VSA *lvsa, VSYM_OBJ_REP *lhs, VSA *rvsa, VSYM_OBJ_REP *rhs) {
    Is_True(lvsa != NULL && lhs != NULL && rvsa != NULL && rhs != NULL,
            ("bad param"));
    Is_True(!lvsa->Is_special_vor(lhs) && !rvsa->Is_special_vor(rhs),
            ("special vor"));
    NODE_IDX lhs_id = Find_variable_node(_vor_map, (uintptr_t)lhs);
    if (lhs_id == VG_NULL_IDX)
      return OP_ERROR;

    return Append_variable(_vor_map, lhs_id, (uintptr_t)rhs);
  }

  // handle assignment like cr = vor
  OP_RESULT Add_assign(VSA *lvsa, CODEREP *lhs, VSA *rvsa, VSYM_OBJ_REP *rhs) {
    Is_True(lvsa != NULL && lhs != NULL && rvsa != NULL && rhs != NULL,
            ("bad param"));
    Is_True(lhs->Kind() == CK_VAR && !lhs->Is_flag_set(CF_IS_ZERO_VERSION),
            ("bad lhs cr"));
    Is_True(!rvsa->Is_special_vor(rhs),
            ("special vor"));
    NODE_IDX lhs_id = Add_cr_node(lvsa, lhs);
    if (lhs_id == VG_NULL_IDX)
      return OP_ERROR;
    return Append_variable(_vor_map, lhs_id, (uintptr_t)rhs);;
  }

  // handle assignment like vor = cr
  OP_RESULT Add_assign(VSA *lvsa, VSYM_OBJ_REP *lhs, VSA *rvsa, CODEREP *rhs) {
    Is_True(lvsa != NULL && lhs != NULL && rvsa != NULL && rhs != NULL,
            ("bad param"));
    Is_True(!lvsa->Is_special_vor(lhs),
            ("special vor"));
    NODE_IDX lhs_id = Find_variable_node(_vor_map, (uintptr_t)lhs);
    if (lhs_id == VG_NULL_IDX)
      return OP_ERROR;

    return Append_cr(lhs_id, rvsa, rhs);
  }

  // handle comparison like lhs opr rhs, include EQ/NE/GE/GT/LE/LT
  OP_RESULT Add_cmp_cr(OPERATOR opr, VSA *vsa, CODEREP *lhs, CODEREP *rhs) {
    switch (opr) {
    case OPR_EQ:
      return Handle_eq(vsa, lhs, vsa, rhs);
    case OPR_NE:
      return Handle_compare<OPR_NE>(vsa, lhs, vsa, rhs);
    case OPR_GE:
      return Handle_compare<OPR_GE>(vsa, lhs, vsa, rhs);
    case OPR_GT:
      return Handle_compare<OPR_GT>(vsa, lhs, vsa, rhs);
    case OPR_LE:
      return Handle_compare<OPR_LE>(vsa, lhs, vsa, rhs);
    case OPR_LT:
      return Handle_compare<OPR_LT>(vsa, lhs, vsa, rhs);
    default:
      Is_True(FALSE, ("%s is not comparison", OPERATOR_name(opr) + 4));
      return OP_ERROR;
    }
  }

  // handle op cr. so far only EQ/NE/GE/GT/LE/LT handled
  // probably add/sub/neg/abs/etc can be added
  OP_RESULT Add_op_cr(VSA *vsa, CODEREP *cr) {
    Is_True(cr && cr->Kind() == CK_OP, ("bad cr"));
    switch (cr->Opr()) {
    case OPR_EQ:
      return Handle_eq(vsa, cr->Opnd(0),
                      vsa, cr->Opnd(1));
    case OPR_NE:
      return Handle_compare<OPR_NE>(vsa, cr->Opnd(0),
                                   vsa, cr->Opnd(1));
    case OPR_GE:
      return Handle_compare<OPR_GE>(vsa, cr->Opnd(0),
                                   vsa, cr->Opnd(1));
    case OPR_GT:
      return Handle_compare<OPR_GT>(vsa, cr->Opnd(0),
                                   vsa, cr->Opnd(1));
    case OPR_LE:
      return Handle_compare<OPR_LE>(vsa, cr->Opnd(0),
                                   vsa, cr->Opnd(1));
    case OPR_LT:
      return Handle_compare<OPR_LT>(vsa, cr->Opnd(0),
                                   vsa, cr->Opnd(1));
    default: 
      Is_True(FALSE, ("TODO: %s\n", OPERATOR_name(cr->Opr()) + 4));
      return OP_ERROR;
    }
  }

#if 0
  BOOL Is_cr_relevant(DNA_NODE *dna, CODEREP *cr);
  BOOL Is_var_relevant(DNA_NODE *dna, CODEREP *cr);
  BOOL Is_ivar_relevant(DNA_NODE *dna, CODEREP *cr);
  BOOL Is_op_relevant(DNA_NODE *dna, CODEREP *cr);
  BOOL Add_cda(DNA_NODE *dna, CDA_VALUE cda);
#endif
  OP_RESULT Add_control_dependency(DNA_NODE *dna, BB_NODE *pred, BB_NODE *succ);
  OP_RESULT Add_phi_opnd(DNA_NODE *dna, PHI_NODE *phi, INT32 opnd_idx, BOOL &maybe);
  OP_RESULT Add_cmp_cda(DNA_NODE *dna, CDA_VALUE cda);
  OP_RESULT Eval_graph(BOOL &maybe);

  // set the target to be satisfied
  OP_RESULT Set_target(VSA *vsa, EDGE_OPER opr, CODEREP *lhs, CODEREP *rhs) {
    Is_True(_nodes.size() == 1 && _edges.size() == 1,
            ("target already set"));
    NODE_IDX lhs_id = Add_cr_node(vsa, lhs);
    if (lhs_id == VG_NULL_IDX)
      return OP_ERROR;
    NODE_IDX rhs_id = Add_cr_node(vsa, rhs);
    if (rhs_id == VG_NULL_IDX || lhs_id == rhs_id)
      return OP_ERROR;
    Add_edge(lhs_id, rhs_id, VG_NULL_IDX, opr, EF_TARGET);
    return OP_CONTINUE;
  }

  // set the target to be satisfied
  OP_RESULT Set_target(VSA *vsa, EDGE_OPER opr, CODEREP *lhs, UINT64 rhs) {
    Is_True(_nodes.size() == 1 && _edges.size() == 1,
            ("target already set"));
    NODE_IDX lhs_id = Add_cr_node(vsa, lhs);
    if (lhs_id == VG_NULL_IDX)
      return OP_ERROR;
    NODE_IDX rhs_id = Add_const_node(rhs);
    if (rhs_id == VG_NULL_IDX || lhs_id == rhs_id)
      return OP_ERROR;
    Add_edge(lhs_id, rhs_id, VG_NULL_IDX, opr, EF_TARGET);
    return OP_CONTINUE;
  }

  // dump map
  template<typename _KEY>
  void Dump_map(const NODE_MAP &map, FILE *fp) const;

  // dump single node
  void Dump(NODE_IDX idx, FILE *fp) const;

  // dump whole graph
  void Dump(FILE *fp) const;

  // Const value from CODEREP
  static UINT64 Const_val(CODEREP *cr);

}; // VALUE_GRAPH

#endif /* opt_vsa_graph_INCLUDED */

