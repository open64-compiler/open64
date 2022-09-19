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
// opt_vsa_scc.h
//
// Template for Strongly Connected Component (SCC) processing
// ==================================================================

#ifndef opt_vsa_scc_INCLUDED
#define opt_vsa_scc_INCLUDED

// ==================================================================
// Tarjan's Strongly Connected Component for the directed graph like
// CFG, SSA and CG to keep track of information with circular depend-
// ency caused by back-edge in loop or recursion call.
// ==================================================================

#include "defs.h"
#include "mempool_allocator.h"
#include <vector>

// put in a anonymous namespace so that different template specializations
// with same template parameters won't conflict
namespace {

// template declaration for SCC
// _N is the node type, which can be BB_NODE, CODEREP, DNA_NODE
// _G is the graph type, which can be CFG, SSA, IPSA
template<typename _N, typename _G>
class TSCC {
private:
  // NODE in this SCC
  class NODE {
  private:
    _N     _id;             // object ID, which can be BB, CR or DNA
    UINT32 _low : 31;       // low-link of this ID in DFS stack
    UINT32 _on_stack : 1;   // if this ID is on solver stack

  public:
    // constructor
    NODE(_N id, UINT32 low, BOOL on_stack)
      : _id(id), _low(low), _on_stack(on_stack) { }

    // return node or node id
    _N     Id() const            { return _id;       }
    // return low link
    UINT32 Low() const           { return _low;      }
    // return if node is on dfs stack
    BOOL   On_stack() const      { return _on_stack; }

    // set node or node id
    void   Set_id(_N id)         { _id = id;         }
    // set low link
    void   Set_low(UINT32 low)   { _low = low;       }
    // set node on dfs stack
    void   Set_on_stack(BOOL on) { _on_stack = on;   }
  };

private:
  typedef mempool_allocator<NODE>           NODE_ALLOCATOR;
  typedef std::vector<NODE, NODE_ALLOCATOR> NODE_VEC;

  UINT32      _id;        // last entry in the vector
  UINT32      _count;     // count of entries in this scc
  NODE_VEC   *_node_vec;  // actual content in this SCC
  STACK<_N>  *_stack;     // solver stack
  MEM_POOL    _mem_pool;  // self managed mempool
  BOOL        _trace;     // is tracing on

  TSCC(const TSCC&);                // disable copy constructor
  TSCC& operator = (const TSCC&);   // disable assign operator

private:
  // return the scc id
  UINT32 Id() const { return _id; }
  // return the node count in the scc
  UINT32 Count() const { return _count; }
  // increase node count by 1
  void   Inc_count() { ++_count; }
  // return given node's low linka by node index
  UINT32 Low(UINT32 i) const { return (*_node_vec)[i].Low(); }
  // return given node or node id by node index
  _N     Node(UINT32 i) const { return (*_node_vec)[i].Id(); }
  // check if node on stack by node index
  BOOL   On_stack(UINT32 i) const { return (*_node_vec)[i].On_stack(); }
  // set node's low link by node index
  void   Set_low(UINT32 i, UINT32 v) { (*_node_vec)[i].Set_low(v); }
  // check if tracing is on
  BOOL   Tracing() const { return _trace; }

  // add node x into SCC
  void   Push(_N x) {
    _node_vec->push_back(NODE(x, _id, TRUE));
    ++ _id;
    _stack->Push(x);
  }

  // search node x in SCC and return the index in node vector
  INT32  Lookup(_N x) const {
    for (INT32 i = 0; i < _node_vec->size(); ++i) {
      if (Node(i) == x)  // probably need override `==' here
        return i;
    }
    return -1;
  }

  // get PHI_NODE which defines node X
  // implemented below in this file
  PHI_NODE *Defphi(_N x, _G g) const;

  // do a Dfs search in graph follow node x's U-D
  // implemented below in this file
  // refer SCC::Dfs in opt_vsa.cxx ~ line 80
  void   Dfs(_N x, _G g, hash_set<UINT32>&);

public:
  // construct SCC fron node x inside graph g
  TSCC(_N x, _G g, BOOL trace) : _id(0), _count(1), _trace(trace) {
    // initialize mempool
    OPT_POOL_Initialize(&_mem_pool, "scc pool", FALSE, VSA_DUMP_FLAG);
    OPT_POOL_Push(&_mem_pool, VSA_DUMP_FLAG);
    // initialize vector and stack
    _node_vec = CXX_NEW(NODE_VEC(NODE_ALLOCATOR(&_mem_pool)),
                        &_mem_pool);
    _stack = CXX_NEW(STACK<_N>(&_mem_pool), &_mem_pool);
    // do DFS search
    hash_set<UINT32> visited;
    Dfs(x, g, visited);
  }

  // destructor
  ~TSCC() {
    // destroy mempool
    OPT_POOL_Pop(&_mem_pool, VSA_DUMP_FLAG);
    OPT_POOL_Delete(&_mem_pool, VSA_DUMP_FLAG);
  }

  // check if the process of x should be defered
  BOOL   Defer(_N x) const {
    INT32 i = Lookup(x);
    if (i == -1)
      return FALSE;
    if (Low(i) == i && i+1 < Id() && Low(i+1) == i)
      return TRUE;  // first element of scc and the scc has multiple components
    if (Low(i) != i && Low(Low(i)) == Low(i))
      return TRUE;  // not 1st element, then it's low_link must point to itself
    return FALSE;
  }

  // update all nodes in this scc
  // implemented by template specialization
  // refer SCC::Update in opt_vsa.cxx ~ line 150
  void Update(_G g) const;

  // print this scc
  // implemented by template specialization
  // refer SCC::Print in opt_vsa.cxx ~ line 200
  void Print(FILE *fp, _G g) const;

};  // class TSCC

// TSCC<CODEREP *, COMP_UNIT *>::Defphi
// get defphi for the coderep
template<> PHI_NODE *
TSCC<CODEREP *, COMP_UNIT *>::Defphi(CODEREP *x, COMP_UNIT *g) const
{
  Is_True(x->Aux_id() != g->Opt_stab()->Default_vsym() &&
          x->Aux_id() != g->Opt_stab()->Return_vsym(),
          ("def/ret vsym"));

  // only visit phi result and non-zero version
  if (x->Is_flag_set(CF_DEF_BY_PHI) &&
      !x->Is_flag_set(CF_IS_ZERO_VERSION))
    return x->Defphi();
  else
    return NULL;
}  // TSCC::Defphi

// TSCC<VSYM_OBJ_REP *, VSA*>::Defphi
// get defphi for the VSYM_OBJ_REP
template<> PHI_NODE *
TSCC<VSYM_OBJ_REP *, VSA*>::Defphi(VSYM_OBJ_REP *x, VSA *g) const
{
  // only visit phi result
  if (x->Attr() == ROR_DEF_BY_PHI)
    return x->Phi_def();
  else
    return NULL;
}  // TSCC:Defphi

// template<typename _N, typename _G>
// do a DFS search and add node into SCC
template<typename _N, typename _G> void
TSCC<_N, _G>::Dfs(_N x, _G g, hash_set<UINT32>& visited)
{
  Is_True(x != NULL, ("null var"));

  PHI_NODE *phi = Defphi(x, g);
  if (phi == NULL)
    return;

  Is_True(phi && phi->RESULT() == (CODEREP *)x, ("bad phi"));
  if (visited.find(phi->Bb()->Id()) != visited.end()) {
    INT id = Lookup(x);
    if ((id >= 0) && On_stack(id)) {
      for (INT i = Id()-1; i > id; --i) {
        Set_low(i, (Low(id) < Low(i))? Low(id) : Low(i));
        Inc_count();
      }
    }
    return;
  }
  visited.insert(phi->Bb()->Id());

  Push(x);
  // check all its operands since it has not been checked
  CODEREP *opnd;
  PHI_OPND_ITER phi_opnd_iter(phi);
  FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
    Dfs((_N)opnd, g, visited);
  }
}  // TSCC:Dfs

}  // end of anonymous namespace

#endif /* opt_vsa_scc_INCLUDED */
