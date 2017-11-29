/*
 Copyright (C) 2010, Hewlett-Packard Development Company, L.P.
 All Rights Reserved.

 Open64 is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 Open64 is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA.
*/
#ifndef _IPA_NYSTROM_ALIAS_ANALYZER_INCLUDED
#define _IPA_NYSTROM_ALIAS_ANALYZER_INCLUDED

#include <ext/hash_map>
#include "config_opt.h"
#include "constraint_graph.h"
#include "ipa_cg.h"
#include "opt_defs.h"

using namespace std;
using namespace __gnu_cxx;

class EdgeDelta;
class IPA_EscapeAnalysis;

typedef hash_map<NODE_INDEX, ConstraintGraph *> IPACGMap;
typedef IPACGMap::const_iterator IPACGMapIterator;

typedef struct {
   size_t operator() (const ST * const&k) const { return (size_t)k; }
 } hashSTToNode;
typedef struct {
   bool operator() (const ST * const &k1, ST * const&k2) const { return k1 == k2; }
} equalSTToNode;
typedef hash_map<ST *,IPA_NODE *,hashSTToNode,equalSTToNode> STToNodeMap;


extern BOOL Write_ALIAS_CGNODE_Map;

class IPANodeSCCInfo {
public:
  IPANodeSCCInfo() : _visited(false), _inSCC(false), _rep(NULL) {}

  bool visited(void) const { return _visited; }
  void visited(bool v)     { _visited = v;    }
  bool inSCC(void)   const { return _inSCC;   }
  void inSCC(bool v)       { _inSCC = v;      }
  IPA_NODE *rep(void)const { return _rep;     }
  void rep(IPA_NODE *n)    { _rep = n;     }

private:
  bool     _visited;
  bool     _inSCC;
  IPA_NODE *_rep;
};

typedef AUX_IPA_NODE<IPANodeSCCInfo> IPANodeSCCAuxInfo;

class IPA_NystromAliasAnalyzer {

public:
  static IPA_NystromAliasAnalyzer *create() 
  {
    if (Alias_Nystrom_Analyzer)
      if (_ipa_naa == NULL)
        _ipa_naa = new IPA_NystromAliasAnalyzer();
    return _ipa_naa;
  }

  static void clean()
  {
    if (_ipa_naa)
      delete _ipa_naa;
  }

  static IPA_NystromAliasAnalyzer *aliasAnalyzer() { return _ipa_naa; }
 
  IPA_NystromAliasAnalyzer()  
    : _ipaNodeSCCAuxInfo(NULL)
  {
    if (Get_Trace(TP_ALIAS, NYSTROM_MEMORY_TRACE_FLAG))
      MEM_Tracing_Enable();
    MEM_POOL_Initialize(&_memPool, "IPA_NystromAliasAnalyzer_pool", FALSE);
    ConstraintGraph::inIPA(true);
    ConstraintGraph::globalCG(CXX_NEW(ConstraintGraph(&_memPool), &_memPool));
    // Set flag to dump the WN to CGNodeId map during Write_PU_Info
    Write_ALIAS_CGNODE_Map = TRUE;
  }

  ~IPA_NystromAliasAnalyzer() 
  {
    MEM_POOL_Delete(&_memPool);
  }

  void deleteConstraintGraph(IPA_NODE *node)
  {
    // For now just delete the entry from the map
    _ipaConstraintGraphs.erase(node->Node_Index());
  } 

  void buildIPAConstraintGraph(IPA_NODE *ipa_node);
  void print(FILE *file);

  // Perform a global context-[in]sensitive points-to solution
  void solver(IPA_CALL_GRAPH *);

  ConstraintGraph *cg(NODE_INDEX idx) const
  {
    hash_map<NODE_INDEX,ConstraintGraph*>::const_iterator iter=
        _ipaConstraintGraphs.find(idx);
    if (iter != _ipaConstraintGraphs.end())
      return iter->second;
    else
      return NULL;
  }

  void mapWNToUniqCallSiteCGNodeId(IPA_NODE *node);

  void updateLocalSyms(IPA_NODE *node);

  void updateCGForBE(IPA_NODE *node);

  bool processInlineFormal(IPA_NODE *caller, IPA_NODE *callee, WN* actaul, ST* formal_st);

  ConstraintGraphNode* getCGNode(WN* wn, IPA_NODE* ipaNode);

  void solveInlineConstraints(IPA_NODE *callee, IPA_NODE *caller);

  bool inlineSolveNode(ConstraintGraphNode *node, ConstraintGraph* callee_cg, IPA_NODE *caller);

  ConstraintGraphNode* getCloneNode(ConstraintGraphNode *callee_node, ConstraintGraph* callee_cg, IPA_NODE* caller);

  void addInlineNodeMap(CGNodeId orig, CGNodeId new_cs) {
    _inline_node_map[orig] = new_cs;
  }

  CGNodeId getInlineNodeMap(CGNodeId orig) {
    hash_map<UINT32, UINT32>::const_iterator iter = _inline_node_map.find(orig);
    if (iter == _inline_node_map.end())
      return 0;
    return iter->second;
  }

  void updateCloneTreeWithCgnode(WN* tree);

  // For IPA call graph SCC detection
  void allocSCCInfo(IPA_CALL_GRAPH *icg)
  {
    if (!_ipaNodeSCCAuxInfo)
      _ipaNodeSCCAuxInfo = CXX_NEW(IPANodeSCCAuxInfo(icg,&_memPool),&_memPool);
  }
  bool visited(const IPA_NODE *n)  { return (*_ipaNodeSCCAuxInfo)[n].visited(); }
  bool inSCC(const IPA_NODE *n)    { return (*_ipaNodeSCCAuxInfo)[n].inSCC(); }
  IPA_NODE *rep(const IPA_NODE *n) { return (*_ipaNodeSCCAuxInfo)[n].rep(); }

  void setVisited(const IPA_NODE *n,bool v) {(*_ipaNodeSCCAuxInfo)[n].visited(v); }
  void setInSCC(const IPA_NODE *n,bool v)   {(*_ipaNodeSCCAuxInfo)[n].inSCC(v); }
  void rep(const IPA_NODE *n, IPA_NODE *r)  {(*_ipaNodeSCCAuxInfo)[n].rep(r); }

  // Temporary table used to track indirect call edges resolved
  // during analysis.  Eventually we should be able to update the
  // IPA_CALL_GRAPH directly and add additional IPA_EDGEs.
  class IPAEdge {
  public:
    IPAEdge(NODE_INDEX clr, NODE_INDEX cle, CallSiteId id)
      : _callerIdx(clr), _calleeIdx(cle), _csId(id) {}

    NODE_INDEX callerIdx(void) const { return _callerIdx; }
    NODE_INDEX calleeIdx(void) const { return _calleeIdx; }
    CallSiteId csId(void) const      { return _csId; }
    size_t hash() const { return _callerIdx << 16 ^ _calleeIdx << 8 ^ _csId; }
    bool operator ==(const IPAEdge &that) const {
      return _callerIdx == that._callerIdx &&
          _calleeIdx == that._calleeIdx &&
          _csId == that._csId;
    }
  private:
    NODE_INDEX _callerIdx;
    NODE_INDEX _calleeIdx;
    CallSiteId _csId;
  };

  typedef struct {
    size_t operator() (const IPAEdge &k) const { return k.hash(); }
  } hashIPAEdgeData;
  typedef struct {
    bool operator() (const IPAEdge &k1, const IPAEdge &k2) const { return k1 == k2; }
  } equalIPAEdgeData;
  typedef hash_set<IPAEdge, hashIPAEdgeData, equalIPAEdgeData> IndirectEdgeSet;


private:

  void callGraphSetup(IPA_CALL_GRAPH *, list<IPAEdge> &edgeList,
                      list<pair<IPA_NODE *, CallSiteId> > &indirectCallList);

  void callGraphPrep(IPA_CALL_GRAPH *ipaCG, list<IPAEdge> &workList,
                     EdgeDelta &delta, list<IPA_NODE *>&revTopOrder,
                     UINT32 round);

  bool updateCallGraph(IPA_CALL_GRAPH *ipaCG,
                       list<pair<IPA_NODE *,CallSiteId> > &indCallList,
                       list<IPAEdge> &edgeList);

  bool findIncompleteIndirectCalls(IPA_CALL_GRAPH *ipaCallGraph,
                list<pair<IPA_NODE *,CallSiteId> > &indCallList,
                list<IPAEdge> &edgeList,
                IPA_EscapeAnalysis &escAnal);

  bool validTargetOfVirtualCall(CallSite *cs, ST_IDX stIdx);

  static IPA_NystromAliasAnalyzer *_ipa_naa;
  IPACGMap _ipaConstraintGraphs;
  MEM_POOL _memPool;
  IPANodeSCCAuxInfo * _ipaNodeSCCAuxInfo;

  // Indirect edges that have been resolved in the IPA call graph
  IndirectEdgeSet _indirectEdgeSet;

  // Mapping from ST * to IPA_NODE * to facilitate resolving indirect
  // call targets from the points-to set.
  STToNodeMap _stToIndTgtMap;

  // Mapping from ST* to all functions in the IPA scope, used for
  // determining calls to external functions.
  STToNodeMap _stToIPANodeMap;

  // The set of external calls made from within the IPA scope
  hash_set<ST_IDX> _extCallSet;

  // hashmap map original cg node id with cloned cg node for nystrom inline
  hash_map<UINT32, UINT32> _inline_node_map;
};

#endif
