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
#include <stack>
#include "be_util.h"
#include "config_opt.h"
#include "constraint_graph.h"
#include "constraint_graph_escanal.h"
#include "constraint_graph_solve.h"
#include "opt_defs.h"
#include "opt_sys.h"
#include "tracing.h"

double ConstraintGraphSolve::totalTime       = 0.0;
UINT32 ConstraintGraphSolve::totalIterCount  = 0;
UINT32 ConstraintGraphSolve::totalCopyCount  = 0;
UINT32 ConstraintGraphSolve::totalSkewCount  = 0;
UINT32 ConstraintGraphSolve::totalLoadCount  = 0;
UINT32 ConstraintGraphSolve::totalStoreCount = 0;

//
// Performs cycle detection within the constraint graph.  The
// algorithm employed is Nuutila's algorithm:
//
// Esko Nuutila and Eljas Soisalon-Soininen, "On finding the
//   strongly connected components in a directed graph".
//   Inf. Process. Letters, 49(1):9-14, 1994.
//
// The algorithm provides two benefits.  First it is an improvement
// over Tarjan's algorithm, runs in O(E) time.  Second it produces
// a topological ordering of the target graph for free, which
// will improve the efficiency of the constraint graph solver.
//
// The implementation is derived from the pseudo code in the
// following paper:
//
// Pereira and Berlin, "Wave Propagation and Deep Propagation for
//   Pointer Analysis", CGO 2009, 126-135, 2009.
//
class SCCDetection {

  typedef mempool_allocator<ConstraintGraphNode *> CGNodeAllocator;
  typedef deque<ConstraintGraphNode *,CGNodeAllocator> CGNodeDeque;

public:
  typedef stack<ConstraintGraphNode *,CGNodeDeque> CGNodeStack;

  SCCDetection(ConstraintGraph *graph, MEM_POOL *mpool)
  : _graph(graph),
    _memPool(mpool),
    _I(0),
    _D(graph->totalCGNodes()),
    _S(CGNodeDeque(mpool)),
    //_T(CGNodeDeque(mpool)),
    _nodeCount(0),
    _topoOrderIndex(0)
  {}

  ~SCCDetection() {}

  // Detect and unify all SCCS within the constraint graph.
  void findAndUnify(UINT32 noMergeMask = 0);

  // Return a handle to the stack of nodes in topological
  // order.  This will be used to seed the initial solution
  // and improve efficiency.
  //CGNodeStack &topoNodeStack() { return _T; }

  typedef struct {
    size_t operator()(const ConstraintGraphNode *k) const
    {
      return k->id();
    }
  } hashCGNodeId;
  typedef hash_map<ConstraintGraphNode*,UINT32,hashCGNodeId> NodeToKValMap;
  typedef NodeToKValMap::const_iterator NodeToKValMapIterator;


protected:

  virtual void find(void);
  void checkFind(ConstraintGraphNode *node);

  virtual void unify(UINT32 noMergeMap, NodeToKValMap&);
  void checkUnify(ConstraintGraphNode *node, UINT32 noMergeMap, NodeToKValMap&);

  UINT32                 _nodeCount;

private:

  void visit(ConstraintGraphNode *node);
  void pointsToAdjust(NodeToKValMap&);

  ConstraintGraph       *_graph;
  MEM_POOL              *_memPool;
  UINT32                 _I;
  hash_map<CGNodeId,UINT32>   _D;
  hash_map<CGNodeId,CGNodeId> _R;
  CGNodeStack            _S;
  //CGNodeStack            _T;
  UINT32                 _topoOrderIndex;
};

class IPA_SCCDetection : public SCCDetection {
public:
  IPA_SCCDetection(MEM_POOL *mPool):
    SCCDetection(NULL,mPool) {}

  virtual void find(void);
  virtual void unify(UINT32 noMergeMap, NodeToKValMap&);
};

void
SCCDetection::visit(ConstraintGraphNode *v)
{
  if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
    fprintf(stderr,"visit: Node %d\n",v->id());
  _I += 1;
  _D[v->id()] = _I;
  _R[v->id()] = v->id();
  v->addFlags(CG_NODE_FLAGS_VISITED);
  for (CGEdgeSetIterator iter = v->outCopySkewEdges().begin();
      iter != v->outCopySkewEdges().end(); ++iter ) {
    ConstraintGraphEdge *edge = *iter;
    if (!edge->checkFlags(CG_EDGE_PARENT_COPY)) {
      ConstraintGraphNode *w = edge->destNode();
      // Cycle detection does not span constraint graphs
      if (_graph && w->cg() != _graph)
        continue;
      if (!w->checkFlags(CG_NODE_FLAGS_VISITED))
        visit(w);
      if (!w->checkFlags(CG_NODE_FLAGS_SCCMEMBER))
      {
        CGNodeId rep;
        rep = _D[_R[v->id()]] < _D[_R[w->id()]] ? _R[v->id()] : _R[w->id()];
        _R[v->id()] = rep;
      }
    }
  }
  if (_R[v->id()] == v->id() /*v->repParent() == v*/) {
    v->addFlags(CG_NODE_FLAGS_SCCMEMBER);
    while (!_S.empty()) {
      ConstraintGraphNode *w = _S.top();
      if (_D[w->id()] <= _D[v->id()])
        break;
      else {
        _S.pop();
        w->addFlags(CG_NODE_FLAGS_SCCMEMBER);
        _R[w->id()] = v->id(); /*w->repParent(v);*/
      }
    }
    v->topoOrderNum(++_topoOrderIndex);
    //_T.push(v);
  }
  else
    _S.push(v);
}

void
SCCDetection::checkFind(ConstraintGraphNode *node)
{
  if (!node->checkFlags(CG_NODE_FLAGS_VISITED)) {
    // We skip any nodes that have a representative other than
    // themselves.  Such nodes occur as a result of merging
    // nodes either through unifying an ACC or other node
    // merging optimizations.  Any such node should have no
    // outgoing edges and therefore should no longer be a member
    // of an SCC.
    if (node->repParent() == NULL || node->repParent() == node)
      visit(node);
    else {
      FmtAssert(node->checkFlags(CG_NODE_FLAGS_MERGED),
                ("Node with parent has not been merged!"));
      FmtAssert(node->outCopySkewEdges().empty(),
                ("Found copy edge: expect node with representative "
                    "to be a leaf node.\n"));
      FmtAssert(node->outLoadStoreEdges().empty(),
                ("Found copy edge: expect node with representative "
                    "to be a leaf node.\n"));
    }
  }
}

void
SCCDetection::find(void)
{
  // We capture the current number of nodes in the graph.  As we
  // merge nodes, additional offsets may materialize that will not
  // be marked as "visited".  They should have no incoming/outgoing
  // edges so we do not flag this as a problem.
  _nodeCount = 0;

  // Visit each unvisited root node.   A root node is defined
  // to be a node that has no incoming copy/skew edges
  for (CGNodeToIdMapIterator iter = _graph->lBegin();
      iter != _graph->lEnd(); iter++, _nodeCount++) {
    ConstraintGraphNode *node = iter->first;
    checkFind(node);
  }
}

void
IPA_SCCDetection::find(void)
{
  if (Get_Trace(TP_ALIAS, NYSTROM_SOLVER_FLAG))
    fprintf(stderr,"SCC: Global cycle detection begin\n");

  // We capture the current number of nodes in the graph.  As we
  // merge nodes, additional offsets may materialize that will not
  // be marked as "visited".  They should have no incoming/outgoing
  // edges so we do not flag this as a problem.
  _nodeCount = 0;

  // Visit each unvisited root node.   A root node is defined
  // to be a node that has no incoming copy/skew edges
  for (CGIdToNodeMapIterator iter = ConstraintGraph::gBegin();
       iter != ConstraintGraph::gEnd(); iter++, _nodeCount++) {
    ConstraintGraphNode *node = iter->second;
    if (!node->checkFlags(CG_NODE_FLAGS_MERGED))
      for ( PointsToIterator pti(node); pti != 0; ++pti )
        node->sanityCheckPointsTo(pti.qual());
    checkFind(node);
  }
}

void
SCCDetection::checkUnify(ConstraintGraphNode *node,
                         UINT32 noMergeMask,
                         NodeToKValMap &nodeToKValMap)
{
  FmtAssert(node->checkFlags(CG_NODE_FLAGS_VISITED) ||
            node->checkFlags(CG_NODE_FLAGS_MERGED) ||
            // New nodes created when merging nodes and
            // adjusting the points-to set for an updated
            // inKCycle() value.
            (node->id() >= _nodeCount &&
                node->outCopySkewEdges().empty() &&
                node->outLoadStoreEdges().empty() &&
                node->inCopySkewEdges().empty() &&
                node->inLoadStoreEdges().empty()),
                ("Node %d unvisited during SCC detection\n",node->id()));
  // The _R[] table should contain only those visited nodes that do
  // have a representative prior
  if (!node->checkFlags(CG_NODE_FLAGS_MERGED) &&
      _R[node->id()] != node->id()) {
    // If this is a node that we do not want to merge, yes cycle
    // detection may leave cycles in the graph, then we skip it.
    // However, we must reset the representative.
    if (node->stInfo()->flags() & noMergeMask) {
      //node->repParent(NULL);
      return;
    }
    ConstraintGraphEdge dummy(node->repParent(),node,ETYPE_COPY,CQ_HZ,0);
    if (!node->inEdge(&dummy)) {
      CGNodeId repId = _R[node->id()];
      if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
        fprintf(stderr,"Unify: Node %d -> Node %d\n",
                node->id(),repId);
      // We need to track this node so as to update the modulus
      // of the representative points-to set based on the 'new'
      // value of inKCycle() after the SCC has been collapsed.
      ConstraintGraphNode *rep = ConstraintGraph::cgNode(repId); //node->findRep();
      if (!rep->checkFlags(CG_NODE_FLAGS_INKVALMAP)) {
        nodeToKValMap[rep] = rep->inKCycle();
        rep->addFlags(CG_NODE_FLAGS_INKVALMAP);
      }
      rep->merge(node);
      node->repParent(rep);
    }
  }
  node->clearFlags(CG_NODE_FLAGS_VISITED|CG_NODE_FLAGS_SCCMEMBER);
}

void
SCCDetection::unify(UINT32 noMergeMask, NodeToKValMap &nodeToKValMap)
{
  // Unify the nodes in an SCC into a single node
  for (CGNodeToIdMapIterator iter = _graph->lBegin();
      iter != _graph->lEnd(); iter++) {
    ConstraintGraphNode *node = iter->first;
    checkUnify(node,noMergeMask,nodeToKValMap);
  }
}

void
IPA_SCCDetection::unify(UINT32 noMergeMask, NodeToKValMap &nodeToKValMap)
{
  // Unify the nodes in an SCC into a single node
  for (CGIdToNodeMapIterator iter = ConstraintGraph::gBegin();
        iter != ConstraintGraph::gEnd(); iter++) {
    ConstraintGraphNode *node = iter->second;
    checkUnify(node,noMergeMask,nodeToKValMap);
  }
  if (Get_Trace(TP_ALIAS, NYSTROM_SOLVER_FLAG))
    fprintf(stderr,"SCC: Global cycle detection end\n");
}

void
SCCDetection::pointsToAdjust(NodeToKValMap &nodeToKValMap)
{
  // Now that all SCCs have been merged, we update the merged
  // points-to sets based on the final K value.
  for (NodeToKValMapIterator iter = nodeToKValMap.begin();
       iter != nodeToKValMap.end(); ++iter ) {
    ConstraintGraphNode *rep = iter->first;
    UINT32 origKVal = iter->second;
    UINT32 newKVal = rep->inKCycle();
    for ( PointsToIterator pti(rep); pti != 0; ++pti ) {
      PointsTo tmp;
      PointsTo &ptsTo = *pti;
      ConstraintGraph::adjustPointsToForKCycle(rep,ptsTo,tmp,pti.qual());
      // Before we clear ptsTo, remove rep from rev pts of nodes in ptsTo
      for (PointsTo::SparseBitSetIterator sbsi(&ptsTo, 0); sbsi != 0; ++sbsi) {
        CGNodeId p = *sbsi;
        ConstraintGraphNode *pn = ConstraintGraph::cgNode(p);
        pn->removeRevPointsTo(rep->id(), pti.qual());
      }
      ptsTo.clear();
      rep->unionPointsTo(tmp, pti.qual());
      tmp.clear();
    }
  }
}

void
SCCDetection::findAndUnify(UINT32 noMergeMask)
{
  // Reset state
  _I = 0;
  _D.clear();
  _R.clear();
  while (!_S.empty()) _S.pop();
  //while (!_T.empty()) _T.pop();

  // Mapping of representative node points to the original K value
  // used to determine the updated needed to the merged points-to sets.
  NodeToKValMap nodeToKValMap;

  find();
  unify(noMergeMask,nodeToKValMap);
  pointsToAdjust(nodeToKValMap);
}

void
EdgeDelta::add(list<ConstraintGraphEdge *> &edgeList)
{
  for (list<ConstraintGraphEdge *>::iterator iter = edgeList.begin(); 
       iter != edgeList.end(); iter++)
    add(*iter);
}
      
void
EdgeDelta::add(ConstraintGraphEdge *e)
{
  bool added;
  if (e->edgeType() == ETYPE_COPY ||
      e->edgeType() == ETYPE_SKEW)
    added = copySkewList().push(e);
  else {
    FmtAssert(e->edgeType() == ETYPE_LOAD ||
              e->edgeType() == ETYPE_STORE,
              ("Unknown edgetype in ConstraintGraphDelta::add()"));
    added = loadStoreList().push(e);
  }
  if (added && Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG)) {
    fprintf(stderr,"   Added to worklist: ");
    e->print(stderr);
    fprintf(stderr,"\n");
  }
}

void
EdgeDelta::find(CGNodeId src, CGNodeId dst)
{
  list<ConstraintGraphEdge *> &copyList = copySkewList().workList();
  list<ConstraintGraphEdge *>::iterator copyIter = copyList.begin();
  for ( ; copyIter != copyList.end(); ++copyIter ) {
    ConstraintGraphEdge *edge = *copyIter;
    if (edge->srcNode()->id() == src &&
        edge->destNode()->id() == dst) {
      fprintf(stderr,"Found: ");
      edge->print(stderr);
      fprintf(stderr," flags 0x%x\n",edge->flags());
    }
  }

  list<ConstraintGraphEdge *> &loadList = loadStoreList().workList();
  list<ConstraintGraphEdge *>::iterator loadIter = loadList.begin();
  for ( ; loadIter != loadList.end(); ++loadIter ) {
    ConstraintGraphEdge *edge = *copyIter;
    if (edge->srcNode()->id() == src &&
        edge->destNode()->id() == dst) {
      fprintf(stderr,"Found: ");
      edge->print(stderr);
      fprintf(stderr," flags 0x%x\n",edge->flags());
    }
  }
}

void
EdgeDelta::findPtr(ConstraintGraphEdge *e)
{
  list<ConstraintGraphEdge *> &copyList = copySkewList().workList();
  list<ConstraintGraphEdge *>::iterator copyIter = copyList.begin();
  for ( ; copyIter != copyList.end(); ++copyIter ) {
    ConstraintGraphEdge *edge = *copyIter;
    if (edge == e) {
      fprintf(stderr,"Found: ");
      edge->print(stderr);
      fprintf(stderr," flags 0x%x\n",edge->flags());
    }
  }

  list<ConstraintGraphEdge *> &loadList = loadStoreList().workList();
  list<ConstraintGraphEdge *>::iterator loadIter = loadList.begin();
  for ( ; loadIter != loadList.end(); ++loadIter ) {
    ConstraintGraphEdge *edge = *copyIter;
    if (edge == e) {
      fprintf(stderr,"Found: ");
      edge->print(stderr);
      fprintf(stderr," flags 0x%x\n",edge->flags());
    }
  }
}

// Method to perform cycle detection/unification only
void
ConstraintGraphSolve::cycleDetection(UINT32 noMergeMask)
{
  FmtAssert(_cg,("cycleDetection: Requires ConstraintGraph!\n"));
  SCCDetection sccs(_cg,_memPool);
  sccs.findAndUnify(noMergeMask);
}

bool
ConstraintGraphNode::updatePointsToFromDiff()
{
  if (Get_Trace(TP_ALIAS, NYSTROM_SOLVER_FLAG))
    fprintf(stderr,"Update pts of %d\n",id());
  FmtAssert(!checkFlags(CG_NODE_FLAGS_MERGED),
            ("Node %d should not be merged at the beginning of update.\n",id()));

  findRep()->checkIsPtrAligned();

  bool ptsChange = false;
  for ( PointsToIterator pti(this,PtsDiff); pti != 0; ++pti ) {
    PointsTo &diff = *pti;
    PointsTo &dstPts = _getPointsTo(pti.qual());
    for (PointsTo::SparseBitSetIterator siter(&diff,0); siter != 0; ++siter) {
      ConstraintGraphNode *ptNode = ConstraintGraph::cgNode(*siter);
      // This may happen because the diff sets are not updated
      // when nodes are collapsed.
      while (ptNode->checkFlags(CG_NODE_FLAGS_COLLAPSED))
        ptNode = ConstraintGraph::cgNode(ptNode->collapsedParent());

      // First we deal with any K Cycle value that may exist on
      // the current node.
      ConstraintGraphNode *adjPtNode = NULL;
      if (inKCycle() != 0)
        ConstraintGraph::adjustNodeForKCycle(this,ptNode,adjPtNode);
      else
        adjPtNode = ptNode;

      // As a result of the K cycle adjustment, the current node may be
      // merged with another.  At that point the parent is now on the
      // worklist and the "diff" has been copied to the parent.  We
      // stop processing this node at this time.
      if (checkFlags(CG_NODE_FLAGS_MERGED))
        return false;

      if (adjPtNode->offset() != -1) {
        ConstraintGraphNode *minusOne =
              adjPtNode->cg()->checkCGNode(adjPtNode->cg_st_idx(),-1);
        // Destination set contains <ST, -1>, so we skip the current node
        if (minusOne && dstPts.isSet(minusOne->id()))
          continue;
      }
      // The current node is <ST,-1> so we need to remove all
      // occurrences of <ST,ofst> from the destination set
      else {
        ConstraintGraphNode::removeNonMinusOneOffsets(dstPts,
                                                      adjPtNode->cg_st_idx(),
                                                      this,pti.qual());
      }

      // Set up the (rev)points-to relationship.
      bool change = dstPts.setBit(adjPtNode->id());
      ptsChange |= change;
      if (change)
        adjPtNode->_addRevPointsTo(id(),pti.qual());
    }

    Is_True(sanityCheckPointsTo(pti.qual()),(""));

    // We are done with this diff, so nuke it.
    diff.clear();
  }
  return ptsChange;
}

static void
checkNode(ConstraintGraphNode *n)
{
  for (PointsToIterator pti(n); pti != 0; ++pti ) {
    PointsTo &curSet = *pti;
    if (curSet.numBits() > 100) {
      fprintf(stderr, "\nExcess pts set for n (qual:%d)",  pti.qual());
      n->print(stderr);
    }
  }
}

static void
checkExcessPtsSet(UINT32& dumped, ConstraintGraphNode *node)
{
  if (dumped>0) {
    UINT32 card = 0;
    for (PointsToIterator pti(node); pti != 0; ++pti) {
      PointsTo &pts = *pti;
      card += pts.numBits();
    }
    if (card > 500) {
      fprintf(stderr,"Excess pts set (card %d):\n",card);
      fprintf(stderr,"  Node %d, offset %d, ty %s, cg %s\n",
              node->id(),node->offset(), TY_name(node->ty_idx()),
              node->cg()->name());
      fprintf(stderr,"  Points to...\n");
      for (PointsToIterator pti(node); pti != 0; ++pti) {
        PointsTo &pts = *pti;
        for (PointsTo::SparseBitSetIterator iter(&pts,0); iter != 0; ++iter) {
          ConstraintGraphNode *n = ConstraintGraph::cgNode(*iter);
          fprintf(stderr,"    Node %d, offset %d, ty %s, cg %s\n",
                  n->id(),n->offset(), TY_name(n->id()),n->cg()->name());
          if (n->id() == 4148)
            n->print(stderr);
        }
      }
      dumped -= 1;
    }
  }
}

static int
topoCGNodeCompare(const void *n1, const void *n2)
{
  const ConstraintGraphNode *cgn1 = static_cast<const ConstraintGraphNode *>(n1);
  const ConstraintGraphNode *cgn2 = static_cast<const ConstraintGraphNode *>(n2);
  return (cgn1->topoOrderNum() < cgn2->topoOrderNum() ? -1 : 1);
}

// Core method to solve a constraint graph assuming that the
// boundary conditions for the solution are provided by 'delta'
// This method assumes that the constraint graph is acyclic.
bool
ConstraintGraphSolve::solveConstraints(UINT32 noMergeMask)
{
  bool trace = Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG);
  bool lw_trace = Get_Trace(TP_ALIAS,NYSTROM_LW_SOLVER_FLAG);

  // TODO: Perform cycle detection, here
  SCCDetection  *sccs = NULL;
  if (_cg)
    sccs = new SCCDetection(_cg,_memPool);
  else if (Alias_Nystrom_Global_Cycle_Detection)
    sccs = new IPA_SCCDetection(_memPool);

  bool seed = edgeDelta().empty() && (_cg != NULL);
  EdgeWorkList &copySkewList = edgeDelta().copySkewList();
  EdgeWorkList &loadStoreList = edgeDelta().loadStoreList();

  // If there are nodes in the modNodeList that were added during build
  // (eg: buildCGNodeipa), add their edges
  NodeWorkList &modNodeList = *(ConstraintGraph::solverModList());
  if (!modNodeList.empty()) {
    UINT32 numNodes = modNodeList.size();
    if (trace)
      fprintf(stderr, "Processing %d initial modified nodes\n",numNodes);
    ConstraintGraphNode **topoOrderArray =
      (ConstraintGraphNode **)malloc(sizeof(ConstraintGraphNode *)* numNodes);
    UINT32 i = 0;
    while (!modNodeList.empty()) {
      ConstraintGraphNode *node = modNodeList.pop();
      topoOrderArray[i++] = node;
    }
    // Sort based on topo-order number
    qsort(topoOrderArray,i,sizeof(ConstraintGraphNode *),topoCGNodeCompare);
    // Add edges to be processed in topological order
    for ( UINT32 j = 0; j < i; j++) {
      ConstraintGraphNode *node = topoOrderArray[j];
      if (trace) fprintf(stderr,"Node %d\n",node->id());
      ConstraintGraph::addEdgesToWorkList(node);
    }
    free(topoOrderArray);
  }

  UINT32 iterCount = 0;
  UINT32 copyCount = 0;
  UINT32 skewCount = 0;
  UINT32 loadCount = 0;
  UINT32 storeCount = 0;

  UINT32 startTime = CLOCK_IN_MS();

  do {
    // Here we walk the constraint graph to locate any SCCs and
    // collapse them to ensure that the solver will converge.
    if (sccs)
      sccs->findAndUnify(noMergeMask);

    // We need to seed the the solver with the appropriate
    // edges, either based on the SCCDetection traversal or the
    // provided edge delta.
    if (seed) {
      seed = false;
      if (trace) fprintf(stderr,"\nSeeding solver:\n");
      ConstraintGraphNode **topoOrderArray =
          (ConstraintGraphNode **)malloc(sizeof(ConstraintGraphNode *)*
                                         _cg->totalCGNodes());
      UINT32 i = 0;
      for (CGNodeToIdMapIterator iter = _cg->lBegin();
           iter != _cg->lEnd(); ++iter)
        topoOrderArray[i++] = iter->first;
      qsort(topoOrderArray,i,sizeof(ConstraintGraphNode *),topoCGNodeCompare);
      // Add edges to be processed in topological order
      for ( UINT32 j = 0; j < i; j++) {
        ConstraintGraphNode *node = topoOrderArray[j];
        if (trace) fprintf(stderr,"Node %d\n",node->id());
        ConstraintGraph::addEdgesToWorkList(node);
      }
      free(topoOrderArray);
    }

    ++iterCount;

    if (trace || lw_trace) {
      fprintf(stderr,"Solver Iteration %d\n",iterCount);
      ConstraintGraph::stats();
    }
    do {
      if (lw_trace) {
        fprintf(stderr,"start solve copySkewList, size is %d\n", copySkewList.size());
      }
      while (!copySkewList.empty()) {
        ConstraintGraphEdge *edge = copySkewList.pop();
        if (edge->checkFlags(CG_EDGE_TO_BE_DELETED)) {
          CXX_DELETE(edge,ConstraintGraph::edgePool());
          continue;
        }
        if (trace) {
          fprintf(stderr," Copy Edge:");
          edge->print(stderr);
          fprintf(stderr,"\n");
        }
        if (edge->edgeType() == ETYPE_COPY) {
          processAssign(edge);
          copyCount += 1;
        }
        else {
          FmtAssert(edge->edgeType() == ETYPE_SKEW,
                    ("ConstraintGraph::solveConstraints: type %d edge in "
                        "copy/skew worklist",edge->edgeType()));
          processSkew(edge);
          skewCount += 1;
        }
      }
      if (lw_trace) {
        fprintf(stderr,"end solve copySkewList\n");
      }

      // Determine which edges need to be visited as a result of the
      // most recent round of copy edge processing.  We also do some
      // work on the pts of the modified nodes to improve solver efficiency.
      if (lw_trace) {
        fprintf(stderr,"start solve modNodeList, size is %d\n", modNodeList.size());
      }
      NodeWorkList &modNodeList = *(ConstraintGraph::solverModList());
      if (!modNodeList.empty()) {
        UINT32 numNodes = modNodeList.size();
        if (trace)
          fprintf(stderr, "Processing %d modified nodes\n",numNodes);
        ConstraintGraphNode **topoOrderArray =
            (ConstraintGraphNode **)malloc(sizeof(ConstraintGraphNode *)*
                                           numNodes);
        // Copy node ptrs to array
        UINT32 i = 0;
        UINT32 dumped = 5;
        while (!modNodeList.empty()) {
          ConstraintGraphNode *node = modNodeList.pop();
          // During this loop we may encounter merged nodes as the
          // points-to processing we are doing may result in the
          // collapsing of the offsets for a given StInfo.
          if (node->checkFlags(CG_NODE_FLAGS_MERGED))
            continue;

          // Here we perform lots of other work on the points-to sets
          // of the modified nodes.  This may become too time consuming
          // and we may have to implement some kind of diff tracking to
          // reduce the amount of unnecessary work that goes on here.
          bool change = node->updatePointsToFromDiff();
          if (!change)
            continue;

          // While processing a node, we could add additional nodes
          // to the worklist, so we reallocate to accomodate.
          if (i == numNodes) {
            topoOrderArray = (ConstraintGraphNode **)
                    realloc(topoOrderArray,
                            sizeof(ConstraintGraphNode *)*numNodes*2);
            numNodes *= 2;
          }
          topoOrderArray[i++] = node;
          // checkExcessPtsSet(dumped, node);
        }
        // Sort based on topo-order number
        qsort(topoOrderArray,i,sizeof(ConstraintGraphNode *),topoCGNodeCompare);

        // If we have objects in the pts of the updated object 
        // whose types are incompatible with the type of the updated object
        // collapse them
        if (ConstraintGraph::inIPA()) {
          for (UINT32 j = 0; j < i; j++)
            topoOrderArray[j]->collapseTypeIncompatibleNodes();
        }

        // Add edges to be processed in topological order
        for ( UINT32 j = 0; j < i; j++)
          ConstraintGraph::addEdgesToWorkList(topoOrderArray[j]);
        free(topoOrderArray);
      }
      if (lw_trace) {
        fprintf(stderr,"end solve modNodeList\n");
      }

    } while (!copySkewList.empty());

    if (lw_trace) {
      fprintf(stderr,"start solve loadStoreList, size is %d\n", loadStoreList.size());
    }
    while (!loadStoreList.empty()) {
      ConstraintGraphEdge *edge = loadStoreList.pop();
      if (edge->checkFlags(CG_EDGE_TO_BE_DELETED)) {
        CXX_DELETE(edge,ConstraintGraph::edgePool());
        continue;
      }
      if (trace) {
        fprintf(stderr," Ld/St Edge:");
        edge->print(stderr);
        fprintf(stderr,"\n");
      }
      if (edge->edgeType() == ETYPE_LOAD) {
        processLoad(edge);
        loadCount += 1;
      }
      else {
        FmtAssert(edge->edgeType() == ETYPE_STORE,
            ("ConstraintGraph::solveConstraints: type %d edge in"
                "load/store worklist",edge->edgeType()));
        if (!processStore(edge))
          // Bad news.  At the very least, this means that both
          // the src and dest of this edge were flagged as unknown,
          // which means we may as well give up.
          return false;
        storeCount += 1;
      }
    }
    if (lw_trace) {
      fprintf(stderr,"end solve loadStoreList\n");
    }
  } while (!copySkewList.empty());

  UINT32 endTime = CLOCK_IN_MS();

  if (lw_trace)
    fprintf(stderr,"Solver required %d iter, "
            "processed %d c, %d s, %d l, %d s edges in %.1lfs\n",
            iterCount,copyCount,skewCount,loadCount,storeCount,
            double(endTime-startTime)/1000);

  totalIterCount += iterCount;
  totalCopyCount += copyCount;
  totalSkewCount += skewCount;
  totalLoadCount += loadCount;
  totalStoreCount += storeCount;
  totalTime += (double(endTime-startTime)/1000);

  FmtAssert(ConstraintGraph::solverModList()->empty(), 
            ("Expecting modNodeList to be empty"));
  if (sccs)
    delete sccs;

  return true;
}

void
ConstraintGraphSolve::printStats()
{
  fprintf(stderr,"Overall Solver required %d iter, "
          "processed %d c, %d s, %d l, %d s edges in %.1lfs\n",
          totalIterCount,totalCopyCount,totalSkewCount,totalLoadCount,
          totalStoreCount,totalTime);
}

bool
ConstraintGraph::nonIPASolver()
{
  // Here we solve the constraint graph for the current procedure
  EdgeDelta delta;
  ConstraintGraphSolve solver(delta,this,_memPool);

  // Provide the constraint graph with a handle on the edge delta
  // so that we updated it correctly when deleting edges
  workList(&delta);

  // For now we try to cycle eliminate all variables including GLOBALS.
  if (!solver.solveConstraints()) {
    workList(NULL);
    return false;
  }

  // Now we perform escape analysis to in order to augment the
  // the points-to sets of "incomplete" symbols to facilitate
  // comparison of their points-to sets with symbols for which
  // we have "complete" information.
  EscapeAnalysis escAnal(this,false/*not summary*/,_memPool);
  escAnal.perform();
  escAnal.markEscaped();

  escAnal.identifyMallocWrappers();

  workList(NULL);

  return true;
}

UINT32
ConstraintGraphNode::computeMaxAccessSize()
{
  UINT32 max = accessSize();
  for (CGEdgeSetIterator i1 = inLoadStoreEdges().begin();
      i1 != inLoadStoreEdges().end(); ++i1) {
    ConstraintGraphEdge *edge = *i1;
    if (edge->edgeType() == ETYPE_STORE)
      if (edge->size() > max)
        max = edge->size();
  }
  for (CGEdgeSetIterator i2 = outLoadStoreEdges().begin();
      i2 != outLoadStoreEdges().end(); ++i2) {
    ConstraintGraphEdge *edge = *i2;
    if (edge->edgeType() == ETYPE_LOAD)
      if (edge->size() > max)
        max = edge->size();
  }
  return max;
}

void
ConstraintGraphNode::postProcessPointsTo(PointsTo &adjustSet)
{
  // Max access size is defined to be the max across all outgoing
  // load and incoming store edges.  We default to 1.
  UINT32 maxAccessSize = computeMaxAccessSize();
  for (PointsToIterator pti(this); pti != 0; ++pti ) {
    PointsTo &curSet = *pti;
    for (PointsTo::SparseBitSetIterator sbsi(&curSet,0); sbsi != 0; ++sbsi)
    {
      ConstraintGraphNode *node = ConstraintGraph::cgNode(*sbsi);
      if (node->offset() == -1)
      {
        ConstraintGraphNode *cur = node->nextOffset();
        while (cur) {
          adjustSet.setBit(cur->id());
          cur = cur->nextOffset();
        }
      }
      else {
        // Handle changes in the size of the modulus that may have
        // made the current offset "out of bounds".  The modulus
        // applied to an offset is defined by a range, start...end.
        UINT32 startOffset;
        UINT32 modulus = node->stInfo()->getModulus(node->offset());
        UINT32 applyOffset =
            node->stInfo()->applyModulus(node->offset(),startOffset);
        if (node->offset() < (startOffset+modulus))
          adjustSet.setBit(node->id());
        else {
          if (!node->repParent()) {
            fprintf(stderr,"Insanity in StInfo %p\n",node->stInfo());
            node->stInfo()->print(stderr,true);
            FmtAssert(FALSE,("Node beyond modulus must have parent\n"));
          }

          node = node->repParent();
          adjustSet.setBit(node->id());
          // If we have overrun our offset limit, then the parent may
          // actually be <ST,-1> rather than <ST,offset%modulus>.
          // In that case we are done.
          if (node->offset() == -1)
            continue;
        }

        // Now we walk from offset to offset+accessSize and deal
        // with any wrap around that may occur at 'modulus'
        UINT32 endOffset = node->offset() + maxAccessSize-1;
        UINT32 endOffset2;
        bool wrapAround = (endOffset >= (startOffset + modulus));
        if (wrapAround) {
          endOffset = startOffset + modulus;
          endOffset2 = node->stInfo()->applyModulus(endOffset);
        }
        ConstraintGraphNode *cur = node->nextOffset();
        while (cur && cur->offset() < endOffset) {
          adjustSet.setBit(cur->id());
          cur = cur->nextOffset();
        }
        if (wrapAround){
          ConstraintGraphNode *cur = node->stInfo()->firstOffset();
          while (cur && cur->offset() < startOffset)
            cur = cur->nextOffset();
          // Now we walk the first part of the range until we hit
          // the new offset.
          while (cur && cur->offset() < endOffset2) {
            adjustSet.setBit(cur->id());
            cur = cur->nextOffset();
          }
        }
      }
    }
  }
}

void
ConstraintGraphSolve::postProcessPointsTo()
{
  PointsTo adjustSet;
  for (CGIdToNodeMapIterator iter = ConstraintGraph::gBegin();
       iter != ConstraintGraph::gEnd(); iter++) {
    ConstraintGraphNode *gNode = iter->second;
    // Max access size is defined to be the max across all outgoing
    // load and incoming store edges.  We default to 1.
    UINT32 maxAccessSize = gNode->computeMaxAccessSize();
    for ( PointsToIterator pti(gNode); pti != 0; ++pti ) {
      PointsTo &curSet = *pti;
      for (PointsTo::SparseBitSetIterator sbsi(&curSet,0); sbsi != 0; ++sbsi)
      {
        ConstraintGraphNode *node = ConstraintGraph::cgNode(*sbsi);
        if (node->offset() == -1)
        {
          ConstraintGraphNode *cur = node->nextOffset();
          while (cur) {
            adjustSet.setBit(cur->id());
            cur = cur->nextOffset();
          }
          // We must preserve the presence of <ST,-1> in the points
          // to set for communication between IPL and IPA_LINK. The
          // current theory being the presence of <ST,-1> does not
          // degrade the quality of the O escape analysis
          // nor the IPL alias queries.
          if (!ConstraintGraph::inIPA())
            adjustSet.setBit(node->id());
        }
        else {
          // Handle changes in the size of the modulus that may have
          // made the current offset "out of bounds".  The modulus
          // applied to an offset is defined by a range, start...end.
          // We can infer the start offset by applying the modulus to
          // the current offset
          UINT32 modulus = node->stInfo()->getModulus(node->offset());
          // applyOffset = startOffset + node->offset() % modulus
          UINT32 applyOffset = node->stInfo()->applyModulus(node->offset());
          UINT32 startOffset = applyOffset - (node->offset() % modulus);
          if (node->offset() >= (startOffset+modulus)) {
            if (!node->repParent()) {
              fprintf(stderr,"Insanity in StInfo %p\n",node->stInfo());
              node->stInfo()->print(stderr,true);
              FmtAssert(FALSE,("Node beyond modulus must have parent\n"));
            }

            node = node->repParent();
            adjustSet.setBit(node->id());
            // If we have overrun our offset limit, then the parent may
            // actually be <ST,-1> rather than <ST,offset%modulus>.
            // In that case we are done.
            if (node->offset() == -1)
              continue;
          }
          else
            adjustSet.setBit(node->id());

          // Now we walk from offset to offset+accessSize and deal
          // with any wrap around that may occur at 'modulus'
          UINT32 endOffset = node->offset() + maxAccessSize-1;
          UINT32 endOffset2;
          bool wrapAround = (endOffset >= (startOffset + modulus));
          if (wrapAround) {
            endOffset = startOffset + modulus;
            endOffset2 = node->stInfo()->applyModulus(endOffset);
          }
          ConstraintGraphNode *cur = node->nextOffset();
          while (cur && cur->offset() < endOffset) {
            adjustSet.setBit(cur->id());
            cur = cur->nextOffset();
          }
          if (wrapAround){
            ConstraintGraphNode *cur = node->stInfo()->firstOffset();
            while (cur && cur->offset() < startOffset)
              cur = cur->nextOffset();
            // Now we walk the first part of the range until we hit
            // the new offset.
            while (cur && cur->offset() < endOffset2) {
              adjustSet.setBit(cur->id());
              cur = cur->nextOffset();
            }
          }
        }
      }
      curSet.clear();
      curSet.setUnion(adjustSet);
      adjustSet.clear();
    }
  }
}

// give a src points to, and its points to qual and edge qual
// exclude the src points to from dest points to set.
void 
ConstraintGraphSolve::Exclude(PointsTo &src, CGEdgeType et, CGEdgeQual aq, 
                                CGEdgeQual eq, bool cs, PointsTo &dest)
{
  CGEdgeQual targetqual = qualMap(et, aq, eq, cs);
  if (targetqual == CQ_NONE)
    return;
  else
    dest.setDiff(src);
}

void
ConstraintGraph::addEdgesToWorkList(ConstraintGraphNode *node)
{
  EdgeDelta *edgeDelta = workList();
  // If this routine is being called, then the points-to set of
  // this node has been updated.  We need to add the following
  // edges to the work list for further processing:

  // 1) All outgoing copy/skew edges
  const CGEdgeSet &outCopySkew = node->outCopySkewEdges();
  for (CGEdgeSetIterator iter = outCopySkew.begin();
       iter != outCopySkew.end();
       iter++) {
    edgeDelta->add(*iter);
  }

  // 2) All outgoing load (assign_deref) edges
  const CGEdgeSet &outLoadStore = node->outLoadStoreEdges();
  for (CGEdgeSetIterator iter = outLoadStore.begin();
       iter != outLoadStore.end();
       iter++) {
    ConstraintGraphEdge *e = (*iter);
    if (e->edgeType() == ETYPE_LOAD)
      edgeDelta->add(e);
  }

  // 3) All incoming store (deref_assign) edges
  const CGEdgeSet &inLoadStore = node->inLoadStoreEdges();
  for (CGEdgeSetIterator iter = inLoadStore.begin();
       iter != inLoadStore.end();
       iter++) {
    ConstraintGraphEdge *e = (*iter);
    if (e->edgeType() == ETYPE_STORE)
      edgeDelta->add(e);
  }
}

void
ConstraintGraphSolve::updateOffsets(const ConstraintGraphNode *dst,
                                    const PointsTo &pts,
                                    CGEdgeQual dstQual)
{
  if (dst->offset() == -1) {
    ConstraintGraphNode *cur = dst->nextOffset();
    while (cur != NULL) {
      if ((cur->offset() % Pointer_Size) != 0) {
        cur = cur->nextOffset();
        continue;
      }
      bool change = false;
      change |= cur->unionDiffPointsTo(pts, dstQual);
      // Mark outgoing edges as to be updated....
      if (change)
        ConstraintGraph::solverModList()->push(cur);
      cur = cur->nextOffset();
    }
  } else {
    ConstraintGraphNode *firstOffset = dst->stInfo()->firstOffset();
    if (firstOffset && firstOffset->offset() == -1) {
      bool change = firstOffset->unionDiffPointsTo(pts, dstQual);
      // Mark outgoing edges as to be updated....
      if (change)
        ConstraintGraph::solverModList()->push(firstOffset);
    }
  }
}

void
ConstraintGraphNode::removeCollapsedNodes(PointsTo &dst)
{
  for (PointsTo::SparseBitSetIterator iter(&dst,0); iter != 0; ++iter) {
    ConstraintGraphNode *node = ConstraintGraph::cgNode(*iter);
    // For COLLAPSED nodes, replace the node with the collapsed parent
    if (node->checkFlags(CG_NODE_FLAGS_COLLAPSED)) {
      dst.clearBit(node->id());
      ConstraintGraphNode *collapsedParent = 
                           ConstraintGraph::cgNode(node->collapsedParent());
      FmtAssert(!collapsedParent->checkFlags(CG_NODE_FLAGS_COLLAPSED),
                ("collapsed parent: %d of node: %d is also collapsed\n",
                node->collapsedParent(), node->id()));
      dst.setBit(node->collapsedParent());
    }
  }
}

void
ConstraintGraphNode::removeNonMinusOneOffsets(PointsTo &dst, CG_ST_IDX idx,
                                              ConstraintGraphNode *theNode,
                                              CGEdgeQual qual)
{
  for (PointsTo::SparseBitSetIterator iter(&dst,0); iter != 0; ++iter) {
    ConstraintGraphNode *node = ConstraintGraph::cgNode(*iter);
    if (node->cg_st_idx() == idx && node->offset() != -1) {
      dst.clearBit(node->id());
      // If we are actually removing bits from the pts of a node,
      // rather than a temporary, we must update the rev pts as well.
      if (theNode) {
        FmtAssert(&theNode->pointsTo(qual) == &dst,
                  ("Expect PointsTo to be associated with provided CG node!\n"));
        node->removeRevPointsTo(theNode->id(),qual);
      }
    }
  }
}

void
ConstraintGraphNode::sanitizePointsTo(CGEdgeQual qual)
{
  PointsTo *pts = _findPointsTo(qual,Pts);
  if (pts)
    sanitizePointsTo(*pts,this,qual);
}

void
ConstraintGraphNode::sanitizePointsTo(PointsTo &pts,
                                      ConstraintGraphNode *theNode,
                                      CGEdgeQual qual)
{
   hash_set<CG_ST_IDX,hashCGstidx,equalCGstidx> minusOneSts;
   for (PointsTo::SparseBitSetIterator i1(&pts,0); i1 != 0; ++i1) {
     ConstraintGraphNode *node = ConstraintGraph::cgNode(*i1);
     if (node->offset() == -1)
       minusOneSts.insert(node->cg_st_idx());
   }

   hash_set<CG_ST_IDX,hashCGstidx,equalCGstidx>::iterator iter;
   for (iter = minusOneSts.begin(); iter != minusOneSts.end(); ++iter) {
     CG_ST_IDX idx = *iter;
     removeNonMinusOneOffsets(pts,idx,theNode,qual);
   }
}

bool
ConstraintGraphNode::sanityCheckPointsTo(CGEdgeQual qual)
{
  // Ignore merged nodes
  if (checkFlags(CG_NODE_FLAGS_MERGED))
    return true;

  const PointsTo &pts = pointsTo(qual);
  hash_set<CG_ST_IDX,hashCGstidx,equalCGstidx> minusOneSts;
  for (PointsTo::SparseBitSetIterator i1(&pts,0); i1 != 0; ++i1) {
    ConstraintGraphNode *node = ConstraintGraph::cgNode(*i1);
    if (node->offset() == -1)
      minusOneSts.insert(node->cg_st_idx());
    // If node is present in the pts, it had better not have
    // the COLLASPED flag.
    FmtAssert(!node->checkFlags(CG_NODE_FLAGS_COLLAPSED),
              ("Merge node failure: found COLLAPSED node %d with parent %d "
               "in pts of node %d",node->id(),node->collapsedParent(),id()));
    // If "node" is present in the pts of "this", then "this"
    // must be in the rev-pts of "node".  Here we are checking
    // for missing nodes in the rev-pts set.
    FmtAssert(node->_checkRevPointsTo(id(),qual),
              ("Node %d in pts of %d, but %d not in rev-pts of %d\n",
               node->id(),id(),id(),node->id()));
  }

  for (PointsTo::SparseBitSetIterator i2(&pts,0); i2 != 0; ++i2) {
    ConstraintGraphNode *node = ConstraintGraph::cgNode(*i2);
    if (node->offset() != -1) {
      hash_set<CG_ST_IDX,hashCGstidx,equalCGstidx>::const_iterator iter =
        minusOneSts.find(node->cg_st_idx());
      FmtAssert(iter == minusOneSts.end(),
                ("Node %d contains ST:%s offsets %d and -1\n",
                    id(),node->stName(),node->offset()));
    }
  }

  const PointsTo &revPts = revPointsTo(qual);
  for (PointsTo::SparseBitSetIterator i3(&revPts,0); i3 != 0; ++i3) {
    ConstraintGraphNode *node = ConstraintGraph::cgNode(*i3);
    // If "node" is present in the rev-pts of "this", then "this"
    // must be in the pts of "node".  Here we are checking for extra
    // nodes in the rev-pts set.
    FmtAssert(node->checkPointsTo(this,qual),
              ("Node %d in rev pts of %d, but %d not in pts of %d\n",
               node->id(),id(),id(),node->id()));
  }

  return true;
}

bool
ConstraintGraphNode::addPointsTo(ConstraintGraphNode *node, CGEdgeQual qual)
{
//  FmtAssert(!cg()->buildComplete() || node->offset() != -1,
//            ("Attempting to directly add <%d,%d> to pts\n",
//                SYM_ST_IDX(node->cg_st_idx()),node->offset()));
  node->addFlags(CG_NODE_FLAGS_ADDR_TAKEN);
  ConstraintGraphNode *repNode = findRep();
  // Check if add to a ptr aligned node. Ignore if we are adding blackholes
  if (node->id() != ConstraintGraph::blackHole()->id())
    repNode->checkIsPtrAligned();
  bool change = repNode->_addPointsTo(node->id(),qual);
  if (change) {
    addFlags(CG_NODE_FLAGS_PTSMOD);
    node->_addRevPointsTo(repNode->id(),qual);
  }
  return change;
}

void printPointsTo(const PointsTo &pts)
{
  for (PointsTo::SparseBitSetIterator iter(&pts,0); iter != 0; ++iter) {
    ConstraintGraphNode *node = ConstraintGraph::cgNode(*iter);
    fprintf(stderr,"(%d)<%d,%d> ",
            node->id(),SYM_ST_IDX(node->cg_st_idx()),node->offset());
  }
}

bool
ConstraintGraphNode::unionDiffPointsTo(const PointsTo &ptsToSet,
                                       CGEdgeQual qual)
{
  PointsTo diff;
  diff = ptsToSet;
  diff.setDiff(findRep()->_getPointsTo(qual));

  PointsTo &dst = findRep()->_getPointsTo(qual,PtsDiff);
  UINT32 trackNodeId = Alias_Nystrom_Solver_Track;
  if (trackNodeId != 0) {
    if (id() == trackNodeId && !diff.isEmpty()) {
      fprintf(stderr,"TRACK: In union points to:\n");
      fprintf(stderr,"TRACK:  Cur: ");
      printPointsTo(dst);
      fprintf(stderr,"\nTRACK:  New: ");
      printPointsTo(ptsToSet);
      fprintf(stderr,"\nTRACK:  Dif: ");
      printPointsTo(diff);
      fprintf(stderr,"\n");
    }

    if (diff.isSet(trackNodeId))
      fprintf(stderr,"TRACK: adding %d to pts of %d\n",trackNodeId,id());
  }


  bool change = dst.setUnion(diff);
  diff.clear();
  return change;
}

//  Given a 'src' and 'dest' points to set, insert the contents
// of 'src' into 'dest' subject to the followin rules.
// (1) if 'src' contains <ST,ofst>, 'dst' gets <ST,ofst> iff it
//     does not contain <ST,-1>
// (2) if 'src' contains <ST,-1>, 'dst' get <ST,-1> and all
//     <ST,ofst> are removed from 'dst'.
bool
ConstraintGraphNode::unionPointsTo(const PointsTo &ptsToSet, CGEdgeQual qual)
{
  bool change = false;

  if (ptsToSet.isEmpty())
    return change;

  findRep()->checkIsPtrAligned();

  // First we compute the difference between the current points-to
  // and the "new" values to be merged.
  PointsTo diff;
  PointsTo &dst = findRep()->_getPointsTo(qual);
  diff = ptsToSet;
  diff.setDiff(dst);

  // First we compute the difference between the current points-to
  // and the "new" values to be merged.
  UINT32 trackNodeId = Alias_Nystrom_Solver_Track;
  if (trackNodeId != 0) {
    if (id() == trackNodeId && !diff.isEmpty()) {
      fprintf(stderr,"TRACK: In union points to:\n");
      fprintf(stderr,"TRACK:  Cur: ");
      printPointsTo(dst);
      fprintf(stderr,"\nTRACK:  New: ");
      printPointsTo(ptsToSet);
      fprintf(stderr,"\nTRACK:  Dif: ");
      printPointsTo(diff);
      fprintf(stderr,"\n");
    }

    if (diff.isSet(trackNodeId))
      fprintf(stderr,"TRACK: adding %d to pts of %d\n",trackNodeId,id());
  }

  // Here is the actual update of the points-to set
  //_getPointsTo(qual).setUnion(ptsToSet);

  // Now we have to walk only the nodes that are not present
  // in the target points-to set.
  for (PointsTo::SparseBitSetIterator siter(&diff,0); siter != 0; ++siter) {
    ConstraintGraphNode *node = ConstraintGraph::cgNode(*siter);
    if (node->offset() != -1) {
      ConstraintGraphNode *minusOne =
          node->cg()->checkCGNode(node->cg_st_idx(),-1);
      // Destination set contains <ST, -1>, so we skip the current node
      if (minusOne && dst.isSet(minusOne->id())) {
        //fprintf(stderr,"Prevent adding %d\n",node->id());
        continue;
      }
      else {
        FmtAssert(!minusOne || !diff.isSet(minusOne->id()),
                  ("Have both %d:<%d,%d> and %d:<%d,-1> in the "
                      "incoming pts (dest is %d).\n",
                      node->id(),
                      SYM_ST_IDX(node->cg_st_idx()),node->offset(),
                      minusOne->id(),
                      SYM_ST_IDX(node->cg_st_idx()),id()));
        dst.setBit(node->id());
        node->_addRevPointsTo(id(),qual);
        change = true;
      }
    }
    // The current node is <ST,-1> so we need to remove all
    // occurrences of <ST,ofst> from the destination set
    else {
      removeNonMinusOneOffsets(dst,node->cg_st_idx(),this,qual);
      dst.setBit(node->id());
      node->_addRevPointsTo(id(),qual);
      change = true;
    }
  }


  if ((id() == trackNodeId && !diff.isEmpty()) || diff.isSet(trackNodeId)) {
    fprintf(stderr,"TRACK:  Mrg: ");
    printPointsTo(dst);
    fprintf(stderr,"\n");
  }
  diff.clear();

  Is_True(sanityCheckPointsTo(qual),
          ("Node %d destination contains <ST,x> and <ST,-1>\n",id()));
  if (change)
    addFlags(CG_NODE_FLAGS_PTSMOD);
  return change;
}

/*
 * Here is the original assignment rule from Nystrom's implementation
 *
 * u =[t1,z1,s1] v &&  v =&[t2,s2] w && s1 <= t2 <= s1+z1-4
 *--------------------------------------------------------------
 *                   u =&[t1+(t2-s1),s2] w
 *
 * Mapping to our implementation:
 *
 *   We have a special case for the "universal" stride scenario, which
 *   occurs when either when 't1' or 's1' == -1, i.e. <u,-1> or <v,-1>.
 *
 *   TODO: All cases below are effectively identical, fix
 *         comment and the code.
 *
 *   Case 1: t1 != -1, s1 != -1
 *
 *      for (t2 = s1; t2 <= s1+z1-4; t2++)
 *         pts-to(<u,t1+(t2-s1)>) U= pts-to(<v,t2>)
 *
 *   Case 2: t1 == -1, s1 != -1
 *
 *      sum = {};
 *      for (t2=s1; t2 <= s1+z1-4; t2++)
 *         pts-to(<u,-1)>) U= pts-to(<v,t2>)
 *         sum U= pts-to(<v,t2>)
 *      for (i=0; i < varsize(u); i++)
 *         if (exists(<u,i>))
 *            pts-to(<u,i>) U= sum;
 *
 *   Case 3: t1 != -1, s1 == -1
 *
 *      pts-to(<u,t1>) U= pts-to(<v,-1>);
 *
 *   Case 4: t1 == -1, s1 == -1
 *
 *      pts-to(<u,-1>) U= pts-to(<v,-1>);
 *      for (i=0; i < varsize(u); i++)
 *         if (exists(<u,i>))
 *            pts-to(<u,i>) U= pts-to(<v,-1>);
 *
 */

void
ConstraintGraphSolve::processAssign(const ConstraintGraphEdge *edge)
{
  // If the source of the copy edge has an unknown
  // points-to set, then we simply need to propagate
  // the unknown flag to the destination and we are
  // done. NOTE: Unfortunately we still need to propagate
  // the points-to sets across the edge to ensure we
  // handle unknown load/store edges correctly.
  if (edge->srcNode()->checkFlags(CG_NODE_FLAGS_UNKNOWN)) {
    if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG) &&
         !edge->destNode()->checkFlags(CG_NODE_FLAGS_UNKNOWN))
       fprintf(stderr,"processAssign: propagate unknown %d -> %d\n",
           edge->srcNode()->id(),edge->destNode()->id());
    edge->destNode()->addFlags(CG_NODE_FLAGS_UNKNOWN);
  }

  ConstraintGraphNode *src = edge->srcNode();
  ConstraintGraphNode *dst = edge->destNode();

  //if (src->checkFlags(CG_NODE_FLAGS_NOT_POINTER))
  //  fprintf(stderr,"SOLVE: Copy source %d is not a pointer!\n",src->id());
  //if (dst->checkFlags(CG_NODE_FLAGS_NOT_POINTER))
  //  fprintf(stderr,"SOLVE: Copy target %d is not a pointer!\n",dst->id());

  // Is this constraint context sensitive?
  bool cntxt = 
       !src->cg()->stInfo(src->cg_st_idx())->checkFlags(CG_ST_FLAGS_NOCNTXT);

  // If the copy edge is a copy from parent to child, we
  // call updateOffsets on the child, i.e. the dest
  if (edge->checkFlags(CG_EDGE_PARENT_COPY)) {
    for (PointsToIterator pti(src); pti != 0; ++pti) {
      CGEdgeQual srcQual = pti.qual();
      CGEdgeQual edgeQual = edge->edgeQual();
      CGEdgeQual dstQual = qualMap(ETYPE_COPY, srcQual, edgeQual, cntxt);
      updateOffsets(dst, *pti, dstQual);
      return;
    }
  }

  CGNodeId trackNodeId = 0;

  UINT32 assignSize = edge->size();
  StInfo *dstStInfo = dst->cg()->stInfo(dst->cg_st_idx());

  INT32 dstStOffset = dst->offset();
  INT32 srcStOffset = src->offset();
  INT32 curEndOffset = src->offset() + assignSize;

  CGEdgeQual edgeQual = edge->edgeQual();
  for ( PointsToIterator pti(src); pti != 0; ++pti ) {
    CGEdgeQual srcQual = pti.qual();
    CGEdgeQual dstQual = qualMap(ETYPE_COPY,srcQual,edgeQual,cntxt);
    if (dstQual != CQ_NONE) {
      bool change = false;
      change |= dst->unionDiffPointsTo(src->pointsTo(srcQual), dstQual);
      if (change) {
        ConstraintGraph::solverModList()->push(dst);
        updateOffsets(dst,src->pointsTo(srcQual),dstQual);
      }
    }
  }
}

/*
 * Here is the rule from Nystrom's implementation:
 *
 *   u <<[t1,k1,s1] v &&  v =&[t2,s2] w && t2=s1 && valid(w,s2+k1)
 *  --------------------------------------------------------------
 *                   u =&[t1,s2+k1] w
 *
 *  The semantics of valid(w,s2+k1) are such that it returns true if
 *  's2+k1' is less than the object's, 'w', allocated size.  For heap
 *  this is trivially true as the size will be unbounded.
 *
 *  foreach <w,s2> in pts-to(<v,s1>)
 *    if ( varSize(w) < s2+k1 )
 *      pts-to(<u,t1>) U= { <w,s2+k1> }
 *
 */

void
ConstraintGraphSolve::processSkew(const ConstraintGraphEdge *edge)
{
  ConstraintGraphNode *src = edge->srcNode();
  ConstraintGraphNode *dst = edge->destNode();

  // If the source of the skew edge has an unknown
  // points-to set, then we simply need to propagate
  // the unknown flag to the destination and we are
  // done. NOTE: Unfortunately we still need to propagate
  // the points-to sets across the edge to ensure we
  // handle unknown load/store edges correctly.
  if (src->checkFlags(CG_NODE_FLAGS_UNKNOWN))
  {
    if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG) &&
        !dst->checkFlags(CG_NODE_FLAGS_UNKNOWN))
      fprintf(stderr,"processSkew: propagate unknown %d -> %d\n",
          src->id(),dst->id());
    dst->addFlags(CG_NODE_FLAGS_UNKNOWN);
  }

  UINT32 skew = edge->skew();
  CGEdgeQual edgeQual = edge->edgeQual();
  bool change = false;
  bool cntxt = 
       !src->cg()->stInfo(src->cg_st_idx())->checkFlags(CG_ST_FLAGS_NOCNTXT);
  for ( PointsToIterator pti(src); pti != 0; ++pti ) {
    CGEdgeQual curQual = pti.qual();
    CGEdgeQual dstQual = qualMap(ETYPE_COPY/*COPY==SKEW*/,curQual,edgeQual,cntxt);
    if (dstQual != CQ_NONE) {
      PointsTo &srcPTS = *pti;
      PointsTo tmp;
      bool addedMinusOne = false;
      for (PointsTo::SparseBitSetIterator iter(&srcPTS,0); iter != 0; iter++)
      {
        CGNodeId nodeId = *iter;
        ConstraintGraphNode *node = ConstraintGraph::cgNode(nodeId);
        StInfo *st = node->stInfo();
        FmtAssert(!st->checkFlags(CG_ST_FLAGS_PREG),
                  ("processSkew: preg found in pts set of node: %d\n", 
                  node->id()));
        INT32 newOffset;
        if (node->offset() == -1)
          newOffset = -1;
        else {
          newOffset = node->offset() + skew;
          if (newOffset < 0) newOffset = -newOffset;
        }
        ConstraintGraphNode *skewNode = 
                       node->cg()->getCGNode(node->cg_st_idx(),newOffset);
        skewNode->addFlags(CG_NODE_FLAGS_ADDR_TAKEN);
        tmp.setBit(skewNode->id());
        if (newOffset != -1 && skewNode->offset() == -1)
          addedMinusOne = true;
      }
      if (addedMinusOne)
        ConstraintGraphNode::sanitizePointsTo(tmp,NULL,CQ_NONE);
      bool change = dst->unionDiffPointsTo(tmp,dstQual);
      if (change) {
        ConstraintGraph::solverModList()->push(dst);
        updateOffsets(dst,tmp,dstQual);
      }
      tmp.clear();
    }
  }
}

void
ConstraintGraphSolve::removeFieldSensitiveEdges(CGEdgeType etype,
                                                ConstraintGraphEdge *edge)
{
  // We have added an edge to (store) or from (load) <ST,-1>.  At this
  // point there may already be edges to (store) or from (load) <ST,ofst>,
  // which are now completely redundant and should be removed to
  // improve solution time.
  ConstraintGraphNode *minusOne = (etype == ETYPE_LOAD) ?
                                   edge->srcNode() : edge->destNode();

  CGEdgeSetIterator begin;
  CGEdgeSetIterator end;
  if (etype == ETYPE_LOAD) {
    begin = edge->destNode()->inCopySkewEdges().begin();
    end = edge->destNode()->inCopySkewEdges().end();
  }
  else {
    begin = edge->srcNode()->outCopySkewEdges().begin();
    end = edge->srcNode()->outCopySkewEdges().end();
  }

  // Finding the redundant edges via "hash" lookups may be more efficient,
  // but that requires additional work on the hashCGEdge, equalCGEdge and
  // all clients of ConstraintGraph::addEdge().
  for (CGEdgeSetIterator iter = begin; iter != end; ) {
    ConstraintGraphEdge *e = *iter;
    ++iter;  // We may remove the current edge
    ConstraintGraphNode *node = (etype == ETYPE_LOAD) ?
                                 e->srcNode() : e->destNode();
    if (node->offset() != -1 && node->cg_st_idx() == minusOne->cg_st_idx()) {
      ConstraintGraph::removeEdge(e);
    }
  }
}

void
ConstraintGraphSolve::addCopiesForLoadStore(ConstraintGraphNode *src,
                                            ConstraintGraphNode *dst,
                                            CGEdgeType etype,
                                            CGEdgeQual qual,
                                            UINT32 size,
                                            SparseBitSet<CGNodeId> &ptSet)
{
  FmtAssert(etype == ETYPE_LOAD || etype == ETYPE_STORE,
            ("Expected only adding copy edges for load/store constraints"));
  for (PointsTo::iterator iter(&ptSet,0); iter != 0; iter++)
  {
    CGNodeId nodeId = *iter;
    ConstraintGraphNode *node = ConstraintGraph::cgNode(nodeId);

    // The node we pull out of the points-to set may have been part of a
    // SCC and been merged with a representative node.  Here we make sure
    // that any edge we add targets only the representative
    ConstraintGraphNode *nodeRep = node->findRep();

    // Create the new assignment edge.  If it turns out the edge
    // does already exist nothing further is required.  Otherwise,
    // we add the new edge to the worklist.
    ConstraintGraphEdge *newEdge;
    bool added = false;
    ConstraintGraphNode *copySrc;
    ConstraintGraphNode *copyDst;
    if (etype == ETYPE_LOAD) {
      copySrc = nodeRep;
      copyDst = dst;
    }
    else {
      copySrc = src;
      copyDst = nodeRep;
    }

    // We don't add copy edges to "not-a-pointer"...
    if (copySrc->checkFlags(CG_NODE_FLAGS_NOT_POINTER) ||
        copyDst->checkFlags(CG_NODE_FLAGS_NOT_POINTER))
       continue;


#if 0
    // If we are adding an edge <src,ofst1> to <dst, ofst2> and the
    // following edge already exists:
    // (a) ETYPE_LOAD :  <src,-1>   to <dst,ofst2>
    // (b) ETYPE_STORE:  <src,ofst1> to <dst,-1>
    // The new edge is redundant.
    if (nodeRep->offset() != -1) {
      ConstraintGraphNode *minusOne =
          copySrc->cg()->checkCGNode(nodeRep->cg_st_idx(),-1);
      if (minusOne) {
        ConstraintGraphEdge cgEdge((etype == ETYPE_LOAD)?minusOne:copySrc,
                                   (etype == ETYPE_LOAD)?copyDst:minusOne,
                                   ETYPE_COPY,CQ_HZ,size);
        ConstraintGraphEdge *foundEdge = (etype == ETYPE_LOAD) ?
                                          copyDst->inEdge(&cgEdge) :
                                          copySrc->outEdge(&cgEdge);
        if (foundEdge) {
          fprintf(stderr,"Exist edge <%d,%d>:",
                  SYM_ST_IDX(minusOne->cg_st_idx()),minusOne->offset());
          foundEdge->print(stderr);
          fprintf(stderr," makes %s <%d,%d> redundant\n",
                  (etype == ETYPE_LOAD)?"from":"to",
                      SYM_ST_IDX(nodeRep->cg_st_idx()),nodeRep->offset());
          continue;
        }
      }
    }
#endif

    list<ConstraintGraphEdge *> newEdgeList;
    added = ConstraintGraph::addPtrAlignedEdges(copySrc, copyDst, ETYPE_COPY,
                                                qual, size, newEdgeList);
    if (added) {
#if 0
     if (nodeRep->offset() == -1) {
        for (CGEdgeSetIterator iter = newEdgeSet.begin();
             iter != newEdgeSet.end(); iter++) {
          ConstraintGraphEdge *newEdge = *iter;
          removeFieldSensitiveEdges(etype,newEdge);
        }
      }
#endif
      edgeDelta().add(newEdgeList);
    }
  }
}

void
ConstraintGraphSolve::processLoad(const ConstraintGraphEdge *edge)
{
  ConstraintGraphNode *src = edge->srcNode();
  ConstraintGraphNode *dst = edge->destNode();
  UINT32 sz = edge->size();
  CGEdgeQual edgeQual = edge->edgeQual();

  // If the source of the edge is unknown, that means it
  // may point to symbols that are not present in its
  // points to set, which means that we are not able to
  // add all necessary copy edges to the target of the load
  // This means we must mark the destination as unknown.
  // NOTE: if the target of the edge is unknown, no action
  // is necessary.
  if (src->checkFlags(CG_NODE_FLAGS_UNKNOWN)) {
    if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG) &&
         !dst->checkFlags(CG_NODE_FLAGS_UNKNOWN))
       fprintf(stderr,"processLoad: propagate unknown %d -> %d\n",
           src->id(),dst->id());
    dst->addFlags(CG_NODE_FLAGS_UNKNOWN);
  }

  bool cntxt = 
       !src->cg()->stInfo(src->cg_st_idx())->checkFlags(CG_ST_FLAGS_NOCNTXT);
  for ( PointsToIterator pti(src); pti != 0; ++pti ) {
     CGEdgeQual curQual = pti.qual();
     CGEdgeQual cpQual = qualMap(ETYPE_LOAD,curQual,edgeQual,cntxt);
     if (cpQual != CQ_NONE)
       addCopiesForLoadStore(src,dst,ETYPE_LOAD,cpQual,sz,*pti);
  }
}

bool
ConstraintGraphSolve::processStore(const ConstraintGraphEdge *edge)
{
  ConstraintGraphNode *src = edge->srcNode();
  ConstraintGraphNode *dst = edge->destNode();
  UINT32 sz = edge->size();
  CGEdgeQual edgeQual = edge->edgeQual();

  // If the target of the edge is unknown, that means the
  // node may point to symbols that are *not* present in
  // its points-to set.  Since we process a store edge
  // by adding copy edges from the source to each element
  // in the points-to set of the destination, we effectively
  // have missing copy edges.  The destination could point
  // to any symbol.  If the source of the edge is known,
  // we place the contents of its points-to set into
  // every "known" node in the graph (ouch).  If the source
  // of the edge is also unknown, we have a unknown pointer
  // writing an unknown value and we have no choice but to
  // punt.  We simply terminate the solver.
  if (dst->checkFlags(CG_NODE_FLAGS_UNKNOWN)) {
    // Here we dump a giant warning to help track down these issues
    fprintf(stderr,"=========================================\n");
    fprintf(stderr,"PU: %d processStore",Current_PU_Count());
    edge->print(stderr);
    fprintf(stderr,"\n");
    src->print(stderr);
    dst->print(stderr);
    fprintf(stderr,"=========================================\n");
    if (!src->checkFlags(CG_NODE_FLAGS_UNKNOWN)) {
      for (CGIdToNodeMapIterator iter = ConstraintGraph::gBegin();
           iter != ConstraintGraph::gEnd(); iter++) {
        ConstraintGraphNode *node = iter->second;
        if (!node->checkFlags(CG_NODE_FLAGS_UNKNOWN)) {
          bool change = false;
          for (PointsToIterator pti(src); pti != 0; ++pti)
            change |= node->unionDiffPointsTo(*pti,CQ_GBL);
          if (change)
            ConstraintGraph::solverModList()->push(node);
        }
      }
    }
    else
      return false;
  }

  bool cntxt = 
       !dst->cg()->stInfo(dst->cg_st_idx())->checkFlags(CG_ST_FLAGS_NOCNTXT);
  for ( PointsToIterator pti(dst); pti != 0; ++pti ) {
     CGEdgeQual curQual = pti.qual();
     CGEdgeQual cpQual = qualMap(ETYPE_STORE,curQual,edgeQual,cntxt);
     if (cpQual != CQ_NONE)
       addCopiesForLoadStore(src,dst,ETYPE_STORE,cpQual,sz,*pti);
  }
  return true;
}

CGEdgeQual
ConstraintGraphSolve::qualMap(CGEdgeType et,
                              CGEdgeQual aq,
                              CGEdgeQual eq,
                              bool cs)
{
  /* context sensitive */
  if (cs) {
    if (et == ETYPE_STORE)
      return (aq == CQ_DN) ? CQ_UP : aq;
    else if (et == ETYPE_LOAD)
      return aq;
    else if (et == ETYPE_COPY) {
      if (eq == CQ_HZ)
        return aq;
      else if (eq == CQ_DN || eq == CQ_GBL)
        return eq;
      else /* eq == CQ_UP */
        return (aq == CQ_GBL) ? aq : CQ_NONE;
    }
  }
  /* context insensitive */
  else {
    if (et == ETYPE_COPY)
      return (eq == CQ_DN ? CQ_DN : CQ_GBL);
    else
      return CQ_GBL;
  }
}

void
ConstraintGraph::simpleOptimizer()
{
  if (Get_Trace(TP_ALIAS,NYSTROM_CG_OPT_FLAG))
    fprintf(stderr, "Optimizing ConstraintGraphs...\n");
  // Iterate over all nodes in the graph
  // Simple optimizer, find nodes that have single outgoing/incoming
  // copy edge connecting the two nodes
  for (CGNodeToIdMapIterator iter = ConstraintGraph::lBegin();
       iter != ConstraintGraph::lEnd(); iter++) {
    ConstraintGraphNode *srcNode = iter->first->parent();
    // Ignore if there are outgoing load/store edges
    if (!srcNode->outLoadStoreEdges().empty())
      continue;
    // Single outgoing copy edge
    if (srcNode->outCopySkewEdges().size() != 1)
      continue;
    const CGEdgeSet &eset = srcNode->outCopySkewEdges();
    CGEdgeSetIterator eiter = eset.begin();
    ConstraintGraphEdge *edge = *eiter;
    // Handle only copy edges
    if (edge->edgeType() != ETYPE_COPY)
      continue;
    // Already merged?
    if (edge->checkFlags(CG_EDGE_PARENT_COPY))
      continue;
    ConstraintGraphNode *destNode = edge->destNode();
    FmtAssert(_toBeDeletedNodes.find(destNode->id()) == _toBeDeletedNodes.end(),
              ("Node already deleted"));
    // They should be from the same CG
    if (srcNode->cg() != destNode->cg())
      continue;
    // Ignore if there are incoming load/store edges
    if (!destNode->inLoadStoreEdges().empty())
      continue;
    // Single incoming copy edge
    if (destNode->inCopySkewEdges().size() != 1)
      continue;

    // src should be a node that will always
    // have only a single offset (for now that means just pregs)
    if (!srcNode->isOnlyOffset())
      continue;

    // We prefer the toBeMergedNode to be a preg that can be deleted
    // else we prefer atleast a preg so as that we we can avoid a PARENT_COPY
    // edge since we know that they will have only a single offset
    ConstraintGraphNode *parentNode;
    ConstraintGraphNode *toBeMergedNode;
    if (srcNode->stInfo()->checkFlags(CG_ST_FLAGS_PREG) &&
        srcNode->canBeDeleted()) {
      toBeMergedNode = srcNode;
      parentNode = destNode;
    } else if (destNode->stInfo()->checkFlags(CG_ST_FLAGS_PREG) &&
               destNode->canBeDeleted()) {
      toBeMergedNode = destNode;
      parentNode = srcNode;
    } else if (srcNode->stInfo()->checkFlags(CG_ST_FLAGS_PREG)) {
      toBeMergedNode = srcNode;
      parentNode = destNode;
    } else {
      toBeMergedNode = destNode;
      parentNode = srcNode;
    }

    // Perform the merge
    parentNode->merge(toBeMergedNode);
    toBeMergedNode->repParent(parentNode);
    if (parentNode->inKCycle() > 0)
      ConstraintGraph::adjustPointsToForKCycle(parentNode);

    if (Get_Trace(TP_ALIAS,NYSTROM_CG_OPT_FLAG))
      fprintf(stderr, "simpleOptimizer - Merging node %d with %d\n",
              toBeMergedNode->id(), parentNode->id());
    if (toBeMergedNode->stInfo()->checkFlags(CG_ST_FLAGS_PREG) &&
        toBeMergedNode->canBeDeleted())
      _toBeDeletedNodes.insert(toBeMergedNode->id());
  }

  hash_set<CGNodeId> areParents;
  // Adjust parents, so that nodes who have deleted nodes as their parents
  // point to the parent of the deleted nodes, which should not be marked 
  // deleted
  for (CGNodeToIdMapIterator iter = ConstraintGraph::lBegin();
       iter != ConstraintGraph::lEnd(); iter++) {
    ConstraintGraphNode *n = iter->first;
    n->findRep();
    FmtAssert(_toBeDeletedNodes.find(n->parent()->id()) == 
              _toBeDeletedNodes.end(), 
              ("Parent %d of node: %d marked deleted!\n", n->parent()->id(),
                                                          n->id()));
    // Record all nodes that are parents of another non-deleted node
    if (n->parent() != n && 
        _toBeDeletedNodes.find(n->id()) == _toBeDeletedNodes.end())
      areParents.insert(n->parent()->id());
  }

  // Iterate until there are no more deleted nodes
  bool change = true;
  while (change) {
    change = false;
    for (CGNodeToIdMapIterator iter = ConstraintGraph::lBegin();
         iter != ConstraintGraph::lEnd(); iter++) 
    {
      ConstraintGraphNode *node = iter->first->parent();
      // If the node is a parent of someone, ignore
      if (areParents.find(node->id()) != areParents.end())
        continue;
      // If destNode is marked deleted ignore
      if (_toBeDeletedNodes.find(node->id()) != _toBeDeletedNodes.end())
        continue;
      // If node is not a preg that can be deleted, ignore
      if (!(node->stInfo()->checkFlags(CG_ST_FLAGS_PREG) &&
            node->canBeDeleted()))
        continue;
      // Ignore if there are any outgoing edges
      if (!node->outLoadStoreEdges().empty() ||
          !node->outCopySkewEdges().empty())
        continue;
      const CGEdgeSet &loadStoreSet = node->inLoadStoreEdges();
      bool hasInStore = false;
      for (CGEdgeSetIterator eiter = loadStoreSet.begin();
           eiter != loadStoreSet.end(); eiter++) {
        ConstraintGraphEdge *edge = *eiter;
        if (edge->edgeType() == ETYPE_STORE) {
          hasInStore = true;
          break;
        }
      }
      // Ignore if there are incoming store edges
      if (hasInStore)
        continue;

      // Delete incoming edges
      CGEdgeSet deleteEdges;
      for (CGEdgeSetIterator eiter = loadStoreSet.begin();
           eiter != loadStoreSet.end(); eiter++) {
        ConstraintGraphEdge *edge = *eiter;
        deleteEdges.insert(edge);
      }
      const CGEdgeSet &copySkewSet = node->inCopySkewEdges();
      for (CGEdgeSetIterator eiter = copySkewSet.begin();
           eiter != copySkewSet.end(); eiter++) {
        ConstraintGraphEdge *edge = *eiter;
        deleteEdges.insert(edge);
      }
      for (CGEdgeSetIterator eiter = deleteEdges.begin();
           eiter != deleteEdges.end(); eiter++)
        removeEdge(*eiter);

      // Remove 'node' from the reverse points to set of all nodes in 'node's'
      // points to set
      for (PointsToIterator pti(node); pti != 0; ++pti) {
        PointsTo &pts = *pti;
        CGEdgeQual qual = pti.qual();
        for (PointsTo::SparseBitSetIterator sbsi(&pts,0); sbsi != 0; ++sbsi) {
          ConstraintGraphNode *ptdNode = ConstraintGraph::cgNode(*sbsi);
          ptdNode->removeRevPointsTo(node->id(), qual);
        }
      }

      // Verify that its reverse pts set is empty
      for (PointsToIterator pti(node,PtsRev); pti != 0; ++pti) {
        PointsTo &rpts = *pti;
        FmtAssert(rpts.isEmpty(), 
                  ("node: %d's rev pts not empty\n", node->id()));
      }

      node->deleteEdgesAndPtsSetList();
      
      // Mark the node for deletion and iterate
      _toBeDeletedNodes.insert(node->id());
      change = true;
    }
    // Update nodes that are parents
    areParents.clear();
    for (CGNodeToIdMapIterator iter = ConstraintGraph::lBegin();
         iter != ConstraintGraph::lEnd(); iter++) 
    {
      ConstraintGraphNode *n = iter->first;
      // Record all nodes that are parents of another non-deleted node
      if (n->parent() != n && 
          _toBeDeletedNodes.find(n->id()) == _toBeDeletedNodes.end())
        areParents.insert(n->parent()->id());
    }
  }

  if (Get_Trace(TP_ALIAS,NYSTROM_CG_OPT_FLAG))
    fprintf(stderr, "Done optimizing ConstraintGraphs\n");
}

#define MAX_ALLOWED_ST_PER_TYPE 32

void
ConstraintGraph::ipaSimpleOptimizer()
{
  if (Get_Trace(TP_ALIAS,NYSTROM_CG_OPT_FLAG))
    fprintf(stderr, "IPA optimizing ConstraintGraphs...\n");

  CGStInfoMap allStInfos;
  for (CGIdToNodeMapIterator iter = gBegin(); iter != gEnd(); iter++) 
  {
    ConstraintGraphNode *node = iter->second;
    if (node->stInfo()->checkFlags(CG_ST_FLAGS_PREG))
      continue;
    if (allStInfos.find(node->cg_st_idx()) == allStInfos.end()) 
      allStInfos[node->cg_st_idx()] = node->stInfo();
  }
    
  hash_map<TY_IDX, UINT32> stInfoCount;
  hash_map<TY_IDX, StInfo *> repStofType;

  for (CGStInfoMapIterator iter = allStInfos.begin();
       iter != allStInfos.end(); iter++) 
  {
    StInfo *stInfo = iter->second;
    TY_IDX ty_idx = stInfo->ty_idx();
    TY &ty = Ty_Table[ty_idx];
    if (TY_kind(ty) == KIND_STRUCT || TY_kind(ty) == KIND_ARRAY)
    {
      if (stInfoCount[ty_idx] > MAX_ALLOWED_ST_PER_TYPE) {
        // Collapse the StInfo to create a single node for this StInfo
        fprintf(stderr, "Count: %d exceeded threshold for st cg_st_idx: %llu "
                "..collapsing\n", stInfoCount[ty_idx], iter->first);
        stInfo->print(stderr);
        stInfo->collapse();
        if (repStofType.find(ty_idx) == repStofType.end()) {
          // Mark this collapsed StInfo as the representative
          repStofType[ty_idx] = stInfo;
          fprintf(stderr, "Identifying rep:\n");
          stInfo->print(stderr);
          FmtAssert(stInfo->firstOffset()->nextOffset() == NULL,
                    ("Only single offset expected"));
        }
        else {
          StInfo *repStInfo = repStofType.find(ty_idx)->second;
          FmtAssert(stInfo->firstOffset()->nextOffset() == NULL,
                    ("Only single offset expected"));
          ConstraintGraphNode *repNode = repStInfo->firstOffset();
          ConstraintGraphNode *node = stInfo->firstOffset();
          // Collapse the firstOffset with the rep's firstOffset
          repNode->collapse(node);
          fprintf(stderr, "Collapsing node: %d with %d\n", node->id(),
                  repNode->id());
        }
      } else
        stInfoCount[ty_idx]++;
    }
  }

  if (Get_Trace(TP_ALIAS,NYSTROM_CG_OPT_FLAG))
    fprintf(stderr, "Done IPA optimizing ConstraintGraphs\n");
}
