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
#ifndef CONSTRAINT_GRAPH_SOLVE_H_
#define CONSTRAINT_GRAPH_SOLVE_H_

#include "constraint_graph.h"

class EdgeDelta {
public:
  EdgeDelta() {}
  ~EdgeDelta() {}

  void add(ConstraintGraphEdge *e);
  void add(list<ConstraintGraphEdge *> &edgeList);
  EdgeWorkList &copySkewList() { return _copySkew; }
  EdgeWorkList &loadStoreList() { return _loadStore; }

  bool empty() const { return _copySkew.empty() &&
                              _loadStore.empty(); }

  void find(CGNodeId src, CGNodeId dst);
  void findPtr(ConstraintGraphEdge *e);

private:
  EdgeWorkList _copySkew;
  EdgeWorkList _loadStore;
};

class ConstraintGraphSolve {
public:
  ConstraintGraphSolve(EdgeDelta &delta,
                       ConstraintGraph *cg,
                       MEM_POOL *memPool,
                       ConstraintGraph *gcg=NULL)
  : _cg(cg),
    _globalCG(gcg==NULL?cg:gcg),
    _memPool(memPool),
    _edgeDelta(delta)
    {}

  ~ConstraintGraphSolve() {}

  bool solveConstraints(UINT32 noMergeMask = 0);

  void cycleDetection(UINT32 noMergeMask = 0);

  static void postProcessPointsTo();

  static void Exclude(PointsTo &src, CGEdgeType et, CGEdgeQual aq, 
                        CGEdgeQual eq, bool cs, PointsTo &orig);

  // Edge qualifier matrix mapping
  static CGEdgeQual qualMap(CGEdgeType et,CGEdgeQual aq,CGEdgeQual eq, bool cs);

  static void printStats();

private:
  // Solver statistics
  static double totalTime;
  static UINT32 totalIterCount;
  static UINT32 totalCopyCount;
  static UINT32 totalSkewCount;
  static UINT32 totalLoadCount;
  static UINT32 totalStoreCount;

  void updateOffsets(const ConstraintGraphNode *, const PointsTo &, CGEdgeQual);
  void processAssign(const ConstraintGraphEdge *);
  void processSkew(const ConstraintGraphEdge *);
  void processLoad(const ConstraintGraphEdge *);
  bool processStore(const ConstraintGraphEdge *);
  void addCopiesForLoadStore(ConstraintGraphNode *src,
                             ConstraintGraphNode *dst,
                             CGEdgeType etype,
                             CGEdgeQual qual,
                             UINT32 size,
                             SparseBitSet<CGNodeId> &ptSet);
  void removeFieldSensitiveEdges(CGEdgeType etype,ConstraintGraphEdge *edge);

  EdgeDelta &edgeDelta() { return _edgeDelta; }

  ConstraintGraph *_cg;
  ConstraintGraph *_globalCG;
  MEM_POOL        *_memPool;
  EdgeDelta       &_edgeDelta;
};



#endif /* CONSTRAINT_GRAPH_SOLVE_H_ */
