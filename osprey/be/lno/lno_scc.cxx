/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


//-*-c++-*-

/**
*** Module: lno_scc.cxx
*** $Revision$
*** $Date$
*** $Author$
*** $Source$
*** 
*** Revision history:
*** 
***     10-NOV-94 dkchen - Original Version
*** 
*** Description:
*** 
*** This file contains definitions for fuctions in the SCC_DIRECTED_GRAPH16.
*** It is derived from DIRECTED_GRAPH16 described in "graph_template.h".
*** The set of vertices and edges are implemented with dynamic arrays.
*** The index 0 to these array is reserved as NULL pointer, which is used
*** to represent the end of the free vertex/edge list as well as the end
*** of the in/out edge-lists for a vertex. Therefore, when we have M
*** vertices, the _v array has M+1 elements.
***
*** Besides, we require that initial vertices and edges counts be given
*** to the constructor.
*** 
**/

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "cxx_graph.h"
#include "cxx_memory.h"
#include "errors.h"
#include "lno_scc.h"

extern MEM_POOL LNO_local_pool;

SCC_DIRECTED_GRAPH16::
SCC_DIRECTED_GRAPH16(const VINDEX16 vsize, const EINDEX16 esize)
	:DIRECTED_GRAPH16<EDGE16,VERTEX16>(vsize, esize){


  _sccmpool = CXX_NEW(MEM_POOL,Malloc_Mem_Pool);
  MEM_POOL_Initialize(_sccmpool,"sccmpool",FALSE);
  MEM_POOL_Push(_sccmpool);

  _scc_id.Set_Mem_Pool(_sccmpool);
  _scc_id.Alloc_array(vsize+1);
  _scc_id.Setidx( vsize );
  Invalidate_Scc();
}


VINDEX16
SCC_DIRECTED_GRAPH16::Add_Vertex() {

  Invalidate_Scc();
  return DIRECTED_GRAPH16<EDGE16,VERTEX16>::Add_Vertex();

}

EINDEX16
SCC_DIRECTED_GRAPH16::Add_Edge(VINDEX16 from, VINDEX16 to) {

  // By conservative assumption, adding an edge between vertices from
  // different SCC changes the SCC structure.
  if (Scc_Is_Valid() && _scc_id[from] != _scc_id[to])
    Invalidate_Scc();

  return DIRECTED_GRAPH16<EDGE16,VERTEX16>::Add_Edge(from,to);
}

EINDEX16
SCC_DIRECTED_GRAPH16::Add_Unique_Edge(VINDEX16 from, VINDEX16 to) {
  EINDEX16 new_edge;

  // By conservative assumption, adding an edge between vertices from
  // different SCC changes the SCC structure.
  if (Scc_Is_Valid() && _scc_id[from] != _scc_id[to])
    Invalidate_Scc();

  return DIRECTED_GRAPH16<EDGE16,VERTEX16>::Add_Unique_Edge(from,to);

}

void
SCC_DIRECTED_GRAPH16::Delete_Vertex(VINDEX16 v) {

  Invalidate_Scc();
  DIRECTED_GRAPH16<EDGE16,VERTEX16>::Delete_Vertex(v);

}

void
SCC_DIRECTED_GRAPH16::Delete_Edge(EINDEX16 e) {
  
  VINDEX16 from, to;

  // see if an edge already exists
  FmtAssert (Edge_Is_In_Graph(e), ("Edge not in graph\n"));

  from = _e[e].Get_Source();
  to = _e[e].Get_Sink();

  // By conservative assumption, deleting an edge between vertices from
  // the same SCC changes the SCC structure.
  if (Scc_Is_Valid() && _scc_id[from] == _scc_id[to])
    Invalidate_Scc();

  DIRECTED_GRAPH16<EDGE16,VERTEX16>::Delete_Edge(e);

}

SCC_DIRECTED_GRAPH16&
SCC_DIRECTED_GRAPH16::operator=(const SCC_DIRECTED_GRAPH16& g) {

  // copy everything except temp array and mpool

  DIRECTED_GRAPH16<EDGE16,VERTEX16>::operator=(g);

  _scc_cnt = g._scc_cnt;
  _scc_id = g._scc_id;

  return *this;
}

/**
*** The (Tarjan's) SCC identification algorithm is from Zima's book pp. 341 
**/

void
SCC_DIRECTED_GRAPH16::Scc_Dfs(VINDEX16 n) {

  EINDEX16 e=_v[n].Get_Out_Edge();
  VINDEX16 j;

  _visited[n] = 1;
  _dfn[n] = _df_count;
  _link[n] = _df_count++;
  _dfs_stack->Push(n);
  _in_stack[n] = 1;
  while (e) {
    j = _e[e].Get_Sink();
    if (!_visited[j]) {
	Scc_Dfs(j);
	if (_link[j] < _link[n]) _link[n] = _link[j];
    } else if (_dfn[j] < _dfn[n] && _in_stack[j] )
	if (_dfn[j] < _link[n]) _link[n] = _dfn[j];
    e = _e[e].Get_Next_Out_Edge();
  }
  if (_link[n] == _dfn[n]) {
    do {
      j = _dfs_stack->Top();
      _dfs_stack->Pop();
      _in_stack[j] = 0;
      _scc_id[j] = _scc_cnt;
    } while (j != n);
    _scc_cnt++;
  }
}


void
SCC_DIRECTED_GRAPH16::Find_Scc() {

  VINDEX16 i;
  VINDEX16 vcnt = Get_Vertex_Count();

  if (Scc_Is_Valid()) return;

  MEM_POOL_Push(&LNO_local_pool);

  _link = CXX_NEW_ARRAY(VINDEX16,vcnt+1,&LNO_local_pool);
  _dfn = CXX_NEW_ARRAY(VINDEX16,vcnt+1,&LNO_local_pool);
  _visited = CXX_NEW_ARRAY(BOOL,vcnt+1,&LNO_local_pool);
  _in_stack = CXX_NEW_ARRAY(BOOL,vcnt+1,&LNO_local_pool);
  _dfs_stack = CXX_NEW(STACK<VINDEX16>(&LNO_local_pool), &LNO_local_pool);

  if (_scc_id.Sizeof() < vcnt+1) {
    _scc_id.Realloc_array(vcnt+1);
  }
  _scc_id.Setidx(vcnt) ;

  _df_count=1; 
  _scc_cnt=1;
  for (i=1; i<vcnt+1; i++) _visited[i]=_in_stack[i]=0;
  for (i=1; i<vcnt+1; i++) if (!_visited[i]) Scc_Dfs(i);
  _scc_cnt--;

  MEM_POOL_Pop(&LNO_local_pool);
}

VINDEX16
SCC_DIRECTED_GRAPH16::Get_Scc_Size(VINDEX16 i) {

  VINDEX16 count=0;
  VINDEX16 vcnt = Get_Vertex_Count();

  if ( ! Scc_Is_Valid() ) Find_Scc();

  for (VINDEX16 j=1; j<vcnt+1; j++) if ( _scc_id[j] == i ) count++;
  return count;
}

mUINT16
SCC_DIRECTED_GRAPH16::Get_Level(mUINT16 level[]) {

  VINDEX16  *queue;
  VINDEX16  head,i;
  mUINT16  max_level;
  VINDEX16 vcnt = Get_Vertex_Count();

  if ( ! Scc_Is_Valid() ) Find_Scc();
  FmtAssert( Get_Scc_Count() == Get_Vertex_Count(),
    ("Directed graph with cycle passed to Get_Level()\n"));

  MEM_POOL_Push(&LNO_local_pool);

  // allocate new graph
  DIRECTED_GRAPH16<EDGE16,VERTEX16> g(Get_Vertex_Count(), Get_Edge_Count());  
  g = *this;  // copy the current graph

  queue = CXX_NEW_ARRAY(VINDEX16,Get_Vertex_Count(),&LNO_local_pool);
  head = 0;

  for (i = 1; i<vcnt+1; i++) {
    
    if (!_v[i].Get_In_Edge()) { // find vertex without predecessors
      queue[head++] = i;	// enque
      level[i] = 0;		// assign it level 0
      
    } else {
      
      FmtAssert(!Get_Edge(i,i),
        ("Directed graph with self-cycle passed to Get_Level()\n"));
    }
  }

  max_level = 0;
  for (i = 0; i <head; i++) {
    VINDEX16 j = queue[i];	// process queued vertex
    EINDEX16 e;
    e = g.Get_Out_Edge(j);
    while (e) {			// remove all of its out edges
      EINDEX16 e1;
      e1 = e;
      e = g.Get_Next_Out_Edge(e);
      VINDEX16 sink = g.Get_Sink(e1);
      g.Delete_Edge(e1);
      if (!g.Get_In_Edge(sink)) { // find vertex without predecessors
	queue[head++] = sink;	  // enque
	level[sink] = level[j]+1; // assign it one level deeper than
				  // the current vertex j
	if (level[sink]>max_level) max_level = level[sink];
				  // update the max_level info
      }
    }
  }

  MEM_POOL_Pop(&LNO_local_pool);

  return max_level;

}

mUINT16
SCC_DIRECTED_GRAPH16::Level_Sort(VINDEX16 queue[]) {

  VINDEX16  head,i;
  mUINT16  max_level;
  VINDEX16 vcnt = Get_Vertex_Count();

  if ( ! Scc_Is_Valid() ) Find_Scc();
  FmtAssert( Get_Scc_Count() == Get_Vertex_Count(),
    ("Directed graph with cycle passed to Level_Sort()\n"));

  MEM_POOL_Push(&LNO_local_pool);

  // allocate new graph
  DIRECTED_GRAPH16<EDGE16,VERTEX16> g(Get_Vertex_Count(), Get_Edge_Count());  
  g = *this;  // copy the current graph

  mUINT16* level = CXX_NEW_ARRAY(mUINT16,Get_Vertex_Count()+1,&LNO_local_pool);
  head = 0;

  for (i = 1; i<vcnt+1; i++) {
    
    if (!_v[i].Get_In_Edge()) { // find vertex without predecessors
      queue[head++] = i;	// enque
      level[i] = 0;		// assign it level 0
      
    } else {
      
      FmtAssert(!Get_Edge(i,i),
        ("Directed graph with self-cycle passed to Level_Sort()\n"));
    }
  }

  max_level = 0;
  for (i = 0; i <head; i++) {
    VINDEX16 j = queue[i];	// process queued vertex
    EINDEX16 e;
    e = g.Get_Out_Edge(j);
    while (e) {			// remove all of its out edges
      EINDEX16 e1;
      e1 = e;
      e = g.Get_Next_Out_Edge(e);
      VINDEX16 sink = g.Get_Sink(e1);
      g.Delete_Edge(e1);
      if (!g.Get_In_Edge(sink)) { // find vertex without predecessors
	queue[head++] = sink;	  // enque
	level[sink] = level[j]+1; // assign it one level deeper than
				  // the current vertex j
	if (level[sink]>max_level) max_level = level[sink];
				  // update the max_level info
      }
    }
  }

  MEM_POOL_Pop(&LNO_local_pool);

  return max_level;

}

SCC_DIRECTED_GRAPH16*
SCC_DIRECTED_GRAPH16::Acyclic_Condensation(MEM_POOL *mpool) {

  SCC_DIRECTED_GRAPH16 *g;
  VINDEX16 vcnt = Get_Vertex_Count();
  VINDEX16 i;
  VINDEX16 j;
  VINDEX16 k;

  if ( ! Scc_Is_Valid() ) Find_Scc();

  // allocate new graph
  g= CXX_NEW( SCC_DIRECTED_GRAPH16(Get_Scc_Count(), 0), mpool );

  k=Get_Scc_Count();
  for (i=1; i<=k; i++) {
    j = g->Add_Vertex();
    FmtAssert(i == j,
      ("SCC id (%d) does not match VINDEX (%d)\n", i, j));
   }

  EINDEX16 e = Get_Edge();
  while (e) {
    
    if (_scc_id[Get_Source(e)]!=_scc_id[Get_Sink(e)])
      g->Add_Unique_Edge(_scc_id[Get_Source(e)],_scc_id[Get_Sink(e)]);
    e = Get_Next_Edge(e);

  }
  return g;
}


