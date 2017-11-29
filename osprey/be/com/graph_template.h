/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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
//
/**
*** Module: graph_template.h
*** $Revision: 1.8 $
*** $Date: 05/12/05 08:59:13-08:00 $
*** $Author: bos@eng-24.pathscale.com $
*** $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.graph_template.h $
*** 
*** Revision history:
***
***     10-NOV-94 dkchen - Original Version
***
*** Description:
*** 
***     This interface describes a directed graph class. It can be used to
***     represent a simple directed graph which has no multiple edges for
***     a given pair of source and sink vertices. The size of the graph
***     changes automatically as the number of vertices and edges can grow
***     increases. Note that with the current implementation,
***     the number of vertices/edges cannot exceed 2**16 - 2.
***     The descriptions on type references here, such as EINDEX16,
***     VINDEX16, EDGE, and VERTEX can be found in "cxx_graph.h".
***
*** WARNING: DIRECTED_GRAPH16 does not invoke constructors
***          for EDGE_TYPE and VERTEX_TYPE as new objects are allocated.
***          The class user must explicitly provide any required
***          initialization for new edges and vertices.
*** 
*** Exported Types and Functions:
***
***     DIRECTED_GRAPH16
*** 
*** 	The directed graph type. Note that the vertices have index
***     values from 1 to Get_Vertex_Count() and the edges have index
***     value from 1 to Get_Edge_Count().
*** 
***   		DIRECTED_GRAPH16(const VINDEX16 vsize, const EINDEX16 esize)
*** 
*** 		Construct a directed graph with 'vsize' vertices and
***             'esize' edges.
*** 
***   		~DIRECTED_GRAPH16()
*** 
*** 		Destruct a directed graph.
*** 
***         VINDEX16	Add_Vertex()
*** 
*** 	        Allocate a new vertex in the graph. Return (VINDEX) 0
***             if the number of vertices exceed 2**16 - 2.
*** 
***         EINDEX16	Add_Edge(VINDEX16 from, VINDEX16 to)
*** 
*** 		Add a new edge given the 'from' (source) and 'to' (sink)
*** 		vertices. Return (VINDEX) 0 if the number of vertices
***             exceed 2**16 - 2. No check is done for duplicated edges
***             between the same pair of source and sink vertices.
*** 
***         EINDEX16	Add_Unique_Edge(VINDEX16 from, VINDEX16 to)
*** 
*** 		Add a new edge given the 'from' (source) and 'to' (sink)
*** 		vertices. Return (VINDEX) 0 if the number of vertices
***             exceed 2**16 - 2. Check and return an existing edge.
*** 
***         EINDEX16	Get_Edge(VINDEX16 from, VINDEX16 to) const
*** 
*** 		Get the edge between 'from' and 'to' vertices.
***             Return 0 if there is no such edge.
*** 
***         void	Delete_Vertex(VINDEX16 v)
*** 
*** 		Delete the vertex 'v' and corresponding edges.
*** 
***         void	Delete_Edge(EINDEX16 e)
*** 
*** 		Delete the edge 'e'.
*** 
***         VINDEX16	Get_Vertex() const
*** 
*** 		Get the first vertex of a directed graph.
*** 
***         VINDEX16	Get_Next_Vertex(VINDEX16 v) const
*** 
*** 		Get the next vertex after 'v' of a directed graph.
*** 
*** 		Therefore, to iterate through all vertices of a
*** 		graph 'g', we can do the following:
*** 
*** 		v = g.Get_Vertex();
*** 		while (v) {
*** 		  ...
***               Assuming that the graph has not been changed
***
*** 		  v = g.Get_Next_Vertex(v);
*** 		}
*** 
***         EINDEX16	Get_Edge() const
*** 
*** 		Get the first edge of a directed graph.
*** 
***         EINDEX16	Get_Next_Edge(EINDEX16 v) const
*** 
*** 		Get the next edge after 'e' of a directed graph.
*** 
*** 		Therefore, to iterate through all edges of a
*** 		graph 'g', we can do the following:
*** 
*** 		e = g.Get_Edge();
*** 		while (e) {
*** 		  ...
***
***               Assuming that the graph has not been changed
***
*** 		  e = g.Get_Next_Edge(e);
*** 		}
*** 
***         VINDEX16 	Get_Source(EINDEX16 e) const
***         VINDEX16 	Get_Sink(EINDEX16 e) const
*** 
*** 
***         EINDEX16 	Get_In_Edge(VINDEX16 v) const
***         EINDEX16 	Get_Out_Edge(VINDEX16 v) const
*** 
*** 		Get the first in/out edge of vertex 'v'.
*** 
***         EINDEX16 	Get_Next_In_Edge(EINDEX16 e) const
*** 
*** 		Get the next in edge after 'e' for the sink of 'e'.
***
***         EINDEX16 	Get_Next_Out_Edge(EINDEX16 e) const
*** 
*** 		Get the next out edge after 'e' for the source of 'e'.
*** 
*** 		Therefore, to iterate through all out-edges of a
*** 		vertex 'v' in graph 'g', we can do the following:
*** 
*** 		e = g.Get_Out_Edge(v);
*** 		while (e) {
*** 		  ...
***
***               Assuming that the graph has not been changed
***
*** 		  e = g.Get_Next_Out_Edge(e);
*** 		}
***
***         BOOL	Vertex_Is_In_Graph(VINDEX16 v) const
***         BOOL	Edge_Is_In_Graph(EINDEX16 e) const
*** 
*** 		Membership functions.
*** 
***         VINDEX16	Get_Vertex_Count() const
***         EINDEX16	Get_Edge_Count() const
*** 
*** 		Get total number of vertices and edges.
*** 
***         BOOL	Is_Empty() const
*** 
*** 		Is true when there is no vertex.
***
***	    VOID Erase_Graph()
***
***		Erase the graph
*** 
***         DIRECTED_GRAPH16& operator=(DIRECTED_GRAPH16& g)
*** 
*** 		Graph assignment operation. The vertices, edges and the
*** 		size are copied.
*** 
***         void Print(FILE* file) const
***
***             Print the graph info into file 'file'. For each vertex 'v',
***             one line in the following format is printed:
***             ( e_in_1, .. , e_in_m) v ( e_out_1, .. , e_out_m)
***             where e_in_* are in edges of 'v' and e_out_* are out edges.
*** 
**/

#ifndef graph_template_INCLUDED
#define graph_template_INCLUDED "graph_template.h"

#ifdef _KEEP_RCS_ID
static char *graph_template_rcs_id = graph_template_INCLUDED "$Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */

#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef mempool_INCLUDED
#include "mempool.h"
#endif
#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif
#ifndef cxx_graph_INCLUDED
#include "cxx_graph.h"
#endif
#include "cxx_memory.h"

typedef mUINT16 VINDEX16;
typedef mUINT16 EINDEX16;

template <class EDGE_TYPE, class VERTEX_TYPE>
class DIRECTED_GRAPH16 {
private:
  MEM_POOL	*_vmpool;	// pool used for vertex allocation
  MEM_POOL	*_empool;	// pool used for edge allocation
  VINDEX16	_vfree;		// the first vertex in the free vertex list
  EINDEX16	_efree;		// the first edge in the free edge list

		DIRECTED_GRAPH16(const DIRECTED_GRAPH16&);
protected:
  DYN_ARRAY<VERTEX_TYPE> _v;		// dynamic array for vertices
  VINDEX16	_vcnt;		// how many nodes there are in the graph
  DYN_ARRAY<EDGE_TYPE> _e;		// dynamic array for edges
  EINDEX16	_ecnt;		// how many edges there are in the graph

public:
  		DIRECTED_GRAPH16(const VINDEX16 vsize, const EINDEX16 evsize);
  		~DIRECTED_GRAPH16()		{ 
    			_v.Free_array();
			_e.Free_array();
			MEM_POOL_Pop(_vmpool);
			MEM_POOL_Delete(_vmpool);
			CXX_DELETE(_vmpool,Malloc_Mem_Pool);
			MEM_POOL_Pop(_empool);
			MEM_POOL_Delete(_empool);
			CXX_DELETE(_empool,Malloc_Mem_Pool);
			}

  DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>& 
		operator=(const DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>& g);

  void	Erase_Graph() { 
   	 _v.Setidx(0); _vcnt = 0; _vfree = 0; _e.Setidx(0); _ecnt = 0;
         _efree = 0; }

  VINDEX16	Add_Vertex();
  EINDEX16	Add_Edge(VINDEX16 from, VINDEX16 to);
  EINDEX16	Add_Unique_Edge(VINDEX16 from, VINDEX16 to);

  EINDEX16	Get_Edge(VINDEX16 from, VINDEX16 to) const;
  void		Delete_Vertex(VINDEX16 v);
  void		Delete_Edge(EINDEX16 e);

  VINDEX16	Get_Vertex() const;
  VINDEX16	Get_Next_Vertex(VINDEX16 v) const;
  VINDEX16	Get_Next_Vertex_In_Edit(VINDEX16 v) const;

  EINDEX16	Get_Edge() const;
  EINDEX16	Get_Next_Edge(EINDEX16 e) const;

  BOOL		Vertex_Is_In_Graph(VINDEX16 v) const	{
  		  return ((v <=_v.Lastidx()) && v>0 && !_v[v].Is_Free()); }
  BOOL		Edge_Is_In_Graph(EINDEX16 e) const	{
  		  return ((e <=_e.Lastidx()) && e>0 && !_e[e].Is_Free()); }

  VINDEX16 	Get_Source(EINDEX16 e) const 	{ 
		  Is_True (Edge_Is_In_Graph(e), ("Edge not in graph\n"));
		  return _e[e].Get_Source(); }
  VINDEX16 	Get_Sink(EINDEX16 e) const 	{ 
		  Is_True (Edge_Is_In_Graph(e), ("Edge not in graph\n"));
		  return _e[e].Get_Sink(); }

  EINDEX16 	Get_In_Edge(VINDEX16 v) const 	{ 
                  if (!Vertex_Is_In_Graph(v))
                      FmtAssert(FALSE, (" "));
		  Is_True (Vertex_Is_In_Graph(v), ("Vertex not in graph\n"));
		  return _v[v].Get_In_Edge(); }
  EINDEX16 	Get_Out_Edge(VINDEX16 v) const 	{ 
		  Is_True (Vertex_Is_In_Graph(v), ("Vertex not in graph\n"));
		  return _v[v].Get_Out_Edge(); }

  EINDEX16 	Get_Next_In_Edge(EINDEX16 e) const
					{ return _e[e].Get_Next_In_Edge(); }
  EINDEX16 	Get_Next_Out_Edge(EINDEX16 e) const
					{ return _e[e].Get_Next_Out_Edge(); }

  VINDEX16	Get_Vertex_Count() const	{ return _vcnt; }
  EINDEX16	Get_Edge_Count() const		{ return _ecnt; }

  BOOL		Is_Empty() const		{ return _vcnt == 0; }

  void		Print(FILE *file) const;
};

// Implementation stuff from graph_template.cxx, since g++ (rightly)
// doesn't do implicit .cxx file inclusion.
template <class EDGE_TYPE, class VERTEX_TYPE>
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::
DIRECTED_GRAPH16( const VINDEX16 vsize, const EINDEX16 esize) {

  //Is_True( vsize >=0, ("Illegal graph size\n"));

  _vmpool = CXX_NEW(MEM_POOL,Malloc_Mem_Pool);
  MEM_POOL_Initialize(_vmpool,"vmpool",FALSE);
  MEM_POOL_Push(_vmpool);
  _v.Set_Mem_Pool(_vmpool);
  _v.Alloc_array(vsize+1);
  _v.Setidx( 0 );
  _vcnt = 0;
  _vfree = 0;

  //for (mINT16 i=1; i<=_vcnt; i++) {	// initialization
  //  _v[i].Set_Out_Edge(0);
  //  _v[i].Set_In_Edge(0);
  //}

  _empool = CXX_NEW(MEM_POOL,Malloc_Mem_Pool);
  MEM_POOL_Initialize(_empool,"empool",FALSE);
  MEM_POOL_Push(_empool);
  _e.Set_Mem_Pool(_empool);
  _e.Alloc_array(esize+1);
  _e.Setidx( 0 );
  _ecnt = 0;
  _efree = 0;

}

template <class EDGE_TYPE, class VERTEX_TYPE>
VINDEX16
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Add_Vertex() {
  VINDEX16 new_vertex;

  // Is_True(_vcnt < GRAPH16_CAPACITY, ("Too many vertices\n"));
#ifdef KEY // bug 4024
  Is_True(_vcnt < GRAPH16_CAPACITY, ("Too many vertices\n"));
#endif
  if (_vcnt == GRAPH16_CAPACITY) return 0;

  if (_vfree == 0) { // grow the _v[] to accept more vertices
    new_vertex = _v.Newidx();
  } else {
    new_vertex = _vfree;
    _vfree = _v[_vfree].Get_Next_Free_Vertex();
  }

  // reset the in/out edge-lists to NULL
  _v[new_vertex].Set_Out_Edge(0);
  _v[new_vertex].Set_In_Edge(0);

  _vcnt++;

  return new_vertex;

}

template <class EDGE_TYPE, class VERTEX_TYPE>
EINDEX16
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Add_Edge(VINDEX16 from, VINDEX16 to) {
  EINDEX16 new_edge;

  // Is_True(_ecnt < GRAPH16_CAPACITY, ("Too many edges\n"));
#ifdef KEY // bug 4024
#ifdef Is_True_On
  if (_ecnt == GRAPH16_CAPACITY)
    DevWarn("Too many edges\n");
#endif
#endif
  if (_ecnt == GRAPH16_CAPACITY) return 0;
  if (_efree == 0) { // grow the _e[] to accept more edges
    new_edge = _e.Newidx();
  } else {
    new_edge = _efree;
    _efree = _e[_efree].Get_Next_Free_Edge();
  }
  
  _e[new_edge].Set_Source(from);
  _e[new_edge].Set_Sink(to);

  _ecnt++;

  // insert this edge into the out-edge list of the source vertex 'from'
  _e[new_edge].Set_Next_Out_Edge(_v[from].Get_Out_Edge());
  _v[from].Set_Out_Edge(new_edge);

  // insert this edge into the in-edge list of the sink vertex 'to'
  _e[new_edge].Set_Next_In_Edge(_v[to].Get_In_Edge());
  _v[to].Set_In_Edge(new_edge);

  return new_edge;

}

template <class EDGE_TYPE, class VERTEX_TYPE>
EINDEX16
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Add_Unique_Edge(VINDEX16 from, VINDEX16 to) {
  EINDEX16 new_edge;

  // see if an edge already exists. No multiple edges between the same
  // pair of source and sink vertices.
  if (new_edge=Get_Edge(from,to)) return new_edge;

  return Add_Edge(from,to);

}

template <class EDGE_TYPE, class VERTEX_TYPE>
EINDEX16
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Get_Edge(VINDEX16 from, VINDEX16 to) const {
  EINDEX16 e;
  e = _v[from].Get_Out_Edge();
  while (e != 0) {
    if (_e[e].Get_Sink() == to) return e;
    e = _e[e].Get_Next_Out_Edge();
  }
  return 0; // no corresponding edge
}

template <class EDGE_TYPE, class VERTEX_TYPE>
void
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Delete_Vertex(VINDEX16 v) {

  EINDEX16 e;

  // see if the vertex exists
  Is_True (Vertex_Is_In_Graph(v), ("Vertex not in graph\n"));

  while (e = _v[v].Get_In_Edge()) {
    Delete_Edge(e);
  };
  while (e = _v[v].Get_Out_Edge()) {
    Delete_Edge(e);
  };

  // insert the deleted vertex in free list
  _v[v].Set_Next_Free_Vertex(_vfree);
  _v[v].Set_To_Free();
  _vfree = v;

  _vcnt--;
  
}

template <class EDGE_TYPE, class VERTEX_TYPE>
void
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Delete_Edge(EINDEX16 e) {

  VINDEX16 from, to;
  EINDEX16 e1;

  // see if the edge exists
  Is_True (Edge_Is_In_Graph(e), ("Edge not in graph\n"));

  from = _e[e].Get_Source();
  to = _e[e].Get_Sink();

  if (_v[from].Get_Out_Edge() == e) {  // to delete the first edge in list
    _v[from].Set_Out_Edge(_e[e].Get_Next_Out_Edge());
  } else {
    e1 = _v[from].Get_Out_Edge();
    while (_e[e1].Get_Next_Out_Edge() != e) 
	e1 = _e[e1].Get_Next_Out_Edge();
    _e[e1].Set_Next_Out_Edge(_e[e].Get_Next_Out_Edge());
  }

  if (_v[to].Get_In_Edge() == e) {  // to delete the first edge in list
    _v[to].Set_In_Edge(_e[e].Get_Next_In_Edge());
  } else {
    e1 = _v[to].Get_In_Edge();
    while (_e[e1].Get_Next_In_Edge() != e) 
	e1 = _e[e1].Get_Next_In_Edge();
    _e[e1].Set_Next_In_Edge(_e[e].Get_Next_In_Edge());
  }

  _e[e].Set_Next_Free_Edge(_efree);  // insert the deleted edge in free list
  _e[e].Set_To_Free();
  _efree = e;

  _ecnt--;
  
}

template <class EDGE_TYPE, class VERTEX_TYPE>
VINDEX16
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Get_Vertex() const {
  VINDEX16 v;

  if (Is_Empty()) return 0;

  v = _v.Lastidx();
  while (v>0 &&_v[v].Is_Free() ) v--;	// skip over free vertices
  Is_True( v>0 , ("Fail to get vertex\n"));

  return v;
}

template <class EDGE_TYPE, class VERTEX_TYPE>
VINDEX16
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Get_Next_Vertex(VINDEX16 v) const {

  Is_True( Vertex_Is_In_Graph(v), ("Vertex does not exist in graph\n"));

  do {
    v--;
  } while (v>0 && _v[v].Is_Free() );	// skip over free vertices

  return v;
}

template <class EDGE_TYPE, class VERTEX_TYPE>
VINDEX16
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Get_Next_Vertex_In_Edit(VINDEX16 v) const {
  do {
    v--;
  } while (v>0 && _v[v].Is_Free() );	// skip over free vertices

  return v;
}

template <class EDGE_TYPE, class VERTEX_TYPE>
EINDEX16
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Get_Edge() const {
  EINDEX16 e;

  if (_ecnt == 0) return 0;

  e = _e.Lastidx();
  while (_e[e].Is_Free() && e>0) e--;	// skip over free edges
  Is_True( e>0 , ("Fail to get edge\n"));

  return e;
}

template <class EDGE_TYPE, class VERTEX_TYPE>
EINDEX16
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Get_Next_Edge(EINDEX16 e) const {

  Is_True( Edge_Is_In_Graph(e), ("Edge does not exist in graph\n"));

  do {
    e--;
  } while (e > 0 && _e[e].Is_Free() );	// skip over free edges

  return e;
}

template <class EDGE_TYPE, class VERTEX_TYPE>
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>&
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::
operator=(const DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>& g) {

  // copy everything except mpool

  _vfree = g._vfree;
  _vcnt = g._vcnt;
  _efree = g._efree;
  _ecnt = g._ecnt;

  _v = g._v;
  _e = g._e;

  return *this;
}

template <class EDGE_TYPE, class VERTEX_TYPE>
void
DIRECTED_GRAPH16<EDGE_TYPE,VERTEX_TYPE>::Print(FILE *file) const {
  VINDEX16 i;
  EINDEX16 e;

  fprintf(file,"Print out graph edges and vertices ...\n");
  for (i=1; i<_e.Lastidx()+1; i++)
   if (!_e[i].Is_Free())
    fprintf(file, "%d: %d --> %d\n", i, _e[i]._from, _e[i]._to);

  for (i=1; i<_v.Lastidx()+1; i++)
   if (!_v[i].Is_Free()) {
    fprintf(file, "( ");
    e = _v[i].Get_In_Edge();
    while (e) {
      fprintf(file, "%d ", e);
      e = _e[e].Get_Next_In_Edge();
    }
    fprintf(file, ") %d ( ", i);
    e = _v[i].Get_Out_Edge();
    while (e) {
      fprintf(file, "%d ", e);
      e = _e[e].Get_Next_Out_Edge();
    }
    fprintf(file, ")\n");
  }
}

#endif		// graph_template_INCLUDED

