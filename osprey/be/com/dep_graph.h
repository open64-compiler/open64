/*
 * Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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
//                      Data Dependence Graph for Arrays
//                     ---------------------------------
//
// Description:
//
//     A data dependence graph. This data structure is used for three different
//     graphs.
//
//     1. LNO has a graph for all array statements in the program unit.
//     There is one graph per program unit but edges only exist between
//     two array loads/stores that share at least one common "good" DO loop.
//     We attach to each edge a DEPV_ARRAY.
//     Each load/store of an array WN is mapped to the vertex in the graph.
//     Each dependence is defined to be the difference between loop iterations,
//	ie, we divide by the step size
//
//	Non-array loads/stores (i.e. LDIDs/STIDs) can be in the graph, as
//     second class citizens, if there is a dependence between an array 
//     load/store and an LDID/STID.  They are second class citizens in the
//	sense that they only appear in the graph if they are dependent on an
//	array load/store, and there is never any edges between two LDIDs/STIDs.
//
//     2. Fission/fusion usess a graph of dependence levels.  The dependences
//     are mapped to statements.
//
//     3. CG uses a dependence graph of DEPs.  This defines edges on 
//     array loads/stores in the same "good" inner DO loop.
// TODO: also should put edges in non-good inner loops.
//     The CG graph also contains must dependences.  A must dependence
//     means that modulo control flow, there is guranteed to be a dependence
//     between all pairs of references satisfying the dependence 
//     distance/direction.  In addition, two references with a must dependence
//     must be the same size.  As opposed to regular dependences, must
//     dependences can also be read-read dependences.
//     other loads.  A boolean flag is set to true on the dependence
//     edge if the dependence edge is a must dependence.
//     Each dependence is defined to be the difference between loop iterations,
//	ie, we divide by the step size
//     As opposed to the DEPV_ARRAY graph, LDIDs/STIDs never appear in the graph.
//
//     For all graphs:
//     An unmapped vertex must be assumed to be dependent on everything.
//     Each edge only contains lexicographically positive dependences.
//
//        
//
//
//
// Exported types and functions:
//
//      enum DEP_GRAPH_TYPE 
//	  {DEPV_ARRAY_ARRAY_GRAPH,LEVEL_ARRAY_GRAPH,DEP_ARRAY_GRAPH};
//
//	    	Which type of graph
//
//      ARRAY_VERTEX16 : public VERTEX16
//
//		A vertex 
//
//	    WN *wn
//
//		The WN of the load/store
//
//
//	ARRAY_EDGE16 : public EDGE16
//
//		An edge 
//
//	  Union
//	    class DEPV_ARRAY *Depv_Array
//	    class LEVEL_STRUCT Level_Info
//	    Struct {
//	      DEP Dep
//	      mBOOL Is_Must
//	    }
//
//	        The dependence
//
//     ARRAY_DIRECTED_GRAPH16 : 
//		public DIRECTED_GRAPH16<ARRAY_VERTEX16, ARRAY_EDGE16>
//
//		The graph
//
//	    ARRAY_DIRECTED_GRAPH16(mUINT16 num_v, mUINT16 num_e, WN_MAP map,
//		DEP_GRAPH_TYPE type) 
//
//		Create a graph of type type.  
//		The graph will be attached to the code using map.
//
//	    EINDEX16 Add_Edge(VINDEX16 from, VINDEX16 to, DEP dep,
//				BOOL is_must = FALSE)
//
//		Add a non-must  edge to the graph.  
//		Return 0 if the graph is full.
//
//	    VINDEX16 Add_Vertex(WN *wn)
//
//		Add a vertex to the graph.  Return 0 if the graph is full.
//		Use map to map the vertex to this wn.
//
//	    UINT8	Level(EINDEX16 edge)
//
//		Return the dependence associated with the edge
//
//	    void Set_Level(EINDEX16 edge, UINT8 level) 
//
//	    DEP		Dep(EINDEX16 edge)
//
//		Return the dependence associated with the edge
//
//	    BOOL	Is_Must(EINDEX16 edge)
//
//		Is the dependence a must dependence
//
//	    void	Set_Dep(EINDEX16 edge, DEP dep, BOOL is_must)
//
//	    MEM_POOL *Pool()
//
//		Return the pool used for the DEPV_ARRAYS in the graph
//
//          void Print(FILE *fp)
//
//	    void Remove_Edge(EINDEX16 e)
//
//		Remove the edge from the graph.  Do not delete the
//		Depv_Array (this is useful for stack-based memory disciplines)
//
//	    VINDEX16 Get_Vertex(WN *wn)
//
//		Use the map to find  the index associated with this wn.
//		Returns 0 if this wn has no vertex
//
//	    VINDEX16 Get_Vertex()
//
//		Get the first vertex in the graph
//
//	    WN *Get_Wn(VINDEX16 v)
//
//		Find the wn associated with this v.
//
//          void Set_Wn(VINDEX16 v, WN* wn) {
//
//              Make wn to be associated with v and update the map
//
//          void Clear_Map_For_Vertex(VINDEX v)
//
//              Remove the wn from the map and reset the wn of the vertex.
//              This method is solely used by wopt when wn node is being
//              transformed into the CODEREP representations.
//              
//          void Clear_Map()
//
//              Remove all mappings in the map and reset wns of all
//              vertex to be NULL. 
//
//          mBOOL Copy_Vertex(VINDEX16 v1, VINDEX16 v2)
//
//              Copy all edges to/from v1 to v2
//		return 0 if the graph is full.
//		This only works on CG's dependence graph.
//
//	    void Erase_Graph()
//
//		Remove all edge edges and vertices from the graph
//
// The remaining routines of this class are LNO specific.  No one else
// should use them
//
//	    EINDEX16 Add_Edge(VINDEX16 from, VINDEX16 to, DEPV_ARRAY *array)
//
//		Add an edge to the graph.  Return 0 if the graph is full.
//
//	    BOOL Add_Edge(WN *ref1, const DOLOOP_STACK *s1,
//			  WN *ref2, const DOLOOP_STACK *s2,
//			  BOOL s1_lex_before_s2,BOOL use_bounds=TRUE)
//
//		Compute the dependences (a DEPV_ARRAY or a DEP) and 
//		add two edges to the graph (from ref1 to ref2 and
//		from ref2 to ref1).  Ref1 and Ref2 are loads/stores/call nodes.
//	        s1_lex_before_s2 should be true
//		if s1 appears lexically before s2 in the code.
//		This routine assumes that the vertices for the two
//		references are already in the graph.
//		If this is a DEP graph and not a DEPV_ARRAY graph, the
//		two references must be in the same inner loop
//		Return 0 if the graph is full.
//		If use_bounds is FALSE, don't use bounds information to
//		compute dependences (this is an efficiency vrs accuracy
//		issue)
//
//	    BOOL Add_Edge_Stars(WN *ref1, DOLOOP_STACK *s1, WN *ref2, 
//				DOLOOP_STACK *s2,BOOL s1_lex_before_s2,
//				BOOL pos_only=FALSE)
//
//		Like Add_Edge above, but rather than actually computing
//		depedences, just make them all stars.  pos_only adds
//		an edge only from ref1 to ref2.
//
//	    BOOL Add_Edge_Equals(WN *ref1, DOLOOP_STACK *s1, WN *ref2, 
//				DOLOOP_STACK *s2)
//
//		Add an all equals edge from ref1 to ref2
//
//	    EINDEX16 Add_Edge(VINDEX16 from, VINDEX16 to, UINT8 level)
//
//		Add an edge to the graph.  Return 0 if the graph is full.
//
//	    DEPV_ARRAY *Depv_Array(EINDEX16 edge)
//
//	    void Set_Depv_Array(EINDEX16 edge, DEPV_ARRAY*)
//
//	    INT Build(WN *func_nd,MEM_POOL *pool=0);
//
//		Build the graph for the procedure. Store the DEPV_ARRAYs,
//		if this is a DEPV_ARRAY graph, in pool.
//		(the actual graph is stored in its own pool)
//		Map each l/s node to its vertex in the graph. 
//		Return 0 on error
//
//	    INT Build(ARRAY_DIRECTED_GRAPH16 *da_graph)
//
//		Given a DEPV_ARRAY graph, build a DEP graph.  Copy the
//		vertices (although map them to l/s rather than arrays).
//		Throw out all the edges between two vertices not in the
//		same inner loop.  Convert all remaining DEPV_ARRAYs to
//		DEPs. 
//
//	    void Delete_Array_Edge(EINDEX16 e)
//
//		Remove the edge from a DEPV_ARRAY graph and delete the array
//
//	    INT Add_Deps_To_Copy_Block(WN *orig,WN *copy,
//                                     BOOL keep_internal_edge)
//
//		Given that copy is a copy of the tree rooted at orig.
//		Given that all the dependences for orig are in the graph
//		Add copies of the dependences to the graph for the nodes in
//		copy.  This works on the DEPV_ARRAY graph. By default,
//              keep_internal_edge is set to TRUE which allows copying
//              the edges within the orig. tree. Set it to FALSE would make
//              edges within the orig. tree NOT be copied.
//		Return 0 on error
//
//	    INT Unrolled_Dependences_Update(WN** bodies, UINT u, UINT loopno)
//
//		Fix the array dependences after loop unrolling.  
//		Loopno is the number of the loop being unrolled u times.
//		bodies[0..u-1] are identical copies of code  
//		bodies 0 is the original.
//		Return 0 on error
//		
//	    void Fission_Dep_Update(WN* in_loop,UINT32 total_loops)
//
//		Fix the array dependences after fission.  
//		in_loop points to the outer loop of the first fissioned loop 
//		(the rest can be found via next pointers). Total_loops gives 
//		the number of loops. 
//
//	    INT Fission_Dep_Update(WN* in_loop,UINT32 total_loops, 
//					UINT fission_depth)
//		Fix the statement dependence graph after fission.
//		in_loop points to the outer loop of the first fissioned loop 
//		(the rest can be found via next pointers). Total_loops gives 
//		the number of loops.  Fission_depth gives the number of
//		perfectly nested loops being fissioned together
//		Return 0 on error (can only happen for statement graph).
//
//          INT Build_Region(WN* start, WN* end, DOLOOP_STACK *stack,
//                           BOOL rebuild=FALSE, BOOL skip_bad=FALSE)
//
//              Build the dependences WITHIN a region specified by start
//              and end WHIRL nodes. Old edges will be removed 
//              if rebuild==TRUE.
//		If skip_bad do not visit bad loops within this region
//
//
//          void Versioned_Dependences_Update (WN* body_orig, WN* body_new, 
//              UINT loopno, WN_MAP version_map)
//              Fix the array dependence graph after versioning loops for 
//              prefetching.
//              body_orig is the original body (the then part after versioning),
//              body_new is the new body (the then part after versioning),
//              and loopno is the depth of the loop being split.
//
//          void Versioned_Create_Vertices (WN* body_orig, WN* body_new)
//              Given the original and new body of a versioned loop,
//              add vertices in the new body for each node in orig-body that
//              has a vertex.
//
//          void Versioned_Dependences_Update_E (WN *body_orig,
//                                               WN* body_new,
//                                               WN* root_orig,
//                                               WN* root_new,
//                                               UINT loopno, 
//						 WN_MAP version_map)
//              Given the orig-body and new-body of a versioned loop,
//              add the edges based on the edges in the original dependence 
//		graph.
//
//          void Check_Graph()
//
//              Assertion fails if a few simple checks fail.
//              Only defined if Is_True_On is set.
//
//

/** $Revision: 1.2 $
*** $Date: 02/11/07 23:41:35-00:00 $
*** $Author: fchow@keyresearch.com $
*** $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.dep_graph.h $
**/

#ifndef dep_graph_INCLUDED
#define dep_graph_INCLUDED "dep_graph.h"

#ifdef _KEEP_RCS_ID
static char *dep_graph_rcs_id = dep_graph_INCLUDED "$Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#ifndef cxx_graph_INCLUDED
#include "cxx_graph.h"
#endif
#ifndef graph_template_INCLUDED
#include "graph_template.h"
#endif
#ifndef _defs_INCLUDED
#include "cxx_memory.h"
#endif

#ifdef LNO
#ifndef dep_INCLUDED
#include "dep.h"
#endif
#ifndef cxx_hash_INCLUDED
#include "cxx_hash.h"
#endif
#endif

#ifndef LNO
#ifndef dvector_INCLUDED
#include "dvector.h"
#endif
#endif

/* C interface functions used for reading and writing graphs */
extern "C" {
  void *C_Dep_Graph(void);
  void Init_Dep_Graph(void *g);
  void Dealloc_Dep_Graph(void);
  void Depgraph_Write(void *depgraph, struct output_file *fl, WN_MAP off_map);
  void *Depgraph_Read(char *cur_addr, char *end_addr, char *wn_base);

  void LNOPreserveMapPair(WN *orig, WN *wn1, WN *wn2);
  void LNOPruneMapsUsingParity(void);
  void LNOPrintDepGraph(FILE *fp);
  VINDEX16 LNOGetVertex(WN *wn);

  BOOL WN_parity_independent(WN *wn1, WN *wn2);

  BOOL LnoDependenceEdge(WN *wn1, WN *wn2, EINDEX16 *distance, DIRECTION *direction, BOOL *is_must, BOOL *status);
}

enum DEP_GRAPH_TYPE {DEPV_ARRAY_ARRAY_GRAPH,LEVEL_ARRAY_GRAPH,DEP_ARRAY_GRAPH};

class ARRAY_VERTEX16 : public VERTEX16 {
public:
  WN *Wn;
  ARRAY_VERTEX16(WN *wn) { Wn = wn;}
  friend class ARRAY_DIRECTED_GRAPH16;
};

#define  HAS_ALL_ZERO 		0x0001
#define  HAS_SCALAR_FLOW 	0x0002
#define  HAS_SCALAR_ANTI 	0x0004
#define  HAS_SCALAR_OUTPUT 	0x0008
#define  HAS_ARRAY_FLOW 	0x0010
#define  HAS_ARRAY_ANTI 	0x0020
#define  HAS_ARRAY_OUTPUT 	0x0040

struct LEVEL_STRUCT {
  UINT8 Level;
  UINT16 Property;
};

struct DEP_STRUCT {
 DEP Dep;
 mBOOL Is_Must;
}; 

class ARRAY_EDGE16 : public EDGE16 {
public:
  friend class ARRAY_DIRECTED_GRAPH16;
  union {
    class DEPV_ARRAY *Depv_Array;
    struct LEVEL_STRUCT Level_Info;
    DEP_STRUCT DEP_Struct;
  };
};

#ifdef LNO
class REFERENCE_LIST;
typedef STACK<REFERENCE_LIST *> REF_LIST_STACK;
typedef STACK<VINDEX16 > VINDEX16_STACK;

class VINDEX16P_LEX_COUNT {
public:
  VINDEX16 *_vp;
  INT _lex_count;
  VINDEX16P_LEX_COUNT(VINDEX16 *vp, INT lex_count) {
    _vp = vp;
    _lex_count = lex_count;
  }
};

#endif

class  ARRAY_DIRECTED_GRAPH16 : 
	public DIRECTED_GRAPH16<ARRAY_EDGE16,ARRAY_VERTEX16> {
  WN_MAP _map;
  DEP_GRAPH_TYPE  _type;
  MEM_POOL *_pool;
public:

#if defined(SHARED_BUILD) && !defined(LNO)
  void Print(FILE *fp);
#else
  void Print(FILE *fp, INT dummy=0);
#endif

  ARRAY_DIRECTED_GRAPH16(mUINT16 num_v, mUINT16 num_e, WN_MAP map, 
	DEP_GRAPH_TYPE type) : 
	DIRECTED_GRAPH16<ARRAY_EDGE16,ARRAY_VERTEX16>(num_v,num_e) {
    _map=map;
    _type = type;
    _pool = NULL;
  }

  MEM_POOL *Pool() { return _pool; }

  void Erase_Graph();

  EINDEX16 Add_Edge(VINDEX16 from, VINDEX16 to, DEP dep, BOOL is_must=FALSE) {
    Is_True(_type==DEP_ARRAY_GRAPH,
      ("Trying to add a dep edge to a non-dep graph"));
    EINDEX16 result = 
      DIRECTED_GRAPH16<ARRAY_EDGE16,ARRAY_VERTEX16>::Add_Edge(from,to);
    if (result != 0) _e[result].DEP_Struct.Dep = dep;
    _e[result].DEP_Struct.Is_Must = is_must;
    return result;
  }

  DEP Dep(EINDEX16 edge) {
    Is_True(_type==DEP_ARRAY_GRAPH,
      ("Trying to get a dep edge from a non-dep graph"));
    return(_e[edge].DEP_Struct.Dep);
  }
  BOOL Is_Must(EINDEX16 edge) {
    Is_True(_type==DEP_ARRAY_GRAPH,
      ("Trying to get a dep edge from a non-dep graph"));
    return(_e[edge].DEP_Struct.Is_Must);
  }
  void Set_Dep(EINDEX16 edge, DEP dep, BOOL is_must=FALSE) {
    Is_True(_type==DEP_ARRAY_GRAPH,
      ("Trying to set a dep edge in a non-dep graph"));
    _e[edge].DEP_Struct.Dep = dep;
    _e[edge].DEP_Struct.Is_Must = is_must;
  }

  VINDEX16 Add_Vertex(WN *wn) {
    VINDEX16 result = 
      DIRECTED_GRAPH16<ARRAY_EDGE16,ARRAY_VERTEX16>::Add_Vertex();
    if (result != 0) {
      _v[result].Wn = wn;
      WN_MAP_Set(_map,wn, (void *) (INTPTR)(UINT) result);
    }
    return result;
  }

  void Remove_Edge(EINDEX16 e) { 
    DIRECTED_GRAPH16<ARRAY_EDGE16,ARRAY_VERTEX16>::Delete_Edge(e);
  }

  VINDEX16 Get_Vertex(WN *wn) {
    return (VINDEX16) (UINT) (INTPTR) WN_MAP_Get(_map,wn);
  }

  VINDEX16 Get_Vertex() {
    return DIRECTED_GRAPH16<ARRAY_EDGE16,ARRAY_VERTEX16>::Get_Vertex();
  }

  void Delete_Vertex(VINDEX16 v) {
    WN_MAP_Set(_map, _v[v].Wn, NULL);
    DIRECTED_GRAPH16<ARRAY_EDGE16,ARRAY_VERTEX16>::Delete_Vertex(v);
  }

  void Delete_Map() { WN_MAP_Delete(_map); _map = 0;};

  WN* Get_Wn(VINDEX16 v) {
    return _v[v].Wn;
  }

  void Set_Wn(VINDEX16 v, WN* wn) {
    _v[v].Wn=wn;
    WN_MAP_Set(_map, _v[v].Wn, (void*)(INTPTR)(UINT)v);
  }

  void Delete_Vertex(WN *wn) {
    Delete_Vertex(Get_Vertex(wn));
  }

  void Clear_Map_For_Vertex(VINDEX16 v) {
    WN_MAP_Set(_map, _v[v].Wn, NULL);
    _v[v].Wn=NULL;
  }

  void Clear_Map() {
    VINDEX16 v=Get_Vertex();
    while (v!=0) {
      Clear_Map_For_Vertex(v);
      v=Get_Next_Vertex(v);
    }
  }

  mBOOL Copy_Vertex(VINDEX16 v1, VINDEX16 v2) {
    Is_True(_type==DEP_ARRAY_GRAPH,
      ("Copy_Vertex only works on DEP_ARRAY_GRAPH"));
    if (v2 != 0) {
      EINDEX16 e=Get_Out_Edge(v2);
      while (e) {
        EINDEX16 e1=Get_Next_Out_Edge(e);
        Remove_Edge(e);
        e=e1;
      }
      e=Get_In_Edge(v2);
      while (e) {
        EINDEX16 e1=Get_Next_In_Edge(e);
        Remove_Edge(e);
        e=e1;
      }
      e=Get_Out_Edge(v1);
      while (e) {
	VINDEX16 to=Get_Sink(e);
	if (!Add_Edge(v2,to,_e[e].DEP_Struct.Dep,_e[e].DEP_Struct.Is_Must)) {
	  return 0;
        }
        e=Get_Next_Out_Edge(e);
      }
      e=Get_In_Edge(v1);
      while (e) {
	VINDEX16 source=Get_Source(e);
	if (!Add_Edge(source,v2,_e[e].DEP_Struct.Dep,_e[e].DEP_Struct.Is_Must)) {
	  return 0;
        }
        e=Get_Next_In_Edge(e);
      }
    }
    return 1;
  }


  void PreserveMapPair(WN *orig, WN *wn1, WN *wn2)
  {
    VINDEX16	origV;
    Is_True((Get_Vertex(wn1)==0),("Dep_PreserveMapPair(): unexpected map"));
    Is_True((Get_Vertex(wn2)==0),("Dep_PreserveMapPair(): unexpected map"));

    if (origV = Get_Vertex(orig))
    {
      VINDEX16	v1 = Get_Vertex(wn1);
      VINDEX16	v2 = Get_Vertex(wn2);

      if (v1 == 0)
	v1 = Add_Vertex(wn1);

      if (v2 == 0)
	v2 = Add_Vertex(wn2);

      Copy_Vertex(origV, v1);
      Copy_Vertex(origV, v2);
    }
  }

  void PruneMapsUsingParity(void);


#ifdef LNO
  INT Build(WN *func_nd,MEM_POOL *pool=0);
  INT Build(ARRAY_DIRECTED_GRAPH16 *da_graph);
  EINDEX16 Add_Edge(VINDEX16 from, VINDEX16 to, DEPV_ARRAY *array) {
    Is_True(_type==DEPV_ARRAY_ARRAY_GRAPH,
      ("Trying to add a DEPV_ARRAY edge to a non-DEPV_ARRAY graph"));
    Is_True(!Get_Edge(from,to),("Duplicate edge in Add_Edge \n"));
    Is_True(array,("Null array in Add_Edge"));
    if (!array) return (EINDEX16) 0;
    EINDEX16 result = 
      DIRECTED_GRAPH16<ARRAY_EDGE16,ARRAY_VERTEX16>::Add_Edge(from,to);
    if (result != 0) _e[result].Depv_Array = array;
    return result;
  }
  EINDEX16 Add_Edge(VINDEX16 from, VINDEX16 to, UINT8 level) {
    Is_True(_type==LEVEL_ARRAY_GRAPH,
      ("Trying to add a level edge to a non-level graph"));
    EINDEX16 result = 
      DIRECTED_GRAPH16<ARRAY_EDGE16,ARRAY_VERTEX16>::Add_Edge(from,to);
    if (result != 0) _e[result].Level_Info.Level = level;
    return result;
  }
  BOOL Add_Edge(WN *ref1, const DOLOOP_STACK *s1,
		WN *ref2, const DOLOOP_STACK *s2,
		BOOL s1_lex_before_s2, BOOL use_bounds=TRUE);
  BOOL Add_Edge_Stars(WN *ref1, const DOLOOP_STACK *s1,
		      WN *ref2, const DOLOOP_STACK *s2,
		      BOOL s1_lex_before_s2, BOOL pos_only=FALSE);
  BOOL Add_Edge_Equals(WN *ref1, const DOLOOP_STACK *s1,
		      WN *ref2, const DOLOOP_STACK *s2);

  DEPV_ARRAY *Depv_Array(EINDEX16 edge) {
    Is_True(_type==DEPV_ARRAY_ARRAY_GRAPH,
      ("Trying to get a DEPV_ARRAY edge from a non-DEPV_ARRAY graph"));
    return(_e[edge].Depv_Array);
  }

  void Set_Depv_Array(EINDEX16 edge, DEPV_ARRAY* newdv) {
    Is_True(_type==DEPV_ARRAY_ARRAY_GRAPH,
      ("Trying to set a DEPV_ARRAY edge in a non-DEPV_ARRAY graph"));
    _e[edge].Depv_Array = newdv;
  }

  UINT8 Level(EINDEX16 edge) {
    Is_True(_type==LEVEL_ARRAY_GRAPH,
      ("Trying to get a level edge from a non-level graph"));
    return(_e[edge].Level_Info.Level);
  }

  void Set_Level(EINDEX16 edge, UINT8 level) {
    Is_True(_type==LEVEL_ARRAY_GRAPH,
      ("Trying to set a level edge in a non-level graph"));
    _e[edge].Level_Info.Level = level;
  }


  void Delete_Array_Edge(EINDEX16 e) { 
    Is_True(_type==DEPV_ARRAY_ARRAY_GRAPH,
      ("Trying to delete a DEPV_ARRAY edge from a non-DEPV_ARRAY graph"));
    Delete_DEPV_ARRAY(_e[e].Depv_Array,_pool);
    DIRECTED_GRAPH16<ARRAY_EDGE16,ARRAY_VERTEX16>::Delete_Edge(e);
  }


  void Set_Level_Property(EINDEX16 e, UINT16 property) {
    _e[e].Level_Info.Property |= property;
  }

  BOOL Get_Level_Property(EINDEX16 e, UINT16 property) {
    return ((_e[e].Level_Info.Property & property) != 0);
  }

  INT Add_Deps_To_Copy_Block(WN *orig, WN *copy, BOOL keep_internal_edge=TRUE);
  INT Unrolled_Dependences_Update(WN** bodies, UINT u, UINT loopno);
  void Fission_Dep_Update(WN* in_loop,UINT32 total_loops);   
  INT Fission_Dep_Update(WN* in_loop,UINT32 total_loops, UINT fission_depth);
  void Fission_Dep_Update_R(WN* wn,WN *in_loop,UINT depth);   
  INT Build_Region(WN* start, WN* end,DOLOOP_STACK *stack,BOOL rebuild=FALSE, BOOL skip_bad=FALSE);
  BOOL Versioned_Dependences_Update (WN* body_orig, WN* body_new, UINT loopno,
    WN_MAP version_map);
  void Versioned_Create_Vertices (WN* body_orig, WN* body_new);
  BOOL Versioned_Dependences_Update_E (WN *body_orig,
                                       WN* body_new,
                                       WN* root_orig,
                                       WN* root_new,
                                       UINT loopno,
				       WN_MAP version_map);

#ifdef Is_True_On
  void Check_Graph();
#endif
#endif /* LNO */

private:

#ifdef LNO
  void Add_Must();
  void Set_Must(EINDEX16 edge) {
    _e[edge].DEP_Struct.Is_Must = TRUE;
  }
  BOOL Is_Must(ACCESS_ARRAY *a1, ACCESS_ARRAY *a2, WN *inner_loop,
					DEP *dep=NULL);

  INT Find_Region(WN *wn, DOLOOP_STACK *stack);
  INT Gather_References(WN *wn, REF_LIST_STACK *writes, 
	REF_LIST_STACK *reads, DOLOOP_STACK *stack,
	class SCALAR_STACK *scalar_writes, class SCALAR_STACK *scalar_reads,
	class CALL_STACK *calls, BOOL skip_bad=FALSE);
  INT Add_Deps_To_Copy_Block_V(WN *orig, WN *copy, 
		class HASH_TABLE<VINDEX16,VINDEX16> *hash_table);
  INT Add_Deps_To_Copy_Block_E(WN *orig, WN *copy,
		class HASH_TABLE<VINDEX16,VINDEX16> *hash_table,
		BOOL keep_internal_edge=TRUE);
  INT Unrolled_Dependences_Update_V(WN** bodies, UINT u, 
	class HASH_TABLE<VINDEX16,VINDEX16P_LEX_COUNT *> *hash_table,
	VINDEX16_STACK *orig_vertices);
  INT Unrolled_Dependences_Update_E(UINT u, 
	UINT loopno,class HASH_TABLE<VINDEX16,VINDEX16P_LEX_COUNT*>*hash_table,
	VINDEX16_STACK *orig_vertices);
  INT Unrolled_Dependences_Update_E(VINDEX16 *sources, VINDEX16 *sinks,
	EINDEX16 fedge, EINDEX16 bedge,UINT u, UINT loopno, INT lex_count,
	INT sink_lex_count);
  void Fission_Dep_Update_V(VINDEX16 v,WN *in_loop, UINT depth);   
  INT Copy_Do_Loop_Deps(VINDEX16 *do_loop_vertices, INT num_loops);
  INT Fission_Dep_Update_R(WN *in_loop,UINT fission_depth, UINT depth,
	BOOL outer_good_do);   
#endif

  /* friends called from C functions for reading and writing graphs */
  friend void Depgraph_Write(void *depgraph, struct output_file *fl,
			     WN_MAP off_map);
  friend void *Depgraph_Read(char *cur_addr, char *end_addr, char *wn_base);
};

extern ARRAY_DIRECTED_GRAPH16 *Current_Dep_Graph;
extern void LNO_Erase_Dg_From_Here_In(WN* wn, ARRAY_DIRECTED_GRAPH16* dg);
extern void Unmapped_Vertices_Here_Out(WN* wn);

#ifdef LNO
// A list of references and their associated DOLOOP_STACKs
// For efficiency reasons, the build routines use stacks of 
// REFERENCE_LISTs, one element for each base array.  
// A base array is represented by its ST_Base pointer.  
// We only call the dependence routines on two arrays with the same
// base pointer.  If we can't figure out an element's
// base pointer, we put it on the list of zero base pointers.
// Zero base pointers will be compared to everything.


class REFERENCE_NODE: public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS( REFERENCE_NODE);
public:
  DOLOOP_STACK *Stack;
  WN *Wn;
  UINT Statement_Number;

  REFERENCE_NODE(WN *wn, DOLOOP_STACK *stack, UINT statement_number) { 
    Wn = wn;
    Stack = stack; 
    Statement_Number = statement_number;
  };
  void Print(FILE *fp) {
    fprintf(fp,"Wn, Statment_Number = 0x%p %d\n",
	Wn,Statement_Number); 
  }
  ~REFERENCE_NODE(); 
};

class REFERENCE_LIST: public SLIST {
  DECLARE_SLIST_CLASS( REFERENCE_LIST, REFERENCE_NODE )
public:
  ST *ST_Base;
  WN *Array;
  WN *Inner_Loop;
  REFERENCE_LIST(ST *st_base, WN *array, WN *inner_loop=0) { 
    ST_Base = st_base; 
    Array = array;
    Inner_Loop = inner_loop;
  };
  void Print(FILE *fp);
  ~REFERENCE_LIST();
};


// A stack of SYMBOLs and all the WNs that access that symbol

// one reference
class SCALAR_REF {
public:
  WN *Wn;
  UINT Statement_Number;
  SCALAR_REF(WN *wn, UINT statement_number) {
    Wn = wn;
    Statement_Number = statement_number;
  }
  SCALAR_REF& operator=(const SCALAR_REF& scalar_ref) {
    Wn = scalar_ref.Wn;
    Statement_Number = scalar_ref.Statement_Number;
    return *this;
  }
};

// all the references to a particular SYMBOL
class SCALAR_NODE
{
  MEM_POOL *_pool;
public:
  SYMBOL _scalar;
  STACK <SCALAR_REF> *_scalar_ref_stack;
  SCALAR_NODE(MEM_POOL *pool, SYMBOL scalar) { 
    typedef STACK<SCALAR_REF> SCALAR_REF_STACK;
    _pool = pool;
    _scalar_ref_stack = CXX_NEW(SCALAR_REF_STACK(pool),pool);
    _scalar = scalar;
  }
  INT Elements() const { return _scalar_ref_stack->Elements(); };
  SCALAR_REF *Bottom_nth(INT i) { return &_scalar_ref_stack->Bottom_nth(i); };
  SCALAR_REF *Top_nth(INT i) { return &_scalar_ref_stack->Top_nth(i); };
};

class SCALAR_STACK
{
  STACK <SCALAR_NODE> *_stack;
  MEM_POOL *_pool;
public:
  SCALAR_STACK(MEM_POOL *pool) { 
    typedef STACK<SCALAR_NODE> SNODE_STACK;
    typedef STACK<INT> INT_STACK;
    _pool = pool;
    _stack = CXX_NEW(SNODE_STACK(pool),pool);
  }
  void Add_Scalar(WN *wn, UINT snumber);
  void Add_Scalar(WN *wn_call, SYMBOL* symbol, UINT snumber);
  void Remove_Scalar(WN *wn);
  void Print(FILE *fp);
  void Clear() { _stack->Clear(); };
  INT Elements() const { return _stack->Elements(); };
  SCALAR_NODE* Bottom_nth(INT i) { return &_stack->Bottom_nth(i); };
  SCALAR_NODE* Top_nth(INT i) { return &_stack->Top_nth(i); };
  void Add_Scalar_Node(SCALAR_NODE* sn) {_stack->Push(*sn);};
  void Clear_Formal(INT i); 
};

// describe the calls
class CALL_NODE
{
public:
  WN *_call;
  UINT _statement_number;
  DOLOOP_STACK *_stack;
  BOOL _is_concurrent_call;
  CALL_NODE(WN *call, UINT statement_number, DOLOOP_STACK *stack,
			BOOL is_concurrent_call) {
    _call = call;
    _statement_number = statement_number;
    _stack = stack;
    _is_concurrent_call = is_concurrent_call;
  }
};

class CALL_STACK
{
  STACK <CALL_NODE> *_stack;
  MEM_POOL *_pool;
public:
  CALL_STACK(MEM_POOL *pool) { 
    typedef STACK<CALL_NODE> CNODE_STACK;
    _pool = pool;
    _stack = CXX_NEW(CNODE_STACK(pool),pool);
  }
  void Clear() { _stack->Clear(); }
  INT Elements() const { return _stack->Elements(); };
  CALL_NODE *Bottom_nth(INT i) { return &_stack->Bottom_nth(i); };
  CALL_NODE *Top_nth(INT i) { return &_stack->Top_nth(i); };
  void Push(WN *call, UINT statement_number,DOLOOP_STACK *stack,
		BOOL is_concurrent_call) { 
    _stack->Push(CALL_NODE(call,statement_number,stack,is_concurrent_call)); 
  };
};

// describe the calls



class REFERENCE_ITER: public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( REFERENCE_ITER, REFERENCE_NODE ,REFERENCE_LIST);
public:
  ~REFERENCE_ITER() {};
};

#endif 


#endif


