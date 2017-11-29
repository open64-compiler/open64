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
*** Module: lno_scc.h
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
***     This interface describes a directed graph class. It can be used to
***     represent a simple directed graph which has no multiple edges for
***     a given pair of source and sink vertices. The size of the graph
***     changes automatically as the number of vertices and edges can grow
***     increases. Functions relates to strongly-connected component
***     (SCC) are also provided. Note that with the current implementation,
***     the number of vertices/edges cannot exceed 2**16 - 2. 
***
***     This SCC_DIRECTED_GRAPH16 class is a derived class of
***     DIRECTED_GRAPH16<EDGE16,VERTEX16>. We will only describe the
***     interface specific to the SCC calculation. For details on
***     DIRECTED_GRAPH16, please see "graph_template.h". "cxx_graph.h"
***     provides additional information on EDGE16 and VERTEX16.
*** 
***     SCC_DIRECTED_GRAPH16
*** 
*** 	This is the  derived directed graph type which keeps additional
***     information about the strongly-connected component (SCC) structure
***     of the directed graph. Note that the vertices have index
***     values from 1 to Get_Vertex_Count() and the edges have index
***     value from 1 to Get_Edge_Count(). See "graph_template.h" for more
***     details.
*** 
***         VINDEX16	Get_Scc_Id(VINDEX16 i)
*** 
*** 		Return the ID number of the strongly-connected component
*** 		(SCC) of which the vertex 'i' belongs to. SCC info is
***             re-calculated if necessary. The valid SCC id starts
***             from 1.
*** 
***         VINDEX16	Get_Scc_Count()
*** 
*** 		Return the number of SCC in this graph. SCC info is
***             re-calculated if necessary.
*** 
***         VINDEX16	Get_Scc_Size(VINDEX16 i)
*** 
*** 		Return the size of the SCC of which the vertex 'i' belongs
*** 		to. SCC info is re-calculated if necessary. The valid
***             SCC id starts from 1.
***
***         mUINT16      Get_Level(mUINT16 level[])
***
***             Return the level-sort (similar to topological-sort)
***             result for an acyclic directed graph. Array 'level' should
***             be of size Get_Vertex_Count()+1. For each vertex with
***             corresponding VINDEX16 'v' in the graph, level[v] shows the
***             depth of dependence chain for 'v'. The levels start from
***             0 and the function return value is the maximal level.
*** 
***         mUINT16      Level_Sort(VINDEX16 queue[])
***
***             Return the topological-sort result for an acyclic
***             directed graph. Array 'queue' should be of size
***             Get_Vertex_Count(). queue[i] is the i-th vertex
***             after the topological-sorting.  The levels start
***             from 0 and the function return value is the
***             maximal level.
***
***         SCC_DIRECTED_GRAPH16& Acyclic_Condensation(MEM_POOL *mpool)
*** 
*** 		Return acyclic condensation (AC) of the directed graph.
*** 		Memory pool 'mpool' is used to allocate memory for the new
*** 		graph. SCC info is re-calculated if necessary.
*** 
***         SCC_DIRECTED_GRAPH16& operator=(SCC_DIRECTED_GRAPH16& g)
*** 
*** 		Graph assignment operation. The vertices, edges and the
*** 		scc info are copied.
*** 
**/

#ifndef lno_scc_RCS_ID
#define lno_scc_RCS_ID
#ifdef _KEEP_RCS_ID
static char *lno_scc_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */
#endif

#ifndef _lno_scc_INCLUDED
#define _lno_scc_INCLUDED

#include "defs.h"
#include "mempool.h"
#include "cxx_template.h"
#include "cxx_graph.h"
#include "graph_template.h"

class SCC_DIRECTED_GRAPH16 : public DIRECTED_GRAPH16<EDGE16,VERTEX16> {
private:

  MEM_POOL	*_sccmpool;	// pool used for scc_id allocation
  VINDEX16	_scc_cnt;	// total number of SCCs in this graph
				// it is set to -1 if the SCC info is not
				// valid
  DYN_ARRAY<VINDEX16> _scc_id;	// dynamic array for SCC id (one element
				// for every vertex)

  VINDEX16	*_link;		// temporary array for SCC identification
  VINDEX16	*_dfn;		// temporary array for SCC identification
  BOOL		*_visited;	// temporary array for SCC identification
  BOOL		*_in_stack;	// temporary array for SCC identification
  STACK<VINDEX16> *_dfs_stack;	// temporary stack for SCC identification
  VINDEX16	_df_count;	// counter for Depth-First-Search in SCC
				// identification

		SCC_DIRECTED_GRAPH16(const SCC_DIRECTED_GRAPH16&);

  void		Scc_Dfs(VINDEX16 n);	// Depth-First-Search used in SCC
					// identification

  BOOL		Scc_Is_Valid()		{ return _scc_cnt!=INVALID_VINDEX16; }

					// _scc_cnt==INVALID_VINDEX16
					// invalidates SCC info
  void		Invalidate_Scc()	{ _scc_cnt = INVALID_VINDEX16 ;}
  void		Find_Scc();		// Identify SCCs in the graph
public:
  		SCC_DIRECTED_GRAPH16(
			const VINDEX16 vsize, const EINDEX16 evsize);
  		~SCC_DIRECTED_GRAPH16()			{ 
    			_scc_id.Free_array();
			MEM_POOL_Pop(_sccmpool);
			MEM_POOL_Delete(_sccmpool);
			CXX_DELETE(_sccmpool,Malloc_Mem_Pool);
			}

  SCC_DIRECTED_GRAPH16& operator=(const SCC_DIRECTED_GRAPH16& g);

  EINDEX16	Add_Vertex();
  void		Delete_Vertex(VINDEX16 v);

  EINDEX16	Add_Edge(VINDEX16 from, VINDEX16 to);
  EINDEX16	Add_Unique_Edge(VINDEX16 from, VINDEX16 to);
  void		Delete_Edge(EINDEX16 e);

  VINDEX16	Get_Scc_Id(VINDEX16 i)		{ 
    			if ( ! Scc_Is_Valid() ) Find_Scc();
    			return _scc_id[i]; }
  VINDEX16	Get_Scc_Count()			{
    			if ( ! Scc_Is_Valid() ) Find_Scc();
    			return _scc_cnt; }
  VINDEX16	Get_Scc_Size(VINDEX16 i);

  mUINT16	Get_Level(mUINT16 level[]);

  mUINT16       Level_Sort(VINDEX16 queue[]);

  SCC_DIRECTED_GRAPH16* Acyclic_Condensation(MEM_POOL *mpool);

  void		Print(FILE *file) const {
                        DIRECTED_GRAPH16<EDGE16,VERTEX16>::Print(file);
                        }
};

#endif		// _lno_scc_INCLUDED

