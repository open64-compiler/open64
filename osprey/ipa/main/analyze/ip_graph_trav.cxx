/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include "defs.h"
#include "cxx_base.h"
#include "cxx_memory.h"
#include "ip_graph.h"
#include "ip_graph_trav.h"
#include "tracing.h"

// maximum depth of the call graph
INT IPA_CG_Max_Depth = 0;

static INT *Level_Count = NULL;

/*--------------------------------------------------------------------------*/
/* note, a walk order is built for nodes reachable from the designated root */
/* This implies that in the example below:                                  */
/* main--->A , B--->A, assuming the root is main, then an ordering will     */
/* be constructed for main--->A and not B--->A since B is not reachable     */
/* from main.                                                               */
/*--------------------------------------------------------------------------*/
void
ORDERED_NODE_ITER::BuildVector (TRAVERSAL_ORDER order)
{
    mUINT8 *visit;
    v = CXX_NEW (VVECTOR(GRAPH_vmax(g)), Malloc_Mem_Pool);

    if (order == DONTCARE) {
	for (INT i = 0; i < GRAPH_vmax(g); i++) {
	    if (NODE_fcnt(&GRAPH_v_i(g, i)) != -1)
		v->Append(i);
	}
	return;
    }

    visit = CXX_NEW_ARRAY (mUINT8, GRAPH_vmax(g), Malloc_Mem_Pool);
    BZERO (visit, sizeof(mUINT8)*GRAPH_vmax(g));

    // level count, stores the count of vertices at each level
    // this is needed only during the postorder walk
    if (order == LEVELORDER) {
	Level_Count = CXX_NEW_ARRAY (INT, GRAPH_vmax(g), Malloc_Mem_Pool);
	BZERO (Level_Count, sizeof(INT)*GRAPH_vmax(g)); 
    }

    if (order == POSTORDER || order == PREORDER || order == LEVELORDER)
	Walk (GRAPH_root(g), visit);
    CXX_DELETE_ARRAY (visit, Malloc_Mem_Pool);

    if (order == LEVELORDER) {
	Build_Level_Order();
	CXX_DELETE_ARRAY(Level_Count, Malloc_Mem_Pool);
    }

} // ORDERED_NODE_ITER::BuildVector

/*--------------------------------------------------------------------------*/
/* print the vertices based on the traversal order                          */
/*--------------------------------------------------------------------------*/
void
ORDERED_NODE_ITER::Print(FILE* fp)
{
  if (order == POSTORDER)
    fprintf(fp, "PRINTING THE DEPTH FIRST POST ORDERING OF VERTICES \n");

  else if (order == PREORDER)
    fprintf(fp, "PRINTING THE DEPTH FIRST PRE ORDERING OF VERTICES \n");

  else
    fprintf(fp, "PRINTING THE LEVEL ORDERING OF VERTICES \n");

    for (INT i=0; i< v->Cnt(); ++i) {
	fprintf(fp, "Vertex %d is in position %d \n", (*v)[i], i);
    }
} // ORDERED_NODE_ITER::Print 

/*--------------------------------------------------------------------------*/
/* mark edges that are recursive                                            */
/*--------------------------------------------------------------------------*/
void ORDERED_NODE_ITER::MarkRecursive(mUINT8 visit[], NODE_INDEX r)
{
 NODE_ITER vitr(g, r);

 for (NODE_INDEX vi = vitr.First_Succ(); vi != -1; vi = vitr.Next_Succ())
  if (visit[vi] != 2)
   {
    Set_EDGE_recursive(&GRAPH_e_i(g,vitr.Current_Edge_Index()));
#ifdef IPA_DEBUG
    fprintf ( TFile, "edge is recursive %d %d \n", 
	      GRAPH_e_i(g,vitr.Current_Edge_Index()).from,
	      GRAPH_e_i(g,vitr.Current_Edge_Index()).to );
#endif
   }
}

/*--------------------------------------------------------------------------*/
/* visit the vertices starting at the root, compute the depth first post    */
/* ordering. While processing, also mark the edges that correspond to back  */
/* edges since they will form a cycle in the graph (recursive)              */
/*--------------------------------------------------------------------------*/
void ORDERED_NODE_ITER::Walk (NODE_INDEX r, mUINT8 visit[])
{
    visit[r] = 1;
    NODE_ITER vitr(g, r);
    
    /* preorder tranversal does *NOT* mark recursive calls */
    if (order == PREORDER) {
	v->Append (r);
    }

    for (NODE_INDEX vi = vitr.First_Succ(); vi != -1; vi = vitr.Next_Succ()) {
	if ((visit[vi] != 1) && (visit[vi] != 2))
	    Walk (vi, visit);
    }

    if (order == POSTORDER) {
	v->Append(r);  
	MarkRecursive(visit, r);
	visit[r] = 2;
    }

    if (order == LEVELORDER) {
      // mark recursive edges
      MarkRecursive(visit, r);
      // give each vertex its level number in the graph, compute
      // the count of vertices at each level
      Compute_Node_Level(r);
      visit[r] = 2;
    }
} // ORDERED_NODE_ITER::Walk

//--------------------------------------------------------------------------
// compute the node level of the vertex and record it in the vertex 
// note, the level is computed during the postorder walk, so the
// algorithm I use is :
// Compute the max level of all the successors of a given node
// The level of the node is (max(level of successors) + 1)
// If there are no children 
// then it must be a leaf node and the level
// of the leaf node is 0. During this process, the count of nodes at
// each level is also recorded. 
//--------------------------------------------------------------------------
void 
ORDERED_NODE_ITER::Compute_Node_Level(NODE_INDEX r)
{
  NODE_ITER vitr(g, r);
  INT level = 0;
  INT succ_level = 0;
  
  for (NODE_INDEX vi = vitr.First_Succ(); vi != -1; vi = vitr.Next_Succ())
    {
      // get the level of the successor
      succ_level = g->Node_Level(vi);

      // if the level of the successor is greater than current level
      // then set the level to be the level of the successor + 1
      if (level <= succ_level)
	level = succ_level+1;

      // record the maximum depth of the call graph
      if (IPA_CG_Max_Depth < level)
	IPA_CG_Max_Depth = level;
    }

  g->Set_Node_Level(r, level);

  Level_Count[level]++;
}

//-------------------------------------------------------------------------
// build the ordering for level ordering, i.e. leaf upwards
//-------------------------------------------------------------------------
void
ORDERED_NODE_ITER::Build_Level_Order()
{
  INT i;
  INT *cur_level_pos;
  
  cur_level_pos = CXX_NEW_ARRAY(INT, IPA_CG_Max_Depth+1,
				Malloc_Mem_Pool);
  cur_level_pos[0] = 0;

  // compute the current  position 
  for (i=1; i<= IPA_CG_Max_Depth; ++i)
    {
      cur_level_pos[i] =  cur_level_pos[i-1] + Level_Count[i-1];
    }

  for (NODE_INDEX vertex=0; vertex<GRAPH_vmax(g); vertex++)
    if (NODE_fcnt(&GRAPH_v_i(g,vertex)) != -1)
      {
	INT level = g->Node_Level(vertex);
	INT i = cur_level_pos[level];
	v->Append(vertex, i);
	++cur_level_pos[level];
      }

  CXX_DELETE_ARRAY (cur_level_pos, Malloc_Mem_Pool);

}
