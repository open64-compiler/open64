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


/* ====================================================================
 * ====================================================================
 *
 * Module: ip_graph.c
 * Author: Seema Hiranandani
 *
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/common/ip_graph.cxx,v $
 *
 * Revision history:
 *  19-Aug-94 - Original Version
 *
 * Description:
 *
 * This module contains a routines to construct and manage graphs. 
 * The edge and vertex lists of the graph grow as needed.
 *
 * There is a depth first vertex ordering mechanism along with 
 * successor and predecessor graph iterators which will be needed for
 * the data flow iterative solver.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "erglob.h"
#include "cxx_memory.h"
#include "tracing.h"
#include "ip_graph.h"

/*------------------------------------------------------------------------*/
/* construct a graph, with the given number of vertices and edges         */
/*------------------------------------------------------------------------*/
void
GRAPH::Build()
{

  NODE_INDEX i;

  /* build the vertices */
  if (vmax != 0) {
    v = (GRAPH::NODE_TYPE *) 
      MEM_POOL_Alloc(m, sizeof(GRAPH::NODE_TYPE) * vmax);
    BZERO(v, sizeof(GRAPH::NODE_TYPE) * vmax);
  }
  
  /* build the edges    */
  if (emax != 0) {
    e = (GRAPH::EDGE_TYPE *)
      MEM_POOL_Alloc(m, sizeof(GRAPH::EDGE_TYPE) * emax);
    BZERO(e, sizeof(GRAPH::EDGE_TYPE) * emax);
  }

  /*----------------------------set up the vertices-----------------------*/
  for (i = 0; i < vmax; ++i) {
    NODE_from(&v[i]) = i+1; /* set linked list of free vertices */
    NODE_fcnt(&v[i]) = -1;  /* node is not being used           */
  }
  /* last free vertex points to no entry  */
  NODE_from(&v[vmax-1]) = INVALID_NODE_INDEX;

  /*----------------------------set up the edges--------------------------*/
  for (i = 0; i < emax; ++i) {
    EDGE_nfrom(&e[i]) = i+1; /* set linked list of free edges */
    EDGE_from(&e[i]) = INVALID_NODE_INDEX; /* edge is not being used */
  }
  /* last free edge points to no entry */
  EDGE_nfrom(&e[emax-1]) = -1; 
}


/*------------------------------------------------------------------------*/
/* grow the graph, more vertices are needed, double it                    */
/*------------------------------------------------------------------------*/
void 
GRAPH::Grow_Node_Array()
{
  NODE_INDEX new_max = (vmax < 8 ? 16 : vmax * 2); // double the current size

  v = (GRAPH::NODE_TYPE *)
    MEM_POOL_Realloc (m, v, 
                      sizeof(GRAPH::NODE_TYPE) * vmax, 
                      sizeof(GRAPH::NODE_TYPE) * new_max);

  for (NODE_INDEX i = vmax; i < new_max; ++i) {
    NODE_from(&v[i]) = i+1;      /* set the linked list of free vertices */
    NODE_fcnt(&v[i]) = -1;       /* node is not being used               */
  }
  /* last free vertex points to no entry */
  NODE_from(&v[new_max-1]) = INVALID_NODE_INDEX;

  vfree = vmax;         /* free vertices = total added vertices */
  vmax  = new_max;      /* max vertices = new maximum           */
}


/*------------------------------------------------------------------------*/
/* grow the graph, more edges are needed, double it                         */
/*------------------------------------------------------------------------*/
void 
GRAPH::Grow_Edge_Array()
{
  EDGE_INDEX new_max = (emax < 8 ? 16 : emax * 2); // double the current size
  
  e = (GRAPH::EDGE_TYPE *)
    MEM_POOL_Realloc (m, e,
		      sizeof(GRAPH::EDGE_TYPE) * emax,
		      sizeof(GRAPH::EDGE_TYPE) * new_max);

  for (EDGE_INDEX i = emax; i < new_max; ++i) {
    EDGE_nfrom(&e[i]) = i+1;    /* set the linked list of free edges */
    EDGE_from(&e[i]) = INVALID_EDGE_INDEX; /* edge is not being used */
  }
  /* last free vertex points to no entry */
  EDGE_nfrom(&e[new_max-1]) = INVALID_EDGE_INDEX;

  efree = emax;         /* free edges = total added edges */
  emax  = new_max;      /* max edges = new maximum */
}


/*------------------------------------------------------------------------*/
/* add a vertex containing user info.      Return a unique vertex number  */
/*------------------------------------------------------------------------*/
NODE_INDEX 
GRAPH::Add_Node(void *user)
{
  // are there any free nodes? grow the node list if necessary
  if (vfree == -1) {
    Grow_Node_Array();
  }
 
  // get the free vertex
  NODE_INDEX new_node_idx = vfree; 

  // set free vertex to next free vertex
  vfree = NODE_from(&v[new_node_idx]);

  // add user information
  NODE_user(&v[new_node_idx]) = user; 

  // no edges, from & to counts, or level yet
  NODE_from(&v[new_node_idx])  = INVALID_EDGE_INDEX;
  NODE_to(&v[new_node_idx])    = INVALID_EDGE_INDEX;
  NODE_fcnt(&v[new_node_idx])  = 0;
  NODE_tcnt(&v[new_node_idx])  = 0;
  NODE_level(&v[new_node_idx]) = -1;

  // increment vertex count
  vcnt++;

  return new_node_idx;
}


/*----------------------------------------------------------------------*/
/* add an edge from vertex (from) to vertex (to)                        */
/* containing user info. Return a unique edge number                    */
/*----------------------------------------------------------------------*/
EDGE_INDEX 
GRAPH::Add_Edge(NODE_INDEX from, NODE_INDEX to, void *user)
{
  // are there any free edges? grow the edge list if necessary
  if(efree == -1) {
    Grow_Edge_Array();
  }

  // get a free edge
  EDGE_INDEX new_edge_idx = efree; 

  // reset the free edge pointer
  efree = EDGE_nfrom(&e[new_edge_idx]); 

  // store the user information
  EDGE_user(&e[new_edge_idx]) = user;

  // from vertex is = from
  EDGE_from(&e[new_edge_idx]) = from;
 
  // to vertex is = to  
  EDGE_to(&e[new_edge_idx]) = to;

  // increment from count for from vertex
  NODE_fcnt(&v[from])++;

  // increment to count for to vertex
  NODE_tcnt(&v[to])++;

  // incr. total used edge count
  ecnt++;

  // set up the list of from edges for the from vertex
  EDGE_nfrom(&e[new_edge_idx]) = NODE_from(&v[from]);
  NODE_from(&v[from]) = new_edge_idx;

  // set up the list of to edges for the to vertex
  EDGE_nto(&e[new_edge_idx]) = NODE_to(&v[to]);
  NODE_to(&v[to]) = new_edge_idx;

  // set the recursive edge field to be zero
  EDGE_etype(&e[new_edge_idx]) = 0;

  return new_edge_idx;
}


/*---------------------------------------------------------------------*/
/* Delete a given edge                                                 */
/*---------------------------------------------------------------------*/
void 
GRAPH::Delete_Edge(EDGE_INDEX edge)
{
  /* update the information for the from vertex */
  /* get the from vertex */
  NODE_INDEX from_vertex =  EDGE_from(&e[edge]);

 /* decrement from count */
  NODE_fcnt(&v[from_vertex])--;

  /* get the next edge in linked list */
  EDGE_INDEX next_edge = EDGE_nfrom(&e[edge]);

  /* get the first edge in linked list */
  EDGE_INDEX head_edge = NODE_from(&v[from_vertex]);
 
  /* if the head is set to edge */
  /* set it to next from edge */
  if (head_edge == edge) {
    NODE_from(&v[from_vertex]) = next_edge;
  }
  else {
    /* else walk the list of edges */
    /* unlink the edge from listofedges */
    while (EDGE_nfrom(&e[head_edge]) != edge) {
      head_edge = EDGE_nfrom(&e[head_edge]);
    }
    EDGE_nfrom(&e[head_edge]) = next_edge; 
  }

  /*---- update the information for the to vertex ----*/

 /* get the to vertex */
  NODE_INDEX to_vertex =  EDGE_to(&e[edge]);

  /* decrement to count */
  NODE_tcnt(&v[to_vertex])--;

  /* get the next edge in linked list */
  next_edge = EDGE_nto(&e[edge]);

  /* get the first edge in linked list */
  head_edge = NODE_to(&v[to_vertex]);

  /* if head is set to edge */
  /* set it to next to edge */
  if (head_edge == edge) {
    NODE_to(&v[to_vertex]) = next_edge;
  }
  else {
    /* else walk the list of edges */
    /* unlink the edge from the list of to edges */
    while (EDGE_nto(&e[head_edge]) != edge) {
      head_edge = EDGE_nto(&e[head_edge]);
    }
    EDGE_nto(&e[head_edge]) = next_edge;
  }
  
  /* add the free edge to the linked list of free edges */
  EDGE_nfrom(&e[edge]) = efree;
  EDGE_from(&e[edge]) = INVALID_NODE_INDEX;
  efree = edge;
  
  /* decrement edge count */
  ecnt--;      
}


/*---------------------------------------------------------------------*/
/* Delete a given vertex                                               */
/*---------------------------------------------------------------------*/
void* 
GRAPH::Delete_Node(NODE_INDEX vertex)
{
  void* user = NODE_user(&v[vertex]);
 
 /* delete the from edges */
  EDGE_INDEX nfrom;
  EDGE_INDEX from = NODE_from(&v[vertex]);
  while (from != INVALID_EDGE_INDEX) {
    nfrom = EDGE_nfrom(&e[from]);
    Delete_Edge(from);
    from = nfrom;
  }

  /* delete the to edges */
  EDGE_INDEX nto;
  EDGE_INDEX to =  NODE_to(&v[vertex]);
  while (to != INVALID_EDGE_INDEX) {
    nto = EDGE_nto(&e[to]);
    Delete_Edge(to);
    to = nto;
  }

  /* add the free node to the linked list of free nodes */
  NODE_fcnt(&v[vertex]) = -1;
  NODE_from(&v[vertex]) = vfree;
  vfree = vertex;

  /* decrement node count */
  vcnt--;

  return user;
}


/*---------------------------------------------------------------*/
/* get first predeccessor                                        */
/*---------------------------------------------------------------*/
NODE_INDEX 
NODE_ITER::First_Pred()
{
  // get the first to-edge
 if ( to_e == INVALID_EDGE_INDEX )  {
   return INVALID_NODE_INDEX;
 }

 // return its source vertex
 NODE_INDEX from = EDGE_from(&GRAPH_e_i(g, to_e));  

 // store the next to-edge
 nto = EDGE_nto(&GRAPH_e_i(g, to_e));

 // store the current edge
 c_e = to_e;

 return from; 
}


/*---------------------------------------------------------------*/
/* get next predeccessor                                         */
/*---------------------------------------------------------------*/
NODE_INDEX  
NODE_ITER::Next_Pred()
{
  // get the next to-edge
  if (nto == -1) {
    return INVALID_NODE_INDEX;
  }

  // get the next to-edge
  EDGE_INDEX e = nto;

  // return its source vertex 
  NODE_INDEX from = EDGE_from(&GRAPH_e_i(g, e));
  
  // store the next to-edge
  nto = EDGE_nto(&GRAPH_e_i(g, e));

  // store the current edge */
  c_e = e;
  
  return from;
}


/*---------------------------------------------------------------*/
/* get the first successor vertex                                */
/*---------------------------------------------------------------*/
NODE_INDEX 
NODE_ITER::First_Succ()
{
  // get the first from-edge
  if ( from_e == INVALID_EDGE_INDEX )  {
    return INVALID_NODE_INDEX;
  }

  // return its sink vertex
  NODE_INDEX to = EDGE_to(&GRAPH_e_i(g, from_e));  
  
  // store the next from-edge
  nfrom =  EDGE_nfrom(&GRAPH_e_i(g, from_e));

  // store the current edge
  c_e = from_e;

  return to;
}

/*---------------------------------------------------------------*/
/* get the next successor vertex                                 */
/*---------------------------------------------------------------*/
NODE_INDEX 
NODE_ITER::Next_Succ()
{
  // get the next from-edge
  if ( nfrom == -1 ) {
    return INVALID_NODE_INDEX;
  }

  EDGE_INDEX e = nfrom;

  // return it's to vertex
  NODE_INDEX to = EDGE_to(&GRAPH_e_i(g, e));

  // store the next from-edge
  nfrom = EDGE_nfrom(&GRAPH_e_i(g, e));

  // store the current edge
  c_e = e;

  return to;
}


/* ====================================================================
 *
 * Search
 *
 * Helper search function for depth first ordering, as described in the
 * dragon book, page 662.
 *
 * This function recursively searches the subgraph rooted at vertex v,
 * depth-first, finally adding v to the beginning of the node order
 * when it has added all its successors.  The effect is that a vertex
 * will always precede all of its successors in the list, and will
 * always follow all of its predecessors (ignoring back-edges in the
 * graph).  Therefore, both the resulting node order and its reversal
 * are topological sorts.
 *
 * When called, the DFN struct contains the vertexes already added
 * to the order, with DFN_first being the smallest index where a
 * vertex has been added.  The search uses a work array of flags
 * (visit) to keep track of whether a vertex has already been visited.
 *
 * ====================================================================
 */

#define VISITED TRUE

static void
Search ( GRAPH *g, NODE_INDEX v, DFN *d, mBOOL *visit )
{
  visit[v] = VISITED;	/* Mark the vertex as visited */

  /* Create a vertex iterator: */
  NODE_ITER v_iter(g, v);

  for ( NODE_INDEX vtx = v_iter.First_Succ();
	vtx != INVALID_NODE_INDEX ;
	vtx = v_iter.Next_Succ() )
  {
     /* Recursively search from vtx if it has not been visited: */
    if ( !visit[vtx] ) {
      Search ( g, vtx, d, visit ); 
    }
  }

  /* Add v to the order at DFN_first: */
  DFN_v_list_i ( d, --DFN_first(d) ) = v;
}

/* ====================================================================
 *
 * Depth_First_Ordering
 *
 * Create a topological sort graph traversal order based on a depth
 * first ordering (see the dragon book, page 660).  Such an order
 * helps to speed up iterative data flow algorithms and can also be
 * used to detect loops (i.e. back-edges in the graph).
 *
 * ====================================================================
 */

DFN *
Depth_First_Ordering ( GRAPH *g, MEM_POOL* m )
{
  NODE_INDEX vertex_count = GRAPH_vcnt(g);
  EDGE_INDEX edge_count = GRAPH_ecnt(g);
  INT vertex_max = GRAPH_vmax(g);
  DFN *d;
  mBOOL *visit;

  /* Validate and trace: */
  Is_True ( GRAPH_root(g) != INVALID_NODE_INDEX,
	    ( "Invalid root for graph at %lx\n", g ) );
  if ( Get_Trace ( TP_IPA, 8 ) ) {
    fprintf ( TFile, "Depth_First_Ordering: vertex count = %d \n",
	      vertex_count );
  }

  /* Allocate the DFN struct: */
  d = (DFN*) MEM_POOL_Alloc ( m, sizeof(DFN) );
  if ( d == NULL ) {
    ErrMsg ( EC_No_Mem, "Depth_First_Ordering: d" );
  }
  DFN_v_list(d) = (NODE_INDEX*)
		MEM_POOL_Alloc ( m, sizeof(NODE_INDEX) * vertex_count );
  if ( DFN_v_list(d) == NULL ) {
    ErrMsg ( EC_No_Mem, "Depth_First_Ordering: list" );
  }

  /* Allocate work array to keep track of visited vertices: */
  visit = (mBOOL*) MEM_POOL_Alloc ( m, sizeof(mBOOL)*vertex_max );
  if ( visit == NULL ) {
    ErrMsg ( EC_No_Mem, "Depth_First_Ordering: visit" );
  }
  BZERO ( visit, sizeof(mBOOL)*vertex_max );

  /* Initialize the DFN struct -- set end and first to point beyond
   * the first element:
   */
  DFN_first(d) = DFN_end(d) = vertex_count;

  /* Go do the depth-first walk from the root: */
  Search ( g, GRAPH_root(g), d, visit );

  /* Free work array: */
  MEM_POOL_FREE(m, visit);

  return(d);
}

/* ====================================================================
 *
 * Print_DFN
 *
 * Print the vertices of a depth first numbering.
 *
 * ====================================================================
 */

void Print_DFN ( const FILE *fp, DFN* d )
{
  NODE_INDEX i, j;

  fprintf ( (FILE *)fp, "%sDepth First Numbering: [%d..%d)\n%s",
	    SBar, DFN_first(d), DFN_end(d), SBar );
  for ( i = DFN_first(d), j=1; i< DFN_end(d); i++, j++ ) {
    fprintf ( (FILE *)fp, " %3d", DFN_v_list_i(d,i) );
    if ( j == 18 ) {
      fprintf ( (FILE *)fp, "\n" );
      j = 0;
    }
  }
  fprintf ( (FILE *)fp, "\n%s", SBar );
}

/* ====================================================================
 *
 * Free_DFN
 *
 * Free the memory used by a depth first numbering.  Must be
 * called with the same MEM_POOL that was used to allocate it.
 *
 * ====================================================================
 */

void
Free_DFN ( DFN* d, MEM_POOL* m )
{
  MEM_POOL_FREE ( m, DFN_v_list(d) );
  MEM_POOL_FREE ( m, d );
}
