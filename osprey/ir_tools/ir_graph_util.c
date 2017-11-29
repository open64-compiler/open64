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


/*
 * Originally ported from ipa/common/ip_graph.c 
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

#include <stdio.h>
#include <assert.h>
#include "ir_graph_util.h"


/*------------------------------------------------------------------------*/
/* construct a graph, with the given number of vertices and edges         */
/*------------------------------------------------------------------------*/
GRAPH* build_graph_u(VINDEX vertex_size, EINDEX edge_size, MEM_POOL* m)
{
 GRAPH *g;
 int i;


 GR_ASSERT(vertex_size >= 0, "build_graph_u vertex_size < 0\n");
 GR_ASSERT(edge_size >= 0, "build_graph_u edge_size < 0\n");

 g = (GRAPH*) MEM_POOL_Alloc(m, sizeof(GRAPH));
 GR_ASSERT (g != 0, "build_graph_u g == 0\n");
 bzero(g, sizeof(GRAPH));

 if (vertex_size != 0)    /* build the vertices */
   {
   GRAPH_v(g) = (VERTEX*) MEM_POOL_Alloc(m, sizeof(VERTEX)*vertex_size);
   GR_ASSERT (GRAPH_v(g) != 0, "build_graph_u graph_v(g) == 0\n");
   bzero(GRAPH_v(g), sizeof(VERTEX)*vertex_size);
   }

 if (edge_size != 0)      /* build the edges    */
   {
   GRAPH_e(g) = (EDGE*) MEM_POOL_Alloc(m, sizeof(EDGE)*edge_size);
   GR_ASSERT (GRAPH_e(g) != 0, "build_graph_u graph_e(g) == 0\n");
   bzero(GRAPH_e(g), sizeof(EDGE)*edge_size);
   }

/*----------------------------set up the vertices-------------------------*/
 GRAPH_vcnt(g) = 0;               /* used vertices   */
  
 for (i = 0; i<vertex_size; ++i) 
   {
   VERTEX_from(&GRAPH_v_i(g,i)) = i+1; /* set the linked list of free vertices */
   VERTEX_fcnt(&GRAPH_v_i(g,i)) = -1;  /* node is not being used               */
   }

 /* last free vertex points to no entry  */
 VERTEX_from(&GRAPH_v_i(g,vertex_size-1)) = INVALID_VINDEX;
 GRAPH_vfree(g) = 0;              /* free vertices = total added vertices */
 GRAPH_vmax(g)  = vertex_size;    /* max vertices = new maximum           */


/*----------------------------set up the edges----------------------------*/
 GRAPH_ecnt(g) = 0;               /* used edges      */
 for (i = 0; i<edge_size; ++i) 
   {
   EDGE_nfrom(&GRAPH_e_i(g,i)) = i+1; /* set the linked list of free edges */
   EDGE_from(&GRAPH_e_i(g,i)) = INVALID_VINDEX; /* edge is not being used */
   }
 
 /* last free edge points to no entry */
 EDGE_nfrom(&GRAPH_e_i(g, edge_size-1)) = -1; 
 GRAPH_efree(g) = 0;    /* free edges      */
 GRAPH_emax(g) = edge_size;     /* total edges     */ 


 GRAPH_root(g) = INVALID_VINDEX; /* no root         */
 GRAPH_m(g) = m;
 return g;
}


/*------------------------------------------------------------------------*/
/* construct a graph, in this case the graph utility handles the growth   */
/* of the graph since the user has no clue about what the sizes should be */
/*------------------------------------------------------------------------*/
GRAPH* build_graph(MEM_POOL* m)
{
 GRAPH *g;

 g = (GRAPH*) MEM_POOL_Alloc(m, sizeof(GRAPH));
 bzero(g, sizeof(GRAPH));
 GR_ASSERT (g != 0, "build_graph g == 0\n");

 GRAPH_vcnt(g) = 0;      /* total vertices  */
 GRAPH_ecnt(g) = 0;      /* total edges     */
 GRAPH_vfree(g) = -1;    /* free vertices   */
 GRAPH_efree(g) = -1;    /* free edges      */
 GRAPH_root(g) = INVALID_VINDEX;     /* no root         */
 GRAPH_m(g) = m;
 return g;
}

/*------------------------------------------------------------------------*/
/* grow the graph, more vertices are needed, double it                    */
/*------------------------------------------------------------------------*/
static void grow_vertex(GRAPH *g)
{
 VINDEX max, i;

 MEM_POOL *m = GRAPH_m(g);

 if (GRAPH_vmax(g) < 8) 
  max = 16;
 else
  max = GRAPH_vmax(g)*2;                    /* set max to double current size */
 
 GR_ASSERT(max > GRAPH_vmax(g), "grow_vertex max <= GRAPH_vmax(g)\n");

 GRAPH_v(g) = (VERTEX*) MEM_POOL_Realloc(m,GRAPH_v(g),sizeof(VERTEX)*GRAPH_vmax(g),
                                                    sizeof(VERTEX)*max);
 for (i = GRAPH_vmax(g); i<max; ++i) {
 VERTEX_from(&GRAPH_v_i(g,i)) = i+1;    /* set the linked list of free vertices */
 VERTEX_fcnt(&GRAPH_v_i(g,i)) = -1;     /* node is not being used               */
 }

 /* last free vertex points to no entry */
 VERTEX_from(&GRAPH_v_i(g,max-1)) = INVALID_VINDEX;

 GRAPH_vfree(g) = GRAPH_vmax(g);        /* free vertices = total added vertices */
 GRAPH_vmax(g)  = max;                  /* max vertices = new maximum */

}

/*------------------------------------------------------------------------*/
/* grow the graph, more edges are needed, double it                         */
/*------------------------------------------------------------------------*/
static void grow_edge(GRAPH *g)
{
 EINDEX max, i, diff;
 MEM_POOL *m = GRAPH_m(g);

 if (GRAPH_emax(g) < 8) 
  max = 16;
 else
  max = GRAPH_emax(g)*2;        /* set max to double current size */
  
 
 GR_ASSERT(max > GRAPH_emax(g), "grow_edge graph_emax >= max\n"); 
 GRAPH_e(g) = (EDGE*) MEM_POOL_Realloc(m,GRAPH_e(g), sizeof(EDGE)*GRAPH_emax(g),
                                                   sizeof(EDGE)*max);
/*  diff = max - GRAPH_emax(g); */
/*  bzero(&GRAPH_e_i(g,GRAPH_emax(g)), sizeof(EDGE)*(diff));  */

 for (i = GRAPH_emax(g); i<max; ++i) {
  EDGE_nfrom(&GRAPH_e_i(g,i)) = i+1; /* set the linked list of free edges */
  EDGE_from(&GRAPH_e_i(g,i)) = INVALID_EINDEX; /* edge is not being used */
  }
 
  /* last free vertex points to no entry */
  EDGE_nfrom(&GRAPH_e_i(g, max-1)) = INVALID_EINDEX;

  GRAPH_efree(g) = GRAPH_emax(g); /* free edges = total added edges */
  GRAPH_emax(g) = max;            /* max edges = new maximum */
}


/*------------------------------------------------------------------------*/
/* add a vertex containing user info.      Return a unique vertex number  */
/*------------------------------------------------------------------------*/
VINDEX add_vertex(GRAPH* g, void *user)
{
 VINDEX new_vertex;
 MEM_POOL *m = GRAPH_m(g);

 if (GRAPH_vfree(g) == -1) 
   grow_vertex(g);

 new_vertex = GRAPH_vfree(g);       /* get the free vertex  */

 /* set free vertex to next free vertex */
 GRAPH_vfree(g) = VERTEX_from(&GRAPH_v_i(g,new_vertex));

 /* no edges yet */
 VERTEX_from(&GRAPH_v_i(g,new_vertex)) = INVALID_EINDEX;
 VERTEX_to(&GRAPH_v_i(g,new_vertex)) = INVALID_EINDEX;

 /* add user information */
 VERTEX_user(&GRAPH_v_i(g,new_vertex)) = user; 

 /* no from and to counts */
 VERTEX_fcnt(&GRAPH_v_i(g,new_vertex)) = 0;
 VERTEX_tcnt(&GRAPH_v_i(g,new_vertex)) = 0;

 /* no level yet */
 VERTEX_level(&GRAPH_v_i(g,new_vertex)) = -1;

 /* increment vertex count */
 GRAPH_vcnt(g)++;

 return new_vertex;
}


/*----------------------------------------------------------------------*/
/* add an edge from vertex (from) to vertex (to)                        */
/* containing user info. Return a unique edge number                    */
/*----------------------------------------------------------------------*/
EINDEX add_edge(GRAPH *g, VINDEX from, VINDEX to, void *user)
{
 EINDEX new_edge, e2;
 MEM_POOL *m = GRAPH_m(g);

 GR_ASSERT(is_vertex(g,from), "add_edge is_vertex(g, from\n");
 GR_ASSERT(is_vertex(g,from), "add_edge is_vertex(g, to\n");

 /* are there any free edges? */
 if(GRAPH_efree(g) == -1)

 /* grow the edge list if no free edges */
 grow_edge(g);     

 /* get a free edge */
 new_edge = GRAPH_efree(g); 

 /* reset the free edge pointer */
 GRAPH_efree(g)=  EDGE_nfrom(&GRAPH_e_i(g,new_edge)); 

 /* store the user information  */
 EDGE_user(&GRAPH_e_i(g,new_edge)) = user;

 /* from vertex is = from       */
 EDGE_from(&GRAPH_e_i(g,new_edge)) = from;
 
 /* to vertex is = to           */ 
 EDGE_to(&GRAPH_e_i(g,new_edge)) = to;

 /* incr. from count for from vertex */
 VERTEX_fcnt(&GRAPH_v_i(g,from))++;

 /* incr. to count for to vertex */
 VERTEX_tcnt(&GRAPH_v_i(g,to))++;

 /* incr. total used edge count  */
 GRAPH_ecnt(g)++;

 /* set up the list of from edges for the from vertex */
 e2 =  VERTEX_from(&GRAPH_v_i(g,from));
 EDGE_nfrom(&GRAPH_e_i(g,new_edge)) = e2;
 VERTEX_from(&GRAPH_v_i(g,from)) = new_edge;

 /* set up the list of to edges for the to vertex */
 e2 = VERTEX_to(&GRAPH_v_i(g,to));
 EDGE_nto(&GRAPH_e_i(g,new_edge)) = e2;
 VERTEX_to(&GRAPH_v_i(g,to)) = new_edge;

 /* set the recursive edge field to be zero */
 EDGE_etype(&GRAPH_e_i(g,new_edge)) = 0;

 return new_edge;
}

/*-----------------------------------------------------------------------*/
/* check to see if it is a valid vertex number                           */
/* if the vertex number is less than the total count and it exists       */
/*-----------------------------------------------------------------------*/
BOOL is_vertex(GRAPH *g, VINDEX vertex)
{
 return ( ( vertex < GRAPH_vmax(g) )
       && ( VERTEX_fcnt(&GRAPH_v_i(g,vertex)) != INVALID_VINDEX ) );
}

/*-----------------------------------------------------------------------*/
/* get the next vertex                                                   */
/*-----------------------------------------------------------------------*/
VINDEX next_vertex(GRAPH *g, VINDEX vertex)
{
  GR_ASSERT(is_vertex(g, vertex), "next_vertex is_vertex(g,vertex)\n");

  while (1) {
    if (is_vertex(g, ++vertex))
      return vertex;
    else
      if (vertex >= GRAPH_vmax(g))
	return INVALID_VINDEX;
  }
}

/*---------------------------------------------------------------------*/
/* Delete a given edge                                                 */
/*---------------------------------------------------------------------*/
void delete_edge(GRAPH *g, EINDEX edge)
{

 VINDEX from_vertex, to_vertex;
 EINDEX next_edge, head_edge;

 GR_ASSERT(is_edge(g, edge), "delete_edge is_edge\n");

 /* update the information for the from vertex */
 /* get the from vertex */
 from_vertex =  EDGE_from(&GRAPH_e_i(g,edge));

 /* decrement from count */
 VERTEX_fcnt(&GRAPH_v_i(g,from_vertex))--;

 /* get the next edge in linked list */
 next_edge = EDGE_nfrom(&GRAPH_e_i(g,edge));

 /* get the first edge in linked list */
 head_edge = VERTEX_from(&GRAPH_v_i(g,from_vertex));
 
 /* if the head is set to edge */
 if (head_edge == edge)                  

  /* set it to next from edge */
  VERTEX_from(&GRAPH_v_i(g,from_vertex)) = next_edge;

 /* else walk the list of edges */
 else
   {   
   /* unlink the edge from listofedges */
   while (EDGE_nfrom(&GRAPH_e_i(g,head_edge)) != edge)
     head_edge = EDGE_nfrom(&GRAPH_e_i(g,head_edge));
   EDGE_nfrom(&GRAPH_e_i(g,head_edge)) = next_edge; 
   }

 /*---- update the information for the to vertex ----*/

 /* get the to vertex */
 to_vertex =  EDGE_to(&GRAPH_e_i(g,edge));

 /* decrement to count */
 VERTEX_tcnt(&GRAPH_v_i(g,to_vertex))--;

 /* get the next edge in linked list */
 next_edge = EDGE_nto(&GRAPH_e_i(g,edge));

 /* get the first edge in linked list */
 head_edge = VERTEX_to(&GRAPH_v_i(g,to_vertex));

 /* if head is set to edge */
 if (head_edge == edge)
  /* set it to next to edge */
  VERTEX_to(&GRAPH_v_i(g,to_vertex)) = next_edge;

 else                                  
  /* else walk the list of edges */
  {
   /* unlink the edge from the list of to edges */
   while (g->e[head_edge].nto != edge)  
     head_edge = EDGE_nto(&GRAPH_e_i(g,head_edge));
     EDGE_nto(&GRAPH_e_i(g,head_edge)) = next_edge;
  }
  
 /* add the free edge to the linked list of free edges */
  EDGE_nfrom(&GRAPH_e_i(g,edge)) = GRAPH_efree(g);
  EDGE_from(&GRAPH_e_i(g,edge)) = INVALID_VINDEX;
  GRAPH_efree(g) = edge;
  GRAPH_ecnt(g)--;                            /* decrement edge count */
}

/*---------------------------------------------------------------------*/
/* Delete a given vertex                                               */
/*---------------------------------------------------------------------*/
void* delete_vertex(GRAPH *g, VINDEX vertex)
{
 void *user;
 EINDEX from, nfrom, to, nto;
  
 GR_ASSERT(is_vertex(g, vertex), "delete vertex is_vertex\n");

 user = VERTEX_user(&GRAPH_v_i(g,vertex));
 
 /* delete the from edges */
  from = VERTEX_from(&GRAPH_v_i(g,vertex));
  while (from != INVALID_EINDEX)
    {
    nfrom = EDGE_nfrom(&GRAPH_e_i(g,from));
    delete_edge(g, from);
    from = nfrom;
    }

 /* delete the to edges */
 to =  VERTEX_to(&GRAPH_v_i(g,vertex));
 while (to != INVALID_EINDEX)
   {
   nto = EDGE_nto(&GRAPH_e_i(g,to));
   delete_edge(g, to);
   to = nto;
   }

 VERTEX_fcnt(&GRAPH_v_i(g,vertex)) = -1;
 VERTEX_from(&GRAPH_v_i(g,vertex)) = GRAPH_vfree(g);
 GRAPH_vfree(g) = vertex;
 GRAPH_vcnt(g)--;

 return user;

}

/*---------------------------------------------------------------------*/
/* check to see if it is a valid edge                                  */
/*---------------------------------------------------------------------*/
BOOL is_edge(GRAPH *g, EINDEX edge)
{
  return ( ( edge < GRAPH_emax(g) )
	&& EDGE_from(&GRAPH_e_i(g,edge)) != INVALID_VINDEX );
}

/*---------------------------------------------------------------*/
/* get the user info attached to the vertex                      */
/*---------------------------------------------------------------*/
void* get_vertex(GRAPH *g, VINDEX vertex)
{
  GR_ASSERT(is_vertex(g,vertex), "get_vertex\n");

  return (VERTEX_user(&GRAPH_v_i(g,vertex)));
}

/*---------------------------------------------------------------*/
/* get the count of incoming edges                               */
/*---------------------------------------------------------------*/
int num_preds(GRAPH *g, VINDEX vertex)
{
  GR_ASSERT(is_vertex(g,vertex), "num_preds is_vertex\n");

  return (VERTEX_tcnt(&GRAPH_v_i(g,vertex)));
}

/*---------------------------------------------------------------*/
/* get the count of outgoing edges                               */
/*---------------------------------------------------------------*/
int num_succs(GRAPH *g, VINDEX vertex)
{
  GR_ASSERT(is_vertex(g,vertex), "num_succs is_vertex\n");

  return (VERTEX_fcnt(&GRAPH_v_i(g,vertex)));
}

/*---------------------------------------------------------------*/
/* get an edge given the vertices                                */
/*---------------------------------------------------------------*/
void* get_edge(GRAPH *g, VINDEX from, VINDEX to)
{
  EINDEX e;
  

  GR_ASSERT(is_vertex(g, from), "get_edge is_vertex from\n");
  GR_ASSERT(is_vertex(g, to), "get_edge is_vertex to\n");

  e = VERTEX_from(&GRAPH_v_i(g, from));
  
  while ( e != INVALID_EINDEX ) {
    if(EDGE_to(&GRAPH_e_i(g,e)) == to) 
      break;
    e = (EDGE_nfrom(&GRAPH_e_i(g,e)));
  }
  return EDGE_user(&GRAPH_e_i(g,e));
}

/*---------------------------------------------------------------*/
/* get the count of edges from->to                               */
/*---------------------------------------------------------------*/
int 
edge_count(GRAPH* g, VINDEX from, VINDEX to)
{
  int count= 0;
  EINDEX e;
  
  GR_ASSERT(is_vertex(g, from), "edge_count is_vertex from\n");
  GR_ASSERT(is_vertex(g, to), "edge_count is_vertex to\n");
  
  e = VERTEX_from(&GRAPH_v_i(g, from));
  
  while ( e != INVALID_EINDEX ) {
    if(EDGE_to(&GRAPH_e_i(g,e)) == to) 
      ++count;
    e = (EDGE_nfrom(&GRAPH_e_i(g,e)));
  }
  return count;
}

/*---------------------------------------------------------------*/
/* get an edge given the vertices                                */
/*---------------------------------------------------------------*/
void*
get_edge_u(GRAPH *g, EINDEX e)
{
  GR_ASSERT(is_edge(g, e), "get_edge_u is_edge\n");

  return EDGE_user(&GRAPH_e_i(g,e));
}


/* set the edge user value */
void
set_edge_u (GRAPH *g, EINDEX e, void *user)
{
    EDGE_user(&GRAPH_e_i(g,e)) = user;
}

/*---------------------------------------------------------------*/
/* initialize a vertex_iterator                                  */
/*---------------------------------------------------------------*/
V_ITER* create_vertex_iter(GRAPH *g, VINDEX vertex, MEM_POOL* m)
{
 V_ITER *v_i;
 VERTEX *v_ptr;
 v_i = (V_ITER*) MEM_POOL_Alloc(m, sizeof(V_ITER));


 V_ITER_g(v_i) = g;
 V_ITER_c_v(v_i) = vertex;
 v_ptr = GRAPH_v(g);
 V_ITER_from_e(v_i) = VERTEX_from(&v_ptr[vertex]);
 V_ITER_to_e(v_i) = VERTEX_to(&v_ptr[vertex]);
 V_ITER_fcnt(v_i) = VERTEX_fcnt(&v_ptr[vertex]);
 V_ITER_tcnt(v_i) = VERTEX_tcnt(&v_ptr[vertex]);
 V_ITER_nfrom(v_i) = -1;
 V_ITER_nto(v_i) = -1;
 V_ITER_m(v_i) = m;
 return v_i;
}

/*---------------------------------------------------------------*/
/* get first predeccessor                                        */
/*---------------------------------------------------------------*/
VINDEX first_v_preds(V_ITER *v_i)
{
 EINDEX e;
 VINDEX from;

 /* get the first to edge */
 if ( V_ITER_to_e(v_i) == INVALID_EINDEX )
   {

   MEM_POOL_FREE(V_ITER_m(v_i),v_i);

   return INVALID_VINDEX;
   }

 /* get the first to edge */
 e =  V_ITER_to_e(v_i);
 /* return it's from vertex */
 from = EDGE_from(&GRAPH_e_i(V_ITER_g(v_i),e));  
 V_ITER_nto(v_i) =  EDGE_nto(&GRAPH_e_i(V_ITER_g(v_i),e));

 /* store the current edge */
 V_ITER_c_e(v_i) = e;
 return from; 
}

/*---------------------------------------------------------------*/
/* get next predeccessor                                         */
/*---------------------------------------------------------------*/
VINDEX  next_v_preds(V_ITER *v_i)
{
 EINDEX e;
 VINDEX from;
 
 /* get the next to edge */
 if(V_ITER_nto(v_i) == -1)
   {

   MEM_POOL_FREE(V_ITER_m(v_i),v_i);

   return INVALID_VINDEX;
   }

 /* get the next to edge */
 e = V_ITER_nto(v_i);

 /* return it's from vertex */
 from = EDGE_from(&GRAPH_e_i(V_ITER_g(v_i), e));
 V_ITER_nto(v_i) = EDGE_nto(&GRAPH_e_i(V_ITER_g(v_i),e));

 /* store the current edge */
 V_ITER_c_e(v_i) = e;
 return from;
}

/*---------------------------------------------------------------*/
/* get the first successor vertex                                */
/*---------------------------------------------------------------*/
VINDEX first_v_succs(V_ITER *v_i)
{
  EINDEX e;
  VINDEX to;

 /* get the first from edge */
 if ( V_ITER_from_e(v_i) == INVALID_EINDEX )
   {
   MEM_POOL_FREE(V_ITER_m(v_i),v_i);
   return INVALID_VINDEX;
   }

 /* return it's to vertex */
 e =  V_ITER_from_e(v_i);
 to = EDGE_to(&GRAPH_e_i(V_ITER_g(v_i),e));  
 V_ITER_nfrom(v_i) =  EDGE_nfrom(&GRAPH_e_i(V_ITER_g(v_i),e));

 /* store the current edge */
 V_ITER_c_e(v_i) = e;
 return to;
}

/*---------------------------------------------------------------*/
/* get the next successor vertex                                 */
/*---------------------------------------------------------------*/
VINDEX next_v_succs(V_ITER *v_i)
{
 EINDEX ei;
 VINDEX to;
 
 /* get the next from edge */
 if(V_ITER_nfrom(v_i) == -1)
   {
   MEM_POOL_FREE(V_ITER_m(v_i),v_i);

   return INVALID_VINDEX;
   }

 /* return it's to vertex */
 ei = V_ITER_nfrom(v_i);
 to = EDGE_to(&GRAPH_e_i(V_ITER_g(v_i), ei));
 V_ITER_nfrom(v_i) = EDGE_nfrom(&GRAPH_e_i(V_ITER_g(v_i),ei));

 /* store the current edge */
 V_ITER_c_e(v_i) = ei;
 return to;
}

/*---------------------------------------------------------------*/
/* get the level of the vertex                                   */
/*---------------------------------------------------------------*/
int
get_vertex_level(GRAPH* g, VINDEX v)
{
    return (VERTEX_level(&GRAPH_v_i(g,v)));
}

/*---------------------------------------------------------------*/
/* set the level of the vertex                                   */
/*---------------------------------------------------------------*/
void
set_vertex_level(GRAPH* g, VINDEX v, int level)
{
    VERTEX_level(&GRAPH_v_i(g,v)) = level;
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
static  char *Malloc_Mem_Pool;
static  int lvl;

static void
Search ( GRAPH *g, VINDEX v, DFN *d, BOOL *visit, EINDEX ei )
{
  EINDEX edge;
  VINDEX vtx;
  V_ITER *v_iter;
  int tmp;

  visit[v] = VISITED;	/* Mark the vertex as visited */
  set_vertex_level(g, v, lvl);

  lvl++;
  /* Create a vertex iterator: */
  v_iter = create_vertex_iter ( g, v, Malloc_Mem_Pool );

  for ( vtx=first_v_succs(v_iter);
	vtx != INVALID_VINDEX ;
	vtx=next_v_succs(v_iter) )
  {
     /* Recursively search from vtx if it has not been visited: */
    if ( !visit[vtx] ) {
      Search ( g, vtx, d, visit, V_ITER_c_e(v_iter) ); 
    }
  }
  lvl--;

  /* Add v to the order at DFN_first: */
  --DFN_first(d);
  tmp = DFN_first(d);
  DFN_v_list_i ( d, tmp ) = v;

  if (ei != INVALID_VINDEX) {
    void *u;
   
    u =  get_edge_u(g, ei);
    DFN_user_i(d, tmp) = u;
  }
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
  VINDEX vertex_count = GRAPH_vcnt(g);
  EINDEX edge_count = GRAPH_ecnt(g);
  int vertex_max = GRAPH_vmax(g);
  DFN *d;
  BOOL *visit;

  /* Validate and trace: */
  GR_ASSERT ( GRAPH_root(g) != INVALID_VINDEX, "Invalid root g for graph\n");
printf ("Depth_First_Ordering: vertex count = %d \n", vertex_count );

  /* Allocate the DFN struct: */
  d = (DFN*) MEM_POOL_Alloc ( m, sizeof(DFN) );
  GR_ASSERT(d != NULL, "Depth_First_Ordering: d" );

  DFN_v_list(d) = (VINDEX *)
		MEM_POOL_Alloc ( m, sizeof(VINDEX) * vertex_count );
  GR_ASSERT(DFN_v_list(d) != NULL, "Depth_First_Ordering: list" );

  DFN_user(d) = (void *)
		MEM_POOL_Alloc ( m, sizeof(void *) * vertex_count );
  GR_ASSERT(DFN_user(d) != NULL, "Depth_First_Ordering: user" );

  /* Allocate work array to keep track of visited vertices: */
  visit = (BOOL *) MEM_POOL_Alloc ( m, sizeof(BOOL)*vertex_max );
  GR_ASSERT ( visit != NULL, "Depth_First_Ordering: visit" );

  bzero ( visit, sizeof(BOOL)*vertex_max );

  /* Initialize the DFN struct -- set end and first to point beyond
   * the first element:
   */
  DFN_first(d) = DFN_end(d) = vertex_count;

  /* Go do the depth-first walk from the root: */
  Search ( g, GRAPH_root(g), d, visit, INVALID_EINDEX );

  /* Free work array: */
  MEM_POOL_FREE(m, visit);

  return(d);
}


void Print_Pred(GRAPH *g, VINDEX v, void (*prn)())
{
  V_ITER *vi;
  VINDEX vtx;
  void * dummy = 0;
  int *u, total = 0;
 
  vi = create_vertex_iter(g, v, dummy);
  
  printf("  [");
  for (vtx = first_v_preds(vi);
       vtx != INVALID_VINDEX;
       vtx = next_v_preds(vi)) {
    (*prn)(vtx);
    u = (int *)get_edge_u(g, V_ITER_c_e(vi));
    GR_ASSERT(u, "user field in pred NULL");
    printf("/%d ", *u);
    total += *u;
  }
  printf(" - total %d]", total);
}


void Print_DFN (DFN* d, GRAPH *g, void (*prn)(), void (*prn_c)() )
{
  VINDEX i, j;
  long *k;

  printf ("Depth First Numbering: [%d..%d)\n",DFN_first(d), DFN_end(d) );
  for ( i = DFN_first(d); i< DFN_end(d); i++ ) {
    for (j = 0; j < get_vertex_level(g,DFN_v_list_i(d,i)); j++) 
      printf("+ ");
    (*prn)(DFN_v_list_i(d,i));
    Print_Pred(g, DFN_v_list_i(d,i), prn);
    printf("\n");
  }
  printf("\n");
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

