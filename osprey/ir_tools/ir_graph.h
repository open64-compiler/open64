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
 * Module: ip_graph.h
 * Author: Seema Hiranandani
 *
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:22:18 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/ir_tools/ir_graph.h,v $
 *
 * Revision history:
 *  19-Aug-95 - Original Version
 *
 * Description:
 *
 * This module contains data structures for a graph abstraction.
 * This graph is used to construct, for instance, a call graph.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef ip_graph_INCLUDED
#define ip_graph_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 *
 * VERTEX / EDGE / GRAPH : Directed graph implementation
 *
 * A GRAPH is a set of VERTEX objects, connected by a set of EDGE
 * objects.  These sets are implemented as a vector of VERTEXes and a
 * vector of EDGEs, with all references being indices into these
 * vectors.  The sets are allowed to grow (by adding VERTEXes or EDGEs)
 * or to shrink (by deleting them).  Growth may cause reallocation of
 * the vector.  To minimize reallocation cost, it is done in larger
 * chunks than necessary.  To avoid rearrangement of the data upon
 * deletion, the deleted VERTEX/EDGE is simply marked invalid.
 *
 * ====================================================================
 */

/* EDGE/VERTEX index types, and reserved index values: */
typedef int EINDEX;
typedef int VINDEX;
#define INVALID_EINDEX -1
#define INVALID_VINDEX -1

/* ====================================================================
 *
 * A VERTEX contains two singly-linked lists of EDGEs, one of EDGEs
 * starting at that VERTEX, and one of EDGEs ending at that VERTEX.  
 * Each list is represented in the VERTEX by the index of its first
 * element (VERTEX_from and VERTEX_to) and a count of its elements
 * (VERTEX_fcnt and VERTEX_tcnt).  An invalid (i.e. unused) VERTEX is
 * indicated by VERTEX_fcnt == INVALID_EINDEX.  The VERTEX also
 * contains a pointer VERTEX_user to additional data required by the
 * client derived graph. The level of a vertex is defined as the (max level
 * of the immediate successors of the vertex) + 1
 * ====================================================================
 */

typedef struct vertex {
  void* user;         /* user information */
  EINDEX from, to;    /* first edge from and to this vertex */
  EINDEX fcnt, tcnt;  /* from/to counts                     */
  int level;          /* level of the vertex                */
} VERTEX;

/* Field access macros: */
#define VERTEX_user(vertex) ((vertex)->user)
#define VERTEX_from(vertex) ((vertex)->from)
#define VERTEX_to(vertex)   ((vertex)->to)
#define VERTEX_fcnt(vertex) ((vertex)->fcnt)
#define VERTEX_tcnt(vertex) ((vertex)->tcnt)
#define VERTEX_level(vertex) ((vertex)->level)
/* ====================================================================
 *
 * A EDGE contains two VERTEXes, its from VERTEX (EDGE_from) and its
 * to vertex (EDGE_to).  It contains links (indices) to the next edges
 * in the VERTEX_from list for its from VERTEX (EDGE_nfrom), and in the
 * VERTEX_to list for its to VERTEX (EDGE_nto).  It contains a flag
 * word containing attributes, and a pointer EDGE_user to additional
 * data required by the client derived graph.
 *
 * ====================================================================
 */

/* EDGE data structure: */

typedef int ETYPEX;
typedef int BOOL;
#ifndef FALSE
#define FALSE 0
#define TRUE  1
#endif
typedef char MEM_POOL;

typedef struct edge {
  void * user;        /* user information     */
  VINDEX from, to;    /* from and to vertices */
  EINDEX nfrom;       /* next edge from the from vertex */
  EINDEX nto;         /* next edge to the to vertex */
  ETYPEX etype;       /* edge type, i.e. is it a back edge resulting */
                      /* in a cycle: used to locate recursion  */
} EDGE; 

/* Field access macros: */
#define EDGE_user(edge)  ((edge)->user)
#define EDGE_from(edge)  ((edge)->from)
#define EDGE_to(edge)    ((edge)->to)
#define EDGE_nfrom(edge) ((edge)->nfrom)
#define EDGE_nto(edge)   ((edge)->nto)
#define EDGE_etype(edge)   ((edge)->etype)

/* Flag access: */
#define EDGE_RECURSIVE 1
#define Set_EDGE_recursive(edge) (EDGE_etype(edge) |= EDGE_RECURSIVE)
#define EDGE_recursive(edge)   (EDGE_etype(edge) & EDGE_RECURSIVE)

/* ====================================================================
 *
 * A GRAPH is the top-level directed graph data structure.  For both
 * vertices and edges, it contains the number of VERTEX/EDGE structures
 * actually allocated (GRAPH_vmax, GRAPH_emax), the number of actual
 * VERTEX/EDGE components of the graph (GRAPH_vcnt, GRAPH_ecnt), the
 * number of free VERTEX/EDGE structures that are allocated and
 * available for use (GRAPH_vfree, GRAPH_efree), and the vector of
 * VERTEX_EDGE components (GRAPH_v, GRAPH_e).  It also contains the
 * root of the graph (GRAPH_root) and the memory pool to use for
 * allocation (GRAPH_m).
 *
 * ====================================================================
 */

typedef struct graph {
  VINDEX vcnt;  /* vertex count            */
  VINDEX vmax;  /* max vertices            */
  VINDEX vfree; /* number of free vertices */

  EINDEX ecnt;  /* edge count              */
  EINDEX emax;  /* max edges               */
  EINDEX efree; /* number of free edges    */
 
  VINDEX root;  /* root of the graph       */
  VERTEX *v;    /* list of vertices        */
  EDGE *e;      /* list of edges           */
  MEM_POOL *m;  /* mem pool                */
} GRAPH;

/* Field access macros: */
#define GRAPH_vcnt(g) ((g)->vcnt)
#define GRAPH_vmax(g) ((g)->vmax)
#define GRAPH_vfree(g)((g)->vfree)
#define GRAPH_ecnt(g) ((g)->ecnt)
#define GRAPH_emax(g) ((g)->emax)
#define GRAPH_efree(g) ((g)->efree)
#define GRAPH_root(g)  ((g)->root)
#define GRAPH_m(g) ((g)->m)
#define GRAPH_v(g)  ((g)->v)
#define GRAPH_v_i(g,index) ((g)->v[index])
#define GRAPH_e(g)  ((g)->e)
#define GRAPH_e_i(g,index) ((g)->e[index])

/* Walk all the valid vertices in the graph g, using VINDEX v: */
#define FOR_EACH_VERTEX(g,v)	\
	for ( v = 0; v < GRAPH_vmax(g); v++ )	\
	  if ( VERTEX_fcnt (&GRAPH_v_i(g,v)) != INVALID_VINDEX )

/* ====================================================================
 *
 * DFN: Depth-first numbering graph traversal control struct
 *
 * This struct is used to store an ordering of the nodes in a graph, to
 * control traversal in a particular order.  It contains a vector of
 * the graph vertices in the traversal order (DFN_v_list), and the
 * indices of the first useful index in the vector (GRAV_first) and of
 * the index after the last useful index in the vector (DFN_end).
 *
 * ====================================================================
 */

typedef struct dfn {
  int first;
  int end;
  VINDEX* v_list;
} DFN;

/* Field access macros: */
#define DFN_v_list(d) ((d)->v_list)
#define DFN_v_list_i(d,i) ((d)->v_list[i])
#define DFN_first(d) ((d)->first)
#define DFN_end(d) ((d)->end)

/* Trace a DFN ordering: */
extern void Print_DFN ( DFN * );

/* Construct a depth-first numbering of the given graph: */
extern DFN* Depth_First_Ordering ( GRAPH *, MEM_POOL * );

/* De-allocate a DFN ordering: */
extern void Free_DFN ( DFN *, MEM_POOL * );

/* ====================================================================
 *
 * V_ITER: Vertex iterator control struct
 *
 * This struct is used to control an iterator over vertices.
 *
 * ====================================================================
 */

typedef struct v_iter {
  GRAPH *g;       /* the graph */
  VINDEX c_v;     /* the current vertex */
  EINDEX from_e;  /* from edge   */
  EINDEX to_e;    /* to edge     */
  EINDEX  fcnt;   /* from count  */
  EINDEX nfrom;   /* next from edge */
  EINDEX tcnt;    /* to count      */
  EINDEX nto;     /* next to edges */
  EINDEX c_e;     /* current edge  */
  MEM_POOL *m;     /* mempool  */
} V_ITER;

/* Field access macros: */
#define V_ITER_g(vi) ((vi)->g)
#define V_ITER_c_v(vi) ((vi)->c_v)
#define V_ITER_from_e(vi) ((vi)->from_e)
#define V_ITER_to_e(vi) ((vi)->to_e)
#define V_ITER_fcnt(vi) ((vi)->fcnt)
#define V_ITER_nfrom(vi) ((vi)->nfrom)
#define V_ITER_tcnt(vi)  ((vi)->tcnt)
#define V_ITER_nto(vi)   ((vi)->nto)
#define V_ITER_c_e(vi)   ((vi)->c_e)
#define V_ITER_m(vi)   ((vi)->m)

#define MEM_POOL_Alloc(m, s)            calloc(m, s)
#define MEM_POOL_Realloc(m, ob, os, ns) realloc(m, ns)
#define MEM_POOL_FREE(m, s)             /* we don't free for now */

/* ====================================================================
 *
 * External function declarations
 *
 * ====================================================================
 */

extern GRAPH* build_graph_u(VINDEX, EINDEX, MEM_POOL*);
extern GRAPH* build_graph(MEM_POOL*);

extern VINDEX add_vertex(GRAPH*, void*);
extern EINDEX add_edge(GRAPH*, VINDEX, VINDEX, void*);

extern BOOL is_vertex(GRAPH*, VINDEX);
extern BOOL is_edge(GRAPH*, EINDEX);

extern void  delete_edge(GRAPH*, EINDEX);
extern void* delete_vertex(GRAPH*, VINDEX);

extern void* get_vertex(GRAPH*, VINDEX);
extern void* get_edge(GRAPH*, VINDEX, VINDEX);
extern void* get_edge_u(GRAPH*, EINDEX );
extern void set_edge_u (GRAPH *, EINDEX, void *);

extern int num_preds(GRAPH*, VINDEX);
extern int num_succs(GRAPH*, VINDEX);
extern int edge_count(GRAPH*, VINDEX from, VINDEX to);

extern VINDEX next_vertex(GRAPH *g, VINDEX vertex);

extern V_ITER* create_vertex_iter(GRAPH*, VINDEX, MEM_POOL*);
extern VINDEX  first_v_preds(V_ITER*);
extern VINDEX  next_v_preds(V_ITER*);
extern VINDEX  first_v_succs(V_ITER*);
extern VINDEX  next_v_succs(V_ITER*);

/* related to the level of a vertex in the graph */
extern void set_vertex_level(GRAPH*, VINDEX r, int level);
extern int get_vertex_level(GRAPH*, VINDEX r);

#ifdef __cplusplus
}
#endif
#endif /* ip_graph_INCLUDED */

