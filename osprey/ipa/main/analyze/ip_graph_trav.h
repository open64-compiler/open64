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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#ifndef cxx_ip_graph_trav_INCLUDED
#define cxx_ip_graph_trav_INCLUDED

#ifndef CXX_MEMORY_INCLUDED
#include "cxx_memory.h"
#endif


extern INT IPA_CG_Max_Depth;
/*------------------------------------------------------------*/
/* contains a list of graph vertices used for an ordering     */
/*------------------------------------------------------------*/
class VVECTOR
{
private:
    UINT size;			/* total size */
    UINT count;			/* actual number of vertices */
    NODE_INDEX *a;			/* array of vertices */
    MEM_POOL *mm;

public:
    VVECTOR(UINT sz) {
	size = sz;
	mm = Malloc_Mem_Pool;
	a = CXX_NEW_ARRAY(NODE_INDEX, sz+1, Malloc_Mem_Pool);
	count = 0;
    };

    ~VVECTOR (void) { CXX_DELETE (a, Malloc_Mem_Pool); };

    void Append(NODE_INDEX vtx) {
	FmtAssert(count <=  size,
		  ("Too many vertices, size = %d, count = %d \n", size,count));
	a[count++] = vtx;
    };
    
    void Append(NODE_INDEX vtx, INT pos)
      {
	FmtAssert(count <=  size,
		  ("Too many vertices, size = %d, count = %d \n",
		   size,count));

	FmtAssert(pos <= size, ("Illegal position %d for append vertex \n", pos));
	a[pos] = vtx;
	count++;
      }

    UINT Cnt() { 
	return count;
    };

    NODE_INDEX& operator[](UINT i) {
	FmtAssert(i < size, ("vertex out of range "));
	return a[i];
    };

};

/*------------------------------------------------------------------------*/
/* create a vector of something. I use it for creating a vector of edges  */
/* used during the sorting phase                                          */
/*------------------------------------------------------------------------*/
class EVECTOR
{
private:
    UINT size;			/* total size */
    UINT count;			/* actual number of edges  */
    void **a; 

public:
    EVECTOR(UINT sz) {
	size = sz; 
	a = CXX_NEW_ARRAY(void*, sz+1, Malloc_Mem_Pool);
	count = 0;
    };

    void Append(void* e) {
	FmtAssert(count <= size,
		  ("Too many edges, size = %d, count = %d \n", size,count));
	a[count++] = e;
    };
    
    UINT Cnt() { 
	return count;
    };

    void* & operator[](UINT i) {
	FmtAssert(i < size, ("edge %d out of range %d \n ", i, size));
	return a[i];
    };

    ~EVECTOR() {
	CXX_DELETE_ARRAY(a, Malloc_Mem_Pool);
    };
};

/*------------------------------------------------------------------------*/
/* GraphVertexIterator: supports a traversals of the vertices             */
/* of a directed graph                                                    */
/* For now only depth first postorder since we use that for inlining      */
/*------------------------------------------------------------------------*/
typedef enum traveral_order
{
    POSTORDER = 0,
    PREORDER  = 1,		/* preorder does not mark recursive */
				/* calls */
    LEVELORDER = 2,             /* based on level of the vertices */
    DONTCARE = 3,		/* just iterate all vertices */
    ORDER_LAST = 4		/* number of traveral order supported
				   (must be last) */
} TRAVERSAL_ORDER;

/*------------------------------------------------------------------------*/
/* vertex iterator class, that constructs a particular ordering           */
/*------------------------------------------------------------------------*/
class ORDERED_NODE_ITER {

private:
    GRAPH *g;    
    INT cur_v;
    VVECTOR *v; 
    TRAVERSAL_ORDER order;
    MEM_POOL *m;

    void Walk (NODE_INDEX r, mUINT8 *visit);
    void BuildVector(TRAVERSAL_ORDER);
    void MarkRecursive (mUINT8 visit[], NODE_INDEX r);
    void Build_Level_Order();

public:

    ORDERED_NODE_ITER (GRAPH *gr, TRAVERSAL_ORDER o, MEM_POOL *mm) {
	g = gr;
	order = o;
	m = mm;
	cur_v = 0;
	BuildVector(order);
    };

    ~ORDERED_NODE_ITER (void) {
	if (v != 0)
	    CXX_DELETE (v, Malloc_Mem_Pool);
    }

    void operator ++() { ++cur_v; }
    NODE_INDEX Current() const { return (*v)[cur_v]; }
    BOOL Is_Empty() const { return (cur_v >= v->Cnt()); }
    void Reset() { cur_v = 0; }
    void Print(FILE*);
    void Compute_Node_Level(NODE_INDEX);

};

#endif // cxx_ip_graph_trav_INCLUDED
