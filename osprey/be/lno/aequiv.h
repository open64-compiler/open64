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
//                     Register Coloring of Temporary Arrays
//                     -------------------------------------
//
// Description:
//
//     University of Minnessota idea.
//     Save space by equivalencing temporary arrays.  
//
//
// Exported types and functions:
//
//	AEQUIV
//
//	    AEQUIV(WN *func_nd)
//
//	    void Equivalence_Arrays()
//
//		Equivalence the temporary arrays
//
//
//


/**
*** $Source$
**/

#ifndef AEQUIV_RCS_ID
#define AEQUIV_RCS_ID
#ifdef _KEEP_RCS_ID
static char *aequiv = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */
#endif

#include "cxx_hash.h"
#include "cxx_graph.h"
#include "graph_template.h"
#include "lnoutils.h"
#include "lno_bv.h"
#include "lno_scc.h"


#ifndef AEQUIV_DELCARE
#define AEQUIV_DELCARE

extern MEM_POOL AEQUIV_pool;

// information about local array
class LOCAL_ARRAY_DESC {
public:
  LOCAL_ARRAY_DESC(INT id) { _is_written = _is_read = _address_taken = FALSE; 
					_id = id; };
  mBOOL _is_written;
  mBOOL _is_read;
  mBOOL _address_taken;
  INT _id;  // it's id, or position in the _la_stack
};

typedef HASH_TABLE<ST *,LOCAL_ARRAY_DESC *> LOCAL_ARRAY_HASH_TABLE;
typedef STACK<ST *> LOCAL_ARRAY_STACK;
typedef STACK<BIT_VECTOR*> BIT_VECTOR_STACK;  

// Classes used in building CFG
// A goto and it's associated vertex number
struct GOTO_VERTEX {
  WN *_wn;
  VINDEX16 _vindex;
};
typedef STACK<GOTO_VERTEX> GOTO_VERTEX_STACK;
typedef STACK<VINDEX16> LABEL_STACK; // stack of vertex numbers of the labels

// hash table mapping labels to their associated vertices
typedef HASH_TABLE<INT ,VINDEX16> LABEL_VERTEX_HASH;

class AEQUIV {
  LOCAL_ARRAY_HASH_TABLE *_la_hash_table; // maps loc array to LOCAL_ARRAY_DESC
  LOCAL_ARRAY_STACK *_la_stack;
  WN *_func_nd;
  MEM_POOL *_pool;
  ARRAY_DIRECTED_GRAPH16 *_array_dep_graph;
  INT Num_Arrays() { return _la_stack->Elements(); };

  // which arrays are used in vertex 'v' 
  // We use a stack of bitvectors, one for each vertex in the cfg graph
  // This relies (from an efficiency point of view)
  // on the fact that vertex numbers are close to dense and
  // are numbered starting at a low number
  BIT_VECTOR_STACK *_cyclic_bit_vector;  // corresponding to the _cfg
  BIT_VECTOR_STACK *_acyclic_bit_vector; // corresponding to the _acfg 

  // in which vertices is array 'a' live
  BIT_VECTOR_STACK *_array_bit_vector;
public:
  AEQUIV(WN *func_nd,ARRAY_DIRECTED_GRAPH16 *array_dep_graph) { 
    _cyclic_bit_vector = NULL;
    _acyclic_bit_vector = NULL;
    _pool = &AEQUIV_pool;
    _func_nd = func_nd;
    _la_hash_table = NULL;
    _la_stack = NULL;
    _cfg = NULL;
    _acfg = NULL;
    _array_dep_graph = array_dep_graph;
  }
  void Equivalence_Arrays();
private:
  void Enter_Locals_Stack();
  void Sort_Stack();
  void Enter_Locals_Hash();
  // Control Flow graph
  SCC_DIRECTED_GRAPH16 *_cfg;
  SCC_DIRECTED_GRAPH16 *_acfg;
  VINDEX16 _head_vertex;  // entry to routine
  VINDEX16 _tail_vertex;  // exit from routine
  INT Build_CFG();
  INT Build_CFG_Rec(WN *wn, VINDEX16 *current_v, VINDEX16 next_v,
		GOTO_VERTEX_STACK *goto_stack,LABEL_STACK *label_stack,
	  	LABEL_VERTEX_HASH *label_hash);
  INT Build_CFG_Loop(WN *wn,VINDEX16 loopv,
		GOTO_VERTEX_STACK *goto_stack,LABEL_STACK *label_stack,
	  	LABEL_VERTEX_HASH *label_hash);
  void Handle_Store(WN *wn, VINDEX16 v);
  void Handle_Lhs(WN *wn, VINDEX16 v);
  void Handle_Rhs(WN *wn, VINDEX16 v);
  void Handle_Call(WN *wn, VINDEX16 v);
  INT Backpatch_CFG( GOTO_VERTEX_STACK *goto_stack,LABEL_STACK *label_stack,
	  	LABEL_VERTEX_HASH *label_hash);

  VINDEX16 Add_CFG_Vertex(BIT_VECTOR *bit_vector);
  VINDEX16 Add_CFG_Edge(VINDEX16 from, VINDEX16 to) { 
    return(_cfg->Add_Edge(from,to)); 
  };
  void Print_Graph(FILE *fp,SCC_DIRECTED_GRAPH16 *graph);

  // Acyclic control flow graph
  void Set_Acyclic();

  void Do_Dataflow();
  void Set_Array_Bit_Vector();
  BOOL Do_Color(mBOOL *equivalenced_array);
  void Update_Code(WN *wn,mBOOL *equivalenced_array);
  BOOL Contains_Unread_Array(WN *wn, mBOOL *equivalenced_array);
};


#endif  // AEQUIV_DECLARE

