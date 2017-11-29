/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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
//                     Array Dependence Graph
//                     -----------------------
//
//
//

/* ====================================================================
 * ====================================================================
 *
 * Module: dep_graph.cxx  
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/com/dep_graph.cxx,v $
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Build array dependence graph
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: be/com/SCCS/s.dep_graph.cxx $ $Revision: 1.7 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */

#define USE_STANDARD_TYPES	    /* override unwanted defines in "defs.h" */

#include <bstring.h>
#include "wn.h"
#include "erglob.h"
#include "errors.h"
#include "strtab.h"		    /* for strtab */
#include "stab.h"		    /* for symtab */
#include "irbdata.h"		    /* for inito */
#include "dwarf_DST_mem.h"	    /* for DST */
#include "pu_info.h"
#ifdef __MINGW32__
#include <WINDOWS.h>
#endif /* __MINGW32__ */
#include "ir_bwrite.h"
#include "ir_bcom.h"

#include "dep_graph.h"
#include "stab.h"

#ifdef LNO
#include "call_info.h"
#include "config.h"
#include "config_cache.h"
#include "errors.h"
#include "erbe.h"

#include "lnopt_main.h"
#include "soe.h"
#include "lwn_util.h"
#include "opt_alias_interface.h"
#include "lego_util.h"
#include "opt_du.h"

// return TRUE if any erased vertices.
static BOOL LNO_Erase_Dg_From_Here_In_X(WN* wn, ARRAY_DIRECTED_GRAPH16* dg)
{
  BOOL erased_vertices = FALSE;

  if (WN_opcode(wn) == OPC_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
      if (LNO_Erase_Dg_From_Here_In_X(w, dg))
        erased_vertices = TRUE;
  }
  else {
    for (INT k = 0; k < WN_kid_count(wn); k++)
      if (LNO_Erase_Dg_From_Here_In_X(WN_kid(wn,k), dg))
        erased_vertices = TRUE;
  }

  OPCODE   op  = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(op);

  VINDEX16      v = dg->Get_Vertex(wn);
  if (OPCODE_is_load(op) || OPCODE_is_store(op) ||
      OPCODE_is_call(op) || (OPCODE_operator(op) == OPR_INTRINSIC_OP)
#ifdef KEY
      || (OPCODE_operator(op) == OPR_PURE_CALL_OP)
#endif
      ) {
    if (v) {
      EINDEX16 enext = 0;
      EINDEX16 e;
      for (e = dg->Get_In_Edge(v); e; e = enext) {
        enext = dg->Get_Next_In_Edge(e);
        dg->Delete_Array_Edge(e);
      }
      for (e = dg->Get_Out_Edge(v); e; e = enext) {
        enext = dg->Get_Next_Out_Edge(e);
        dg->Delete_Array_Edge(e);
      }
      dg->Delete_Vertex(v);

      erased_vertices = TRUE;
    }
  }
  if (opr == OPR_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    if (dli->Has_Bad_Mem == TRUE)
      erased_vertices = TRUE;
    dli->Has_Bad_Mem = erased_vertices;
  }

  return erased_vertices;
}

void Unmapped_Vertices_Here_Out(WN* wn)
{
  for ( ; wn; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
      dli->Has_Bad_Mem = 1;
    }
  }
}

void LNO_Erase_Dg_From_Here_In(WN* wn, ARRAY_DIRECTED_GRAPH16* dg)
{
  WN *outer_do = wn;
  WN *tmp = wn;
  while (tmp) {
    if (WN_opcode(tmp) == OPC_DO_LOOP) {
      outer_do = tmp;
    }
    tmp = LWN_Get_Parent(tmp);
  }
  if (LNO_Erase_Dg_From_Here_In_X(outer_do, dg))
    Unmapped_Vertices_Here_Out(outer_do);
}

extern WN* Get_Only_Loop_Inside(const WN* wn, BOOL regions_ok);
#endif


ARRAY_DIRECTED_GRAPH16 *Current_Dep_Graph = NULL;

void *C_Dep_Graph(void)
{
    return Current_Dep_Graph;
}


void
Init_Dep_Graph(void *g)
{
    if (Current_Dep_Graph != NULL)
	Dealloc_Dep_Graph ();

    Current_Dep_Graph = (ARRAY_DIRECTED_GRAPH16 *)g;
}


void
Dealloc_Dep_Graph(void)
{
    /* deallocate the old dependence graph */
    if (Current_Dep_Graph) {
	CXX_DELETE(Current_Dep_Graph, Malloc_Mem_Pool);
	Current_Dep_Graph = NULL;
    }
}

void LNOPruneMapsUsingParity(void)
{
    if (Current_Dep_Graph)
	Current_Dep_Graph->PruneMapsUsingParity();
}

void LNOPreserveMapPair(WN *orig, WN *wn1, WN *wn2)
{
    if (Current_Dep_Graph)
	Current_Dep_Graph->PreserveMapPair(orig, wn1, wn2);
}

void LNOPrintDepGraph(FILE *fp)
{
    if (Current_Dep_Graph)
	Current_Dep_Graph->Print(fp);
}


BOOL LnoDependenceEdge(WN *wn1, WN *wn2, EINDEX16 *distance, DIRECTION *direction, BOOL *is_must, BOOL *status)
{
  VINDEX16 v1, v2;
  EINDEX16 edge;

  *status=	FALSE;

  if (Current_Dep_Graph == NULL)
    return FALSE;

  v1 = Current_Dep_Graph->Get_Vertex(wn1);
  v2 = Current_Dep_Graph->Get_Vertex(wn2);

  if (v1 == 0 || v2 == 0)
    return FALSE;

  *status =	TRUE;
  edge = Current_Dep_Graph->Get_Edge(v1, v2);

  if (edge)
  {
    DEP dep = Current_Dep_Graph->Dep(edge);

    *direction = DEP_Direction(dep);

    *is_must = Current_Dep_Graph->Is_Must(edge);

    *distance = DEP_IsDistance(dep) ? DEP_Distance(dep) : DEP_DistanceBound(dep);

    return TRUE;
  }
  return FALSE;
}


VINDEX16 LNOGetVertex(WN *wn)
{
  if (Current_Dep_Graph)
    return Current_Dep_Graph->Get_Vertex(wn);
  return 0;
}

// remove all the edges and vertices from the graph
void ARRAY_DIRECTED_GRAPH16::Erase_Graph()
{
  for(VINDEX16 v= Get_Vertex(); v; v= Get_Next_Vertex(v)) {
    WN *wn = Get_Wn(v);
    if (wn != NULL)
      WN_MAP_Set(_map,wn,0);
  }
#ifdef LNO
  if (_type==DEPV_ARRAY_ARRAY_GRAPH) {
    for(EINDEX16 e= Get_Edge(); e; e= Get_Next_Edge(e)) {
      Delete_DEPV_ARRAY(_e[e].Depv_Array,_pool);
    }
  }
#endif
}

void ARRAY_DIRECTED_GRAPH16::PruneMapsUsingParity(void)
{
 /*
  *  iterate over all vertices and remove nodes that are
  *  proven independent by means of parity
  */
  for(VINDEX16 v= Get_Vertex(); v; v= Get_Next_Vertex(v))
  {
    EINDEX16	e= Get_Out_Edge(v);
    WN	*tree= Get_Wn(v);

    while(e)
    {
	EINDEX16 e1= Get_Next_Out_Edge(e);

	if (WN_parity_independent(tree, Get_Wn(Get_Sink(e))))
	{
	  Remove_Edge(e);
	}
	e = e1;
    }

    e = Get_In_Edge(v);
    while(e)
    {
	EINDEX16 e1= Get_Next_In_Edge(e);

	if (WN_parity_independent(tree, Get_Wn(Get_Sink(e))))
	{
	  Remove_Edge(e);
	}
	e = e1;
    }
  }
}

#if defined(SHARED_BUILD) && !defined(LNO)
void ARRAY_DIRECTED_GRAPH16::Print(FILE *fp)
#else
void ARRAY_DIRECTED_GRAPH16::Print(FILE *fp, INT)
#endif
{
  VINDEX16 i;
  EINDEX16 e;
  if (_type==DEPV_ARRAY_ARRAY_GRAPH) {
    fprintf(fp,"Printing an ARRAY_DIRECTED_GRAPH16 of type DEPV_ARRAY \n");
  } else if (_type == LEVEL_ARRAY_GRAPH) {
    fprintf(fp,"Printing an ARRAY_DIRECTED_GRAPH16 of type level \n");
  } else {
    fprintf(fp,"Printing an ARRAY_DIRECTED_GRAPH16 of type DEP \n");
  }
  for (i=1; i<_v.Lastidx()+1; i++) {
   if (!_v[i].Is_Free()) {
    if (_type==DEPV_ARRAY_ARRAY_GRAPH) {
#ifdef LNO
      BOOL is_load = FALSE;
      BOOL is_call = FALSE;
      WN *wn = _v[i].Wn;
      if (OPCODE_is_load(WN_opcode(wn))) {
	is_load = TRUE;
	if (WN_kid_count(wn) >= 1) {
	  wn = WN_kid0(wn); // not an ldid
        }
      } else if (OPCODE_is_store(WN_opcode(wn))) {
	if (WN_kid_count(wn) >= 2) {
	  wn = WN_kid1(wn); // not an stid
        }
      } else {
	is_call = TRUE;
      }
      if (WN_operator(wn) == OPR_ARRAY) {
	WN *base = WN_array_base(wn);
	if (OPCODE_has_sym(WN_opcode(base)) && WN_st(base)) {
          if (is_load) {
            fprintf(fp,"Vertex %d for load from Wn = %s",i,
          		ST_name(WN_st(WN_array_base(wn))));
          } else {
            fprintf(fp,"Vertex %d for store into Wn = %s",i,
          		ST_name(WN_st(WN_array_base(wn))));
          }
        } else {
	  if (is_load) {
            fprintf(fp,"Vertex %d for load from Wn = ??? ",i);
          } else {
            fprintf(fp,"Vertex %d for store into Wn = ??? ",i);
          }
        }
        ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,wn);
        array->Print(fp);
      } else {
	if (is_load) {
          fprintf(fp,"Vertex %d for load from Wn = ",i);
        } else if (is_call) {
          fprintf(fp,"Vertex %d for call into Wn = ",i);
        } else {
          fprintf(fp,"Vertex %d for store into Wn = ",i);
        }
        Dump_WN(wn,fp,TRUE,0,0);
      }
#endif
    } else {
      fprintf(fp,"Vertex %d for Wn = 0x%p",i,_v[i].Wn);
#ifdef LNO
      // this is LNO specific but other groups might want something similar
      Dump_WN(_v[i].Wn,fp,TRUE,0,0);
#endif
      fprintf(fp,"\n");
    }
    e = _v[i].Get_Out_Edge();
    while (e) {
      fprintf(fp,"Edge %d to vertex %d ",e,_e[e].Get_Sink());
      if (_type==DEPV_ARRAY_ARRAY_GRAPH) {
#ifdef LNO
        fprintf(fp," has DEPV_ARRAY = ");
        _e[e].Depv_Array->Print(fp);
#endif
      } else if (_type == LEVEL_ARRAY_GRAPH) {
#ifdef LNO
	fprintf(fp," has level %d \n",_e[e].Level_Info.Level);
#endif
      } else {
	fprintf(fp," has dep ");
	DEP_Print(_e[e].DEP_Struct.Dep,fp); 
	fprintf(fp," and Is_Must is %d\n",_e[e].DEP_Struct.Is_Must);
      }
      e = _e[e].Get_Next_Out_Edge();
    }
   }
  }

}

extern "C" void
Depgraph_Write (void *depgraph, Output_File *fl, WN_MAP off_map)
{
  ARRAY_DIRECTED_GRAPH16 *g = (ARRAY_DIRECTED_GRAPH16 *)depgraph;
  VINDEX16 vmax, new_vmax, v, ve, x;
  EINDEX16 emax, e;
  Elf64_Word wn_off;
  DEP_STRUCT dep;
  VINDEX16 *vertex_map;

  /* write out the numbers of vertices and edges */
  vmax = g->_v.Lastidx();

  emax = g->_e.Lastidx();

  /* remap the vertex indices to get rid of freed vertices */
  vertex_map = CXX_NEW_ARRAY(VINDEX16, vmax + 1, Malloc_Mem_Pool);
  x = 1;
  for (v = 1; v <= vmax; v++) {
    // vertex is still in graph and there is some wn associated with this vertex
    if (g->Vertex_Is_In_Graph(v) && g->Get_Wn(v) != NULL) {
      vertex_map[v] = x;
      x += 1;
    } else {
      vertex_map[v] = 0;
    }
  }
  new_vmax = x - 1;

  ir_b_save_buf((void *)&new_vmax, sizeof(VINDEX16), sizeof(VINDEX16), 0, fl);
  ir_b_save_buf((void *)&emax, sizeof(EINDEX16), sizeof(EINDEX16), 0, fl);

  /* first write out all the vertices */
  for (v = 1; v <= vmax; v++) {

    /* skip over freed vertices */
    if (vertex_map[v] == 0) continue;

    /* write the offset of the WN */
    wn_off = (Elf64_Word)WN_MAP32_Get(off_map, g->Get_Wn(v));
    ir_b_save_buf((void *)&wn_off, sizeof(Elf64_Word), sizeof(Elf64_Word),
		      0, fl);
  }

  /* then write out the edges for each vertex */
  for (v = 1; v <= vmax; v++) {

    /* skip over freed vertices */
    if (vertex_map[v] == 0) continue;

    /* iterate through the "out" edges */
    e = g->Get_Out_Edge(v);
    while (e) {

      /* write the sink of the edge */
      ve = vertex_map[g->Get_Sink(e)];
      if (ve) { // the sink exists
        ir_b_save_buf((void *)&ve, sizeof(VINDEX16), sizeof(VINDEX16),0,fl);

        /* write the dependence information */
        dep = g->_e[e].DEP_Struct;
        ir_b_save_buf((void *)&dep, sizeof(DEP_STRUCT), 
		      MAX(4,sizeof(DEP_STRUCT)), 0, fl);

      } else {
	DevWarn("Missing sink \n");
      }
      e = g->Get_Next_Out_Edge(e);
    }

    /* write a sink of zero to mark the last edge */
    ve = 0;
    ir_b_save_buf((void *)&ve, sizeof(VINDEX16), sizeof(VINDEX16), 0, fl);
  }

  CXX_DELETE_ARRAY(vertex_map, Malloc_Mem_Pool);
}


#define WORD_ALIGNED(sz)	(((sz) % 4) == 0 ? (sz) : (sz)+(4-((sz)%4)))

extern "C" void *
Depgraph_Read (char *cur_addr, char *end_addr, char *wn_base)
{
  ARRAY_DIRECTED_GRAPH16 *g;
  VINDEX16 vmax, v, ve;
  EINDEX16 emax;
  Elf64_Word node_offset;
  DEP_STRUCT dep;
  WN *node;

  /* get the numbers of vertices and edges */
  vmax = *(VINDEX16 *)cur_addr;
  cur_addr += (sizeof(VINDEX16));
  emax = *(EINDEX16 *)cur_addr;
  cur_addr += (sizeof(EINDEX16));

  g = CXX_NEW(ARRAY_DIRECTED_GRAPH16(vmax+1, emax+1, WN_MAP_DEPGRAPH,
				     DEP_ARRAY_GRAPH), Malloc_Mem_Pool);

  /* create the vertices */
  cur_addr = (char*) WORD_ALIGNED((INTPTR)cur_addr);
  for (v = 1; v <= vmax; v++) {

    node_offset = *(Elf64_Word *)cur_addr;
    cur_addr += (sizeof(Elf64_Word));

    node = (WN *)(wn_base + node_offset);
    BOOL result = g->Add_Vertex(node);
    Is_True((result == v), ("New vertex not in order\n"));
  }

  /* read the edges */
  for (v = 1; v <= vmax; v++) {

    while (1) {
      ve = *(VINDEX16 *)cur_addr;
      cur_addr += (sizeof(VINDEX16));
      if (ve == 0) break;

      cur_addr = (char *)WORD_ALIGNED((INTPTR)cur_addr);

      dep = *(DEP_STRUCT *)cur_addr;
      cur_addr += (sizeof(DEP_STRUCT));


      /* check for errors */
      if ((ve > vmax) || (cur_addr > end_addr)) {
	CXX_DELETE(g, Malloc_Mem_Pool);
	return 0;
      }

      g->Add_Edge(v, ve, dep.Dep,dep.Is_Must);
    }
  }

  return (void *)g;
}

// All the remaining code is LNO specific and will not be linked in
// by other phases

#ifdef LNO

extern MEM_POOL LNO_local_pool; // the local pool
extern MEM_POOL LNO_default_pool; // the local pool
static MEM_POOL DEP_local_pool;  // a local pool for this file
static BOOL dep_mempool_initialized = FALSE;
static INT Common_Nest(const DOLOOP_STACK *s1, const DOLOOP_STACK *s2);
static INT Num_Bad(const DOLOOP_STACK *s1);
static DOLOOP_STACK *Copy_Doloop_Stack(DOLOOP_STACK *orig,MEM_POOL *pool);



// mark all the DO loops surrounding wn as having bad memory references
static void Set_Bad_Mem(WN *wn)
{
  while (wn) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
      if (dli->Has_Bad_Mem) { 
	return;
      }
      dli->Has_Bad_Mem = TRUE;
    }
    wn = LWN_Get_Parent(wn);
  }
}

// Build the DEPV_ARRAY or DEP graph, return 0 on error
INT ARRAY_DIRECTED_GRAPH16::Build(WN *func_nd, MEM_POOL *pool)
{
  Is_True(_type!=LEVEL_ARRAY_GRAPH,
      ("Build called on a LEVEL_ARRAY_GRAPH"));
  _pool = pool;
  if (!dep_mempool_initialized) {
    MEM_POOL_Initialize(&DEP_local_pool,"DEP_local_pool",FALSE);
    dep_mempool_initialized = TRUE;
  }
  MEM_POOL_Push(&LNO_local_pool);
  DOLOOP_STACK *stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),&LNO_local_pool); 
  INT res=Find_Region(func_nd,stack);
  MEM_POOL_Pop(&LNO_local_pool);


  if (res && _type == DEP_ARRAY_GRAPH) Add_Must();

  return(res);
}


// find and then process an outer region
// For DEPV_ARRAY graphs, an outer region is an outer "good" DO loop
// For DEP graphs, we build graphs on inner loops, so an outer region
// is an inner "good" do loop
// Return 0 on error
INT ARRAY_DIRECTED_GRAPH16::Find_Region(WN *wn, DOLOOP_STACK *stack)
{
  WN *kid;

  if (OPCODE_is_leaf(WN_opcode(wn))) return(1);

  if (WN_opcode(wn) == OPC_BLOCK) {
    kid = WN_first (wn);
    while (kid) {
      if (!Find_Region(kid,stack)) return(0);
      kid = WN_next(kid);
    }
  } else if (WN_opcode(wn) == OPC_DO_LOOP) {
    DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
    dli->Has_Bad_Mem = FALSE;
    stack->Push(wn);
    if (Do_Loop_Is_Good(wn) && 
	 (!dli->Has_Unsummarized_Calls || dli->Is_Concurrent_Call) &&
	 !dli->Has_Exits && !dli->Has_Barriers && 
	((_type == DEPV_ARRAY_ARRAY_GRAPH) || 
	  (Do_Loop_Is_Inner(wn) && !dli->Has_Gotos && !dli->Has_Barriers))) {
      if (!Build_Region(WN_do_body(wn),WN_do_body(wn),stack)) {
#ifdef KEY
	if (_type == DEPV_ARRAY_ARRAY_GRAPH)
#endif
        LNO_Erase_Dg_From_Here_In(wn, this);
        return(0);
      } 
      if (dli->Has_Bad_Mem) LNO_Erase_Vertices_In_Loop(wn,this);
    } else {
      Set_Bad_Mem(wn);
      for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
        kid = WN_kid(wn,kidno);
        if (!Find_Region(kid,stack))
          return(0);
      }
    }
    stack->Pop();
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      kid = WN_kid(wn,kidno);
      if (!Find_Region(kid,stack)) return(0);
    }
  }
  return(1);
}

// Is wn a loop variable in a common nesting of stack1 and stack2
static BOOL Loop_Var(WN *wn, DOLOOP_STACK *stack1, DOLOOP_STACK *stack2)
{
  SYMBOL sym(wn);
  INT common_nest = Common_Nest(stack1,stack2);
  for (INT i=0; i<common_nest; i++) {
    if (sym == SYMBOL(WN_index(stack1->Top_nth(i)))) {
      return TRUE;
    }
  }
  return FALSE;
}


static UINT statement_number;

// Process an outer region (an outer do loop if this is a DEPV_ARRAY graph,
// an inner do loop if this is a DEP graph)
// First make a list of all the reads and writes references
// (this saves walking the code n^2 times)
// While making the list, create a vertex for every reference
// Then actually build the dependences
// Return 0 on error
INT ARRAY_DIRECTED_GRAPH16::Build_Region(WN *start, WN* end,
DOLOOP_STACK *stack, BOOL rebuild, BOOL skip_bad)
{
  REF_LIST_STACK *writes = CXX_NEW(REF_LIST_STACK(&LNO_local_pool),
	&LNO_local_pool);
  REF_LIST_STACK *reads = CXX_NEW(REF_LIST_STACK(&LNO_local_pool),
	&LNO_local_pool);
  CALL_STACK *calls = CXX_NEW(CALL_STACK(&LNO_local_pool),
				&LNO_local_pool);

  SCALAR_STACK *scalar_writes = CXX_NEW(SCALAR_STACK(&LNO_local_pool),
	&LNO_local_pool);
  SCALAR_STACK *scalar_reads = CXX_NEW(SCALAR_STACK(&LNO_local_pool),
	&LNO_local_pool);

  statement_number = 0;

  for (WN* wn=start; wn!=WN_next(end); wn=WN_next(wn)) {
    if (!Gather_References(wn,writes,reads,stack,scalar_writes,scalar_reads,
	calls,skip_bad)) 
	return(0);
  }
  Is_True(!calls->Elements() || Has_Call_Info(calls->Bottom_nth(0)->_call) ||
    WN_operator(calls->Bottom_nth(0)->_call) == OPR_INTRINSIC_CALL ||
	Do_Loop_Is_Concurrent_Call(Enclosing_Do_Loop(start)),
	("Unexpected call in Build_Region"));

  // If there are any permuation arrays, check if they're well behaved
  // (i.e. not written in the region of interest)
  if (Permutation_Arrays->Elements()) {
    INT i;
    for (i=0; i<Permutation_Arrays->Elements(); i++) {
      Permutation_Arrays->Bottom_nth(i)._is_good = TRUE;
    }
    for (i=0; i<reads->Elements(); i++) {
      REFERENCE_ITER iter1(reads->Bottom_nth(i));
      for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty(); 
					n1=iter1.Next()) {
        WN *read = n1->Wn;
	WN *array = WN_kid0(read);
	if (WN_operator(array) == OPR_ADD) {
	  WN *kid0 = WN_kid0(array);
	  WN *kid1 = WN_kid1(array);
	  if (WN_operator(kid0) == OPR_ARRAY) {
	    array = kid0;
          } else {
	    Is_True(WN_operator(kid1) == OPR_ARRAY,
			("Bad ref in Build_Region"));
            array = kid1;
          }
        }
	Is_True(WN_operator(array) == OPR_ARRAY,
			("Bad ref in Build_Region"));
        WN *base = WN_array_base(array);
	ST *read_st = WN_st(base);
	for (INT j=0; j<Permutation_Arrays->Elements(); j++) {
	  if (read_st == Permutation_Arrays->Bottom_nth(j)._st) {
	    // found a permutation, make sure it's not written
	    BOOL is_bad = FALSE;
            for (INT k=0; k<writes->Elements() &&!is_bad; k++) {
              REFERENCE_ITER iter2(writes->Bottom_nth(k));
              for (REFERENCE_NODE *n2=iter2.First(); !iter2.Is_Empty() &&
						!is_bad; n2=iter2.Next()) {
		WN *write = n2->Wn;
	        if (Overlapped_base(Alias_Mgr,write,read)!=NOT_ALIASED){
		  is_bad = TRUE;
                }
              }
            }
            for (INT si=0; si<scalar_writes->Elements(); si++) {
              SCALAR_NODE *snode = scalar_writes->Bottom_nth(si);
              WN *scalar_write = snode->Bottom_nth(0)->Wn;
	      if (Overlapped_base(Alias_Mgr,scalar_write,read)!=NOT_ALIASED){
		is_bad = TRUE;
              }
            }
	    if (is_bad) Permutation_Arrays->Bottom_nth(j)._is_good = FALSE;
          }
        }
      }
    }
  }
                

  // Now visit the list, building the graph
  // First every write vrs every >= write
  INT i;
  for (i=0; i<writes->Elements(); i++) {
    for (INT j=i; j<writes->Elements(); j++) {
      // each element on the writes stack is a different base array
      // unless one of the base st pointers is zero
      if (i==j || !writes->Bottom_nth(i)->ST_Base || 
				!writes->Bottom_nth(j)->ST_Base) {
        REFERENCE_ITER iter1(writes->Bottom_nth(i));
        for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty(); 
					n1=iter1.Next()) {
          VINDEX16 v1=Get_Vertex(n1->Wn);
	  if (v1) { // might not exist if a bad memory caused us to give up on loop
	    REFERENCE_ITER iter2;
	    if (i == j) iter2.Init(n1);
	    else iter2.Init(writes->Bottom_nth(j));
            for (REFERENCE_NODE *n2=iter2.First(); !iter2.Is_Empty();
						n2=iter2.Next()) {
              VINDEX16 v2=Get_Vertex(n2->Wn);
	      if (v2) {
                if (rebuild) {
                  EINDEX16 e=Get_Edge(v1,v2);
	          if (e)
	            Delete_Edge(e);
                  e=Get_Edge(v2,v1);
	          if (e)
	            Delete_Edge(e);
	        }
	        if (!Add_Edge(n1->Wn,n1->Stack,n2->Wn,n2->Stack,
			n1->Statement_Number < n2->Statement_Number)) {
                  return(0);
                }
              }
            }
          }
        }
      }
    }
  }

  // Now every write vrs every read
  for (i=0; i<writes->Elements(); i++) {
    for (INT j=0; j<reads->Elements(); j++) {
      ST *base1 = writes->Bottom_nth(i)->ST_Base;
      ST *base2 = reads->Bottom_nth(j)->ST_Base;
      if (!base1 || !base2 || (base1 == base2)) {
        REFERENCE_ITER iter1(writes->Bottom_nth(i));
        for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty(); 
					n1=iter1.Next()) {
          VINDEX16 v1=Get_Vertex(n1->Wn);
	  if (v1) {
            REFERENCE_ITER iter2(reads->Bottom_nth(j)); 
            for (REFERENCE_NODE *n2=iter2.First();!iter2.Is_Empty();
							n2=iter2.Next()) {
              VINDEX16 v2=Get_Vertex(n2->Wn);
	      if (v2) {
                if (rebuild) {
                  EINDEX16 e=Get_Edge(v1,v2);
	          if (e)
	            Delete_Edge(e);
                  e=Get_Edge(v2,v1);
	          if (e)
	            Delete_Edge(e);
	        }
	        if (!Add_Edge(n1->Wn,n1->Stack,n2->Wn,n2->Stack,
			n1->Statement_Number < n2->Statement_Number)) {
	          return(0);
                }
	      }
            }
          }
        }
      }
    }
  }

  // reset permutations
  for (i=0; i<Permutation_Arrays->Elements(); i++) {
    Permutation_Arrays->Bottom_nth(i)._is_good = FALSE;
  }


  // Deal with the concurrent calls
  // the only dependece with a concurrent call
  // is an all equals dependence
  for (i=0; i<calls->Elements(); i++) {
    if (calls->Bottom_nth(i)->_is_concurrent_call) {
      WN *call = calls->Bottom_nth(i)->_call;
      DOLOOP_STACK *call_stack = calls->Bottom_nth(i)->_stack;
      UINT call_statement_number = calls->Bottom_nth(i)->_statement_number;
      VINDEX16 vcall = Get_Vertex(call);
      INT j;
      for (j=i; j<calls->Elements(); j++) {
        if (calls->Bottom_nth(j)->_is_concurrent_call) {
          WN *call2 = calls->Bottom_nth(j)->_call;
          DOLOOP_STACK *call_stack2 = calls->Bottom_nth(j)->_stack;
          UINT call_statement_number2 = calls->Bottom_nth(j)->_statement_number;
          VINDEX16 vcall2 = Get_Vertex(call2);
	  if (call_statement_number < call_statement_number2) {
            if (rebuild) {
              EINDEX16 e=Get_Edge(vcall,vcall2);
	      if (e) Delete_Edge(e);
            }
	    if (!Add_Edge_Equals(call,call_stack,call2,call_stack2)) {
	      return 0;
            }
          } else {
            if (rebuild) {
              EINDEX16 e=Get_Edge(vcall2,vcall);
	      if (e) Delete_Edge(e);
            }
	    if (!Add_Edge_Equals(call2,call_stack2,call,call_stack)) {
	      return 0;
            }
	  }
        }
      }
      for (j=0; j<writes->Elements(); j++) {
        REFERENCE_ITER iter1(writes->Bottom_nth(j));
        for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty(); 
					n1=iter1.Next()) {
          VINDEX16 v1=Get_Vertex(n1->Wn);
	  if (call_statement_number < n1->Statement_Number) {
            if (rebuild) {
              EINDEX16 e=Get_Edge(vcall,v1);
	      if (e) Delete_Edge(e);
            }
	    if (!Add_Edge_Equals(call,call_stack,n1->Wn,n1->Stack)) {
	      return 0;
            }
          } else {
            if (rebuild) {
              EINDEX16 e=Get_Edge(v1,vcall);
	      if (e) Delete_Edge(e);
            }
	    if (!Add_Edge_Equals(n1->Wn,n1->Stack,call,call_stack)) {
	      return 0;
            }
	  }
        }
      }
      for (j=0; j<reads->Elements(); j++) {
        REFERENCE_ITER iter1(reads->Bottom_nth(j));
        for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty(); 
					n1=iter1.Next()) {
          VINDEX16 v1=Get_Vertex(n1->Wn);
	  if (call_statement_number < n1->Statement_Number) {
            if (rebuild) {
              EINDEX16 e=Get_Edge(vcall,v1);
	      if (e) Delete_Edge(e);
            }
	    if (!Add_Edge_Equals(call,call_stack,n1->Wn,n1->Stack)) {
	      return 0;
            }
          } else {
            if (rebuild) {
              EINDEX16 e=Get_Edge(v1,vcall);
	      if (e) Delete_Edge(e);
            }
	    if (!Add_Edge_Equals(n1->Wn,n1->Stack,call,call_stack)) {
	      return 0;
            }
	  }
        }
      }
    }
  }

  // Now deal with non-cunccurrent calls
  for (i=0; i<calls->Elements(); i++) {
    WN *call = calls->Bottom_nth(i)->_call;
    DOLOOP_STACK *call_stack = calls->Bottom_nth(i)->_stack;
    UINT call_statement_number = calls->Bottom_nth(i)->_statement_number;
    VINDEX16 vcall = Get_Vertex(call);
    for (INT j=i; j<calls->Elements(); j++) {
      WN *call2 = calls->Bottom_nth(j)->_call;
      if (!calls->Bottom_nth(i)->_is_concurrent_call ||
	    !calls->Bottom_nth(j)->_is_concurrent_call) {
        DOLOOP_STACK *call_stack2 = calls->Bottom_nth(j)->_stack;
        UINT call_statement_number2 = calls->Bottom_nth(j)->_statement_number;
        VINDEX16 vcall2 = Get_Vertex(call2);
        if (rebuild) {
          EINDEX16 e=Get_Edge(vcall,vcall2);
	  if (e) Delete_Edge(e);
          e=Get_Edge(vcall2,vcall);
	  if (e) Delete_Edge(e);
	}
	if (!Add_Edge(call,call_stack,call2,call_stack2,
		call_statement_number < call_statement_number2)) {
	  return(0);
        }
      }
    }
    if (!calls->Bottom_nth(i)->_is_concurrent_call) {
      INT j;
      for (j=0; j<writes->Elements(); j++) {
        REFERENCE_ITER iter1(writes->Bottom_nth(j));
        for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty(); 
					n1=iter1.Next()) {
          VINDEX16 v1=Get_Vertex(n1->Wn);
          if (rebuild) {
            EINDEX16 e=Get_Edge(vcall,v1);
	    if (e) Delete_Edge(e);
          }
	  if (!Add_Edge(call,call_stack,n1->Wn,n1->Stack,
			call_statement_number < n1->Statement_Number)) {
	    return 0;
          }
        }
      }
      for (j=0; j<reads->Elements(); j++) {
        REFERENCE_ITER iter1(reads->Bottom_nth(j));
        for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty(); 
					n1=iter1.Next()) {
          VINDEX16 v1=Get_Vertex(n1->Wn);
          if (rebuild) {
            EINDEX16 e=Get_Edge(vcall,v1);
	    if (e) Delete_Edge(e);
          }
	  if (!Add_Edge(call,call_stack,n1->Wn,n1->Stack,
			call_statement_number < n1->Statement_Number)) {
	    return 0;
          }
        }
      }
    }
  }
  
  if (_type == DEPV_ARRAY_ARRAY_GRAPH) {
    // search for strange cross dependences between arrays and scalars

    // first the array writes vrs all the scalar references
    for (i=0; i<writes->Elements(); i++) {
      INT si;
      for (si=0; si<scalar_writes->Elements(); si++) {
        SCALAR_NODE *snode = scalar_writes->Bottom_nth(si);
        WN *scalar_write = snode->Bottom_nth(0)->Wn;
        ST *base = writes->Bottom_nth(i)->ST_Base;
        WN *array_write = writes->Bottom_nth(i)->Head()->Wn;
        if (!base || 
	  Overlapped_base(Alias_Mgr,scalar_write,array_write) != NOT_ALIASED) {
          REFERENCE_ITER iter1(writes->Bottom_nth(i)); 
          for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty(); 
					n1=iter1.Next()) {
            array_write = n1->Wn;
	    VINDEX16 awv = Get_Vertex(array_write);
	    if (awv) {
              if (base || 
	       Overlapped_base(Alias_Mgr,scalar_write,array_write)!=NOT_ALIASED){
	        for (INT sj=0; sj<snode->Elements(); sj++) {
	          scalar_write = snode->Bottom_nth(sj)->Wn;
	          DOLOOP_STACK *scalar_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
			&LNO_local_pool);
	          Build_Doloop_Stack(scalar_write, scalar_stack);
		  if (!Loop_Var(scalar_write,scalar_stack,n1->Stack)) {
	            VINDEX16 swv = Get_Vertex(scalar_write);
                    if (!swv) {
		      if (!Add_Vertex(scalar_write)) {
		        return 0;
                      }
		      swv = Get_Vertex(scalar_write);
                    }
                    if (rebuild) {
                      EINDEX16 e=Get_Edge(awv,swv);
	              if (e)
	                Delete_Edge(e);
                      e=Get_Edge(swv,awv);
	              if (e)
	                Delete_Edge(e);
	            }
	            if (!Add_Edge_Stars(array_write,n1->Stack,
		      scalar_write,scalar_stack,
		      n1->Statement_Number < snode->Bottom_nth(sj)->Statement_Number,
								FALSE)) {
		      return 0;
                    }
                  }
                }
              }
            }
          }
        }
      }
      for (si=0; si<scalar_reads->Elements(); si++) {
        SCALAR_NODE *snode = scalar_reads->Bottom_nth(si);
        WN *scalar_read = snode->Bottom_nth(0)->Wn;
        ST *base = writes->Bottom_nth(i)->ST_Base;
        WN *array_write = writes->Bottom_nth(i)->Head()->Wn;
        if (!base || 
	    Overlapped_base(Alias_Mgr,scalar_read,array_write) != NOT_ALIASED) {
          REFERENCE_ITER iter1(writes->Bottom_nth(i)); // put in n^2 edges
          for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty(); 
					  n1=iter1.Next()) {
            array_write = n1->Wn;
	    VINDEX16 awv = Get_Vertex(array_write);
	    if (awv) {
              if (base || 
	         Overlapped_base(Alias_Mgr,scalar_read,array_write)!=NOT_ALIASED) {
	        for (INT sj=0; sj<snode->Elements(); sj++) {
	          scalar_read = snode->Bottom_nth(sj)->Wn;
	          DOLOOP_STACK *scalar_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
			&LNO_local_pool);
	          Build_Doloop_Stack(scalar_read, scalar_stack);
	          VINDEX16 srv = Get_Vertex(scalar_read);
                  if (!srv) {
		    if (!Add_Vertex(scalar_read)) {
		      return 0;
                    }
		    srv = Get_Vertex(scalar_read);
                  }
                  if (rebuild) {
                    EINDEX16 e=Get_Edge(awv,srv);
	            if (e)
	              Delete_Edge(e);
                    e=Get_Edge(srv,awv);
	            if (e)
	              Delete_Edge(e);
	          }
	          if (!Add_Edge_Stars(array_write,n1->Stack,scalar_read,
		      scalar_stack,
		    n1->Statement_Number < snode->Bottom_nth(sj)->Statement_Number,
								  FALSE)) {
		    return 0;
                  }
                }
              }
            }
          }
        }
      }
    }

    // now the array reads vrs all the scalar writes
    for (i=0; i<reads->Elements(); i++) {
      for (INT si=0; si<scalar_writes->Elements(); si++) {
        SCALAR_NODE *snode = scalar_writes->Bottom_nth(si);
        WN *scalar_write = snode->Bottom_nth(0)->Wn;
        ST *base = reads->Bottom_nth(i)->ST_Base;
        WN *array_read = reads->Bottom_nth(i)->Head()->Wn;
        if (!base || 
	 Overlapped_base(Alias_Mgr,scalar_write,array_read) != NOT_ALIASED) {
          REFERENCE_ITER iter1(reads->Bottom_nth(i)); 
          for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty(); 
					n1=iter1.Next()) {
            array_read = n1->Wn;
	    VINDEX16 arv = Get_Vertex(array_read);
	    if (arv) {
              if (base || 
	         Overlapped_base(Alias_Mgr,scalar_write,array_read)!=NOT_ALIASED) {
	        for (INT sj=0; sj<snode->Elements(); sj++) {
	          scalar_write = snode->Bottom_nth(sj)->Wn;
	          DOLOOP_STACK *scalar_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
			&LNO_local_pool);
	          Build_Doloop_Stack(scalar_write, scalar_stack);
		  VINDEX16 swv = Get_Vertex(scalar_write);
                  if (!swv) {
		    if (!Add_Vertex(scalar_write)) {
		      return 0;
                    }
		    swv = Get_Vertex(scalar_write);
                  }
                  if (rebuild) {
                    EINDEX16 e=Get_Edge(arv,swv);
	            if (e)
	              Delete_Edge(e);
                    e=Get_Edge(swv,arv);
	            if (e)
	              Delete_Edge(e);
	          }
	          if (!Add_Edge_Stars(array_read,n1->Stack,scalar_write,
		    scalar_stack,
		    n1->Statement_Number < snode->Bottom_nth(sj)->Statement_Number,
								FALSE)) {
		    return 0;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return(1);
}

// are the two memory array references "equivalent"
// used as a guide for ivdep, if they are equivalent, we put in all equals
// dependences regardless of ivdep
static BOOL Equiv_Memory(WN *wn1, WN *wn2)
{
  if (!wn1 || !wn2) return FALSE;
  if (!WN_Equiv(wn1,wn2)) {
    return FALSE;
  }
  for (INT i=0; i<WN_kid_count(wn1); i++) {
    if (!Equiv_Memory(WN_kid(wn1,i),WN_kid(wn2,i))) {
      return FALSE;
    }
  }
  return TRUE;
}

// is any part of the address a compiler_generated array
// wn must be an expression
static BOOL Compiler_Generated(WN *wn)
{
  Is_True(OPCODE_is_expression(WN_opcode(wn)),
		("Bad wn for Compiler_Generated"));
  OPCODE opc = WN_opcode(wn);
  if (OPCODE_has_sym(opc) && ST_pt_to_compiler_generated_mem(WN_st(wn))) {
    return TRUE;
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      if (Compiler_Generated(WN_kid(wn,kidno))) {
	return TRUE;
      }
    }
  }
  return FALSE;
}

// Is this address invariant in its inner loop
// if not sure, return FALSE
static BOOL Ref_Inner_Invar(WN *addr, WN *loop)
{
  OPCODE opcode = WN_opcode(addr);
  OPERATOR oper = OPCODE_operator(opcode);
  if ((oper == OPR_LDID) || (oper == OPR_PARM)) {
    DEF_LIST *defs = Du_Mgr->Ud_Get_Def(addr);
    if (defs) {
      DEF_LIST_ITER iter(defs);
      for(DU_NODE *node=iter.First(); !iter.Is_Empty(); node=iter.Next()) {
	WN *def = node->Wn();
	WN *parent = def;
	while (parent && WN_opcode(parent) != OPC_DO_LOOP) {
	  parent = LWN_Get_Parent(parent);
        }
	if (parent == loop) return FALSE;
      }
    }
  } else if (OPCODE_is_load(opcode)) {
    return FALSE;
  }
  for (INT kidno=0; kidno<WN_kid_count(addr); kidno++) {
    if (!Ref_Inner_Invar(WN_kid(addr,kidno),loop)) {
      return FALSE;
    }
  }
  return TRUE;
}

// Find the line number associated with this references
static SRCPOS Find_Line(WN *wn)
{
  WN *tmp_wn = wn; 
  while (OPCODE_is_expression(WN_opcode(tmp_wn))) {
    tmp_wn = LWN_Get_Parent(tmp_wn);
  }
  return WN_Get_Linenum(tmp_wn);
}

// Compute the dependences and add two edges to the graph
BOOL ARRAY_DIRECTED_GRAPH16::Add_Edge(WN *ref1, const DOLOOP_STACK *s1,
				      WN *ref2, const DOLOOP_STACK *s2,
				      BOOL s1_lex_before_s2, BOOL use_bounds)
{
  OPCODE op1 = WN_opcode(ref1);
  OPCODE op2 = WN_opcode(ref2);
  OPERATOR oper1 = OPCODE_operator(op1);
  OPERATOR oper2 = OPCODE_operator(op2);
  Is_True(oper1 != OPR_ARRAY, ("ref 1 is an array in Add_Edge\n"));
  Is_True(oper2 != OPR_ARRAY, ("ref 2 is an array in Add_Edge\n"));
  if (!OPCODE_is_load(op1) && !OPCODE_is_store(op1) && !OPCODE_is_call(op1)) {
    return Add_Edge_Stars(ref1,s1,ref2,s2,s1_lex_before_s2);
  }
  if (!OPCODE_is_load(op2) && !OPCODE_is_store(op2) && !OPCODE_is_call(op2)) {
    return Add_Edge_Stars(ref1,s1,ref2,s2,s1_lex_before_s2);
  }
  if ((oper1 == OPR_LDID) || (oper1 == OPR_STID) || 
      (oper2 == OPR_LDID) || (oper2 == OPR_STID)) {
    return Add_Edge_Stars(ref1,s1,ref2,s2,s1_lex_before_s2);
  }

  if (OPCODE_is_call(op1)) {
    if (Do_Loop_Is_Concurrent_Call(Enclosing_Do_Loop(ref1))) {
      if (s1_lex_before_s2) {
        return Add_Edge_Equals(ref1,s1,ref2,s2);
      } else {
        return Add_Edge_Equals(ref2,s2,ref1,s1);
      }
    } else if (!Has_Call_Info(ref1)) {
      return Add_Edge_Stars(ref1,s1,ref2,s2,s1_lex_before_s2);
    }
  } else if (OPCODE_is_call(op2)) {
    if (Do_Loop_Is_Concurrent_Call(Enclosing_Do_Loop(ref2))) {
      if (s1_lex_before_s2) {
        return Add_Edge_Equals(ref1,s1,ref2,s2);
      } else {
        return Add_Edge_Equals(ref2,s2,ref1,s1);
      }
    } else if (!Has_Call_Info(ref2)) {
      return Add_Edge_Stars(ref1,s1,ref2,s2,s1_lex_before_s2);
    }
  }

  WN *addr1=NULL,*addr2=NULL;
  if (!OPCODE_is_call(op1) ) {
    addr1 = (OPCODE_is_store(op1) ? (WN_kid1(ref1)) : (WN_kid0(ref1))) ;
  }
  if (!OPCODE_is_call(op2)) {
    addr2 = (OPCODE_is_store(op2) ? (WN_kid1(ref2)) : (WN_kid0(ref2))) ;
  }

  BOOL is_ivdep = FALSE;
  BOOL concurrent_directive = FALSE;
  BOOL s2_lex_before_s1 = ((!s1_lex_before_s2) && !(ref1 == ref2));
  Is_True(_type!=LEVEL_ARRAY_GRAPH,
      ("Add_Edge called on a graph of type level"));
  UINT8 common_nest;
  if (_type == DEPV_ARRAY_ARRAY_GRAPH) {
    common_nest = Common_Nest(s1,s2);
    if ((common_nest == s1->Elements()) && 
	 Do_Loop_Is_Ivdep(s1->Top_nth(0))) {
      is_ivdep = TRUE;
    }
    for (INT i=0; i<common_nest; i++) {
      if (Do_Loop_Concurrent_Directive(s1->Bottom_nth(i))) {
	concurrent_directive = TRUE;
      }
    }
  } else {
    Is_True(s1->Elements() == s2->Elements(),
    ("Add_Edge called on a DEP graph with refs not in the same inner loop"));
    common_nest = s1->Elements();
    if (Do_Loop_Concurrent_Directive(s1->Top_nth(0))) {
      concurrent_directive = TRUE;
    }
    if (Do_Loop_Is_Ivdep(s1->Top_nth(0))) {
      is_ivdep = TRUE;
    }
  }
  if (is_ivdep) { // ivdep should not apply to compiler generated arrays
    if (Compiler_Generated(addr1) || 
	Compiler_Generated(addr2)) {
      is_ivdep = FALSE;
    }
  }

  if (is_ivdep && !Cray_Ivdep && !Liberal_Ivdep && 
	!OPCODE_is_call(op1) && !OPCODE_is_call(op2) &&
	Ref_Inner_Invar(addr1,s1->Top_nth(0)) && 
	Ref_Inner_Invar(addr2,s1->Top_nth(0))) {
    is_ivdep = FALSE;  // ivdep doesn't apply to "scalars"
  }

  UINT8 num_bad = Num_Bad(s1);
  if ((num_bad < common_nest) || (_type == DEP_ARRAY_GRAPH)) {
    MEM_POOL_Push(&DEP_local_pool);

    INT dv_dim; // how many dimensions of dvector to compute
    if (_type == DEPV_ARRAY_ARRAY_GRAPH) {
      dv_dim = common_nest-num_bad;  // as many as possible
    } else {
      dv_dim = 1;  // 1
    }

    DEPV_LIST *tmp = CXX_NEW(DEPV_LIST(ref1,ref2,common_nest,
	     dv_dim,use_bounds,&DEP_local_pool,s1,s2),&DEP_local_pool);
    if (!tmp->Is_Empty()) {

      VINDEX16 v1 = Get_Vertex(ref1);
      VINDEX16 v2 = Get_Vertex(ref2);
      if (v1 == 0 || v2 == 0) 
        return FALSE; 
      if (_type == DEPV_ARRAY_ARRAY_GRAPH) {
        DEPV_LIST *pos = CXX_NEW(DEPV_LIST(tmp->Num_Dim(),tmp->Num_Unused_Dim(),
					&DEP_local_pool), &DEP_local_pool);
        DEPV_LIST *neg = CXX_NEW(DEPV_LIST(tmp->Num_Dim(),tmp->Num_Unused_Dim(),
					&DEP_local_pool),&DEP_local_pool);
        if (ref1 == ref2) {
	  tmp->Lex_Pos_Decompose(&DEP_local_pool,pos,neg,FALSE,FALSE);
        } else if (s1_lex_before_s2) {
	  tmp->Lex_Pos_Decompose(&DEP_local_pool,pos,neg,TRUE,FALSE);
        } else {
	  tmp->Lex_Pos_Decompose(&DEP_local_pool,pos,neg,FALSE,TRUE);
        }


	// Get rid of non-obvious dependences that inhibit parallelism
	if (concurrent_directive) {
	  if (!OPCODE_is_call(op1) && !OPCODE_is_call(op2)) {
	    if (!Ref_Inner_Invar(addr1,s1->Top_nth(0)) &&
	          !Ref_Inner_Invar(addr2,s2->Top_nth(0))) {
	      for (INT i=0; i<dv_dim; i++) {
		if (Do_Loop_Concurrent_Directive(s1->Bottom_nth(i))) {
		  pos->Eliminate_Non_Distance_Carried_By(i);
		  neg->Eliminate_Non_Distance_Carried_By(i);
	        }
	      }
            }
          }
	}
	BOOL bad_ivdep = FALSE;
	if (is_ivdep) {
	  if (Cray_Ivdep) {
            if (s1_lex_before_s2) {
              if (neg->Is_Inner_Non_Zero_Single_Distance()) bad_ivdep = TRUE;
	      neg->Eliminate_Inner_Carried();
            } else if (s2_lex_before_s1) {
              if (pos->Is_Inner_Non_Zero_Single_Distance()) bad_ivdep = TRUE;
	      pos->Eliminate_Inner_Carried();
            } else {
              if (neg->Is_Inner_Non_Zero_Single_Distance()) bad_ivdep = TRUE;
              if (pos->Is_Inner_Non_Zero_Single_Distance()) bad_ivdep = TRUE;
	      pos->Eliminate_Inner_Carried();
	      neg->Eliminate_Inner_Carried();
            }
          } else if (Liberal_Ivdep) {
	      if ((ref1 != ref2) && Equiv_Memory(addr1,addr2)) bad_ivdep = TRUE;
              if (neg->Is_Inner_Non_Zero_Single_Distance()) bad_ivdep = TRUE;
              if (pos->Is_Inner_Non_Zero_Single_Distance()) bad_ivdep = TRUE;
	      pos->Eliminate_Inner_Carried_Or_All_Equals();
	      neg->Eliminate_Inner_Carried_Or_All_Equals();
          } else {
              if (neg->Is_Inner_Non_Zero_Single_Distance()) {
		bad_ivdep = TRUE;
              }
              if (pos->Is_Inner_Non_Zero_Single_Distance()) {
		bad_ivdep = TRUE;
              }
	      pos->Eliminate_Inner_Carried();
	      neg->Eliminate_Inner_Carried();
          }
        }
	if (bad_ivdep) {
	  char error[120];
	  sprintf(error,
	    "IVDEP where there is an obvious dependence to ref on line %d.  Dependence ignored.",
	    Srcpos_To_Line(Find_Line(ref2)));
	  ErrMsgSrcpos(EC_LNO_Generic,Find_Line(ref1),error);
        }

        if (!pos->Is_Empty()) {
          DEPV_ARRAY *array = Create_DEPV_ARRAY(pos,_pool);
          if (!Add_Edge(v1, v2,array)) {
            MEM_POOL_Pop(&DEP_local_pool);
	    return(FALSE);
	  }
        }
        if (!neg->Is_Empty() && (ref2 != ref1)) {
          DEPV_ARRAY *array = Create_DEPV_ARRAY(neg,_pool);
          if (!Add_Edge(v2, v1,array)) {
            MEM_POOL_Pop(&DEP_local_pool);
	    return(FALSE);
          }
        }
      } else { // a DEP_ARRAY_GRAPH
	DEP tmp_dep = tmp->Convert_To_Dep();
	DEP *pos,*neg;
        if (ref1 == ref2) {
	  DEP_Lex_Pos_Decompose(tmp_dep,&DEP_local_pool,&pos,&neg,0,0);
        } else if (s1_lex_before_s2) {
	  DEP_Lex_Pos_Decompose(tmp_dep,&DEP_local_pool,&pos,&neg,TRUE,FALSE);
        } else {
	  DEP_Lex_Pos_Decompose(tmp_dep,&DEP_local_pool,&pos,&neg,FALSE,TRUE);
        }

	BOOL bad_ivdep = FALSE;
	if (is_ivdep) {
	  if (Cray_Ivdep) {
	    if (DEP_IsDistance(tmp_dep)&&DEP_Distance(tmp_dep)) bad_ivdep=TRUE;
	    if (s1_lex_before_s2) {
	      neg = NULL;
	    } else if (s2_lex_before_s1) {
	      pos = NULL;
            } else {
	      neg = pos = NULL;
            }
	  } else if (Liberal_Ivdep) { 
	    if ((ref1 != ref2) && Equiv_Memory(addr1,addr2)) bad_ivdep = TRUE;
	    if (DEP_IsDistance(tmp_dep)&&DEP_Distance(tmp_dep)) bad_ivdep=TRUE;
	    neg = pos = NULL;
          } else {
	    if (DEP_IsDistance(tmp_dep)&&DEP_Distance(tmp_dep)) bad_ivdep=TRUE;
	    if (!neg || (DEP_Direction(*neg) == DIR_POS)) {
	      neg = NULL;
            } else {
	      *neg = DEP_SetDistance(0);
            }
	    if (!pos || (DEP_Direction(*pos) == DIR_POS)) {
	      pos = NULL;
            } else {
	      *pos = DEP_SetDistance(0);
            }
          }
        }
	if (bad_ivdep) {
	  char error[120];
	  sprintf(error,
	    "IVDEP where there is an obvious dependence to ref on line %d.  Dependence ignored.",
	    Srcpos_To_Line(Find_Line(ref2)));
	  ErrMsgSrcpos(EC_LNO_Generic,Find_Line(ref1),error);
        }

	if (pos) {	
          if (!Add_Edge(v1, v2,*pos)) {
            MEM_POOL_Pop(&DEP_local_pool);
	    return(FALSE);
	  }
	}
        if (neg && (ref2 != ref1)) {
          if (!Add_Edge(v2, v1,*neg)) {
            MEM_POOL_Pop(&DEP_local_pool);
	    return(FALSE);
	  }
        }
      }
    }
    MEM_POOL_Pop(&DEP_local_pool);
  }
  return(TRUE);
}


// Compute the dependences and add two edges to the graph
BOOL ARRAY_DIRECTED_GRAPH16::Add_Edge_Stars(WN *ls1, const DOLOOP_STACK *s1,
					    WN *ls2, const DOLOOP_STACK *s2,
					    BOOL s1_lex_before_s2,
					    BOOL pos_only)
{
  Is_True(OPCODE_is_load(WN_opcode(ls1)) || OPCODE_is_store(WN_opcode(ls1))
	  || OPCODE_is_call(WN_opcode(ls1)),
    ("bad ls1 in Add_Edge_Stars\n"));
  Is_True(OPCODE_is_load(WN_opcode(ls2)) || OPCODE_is_store(WN_opcode(ls2))
	  || OPCODE_is_call(WN_opcode(ls2)),
    ("bad ls2 in Add_Edge_Stars\n"));
  UINT8 common_nest;
  if (_type == DEPV_ARRAY_ARRAY_GRAPH) {
    common_nest = Common_Nest(s1,s2);
  } else {
    Is_True(s1->Elements() == s2->Elements(),
    ("Add_Edge called on a DEP graph with refs not in the same inner loop"));
    common_nest = s1->Elements();
  }
  UINT8 num_bad = Num_Bad(s1);
  if (num_bad < common_nest) {
    MEM_POOL_Push(&DEP_local_pool);

    INT dv_dim; // how many dimensions of dvector to compute
    if (_type == DEPV_ARRAY_ARRAY_GRAPH) {
      dv_dim = common_nest-num_bad;  // as many as possible
    } else {
      dv_dim = 1;  // 1
    }

    DEPV_ARRAY *dv_array = Create_DEPV_ARRAY(1, dv_dim, common_nest-dv_dim,
					     &DEP_local_pool);
    for (INT ii = 0; ii < dv_dim; ii++)
      DEPV_Dep(dv_array->Depv(0), ii) = DEP_SetDirection(DIR_STAR);
    DEPV_LIST *tmp = CXX_NEW(DEPV_LIST(dv_array, &DEP_local_pool),
			     &DEP_local_pool);

    VINDEX16 v1 = Get_Vertex(ls1);
    VINDEX16 v2 = Get_Vertex(ls2);
    if (v1 == 0 || v2 == 0) 
      return FALSE; 
    if (_type == DEPV_ARRAY_ARRAY_GRAPH) {
      DEPV_LIST *pos = CXX_NEW(DEPV_LIST(tmp->Num_Dim(),tmp->Num_Unused_Dim(),
					 &DEP_local_pool), &DEP_local_pool);
      DEPV_LIST *neg = CXX_NEW(DEPV_LIST(tmp->Num_Dim(),tmp->Num_Unused_Dim(),
					 &DEP_local_pool),&DEP_local_pool);
      if (ls1 == ls2) {
	tmp->Lex_Pos_Decompose(&DEP_local_pool,pos,neg,FALSE,FALSE);
      } else if (s1_lex_before_s2) {
	tmp->Lex_Pos_Decompose(&DEP_local_pool,pos,neg,TRUE,FALSE);
      } else {
	tmp->Lex_Pos_Decompose(&DEP_local_pool,pos,neg,FALSE,TRUE);
      }

      if (!pos->Is_Empty()) {
	DEPV_ARRAY *array = Create_DEPV_ARRAY(pos,_pool);
	if (!Add_Edge(v1, v2,array)) {
	  MEM_POOL_Pop(&DEP_local_pool);
	  return(FALSE);
	}
      }
      if (!pos_only && !neg->Is_Empty() && (ls2 != ls1)) {
	DEPV_ARRAY *array = Create_DEPV_ARRAY(neg,_pool);
	if (!Add_Edge(v2, v1,array)) {
	  MEM_POOL_Pop(&DEP_local_pool);
	  return(FALSE);
	}
      }
    } else { // a DEP_ARRAY_GRAPH
      DEP tmp_dep = tmp->Convert_To_Dep();
      DEP *pos,*neg;
      if (ls1 == ls2) {
	DEP_Lex_Pos_Decompose(tmp_dep,&DEP_local_pool,&pos,&neg,0,0);
      } else if (s1_lex_before_s2) {
	DEP_Lex_Pos_Decompose(tmp_dep,&DEP_local_pool,&pos,&neg,TRUE,FALSE);
      } else {
	DEP_Lex_Pos_Decompose(tmp_dep,&DEP_local_pool,&pos,&neg,FALSE,TRUE);
      }
      if (pos) {	
	if (!Add_Edge(v1, v2,*pos)) {
	  MEM_POOL_Pop(&DEP_local_pool);
	  return(FALSE);
	}
      }
      if (!pos_only && neg && (ls2 != ls1)) {
	if (!Add_Edge(v2, v1,*neg)) {
	  MEM_POOL_Pop(&DEP_local_pool);
	  return(FALSE);
        }
      }
    }
    MEM_POOL_Pop(&DEP_local_pool);
  }
  return(TRUE);
}

// Add an all equals dependence from ls1 to ls2
BOOL ARRAY_DIRECTED_GRAPH16::Add_Edge_Equals(WN *ls1, const DOLOOP_STACK *s1,
					    WN *ls2, const DOLOOP_STACK *s2)
{
  Is_True(OPCODE_is_load(WN_opcode(ls1)) || OPCODE_is_store(WN_opcode(ls1))
	  || OPCODE_is_call(WN_opcode(ls1)), 
    ("bad ls1 in Add_Edge_Equals\n"));
  Is_True(OPCODE_is_load(WN_opcode(ls2)) || OPCODE_is_store(WN_opcode(ls2))
	  || OPCODE_is_call(WN_opcode(ls2)),
    ("bad ls2 in Add_Edge_Equals\n"));
  UINT8 common_nest;
  if (_type == DEPV_ARRAY_ARRAY_GRAPH) {
    common_nest = Common_Nest(s1,s2);
  } else {
    Is_True(s1->Elements() == s2->Elements(),
    ("Add_Edge called on a DEP graph with refs not in the same inner loop"));
    common_nest = s1->Elements();
  }
  UINT8 num_bad = Num_Bad(s1);
  if (num_bad < common_nest) {
    MEM_POOL_Push(&DEP_local_pool);

    INT dv_dim; // how many dimensions of dvector to compute
    if (_type == DEPV_ARRAY_ARRAY_GRAPH) {
      dv_dim = common_nest-num_bad;  // as many as possible
    } else {
      dv_dim = 1;  // 1
    }

    VINDEX16 v1 = Get_Vertex(ls1);
    VINDEX16 v2 = Get_Vertex(ls2);
    if (v1 == 0 || v2 == 0) 
      return FALSE; 
    if (_type == DEPV_ARRAY_ARRAY_GRAPH) {
      DEPV_ARRAY *dv_array = Create_DEPV_ARRAY(1, dv_dim, common_nest-dv_dim,
					     _pool);
      for (INT ii = 0; ii < dv_dim; ii++) {
        DEPV_Dep(dv_array->Depv(0), ii) = DEP_SetDirection(DIR_EQ);
      }
      if (!Add_Edge(v1, v2,dv_array)) {
	MEM_POOL_Pop(&DEP_local_pool);
	return(FALSE);
      }
    } else { // a DEP_ARRAY_GRAPH
      DEP dep = DEP_SetDistance(0);
      if (!Add_Edge(v1, v2,dep)) {
	MEM_POOL_Pop(&DEP_local_pool);
	return(FALSE);
      }
    }
  }
  return(TRUE);
}


// Add all array reads and writes onto the appropriate list
// We have one list for each base array (based on an arrays ST_Base pointer).
// For efficiency reasons, we don't call dependence analysis on arrays with
// different base pointers.  They can never be aliased.  If we can't figure
// out the base pointer, we set it to zero.  We call the dependence routines
// on every zero-based array vrs every other array.
//
// Each reference is also annotated with the number of its statement
// Statements are numbered so that if a comes lexically before b,
// its statement number is less than b's (note that the converse
// is not true due to IF nodes (i.e. we say all the statements in the
// then clause come before all the statements in the else))
// Return 0 on error overflow
//
// Add all non-preg no loop_variable scalar writes/reads to the appropriate 
// SCALAR_STACK we use these to check for weird cross dependences 
// (scalar-array)
INT ARRAY_DIRECTED_GRAPH16::Gather_References(WN *wn, REF_LIST_STACK *writes,
	REF_LIST_STACK *reads, DOLOOP_STACK *stack,
	SCALAR_STACK *scalar_writes, SCALAR_STACK *scalar_reads,
	CALL_STACK *calls, BOOL skip_bad) 
{
  WN *kid;

  if (OPCODE_is_stmt(WN_opcode(wn)) || OPCODE_is_scf(WN_opcode(wn))) {
    statement_number++;
  }

  if (WN_opcode(wn) == OPC_BLOCK) {
    kid = WN_first (wn);
    while (kid) {
      if (!Gather_References(kid,writes,reads,stack,scalar_writes,
		scalar_reads,calls,skip_bad)) return(0);
      kid = WN_next(kid);
    }
    return (1);
  } 
  
  if (WN_opcode(wn) == OPC_DO_LOOP) {
    if (!skip_bad || Do_Loop_Is_Good(wn)) {
      DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
      dli->Has_Bad_Mem = FALSE;
      WN *body = WN_do_body(wn);
      for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
        kid = WN_kid(wn,kidno);
        if (kid != body) {
          if (!Gather_References(kid,writes,reads,stack,
		  scalar_writes,scalar_reads,calls,skip_bad)) { 
            return(0);
          }
        }
      }
      stack->Push(wn);
      if (!Gather_References(body,writes,reads,stack,scalar_writes,
					scalar_reads,calls,skip_bad)) { 
        return(0);
      }
      stack->Pop();
      dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
      if (dli->Has_Bad_Mem && _type==DEPV_ARRAY_ARRAY_GRAPH) { 
        LNO_Erase_Vertices_In_Loop (wn,this);
      }
    }
  } else if (OPCODE_is_call(WN_opcode(wn))) {
    VINDEX16 vindex = Get_Vertex(wn);
    if (!vindex) {
      vindex = Add_Vertex(wn);
    }
    if (!vindex) {
      DevWarn("Out of space for vertex?");
      Set_Bad_Mem(wn);
      return 0;
    }
    DOLOOP_STACK *s = Copy_Doloop_Stack(stack,&LNO_local_pool);
    calls->Push(wn,statement_number,s,
	Do_Loop_Is_Concurrent_Call(s->Top_nth(0)));
  } else if (WN_opcode(wn) == OPC_IO) {
    Set_Bad_Mem(wn);
    return 1;
  } else if (OPCODE_is_load(WN_opcode(wn))) {
    OPCODE opcode = WN_opcode(wn);
    if (OPCODE_has_1ty(opcode) && TY_is_volatile(WN_ty(wn))) {
      Set_Bad_Mem(wn);
      return 1;
    } else if (OPCODE_has_2ty(opcode)&&(TY_is_volatile(WN_ty(wn)) ||
                                        TY_is_volatile(WN_load_addr_ty(wn)))) {
      Set_Bad_Mem(wn);
      return 1;
    } 

    if ((WN_kid_count(wn) == 1)) {

      if (stack->Elements()>0) {
	WN *array = WN_kid0(wn);
	if (WN_operator(array) == OPR_ADD) {
	  // arrays of structures
	  OPERATOR kid0 = WN_operator(WN_kid0(array));
	  OPERATOR kid1 = WN_operator(WN_kid1(array));
	  if ((kid0 == OPR_ARRAY) && (kid1 == OPR_INTCONST)) {
	    array = WN_kid0(array);
          } else if ((kid1 == OPR_ARRAY) && (kid0 == OPR_INTCONST)) {
	    array = WN_kid1(array);
          } else {
	    Set_Bad_Mem(wn);
	    return 1;
          }
        } else if (WN_operator(array) != OPR_ARRAY) {
	  Set_Bad_Mem(wn);
	  return 1;
        }

	
        DOLOOP_STACK *s = Copy_Doloop_Stack(stack,&LNO_local_pool);

        VINDEX16 vindex = Get_Vertex(wn);
        if (!vindex) {
	  vindex = Add_Vertex(wn);
        }
        if (!vindex) {
	    DevWarn("Out of space for vertex?");
	    if (WN_operator(wn) != OPR_LDID)
	      Set_Bad_Mem(wn);
	    return 0;
	  }
	  WN *base = WN_array_base(array);
	  ST *st_base=Get_ST_Base(base);
          INT i;
	  for (i=0;i<reads->Elements()&&
		   !(reads->Bottom_nth(i)->ST_Base==st_base); i++); 
	  if (i==reads->Elements()) {
	    REFERENCE_LIST *rl = 
	      CXX_NEW(REFERENCE_LIST(st_base, wn),&LNO_local_pool);
	    reads->Push(rl);
	  } 
	  reads->Bottom_nth(i)->Append(CXX_NEW(REFERENCE_NODE(wn,s,
			statement_number), &LNO_local_pool));
	} else {
	  if ((WN_operator(wn) != OPR_LDID)
	      && !Is_Loop_Invariant_Indir(wn))
	    Set_Bad_Mem(wn);
	  VINDEX16 vindex = Get_Vertex(wn);
	  if (vindex)
	    Delete_Vertex(vindex);
	}
      } else if (WN_operator(wn) == OPR_LDID) {
	if (ST_class(WN_st(wn)) != CLASS_PREG && stack->Elements()>0) {
	  scalar_reads->Add_Scalar(wn,statement_number);
	}
      } else { 
	Set_Bad_Mem(wn);
      }
    } else if (OPCODE_is_store(WN_opcode(wn))) {
      TY_IDX ty = WN_ty(wn);
      if (TY_is_volatile(ty)) {
	Set_Bad_Mem(wn);
	return 1;
      } else if ((WN_operator(wn) != OPR_STID) &&
	   TY_is_volatile(TY_pointed(WN_ty(wn)))) {
	Set_Bad_Mem(wn);
	return 1;
      }
	

      if (WN_kid_count(wn) == 2) {
	DOLOOP_STACK *s = Copy_Doloop_Stack(stack,&LNO_local_pool);
	VINDEX16 vindex = Get_Vertex(wn);
	if (stack->Elements() > 0) { 
	  WN *array = WN_kid1(wn);
	  if (WN_operator(array) == OPR_ADD) {
	    // arrays of structures
	    OPERATOR kid0 = WN_operator(WN_kid0(array));
	    OPERATOR kid1 = WN_operator(WN_kid1(array));
	    if ((kid0 == OPR_ARRAY) && (kid1 == OPR_INTCONST)) {
	      array = WN_kid0(array);
	    } else if ((kid1 == OPR_ARRAY) && (kid0 == OPR_INTCONST)) {
	      array = WN_kid1(array);
	    } else {
	      Set_Bad_Mem(wn);
	      return 1;
	    }
	  } else if (WN_operator(array) != OPR_ARRAY) {
	    Set_Bad_Mem(wn);
	    return 1;
	  }

	  
	  if (!vindex) {
	    vindex = Add_Vertex(wn);
	  }
	  if (!vindex) {
	    DevWarn("Out of space for vertex?");
          if (WN_operator(wn) != OPR_STID)
            Set_Bad_Mem(wn);
          return 0;
        }
  
        WN *base = WN_array_base(array);
        ST *st_base=Get_ST_Base(base);
        INT i;
        for (i=0;i<writes->Elements() && 
	         !(writes->Bottom_nth(i)->ST_Base==st_base); i++); 
        if (i==writes->Elements()) {
	  REFERENCE_LIST *rl = 
	    CXX_NEW(REFERENCE_LIST(st_base,wn),&LNO_local_pool);
	  writes->Push(rl);
        } 
        writes->Bottom_nth(i)->Append(CXX_NEW(REFERENCE_NODE(wn,s,
		      statement_number), &LNO_local_pool));
      } else {
        if (WN_operator(wn) != OPR_STID)
          Set_Bad_Mem(wn);
	VINDEX16 vindex = Get_Vertex(wn);
	if (vindex)
	  Delete_Vertex(vindex);
      }
    } else if (WN_operator(wn) == OPR_STID) {
      if (ST_class(WN_st(wn)) != CLASS_PREG && stack->Elements()>0) {
        scalar_writes->Add_Scalar(wn,statement_number);
      }
    } else { 
      Set_Bad_Mem(wn);
    }
  } else if (WN_operator(wn)  == OPR_ARRAY) {
    WN *ancestor = LWN_Get_Parent(wn);
    while (ancestor && !OPCODE_is_store(WN_opcode(ancestor)) && 
	             !OPCODE_is_load(WN_opcode(ancestor))) { 
      if (WN_operator(ancestor) == OPR_PARM) {
	if (stack->Elements() > 0 
	    && !Do_Loop_Is_Concurrent_Call(stack->Top_nth(0))
	    && !Has_Call_Info(LWN_Get_Parent(ancestor))) {
          Set_Bad_Mem(wn);
	}
      } 
      ancestor = LWN_Get_Parent(ancestor);
    }
  }


  if (WN_opcode(wn) != OPC_DO_LOOP)  {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      kid = WN_kid(wn,kidno);
      if (!Gather_References(kid,writes,reads,stack,
		scalar_writes,scalar_reads,calls,skip_bad)) { 
        return(0);
      }
    }
  }
  return(1);
}

// what is the Do loop enclosing this node
// This assumes that wn is enclosed in a DO loop
static WN *Get_Do(WN *wn)
{
  while (wn && WN_opcode(wn) != OPC_DO_LOOP) {
    wn = LWN_Get_Parent(wn);
  }
  FmtAssert(wn, ("Missing enclosing do loop"));
  return (wn);
}

// Convert the DEPV_ARRAY into a DEP graph 
// Return 0 on error
INT ARRAY_DIRECTED_GRAPH16::Build(ARRAY_DIRECTED_GRAPH16 *da_graph)
{
  MEM_POOL_Push(&LNO_local_pool);
  Is_True((_type==DEP_ARRAY_GRAPH) && 
	  (da_graph->_type==DEPV_ARRAY_ARRAY_GRAPH),
      ("Build called on wrong types of graph"));
  
  // First copy the inner loop vertices
  // Don't copy ldids/stids, they appear in the DEPV_ARRAY graph but
  // not in the DEP graph
  // Since we're not copying them, we're setting Bad_Mem on loops that
  // have them
  for (VINDEX16 v=da_graph->Get_Vertex(); v; v=da_graph->Get_Next_Vertex(v)) {
    WN *wn = da_graph->Get_Wn(v);
    WN *do_wn = Get_Do(wn);
    if (Do_Loop_Is_Inner(do_wn) && !Do_Loop_Has_Gotos(do_wn)) {
      OPERATOR oper = WN_operator(wn);
      if ((oper != OPR_LDID) && (oper != OPR_STID) && (oper != OPR_CALL)) { 
        if (!Add_Vertex(da_graph->Get_Wn(v))) {
          return(0);
        }
      } else {
	Set_Bad_Mem(wn);
      }
    }
  }

  // now the edges
  for (VINDEX16 source_v=da_graph->Get_Vertex(); source_v; 
		source_v=da_graph->Get_Next_Vertex(source_v)) {
    EINDEX16 e = da_graph->_v[source_v].Get_Out_Edge();
    WN *source_wn = da_graph->Get_Wn(source_v);
    WN *inner_do = Get_Do(source_wn);
    if (Do_Loop_Is_Inner(inner_do) && !Do_Loop_Has_Gotos(inner_do)) {
      OPERATOR source_oper = WN_operator(source_wn);
      if ((source_oper != OPR_LDID)&&(source_oper != OPR_STID)
	  && (source_oper != OPR_CALL)) { 
        while (e) {
          DEPV_ARRAY *da = da_graph->_e[e].Depv_Array;
          // Is this an edge to a vertex in the same inner loop
          VINDEX16 sink_v = da_graph->_e[e].Get_Sink();
	  WN *sink_wn = da_graph->Get_Wn(sink_v);
          WN *inner_do2 = Get_Do(sink_wn);
          if (inner_do == inner_do2) {
            OPERATOR sink_oper = WN_operator(sink_wn);
            if ((sink_oper != OPR_LDID) && (sink_oper != OPR_STID)
		&& (sink_oper != OPR_CALL)) { 
	      // find the reverse edge, we do two edges at a time
	      // because dividing by step might cause us switch edges
	      DEPV_ARRAY *da2=NULL;
	      DEP *dep=NULL;
	      DEP *dep2 = NULL;
  
	      EINDEX16 e2 = da_graph->Get_Edge(sink_v,source_v);
	      if (e2 && (e < e2)) { // make sure we only get each edge once
                dep = da->Shorten_To_Dep(&LNO_local_pool);
	        da2 = da_graph->_e[e2].Depv_Array;
	        dep2 = da2->Shorten_To_Dep(&LNO_local_pool);
	      } else if (e == e2) {
                dep = da->Shorten_To_Dep(&LNO_local_pool);
	      } else if (!e2) {
                dep = da->Shorten_To_Dep(&LNO_local_pool);
	      }
	      VINDEX16 source_v2 = Get_Vertex(source_wn); // the corresponding
						// vertex in this graph
              WN *sink_wn = da_graph->Get_Wn(sink_v);
	      VINDEX16 sink_v2 = Get_Vertex(sink_wn);
	      if (dep && !Add_Edge(source_v2,sink_v2,*dep)) {
                MEM_POOL_Pop(&LNO_local_pool);
	        return(0);
	      }
	      if (dep2 && !Add_Edge(sink_v2,source_v2,*dep2)) {
                MEM_POOL_Pop(&LNO_local_pool);
	        return(0);
	      }
            } 
          } 
          e = da_graph->_e[e].Get_Next_Out_Edge();
        }
      }
    }
  }
  MEM_POOL_Pop(&LNO_local_pool);
  Add_Must();
  return(1);
}


INT ARRAY_DIRECTED_GRAPH16::Add_Deps_To_Copy_Block(WN *orig, WN *copy,
BOOL keep_internal_edge)
{
  Is_True(_type==DEPV_ARRAY_ARRAY_GRAPH,
      ("Add_Deps_To_Copy_Block called on a non-DEPV_ARRAY graph"));
  INT result;
  MEM_POOL_Push(&LNO_local_pool);
  {
  // a hash table to map old vertices to the corresponding new ones
  // we could use the mapping mechanism, but it's a waste since this
  // is purely local info
  HASH_TABLE<VINDEX16,VINDEX16> hash_table(MIN(Get_Vertex_Count(),512),
	&LNO_local_pool);

  result = Add_Deps_To_Copy_Block_V(orig, copy,&hash_table);
  if (result)
    result=Add_Deps_To_Copy_Block_E(orig,copy,&hash_table,keep_internal_edge);

  }
  MEM_POOL_Pop(&LNO_local_pool);
  return result;
}

// Add the new vertices to the graph
// Attacth the new vertices to the wn using _map
// Enter the mapping from old vertices to new into the hash table
// Return 0 on error
INT ARRAY_DIRECTED_GRAPH16::Add_Deps_To_Copy_Block_V(WN *orig, WN *copy,
		HASH_TABLE<VINDEX16,VINDEX16> *hash_table)
{
  if (orig) {
    if (OPCODE_is_load(WN_opcode(orig)) || 
	OPCODE_is_store(WN_opcode(orig)) ||
	OPCODE_is_call(WN_opcode(orig))) {
      VINDEX16 origv = Get_Vertex(orig);
      if (origv) {
        VINDEX16 newv = Add_Vertex(copy);
	if (!newv) return(0);
        hash_table->Enter(origv, newv);
      } 
    }

    if (WN_opcode(orig) == OPC_BLOCK) {
      WN *orig_kid = WN_first (orig);
      WN *copy_kid = WN_first (copy);
      while (orig_kid) {
        if (!Add_Deps_To_Copy_Block_V(orig_kid, copy_kid,hash_table)) {
	  return 0;
	}
        orig_kid = WN_next(orig_kid);
        copy_kid = WN_next(copy_kid);
      }
    } else {
      for (INT kidno=0; kidno<WN_kid_count(orig); kidno++) {
        if (!Add_Deps_To_Copy_Block_V(WN_kid(orig,kidno), WN_kid(copy,kidno), 
							hash_table)) {
          return 0;
	}
      }
    }
  }
  return 1;
}

// Add the edges to the graph
// For each vertex V in the region
//   Check all its out edges
//     If an out edge is to a vertex in the region, add an edge to the copy
//     If an out edge is to a vertex outside of the region, add edge from
//	 copy of source to orig sink
//   Check all its in edges
//     If an in edge is to a vertex in the region, do nothing (caught above)
//     If an in edge is to a vertex outside the region, add an edge from orig
// return 0 on error
INT ARRAY_DIRECTED_GRAPH16::Add_Deps_To_Copy_Block_E(WN *orig, WN *copy,
	HASH_TABLE<VINDEX16,VINDEX16> *hash_table, BOOL keep_internal_edge)
{
  VINDEX16 newv, new_sinkv;

  if (orig) {
    if (OPCODE_is_load(WN_opcode(orig)) || 
	OPCODE_is_store(WN_opcode(orig))  ||
	OPCODE_is_call(WN_opcode(orig))) {
      VINDEX16 origv = Get_Vertex(orig);
      // get all the edges from this vertex
      if (origv) {
	newv = hash_table->Find(origv);
	// newv is the corresponding vertex, now find all the edges
	EINDEX16 edge = Get_Out_Edge(origv);
	while (edge) {
	  VINDEX16 orig_sinkv = Get_Sink(edge);
	  VINDEX16 hash_element = hash_table->Find( orig_sinkv);
	  if (hash_element) {  // the sink is in the copy region
	    if (keep_internal_edge) {
	      
	      new_sinkv = (VINDEX16) (UINT) hash_element;
	      if (!Add_Edge(newv,new_sinkv,
	    		Create_DEPV_ARRAY(Depv_Array(edge),_pool))) {
	        return 0;
	      }
	    }
	  } else {
	    new_sinkv = orig_sinkv;
	    if (!Add_Edge(newv,new_sinkv,
	    		Create_DEPV_ARRAY(Depv_Array(edge),_pool))) {
	      return 0;
	    }
	  }
	  edge = Get_Next_Out_Edge(edge);
	}

	edge = Get_In_Edge(origv);
	while (edge) {
	  VINDEX16 orig_sourcev = Get_Source(edge);
	  VINDEX16 hash_element = hash_table->Find(orig_sourcev);
	  if (!hash_element) {  // the sink is not in the copy region
	    if (!Add_Edge(orig_sourcev,newv,
	    		Create_DEPV_ARRAY(Depv_Array(edge),_pool))) {
	      return 0;
	    }
	  }
	  edge = Get_Next_In_Edge(edge);
	}
      }
    }

    if (WN_opcode(orig) == OPC_BLOCK) {
      WN *orig_kid = WN_first (orig);
      WN *copy_kid = WN_first (copy);
      while (orig_kid) {
        if (!Add_Deps_To_Copy_Block_E(orig_kid,copy_kid,
	                              hash_table, keep_internal_edge)) {
	  return 0;
	}
        orig_kid = WN_next(orig_kid);
        copy_kid = WN_next(copy_kid);
      }
    } else {
      for (INT kidno=0; kidno<WN_kid_count(orig); kidno++) {
        if (!Add_Deps_To_Copy_Block_E(WN_kid(orig,kidno), WN_kid(copy,kidno), 
				hash_table, keep_internal_edge)) {
          return 0;
	}
      }
    }
  }
  return 1;
}


// Copy a DOLOOP_STACK
static DOLOOP_STACK *Copy_Doloop_Stack(DOLOOP_STACK *orig,MEM_POOL *pool)
{
  DOLOOP_STACK *copy = CXX_NEW(DOLOOP_STACK(pool),pool);
  INT elements = orig->Elements();
  for (INT i=0; i<elements; i++) {
    copy->Push(orig->Bottom_nth(i));
  }
  return(copy);
}


static INT Common_Nest(const DOLOOP_STACK *s1, const DOLOOP_STACK *s2) 
{
  INT i;
  for (i=0; i<MIN(s1->Elements(),s2->Elements()); i++) {
    if (s1->Bottom_nth(i) != s2->Bottom_nth(i)) return(i);
  }
  return(i);
}

// how many of the outer loops are bad
static INT Num_Bad(const DOLOOP_STACK *s1)
{
  INT i;
  for (i=0; i<s1->Elements() && !Do_Loop_Is_Good(s1->Bottom_nth(i)); i++);
  return(i);
}

static INT lex_count;

// Fix up the dependence graph after loop unrolling.
// First, 
//   Add the new vertices to the graph
//   Attach the new vertices to the wn using _map
//   Enter the mapping from old vertices to new into the hash table
// Then get the edges right
//   Each edge in to or out of the region gets copied
//   For each edge within region 
//	if (d+n) % u == 0 repace distance component d = (d+n)/u 
//	otherwise get rid of the edge
//   where n is the "copy number" 
// Return 0 on error
INT ARRAY_DIRECTED_GRAPH16::Unrolled_Dependences_Update(
			WN** bodies, UINT u, UINT loopno)
{
  Is_True(_type==DEPV_ARRAY_ARRAY_GRAPH,
      ("Unrolled_Dependences_Update called on a non-DEPV_ARRAY graph"));
  INT result;
  MEM_POOL_Push(&LNO_local_pool);

  // a hash table to map old vertices to arrays of the corresponding new ones
  // and a count giving the lexical position of the original vertex
  //
  // we could use the mapping mechanism, but it's a waste since this
  // is purely local info
  typedef HASH_TABLE<VINDEX16,VINDEX16P_LEX_COUNT *> HTABLE_TYPE;
  HTABLE_TYPE *hash_table = 
     CXX_NEW(HTABLE_TYPE(MIN(Get_Vertex_Count(),512),&LNO_local_pool),
     &LNO_local_pool);

  // a list of all the original vertices in bodies[0]
  VINDEX16_STACK *orig_vertices = 
	CXX_NEW(VINDEX16_STACK(&LNO_local_pool),&LNO_local_pool);

  lex_count=0;
  result = Unrolled_Dependences_Update_V(bodies,u,hash_table,orig_vertices);
  if (!result) {
    CXX_DELETE(hash_table, &LNO_local_pool);
    MEM_POOL_Pop(&LNO_local_pool);
    return(0);
  }

  result = Unrolled_Dependences_Update_E(u,loopno,hash_table,orig_vertices);

  CXX_DELETE(hash_table, &LNO_local_pool);
  CXX_DELETE(orig_vertices, &LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
  return(result);
}

// Add the new vertices to the graph
// Attacth the new vertices to the wn using _map
// Enter the mapping from old vertices to new into the hash table
// Return 0 on error
INT ARRAY_DIRECTED_GRAPH16::Unrolled_Dependences_Update_V(WN **bodies, 
		UINT u, HASH_TABLE<VINDEX16,VINDEX16P_LEX_COUNT *> *hash_table,
		VINDEX16_STACK *orig_vertices)
{
  if (bodies[0]) {

    if (WN_opcode(bodies[0]) == OPC_BLOCK) {
      WN **new_bodies = CXX_NEW_ARRAY(WN *,u,&LNO_local_pool);
      for (INT i=0; i<u; i++) {
	new_bodies[i] = WN_first(bodies[i]);
      }
      while (new_bodies[0]) {
        if (!Unrolled_Dependences_Update_V(new_bodies, u,hash_table,
							orig_vertices)) {
	  return 0;
	}
        for (INT i=0; i<u; i++) {
	  new_bodies[i] = WN_next(new_bodies[i]);
        }
      }
    } else if (WN_kid_count(bodies[0])) {
      WN **new_bodies = CXX_NEW_ARRAY(WN *,u,&LNO_local_pool);
      for (INT kidno=0; kidno<WN_kid_count(bodies[0]); kidno++) {
        for (INT i=0; i<u; i++) {
	  new_bodies[i] = WN_kid(bodies[i],kidno);
        }
        if (!Unrolled_Dependences_Update_V(new_bodies, u,hash_table,
							orig_vertices)) {
	  return 0;
	}
      }
    }

    if (OPCODE_is_load(WN_opcode(bodies[0])) || 
	OPCODE_is_store(WN_opcode(bodies[0])) ||
	OPCODE_is_call(WN_opcode(bodies[0]))) {
      VINDEX16 origv = Get_Vertex(bodies[0]);
      if (origv) {
	orig_vertices->Push(origv);
	VINDEX16 *newv = CXX_NEW_ARRAY(VINDEX16,u,&LNO_local_pool);
	newv[0] = origv;
	for (INT i=1; i<u; i++) {
          newv[i] = Add_Vertex(bodies[i]);
	  if (!newv[i]) return(0);
	}
	VINDEX16P_LEX_COUNT *vlp = CXX_NEW(VINDEX16P_LEX_COUNT(newv,lex_count),
					   &LNO_local_pool);
        hash_table->Enter(origv,vlp);
      } 
      lex_count++;
    }
  }
  return 1;
}

// a hash table of pairs of vertices
class VERTEX_PAIR
{
public:
    VINDEX16 _v1;
    VINDEX16 _v2;
    VERTEX_PAIR(VINDEX16 v1, VINDEX16 v2) { _v1=v1; _v2=v2; };
};

struct VERTEX_PAIR_HASH
{
  INT operator() ( VERTEX_PAIR vp) const {
    return vp._v1 + 64*1024*vp._v2;
  }
};

struct VERTEX_PAIR_EQ
{
  BOOL operator() ( VERTEX_PAIR vp1, VERTEX_PAIR vp2) const
    { return vp1._v1 == vp2._v1 && vp1._v2 == vp2._v2; }
};

typedef USER_HASH_TABLE<VERTEX_PAIR,BOOL,VERTEX_PAIR_HASH,VERTEX_PAIR_EQ> PAIR_TABLE;

// get the edges right for updating after unrolling
//   Each edge in to or out of the region gets copied
//   For each edge within region repace distance component d = (d+n)/u
//   where n is the "copy number" 
// Return 0 on error

INT ARRAY_DIRECTED_GRAPH16::Unrolled_Dependences_Update_E(UINT u,
	UINT loopno, HASH_TABLE<VINDEX16,VINDEX16P_LEX_COUNT *> *hash_table,
	VINDEX16_STACK *orig_vertices)
{
  // first do all the external edges 
  INT v;
  for (v=0; v<orig_vertices->Elements(); v++) {
    VINDEX16 origv = orig_vertices->Bottom_nth(v);
    // get all the edges from this vertex
    VINDEX16 *newv = hash_table->Find(origv)->_vp;
    // newv are the corresponding vertices, now find all the edges

    EINDEX16 edge = Get_Out_Edge(origv);
    while (edge) {
      VINDEX16 orig_sinkv = Get_Sink(edge);
      VINDEX16P_LEX_COUNT *hash_element = hash_table->Find(orig_sinkv);
      if (!hash_element) {  // the sink goes out of the region
        for (INT i=1; i<u; i++) {
          if (!Add_Edge(newv[i],orig_sinkv,
            	Create_DEPV_ARRAY(Depv_Array(edge),_pool))) {
            return 0;
          }
        }
      }
      edge = Get_Next_Out_Edge(edge);
    }
    edge = Get_In_Edge(origv);
    while (edge) {
      VINDEX16 orig_sourcev = Get_Source(edge);
      VINDEX16P_LEX_COUNT *hash_element = hash_table->Find(orig_sourcev);
      if (!hash_element) {  // the source comes from out of the region
	for (INT i=1; i<u; i++) {
	  if (!Add_Edge(orig_sourcev,newv[i],
	    		Create_DEPV_ARRAY(Depv_Array(edge),_pool))) {
	    return 0;
	  }
	}
      } 
      edge = Get_Next_In_Edge(edge);
    }
  }

  // now the internal edges

  // pair_table is the set of pairs that we've processeed
  PAIR_TABLE *pair_table = CXX_NEW(PAIR_TABLE(200,&LNO_local_pool),
				&LNO_local_pool);
  for (v=0; v<orig_vertices->Elements(); v++) {
    VINDEX16 origv = orig_vertices->Bottom_nth(v);
    VINDEX16P_LEX_COUNT *vlc = hash_table->Find(origv);
    VINDEX16 *newv = vlc->_vp;
    INT lex_count = vlc->_lex_count;

    EINDEX16 edge = Get_Out_Edge(origv);
    while (edge) {
      EINDEX16 next_out = Get_Next_Out_Edge(edge);
      VINDEX16 orig_sinkv = Get_Sink(edge);
      VINDEX16P_LEX_COUNT *vlc_sink = hash_table->Find(orig_sinkv);
      if (vlc_sink) {  // the sink is in region
        VINDEX16 *new_sinkv = vlc_sink->_vp;
        INT sink_lex_count = vlc_sink->_lex_count;
	// we do both edges ((origv,orig_sinkv) and (orig_sinkv,origv))
	// at a time so want to make sure we only do them once
	if ((origv <= orig_sinkv) || 
	    (!Get_Edge(orig_sinkv,origv) &&
	     !pair_table->Find(VERTEX_PAIR(origv,orig_sinkv)))) {
	  if (origv < orig_sinkv) {
	    pair_table->Enter(VERTEX_PAIR(orig_sinkv,origv),1);
          }
	  if (!Unrolled_Dependences_Update_E(
		newv,new_sinkv,edge,Get_Edge(orig_sinkv,origv),u,loopno,
		lex_count,sink_lex_count)) {
	    return 0;
	  }
	}
      }
      edge = next_out;
    }
  }

  return(1);
}

// Fix one internal edge
// sources is the array of new source vertices (element 0 is the orig)
// sinks is the array of new sink vertices (element 0 is the orig)
// fedge is the edge from sources[0] to sinks[0]
// bedge is the edge from sinks[0] to sources[0]
// u is the unrolling factor
// loop number is the number of the loop being unrolled
INT ARRAY_DIRECTED_GRAPH16::Unrolled_Dependences_Update_E(VINDEX16 *sources,
			VINDEX16 *sinks, EINDEX16 fedge, EINDEX16 bedge,
			UINT u, UINT loopno, INT lex_count, INT sink_lex_count)
{
  DEPV_ARRAY *farray = Depv_Array(fedge);
  DEPV_ARRAY *barray = 0;
  if (bedge) barray=Depv_Array(bedge);

  DEPV_LIST *fl = CXX_NEW(DEPV_LIST(farray,&LNO_local_pool),&LNO_local_pool);
  DEPV_LIST *bl;
  if (bedge) {
    bl=CXX_NEW(DEPV_LIST(barray,&LNO_local_pool),&LNO_local_pool);
  } else {
    bl=CXX_NEW(DEPV_LIST(fl->Num_Dim(),fl->Num_Unused_Dim(),&LNO_local_pool),
	&LNO_local_pool);
  }
  DEPV_LIST *dl = Lex_Pos_Compose(&LNO_local_pool,fl,bl);

  if (farray == barray) {
    Is_True(fedge==bedge,
      ("same array different edge in Unrolled_Dependences_Update_E"));
    Delete_DEPV_ARRAY(farray,_pool);
    Remove_Edge(fedge);
  } else {
    Delete_DEPV_ARRAY(farray,_pool);
    Delete_DEPV_ARRAY(barray,_pool);
    Remove_Edge(fedge);
    if (bedge) Remove_Edge(bedge);
  }

  // compare source of every copy vrs sink of every copy
  // unless source and sink are the same reference in which case
  // only compare half (reverse edges will get the other half)
  for (INT c1=0; c1<u; c1++) {
    for (INT c2=0; c2<(sources[0]==sinks[0] ? (c1+1) : u); c2++) {  
      // create the edge btwn copies c1 and c2
      INT diff = c1-c2;
      DEPV_LIST *result = CXX_NEW(DEPV_LIST(dl->Num_Dim(),dl->Num_Unused_Dim(),
				&LNO_local_pool), &LNO_local_pool);
      UINT ln_pos = loopno-dl->Num_Unused_Dim();
      DEPV_ITER iter(dl);
      for (DEPV_NODE *node=iter.First(); !iter.Is_Empty();node=iter.Next()) {
	DEPV *Depv = node->Depv;
	DEP dep = DEPV_Dep(Depv,ln_pos);
	if (DEP_IsDistance(dep)) {
	  INT dist = DEP_Distance(dep);
	  if ((abs(dist+diff) % u) == 0) {
	    DEPV *r = DEPV_Copy(&LNO_local_pool,Depv,dl->Num_Dim());
	    DEPV_Dep(r,ln_pos) = DEP_SetDistance((dist+diff)/u);
	    result->Append(CXX_NEW(DEPV_NODE(r), &LNO_local_pool));
	  } 
	} else {
	  DEPV *r = DEPV_Copy(&LNO_local_pool,Depv,dl->Num_Dim());
	  if ((diff != 0) && DEP_Direction(dep) != DIR_STAR) {
	    if (DEP_Direction(dep) == DIR_POSNEG) {
	      DEPV_Dep(r,ln_pos) = DEP_SetDirection(DIR_STAR);
	    } else if (DEP_Direction(dep) == DIR_POS) {
	      if (diff < 0) DEPV_Dep(r,ln_pos) = DEP_SetDirection(DIR_POSEQ);
	    } else if (DEP_Direction(dep) == DIR_NEG) {
	      if (diff > 0) DEPV_Dep(r,ln_pos) = DEP_SetDirection(DIR_NEGEQ);
	    } else if (DEP_Direction(dep) == DIR_NEGEQ) {
	      if (diff > 0) DEPV_Dep(r,ln_pos) = DEP_SetDirection(DIR_STAR);
	      if (diff < 0) DEPV_Dep(r,ln_pos) = DEP_SetDirection(DIR_NEG);
	    } else if (DEP_Direction(dep) == DIR_POSEQ) {
	      if (diff < 0) DEPV_Dep(r,ln_pos) = DEP_SetDirection(DIR_STAR);
	      if (diff > 0) DEPV_Dep(r,ln_pos) = DEP_SetDirection(DIR_POS);
	    }
	  }
	  result->Append(CXX_NEW(DEPV_NODE(r), &LNO_local_pool));
	}
      }
      DEPV_LIST *pos = CXX_NEW(DEPV_LIST(result->Num_Dim(),
	    	result->Num_Unused_Dim(),&LNO_local_pool), &LNO_local_pool);
      DEPV_LIST *neg = CXX_NEW(DEPV_LIST(result->Num_Dim(),
		result->Num_Unused_Dim(),&LNO_local_pool),&LNO_local_pool);
      if (c1 < c2) {
        result->Lex_Pos_Decompose(&LNO_local_pool,pos,neg,TRUE,FALSE);
      } else if (c2 < c1) {
        result->Lex_Pos_Decompose(&LNO_local_pool,pos,neg,FALSE,TRUE);
      } else {
        result->Lex_Pos_Decompose(&LNO_local_pool,pos,neg,
		lex_count < sink_lex_count,sink_lex_count < lex_count);
      }
      if (!pos->Is_Empty()) {
        DEPV_ARRAY *array = Create_DEPV_ARRAY(pos,_pool);
        if (!Add_Edge(sources[c1],sinks[c2],array)) {
	  return(0);
	}
      }
      if ((sources[c1] != sinks[c2]) && !neg->Is_Empty()) {
        DEPV_ARRAY *array = Create_DEPV_ARRAY(neg,_pool);
        if (!Add_Edge(sinks[c2],sources[c1],array)) {
	  return(0);
	}
      }
    }
  }
  return 1;
}



void REFERENCE_LIST::Print(FILE *fp)
{
  REFERENCE_ITER iter(this);
  REFERENCE_NODE *first = iter.First();
  for (REFERENCE_NODE *node=first; !iter.Is_Empty(); node = iter.Next()) {
      node->Print(fp);
  }
  fprintf(fp,"\n");
}

// Fix dependences in array graph after loop fission
//
// Given a reference inside a fissioned loop
// All its dependences to references outside the region of fission don't change
// All its dependences to references inside the same post-fission loop
//	don't change
// All its dependences to references inside a different post-fission loop
// get choped so that the number of dimensions equal the new common_nesting
// (some edges might get erased if there are no longer any common good loops)
//
void ARRAY_DIRECTED_GRAPH16::Fission_Dep_Update(WN* in_loop, UINT32 total_loops)
{
  Is_True(_type==DEPV_ARRAY_ARRAY_GRAPH,
    ("Fission_Dep_Update called on a non-DEPV_ARRAY graph"));

  UINT depth = Do_Loop_Depth(in_loop);
  for (INT i=0; i<total_loops; i++) {
    Is_True(WN_opcode(in_loop) == OPC_DO_LOOP,
	  ("Non do loop in Fission_Dep_Update"));
    Fission_Dep_Update_R(WN_do_body(in_loop),in_loop,depth);
    in_loop = WN_next(in_loop);
  }
}

// Update one loop
void ARRAY_DIRECTED_GRAPH16::Fission_Dep_Update_R(WN *wn, WN *in_loop,
					UINT depth)
{
  OPCODE opcode = WN_opcode(wn);
  VINDEX16 v;

  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Fission_Dep_Update_R(kid,in_loop,depth);
      kid = WN_next(kid);
    }
    return;
  } 
  if ((v=Get_Vertex(wn)) != 0) {
    Fission_Dep_Update_V(v,in_loop,depth);
  }
  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    WN *kid = WN_kid(wn,kidno);
    Fission_Dep_Update_R(kid,in_loop,depth);
  }
}

// Find the ancestor of wn1 that is a child of wn2, return NULL if none
// return wn1 if wn1 is a child of wn2
static WN *Find_Ancestor_WN1_Child_WN2(WN *wn1, WN *wn2)
{
  while (wn1) {
    WN *parent = LWN_Get_Parent(wn1);
    if (parent == wn2) return(wn1);
    wn1 = parent;
  }
  return NULL;
}

// Update one reference in the loop
void ARRAY_DIRECTED_GRAPH16::Fission_Dep_Update_V(VINDEX16 v,WN *in_loop, 
							UINT depth)
{
//WN *Find_Ancestor_WN1_Child_WN2(WN *wn1, WN *wn2);
  EINDEX16 e = _v[v].Get_Out_Edge();
  while (e) {
    EINDEX16 next_e = _e[e].Get_Next_Out_Edge();
    WN *sink = _v[_e[e].Get_Sink()].Wn;
    WN *sink_loop = Find_Ancestor_WN1_Child_WN2(sink,LWN_Get_Parent(in_loop));
    if (sink_loop && (sink_loop != in_loop)) { // the sink is in a diff copy
      INT unused = _e[e].Depv_Array->Num_Unused_Dim();
      if (unused >= depth) {
	Delete_Array_Edge(e);
      } else {
	if (_e[e].Depv_Array->Num_Dim() > depth-unused) { // its too big
          DEPV_ARRAY *da = _e[e].Depv_Array->Shorten(depth-unused,_pool);
          Delete_DEPV_ARRAY(_e[e].Depv_Array,_pool);
          _e[e].Depv_Array = da;
        }
      }
    }
    e = next_e;
  }
}

// 
// Fix dependences in statement dependence graph after loop fission
// 
// 1. Copy all the dependences from "do loop1" to the other copies
//    ie given
//      s0:
//      do i1
//      do i2
//    copy all the dependences (s0 <-> do i1) to (s0 <->do i2)
//    This is a conservative approximation.  It is possible that s0 is only
//    dependent on the references in one of the do loops.  Nonetheless, it
//    is impossible for us to know this since the statement dependence graph
//    doesn't have the dependences between s0 and the things inside a do
//    loop (it only has dependences to things in the same level).  This 
//    conservatism will never make a difference given our uses of fission,
//    but it might if we change our algorithm at some point.
// 2. Given a reference inside a fissioned loop
//    All its dependeces to references inside the same post-fission loop remain
//    All its dependences to references inside a different post-fission loop
//      get moved to the dependence between the corresponding do loops
//	the level is chopped to the minimum(old_level, depth in_loop)
//	unless the corresponding do loops are the outer good do loops,
//	  in which case we just get rid of the edge
// Return 0 on overflow 
// fission_depth is the number of perfectly nested loops fissioned together
INT ARRAY_DIRECTED_GRAPH16::Fission_Dep_Update(WN* in_loop,
			UINT32 total_loops, UINT fission_depth)
{
  Is_True(_type==LEVEL_ARRAY_GRAPH,
    ("Fission_Dep_Update called on a non-level graph"));
  MEM_POOL_Push(&LNO_local_pool);

  BOOL outer_good_do = TRUE;
  WN *tmp = LWN_Get_Parent(in_loop);
  while (tmp && (WN_opcode(tmp) != OPC_DO_LOOP)) tmp = LWN_Get_Parent(tmp);
  if (tmp && Do_Loop_Is_Good(tmp)) outer_good_do = FALSE;

  VINDEX16 *do_loop_vertices = 
    CXX_NEW_ARRAY(VINDEX16,total_loops,&LNO_local_pool);

  tmp = in_loop;
  INT i;
  for (i=0; i<total_loops; i++) {

    do_loop_vertices[i] = Get_Vertex(tmp);
    tmp = WN_next(tmp);
  }

  for (INT j=0; j<fission_depth; j++) {

    if (!Copy_Do_Loop_Deps(do_loop_vertices,total_loops)) {
      MEM_POOL_Pop(&LNO_local_pool);
      return 0;
    }

    if (j<fission_depth-1)
    for (INT i=0; i<total_loops; i++) {
      WN* wn=Get_Only_Loop_Inside(Get_Wn(do_loop_vertices[i]),FALSE);

      do_loop_vertices[i] = Get_Vertex(wn);
    }

  }

  UINT depth = Do_Loop_Depth(in_loop);
  for (i=0; i<total_loops; i++) {
    Is_True(WN_opcode(in_loop) == OPC_DO_LOOP,
	  ("Non do loop in Fission_Dep_Update"));

    if (!Fission_Dep_Update_R(in_loop,fission_depth,depth,outer_good_do)) {
      return(0);
    }
    in_loop = WN_next(in_loop);
  }
  MEM_POOL_Pop(&LNO_local_pool);
  return(1);
}

// Copy all the dependences to/from the first loop to the other loops
INT ARRAY_DIRECTED_GRAPH16::Copy_Do_Loop_Deps(VINDEX16 *do_loop_vertices, 
						INT num_loops) 
{
  EINDEX16 e = _v[do_loop_vertices[0]].Get_Out_Edge();
  while (e) {
    for (INT i=1; i<num_loops; i++) {
      Add_Edge(do_loop_vertices[i],_e[e].Get_Sink(),Level(e));
    }
    e = _e[e].Get_Next_Out_Edge();
  }

  e = _v[do_loop_vertices[0]].Get_In_Edge();
  while (e) {
    for (INT i=1; i<num_loops; i++) {
      if (!Add_Edge(_e[e].Get_Source(),do_loop_vertices[i],Level(e))) {
	return(0);
      }
    }
    e = _e[e].Get_Next_In_Edge();
  }
  return(1);
}


// Fix all the dependences in the copy rooted at in_loop
// All intra copy dependences remain
// All inter copy get moved to the level of the do loop and get chopped
// Fission_depth is the number of perfectly nested loops fissioned together
// Depth is the depth of each outer do loop
// Outer good_do is TRUE if in_loop is the outermost good do loop
INT ARRAY_DIRECTED_GRAPH16::Fission_Dep_Update_R(WN *in_loop, 
		UINT fission_depth, UINT depth, BOOL outer_good_do)
{
  VINDEX16 in_loop_v = Get_Vertex(in_loop);
  Is_True(in_loop_v,("No vertex for one of the fission copies"));

  // find the first statement in this copy
  WN *statement, *parent;
  WN* loop = in_loop;
  for (INT i=0; i<fission_depth; i++) {
    parent = WN_do_body(loop);
    statement = WN_first(parent);

    EINDEX16 e=0;
    while (statement) {
      if (WN_opcode(statement) == OPC_DO_LOOP)
	loop=statement;
      VINDEX16 v = Get_Vertex(statement);
      Is_True(v,("No vertex for one of the fission copies"));
      if (v) {
        e = _v[v].Get_Out_Edge();
      } else
      while (e) {
        EINDEX16 next_e = _e[e].Get_Next_Out_Edge();
        UINT level = MIN(Level(e),depth);
        VINDEX16 v2 = _e[e].Get_Sink();
        WN *wn2 = Get_Wn(v2);
        if (LWN_Get_Parent(wn2) != parent) {  // not the same copy
          if (!outer_good_do) {
	    // find the outer do loop corresponding to the sink
	    WN *parent2 = wn2;
	    for (INT j=0; j<=i; j++) {
	      parent2 = LWN_Get_Parent(LWN_Get_Parent(parent2));
	    }
	    VINDEX16 sinkv = Get_Vertex(parent2);
	    Is_True(sinkv,("No vertex for one of the fission copies"));
	    EINDEX16 new_e = Get_Edge(in_loop_v,sinkv);
	    if (new_e) {  // an edge already exists, update level if need be
	      _e[new_e].Level_Info.Level = MAX(Level(new_e),level);
	    } else {
	      if (!Add_Edge(in_loop_v,sinkv,level)) {
	        return 0;
	      }
            }
	  }
	  Remove_Edge(e);
        }
        e = next_e;
      }
      statement = WN_next(statement);
    }
  }
  return 1;
}



// Find the inner loop surrounding this wn
// Assumes there is one
static WN *Find_Inner(WN *wn) 
{
  if (WN_opcode(wn) == OPC_DO_LOOP) {
    return(wn);
  } else {
    return(Find_Inner(LWN_Get_Parent(wn)));
  }
}

// is this array invariant in the inner loop
static BOOL Inner_Loop_Invariant(ACCESS_ARRAY *a)
{
  if (a->Too_Messy) return FALSE;
  for (INT i=0; i<a->Num_Vec(); i++) {
    ACCESS_VECTOR *av = a->Dim(i);
    if (av->Too_Messy) {
      return FALSE;
    }
    if (av->Non_Const_Loops() >= av->Nest_Depth()) {
      return FALSE;
    }
    if (av->Loop_Coeff(av->Nest_Depth()-1)) {
      return FALSE;
    }
  }
  return TRUE;
}

// Given CG's dependence graph, add in the "must" edges
// This is a two part algorithm.  First, we go through all edges
// in the graph.  If their distance is constant
// we set the must bit.  If the references are invariant and identical,
// we also set the must bit.  Otherwise if the base array is the same,
// we check for must dependences.
//
// Then we check every read in the graph vrs every read to the same
// base array in the same inner loop.  We add edges for all the must
// dependences
void ARRAY_DIRECTED_GRAPH16::Add_Must()
{
  MEM_POOL_Push(&LNO_local_pool);
  Is_True(_type==DEP_ARRAY_GRAPH,
    ("Add_Must called on non-CG dependence graph"));

  // first the non ld-ld 
  VINDEX16 v;
  for (v = Get_Vertex(); v; v = Get_Next_Vertex(v)) {
    WN *source = _v[v].Wn;
    WN *array1=0,*array2=0;
    ACCESS_ARRAY *a1=0,*a2=0;
    if (OPCODE_is_load(WN_opcode(source)) || 
	OPCODE_is_store(WN_opcode(source))) {
      if (OPCODE_is_load(WN_opcode(source))) {
        if (WN_kid_count(source) >= 1) {
          array1 = WN_kid0(source);
        }
      } else {
        if (WN_kid_count(source) >= 2) {
          array1 = WN_kid1(source);
        }
      }
      if (WN_operator(array1) == OPR_ADD) {
        if (WN_operator(WN_kid0(array1)) == OPR_ARRAY) {
	  array1 = WN_kid0(array1);
        } else {
	  array1 = WN_kid1(array1);
        }
      }
      if (array1) a1=(ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array1);
      if (a1) {
        BOOL a1_invar = Inner_Loop_Invariant(a1);
        EINDEX16 e = Get_Out_Edge(v);
        while (e) {
          EINDEX16 new_e = Get_Next_Out_Edge(e);
          VINDEX16 v2 = Get_Sink(e);
          WN *sink = _v[v2].Wn;
          if (OPCODE_is_load(WN_opcode(sink)) || 
	      OPCODE_is_store(WN_opcode(sink))) {
	    if (DEPV_COMPUTE::Base_Test(source,NULL,sink,NULL)==DEP_CONTINUE) {
              if (OPCODE_is_load(WN_opcode(sink))) {
                if (WN_kid_count(sink) >= 1) {
	          array2 = WN_kid0(sink);
                }
              } else {
                if (WN_kid_count(sink) >= 2) {
	          array2 = WN_kid1(sink);
                }
              }
              if (WN_operator(array2) == OPR_ADD) {
                if (WN_operator(WN_kid0(array2)) == OPR_ARRAY) {
	          array2 = WN_kid0(array2);
                } else {
	          array2 = WN_kid1(array2);
                }
              }
              if (array2) a2=(ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array2);
	      if (a2) {
                DEP dep = Dep(e);
                if (DEP_IsDistance(dep)) {
	          DEP dep2;
	          if (Is_Must(a1,a2,Find_Inner(source),&dep2)) {
	            if (DEP_IsDistance(dep2)) {
	              if (DEP_Distance(dep) == DEP_Distance(dep2)) {
	                Set_Must(e);
                      } else {
		        Delete_Edge(e);  // incompatible so must be spurious
		      }
                    }
	          }
                } else if (DEPV_COMPUTE::Base_Test(source,NULL,sink,NULL) 
						== DEP_CONTINUE) {
	          DEP dep2;
	          if (a1_invar && (*a1 == *a2)) {
	            Set_Must(e);
	          } else if (!a1_invar && 
			 Is_Must(a1,a2,Find_Inner(source),&dep2)) {
	            if (DEP_Distance(dep2) > 0) {
		      if ((DEP_Direction(dep) == DIR_POS) ||
		          (DEP_Direction(dep) == DIR_POSEQ) ||
		          (DEP_Direction(dep) == DIR_STAR) ||
		          (DEP_Direction(dep) == DIR_POSNEG)) {
		        Set_Dep(e,dep2,TRUE);
                      } else {
		        Delete_Edge(e);  // incompatible so must be spurious
                      }
	            } else if (DEP_Distance(dep2) == 0) {
		      if ((DEP_Direction(dep) == DIR_EQ) ||
		        (DEP_Direction(dep) == DIR_POSEQ) ||
		        (DEP_Direction(dep) == DIR_STAR) ||
		        (DEP_Direction(dep) == DIR_NEGEQ)) {
		        Set_Dep(e,dep2,TRUE);
                      } else {
		        Delete_Edge(e);
                      }
	            }
                  }
	        }
              }
	    }
	  }
          e = new_e;
        }
      }
    }
  }
  
    // now the ld-ld

  // first gather from the graph all the reads
  // we partitition the loads from the graph by base array and inner loop
  // to avoid doing n^2 checks
  // we don't put on our list things with strange base arrays, we can
  // never get must dependences from these
  REF_LIST_STACK *reads = CXX_NEW(REF_LIST_STACK(&LNO_local_pool),
							&LNO_local_pool);
  for (v = Get_Vertex(); v; v = Get_Next_Vertex(v)) {
    WN *source = _v[v].Wn;
    OPCODE opcode = WN_opcode(source);
    if (OPCODE_is_load(opcode)) {
      if (WN_kid_count(source) == 1) {
        WN *inner_loop = Find_Inner(source);
	WN *array = WN_kid0(source);
        if (WN_operator(array) == OPR_ADD) {
          if (WN_operator(WN_kid0(array)) == OPR_ARRAY) {
	    array = WN_kid0(array);
          } else {
	    array = WN_kid1(array);
          }
        }
	if (WN_operator(array) == OPR_ARRAY) {
          WN *base = WN_array_base(array);
          ST *st_base = Get_ST_Base(base);
          INT i;
          for (i=0;i<reads->Elements() && 
		!((reads->Bottom_nth(i)->ST_Base==st_base) &&
		  (reads->Bottom_nth(i)->Inner_Loop==inner_loop)); i++);
          if (i==reads->Elements()) {  // a new partition
	    REFERENCE_LIST *rl = 
	     CXX_NEW(REFERENCE_LIST(st_base,source,inner_loop),&LNO_local_pool);
            reads->Push(rl);
          }
          reads->Bottom_nth(i)->Append(CXX_NEW(REFERENCE_NODE(source,0, 0), 
						&LNO_local_pool));
         }
      }
    }
  }

  // now do an n^2 comparison on each partition.  Note that we assume that
  // for two pairs to have a must dependence, they must be in the same 
  // partition.  It's always conservative to not set a must dependence.
  for (INT i=0; i<reads->Elements(); i++) {
    INT num_iterations = 0;
    WN *trip_count = WN_LOOP_TripCount(reads->Bottom_nth(i)->Inner_Loop);
    if (trip_count) {
      LWN_Parentize(trip_count);
      LWN_Set_Parent(trip_count, NULL);
    }
    BOOL const_trip = (trip_count) &&
         (WN_operator(trip_count) == OPR_INTCONST);
    if (const_trip) num_iterations = WN_const_val(trip_count);
    LWN_Delete_Tree(trip_count);

    REFERENCE_ITER iter1(reads->Bottom_nth(i));
    for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty();
					n1=iter1.Next()) {
      WN *wn1 = n1->Wn;
      WN *array1 = WN_kid0(wn1);
      if (WN_operator(array1) == OPR_ADD) {
        if (WN_operator(WN_kid0(array1)) == OPR_ARRAY) {
	  array1 = WN_kid0(array1);
        } else {
	  array1 = WN_kid1(array1);
        }
      }
      ACCESS_ARRAY *a1=(ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array1);
      BOOL a1_invar = Inner_Loop_Invariant(a1);
      REFERENCE_ITER iter2(n1);
      REFERENCE_NODE *n2 = iter2.First();
      for (n2=iter2.Next(); !iter2.Is_Empty(); n2=iter2.Next()) {
        WN *wn2 = n2->Wn;
        WN *array2 = WN_kid0(wn2);
        if (WN_operator(array2) == OPR_ADD) {
          if (WN_operator(WN_kid0(array2)) == OPR_ARRAY) {
	    array2 = WN_kid0(array2);
          } else {
	    array2 = WN_kid1(array2);
          }
        }
	// are they the same base
	if (DEPV_COMPUTE::Base_Test(wn1,NULL,wn2,NULL) == DEP_CONTINUE) {
          ACCESS_ARRAY *a2=(ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array2);
	  DEP dep;
	  if (a1_invar && (*a1 == *a2)) {
	    VINDEX16 v1 = Get_Vertex(wn1);
	    VINDEX16 v2 = Get_Vertex(wn2);
	    if (!Add_Edge(v1,v2,DEP_SetDirection(DIR_POSEQ),TRUE)) {
              MEM_POOL_Pop(&LNO_local_pool);
	      return;
            }
	    if (!Add_Edge(v2,v1,DEP_SetDirection(DIR_POS),TRUE)) {
              MEM_POOL_Pop(&LNO_local_pool);
	      return;
            }
	  } else if (Is_Must(a1,a2,reads->Bottom_nth(i)->Inner_Loop,&dep)) {
	    VINDEX16 v1 = Get_Vertex(wn1);
	    VINDEX16 v2 = Get_Vertex(wn2);
	    if (!num_iterations || (abs(DEP_Distance(dep)) < num_iterations)) {
	      if (DEP_Distance(dep) >= 0) {
	        if (!Add_Edge(v1,v2,dep,TRUE)) {
                  MEM_POOL_Pop(&LNO_local_pool);
	          return;
                }
              } else {
	        if (!Add_Edge(v2,v1,DEP_SetDistance(-DEP_Distance(dep)),TRUE)) {
                  MEM_POOL_Pop(&LNO_local_pool);
	          return;
                }
              }
            }
          }
        }
      }
    }
  }
  MEM_POOL_Pop(&LNO_local_pool);
}

// Is there a must dependence from a1 to a2
// This routine assume that either a1 or a2 is variant in the inner loop
// This routine assumes a1 and a2 are in the same inner loop
// For there to be a must dependence, the two references must vary only
// in the constant term
// If there is a must dependence and dep != NULL, set dep to its value
BOOL ARRAY_DIRECTED_GRAPH16::Is_Must(ACCESS_ARRAY *a1, ACCESS_ARRAY *a2,
				     WN *inner_loop,DEP *dep)
{
  if (a1->Too_Messy || a2->Too_Messy) return(FALSE);
  if (a1->Num_Vec() != a2->Num_Vec()) return(FALSE);

  DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,inner_loop);
  INT depth = dli->Depth;

  ACCESS_VECTOR *ac_step = dli->Step;
  if (!ac_step->Is_Const()) {
    return(FALSE);
  } 
  INT64 step = ac_step->Const_Offset;

  BOOL seen_mult = FALSE;
  INT diff=0;

  // for each dimension
  for (INT i=0; i<a1->Num_Vec(); i++) {
    ACCESS_VECTOR *av1 = a1->Dim(i);
    ACCESS_VECTOR *av2 = a2->Dim(i);
    if (av1->Too_Messy || av2->Too_Messy) return(FALSE);

    // does either have a constant term that varies in the inner loop
    if (av1->Non_Const_Loops() == (depth + 1)) return(FALSE);
    if (av2->Non_Const_Loops() == (depth + 1)) return(FALSE);

    INT dist = av1->Const_Offset - av2->Const_Offset;

    // short circuit to get const reference case quickly, ie a[0] vrs a[1]
    if (!av1->Has_Loop_Coeff() || !av2->Has_Loop_Coeff()) {
      if (dist) return(FALSE);
    }

    // the symbols must be equal
    if (av1->Lin_Symb!=NULL && !av1->Lin_Symb->Is_Empty()) { // av1 has a symb
      if (av2->Lin_Symb == NULL || av2->Lin_Symb->Is_Empty() ||
          !(*av1->Lin_Symb == *av2->Lin_Symb)) {
         return(FALSE);
       }
    } else if (av2->Lin_Symb != NULL && !av2->Lin_Symb->Is_Empty()) {
      return(FALSE);
    }
    if (av1->Non_Lin_Symb != NULL && !av1->Non_Lin_Symb->Is_Empty()) {
      if (av2->Non_Lin_Symb == NULL || av2->Non_Lin_Symb->Is_Empty() ||
                             !(*av1->Non_Lin_Symb == *av2->Non_Lin_Symb)) {
        return(FALSE);
      }
    } else if (av2->Non_Lin_Symb != NULL && !av2->Non_Lin_Symb->Is_Empty()) {
      return(FALSE);
    }

    // Now check the induction variables
    for (INT ii=0; ii<av1->Nest_Depth(); ii++) {
      if (av1->Loop_Coeff(ii) != av2->Loop_Coeff(ii)) {
        return(FALSE);
      }
    }
    INT mult = av1->Loop_Coeff(depth);
    if (mult) {
      if ((dist % (step*mult)) != 0) {
        return(FALSE); // independent
      }
      INT this_diff = (dist / (step*mult));
      if (seen_mult && (this_diff != diff)) {  // contradictory coupling
        return(FALSE);
      }
      seen_mult = TRUE;
      diff = this_diff;
    } else {
      if (dist != 0) {
	return(FALSE);  // independent
      }
    }
  }
  if (dep) {
    *dep = DEP_SetDistance(diff);
    if (!DEP_IsDistance(*dep)) return FALSE;  // overflow
  }
  return(TRUE);
}

/***********************************************************************
 *
 * Return true if wn is contained within the tree rooted at root,
 * false otherwise.
 *
 ***********************************************************************/
static BOOL Node_In_Tree (WN* wn, WN* root) {
  while (1) {
    if (wn == root) return TRUE;
    if (wn == NULL) return FALSE;
    wn = LWN_Get_Parent (wn);
  }
}

/***********************************************************************
 *
 * Called to update the dependence graph when a loop is versioned
 * for prefetching. 
 *  - body_orig == pointer to loop-body of the original loop (then part)
 *  - body_new == pointer to loop-body of the new version of loop (else part)
 *  - loopno == depth of the loop
 *
 ***********************************************************************/
BOOL ARRAY_DIRECTED_GRAPH16::Versioned_Dependences_Update(WN* body_orig,
                                                          WN* body_new,
                                                          UINT loopno, 
							  WN_MAP version_map) 
{
  Is_True(_type==DEPV_ARRAY_ARRAY_GRAPH,
      ("Unrolled_Dependences_Update called on a non-DEPV_ARRAY graph"));
  MEM_POOL_Push(&LNO_local_pool);

  // First create the vertices for the new version of the loop body.
  Versioned_Create_Vertices (body_orig, body_new);

  BOOL ok = Versioned_Dependences_Update_E (body_orig, body_new,
                                  body_orig, body_new,
                                  loopno, version_map);

  MEM_POOL_Pop(&LNO_local_pool);
  return ok; 
}

/***********************************************************************
 *
 * Given a loop body, create vertices in the dependence graph for those
 * loads/stores in the new loop body for which the corresponding load/store
 * in the orig loop body had a vertex.
 *
 ***********************************************************************/
void ARRAY_DIRECTED_GRAPH16::Versioned_Create_Vertices (WN* body_orig, WN* body_new) {

  if (body_orig == NULL) {
    Is_True (body_new == NULL, ("mismatch in body_orig and body_new\n"));
    return;
  }
  if (Get_Vertex (body_orig)) Add_Vertex (body_new);

  if (WN_opcode(body_new) == OPC_BLOCK) {
    WN* kid_new = WN_first(body_new);
    WN* kid_orig = WN_first(body_orig);
    while (kid_new) {
      Versioned_Create_Vertices (kid_orig, kid_new);
      kid_orig = WN_next (kid_orig);
      kid_new = WN_next (kid_new);
    }
  }
  else if (WN_kid_count(body_new)) {
    for (INT kidno=0; kidno<WN_kid_count(body_new); kidno++) {
      Versioned_Create_Vertices (WN_kid(body_orig, kidno), WN_kid(body_new, kidno));
    }
  }
}

/***********************************************************************
 *
 * Update the edges after versioning (or rohit-splitting)
 * Each edge in to or out of the region gets copied.
 * For each edge within the region,
 *  - copy it in the new body
 *  - if any of the dependences from outermost loop to this loop are not EQ,
 *    then add cross edges between the two versions of the code.
 */
BOOL ARRAY_DIRECTED_GRAPH16::Versioned_Dependences_Update_E(WN* body_orig,
                                                            WN* body_new,
                                                            WN* root_orig,
                                                            WN* root_new,
                                                            UINT loopno, 
							    WN_MAP version_map) {

  if (body_orig == NULL) {
    Is_True (body_new == NULL, ("mismatch in body_orig and body_new\n"));
    return TRUE;
  }

  if (OPCODE_is_load(WN_opcode(body_orig)) || 
      OPCODE_is_store(WN_opcode(body_orig)) ||
      OPCODE_is_call(WN_opcode(body_orig))) {
    VINDEX16 origv = Get_Vertex(body_orig);
    if (origv) {
      VINDEX16 newv = Get_Vertex (body_new);

      // since processing the internal edges may add external edges 
      // from body_orig to body_new, process the external edges first.
      // First the outedges
      EINDEX16 edge = Get_Out_Edge(origv);
      while (edge) {
        VINDEX16 orig_sinkv = Get_Sink(edge);
        WN* orig_sinkwn = Get_Wn(orig_sinkv);
        if (!Node_In_Tree (orig_sinkwn, root_orig) &&
            !Node_In_Tree (orig_sinkwn, root_new)) {
          // not in root_orig, hence an outside edge.
          // The test for sink in root_new is added since this could be
          // a cross-edge that had been added while traversing an earlier
          // node.
          // so add it to the body_new
          if (Add_Edge(newv,orig_sinkv,
                       Create_DEPV_ARRAY(Depv_Array(edge),_pool)) == 0) {
            LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(root_orig), this);
	    LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(body_orig), this);
	    return FALSE; 
          }
        }
        edge = Get_Next_Out_Edge(edge);
      }
      // now the in-edges to this vertex
      edge = Get_In_Edge (origv);
      while (edge) {
        VINDEX16 orig_sourcev = Get_Source(edge);
        WN* orig_sourcewn = Get_Wn(orig_sourcev);
        VINDEX16 new_sourcev;
        if (!Node_In_Tree (orig_sourcewn, root_orig) &&
            !Node_In_Tree (orig_sourcewn, root_new)) {
          new_sourcev = orig_sourcev;
          if (Add_Edge(new_sourcev,newv,
                       Create_DEPV_ARRAY(Depv_Array(edge),_pool)) == 0) {
            LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(root_orig), this);
            LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(body_orig), this);
	    return FALSE;
          }
        }
        edge = Get_Next_In_Edge(edge);
      }

      // now all external edges have been processed. 
      // so now do internal edges.
      // get all the edges from this vertex
      // process internal edges only as outedges,
      // to avoid processing them twice
      edge = Get_Out_Edge(origv);
      while (edge) {
        VINDEX16 orig_sinkv = Get_Sink(edge);
        WN* orig_sinkwn = Get_Wn(orig_sinkv);
        VINDEX16 new_sinkv;
        if (Node_In_Tree (orig_sinkwn, root_orig)) {
          // sink is within the replicated body, so internal edge.
          // add the internal edge to body_new
          new_sinkv = Get_Vertex((WN*) WN_MAP_Get (version_map, orig_sinkwn));
          if (Add_Edge(newv,new_sinkv,
                       Create_DEPV_ARRAY(Depv_Array(edge),_pool)) == 0) {
            LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(root_orig), this);
            LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(body_orig), this);
	    return FALSE; 
          }

          // see if we should add cross-edges
          if (Depv_Array(edge)->Equal_Through_Depth (loopno) == FALSE) {
            // maybe add cross edges
            INT unused = _e[edge].Depv_Array->Num_Unused_Dim();
            if (loopno > unused) {
              // add cross edges only if there really is a dependence
              // Shorten returns a new copy of depv_array
              // loopno - unused is the number of loops with dependences.
              DEPV_ARRAY* tmp = Depv_Array (edge)->Shorten (loopno-unused, _pool);
              if (Add_Edge (origv, new_sinkv, tmp) == 0) {
                LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(root_orig), this);
                LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(body_orig), this);
		return FALSE; 
              }
              tmp = Depv_Array (edge)->Shorten (loopno-unused, _pool);
              if (Add_Edge (newv, orig_sinkv, tmp) == 0) {
                LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(root_orig), this);
                LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(body_orig), this);
	        return FALSE; 
              }
            }
          }
        }
        edge = Get_Next_Out_Edge(edge);
      }
    }
  }

  if (WN_opcode(body_new) == OPC_BLOCK) {
    WN* kid_new = WN_first(body_new);
    WN* kid_orig = WN_first(body_orig);
    while (kid_new) {
      if (!Versioned_Dependences_Update_E (kid_orig, kid_new,
                                      root_orig, root_new, loopno,
				      version_map))
	return FALSE;
      kid_orig = WN_next (kid_orig);
      kid_new = WN_next (kid_new);
    }
  }
  else if (WN_kid_count(body_new)) {
    for (INT kidno=0; kidno<WN_kid_count(body_new); kidno++) {
      if (!Versioned_Dependences_Update_E (WN_kid(body_orig, kidno),
                                      WN_kid(body_new, kidno),
                                      root_orig, root_new, 
                                      loopno, version_map))
	return FALSE;
    }
  }
  return TRUE; 
}

#ifdef Is_True_On
void ARRAY_DIRECTED_GRAPH16::Check_Graph()
{
  MEM_POOL_Push(&LNO_local_pool);
  {
    HASH_TABLE<VINDEX16,INT> vertices(200, &LNO_local_pool);
    for (VINDEX16 v = Get_Vertex(); v; v = Get_Next_Vertex(v)) {
      WN *wn = Get_Wn(v);
      FmtAssert(wn, ("Missing wn for vertex %d", v));
      FmtAssert(Get_Do(wn), ("Missing enclosing loop for vertex %d", v));
      vertices.Enter(v, 1);
    }
    for (EINDEX16 e = Get_Edge(); e; e = Get_Next_Edge(e)) {
      FmtAssert(_e[e].Depv_Array,("Null Array for edge %d \n",e));
      VINDEX16 v1 = Get_Source(e);
      FmtAssert(vertices.Find(v1),
                ("Edge %d has source vertex %d not in graph", e, v1));
      VINDEX16 v2 = Get_Sink(e);
      FmtAssert(vertices.Find(v2),
                ("Edge %d has sink vertex %d not in graph", e, v2));
    }
  }
  MEM_POOL_Pop(&LNO_local_pool);
}


#endif /* Is_True_On */

// add a reference to the scalar stack
void SCALAR_STACK::Add_Scalar(WN *wn, UINT snumber)
{
  Is_True((WN_operator(wn) == OPR_LDID) ||
          (WN_operator(wn) == OPR_STID), 
	   ("Non scalar passed to SCALAR_STACK::Add_Scalar"));
  SYMBOL symbol(wn);
  SCALAR_REF sref(wn,snumber);

  for (INT i=0; i<_stack->Elements(); i++) {
    if (symbol == _stack->Top_nth(i)._scalar) {
      _stack->Top_nth(i)._scalar_ref_stack->Push(sref);
      return;
    }
  }
  _stack->Push(SCALAR_NODE(_pool,symbol));
  _stack->Top_nth(0)._scalar_ref_stack->Push(sref);
}

// add a reference to the scalar stack
void SCALAR_STACK::Add_Scalar(WN *wn_call, SYMBOL* symbol, UINT snumber)
{
  Is_True(WN_operator(wn_call) == OPR_CALL
      || WN_operator(wn_call) == OPR_LDID
      || WN_operator(wn_call) == OPR_LDA,
    ("Non scalar passed to SCALAR_STACK::Add_Scalar"));
  SCALAR_REF sref(wn_call, snumber);

  for (INT i=0; i<_stack->Elements(); i++) {
    if (*symbol == _stack->Top_nth(i)._scalar) {
      _stack->Top_nth(i)._scalar_ref_stack->Push(sref);
      return;
    }
  }
  _stack->Push(SCALAR_NODE(_pool,*symbol));
  _stack->Top_nth(0)._scalar_ref_stack->Push(sref);
}

void SCALAR_STACK::Remove_Scalar(WN *wn)
{
  Is_True((WN_operator(wn) == OPR_LDID) ||
          (WN_operator(wn) == OPR_STID), 
	  ("Non scalar passed to SCALAR_STACK::Remove_Scalar"));

  SYMBOL symbol(wn);

  for (INT i=0; i<_stack->Elements(); i++) {
    if (symbol == _stack->Top_nth(i)._scalar) {
      //remove all scalar_references to this symbol
      for (INT j = 0; j < _stack->Top_nth(i)._scalar_ref_stack->Elements(); j++)
	_stack->Top_nth(i)._scalar_ref_stack->DeleteTop(j);

      //remove the SCALAR_NODE since its _scalar_ref_stack is now empty.
      if (_stack->Top_nth(i)._scalar_ref_stack->Elements() == 0)
	_stack->DeleteTop(i);
      return;
    }
  }
}

void SCALAR_STACK::Print(FILE *fp)
{
  for (INT i=0; i<_stack->Elements(); i++) {
    SCALAR_NODE *node = &_stack->Bottom_nth(i);
    fprintf(fp,"The symbol is "); node->_scalar.Print(fp); fprintf(fp,"\n");
    for (INT j=0; j<node->Elements(); j++) {
      fprintf(fp,"One with statement number %d \n",
				node->Bottom_nth(j)->Statement_Number);
    }
  }
}

void SCALAR_STACK::Clear_Formal(INT i)
{
  STACK<SCALAR_NODE> temp_stack(_pool);
  INT j;
  for (j = 0; j < _stack->Elements(); j++) { 
    SCALAR_NODE* sn = &_stack->Bottom_nth(j);
    if (!(sn->_scalar.Is_Formal() && sn->_scalar.Formal_Number() == i))
      temp_stack.Push(*sn);
  } 
  _stack->Clear();
  for (j = 0; j < temp_stack.Elements(); j++)
    _stack->Push(temp_stack.Bottom_nth(j));
} 

#endif  /* ifdef LNO */



