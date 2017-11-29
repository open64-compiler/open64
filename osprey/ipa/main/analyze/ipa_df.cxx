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


#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "assert.h"
#include "defs.h"
#include "mempool.h"
#include "errors.h"
#include "ip_graph.h"
#include "ipa_cg.h"
#include "ipa_df.h"

/*---------------------------------------------------------------------*/
/* reverse the graph for backward data flow problems                   */
/*---------------------------------------------------------------------*/
static void reverse_graph(GRAPH *g)
{
 NODE_INDEX v;
 EDGE_INDEX e, e2;


/* switch from vertex to to vertex */
/* switch to vertex to from vertex */

 for (v=0; v<GRAPH_vmax(g); v++)
   if (NODE_fcnt(&GRAPH_v_i(g,v)) != -1) 
     {
     e = NODE_from(&GRAPH_v_i(g,v));
     NODE_from(&GRAPH_v_i(g,v)) = NODE_to(&GRAPH_v_i(g,v));
     NODE_to(&GRAPH_v_i(g,v)) = e;
     }


 for(e=0;e<GRAPH_emax(g);e++) 
   {
   if (EDGE_from(&GRAPH_e_i(g,e)) != -1)
     {
     v = EDGE_from(&GRAPH_e_i(g,e));
     EDGE_from(&GRAPH_e_i(g,e)) = EDGE_to(&GRAPH_e_i(g,e));
     EDGE_to(&GRAPH_e_i(g,e)) = v;
     e2 = EDGE_nfrom(&GRAPH_e_i(g,e));
     EDGE_nfrom(&GRAPH_e_i(g,e)) = EDGE_nto(&GRAPH_e_i(g,e));
     EDGE_nto(&GRAPH_e_i(g,e)) = e2;
    }
  }
}

/*---------------------------------------------------------------------*/
/* add entry and exit nodes, connect the entire graph                  */
/*---------------------------------------------------------------------*/
static void
pre_process_graph ( GRAPH *g, DFS *df, MEM_POOL * )
{
    NODE_INDEX exit, v;

    exit = g->Add_Node(0);
 
    /* iterate over all the vertices */

    for (v=0; v < GRAPH_vmax(g); v++) {
	if (NODE_fcnt(&GRAPH_v_i(g,v)) != -1) {
    
	    /* if the vertex has no successors, create an edge from vertex
	       to exit */ 
	    if (g->Num_Succs(v) == 0) {
		if (v != exit)
		    g->Add_Edge(v, exit, 0);
	    }
	}
    }
    /* set the root of the graph to be the entry vertex  */
    //   GRAPH_root(g) = entry;

    DFS_exit(df) = exit;
}

/*---------------------------------------------------------------------*/
/* remove the entry and exit nodes, restore graph to original form     */
/*---------------------------------------------------------------------*/
static void post_process_graph(GRAPH *g, DFS *df)
{
/* delete those unneccesary vertices, all edges originating to and from */
/* the vertices will be eliminated by default                           */

 g->Delete_Node(DFS_exit(df));

}
//-----------------------------------------------------------------
// initialize the node annotation         
//------------------------------------------------------------------
void
IPA_DATA_FLOW::Init()
{
  INT i;
  DFN *d;

  /* note at this point the exit vertex has not yet been set */
  /* get the depth first ordering of the vertices */
  d = DFS_d(df);

  for ( i=DFN_first(d); i< DFN_end(d); ++i ) {
    INT vindex = DFN_v_list_i(d,i);

    if (vindex == DFS_entry(df))
	continue;
    InitializeNode ( NODE_user(&GRAPH_v_i(IPA_Call_Graph->Graph(),vindex)) );
  }
}

/*----------------------------------------------------------------------*/
/* perform meet operation on incoming edges, transfer the result to out.*/
/* the parameters passed to meet are the sets being meeted into (in),   */
/* the predecessor's current data flow set (pred_set), the annotation    */
/* obtained from the local phase for the current vertex, and the        */
/* edge annotation obtained from the local phase.  This is for the      */
/* first iteration. For the next iterations, it reflects the result of  */
/* the trans operation.                                                 */
/* For a call graph, local node annotation corresponds to summary       */
/* information collected for a function or procedure and edge           */
/* annotation refers to summary information collected for a call site   */
/* the parameters passed to trans is the result of the meet operation   */
/* and the previous out annotation and the variable change which        */
/* is used to determine if the data flow problem has settled.           */
/*----------------------------------------------------------------------*/
void IPA_DATA_FLOW::iterative_solver(DFS *df)
{
  DFN *d;
  NODE_INDEX i;

  /* get the depth first ordering of the vertices */
  d = DFS_d(df);

  // Make sure that d is not empty:
  assert ( d != NULL );

  // for a forward problem
  if (DFS_di(df) == FORWARD)
    {
      /* For all vertices in depth first ordering, perform meet and trans */
      for ( i=DFN_first(d); i< DFN_end(d); ++i) {
	INT vindex = DFN_v_list_i(d,i);

	if (vindex == DFS_entry(df) || vindex == DFS_exit(df))
	  /* avoid those entry nodes, since they are there */
	  /* to connect the entire graph                   */
	  continue;
	
	/* compute the meet of incoming edge and existing in. for
	   the meet operation, pass the in set, the predecessors
	   out annotation, the current node annotation and current
	   edge annotation note, for a call graph and the forward
	   dataflow problem, the current node annotation refers to
	   the callee and the edge annotation refers to the
	   callsite note, the meet operation will delete the old
	   in and return the new in.
	   */
	    
	Meet (0,
              NODE_user (&GRAPH_v_i (IPA_Call_Graph->Graph(),
                                     vindex)),
              &DFS_change(df));

	/* for the trans operation, pass the in set, the out set,        */
	/* and the current node annotation (for a call graph, it is the  */
	/* procedure node) and change to determine if the dataflow       */
	/* problem has settled.                                          */
	/* note, the trans operation will delete the old out and return  */
	/* the new out                                                   */
	
	Trans (0,
               0,
               NODE_user (&GRAPH_v_i (IPA_Call_Graph->Graph(),
                                      vindex)),
               &DFS_change(df));
	  
      }
    }
  else
    // for a backward problem go from last to first
    {
      for (i = DFN_end(d)-1; i >= DFN_first(d); --i) 
	{
	  INT vindex = DFN_v_list_i(d,i);

	  if (vindex == DFS_entry(df) || vindex == DFS_exit(df))
	    /* avoid those entry nodes, since they are there */
	    /* to connect the entire graph                   */
	    continue;
	  Meet (0,
                NODE_user (&GRAPH_v_i (IPA_Call_Graph->Graph(),
                                       vindex)),
                &DFS_change(df));
	  
	  Trans (DFS_in_i(df,vindex),
                 DFS_out_i(df,vindex), 
                 NODE_user (&GRAPH_v_i (IPA_Call_Graph->Graph(),
                                        vindex)),
                 &DFS_change(df));
	}
    }
}


// When clone nodes are added to the graph during constant propagation
// Depth-First Numbering of nodes needs to be rebuilt.
BOOL IPA_Should_Rebuild_DFN;

/*---------------------------------------------------------------------*/
/* solve the data flow problem                                         */
/*---------------------------------------------------------------------*/
void IPA_DATA_FLOW::dataflow(DFS *df)
{
  NODE_INDEX tmp, root_node;
  INT i;

  root_node = IPA_Call_Graph->Root();
  pre_process_graph(IPA_Call_Graph->Graph(), df, m);

  IPA_Call_Graph->Set_Root(DFS_entry(df)); 

  if (DFS_di(df) == BACKWARD)
    {
      if ( DFS_d(df) == NULL ) {
	DFS_d(df) = Depth_First_Ordering ( IPA_Call_Graph->Graph(), m );
      }
    }

  /* create the in, out annotations for all vertices */
  DFS_in(df) = (void **)
    MEM_POOL_Alloc ( m, sizeof(void*) * GRAPH_vmax(IPA_Call_Graph->Graph()) );

  assert(DFS_in(df) != 0);
  bzero(DFS_in(df), sizeof(void*)*GRAPH_vmax(IPA_Call_Graph->Graph()));

  DFS_out(df) = (void**)
    MEM_POOL_Alloc(m, sizeof(void*)*GRAPH_vmax(IPA_Call_Graph->Graph()));
  bzero(DFS_out(df), sizeof(void*)*GRAPH_vmax(IPA_Call_Graph->Graph()));
  assert(DFS_out(df) != 0);

  DFS_change(df) = 1;

  /* while the data flow problem has not settled do */
  while(DFS_change(df)) {

    /* reset change to 0, during the trans operation, if the new out  */
    /* is different from the old out then the problem has not settled */
    /* and change must be set to 1                                    */
    DFS_change(df) = 0; 
    IPA_Should_Rebuild_DFN = FALSE;

    /* call the iterative dataflow solver */
    iterative_solver(df);

    if (IPA_Should_Rebuild_DFN) {
      DFS_d(df) = Depth_First_Ordering(IPA_Call_Graph->Graph(), m);
    }
  }


  /* this pass is used if any post processing is needed.             */
  /* in the case of constant propagation, the tcons need to be reset */

  DFN* dd = DFS_d(df);
  for ( i=DFN_first(dd); i< DFN_end(dd); ++i ) {
    INT vindex = DFN_v_list_i(dd,i);
    if (vindex == DFS_entry(df) || vindex == DFS_exit(df))
      continue;
    PostProcessIO(NODE_user(&GRAPH_v_i(IPA_Call_Graph->Graph(),vindex)));
  }

  /* post process the graph, remove the exit and entry and additional edges */
  post_process_graph(IPA_Call_Graph->Graph(),df);  
  IPA_Call_Graph->Set_Root(root_node); 
}

/*---------------------------------------------------------------------*/
/* This routine must be invoked to start a data flow problem           */
/* It sets up all the fields in the DFS data structure and             */
/* solves the dataflow problem using an iterative solution             */
/*---------------------------------------------------------------------*/
void  IPA_DATA_FLOW::Solve()
{
 dataflow(df);
}

/*----------------------------------------------------------------------*/
/* clone a particular node                                              */
/*----------------------------------------------------------------------*/
IPA_NODE* IPA_DATA_FLOW::Clone(IPA_NODE* n)
{
 return IPA_Call_Graph->Create_Clone(n);
}

/*----------------------------------------------------------------------*/
/* the in annotation is the current in for the vertex,                  */
/* edge_in is the in annotation for the incoming edge. vertex is the    */
/* caller, edge is the callsite                                         */
/* return the result of the meet operation, which is the out set        */
/*----------------------------------------------------------------------*/

void *
IPA_DATA_FLOW::Meet ( void* ,  void* vertex, INT * )
{
  IPA_NODE *n = (IPA_NODE *)vertex;
  fprintf ( TFile, "Entered the MEET function \n" );
  return 0;
}

/*----------------------------------------------------------------------*/
/* return the new out set.                                              */
/*----------------------------------------------------------------------*/

void *
IPA_DATA_FLOW::Trans ( void *, void *, void *vertex, INT *)
{
  /* IPA_NODE *nclone; */
  IPA_NODE *n = (IPA_NODE *)vertex;
  fprintf ( TFile, "Entered the trans function \n" );
  /* nclone = Clone(n);  */
  return 0;
}

/*----------------------------------------------------------------------*/
/* get the caller, given the edge                                       */
/*----------------------------------------------------------------------*/
IPA_NODE* 
IPA_DATA_FLOW::Get_caller(IPA_EDGE *edge)
{
  return IPA_Call_Graph->Caller (edge);
}

/*----------------------------------------------------------------------*/
/* get the callee, given the edge  */
/*----------------------------------------------------------------------*/
IPA_NODE* IPA_DATA_FLOW::Get_callee(IPA_EDGE *edge)
{
    return IPA_Call_Graph->Callee (edge);
}

/*----------------------------------------------------------------------*/
/* constructor                                                          */
/*----------------------------------------------------------------------*/
IPA_DATA_FLOW::IPA_DATA_FLOW (DF_DIRECTION ddf, MEM_POOL *mm)
{
  m = mm;
  d = ddf;

  df = (DFS*) MEM_POOL_Alloc(m,sizeof(DFS));
 
  DFS_di(df) = d;

  // note, build the depth first ordering only for a forward problem
  // for a backward problem, build it after the graph has been
  // reversed
 
  // build a depth first ordering of the graph 
  DFS_d(df) = Depth_First_Ordering ( IPA_Call_Graph->Graph(), m );
  DFS_entry(df) = IPA_Call_Graph->Root();

}

//----------------------------------------------------------------------
// print the output after solving the dataflow problem
//----------------------------------------------------------------------
void 
IPA_DATA_FLOW::Print(FILE* fp)
{
  INT i;

  if (df == NULL)
    Fail_FmtAssertion("You cannot print before solving the problem!! \n");

  DFN* dd = DFS_d(df);
  for ( i=DFN_first(dd); i< DFN_end(dd); ++i ) {
    INT vindex = DFN_v_list_i(dd,i);
    if (vindex == DFS_entry(df) || vindex == DFS_exit(df))
      continue;
    if (NODE_user(&GRAPH_v_i(IPA_Call_Graph->Graph(),vindex)) != NULL)
      Print_entry(fp, DFS_out_i(df,vindex), 
		  NODE_user(&GRAPH_v_i(IPA_Call_Graph->Graph(),vindex)));
  }
}

//----------------------------------------------------------------------
// print the output for each entry
//----------------------------------------------------------------------
void 
IPA_DATA_FLOW::Print_entry ( FILE *fp, void *, void *)
{
  fprintf ( fp, "Entered the print_entry function \n" );
}
