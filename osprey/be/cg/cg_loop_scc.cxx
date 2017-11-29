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



#include <alloca.h>

#include "defs.h"
#include "mempool.h"
#include "stdio.h"
#include "erglob.h"
#include "tracing.h"
#include "cg.h"
#include "cgir.h"
#include "op_list.h"
#include "cg_loop_scc_prop.h"
#include "cg_loop_scc.h"
#include "cg_loop.h"
#include "op_map.h"

/* Exported variables (See interface documentation in cg_loop_scc.h.)
 */
BB          *CG_LOOP_SCC_Current_BB;
CG_LOOP_SCC *CG_LOOP_SCC_Vec;
INT          CG_LOOP_SCC_Count;
INT32        CG_LOOP_max_scc_member_count;
INT32        CG_LOOP_SCC_mark_count;  /* To be used only by 1 set macros. */

/* This type holds the state we need to track for each OP during
 * Tarjan's algorithm
 */
typedef struct scc_op {
  mINT32        dfs_number;
  mINT32        low_link;
  mBOOL         stacked;
} SCC_OP;

/* Accessors for SCC_OP structure.
 */
#define SCC_OP_dfs_number(x) ((x)->dfs_number)
#define SCC_OP_low_link(x)   ((x)->low_link)
#define SCC_OP_stacked(x)    ((x)->stacked)

#define     SCC_OP_Map(x)    ((SCC_OP *)BB_OP_MAP_Get(scc_op_map,x))
#define Set_SCC_OP_Map(x,v)  BB_OP_MAP_Set(scc_op_map,x,(v))

#define       OP_dfs_number(x)   (SCC_OP_dfs_number(SCC_OP_Map(x))+0)
#define   Set_OP_dfs_number(x,v) (SCC_OP_dfs_number(SCC_OP_Map(x))=(v))
#define       OP_low_link(x)     (SCC_OP_low_link(SCC_OP_Map(x))+0)
#define   Set_OP_low_link(x,v)   (SCC_OP_low_link(SCC_OP_Map(x))=(v))
#define       OP_stacked(x)      (SCC_OP_stacked(SCC_OP_Map(x))+0)
#define   Set_OP_stacked(x)      (SCC_OP_stacked(SCC_OP_Map(x))=TRUE)
#define Reset_OP_stacked(x)      (SCC_OP_stacked(SCC_OP_Map(x))=FALSE)

/* There is no structure field to keep track of 'visited'. Instead
 * we use the presence of the op-map entries existence as the flag.
 */
#define     OP_visited(x)      (SCC_OP_Map(x) != 0)

/* Local variables
 */
static OP **stack;         /* Stack of OP*s used by Tarjan's algorithm.  Always
                            * points at the next location to add a new element.
                            * Grows upward.
                            */
static OP **members_vec;   /* Storage for all the _members vectors,
                            * preallocated.  We can do this because we know
                            * that each OP is a member of exactly one SCC.
                            */
static INT dfs_count;      /* Counter used to assign DFS numbers to OPs during
                            * Tarjan's algorithm
                            */
static BB_OP_MAP scc_op_map;  /* OP map to keep track of per-OP state during
                            * Tarjan's algorithm
                            */
static INT trace_scc = -1; /* Flag to enable tracing; -1 means we haven't
                            * determined if we should trace yet.
                            */
static SCC_OP *scc_ops;    /* Pointer to list of per-OP state structures
                            * to be used as needed.
                            */
static MEM_POOL scc_map_pool;

/* =======================================================================
 *
 *  Trace_Begin_Scc
 *
 *  <ls> Find SCCs [
 *
 * =======================================================================
 */
static void
Trace_Begin_Scc(BB *bb)
{
  if ( trace_scc ) {
    if ( trace_scc < 0 ) {
      trace_scc = Get_Trace(TP_CGLOOP,0x1000,bb);
      if ( !trace_scc ) return;
    }
    fprintf(TFile,"<ls> Find SCCs[");
    fflush(TFile);
  }
}


/* =======================================================================
 *
 *  Trace_End_Scc
 *
 *  <ls> Find SCCs [OK]
 *  <scc> #0
 *  <scc member> $TN152 :- sdc1 TN150 TN151 TN1(0xfffffff8) ;
 *  <scc> #1
 *  <scc member> $TN152 :- sdc1 TN150 TN151 TN1(0xfffffff8) ;
 *
 * =======================================================================
 */
static void
Trace_End_Scc(void)
{
  if ( trace_scc) {
    fprintf(TFile,"OK]\n");
    CG_LOOP_Print_Strongly_Connected_Components();
  }
}

/* ====================================================================
 *
 *  Make_Component
 *
 *  Everything on the stack above and including "op" are members of
 *  the same stongly connected component.  Pop them off the stack and
 *  put them into a new SCC.  Initialize its fields.
 *
 * ====================================================================
 */
static void
Make_Component(
  OP  *op
)
{
  CG_LOOP_SCC *scc = CG_LOOP_SCC_Vec + CG_LOOP_SCC_Count;
  INT          n;
  INT          i;
  OP         **mp;
  OP         **pop;

  Set_CG_LOOP_SCC_id(scc,CG_LOOP_SCC_Count);
  ++CG_LOOP_SCC_Count;

  /* Count elements in component.
   */
  for ( pop = stack - 1; *pop != op; --pop );

  n = stack - pop;
  Set_CG_LOOP_SCC_member_count(scc,n);

  if ( CG_LOOP_max_scc_member_count < n )
    CG_LOOP_max_scc_member_count = n;

  /* Allocate memory for members:
   */
  Set_CG_LOOP_SCC_members(scc,(mp = members_vec));
  members_vec += n;

  /* Loop up the stack from op, adding elements to scc, and
   * setting scc related fields.  Can use do..while because we know
   * there will always be at least 1 (op) element on stack.
   */
  i = 0;
  do {
    OP *sop = pop[i];

    mp[i] = sop;

    Reset_OP_stacked(sop);
    Set_OP_scc(sop,scc);
    Set_OP_scc_index(sop,i);

  } while ( ++i < n );


  /* Pop OP and everything above off the stack:
   */
  stack = pop;
}

/* ====================================================================
 *
 *  Visit
 *
 *  Recursive DFS routine for finding strongly connected components.
 *  This is the amazing part.
 *
 * ====================================================================
 */
static void
Visit(
  OP   *op,
  BOOL ignore_non_def_mem_deps
)
{
  ARC_LIST *succs;

  /* The op becomes 'visited' by creating the op-map entry for the op.
   */
  Set_SCC_OP_Map(op,scc_ops++);
  Set_OP_dfs_number(op,dfs_count);
  Set_OP_low_link(op,dfs_count);
  dfs_count++;

  *stack++ = op;
  Set_OP_stacked(op);

  for ( succs = OP_succs(op);
        succs != NULL;
        succs = ARC_LIST_rest(succs)
  ) {
    ARC *succ_arc = ARC_LIST_first(succs);
    OP  *succ     = ARC_succ(succ_arc);

    if (!ignore_non_def_mem_deps || !ARC_is_mem(succ_arc) ||
	ARC_is_definite(succ_arc)) {

      /* You might think you could simplify the control flow of the
       * following so as to have only one copy of the minimization
       * statement, but beware, it's more subtle than it seems at first.
       */
      if ( !OP_visited(succ) ) {
	Visit(succ, ignore_non_def_mem_deps);
	Set_OP_low_link(op,Min(OP_low_link(op),OP_low_link(succ)));
      }
      else if (    OP_dfs_number(succ) < OP_dfs_number(op)
	       && OP_stacked(succ)
	       ) {
	Set_OP_low_link(op,Min(OP_low_link(op),OP_dfs_number(succ)));
      }
    }
  }

  if ( OP_low_link(op) == OP_dfs_number(op) ) {

    /* Everything on the stack above and including op is a strongly
     * connected component.
     */

    Make_Component(op);
  }
}

/* ====================================================================
 *
 *  CG_LOOP_Make_Strongly_Connected_Components
 *
 *  See cg_loop_scc.h for interface description.
 *
 * ====================================================================
 */
void
CG_LOOP_Make_Strongly_Connected_Components(
  BB   *bb,
  MEM_POOL *vec_pool,
  BOOL ignore_non_def_mem_deps
)
{
  static BOOL init_done = FALSE;
  OP         *op;
  INT16       op_count = BB_length(bb);

  if ( ! init_done ) {
    MEM_POOL_Initialize(&scc_map_pool, "SCC_OP_MAP_pool", FALSE);
    init_done = TRUE;
  }

  Trace_Begin_Scc(bb);
  CG_LOOP_max_scc_member_count = 0;

  MEM_POOL_Push(&scc_map_pool);

  /* Create the OP_MAP we use to attach extra state to each OP during
   * processing. We also create a vector of state structures to be
   * used as OPs are "Visit"ed.
   */
  scc_op_map = BB_OP_MAP_Create(bb, &scc_map_pool);
  scc_ops = (SCC_OP *) alloca(sizeof(SCC_OP) * op_count);

  CG_LOOP_SCC_mark_count = 0;

  CG_LOOP_SCC_Vec = TYPE_MEM_POOL_ALLOC_N(CG_LOOP_SCC, vec_pool, op_count);
  members_vec = TYPE_MEM_POOL_ALLOC_N(OP *, vec_pool, op_count);

  CG_LOOP_SCC_Current_BB = bb;
  CG_LOOP_SCC_Count      = 0;

  stack = (OP **) alloca(sizeof(OP *) * op_count);
  dfs_count = 0;

  for ( op = BB_first_op(bb); op != NULL; op = OP_next(op) ) {
    if ( ! OP_visited(op) ) {
      Visit(op, ignore_non_def_mem_deps);
    }
  }

  MEM_POOL_Pop(&scc_map_pool);

  Trace_End_Scc();
}

/* ====================================================================
 *
 *  Print_SCC_Vec
 *
 *  'Sv' is a vector of SCCs and 'n' gives its length.  Print a
 *  representation of this vector using the id of the elements.
 *
 * ====================================================================
 */
static void
Print_SCC_Vec(
  CG_LOOP_SCC **sv,
  INT           n
)
{
  INT i;
  BOOL printed_first = FALSE;

  fprintf(TFile,"(");
  for ( i = 0; i < n; ++i ) {

    if ( printed_first )
      fprintf(TFile," ");
    else
      printed_first = TRUE;

    fprintf(TFile,"%d",CG_LOOP_SCC_id(sv[i]));
  }
  fprintf(TFile,")");
}


/* ====================================================================
 *
 *  Print_OP_Vec
 *
 *  'ov' is a vector of OPs and 'n' gives its length.  Print the
 *  OPs in the vector.
 *
 * ====================================================================
 */
static void
Print_OP_Vec(
  OP   **ov,
  INT    n
)
{
  INT       i;
  ARC_LIST *al;

  for ( i = 0; i < n; ++i ) {
    OP *op = ov[i];

    /* Print ARCs to ancestors in same SCC.
     */
    for ( al = OP_scc_ancestors(op);
          al != NULL;
          al = ARC_LIST_rest(al)
    ) {
      ARC *arc = ARC_LIST_first(al);
      OP  *anc = ARC_pred(arc);
      INT  omega = ARC_omega(arc);
      INT  latency = ARC_latency(arc);

      fprintf(TFile,"<scc ancestor arc> %d omega %d latency %d\n",
                    OP_scc_index(anc),
                    omega,
                    latency);
    }

    fprintf(TFile,"<scc member> #%d (%d in scc): ", OP_map_idx(op),
	    OP_scc_index(op));
    Print_OP_No_SrcLine(op);

    /* Print ARCs to descendents in same SCC.
     */
    for ( al = OP_scc_descendents(op);
          al != NULL;
          al = ARC_LIST_rest(al)
    ) {
      ARC *arc = ARC_LIST_first(al);
      OP  *des = ARC_succ(arc);
      INT  omega = ARC_omega(arc);
      INT  latency = ARC_latency(arc);

      fprintf(TFile,"<scc descendent arc> %d omega %d latency %d\n",
                    OP_scc_index(des),
                    omega,
                    latency);
    }
  }
}

/* ====================================================================
 *
 *  CG_LOOP_SCC_Print
 *
 *  See interface description
 *
 * ====================================================================
 */
void CG_LOOP_SCC_Print(
    CG_LOOP_SCC   *scc
)
{
  fprintf(TFile,"<scc> #%d\n",CG_LOOP_SCC_id(scc));
  if ( CG_LOOP_SCC_pred_count(scc) > 0 ) {
    fprintf(TFile,"Preds:");
    Print_SCC_Vec(CG_LOOP_SCC_preds(scc),CG_LOOP_SCC_pred_count(scc));
  }
  if ( CG_LOOP_SCC_succ_count(scc) > 0 ) {
    fprintf(TFile,"Succs:");
    Print_SCC_Vec(CG_LOOP_SCC_succs(scc),CG_LOOP_SCC_succ_count(scc));
  }
  Print_OP_Vec(CG_LOOP_SCC_members(scc),CG_LOOP_SCC_member_count(scc));
}


/* ====================================================================
 *
 *  CG_LOOP_SCC_Print_With_Costs
 *
 *  See interface description
 *
 * ====================================================================
 */
void CG_LOOP_SCC_Print_With_Costs(
    CG_LOOP_SCC   *scc
)
{
  ErrMsg( EC_Unimplemented, "funcname" );
}


/* ====================================================================
 *
 *   CG_LOOP_Print_Strongly_Connected_Components
 *
 *  See interface description.
 *
 * ====================================================================
 */
void CG_LOOP_Print_Strongly_Connected_Components(void)
{
  INT i;

  for ( i = 0;
        i < CG_LOOP_SCC_Count;
        ++i
  ) {
    CG_LOOP_SCC_Print(CG_LOOP_SCC_Vec + i);
  }
}


/* ====================================================================
 *
 *  CG_LOOP_Print_Strongly_Connected_Components_With_Costs(void)
 *
 *  See interface description.
 *
 * ====================================================================
 */
void CG_LOOP_Print_Strongly_Connected_Components_With_Costs(void)
{
  ErrMsg( EC_Unimplemented, "funcname" );
}
