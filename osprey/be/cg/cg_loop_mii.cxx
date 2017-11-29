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




#include "math.h"   /* Must be before defs.h */
#include "limits.h"
#include "defs.h"
#include "erglob.h"
#include "tracing.h"
#include "mempool.h"
#include "cg.h"
#include "cgir.h"
#include "op_list.h"
#include "ti_res_count.h"
#include "register.h"
#include "config.h"
#include "tn_prop.h"
#include "bb.h"
#include "cgtarget.h"
#include "cg_loop_scc_prop.h"
#include "cg_loop_scc.h"
#include "cg_loop.h"

#include "cg_loop_mii.h"

INT32       CG_LOOP_min_ii;
INT32       CG_LOOP_res_min_ii;
#ifdef TARG_IA64
INT32       CG_LOOP_rec_min_ii_with_dspec; /* Rec MII ignoring violable mem-dep */  
#endif

INT32       CG_LOOP_rec_min_ii;

static INT trace_mii = -1;
static CG_LOOP_SCC *scc;

/* ====================================================================
 *
 *  Trace_Begin_Min_II
 *
 *  <cg_loop_mii> Min II calculation
 *
 * ====================================================================
 */
static void Trace_Begin_Min_II(BB *bb)
{
  if ( trace_mii ) {
    #pragma mips_frequency_hint NEVER
    if ( trace_mii < 0 ) {
      trace_mii = Get_Trace(TP_CGLOOP,0x2000,bb);
      if ( !trace_mii ) return;
    }
    fprintf(TFile,"<cg_loop_mii> Begin %s Min II calculation for loop at line %d\n",
	    Get_Error_Phase(), BB_Loop_Lineno(bb));
  }
}


/* ====================================================================
 *
 *  Trace_End_Min_II
 *
 *  <cg_loop_mii> CG_LOOP_min_ii 123
 *
 * ====================================================================
 */
static void Trace_End_Min_II(void)
{
  if ( trace_mii ) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"<cg_loop_mii> CG_LOOP_min_ii %d\n",CG_LOOP_min_ii);
  }
}


/* ====================================================================
 *
 *  Trace_Begin_Min_II_Resource
 *
 *  <cg_loop_mii> Min resource II [
 *
 * ====================================================================
 */
static void Trace_Begin_Min_II_Resource(void)
{
  if ( trace_mii ) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"<cg_loop_mii> Min resource II [");
    fflush(TFile);
  }
}


/* ====================================================================
 *
 *  Trace_Min_II_Resource_Pre_LOH
 *
 *  <cg_loop_mii> Min resource II [OK] 124 (xlo)
 *
 * ====================================================================
 */
static INT32 pmin_ii;

static void Trace_Min_II_Resource_Pre_LOH(TI_RES_COUNT *res_counts)
{
  pmin_ii = CG_LOOP_min_ii;

  if ( trace_mii ) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"OK] %d",CG_LOOP_min_ii);
    fprintf(TFile,"\n<cg_loop_mii> ");

    TI_RES_COUNT_Print(TFile, res_counts);
    fprintf(TFile,"\n");
  }
}


/* ====================================================================
 *
 *  Trace_Min_II_Resource_LOH(
 *
 *  <cg_loop_mii> Min LOH resource II 125 (baz)
 *
 * ====================================================================
 */
static void Trace_Min_II_Resource_LOH(
    INT32 loh_mii
)
{
  if (    trace_mii
       && pmin_ii < loh_mii
  ) {
    #pragma mips_frequency_hint NEVER
    pmin_ii = loh_mii;
    fprintf(TFile,"<cg_loop_mii> Min LOH resource II %d",loh_mii);

    fprintf(TFile,"\n");
  }
}


/* ====================================================================
 *
 *  Trace_Begin_Min_II_Scc
 *
 *  <cg_loop_mii> Min recurrence II for scc n [
 *
 * ====================================================================
 */
static void Trace_Begin_Min_II_Scc(
  BB *bb,
  CG_LOOP_SCC *s
)
{
  if ( trace_mii ) {
    #pragma mips_frequency_hint NEVER
    if ( trace_mii < 0 ) {
      trace_mii = Get_Trace(TP_CGLOOP,0x2000,bb);
      if ( !trace_mii ) return;
    }

    scc = s;

    fprintf(TFile,"<cg_loop_mii> Min recurrence II after scc #%d [",
            CG_LOOP_SCC_id(scc));
    fflush(TFile);
  }
}


/* ====================================================================
 *
 *  Trace_End_Min_II_Scc
 *
 *  <cg_loop_mii> Min recurrence II for scc n [OK] 123
 *  <scc> #n
 *  <scc ancestor arc> "OP_scc_index" "omega" latency"
 *  <scc member> $TN152 :- sdc1 TN150 TN151 TN1(0xfffffff8) ;
 *  <scc descendent arc>  "OP_scc_index" "omega" latency"
 *
 * ====================================================================
 */
static void Trace_End_Min_II_Scc(void)
{
  if ( trace_mii ) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"OK] %d\n",CG_LOOP_min_ii);

    if ( CG_LOOP_SCC_member_count(scc) > 1 )
      CG_LOOP_SCC_Print(scc);
  }
}

/* ====================================================================
 *
 *  Ceiling_Divide
 *
 *  ceiling( 'x' / 'y' ) where the result, 'x', and 'y' are all
 *  integers.
 *
 * ====================================================================
 */
inline INT
Ceiling_Divide(
  INT x,
  INT y
)
{
  Is_True(y != 0,("Divide by 0"));

  return (x + y - 1) / y;
}

/* ====================================================================
 *
 *  Calculate_Min_Resource_II
 *
 *  Sum all the resources required for all the OPs and compare to the
 *  number available per cycle for each resource kind. 
 *
 * ====================================================================
 */
static void
Calculate_Min_Resource_II(
  BB      *loop_body,
  OP_LIST *loop_overhead,
  BOOL ignore_prefetches,
  BOOL ignore_prefetch_strides
)
{
  TI_RES_COUNT *res_counts;
  OP           *op;
  OP_LIST      *loh_l;

  Trace_Begin_Min_II_Resource();

  /* Initialize:
   */
  res_counts = TI_RES_COUNT_Alloc(&MEM_local_nz_pool);

  /* Sum resources from each OP's resource usage, omitting loop
   * overhead and the final branch for now.
   */
  for ( op = BB_first_op(loop_body); op != NULL; op = OP_next(op) ) {
    if (!OP_loh(op)) {
      TOP opc = OP_code(op);
      if (OP_prefetch(op)) {
	if (!ignore_prefetches) {
	  if (ignore_prefetch_strides) {
	    TI_RES_COUNT_Add_Op_Resources(res_counts, opc);
	  } else {
	    double stride = CG_LOOP_Prefetch_Stride(op);
	    TI_RES_COUNT_Add_Op_Resources_Scaled(res_counts, opc, 1.0/stride);
	  }
	}
      } else {
	TI_RES_COUNT_Add_Op_Resources(res_counts, opc);
      }
    }
  }

  CG_LOOP_min_ii = TI_RES_COUNT_Min_II(res_counts);

  Trace_Min_II_Resource_Pre_LOH(res_counts);

  /* As II increases, we will schedule as many of our loop overhead IV
   * updates as we can do for free.  These are not necessarily
   * included in the final schedule; rather they are used as
   * convenient stand-ins, so we know where the holes are.
   */
  for ( loh_l = loop_overhead; loh_l; loh_l = OP_LIST_rest(loh_l) ) {
    INT32 iv_update_mii;
    OP   *loh = OP_LIST_first(loh_l);

    TI_RES_COUNT_Add_Op_Resources(res_counts,OP_code(loh));
    iv_update_mii = (INT32)ceil(TI_RES_COUNT_Min_Cycles(res_counts));

    Set_OP_loh_mii(loh,iv_update_mii);
    Trace_Min_II_Resource_LOH(iv_update_mii);
  }
}

/* ====================================================================
 * ====================================================================
 *
 * Local data structures for the max cost paths algorithm.
 *
 * Data structures to represent costs, vectors of costs, and 2-d
 * arrays of vectors of costs.  The biggest performace problem with
 * previous implementations was due to memory management.  I've
 * attempted to address this here by using what should (in almost all
 * cases) be a once (per SCC) allocated data structure.  I've also
 * tried to optimize for the case where there is only a single cost
 * path kept at any time.
 *
 * Types:
 *
 *    typedef struct cost COST
 *
 *        Represents the cost of a path of ARCs through the SCC.  It
 *        has the following fields.
 *
 *            INT omega
 *
 *                Gives the iteration distance of the path.
 *
 *            INT latency
 *
 *                Gives its latency (= "delay").
 *
 *    typedef struct cost_vec COST_VEC
 *
 *        Represents a vector of COSTs, but keeps track of length and
 *        allocated length to allow for dynamic sizing.
 *
 *            INT length
 *
 *                Current number of element.
 *
 *            INT alloc_length
 *
 *                Maximum number of elements
 *
 *            COST *costs
 *
 *                Pointer to the costs.
 *
 *    typedef struct cost_table COST_TABLE
 *
 *        Represents a square 2-d array of COST_VECs.  Cost_table[i,j]
 *        is the cost of paths from the i-th member of the SCC to the
 *        j-th member.
 *
 *            INT n
 *
 *                How many elements on a side.
 *
 *            COST_VEC costs_vecs[]
 *
 *                Storage for the COST_VECs.  Access with
 *                COST_TABLE_cost_vec(cost_table,i,j).
 *
 *
 * ====================================================================
 * ====================================================================
 */

typedef struct cost {
  INT   latency;
  INT   omega;
} COST;

#define COST_latency(x) ((x)->latency)
#define COST_omega(x) ((x)->omega)

typedef struct cost_vec {
  INT   length;
  INT   alloc_length;
  COST *costs;
} COST_VEC;

#define COST_VEC_length(x)        ((x)->length)
#define COST_VEC_alloc_length(x)  ((x)->alloc_length)
#define COST_VEC_costs(x)         ((x)->costs)

#define COST_VEC_cost(x,i)        (COST_VEC_costs(x)[(i)])
#define COST_VEC_latency(x,i)     COST_latency(&COST_VEC_cost(x,i))
#define COST_VEC_omega(x,i)       COST_omega(&COST_VEC_cost(x,i))

typedef struct cost_table {
  INT n;
  COST_VEC cost_vecs[1];
} COST_TABLE;

#define COST_TABLE_n(x) ((x)->n)
#define COST_TABLE_cost_vecs(x) ((x)->cost_vecs)

#define COST_TABLE_cost_vec(x,i,j)                                     \
  (&COST_TABLE_cost_vecs(x)[((i) * COST_TABLE_n(x)) + j])

/* ====================================================================
 *
 *  COST_VEC_Initialize
 *
 *  Allocate initial space of the COSTs.  I've just guessed at a good
 *  initial size.  In fact, I don't expect this to exceed 1 very
 *  often.
 *
 * ====================================================================
 */
static void
COST_VEC_Initialize(
  COST_VEC *self
)
{
  INT initial_length = 4;

  COST_VEC_length(self) = 0;
  COST_VEC_alloc_length(self) = initial_length;

  COST_VEC_costs(self) = TYPE_MEM_POOL_ALLOC_N(COST,
					       &MEM_local_nz_pool,
					       initial_length);
}

/* ====================================================================
 *
 *  COST_VEC_Add_Cost
 *
 *  Add the cost <'omega','latency'> to 'cost_vec', allocating new
 *  storate as necessary.
 *
 * ====================================================================
 */
static void
COST_VEC_Add_Cost(
  COST_VEC *cost_vec,
  INT       omega,
  INT       latency
)
{
  INT length = COST_VEC_length(cost_vec);

  if (length == COST_VEC_alloc_length(cost_vec)) {
    INT   i;
    INT   new_length = length * 2;
    COST *new_vec    = TYPE_MEM_POOL_ALLOC_N(COST,
					     &MEM_local_nz_pool,
					     new_length);
    COST *old_vec    = COST_VEC_costs(cost_vec);

    for ( i = 0; i < length; ++i ) {
      *(new_vec + i) = *(old_vec + i);  /* structure assignment */
    }

    COST_VEC_alloc_length(cost_vec) = new_length;
    COST_VEC_costs(cost_vec) = new_vec;
  }

  COST_VEC_omega(cost_vec,length) = omega;
  COST_VEC_latency(cost_vec,length) = latency;
  COST_VEC_length(cost_vec) = length + 1;
}

/* ====================================================================
 *
 *  COST_TABLE_Add_Cost
 *
 *  Add the cost <'omega','latency'> from the 'i'-th member to the
 *  'j'-th member in 'self'.
 *
 * ====================================================================
 */

static void
COST_TABLE_Add_Cost(
  COST_TABLE  *self,
  INT          i,
  INT          j,
  INT          omega,
  INT          latency
) {
  COST_VEC_Add_Cost(COST_TABLE_cost_vec(self,i,j),omega,latency);
}

/* ====================================================================
 *
 *  COST_TABLE_Alloc
 *
 *  Allocate a 'n' X 'n' COST_TABLE.  Initialize its COST_VECs.
 *
 * ====================================================================
 */
static COST_TABLE*
COST_TABLE_Alloc(
  INT n
)
{
  INT         i;
  COST_TABLE *result;
  INT         nxn = n * n;

  /* One COST_VEC was already included in sizeof(COST_TABLE) (see
   * declaration.)
   */
  result = (COST_TABLE *) L_Alloc(  sizeof(COST_TABLE)
                   + sizeof(COST_VEC) * ((nxn) - 1));

  COST_TABLE_n(result) = n;

  for ( i = 0; i < nxn; ++i )
    COST_VEC_Initialize(&COST_TABLE_cost_vecs(result)[i]);

  return result;
}

/* ====================================================================
 *
 *  Calculate_Component_Direct_Costs_And_Min_Recurrence_II
 *
 *  Use the direct ARCs within 'scc' to initialize 'costs'.
 *  Make sure that Min_II reflects any self ARCs.
 *
 * ====================================================================
 */
static void
Calculate_Component_Direct_Costs_And_Min_Recurrence_II(
  CG_LOOP_SCC  *scc,
  COST_TABLE   *costs
)
{
  INT i;
  INT n = CG_LOOP_SCC_member_count(scc);

  for ( i = 0; i < n; ++i ) {
    ARC_LIST *succ_arcs;
    OP       *op = CG_LOOP_SCC_member(scc,i);

    for ( succ_arcs = OP_succs(op);
          succ_arcs != NULL;
          succ_arcs = ARC_LIST_rest(succ_arcs)
    ) {
      ARC *succ_arc = ARC_LIST_first(succ_arcs);
      OP  *succ_op  = ARC_succ(succ_arc);

      Is_True(op == ARC_pred(succ_arc),
              ("Thought ARC_pred == ARC_pred"));

      if ( OP_scc(succ_op) == scc ) {

        COST_TABLE_Add_Cost(costs,OP_scc_index(op),
                                  OP_scc_index(succ_op),
                                  ARC_omega(succ_arc),
                                  CGTARG_ARC_Sched_Latency(succ_arc));

        /* Don't forget to initialize CG_LOOP_min_ii for the self ARCs.
         * (Monica Lam forgot this.)
         */
        if ( succ_op == op ) {
	  Set_CG_LOOP_SCC_rec_mii(scc,
	    Max(CG_LOOP_SCC_rec_mii(scc),
		Ceiling_Divide(CGTARG_ARC_Sched_Latency(succ_arc),
			       ARC_omega(succ_arc))));
          CG_LOOP_min_ii = Max(CG_LOOP_min_ii, CG_LOOP_SCC_rec_mii(scc));
        }
      }
    }
  }
}

/* ====================================================================
 *
 *  Is_Max_Cost
 *
 *  Is the cost <'omega','latency'> maximal relative to the elements
 *  in 'cv' given 'min_ii'?  'Offset' gives the index of the first
 *  element in 'cv' to check.
 *
 * ====================================================================
 */
static BOOL
Is_Max_Cost(
  INT       omega,
  INT       latency,
  COST_VEC *cv,
  INT32     min_ii,
  INT       offset
)
{
  INT   i;
  INT   len = COST_VEC_length(cv);
  COST *cp  = COST_VEC_costs(cv);

  for ( i = offset; i < len; ++i ) {
    INT cvomega   = COST_omega(cp + i);
    INT cvlatency = COST_latency(cp + i);

    /* We can reject things with duplicates, since never check a cost
     * against itself.
     */
    if (   (omega == cvomega && latency <= cvlatency)
        ||    omega > cvomega
           && (latency - cvlatency) <= ((omega - cvomega) * min_ii)
    ) {
      return FALSE;
    }
  }
  return TRUE;
}

/* ====================================================================
 *
 *  Add_Maximal_Costs
 *
 *  'Cvij', 'cvik', 'cvkj' are existing entries in the cost table.
 *  Make 'cvij' be the maximal costs (given the current CG_LOOP_min_ii) in the
 *  set cvij U (cvik + cvkj).  In other words, we want to consider
 *  paths from i to j via k as well as all the paths from i to j we
 *  have already considered.  Only those that we can be maximal (given
 *  the min_ii) are included in the resulting 'cvij'.
 *
 * ====================================================================
 */
static void
Add_Maximal_Costs(
  COST_VEC  *cvij,
  COST_VEC  *cvik,
  COST_VEC  *cvkj,
  INT32	     min_ii
)
{
  INT   i, j;
  COST *cvij_costs;
  INT   cvij_length;
  COST *cvik_costs   = COST_VEC_costs(cvik);
  COST *cvkj_costs   = COST_VEC_costs(cvkj);
  INT   cvik_length  = COST_VEC_length(cvik);
  INT   cvkj_length  = COST_VEC_length(cvkj);

  /* First consider costs in the cross product of cjik and cvkj.
   * We'll add a cost if it is maximal relative to the current costs
   * in cvij.
   */
  for ( i = 0; i < cvik_length; ++i ) {
    COST *cpik      = cvik_costs + i;
    INT   ikomega   = COST_omega(cpik);
    INT   iklatency = COST_latency(cpik);

    for ( j = 0; j < cvkj_length; ++j ) {
      COST *cpkj       =  cvkj_costs + j;
      INT   kjomega    = COST_omega(cpkj);
      INT   kjlatency  = COST_latency(cpkj);
      INT   ikjomega   = ikomega + kjomega;
      INT   ikjlatency = iklatency + kjlatency;

      if ( Is_Max_Cost(ikjomega,ikjlatency,cvij,min_ii,0) )
        COST_VEC_Add_Cost(cvij,ikjomega,ikjlatency);
    }
  }

  /* Now each cost in cvij is maximal relative to the preceeding
   * costs, but not necessarily relative to the succeeding costs.
   * We'll compare each cost to the succeeding costs, and delete it if
   * it is not (possibly) maximal.  Deletion is accompilshed by
   * copying the last element of cvij into the element to be deleted
   * and decrementing the length of cvij.  We consider the elements in
   * reverse order so that will not disturb the part of the vector not
   * yet processed.
   */

  cvij_costs  = COST_VEC_costs(cvij);   /* Possibly side-effected */
  cvij_length = COST_VEC_length(cvij);  /* ..by previous loop.    */

  for ( i = cvij_length - 1; i >= 0; --i ) {
    COST *cpij = cvij_costs + i;
    INT   omega   = COST_omega(cpij);
    INT   latency = COST_latency(cpij);

    if ( ! Is_Max_Cost(omega,latency,cvij,min_ii,i+1) ) {
      /* Delete by replacing with last element...
       */
      if ( i != cvij_length - 1 ) {
        COST *ij_last = cvij_costs + (cvij_length - 1);

        *cpij = *ij_last;           /* Structure copy. */
      }
      /* ... and decrementing length.
       */
      --cvij_length;
    }
  }

  COST_VEC_length(cvij) = cvij_length;
}

/* ====================================================================
 *
 *  Maximize_Min_II
 *
 *  Check for new maximal cost cycles.  'Cv1' and 'cv2' are the
 *  current cost vectors from some element to another and back again.
 *  The cross product of paths is considered and checked against
 *  CG_LOOP_min_ii, which is updated if a new max is found.  The SCC's
 *  current recurrence min_ii is also updated if it is surpassed.
 *
 * ====================================================================
 */
static void
Maximize_Min_II(
  CG_LOOP_SCC *scc,
  COST_VEC    *cv1,
  COST_VEC    *cv2
)
{
  INT   i, j;
  COST *cp1 = COST_VEC_costs(cv1);
  COST *cp2 = COST_VEC_costs(cv2);
  INT   len1 = COST_VEC_length(cv1);
  INT   len2 = COST_VEC_length(cv2);
  INT   mii = CG_LOOP_min_ii;
  INT   scc_mii = CG_LOOP_SCC_rec_mii(scc);

  for ( i = 0; i < len1; ++i ) {
    INT omega1   = COST_omega(cp1 + i);
    INT latency1 = COST_latency(cp1 + i);

    for ( j = 0; j < len2; ++j) {
      INT omega2   = COST_omega(cp2 + j);
      INT latency2 = COST_latency(cp2 + j);
      INT path_mii = Ceiling_Divide(latency1+latency2, omega1+omega2);
      scc_mii = Max(scc_mii, path_mii);
      mii = Max(mii, path_mii);
    }
  }

  CG_LOOP_min_ii = mii;
#ifdef TARG_IA64
  CG_LOOP_rec_min_ii = Max(CG_LOOP_rec_min_ii, scc_mii);
#else
  CG_LOOP_rec_min_ii = scc_mii;
#endif
  Set_CG_LOOP_SCC_rec_mii(scc,scc_mii);
}

/* ====================================================================
 *
 *  Calculate_Component_Max_Costs_And_Min_Recurrence_II
 *
 *  "Scc" is a multinode SCC.  Calculate its maximum cost internal
 *  paths. Update Min_II if necessary to reflect the cost of its
 *  longest path.  Finally, use the max cost table to set the
 *  _scc_ancestors and _scc_descendents fields of each member.
 *
 * ====================================================================
 */
static void
Calculate_Component_Max_Costs_And_Min_Recurrence_II(
  CG_LOOP_SCC  *scc
)
{
  INT         i, j, k;
  COST_TABLE *costs;
  INT         n = CG_LOOP_SCC_member_count(scc);

  MEM_POOL_Push(&MEM_local_nz_pool); /* Path table junk locally allocated */

  /* Allocate a NxN array of ARC_LISTs.  costs[i][j] represents the
   * maximum cost paths from the i-th to the j-th element of the SCC.
   * The OPs' _scc_index field is used to determine their sequence id
   * within the SCC.
   */
  costs = COST_TABLE_Alloc(n);

  Calculate_Component_Direct_Costs_And_Min_Recurrence_II(scc,costs);
  /* Floyd's algorithm.
   */
  for ( k = 0 ; k < n; ++k ) {
    for ( i = 0; i < n; ++i ) {
      for ( j = 0; j < n; ++j ) {
	INT32 min_ii;

	/*
	 * Don't bother checking self paths through self.
	 * These are guaranteed to not be maximal.
	 */
	if (i == j && j == k) continue;

	min_ii = CG_LOOP_min_ii;

        /* Consider the existing costs of paths from i to j and the
         * costs of paths from i to j via k.  Filter out those that
         * are cannot be maximal (given a min II) and replace the
         * costs of paths from i to j with the result.
	 */
        Add_Maximal_Costs(COST_TABLE_cost_vec(costs,i,j),
                          COST_TABLE_cost_vec(costs,i,k),
                          COST_TABLE_cost_vec(costs,k,j),
			  min_ii);

        /* Check to see whether we now have a self path with cost
         * greater than CG_LOOP_min_ii and/or CG_LOOP_SCC_rec_mii(scc).  If
	 * so, we have a new CG_LOOP_min_ii an/or CG_LOOP_SCC_rec_mii(scc).
         */
        Maximize_Min_II(scc,
			COST_TABLE_cost_vec(costs,i,j),
                        COST_TABLE_cost_vec(costs,j,i));
      }
    }
  }

  /* Use table to initialize the _scc_ancestors and _scc_descendents
   * fields of each member OP.
   */
  for ( i = 0; i < n; ++i ) {
    OP *iop = CG_LOOP_SCC_member(scc,i);

    for ( j = 0; j < n; ++j ) {
      OP       *jop       = CG_LOOP_SCC_member(scc,j);
      COST_VEC *cv        = COST_TABLE_cost_vec(costs,i,j);
      INT       cv_length = COST_VEC_length(cv);

      for ( k = 0; k < cv_length; ++k ) {
        CG_LOOP_Add_SCC_Arc(iop,jop,COST_VEC_omega(cv,k),
                            COST_VEC_latency(cv,k));
      }
    }
  }

  MEM_POOL_Pop(&MEM_local_nz_pool);
}


/* ====================================================================
 *
 *  Calculate_Self_Recurrence_Min_II
 *
 *  'Scc' is a single node strongly connected component.  Check to see
 *  if the single OP is a self recurrence, and if so make sure Min_II
 *  is large enough to contain it.
 *
 * ====================================================================
 */
static void
Calculate_Self_Recurrence_Min_II(
  CG_LOOP_SCC*  scc,
  BOOL ignore_non_def_mem_deps
)
{
  ARC_LIST *succs;
  OP       *self = CG_LOOP_SCC_member(scc,0);

  /* Loop overhead self recurrence arcs don't increase the MII,
   * since we can always replicate our way out of this problem.
   */
  if ( OP_loh(self) )
    return;

  for ( succs = OP_succs(CG_LOOP_SCC_member(scc,0));
        succs != NULL;
        succs = ARC_LIST_rest(succs)
  ) {
    ARC *succ_arc = ARC_LIST_first(succs);

    if ( ARC_succ(succ_arc) == ARC_pred(succ_arc) &&
	 (!ignore_non_def_mem_deps || !ARC_is_mem(succ_arc) ||
	  ARC_is_definite(succ_arc))
	 ) {
      
      Is_True(((INT) ARC_omega(succ_arc)) > 0,  /* Cast for lint */
              ("Unexpected non-positive omega in self recurrence."));

      Set_CG_LOOP_SCC_rec_mii(scc,Max(CG_LOOP_SCC_rec_mii(scc),
				  Ceiling_Divide(CGTARG_ARC_Sched_Latency(succ_arc),
					       ARC_omega(succ_arc))));
      CG_LOOP_rec_min_ii = Max(CG_LOOP_rec_min_ii, CG_LOOP_SCC_rec_mii(scc));
      CG_LOOP_min_ii = Max(CG_LOOP_min_ii, CG_LOOP_SCC_rec_mii(scc));
    }
  }
}

/* ====================================================================
 *
 *  CG_LOOP_Calculate_Min_Recurrence_II
 *
 *  See interface description.
 *
 * ====================================================================
 */
void
CG_LOOP_Calculate_Min_Recurrence_II(BB *loop_body, BOOL ignore_non_def_mem_deps)
{
  CG_LOOP_SCC *scc;
  CG_LOOP_SCC *scc_fp;

  /*  For each multinode SCC, find its max cost paths, maxing CG_LOOP_min_ii
   *  as we go.  This is Monica Lam's method.  See "A Systolic Array
   *  Optimizing Compiler", page 109.  (In the Kluwer hardback.)
   */

  for ( scc = CG_LOOP_SCC_Vec, scc_fp = scc + CG_LOOP_SCC_Count;
        scc < scc_fp;
        ++scc
  ) {

    Trace_Begin_Min_II_Scc(loop_body,scc);

    Set_CG_LOOP_SCC_rec_mii(scc,0);

    if ( CG_LOOP_SCC_member_count(scc) > 1 )
      Calculate_Component_Max_Costs_And_Min_Recurrence_II(scc);
    else
      Calculate_Self_Recurrence_Min_II(scc, ignore_non_def_mem_deps);

    Trace_End_Min_II_Scc();
  }
}

/* ====================================================================
 *
 *  CG_LOOP_Calculate_Min_Resource_II
 *
 *  See interface description
 *
 * ====================================================================
 */
void
CG_LOOP_Calculate_Min_Resource_II(
  BB *loop_body,
  OP_LIST *loop_overhead,
  BOOL ignore_prefetches,
  BOOL ignore_prefetch_strides
)
{
  INT32 special_min_ii;

  Trace_Begin_Min_II(loop_body);

  MEM_POOL_Push(&MEM_local_nz_pool);

  Calculate_Min_Resource_II(loop_body, loop_overhead,
			    ignore_prefetches, ignore_prefetch_strides);

  special_min_ii = CGTARG_Special_Min_II(loop_body, trace_mii);
  CG_LOOP_min_ii = MAX(CG_LOOP_min_ii, special_min_ii);

  CG_LOOP_res_min_ii = CG_LOOP_min_ii;

  MEM_POOL_Pop(&MEM_local_nz_pool);

  Trace_End_Min_II();
}

/* ====================================================================
 *
 *  CG_LOOP_Calculate_Max_Costs_And_Min_II
 *
 *  See interface description
 *
 * ====================================================================
 */
void
CG_LOOP_Calculate_Max_Costs_And_Min_II(
  BB *loop_body,
  OP_LIST *loop_overhead,
  BOOL ignore_prefetches,
  BOOL ignore_prefetch_strides,
  BOOL ignore_non_def_mem_deps
)
{
  INT32 special_min_ii;

  Trace_Begin_Min_II(loop_body);

  MEM_POOL_Push(&MEM_local_nz_pool);

  Calculate_Min_Resource_II(loop_body, loop_overhead,
			    ignore_prefetches, ignore_prefetch_strides);
  special_min_ii = CGTARG_Special_Min_II(loop_body, trace_mii);
  CG_LOOP_min_ii = MAX(CG_LOOP_min_ii, special_min_ii);

  CG_LOOP_res_min_ii = CG_LOOP_min_ii;
  CG_LOOP_Calculate_Min_Recurrence_II(loop_body, ignore_non_def_mem_deps);

  MEM_POOL_Pop(&MEM_local_nz_pool);

  Trace_End_Min_II();
}
