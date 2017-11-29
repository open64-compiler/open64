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


#ifndef MFMC_H_INCLUDED
#define MFMC_H_INCLUDED "mfmc.h"
/* ======================================================================
 *
 * Module: mfmc.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:37-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.mfmc.h $
 *
 * Revision history:
 * 17-Jul-96 - Original version
 *
 * Description:
 *
 * This package provides a functional interface to a fast
 * implementation of one of the best practical algorithms to solve
 * maximum flow and minimum s-t cut problems.
 *
 * The algorithm works in the push-relabel framework of Goldberg, and
 * uses the maximum distance (i.e., highest level) node selection
 * strategy, along with global relabeling and gap relabeling
 * heuristics. This combination of techniques is the best known in
 * practice for a wide variety of classes of maximum flow problem
 * instances.
 *
 * The interface functions visible to users are divided into three
 * classes: setup, query, and other.
 *
 * Setup functions (mfmc_setup.c)
 * ===============
 * Setup functions are called by the user to describe the particular
 * flow/cut problem instance.
 *
 *	MFMC_HANDLE MFMC_Init_problem(MEM_POOL *mem_pool,
 *				      BOOL      trace,
 *				      INT32     n,
 *				      INT32     m,
 *				      INT32     n_sources,
 *				      INT32     n_sinks)
 *
 *	    Perform initialization to begin setting up a particular
 *	    problem instance, and return a handle that will refer to
 *	    the problem instance in subsequent MFMC interface function
 *	    calls.
 *
 *		mem_pool  - a pointer to a MEM_POOL from which the
 *			    MFMC package should allocate memory
 *			    required to solve the problem.
 *		trace     - (for future use) whether or not to print
 *			    tracing information during problem setup
 *			    and solution. Currently no tracing output
 *			    is implemented.
 *		n         - the number of nodes in the problem
 *			    instance. The nodes of the problem
 *			    instance will be referred to by integers
 *			    in the range [0 .. n-1].
 *		m         - the number of arcs (directed edges) in the
 *			    problem instance.
 *		n_sources - the number of source nodes in the problem
 *			    instance. This value must fall between 1
 *			    and n-1, inclusive.
 *		n_sinks   - the number of sink nodes in the problem
 *			    instance. This value must fall between 1
 *			    and n-1, inclusive.
 *
 *	    This routine returns NULL if and only if it is unable to
 *	    allocate some of the resources required to solve the
 *	    problem. At present, no MEM_POOL resources are allocated
 *	    by the MFMC package outside this function, but please
 *	    don't assume this will always be the case.
 *
 *	MFMC_EC MFMC_Set_source(MFMC_HANDLE handle,
 *				INT32       s)
 *
 *		handle - the handle for a particular flow/cut problem
 *		s      - a source node in the problem referred to by
 *			 handle
 *
 *	    This routine tells the MFMC package that node s is a
 *	    source in the problem referred to by handle.
 *
 *	    Possible error returns:
 *		MFMC_NO_ERROR    - all is well
 *		MFMC_S_T_OVERLAP - node s has already been mentioned
 *				   along with handle in a call to
 *				   MFMC_Set_source or MFMC_Set_sink.
 *
 *	MFMC_EC MFMC_Set_sink(MFMC_HANDLE handle,
 *			      INT32	  t)
 *
 *		handle - the handle for a particular flow/cut problem
 *		t      - a sink node in the problem referred to by
 *			 handle
 *
 *	    This routine tells the MFMC package that node t is a sink
 *	    in the problem referred to by handle.
 *
 *	    Possible error returns:
 *		same as for MFMC_Set_source
 *
 *	MFMC_EC MFMC_Place_arc(MFMC_HANDLE      handle,
 *			       INT32            tail,
 *			       INT32	        head,
 *			       INT64	        lower_cap,
 *			       INT64	        upper_cap,
 *			       MFMC_ARC_HANDLE *arc_handle)
 *
 *	    Place an arc from node tail to node head; the arc has
 *	    forward capacity equal to upper_cap, and backward capacity
 *	    equal to -lower_cap. In a future refinement of the
 *	    implementation, if arc_handle is non-null MFMC_Place_arc
 *	    will store a unique identifier for the arc in *arc_handle
 *	    so it can be referred to later in query functions.
 *
 *		handle     - the handle for a particular flow/cut
 *			     problem
 *		tail       - the origin of the arc being placed
 *		head       - the destination of the arc being placed
 *		lower_cap  - lower bound on the flow through the arc;
 *			     in the present implementation this value
 *			     must be nonpositive
 *		upper_cap  - upper bound on the flow through the arc;
 *			     in the present implementation this value
 *			     must be nonnegative
 *		arc_handle - if non-NULL, a pointer to where a future
 *			     implementation will store a unique
 *			     identifier for the placed arc; must be
 *			     NULL in the present implementation
 *
 *	    Possible error returns:
 *		MFMC_NO_ERROR        - all is well
 *		MFMC_INFEASIBLE      - upper_cap < lower_cap
 *		MFMC_ZERO_INFEASIBLE - zero flow is infeasible; this
 *				       will not be an error in a
 *				       future version
 *		MFMC_BAD_ARC_COUNT   - more arcs have been placed than
 *				       were promised in the call to
 *				       MFMC_Init_problem
 *		MFMC_NOT_IMPLEMENTED - arc_handle is non-NULL; arc
 *				       handles are not currently
 *				       implemented
 *
 *	MFMC_EC MFMC_Solve_problem(MFMC_HANDLE handle)
 *
 *		handle - the handle for a particular flow/cut problem
 *
 *	    This routine tells the MFMC package that the problem setup
 *	    phase is finished for handle, and that the package should
 *	    proceed with finding a max flow/min cut pair.
 *
 *	    Possible error returns:
 *		MFMC_NO_ERROR      - all is well
 *		MFMC_UNSEEN_S_T    - the number of sources or sinks
 *				     set via calls to MFMC_Set_source
 *				     and/or MFMC_Set_sink does not
 *				     match the number promised in the
 *				     call to MFMC_Init_problem
 *		MFMC_BAD_ARC_COUNT - the number of arcs placed via
 *				     calls to MFMC_Place_arc does not
 *				     match the number promised in the
 *				     call to MFMC_Init_problem
 *
 * Query functions (mfmc_query.c)
 * ===============
 * Query functions are called by the user when a problem has been
 * solved and the user wants to learn details of the solution and/or
 * of the solution process.
 *
 *	INT64 MFMC_Objective_value(MFMC_HANDLE handle)
 *
 *	    Returns the value of the minimum cut (equal to the value
 *	    of the maximum flow) in the problem referred to by handle.
 *
 *	BOOL MFMC_Min_cut_lhs(MFMC_HANDLE handle,
 *			      INT32 i)
 *
 *	    Returns TRUE iff node i is on the left-hand (i.e., source)
 *	    side of the minimum cut found by the algorithm.
 *
 *	INT64 MFMC_Max_flow_arc_flow(MFMC_HANDLE handle,
 *				     MFMC_ARC_HANDLE arc_handle)
 *
 *	    Returns the value of the flow on the arc specified by
 *	    arc_handle in the maximum flow found by the
 *	    algorithm. This function is not implemented yet, and 
 *	    always asserts FALSE in the present version.
 *
 *	MFMC_STATS *MFMC_Stats(MFMC_HANDLE handle)
 *
 *	    Returns a pointer to a structure describing statistics
 *	    collected during the process of solving the problem
 *	    referred to by handle. Typically these statistics count
 *	    either process time or the number of times particular
 *	    operations were executed.
 *
 * ======================================================================
 */
#include "defs.h"
#include "mempool.h"
#include "errors.h"

typedef struct mfmc_handle *MFMC_HANDLE;
typedef struct mfmc_arc    *MFMC_ARC_HANDLE;

typedef struct mfmc_stats {
  INT32		n_push;
  INT32		n_rel;
  INT32		n_up;
  INT32		n_gap;
  INT32		n_gnode;
  float		time;
} MFMC_STATS;

typedef enum {
  MFMC_NO_ERROR,		/* Everything OK so far. */
  MFMC_S_T_OVERLAP,		/* Source and sink sets intersect, or
				 * some source/sink set twice.
				 */
  MFMC_INFEASIBLE,		/* Upper capacity < lower capacity. */
  MFMC_ZERO_INFEASIBLE,		/* (Lower capacity > 0 or
				 *  upper capacity < 0).
				 * Relax this restriction later?
				 */
  MFMC_UNSEEN_S_T,		/* Promised number of sources/sinks
				 * doesn't match the number actually
				 * set.
				 */
  MFMC_BAD_ARC_COUNT,		/* Promised number of arcs doesn't
				 * match the number actually placed.
				 */
  MFMC_NOT_IMPLEMENTED		/* User asked for something legitimate
				 * that we're too stupid or lazy to do.
				 */
} MFMC_EC;

#ifdef __cplusplus
extern "C" {
#endif

/* Setup interface functions */

MFMC_HANDLE	MFMC_Init_problem(MEM_POOL *, BOOL, INT32, INT32,
				  INT32, INT32);
MFMC_EC		MFMC_Place_arc(MFMC_HANDLE, INT32, INT32,
			       INT64, INT64, MFMC_ARC_HANDLE *);
MFMC_EC		MFMC_Set_source(MFMC_HANDLE, INT32);
MFMC_EC		MFMC_Set_sink(MFMC_HANDLE, INT32);
MFMC_EC		MFMC_Solve_problem(MFMC_HANDLE);

/* Query interface functions */

extern BOOL		MFMC_Min_cut_lhs(MFMC_HANDLE, INT32);
inline INT64	MFMC_Max_flow_arc_flow(MFMC_HANDLE h, MFMC_ARC_HANDLE ah)
{
  FmtAssert(FALSE, ("MFMC_Max_flow_arc_flow: arc handles not implemented"));
  /*NOTREACHED*/
}

INT64
MFMC_Objective_value(MFMC_HANDLE);

/* Other interface functions */
extern char *MFMC_msgs[];

inline void
MFMC_Print_error(FILE *f, MFMC_EC err)
{
  (void) fprintf(f, "%s\n", MFMC_msgs[err]);
}

MFMC_STATS *
MFMC_Stats(MFMC_HANDLE);

#ifdef __cplusplus
}
#endif
#endif
