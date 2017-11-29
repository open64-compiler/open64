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


#ifndef MFMC_DEFS_H_INCLUDED
#define MFMC_DEFS_H_INCLUDED "mfmc_defs.h"

#include "mfmc.h"

typedef struct mfmc_arc MFMC_ARC;
typedef struct mfmc_node MFMC_NODE;
typedef struct mfmc_layer MFMC_LAYER;

typedef enum {
  SOURCE,
  USER_SOURCE,
  TRANSSHIPMENT,	/* the default */
  USER_SINK,
  SINK
} NODE_TYPE;

/* Possible TODO: Separate the MFMC_HANDLE structure into two
 * structures with different lifetimes. The primary handle structure
 * will live throughout, but there will be a secondary structure that
 * lives only during initialization and contains temporary information
 * required to set the problem up. After initialization, the secondary
 * structure can be freed. Also, maybe add an optional statistics
 * structure.
 */

struct mfmc_handle {
  MEM_POOL     *mem_pool;	/* where to get memory from */
  BOOL		trace;		/* whether to spit out tracing info */
  INT32		n;		/* number of nodes in the problem we
				 * solve
				 */
  INT32		m;		/* number of forward arcs in the
				 * problem we solve
				 */
  MFMC_NODE    *the_source;	/* the source in the problem we solve */
  MFMC_NODE    *the_sink;	/* the sink in the problem we solve */
  MFMC_NODE    *nodes;		/* array of nodes in our problem */
  MFMC_ARC     *arcs;		/* array of arcs in our problem */
  INT64	       *acap;		/* original arc capacities */
  INT64		max_cap;	/* maximum capacity magnitude in input */
  INT64		flow_value;	/* objective function value */

  /*  fields for algorithm overhead outside the problem description  */
  MFMC_NODE   **queue;		/* queue of nodes for (backward) BFS */
  MFMC_LAYER   *layers;		/* structure of distance-estimate
				 * levels for node selection
				 */

  /* -------------- fields for performance statistics -------------- */
  MFMC_STATS	stats;		/* block of statistics counts */

  /* --------------- fields below are only for setup --------------- */

  INT32		user_n;		/* number of nodes the user gives us */
  INT32		user_m;		/* number of arcs the user gives us */
  INT32		n_sources;	/* number of sources promised */
  INT32		n_sources_set;	/* number of sources set by the user */
  INT32		n_sinks;	/* number of sinks promised */
  INT32		n_sinks_set;	/* number of sinks set by the user */
  INT32	       *arc_tail;	/* temporary for use in arc ordering */
  INT32	       *arc_first;	/* temporary for use in arc ordering */
  MFMC_ARC     *arc_current;	/* temporary for use in arc ordering */
  INT32		pos_current;	/* temporary for use in arc ordering */
};

struct mfmc_arc
{
   INT64      r_cap;          /* residual capacity */
   MFMC_NODE *head;           /* head */
   MFMC_ARC  *reverse;        /* opposite arc */
   MFMC_ARC  *next;           /* next arc with the same tail;
			       * TODO: is this field needed?
			       */
};

struct mfmc_node
{
   MFMC_ARC  *first;           /* first outgoing arc */
   MFMC_ARC  *current;         /* current incident arc */
   INT64      excess;          /* excess of the node */
   INT32      rank;            /* distance from the sink */
   MFMC_NODE *nl_next;         /* next node in layer-list */
   MFMC_NODE *nl_prev;         /* previous node in layer-list */

   /* -------------- fields below are only for setup ------------- */
   NODE_TYPE  node_type;
};

struct mfmc_layer
{
   MFMC_NODE *push_first;      /* 1st node with positive excess */
   MFMC_NODE *trans_first;     /* 1st node with zero excess */
};

BOOL MFMC_Find_max_flow_min_cut(INT32,		/* n */
				MFMC_NODE *,	/* nodes */
				MFMC_ARC *,	/* arcs */
				INT64 *,	/* original capacities */
				MFMC_NODE *,	/* the source */
				MFMC_NODE *,	/* the sink */
				MFMC_NODE **,	/* BFS queue */
				MFMC_LAYER *,	/* distance levels */
				INT64,		/* flow upper bound */
				MFMC_STATS *,	/* statistics block */
				INT64 *);	/* flow value */
#endif
