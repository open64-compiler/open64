/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


// -*-C++-*-

/**
***			Fission Utilities
***			-----------------
***
*** Description:
***
***         This file contains all fission utilities available to LNO.
***
*** Exported types:
***
***     FF_STMT_NODE
***
***         Node type for nodes in a statement list (see below and
***         be/com/cxx_base.h).
***
***         void Set_Stmt(WN *stmt)
***
***             Set the statement in the FF_STMT_NODE to be 'stmt'.
***
***         WN *Get_Stmt() const
***
***             Get the statement in the FF_STMT_NODE.
***
***     FF_STMT_LIST
***
***         Statement list type (see also be/com/cxx_base.h).
***
***         void Append(WN *stmt, MEM_POOL *mpool)
***
***             Append 'stmt' to the list.
***             A new FF_STMT_NODE is created implicitly using 'mpool'.
***
***     FF_STMT_ITER
***
***         Statement list iterator type (see also be/com/cxx_base.h).
***
*** Exported symbols:
***
***     FISSION_BIT_STMT_REORDERING
***     FISSION_BIT_PERFECT_NESTING
***     FISSION_BIT_SCALAR_EXPANSION
***     FISSION_BIT_EXPRESSION_BASED
***     FISSION_BIT_TEST_ONLY
***
***         Individual bit vectors for opt_flags used in fission functions
***         (see below).
***
***     OPT_FLAG_STMT_SIMPLE
***     OPT_FLAG_STMT_PERFECT
***     OPT_FLAG_STMT_EXPANSION
***     OPT_FLAG_STMT_REORDERING
***     OPT_FLAG_STMT_REORDERING_PERFECT
***     OPT_FLAG_STMT_BEST
***
***         Compositions of the simple bit vectors, more readable.
***
***                             Expr.   Perf.   Scalar   Stmt.Test
***                             Based   Nest.   Expan.   Reord	
***
***     simple
***     perfect                         X
***     expansion                               X
***     reordering_perfect              X               X
***     best                            X       X       X
***
*** Exported functions:
***
***     void Separate(WN* in_loop,WN* in_stmt, UINT8 level, WN** new_loop);
***
***		Separate a given loop such that 'in_stmt' is the last
***		statement of the first loop. All statements after
***		'in_stmt' is in the second loop. 'in_loop' is changed
***		to include statements up to and include 'in_stmt'. 'new_loop'
***             is created using the same loop info as in 'in_loop'.
***             'level' specifies level of enclosing loops to be fused.
***             No legality test is done here and should be done with
***             other fission routines (see below).
***
***     UINT32 Fission_Test(WN* in_loop, mUINT16 opt_flag, UINT8 fission_level,
***             WN_MAP loop_map, FF_STMT_LIST *stl_1=NULL, 
***		FF_STMT_LIST *stl_2=NULL)
***
***             Given two statement lists of statements from in_loop,
***             this function tries to find a legal fission for in_loop
***             such that statements from different statement lists go to
***             different loops. Other options are specified with
***             'opt_flag'. A mapping should be allocated and provided
***             by the caller. No real fission is performed. Instead,
***             the fission results are stored in the mapping. That is,
***             statements in the first loop after fission will have a
***             be given a mapping value of 1. The statements in the
***             second loop will have mapping value 2, and so on.
***             'fission_level' specifies the level of enclosing loop
***             nest to be fissioned. The return value indicates how
***             many loops are created.
***
***             Note: the WN_MAP_Delete() to 'loop_map' has to be done
***             by the caller once the mapping is no longer used.
***
***     void Form_Loops(WN* in_loop, mUINT16 opt_flag, UINT8 fission_level,
***             FF_STMT_LIST *stl_1, FF_STMT_LIST *stl_2,
***             DYN_ARRAY<FF_STMT_LIST>& loop)
***             
***             Similar to the Fission_Test except that no map is used.
***             Instead, the statements in different loops are stored
***             in statement list array 'loop'. Number of loops created
***             can be found with loop.Lastidx()+1 (see cxx_base.cxx).
***
***     FISSION_FUSION_STATUS
***          Fission(WN* in_loop, mUINT16 opt_flag, UINT8 fission_level,
***             WN_MAP loop_map= WN_MAP_UNDEFINED, UINT32 total_loops= 0,
***             FF_STMT_LIST *stl_1=NULL, FF_STMT_LIST *stl_2=NULL)
***
***             Similar to the above test routine except that the fission
***             is actually done. If a mapping (<> WN_MAP_UNDEFINED) is 
***		provided, no legality test will be done. The mapping will 
***		guide the fission instead. Returns TRUE if more than one 
***		loops are formed.
***
***     FISSION_FUSION_STATUS
***          Fission(WN* in_loop, WN* stmt, UINT8 fission_level)
***
***             Fission 'in_loop' after the statement 'stmt' for
***             'fission_level' level of enclosing loops. Performs
***             necessary legality test and returns FALSE if not
***             fission-able.
***
***     FISSION_FUSION_STATUS
***          Fission(WN* in_loop, WN* stmt1, WN* stmt2, UINT8 fission_level,
***            BOOL parition_based)
***
***             Fission 'in_loop' such that the two statements 'stmt1'
***             and 'stmt2' are at different loop after fission. Performs
***             necessary legality test and returns FALSE if not
***             fission-able.
***
***     void Fission_Init()
***
***             Initialization routine for Fission. Set up 
***             and initialize the memory pool.
***             
***     WN* WN* WN_to_Stmt(WN* tree, MEM_POOL *mem_pool) 
***
***		Create and return a new statement which assigns the
***		evaluation of 'tree' to a new temp variable and use that
***		variable in place of the 'tree'. The new statement is
***		inserted right before the statement containing 'tree'.
***             The required memory is allocated from 'mem_pool'.
***		Note that the caller has to make sure that the clipping
***		and re-attaching of the 'tree' is safe from hazards of
***		data dependences.
***
**/

#ifndef fission_INCLUDED
#define fission_INCLUDED

#include "defs.h"
#include "cxx_base.h"
#include "cxx_memory.h"
#include "wn.h"
#include "dep_graph.h"
#include "ff_utils.h"

extern const mUINT16 FISSION_BIT_STMT_REORDERING;
extern const mUINT16 FISSION_BIT_PERFECT_NESTING;
extern const mUINT16 FISSION_BIT_SCALAR_EXPANSION;
extern const mUINT16 FISSION_BIT_EXPRESSION_BASED;
extern const mUINT16 FISSION_BIT_TEST_ONLY;

extern const mUINT16 OPT_FLAG_STMT_SIMPLE;
extern const mUINT16 OPT_FLAG_STMT_PERFECT;
extern const mUINT16 OPT_FLAG_STMT_EXPANSION;
extern const mUINT16 OPT_FLAG_STMT_REORDERING;
extern const mUINT16 OPT_FLAG_STMT_REORDERING_PERFECT;
extern const mUINT16 OPT_FLAG_STMT_BEST;

class FF_STMT_NODE: public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS( FF_STMT_NODE);
private:
  WN *_stmt;
public:
  FF_STMT_NODE() { _stmt = NULL; };
  FF_STMT_NODE(WN *stmt) { _stmt = stmt; };
  ~FF_STMT_NODE() {};
  void Set_Stmt(WN *stmt) { _stmt = stmt; }
  WN *Get_Stmt() const { return _stmt; }
};

class FF_STMT_LIST: public SLIST {
  DECLARE_SLIST_CLASS( FF_STMT_LIST, FF_STMT_NODE )
public:
  //FF_STMT_LIST() {};
  ~FF_STMT_LIST(void){};
  void Append(WN *stmt, MEM_POOL *mpool) {
    Append(CXX_NEW(FF_STMT_NODE(stmt), mpool));
  }
  void Prepend(WN *stmt, MEM_POOL *mpool) {
    Prepend(CXX_NEW(FF_STMT_NODE(stmt), mpool));
  }
};

class FF_STMT_ITER: public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( FF_STMT_ITER, FF_STMT_NODE, FF_STMT_LIST )
public:
  //FF_STMT_ITER() {};
  ~FF_STMT_ITER() {};
};

// separate two loops at in_stmt
// if (create_empty_loop==TRUE && in_stmt==NULL) 
//    new_loop will contain all the stmts of the in_loop
// if (create_empty_loop==TRUE && WN_next(in_stmt)==NULL)
//    new_loop will be an empty loop
extern void Separate(WN* in_loop,WN* in_stmt, UINT8 level, WN** new_loop,
		     BOOL create_empty_loop=FALSE);

// test fissioning in_loop based on two statement lists and opt_flag
extern UINT32 Fission_Test(WN* in_loop, mUINT16 opt_flag, UINT8 fission_level,
	WN_MAP loop_map, FF_STMT_LIST *stl_1=NULL, FF_STMT_LIST *stl_2=NULL);

// decompose in_loop based on two statement lists and put the
// statements of the decomposed loops into the array 'loop' without
// real fission
extern void Form_Loops(WN* in_loop, mUINT16 opt_flag, UINT8 fission_level,
        FF_STMT_LIST *stl_1, FF_STMT_LIST *stl_2,
	ARRAY_DIRECTED_GRAPH16* sdg, DYN_ARRAY<FF_STMT_LIST>& loop,
        MEM_POOL* pool);

// fission in_loop based on two statement lists
extern FISSION_FUSION_STATUS
    Fission(WN* in_loop, mUINT16 opt_flag, UINT8 fission_level,
        WN_MAP loop_map= WN_MAP_UNDEFINED, UINT32 total_loops= 0,
	FF_STMT_LIST *stl_1=NULL, FF_STMT_LIST *stl_2=NULL);

// fission in_loop after the statement 'stmt'
extern FISSION_FUSION_STATUS
    Fission(WN* in_loop, WN* stmt, UINT8 fission_level);

// fission in_loop such that the two statements 'stmt1' and 'stmt2'
// are at different loop after fission
extern FISSION_FUSION_STATUS
    Fission(WN* in_loop, WN* stmt1, WN* stmt2, UINT8 fission_level,
      BOOL partition_based);

// gather serial distribution boundary lists
extern void
    Get_Distribution_List(FF_STMT_LIST& stl_1, WN* in_loop, 
      UINT8 fission_level);

// fission init routine
extern void Fission_Init();

// fission finish routine
extern void Fission_Finish();

extern void Separate_And_Update(WN* in_loop,DYN_ARRAY<FF_STMT_LIST>& loop,
        UINT fission_level, BOOL rename_loop_var=TRUE);


#endif // fission_INCLUDED

