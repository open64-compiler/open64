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

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <alloca.h>
#include "pu_info.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "config.h"
#include "debug.h"
#include "glob.h"
#include "lnopt_main.h"
#include "reduc.h" 
#include "wind_down.h"
#include "snl_utils.h" 

//-----------------------------------------------------------------------
// Wind-down code is used by register tiling and cache tiling.
//
// static WN* Wind_Down(WN* loop, INT est_num_iters, BOOL cache_winddown)
//
// This makes a copy of loop, changes the lower bound of newloop to the index
// variable.  Does not change the upper bounds of the original loop at all,
// so other code must do this.
// Draws all the dependence arcs quickly and conservatively, assuming that
// the second loop only starts being executed when the first loop never
// will.  Return copy.  Don't update access vectors!  Update DU.
// If not a cache_winddown, the a register winddown.  For annotation
// purposes.
//
// Returns NULL on failure.
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: Wind_Down_Dep_V
// FUNCTION: Create the vertices of the dependence graph of the wind down
//  loop 'copy' given those of the original loop 'orig'.  Use the 'hash_
//  table' to collect pairs corresponding vertices from the original loop 
//  and wind down loop. 
//-----------------------------------------------------------------------

extern void Wind_Down_Dep_V(WN* orig, 
			    WN* copy,
                            HASH_TABLE<VINDEX16,VINDEX16>* hash_table,
                            ARRAY_DIRECTED_GRAPH16* dg)
{
  if (orig == NULL) {
    FmtAssert(copy == NULL, ("Bad call to Wind_Down_Dep_V()"));
    return;
  }

  FmtAssert(copy,
            ("Copy null for non-null orig, opcode %d", WN_opcode(orig)));
  FmtAssert(WN_opcode(copy) == WN_opcode(orig),
            ("opcode orig = %d, opcode copy = %d",
             WN_opcode(copy), WN_opcode(orig)));

  OPCODE        op = WN_opcode(orig);

  switch (op) {
   case OPC_BLOCK:
    {
      WN *orig_kid = WN_first(orig);
      WN *copy_kid = WN_first(copy);
      while (orig_kid) {
        Wind_Down_Dep_V(orig_kid, copy_kid, hash_table, dg);
        orig_kid = WN_next(orig_kid);
        copy_kid = WN_next(copy_kid);
      }
    }
    break;

   default:
    {
      if (OPCODE_is_load(op) || OPCODE_is_store(op) || OPCODE_is_call(op)) {
        VINDEX16 origv = dg->Get_Vertex(orig);
        if (origv) {
          VINDEX16 copyv = dg->Get_Vertex(copy);
          FmtAssert(copyv, ("Missing corresponding vertex"));
          hash_table->Enter(origv, copyv);
        }
      }
      for (INT kidno = 0; kidno < WN_kid_count(orig); kidno++)
        Wind_Down_Dep_V(WN_kid(orig,kidno),WN_kid(copy,kidno),hash_table,dg);
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Wind_Down_Shorten 
// FUNCTION: Shorten the original dependence 'orig_dv' so that it has 
//   'num_dim' used dimensions and 'num_unused_dim' unused dimensions. 
//   The BOOL 'allow_zerovec' is TRUE if we allow for the loop-independent
//   dependence of all zeros.  
// NOTE: This is done to create cross dependence arcs between the ori-
//   ginal loop and the wind down loop. 
//-----------------------------------------------------------------------

static DEPV_ARRAY* Wind_Down_Shorten(DEPV_ARRAY* orig_dv, 
				     INT num_dim,
                                     INT num_unused_dim, 
				     BOOL allow_zerovec)
{
  if (num_dim <= 0)
    return NULL;

  DEPV_ARRAY* copy_dv = Create_DEPV_ARRAY(orig_dv->Num_Vec(),
                                          num_dim, num_unused_dim,
                                          &LNO_default_pool);
  INT vnew = 0;

  for (INT v = 0; v < orig_dv->Num_Vec(); v++) {
    DEPV*       odv = orig_dv->Depv(v);
    DEPV*       ndv = copy_dv->Depv(vnew);

    // For a zero vector, we know that the num_dim dimension must be
    // greater than zero, or else no way.  In fact, typically that dimension
    // must be positive, but not always, so oh well.

    BOOL zerovec = TRUE;
    for (INT dd = 0; dd < num_dim; dd++) {
      DEPV_Dep(ndv, dd) = DEPV_Dep(odv, dd);
      zerovec = zerovec && DEP_Direction(DEPV_Dep(ndv,dd)) == DIR_EQ;
    }

    if (zerovec) {
      if (allow_zerovec) {
        switch (DEP_Direction(DEPV_Dep(odv, num_dim))) {
         case DIR_POS:
         case DIR_POSEQ:
          vnew++;
        }
      }
    }
    else
      vnew++;
  }

  if (vnew == 0)
    return NULL;
  else if (vnew == copy_dv->Num_Vec())
    return copy_dv;
  else {
    DEPV_ARRAY* copy2_dv = Create_DEPV_ARRAY(vnew, num_dim, num_unused_dim,
                                             &LNO_default_pool);
    for (INT v = 0; v < vnew; v++) {
      DEPV*     ndv = copy_dv->Depv(v);
      DEPV*     n2dv = copy2_dv->Depv(v);
      for (INT dd = 0; dd < num_dim; dd++)
        DEPV_Dep(n2dv, dd) = DEPV_Dep(ndv, dd);
    }
    return copy2_dv;
  }
}

//-----------------------------------------------------------------------
// NAME: Wind_Down_Dep_E
// FUNCTION: Create the edges of the dependence graph for the original 
//   loop and wind down loop which go between the pairs of corresponding 
//   vertices in the 'hash_table'.  The original loop for the which the 
//   wind down loop was created has depth 'd'.  Returns TRUE if all the 
//   edges were put in successfully, FALSE otherwise.
//-----------------------------------------------------------------------

extern BOOL Wind_Down_Dep_E(HASH_TABLE<VINDEX16,VINDEX16>* hash_table,
                            INT d, 
			    ARRAY_DIRECTED_GRAPH16* dg)
{
  // which edges already exist
  HASH_TABLE_ITER<VINDEX16,VINDEX16> htit(hash_table);
  VINDEX16 origv;
  VINDEX16 copyv;

  DYN_ARRAY<EINDEX16>  interesting_edges(&LNO_local_pool);
  while (htit.Step(&origv, &copyv)) {
    FmtAssert(origv && copyv, ("broken vertex table %d %d", origv, copyv));
    for (EINDEX16 edge = dg->Get_Out_Edge(origv); edge;
         edge = dg->Get_Next_Out_Edge(edge)) {

      VINDEX16 orig_sinkv = dg->Get_Sink(edge);
      VINDEX16 new_sinkv = hash_table->Find(orig_sinkv);

      // it goes somewhere outside -- ignore
      if (new_sinkv) {
        INT idx = interesting_edges.Newidx();
        interesting_edges[idx] = edge;
      }
    }
  }

  for (INT i = interesting_edges.Elements() - 1; i >= 0; i--) {
    EINDEX16 edge = interesting_edges[i];
    VINDEX16 orig_sinkv = dg->Get_Sink(edge);
    VINDEX16 new_sinkv = hash_table->Find(orig_sinkv);
    origv = dg->Get_Source(edge);
    copyv = hash_table->Find(origv);

    FmtAssert(new_sinkv, ("impossible"));

    // add edge from from origv to new_sinkv, and from copyv to orig_sinkv

    DEPV_ARRAY* orig_dv = dg->Depv_Array(edge);
    Is_True(orig_dv, ("pro-blem: edge has no arc"));
    INT num_dim = orig_dv->Num_Dim();
    INT num_unused_dim = orig_dv->Num_Unused_Dim();
    FmtAssert(num_unused_dim <= d, ("Bug1 in Wind_Down_Dep_E"));
    FmtAssert(d < num_unused_dim + num_dim,
              ("Bug2 in Wind_Down_Dep_E: %d,%d,%d",
               d, num_unused_dim, num_dim));
    num_dim = d - num_unused_dim;

    if (num_dim) {
      DEPV_ARRAY* copy_dv;
      copy_dv = Wind_Down_Shorten(orig_dv, num_dim, num_unused_dim, TRUE);
      if (copy_dv) {
        if (!dg->Add_Edge(origv, new_sinkv, copy_dv))
          return FALSE;
      }

      copy_dv = Wind_Down_Shorten(orig_dv, num_dim, num_unused_dim, FALSE);
      if (copy_dv) {
        if (!dg->Add_Edge(copyv, orig_sinkv, copy_dv))
          return FALSE;
      }
    }
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Set_Winddown_Annotations_B
// FUNCTION: Set the winddown annotations for all of the loops in the 
//   tree rooted at 'newbody'.  If this is being done for a cache wind 
//   down loop 'cache_winddown' should be TRUE, otherwise it is being 
//   done for a register wind down loop.  Set the register usage as 
//   given by 'est_register_usage'. 
//-----------------------------------------------------------------------

static void Set_Winddown_Annotations_B(WN* newbody,
                                       BOOL cache_winddown,
                                       EST_REGISTER_USAGE est_register_usage)
{
  FmtAssert(WN_opcode(newbody) == OPC_BLOCK, ("Bad newbody"));

  for (WN* wn = WN_first(newbody); wn; wn = WN_next(wn)) {
    switch (WN_opcode(wn)) {
     case OPC_IF:
      Set_Winddown_Annotations_B(WN_then(wn), cache_winddown,
                                 est_register_usage);
      Set_Winddown_Annotations_B(WN_else(wn), cache_winddown,
                                 est_register_usage);
      break;
     case OPC_DO_LOOP:
      Set_Winddown_Annotations(wn, cache_winddown,
                                 est_register_usage, FALSE);
      break;
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      Set_Winddown_Annotations_B(WN_while_body(wn), cache_winddown,
                                 est_register_usage);
     default:
      break;
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Set_Winddown_Annotations 
// FUNCTION: Set the annotations of the wind down loop 'newloop'.  If this
//   is a cache winddown loop 'cache_winddown' is TRUE, otherwise it is a 
//   register winddown loop.  The estimated register usage of the new loop
//   is given by 'est_register_usage'.  If this is not an innermost loop, 
//   'outer' is TRUE, otherwise it is FALSE. 
//-----------------------------------------------------------------------

extern void Set_Winddown_Annotations(WN* newloop,
                                     BOOL cache_winddown,
                                     EST_REGISTER_USAGE  est_register_usage,
                                     BOOL outer)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(newloop);

  if (outer) {
    if (cache_winddown)
      dli->Set_Cache_Winddown();
    else
      dli->Set_Register_Winddown();
  }
  else {
    if (cache_winddown)
      dli->Set_In_Cache_Winddown();
    else
      dli->Set_In_Register_Winddown();
  }

  if (dli->Is_Inner)
    dli->Est_Register_Usage = est_register_usage;
  else
    Set_Winddown_Annotations_B(WN_do_body(newloop), cache_winddown,
                               est_register_usage);
}

//-----------------------------------------------------------------------
// NAME: Wind_Down  
// FUNCTION: Create a wind down loop for the loop 'loop'.  If 'cache_wind-
//   down' is TRUE, annotate it as a cache winddown loop, otherwise, anno-
//   tate it as a register winddown loop.  Mark the windown as having the 
//   'est_num_iterations' iterations.  Assign it an estimated register 
//   usage given by 'est_register_usage'. 
// NOTES: 
//   This routine takes a loop of the form: 
//     do i = lb, ub, step 
//       code(i)  
//     end do 
//   and follows it with a loop of the form: 
//     do i = i, ub, step 
//       code(i) 
//     end do 
//   It draws dependence arcs between the two loops quickly and conserva-
//   tively, assuming that the second loop only starts being executed when 
//   the first loop never will.  Returns the copy.  Annotates the loop as
//   either a register winddown or cache winddown loop. 
//     This routine did not originally update the access vectors, but I 
//   modified it to do that in case someone decides to call it as a 
//   stand-alone routine. 
//     We currently only use this routine to do register winddowns.  The 
//   cache winddowns are done with the tile-splitting code in split_tiles.
//   cxx, and are annotated as such.  The reason why there are so many 
//   external routines in this file is that the dependence and annotation
//   routines are used by the tile splitting routines as well as by this 
//   one. 
//-----------------------------------------------------------------------

extern WN* Wind_Down(WN* loop, 
		     INT est_num_iterations, 
		     BOOL cache_winddown,
                     EST_REGISTER_USAGE est_register_usage)
{
  const INT bufsz = 64;
  char      buf[bufsz];
  INT       bufcnt;

  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;

  WN* newloop = LWN_Copy_Tree(loop, TRUE, LNO_Info_Map);
  WN* fake_unroll_bodies[2];
  fake_unroll_bodies[0] = loop;
  fake_unroll_bodies[1] = newloop;
  if (red_manager) red_manager->Unroll_Update(fake_unroll_bodies, 2);
  Unrolled_DU_Update(fake_unroll_bodies, 2, Do_Loop_Depth(loop)-1, TRUE, FALSE);  if (dg->Add_Deps_To_Copy_Block(loop, newloop)) {
    // Update dependences.  All dependences from the outside world to newloop
    // and from newloop to newloop have been updated because of
    // Add_Deps_To_Copy_Block.  So the only remaining work is to add arcs from
    // arrays in loop to arrays in newloop.  These routines are modified
    // from Add_Deps_To_Copy_Block().

    // No need to push the LNO_local_pool ... this is a static function,
    // so I know that all its callers push and pop this pool.  In fact,
    // they freeze it.

    HASH_TABLE<VINDEX16,VINDEX16>
    hash_table(MIN(dg->Get_Vertex_Count(),512),&LNO_local_pool);

    Wind_Down_Dep_V(loop, newloop, &hash_table, dg);

    if (!Wind_Down_Dep_E(&hash_table, Do_Depth(loop), dg)) {
      SNL_DEBUG0(0, "Wind_Down_Dep_E() failed -- continuing");
      LWN_Update_Dg_Delete_Tree(newloop, dg);
      LNO_Erase_Dg_From_Here_In(newloop, dg);
      Unmapped_Vertices_Here_Out(LWN_Get_Parent(loop));
    }
  } else {
    SNL_DEBUG0(0, "Add_Deps_To_Copy_Block() failed -- continuing");
    LWN_Update_Dg_Delete_Tree(newloop, dg);
    LNO_Erase_Dg_From_Here_In(newloop, dg);
    Unmapped_Vertices_Here_Out(LWN_Get_Parent(loop));
  }

  LWN_Insert_Block_After(LWN_Get_Parent(loop), loop, newloop);
  ST* st0 = WN_st(WN_index(loop));
  WN_OFFSET offset0 = WN_offset(WN_index(loop));
  TYPE_ID   wtype0 = WN_desc(WN_start(loop));
  bufcnt = sprintf(buf, "$wd_");
  (SYMBOL(WN_index(loop))).Name(buf+bufcnt, bufsz-bufcnt);
  Replace_Symbol(newloop, SYMBOL(st0, offset0, wtype0),
                 Create_Preg_Symbol(buf, wtype0), NULL, newloop);
  Fix_Do_Du_Info(newloop, NULL, TRUE, NULL, 1);

  WN* newbegin = WN_start(newloop);
  LWN_Delete_Tree(WN_kid0(newbegin));
  WN_kid0(newbegin) = LWN_CreateLdid(OPCODE_make_op(OPR_LDID, wtype0, wtype0),
                                     WN_start(loop));
  LWN_Set_Parent(WN_kid0(newbegin), newbegin);
  Fix_Do_Du_Info(newbegin, NULL, FALSE, loop, 0);

  DO_LOOP_INFO* dli = Get_Do_Loop_Info(newloop);
  if (Cur_PU_Feedback && dli->Est_Num_Iterations>0) {
    INT32 org_count = WN_MAP32_Get(WN_MAP_FEEDBACK, WN_start(loop));
    float ratio = ((float) est_num_iterations) 
      / ((float) dli->Est_Num_Iterations);
    LWN_Scale_Frequency(WN_step(newloop), ratio);
    LWN_Scale_Frequency_Tree(WN_do_body(newloop), ratio);
  }
  dli->Est_Num_Iterations = est_num_iterations;
  dli->Num_Iterations_Symbolic = FALSE;
  dli->Num_Iterations_Profile = FALSE;
  dli->Is_Ivdep = Get_Do_Loop_Info(loop)->Is_Ivdep;
  dli->Is_Concurrent_Call = Get_Do_Loop_Info(loop)->Is_Concurrent_Call;
  dli->Concurrent_Directive = Get_Do_Loop_Info(loop)->Concurrent_Directive;
  dli->LB->Too_Messy = TRUE;
  if (cache_winddown)
    dli->Set_Cache_Winddown();
  else
    dli->Set_Register_Winddown();

  Set_Winddown_Annotations(newloop, cache_winddown, est_register_usage, TRUE);

  DOLOOP_STACK shortstack(&LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(newloop), &shortstack);
  LNO_Build_Access(newloop, &shortstack, &LNO_default_pool);

  return newloop;
}

