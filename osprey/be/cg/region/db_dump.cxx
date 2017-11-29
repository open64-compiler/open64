/*
  Copyright (C) 2000-2003, Institute of Computing Technology, Chinese Academy of Sciences
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer. 

  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
 
/* ====================================================================
 * ====================================================================
 *
 * Module: db_dump.c
 * $Revision: 1.1 $
 * $Date: 2005/12/30 01:47:13 $
 * $Author: weitang $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/region/db_dump.cxx,v $
 *
 * Revision history:
 *
 * Description:
 *
 * Dump methods called in debugger.
 *
 * ====================================================================
 * ====================================================================
 */

#include <alloca.h>
#include <stdio.h>
#include <iterator>
#include "defs.h"
#include "bb.h"
#include "op.h"
#include "tracing.h"
#include "cg_region.h"
#include "cg_dep_graph.h"
#include "DaVinci.h"
#include "vt_region.h"
#include "dominate.h"
#include "ir_reader.h"
#include "register.h"
#include "tn.h"
#include "annotations.h"
#include "cg_loop_scc.h"

#if (__GNUC__ == 3)
using std::set;
#endif

class dp{
public:
  static void region_cfg(REGION *r);
  static void draw_bb(BB *bb);
  static void dump_bb(BB *bbg);
  static void dump_op(OP *opg);
  static void global_cfg();
  static BB* get_bb (mBB_NUM bb_idg);
  static OP* get_op (mBB_NUM bb_id, mUINT16 map_idxg);
  static void dump_succs(mBB_NUM bb_id, mUINT16 map_idxg);
  static void dump_succs_p(OP* op);
  static void dump_preds(mBB_NUM bb_id, mUINT16 map_idxg);
  static void dump_preds_p(OP* op);
  static void dump_pdom(mBB_NUM bb_idg);
  static void dump_tn_set (BS *setg);
  static void dump_bb_set(BB_SET* setg);
  static void dump_wn_stmt(WN* wn);
  static void dump_wn_node(WN* wn);
};

void dp::dump_wn_node(WN* wn)
{
    dump_wn(wn);
}

void dp::dump_wn_stmt(WN* wn)
{
    dump_tree(wn);
}

void dp::draw_bb(BB *bb)
{
  char caption[100];
  sprintf (caption,"BB (id = %d) op graph" , bb->id);
  draw_bb_op (bb,caption);
}

void dp::dump_bb(BB *bb)
{
    Set_Trace_File_internal(stderr);
    Print_BB_No_Srclines(bb);
    Set_Trace_File_internal(TFile);
}


void dp::dump_op(OP *op)
{
   fprintf (stderr, "[map_idx %d]" , OP_map_idx(op));
   Set_Trace_File_internal(stderr);
   Print_OP_No_SrcLine(op);
   Set_Trace_File_internal(TFile);
}

void dp::region_cfg(REGION *r)
{
  draw_regional_cfg(r);
}

void dp::global_cfg()
{
  draw_global_cfg();
}

BB *
dp::get_bb (mBB_NUM bb_id) {
    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
      if (BB_id (bb) == bb_id) {
        fprintf (stderr, "BB: %d Address: %p\n", bb_id,bb);
        return bb ; 
      }
    }
    return NULL ;
}

OP *
dp::get_op (mBB_NUM bb_id, mUINT16 map_idx)
{
  BB * bb = dp::get_bb (bb_id) ;
  if (!bb) return NULL ;

  OP * op ;
  FOR_ALL_BB_OPs (bb, op) {
    if (OP_map_idx (op) == map_idx) {
       fprintf (stderr, "OP Address: %p\n", op); 
       return op ;
    }
  }
  return NULL ;
}

static char *arc_txt[] = {
  "REGIN", 
  "REGOUT", 
  "REGANTI", 
  "MEMIN", 
  "MEMOUT", 
  "MEMANTI", 
  "MEMVOL",
  "MEMREAD", 
  "SPILLIN", 
  "PREFIN", 
  "PREFOUT", 
  "PREBR", 
  "POSTBR",
  "SCC", 
  "PRECHK", 
  "POSTCHK", 
  "MISC"
};

void
dp::dump_succs(mBB_NUM bb_id, mUINT16 map_idx) {
    OP* op = dp::get_op(bb_id, map_idx);
    if (!op) return;

    for(ARC_LIST* arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
        ARC *arc = ARC_LIST_first(arcs) ;
        OP *succ = ARC_succ(arc) ;
        fprintf(stderr,"[%d/%d]\t-->%p(%s)\t LAT=%d ARC=%s\t %s\t %s\n", 
			          succ->bb->id, 
			          succ->map_idx, 
                                  succ,
                                  TOP_Name((TOP)succ->opr),
                                  ARC_latency(arc),
			          arc_txt[ARC_kind(arc)], 
			          ARC_is_dotted(arc)   ? "dotted"   : "strict", 
			          ARC_is_definite(arc) ? "definite" : "indefinite");
    }

    return;
}

void 
dp::dump_succs_p(OP* op)
{
    mBB_NUM bb_id = op->bb->id;
    mUINT16 map_idx = op->map_idx;
    dump_succs(bb_id,map_idx);
}

void
dp::dump_preds(mBB_NUM bb_id, mUINT16 map_idx) {
    OP* op = dp::get_op(bb_id, map_idx);
    if (!op) return;

    for(ARC_LIST* arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
        ARC *arc = ARC_LIST_first(arcs) ;
        OP *pred = ARC_pred(arc) ;
        fprintf(stderr,"[%d/%d]\t%p(%s)-->\t LAT=%d\tARC=%s\t %s\t %s\n", 
			          pred->bb->id, 
			          pred->map_idx,
                                  pred,
                                  TOP_Name((TOP)pred->opr),
                                  ARC_latency(arc),
		       	          arc_txt[ARC_kind(arc)], 
			          ARC_is_dotted(arc)   ? "dotted"   : "strict", 
			          ARC_is_definite(arc) ? "definite" : "indefinite");
    }

    return;
}

void
dp::dump_preds_p(OP* op)
{
    mBB_NUM bb_id = op->bb->id;
    mUINT16 map_idx = op->map_idx;
    dump_preds(bb_id,map_idx);
    return;
}


void
dp::dump_pdom(mBB_NUM bb_id) {
   BB * bb = dp::get_bb(bb_id) ; 
   if (!bb) {
        fprintf (stderr, "no such bb %d\n" , bb_id);
        return ;
   }

   fprintf (stderr,"\n-------- BB (%d)'s post dominator ---------\n",bb_id);
   BB_SET_Print (BB_pdom_set(bb), stderr);

   
   fprintf (stderr,"\n");
   fflush (stderr) ;
}

void
dp::dump_tn_set (BS *set) {
  TN_SET_Print (set, stderr) ;
  fprintf (stderr, "\n");
  fflush (stderr) ;
}
