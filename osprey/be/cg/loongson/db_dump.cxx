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

#include <alloca.h>
#include <stdio.h>
#include <iterator.h>
#include "defs.h"
#include "bb.h"
#include "op.h"
#include "tracing.h"
#include "ir_reader.h"
#include "register.h"
#include "tn.h"
#include "cg_loop_scc.h"
#include "cg_dep_graph.h"
#include "dominate.h"
/*
#include <alloca.h>
#include <stdio.h>
#include <iterator.h>
#include "defs.h"
#include "bb.h"
#include "op.h"
#include "tracing.h"
#include "cg_region.h"
#include "cg_dep_graph.h"
#include "DaVinci.h"
#include "vt_dag.h"
#include "vt_region.h"
#include "vt_partition.h"
#include "region_bb_util.h"
#include "dominate.h"
#include "ir_reader.h"
#include "register.h"
#include "tn.h"
#include "annotations.h"
#include "cg_loop_scc.h"


class dp{
public:

  static void bb_dag(BB *bb);
  static void region_cfg(REGION *r);
  static void draw_bb(BB *bb);
  static void dump_bb(BB *bbg);
  static void dump_op(const OP *opg);
  static void region_tree(REGION *r);
  static void partition_graph(PARTITION_GRAPH *pg);
  static void region_partition_graph(REGION *r);
  static void global_cfg();
  static void dump_bb_vector(BB_VECTOR& bbsg);
  static void dump_node_vector(NODE_VECTOR& nodesg);
  static void dump_node_list(list<REGIONAL_CFG_NODE *>& nodes);
  static BB * get_bb (mBB_NUM bb_idg);
  static OP * get_op (mBB_NUM bb_id, mUINT16 map_idxg);
  static void dump_succs(mBB_NUM bb_id, mUINT16 map_idxg);
  static void dump_succs_p(OP* op);
  static void dump_preds(mBB_NUM bb_id, mUINT16 map_idxg);
  static void dump_preds_p(OP* op);
  static void dump_pdom(mBB_NUM bb_idg);
  static void dump_tn_set (BS *setg);
  static void dump_bb_set(BB_SET* setg);
  static void dump_reg_set(REGISTER_SET regset);
  static void dump_tn_stl_set(set<TN*>& TNsg);
  static void dump_rotate_info(struct ROTATING_KERNEL_INFO *info, int type = 0);
  static void tn_num(TN* tng);
  static void dump_wn_stmt(WN* wn);
  static void dump_wn_node(WN* wn);
  static void dump_st_type(ST* stg);
  static void dump_scc(CG_LOOP_SCC *scc);
  static void dump_tn(TN* dump_tn);
  static void dump_st(ST* st);	
    	
};
	*/

class dp
{
  static void dump_bb(BB *bbg);
  static void dump_op(const OP *opg);
  static void dump_ops(OPS * ops);   
  static void tn_num(TN* tng);
  static void dump_wn_stmt(WN* wn);
  static void dump_wn_node(WN* wn);
  static void dump_tn(TN* dump_tn);
  static void dump_st(ST* st);	
  static BB * get_bb (mBB_NUM bb_idg);
  static OP * get_op (mBB_NUM bb_id, mUINT16 map_idxg);
  static void dump_bb_set(BB_SET* setg);
  static void dump_succs(mBB_NUM bb_id, mUINT16 map_idxg);
  static void dump_succs_p(OP* op);
  static void dump_preds(mBB_NUM bb_id, mUINT16 map_idxg);
  static void dump_preds_p(OP* op);
  static void dump_pdom(mBB_NUM bb_idg);
  static void dump_tn_set (BS *setg);
  static void dump_reg_set(REGISTER_SET regset);
  static void dump_rotate_info(struct ROTATING_KERNEL_INFO *info, int type = 0);
  static void dump_st_type(ST* stg);
  static void dump_scc(CG_LOOP_SCC *scc);
};
//The following are added to help we get useful info when debuging
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

void
dp::dump_bb_set (BB_SET *set) {
  BB_SET_Print (set, stderr) ;
  fprintf (stderr, "\n");
  fflush (stderr) ;
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

void 
dp::dump_reg_set(REGISTER_SET regset){
  REGISTER_SET_Print(regset,stderr);
  fprintf(stderr, "\n");
  fflush(stderr);
}

void dp::dump_st_type(ST* st)
{
    TY_IDX ty_idx = ST_type(st);
    TY& ty = Ty_Table[ty_idx];
    ty.Print(stderr);
}

void dp::dump_rotate_info(struct ROTATING_KERNEL_INFO *info, int type)
{

    static char *reg_class[] = { "u","r","f","p","b","a","c"};
    vector<struct tn *> copyin = ROTATING_KERNEL_INFO_copyin(info);  
    vector<struct tn *> copyout = ROTATING_KERNEL_INFO_copyout(info);  
    vector<struct tn *> localdef = ROTATING_KERNEL_INFO_localdef(info);  
 
    if(type == 0 || type == 1){ 
      fprintf(stderr,"Copyin: "); 
      for(vector<struct tn *>::iterator iter = copyin.begin(); iter != copyin.end(); iter++){
        TN *tn = *iter;
        int reg = TN_register(tn) - 1;
        int regc = TN_register_class(tn);
        int number = TN_number(tn); 
        fprintf(stderr,"[%s%d/%d] ", reg_class[regc],reg,number);
      }
      fprintf(stderr,"\n");  
      fflush(stderr); 
    }
    if(type == 0 || type == 2){ 
      fprintf(stderr,"Copyout: "); 
      for(vector<struct tn *>::iterator iter = copyout.begin(); iter != copyout.end(); iter++){
        TN *tn = *iter;
        int reg = TN_register(tn) - 1;
        int regc = TN_register_class(tn);
        int number = TN_number(tn); 
        fprintf(stderr,"[%s%d/%d] ", reg_class[regc],reg,number);
      }
      fprintf(stderr,"\n");  
      fflush(stderr); 
    }
    if(type == 0 || type == 3){ 
      fprintf(stderr,"Zombie: "); 
      for(vector<struct tn *>::iterator iter = localdef.begin(); iter != localdef.end(); iter++){
        TN *tn = *iter;
        int reg = TN_register(tn) - 1;
        int regc = TN_register_class(tn);
        int number = TN_number(tn); 
        fprintf(stderr,"[%s%d/%d] ", reg_class[regc],reg,number);
      }
      fprintf(stderr,"\n");  
      fflush(stderr); 
    }
}

void dp::dump_scc(CG_LOOP_SCC *scc)
{
    Set_Trace_File_internal(stderr);
    CG_LOOP_SCC_Print(scc);
    Set_Trace_File_internal(TFile);
}


void dp::dump_bb(BB *bb)
{
    Set_Trace_File_internal(stderr);
    Print_BB_No_Srclines(bb);
    Set_Trace_File_internal(TFile);
}


void dp::dump_op(const OP *op)
{
   fprintf (stderr, "[map_idx %d]" , OP_map_idx(op));
   Set_Trace_File_internal(stderr);
   Print_OP_No_SrcLine(op);
   Set_Trace_File_internal(TFile);
}

void dp::dump_ops(OPS * ops)
{
    Set_Trace_File_internal(stderr);
    Set_Trace_File_internal(TFile);
}

void dp::tn_num(TN* tn)
{
    if(TN_is_global_reg(tn)){
        fprintf(stderr,"GTN%d", TN_number(tn));
    } else {
        fprintf(stderr,"TN%d", TN_number(tn));
    }
}

void dp::dump_wn_stmt(WN* wn)
{
    dump_tree(wn);
}

void dp::dump_wn_node(WN* wn)
{
    dump_wn(wn);
}

void dp::dump_tn(TN* dump_tn)
{
   fPrint_TN(stderr,"%s", dump_tn);
   fprintf (stderr, "\n");
   fflush (stderr) ;
}

void dp::dump_st(ST* st){
   st->Print(stdout);
    	switch (st->sym_class) {
	case CLASS_BLOCK:
		Blk_Table[st->u2.blk].Print (stdout);
		break;
	case CLASS_FUNC:
		Pu_Table[st->u2.pu].Print (stdout);
		break;
	}
  fflush (stderr) ;
 }

