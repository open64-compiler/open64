
// The cg part of SL ZDL implementation.

// Copyright (C) 2011 Simplight Nanoelectronics, Inc. All Rights Reserved.

// Open64 is free software; you can redistribute it and/or modify it 
// under the terms of the GNU General Public License as published by 
// the Free Software Foundation; either version 2 of the License, 
// or (at your option) any later version.

// Open64 is distributed in the hope that it will be useful, but 
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along 
// with this program; if not, write to the Free SoftwareFoundation, Inc., 
// 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

                       

#include <stdint.h>
#include <math.h>
#include <stdarg.h>
#include <set>
#include <vector>
#include <list>
#include <utility>

#include "defs.h"
#include "config.h"
#include "errors.h"
#include "mempool.h"
#include "cg_flags.h"
#include "cgir.h"
#include "tracing.h"
#include "config_asm.h"
#include "tn_map.h"
#include "cio_rwtran.h"
#include "cgprep.h"
#include "op.h"
#include "op_list.h"
#include "bb.h"
#include "cgtarget.h"
#include "cg_swp_target.h"
#include "wn.h"
#include "wn_core.h"
#include "wn_util.h"
#include "whirl2ops.h"

#include "dep_graph.h"  /* for Current_Dep_Graph */
#include "cg.h"
#include "register.h"	/* needed for "gra.h" */
#include "tn_set.h"	/* needed for "gra.h" */
#include "gra.h"
#include "gra_live.h"
#include "bb_set.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "pf_cg.h"
#include "note.h"
#include "cgexp.h"
#include "cio.h"
#include "cio_rwtran.h"
#include "cg_cflow.h"
#include "resource.h"
#include "cg_db_op.h"
#include "dominate.h"
#include "ti_res_count.h"
#include "ti_latency.h"
#include "cg_loop_scc.h"
#include "cg_loop_recur.h"
#include "cg_loop_mii.h"
#include "freq.h"
#include "cg_region.h"
#include "cg_sched_est.h"
#include "cg_loop.h"
#include "cg_swp.h"
#include "cg_swp_options.h"
#include "findloops.h"
#include "dominate.h"
#include "ebo.h"
#include "hb.h"
#include "gra_live.h"
#include "lra.h"
#include "calls.h"

#include "tag.h"
#include "label_util.h"
#include "profile_util.h"
#include "cflow.h"
#include "glob.h" //Src_File_Name
#include "gcm_licm.h" //Is_BB_Empty

#include "region.h"
#include "region_bb_util.h"


static TN* Gen_Const_To_Reg_TN(BB* bb, INT32 val) {
  TN *gpr_tn = Gen_Register_TN( ISA_REGISTER_CLASS_integer, 4 );
  if (ISA_LC_Value_In_Class (val, LC_simm16)){
    BB_Append_Op( bb, Mk_OP(TOP_addiu, gpr_tn, Zero_TN, Gen_Literal_TN(val,4)));
  } else if (ISA_LC_Value_In_Class (val, LC_uimm16)){
    BB_Append_Op( bb, Mk_OP(TOP_ori, gpr_tn, Zero_TN, Gen_Literal_TN(val,4)));
  } else if(val >= 32768 && val <= INT32_MAX ) {
    if((val & 0xffff) == 0) {
      BB_Append_Op( bb, Mk_OP(TOP_lui, gpr_tn, Gen_Literal_TN((val >> 16) & 0xffff, 4)));
    } else {
      BB_Append_Op( bb, Mk_OP(TOP_lui, gpr_tn, Gen_Literal_TN((val >> 16) & 0xffff,4)));
      BB_Append_Op( bb, Mk_OP(TOP_ori, gpr_tn, gpr_tn, Gen_Literal_TN(val & 0xffff, 4)));
    }
  }
  return gpr_tn;
}

static void Restore_ZDL(BB* prolog, BB* tail, TN * trip_count_tn) {
  TN *gpr_tn = Gen_Register_TN( ISA_REGISTER_CLASS_integer, 4 );
  TN *gpr2_tn;
  if ( TN_is_constant(trip_count_tn) && 
       TN_has_value(trip_count_tn)) {
    gpr2_tn = Gen_Const_To_Reg_TN(prolog, TN_value(trip_count_tn));
  }
  else {
    gpr2_tn = trip_count_tn;
  } 
  BB_Append_Op(prolog, Mk_OP(TOP_add, gpr_tn, gpr2_tn, Zero_TN));
  
  TN* target_tn= OP_opnd(BB_last_op(tail), 0);
  BB_Remove_Op(tail, BB_last_op(tail));
  BB_Append_Op(tail, Mk_OP(TOP_addiu, gpr_tn, gpr_tn, Gen_Literal_TN(-1, 4)));
  BB_Append_Op(tail, Mk_OP(TOP_bgtz, gpr_tn, target_tn));
  Set_BB_zdl_body( tail );
}

static void Do_ZDL_Change(BB* prolog, BB* tail, TN* trip_count_tn, INT zdl_level) {
    //setting up lc_tn
    TN* lc_tn;
    switch(zdl_level) {
    case 0: lc_tn = LC0_TN; break;
    case 1: lc_tn = LC1_TN; break;
    case 2: lc_tn = LC2_TN; break;
    case 3: lc_tn = LC3_TN; break;
    default:
      Is_True(FALSE, ("invalide loop-counter-index"));
    }

    OP* mvtc_op;
    /* Set the loop counter, using mvtc */
    if (TN_is_constant(trip_count_tn)) {
      if (TN_has_value(trip_count_tn)) {
        INT32 val = TN_value(trip_count_tn);
        if (ISA_LC_Value_In_Class(val, LC_uimm10)) {
          mvtc_op = Mk_OP(TOP_mvtc_i, lc_tn, trip_count_tn);
        } else {
          TN *gpr_tn = Gen_Const_To_Reg_TN(prolog, val);
          mvtc_op = Mk_OP(TOP_mvtc, lc_tn, gpr_tn);
        }
      }
      else {
        mvtc_op = Mk_OP(TOP_mvtc, lc_tn, trip_count_tn);
      }
    } 
    else {
      mvtc_op = Mk_OP(TOP_mvtc, lc_tn, trip_count_tn);
    }

    BB_Append_Op( prolog, mvtc_op );
    /* setup loop op */
    LABEL_IDX tag = Gen_Tag();
    TN* label_tn = Gen_Label_TN( tag, 0 );
    TN* lc_index_tn = Gen_Literal_TN( zdl_level, 4 );
    OP* loop_op = Mk_OP( TOP_loop, lc_index_tn, label_tn, lc_tn);
    BB_Append_Op( prolog, loop_op );

    /* tag now paired to auxbr */
    Set_OP_Tag(BB_last_op(tail), tag);
        
    Set_BB_zdl_prolog( prolog );
    Set_BB_zdl_body( tail );
}

static BOOL Null_ZDL(BB* prolog, BB* tail){
  BB* iter = prolog;
  while(iter != tail) {
    if (!Is_BB_Empty(iter)) {
      break;
    }
    iter = BB_next(iter);
  }
  return (iter == tail && Is_BB_Empty(iter));
}

static BOOL Has_Jmp_Out(LOOP_DESCR *loop, BB *tail) {
  std::set<BB*> zdl_body;
  std::map<OP*,BB*> jmp_map;
  BB* iter = LOOP_DESCR_loophead(loop);
  while(iter && iter != tail ) {
    zdl_body.insert(iter);
    if (BB_length(iter) != 0) {
      switch (OP_code(BB_last_op(iter))) {
      case TOP_auxbr:
      case TOP_j:
        {
          TN *l_tn = OP_opnd(BB_last_op(iter),0);
          LABEL_IDX j_label =  TN_label(l_tn);
          jmp_map[BB_last_op(iter)] = Get_Label_BB(j_label);
        }
        break;
      case TOP_bgez:
      case TOP_bgezal:
      case TOP_blez:
      case TOP_bltz:
      case TOP_bltzal:
        {
          TN *l_tn = OP_opnd(BB_last_op(iter),1);
          LABEL_IDX j_label = TN_label(l_tn);
          jmp_map[BB_last_op(iter)] = Get_Label_BB(j_label);
        }
        break;
      case TOP_bne:
      case TOP_br16_nez:
      case TOP_beq:
      case TOP_br16_eqz:
        {
          TN *l_tn = OP_opnd(BB_last_op(iter),2);
          LABEL_IDX j_label =  TN_label(l_tn);
          jmp_map[BB_last_op(iter)] = Get_Label_BB(j_label);
        }
        break;
      default:
        {
        }
      }
    }
    iter = BB_next(iter);
  }
  if (iter == tail) {
    zdl_body.insert(iter);
    TN *l_tn = OP_opnd(BB_last_op(iter),0);
    LABEL_IDX j_label = TN_label(l_tn);
    jmp_map[BB_last_op(iter)] = Get_Label_BB(j_label);
  }

  std::map<OP*,BB*>::iterator it = jmp_map.begin();
  for(; it != jmp_map.end(); it ++) {
    std::set<BB*>::iterator body_iter = zdl_body.find(jmp_map[(*it).first]);
    if( body_iter == zdl_body.end()) {
      return TRUE;
    }
  }
  return FALSE;
}

static BOOL Should_Restore_ZDL(LOOP_DESCR *loop, BB* tail, INT32 zdl_level) {

  BB_SET *loop_bbs = LOOP_DESCR_bbset(loop);
  BB* bb;
  FOR_ALL_BB_SET_members (loop_bbs, bb) {
    if (BB_asm(bb)) {
      if (Get_Trace(TP_CGLOOP,0x4000)) {
        LOOP_DESCR_Dump_Loop_Brief( loop );
        fprintf(TFile, "failed zdl, because asm op in it!\n");
      }
      return TRUE;
    }
    if (BB_length(bb) != 0) {
      switch(OP_code(BB_last_op(bb))) {
      case TOP_jal:
      case TOP_jalr:
      case TOP_jr16_lnk:
      case TOP_jr:
      case TOP_jr16:
      case TOP_ret:
      case TOP_ret16:
      case TOP_break:
        {
          if (Get_Trace(TP_CGLOOP,0x4000)) {
            LOOP_DESCR_Dump_Loop_Brief( loop );
            fprintf(TFile, "failed zdl, because has call in it!\n");
          }
          return TRUE;
        }
      }
    }
  }
    
  if (Has_Jmp_Out(loop, tail)) {
    if (Get_Trace(TP_CGLOOP,0x4000)) {
      LOOP_DESCR_Dump_Loop_Brief( loop );
      fprintf(TFile, "failed zdl, because jump out!\n");
    }
  }
  
  if (zdl_level >  CG_zdl_enabled_level - 1 || zdl_level > CG_max_zdl_level - 1) {
    if (Get_Trace(TP_CGLOOP,0x4000)) {
      LOOP_DESCR_Dump_Loop_Brief( loop );
      fprintf(TFile, "failed zdl, because over level!\n");
    }
    return TRUE;
  }
  
  return FALSE;
}

static inline BOOL One_Trip_ZDL(TN * trip_count_tn) {
  return TN_is_constant(trip_count_tn) && TN_has_value(trip_count_tn) && TN_value(trip_count_tn) == 1;
}

INT CG_LOOP_ZDL_Gen(LOOP_DESCR* loop) {

  INT zdl_level=0;

  for( INT i = 0; i < VECTOR_count( loop->children ); i++ ) {
    LOOP_DESCR * child_loop = (LOOP_DESCR*) VECTOR_element( loop->children, i );
    INT child_level = CG_LOOP_ZDL_Gen( child_loop );
    if( child_level > zdl_level ) {
      zdl_level = child_level;
    }    // return child's reverse nested level
  }

  CG_LOOP cl( loop );
  BB* head  = cl.Loop_header();
  BB* prolog = cl.Prolog_end();
  BB* tail = NULL;
  if (BB_preds_len(head) ==2 )
    tail = BB_Other_Predecessor(head, prolog);
  else {
    return zdl_level;
  }

  if (OP_code(BB_last_op(tail)) == TOP_auxbr ) { //Yes, this is a zdl loop
    if (OP_has_tag(BB_last_op(tail))){ //it has been already changed.
      return zdl_level + 1;
    }

    Is_True(BB_Unique_Successor(prolog) == head, ("prolog is not the fall thru predecessor of head!\n"));
    Is_True(BB_prev(head) == prolog, ("prolog is not the prev of head!\n"));

    TN* trip_count_tn = cl.Trip_count_tn();
    if (!trip_count_tn) { //Early exit
      LOOPINFO *loop_info=ANNOT_loopinfo(ANNOT_Get(BB_annotations(head), ANNOT_LOOPINFO));
      WN *trip_wn = WN_loop_trip(LOOPINFO_wn(loop_info));
      Is_True(trip_wn, ("trip_wn is NULL"));
      trip_count_tn = Expand_Expr(trip_wn, NULL, NULL);
      Restore_ZDL(prolog,tail,trip_count_tn);
      return zdl_level;
    }
        
    //check one tripcount loops, turn this loop into normal straight-line code
    if (One_Trip_ZDL(trip_count_tn)) {
      BB_Remove_Op(tail, BB_last_op(tail));
      Unlink_Pred_Succ(tail, head);
      return zdl_level;
    }

    // check null loops
    if (Null_ZDL(prolog,tail)) {
      BB_Remove_Op(tail, BB_last_op(tail)); // do not generate null zdl
      Unlink_Pred_Succ(tail, head);
      return zdl_level;
    }

    // roll back those loops unfit
    if (Should_Restore_ZDL(loop,tail,zdl_level)) {
      Restore_ZDL(prolog,tail,trip_count_tn);
      return zdl_level;
    }

    //now, do real change.
    Do_ZDL_Change(prolog, tail, trip_count_tn, zdl_level);
    cl.Recompute_Liveness();
    return zdl_level + 1;
  }
  else {
    return zdl_level;
  }
}

/* the inside check routine of zdl, to make sure there are no jmp out problem */
static BB* check_zdl_correct_inside(BB* zdl_start) {
  FmtAssert(( BB_length(zdl_start) != 0 && OP_code(BB_last_op(zdl_start)) == TOP_loop),
          ("failed entry check of check_zdl_correct_inside!\n"));
  BB* r_bb = NULL;
  BOOL paired = FALSE;
  std::set<BB*> zdl_body;
  std::map<OP*,BB*> jmp_map;
  OP *zdl_loop = BB_last_op(zdl_start);
  TN *label_tn = OP_opnd(zdl_loop , 1);
  LABEL_IDX zdl_label =  TN_label(label_tn);
  zdl_body.insert(zdl_start);
  BB* iter = BB_next(zdl_start);

  while(iter) {
    if ( BB_length(iter) == 0 ) {
      zdl_body.insert(iter);
      iter=BB_next(iter);
    }
    else {
      switch (OP_code(BB_last_op(iter))) {
      case TOP_loop:
        {
          zdl_body.insert(iter);
          iter = check_zdl_correct_inside(iter);
        }
        break;
      case TOP_auxbr:
        {
          zdl_body.insert(iter);
          LABEL_IDX auxbr_label = Get_OP_Tag(BB_last_op(iter));
          FmtAssert((auxbr_label == zdl_label), ("loop and auxbr paired, but labels are not equal\n"));
          paired=TRUE;
          r_bb= iter->next;
        }
        break;
      case TOP_jal:
      case TOP_jalr:
      case TOP_jr16_lnk:
      case TOP_jr:
      case TOP_jr16:
      case TOP_ret:
      case TOP_ret16:
      case TOP_break:
        {
          FmtAssert(FALSE, ("OP %s jump out of zdl tag %s in PU:%s\n",
                          TOP_Name(OP_code(BB_last_op(iter))),
                          LABEL_name(zdl_label),
                          ST_name(Get_Current_PU_ST())));
        }
        break;
      case TOP_j:
        {
          TN *l_tn = OP_opnd(BB_last_op(iter),0);
          LABEL_IDX j_label =  TN_label(l_tn);
          jmp_map[BB_last_op(iter)] = Get_Label_BB(j_label);
          zdl_body.insert(iter);
          iter = BB_next(iter);
        }
        break;
      case TOP_bgez:
      case TOP_bgezal:
      case TOP_blez:
      case TOP_bltz:
      case TOP_bltzal:
        {
          TN *l_tn = OP_opnd(BB_last_op(iter),1);
          LABEL_IDX j_label = TN_label(l_tn);
          jmp_map[BB_last_op(iter)] = Get_Label_BB(j_label);
          zdl_body.insert(iter);
          iter = BB_next(iter);
        }
        break;
      case TOP_bne:
      case TOP_br16_nez:
      case TOP_beq:
      case TOP_br16_eqz:
        {
          TN *l_tn = OP_opnd(BB_last_op(iter),2);
          LABEL_IDX j_label =  TN_label(l_tn);
          jmp_map[BB_last_op(iter)] = Get_Label_BB(j_label);
          zdl_body.insert(iter);
          iter = BB_next(iter);
        }
        break;
      default:
        {
          zdl_body.insert(iter);
          iter = BB_next(iter);
        }
      }
    }
    if (paired) 
        break;
  }
  if (! paired) {
    FmtAssert(FALSE, ("fail, no paired aux_br for loop !\n"));
  }
  else { // check_and_return
    std::map<OP*,BB*>::iterator it = jmp_map.begin();
    std::map<OP*,BB*> j_out;
    for(; it != jmp_map.end(); it ++) {
      std::set<BB*>::iterator body_iter = zdl_body.find(jmp_map[(*it).first]);
      if( body_iter == zdl_body.end()) {
        j_out[(*it).first] = jmp_map[(*it).first];
      }
    }
    if (! j_out.empty() ) {
      std::map<OP*,BB*>::iterator it = j_out.begin();
      for(; it != j_out.end(); it++ ) {
        fprintf(stderr,
                "OP %s jump out of zdl tag %s to BB %d in PU:%s\n",
                TOP_Name(OP_code((*it).first)),
                LABEL_name(zdl_label),
                BB_id(j_out[(*it).first]),
                ST_name(Get_Current_PU_ST())
                );
      }
      FmtAssert(FALSE, ("fail, jmp out to other bb"));
    }
  } //yes, check and return
  return r_bb;
}


/* the outer check routine of zdl, to make sure there are no jump out problem */
static void check_zdl_correct_outside(BB* start) {
  BB* iter = start;
  while(iter) {
    if ( BB_length(iter) != 0 ) {
      switch(OP_code(BB_last_op(iter))) {
      case TOP_loop:
        iter = check_zdl_correct_inside(iter);
        break;
      case TOP_auxbr:
        FmtAssert(FALSE,("False, auxbr should be paired to loop in PU:%s\n",
                         ST_name(Get_Current_PU_ST())));
        break;
      default:
        iter = BB_next(iter);
      }
    }
    else {
      iter = BB_next(iter);
    }
  }
}

void Target_Specific_ZDLBR_Expansion(TN* target_tn) {
  Build_OP (TOP_auxbr, target_tn, &New_OPs);
}

void Emit_Phase_Validity_Check(void){
  check_zdl_correct_outside(REGION_First_BB);
}

