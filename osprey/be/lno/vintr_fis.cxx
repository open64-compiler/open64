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
//                  	LNO Vector Intrinsic Fission
//                  	----------------------------
//

/* ====================================================================
 * ====================================================================
 *
 * Module: vintr_fis.cxx
 * $Revision: 1.18 $
 * $Date: 05/05/23 12:17:22-07:00 $
 * $Author: kannann@iridot.keyresearch $
 * $Source: be/lno/SCCS/s.vintr_fis.cxx $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Fission innermost loop to enable vectorization for
 *		intrinsic ops
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: be/lno/SCCS/s.vintr_fis.cxx $ $Revision: 1.18 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <stdlib.h>
#include "defs.h"
#include "glob.h"
#include "wn.h"
#include "wn_map.h"
#include "cxx_memory.h"
#include "lwn_util.h"
#include "ff_utils.h"
#include "lnoutils.h"
#include "lnopt_main.h"
#include "scalar_expand.h"
#include "fission.h"
#include "opt_du.h"
#include "dep_graph.h"
#include "btree.h"
#include "reduc.h"
#include "lno_bv.h"
#include "snl.h"
#include "name.h"
#include "inner_fission.h"
#include "lno_scc.h"
#include "config_targ.h"
#include "tlog.h"
#include "sxlimit.h"
#include "intrn_info.h"
#ifdef KEY
#include "wn_simp.h"            // for WN_Simp_Compare_Trees
#include "config_opt.h"         // for CIS_Allowed
#endif

#pragma weak New_Construct_Id

BOOL Variant_Array(WN *store, WN *split_point,
                   ARRAY_DIRECTED_GRAPH16 *dep_graph);
INT Split_Array(WN *statement, WN *split_point,
                   ARRAY_DIRECTED_GRAPH16 *dep_graph);
extern WN *Split_Using_Preg(WN* stmt, WN* intrinsic_op,
                   ARRAY_DIRECTED_GRAPH16* dep_graph,
                   BOOL recursive=TRUE);
typedef STACK<WN*> STACK_OF_WN;
typedef HASH_TABLE<WN*,VINDEX16> WN2VINDEX;
typedef HASH_TABLE<WN*,UINT> WN2UINT;
typedef HASH_TABLE<WN*,INT> WN2INT;
typedef DYN_ARRAY<UINT> UINT_DYN_ARRAY;

#define ESTIMATED_SIZE 100	// used to initialized hash table, etc.
#define Iteration_Count_Threshold 10 // threshold to determine if a loop
                                     // has too few a number of iterations

extern REDUCTION_MANAGER *red_manager;	// LNO reduction manager
extern MEM_POOL SNL_local_pool;		// SNL private mem pool
static MEM_POOL VINTR_FIS_default_pool;	// vintr_fis private mem pool
static ARRAY_DIRECTED_GRAPH16 *adg;	// PU array dep. graph

static void vintr_fission_verbose_info(
  SRCPOS        srcpos,
  char*         message)
{
  
  printf("#### Vintr Fission(%d): %s\n", Srcpos_To_Line(srcpos), message);

}

static void vintr_fission_analysis_info(
  BOOL          success,
  SRCPOS        srcpos,
  char*         message)
{
  
  if (success)
    fprintf(LNO_Analysis,"( LNO_Vintr_Fission_Success ");
  else
    fprintf(LNO_Analysis,"( LNO_Vintr_Fission_Failure ");

  fprintf(LNO_Analysis,"(%s %d) \"%s\" )\n",
    Cur_PU_Name, Srcpos_To_Line(srcpos), message);
}

static void vintr_fission_tlog_info(
  FISSION_FUSION_STATUS     status,
  SRCPOS        srcpos,
  char*         index_name,
  char*         message)
{
  
  char in_string[30];
  char out_string[30];
  sprintf(in_string,"%d", Srcpos_To_Line(srcpos));
  sprintf(out_string,"%d", status);
  Generate_Tlog("LNO","vintr_fission",
                Srcpos_To_Line(srcpos),
                index_name,
                in_string, out_string, message);
}

/* Given a machine type id. return the size in mem */
static INT64 MTYPE_size(TYPE_ID mtype)
{
  INT64 sz;
  switch (mtype) {
	case MTYPE_B:		sz=1; break;
	case MTYPE_I1:		sz=1; break;
	case MTYPE_I2:		sz=2; break;
	case MTYPE_I4:		sz=4; break;
	case MTYPE_I8:		sz=8; break;
	case MTYPE_U1:		sz=1; break;
	case MTYPE_U2:		sz=2; break;
	case MTYPE_U4:		sz=4; break;
	case MTYPE_U8:		sz=8; break;
	case MTYPE_F4:		sz=4; break;
	case MTYPE_F8:		sz=8; break;
#if defined(TARG_IA64)
	case MTYPE_F10:		sz=16; break;
#else
	case MTYPE_F10:		sz=10; break;
#endif
	case MTYPE_F16:		sz=16; break;
	case MTYPE_STR:		sz=0; break;
	case MTYPE_FQ:		sz=16; break;
	case MTYPE_M:		sz=0; break;
	case MTYPE_C4:		sz=8; break;
	case MTYPE_C8:		sz=16; break;
#if defined(TARG_IA64)
	case MTYPE_C10:		sz=32; break;
#endif
	case MTYPE_CQ:		sz=32; break;
	case MTYPE_V:		sz=0; break;
	default:		sz=0; break;
  }

  return sz;
}

static BOOL
Is_Proper_Descendant(WN *node, WN *ancestor)
{
  do {
    node = LWN_Get_Parent(node);
  } while (node && node != ancestor);
  return (node != NULL);
}

// vintr_fis_2 : examine all scalar reads and writes and do the following
//	1. create name to bit position mappings for new symbol names
//	2. for STID, check if it is scalar expandable
extern  UINT vintr_fis_2(
	WN* loop,		// enclosing loop
	SCALAR_STACK* scalar_reads,	// read refs to be examined
	SCALAR_STACK* scalar_writes,	// write refs to be examined
	BINARY_TREE<NAME2BIT> *mapping_dictionary,
		// dictionary to be updated which records mapping from
		// symbol names to bit positions
	FF_STMT_LIST& expandable_ref_list)
		// list contains all expandable refs after vintr_fis_2
{
  
  UINT bit_position=0;

  SCALAR_STACK *scalar_ref_list[2];
  scalar_ref_list[0]=scalar_reads;
  scalar_ref_list[1]=scalar_writes;

  // look at both reads and writes
  for (INT i=0; i<2; i++) {

    for (INT j=0; j<scalar_ref_list[i]->Elements(); j++) {
  
      WN* scalar_ref=scalar_ref_list[i]->Bottom_nth(j)->Bottom_nth(0)->Wn;
      NAME2BIT temp_map;

      temp_map.Set_Symbol(scalar_ref);

      // create and enter new name to bit_position mapping if it has
      // not been created
      // also check if it is scalar expandable if the ref is a STID
      const BINARY_TREE_NODE<NAME2BIT> *tree_node;
      if (mapping_dictionary->Find(temp_map)==NULL) {

        if (LNO_Test_Dump) {
          temp_map.Get_Symbol().Print(stdout);
          printf("\t\tat bit %d\n", bit_position);
        }
        temp_map.Set_Bit_Position(bit_position);
        mapping_dictionary->Enter(temp_map);
      }

      if (i==1) {
        SE_RESULT se_result = Scalar_Expandable(scalar_ref,loop, Du_Mgr);
        if (!Get_Trace(TP_LNOPT2, TT_LNO_DISABLE_SEFIN) 
            && se_result != SE_NONE || se_result == SE_EASY)
          expandable_ref_list.Append(scalar_ref,&VINTR_FIS_default_pool);
      }

      bit_position++;
    }
  }
  return bit_position;
}

static BOOL is_call_by_value(WN* intrinsic_wn) {
  INTRINSIC intrinsic=(INTRINSIC)WN_intrinsic(intrinsic_wn);
  if (INTRN_by_value(intrinsic))
    return TRUE;
  else
    return FALSE;
}

static INTRINSIC get_vec_intrinsic(INTRINSIC intr_id) {

  switch (intr_id) {
    case INTRN_F4ACOS: return INTRN_F4VACOS;
    case INTRN_F8ACOS: return INTRN_F8VACOS;
    case INTRN_F4ASIN: return INTRN_F4VASIN;
    case INTRN_F8ASIN: return INTRN_F8VASIN;
    case INTRN_F4ATAN: return INTRN_F4VATAN;
    case INTRN_F8ATAN: return INTRN_F8VATAN;
    case INTRN_F4COS: return INTRN_F4VCOS ;
    case INTRN_F8COS: return INTRN_F8VCOS ;
    case INTRN_F4EXP: return INTRN_F4VEXP ;
    case INTRN_F8EXP: return INTRN_F8VEXP ;
#ifndef KEY 
    case INTRN_F4LOG: return INTRN_F4VLOG ;
    case INTRN_F8LOG: return INTRN_F8VLOG ;
#else
    case INTRN_F4LOG: 
      if (LNO_Run_Vintr == 2) return INTRN_F4VLOG ;
      else return INTRINSIC_INVALID;
    // vlog implementation has a bug that shows up with NAS/EP -O3
    case INTRN_F8LOG: 
      if (LNO_Run_Vintr == 2) return INTRN_F8VLOG ;
      else return INTRINSIC_INVALID;
#endif
    case INTRN_F4LOG10: return INTRN_F4VLOG10;
    case INTRN_F8LOG10: return INTRN_F8VLOG10;
    case INTRN_F4SIN: return INTRN_F4VSIN ;
    case INTRN_F8SIN: return INTRN_F8VSIN ;
    // case INTRN_F4SQRT: return INTRN_F4VSQRT;
    // case INTRN_F8SQRT: return INTRN_F8VSQRT;
    case INTRN_F4TAN: return INTRN_F4VTAN ;
    case INTRN_F8TAN: return INTRN_F8VTAN ;
    default: return INTRINSIC_INVALID;
  }
  
}

#ifdef KEY
       WN* find_loop_var_in_simple_ub(WN* loop) {
#else  
static WN* find_loop_var_in_simple_ub(WN* loop) { 
#endif
  WN* start=WN_start(loop);
  WN* end=WN_end(loop);
  WN* step;
  WN* add=WN_kid0(WN_step(loop));

  if (WN_operator(WN_kid0(add))==OPR_INTCONST)
    step=WN_kid0(add);
  else if (WN_operator(WN_kid1(add))==OPR_INTCONST)
    step=WN_kid1(add);
  else
    return NULL;

  if (WN_const_val(step)!=1)
    return NULL;

  ACCESS_ARRAY* aa=Get_Do_Loop_Info(loop)->UB;
  //if (aa->Too_Messy || aa->Num_Vec()!=1)
  if (aa->Too_Messy)
    return NULL;

  WN* loop_index=NULL;
  USE_LIST* use_list=Du_Mgr->Du_Get_Use(start);
  if (use_list->Incomplete())
    return NULL;
  USE_LIST_ITER uiter(use_list);
  for (DU_NODE* u=uiter.First(); !uiter.Is_Empty(); u=uiter.Next()) {
    WN* use=u->Wn();
    if (use==loop_index)  // duplicate uses in the list
      continue;
    WN* tmp=use;
    while (tmp && tmp!=end)
      tmp=LWN_Get_Parent(tmp);
    if (tmp==end)
      if (loop_index)
        return NULL;  // a second use of loop index var is found
      else
        loop_index=use;
  }

#ifdef KEY
   FmtAssert(loop_index,
   ("The index variable does not appear in the loop end (try compiling without -WOPT:copy=off)!"));
#endif
  WN* tmp=LWN_Get_Parent(loop_index);
  OPERATOR opr=WN_operator(tmp);
  if (opr==OPR_MPY) {
    WN* coeff_wn=WN_kid0(tmp);
    if (coeff_wn==loop_index)
      coeff_wn=WN_kid1(tmp);
    if (WN_operator(coeff_wn)!=OPR_INTCONST)
      return NULL;
    tmp=LWN_Get_Parent(tmp);
  } else if (opr==OPR_ADD) {
    tmp=LWN_Get_Parent(tmp);
  } else if (opr==OPR_GE || opr==OPR_LE ||
             opr==OPR_GT || opr==OPR_LT) {
  } else
    return NULL;

  while (tmp!=end) {
    if (WN_operator(tmp)!=OPR_ADD)
      return NULL;
    tmp=LWN_Get_Parent(tmp);
  }
  return loop_index;
}

typedef enum {
  Invariant=0,		// e.g. sin(3) or sin(n) where n is invariant
  Reference=1,		// e.g. sin(&n)
  Simple=2,		// e.g. sin(a(i)) in i-loop
  Complex=3		// everthing else
} INTRINSIC_OPERAND_KIND;

static INTRINSIC_OPERAND_KIND intrinsic_operand_kind(WN* wn, WN* loop) {

  OPERATOR opr=WN_operator(wn);
  if (opr==OPR_PARM) {
    if (WN_Parm_By_Reference(wn))
      return Reference;
    wn=WN_kid0(wn);
    opr=WN_operator(wn);
  }

  if (opr==OPR_CONST || opr==OPR_INTCONST) {
    return Invariant;
  } else if (opr==OPR_LDA) {
    return Reference;
  } else if (opr==OPR_LDID) {
    SYMBOL symbol1(wn);
    SYMBOL symbol2(WN_index(loop));
    if (symbol1==symbol2)
      return Complex;
    DEF_LIST* def_list=Du_Mgr->Ud_Get_Def(wn);
    WN* loop_stmt=def_list->Loop_stmt();
    WN* body=WN_do_body(loop);
    DEF_LIST_ITER d_iter(def_list);
    for (DU_NODE* dnode=d_iter.First(); !d_iter.Is_Empty();
                  dnode=d_iter.Next()) {
      WN* def=dnode->Wn();
      WN* stmt=Find_Stmt_Under(def,body);
      if (stmt!=NULL)
        return Complex;
    }
    return Invariant;
  } else if (opr==OPR_ILOAD) {
    if (WN_kid_count(wn) != 1 || WN_offset(wn) != 0  ||
        WN_operator(WN_kid0(wn)) != OPR_ARRAY)
      return Complex;

    ACCESS_ARRAY* aa=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,WN_kid0(wn));

    if (aa->Too_Messy)
      return Complex;

    ACCESS_VECTOR* av;
    INT loopno=Do_Loop_Depth(loop);

    BOOL seen_non_zero=FALSE;
    for (INT i=0; i<aa->Num_Vec(); i++) {
      av=aa->Dim(i);
      if (av->Too_Messy || av->Non_Lin_Symb)
        return Complex;
      if ((av->Non_Const_Loops() > loopno))
        return Complex;
      if (av->Loop_Coeff(loopno)!=0)
        if (seen_non_zero) // cannot have two non-zero
          return Complex;
        else
          seen_non_zero=TRUE;
    }
    if (!seen_non_zero)
      return Invariant;
    return Simple;
  }

  return Complex;

}

static BOOL is_vectorizable_intrinsic_op_stmt(WN* stmt, WN* loop) {

  OPERATOR opr=WN_operator(stmt);
  if (opr==OPR_STID || opr==OPR_ISTORE) {
    WN* rhs=WN_kid0(stmt);
    opr=WN_operator(rhs);
    if (opr==OPR_INTRINSIC_OP) {
      INTRINSIC intrinsic_id=(INTRINSIC)WN_intrinsic(rhs);
      if (get_vec_intrinsic(intrinsic_id)==INTRINSIC_INVALID)
        return FALSE;
      WN* operand=WN_kid0(rhs);
      INTRINSIC_OPERAND_KIND  kind= intrinsic_operand_kind(operand, loop);
      if (kind!=Invariant && kind!=Reference)
        // will not handle call-by-reference intrinsics
        return TRUE;
    }
  }
  return FALSE;
}

static void copy_propagation(WN* def, WN* use) {

  FmtAssert(WN_operator(def)==OPR_STID,
    ("Expecting an STID"));
  FmtAssert(WN_operator(use)==OPR_LDID,
    ("Expecting an LDID"));
  LWN_Copy_Frequency_Tree(def,use);
  WN* parent=LWN_Get_Parent(use);
  if (WN_kid0(parent)==use) {
    WN_kid0(parent)=WN_kid0(def);
    LWN_Set_Parent(WN_kid0(parent),parent);
  } else {
    WN_kid1(parent)=WN_kid0(def);
    LWN_Set_Parent(WN_kid1(parent),parent);
  }
  WN_kid0(def)=use;
  Du_Mgr->Delete_Def_Use(def,use);
  LWN_Delete_From_Block(LWN_Get_Parent(def),def);
}

static UINT_DYN_ARRAY* vintr_fis_merge_scc_to_form_new_loop(
        UINT            total_scc,      // total number of SCCs
        FF_STMT_LIST*      scc,            // list of statements for SCCs
        UINT*		scc_size,	// size of each scc
        WN*             loop,           // loop enclosing the SCCs
        SCC_DIRECTED_GRAPH16 *scc_dep_g // SCC dependence graph
        )
{

  // store sccs chosen as seeds to form new loops
  UINT_DYN_ARRAY *seed_scc=CXX_NEW(UINT_DYN_ARRAY(&VINTR_FIS_default_pool),
                    &VINTR_FIS_default_pool);

  // the queues for SCC available to be merged
  // scc_queue[0] stores intrinsic scc
  // scc_queue[1] stores non_intrinsic scc
  INT* scc_queue[2];
  UINT head0, head1, tail0, tail1;  // heads and tails of scc_queue

  INT scc_remained=total_scc;
  UINT intrinsic=0;
  UINT non_intrinsic=1;

  UINT i;
  for (i=0; i<2; i++) {
    scc_queue[i]= CXX_NEW_ARRAY(INT,total_scc+1,&VINTR_FIS_default_pool);
  }
  head0=tail0=0;
  head1=tail1=0;

  // initially, only those SCCs without any predecessor are available
  for (i=1; i<=total_scc; i++) {

    if (scc_size[i]>0 && scc_dep_g->Get_In_Edge(i)==0) {
      // scc_size could be 0 if the single assignment stmt is removed
      // after copy_propagation
      if (scc_size[i]==1) {
        WN* stmt=scc[i].Head()->Get_Stmt();
        if (is_vectorizable_intrinsic_op_stmt(stmt,loop))
          scc_queue[intrinsic][head0++]=i;
        else
          scc_queue[non_intrinsic][head1++]=i;
      } else
          scc_queue[non_intrinsic][head1++]=i;
    } else if (scc_size[i]==0)
      scc_remained--;
  }

  INT kind=intrinsic;
  INT last_loop_kind=intrinsic;
  WN* body=WN_do_body(loop);
  while (1) {
    UINT current_scc;
    if (kind==intrinsic && head0!=tail0) {
      current_scc=scc_queue[intrinsic][tail0++];
      UINT loop_id=seed_scc->Newidx();
      (*seed_scc)[loop_id]=current_scc;
      last_loop_kind=intrinsic;
      scc_remained--;
    } else if (kind==non_intrinsic && head1!=tail1) {
      current_scc=scc_queue[non_intrinsic][tail1++];

      if (last_loop_kind!=non_intrinsic) {
        UINT loop_id=seed_scc->Newidx();
        (*seed_scc)[loop_id]=current_scc;
      } else {
        scc[(*seed_scc)[seed_scc->Lastidx()]].Append_List(&scc[current_scc]);
      }
      last_loop_kind=non_intrinsic;
      scc_remained--;
    } else {
      if (head0!=tail0)
        kind=intrinsic;
      else if (head1!=tail1)
        kind=non_intrinsic;
      else
        break;
      continue;
    }

    // remove all out-edges of scc and put new candidate SCCs in queue
    EINDEX16 e=scc_dep_g->Get_Out_Edge(current_scc);
    while (e) {
  
      VINDEX16 v=scc_dep_g->Get_Sink(e);
      scc_dep_g->Delete_Edge(e);
      if (scc_dep_g->Get_In_Edge(v)==0) {
        if (scc_size[v]==1) {
          WN* stmt=scc[v].Head()->Get_Stmt();
          if (is_vectorizable_intrinsic_op_stmt(stmt,loop))
            scc_queue[intrinsic][head0++]=v;
          else
            scc_queue[non_intrinsic][head1++]=v;
        } else
          scc_queue[non_intrinsic][head1++]=v;
      }
      e=scc_dep_g->Get_Next_Out_Edge(e);
    }
  }
  FmtAssert(scc_remained==0,("Merging not finished in vfission"));
  return seed_scc;

}

static void vintr_fis_separate_loop_and_scalar_expand(
	UINT_DYN_ARRAY* new_loops,
	FF_STMT_LIST* scc,
	WN* loop,
	FF_STMT_LIST& expandable_ref_list)
{
  WN* body=WN_do_body(loop);
  UINT total_loops=new_loops->Lastidx()+1;
  UINT *loop_size=CXX_NEW_ARRAY(UINT,total_loops,&VINTR_FIS_default_pool);
  // hash table which maps a statement to a result loop (id)
  WN2INT *stmt_to_loop=
  CXX_NEW(WN2INT(ESTIMATED_SIZE, &VINTR_FIS_default_pool),&VINTR_FIS_default_pool);

  BOOL fission_ok = (total_loops>1);
  UINT i;
  for (i=0; i<total_loops; i++) {

    UINT seed_scc=(*new_loops)[i];
    UINT total_stmt=0;
    FF_STMT_ITER s_iter(&scc[seed_scc]);
    for (FF_STMT_NODE* stmt_node=s_iter.First(); !s_iter.Is_Empty();
      stmt_node=s_iter.Next()) {
      WN* stmt=stmt_node->Get_Stmt();
      stmt_to_loop->Enter(stmt,i);
      LWN_Insert_Block_Before(body,NULL,LWN_Extract_From_Block(stmt));
      total_stmt++;
    }
    loop_size[i]=total_stmt;

  }

  if (total_loops>1) {
    BOOL has_calls_or_gotos_or_inner_loops = FALSE;
    DO_LOOP_INFO* loop_info=Get_Do_Loop_Info(loop, FALSE);
    if (loop_info->Has_Calls || loop_info->Has_Gotos || !loop_info->Is_Inner) {
      has_calls_or_gotos_or_inner_loops = TRUE;
    }

    BOOL need_expansion = FALSE; 
    BOOL need_finalization = FALSE; 
    STACK<WN*> se_stack(&VINTR_FIS_default_pool);
    STACK<BOOL> finalize_stack(&VINTR_FIS_default_pool);
    FF_STMT_ITER r_iter(&expandable_ref_list);
    for (FF_STMT_NODE* ref_node=r_iter.First(); !r_iter.Is_Empty();
        ref_node=r_iter.Next()) {
        WN* ref=ref_node->Get_Stmt();
        WN* stmt0=Find_Stmt_Under(ref,body);
	WN* wn_eq_loop = NULL; 
        STACK<WN*>* equivalence_class=
          Scalar_Equivalence_Class(ref, Du_Mgr, &VINTR_FIS_default_pool,
	    TRUE, &wn_eq_loop);
        BOOL expand = FALSE;
        BOOL finalize = FALSE;
        while (!equivalence_class->Is_Empty() && !expand) {
          WN* ref1=equivalence_class->Pop();
          WN* stmt1=Find_Stmt_Under(ref1,body);
          if (stmt_to_loop->Find(stmt0)!=stmt_to_loop->Find(stmt1)) {
            expand = TRUE;
  	    need_expansion = TRUE; 
            if (wn_eq_loop != NULL) {
	      finalize = TRUE; 
	      need_finalization = TRUE; 
	    } 
	  }
        }
        // cannot do expansion right away because it will
        // destroy stmt_to_loop mapping
        if (expand) {
          se_stack.Push(ref);
	  finalize_stack.Push(finalize); 
	}
    }
    WN* guard_tests[1];
    guard_tests[0] = NULL;
    if (need_finalization)
      SE_Guard_Tests(loop, 1, guard_tests, Do_Loop_Depth(loop));
    WN* tile_loop = NULL; 
    if (need_expansion && !Get_Trace(TP_LNOPT, TT_LNO_BIG_SCALAR_TILES)) {
      tile_loop = SE_Tile_Inner_Loop(loop, &LNO_default_pool); 
    }
    for (i=0; i<se_stack.Elements(); i++) {
      WN* wn_ref = se_stack.Top_nth(i); 
      SYMBOL sym(wn_ref); 
      if (tile_loop != NULL) { 
        BOOL finalize = finalize_stack.Top_nth(i); 
	WN* loops[1];
	loops[0] = loop;
	WN* tile_loops[1];
	tile_loops[0] = tile_loop;
	INT strip_sizes[1];
	strip_sizes[0] = (INT) Step_Size(tile_loop, 0);
	INT order[1];
	order[0] = 0;
	Scalar_Expand(tile_loop, loop, NULL, sym, loops, order, 1, TRUE, 
	  finalize, FALSE, guard_tests, NULL, tile_loops, strip_sizes, 1);
      } else { 
	INT dummy[1]={0};
        BOOL finalize = finalize_stack.Top_nth(i); 
	Scalar_Expand(loop, loop, NULL, sym, &loop, dummy, 1, FALSE, 
	  finalize, FALSE, guard_tests);
      } 
    }

    WN* tmp_loop1=loop;
    WN** wn_starts=CXX_NEW_ARRAY(WN*, total_loops, &VINTR_FIS_default_pool);
    WN** wn_ends=CXX_NEW_ARRAY(WN*, total_loops, &VINTR_FIS_default_pool);
    WN** wn_steps=CXX_NEW_ARRAY(WN*, total_loops, &VINTR_FIS_default_pool);
    WN** new_loops=CXX_NEW_ARRAY(WN*, total_loops, &VINTR_FIS_default_pool);

    wn_starts[0]=WN_kid0(WN_start(tmp_loop1));
    wn_ends[0]=WN_end(tmp_loop1);
    wn_steps[0]=WN_kid0(WN_step(tmp_loop1));
    new_loops[0]=loop;
    WN* stmt=WN_first(body);

    for (i=0; i<total_loops-1; i++) {
  
      INT size=loop_size[i];

      for (INT j=0; j<size; j++)
        stmt=WN_next(stmt);

      WN* tmp_loop2;

      Separate(tmp_loop1, WN_prev(stmt), 1, &tmp_loop2);
      LWN_Parentize(tmp_loop2);
      DO_LOOP_INFO* new_loop_info =
        CXX_NEW(DO_LOOP_INFO(loop_info,&LNO_default_pool), &LNO_default_pool);
      Set_Do_Loop_Info(tmp_loop2,new_loop_info);
      if (has_calls_or_gotos_or_inner_loops) {
        // should check gotos and calls when they are allowed to be in
        // loops handled by vintr_fis phase
      }
      wn_starts[i+1]=WN_kid0(WN_start(tmp_loop2));
      wn_ends[i+1]=WN_end(tmp_loop2);
      wn_steps[i+1]=WN_kid0(WN_step(tmp_loop2));
      new_loops[i+1]=tmp_loop2;

      tmp_loop1=tmp_loop2;
    }

    Fission_DU_Update(Du_Mgr,red_manager,wn_starts,wn_ends,wn_steps,total_loops,new_loops);
    for (i=0; i<total_loops-1; i++)
      scalar_rename(LWN_Get_Parent(wn_starts[i]));

    adg->Fission_Dep_Update(new_loops[0],total_loops);
  }

}

void Gather_Intrinsic_Ops(
  WN* wn, SCALAR_REF_STACK* intrinsic_ops, MEM_POOL *pool)
{
  if (WN_opcode(wn) == OPC_BLOCK) {
    WN* kid = WN_first (wn);
    while (kid) {
      Gather_Intrinsic_Ops(kid,intrinsic_ops,pool);
      kid = WN_next(kid);
    }
    return;
  }
  if (WN_operator(wn)==OPR_INTRINSIC_OP) {
    SCALAR_REF scalar_ref(wn,0);
    intrinsic_ops->Push(scalar_ref);
  }
  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    WN* kid = WN_kid(wn,kidno);
    Gather_Intrinsic_Ops(kid,intrinsic_ops,pool);
  }
}

#ifdef KEY
WN * Loop_being_replaced=NULL;

typedef enum {
  SIN = 0,
  COS = 1,
  OTHER = 2
} INTRN_TYPE;

INTRN_TYPE Intrinsic_Type (INTRINSIC intr_id) {
  switch(intr_id) {
  case INTRN_F4COS:
  case INTRN_F8COS:
    return COS;
  case INTRN_F4SIN:
  case INTRN_F8SIN:
    return SIN;
  default:
    return OTHER;
  }
}

//Bug 10421: to determine whether ub is defined in the loop 
static BOOL UB_Defined_In_Loop(WN * wn, WN* loop,
                               SYMBOL loop_var, 
                               DU_MANAGER* du)
{
  if (WN_operator(wn) == OPR_LDID && loop_var != SYMBOL(wn)){ 
     DEF_LIST *def_list = du->Ud_Get_Def(wn);
     DEF_LIST_ITER iter(def_list);
     const DU_NODE* node = NULL;
     for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
       WN* def = node->Wn();
       if (Wn_Is_Inside(def, loop))
       return TRUE;
     }//end for
   }//end if
   for (INT kid = 0; kid < WN_kid_count(wn); kid ++)
    if (UB_Defined_In_Loop(WN_kid(wn, kid), loop,loop_var, du))
      return TRUE;
   
   return FALSE;
}
#endif

static char fail_msg[128];

// Fission a inner loop 'innerloop' such that the intrinsic ops inside
// can be separated and be vectorized
static INT Vintrinsic_Fission(WN* innerloop)
{
  if(!Do_Loop_Is_Good(innerloop)){
    sprintf(fail_msg, "Do loop has unmapped loads/stores/calls");
    return 0;
  }
  if (Do_Loop_Has_Calls(innerloop)){
    sprintf(fail_msg, "Do loop has calls");
    return 0;
  }

  if(Do_Loop_Has_Gotos(innerloop)){
    sprintf(fail_msg, "Do loop has gotos");
    return 0;  
  }

  if(Do_Loop_Is_Mp(innerloop)){
    sprintf(fail_msg, "Do loop is a MP loop");
    return 0;
  }
  //double check ?
  DO_LOOP_INFO* dli=Get_Do_Loop_Info(innerloop);
  if (dli->Has_Gotos || dli->Has_Calls) {
    sprintf(fail_msg, "Do loop has calls or gotos");
    return 0;
  }

#ifdef TARG_X8664
  // Disregard the remainder loop from SIMD vectorizer.
  if (dli->Is_Generally_Unimportant()) {
    sprintf(fail_msg, "It is a reminder loop from SIMD vectorizer");
    return 0;
  }
#endif
  // if there are too few iterations, we will not fission
  if (dli->Est_Num_Iterations < Iteration_Count_Threshold) {
    sprintf(fail_msg, "Too few iterations");
    return 0;
  }

  // if the loop index var is live at exit and cannot be finalized,
  // we will not vectorize
  if (Index_Variable_Live_At_Exit(innerloop)) {
  
    if (Upper_Bound_Standardize(WN_end(innerloop),TRUE)==FALSE) {
      sprintf(fail_msg, "Upper bound could not be standarized");
      return 0;
    }
    Finalize_Index_Variable(innerloop,FALSE);
    scalar_rename(WN_start(innerloop));
  }

  // if the loop upper bound is too complicated, we will not vectorize
  if (find_loop_var_in_simple_ub(innerloop)==NULL){
    sprintf(fail_msg, "Loop upper bound is too complicated");
    return 0;
  }

#ifdef KEY
  // Bug 10421: if ub has a definition in the loop, we will not vectorize
  if(UB_Defined_In_Loop(WN_end(innerloop), innerloop,
                        SYMBOL(WN_index(innerloop)),
                        Du_Mgr)){
   sprintf(fail_msg, "Loop upper bound has a definition in the loop body");
   return 0;
  }
#endif 

  MEM_POOL_Push(&VINTR_FIS_default_pool);
  {

  char loop_index_name[80];
  if (strlen(ST_name(WN_st(WN_index(innerloop))))>=80) {
    strcpy(loop_index_name,"name_too_long");
  } else
    strcpy(loop_index_name,ST_name(WN_st(WN_index(innerloop))));


  WN* parent_block=LWN_Get_Parent(innerloop);

  TYPE_ID index_type=WN_rtype(WN_end(innerloop));
  char source_line[80];
  if (strlen(Cur_PU_Name)>=65) {
  
    sprintf(source_line,"%s:%d", "name_too_long",
          Srcpos_To_Line(WN_Get_Linenum(innerloop)));
  } else
    sprintf(source_line,"%s:%d", Cur_PU_Name,
          Srcpos_To_Line(WN_Get_Linenum(innerloop)));

  // main statement dependence graph for statements in the loop
  SCC_DIRECTED_GRAPH16 *dep_g_p =
    CXX_NEW(SCC_DIRECTED_GRAPH16(ESTIMATED_SIZE,ESTIMATED_SIZE),
    &VINTR_FIS_default_pool);

  // hash table which associates the statements in the loop and vertices in the
  // above dependence graph 'dep_g_p'
  WN2VINDEX *stmt_to_vertex=
  CXX_NEW(WN2VINDEX(ESTIMATED_SIZE, &VINTR_FIS_default_pool),
    &VINTR_FIS_default_pool);

  SCALAR_REF_STACK *intrinsic_ops =
        CXX_NEW(SCALAR_REF_STACK(&VINTR_FIS_default_pool),
        &VINTR_FIS_default_pool);

  WN* stmt;
  UINT stmt_count=0;
  WN* body=WN_do_body(innerloop);

  // TODO we need copy propagation for (FP) scalars

  // step 1: gather all (scalar and array) references in the loop
  //         allocate a vertex in the stmt. dep. graph for each stmt
  //         assign statement id for each statement

  for (stmt=WN_first(body); stmt; stmt=WN_next(stmt)) {
    Gather_Intrinsic_Ops(stmt,intrinsic_ops,&VINTR_FIS_default_pool) ;
  }

  if (intrinsic_ops->Elements()==0) { // no intrinsic op in this loop
    CXX_DELETE(dep_g_p, &VINTR_FIS_default_pool);
    MEM_POOL_Pop(&VINTR_FIS_default_pool);
    sprintf(fail_msg, "No intrinsic ops in the loop");
    return 0;
  }

#ifdef KEY
  if (CIS_Allowed) {
    // Bug 3887 - If the loop contains calls to SIN and COS with identical
    // arguments, then it is preferrable to convert them to SINCOS inside
    // be/opt/opt_combine.cxx. Vectorizing the loop will inhibit that
    // optimization. Screen these loops here.
    for (INT i=0; i<intrinsic_ops->Elements(); i++) {
      WN* intrini = intrinsic_ops->Top_nth(i).Wn;
      INTRN_TYPE typei = Intrinsic_Type(WN_intrinsic(intrini));
      if (typei != SIN && typei != COS)
	continue;
      WN* arg_intrini = WN_kid0(intrini);
      for (INT j=i+1; j<intrinsic_ops->Elements(); j++) {
	WN* intrinj = intrinsic_ops->Top_nth(j).Wn;
	INTRN_TYPE typej = Intrinsic_Type(WN_intrinsic(intrinj));
	if (typej != SIN && typej != COS)
	  continue;
	if (typei == typej)
	  continue;
	WN* arg_intrinj = WN_kid0(intrinj);
	if (WN_Simp_Compare_Trees(arg_intrini, arg_intrinj) == 0) {
	  CXX_DELETE(dep_g_p, &VINTR_FIS_default_pool);
	  MEM_POOL_Pop(&VINTR_FIS_default_pool);
          sprintf(fail_msg, "Vectorization may inhibit converting to SINCOS");
	  return 0;	
	}
      }
    }  
  }
#endif
  STACK_OF_WN *vec_intrinsic_ops=
    CXX_NEW(STACK_OF_WN(&VINTR_FIS_default_pool),&VINTR_FIS_default_pool);

  INT i;
  for (i=0; i<intrinsic_ops->Elements(); i++) {

    WN* intrinsic_op=intrinsic_ops->Top_nth(i).Wn;
    //WN* stmt=Find_Stmt_Under(intrinsic_op,body);
    WN* stmt=intrinsic_op;
    WN* stmt1;
    BOOL under_scf=FALSE;
    while ((stmt1=LWN_Get_Parent(stmt))!=body) {
      stmt=stmt1;
      if (WN_opcode(stmt)==OPC_BLOCK) {
        under_scf=TRUE;
        break;
      }
    }
    if (under_scf)
      continue;
    if (get_vec_intrinsic((INTRINSIC)WN_intrinsic(intrinsic_op))==
          INTRINSIC_INVALID)
      continue;

    UINT kid_no;
    BOOL splitted=FALSE;
    for (kid_no=0; kid_no<WN_kid_count(intrinsic_op); kid_no++) {
      WN* tmp=WN_kid0(WN_kid(intrinsic_op,kid_no));
      INTRINSIC_OPERAND_KIND kind=intrinsic_operand_kind(tmp,innerloop);
      if (kind==Simple || kind==Complex) {
        tmp = Split_Using_Preg(stmt,tmp,adg,FALSE);
        FmtAssert(WN_operator(tmp)==OPR_STID,
          ("Expecting STID after splitting"));
        USE_LIST* use_list=Du_Mgr->Du_Get_Use(tmp);
        DU_NODE* node=use_list->Head();
        FmtAssert(use_list->Tail()==node, ("Too many uses after splitting"));
        splitted=TRUE;
      }
    }
    if (!splitted)
      continue;

    vec_intrinsic_ops->Push(intrinsic_op);
    WN_OFFSET offset=WN_offset(WN_prev(stmt));

    WN *intrinsic_root = Split_Using_Preg(stmt,intrinsic_op,adg,FALSE);
    FmtAssert(WN_operator(intrinsic_root)==OPR_STID,
      ("Expecting STID after splitting"));
    USE_LIST* use_list=Du_Mgr->Du_Get_Use(intrinsic_root);
    DU_NODE* node=use_list->Head();
    FmtAssert(use_list->Tail()==node, ("Too many uses after splitting"));
    WN* use=node->Wn();
    WN_offset(intrinsic_root)=WN_offset(use)=offset;
    // reuse the temp preg to save scalar expansion

    if (WN_operator(stmt)==OPR_ISTORE &&
        //Types_Are_Compatible(WN_desc(stmt),use,NULL) &&
        WN_desc(stmt)==WN_desc(use) &&
        Variant_Array(stmt,use,adg) && 
        Is_Proper_Descendant(use, WN_kid0(stmt))) {
      if (!Split_Array(stmt,use,adg)) {
#ifndef TARG_X8664
        FmtAssert(0, ("Failed to split using array"));
#else
        // Due to SIMD, we reach the dependence graph (edge) limit sooner.
	// If that is the case, then Split_Array will fail and we should return 
	// without performing vectorization.
        CXX_DELETE(dep_g_p, &VINTR_FIS_default_pool);
        MEM_POOL_Pop(&VINTR_FIS_default_pool);
        sprintf(fail_msg, "Failed to split using array");
        return 0;
#endif
      }
    }

  }

  if (vec_intrinsic_ops->Elements()==0) {
    // no vecorizable intrinsic op in this loop
    CXX_DELETE(dep_g_p, &VINTR_FIS_default_pool);
    MEM_POOL_Pop(&VINTR_FIS_default_pool);
    sprintf(fail_msg, "No vectorizable intrinsic operations");
    return 0;
  }

  REF_LIST_STACK* writes = CXX_NEW(REF_LIST_STACK(&VINTR_FIS_default_pool),
        &VINTR_FIS_default_pool);
  REF_LIST_STACK* reads = CXX_NEW(REF_LIST_STACK(&VINTR_FIS_default_pool),
        &VINTR_FIS_default_pool);

  SCALAR_STACK* scalar_writes = CXX_NEW(SCALAR_STACK(&VINTR_FIS_default_pool),
        &VINTR_FIS_default_pool);
  SCALAR_STACK* scalar_reads = CXX_NEW(SCALAR_STACK(&VINTR_FIS_default_pool),
        &VINTR_FIS_default_pool);
  SCALAR_REF_STACK* params = CXX_NEW(SCALAR_REF_STACK(&VINTR_FIS_default_pool),
        &VINTR_FIS_default_pool);

  // stack used in collecting references
  DOLOOP_STACK *stack1=CXX_NEW(DOLOOP_STACK(&VINTR_FIS_default_pool),
                              &VINTR_FIS_default_pool);
  Build_Doloop_Stack(innerloop, stack1);

  // gather again after intrinsic ops are splitted out of old stmts
  Init_Ref_Stmt_Counter();
  INT32 gather_status = 0;
  for (stmt=WN_first(body); stmt && gather_status!= -1; stmt=WN_next(stmt)) {
    gather_status=New_Gather_References(stmt,writes,reads,stack1,
        scalar_writes,scalar_reads,
        params,&VINTR_FIS_default_pool) ;
  }
  if (gather_status == -1) {
    DevWarn("Error in gathering references");
    CXX_DELETE(dep_g_p, &VINTR_FIS_default_pool);
    MEM_POOL_Pop(&VINTR_FIS_default_pool);
    sprintf(fail_msg, "Error in gathering references");
    return 0;
  }

  for (stmt=WN_first(body); stmt; stmt=WN_next(stmt)) {
    VINDEX16 v=dep_g_p->Add_Vertex();
    if (v==0) {
      DevWarn("Statement dependence graph problem");
      CXX_DELETE(dep_g_p, &VINTR_FIS_default_pool);
      MEM_POOL_Pop(&VINTR_FIS_default_pool);
      sprintf(fail_msg, "Statement dependence graph problem");
      return 0;
    }
    stmt_to_vertex->Enter(stmt, v);
  }
  // a dictionary used for looking up the bit position for a symbol
  BINARY_TREE<NAME2BIT> *mapping_dictionary = 
    CXX_NEW(BINARY_TREE<NAME2BIT>(&VINTR_FIS_default_pool),
    &VINTR_FIS_default_pool);

  // list of references that use scalar-expandable variables
  FF_STMT_LIST expandable_ref_list;

  // step 2: examine all reads and writes and do the following
  //		1. classify them as scalar or array
  //		2. create name to bit position mappings for new symbol names
  //		3. if the ref is STID, check if it is scalar expandable
  UINT sym_count=vintr_fis_2(innerloop, scalar_reads, scalar_writes,
	 mapping_dictionary, expandable_ref_list);

  // we also need to have a set of expandable scalars
  BIT_VECTOR Expandable_Scalar_Set(sym_count, &VINTR_FIS_default_pool);

  // now look at all references in 'expandable_ref_list' and set the
  // corresponding bit in 'Expandable_Scalar_Set'
  FF_STMT_ITER e_iter(&expandable_ref_list);
  for (FF_STMT_NODE* ref_node=e_iter.First(); !e_iter.Is_Empty();
      ref_node=e_iter.Next()) {
      NAME2BIT temp_map;
      temp_map.Set_Symbol(ref_node->Get_Stmt());
      Expandable_Scalar_Set.Set(mapping_dictionary->Find(temp_map)->
               Get_Data()->Get_Bit_Position());
  }

  if (LNO_Test_Dump) {
    printf("Expandable_Scalar_Set=\n");
    Expandable_Scalar_Set.Print(stdout);
  }

  WN_MAP sdm=WN_MAP_Create(&VINTR_FIS_default_pool);
  ARRAY_DIRECTED_GRAPH16 *sdg =
    CXX_NEW(ARRAY_DIRECTED_GRAPH16(100,500,sdm,LEVEL_ARRAY_GRAPH),
      &VINTR_FIS_default_pool);

  for (stmt = WN_first(body); stmt; stmt = WN_next(stmt)) {
    if (!Map_Stmt_To_Level_Graph(stmt,sdg)) {
      FmtAssert(0, ("Error in mapping stmt to level graph\n"));
      CXX_DELETE(dep_g_p, &VINTR_FIS_default_pool);
      CXX_DELETE(sdg, &VINTR_FIS_default_pool);
      WN_MAP_Delete(sdm);
      MEM_POOL_Pop(&VINTR_FIS_default_pool);
      sprintf(fail_msg, "Error in mapping stmt to level graph");
      return(0);
    }
  }

  BOOL status=Generate_Scalar_Dependence_For_Statement_Dependence_Graph(
    innerloop, scalar_reads, scalar_writes, params, sdg, red_manager,
    &Expandable_Scalar_Set, mapping_dictionary);
  if (status==FALSE) {
    DevWarn("Statement dependence graph problem");
    CXX_DELETE(dep_g_p, &VINTR_FIS_default_pool);
    CXX_DELETE(sdg, &VINTR_FIS_default_pool);
    WN_MAP_Delete(sdm);
    MEM_POOL_Pop(&VINTR_FIS_default_pool);
    sprintf(fail_msg, "Statement dependence graph problem");
    return(0);
  }

  status=Generate_Array_Dependence_For_Statement_Dependence_Graph(
    innerloop, reads, writes, sdg, red_manager, adg);
  if (status==FALSE) {
    DevWarn("Statement dependence graph problem");
    CXX_DELETE(dep_g_p, &VINTR_FIS_default_pool);
    CXX_DELETE(sdg, &VINTR_FIS_default_pool);
    WN_MAP_Delete(sdm);
    MEM_POOL_Pop(&VINTR_FIS_default_pool);
    sprintf(fail_msg, "Statement dependence graph problem");
    return(0);
  }

  // dep_g_p would not overflow if sdg did not overflow so no checking
  // is needed

  EINDEX16 e=sdg->Get_Edge();
  while (e) {
    WN* source=sdg->Get_Wn(sdg->Get_Source(e));
    WN* sink=sdg->Get_Wn(sdg->Get_Sink(e));
    if (LWN_Get_Parent(source) == body || LWN_Get_Parent(sink) == body)
      // add edges only if the source and sink are immediate children
      dep_g_p->Add_Unique_Edge(
        stmt_to_vertex->Find(source),
        stmt_to_vertex->Find(sink));
    e=sdg->Get_Next_Edge(e);

  }

  // ac_g is the acyclic condensation graph of dep_g_p
  // it stores dependence relations between SCCs
  SCC_DIRECTED_GRAPH16 *ac_g;
  ac_g = dep_g_p->Acyclic_Condensation(&VINTR_FIS_default_pool);

  VINDEX16 total_scc = dep_g_p->Get_Scc_Count();

  // scc[i] is a list of statemens in i-th SCC
  FF_STMT_LIST *scc;
  scc = CXX_NEW_ARRAY(FF_STMT_LIST, total_scc+1, &VINTR_FIS_default_pool);

  UINT *scc_size=CXX_NEW_ARRAY(UINT, total_scc+1, &VINTR_FIS_default_pool);

  for (i=1; i<=total_scc; i++) {
    scc_size[i]=0;
  }

  // Append statements to the statement list of proper SCC
  for (stmt = WN_first(WN_do_body(innerloop)); stmt; stmt = WN_next(stmt)) {
    VINDEX16 scc_id;
    scc_id = dep_g_p->Get_Scc_Id(stmt_to_vertex->Find(stmt));
    scc_size[scc_id]++;
  }

  for (i=0; i<vec_intrinsic_ops->Elements(); i++) {
    WN* intrinsic_op=vec_intrinsic_ops->Top_nth(i);
    stmt=Find_Stmt_Under(intrinsic_op,body);
    VINDEX16 scc_id = dep_g_p->Get_Scc_Id(stmt_to_vertex->Find(stmt));
    if (scc_size[scc_id]!=1) {
      // cannot be vectorized
      // can eliminate temp pregs using copy propagation
      continue;
    } else {
      FmtAssert(WN_operator(stmt)==OPR_STID,
        ("Expecting an STID"));
      UINT kid_no;
      for (kid_no=0; kid_no<WN_kid_count(intrinsic_op); kid_no++) {
        WN* tmp=WN_kid0(WN_kid(intrinsic_op,kid_no));
        if (WN_operator(tmp)==OPR_LDID) {
          DEF_LIST* def_list=Du_Mgr->Ud_Get_Def(tmp);
          WN* def=def_list->Head()->Wn();
          WN* source_stmt=def;
          if (def_list->Head()!=def_list->Tail() || stmt!=WN_next(source_stmt))
            continue;
          VINDEX16 source_scc_id = 
            dep_g_p->Get_Scc_Id(stmt_to_vertex->Find(source_stmt));
          if (scc_size[source_scc_id]==1 && 
              intrinsic_operand_kind(WN_kid0(def),innerloop)==Simple) {
            // ac_g would not overflow if dep_g_p did not overflow
            // so no checking is needed
            EINDEX16 e=ac_g->Get_In_Edge(source_scc_id);
            while (e) {
              VINDEX16 source_source_scc_id=ac_g->Get_Source(e);
              if (source_source_scc_id!=scc_id)
                ac_g->Add_Unique_Edge(source_source_scc_id,scc_id);
              e=ac_g->Get_Next_In_Edge(e);
            }
            e=ac_g->Get_Out_Edge(source_scc_id);
            while (e) {
              VINDEX16 sink_of_source_scc_id=ac_g->Get_Sink(e);
              if (scc_id!=sink_of_source_scc_id)
                ac_g->Add_Unique_Edge(scc_id,sink_of_source_scc_id);
              e=ac_g->Get_Next_Out_Edge(e);
            }
            ac_g->Delete_Vertex(source_scc_id);
            FF_STMT_ITER r_iter(&expandable_ref_list);
            FF_STMT_NODE* prev=NULL;
            for (FF_STMT_NODE* ref_node=r_iter.First(); !r_iter.Is_Empty();
            ref_node=r_iter.Next()) {
              if (ref_node->Get_Stmt()==source_stmt) {
                SYMBOL symbol1(source_stmt);
                SYMBOL symbol2(stmt);
                if (symbol1==symbol2)
                  ref_node->Set_Stmt(stmt);
                else
                  expandable_ref_list.Remove(prev,ref_node);
                break;
              }
              prev=ref_node;
            }
            copy_propagation(source_stmt,tmp);
          }
        }
      }
      USE_LIST* use_list=Du_Mgr->Du_Get_Use(stmt);
      WN* use=use_list->Head()->Wn();
      WN* sink_stmt=Find_Stmt_Under(use,body);
      if (use_list->Head()!=use_list->Tail() || sink_stmt!=WN_next(stmt))
        continue;
      VINDEX16 sink_scc_id = 
        dep_g_p->Get_Scc_Id(stmt_to_vertex->Find(sink_stmt));
      if (scc_size[sink_scc_id]==1 && LWN_Get_Parent(use)==sink_stmt &&
          Variant_Array(sink_stmt,use,adg)) {
        // ac_g would not overflow if dep_g_p did not overflow
        // so no checking is needed
        EINDEX16 e=ac_g->Get_In_Edge(scc_id);
        while (e) {
          VINDEX16 source_scc_id=ac_g->Get_Source(e);
          if (source_scc_id!=sink_scc_id)
            ac_g->Add_Unique_Edge(source_scc_id,sink_scc_id);
          e=ac_g->Get_Next_In_Edge(e);
        }
        e=ac_g->Get_Out_Edge(scc_id);
        while (e) {
          VINDEX16 sink_of_scc_id=ac_g->Get_Sink(e);
          if (sink_scc_id!=sink_of_scc_id)
            ac_g->Add_Unique_Edge(sink_scc_id,sink_of_scc_id);
          e=ac_g->Get_Next_Out_Edge(e);
        }
        ac_g->Delete_Vertex(scc_id);
        FF_STMT_ITER r_iter(&expandable_ref_list);
        FF_STMT_NODE* prev=NULL;
        for (FF_STMT_NODE* ref_node=r_iter.First(); !r_iter.Is_Empty();
        ref_node=r_iter.Next()) {
          if (ref_node->Get_Stmt()==stmt) {
            expandable_ref_list.Remove(prev,ref_node);
            break;
          }
          prev=ref_node;
        }
        copy_propagation(stmt,use);
      }
    }
  }

  for (i=1; i<=total_scc; i++) {
    scc_size[i]=0;
  }

  // Append statements to the statement list of proper SCC
  for (stmt = WN_first(WN_do_body(innerloop)); stmt; stmt = WN_next(stmt)) {
    VINDEX16 scc_id;
    scc_id = dep_g_p->Get_Scc_Id(stmt_to_vertex->Find(stmt));
    scc[scc_id].Append(stmt, &VINTR_FIS_default_pool);  
    scc_size[scc_id]++;
  }

  if (LNO_Test_Dump)
    for (i=1; i<=total_scc; i++) {

      printf("Vintr_Fis:scc %d:", i);
      FF_STMT_ITER s_iter(&scc[i]);
      INT j=0;
      for (FF_STMT_NODE *stmt_node=s_iter.First(); !s_iter.Is_Empty();
	   stmt_node=s_iter.Next()) {
          stmt=stmt_node->Get_Stmt();
          Dump_WN(stmt,stdout,TRUE,4,4);
        j++;
      }
      printf(" has %d stmts\n", j);

    }

  if (total_scc==1 && scc_size[1]>1) {
    CXX_DELETE(ac_g, &VINTR_FIS_default_pool);
    CXX_DELETE(dep_g_p, &VINTR_FIS_default_pool);
    CXX_DELETE(sdg, &VINTR_FIS_default_pool);
    WN_MAP_Delete(sdm);
    MEM_POOL_Pop(&VINTR_FIS_default_pool);
    sprintf(fail_msg, "Dependence cycle exists");
    return(0);
  }

  UINT_DYN_ARRAY* new_loops;

  new_loops=vintr_fis_merge_scc_to_form_new_loop(total_scc,scc,scc_size,
    innerloop,ac_g);

  // new_loops[i] is the i-th seed SCC

  // separate the loop and expand scalars which is expandable and has
  // references in different fissions loops
  vintr_fis_separate_loop_and_scalar_expand(new_loops,scc,
	innerloop,expandable_ref_list);

  BOOL does_vectorization=FALSE;
  for (i=0; i<vec_intrinsic_ops->Elements(); i++) {
    WN* intrinsic_op=vec_intrinsic_ops->Top_nth(i);

    if (!is_call_by_value(intrinsic_op))
      continue;

    INTRINSIC intr_id=(INTRINSIC)WN_intrinsic(intrinsic_op);
    INTRINSIC vec_intr_id=get_vec_intrinsic(intr_id);

    // if there is no vectorized version then we skip this one
    // TODO should not even fission for the non-vectorizable ones
    if (vec_intr_id==INTRINSIC_INVALID)
      continue;

    WN* iload=WN_kid0(WN_kid0(intrinsic_op));
    WN* istore=LWN_Get_Parent(intrinsic_op);
    WN* new_body=LWN_Get_Parent(istore);
    WN* new_loop=LWN_Get_Parent(new_body);
    if (WN_opcode(new_body)!= OPC_BLOCK ||
        WN_first(new_body)!=istore || WN_last(new_body)!=istore)
      continue;

    if (WN_operator(iload)!=OPR_ILOAD) {
      DevWarn("Expect an ILOAD in Vintrinsic_Fission");
      continue;
    } else if (WN_operator(WN_kid0(iload))!=OPR_ARRAY ||
               WN_kid_count(iload) != 1) {
      DevWarn("Expect a good ARRAY in Vintrinsic_Fission");
      continue;
    }

    TYPE_ID rtype=WN_rtype(intrinsic_op);
    TYPE_ID ptr_type= Pointer_Size == 8 ? MTYPE_U8 : MTYPE_U4;
    TYPE_ID long_type= Pointer_Size == 8 ? MTYPE_I8 : MTYPE_I4;
    OPCODE intconst_opc= OPCODE_make_op(OPR_INTCONST,index_type, MTYPE_V);
    OPCODE add_opc= OPCODE_make_op(OPR_ADD,index_type, MTYPE_V);
    OPCODE sub_opc= OPCODE_make_op(OPR_SUB,index_type, MTYPE_V);
    OPCODE mpy_opc= OPCODE_make_op(OPR_MPY,index_type, MTYPE_V);
    OPCODE addr_add_opc= OPCODE_make_op(OPR_ADD,ptr_type, MTYPE_V);
    OPCODE addr_intconst_opc= OPCODE_make_op(OPR_INTCONST,ptr_type, MTYPE_V);
    OPCODE addr_mpy_opc= OPCODE_make_op(OPR_MPY,ptr_type, MTYPE_V);
    OPCODE addr_div_opc= OPCODE_make_op(OPR_DIV,ptr_type, MTYPE_V);
    TY_IDX ptr_ty=Make_Pointer_Type(WN_ty(istore));
    TYPE_ID rtype1=WN_desc(iload);
    if (rtype!=rtype1) {
      DevWarn("Load storage size differs in Vintrinsic_Fission: %s %s",
              MTYPE_name(rtype),MTYPE_name(rtype1));
    }
    TYPE_ID rtype2=WN_desc(istore);
    if (rtype!=rtype2) {
      DevWarn("Store storage size differs in Vintrinsic_Fission: %s %s",
              MTYPE_name(rtype),MTYPE_name(rtype2));
    }

    if (WN_operator(istore)!=OPR_ISTORE) {
      DevWarn("Expect an ISTORE in Vintrinsic_Fission");
      continue;
    } else if (WN_operator(WN_kid1(istore))!=OPR_ARRAY ||
               WN_kid_count(istore) != 2) {
      DevWarn("Expect a good ARRAY in Vintrinsic_Fission");
      continue;
    }

    char intr_op_name[80];
    SRCPOS srcpos=Srcpos_To_Line(WN_Get_Linenum(istore));
    sprintf(intr_op_name,"%s",
            INTRN_rt_name((INTRINSIC)WN_intrinsic(intrinsic_op)));

    INT loopno=Do_Loop_Depth(new_loop);

    WN* arrayx=WN_kid0(iload);
    WN* startx=LWN_Copy_Tree(arrayx);
    LWN_Copy_Def_Use(arrayx,startx,Du_Mgr);
    if (WN_operator(WN_kid0(arrayx))==OPR_LDA) {
      ST* arrayx_st=WN_st(WN_kid0(arrayx));
#ifdef _NEW_SYMTAB
      Clear_ST_addr_not_passed(arrayx_st);
#else
      Set_ST_addr_taken_passed(arrayx_st);
#endif
    }
    startx = LWN_CreateExp2(addr_add_opc, startx,
                   WN_CreateIntconst(addr_intconst_opc, WN_offset(iload)));
    WN* stridex=WN_CreateIntconst(addr_intconst_opc,0);
    ACCESS_ARRAY* aa=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,arrayx);
    INT64 esize=WN_element_size(arrayx);
    INT64 ty_size=MTYPE_size(rtype);

    if (esize > 0) {
       FmtAssert(esize%ty_size==0,
		 ("Input array element size not multiple of size of operation type"));
       for (INT j=0; j<aa->Num_Vec(); j++) {
	  if (j!=0) {
	     WN* size=WN_array_dim(arrayx,j);
	     WN* tmp=LWN_Copy_Tree(size);
	     LWN_Copy_Def_Use(size,tmp,Du_Mgr);
	     stridex = LWN_CreateExp2(addr_mpy_opc, stridex, tmp);
	  }
	  stridex = LWN_CreateExp2(addr_add_opc, stridex, 
				   WN_CreateIntconst(addr_intconst_opc,
						     aa->Dim(j)->Loop_Coeff(loopno)));
       }
       stridex = LWN_CreateExp2(addr_mpy_opc, stridex,
			 WN_CreateIntconst(addr_intconst_opc,esize/ty_size));
    } else {
       // the non-contiguous index case
       for (INT j=0; j<aa->Num_Vec(); j++) {
	  WN* size=WN_array_dim(arrayx,j);
	  WN* tmp=LWN_Copy_Tree(size);
	  WN* tmp1;
	  LWN_Copy_Def_Use(size,tmp,Du_Mgr);
	  tmp1 = LWN_CreateExp2(addr_mpy_opc, tmp,
				   WN_CreateIntconst(addr_intconst_opc,
						     aa->Dim(j)->Loop_Coeff(loopno)));
	  stridex = LWN_CreateExp2(addr_add_opc,stridex,tmp1);
       }
       // Scale down by type_size/element size, since element size is a byte scale factor
       stridex = LWN_CreateExp2(addr_div_opc,stridex,
				WN_CreateIntconst(addr_intconst_opc,-ty_size/esize));
    }

    SYMBOL index_symbol(WN_start(new_loop));

    WN* arrayy=WN_kid1(istore);
    WN* starty=LWN_Copy_Tree(arrayy);
    LWN_Copy_Def_Use(arrayy,starty,Du_Mgr);
    if (WN_operator(WN_kid0(arrayy))==OPR_LDA) {
      ST* arrayy_st=WN_st(WN_kid0(arrayy));
#ifdef _NEW_SYMTAB
      Clear_ST_addr_not_passed(arrayy_st);
#else
      Set_ST_addr_taken_passed(arrayy_st);
#endif
    }
    starty = LWN_CreateExp2(addr_add_opc, starty,
                   WN_CreateIntconst(addr_intconst_opc, WN_offset(istore)));
    WN* stridey=WN_CreateIntconst(addr_intconst_opc,0);
    aa=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,arrayy);
    esize=WN_element_size(arrayy);

    if (esize > 0) {
       FmtAssert(esize%ty_size==0,
		 ("Output array element size not multiple of size of operation type"));
       for (INT j=0; j<aa->Num_Vec(); j++) {
	  if (j!=0) {
	     WN* size=WN_array_dim(arrayy,j);
	     WN* tmp=LWN_Copy_Tree(size);
	     LWN_Copy_Def_Use(size,tmp,Du_Mgr);
	     stridey = LWN_CreateExp2(addr_mpy_opc, stridey, tmp);
	  }
	  stridey = LWN_CreateExp2(addr_add_opc, stridey, 
				   WN_CreateIntconst(addr_intconst_opc,
						     aa->Dim(j)->Loop_Coeff(loopno)));
       }
       stridey = LWN_CreateExp2(addr_mpy_opc, stridey,
				WN_CreateIntconst(addr_intconst_opc,esize/ty_size));
    } else {
       // Non-contiguous case
       for (INT j=0; j<aa->Num_Vec(); j++) {
	  WN* size=WN_array_dim(arrayy,j);
	  WN* tmp=LWN_Copy_Tree(size);
	  WN* tmp1;
	  LWN_Copy_Def_Use(size,tmp,Du_Mgr);
	  tmp1 = LWN_CreateExp2(addr_mpy_opc, tmp,
				   WN_CreateIntconst(addr_intconst_opc,
						     aa->Dim(j)->Loop_Coeff(loopno)));
	  stridey = LWN_CreateExp2(addr_add_opc, stridey, tmp1);
       }
       // Scale down by type_size/element size, since element size is a byte scale factor
       stridey = LWN_CreateExp2(addr_div_opc,stridey,
				WN_CreateIntconst(addr_intconst_opc,-ty_size/esize));
    }

    WN* loop_end=WN_end(new_loop);
    WN* loop_index=find_loop_var_in_simple_ub(new_loop);
    WN* coeff_wn;
    WN* tmp=LWN_Get_Parent(loop_index);
    if (WN_operator(tmp)==OPR_MPY) {
      coeff_wn=WN_kid0(tmp);
      if (coeff_wn==loop_index)
        coeff_wn=WN_kid1(tmp);
    } else
      coeff_wn=WN_CreateIntconst(intconst_opc,1);

    if (tmp==loop_end)
      tmp=loop_index;
    else
      while (LWN_Get_Parent(tmp)!=loop_end) {
        tmp=LWN_Get_Parent(tmp);
      }

    UINT kid_id;
    if (WN_kid0(loop_end)==tmp)
      kid_id=0;
    else
      kid_id=1;

    OPCODE opc=WN_opcode(loop_end);
    OPERATOR opr=OPCODE_operator(opc);
    OPCODE new_opcode;
    if (opr==OPR_LE) {
      new_opcode=OPCODE_make_op(OPR_LT,OPCODE_rtype(opc),
                                       OPCODE_desc(opc));
      WN_kid1(loop_end)=LWN_CreateExp2(add_opc, WN_kid1(loop_end),
                          WN_CreateIntconst(intconst_opc,1));
      opr=OPR_LT;
      WN_set_opcode(loop_end,new_opcode);
      LWN_Parentize(loop_end);
    } else if (opr==OPR_GE) {
      new_opcode=OPCODE_make_op(OPR_GT,OPCODE_rtype(opc),
                                       OPCODE_desc(opc));
      WN_kid1(loop_end)=LWN_CreateExp2(sub_opc, WN_kid1(loop_end),
                          WN_CreateIntconst(intconst_opc,1));
      opr=OPR_GT;
      WN_set_opcode(loop_end,new_opcode);
      LWN_Parentize(loop_end);
    }

    WN* count=NULL;
    if (opr==OPR_LT || opr==OPR_GT) {
#ifdef KEY
// bug 3388: For creating the vintrinsic call, we use the loop termination
// condition from the loop that is being replaced. When we are copying the 
// def-use, the loop_stmt for the uses in the orig loop should not be copied
// over to the uses in the terminating condition because 
// 1) it seems to be wrong according to the defn of loop_stmt in opt_du.h
// 2) the loop is going to be replaced and hence invalid.
// Also since 'new_loop', i.e. the outermost loop containing the original
// use is being removed, there is NO outermost loop that contains the use
// in vintrinsic call && that follows the defn of loop_stmt. So loop_stmt
// should be null.
      Loop_being_replaced = new_loop;
#endif
      WN* tmp0=LWN_Copy_Tree(WN_kid(loop_end,1-kid_id));
      WN* tmp1=LWN_Copy_Tree(WN_kid(loop_end,kid_id));
      LWN_Copy_Def_Use(WN_kid(loop_end,1-kid_id),tmp0,Du_Mgr);
      LWN_Copy_Def_Use(WN_kid(loop_end,kid_id),tmp1,Du_Mgr);

      if (WN_const_val(coeff_wn)==1)
        count=LWN_CreateExp2(sub_opc,tmp0,tmp1);
      else {
        WN* kids[2];
        kids[0]=LWN_CreateExp2(sub_opc,tmp0,tmp1);
        kids[1]=LWN_Copy_Tree(coeff_wn);
        LWN_Copy_Def_Use(coeff_wn,kids[1],Du_Mgr);
        count=WN_Create_Intrinsic(
                    OPCODE_make_op(OPR_INTRINSIC_OP,long_type, MTYPE_V),
                    (long_type == MTYPE_I8 ? 
                    INTRN_I8DIVCEIL : INTRN_I4DIVCEIL),2,kids);
      }


      WN* kids[5];
      kids[0]=startx;
      kids[1]=starty;
      kids[2]=count;
      kids[3]=stridex;
      kids[4]=stridey;

      if (LNO_Use_Parm) {
        kids[0]=WN_CreateParm(ptr_type, kids[0], ptr_ty, WN_PARM_BY_REFERENCE);
        Create_vector_alias(Alias_Mgr,iload,kids[0]);
        kids[1]=WN_CreateParm(ptr_type, kids[1], ptr_ty, WN_PARM_BY_REFERENCE);
        Create_vector_alias(Alias_Mgr,istore,kids[1]);
        kids[2]=WN_CreateParm(long_type, kids[2], Be_Type_Tbl(long_type),
                            WN_PARM_BY_VALUE);
        kids[3]=WN_CreateParm(long_type, kids[3], Be_Type_Tbl(long_type),
                            WN_PARM_BY_VALUE);
        kids[4]=WN_CreateParm(long_type, kids[4], Be_Type_Tbl(long_type),
                            WN_PARM_BY_VALUE);
      }

      WN* vintr_wn=WN_Create_Intrinsic(OPC_VINTRINSIC_CALL,
                    vec_intr_id,5,kids);
      Replace_Ldid_With_Exp_Copy(
        index_symbol,startx,WN_kid0(WN_start(new_loop)),Du_Mgr);
      Replace_Ldid_With_Exp_Copy(
        index_symbol,starty,WN_kid0(WN_start(new_loop)),Du_Mgr);
      Replace_Ldid_With_Exp_Copy(
        index_symbol,count,WN_kid0(WN_start(new_loop)),Du_Mgr);
      LWN_Parentize(vintr_wn);
      LWN_Copy_Frequency_Tree(new_loop,new_loop);
      LWN_Copy_Frequency_Tree(vintr_wn,new_loop);
      LWN_Insert_Block_Before(LWN_Get_Parent(new_loop),new_loop,vintr_wn);
      // LWN_Simplify_Tree(vintr_wn);

      DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&VINTR_FIS_default_pool),
                                   &VINTR_FIS_default_pool);
      Build_Doloop_Stack(LWN_Get_Parent(vintr_wn), loop_stack);

      LNO_Build_Access(vintr_wn, loop_stack, &LNO_default_pool);

      VINDEX16 v=adg->Add_Vertex(vintr_wn);
      VINDEX16 vl=adg->Get_Vertex(iload);
      VINDEX16 vs=adg->Get_Vertex(istore);

      DEPV_LIST* depv_list=NULL;
      e=adg->Get_Edge(vs,vs);
      if (e) {
        DEPV_ARRAY* depv_array1=adg->Depv_Array(e);
        if (depv_list==NULL)
          depv_list==CXX_NEW(DEPV_LIST(depv_array1,&VINTR_FIS_default_pool),
                  &VINTR_FIS_default_pool);
        else
          for (INT ii=0; ii<depv_array1->Num_Vec(); ii++)
            depv_list->Append(depv_array1->Depv(ii),0);
        adg->Delete_Edge(e);
      }

      e=adg->Get_Edge(vs,vl);
      if (e) {
        DEPV_ARRAY* depv_array1=adg->Depv_Array(e);
        if (depv_list==NULL)
          depv_list==CXX_NEW(DEPV_LIST(depv_array1,&VINTR_FIS_default_pool),
                  &VINTR_FIS_default_pool);
        else
          for (INT ii=0; ii<depv_array1->Num_Vec(); ii++)
            depv_list->Append(depv_array1->Depv(ii),0);
        adg->Delete_Edge(e);
      }
      e=adg->Get_Edge(vl,vs);
      if (e) {
        DEPV_ARRAY* depv_array1=adg->Depv_Array(e);
        if (depv_list==NULL)
          depv_list==CXX_NEW(DEPV_LIST(depv_array1,&VINTR_FIS_default_pool),
                  &VINTR_FIS_default_pool);
        else
          for (INT ii=0; ii<depv_array1->Num_Vec(); ii++)
            depv_list->Append(depv_array1->Depv(ii),0);
        adg->Delete_Edge(e);
      }

      if (depv_list) {
        DEPV_ARRAY* depv_array=Create_DEPV_ARRAY(depv_list,&LNO_default_pool);
        UINT num_dim=depv_array->Num_Dim();
        if (num_dim>1) {
          depv_array=depv_array->Shorten(num_dim-1,&LNO_default_pool);
          adg->Add_Edge(v,v,depv_array);
        }
      }

      e=adg->Get_In_Edge(vs);
      while (e) {
        DEPV_ARRAY* depv_array=adg->Depv_Array(e);
        adg->Set_Depv_Array(e,NULL);
        VINDEX16 from=adg->Get_Source(e);
        adg->Delete_Edge(e);
	EINDEX16 e1=adg->Get_Edge(from,vl);
	if (e1) {
          DEPV_LIST* depv_list=
            CXX_NEW(DEPV_LIST(depv_array,&VINTR_FIS_default_pool),
                    &VINTR_FIS_default_pool);
          DEPV_ARRAY* depv_array1=adg->Depv_Array(e1);
          for (INT ii=0; ii<depv_array1->Num_Vec(); ii++)
            depv_list->Append(depv_array1->Depv(ii),0);
          depv_array=Create_DEPV_ARRAY(depv_list,&LNO_default_pool);
          adg->Delete_Edge(e1);
        }
        if (from==vl || from==vs) {
          Is_True(0,("Duplicate edge in dep. graph"));
        } else
          adg->Add_Edge(from,v,depv_array);
        e=adg->Get_In_Edge(vs); // because 'e' has been deleted
      }

      e=adg->Get_Out_Edge(vs);
      while (e) {
        DEPV_ARRAY* depv_array=adg->Depv_Array(e);
        adg->Set_Depv_Array(e,NULL);
        VINDEX16 to=adg->Get_Sink(e);
        adg->Delete_Edge(e);
	EINDEX16 e1=adg->Get_Edge(vl,to);
	if (e1) {
          DEPV_LIST* depv_list=
            CXX_NEW(DEPV_LIST(depv_array,&VINTR_FIS_default_pool),
                    &VINTR_FIS_default_pool);
          DEPV_ARRAY* depv_array1=adg->Depv_Array(e1);
          for (INT ii=0; ii<depv_array1->Num_Vec(); ii++)
            depv_list->Append(depv_array1->Depv(ii),0);
          depv_array=Create_DEPV_ARRAY(depv_list,&LNO_default_pool);
          adg->Delete_Edge(e1);
        }
        adg->Add_Edge(v,to,depv_array);
        e=adg->Get_Out_Edge(vs); // because 'e' has been deleted
      }

      adg->Delete_Vertex(vs);
      adg->Delete_Vertex(vl);

      WN* parent=LWN_Get_Parent(vintr_wn);
      while (parent) {
        if (WN_opcode(parent)==OPC_DO_LOOP) {
          if (Do_Loop_Is_Good(parent) && !Do_Loop_Has_Gotos(parent))
            break;
          else {
            parent=NULL;
            break;
          }
        }
        parent=LWN_Get_Parent(parent);
      }
      if (parent==NULL)
        adg->Delete_Vertex(v);

#ifdef KEY
      if (LNO_Vintr_Verbose) {
	printf("(%s:%d) ",
	       Src_File_Name,
	       Srcpos_To_Line(WN_Get_Linenum(new_loop)));
	printf("LOOP WAS VECTORIZED FOR VECTOR INTRINSIC ROUTINE(S).\n");
      }
#endif
      LWN_Update_Def_Use_Delete_Tree(new_loop,Du_Mgr);
      LWN_Update_Dg_Delete_Tree(new_loop,adg);
      LWN_Delete_Tree(new_loop);

      does_vectorization=TRUE;

#ifdef KEY
      Loop_being_replaced = NULL;
#endif

      if (LNO_Verbose)
        vintr_fission_verbose_info(srcpos,intr_op_name);
      if (LNO_Analysis)
        vintr_fission_analysis_info(TRUE, srcpos,intr_op_name);
      if ( LNO_Tlog ) {
        vintr_fission_tlog_info(Succeeded,srcpos,loop_index_name,intr_op_name);
      }
    } else {
      DevWarn("Strange loop upper bound");
      LWN_Delete_Tree(startx);
      LWN_Delete_Tree(stridex);
      LWN_Delete_Tree(starty);
      LWN_Delete_Tree(stridey);
    }
  }

  if (does_vectorization) {
    // mark the info of the parent DO or IF about inner loop info
    WN* wn=parent_block;
    WN* inner_loop_found=NULL;
    while (wn) {
  
      OPCODE opc=WN_opcode(wn);
      if (opc==OPC_DO_LOOP || opc==OPC_IF) {
 
        if (!inner_loop_found)
          inner_loop_found=Find_SCF_Inside(wn,OPC_DO_LOOP);
        if (opc==OPC_DO_LOOP) {

          Get_Do_Loop_Info(wn)->Is_Inner=(inner_loop_found==NULL);
          Get_Do_Loop_Info(wn)->Has_Calls=TRUE;
          inner_loop_found=wn;
        } else if (opc==OPC_IF)
          Get_If_Info(wn)->Contains_Do_Loops=(inner_loop_found!=NULL);
      }
      wn=LWN_Get_Parent(wn);
    }
  }

  CXX_DELETE(dep_g_p, &VINTR_FIS_default_pool);
  CXX_DELETE(ac_g, &VINTR_FIS_default_pool);
  CXX_DELETE(sdg, &VINTR_FIS_default_pool);
  new_loops->Free_array();
  WN_MAP_Delete(sdm);
  }
  MEM_POOL_Pop(&VINTR_FIS_default_pool);

  return 1;
  
}

#ifdef KEY //14182
static BOOL Tree_Contains_Intrinsic(WN *wn)
{
 if(WN_operator(wn)==OPR_INTRINSIC_OP)
  return TRUE;
 else if(WN_operator(wn)==OPR_BLOCK){
   for(WN* stmt=WN_first(wn); stmt; stmt=WN_next(stmt)){
     if(Tree_Contains_Intrinsic(stmt))
       return TRUE;
   }
 }
 for (UINT kidno=0; kidno<WN_kid_count(wn); kidno++){
  if(Tree_Contains_Intrinsic(WN_kid(wn,kidno)))
   return TRUE;
 }
 return FALSE;
} 
#endif

static void Vintrinsic_Fission_Walk(WN* wn) {
  OPCODE opc=WN_opcode(wn);

  if (!OPCODE_is_scf(opc)) 
    return;
  else if (opc==OPC_DO_LOOP) {
#ifndef KEY
    if (Do_Loop_Is_Good(wn) && Do_Loop_Is_Inner(wn) && !Do_Loop_Has_Calls(wn)
	&& !Do_Loop_Is_Mp(wn) && !Do_Loop_Has_Gotos(wn))
      Vintrinsic_Fission(wn);
#else //bug 14182: report non-vectorizable reasons only for an inner loop that
      //           contains intrinsics. Also gives up earlier if the loop does
      //           not has any intrinsics.
    if(Do_Loop_Is_Inner(wn) && Tree_Contains_Intrinsic(wn)){
      sprintf(fail_msg, "Unknown reason");
      if(Vintrinsic_Fission(wn)==0){
        if (LNO_Vintr_Verbose) {
          printf("(%s:%d) %s, ",
               Src_File_Name,
               Srcpos_To_Line(WN_Get_Linenum(wn)),
               fail_msg);
          printf("loop was not intrinsic vectorized!\n");
        }
      }
    }
#endif
    else
      Vintrinsic_Fission_Walk(WN_do_body(wn));
  } else if (opc==OPC_BLOCK)
    for (WN* stmt=WN_first(wn); stmt;) {
      WN* next_stmt=WN_next(stmt);
      Vintrinsic_Fission_Walk(stmt);
      stmt=next_stmt;
    }
  else
    for (UINT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Vintrinsic_Fission_Walk(WN_kid(wn,kidno));
    }
}

void Vintrinsic_Fission_Phase(WN* func_nd) {
  
  MEM_POOL_Initialize(&VINTR_FIS_default_pool,"VINTR_FIS_default_pool",FALSE);
  MEM_POOL_Push(&VINTR_FIS_default_pool);

  adg=Array_Dependence_Graph;

  Vintrinsic_Fission_Walk(func_nd);

  MEM_POOL_Pop(&VINTR_FIS_default_pool);
  MEM_POOL_Delete(&VINTR_FIS_default_pool);

}



