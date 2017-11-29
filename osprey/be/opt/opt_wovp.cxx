/*
  Copyright (C) 2006-2007, Hewlett-Packard Company & Tsinghua University.  All rights reserved.
  
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

#include "opt_wovp.h"
#include "opt_mu_chi.h"

// Depth first search the coderep tree, find the first coderep
// of CK_VAR type who has the same Aux_id
// Find_by_id is used in Write_once_check, finding the "U" of
// U-D chain.
CODEREP* WOVP::Find_by_id(CODEREP *cr, IDTYPE id)
{
  INT i;
  CODEREP *retval = NULL;
  switch (cr->Kind()) {
    case CK_VAR:
      if (cr->Aux_id() == id)
        return cr;
      break;
    case CK_OP:
      for (i=0; i< cr->Kid_count(); i++) {
        retval = Find_by_id(cr->Opnd(i), id);
        if (retval)
          return retval;
      }
      break;
    case CK_IVAR:
      if(cr->Ilod_base())
        retval = Find_by_id(cr->Ilod_base(), id);
      else
        retval = Find_by_id(cr->Istr_base(), id);
      if (retval)
        return retval;
      if (OPCODE_operator(cr->Op()) == OPR_MLOAD)
        return Find_by_id(cr->Mload_size(), id);
      break;
    default:
      break;
  }
  return retval;
}

// Check whether the variable identified by "id" is write-once.
// The checking starts from exit_bb(s) in revers order, follows
// the U-D chain, finds and records how many times it is defined.
// "bb_queue" holds exit_bbs. 
BOOL WOVP::Write_once_check(IDTYPE id, BB_LIST_CONTAINER *bb_queue)
{
  INT       bb_num = _cfg->Last_bb_id();
  IDTYPE    bb_id;
  BOOL      skip_phi;
  BB_NODE  *bb, *tmp_bb;
  CODEREP  *cr;
  STMTREP  *tmp_stmt, *it_stmt, *def_stmt = NULL;
  PHI_NODE *phi;
  CHI_NODE *cnode;
  BB_LIST_ITER  pred_bb_iter;
  PHI_LIST_ITER phi_list_iter;
  CHI_LIST_ITER chi_iter;

  BOOL *bb_not_searched = CXX_NEW_ARRAY(BOOL, bb_num+1, &_pool);
  for(INT i = 0; i <= bb_num; i++){
    bb_not_searched[i] = TRUE;
  }

  while((bb = bb_queue->Remove_head(&_pool)) != NULL){
    bb_id = bb->Id();
    if(bb_not_searched[bb_id]){
      bb_not_searched[bb_id] = FALSE;
      skip_phi = FALSE;

      // Don't use FOR_ALL_NODE_REVERSE for needs of changing it_stmt
      it_stmt = bb->Stmtlist()->Tail();
      for( ; it_stmt != NULL; it_stmt = it_stmt->Prev()){
        if(it_stmt->Rhs()){
          cr = Find_by_id(it_stmt->Rhs(), id );
          if(cr){
            if(cr->Is_flag_set(CF_DEF_BY_PHI)){
              if(def_stmt != NULL)
                return FALSE;
              bb = cr->Defphi()->Bb();
              bb_not_searched[bb->Id()] = FALSE;
              skip_phi = TRUE; // The phi-list is checked or bypassed
              break;
            }
            else if(cr->Defstmt()){
              tmp_stmt = cr->Defstmt();
              if(tmp_stmt->Opr() == OPR_OPT_CHI){ // Entry_CHI
                skip_phi = TRUE; // There is no phi-list in this BB
                break;
              }
              if(def_stmt != NULL)
                return FALSE;
              def_stmt = tmp_stmt;
              it_stmt  = tmp_stmt;
              bb       = tmp_stmt->Bb();
              bb_not_searched[bb->Id()] = FALSE;
              continue;
            }
          }
        }
        if(it_stmt->Lhs()){
          cr = Find_by_id(it_stmt->Lhs(), id );
          if(cr){
            if(cr->Is_flag_set(CF_DEF_BY_PHI)){
              if(def_stmt != NULL)
                return FALSE;
              bb = cr->Defphi()->Bb();
              bb_not_searched[bb->Id()] = FALSE;
              skip_phi = TRUE; // The phi-list is checked or bypassed
              break;
            }
            else if(cr->Defstmt()){
              tmp_stmt = cr->Defstmt();
              if(tmp_stmt->Opr() == OPR_OPT_CHI){ // Entry_CHI
                skip_phi = TRUE; // There is no phi-list in this BB
                break;
              }
              if(def_stmt != NULL)
                return FALSE;
              def_stmt = tmp_stmt;
              it_stmt  = tmp_stmt;
              bb       = tmp_stmt->Bb();
              bb_not_searched[bb->Id()] = FALSE;
              continue;
            }
          }
        }
        if(it_stmt->Has_chi()){
          if(it_stmt->Opr() == OPR_OPT_CHI){ // Entry_CHI
            skip_phi = TRUE; // There is no phi-list in this BB
            break;
          }
          FOR_ALL_NODE(cnode,chi_iter,Init(it_stmt->Chi_list())){
            if(cnode->Aux_id()==id){
              if(def_stmt != NULL)
                return FALSE;
              def_stmt = it_stmt;
            }
          }
        }
      }
      if(!skip_phi){
        FOR_ALL_NODE(phi, phi_list_iter, Init(bb->Phi_list()))
          if(phi->Aux_id() == id)
            if(def_stmt != NULL)
              return FALSE;
      }
      FOR_ALL_ELEM(tmp_bb, pred_bb_iter, Init(bb->Pred()))
        bb_queue->Prepend(tmp_bb, &_pool); // Depth first search.
    }
  }
  return TRUE;
}

// The entrance of write-once check, doing preparation work
BOOL WOVP::Is_write_once(IDTYPE id)
{
  BB_LIST_CONTAINER bb_queue;
  
  MEM_POOL_Push(&_pool);
  
  if(_cfg->Fake_exit_bb() == NULL)
    bb_queue.Append(_cfg->Exit_bb(), &_pool);
  else{
    BB_NODE       *exit_bb;
    BB_LIST_ITER  pred_bb_iter;
    FOR_ALL_ELEM(exit_bb, pred_bb_iter, Init(_cfg->Fake_exit_bb()->Pred()))
      if(exit_bb->Kind() == BB_EXIT)
        bb_queue.Append(exit_bb, &_pool);
  }
  if(Write_once_check(id, &bb_queue)){
    MEM_POOL_Pop(&_pool);
    return TRUE;
  }
  else{
    MEM_POOL_Pop(&_pool);
    return FALSE;
  }
}

// Find the MLOAD-MSTORE pair which is the candidate of promotion
void WOVP::Find_mm_pair()
{
  POBB_ITER    po_iter(_cfg);
  BB_NODE      *bb_node;
  STMT_LIST    *stmt_list;
  STMTREP      *stmtrep;
  CODEREP      *rhs, *lhs;
  AUX_STAB_ENTRY *aux_stab_entry;

  FOR_ALL_ELEM(bb_node, po_iter, Init()){
    stmt_list = bb_node->Stmtlist();
    STMTREP_ITER  stmt_iter(stmt_list);
    FOR_ALL_NODE(stmtrep, stmt_iter, Init()){
#ifdef Is_True_On
      if(stmtrep->Opr() == OPR_MSTORE){
        DevWarn("WOVP doesn't take OPR_MSTORE as candidate!");
      }
#endif
      if(stmtrep->Opr() == OPR_STID){
        lhs=stmtrep->Lhs();
        if(lhs->Dsctyp() == MTYPE_M){
          aux_stab_entry=_opt_stab->Aux_stab_entry(lhs->Aux_id());
          if(ST_sclass(aux_stab_entry->St())==SCLASS_AUTO){
            rhs=stmtrep->Rhs();
            if(rhs->Dsctyp() == MTYPE_M && rhs->Kind() == CK_VAR){
              aux_stab_entry = _opt_stab->Aux_stab_entry(rhs->Aux_id());
              if(ST_sclass(aux_stab_entry->St()) == SCLASS_PSTATIC){
                Add_wo_loc(lhs->Aux_id(),rhs->Aux_id(),stmtrep);
              }
            }
          }
        }
      }
    }
  }
}

// Promote the write-once variable by duplicating symbol table entry
void WOVP::Promote(void)
{
  AUX_STAB_ENTRY *laux, *raux;
  WO_LOC *wo_loc;
  vector<WO_LOC*>::iterator it;
  for(it=_wovp_loc.begin(); it!=_wovp_loc.end(); ++it){
    wo_loc = *it;
    if(wo_loc->Get_promote()){
      laux =  _opt_stab->Aux_stab_entry(wo_loc->Get_lid());
      raux =  _opt_stab->Aux_stab_entry(wo_loc->Get_rid());
      *(laux->St()) = *(raux->St());
    }
  }
}

void WOVP::Canon_cr()
{
  CODEREP_ITER cr_iter;
  CODEREP *cr,*bucket;
  CODEMAP_ITER codemap_iter;
  CODEMAP *cr_map = _cfg->Htable();

  FOR_ALL_ELEM(bucket, codemap_iter, Init(cr_map)) {
    IDX_32 hash_idx = codemap_iter.Cur();
    FOR_ALL_NODE(cr, cr_iter, Init(bucket)) {
      switch (cr->Kind()) {
      case CK_VAR:
      case CK_IVAR:
      case CK_CONST:
      case CK_RCONST:
        break;
      case CK_OP:
	if (OPERATOR_is_call(cr->Opr()))
	  break;
        cr_map->Hash_Op(cr);
        break;
      case CK_LDA:
        cr_map->Hash_Lda(cr);
        break;
      }
    }
  }
}

// Main entrance of write once variable promotion optimization
void WOVP::Do_wovp()
{
  WO_LOC *wo_loc;
  STMTREP *stmt;
  vector<WO_LOC*>::iterator it;

  Find_mm_pair();
  for(it=_wovp_loc.begin(); it!=_wovp_loc.end(); ++it){
    wo_loc = *it;
    if(Is_write_once(wo_loc->Get_lid())&&Is_write_once(wo_loc->Get_rid())){
      wo_loc->Set_promote(TRUE);
    }
  }
  if(!_wovp_loc.empty()) {
    Promote();
    // wovp optimization modified the symbol and need to canonicalize 
    // the coderep of cfg
    Canon_cr();

  }  if(Get_Trace(TP_WOPT2, WOVP_DUMP_FLAG)){
    fprintf(TFile, "%sAfter Write Once Variable Promotion\n%s", DBar, DBar);
    Print_wo_loc(Get_Trace_File());
    _cfg->Print(Get_Trace_File());
  }
  return;
}

// Print candidates of promotion.
void WOVP::Print_wo_loc(FILE *fp)
{
  WO_LOC *wo_loc;
  STMTREP *stmt;
  vector<WO_LOC*>::iterator it;

  fprintf(fp, "Print_wo_loc\n");
  for(it=_wovp_loc.begin(); it!=_wovp_loc.end(); ++it){
    wo_loc = *it;
    if(wo_loc->Get_promote()){
      fprintf(fp, "Left_Opr:%3d    ",wo_loc->Get_lid());
      fprintf(fp, "Right_Opr:%3d\n",wo_loc->Get_rid());
      fprintf(fp, "Stmt:\n");
      wo_loc->Get_stmt()->Print(fp);
    }
  }
}
