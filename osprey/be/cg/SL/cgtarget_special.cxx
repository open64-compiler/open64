/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#include "cxx_memory.h"
#include "mempool.h"
#include "wn.h"
#include "cg.h"
#include "bb.h"
#include "op.h"
#include "vstring.h"

static BOOL OP_SL1_CBUS_Workaround_LD(OP *op) 
{
  FmtAssert((Is_Target_Sl1_pcore() || Is_Target_Sl1_dsp()), ("OP_SL1_CBUS_Workaround_LD: FYI"));
  if (OP_load(op) && !OP_c3_load(op)) 
    return TRUE;
  else
    return FALSE;
}

static BOOL OP_SL1_CBUS_Workaround_ST(OP *op) 
{
  FmtAssert((Is_Target_Sl1_pcore() || Is_Target_Sl1_dsp()), ("OP_SL1_CBUS_Workaround_ST: FYI"));
  if (OP_store(op) && !OP_c3_store(op)) 
    return TRUE;
  else
    return FALSE;
}

static TN *Get_CBUS_Workaround_LD_ST_Base_TN(OP *op)
{
  FmtAssert((OP_SL1_CBUS_Workaround_LD(op) || OP_SL1_CBUS_Workaround_ST(op)), ("Get_CBUS_Workaround_LD_ST_Base_TN: FYI"));
  if (OP_SL1_CBUS_Workaround_LD(op))
    return OP_opnd(op, 0);
  else if (OP_SL1_CBUS_Workaround_ST(op)) 
    return OP_opnd(op, 1);
}

// just for bank check, the offset is divided by 4
static INT32 Get_CBUS_Workaround_LD_ST_Offset(OP *op) 
{
  FmtAssert((OP_SL1_CBUS_Workaround_LD(op) || OP_SL1_CBUS_Workaround_ST(op)), ("Get_CBUS_Workaround_LD_ST_Offset: FYI"));
  vstring buf = vstr_begin(1024);
  INT32 offset, offset_t;
  
  extern BOOL r_apply_l_const (OP *op, INT opidx, vstring *buf);  

  if (OP_SL1_CBUS_Workaround_LD(op)) {
    r_apply_l_const(op, 1, &buf);
    offset_t = TN_offset(OP_opnd(op, 1));
  }
  else if (OP_SL1_CBUS_Workaround_ST(op)) {
    r_apply_l_const(op, 2, &buf);
    offset_t = TN_offset(OP_opnd(op, 2));
  }
  offset = atoi(buf.str);
  
  if (!((OP_code(op) == TOP_pop16) || (OP_code(op) == TOP_push16))) {
    offset = offset >> 2;
  }
  return offset;
}

/* For the following sequence, can not be reordered.
 * pop $5,  3(sp)
 * pop $11, 2(sp)
 * pop $5,  0(sp)
 */
static BOOL Can_Reorder_Pop16_Sequence(vector<OP*> *pop16_vector)
{
  INT32 flag[60] = {0};
  vector<OP*>::iterator temp;
  INT32 tn_num;
  for(temp = pop16_vector->begin(); temp != pop16_vector->end(); temp ++) {
    TN* tn = OP_result(*temp, 0);
    FmtAssert(TN_is_register(tn), ("Can_Reorder_Pop16_Sequence: FYI"));
    tn_num = TN_register(tn);
    if (flag[tn_num] == 1)
      return FALSE;
      //FmtAssert(FALSE, ("Can_Reorder_Pop16_Sequence: Can not reorder Pop"));
    else
      flag[tn_num] = 1;    
  }
  return TRUE;
}

static void Insert_pop16_node_to_BB(BB* bb, OP **op, vector<OP*>::iterator temp, vector<OP*> *pop16_vector)
{
  if (temp != pop16_vector->end()) {
    OP * op_t = *temp;
    temp = pop16_vector->erase(temp); 
    if ((*op) != NULL) {
      (*op)->next = op_t;
    } else {
      bb->ops.first = op_t;
    } 
    op_t->prev = (*op);
    *op = op_t;
  }
}

/* Reorder continuous pop16 instructions to reduce bank accesssing conflict */
void CG_Reorder_Pop16(BB *bb, OP *begin_op, OP *end_op, vector<OP*> *pop16_vector) 
{
  if (pop16_vector->size() <= 2) return;
  if (Can_Reorder_Pop16_Sequence(pop16_vector) == FALSE) return;

  vector<OP*> *pop16_vector_0 = CXX_NEW(vector<OP*>, &MEM_local_pool);
  vector<OP*> *pop16_vector_1 = CXX_NEW(vector<OP*>, &MEM_local_pool);
  vector<OP*> *pop16_vector_2 = CXX_NEW(vector<OP*>, &MEM_local_pool);
  vector<OP*> *pop16_vector_3 = CXX_NEW(vector<OP*>, &MEM_local_pool);
  vector<OP*>::iterator temp;
  for(temp = pop16_vector->begin(); temp != pop16_vector->end(); temp ++) {
    TN* tn = OP_opnd(*temp, 1);
    FmtAssert(TN_is_constant(tn), ("CG_Reorder_Pop16: The Offset of Pop16 is not constant"));
    INT64 offset = Get_CBUS_Workaround_LD_ST_Offset(*temp);
    
    switch (offset & 0x3) {
      case 0: pop16_vector_0->insert(pop16_vector_0->end(), *temp); break;
      case 1: pop16_vector_1->insert(pop16_vector_1->end(), *temp); break;
      case 2: pop16_vector_2->insert(pop16_vector_2->end(), *temp); break;
      case 3: pop16_vector_3->insert(pop16_vector_3->end(), *temp); break;
      default:
        FmtAssert(FALSE, ("CG_Reorder_Pop16: FYI"));      
    }    
  }
  pop16_vector->clear();
  OP* op = begin_op;
  vector<OP*>::iterator temp_0 = pop16_vector_0->begin();    
  vector<OP*>::iterator temp_1 = pop16_vector_1->begin();    
  vector<OP*>::iterator temp_2 = pop16_vector_2->begin();    
  vector<OP*>::iterator temp_3 = pop16_vector_3->begin();
  
  while ((temp_0 != pop16_vector_0->end()) || (temp_1 != pop16_vector_1->end()) 
      || (temp_2 != pop16_vector_2->end()) || (temp_3 != pop16_vector_3->end())) {
    Insert_pop16_node_to_BB(bb, &op, temp_0, pop16_vector_0);
    Insert_pop16_node_to_BB(bb, &op, temp_1, pop16_vector_1);
    Insert_pop16_node_to_BB(bb, &op, temp_2, pop16_vector_2);
    Insert_pop16_node_to_BB(bb, &op, temp_3, pop16_vector_3);
  }
  op->next = end_op;
  if (end_op) 
    end_op->prev = op;
  
  CXX_DELETE(pop16_vector_0, &MEM_local_pool);
  CXX_DELETE(pop16_vector_1, &MEM_local_pool);    
  CXX_DELETE(pop16_vector_2, &MEM_local_pool);
  CXX_DELETE(pop16_vector_3, &MEM_local_pool);
  return;
}

/* reorder pop16 sequence for fewer bank conflict access */
 void CG_Reorder_Pop16_In_BB(BB *bb)
{
  OP *op;
  vector<OP*> *pop16_vector = CXX_NEW(vector<OP*>, &MEM_local_pool);

  FOR_ALL_BB_OPs_FWD(bb, op) {
    if (OP_code(op) == TOP_pop16) {
      pop16_vector->clear();
      OP *op1;
      for (op1 = op; op1 && OP_code(op1) == TOP_pop16; op1 = OP_next(op1)) {
        pop16_vector->insert(pop16_vector->end(), op1);
      }
      
      if (pop16_vector->size() > 2) CG_Reorder_Pop16(bb, OP_prev(op), op1, pop16_vector);
      op = op1;
      if (op == NULL) break;
    }
  }
  CXX_DELETE(pop16_vector, &MEM_local_pool);
}

typedef enum {
  BEFORE,
  AFTER
} NOP16_PLACE;

static void Insert_Nop16(BB* bb, OP *op, UINT32 num_nop16, NOP16_PLACE nop_p)
{  
  if (num_nop16 & 0x1 == 1)  num_nop16++;  //paired 16bit instruction
  for (int i=0; i<num_nop16; i++) {
    OP *op1 = Mk_OP(TOP_nop16);
    if (nop_p == BEFORE)
      BB_Insert_Op_Before(bb, op, op1);
    else if (nop_p == AFTER)
      BB_Insert_Op_After(bb, op, op1);
    else 
      FmtAssert(FALSE, ("Insert_Nop16: FYI"));
  }
  DevWarn("Insert nop16 for working around Hardware RAW error");
}

static void Insert_Nop16_before_ld_st(BB* bb, OP *op, UINT32 num_nop16)
{  
  OP *op_t;

  for (op_t = OP_next(op); (op_t != NULL) && (num_nop16 > 0); op_t = OP_next(op_t)) {
    if (OP_SL1_CBUS_Workaround_LD(op_t) || OP_SL1_CBUS_Workaround_ST(op_t)) 
      break;
    else if (OP_code(op_t) != TOP_noop)
      num_nop16 --;
  }
  
  if ((num_nop16 > 0) 
    && (((op_t != NULL) && (OP_SL1_CBUS_Workaround_LD(op_t) || OP_SL1_CBUS_Workaround_ST(op_t)))
      || (op_t == NULL)))
    Insert_Nop16(bb, op, num_nop16, AFTER);
  
}

/* ld/st, ld, execpt no bank conflict */
static BOOL Is_Bank_Conflict_Instr_Sequence(OP *op1, OP *op2)
{
  if ((OP_SL1_CBUS_Workaround_LD(op1) || OP_SL1_CBUS_Workaround_ST(op1))
    && OP_SL1_CBUS_Workaround_LD(op2)

    && (!((Get_CBUS_Workaround_LD_ST_Base_TN(op1) == Get_CBUS_Workaround_LD_ST_Base_TN(op2)) 
       && ((Get_CBUS_Workaround_LD_ST_Offset(op1) & 0x3) != (Get_CBUS_Workaround_LD_ST_Offset(op2) & 0x3)))))
    return TRUE;
  else
    return FALSE;
}


static BOOL TOP_is_c3_address(TOP top)
{
  if (TOP_is_c3_load(top) && (top != TOP_c3_ld) && (top != TOP_c3_ffe) && (top != TOP_c3_fftld))
    return TRUE;
  else
    return FALSE;
}

/* Add Nop16 for dsp thread to make sure that
 * (1) if "ld, ld", "st, ld", "ld, 1-instr, ld" or "st, 1-instr, ld"
 * instruction sequence is issued in the first cycle,
 * there will be not ld/st instruction issued in the second cycle.
 * (2) if "ld" instruction is issued in the first cycle, there will be no 
 *     c3.*.a/ar indtruction issued in the second cycle.
 */
void CG_Add_Nop16_Workaround(BB* bb)
{
  OP *op;
  INT32 num_nop16;
  
  // Insert nop16 if bb entry has common load instruction
  num_nop16 = 1;
  for (op = BB_first_op(bb); (op != NULL) && (num_nop16 >= 0); op = OP_next(op)) {
    if (OP_SL1_CBUS_Workaround_LD(op)) {        
      Insert_Nop16_before_ld_st(bb, op, 3+num_nop16);
      break;
    } else if (OP_code(op) != TOP_noop){
      num_nop16--;
    }
  }

  // Insert nop16 for ld,ld / st,ld in BB
  FOR_ALL_BB_OPs_FWD(bb, op) {   
    if (OP_SL1_CBUS_Workaround_LD(op) || OP_SL1_CBUS_Workaround_ST(op)) {
      OP *op1;
      num_nop16 = 1;
      for (op1 = OP_next(op); (op1 != NULL) && (num_nop16 >= 0); op1 = OP_next(op1)) {
        if (Is_Bank_Conflict_Instr_Sequence(op, op1)) {  
          Insert_Nop16_before_ld_st(bb, op1, 3+num_nop16);
          break;
        } else if (OP_code(op1) != TOP_noop){
          num_nop16--;
        }
      }
    }
  }

  // Insert nop16 for ld, c3.*.a/ar in BB(at least 5 instructions)
  FOR_ALL_BB_OPs_REV(bb, op) {
    if (TOP_is_c3_address(OP_code(op))) {
      OP *op1;
      num_nop16 = 5;
      for (op1 = OP_prev(op); (op1 != NULL) && (num_nop16 > 0); op1 = OP_prev(op1)) {
        if (OP_SL1_CBUS_Workaround_LD(op1)) {        
          Insert_Nop16(bb, op, num_nop16, BEFORE);
          break;
        } else if (OP_code(op1) != TOP_noop){
          num_nop16--;
        }
      }
      // Add nop16 for c3.*.a/ar operator at the entry of BB
      if ((op1 == NULL) && (num_nop16 > 0))
        Insert_Nop16(bb, op, num_nop16, BEFORE);
    }
  }
}
