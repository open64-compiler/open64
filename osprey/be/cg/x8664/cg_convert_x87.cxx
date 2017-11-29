/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */
                                                                                
                                                                                
/* ====================================================================
 * ====================================================================
 *
 * Module: cg_convert_x87.cxx
 *
 * Description:
 *      Convert all the x87 regs to stack-like regs.
 *
 * ====================================================================
 * ====================================================================
 */

#include <alloca.h>
#include "defs.h"
#include "config.h"
#include "mempool.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "pu_info.h"
#include "cg.h"
#include "cg_flags.h"
#include "ttype.h"
#include "targ_sim.h"
#include "bb_set.h"
#include "freq.h"
#include "cgtarget.h"
#include "whirl2ops.h"
#include "dominate.h"
#include "findloops.h"
#include "cg_vector.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "data_layout.h"
#include "op.h"
#include "cflow.h"
#include "reg_live.h"

#define X87_STACK_SIZE  ( X87_Preg_Max_Offset - X87_Preg_Min_Offset + 1 )
#define FIRST_STACK_REG REGISTER_MIN
#define STACK_IS_EMPTY -1   // this stack is empty
#define STACK_IS_WILD  -2   // this stack has not been initialized yet

typedef struct _Stack {
  int top;			/* index to top stack element    */
  REGISTER_SET live_in;         /* set of live in registers.     */
  REGISTER_SET live_out;        /* set of live out registers.    */
  REGISTER reg[X87_STACK_SIZE]; /* x87 register -> stack mapping */

  struct _Stack* stack_in;      /* an image of the initial stack for back edges */
} Stack;

static int new_bbs = 0;
static BB_MAP bb_stack_info_map;

static BB_MAP dfs_map;

#define BB_NOT_REACHABLE(b)   ( BB_MAP32_Get( dfs_map, (b) ) == 0 )

static inline Stack* Get_Stack_Info( BB* bb )
{
  Stack* stack = (Stack*)BB_MAP_Get(bb_stack_info_map,(bb));
  FmtAssert( stack != NULL, ("NYI") );
  return stack;
}


/* Maybe we should add a flag for <op> that refers to
   x87-stack to speed up the process, instead of calling
   this routine frequently.
*/
static bool OP_refs_x87( OP* op )
{
  // An x87 operation will access x87-stack definitely.
  if( TOP_is_x87(OP_code(op)) )
    return true;

  // The return value could be stored in %st0
  if( OP_call( op ) ){
    const ANNOTATION* ant = ANNOT_Get( BB_annotations(OP_bb(op)), ANNOT_CALLINFO );
    if( ant == NULL ){
      FmtAssert( TN_is_label( OP_opnd( op, 0 ) ), ("call has no annotations") );
      return FALSE;
    }

    const CALLINFO* call_info = ANNOT_callinfo(ant);
    const ST* call_st = CALLINFO_call_st(call_info);
    const WN* call_wn = CALLINFO_call_wn(call_info);
    const TY_IDX call_ty = call_st != NULL ? ST_pu_type(call_st) : WN_ty(call_wn);
    const RETURN_INFO return_info = Get_Return_Info( TY_ret_type(call_ty),
						     No_Simulated,
		       call_st ? PU_ff2c_abi(Pu_Table[ST_pu(call_st)]) : FALSE);

    for( int i = 0; i < RETURN_INFO_count(return_info); i++ ){
      const TYPE_ID type = RETURN_INFO_mtype( return_info, i );

      if( MTYPE_refs_x87( type ) ) {
	return TRUE;
      }
    }
    
    return FALSE;
  }

  // An inline assembly could access x87-stack.
  if( OP_code(op) == TOP_asm ){
    for( int i = 0; i < OP_results(op); i++ ){
      TN* result = OP_result( op, i );
      if( TN_register_class(result) == ISA_REGISTER_CLASS_x87 )
	return true;
    }
    
    for( int i = 0; i < OP_opnds(op); i++ ){
      TN* opnd = OP_opnd( op, i );
      if( TN_is_register( opnd ) &&
	  TN_register_class(opnd) == ISA_REGISTER_CLASS_x87 )
	return true;
    }
  }

  return false;
}


/* Keep a vector of bbs that will access x87 stack. */
static VECTOR bbs_vector = NULL;

static MEM_POOL local_mem_pool;
static MEM_POOL* pu_mem_pool = NULL;

static bool trace = false;

typedef struct {
  mINT8 result_st[ISA_OPERAND_max_results]; /* %st(reg_st[i])  */
  mINT8 opnd_st[ISA_OPERAND_max_operands];  /* %st(opnd_st[i]) */
} OP_stinfo;

extern BB_MAP BBs_Map;

static OP_stinfo* Get_OP_stinfo( OP* op )
{
  BB_OP_MAP op_map = (BB_OP_MAP)BB_MAP_Get( BBs_Map, OP_bb(op) );
  // <op> coule belong to an unreachable BB.
  return op_map == NULL ? NULL : (OP_stinfo*)BB_OP_MAP_Get( op_map, op );
}


// Reture the %st register belongs to <op>.
int Get_OP_stack_reg( OP* op, int opnd )
{
  const OP_stinfo* stinfo = Get_OP_stinfo( op );
  if( stinfo == NULL ){
    return FIRST_STACK_REG - 1;
  }

  const int st = opnd < 0 ? stinfo->result_st[0] - 1 : stinfo->opnd_st[opnd] - 1;
  FmtAssert( st >= 0 && st < X87_STACK_SIZE, ("Invalie stack register.") );

  return st;
}

// Create OP_stinfo for <op>.
static void Create_OP_stinfo( OP* op )
{
  OP_stinfo* stinfo = TYPE_MEM_POOL_ALLOC( OP_stinfo, pu_mem_pool );
  // Because of using bzero, the stack registers have to start from 1, not 0.
  bzero( stinfo, sizeof(stinfo[0]) );
  FmtAssert( OP_bb(op) != NULL, ("NYI") );
  BB_OP_MAP bb_map = (BB_OP_MAP)BB_MAP_Get( BBs_Map, OP_bb(op) );
  BB_OP_MAP_Set( bb_map, op, stinfo );
}


static TOP Pop_Table[TOP_count+1];
static TOP Reverse_Table[TOP_count+1];

static void Init_Pop_Table()
{
  for( int i = 0; i <= TOP_count; i++ ){
    Reverse_Table[i] = Pop_Table[i] = TOP_UNDEFINED;
  }

  // For Pop_Table.

  Pop_Table[TOP_fadd] = TOP_faddp;
  Pop_Table[TOP_fmul] = TOP_fmulp;
  Pop_Table[TOP_fsub] = TOP_fsubp;
  Pop_Table[TOP_fsubr] = TOP_fsubrp;
  Pop_Table[TOP_fdiv]  = TOP_fdivp;
  Pop_Table[TOP_fdivr] = TOP_fdivrp;
  Pop_Table[TOP_fucomi] = TOP_fucomip;

  // For Non_Pop_Table.

  Pop_Table[TOP_fstp]  = TOP_fst;
  Pop_Table[TOP_fstps] = TOP_fsts;
  Pop_Table[TOP_fstpl] = TOP_fstl;
  Pop_Table[TOP_fstps_n32] = TOP_fsts_n32;
  Pop_Table[TOP_fstpl_n32] = TOP_fstl_n32;
  Pop_Table[TOP_fistps] = TOP_fists;
  Pop_Table[TOP_fistpl] = TOP_fistl;

  // For Reverse_Table.

  Reverse_Table[TOP_fsub]  = TOP_fsubr;
  Reverse_Table[TOP_fsubr] = TOP_fsub;
  Reverse_Table[TOP_fsubp]  = TOP_fsubrp;
  Reverse_Table[TOP_fsubrp] = TOP_fsubp;

  Reverse_Table[TOP_fdiv]  = TOP_fdivr;
  Reverse_Table[TOP_fdivr] = TOP_fdiv;
  Reverse_Table[TOP_fdivp]  = TOP_fdivrp;
  Reverse_Table[TOP_fdivrp] = TOP_fdivp;
}


/* Check <tn> is last used by <op>.
 */
static bool Is_TN_Last_Use( Stack* stack, OP* op, TN* tn, bool is_opnd )
{
  const ISA_REGISTER_CLASS cl = TN_register_class(tn);
  const REGISTER reg = TN_register(tn);

  if( is_opnd && OP_Defs_Reg( op, cl, reg ) )
    return true;

  for( OP* next = OP_next(op); next != NULL; next = OP_next(next) ){
    // All the x87 registers are caller-saved.
    if( OP_call( next ) )
      return true;

    // Only care about op that access x87 stack.
    if( OP_refs_x87( next ) ){
      if( OP_Refs_Reg( next, cl, reg ) )
	return false;

      if( OP_Defs_Reg( next, cl, reg ) )
	return true;
    }
  }

  return !REGISTER_SET_MemberP( stack->live_out, reg );
}


static bool Is_Opnd_Last_Use( Stack* stack, OP* op, TN* tn )
{
  return Is_TN_Last_Use( stack, op, tn, true );
}


static bool Is_Result_Last_Use( Stack* stack, OP* op, TN* tn )
{
  return Is_TN_Last_Use( stack, op, tn, false );
}


/* Given a pseudo register <reg>, return the location of this <reg>
   in stack, starting from 0.
*/
static int Get_Stack_Index( Stack* stack, REGISTER reg )
{
  if( reg < FIRST_STACK_REG ||
      reg > REGISTER_CLASS_last_register(ISA_REGISTER_CLASS_x87) ){
    FmtAssert( false, ("Invalid x87 register") );
  }

  FmtAssert( stack->top >= -1,   ("x87 stack underflows.") );
  FmtAssert( stack->top < X87_STACK_SIZE, ("x87 stack overflows.") );

  for( int i = 0; i <= stack->top; i++ ){
    if( stack->reg[i] == reg )
      return i;
  }

  return -1;
}


/* Allocate a stack register to <op>.
 */
static void Alloc_Stack_Reg( Stack* stack, OP* op, int opnd, int stack_idx )
{
  TN* tn = opnd < 0 ? OP_result( op, -opnd-1 ) : OP_opnd( op, opnd );

  if( !TN_is_register(tn)                             ||
      TN_register(tn) == REGISTER_UNDEFINED           ||
      TN_register_class(tn) != ISA_REGISTER_CLASS_x87 ||
      stack_idx < 0 ){
    FmtAssert( false, ("NYI") );
  }

  OP_stinfo* stinfo = Get_OP_stinfo( op );
  if( opnd < 0 )
    stinfo->result_st[-opnd-1] = stack->top - stack_idx + 1;
  else
    stinfo->opnd_st[opnd] = stack->top - stack_idx + 1;

  if( trace ){
    fprintf( TFile, "allocate ST(%d) to %s %d of %s%d(%s)\n",
	     stack->top - stack_idx,
	     opnd < 0 ? "result" : "opnd",
	     opnd < 0 ? -opnd-1 : opnd,
	     TN_is_global_reg(tn) ? "GTN" : "TN",
	     TN_number(tn),
	     REGISTER_name(TN_register_class(tn), TN_register(tn)) );
  }
}


static void Print_Stack( Stack* stack )
{
  if( stack->top == STACK_IS_EMPTY ){
    fprintf( TFile, "stack is empty\n" );
    return;

  } else if( stack->top == STACK_IS_WILD ){
    fprintf( TFile, "stack is uninitialized\n" );
    return;
  }

  fprintf( TFile, "x87 stack [" );


  for( int i = 0; i <= stack->top; i++ ){
    fprintf( TFile, " %s",
	     REGISTER_name( ISA_REGISTER_CLASS_x87,stack->reg[i] ) );
  }

  fprintf( TFile, " <- TOP\n" );
}


/* Perform a swap with st(0) and st(i), where i is the location of
   <dest> in the stack.
*/
static void Expand_Fxch( Stack* stack, OP* op, TN* dest )
{
  Is_True(TN_register_class(dest) == ISA_REGISTER_CLASS_x87,
	  ("Expand_Fxch: invalid register class"));

  const int st_idx = Get_Stack_Index( stack, TN_register(dest) );
  FmtAssert( st_idx >= 0, ("NYI") );
  
  if( st_idx == stack->top )
    return;

  const REGISTER reg = stack->reg[stack->top];
  stack->reg[stack->top] = stack->reg[st_idx];
  stack->reg[st_idx]    = reg;

  /* If stack->reg[stack->top] and stack->reg[st_idx] have the
     same content, then there is no need to generate TOP_fxch here.
  */

  for( OP* prev = OP_prev(op); prev != NULL; prev = OP_prev(prev) ){
    if( !OP_refs_x87(prev) )
      continue;

    /* If the result is coming from a previous copy, then no fxch is
       necessary.
    */
    if( OP_copy(prev) ){
      TN* opnd = OP_opnd( prev, 0 );
      TN* result = OP_result( prev, 0 );
      if( TN_register(result) == stack->reg[st_idx] &&
	  TN_register(opnd) == stack->reg[stack->top] )
	return;
    }

    /* What else ??? */

    break;
  }

  OP* fxch_op = Mk_OP( TOP_fxch, dest );
  BB_Insert_Op_Before( OP_bb(op), op, fxch_op );

  Create_OP_stinfo( fxch_op );
  Alloc_Stack_Reg( stack, fxch_op, 0, st_idx );

  if( trace )
    Print_Stack( stack );
}


/* The <st_idx>th entry of <stack> need to be popped out. Create a
   pop insn before <point>.
 */
static void Pop_Stack( Stack* stack, int st_idx, OP* point, bool before_point )
{
  const REGISTER reg = stack->reg[st_idx];

  /* Copy the contents of %st(0) to %st(stack_idx) and then pop the
     x87 register stack.
  */
  TN* opnd = Build_RCLASS_TN( ISA_REGISTER_CLASS_x87 );
  Set_TN_register( opnd, reg );

  OP* pop_op = Mk_OP( TOP_fstp, opnd );
  if( before_point )
    BB_Insert_Op_Before( OP_bb(point), point, pop_op );
  else
    BB_Insert_Op_After( OP_bb(point), point, pop_op );

  Create_OP_stinfo( pop_op );
  Alloc_Stack_Reg( stack, pop_op, 0, st_idx );

  stack->reg[st_idx] = stack->reg[stack->top];
  stack->top--;
}


/* Generate pop insn in <bb> to pop up <stack_idx>, which is dead outside
   of <stack>.
*/
static void Pop_Stack_Reg( Stack* stack, int stack_idx, BB* bb )
{
  const REGISTER reg = stack->reg[stack_idx];

  /* Copy the contents of %st(0) to %st(stack_idx) and then pop the
     x87 register stack.
  */
  TN* opnd = Build_RCLASS_TN( ISA_REGISTER_CLASS_x87 );
  Set_TN_register( opnd, reg );
  OP* pop_op = Mk_OP( TOP_fstp, opnd );
  BB_Append_Op( bb, pop_op );    

  Create_OP_stinfo( pop_op );
  Alloc_Stack_Reg( stack, pop_op, 0, stack_idx );

  stack->reg[stack_idx] = stack->reg[stack->top];
  stack->top--;

  return;
}


static bool Stacks_Are_Equivalent( Stack* s1, Stack* s2 )
{
  if( s1->top != s2->top )
    return false;

  for( int i = 0; i < s1->top; i++ ){
    if( s1->reg[i] != s2->reg[i] )
      return false;
  }

  return true;
}


/* Put stack->reg[from] to stack->reg[to] in <bb>.
 */
static void Swap( int to, int from, BB* bb, Stack* stack )
{
  // First, put <from> to the top of the stack.
  if( stack->top != from ){
    const REGISTER reg = stack->reg[stack->top];
    stack->reg[stack->top] = stack->reg[from];
    stack->reg[from] = reg;

    TN* opnd = Build_RCLASS_TN( ISA_REGISTER_CLASS_x87 );
    Set_TN_register( opnd, stack->reg[stack->top] );
    OP* fxch_op = Mk_OP( TOP_fxch, opnd );
    BB_Append_Op( bb, fxch_op );

    Create_OP_stinfo( fxch_op );
    Alloc_Stack_Reg( stack, fxch_op, 0, from );
  }

  // Now, put the top of the stack to <to>.

  if( stack->top != to ){
    const REGISTER reg = stack->reg[stack->top];
    stack->reg[stack->top] = stack->reg[to];
    stack->reg[to] = reg;

    TN* opnd = Build_RCLASS_TN( ISA_REGISTER_CLASS_x87 );
    Set_TN_register( opnd, stack->reg[stack->top] );
    OP* fxch_op = Mk_OP( TOP_fxch, opnd );
    BB_Append_Op( bb, fxch_op );
  
    Create_OP_stinfo( fxch_op );
    Alloc_Stack_Reg( stack, fxch_op, 0, to );
  }
}


/* Create and insert a new bb for edge <from> to <to>.
 */
static BB* Insert_Compensate_BB( BB* from, BB* to )
{
  FmtAssert( BB_in_succs( from, to ), ("NYI") );

  BB* prev = BB_prev( to );
  const bool add_goto =
    (prev != from) && ( BB_Fall_Thru_Successor(prev) == to );
  BB* new_bb = Gen_And_Insert_BB_Before( to );

  new_bbs++;

  if( trace ){
    fprintf( TFile, "new bb:%d is created for edge %d -> %d\n",
	     BB_id(new_bb), BB_id(from), BB_id(to) );
  }

  BB_MAP_Set( BBs_Map, new_bb, BB_OP_MAP_Create( new_bb, pu_mem_pool ) );
  BB_MAP32_Set( dfs_map, new_bb, BB_MAP32_Get(dfs_map,from) );

  BB_freq( new_bb ) = BB_freq( to );
  if( !BB_Retarget_Branch( from, to, new_bb ) )
    Change_Succ( from, to, new_bb );
  Link_Pred_Succ_with_Prob( new_bb, to, 1.0F );

  if( add_goto ){
    OP* last = BB_last_op( prev );
    if( last != NULL && OP_cond( last ) ){
      // Create ft_bb here: from -> prev -> ft_bb -> new_bb -> to
      BB* ft_bb = Gen_And_Insert_BB_After( prev );

      BB_MAP_Set( BBs_Map, ft_bb, BB_OP_MAP_Create( ft_bb, pu_mem_pool ) );
      BB_MAP32_Set( dfs_map, ft_bb, BB_MAP32_Get(dfs_map,prev) );
      new_bbs++;

      if( trace ){
	fprintf( TFile, "new bb:%d is created for edge %d -> %d\n",
		 BB_id(ft_bb), BB_id(prev), BB_id(to) );
      }

      // Inherit stack from <prev>.
      Stack* ft_bb_stack = (Stack*)MEM_POOL_Alloc( &local_mem_pool,
						   sizeof(ft_bb_stack[0]) );
      const Stack* prev_stack = Get_Stack_Info( prev );
      *ft_bb_stack = *prev_stack;
      ft_bb_stack->stack_in = NULL;
      BB_MAP_Set( bb_stack_info_map, ft_bb, ft_bb_stack );

      BB_freq( ft_bb ) == BB_freq( to );
      if( !BB_Retarget_Branch( prev, to, ft_bb ) )
	Change_Succ( prev, to, ft_bb );
      //Link_Pred_Succ_with_Prob( ft_bb, to, 1.0F );

      Add_Goto( ft_bb, to );

    } else {
      Add_Goto( prev, to );
    }
  }

  return new_bb;
}


/* <pred> to <succ> is a back edge if <succ> is visited prior to <pred>.
 */
inline static bool Is_Back_Edge( BB* pred, BB* succ )
{
  FmtAssert( BB_MAP32_Get( dfs_map, pred ) > 0, ("Invalid dfs number.") );
  FmtAssert( BB_MAP32_Get( dfs_map, succ ) > 0, ("Invalid dfs number.") );

  return ( BB_MAP32_Get( dfs_map, pred ) >= BB_MAP32_Get( dfs_map, succ ) );
}


/* Given the input stack from <pred> and the current stack from <bb>,
   generate new bb to adjust the different stacks.
*/
static bool Compensate_Stack( BB* pred, BB* bb, bool is_back_edge )
{
  Stack* tgt_stack = Get_Stack_Info( bb );
  Stack* src_stack = Get_Stack_Info( pred );

  if( is_back_edge ){
    tgt_stack = tgt_stack->stack_in;
  }

  FmtAssert( tgt_stack->top != STACK_IS_WILD, ("NYI") );
  FmtAssert( src_stack->top != STACK_IS_WILD, ("NYI") );

  // No compensation is necessary if both stacks are identical.
  if( Stacks_Are_Equivalent( tgt_stack, src_stack ) )
    return false;

  FmtAssert( REGISTER_SET_ContainsP( src_stack->live_out, tgt_stack->live_in ),
	     ("NYI") );

  // Create new bb.
  BB* new_bb = Insert_Compensate_BB( pred, bb );

  // Insert any new stack adjustment code into new_bb.

  Stack stack = *src_stack;

  // Pop out those useless stack registers.

  for( int i = 0; i <= stack.top; i++ ){
    const REGISTER reg = stack.reg[i];
    if( !REGISTER_SET_MemberP( tgt_stack->live_in, reg ) ){
      Pop_Stack_Reg( &stack, i, new_bb );
      i--;
    }
  }

  FmtAssert( stack.top == tgt_stack->top, ("NYI") );

  // Now, sort the stack to be identical with tgt_stack.
  for( int i = 0; i <= stack.top; i++ ){
    if( stack.reg[i] == tgt_stack->reg[i] )
      continue;

    int j = i + 1;
    for( ; stack.reg[j] != tgt_stack->reg[i]; j++ )
      ;
	
    Swap( i, j, new_bb, &stack );
  }

  // The stack of <new_bb_stack> should be identical with <tgt_stack>.
  Stack* new_bb_stack =  (Stack*)MEM_POOL_Alloc( &local_mem_pool,
						 sizeof(new_bb_stack[0]) );
  *new_bb_stack = *tgt_stack;
  BB_MAP_Set( bb_stack_info_map, new_bb, new_bb_stack );

  return true;
}


/* Adjust the stack of <bb> from all the stacks of its predecessors.
 */
static void Adjust_Input_Stack( BB* bb, bool consider_back_edge )
{
  BBLIST* bblst = BB_preds( bb );

  while( bblst != NULL ){
    BB* pbb = BBLIST_item( bblst );

    if( BB_NOT_REACHABLE( pbb ) ){
      bblst = BBLIST_next( bblst );
      continue;      
    }

    const bool is_back_edge = Is_Back_Edge( pbb, bb );

    if( is_back_edge       &&
	consider_back_edge &&
	!VECTOR_Member_Element( bbs_vector, pbb ) ){

      Stack* pbb_stack = Get_Stack_Info( pbb );

      if( pbb_stack->top == STACK_IS_WILD ){

	/* <pbb> is a new created and not-yet-initialized bb.
	   Now inherit the stack from <grand_pa>.
	*/

	BB* grand_pa = BB_Unique_Predecessor( pbb );
	const Stack* grand_pa_stack = Get_Stack_Info( grand_pa );

	FmtAssert( grand_pa_stack->top != STACK_IS_WILD, ("NYI") );

	pbb_stack->top = grand_pa_stack->top;
	for( int i = 0; i <= grand_pa_stack->top; i++ )
	  pbb_stack->reg[i] = grand_pa_stack->reg[i];
      }
    }

    if( ( is_back_edge == consider_back_edge ) &&
	Compensate_Stack( pbb, bb, is_back_edge ) ){
      bblst = BB_preds( bb );

    } else {
      bblst = BBLIST_next( bblst );
    }
  }
}


/* Previous phases could introduce some control path that will never
   be executed. But cflow does not know that, and will identify some
   of the variables as being not defined before use.
   Here we introduce pseudo loads for those uses.
 */
static void Repair_Entry_BB( BB* bb )
{
  Stack* stack = Get_Stack_Info( bb );
  if( REGISTER_SET_EmptyP( stack->live_in ) )
    return;

  REGISTER reg;

  FOR_ALL_REGISTER_SET_members( stack->live_in, reg ){
    if( Get_Stack_Index( stack, reg ) < 0 ){
      DevWarn( "Repair_Entry_BB: pseudo load for ST(%d) is introduced.", reg-1 );

      TN* dest = Build_RCLASS_TN( ISA_REGISTER_CLASS_x87 );
      Set_TN_register( dest, reg );

      OP* load_op = Mk_OP( TOP_fldz, dest );
      BB_Prepend_Op( bb, load_op );

      stack->live_in = REGISTER_SET_Difference1( stack->live_in, reg );
    }
  } 
}


static void Repair_Call_BB( BB* bb )
{
  Stack* stack = Get_Stack_Info( bb );
  if( REGISTER_SET_EmptyP( stack->live_out ) )
    return;

  REGISTER_SET reg_set = REGISTER_SET_EMPTY_SET;
  REGISTER_SET bb_live_out = REGISTER_SET_EMPTY_SET;
  REGISTER reg;

  // Compute the real set of live_out for <bb>.

  const ANNOTATION* ant = ANNOT_Get( BB_annotations(bb), ANNOT_CALLINFO );
  const CALLINFO* call_info = ANNOT_callinfo(ant);
  const ST* call_st = CALLINFO_call_st(call_info);
  const WN* call_wn = CALLINFO_call_wn(call_info);
  const TY_IDX call_ty = call_st != NULL ? ST_pu_type(call_st) : WN_ty(call_wn);
  const RETURN_INFO return_info = Get_Return_Info( TY_ret_type(call_ty),
						   No_Simulated,
		       call_st ? PU_ff2c_abi(Pu_Table[ST_pu(call_st)]) : FALSE);

  for( int i = 0; i < RETURN_INFO_count(return_info); i++ ){
    const TYPE_ID type = RETURN_INFO_mtype( return_info, i );
    if( MTYPE_refs_x87( type ) ) {
      const PREG_NUM retpreg = RETURN_INFO_preg (return_info, i);
      ISA_REGISTER_CLASS cl;

      if( !CGTARG_Preg_Register_And_Class( retpreg, &cl, &reg) )
	FmtAssert( false, ("NYI") );

      bb_live_out = REGISTER_SET_Union1( bb_live_out, reg );
    }
  }

  FOR_ALL_REGISTER_SET_members( stack->live_out, reg ){
    if( !REGISTER_SET_MemberP( bb_live_out, reg ) ){
      reg_set = REGISTER_SET_Union1( reg_set, reg );
    }
  }

  if( REGISTER_SET_EmptyP( reg_set ) )
    return;

  // Insert any loads into new_bb.
  BB* new_bb = Insert_Compensate_BB( bb, BB_next(bb) );
  Stack* new_bb_stack = (Stack*)MEM_POOL_Alloc( &local_mem_pool,
						sizeof(new_bb_stack[0]) );
  *new_bb_stack = *stack;
  new_bb_stack->stack_in = NULL;
  new_bb_stack->top = STACK_IS_EMPTY;

  stack->live_out = bb_live_out;

  FOR_ALL_REGISTER_SET_members( stack->live_out, reg ){
    new_bb_stack->reg[++new_bb_stack->top] = reg;
  }

  FOR_ALL_REGISTER_SET_members( reg_set, reg ){
    DevWarn( "Repair_Call_BB: pseudo load for ST(%d) is introduced.", reg-1 );

    TN* dest = Build_RCLASS_TN( ISA_REGISTER_CLASS_x87 );
    Set_TN_register( dest, reg );

    OP* load_op = Mk_OP( TOP_fldz, dest );
    BB_Append_Op( new_bb, load_op );

    new_bb_stack->reg[++new_bb_stack->top] = reg;
  }

  new_bb_stack->live_in = stack->live_out;

  BB_MAP_Set( bb_stack_info_map, new_bb, new_bb_stack );
}


/* Inherit stack from the predecessor of <bb>.
 */
static void Initialize_Stack( BB* bb )
{
  /* Repair an entry bb or a call bb whenever necessary by introducing
     fake load and store to balance the x87 stack.
     Some of the problems are caused by control flow optimizations,
     and some of them are caused by the front-end that x87 fails to handle.
     (bug#2469)
   */
  {
    if( BB_preds( bb ) == NULL )
      Repair_Entry_BB( bb );

    if( BB_call( bb ) )
      Repair_Call_BB( bb );
  }

  Stack* tgt_stack = Get_Stack_Info( bb );

  if( tgt_stack->top != STACK_IS_WILD ){
    return;
  }

  BBLIST* bblst = NULL;
  BB* pred = NULL;
  float prob = 0.0;

  if( trace ){
    FOR_ALL_BB_PREDS( bb, bblst ){
      BB* pbb = BBLIST_item(bblst);

      if( BB_NOT_REACHABLE( pbb ) )
	continue;

      if( Is_Back_Edge( pbb, bb ) )
	continue;

      Stack* stack = Get_Stack_Info( pbb );

      FmtAssert( stack->top != STACK_IS_WILD, ("NYI") );
      fprintf( TFile, "Input stack from bb:%d\n", BB_id(pbb) );
      Print_Stack( stack );
    }
  }

  /* TODO:
     For the current basic block, the reference input stack is coming
     from the first predecessor. A better choice is to pick the predecessor
     that contributes the highest edge probability.
  */
  FOR_ALL_BB_PREDS( bb, bblst ){
    BB* pbb = BBLIST_item(bblst);

    if( BB_NOT_REACHABLE( pbb ) )
      continue;

    if( !Is_Back_Edge( pbb, bb ) ){
      pred = pbb;
      break;
    }
  }

  Stack* src_stack = Get_Stack_Info( pred );
  tgt_stack->top = STACK_IS_EMPTY;

  /* A simple stack copy if no extra work is required.
   */

  for( int i = 0; i <= src_stack->top; i++ ){
    const REGISTER reg = src_stack->reg[i];
    if( REGISTER_SET_MemberP( tgt_stack->live_in, reg ) ){
      tgt_stack->reg[++tgt_stack->top] = reg;
    }
  }

  Adjust_Input_Stack( bb, false );

  return;
}


static bool Check_Consistency( Stack* stack, REGISTER_SET set )
{
  if( stack->top == STACK_IS_WILD )
    return false;

  REGISTER reg;

  for( int i = 0; i <= stack->top; i++ ){
    reg = stack->reg[i];
    if( !REGISTER_SET_MemberP( set, reg ) ){
      if( trace ){
	fprintf( TFile, "%s does not appear in the register set\n",
		 REGISTER_name( ISA_REGISTER_CLASS_x87, reg ) );
      }
      return false;
    }
  }

  FOR_ALL_REGISTER_SET_members( set, reg ){
    if( Get_Stack_Index( stack, reg ) < 0 ){
      if( trace ){
	fprintf( TFile, "%s does not appear in the stack\n",
		 REGISTER_name( ISA_REGISTER_CLASS_x87, reg ) );
      }
      return false;
    }
  }

  return true;
}


/* Handle <op> which is an inline assembly code.
   <stack> should be adjusted in this routine.
*/
static void Handle_Asm( Stack* stack, OP* op )
{
  // For operands.

  for( int i = 0; i < OP_opnds( op ); i++ ){
    TN* opnd = OP_opnd( op, i );
    if( !TN_is_register( opnd ) ||
	TN_register_class(opnd) != ISA_REGISTER_CLASS_x87 )
      continue;

    int st_idx = Get_Stack_Index( stack, TN_register(opnd) );
    FmtAssert( st_idx >= 0, ("NYI") );

    /* bug#1646
       For x87 inline asm, if the opnd and result are the same register,
       then always put the opnd to the top of the x87 stack.
     */
    if( st_idx != stack->top &&
	OP_Defs_Reg( op, TN_register_class(opnd), TN_register(opnd) ) ){
      Expand_Fxch( stack, op, opnd );
      st_idx = stack->top;
    }

    Alloc_Stack_Reg( stack, op, i, st_idx );

    if( !OP_Defs_Reg( op, TN_register_class(opnd), TN_register(opnd) ) &&
	Is_Opnd_Last_Use( stack, op, opnd ) ){
      Pop_Stack( stack, st_idx, op, false );
    }
  }

  // For results.

  for( int i = 0; i < OP_results( op ); i++ ){
    TN* result = OP_result( op, i );
    if( TN_register_class(result) != ISA_REGISTER_CLASS_x87 )
      continue;

    int st_idx = Get_Stack_Index( stack, TN_register(result) );
    if( st_idx < 0 ){
      stack->reg[++stack->top] = TN_register( result );
      FmtAssert( stack->top < X87_STACK_SIZE, ("x87 stack overflows.") );
      st_idx = stack->top;
    }

    Alloc_Stack_Reg( stack, op, -(i+1), st_idx );

    if( Is_Result_Last_Use( stack, op, result ) ){
      Pop_Stack( stack, st_idx, op, false );
    }
  }
}


static void Convert_Regs( BB* bb )
{
  Initialize_Stack( bb );

  Stack* stack = Get_Stack_Info( bb );
  *stack->stack_in = *stack;

  if( !Check_Consistency( stack, stack->live_in ) ){
    FmtAssert( false, ("x87 stack is inconsistent") );
  }

  OP* next = NULL;

  for( OP* op = BB_first_op(bb); op != NULL; op = next ){
    next = OP_next( op );

    if( !OP_refs_x87(op) )
      continue;

    if( trace ){
      Print_OP_No_SrcLine( op );
      Print_Stack( stack );
    }

    // Set up stinfo for each relevant op.
    Create_OP_stinfo( op );

    const TOP top = OP_code( op );

    // an ASM
    if( top == TOP_asm ){
      Handle_Asm( stack, op );

      continue;
    }

    // load/store x87 control-word
    if( top == TOP_fldcw || top == TOP_fnstcw ){
      continue;
    }

    // a CALL
    if( OP_call( op ) ){
      FmtAssert( stack->top == STACK_IS_EMPTY, ("x87 stack is non-empty") );

      const ANNOTATION* ant = ANNOT_Get( BB_annotations(OP_bb(op)), ANNOT_CALLINFO );
      const CALLINFO* call_info = ANNOT_callinfo(ant);
      const ST* call_st = CALLINFO_call_st(call_info);
      const WN* call_wn = CALLINFO_call_wn(call_info);
      const TY_IDX call_ty = call_st != NULL ? ST_pu_type(call_st) : WN_ty(call_wn);

      Is_True( WHIRL_Return_Info_On, ("whirl return info is off") );
      const RETURN_INFO return_info = Get_Return_Info( TY_ret_type(call_ty),
						       No_Simulated,
		       call_st ? PU_ff2c_abi(Pu_Table[ST_pu(call_st)]) : FALSE);

      REGISTER reg = FIRST_STACK_REG + RETURN_INFO_count(return_info) - 1;

      for( int i = 0; i < RETURN_INFO_count(return_info); i++ ){
	const TYPE_ID type = RETURN_INFO_mtype( return_info, i );

        if( MTYPE_refs_x87( type ) ) {
	  if( !REGISTER_SET_MemberP( stack->live_out, reg ) )
	    stack->live_out = REGISTER_SET_Union1( stack->live_out, reg );
	  stack->reg[++stack->top] = reg--;
	}
      }

      FmtAssert( stack->top < X87_STACK_SIZE, ("x87 stack overflows.") );

      continue;
    }

    // a PUSH
    if( OP_load( op ) ||
	top == TOP_fldz ){
      const REGISTER reg = TN_register( OP_result(op,0) );
      FmtAssert( Get_Stack_Index( stack, reg ) < 0, ("NYI") );
      
      stack->reg[++stack->top] = reg;
      FmtAssert( stack->top < X87_STACK_SIZE, ("x87 stack overflows.") );
      Alloc_Stack_Reg( stack, op, -1, stack->top );

      if( Is_Result_Last_Use( stack, op, OP_result(op,0) ) ){
	if( next == NULL )
	  Pop_Stack( stack, stack->top, BB_last_op(bb), false );
	else
	  Pop_Stack( stack, stack->top, next, true );
      }

      continue;
    }

    // a POP
    if( OP_store( op ) ){
      const int idx = OP_find_opnd_use( op, OU_storeval );
      TN* opnd = OP_opnd( op, idx );

      if( stack->reg[stack->top] != TN_register(opnd) ){
	Expand_Fxch( stack, op, opnd );
      }

      Alloc_Stack_Reg( stack, op, idx, stack->top );

      if( Is_Opnd_Last_Use( stack, op, opnd ) ){
	const REGISTER reg = TN_register( opnd );
	stack->top--;
	//stack->reg_set = REGISTER_SET_Difference1( stack->reg_set, reg );	
	
      } else {
	const TOP new_top = Pop_Table[OP_code(op)];

	if( new_top != TOP_UNDEFINED ){
	  OP_Change_Opcode( op, new_top );

	} else {
	  /* TOP_fistpll and TOP_fstpt do not have a non-pop version.
	     A fld will consume one x87 register.
	  */

	  if( stack->top + 1 >= X87_STACK_SIZE ){
	    // load it back from stack
	    TN* storeval = OP_opnd( op, OP_find_opnd_use( op, OU_storeval ) );
	    const int base_indx = OP_find_opnd_use( op, OU_base );
	    TN* base = ( base_indx >= 0 ) ? OP_opnd( op, base_indx ) : NULL;
	    TN* offset = OP_opnd( op, OP_find_opnd_use( op, OU_offset ) );
	    TN* dest = Build_TN_Like( storeval );
	    Set_TN_register( dest, TN_register( storeval ) );
	    OP* load_op = ( base != NULL )
	      ? Mk_OP( TOP_fldt, dest, base, offset )
	      : Mk_OP( TOP_fldt_n32, dest, offset );

	    BB_Insert_Op_After( bb, op, load_op );
	    Create_OP_stinfo( load_op );

	    stack->top--;
	    next = load_op;

	  } else {
	    // load it back from x87 reg. stack
	    OP* push_op = Mk_OP( TOP_fld, opnd );
	    BB_Insert_Op_Before( bb, op, push_op );

	    Create_OP_stinfo( push_op );
	    Alloc_Stack_Reg( stack, push_op, 0, stack->top );
	  }
	}
      }

      continue;
    }

    // a COPY or MOVE
    if( OP_copy( op ) ||
	TOP_is_move(top) ){
      // First handle the opnd.
      const REGISTER src_reg = TN_register( OP_opnd(op,0) );
      const int st_idx0 = Get_Stack_Index( stack, src_reg );
      Alloc_Stack_Reg( stack, op, 0, st_idx0 );

      // Now handle the result.
      const REGISTER reg = TN_register( OP_result(op,0) );
      if( reg == src_reg ){
	BB_Remove_Op( bb, op );
	continue;
      }

      FmtAssert( Get_Stack_Index( stack, reg ) < 0, ("NYI") );

      stack->reg[++stack->top] = reg;
      FmtAssert( stack->top < X87_STACK_SIZE, ("x87 stack overflows.") );
      Alloc_Stack_Reg( stack, op, -1, stack->top );

      if( Is_Result_Last_Use( stack, op, OP_result(op,0) ) ){
	if( next == NULL )
	  Pop_Stack( stack, stack->top, BB_last_op(bb), false );
	else
	  Pop_Stack( stack, stack->top, next, true );
      }

      if( Is_Opnd_Last_Use( stack, op, OP_opnd(op,0) ) ){
	if( next == NULL )
	  Pop_Stack( stack, st_idx0, BB_last_op(bb), false );
	else
	  Pop_Stack( stack, st_idx0, next, true );
      }

      continue;
    }

    // a CMP op
    if( OP_icmp( op ) ){
      TN* opnd0 = OP_opnd( op, 0 );
      TN* opnd1 = OP_opnd( op, 1 );

      int st_idx0 = Get_Stack_Index( stack, TN_register(opnd0) );
      int st_idx1 = Get_Stack_Index( stack, TN_register(opnd1) );
      FmtAssert( st_idx0 >= 0 && st_idx1 >= 0, ("NYI") );

      // Make sure opnd0 is at the top.
      if( st_idx0 != stack->top ){
	Expand_Fxch( stack, op, opnd0 );
	st_idx0 = stack->top;
	st_idx1 = Get_Stack_Index( stack, TN_register(opnd1) );
      }

      Alloc_Stack_Reg( stack, op, 0, st_idx0 );
      Alloc_Stack_Reg( stack, op, 1, st_idx1 );

      if( Is_Opnd_Last_Use( stack, op, opnd0 ) ){
	// Pop out the dead opnd0.
	if( next == NULL )
	  Pop_Stack( stack, st_idx0, BB_last_op(bb), false );
	else
	  Pop_Stack( stack, st_idx0, next, true );
      }

      if( Is_Opnd_Last_Use( stack, op, opnd1 ) ){
	st_idx1 = Get_Stack_Index( stack, TN_register(opnd1) );
	if( next == NULL )
	  Pop_Stack( stack, st_idx1, BB_last_op(bb), false );
	else
	  Pop_Stack( stack, st_idx1, next, true );
      }

      continue;
    }

    // a CMOV op
    if( OP_cond_move( op ) ){
      TN* result = OP_result( op, 0 );
      int st_result = Get_Stack_Index( stack, TN_register(result) );
      FmtAssert( st_result >= 0, ("NYI") );
      // Make sure result is at the top.

      if( st_result != stack->top ){
	Expand_Fxch( stack, op, result );
	st_result = stack->top;
      }

      Alloc_Stack_Reg( stack, op, -1, st_result );

      TN* opnd0 = OP_opnd( op, 0 );
      int st_idx0 = Get_Stack_Index( stack, TN_register(opnd0) );
      Alloc_Stack_Reg( stack, op, 0, st_idx0 );

      if( Is_Opnd_Last_Use( stack, op, opnd0 ) ){
	if( !TNs_Are_Equivalent( result, opnd0 ) ||
	    Is_Result_Last_Use( stack, op, result ) )
	  Pop_Stack( stack, st_idx0, op, false );
      }

      continue;
    }

    // an ALU op
    if( OP_fadd( op ) ||
	OP_fsub( op ) ||
	OP_fdiv( op ) ||
	OP_fmul( op ) ){
      TN* opnd0 = OP_opnd( op, 0 );
      TN* opnd1 = OP_opnd( op, 1 );
      TN* result = OP_result( op, 0 );
      bool pop_stack = false;

      int st_idx0 = Get_Stack_Index( stack, TN_register(opnd0) );
      int st_idx1 = Get_Stack_Index( stack, TN_register(opnd1) );
      FmtAssert( st_idx0 >= 0 && st_idx1 >= 0, ("NYI") );

      // One of the opnds must be at ST(0).
      if( st_idx0 != stack->top &&
	  st_idx1 != stack->top ){
	Expand_Fxch( stack, op, opnd0 );
	st_idx0 = stack->top;
	st_idx1 = Get_Stack_Index( stack, TN_register(opnd1) );
      }

      Alloc_Stack_Reg( stack, op, 0, st_idx0 );
      Alloc_Stack_Reg( stack, op, 1, st_idx1 );

      if( st_idx0 == st_idx1 ){
	Alloc_Stack_Reg( stack, op, -1, st_idx0 );

      } else if( Is_Opnd_Last_Use( stack, op, opnd1 ) ){
	if( st_idx1 == stack->top ){
	  Alloc_Stack_Reg( stack, op, -1, st_idx0 );

	} else {
	  Alloc_Stack_Reg( stack, op, -1, st_idx1 );
	  stack->reg[st_idx1] = stack->reg[stack->top];

	  // Make sure opnd0 and result have the same register.
	  if( OP_x86_style( op ) &&
	      ( st_idx1 != st_idx0 ) ){
	    // Use the reverse version for non-commutative operations.
	    if( !TOP_is_commutative( OP_code(op) ) ){
	      OP_Change_Opcode( op, Reverse_Table[OP_code(op)] );
	    }

	    Alloc_Stack_Reg( stack, op, 0, st_idx1 );
	    Alloc_Stack_Reg( stack, op, 1, st_idx0 );
	  }
	}

	pop_stack = true;

	const TOP new_top = Pop_Table[OP_code(op)];
	OP_Change_Opcode( op, new_top );

      } else if( Is_Opnd_Last_Use( stack, op, opnd0 ) ){
	Alloc_Stack_Reg( stack, op, -1, st_idx0 );

      } else {
	Alloc_Stack_Reg( stack, op, -1, st_idx0 );
      }

      /* gcc generates floating point instructions with reversed source
	 and destination registers in certain cases. So we have to stuck
	 with it.

	 For example, 
	        fsub %st,%st(3)
         results in `%st(3) = %st - %st(3)' rather than the expected
	 `%st(3) = %st(3) - %st' format.

	 This happens with all the non-commutative arithmetic floating point
	 operations with two register operands where
	 the source register is `%st' and the destination register is `%st(i)'.
      */

      if( !TOP_is_commutative( OP_code(op) ) &&
	  OP_opnds( op ) == 2                &&
	  Get_Stack_Index( stack, TN_register(OP_result(op,0)) ) != stack->top ){
	OP_Change_Opcode( op, Reverse_Table[OP_code(op)] );
      }

      if( pop_stack )
	stack->top--;

      continue;
    }

    // an FCHS or ABS
    if( top == TOP_fchs    ||
	top == TOP_fabs    ||
	top == TOP_frndint ||
	top == TOP_fsqrt   ||
	top == TOP_fcos    ||
	top == TOP_fsin ){
      TN* opnd0 = OP_opnd( op, 0 );
      const int st_idx0 = Get_Stack_Index( stack, TN_register(opnd0) );

      if( st_idx0 != stack->top ){
	Expand_Fxch( stack, op, opnd0 );
      }

      // It must operate on the top of the stack.
      Alloc_Stack_Reg( stack, op, 0,  stack->top );
      Alloc_Stack_Reg( stack, op, -1, stack->top );

      continue;
    }

    FmtAssert( false, ("Unknown x87 operation %s", TOP_Name(OP_code(op))) );
  }

  /* Before a return happens, a %st(0) register means its value should really be
     stored at the top of the register stack, like complex data type.
  */
  if( BB_kind(bb) == BBKIND_RETURN &&
      stack->top == 1 ){
    OP* last = BB_last_op(bb);
    FmtAssert( OP_code(last) == TOP_ret, ("NYI") );
    TN* dest = Build_RCLASS_TN( ISA_REGISTER_CLASS_x87 );
    Set_TN_register( dest, FIRST_STACK_REG );

    Expand_Fxch( stack, last, dest );
  }

  if( !Check_Consistency( stack, stack->live_out ) ){
    FmtAssert( false, ("x87 stack is inconsistent.") );
  }
}


static bool PU_has_x87_op()
{
  for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next( bb ) ){
    for( OP* op = BB_first_op(bb); op != NULL; op = OP_next(op) ){
      if( OP_refs_x87( op ) )
	return true;
    }
  }

  return false;
}


/* Setup live_in and live_out information for each bb.
 */
static void Register_Liveness_Analysis()
{
  REG_LIVE_Analyze_Region();

  const ISA_REGISTER_CLASS cl = ISA_REGISTER_CLASS_x87;

  for( int bb_idx = 0; bb_idx < VECTOR_count(bbs_vector); bb_idx++ ){
    BB* bb = (BB*)VECTOR_element( bbs_vector, bb_idx );
    Stack* stack = Get_Stack_Info( bb );

    for( REGISTER reg = FIRST_STACK_REG;
	 reg <= REGISTER_CLASS_last_register(cl);
	 reg++ ){
      // live in
      if( REG_LIVE_Into_BB( cl, reg, bb ) )
	stack->live_in = REGISTER_SET_Union1( stack->live_in, reg );
      
      // live out
      if( REG_LIVE_Outof_BB( cl, reg, bb ) )
	stack->live_out = REGISTER_SET_Union1( stack->live_out, reg );
    }

    if( trace ){
      REGISTER reg;

      fprintf( TFile, "BB:%d live_in: ", BB_id(bb) );
      FOR_ALL_REGISTER_SET_members( stack->live_in, reg ){
	fprintf( TFile, " %s", REGISTER_name( cl, reg ) );
      }
      fprintf( TFile, "\n" );

      fprintf( TFile, "BB:%d live_out: ", BB_id(bb) );
      FOR_ALL_REGISTER_SET_members( stack->live_out, reg ){
	fprintf( TFile, " %s", REGISTER_name( cl, reg ) );
      }
      fprintf( TFile, "\n" );
    }
  }  

  REG_LIVE_Finish();
}


/* Perform a depth-first-search, and update <vector> in
   post-order.
*/
static int dfs( BB* bb, int max_id, VECTOR vector )
{
  Is_True( BB_MAP32_Get(dfs_map, bb) == 0,
	   ("dfs visited BB:%d twice", BB_id(bb)));

  BB_MAP32_Set( dfs_map, bb, ++max_id );

  /* Recursively visit (once) all the successors of this bb in the region.
   */
  BBLIST* succs = NULL;

  FOR_ALL_BB_SUCCS( bb, succs ){
    BB* succ = BBLIST_item(succs);
    if( BB_MAP32_Get(dfs_map, succ) == 0 )
      max_id = dfs( succ, max_id, vector );
  }

  VECTOR_Add_Element( vector, bb );

  return max_id;
}


static void Create_bbs_vector()
{
  bbs_vector = VECTOR_Init( PU_BB_Count, &local_mem_pool );

  /* Can not use Entry_BB_Head, which does not include unreachable BBs. */
  BB_LIST* Entry = NULL;

  for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next( bb ) ){
    if( BB_preds(bb) == NULL )
      Entry = BB_LIST_Push( bb, Entry, &local_mem_pool );
  }

  /* Topological sort works only on directed acyclic graphs. If the graph is
     cyclic, no topological order exists. Thus, we should not consider any
     back-edges here.
   */

  int max_id = 0;

  for( BB_LIST* entries = Entry; entries != NULL; entries = BB_LIST_rest(entries) ){
    max_id = dfs( BB_LIST_first(entries), max_id, bbs_vector );
  }

  VECTOR_count( bbs_vector ) = max_id;

  /* Now, put bbs_vector in the right order. */

  int from = 0;
  int to = VECTOR_count( bbs_vector ) - 1;

  /* In the dfs() routine, dfs_map is used to mark a bb has been visited only.
     Here, dfs_map is updated to give the topological order for Is_Back_Edge().
  */

  while( from <= to ){
    BB* f_b = (BB*)VECTOR_element( bbs_vector, from );
    BB* t_b = (BB*)VECTOR_element( bbs_vector, to );

    VECTOR_element( bbs_vector, from ) = t_b;
    VECTOR_element( bbs_vector, to ) = f_b;

    BB_MAP32_Set( dfs_map, t_b, from+1 );
    BB_MAP32_Set( dfs_map, f_b, to+1 );

    from++;
    to--;
  }

  if( trace ){
    fprintf( TFile, "BBs visiting order: " );

    for( int bb_idx = 0; bb_idx < VECTOR_count(bbs_vector); bb_idx++ ){
      BB* bb = (BB*)VECTOR_element( bbs_vector, bb_idx );
      FmtAssert( bb_idx + 1 == BB_MAP32_Get(dfs_map,bb), ("NYI") );
      fprintf( TFile, "%d ", BB_id(bb) );
    }
    
    fprintf( TFile, "\n" );
  }
}


void Convert_x87_Regs( MEM_POOL* _mem_pool )
{
  // Back off if no x87 stack reg is referred.
  if( !PU_has_x87_op() )
    return;

  const char* prev_phase = Get_Error_Phase();
  Set_Error_Phase( "Converting x87 stack registers" );

  pu_mem_pool = _mem_pool;

  MEM_POOL_Initialize( &local_mem_pool, "CG_Convert_x87_Pool", TRUE );
  MEM_POOL_Push( &local_mem_pool );

  /* Create and sort all the BBs in "topological" order in bbs_vector.
   */
  dfs_map = BB_MAP32_Create();
  Create_bbs_vector();

  /* Initialize stack for each interested bb.*/

  Init_Pop_Table();
  new_bbs = 0;
  bb_stack_info_map = BB_MAP_Create();

  for( int bb_idx = 0; bb_idx < VECTOR_count(bbs_vector); bb_idx++ ){
    BB* bb = (BB*)VECTOR_element( bbs_vector, bb_idx );
    Stack* stack = (Stack*)MEM_POOL_Alloc( &local_mem_pool, sizeof(stack[0]) );

    stack->top = BB_preds( bb ) == NULL ? STACK_IS_EMPTY : STACK_IS_WILD;
    stack->live_in  = REGISTER_SET_EMPTY_SET;
    stack->live_out = REGISTER_SET_EMPTY_SET;
    for( int i = sizeof(stack->reg) / sizeof(stack->reg[0]) - 1; i >= 0; i-- ){
      stack->reg[i] = REGISTER_UNDEFINED;
    }

    stack->stack_in = (Stack*)MEM_POOL_Alloc( &local_mem_pool, sizeof(stack[0]) );
    stack->stack_in->top = STACK_IS_WILD;

    BB_MAP_Set( bb_stack_info_map, bb, stack );
  }

  Register_Liveness_Analysis();

  /* Scan all the BBs of the current PU. */

  for( int bb_idx = 0; bb_idx < VECTOR_count(bbs_vector); bb_idx++ ){
    BB* bb = (BB*)VECTOR_element( bbs_vector, bb_idx );
    BB_Update_OP_Order( bb );
    BB_MAP_Set( BBs_Map, bb, BB_OP_MAP_Create( bb, pu_mem_pool ) );

    Convert_Regs( bb );
  }

  for( int bb_idx = 0; bb_idx < VECTOR_count(bbs_vector); bb_idx++ ){
    BB* bb = (BB*)VECTOR_element( bbs_vector, bb_idx );
    Adjust_Input_Stack( bb, true );
  }


  BB_MAP_Delete( bb_stack_info_map );
  BB_MAP_Delete( dfs_map );

  MEM_POOL_Pop( &local_mem_pool );
  MEM_POOL_Delete( &local_mem_pool );
  Set_Error_Phase( prev_phase );
}
