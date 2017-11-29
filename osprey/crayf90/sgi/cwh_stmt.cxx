/*
 * Copyright (C) 2009, 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



/* ====================================================================
 * ====================================================================
 *
 * Module: cwh_stmt
 * $Revision: 1.21 $
 * $Date: 05/10/03 14:30:58-07:00 $
 * $Author: scorrell@soapstone.internal.keyresearch.com $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: contains routines to convert statements
 *              from Cray IR to WHIRL. Entry points from
 *              PDGCS layer are
 * 
 *             fei_stmt - start of new statement.
 *	       fei_user_code_start - begin of user statements
 *             fei_object_ref - reference to object
 *             fei_seg_ref - reference to segment
 *             fei_namelist_ref - reference to namelist
 *             fei_member_ref   - reference to derived type component.
 *             fei_constant     - reference to constant.
 *             fei_push_arith_con - reference to constant.
 *             fei_push_pattern_con - reference to string or byte string
 *             fei_function_ref - reference to procedure for call.
 *             fei_label_ref - reference to a a label 
 *
 *             these routines generally push an ST/WN/STR item on the
 *             stack for later processing. Statement level operations are
 *   
 *             fei_store - save rhs in lhs.
 *             fei_non_conform_store - save 1d object in nd, or vice versa.
 *             fei_call - make a procedure call
 *             fei_arg_addr - make an address for a procedure argument.
 *             feI_fcd  - turn cray pointer ref into strinfg reference
 *             fei_addr_con - generate the address of a constant
 *             fei_entry_pt - generate an alternate entry point
 *             fei_goto  - create a goto.
 *             fei_arith_goto  - create arithmetic IF gotos.
 *             fei_label_addr - create index for assign stmt.
 *             fei_indirect_goto - use table for assign and computed goto.
 *             fei_new_select - create a select case
 *             fei_label_def_named - define a label within the code.
 *             fei_brtrue - create a branch on TRUE
 *             fei_where - the WHERE statement, TRUE only.
 *             fei_return - a return statement
 *             fei_concat - create an OPC_CASSIGNMENT for concatenation
 *             fei_doloop - create an DOLOOP statemnet
 *             fei_dowhile - create an DOWHILE statement
 *             fei_doforever - create an DOWHILE TRUE  statement
 *             fei_enddo - back to parent block at end of DO loop.
 *             fei_allocate - the (DE) ALLOCATE statement.
 *
 *
 * ====================================================================
 * ====================================================================
 */

static const char *source_file = __FILE__;


/* sgi includes */

#include "defs.h"
#include "glob.h"  
#include "stab.h"
#include "strtab.h"
#include "errors.h"
#include "targ_const.h"
#include "config_targ.h"  
#include "config_debug.h"  
#include "const.h"
#include "pu_info.h"
#include "wn.h"
#include "wn_util.h"
#include "f90_utils.h"
#include "targ_sim.h"
#ifdef KEY /* Bug 4260 */
#include "../../../clibinc/cray/io_byteswap.h"
#endif /* KEY Bug 4260 */

/* FE includes */

#include "i_cvrt.h"

/* conversion includes */

#include "cwh_defines.h"
#include "cwh_addr.h"
#include "cwh_block.h"
#include "cwh_expr.h"
#include "cwh_stk.h"
#include "cwh_types.h"
#include "cwh_preg.h"
#include "cwh_stab.h"
#include "cwh_auxst.h"
#include "cwh_intrin.h"
#include "cwh_stmt.h"
#include "cwh_dst.h"
#include "cwh_directive.h"
#include "cwh_preg.h"
#include "sgi_cmd_line.h"

#include "cwh_stmt.i"

#include <libgen.h>	/* for dirname */


/*===============================================
 *
 * fei_stmt
 *
 * Initialize data structures for WHIRL conversion
 * at the start of each statement.
 *
 * Set the current line number.
 *
 *===============================================
 */ 
/*ARGSUSED*/
extern void
fei_stmt(INT32  lineno,
	 INT32  stmt_character_flag )
{

  if (lineno) {

    cwh_stmt_init_srcpos(lineno);

    /* Insert any deferred statements */

    cwh_block_append_given(Defer_Block);
   } 
}

/*===============================================
 *
 * fei_user_code_start
 *
 * Marks the beginning of user statements & end
 * of FE generated preamble (ie: saves to temps
 * for decls). Add whirl built for declaration 
 * or pragma processing processing.  
 *
 *===============================================
 */ 
extern void
fei_user_code_start(void)
{
  still_in_preamble = FALSE;
  cwh_block_append_given(Preamble_Block);
  cwh_stmt_add_pragma(WN_PRAGMA_PREAMBLE_END);
  cwh_block_append_given(First_Block);
  (void) cwh_block_toggle_debug(TRUE) ;

  cwh_stk_verify_empty();
}

/*===============================================
 *
 * fei_object_ref
 *
 * Push a reference to an object (an ST) 
 * on the expression stack. It may be an
 * lvalue, so don't fetch it.
 *
 *===============================================
 */ 
/*ARGSUSED*/
extern void
fei_object_ref (INTPTR  sym_idx,
		INT32	whole_array,
		INT32	whole_substring )
{
  STB_pkt *p ;

  p = cast_to_STB(sym_idx);
  DevAssert((p->form == is_ST),("Odd object ref"));

  ST * st = cast_to_ST(p->item);
  DevAssert((st),("null st"));

  if (whole_array) {
    cwh_stk_push(st,ST_item_whole_array) ;
  } else {
    cwh_stk_push(st,ST_item) ;
  }
}

/*===============================================
 *
 * fei_seg_ref
 *
 * Push a reference to an segment (eg. common block) (an ST) 
 * on the expression stack. 
 *
 *===============================================
 */ 
extern void
fei_seg_ref (INTPTR   sym_idx )
{
  STB_pkt *p ;

  p = cast_to_STB(sym_idx);
  DevAssert((p->form == is_ST),("Odd seg ref"));

  ST * st = cast_to_ST(p->item);
  DevAssert((st),("null st"));

  cwh_stk_push(st,ST_item) ;
}

/*===============================================
 *
 * fei_namelist_ref
 *
 * Push a reference to a namelist item (an ST) 
 * on the expression stack. 
 *
 *===============================================
 */ 
void
fei_namelist_ref (INTPTR   sym_idx )
{
  fei_object_ref(sym_idx, 0, 0);
}

/*===============================================
 *
 * fei_member_ref
 *
 * Push a reference to an derived type
 * component on the expression stack. The
 * object (variable) will be TOS, or under other
 * FLD_items.
 *
 *===============================================
 */ 
extern void
fei_member_ref (INTPTR   sym_idx )
{

  cwh_stk_push(cast_to_void(sym_idx),FLD_item) ;
}

/*===============================================
 *
 * fei_constant
 *
 * Push a reference to a constant on the 
 * expression stack.
 *
 * If it's an Arith_con, then the value is passed
 * just create the ST, push a WN on the stack and
 * pass back the WN for later use. If it's an 
 * integral type, there couldn't be an ST, so a
 * WN was created instead. The result will be in
 * a packet.
 * 
 * For a string(pattern) const we push the size
 * too and make it into a STR_item. PCONST_items 
 * are bit strings used for initialization, mostly.
 * Just push those.
 *
 *===============================================
 */ 
extern INTPTR
fei_constant ( TYPE            type,
               INT32           Class,
               char           *start,
               INT64           bitsize )

{
  WN   * wn  ;  
  WN   * wc  ;    
  TY_IDX ty  ;  
  INTPTR cn   ;
  ST *st;
#ifdef KEY /* Bug 10177 */
  STB_pkt *p = 0;
#else /* KEY Bug 10177 */
  STB_pkt *p ;
#endif /* KEY Bug 10177 */
  
  switch ((CONSTANT_CLASS)Class) {
  case Arith_Const:

    cn = fei_arith_con(type,(SLONG *)start) ;
    p  = cast_to_STB(cn);

    if (p->form == is_WN)
      wn = cast_to_WN(p->item);
    else 
      wn = cwh_stab_const(cast_to_ST(p->item));
    
    wc = WN_COPY_Tree(wn);
    wn = WN_COPY_Tree(wn);
    ty = cast_to_TY(t_TY(type));
    cwh_stk_push_typed(cast_to_void(wn),WN_item,ty) ;  
    p = cwh_stab_packet_typed(wc,is_WN,ty);

    break;

  case Pattern_Const:

    cn = fei_pattern_con(type,start,bitsize);

    if (type.basic_type == Char_Fortran) {

       st = (ST *) cast_to_void(cn);
       wn = WN_CreateIntconst (OPC_U4INTCONST,TY_size(ST_type(st)));
       cwh_stk_push_STR(wn,st,ST_type(st),ST_item);
       p = cwh_stab_packet(cast_to_void(cn),is_SCONST);

    } else {
       cwh_stk_push(cast_to_void(cn),PCONST_item);
       p = cwh_stab_packet(cast_to_void(cn),is_PCONST);
    }
    
    break;

  default:	
    DevAssert((0), ("Unimplemented constant"));
    break ;
  }
  
  return(cast_to_long(p));
}

/*===============================================
 *
 * fei_push_arith_con
 *
 * Push a reference to a constant on the 
 * expression stack. Copy the WN passed in.
 * for logical constants, we have have a TY.
 * 
 *===============================================
 */ 
extern void 
fei_push_arith_con ( INTPTR cdx )
{
  WN   * wn  ;
  TY_IDX ty  ;
  STB_pkt *p;

  p  = cast_to_STB(cdx);
  wn = WN_COPY_Tree((WN *) p->item);
  ty = p->ty;

  if (ty != 0)
    cwh_stk_push_typed(cast_to_void(wn),WN_item,ty) ;
  else
    cwh_stk_push(cast_to_void(wn),WN_item) ;
}

/*===============================================
 *
 * fei_push_pattern_con
 * 
 * Push a reference to a string or aggregate 
 * expression stack. Make the ST passed into
 * STR_item or an ST reference.
 *
 *===============================================
 */ 
extern void 
fei_push_pattern_con ( INTPTR cdx )
{
   ST *st;
   TY_IDX ty;
   WN *wn;
   STB_pkt *p;

   p = cast_to_STB(cdx);

   /* called with the ST of a pattern constant */
   st = (ST *) p->item;

   if (p->form == is_SCONST) {
      ty = ST_type(st);
      wn = WN_CreateIntconst (OPC_U4INTCONST,TY_size(ty));
      cwh_stk_push_STR(wn,st,ty,ST_item);

   } else {
      cwh_stk_push(st,PCONST_item);
   }
}
#ifdef KEY /* Bug 4602 */
/*===============================================
 *
 * fei_arg_by_value
 *
 * If the tos is an array, generate a LOAD for it.
 * Used to implement %val() for an actual argument
 * which is an array element
 *
 *===============================================
 */ 
extern void
fei_array_element_by_value() {
  TY_IDX save_type = cwh_stk_get_TY();
  enum item_class save_class = cwh_stk_get_class();
  WN *wn = cwh_stk_pop_WN();

  if (wn != NULL && cwh_addr_is_array(wn)) {
    wn = cwh_addr_load_WN(wn, 0, save_type);
    }
  cwh_stk_push_typed(wn, save_class, save_type);
  }
#endif /* KEY Bug 4602 */


/*===============================================
 *
 * fei_store
 *
 * Generate a store. The rhs will be on
 * top of the stack, and the lhs symbol
 * ST or address WN will be below. 
 *
 * On the lhs we add an OPC_ARRAYEXP to 
 * describe the iterations, if its an 
 * array section address 
 * 
 * Sometimes a NULL WN is on top because WHIRL
 * wants (say) an intrinsic call, or a store
 * has already been done. The FE doesn't know
 * so fei_store is called & the stack cleared.
 *
 *===============================================
 */ 
/*ARGSUSED*/
extern void
fei_store ( TYPE result_type )
{
  WN   * rhs  ;
  WN   * wn   ;
  ST   * st   ;
  ST   * rhs_st;
  TY_IDX  ty;
  TY_IDX  ts;
  
  FLD_det det ;

  if (cwh_stk_get_class() == STR_item) {

    cwh_stmt_character_store(result_type);

  } else if (cwh_stk_get_class() == PCONST_item) {

     rhs_st = cwh_stk_pop_PCONST();
     ty  = ST_type(rhs_st);
     rhs = cwh_addr_address_ST(rhs_st,0);
     rhs = cwh_addr_mload(rhs,0,ty,NULL);
     wn  = cwh_expr_address(f_NONE);
     wn  = cwh_addr_mstore(wn,0,ty,rhs) ;
     cwh_block_append(wn) ;

  } else {

    rhs = cwh_expr_operand(NULL);

    if (rhs == NULL) {
      cwh_stk_pop_whatever() ;
      return ;
    }

    switch(cwh_stk_get_class()) {
    case WN_item:
    case WN_item_whole_array:
      ts = cwh_stk_get_TY();
      wn = cwh_expr_address(f_NONE);
      wn = F90_Wrap_ARREXP(wn) ;
      cwh_addr_store_WN(wn,0,ts,rhs);
      break  ;

    case DEREF_item:
      ts = cwh_stk_get_TY();
      if (ts) {
	 /* Get the type of the item stored from the dope vector */
	 ts = TY_pointed(FLD_type(TY_fld(Ty_Table[ts])));
      }
      wn = cwh_expr_address(f_NONE);
      wn = F90_Wrap_ARREXP(wn) ;
      cwh_addr_store_WN(wn,0,ts,rhs);
      break  ;

    case ST_item:
    case ST_item_whole_array:
      st = cwh_stk_pop_ST();
      cwh_addr_store_ST(st,0,0,rhs);
      break ;

    case FLD_item:
      det = cwh_addr_offset();

      if (cwh_stk_get_class() == ST_item || 
	  cwh_stk_get_class() == ST_item_whole_array) {

	st  = cwh_stk_pop_ST();
	cwh_addr_store_ST(st,det.off,det.type,rhs);

      } else {

	wn = cwh_stk_pop_WHIRL();
	wn = cwh_expr_bincalc(OPR_ADD,wn,WN_Intconst(Pointer_Mtype,det.off));
	wn = F90_Wrap_ARREXP(wn);
	cwh_addr_store_WN(wn,0,det.type,rhs);
      }
      break;

    default:
      DevAssert((0),("odd store LHS"));
    }
  }  
}

/*===============================================
 *
 * fei_non_conform_store
 *
 * Used when a 1d a temp is assigned to an nd 
 * destination or vice-versa. Used in constructors.
 *
 * The OPC_ARRSECTION of the temp is replaced 
 * with one which describes  the same shape as 
 * The destination. hen the items are pushd back 
 * onto the stack and fei_store called. There is
 * an assumption(assertion) that the temp is a
 * contiguous object.
 *
 *===============================================
 */ 
/*ARGSUSED*/
extern void
fei_non_conform_store( TYPE result_type )
{
  WN *wd  ;
  WN *wdl ;
  TY_IDX td  ;  
  TY_IDX ts1 ;
  TY_IDX ts2 ;

  WN *wt  ;
  WN *wtl ;
  TY_IDX tt  ;

  FLD_HANDLE f1 ;
  FLD_HANDLE f2 ;
  FLD_det d1 ;
  FLD_det d2 ;

  switch(cwh_stk_get_class()) {
  case STR_item:
    cwh_stk_pop_STR();
    wtl = cwh_stk_pop_WN();  
    ts1 = cwh_stk_get_TY();
    wt  = cwh_stk_pop_WN();  
    wt = cwh_expr_extract_arrayexp(wt,DELETE_ARRAYEXP_WN);

    cwh_stk_pop_STR();
    wdl = cwh_stk_pop_WN();  
    ts2 = cwh_stk_get_TY();
    wd  = cwh_stk_pop_WN();
    wd = cwh_expr_extract_arrayexp(wd,DELETE_ARRAYEXP_WN);

    cwh_addr_nonc_util(&wt,&wd);

    cwh_stk_push_STR(wdl,wd,ts2,WN_item);
    cwh_stk_push_STR(wtl,wt,ts1,WN_item);
    break;

  default:

    if (cwh_stk_get_class() == FLD_item) {
      d1 = cwh_addr_offset();
      f1 = cwh_types_fld_dummy(d1.off,d1.type);
    }
    tt  = cwh_stk_get_TY();
    wt  = cwh_stk_pop_WHIRL();

    if (!tt) {
       tt = cwh_types_WN_TY(wt,FALSE);
    }

    wt  = cwh_expr_extract_arrayexp(wt,DELETE_ARRAYEXP_WN);

    if (cwh_stk_get_class() == FLD_item) {
      d2 = cwh_addr_offset();
      f2 = cwh_types_fld_dummy(d2.off,d2.type);
    }
    td  = cwh_stk_get_TY();
    wd  = cwh_stk_pop_WHIRL();

    if (!td) {
       td = cwh_types_WN_TY(wd,FALSE);
    }

    wd = cwh_expr_extract_arrayexp(wd,DELETE_ARRAYEXP_WN);

    cwh_addr_nonc_util(&wt,&wd);		   

    cwh_stk_push_typed(wd,WN_item,td); 
    if (!f2.Is_Null ())
      cwh_stk_push((void *)(INTPTR)f2.Idx (),FLD_item);

    cwh_stk_push_typed(wt,WN_item,tt); 
    if (!f1.Is_Null ())
      cwh_stk_push((void *)(INTPTR)f1.Idx(),FLD_item);

  }

  fei_store(result_type);
}

/*===============================================
 *
 * cwh_stmt_character_store.
 *
 * two str items are on the stack. Pop them,
 * and look to see if they are single bytes.
 * if not, call CASSIGNMENT. If so, get rid
 * of the STR items and sizes, then store.
 *
 * Could expand to two bytes etc, but would 
 * need padding and dependence checking, so 
 * better as CASSIGNMENT intrinsic optimization.
 *
 *===============================================
 */ 
static void
cwh_stmt_character_store(TYPE result_type)
{
  WN * src;

  if (cwh_stk_is_byte_STR(0) && 
      cwh_stk_is_byte_STR(1)) {

    cwh_stk_pop_STR();
    cwh_stk_pop_whatever();
    src = cwh_expr_operand(NULL);
    src = cwh_expr_dispose_of_char(src);

    cwh_stk_pop_STR();
    cwh_stk_pop_whatever();

    cwh_stk_push(src,WN_item);
    fei_store(result_type);

  } else {
    cwh_stmt_character_icall(INTRN_CASSIGNSTMT);
  }
}

/*===============================================
 *
 * fei_function_ref
 *
 * Given an ST of a function, stick it on the stack.
 * It will be popped by fei_call.
 *
 *===============================================
 */ 
extern void 
fei_function_ref(INTPTR id)
{
  STB_pkt *p;

  p = cast_to_STB(id) ;
  
  DevAssert((p->form == is_ST),("Fn ST missing"));
  DevAssert((p->item != NULL),("NULL fn imp"));

  cwh_stk_push(cast_to_ST(p->item), ST_item);
}

#ifdef KEY /* Bug 10282 */
/*
 * return struct type if t is array of struct type suitable for returning
 * by value from a function; else return -1
 */
TY_IDX array_of_struct_by_value(TY_IDX t) {
  if (TY_kind(t) == KIND_ARRAY) {
    t = cwh_types_scalar_TY(t);
    if (STRUCT_BY_VALUE(t)) {
      return t;
    }
  }
  return (TY_IDX) -1;
}
#endif /* KEY Bug 10282 */

/*===============================================
 *
 * cwh_stmt_call_helper
 *
 * Build a call stmt. For a conventional call 
 * arguments are on the stack, as ADDR_items or
 * STR_items, with the ST of the call name beneath 
 * them. For an intrinsic or library call, there 
 * may just be a value - so test for WN too.
 * OPC_PARMs are wrapped aroudn everything - ref
 * parms generally, but value parms around WNs.
 *
 * If it's a function call, then the result 
 * PREG is pushed onto the stack to be read 
 * by fei_store. If the function result is 
 * complex*32 the address of the result is the
 * first argument. Results with similar requirements
 * eg: character, derived type > 16 bytes have 
 * already been transformed into arguments by the FFE.
 *
 * Character lengths are appended to the list, 
 * unless a character function result, whn it goes
 * into the spot after the address.
 *
 * This function provides a common interface 
 * for intrinsic routines and user routines.
 * The function returns the call node, although it
 * will already be in the tree, so that flags might
 * be set on it. 
 *
 * inline_state is set to 0=normal, 1=inline, 2=noinline
 *
 *===============================================
 */ 
#include "ir_reader.h"
extern WN * 
cwh_stmt_call_helper(INT32 num_args, TY_IDX ty, INT32 inline_state, INT64 flags)
{
  WN     * wc  ;
  WN     * call_wn  ;
  WN     * wn  ;
  WN     * wa  ;  
  WN     * wt  ;
  WN    ** args;
  ST     * st  ;
  ST     * rt  ;
  TY_IDX  ta  ;
  TY_IDX  ts  ;
  TY_IDX  tr  ;
  INT32    nargs;  
  INT32    clen ;  
  INT32    i,k  ;
  WN *     block;      

  TYPE_ID   rbtype1;
  TYPE_ID   rbtype2;
  OPCODE    opc;

  BOOL        forward_barrier = FALSE;
  BOOL        backward_barrier = FALSE;
  WN * barrier_wn;
#ifdef KEY /* Bug 10282 */
  /* If an elemental function is returning an array of "small" structures,
   * we need to remember the temp which receives the function result, and
   * the base type of the result array. */
  WN *case4_array_temp = 0;
  TY_IDX case4_base_type = (TY_IDX) -1;
#endif /* KEY Bug 10282 */

  /* figure # of args, including character lengths, clear return temp ST */

  nargs  = num_args + cwh_stk_count_STRs(num_args) ; 
  clen   = nargs ;
  rt     = NULL;

  args = (WN **) malloc(nargs*sizeof(WN *));

  for (k = num_args -1 ; k >= 0  ; k --) {

    switch(cwh_stk_get_class()) {
    case STR_item:
      cwh_stk_pop_STR();
      wa = cwh_stk_pop_WN();
      wc = WN_COPY_Tree(wa); 
      args[--clen] = cwh_intrin_wrap_value_parm(wa);
      wa = cwh_stk_pop_ADDR();
      args[k] = cwh_intrin_wrap_char_parm(wa,wc);
      break ;

    case ADDR_item:
      ta = cwh_stk_get_TY();
      wa = cwh_stk_pop_ADDR();
      args[k] = cwh_intrin_wrap_ref_parm(wa,ta);
      break;

    case WN_item:
    case WN_item_whole_array:
      wa = cwh_stk_pop_WN();
      wa = cwh_intrin_wrap_value_parm(wa);

      args[k] = wa;
      break ;

    case FLD_item:
    case ST_item:
    case ST_item_whole_array:
      wa = cwh_expr_operand(NULL);
      wa = cwh_intrin_wrap_value_parm(wa);
      args[k] = wa;
      break ;

    case DEREF_item: /* 11Dec00[sos]: Handle "call sub(%val(apointer))"  */
      wa = cwh_stk_pop_DEREF();
      wa = cwh_intrin_wrap_value_parm(wa);
      args[k] = wa;
      break;

    default:
      DevAssert((0),("Odd call actual")) ; 
    }
  }
  
  /* Function returning character? Reorder to get   */
  /* length of function result as 2nd argument.     */
  /* Function returning struct by value? Delete     */
  /* first arg.                                     */
  /* Will not have function's TY, if via proc_imp   */
  /* so look at first arg.                          */  

  st = cwh_stk_pop_ST(); 
  ts = ty ;
  tr = ty ;

  if (ST_class(st) != CLASS_FUNC) {  /* Must be indirect call, so ptr to */
                                     /* function. Get function type      */

     DevAssert((TY_kind(ST_type(st)) == KIND_POINTER && 
		TY_kind(TY_pointed(ST_type(st))) == KIND_FUNCTION),
                ("Odd ST"));

     tr = TY_ret_type(TY_pointed(ST_type(st)));
  }

  if (ST_auxst_has_rslt_tmp(st) || cwh_types_is_character(tr)) {

    tr = cwh_types_WN_TY(args[0],FALSE);

    if (cwh_types_is_character(tr)) {

      wt = args[clen];

      for (k = clen ; k > 1 ; k--) 
	args[k] = args[k-1];

      args[1] = wt;

    } else if (STRUCT_BY_VALUE(tr)) {

      DevAssert((WNOPR(args[0]) == OPR_PARM),("Odd result"));
      wt = WN_kid(args[0],0);

      DevAssert((wt != NULL),("struct w/o temp"));
      DevAssert((WNOPR(wt) == OPR_LDA),("struct w/o ADDR_item"));

      rt = WN_st(wt);
      ts = tr ;

      nargs --;

      for (i=0; i < nargs; i++) 
	args[i] = args[i+1];

    }
#ifdef KEY /* Bug 10282 */
    else if ((ST_auxst_is_elemental(st)) && (TY_mtype(tr) != MTYPE_V)) {
      case4_base_type = array_of_struct_by_value(tr);
      if (case4_base_type != (TY_IDX) -1) {
	DevAssert((WNOPR(args[0]) == OPR_PARM),("Odd result"));
	case4_array_temp = WN_kid(args[0],0);
	wt = WN_kid(case4_array_temp,0);

	DevAssert((wt != NULL),("struct w/o temp"));
	DevAssert((WNOPR(wt) == OPR_LDA),("struct w/o ADDR_item"));

	rt = WN_st(wt);
	ts = case4_base_type ;

	nargs --;

	for (i=0; i < nargs; i++) 
	  args[i] = args[i+1];
      }
    }
#endif /* KEY Bug 10282 */
  }
  

  /* create call (or indirect call if dummy procedure)  */

  if (WHIRL_Return_Info_On) {

    RETURN_INFO return_info = Get_Return_Info (ts, Use_Simulated);

    if (RETURN_INFO_count(return_info) <= 2 ||
        WHIRL_Return_Val_On) {

      rbtype1 = RETURN_INFO_mtype (return_info, 0);
      rbtype2 = RETURN_INFO_mtype (return_info, 1);
    }

    else
      Fail_FmtAssertion ("cwh_stmt_call_helper: more than 2 return registers");
  }

  else
    Get_Return_Mtypes(ts, Use_Simulated, &rbtype1,&rbtype2);


  if (ST_sclass(st) != SCLASS_FORMAL) {

     opc = OPCODE_make_op(OPR_CALL,TY_mtype(ts),MTYPE_V);
     wn  = WN_Create(opc,nargs);
     WN_st_idx(wn) = ST_st_idx(st);

     /* if the name of the routine is one of mp_setlock mp_unsetlock
        or mp_barrier then set barrier flags (PV 485782) */

     if (cwh_stmt_sgi_mp_flag) {
       if (rbtype1==MTYPE_V && ST_name(st) &&
	   ST_name(st)[0]=='m' && ST_name(st)[1]=='p') {
	 if (!strcmp(&(ST_name(st)[2]),"_setlock_")) {
	   backward_barrier = TRUE;
	 } else if (!strcmp(&(ST_name(st)[2]),"_unsetlock_")) {
	   forward_barrier = TRUE;
	 } else if (!strcmp(&(ST_name(st)[2]),"_barrier_")) {
	   forward_barrier = TRUE;
	   backward_barrier = TRUE;
	 }
       }
     }

  } else {

     opc = OPCODE_make_op (OPR_ICALL,TY_mtype(ts),MTYPE_V);
     wn  = WN_Create(opc,nargs+1);
     WN_set_ty(wn,TY_pointed(ST_type(st)));
     WN_kid(wn,nargs) = cwh_addr_load_ST(st,0,ST_type(st));
  }

  if (forward_barrier) {
    barrier_wn=WN_CreateBarrier ( TRUE, 0 );
    cwh_block_append(barrier_wn);
  }


  WN_Set_Call_Default_Flags(wn);  
  WN_Set_Call_Fortran_Pointer_Rule(wn);

  if (FE_Call_Never_Return &&
      test_flag(flags, FEI_CALL_DOES_NOT_RETURN)) {
    WN_Set_Call_Never_Return(wn);
  }

  if (inline_state == 1) {
    /* inline */
    WN_Set_Call_Inline(wn);
    fe_invoke_inliner = TRUE;
  } else if (inline_state == 2) {
    /* no inline */
    WN_Set_Call_Dont_Inline(wn);
  }

  call_wn = wn;

  for (i=0; i < nargs; i++) {
     WN_kid(wn,i) = args[i];
  }

  free(args);


  /* Function result - for elementals (with array arguments) whose   */ 
  /* scalar lowering returns values in registers, a statement level  */
  /* call is no good, because the f90 lowerer wants to see a store   */
  /* into an array-valued temp. So a COMMA node holds the pregs of   */
  /* the return and the call block                                   */
  
  if ((ST_auxst_is_elemental(st)) && (TY_mtype(ts) != MTYPE_V)) {

     /* ELEMENTAL functions. Build a COMMA node */

     block = cwh_block_new_and_current();
     cwh_block_append(wn);
     block = cwh_block_exchange_current(block);

#ifdef KEY /* Bug 9520, 10282 */
     // Arrange to capture anything cwh_stmt_return_scalar() appends to
     // current block
     WN *stmt_return_block = cwh_block_new_and_current();
     if (case4_base_type != ((TY_IDX) -1)) {
       wn = cwh_stmt_return_scalar(rt,NULL,case4_base_type,FALSE);
     } else {
       wn  = cwh_stmt_return_scalar(rt,NULL,ts,FALSE);
     }
     stmt_return_block = cwh_block_exchange_current(stmt_return_block);
     opc = cwh_make_typed_opcode(OPR_COMMA,rbtype1,MTYPE_V);

     // For the record, the unhappy code in this function handles six cases:
     // 1. scalar
     //    Cray FE calls fcn with user's arglist, assigns result
     //    SGI FE generates straightforward code: calls function, loads
     //      .preg_return_val, stores into whatever
     // 2. elemental scalar
     //    Cray FE calls fcn with user's arglist, assigns result
     //    SGI FE generates slightly tricky code: calls function, puts the
     //      call and the load of .preg_return_val into a comma operator,
     //      stores into whatever
     // 3. small structure
     //    Cray FE calls fcn, passing result as 1st argument
     //    SGI FE removes 1st argument, generates call with remaining
     //      arguments, loads .preg-return_val, stores into what was 1st
     //      argument
     // 4. elemental small ("STRUCT_BY_VALUE") structure
     //    Cray FE calls fcn, passing result as 1st argument
     //      SGI FE tries to do a combination of (2) and (3) but (prior to
     //      this fix) fails miserably
     // 5. large structure
     //    Cray FE calls fcn, passing result as 1st argument
     //    SGI FE generates straightforward code: calls function as received
     //      from FE, then array-assigns what was the 1st
     // 6. elemental large structure
     //    Cray FE calls fcn, passing result as 1st argument
     //    SGI FE generates straightforward code just like (5)
     //
     // Seems like it would have been better to make the Cray FE generate
     // (3) and (4) correctly, but perhaps there's a reason the original
     // authors didn't: anyway, that would only fix half the problem.
     //
     // In the case 4 of an elemental function returning a small structure,
     // cwh_stmt_return_scalar() appends a load and store to the block
     // that was then current. We need to capture the load for use inside
     // the comma operator, and we need to append first the comma and then
     // the store to the real block. The fix to do it at this spot is ugly,
     // but messing with cwh_stmt_return_scalar() is scary since it's used
     // in so many other places, so for now I prefer to do it here..
     if (NULL == wn) {
       WN *store = WN_first(stmt_return_block);
       WN *load = WN_kid0(store);
       wn  = WN_CreateComma(opc,block,load);
       if (case4_array_temp) {
	 cwh_stk_push_typed(case4_array_temp,WN_item,tr);
	 cwh_stk_push_typed(wn,WN_item,tr);
	 TYPE pdg_type_void = fei_descriptor(0, Basic, 0, V_oid, 0, 0);
	 fei_store(pdg_type_void);
       }
       else {
	 WN_kid0(store) = wn;
	 cwh_block_append(store);
       }
     }
     else {
       wn  = WN_CreateComma(opc,block,wn);
       cwh_stk_push_typed(wn,WN_item,ty);
     }
#else /* KEY Bug 9520, 10282 */
     wn  = cwh_stmt_return_scalar(rt,NULL,ts,FALSE);
     opc = cwh_make_typed_opcode(OPR_COMMA,rbtype1,MTYPE_V);
     wn  = WN_CreateComma(opc,block,wn);
     cwh_stk_push_typed(wn,WN_item,ty);
#endif /* KEY Bug 9520, 10282 */
     
  } else {
    
    /* put ARRAYEXPs underneath the parm nodes of elementals */
    
    if (ST_auxst_is_elemental(st)) {
	
      for (k = 0; k < nargs; k ++) {
	WN_kid0(WN_kid(wn,k)) = F90_Wrap_ARREXP(WN_kid0(WN_kid(wn,k)));
      }
    }
    cwh_block_append(wn);
    
    
    /* scalar (in registers) function result?      */
    /* Push read of pregs on stack, unless struct  */
    /* by value when read of temp..                */
    
    if (TY_mtype(ts) != MTYPE_V) {

      wn = cwh_stmt_return_scalar(rt,NULL,ts,FALSE);

      if (wn != NULL)
	cwh_stk_push(wn,WN_item);
    }
  }
  
  if (backward_barrier) {
    barrier_wn=WN_CreateBarrier ( FALSE, 0 );
    cwh_block_append(barrier_wn);
  }

  return (call_wn);
}

/*===============================================
 *
 * fei_call
 *
 * Build a call stmt. For a conventional call 
 * arguments are on the stack, as ADDR_items or
 * STR_items, with the ST of the call name beneath 
 * them. For an intrinsic or library call, a WN may
 * be passed by value.
 *
 * OPC_PARMs are wrapped around everything - ref
 * parms generally, but value parms around WNs.
 *
 * see cwh_stmt_call_helper.
 *
 *===============================================
 */ 
/*ARGSUSED*/
extern void 
fei_call(INT32      num_args,
	 TYPE       result_type,
	 INT32      call_type,
	 INT32      alt_return_flag,
	 INT32      inline_setting,
         INT64      flags)
    
{
   TY_IDX ty;
   ty = cast_to_TY(t_TY(result_type));
   (void) cwh_stmt_call_helper(num_args,ty,inline_setting,flags);
}

/*===============================================
 *
 * fei_arg_addr
 *
 * Build an address and push it back on
 * the stack. These were PARM nodes, but
 * ALOCs were required for some other items
 * so PARMS are deferred to fei_call.
 *
 * For FLD items we need to save the FLD type,
 * so find out the TY, address the FLD, then
 * push a typed ADDR_item on the stack, so later
 * fei_call (say) can put the correct TY in a PARM.
 *
 *===============================================
 */ 
/*ARGSUSED*/
extern void 
fei_arg_addr(TYPE type)
{
  WN  * wn ;
  WN  * wa ;
  TY_IDX ty ;
  TY_IDX ts ;
  FLD_HANDLE  fld;
  FLD_det det;

  switch(cwh_stk_get_class()) {
  case STR_item:
    cwh_stk_pop_STR();
    wn = cwh_stk_pop_WN();
    ts = cwh_stk_get_TY();
    wa = cwh_expr_address(f_T_PASSED);
    cwh_stk_push_STR(wn,wa,ts,ADDR_item);
    break;

  case FLD_item:
    det = cwh_addr_offset();
    fld = cwh_types_fld_dummy(det.off,det.type);
    cwh_stk_push((void *)(INTPTR)fld.Idx (),FLD_item);
    wa  = cwh_expr_address(f_T_PASSED);
    cwh_stk_push_typed(wa,ADDR_item, cwh_types_make_pointer_type(det.type, FALSE));
    break;

  case WN_item_whole_array:
    wa = cwh_expr_address(f_T_PASSED);
    DevAssert ((WNOPR(wa) == OPR_ARRAY), ("Whole array isnt an ARRAY"));
    wa = WN_kid0(wa);	/* the base */
    ty = cwh_types_WN_TY(wa,FALSE);
    ty = cwh_types_make_pointer_type(ty, FALSE);
    cwh_stk_push_typed(wa,ADDR_item,ty);
    break;

  default:
    wa = cwh_expr_address(f_T_PASSED);
    if (WNOPR(wa) == OPR_ARRAY) {
      ty = cwh_types_WN_TY(wa,FALSE);
      ty = cwh_types_array_TY(ty);
      ty = cwh_types_scalar_TY(ty);
      ty = cwh_types_make_pointer_type(ty, FALSE);
      cwh_stk_push_typed(wa,ADDR_item,ty);

    } else
      cwh_stk_push(wa,ADDR_item);
    break;
  }
}

/*===============================================
 *
 * fei_fcd
 *
 * A reference via a cray character pointer is
 * on the stack. Make it into a STR_item and
 * & push it. The address should look as though 
 * it came from fei_arg_addr ie: an ADDR_item.
 *
 *===============================================
 */ 
/*ARGSUSED*/
void
fei_fcd(TYPE result_type)
{
#ifdef KEY /* Bug 10177 */
  WN *wn = 0;
#else /* KEY Bug 10177 */
  WN *wn ;
#endif /* KEY Bug 10177 */
  WN *ad ;
  WN *ln ;
  TY_IDX ts ;

  ts = cwh_stk_get_TY();
  ad = cwh_stk_pop_WHIRL();  
  ln = cwh_stk_pop_WHIRL();

  if (WNOPR(ad) == OPR_INTCONST) {

    wn = WN_Intconst(Pointer_Mtype,WN_const_val(ad));

    WN_DELETE_Tree(ad);
    ad = wn;
    
  }
  if (ts == 0)
    ts = cwh_types_WN_TY(wn,FALSE);

  cwh_stk_push_STR(ln,ad,ts,ADDR_item);

}
/*===============================================
 *
 * fei_addr_con
 *
 * A constant as an actual argument. Find or
 * make (integers) the constant's ST, make an
 * address & push the address.
 *
 *===============================================
 */ 
extern void 
fei_addr_con(TYPE type)
{
  WN * wn;
  WN * wt;
  ST * st;
  TY_IDX  ty;

  TCON tc  ;
  TYPE_ID bt ;


  switch (cwh_stk_get_class()) {
  case STR_item:
    cwh_stk_pop_STR();
    wn = cwh_stk_pop_WN();
    ty = cwh_stk_get_TY();
    wt = cwh_expr_address(f_T_PASSED);
    cwh_stk_push_STR(wn,wt,ty,ADDR_item);
    break;

  default:
    ty = cwh_stk_get_TY();
    wn = cwh_stk_pop_WN();
    
    if (WNOPR(wn) == OPR_INTCONST)  {
      
      if (ty == 0) {
	bt = WNRTY(wn);
      } else {
	bt = TY_mtype(ty);
      }
      tc = Host_To_Targ (bt,WN_const_val(wn));
      st = New_Const_Sym(Enter_tcon (tc), Be_Type_Tbl(bt));

    } else 
      st = WN_st(wn);
    
    wt = cwh_addr_address_ST(st,0);
    cwh_stk_push(wt,ADDR_item);
  }
}

/*===============================================
 *
 * fei_entry_pt
 *
 * Generate an OPC_ALTENTRY and tack the dummy
 * argument list on. Idx is the ST of the entry.
 *
 *===============================================
 */ 
extern void 
fei_entry_pt(INTPTR idx)
{
  ST    *st ;
  ST   **ap ;
  WN    *wn ;
  STB_pkt *p ;
  
  INT16 nkids,i ;
  
  p  = cast_to_STB(idx);
  st = cast_to_ST(p->item);
  
  nkids = cwh_auxst_num_dummies(st);
  ap    = cwh_auxst_arglist(st);

  wn = WN_Create (OPC_ALTENTRY, nkids);
  WN_st_idx(wn) = ST_st_idx(st);
  
  for (i = 0 ; i < nkids ; i ++) 
    WN_kid(wn,i) = WN_CreateIdname ( 0, *ap++);

  cwh_block_append(wn) ;
  (void) cwh_block_toggle_debug(FALSE) ;
}     

/*===============================================
 *
 * fei_goto
 *
 * Generate a GOTO to the label whose ST is provided.
 *
 *===============================================
 */ 
extern void 
fei_goto(INTPTR lbl_idx)
{
  LABEL_IDX lb ;

  lb = cast_to_LB(lbl_idx);
  cwh_stmt_goto(lb);
}

/*===============================================
 *
 * fei_arith_goto
 *
 * Handles the Fortran arithmetic goto statement. 
 * 
 * The expression used for computing the goto is on
 * the stack.
 * If all three labels are equal, a single goto is
 * generated. If any two labels are equal, the labels
 * are combined into two labels. The expression is
 * compared against zero and branches are generated
 * to the right labels.
 *
 *===============================================
 */

extern void
fei_arith_goto(INTPTR eq_lbl,
               INTPTR gt_lbl,
               INTPTR lt_lbl )
{
  WN *expr;
  WN *val1, *val2;
  WN *wn;
  LABEL_IDX lb ;
  TY_IDX ty;
  OPCODE opc;
  OPERATOR opr;
  INTPTR true_lbl;
  INTPTR false_lbl;


  if (lt_lbl == eq_lbl && gt_lbl == eq_lbl) {

    /* All three labels are the same */

    cwh_stmt_goto(cast_to_LB(eq_lbl));
    expr = cwh_expr_operand(NULL);

  } else {

    expr = cwh_expr_operand(NULL);
    ty   = Be_Type_Tbl(WN_rtype(expr));

    if ( WN_operator(expr) == OPR_SUB ) {
      val1 = WN_kid0(expr);
      val2 = WN_kid1(expr);
    } else {
      val1 = expr;
      if (MTYPE_is_integral(TY_mtype(ty))) {
        opc = cwh_make_typed_opcode(OPR_INTCONST, TY_mtype(ty), MTYPE_V);
        val2 = WN_CreateIntconst ( opc, 0 );
      } else {
        val2 = Make_Zerocon ( TY_mtype(ty) );
      }
    }
 
    if (eq_lbl != lt_lbl &&
	eq_lbl != gt_lbl &&
	lt_lbl != gt_lbl ) {
       /* All three labels are different.
	* Nothing much can be done in this case.
	*/
      lb = cast_to_LB(lt_lbl);

      wn = cwh_stmt_truebr(WN_COPY_Tree(val1), WN_COPY_Tree(val2), ty, OPR_LT,lb);
      cwh_block_append(wn);

      lb = cast_to_LB(gt_lbl);
      wn = cwh_stmt_truebr(WN_COPY_Tree(val1), WN_COPY_Tree(val2), ty, OPR_GT,lb);
      cwh_block_append(wn);
      cwh_stmt_goto(cast_to_LB(eq_lbl));


    } else {
      /* Two of the labels are the same.
       * Figure out how to combine these two.
       */
      if (eq_lbl == lt_lbl) {
         opr = OPR_LE;
	 true_lbl  = eq_lbl;
	 false_lbl = gt_lbl;

      } else if (eq_lbl == gt_lbl) {
         opr = OPR_GE;
	 true_lbl  = eq_lbl;
	 false_lbl = lt_lbl;

      } else {
         opr = OPR_NE;
	 true_lbl  = gt_lbl;
	 false_lbl = eq_lbl;
      }

      lb = cast_to_LB(true_lbl);
      wn = cwh_stmt_truebr(WN_COPY_Tree(val1), WN_COPY_Tree(val2), ty, opr,lb);
      cwh_block_append(wn);
      cwh_stmt_goto(cast_to_LB(false_lbl));
    }
  }
}

/*===============================================
 *
 * fei_label_ref
 *
 * Places a label on the stack. 
 *
 *===============================================
 */
extern void
fei_label_ref(INTPTR   lbl_idx)
{
  LABEL_IDX lb;
  lb = cast_to_LB(lbl_idx);
  cwh_stk_push(cast_to_void((INTPTR)lb),LB_item);
}

/*===============================================
 *
 * fei_label_addr
 *
 * Used with Fortran Assign statement. 
 * 
 * Increments the index into the table that has all
 * the assigned goto labels. This is only done if the 
 * label hasn't been seen before, in which case the assign_id
 * field in the AUXST will be -1. Creates an INTCONST out
 * of this index and pushes it on the stack. This node
 * ends up getting stored into the location of the ASSIGN 
 * var later.
 *
 *===============================================
 */
/*ARGSUSED*/
extern void
fei_label_addr(INTPTR lbl_idx)
{
  WN *wn;
  INT32 *assign_id;

  assign_id = cwh_auxst_assign_id(CURRENT_SYMTAB, LABEL_IDX_index((LABEL_IDX)lbl_idx));

  if (*assign_id == -1)
     *assign_id = cwh_assign_label_id++;

  wn = WN_CreateIntconst (OPC_I4INTCONST, *assign_id);
  cwh_stk_push(wn, WN_item);
}

/*===============================================
 *
 * cwh_stmt_computed_goto
 *
 * Handle the Fortran computed goto statement.
 *
 * Labels referenced are pushed on the stack via fei_label_ref.
 * Below the labels is the expression that controls
 * the computed goto.
 * If there are more than 6 distinct labels in the list of labels
 * the routine just generates a COMPGOTO, otherwise, it converts
 * this into the appropriate TRUE and FALSE branches.
 *
 *===============================================
 */

static void
cwh_stmt_computed_goto(INT32 num_labels)
{
  LABEL_IDX *label_list;
  LABEL_IDX  default_label_num = 0;
  WN *parent_block;
  WN *wn;
  WN *default_label;
  WN *expr;
  OPERATOR opr;
  LABEL_IDX lb;
  LABEL_IDX last_label=0;
  INT32 sequences=0;
  INT32 count;
  INT32 i;

  label_list = (LABEL_IDX *) malloc(num_labels*sizeof(LABEL_IDX));

  for(i=num_labels-1; i>=0; i--) {
    label_list[i] = cwh_stk_pop_LB();
    if (label_list[i] != last_label) {
      sequences++;
      last_label = label_list[i];
    }
  }   

  expr = cwh_expr_operand(NULL);

  if (num_labels == 1) {

    cwh_stmt_append_truebr(WN_COPY_Tree(expr),1, OPR_EQ, label_list[0]);

  } else if ( sequences == 1 && num_labels >= 2) {

    (void) New_LABEL (CURRENT_SYMTAB, default_label_num);

    cwh_stmt_append_truebr(WN_COPY_Tree(expr),1, OPR_LT,default_label_num);
    cwh_stmt_append_truebr(WN_COPY_Tree(expr),num_labels, OPR_LE,label_list[0]);

  } else if ( num_labels <= COMPGOTO_IF_ELSE) {

    for(i=0; i<num_labels; i++) {
      cwh_stmt_append_truebr(WN_COPY_Tree(expr),i+1,OPR_EQ,label_list[i]);
    }

  } else if (sequences <= COMPGOTO_IF_ELSE) {

    (void) New_LABEL (CURRENT_SYMTAB, default_label_num);
    cwh_stmt_append_truebr(WN_COPY_Tree(expr),1,OPR_LT,default_label_num);

    last_label = label_list[0];
    count = 0;

    for(i=0; i<num_labels; i++) {
       if (label_list[i] == last_label) {
	  count++;
       } else {
	  lb  = last_label;
	  if (count == 1) 
	    opr = OPR_EQ;
          else 
	    opr = OPR_LE;
	  cwh_stmt_append_truebr(WN_COPY_Tree(expr),i,opr,lb);
	  count = 1;      
	  last_label = label_list[i];
       }
    }

    if (count == 1) 
      opr = OPR_EQ;
    else 
      opr = OPR_LE;

    cwh_stmt_append_truebr(WN_COPY_Tree(expr),num_labels,opr,last_label);      

  } else {

    parent_block = cwh_block_new_and_current();
    (void) New_LABEL (CURRENT_SYMTAB, default_label_num);
    cwh_stmt_goto(default_label_num);

    for(i=0; i<num_labels; i++) {
      cwh_stmt_goto(label_list[i]);
    } 

    default_label = WN_CreateGoto (default_label_num);
    wn = WN_CreateCompgoto (num_labels+1, expr, cwh_block_current(), default_label, 0);
    cwh_block_set_current(parent_block);
    cwh_block_append(wn);

  }
  
  if (default_label_num) {
     wn = WN_CreateLabel(default_label_num, 0,NULL);
     cwh_block_append(wn);
  }
}
  
/*===============================================
 *
 * cwh_stmt_assigned_goto
 *
 * Handle the Fortran Assigned goto statement.
 * All the labels that have appeared in an ASSIGN
 * statement are on the stack in the order they appeared
 * in the source. The VAR that controls the assigned
 * goto is below those labels. The labels are popped and stored
 * into the array cwh_assign_label_array. VAR at this point has
 * a value (from fei_label_addr) that can be used to index the
 * array cwh_assign_label_array to get the corresponding label.
 *
 *===============================================
 */

static void
cwh_stmt_assigned_goto(INT32 num_labels)
{
  INT32 i;
  LABEL_IDX default_label_num = 0;
  WN *expr;
  WN *parent_block;
  WN *wn;
  WN *default_label;
  LABEL_IDX lb;
  LABEL_IDX *cwh_assign_label_array=NULL;

  cwh_assign_label_array = (LABEL_IDX *) malloc (sizeof(LABEL_IDX *) * num_labels);
  
  for(i=0; i<num_labels; i++) 
    cwh_assign_label_array[i] = cwh_stk_pop_LB();

  expr = cwh_expr_operand(NULL);

  if (num_labels <= COMPGOTO_IF_ELSE) {

    for(i=0; i<num_labels; i++ ) {
      lb = cwh_assign_label_array [i];
      cwh_stmt_append_truebr(WN_COPY_Tree(expr),i,OPR_EQ,lb);
    }

  } else {

    parent_block = cwh_block_new_and_current();
    (void) New_LABEL (CURRENT_SYMTAB, default_label_num);
    default_label = WN_CreateGoto (default_label_num);
  
    for(i=0; i<num_labels; i++ ) {
      cwh_stmt_goto(cwh_assign_label_array [i]);
    }
  
    wn = WN_CreateCompgoto (num_labels, expr, cwh_block_current(), default_label, 0);
    cwh_block_set_current(parent_block);
    cwh_block_append(wn);
    wn = WN_CreateLabel(default_label_num, 0,NULL);
    cwh_block_append(wn);
  }
}


/*===============================================
 *
 * cwh_stmt_truebr
 *
 * Utility to generate a OPC_TRUEBR given an
 * expression, val, label and operator. Does
 * not append the WN, but returns it.
 *
 *===============================================
 */
static WN * 
cwh_stmt_truebr(WN *expr, WN *val, TY_IDX ty, OPERATOR opr, INT32 label_no)
{
  WN * wn;
  WN * test;

  OPCODE opc;

  opc  = cwh_make_typed_opcode(opr, MTYPE_I4, Mtype_comparison(TY_mtype(ty)));
  test = WN_CreateExp2 ( opc, expr, val);
  wn   = WN_CreateTruebr (label_no, test );

  return wn;
}

/*===============================================
 *
 * cwh_stmt_append_truebr
 *
 * Utility to generate a OPC_TRUEBR given an
 * integer constant, label and operator & append it
 * to the current block.
 *
 *===============================================
 */
static void
cwh_stmt_append_truebr(WN *expr, INT64 con, OPERATOR opr, INT32 label_no)
{
  WN * wn;
  WN * val;
  TY_IDX ty;
  OPCODE opc;

  ty  = Be_Type_Tbl(WN_rtype(expr));
  opc = cwh_make_typed_opcode(OPR_INTCONST, TY_mtype(ty), MTYPE_V);

  val = WN_CreateIntconst (opc,con);
  wn  = cwh_stmt_truebr(expr,val,ty,opr,label_no) ;
  cwh_block_append(wn);
}

/*===============================================
 *
 * cwh_stmt_falsebr
 *
 * Utility to generate a OPC_FALSEBR given an
 * expression, val, label and operator.Does
 * not append the WN, but returns it.
 *
 *===============================================
 */
static WN * 
cwh_stmt_falsebr(WN *expr, WN *val, TY_IDX ty, OPERATOR opr, INT32 label_no)
{
  WN * wn;
  WN * test;

  OPCODE opc;

  opc  = cwh_make_typed_opcode(opr, MTYPE_I4, Mtype_comparison(TY_mtype(ty)));
  test = WN_CreateExp2 ( opc, expr, val);
  wn   = WN_CreateFalsebr (label_no, test );

  return wn;
}

/*===============================================
 *
 * cwh_stmt_goto
 *
 * Utility to generate an OPC_GOTO the label.
 * Appends it to the current block. 
 *
 *===============================================
 */
static void
cwh_stmt_goto(LABEL_IDX label)
{
  WN * wn;
  wn = WN_CreateGoto(label);
  cwh_block_append(wn) ;
}

/*===============================================
 *
 * fei_indirect_goto
 *
 * Handle computed goto and assigned goto. A zero
 * value for assign_goto_flag indicates that this 
 * is a call for computed goto; a non-zero value
 * indicates that the call is for an assigned goto.
 *
 *===============================================
 */
extern void
fei_indirect_goto(INT32 num_labels,
                  INT32 assign_goto_flag )
{

  if (assign_goto_flag == 0)
     cwh_stmt_computed_goto(num_labels);
  else 
     cwh_stmt_assigned_goto(num_labels);
}


/*===============================================
 *
 * cwh_stmt_select_char
 *
 * Handles the fortran 90 select statement when the controlling expression
 * is a character expression. The expression controlling the select is on 
 * top of the stack. The two args to the routine are:
 * 1. Number of SELECT cases.
 * 2. Symbol table index for default label.
 *
 * The routine just builds a whirl node that contains the goto to the
 * default label. 'last_node' remembers the position where the last whirl
 * node was appended in the current block. This position is used to emit 
 * the IF's to handle the individual cases of the SELECT statement later.
 *
 * Before exit, the routine will push the following items back on 
 * the stack:
 * 1. num_cases
 * 2. Whirl node that conatins the goto to the default label
 * 3. The select expression
 * 4. last_node
 *
 *===============================================      

 */
static void
cwh_stmt_select_char(INT32 num_cases,
               INTPTR default_label_idx )
{
  WN *wn1;
  W_node expr[2];
  WN *default_label;
  WN *last_node;
  LABEL_IDX lb;

  cwh_expr_str_operand(expr);
  
  if (num_cases > 0) {

    last_node = WN_last(cwh_block_current());

    lb = cast_to_LB(default_label_idx);
    default_label = WN_CreateGoto (lb);

    /* Now push num_cases, default_label, expr and last_node back on the stack */

    wn1 = WN_CreateIntconst(OPC_I4INTCONST, num_cases);
    cwh_stk_push(wn1, WN_item);
    cwh_stk_push(default_label, WN_item);
    cwh_stk_push_STR(W_wn(expr[0]), W_wn(expr[1]),W_ty(expr[1]), WN_item);
    cwh_stk_push(last_node, WN_item);

  } else {  /* empty select */

    WN_DELETE_Tree(W_wn(expr[0]));
    WN_DELETE_Tree(W_wn(expr[1]));

  }
}

/*===============================================
 *
 * cwh_stmt_select_case_char
 *
 * Handle individual cases in a select statement controlled by a character
 * expression. On entry, the stack holds the following items, starting from 
 * the top:
 * 1. high_range if present
 * 2. low_range if present
 * 3. Label to branch to
 * 4. last_node, position within current block where to generate the IF's.
 * 5. The select expression
 * 6. whirl node containing goto to the default label
 * 7. Remaining cases to be handled for this select
 *
 * The args to the routine are:
 * 1. flag to indicate low range present
 * 2. flag to indicate high range present
 * 3. flag to indicate if this is followed by a case which needs a branch to
 *    the same label; eg. case (-1, 0), will come as case(-1) and
 *    case(0) and for case(-1) this flag will be true, because case(0)
 *    requires a branch to the same label.
 * 
 * The routine copies the expr and the range items back on the stack and calls
 * cwh_expr_compare to do a character comparison. Depending on the outcome,
 * a branch is generated to the appropriate label.
 *
 * On exit, the routine is expected to push the following items back on 
 * the stack, if there are any remaining cases for this SELECT:
 * 1. If case_follows is TRUE, push the label back.
 * 2. remaining cases
 * 3. Whirl node that contains the goto to the default label
 * 4. The select expr.
 * 5. last_node
 *
 *
 *===============================================
 */

static void
cwh_stmt_select_case_char(INT32 low_value_pres,
                          INT32 high_value_pres,
                          INT32 case_follows)
{
  W_node val[2];
  W_node high_val[2];
  W_node expr[2];

  WN *copy[2];
#ifdef KEY /* Bug 10177 */
  copy[0] = copy[1] = 0;
#endif /* KEY Bug 10177 */
  WN *wn1;

  WN *last_node;
  WN *default_label;
  LABEL_IDX label;
  INT32 remaining_cases;
  LABEL_IDX new_label_num=0;
  OPERATOR opr;

  if (low_value_pres && high_value_pres)
    cwh_expr_str_operand(high_val);
  
  cwh_expr_str_operand(val);
  label     = cwh_stk_pop_LB();
  last_node = cwh_expr_operand(NULL);
  cwh_expr_str_operand(expr);
  default_label = cwh_expr_operand(NULL);
  remaining_cases = WN_const_val(cwh_expr_operand(NULL));
  (void) New_LABEL (CURRENT_SYMTAB, new_label_num);
  
  if (remaining_cases > 0) {
    copy[0] = WN_COPY_Tree(W_wn(expr[0]));
    copy[1] = WN_COPY_Tree(W_wn(expr[1]));
  }
  
  if (low_value_pres && high_value_pres) {

    WN *cpy[2];

    cpy[0] = WN_COPY_Tree(W_wn(expr[0]));
    cpy[1] = WN_COPY_Tree(W_wn(expr[1]));

    last_node = cwh_stmt_str_falsebr_util(OPR_GE,
					  expr,
					  val,
					  new_label_num,
					  last_node);

    W_wn(expr[0]) = cpy[0];
    W_wn(expr[1]) = cpy[1];

    last_node = cwh_stmt_str_falsebr_util(OPR_LE,
					  expr,
					  high_val,
					  new_label_num,
					  last_node);
  } else {
    
    if (low_value_pres) 
      opr = OPR_GE;
    else if (high_value_pres) 
      opr = OPR_LE;
    else
      opr = OPR_EQ;
    
    last_node = cwh_stmt_str_falsebr_util(opr,
					  expr,
					  val,
					  new_label_num,
					  last_node);
  }    
  
  wn1 = WN_CreateGoto(label);
  cwh_block_insert_after(last_node, wn1);
  last_node = wn1;
  
  wn1 = WN_CreateLabel(new_label_num,  0,NULL);
  cwh_block_insert_after(last_node, wn1);
  last_node = wn1;
  
  remaining_cases = remaining_cases - 1;

  if (remaining_cases != 0) {

    wn1 = WN_CreateIntconst(OPC_I4INTCONST, remaining_cases);
    cwh_stk_push(wn1, WN_item);
    cwh_stk_push(default_label, WN_item);
    cwh_stk_push_STR(copy[0], copy[1],W_ty(expr[1]),WN_item);       
    cwh_stk_push(last_node, WN_item);

    if (case_follows)
      cwh_stk_push(cast_to_void((INTPTR)label), LB_item);

  } else {

    cwh_block_insert_after(last_node, default_label);
  }
}

/*===============================================
 *
 * cwh_stmt_select_falsebr_util
 *
 * Utility function for cwh_stmt_select_case_char.
 * Sets up a comparison between the two operands,
 * and adds a Falsebr on the result to label. 
 *
 * Doesn't add to current block, but to a deferred 
 * list of WNs.
 * 
 *===============================================
 */
static WN *
cwh_stmt_str_falsebr_util(OPERATOR opr, 
			  W_node expr[2], 
			  W_node val[2], 
			  INT32 label,
			  WN *last_node)
{
  WN * test;
  WN * wn1 ;

  cwh_stk_push_STR(W_wn(expr[0]),W_wn(expr[1]),W_ty(expr[1]),WN_item);
  cwh_stk_push_STR(W_wn(val[0]), W_wn(val[1]), W_ty(val[1]), WN_item);

  cwh_expr_compare(opr,W_ty(expr[0]));

  test = cwh_expr_operand(NULL);
  wn1  = WN_CreateFalsebr(label, test);
  cwh_block_insert_after(last_node, wn1);

  return wn1 ;
}

/*===============================================
 *
 * fei_new_select
 *
 * Handles the fortran 90 select statement. The expression
 * controlling the select is on top of the stack. The two args
 * to the routine are:
 * 1. Number of SELECT cases.
 * 2. Symbol table index for default label.
 * Case statements such as " case(10:20,31) are split into
 * case(10:20) and case(31) when counting # of SELECT case statements.
 *
 * The select is lowered into an OPC_SWITCH. A block is generated
 * where fei_new_select_case later emits the CASEGOTO's. Also, to
 * handle ranges, the routine first stores the select expression
 * into a a temp and remembers this position in 'last_node', so
 * that later in fei_new_select_case it knows where to emit the code
 * to handle ranges. 
 * 
 * Before exit, the routine will push the following items back on 
 * the stack:
 * 1. num_cases
 * 2. Block where the case goto's will be emitted
 * 3. Temp which holds the select expression
 * 4. last node, position within current block, where the
 *    store to the temp was done.
 *
 *===============================================
 */

void
fei_new_select(INT32 num_cases,
#ifdef KEY /* Bug 12319 */
               INTPTR last_label_idx,
#endif /* KEY Bug 12319 */
               INTPTR default_label_idx )
{
  WN *parent_block;
  WN *wn;
  WN *wn1;
  WN *expr;
  WN *default_label;
  WN *last_node;
  LABEL_IDX lb;
  ST *tmp_st;
  TY_IDX ty;

  if (cwh_stk_get_class() == STR_item) {

     cwh_stmt_select_char(num_cases, default_label_idx);

  } else {
     
     expr = cwh_expr_operand(NULL);

     if ( num_cases > 0) {

       ty = Be_Type_Tbl(WN_rtype(expr));
       tmp_st = cwh_stab_temp_ST(ty, "select_expr");
       cwh_addr_store_ST(tmp_st, 0, ty, WN_COPY_Tree(expr));
       expr = cwh_addr_load_ST(tmp_st, 0, 0);
       last_node = WN_last(cwh_block_current());
       
       /* Create a new block; this is where the CASEGOTO's will be emitted */
   
       parent_block = cwh_block_new_and_current();
   
       lb = cast_to_LB(default_label_idx);
       default_label = WN_CreateGoto (lb);
       wn = WN_CreateSwitch (num_cases, expr, cwh_block_current(), default_label, 0);
   
       /* Now push num_cases, the block that will contain the */ 
       /* case goto's, expr and last_node back on the stack   */
	
       wn1 = WN_CreateIntconst(OPC_I4INTCONST, num_cases);
       cwh_stk_push(wn1, WN_item);
       cwh_stk_push(cwh_block_current(), WN_item);
       cwh_stk_push(expr, WN_item);
       cwh_stk_push(last_node, WN_item);
       
       /* Now get back to parent block and append the OPC_SWITCH */
       
       cwh_block_set_current(parent_block);
       cwh_block_append(wn);
#ifdef KEY /* Bug 12319 */
       LABEL_IDX last_lb = cast_to_LB(last_label_idx);
       WN_last_label(wn) = last_lb;
#endif /* KEY Bug 12319 */

     } else {  /* empty select */

       WN_DELETE_Tree(expr);
     }
   }
}

/*===============================================
 *
 * fei_new_select_case
 *
 * Handle individual cases in a select statement. On entry,
 * the stack holds the following items, starting from the top:
 * 1. high_range if present
 * 2. low_range if present
 * 3. Label to branch to
 * 4. last_node, position within current block where to emit code to
 *    handle ranges
 * 5. temp that holds the select expression
 * 6. Block where the CASEGOTO's will be emitted
 * 7. Remaining cases to be handled for this select
 *
 * The args to the routine are:
 * 1. flag to indicate low range present
 * 2. flag to indicate high range present
 * 3. flag to indicate if this is followed by a case which needs a branch to
 *    the same label; eg. case (-1, 0), will come as case(-1) and
 *    case(0) and for case(-1) this flag will be true, because case(0)
 *    requires a branch to the same label.
 * 
 * The case is converted into a CASEGOTO. If a range is present, a 
 * comparison is generated between the temp that holds the select expr
 * and the range, and if satisfied, the temp is set to to the lower bound
 * of the range if present, else it is set to the upper bound. The value
 * that the temp is set to is then used in the CASEGOTO. 
 *
 * On exit, the routine is expected to push the following items back on 
 * the stack, if there are any remaining cases for this SELECT:
 * 1. If case_follows is TRUE, push the label back.
 * 2. remaining cases
 * 3. Block that holds the CASEGOTO's
 * 4. temp that holds the select expr.
 * 5. last_node, position within current block where the code to handle
 *    ranges is emitted
 *
 *
 *===============================================
 */

void
fei_new_select_case(INT64 low_value_pres,
                    INT64 high_value_pres,
                    INT32 case_follows)
{
  WN *o_val;
#ifdef KEY /* Bug 10177 */
  WN *high_val = 0;
#else /* KEY Bug 10177 */
  WN *high_val;
#endif /* KEY Bug 10177 */
  WN *casegoto_block;
  WN *wn;
  WN *wn1;
  WN *expr;
  WN *last_node;
  LABEL_IDX label;
  TY_IDX ty;
  INT32 remaining_cases;
  LABEL_IDX new_label_num=0;

  if (cwh_stk_get_class() == STR_item) {
     
     cwh_stmt_select_case_char(low_value_pres, high_value_pres, 
			       case_follows);

   } else {

     if (low_value_pres && high_value_pres) 
       high_val = cwh_expr_operand(NULL);

     o_val = cwh_expr_operand(NULL);    
     label = cwh_stk_pop_LB();

     last_node = cwh_expr_operand(NULL);
     expr = cwh_expr_operand(NULL);
     casegoto_block  = cwh_expr_operand(NULL);
     remaining_cases = WN_const_val(cwh_expr_operand(NULL));

     if (low_value_pres ||  high_value_pres) {      /* if not empty or default case */

       ty = Be_Type_Tbl(WN_rtype(expr));
       (void) New_LABEL (CURRENT_SYMTAB, new_label_num);

       if (low_value_pres && high_value_pres) {

	 wn1 = cwh_stmt_falsebr(WN_COPY_Tree(expr), 
				WN_COPY_Tree(o_val), 
				ty, 
				OPR_GE, 
				new_label_num);
	 
	 cwh_block_insert_after(last_node, wn1);
	 last_node = wn1;
	 
	 wn1 = cwh_stmt_falsebr(WN_COPY_Tree(expr),
				WN_COPY_Tree(high_val), 
				ty, 
				OPR_LE, 
				new_label_num);
	 
       }  else {  /* not both, one of high & low */

	 OPERATOR opr = OPR_LE;

	 if (low_value_pres)
	   opr = OPR_GE;
	   
	 wn1 = cwh_stmt_falsebr(WN_COPY_Tree(expr), 
				WN_COPY_Tree(o_val), 
				ty, 
				opr, 
				new_label_num);
	 
       }  

       cwh_block_insert_after(last_node, wn1);
       last_node = wn1;

       wn1 = cwh_addr_stid (WN_st(expr), 0, ty, WN_COPY_Tree(o_val));
       cwh_block_insert_after(last_node, wn1);
       last_node = wn1;

       wn1 = WN_CreateLabel(new_label_num,  0,NULL);
       cwh_block_insert_after(last_node, wn1);
       last_node = wn1;

     }
     wn = WN_CreateCasegoto(WN_const_val(o_val),label);

     cwh_block_append_given_block(wn,casegoto_block);

     remaining_cases = remaining_cases - 1;

     if (remaining_cases != 0) {

       wn1 = WN_CreateIntconst(OPC_I4INTCONST, remaining_cases);
       cwh_stk_push(wn1, WN_item);
       cwh_stk_push(casegoto_block, WN_item);
       cwh_stk_push(expr, WN_item);
       cwh_stk_push(last_node,  WN_item);
       if (case_follows)
	 cwh_stk_push(cast_to_void((INTPTR)label), LB_item);
     }

   }
}

/*===============================================
 *
 * fei_label_def_named
 *
 * Generate an OPC_LABEL at the definition.
 * lbl_idx has the ST of the label.
 *
 * Directives may be set as flags..
 *
 *===============================================
 */ 
/*ARGSUSED*/
void fei_label_def_named(INTPTR         lbl_idx,
			 INT64   label_flag_word,
			 INT32         lineno,
			 INT32         sup_cnt,
			 INT32         keepme,
			 INT32         storage_seg,
			 INT32         safevl,
			 INT32         unroll_cnt,
			 char          *mark_name,
			 INT32         pipe_cnt,
			 INT32         last_argument,
			 INT32         unused1,
			 INT32         unused2,
			 INT32         unused3)
{
  WN * wn ;
  LABEL_IDX lb  ;
  WN * expr;
  

  if (!test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_NOT_REFERENCED)) { 
     lb = cast_to_LB(lbl_idx);
     wn = WN_CreateLabel(lb,0,NULL);
     
     if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_CASE))
	cwh_stk_push(cast_to_void((INTPTR)lb), LB_item);
    
     cwh_block_append(wn) ;
  }  

#ifdef _SGI_DIRS

  /* handle attached directives */

  if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_MAXCPUS)) {
 
     expr = cwh_expr_operand(NULL);
     cwh_stmt_add_xpragma(WN_PRAGMA_CRI_MAXCPUS,FALSE,expr);

  }
  if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_SUPPRESS)) {
    cwh_directive_barrier_insert(NULL,sup_cnt);
  }

  if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_LOOPCHK)) {
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_PERMUTATION)) {
     /* cdir$permutation - DLAI */
     /* use KAP's ASSERT PERMUTATION for now */
     cwh_stmt_add_pragma(WN_PRAGMA_KAP_ASSERT_PERMUTATION);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_IVDEP)) {
     cwh_stmt_add_pragma(WN_PRAGMA_IVDEP);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_NOREDUCE)) {
     cwh_stmt_add_pragma(WN_PRAGMA_NORECURRENCE);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_SHORTLOOP)) {
     cwh_stmt_add_pragma(WN_PRAGMA_CRI_SHORTLOOP,FALSE, NULL,64);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_DO_BL)) {
     cwh_stmt_add_pragma(WN_PRAGMA_CRI_BL,FALSE, NULL,1);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_CONCCALLS)) {
     cwh_stmt_add_pragma(WN_PRAGMA_KAP_ASSERT_CONCURRENT_CALL);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_NEXTSCALAR)) {
     cwh_stmt_add_pragma(WN_PRAGMA_NEXT_SCALAR);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_SHORTLOOP128)) {
     cwh_stmt_add_pragma(WN_PRAGMA_CRI_SHORTLOOP,FALSE, NULL,128);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_SELECT_TASK)) {
     cwh_stmt_add_pragma(WN_PRAGMA_CRI_PREFERTASK);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_NOTASK)) {
     cwh_stmt_add_pragma(WN_PRAGMA_KAP_ASSERT_DO,FALSE, NULL,ASSERT_DO_SERIAL);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_UNROLL)) {
      /* An unroll count of 0 is the default unroll, which means no pragme is needed */
      if (unroll_cnt != 0) {
	 cwh_stmt_add_pragma(WN_PRAGMA_UNROLL,FALSE, NULL,unroll_cnt,-1);
      }
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_FISSIONABLE)) {
     cwh_stmt_add_pragma(WN_PRAGMA_FISSIONABLE);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_FUSABLE)) {
     cwh_stmt_add_pragma(WN_PRAGMA_FUSEABLE);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_NOFISSION)) {
     cwh_stmt_add_pragma(WN_PRAGMA_NO_FISSION);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_NOFUSION)) {
     cwh_stmt_add_pragma(WN_PRAGMA_NO_FUSION);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_NOINTERCHANGE)) {
     cwh_stmt_add_pragma(WN_PRAGMA_NO_INTERCHANGE);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_NOBLOCKING)) {
     cwh_stmt_add_pragma(WN_PRAGMA_NO_BLOCKING);
   }
   if (test_flag(label_flag_word, FEI_LABEL_DEF_NAMED_AGGRESSIVEINNERLOOPFISSION)) {
     cwh_stmt_add_pragma(WN_PRAGMA_AGGRESSIVE_INNER_LOOP_FISSION);
   }
  }
#endif /* _SGI_DIRS */
}

/*===============================================
 *
 * fei_brtrue
 *
 * Generate an OPC_TRUEBR to the label supplied.
 * TOS has the condition as T/F. 
 *
 *===============================================
 */ 
extern void 
fei_brtrue(INTPTR lbl_idx)
{
  WN *wn;
  WN *wc;
  LABEL_IDX lb ;

  lb = cast_to_LB(lbl_idx);
  wc = cwh_expr_operand(NULL);
  wn = WN_CreateTruebr(lb,wc);
  cwh_block_append(wn) ;
}

/*===============================================
 *
 * fei_where
 *
 * Generate an OPC_WHERE. The FFE has already
 * lowered WHERE blocks into WHERE statements,
 * so the ELSE clause of the WHERE is always
 * a empty block. The mask is a temp, or an 
 * lnot of a temp.
 *
 * TOS has the rhs, mask and expression. Tack
 * an OPC_ARRAYEXP around the mask. Change the
 * current block for the expression under the 
 * mask, then make an empty block, then switch 
 * back the original block to append the OPC_WHERE 
 * (and subsequent stmts) 
 *
 *===============================================
 */ 
extern void 
fei_where(INT32 defined_asg,
          INT32 inline_state)
{
  WN *msk ;
  WN *wn  ;
  WN *wl  ;
  TYPE dummy_type;
#ifdef KEY /* Bug 10177 */
  memset(&dummy_type, 0, sizeof(dummy_type));
#endif /* KEY Bug 10177 */
  INT64 flags = 0;

  msk = cwh_expr_operand(NULL);
  msk = F90_Wrap_ARREXP(msk);

  wl = cwh_block_new_and_current(); 

  wn = WN_Create(OPC_WHERE,3);
  WN_kid0(wn) = msk ;
  WN_kid1(wn) = cwh_block_current();

  if (defined_asg) {
    dummy_type = fei_descriptor(0,
                                Basic,
                                0,
                                V_oid,
                                0,
                                0);
    fei_call(2, dummy_type, By_Value_Call, FALSE, inline_state, flags);
  }
  else {
    fei_store(dummy_type);  /* It's OK for this to be uninitialized */
  }

  (void)  cwh_block_new_and_current(); 

  WN_kid2(wn) = cwh_block_current();

  cwh_block_set_current(wl);
  cwh_block_append(wn);

}

/*===============================================
 *
 * fei_stop
 * 
 * Generate a INTRIN_F90_STOP
 *
 * A scalar character stop code is on the stack.
 *
 *===============================================
 */

extern void
fei_stop( void )
{
  WN	*wa;
  WN	*wc;
  WN	*wn;
#ifdef KEY /* Bug 10177 */
  WN	*stop_code = 0;
  WN	*stop_code_len = 0;
#else /* KEY Bug 10177 */
  WN	*stop_code;
  WN	*stop_code_len;
#endif /* KEY Bug 10177 */

  if (cwh_stk_get_class() == STR_item) {
    cwh_stk_pop_STR();
    wa = cwh_stk_pop_WN();
    wc = WN_COPY_Tree(wa);
    stop_code_len = cwh_intrin_wrap_value_parm(wa);
    wa = cwh_stk_pop_ADDR();
    stop_code = cwh_intrin_wrap_char_parm(wa,wc);
  }
  else {
    DevAssert((0),("expected character stop code"));
  }

  wn = WN_Create ( OPC_VINTRINSIC_CALL, 2);
  WN_Set_Call_Default_Flags(wn);

  if (FE_Call_Never_Return)
    WN_Set_Call_Never_Return (wn);

  WN_kid0(wn) = stop_code;
  WN_kid1(wn) = stop_code_len;

  WN_intrinsic(wn) = INTRN_STOP_F90;

  cwh_block_append(wn);
}

/*===============================================
 *
 * fei_return
 *
 * Generate a return  - kind == 2 is void, so
 * just return. kind == 1 is a value, in a result
 * variable whose ST is TOS. kind =3 is an alternate
 * return whose index is a constant on the stack.
 *
 * If returning 
 *  - a scalar, in registers, then load the value
 *    and store into a preg. The exception is CQ
 *    results which have dummy arg introduced as
 *    a ref to the result.
 *
 *  - a character result, it's passed as a dummy, 
 *    but makes it here. A CASSIGNMENT has already
 *    been done, so punt.
 *
 *  - a derived type, larger than 16 bytes it's 
 *    passed as a dummy. Punt, we've done the store.
 *     Smaller than 16 bytes it's returned in regs.
 *
 * If it's an alternate entry point, we just get
 * the ST of the last entry, so ignore it. Put 
 * out a float and an integer version of the result, 
 * if both are required. 
 *
 * if an ST isn't TOS, then the WN/FLD is an 
 * expression which will be an alternate return index.
 *
 *===============================================
 */ 
/*ARGSUSED*/
extern void 
fei_return(INT return_kind, TYPE dummy)
{
  WN * wn;
  WN * ret_wn = NULL;
  ST * st;
  ST * rt;
  TY_IDX  ty;

  TYPE_ID bt;

  BOOL done_int;
  BOOL done_float;
  
  DevAssert(((return_kind >= 1) && (return_kind <= 3)),
	    (" odd return kind "));
  
  if (( return_kind == 1 ) ||
      ( return_kind == 3 )) {
    
    switch (cwh_stk_get_class()) {
    case ST_item:
    case ST_item_whole_array:
      st = cwh_stk_pop_ST();
      ty = ST_type(st);
      
      if ( WHIRL_Return_Val_On ) {

        if((ST_sclass(st) == SCLASS_FORMAL) &&
           (TY_kind(ty) == KIND_POINTER))
          ty = TY_pointed(ty);

        if ((TY_kind(ty) == KIND_SCALAR ||
             TY_kind(ty) == KIND_STRUCT) &&
            (! ST_auxst_is_rslt_tmp(st)) &&
            (! cwh_types_is_character(ty))) {

          ret_wn = cwh_stmt_return_scalar(st,NULL,ST_type(st),TRUE);
        }
        else {
          /* void return */
          ret_wn = WN_CreateReturn();
        }
      }
      else {

        if (!IS_ALTENTRY_TEMP(st)) {

	  if((ST_sclass(st) == SCLASS_FORMAL) &&
	     (TY_kind(ty) == KIND_POINTER))
	    ty = TY_pointed(ty); 
      
          if ((TY_kind(ty) == KIND_SCALAR) &&
              (! ST_auxst_is_rslt_tmp(st)) &&
	      (! cwh_types_is_character(ty))) {

	    ret_wn = cwh_stmt_return_scalar(st,NULL,ST_type(st),TRUE);	

          } else if (STRUCT_BY_VALUE(ty)) {
	    (void) cwh_stmt_return_scalar(st,NULL,ST_type(st),TRUE);

	  } else {
            /* void return */
            ret_wn = WN_CreateReturn();
          }

        } else {
	
	  done_int   = FALSE;
	  done_float = FALSE;
	

	  ITEM *re = NULL;
          while ((re = cwh_auxst_next_element(ST_base(st),re,l_RETURN_TEMPS)) != NULL ) {
	    rt = I_element(re);
	    bt = TY_mtype(ST_type(rt));

	    if (MTYPE_is_float(bt)) {
	      if (! done_float) {
	        done_float = TRUE;
	        cwh_stmt_return_altentry(rt);
	      }
	    } else if (! done_int) {
	        done_int = TRUE;
	        cwh_stmt_return_altentry(rt);
	    }
	  }
        }
      }
      break;
      

    case WN_item:
    case WN_item_whole_array:
    case DEREF_item:
      wn = cwh_expr_operand(NULL);
      ty = Be_Type_Tbl(WNRTY(wn));
      ret_wn = cwh_stmt_return_scalar(NULL,wn,ty,TRUE);
      break;


    case FLD_item:
      ty = cwh_stk_get_FLD_TY();
      wn = cwh_expr_operand(NULL);
      ret_wn = cwh_stmt_return_scalar(NULL,wn,ty,TRUE);
      break;

    default:
      DevAssert((0),("Odd return"));

    }

    if ( WHIRL_Return_Val_On ) {
      if (ret_wn != NULL) {
        cwh_block_append(ret_wn);
      }
    }
    else {
      wn = WN_CreateReturn();
      cwh_block_append(wn) ;
    }
  }    
  else {
    /* void return, return_kind == 2 */
    wn = WN_CreateReturn();
    cwh_block_append(wn) ;
  }
}

/*===============================================
 *
 * cwh_stmt_return_scalar
 *
 * Utility for fei_return and fei_call. Takes a 
 * scalar ST/WN, and reads/writes the value and 
 * returns it in the correct preg. The TY is the
 * that of the result (eg: of the ST, if present)
 * 
 * If this is in a callee, the ST of the result 
 * variable is loaded, and put into a preg,
 * or if ST is NULL and a WN is present 
 * (eg: a constant), that's put into the preg
 * 
 * In a caller the result WN has a load of the preg,
 * the ST is usually NULL and the TY is that of the
 * value. If its a struct by value, then there is
 * a result temp, and a NULL is returned (don't
 * push it as there won't be an fei_store..)
 *
 * If a call the result WN has a load of the preg,
 * the ST is NULL and the TY is that of the value.
 *
 *===============================================
 */ 
extern WN *
cwh_stmt_return_scalar(ST *st, WN * rv, TY_IDX  rty, BOOL callee_return)
{
  TYPE_ID   rbtype1;
  TYPE_ID   rbtype2;
  PREG_NUM  rreg1;
  PREG_NUM  rreg2;


#ifdef KEY /* Bug 10177 */
  WN  * wn = 0;
#else /* KEY Bug 10177 */
  WN  * wn  ;
#endif /* KEY Bug 10177 */
  WN  * wn2  ;
  ST  * pr1 ;
  ST  * pr2 ;
  OFFSET_64 off;

  if (WHIRL_Return_Info_On) {

    RETURN_INFO return_info = Get_Return_Info (rty, Use_Simulated);

    if (RETURN_INFO_count(return_info) <= 2 ||
        WHIRL_Return_Val_On) {

      rbtype1 = RETURN_INFO_mtype (return_info, 0);
      rbtype2 = RETURN_INFO_mtype (return_info, 1);
      rreg1 = RETURN_INFO_preg (return_info, 0);
      rreg2 = RETURN_INFO_preg (return_info, 1);
    }

    else
      Fail_FmtAssertion ("cwh_stmt_return_scalar: more than 2 return registers");
  }

  else {
    Get_Return_Mtypes(rty, Use_Simulated, &rbtype1, &rbtype2);
    Get_Return_Pregs(rbtype1, rbtype2, &rreg1, &rreg2);
  }

  pr1 = MTYPE_To_PREG(rbtype1);
  pr2 = MTYPE_To_PREG(rbtype2);

  if (callee_return) {

    if ( WHIRL_Return_Val_On ) {
      if (st == NULL) {
        wn = WN_CreateReturn_Val (OPR_RETURN_VAL, TY_mtype (rty), MTYPE_V, rv);
        Set_PU_has_very_high_whirl (Get_Current_PU ());
      }
      else {

# if (defined(linux) || defined(BUILD_OS_DARWIN))
        wn2 = cwh_addr_load_ST(st,0,0);
        wn = WN_CreateReturn_Val (OPR_RETURN_VAL, TY_mtype (rty), MTYPE_V, wn2);
# else
        if (IS_ALTENTRY_TEMP(st)) {
          wn2 = cwh_addr_ldid(ST_base(st),0,ST_type(ST_base(st)));
          wn = WN_CreateReturn_Val (OPR_RETURN_VAL, TY_mtype (ST_type(ST_base(st))), MTYPE_V, wn2);
        } else {
          wn2 = cwh_addr_load_ST(st,0,NULL);
          wn = WN_CreateReturn_Val (OPR_RETURN_VAL, TY_mtype (rty), MTYPE_V, wn2);
        }
# endif

        Set_PU_has_very_high_whirl (Get_Current_PU ());
      }
    }
    else {
      if (st == NULL)
        cwh_addr_store_ST(pr1,rreg1,rty,rv);

      else {

        if (TY_kind(ST_type(st)) == KIND_STRUCT) {

          wn = cwh_addr_mk_ldid(st,0,rbtype1,rty);
          cwh_addr_store_ST(pr1,rreg1,Be_Type_Tbl(rbtype1),wn);

          if (rreg2 !=0) {

            off = PREG2_OFFSET(pr1,pr2);
            wn  = cwh_addr_mk_ldid(st,off,rbtype2,rty);
            cwh_addr_store_ST(pr2,rreg2,Be_Type_Tbl(rbtype2),wn);
          }

        } else if (IS_ALTENTRY_TEMP(st)) {

          wn = cwh_addr_ldid(ST_base(st),0,rty);
          cwh_addr_store_ST(pr1,rreg1,rty,wn);

        } else {

          wn = cwh_addr_load_ST(st,0,0);
          cwh_addr_store_ST(pr1,rreg1,rty,wn);
        }
      }
    }
  } else {    /* caller return */
    
    if ( WHIRL_Return_Val_On ) {
      wn = cwh_addr_mk_ldid(Return_Val_Preg,-1, TY_mtype (rty), rty);


      if (STRUCT_BY_VALUE(rty)) {

	/* result into temp that was 1st arg, & don't push result WN */

        cwh_addr_store_ST(st,0,rty,wn);
	wn = NULL;
      }
    }
    else {
      /* caller - read result in pregs - if struct return */
      /* temp store pregs into temp, return temp          */

      wn = cwh_addr_load_ST(pr1,rreg1,Be_Type_Tbl(rbtype1));

    }
  }
  
  return wn;
}

/*===============================================
 *
 * cwh_stmt_return_altentry
 *
 * Utility for fei_return to return a shared altentry
 * result temp. This contains special return values.
 *
 *===============================================
 */ 
static void
cwh_stmt_return_altentry(ST *st)
{
  TYPE_ID   rbtype1;
  TYPE_ID   rbtype2;
  TYPE_ID   bt;

  PREG_NUM  rreg1;
  PREG_NUM  rreg2;

  WN  * wn;
  WN  * wn2;
  ST  * pr;
  TY_IDX rty;
  ST  ** p;
  BOOL same;

                    
  same = ST_auxst_altentry_shareTY(ST_base(st));
  rty  = cwh_stab_altentry_TY(st,same);

  if (TY_mtype(rty) == MTYPE_CQ) {

    p  = cwh_auxst_arglist(Procedure_ST) ;
    wn = cwh_addr_load_ST(st,0,0);

    if ( WHIRL_Return_Val_On ) {
      wn2 = WN_CreateReturn_Val (OPR_RETURN_VAL, TY_mtype (rty), MTYPE_V, wn);
      cwh_block_append(wn2);
      Set_PU_has_very_high_whirl (Get_Current_PU ());
    }
    else {
      cwh_addr_store_ST(p[0],0,0,wn);
    }

  } else {
    
    if ( WHIRL_Return_Val_On ) {

      wn = cwh_addr_ldid(ST_base(st),0,rty);

      wn2 = WN_CreateReturn_Val (OPR_RETURN_VAL, TY_mtype (rty), MTYPE_V, wn);
      cwh_block_append(wn2);
      Set_PU_has_very_high_whirl (Get_Current_PU ());
    }
    else {
      if (WHIRL_Return_Info_On) {

        RETURN_INFO return_info = Get_Return_Info (rty, Use_Simulated);

        if (RETURN_INFO_count(return_info) <= 2) {

          rbtype1 = RETURN_INFO_mtype (return_info, 0);
          rbtype2 = RETURN_INFO_mtype (return_info, 1);
          rreg1 = RETURN_INFO_preg (return_info, 0);
          rreg2 = RETURN_INFO_preg (return_info, 1);
        }

        else
          Fail_FmtAssertion ("cwh_stmt_alt_entry: more than 2 return registers");
      }

      else {
        Get_Return_Mtypes(rty, Use_Simulated, &rbtype1, &rbtype2);
        Get_Return_Pregs(rbtype1, rbtype2, &rreg1, &rreg2);
      }

      pr = MTYPE_To_PREG(rbtype1);

      wn = cwh_addr_ldid(ST_base(st),0,rty);
      bt = TY_mtype(rty);

      if (MTYPE_is_float(bt) && !same) {

        if (bt == MTYPE_C4) {
          wn = WN_CreateStid (OPC_C4STID, 32, Float32_Preg,rty,wn);
          cwh_block_append(wn);
        } else if (TY_size(rty) <= TY_size(Be_Type_Tbl(MTYPE_F8)))
          cwh_addr_store_ST(pr,rreg1,rty,wn);
        else {
          wn = WN_CreateStid (OPC_FQSTID, 32, Float64_Preg, rty, wn );
          cwh_block_append(wn);
        }

      } else
        cwh_addr_store_ST(pr,rreg1,rty,wn);
    }
  }
}

/*===============================================
 *
 * fei_concat
 *
 * Generate the intrinsic for concatenation.
 * The number of operands is the number of 
 * STR_items to pop from the stack. Addresses go
 * in the first half of the argument list, lengths 
 * in the second half. 
 *
 * A destination temp is allocated here, used as
 * 0'th arg (length 1st arg) , pushed onto the 
 * stack and will be copied by fei_store.
 *
 *===============================================
 */ 
extern void 
fei_concat(INT32 numops)
{
  INT32 i,nm,k,sc;
  WN ** sz ;
  WN ** wn ;
  WN  * rsz;
  WN  * wt ;
  WN  * ae ;
  TY_IDX ty ;  
  BOOL *va ;
  WN  *wr;

  ae  = NULL ;
  sc  = numops + 1 ;
  nm  = 2 * sc ;
  sz  = (WN **) malloc(nm * sizeof(WN *)) ;
  wn  = (WN **) malloc(nm * sizeof(WN *)) ;
  va  = (BOOL *) malloc(nm * sizeof(BOOL)) ;
  rsz = WN_Zerocon(cwh_bound_int_typeid);

  for (i = sc ; i >= 2 ;  i--) {
    k = i + numops ;
    cwh_stk_pop_STR();
    wn[k] = cwh_stk_pop_WN();
    wn[i] = F90_Wrap_ARREXP(cwh_expr_address(f_T_PASSED));
    if (WNOPR(wn[i]) == OPR_ARRAYEXP)
      ae = wn[i] ;
    sz[k] = NULL;
    sz[i] = WN_COPY_Tree(wn[k]) ;
    va[k] = TRUE;
    va[i] = FALSE;
    rsz   = cwh_expr_bincalc(OPR_ADD,rsz,WN_COPY_Tree(wn[k]));
  }

  /* if an ARRAYEXP (ae) appeared it was an elemental */
  /* concat and an array valued temp is needed        */

  ty = cwh_types_mk_character_TY(WN_COPY_Tree(rsz),NULL,TRUE);

  if (ae != NULL) {
     ty = cwh_types_array_temp_TY(ae,ty) ;
     wt = cwh_expr_temp(ty,WN_COPY_Tree(rsz),f_T_PASSED);     
     wt = cwh_addr_temp_section(wt,ty);
     wr = WN_COPY_Tree(wt);
     wt = F90_Wrap_ARREXP(wt);
  } else {
     /* character array case */
     WN *e_sz = WN_Intconst(MTYPE_I4,1);
     wt = cwh_expr_temp(ty,WN_COPY_Tree(e_sz),f_T_PASSED);
     wr = WN_COPY_Tree(wt) ;
  }

  wn[0] = wt;
  wn[1] = WN_COPY_Tree(rsz) ;
  sz[0] = WN_COPY_Tree(rsz) ;
  sz[1] = NULL ;
  va[0] = FALSE;
  va[1] = TRUE ;

  cwh_intrin_call(INTRN_CONCATEXPR,nm,wn,sz,va,MTYPE_V);

  cwh_stk_push_STR(rsz,wr,ty,WN_item);

  free(sz);
  free(wn);
  free(va);
}

/*===============================================
 *
 * cwh_stmt_character_icall
 *
 * This is a character intrinsic call. The stack contains
 * two STR items - second argument on top. Pop the length
 * and address of each side, then make the
 * intrinsic call. 
 * (args are addr_lhs,addr_rhs,sz_lhs,sz_rhs)
 *
 * Sizes are passed to create OPC_PARM types.
 *
 *===============================================
 */ 
extern void
cwh_stmt_character_icall(INTRINSIC intrinsic)
{
  WN * ar[4];
  WN * sz[4];
  BOOL va[4];

  cwh_stk_pop_STR();
  ar[3] = cwh_expr_operand(NULL);
  ar[1] = cwh_expr_address(f_NONE);
  ar[1] = F90_Wrap_ARREXP(ar[1]);

  sz[3] = NULL;
  sz[1] = WN_COPY_Tree(ar[3]);
  va[3] = TRUE;
  va[1] = FALSE;

  cwh_stk_pop_STR();
  ar[2] = cwh_expr_operand(NULL);
  ar[0] = cwh_expr_address(f_NONE);
  ar[0] = F90_Wrap_ARREXP(ar[0]);

  sz[2] = NULL;
  sz[0] = WN_COPY_Tree(ar[2]);
  va[2] = TRUE;
  va[0] = FALSE;

#ifdef KEY /* Bug 10670 */
  WN *wn = cwh_intrin_call(intrinsic,4,ar,sz,va,MTYPE_V);
  WN_Set_Linenum(wn, USRCPOS_srcpos(current_srcpos));
#else /* KEY Bug 10670 */
  cwh_intrin_call(intrinsic,4,ar,sz,va,MTYPE_V);
#endif /* KEY Bug 10670 */
}

/*===============================================
 *
 * cwh_stmt_add_to_preamble
 *
 * Append the pragma WN argument to the callsite
 * block of the preamble. Check to see if the 
 * blocks have been set up - if not, then this
 * called from the declaration setup eg: a bounds 
 * expression in an ARRAY TY, so it's ignored.
 * 
 *===============================================
 */ 
extern BOOL
cwh_stmt_add_to_preamble(WN *wn,enum site block)
{
  BOOL res = FALSE; 

  if (block == block_ca)
    if (WN_pragma_ca != NULL) {
      WN_INSERT_BlockFirst (WN_pragma_ca,wn);
      res = TRUE;
    }

  if (block == block_pu)
    if (WN_pragma_pu != NULL) {
      WN_INSERT_BlockFirst (WN_pragma_pu,wn);
      res = TRUE;
    }

  return res; 
}

/*===============================================
 *
 * cwh_stmt_add_pragma
 *
 * Generate a PRAGMA node and add to the current
 * block. All args except the id are default NULL.
 *
 *===============================================
 */ 
extern void
cwh_stmt_add_pragma(WN_PRAGMA_ID  wn_pragma_id,
		    BOOL          is_omp,
		    ST	         *st,
		    INT32         arg1,
		    INT32         arg2)
{
  WN *wn;
  wn = WN_CreatePragma(wn_pragma_id, st, arg1, arg2);
  if (is_omp)
    WN_set_pragma_omp(wn);
  cwh_block_append(wn);
}

#ifdef KEY /* Bug 2660 */
/*===============================================
 *
 * cwh_stmt_add_options_pragma
 *
 * Generate a PRAGMA OPTIONS node and add it to the pragma
 * block for the current program unit.
 *
 *===============================================
 */ 
extern void
cwh_stmt_add_options_pragma(ST *st)
{
  /* Imitate handling of command-line switch -chunk=n; see
   * cwh_stmt_add_parallel_pragmas */
  WN *wn = WN_CreatePragma(WN_PRAGMA_OPTIONS, st, 0, 0);
  cwh_stmt_add_to_preamble(wn,block_pu);
}
#endif /* KEY Bug 2660 */

/*===============================================
 *
 * cwh_stmt_add_xpragma
 *
 * Generate a XPRAGMA node with a single kid 
 * and add to the current block. Arg will be kid0 
 * of xpragma. Omp and expr are default NULL.
 *
 *===============================================
 */ 
extern void
cwh_stmt_add_xpragma(WN_PRAGMA_ID  wn_pragma_id,
		     BOOL is_omp,
		     WN * expr)
{
  WN *wn;
  wn = WN_CreateXpragma(wn_pragma_id, (ST_IDX) NULL, 1);
  WN_kid0(wn) = expr;
  if (is_omp)
    WN_set_pragma_omp(wn);  
  cwh_block_append(wn);
}

/*================================================================
 *
 * fei_enddo
 * 
 * Pop the DOLOOP block, & leave block of loop body.
 *
 *================================================================
 */
void 
fei_enddo(void)
{
  WN *wn;

  if (FE_Endloop_Marker) {
    wn = WN_CreateComment("ENDLOOP");
    cwh_block_append(wn);
    cwh_auxst_clear(WN_st(wn));
  }

  cwh_block_pop_block();
}

/*================================================================
 *
 * fei_dowhile
 * 
 * Create a OPC_DOWHILE. TOS has the expression to
 * be evaluated. Push the current block and build the
 * body in a new block.
 *
 *================================================================
 */
void
fei_dowhile(void)
{
   WN *expr,*block,*w;
   
   expr  = cwh_expr_operand(NULL);
   block = WN_CreateBlock();
   WN_Set_Linenum (block, USRCPOS_srcpos(current_srcpos));
   w = WN_CreateWhileDo(expr,block);
   cwh_block_append(w);

   /* Push current block & set new block for body */

   cwh_block_push_block(NULL,NULL,FALSE);
   cwh_block_set_current(block);
}

/* to check if the step and end expression is of the correct form, 
 * currently we only verify the end opr.
*/
static bool verify_do_loop_sei(WN *loop)
{
   WN *start, *end, *incr;
   DevAssert((WN_opcode(loop) == OPC_DO_LOOP),("The WHIRL node is not DO_LOOP"));
   start = WN_kid1(loop);
   end = WN_kid2(loop);
   incr = WN_kid3(loop);

   if (WN_operator(end) != OPR_LT && WN_operator(end) != OPR_LE
       && WN_operator(end) != OPR_GT && WN_operator(end) != OPR_GE)
     return false;
   

   return true;   
}

WN* convert_doloop_to_while(WN* doloop)
{
// <start>
// WHILE <end>
//  <stmts>
//  <step>
    WN *body; 
    WN *while_body; 
    WN *temp; 
    body = WN_CreateBlock();
    while_body = WN_CreateBlock();

    DevAssert((WN_opcode(doloop) == OPC_DO_LOOP),("The WHIRL node is not DO_LOOP"));
    WN_INSERT_BlockLast(body, WN_COPY_Tree(WN_kid1(doloop)));

    WN_Set_Linenum(while_body, WN_linenum(WN_kid3(doloop)));
    WN_INSERT_BlockLast(while_body, WN_COPY_Tree(WN_kid4(doloop)));
    WN_INSERT_BlockLast(while_body, WN_COPY_Tree(WN_kid3(doloop)));
    temp = WN_CreateWhileDo(WN_COPY_Tree(WN_kid2(doloop)), while_body);
    WN_Set_Linenum(temp, WN_linenum(doloop));
    WN_INSERT_BlockLast(body, temp); 

    return body;        
}

/*================================================================
 *
 * fei_doloop
 * 
 * Create a OPC_DOLOOP. TOS has stride, then ub,lb, variable.
 *
 * First check stride and upper bound, and if expressions move
 * into temps. Then if it's a float loop variable or the stride
 * isn't constant or the loop variable is a pointer the loop 
 * is canonlicalized - count is computed the increment is set 
 * to one and the user's index variable is kept up to date
 * by adding the stride on each iteration. This is done by 
 * maintaining a list of statements which are deferred to 
 * the end of the loop body. A new BLOCK is created and set
 * current to contain the body.
 *
 * For parallel loops, we calculate the user index var based on the
 * normalized index var if the loop is canonlicalized.
 *
 *================================================================
 */
void
fei_doloop(INT32	line)
{
   WN *lb;
   WN *ub,*ubcomp;
   WN *stride,*stride_in_loop;
#ifdef KEY /* Bug 10177 */
   ST *lcv = 0;
#else /* KEY Bug 10177 */
   ST *lcv;
#endif /* KEY Bug 10177 */
   WN *index_id;
   WN *stmts;
   WN *start;
   WN *end;
   WN *step;
   WN *wlcv = NULL;
#ifdef KEY /* Bug 10177 */
   TY_IDX ty = 0;
#else /* KEY Bug 10177 */
   TY_IDX ty;
#endif /* KEY Bug 10177 */

   USRCPOS pos;
   INT32    local_line_num;
   mUINT16  local_file_num;

   TYPE_ID doloop_ty,lcv_t;
   BOOL canonicalize;
   PREG_NUM loop_preg;
   WN *temp, *count;
   WN *deferred_update=NULL;    /* ie: deferred to end of loop body */
   WN *calcu=NULL;		/* calculate user index var */

   WN *doloop;
   WN *body;

   /* 
      example:
      C$DOACROSS NEST(i,j,k)
      DO i		<- is_nested=FALSE
       DO j		<- is_nested=TRUE
	DO k		<- is_nested=TRUE
	 DO l		<- is_nested=FALSE
   */

   BOOL is_top_pdo=FALSE;	/* is this the outermost loop of a PDO? */
   BOOL is_innermost=FALSE;	/* is innermost loop of a nest (if true if not
				   nested) */

   if ((nested_do_descriptor.type == WN_PRAGMA_PDO_BEGIN ||
        nested_do_descriptor.type == WN_PRAGMA_PARALLEL_DO)  &&
       nested_do_descriptor.explicit_end &&
       nested_do_descriptor.current==0 &&
       nested_do_descriptor.depth!=0) {
     is_top_pdo=TRUE;
   }


   if (nested_do_descriptor.depth!=0) {

     /* if a nested parallel do, generate a region for it */

     if (nested_do_descriptor.current>0) {

       body=cwh_mp_region(nested_do_descriptor.type,0,0,0,0,0,0);
       cwh_block_set_current(body);
     }

     nested_do_descriptor.current++;

     if (nested_do_descriptor.current >= nested_do_descriptor.depth) {
       /* this is the last nest, reset the descriptor */
       nested_do_descriptor.depth = 0;
       nested_do_descriptor.current = 0;
       is_innermost=TRUE;
     }
   }


   canonicalize = FALSE;

   stride = cwh_expr_operand(NULL);
   ub = cwh_expr_operand(NULL);
   lb = cwh_expr_operand(NULL);

   /* loop control variable may be ST, or DEREF of pointer - get type */

   if (cwh_stk_get_class() == ST_item || cwh_stk_get_class() == ST_item_whole_array) {

     lcv  = cwh_stk_pop_ST();
     if (ST_sclass(lcv) == SCLASS_FORMAL) {
	lcv_t = TY_mtype(TY_pointed(ST_type(lcv)));
	canonicalize = TRUE;
     } else {
	lcv_t = TY_mtype(ST_type(lcv));
     }

   } else {
     wlcv = cwh_stk_pop_WHIRL();
     ty = cwh_types_WN_TY(wlcv,FALSE);
     lcv_t =  TY_mtype(cwh_types_scalar_TY(ty));
     canonicalize = TRUE;     
   }

   /* create pregs for non-constant bounds or strides   */
   /* or for inconvenient types eg: real do loop varbls */
 
   lb = cwh_convert_to_ty(lb,lcv_t);
   ub = cwh_convert_to_ty(ub,lcv_t);
   stride = cwh_convert_to_ty(stride,lcv_t);

   if (lcv_t != MTYPE_I4 && lcv_t != MTYPE_I8) {
      canonicalize = TRUE;
      doloop_ty = cwh_doloop_typeid;
   } else {
      doloop_ty = lcv_t;
   }

   if (WNOPR(stride) != OPR_INTCONST) {
      canonicalize = TRUE;
   }
   if (WNOPR(stride) != OPR_INTCONST && WNOPR(stride) != OPR_CONST) {
      stride_in_loop = cwh_preg_temp_save("doloop_stride",stride);
   } else {
      stride_in_loop = WN_COPY_Tree(stride);
   }
      
   if (WNOPR(ub) != OPR_INTCONST && WNOPR(ub) != OPR_CONST) {
      ubcomp = cwh_preg_temp_save("doloop_ub",ub);
   } else {
      ubcomp = WN_COPY_Tree(ub);
   }

   /* for loops which can be parallelized, make sure the */
   /* lower bound is a constant or a preg                */

   if (parallel_do_count) {

     if (! ((WNOPR(lb) == OPR_INTCONST) ||
	    (WNOPR(lb) == OPR_LDID && ST_class(WN_st(lb)) == CLASS_PREG))) {
       lb = cwh_preg_temp_save("doloop_lb",lb);
     }
   }
    
   if (canonicalize) {

     /* Initialize lcv  - it needs a temp */

     WN *wc ;

     if (wlcv == NULL) {
       cwh_addr_store_ST(lcv,0,0,WN_COPY_Tree(lb));
       wc = cwh_addr_load_ST(lcv,0,0) ;

     } else {
       cwh_addr_store_WN(wlcv,0,0,WN_COPY_Tree(lb));
       wc = cwh_addr_load_WN(wlcv,0,0) ;
     }

     /* Compute iteration count */
#ifdef KEY // Bug 4660, 8272
     temp  = cwh_addr_extent(WN_COPY_Tree(lb),ub,stride_in_loop);
#else
     temp  = cwh_addr_extent(wc,ub,stride);
#endif
     count = cwh_convert_to_ty(temp,doloop_ty);

     if (WNOPR(count) != OPR_INTCONST) {
       count = cwh_preg_temp_save("doloop_count",count);
     }
     loop_preg = Create_Preg(doloop_ty,Index_To_Str(Save_Str("doloop_var")));
     index_id  = WN_CreateIdname(loop_preg,MTYPE_To_PREG(doloop_ty));

     start = WN_StidPreg(doloop_ty,loop_preg,WN_Intconst(doloop_ty,0));
     end   = WN_CreateExp2(OPCODE_make_op(OPR_LT,MTYPE_I4,doloop_ty),
			   WN_LdidPreg(doloop_ty,loop_preg),
			   count);
     step  = cwh_expr_bincalc(OPR_ADD,WN_LdidPreg(doloop_ty,loop_preg),
			      WN_Intconst(doloop_ty,1));
     step  = WN_StidPreg(doloop_ty,loop_preg,step);

     if (parallel_do_count) { /* parallel, calculate user index */
       calcu = cwh_expr_bincalc(OPR_ADD,WN_COPY_Tree(lb),
	cwh_expr_bincalc(OPR_MPY, WN_LdidPreg(doloop_ty,loop_preg), stride_in_loop));
       if (wlcv)
         calcu = cwh_addr_istore(wlcv,0,ty,calcu);
       else
         calcu = cwh_addr_stid(lcv,0,Be_Type_Tbl(lcv_t),calcu);

     } else { /* not parallel, add stride to user index */

       deferred_update = cwh_expr_bincalc(OPR_ADD,WN_COPY_Tree(wc),stride_in_loop);
       if (wlcv)
         deferred_update = cwh_addr_istore(wlcv,0,ty,deferred_update);
       else
         deferred_update = cwh_addr_stid(lcv,0,Be_Type_Tbl(lcv_t),deferred_update);
     }

     WN_DELETE_Tree(ubcomp);

   } else {
     
     OPERATOR op;
     
     index_id = WN_CreateIdname(0,lcv);
     start = WN_Stid(lcv_t, 0, lcv, Be_Type_Tbl(lcv_t), lb);

     /* Stride is an integer constant (+ve or -ve?)*/

     if (WN_const_val(stride) > 0) 
       op = OPR_LE;
     else
       op = OPR_GE;
     
     end  = WN_CreateExp2(OPCODE_make_op(op,MTYPE_I4,Mtype_comparison(lcv_t)),
			  WN_Ldid(lcv_t,0,lcv,ST_type(lcv)),
			  ubcomp);
     step = cwh_expr_bincalc(OPR_ADD,WN_Ldid(lcv_t,0,lcv,ST_type(lcv)),
			     stride_in_loop);
     step = WN_Stid(lcv_t, 0, lcv, ST_type(lcv), step);
     deferred_update = NULL;
   }
   
   stmts = WN_CreateBlock();
   WN_Set_Linenum (start, USRCPOS_srcpos(current_srcpos) );


   if (line > 0) {  /* 0 means no line number */
      USRCPOS_clear(pos);
      USRCPOS_filenum(pos) = USRCPOS_filenum(current_srcpos);
      USRCPOS_linenum(pos) = global_to_local_line_number(line);
      WN_Set_Linenum (step,  USRCPOS_srcpos(pos));
   }
   else {
      WN_Set_Linenum (step,  USRCPOS_srcpos(current_srcpos));
   }

   WN_Set_Linenum (stmts, USRCPOS_srcpos(current_srcpos) );

   doloop = WN_CreateDO(index_id, start, end, step, stmts, NULL);


   if(!verify_do_loop_sei(doloop))
   {
     WN* whileloop;
     whileloop = convert_doloop_to_while(doloop);
     doloop = whileloop;
   }

   cwh_directive_insert_do_loop_directives();
   cwh_block_append(doloop);

   /* Push the current block & make loop body current block */

   cwh_block_push_block(deferred_update,calcu,is_top_pdo);
   cwh_block_set_current(stmts);

   /* Add any MP directives required to start of the loop body */
      
   if (is_innermost)
     cwh_block_append_given(Top_of_Loop_Block);

   /* add calculation of the user index to the start of the loop body */

   if (calcu) {
     cwh_block_append(WN_COPY_Tree(calcu));
   }
   return;
}

/*================================================================
 *
 * fei_doforever
 * 
 * This is handled by a label & goto. Just keep the block
 * stack consistent for fei_enddo.
 *
 *================================================================
 */
void
fei_doforever(void)
{
   /* Dummy block push */
   cwh_block_push_block(NULL,NULL,FALSE);
}

/*================================================================
 *
 * fei_if
 *
 *================================================================
 */

void
fei_if(void)
{
   WN *test;
   WN *if_then;
   WN *if_else;
   WN *if_cnstrct;

   test = cwh_expr_operand(NULL);

   if_then = WN_CreateBlock();
   if_else = WN_CreateBlock();
   WN_Set_Linenum (if_else, USRCPOS_srcpos(current_srcpos) );
   WN_Set_Linenum (if_then, USRCPOS_srcpos(current_srcpos) );

   if_cnstrct = WN_CreateIf(test, if_then, if_else);

   cwh_block_append(if_cnstrct);

   /* Push the current block */
   cwh_block_push_block(NULL,NULL,FALSE);

   cwh_block_set_current(if_then);

   /* push the if_cnstrct on the stack */
   cwh_stk_push(if_cnstrct, WN_item);

   return;
}

/*================================================================
 *
 * fei_else
 *
 *================================================================
 */

void
fei_else(void)
{
   WN *if_else;
   WN *if_cnstrct;

   /* pop off the if construct */
   if_cnstrct = cwh_stk_pop_WN();

   /* get the else block */
   if_else = WN_kid2(if_cnstrct);

   cwh_block_set_current(if_else);

   /* push the if_cnstrct back on the stack */
   cwh_stk_push(if_cnstrct, WN_item);

   return;
}

/*================================================================
 *
 * fei_endif
 *
 * pop off the if construct from stack
 * 
 *================================================================
 */
void
fei_endif(void)
{
   WN *if_cnstrct;

   if_cnstrct = cwh_stk_pop_WN();

   cwh_block_pop_block();
   return;
}

static ST *allocate_routine_st = NULL;

/*================================================================
 *
 * cwh_inline_allocate
 * 
 * Called for the ALLOCATE statement to do the allocation via the
 * ALLOCATE_SGI intrinsic.This exposes the bounds setup to the optimizer,
 * so bounds can be propagated. Otherwise the whole dope vector is
 * considered to be modified by a call to _ALLOCATE.
 *
 *================================================================
*/

static void 
cwh_inline_allocate(WN **dopes, TY_IDX *types, INT num_dopes, WN *stat)
{
   INT idope,i;
   INT rank;
   WN *dope_addr;
   TY_IDX ty;
   TY_IDX el_ty;
   FLD_HANDLE fl;
   INT64 esize;
   INT64 flag_val;
   WN *size;
   WN *size2;
   WN *assoc;
   WN *flags;
   BOOL is_f90_pointer;
   WN *args[5];
   WN *iop;
   PREG_NUM size_preg;
   PREG_NUM addr_preg;
   TY_IDX addr_ty;

   /* Initialize stat to 0 */
   if (WNOPR(stat) != OPR_INTCONST) {
      cwh_addr_store_WN(WN_COPY_Tree(stat),0,0,WN_Zerocon(MTYPE_I4));
   }

   if (!allocate_routine_st) {
      allocate_routine_st = cwh_intrin_make_intrinsic_symbol("_F90_ALLOCATE_B",Pointer_Mtype);
   }
   

   for (idope=0; idope < num_dopes; idope++) {
      dope_addr = dopes[idope];

      size_preg = Create_Preg(cwh_bound_int_typeid,Index_To_Str(Save_Str("size_preg")));
      
      /* Get the rank and size of the object from the type */
      ty = types[idope];
      if (TY_kind(ty) == KIND_POINTER) ty = TY_pointed(ty);

      /* TY should be the TY of a dope vector Dope */
      TY & tt = Ty_Table[ty];
      is_f90_pointer = TY_is_f90_pointer(tt);

      /* Compute the rank from the size of the dope vector */
      rank = cwh_types_dope_rank(ty);
      
      fl = TY_fld(tt);
      addr_ty = FLD_type(fl);
      ty = TY_pointed(addr_ty);  /* this is the type of the object */
      /* Create a temp symbol to hold the address */
      addr_preg = Create_Preg(Pointer_Mtype,Index_To_Str(Save_Str("alloc_addr")));

      if (rank > 0) {
	 el_ty = TY_AR_etype(ty);
      } else {
	 el_ty = ty;
      }

      esize = TY_size(el_ty);
      if (esize != 0) {
	 size = WN_Intconst(cwh_bound_int_typeid,esize);
      } else {
	 /* This must be an assumed-length character dummy */
	 /* Pick up the size from the element_size field   */
	 cwh_stk_push(WN_COPY_Tree(dope_addr),WN_item);
#ifdef KEY /* Bug6845 */
	 fei_get_dv_hdr_fld(DV_EL_LEN_IDX);
#else /* KEY Bug6845 */
	 fei_get_dv_hdr_fld(2);
#endif /* KEY Bug6845 */
	 size = cwh_expr_operand(NULL);
      }
      
	size2 = WN_Int_Type_Conversion(size,MTYPE_I8);
      /* Build up the size in bytes */
      for (i = 0; i < rank; i++) {
	 cwh_stk_push(WN_COPY_Tree(dope_addr),WN_item);
	 fei_get_dv_extent(i+1,0);
	 size2 = cwh_expr_bincalc(OPR_MPY,cwh_expr_operand(NULL),size2);
      }
      size2 = WN_StidPreg(cwh_bound_int_typeid,size_preg,size2);
      cwh_block_append(size2);
      
	 
      /* First step, set the flags bits if it's a pointer */
      flag_val = 0;
      if (DEBUG_Trap_Uv) {
	 flag_val |= 4;
      }
      if (is_f90_pointer) {
	 cwh_stk_push(WN_COPY_Tree(dope_addr),WN_item);
	 cwh_stk_push(WN_Intconst(MTYPE_I4,1),WN_item);
#ifdef KEY /* Bug6845 */
	 fei_set_dv_hdr_fld(DV_PTR_ALLOC_IDX);
#else /* KEY Bug6845 */
	 fei_set_dv_hdr_fld(4);
#endif /* KEY Bug6845 */
	 flag_val |= 1;
      } 
      flags = WN_Intconst(MTYPE_I4,flag_val);


      /* get the value of assoc from the dope vector */
      cwh_stk_push(WN_COPY_Tree(dope_addr),WN_item);
#ifdef KEY /* Bug6845 */
      fei_get_dv_hdr_fld(DV_ASSOC_IDX);
#else /* KEY Bug6845 */
      fei_get_dv_hdr_fld(3);
#endif /* KEY Bug6845 */
      assoc = cwh_intrin_wrap_value_parm(cwh_expr_operand(NULL));
      
      /* Build up the call to the _ALLOCATE_SGI intrinsic */
      args[0] = cwh_intrin_wrap_value_parm(WN_LdidPreg(cwh_bound_int_typeid,size_preg));
      args[1] = assoc;
      args[2] = cwh_intrin_wrap_value_parm(flags);
      
      if (WNOPR(stat) == OPR_INTCONST) {
	 args[3] = cwh_intrin_wrap_value_parm(WN_COPY_Tree(stat));
      } else {
	 args[3] = cwh_intrin_wrap_ref_parm(WN_COPY_Tree(stat),0);
      }
      
      /* fifth argument is the old value of the dope vector */
      cwh_stk_push(WN_COPY_Tree(dope_addr),WN_item);
#ifdef KEY /* Bug6845 */
      fei_get_dv_hdr_fld(DV_BASE_IDX);
#else /* KEY Bug6845 */
      fei_get_dv_hdr_fld(1);
#endif /* KEY Bug6845 */
      args[4] = cwh_intrin_wrap_value_parm(cwh_expr_operand(NULL));

      iop = WN_Create(opc_call,5);

      for (i=0; i < 5; i++) {
	 WN_kid(iop,i) = args[i];
      }

      /* Build the call to the allocate routine */
      WN_st_idx(iop) = ST_st_idx(allocate_routine_st);
      WN_Set_Call_Does_Mem_Alloc(iop);
      WN_Set_Call_Non_Data_Mod(iop);
      WN_Set_Call_Parm_Mod(iop);
      WN_Set_Call_Parm_Ref(iop);
      cwh_block_append(iop);
      iop = cwh_stmt_return_scalar(NULL, NULL, Be_Type_Tbl(Pointer_Mtype), FALSE);
      iop = WN_StidPreg(Pointer_Mtype,addr_preg,iop);
      cwh_block_append(iop);
      
      /* Add stores to base address, orig_base and orig_size */
      /* base_address */
      cwh_stk_push_typed(WN_COPY_Tree(dope_addr),WN_item, types[idope]);
      cwh_stk_push(WN_LdidPreg(Pointer_Mtype,addr_preg),WN_item);
#ifdef KEY /* Bug6845 */
      fei_set_dv_hdr_fld(DV_BASE_IDX);
#else /* KEY Bug6845 */
      fei_set_dv_hdr_fld(1);
#endif /* KEY Bug6845 */
      
      /* orig_base */
      cwh_stk_push_typed(WN_COPY_Tree(dope_addr),WN_item, types[idope]);
      cwh_stk_push(WN_LdidPreg(Pointer_Mtype,addr_preg),WN_item);
#ifdef KEY /* Bug6845 */
      fei_set_dv_hdr_fld(DV_ORIG_BASE_IDX);
#else /* KEY Bug6845 */
      fei_set_dv_hdr_fld(9);
#endif /* KEY Bug6845 */

      /* orig size */
      cwh_stk_push(WN_COPY_Tree(dope_addr),WN_item);
      size = cwh_expr_bincalc(OPR_SHL,WN_LdidPreg(cwh_bound_int_typeid,size_preg),
			      WN_Intconst(MTYPE_I4,3));
      cwh_stk_push(size,WN_item);
#ifdef KEY /* Bug6845 */
      fei_set_dv_hdr_fld(DV_ORIG_SIZE_IDX);
#else /* KEY Bug6845 */
      fei_set_dv_hdr_fld(10);
#endif /* KEY Bug6845 */
      
      /* Finally, set the assoc bit if allocation was successful */
      cwh_stk_push(WN_COPY_Tree(dope_addr),WN_item);
      assoc = WN_LdidPreg(Pointer_Mtype,addr_preg);
      assoc = WN_CreateExp2(OPCODE_make_op(OPR_GT,MTYPE_I4,Pointer_Mtype),
			    assoc,
			    WN_Zerocon(Pointer_Mtype));
      cwh_stk_push(assoc,WN_item);
#ifdef KEY /* Bug6845 */
      fei_set_dv_hdr_fld(DV_ASSOC_IDX);
#else /* KEY Bug6845 */
      fei_set_dv_hdr_fld(3);
#endif /* KEY Bug6845 */
   }
}


/*================================================================
 *
 * fei_allocate
 * 
 * This routine handles ALLOCATE and DEALLOCATE. Each statement
 * has a list of objects to be (de)allocated. Each object gets
 * its own dope vector and these are stuffed into an argument
 * list and handed to the (de)allocate function. False Parms 
 * are added to indicate that the DOPE vectors are modified.
 *
 * When called the stack has 
 *  1) Arguments to the (DE) ALLOCATE call (count-3 of them)
 *  2) The address of the STAT variable, or 0
 *  3) The version+count argument (I4)
 *  4) The symbol for the routine to call
 *
 *================================================================
*/
extern void
fei_allocate(INT32 count)
{

   INT num_dopes,i,num_args;
   BOOL use_stat;
   WN **dopes;
   TY_IDX *types;
   WN *dope;
   WN *wn;
   WN *stat,*ver;
   ST *routine;
   TY_IDX temp_ty;
   TY_IDX pty;
   ST *temp_st;
   INT64 vernum;
   WN *call;
   char	temp_str[40];
   static INT32 temp_name_idx = 0;

   num_dopes = count - 3;
   dopes = (WN **) malloc(num_dopes*sizeof(WN *));
   types = (TY_IDX *) malloc(num_dopes*sizeof(TY_IDX ));
   for (i=0; i < num_dopes; i++) {
      types[i] = cwh_stk_get_TY();
      dopes[i] = cwh_expr_operand(NULL);
      if (!types[i]) {
	 /* Try to get the type from the actual address node */
	 types[i] = cwh_types_WN_TY(dopes[i],TRUE);
      }
   }
   stat = cwh_expr_operand(NULL);
   ver  = cwh_expr_operand(NULL);
   routine = cwh_stk_pop_ST();

   if (!strcmp(ST_name(routine),"_DEALLOC")) {
      use_stat = FALSE;
      num_args = num_dopes+1;
   } else if (!strcmp(ST_name(routine),"_ALLOCATE")) {
      cwh_inline_allocate(dopes,types,num_dopes,stat);
      free(dopes);
      free(types);
      return;
   } else {
      use_stat = TRUE;
      num_args = num_dopes+2;
   }
   
   /* Create the call */
   call = WN_Create(OPC_VCALL,num_args);
   WN_st_idx(call) = ST_st_idx(routine);
   WN_Set_Call_Parm_Ref(call);
   WN_Set_Call_Parm_Mod(call);
   WN_Set_Call_Does_Mem_Free(call);
   
   /* Make the temp for the argument list to the routine */
   sprintf(temp_str, "%s%d", ".alloctemp.", temp_name_idx);
   temp_ty = cwh_types_array_util(1,Be_Type_Tbl(Pointer_Mtype),Pointer_Size,
				  Pointer_Size*num_dopes+8,temp_str,TRUE);

   ARB_HANDLE arb = TY_arb(temp_ty);
   Set_ARB_ubnd_val(arb, num_dopes + (8/Pointer_Size));
   Set_ARB_stride_val(arb, Pointer_Size);

   sprintf(temp_str, "%s%d", ".alloc", temp_name_idx++);
   temp_st = cwh_stab_address_temp_ST(temp_str,temp_ty,FALSE);   
   Set_ST_base(temp_st, temp_st);
   cwh_expr_set_flags(temp_st, f_T_PASSED);
   
   WN_kid0(call) = cwh_intrin_wrap_ref_parm(cwh_addr_address_ST(temp_st, 0),0);
   
   /* Add the stat argument */
   if (use_stat) {
      if (WNOPR(stat) == OPR_INTCONST) {
	 /* No status present, set to a null */
	 WN_set_opcode(stat,OPCODE_make_op(OPR_INTCONST,Pointer_Mtype,MTYPE_V));
	 stat = cwh_intrin_wrap_value_parm(stat);
      } else {
	 stat = cwh_intrin_wrap_ref_parm(stat,0);
      }
      WN_kid1(call) = stat;
   }
   
   pty = Be_Type_Tbl(Pointer_Mtype);
   /* Fill in the temp */
   DevAssert((WN_opcode(ver) == OPC_I8INTCONST),("Expected I8INTCONST for allocate version."));
   if (Pointer_Size == 4) {
# if defined(linux) || defined(BUILD_OS_DARWIN)
      vernum = WN_const_val(ver) & (0xffffffff);
      cwh_block_append(cwh_addr_stid(temp_st,0,pty,
                                      WN_Intconst(Pointer_Mtype,vernum)));
      vernum = WN_const_val(ver) >> 32;
      cwh_block_append(cwh_addr_stid(temp_st,4,pty,
                                      WN_Intconst(Pointer_Mtype,vernum)));
# else
      vernum = WN_const_val(ver) >> 32;
      cwh_block_append(cwh_addr_stid(temp_st,0,pty,
                                      WN_Intconst(Pointer_Mtype,vernum)));
      vernum = WN_const_val(ver) & (0xffffffff);
      cwh_block_append(cwh_addr_stid(temp_st,4,pty,
                                      WN_Intconst(Pointer_Mtype,vernum)));
# endif
      WN_DELETE_Tree(ver);
   } else {
      cwh_block_append(cwh_addr_stid(temp_st,0,pty, ver));
   }
   
   for (i=0; i < num_dopes; i++) {
      dope = dopes[i];
      wn = cwh_addr_stid(temp_st, 8 + Pointer_Size*i,pty,WN_COPY_Tree(dope));
      cwh_block_append(wn);
      dope = cwh_intrin_wrap_ref_parm(dope,0);
      WN_Set_Parm_Dummy(dope);
      if (use_stat) {
	 WN_kid(call,i+2) = dope;
      } else {
	 WN_kid(call,i+1) = dope;
      }
   }
   
   /* Insert the call */

   cwh_block_append(call);
   free (dopes);
   free (types);
}

/*===============================================
 *
 * cwh_stmt_init_file
 *
 * Initialize data structures for WHIRL conversion
 * at the start of each compilation. The flag says
 * -ump was seen on the command line (SGI mp 
 * directives) and is just convenient here.
 *
 *===============================================
 */ 

extern void
cwh_stmt_init_file(BOOL sgi_mp)
{
  cwh_stmt_sgi_mp_flag = sgi_mp ;
  cwh_addr_init_target() ;
}

/*===============================================
 *
 * cwh_stmt_add_parallel_pragmas
 *
 * Add the pragmas for CHUNK and MP_SCHEDTYPE 
 * as specified on the command line
 *
 *===============================================
 */ 
static void
cwh_stmt_add_parallel_pragmas(void)
{
   WN *prag;

   if (global_chunk_pragma_set) {
      prag = WN_CreateXpragma(WN_PRAGMA_CHUNKSIZE, (ST_IDX) 0, 1);
      WN_kid0(prag) = WN_Intconst(MTYPE_I4,global_chunk_pragma_value);
      cwh_stmt_add_to_preamble(prag,block_pu);
   }
   
   if (global_schedtype_pragma_set) {
      prag = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE, (ST_IDX) NULL, global_schedtype_pragma_val,4);
      cwh_stmt_add_to_preamble(prag,block_pu);
   }
}

#ifdef KEY /* Bug 4260 */
/*
 * symname	Name of global data symbol to export as I*4 variable
 * value	Initial value for symbol
 */
static void
export_i4_sym(const char *symname, int value) {
  TY_IDX int_ty_idx = MTYPE_To_TY(MTYPE_I4);
  ST *st = New_ST(GLOBAL_SYMTAB);
  cwh_auxst_clear(st);
  ST_Init(st, Save_Str(symname), CLASS_VAR, SCLASS_DGLOBAL, EXPORT_PREEMPTIBLE,
    int_ty_idx);
  Set_ST_is_initialized(st);
  INITO_IDX inito = New_INITO(st);
  INITV_IDX inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_I4, value, 1);
  Set_INITO_val(inito, inv);
}
#endif /* KEY Bug 4260 */

/*===============================================
 *
 * cwh_stmt_init_pu
 *
 * Initialize data structures for WHIRL conversion
 * at the start of each PU.
 *
 *===============================================
 */ 

extern void
cwh_stmt_init_pu(ST * st, INT32 lineno)
{
  INT16 nkids,i ;
  ST   **ap     ;

  cwh_stmt_init_srcpos(lineno);
  (void) cwh_block_toggle_debug(FALSE);

  nkids = cwh_auxst_num_dummies(st);
  ap    = cwh_auxst_arglist(st);

  (void) cwh_block_new_and_current() ;

  WN_tree  = WN_CreateEntry (nkids,st,cwh_block_current(), NULL,NULL );

  WN_pragma_pu = WN_kid(WN_tree,nkids);
  WN_pragma_ca = WN_kid(WN_tree,nkids+1);

  for (i = 0 ; i < nkids ; i ++) 
    WN_kid(WN_tree,i) = WN_CreateIdname ( 0, *ap++);

  WN_Set_Linenum (WN_tree, USRCPOS_srcpos(current_srcpos) );
  WN_Set_Linenum (cwh_block_current(), USRCPOS_srcpos(current_srcpos));

  cwh_stmt_add_parallel_pragmas();
#ifdef KEY
  char *compiler_bin = getenv("COMPILER_BIN");
  if (strcmp(ST_name(st), "MAIN__") == 0 &&
      compiler_bin != NULL) {
    size_t str_len = strlen(compiler_bin) + 1;
    // Bug: should not use alloca when passing psc_str out of this function
    // char *psc_str = (char *) alloca(str_len);
    char *psc_str = (char *) malloc(str_len*sizeof(char));
    strcpy(psc_str, compiler_bin);
    // create an array of chars initialized to psc_str
    TY_IDX str_ty_idx;
    TY &str_ty = New_TY(str_ty_idx);
    TY_Init(str_ty, str_len, KIND_ARRAY, MTYPE_M, 0);
    Set_TY_etype(str_ty, MTYPE_To_TY(MTYPE_I1));
    Set_TY_align(str_ty_idx, TY_align(TY_etype(str_ty)));
    ARB_HANDLE arb = New_ARB ();
    ARB_Init (arb, 0, 0, 0);
    Set_TY_arb (str_ty, arb);
    Set_ARB_first_dimen (arb);
    Set_ARB_last_dimen (arb);
    Set_ARB_dimension (arb, 1);
    Set_ARB_const_stride(arb);
    Set_ARB_stride_val(arb, 1);
    Set_ARB_const_lbnd (arb);
    Set_ARB_lbnd_val (arb, 0);
    Set_ARB_const_ubnd (arb);
    Set_ARB_ubnd_val (arb, str_len);
    ST *str_st = New_ST(GLOBAL_SYMTAB);
    // Bug 3713 - (typo) should be clearing auxst for str_st and not st here
    cwh_auxst_clear(str_st);
    ST_Init(str_st, Save_Str("__pathscale_compiler"), CLASS_VAR, SCLASS_DGLOBAL,
    	    EXPORT_PREEMPTIBLE, str_ty_idx);
    Set_ST_is_initialized(str_st);
    INITO_IDX inito = New_INITO(str_st);
    INITV_IDX inv = New_INITV();
    INITV_Init_String(inv, psc_str, str_len);
    Set_INITO_val(inito, inv);
#ifdef KEY /* Bug 4260 */
    /* If command line switch "-byteswap" or "-convert" appeared, export a
     * global data variable of type I*4 to the runtime system to reflect the
     * switch.
     */
    if (IO_DEFAULT != io_byteswap) {
      export_i4_sym("__io_byteswap_value", io_byteswap);
    }
#endif /* KEY Bug 4260 */
#ifdef KEY /* Bug 5089 */
# ifdef TARG_X8664
  /* If -TARG:sse2=off, export a global data variable of type I*4
   * to the runtime system telling it not to attempt to use the MXCSR register
   * in ieee_module_support.c. There exist processors with SSE but not SSE2
   * which do have the MXCSR register, but we're interested in only two
   * cases: SSE2 support or not. */
  if (!(Target_SSE2 || Target_SSE3)) {
    export_i4_sym("__SSE2_off", 1);
    }
# endif /* TARG_X8664 */
#endif /* KEY Bug 5089 */
  }
#endif
}

/*===============================================
 *
 * cwh_stmt_end_pu
 *
 * Return the top of the WN tree and clean up.
 * Setting the pragma blocks to NULL, means
 * additions (from declarations) will be ignored
 * until the next PU is set up.
 *
 *===============================================
 */ 
extern WN *
cwh_stmt_end_pu(void)
{

  WN_pragma_pu = NULL;
  WN_pragma_ca = NULL;

  return(WN_tree) ;
}


/*===============================================
 *
 * cwh_stmt_postprocess_pu
 *
 *===============================================
 */ 
extern void
cwh_stmt_postprocess_pu(void)
{

  if (DEBUG_Conform_Check) {
    cwh_stmt_conformance_checks(WN_tree);
  }

  // if (mp) {
  // cwh_stmt_add_local_pragmas(WN_tree);
  //}
  return;
}


/*===============================================
 *
 * cwh_stmt_init_srcpos
 *
 * Initialize the current line SRCPOS.
 * 
 * The line numbers from the FE occasionally
 * refer to an earlier line (eg: a two part
 * operation like ALLOC/DEALLOC) so ignore
 * the line if < current srcpos. Note that
 * nested procedures are processed first.
 *
 * global_to_local_file returns a pointer into
 * the FE's file table, so can avoid cwh_dst_enter_path
 * if the pointer was the same as last time.
 *
 *===============================================
 */ 
static void
cwh_stmt_init_srcpos(INT32 lineno)
{
  char    *file_name;
  INT32    local_line_num;
  mUINT16  local_file_num;

  static char *last_file_name = NULL;
  static PU *last_pu = NULL;

  if (lineno != 0) {

    file_name = global_to_local_file(lineno);
    local_line_num = global_to_local_line_number(lineno);

    if ((last_file_name != file_name) || 
	(local_line_num > USRCPOS_linenum(current_srcpos)) ||
	(last_pu != &(Get_Current_PU()))) {

      local_file_num = USRCPOS_filenum(current_srcpos) ;

      USRCPOS_clear(current_srcpos);

      if (last_file_name != file_name) 
	USRCPOS_filenum(current_srcpos) = cwh_dst_enter_path(file_name);
      else 
	USRCPOS_filenum(current_srcpos) = local_file_num ;

      USRCPOS_linenum(current_srcpos) = local_line_num;
      Set_Error_Source (file_name );
      Set_Error_Line(local_line_num);
    } 
    last_file_name = file_name ;
    last_pu = &(Get_Current_PU());
  } 
}

//================================================================
//================================================================
//================================================================

/*================================================================
 *  cwh_stmt_insert_conformance_check(WN **s1, WN **s2, INT ndims1, INT ndims2, INT first_axis, 
 *        WN *stmt, WN *block);
 *
 * Do the actual work of inserting the conformance check calls. 
 * 
 * s1, s2 - arrays of size nodes
 * ndims1, ndims2 - number of dimensions to check
 * first_axis - the first axis number to report in the event of a failure. If this is 1, for example
 * the axes will be numbered 1,2,3.... If it's 0, don't report the axis number. 
 * stmt, block - where to put the check. Line number comes from stmt. 
 *
 *================================================================*/

static void 
cwh_stmt_insert_conformance_check(WN **s1, WN **s2, INT ndims1, INT ndims2, INT first_axis, 
				  WN *stmt, WN *block)
{
  INT i;
  WN *eq, *t1,*t2, *gt0, *temp;
  BOOL not_all_const = FALSE;
  BOOL need_gt0_check;
  WN *args[5];
  WN *call;
  WN *if_stmt,*ifthenblock;
  char * proc_name;
#ifdef KEY /* Bug 10177 */
  PREG_NUM r1,r2;
  PREG_NUM rgt0 = 0;
#else /* KEY Bug 10177 */
  PREG_NUM r1,r2,rgt0;
#endif /* KEY Bug 10177 */
  INT64 lineno;

  // quick exit if one or the other ndims is scalar
  if (ndims1 == 0 || ndims2 == 0) return;
  Is_True(ndims1==ndims2,("conformance check rank mismatch."));

  /* Check for all axes non-zero */
  gt0 = WN_Intconst(MTYPE_I4,1);
  for (i=0; i < ndims1; i++) {
    t1 = cwh_convert_to_ty(WN_COPY_Tree(s1[i]),MTYPE_I8);
    t2 = cwh_convert_to_ty(WN_COPY_Tree(s2[i]),MTYPE_I8);
    gt0 = WN_LAND(gt0,WN_LIOR(WN_GT(MTYPE_I8,t1,WN_Zerocon(MTYPE_I8)),
			      WN_GT(MTYPE_I8,t2,WN_Zerocon(MTYPE_I8))));
  }
  
  need_gt0_check = TRUE;
  if (WN_operator(gt0) == OPR_INTCONST) {
     if (WN_const_val(gt0) == 0) {
	/* Zero sized-array, no check needed */
	WN_DELETE_Tree(gt0);
	return;
     } else {
	WN_DELETE_Tree(gt0);
	need_gt0_check = FALSE;
     }
  }

  if (need_gt0_check) {
     rgt0 = Create_Preg(MTYPE_I4,Index_To_Str(Save_Str("ccgt0")));
     WN_INSERT_BlockBefore(block,stmt,WN_StidPreg(MTYPE_I4,rgt0,gt0));
  }
  
  for (i=0; i < ndims1; i++) {
    t1 = cwh_convert_to_ty(WN_COPY_Tree(s1[i]),MTYPE_I8);
    t2 = cwh_convert_to_ty(WN_COPY_Tree(s2[i]),MTYPE_I8);
    eq = WN_EQ(MTYPE_I8,WN_COPY_Tree(t1),WN_COPY_Tree(t2));

    if (WN_operator(eq) != OPR_INTCONST || 
	WN_const_val(eq) == 0) {
      // insert the check
      
      lineno = WN_Get_Linenum(stmt);
      proc_name = cwh_dst_filename_from_filenum(SRCPOS_filenum(lineno));
      //      proc_name = ST_name(Procedure_ST);
      args[0] = cwh_intrin_wrap_value_parm(WN_LdaString(proc_name, 0, strlen(proc_name)));
      args[1] = cwh_intrin_wrap_value_parm(WN_Intconst(MTYPE_I4, SRCPOS_linenum(lineno)));
      if (first_axis != 0) {
	args[2] = cwh_intrin_wrap_value_parm(WN_Intconst(MTYPE_I4,ndims1-1-i+first_axis));
      } else {
	args[2] = cwh_intrin_wrap_value_parm(WN_Intconst(MTYPE_I4,0));
      }

      // Need to stick these in PREGS tp make sure that no array nodes are under the call
      r1 = Create_Preg(MTYPE_I8,Index_To_Str(Save_Str("cc1")));
      r2 = Create_Preg(MTYPE_I8,Index_To_Str(Save_Str("cc2")));
      WN_INSERT_BlockBefore(block,stmt,WN_StidPreg(MTYPE_I8,r1,t1));
      WN_INSERT_BlockBefore(block,stmt,WN_StidPreg(MTYPE_I8,r2,t2));
      args[3] = cwh_intrin_wrap_value_parm(WN_LdidPreg(MTYPE_I8,r1));
      args[4] = cwh_intrin_wrap_value_parm(WN_LdidPreg(MTYPE_I8,r2));
      call = WN_Create_Intrinsic(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V,
			INTRN_F90CONFORM_CHECK, 5, args);
      ifthenblock = WN_CreateBlock();
      WN_INSERT_BlockFirst(ifthenblock,call);
      if_stmt = WN_NE(MTYPE_I8,WN_LdidPreg(MTYPE_I8,r1),WN_LdidPreg(MTYPE_I8,r2));
      if (need_gt0_check) {
	 if_stmt = WN_LAND(WN_LdidPreg(MTYPE_I4,rgt0),if_stmt);
      }
      if_stmt = WN_CreateIf(if_stmt,ifthenblock,WN_CreateBlock());
      WN_INSERT_BlockBefore(block,stmt,if_stmt);
    } else {
      WN_DELETE_Tree(t1);
      WN_DELETE_Tree(t2);
    }
    WN_DELETE_Tree(eq);
  }
}
  


/*===============================================
 *
 * cwh_stmt_conformance_checks_walk (WN *tree, WN *stmt, WN *block, WN ** sizes, INT * ndim)
 * 
 * tree - Tree to check
 * stmt, block - The current statement and block before which to put the checks
 * sizes - array of sizes (output) of the current tree. The nodes need not be copied before use.
 * ndim - dimnesionality of tree (output)
 *
 * This walks the tree and adds the conformance check information. 
 *
 *================================================================*/
#define MAX_KIDS 6

static void 
cwh_stmt_conformance_checks_walk (WN *tree, WN *stmt, WN *block, WN ** sizes, INT * ndim)
{
  OPERATOR op;
  WN *node, *nextnode;
  
  WN *ksizes[MAX_KIDS][MAX_ARY_DIMS];
  INT kndims[MAX_KIDS];
  INT i,j,numkids,i_save,numargs;
  INT dim;
  
  op = WN_operator(tree);
  numkids = WN_kid_count(tree);
  if (ndim) *ndim = 0;
  
  if (op == OPR_BLOCK) {
    node = WN_first(tree);
    while (node) {
      nextnode = WN_next(node); /* Because the walk may insert statements */
      cwh_stmt_conformance_checks_walk (node, NULL, tree, NULL, NULL);
      node = nextnode;
    }

  } else if (op == OPR_WHERE) {
    /* should be three kids */
    DevAssert((numkids == 3),("Expected WHERE to have three kids."));

    /* first the mask */
    cwh_stmt_conformance_checks_walk (WN_kid(tree,0), tree, block, NULL, NULL);

    /* second, the assignment block */
    DevAssert((WN_operator(WN_kid(tree,1)) == OPR_BLOCK),("Expected WHERE to have BLOCK kid 1"));

    node = WN_first(WN_kid(tree,1));
    while (node) {
      nextnode = WN_next(node); /* Because the walk may insert statements */
      /* send tree and block as insert points */
      cwh_stmt_conformance_checks_walk (node, tree, block, NULL, NULL);
      node = nextnode;
    }

    /* third, is empty block, right now. Send it anyway */

    DevAssert((WN_operator(WN_kid(tree,2)) == OPR_BLOCK),("Expected WHERE to have BLOCK kid 2"));

    node = WN_first(WN_kid(tree,2));
    while (node) {
      nextnode = WN_next(node); /* Because the walk may insert statements */
      /* send tree and block as insert points */
      cwh_stmt_conformance_checks_walk (node, tree, block, NULL, NULL);
      node = nextnode;
    }

  } else if (op == OPR_ISTORE || op == OPR_MSTORE) {
    cwh_stmt_conformance_checks_walk (WN_kid(tree,0), (stmt?stmt:tree), block, ksizes[0], &kndims[0]);
    cwh_stmt_conformance_checks_walk (WN_kid(tree,1), (stmt?stmt:tree), block, ksizes[1], &kndims[1]);
    cwh_stmt_insert_conformance_check(ksizes[0],ksizes[1],kndims[0],kndims[1],1,(stmt?stmt:tree),block);
    if (sizes) {
      *ndim = kndims[0];
      for (i=0; i < kndims[0]; i++) {
	sizes[i] = ksizes[0][i];
      }
    } else {
      for (i=0; i < kndims[0]; i++) {
	WN_DELETE_Tree(ksizes[0][i]);
      }
    }
    for (i=0; i < kndims[1]; i++) {
      WN_DELETE_Tree(ksizes[1][i]);
    }

  } else if (op == OPR_INTRINSIC_CALL && WN_intrinsic(tree) == INTRN_CASSIGNSTMT) {
    // Character assignment
    cwh_stmt_conformance_checks_walk (WN_kid(tree,0), (stmt?stmt:tree), block, ksizes[0], &kndims[0]);
    cwh_stmt_conformance_checks_walk (WN_kid(tree,1), (stmt?stmt:tree), block, ksizes[1], &kndims[1]);
    cwh_stmt_conformance_checks_walk (WN_kid(tree,2), (stmt?stmt:tree), block, NULL, NULL);
    cwh_stmt_conformance_checks_walk (WN_kid(tree,3), (stmt?stmt:tree), block, NULL, NULL);
    cwh_stmt_insert_conformance_check(ksizes[0],ksizes[1],kndims[0],kndims[1],1,(stmt?stmt:tree),block);
    if (sizes) {
      *ndim = kndims[0];
      for (i=0; i < kndims[0]; i++) {
	sizes[i] = ksizes[0][i];
      }
    } else {
      for (i=0; i < kndims[0]; i++) {
	WN_DELETE_Tree(ksizes[0][i]);
      }
    }
    for (i=0; i < kndims[1]; i++) {
      WN_DELETE_Tree(ksizes[1][i]);
    }

  } else if (op == OPR_INTRINSIC_CALL && WN_intrinsic(tree) == INTRN_CONCATEXPR) {
    // CONCAT
    cwh_stmt_conformance_checks_walk (WN_kid(tree,0), (stmt?stmt:tree), block, ksizes[0], &kndims[0]);
    cwh_stmt_conformance_checks_walk (WN_kid(tree,1), (stmt?stmt:tree), block, NULL, NULL);
    
    numargs = (numkids - 2)/2;
    for (i=0; i < numargs; i++) {
      cwh_stmt_conformance_checks_walk (WN_kid(tree,i+2), (stmt?stmt:tree), block, ksizes[1], &kndims[1]);
      cwh_stmt_conformance_checks_walk (WN_kid(tree,i+2+numargs), (stmt?stmt:tree), block, NULL, NULL);
      cwh_stmt_insert_conformance_check(ksizes[0],ksizes[1],kndims[0],kndims[1],1,(stmt?stmt:tree),block);
      for (j=0; j < kndims[1]; j++) {
	WN_DELETE_Tree(ksizes[1][j]);
      }
    }

    if (sizes) {
      *ndim = kndims[0];
      for (i=0; i < kndims[0]; i++) {
	sizes[i] = ksizes[0][i];
      }
    } else {
      for (i=0; i < kndims[0]; i++) {
	WN_DELETE_Tree(ksizes[0][i]);
      }
    }

  } else if (OPERATOR_is_stmt(op) || OPERATOR_is_scf(op)) {
    for (i=0; i < numkids; i++) {
      cwh_stmt_conformance_checks_walk (WN_kid(tree,i), (stmt?stmt:tree), block, NULL, NULL);
    }

  } else {
    // Expression nodes
    switch (op) {
     case OPR_ARRAYEXP:
     case OPR_ARRSECTION:
     case OPR_ARRAY:
     case OPR_TRIPLET:
       for (i=0; i < numkids; i++) {
	 cwh_stmt_conformance_checks_walk (WN_kid(tree,i), stmt, block, NULL, NULL);
       }
       if (sizes) {
	 F90_Size_Walk(tree,ndim,sizes);
       }
       break;

     default:
       // Make sure all arguments are the same shape
       if (op == OPR_INTRINSIC_OP && F90_Is_Transformational(WN_intrinsic(tree))) {
	 // Special for transformationals
	 switch (WN_intrinsic(tree)) {
	   // No specific checking needed 
	  default:
	  case INTRN_SPREAD:
	  case INTRN_TRANSPOSE:
	  case INTRN_ALL:
	  case INTRN_ANY:
	  case INTRN_COUNT:
	  case INTRN_RESHAPE:  // we don't generate this yet, so we don't need to check it
	    for (i=0; i < numkids; i++) {
	      cwh_stmt_conformance_checks_walk (WN_kid(tree,0), stmt, block, NULL, NULL);
	    }
	    if (sizes) {
	      F90_Size_Walk(tree,ndim,sizes);
	    }
	    break;

	  case INTRN_MATMUL:
	  case INTRN_DOT_PRODUCT:
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,0), stmt, block, ksizes[0], &kndims[0]);
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,1), stmt, block, ksizes[1], &kndims[1]);
	    if (kndims[0] == 2 && kndims[1] == 2) {
	      cwh_stmt_insert_conformance_check(&ksizes[0][0],&ksizes[1][1],1,1,0,stmt,block);
	      WN_DELETE_Tree(ksizes[0][0]);
	      WN_DELETE_Tree(ksizes[1][1]);
	      if (sizes) {
		sizes[1] = ksizes[0][1];
		sizes[0] = ksizes[1][0];
		*ndim = 2;
	      }
	    } else if (kndims[0] == 2 && kndims[1] == 1) {
	      cwh_stmt_insert_conformance_check(&ksizes[0][0],&ksizes[1][0],1,1,0,stmt,block);
	      WN_DELETE_Tree(ksizes[0][0]);
	      WN_DELETE_Tree(ksizes[1][0]);
	      if (sizes) {
		sizes[0] = ksizes[0][1];
		*ndim = 1;
	      }
	    } else if (kndims[0] == 1 && kndims[1] == 2) {
	      cwh_stmt_insert_conformance_check(&ksizes[0][0],&ksizes[1][1],1,1,0,stmt,block);
	      WN_DELETE_Tree(ksizes[0][0]);
	      WN_DELETE_Tree(ksizes[1][1]);
	      if (sizes) {
		sizes[0] = ksizes[1][0];
		*ndim = 1;
	      }
	    } else {
	      // 1,1 means dot_product
	      cwh_stmt_insert_conformance_check(&ksizes[0][0],&ksizes[1][0],1,1,1,stmt,block);
	      WN_DELETE_Tree(ksizes[0][0]);
	      WN_DELETE_Tree(ksizes[1][0]);
	    }
	    break;

	  case INTRN_PRODUCT:
	  case INTRN_SUM:
	  case INTRN_MAXVAL:
	  case INTRN_MINVAL:
	  case INTRN_MAXLOC:
	  case INTRN_MINLOC:
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,0), stmt, block, ksizes[0], &kndims[0]);
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,2), stmt, block, ksizes[1], &kndims[1]);
	    cwh_stmt_insert_conformance_check(ksizes[0],ksizes[1],kndims[0],kndims[1],1,stmt,block);
	    for (i=0; i < kndims[0]; i++) {
	      WN_DELETE_Tree(ksizes[0][i]);
	    }
	    for (i=0; i < kndims[1]; i++) {
	      WN_DELETE_Tree(ksizes[1][i]);
	    }
	    if (sizes) {
	      F90_Size_Walk(tree,ndim,sizes);
	    }
	    break;

	  case INTRN_CSHIFT:
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,0), stmt, block, ksizes[0], &kndims[0]);
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,1), stmt, block, ksizes[1], &kndims[1]);
	    dim = F90_Get_Dim(WN_kid(tree,2))-1;
	    // check for conformance between the shift argument and the array argument
	    // less the dim dimension
	    if (dim >= 0) {
	      for (i=0,j=0; i < kndims[0]; i++) {
		if (i != kndims[0]-1-dim) {
		  ksizes[2][j] = ksizes[0][i];
		  ++j;
		}
	      }
	      kndims[2] = kndims[0] - 1;
	      cwh_stmt_insert_conformance_check(ksizes[2],ksizes[1],kndims[2],kndims[1],0,stmt,block);
	    }
	    if (sizes) {
	      *ndim = kndims[0];
	      for (i=0; i < kndims[0]; i++) {
		sizes[i] = ksizes[0][i];
	      }
	    } else {
	      for (i=0; i < kndims[0]; i++) {
		WN_DELETE_Tree(ksizes[0][i]);
	      }
	    }
	    for (i=0; i < kndims[1]; i++) {
	      WN_DELETE_Tree(ksizes[1][i]);
	    }
	    break;

	  case INTRN_EOSHIFT:
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,0), stmt, block, ksizes[0], &kndims[0]);
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,1), stmt, block, ksizes[1], &kndims[1]);
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,2), stmt, block, ksizes[2], &kndims[2]);
	    dim = F90_Get_Dim(WN_kid(tree,3))-1;
	    // check for conformance between the shift and boundary arguments and the array argument
	    // less the dim dimension
	    if (dim >= 0) {
	      for (i=0,j=0; i < kndims[0]; i++) {
		if (i != kndims[0]-1-dim) {
		  ksizes[3][j] = ksizes[0][i];
		  ++j;
		}
	      }
	      kndims[3] = kndims[0] - 1;
	      cwh_stmt_insert_conformance_check(ksizes[3],ksizes[1],kndims[3],kndims[1],0,stmt,block);
	      cwh_stmt_insert_conformance_check(ksizes[3],ksizes[2],kndims[3],kndims[2],0,stmt,block);
	    }
	    if (sizes) {
	      *ndim = kndims[0];
	      for (i=0; i < kndims[0]; i++) {
		sizes[i] = ksizes[0][i];
	      }
	    } else {
	      for (i=0; i < kndims[0]; i++) {
		WN_DELETE_Tree(ksizes[0][i]);
	      }
	    }
	    for (i=0; i < kndims[1]; i++) {
	      WN_DELETE_Tree(ksizes[1][i]);
	    }
	    for (i=0; i < kndims[2]; i++) {
	      WN_DELETE_Tree(ksizes[2][i]);
	    }
	    break;

	  case INTRN_PACK:
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,0), stmt, block, ksizes[0], &kndims[0]);
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,1), stmt, block, ksizes[1], &kndims[1]);
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,2), stmt, block, sizes, ndim);
	    cwh_stmt_insert_conformance_check(ksizes[0],ksizes[1],kndims[0],kndims[1],1,stmt,block);
	    for (i=0; i < kndims[0]; i++) {
	      WN_DELETE_Tree(ksizes[0][i]);
	    }
	    for (i=0; i < kndims[1]; i++) {
	      WN_DELETE_Tree(ksizes[1][i]);
	    }
	    break;

	  case INTRN_UNPACK:
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,0), stmt, block, NULL, NULL);
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,1), stmt, block, ksizes[0], &kndims[0]);
	    cwh_stmt_conformance_checks_walk (WN_kid(tree,2), stmt, block, ksizes[1], &kndims[1]);
	    cwh_stmt_insert_conformance_check(ksizes[0],ksizes[1],kndims[0],kndims[1],1,stmt,block);
	    // copy sizes and cleanup
	    if (sizes) {
	      *ndim = kndims[0];
	      for (i=0; i < kndims[0]; i++) {
		sizes[i] = ksizes[0][i];
	      }
	    } else {
	      for (i=0; i < kndims[0]; i++) {
		WN_DELETE_Tree(ksizes[0][i]);
	      }
	    }
	    for (i=0; i < kndims[1]; i++) {
	      WN_DELETE_Tree(ksizes[1][i]);
	    }
	    break;
	    
	 } // intrinsics switch

	 break;
       } // Transformational intrinsics
       
       if (numkids == 0) {
	 break;
       } 
       if (numkids == 1) {
	 cwh_stmt_conformance_checks_walk (WN_kid(tree,0), stmt, block, sizes, ndim);
	 break;
       }
       
       // More than one kid
       if (numkids > MAX_KIDS) break;
       for (i=0; i < numkids; i++) {
	 cwh_stmt_conformance_checks_walk (WN_kid(tree,i), stmt, block,
					   ksizes[i], &kndims[i]);
       }
       for (i=0; i < numkids; i++) {
	 for (j = i+1; j < numkids; j++) {
	   cwh_stmt_insert_conformance_check(ksizes[i],ksizes[j],kndims[i],kndims[j],1,stmt,block);
	 }
       }
       
       /* Figure out which ones to save */
       i_save = -1;
       if (sizes) {
	 for (i=0; i < numkids; i++) {
	   if (kndims[i] > *ndim) {
	     i_save = i;
	     *ndim = kndims[i]; 
	     for (j = 0; j < kndims[i]; j++) {
	       sizes[j] = ksizes[i][j];
	     }
	   }
	 }
       }
       
       /* Clean up the rest */
       for (i=0; i < numkids; i++) {
	 if (i_save != i) {
	   for (j = 0; j < kndims[i]; j++) {
	     WN_DELETE_Tree(ksizes[i][j]);
	   }
	 }
       }
       break;
    } // expressions switch
  } // expressions
  return;
}


/*===============================================
 *
 * cwh_stmt_conformance_checks (WN *tree)
 *
 * Adds the conformance checks for array operations to the tree. This 
 * is normally added only with bounds checking
 *
 *================================================================*/

static void 
cwh_stmt_conformance_checks(WN *tree)
{
  cwh_stmt_conformance_checks_walk(tree,NULL,NULL,NULL,NULL);
}
