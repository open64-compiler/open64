/*
 * Copyright (C) 2009-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007, 2008 PathScale, LLC. All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: expand.c
 * $Revision: 1.363 $
 * $Date: 05/12/01 12:16:40-08:00 $
 * $Author: cfang@dunite.internal.keyresearch.com $
 * $Source: be/cg/x8664/SCCS/s.expand.cxx $
 *
 * Description:
 *
 * This file contains the internals of code expansion. Its interface
 * is 'Exp_OP', which takes an OP, expands it into a list of OPs which
 * are appended to the oplist passed in.
 *
 * It handles all the macro expansions, special handling during 
 * expansion and all the nitty gritty stuff that goes along with it.
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#include "defs.h"
#include "config.h"
#include "erglob.h"
#include "ercg.h"
#include "glob.h"
#include "tracing.h"
#include "util.h"

#include "tn.h"
#include "cg_flags.h"
#include "bb.h"
#include "symtab.h"
#include "opcode.h"
#include "const.h"	/* needed to manipulate target/host consts */
#include "targ_const.h"	/* needed to manipulate target/host consts */
#include "op.h"
#include "data_layout.h"
#include "stblock.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "w2op.h"
#include "label_util.h"
#include "cgtarget.h"
#include "whirl2ops.h"
#include "targ_sim.h"   /* To generate stores of param registers in builtin_apply_args */
#include "targ_const_private.h"
#include "config_opt.h" /* For Force_IEEE_Comparisons */
#include "intrn_info.h" // for INTRN_rt_name
#include "cg.h"
#ifdef KEY
#include "ebo.h"
#endif

BOOL Reuse_Temp_TNs = FALSE;

BOOL Trace_Exp2 = FALSE;      /* extra cgexp trace*/

/* Dup_TN won't dup a dedicated tn, but for our purposes we
 * can just re-use the dedicated tn.  Don't want to re-use a
 * symbolic tn or it will mess up live ranges. */
/* DOESN'T WORK:  causes problems in Create_lvs because it causes
 * a use of a parm reg at the call-site, so it looks like the
 * parm-reg is incoming at the call?  This probably should work,
 * but for now we can use other routine that create a real dup tn. */
#define DUP_TN(tn)	Dup_TN_Even_If_Dedicated(tn)

static BOOL Target_Support_Cmov()
{
  if (Is_Target_32bit() &&
      (Target == TARGET_anyx86 ||
       Target == TARGET_pentium4 ||
       Target == TARGET_xeon ||
       Target == TARGET_athlon) )
    return FALSE;
  else
    return TRUE;
}

static TN_MAP _TN_Pair_table = NULL;

static TN *Exp_Fetch_and_Add    (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops);
static TN *Exp_Fetch_and_Or     (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops);
static TN *Exp_Fetch_and_Xor    (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops);
static TN *Exp_Fetch_and_And    (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops);
static TN *Exp_Fetch_and_Nand   (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops);
static TN *Exp_Add_and_Fetch    (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops);
static TN *Exp_Or_and_Fetch     (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops);
static TN *Exp_Xor_and_Fetch    (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops);
static TN *Exp_And_and_Fetch    (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops);
static TN *Exp_Nand_and_Fetch   (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops);
static TN *Exp_Test_and_Set     (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops);
static TN *Exp_Lock_Release     (TN *addr, TYPE_ID mtype, OPS *ops);
static TN *Exp_Compare_and_Swap (TN *addr, TN *opnd1, TN *opnd2, TYPE_ID mtype, OPS *ops); 
static TN *Exp_Bool_Compare_and_Swap (TN *addr, TN *opnd1, TN *opnd2, TYPE_ID mtype, OPS *ops); 
static TN *Exp_Builtin_Apply_Args (OPS *ops);
static TN *Exp_Builtin_Apply    (TN *addr, TN *args, TN *argsize, OPS *ops);
static TN *Exp_Builtin_Return   (TN *result, OPS *ops);

static void Store_To_Temp_Stack(TYPE_ID desc, TN *src, const char *sym_name, TN **mem_base_tn,
		    TN **mem_ofst_tn, OPS *ops);

void
Expand_Cmov (TOP top, TN *result, TN *src, TN *rflags, OPS *ops, TN *result2,
	     TN *src2)
{
  // OSP, laijx
  if ( ! Target_Support_Cmov() ) {
    // Processor doesn't support cmov.  Emit conditional branch followed by
    // mov.

    TN *tmp_result = result;
    TN *tmp_result2 = result2;

    if (TN_is_dedicated(result)) {
      tmp_result = Build_TN_Like(result);
    }
    if (result2 != NULL &&
	TN_is_dedicated(result2)) {
      tmp_result2 = Build_TN_Like(result2);
    }

    // Determine the branch instruction from the cmov.
    TOP br_top;
    switch (top) {
      case TOP_cmovb:	br_top = TOP_jae;	break;
      case TOP_cmovae:	br_top = TOP_jb;	break;
      case TOP_cmovp:	br_top = TOP_jnp;	break;
      case TOP_cmovnp:	br_top = TOP_jp;	break;
      case TOP_cmove:	br_top = TOP_jne;	break;
      case TOP_cmovne:	br_top = TOP_je;	break;
      case TOP_cmovbe:	br_top = TOP_ja;	break;
      case TOP_cmova:	br_top = TOP_jbe;	break;
      case TOP_cmovl:	br_top = TOP_jge;	break;
      case TOP_cmovge:	br_top = TOP_jl;	break;
      case TOP_cmovle:	br_top = TOP_jg;	break;
      case TOP_cmovg:	br_top = TOP_jle;	break;
      case TOP_cmovs:	br_top = TOP_jns;	break;
      case TOP_cmovns:	br_top = TOP_js;	break;
      case TOP_fcmovb:	br_top = TOP_jae;	break;
      case TOP_fcmovbe:	br_top = TOP_ja;	break;
      case TOP_fcmovnb:	br_top = TOP_jb;	break;
      case TOP_fcmovnbe: br_top = TOP_jbe;	break;
      case TOP_fcmove:	br_top = TOP_jne;	break;
      case TOP_fcmovne:	br_top = TOP_je;	break;
      case TOP_fcmovu:	br_top = TOP_jnp;	break;	// br if PF=0
      case TOP_fcmovnu:	br_top = TOP_jp;	break;	// br if Pf=1
      default:		FmtAssert(FALSE, ("Expand_Cmov: unexpected OP code"));
    }
    BB *bb_entry = Cur_BB;
    BB *bb_then = Gen_And_Append_BB(bb_entry);
    BB *bb_exit = Gen_And_Append_BB(bb_then);

    const LABEL_IDX bb_exit_label = Gen_Label_For_BB(bb_exit);

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR, 1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = bb_exit_label;

    // Build bb_entry.
    {
      if (result != tmp_result) {
	Exp_COPY(tmp_result, result, ops);
      }
      if (result2 != NULL &&
	  result2 != tmp_result) {
	Exp_COPY(tmp_result2, result2, ops);
      }
      Build_OP(br_top, rflags, Gen_Label_TN(bb_exit_label, 0), ops);
      if (&New_OPs != ops)
        OPS_Append_Ops(&New_OPs, ops);
      Process_New_OPs();
      BB_Append_Ops(bb_entry, &New_OPs);
      OPS_Init(&New_OPs);
      OPS_Init(ops);
    }

    // Build bb_then.
    {
      OPS *bb_then_ops = &New_OPs;
      Exp_COPY(tmp_result, src, bb_then_ops);
      if (result2 != NULL)
        Exp_COPY(tmp_result2, src2, bb_then_ops);
      total_bb_insts = 0;
      Last_Processed_OP = NULL;
      Process_New_OPs();
      BB_Append_Ops(bb_then, bb_then_ops);
      OPS_Init(bb_then_ops);
    }

    Cur_BB = bb_exit;

    if (result != tmp_result)
      Exp_COPY(result, tmp_result, ops);
    if (result2 != tmp_result2)
      Exp_COPY(result2, tmp_result2, ops);
  } else {
    // Processor supports cmov.
    Build_OP(top, result, src, rflags, ops);
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
    if (result2 != NULL) {
      Is_True(src2 != NULL, ("Expand_Cmov: invalid src2"));
      Build_OP(top, result2, src2, rflags, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
    }
  }
}

void Expand_Start()
{
  if( !Is_Target_32bit() )
    return;

  FmtAssert( _TN_Pair_table == NULL, ("TN_Pair_table is not NULL") );
  _TN_Pair_table = TN_MAP_Create();
}


// Always use the lower part as key.
TN* Get_TN_Pair( TN* key )
{
  TN* pair = NULL;

  if( Is_Target_32bit() &&
      TN_is_register( key ) ){
    pair = (TN*)TN_MAP_Get( _TN_Pair_table, key );
  }

  return pair;
}

void Delete_TN_Pair( TN *key) 
{
  Is_True( Get_TN_Pair( key ) != NULL, ("Delete_TN_Pair: higher 32-bit is NULL") );
  TN_MAP_Set( _TN_Pair_table, key, NULL );
}

void Create_TN_Pair( TN* key, TN* pair )
{
  Is_True( Get_TN_Pair( key ) == NULL, ("Add_TN_Pair: higher 32-bit is missing") );
  TN_MAP_Set( _TN_Pair_table, key, pair );    
}


/* Always use the lower part as key.
   Notice that literal TNs will not have pairs.
 */
TN* Create_TN_Pair( TN* key, TYPE_ID mtype )
{
  FmtAssert( TN_is_register(key), ("TN is not a register type") );

  if( mtype == MTYPE_I8 )
    mtype = MTYPE_I4;
  else if( mtype == MTYPE_U8 )
    mtype = MTYPE_U4;

  TN* pair = Get_TN_Pair( key );

  if( pair == NULL ){
    Set_TN_size( key, MTYPE_byte_size(mtype) );
    /* We don't know what <pair> will be later. So don't use
       Dup_TN that will carry the homing info of <key>.
     */
    pair = Build_TN_Like( key );
    TN_MAP_Set( _TN_Pair_table, key, pair );
  }

  if( TN_register(key) != REGISTER_UNDEFINED ){
    Is_True( TN_register(pair) != REGISTER_UNDEFINED, ("pair TN is async") );
  }

  return pair;
}


void Expand_Finish()
{
  if( !Is_Target_32bit() )
    return;

  FmtAssert( _TN_Pair_table != NULL, ("TN_Pair_table is NULL") );
  TN_MAP_Delete( _TN_Pair_table );
  _TN_Pair_table = NULL;
}

static TN* Gen_Const_Symbol_TN( INT64 int_val,
				double float_val,
				TYPE_ID mtype,
				TN_RELOCS relocs = TN_RELOC_NONE )
{
  FmtAssert( !MTYPE_is_quad(mtype), ("Quad const is not supported") );
  FmtAssert( !MTYPE_is_vector(mtype), ("Vector const is not supported") );

  const TCON tcon = MTYPE_is_integral(mtype)
    ? Host_To_Targ( mtype, int_val ) : Host_To_Targ_Float( mtype, float_val );

  ST* sym = New_Const_Sym( Enter_tcon(tcon),  Be_Type_Tbl( TCON_ty(tcon) ) );

  ST* base_sym = NULL;
  INT64 base_ofst = 0;

  Allocate_Object(sym);
  Base_Symbol_And_Offset_For_Addressing( sym, 0, &base_sym, &base_ofst );

  return Gen_Symbol_TN( base_sym, base_ofst, relocs );
}


/*  <result> = <cmp_kid1> <compare> <cmp_kid2> ? <true_tn> : <false_tn>
 */
static void Expand_Split_Select( TN* dest, OPERATOR compare, TOP cmp_opcode,
				 TN* cmp_kid1, TN* cmp_kid2, TYPE_ID cmp_type,
				 TN* true_tn, TN* false_tn, TYPE_ID select_type,
				 OPS* ops )
{
  TN* result = dest;

  if( TN_is_dedicated(dest) ){
    result = Build_TN_Like( dest );

    if( Get_TN_Pair(dest) != NULL ){
      TN* result_hi = Create_TN_Pair( result, MTYPE_I8 );
      Build_OP( TOP_ldc32, result_hi, Gen_Literal_TN(0,4), ops );
    }
  }

  FmtAssert( result != false_tn, ("result and false_tn are identical") );

  Expand_Copy( result, true_tn, select_type, ops );

  switch( cmp_opcode ){
  case TOP_cmp64:   cmp_opcode = TOP_cmp32;   break;
  case TOP_cmpi64:  cmp_opcode = TOP_cmpi32;  break;
  case TOP_test64:  cmp_opcode = TOP_test32;  break;
  case TOP_testi64: cmp_opcode = TOP_testi32; break;
  default:
    FmtAssert( false, ("Expand_Split_Select: Unknown compare opcode") );
  }

  TN* cmp_kid1_hi = Get_TN_Pair( cmp_kid1 );
  TN* cmp_kid2_hi = Get_TN_Pair( cmp_kid2 );
  TN* rflags = Rflags_TN();

  if( cmp_kid2_hi == NULL ){
    if( TN_has_value(cmp_kid2) ){
      const INT64 val = TN_value(cmp_kid2);
      cmp_kid2_hi = Gen_Literal_TN( val >> 32, 4 );

    } else {
      DevWarn( "The higher 32-bit of TN%d is treated as 0\n",
	       TN_number(cmp_kid2) );
      cmp_kid2_hi = Build_TN_Like( cmp_kid2 );
      Build_OP( TOP_ldc32, cmp_kid2_hi, Gen_Literal_TN(0,4), ops );    
    }
  }

  if( cmp_kid1_hi == NULL ){
    if( TN_has_value(cmp_kid1) ){
      const INT64 val = TN_value(cmp_kid1);
      cmp_kid1_hi = Gen_Literal_TN( val >> 32, 4 );

    } else {
      DevWarn( "The higher 32-bit of TN%d is treated as 0\n",
	       TN_number(cmp_kid1) );
      cmp_kid1_hi = Build_TN_Like( cmp_kid1 );
      Build_OP( TOP_ldc32, cmp_kid1_hi, Gen_Literal_TN(0,4), ops );    
    }
  }

  BB* bb_entry  = Cur_BB;
  BB* bb_cmp_hi = Gen_And_Append_BB( bb_entry );
  BB* bb_cmp_lo = Gen_And_Append_BB( bb_cmp_hi );
  BB* bb_non_set = Gen_And_Append_BB( bb_cmp_lo );
  const LABEL_IDX bb_non_set_label = Gen_Label_For_BB( bb_non_set );
  BB* bb_exit    = Gen_And_Append_BB( bb_non_set );
  const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

  BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_entry)) = NULL;
  WN_label_number(BB_branch_wn(bb_entry)) = bb_exit_label;

  BB_branch_wn(bb_cmp_hi) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_cmp_hi)) = NULL;
  WN_label_number(BB_branch_wn(bb_cmp_hi)) = bb_non_set_label;

  BB_branch_wn(bb_cmp_lo) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_cmp_lo)) = NULL;
  WN_label_number(BB_branch_wn(bb_cmp_lo)) = bb_exit_label;

  // Compare the higher 32-bit here.
  {
    if( compare != OPR_EQ ){
      TOP jmp = TOP_UNDEFINED;
      switch( compare ){
      case OPR_GT:
      case OPR_GE:  jmp = MTYPE_is_signed(cmp_type) ? TOP_jg : TOP_ja;   break;
      case OPR_LT:
      case OPR_LE:  jmp = MTYPE_is_signed(cmp_type) ? TOP_jl : TOP_jb;   break;
      case OPR_NE:  jmp = TOP_jne; break;
      }

      Build_OP( cmp_opcode, rflags, cmp_kid1_hi, cmp_kid2_hi, ops );
      Build_OP( jmp, rflags, Gen_Label_TN( bb_exit_label, 0 ), ops );
    }

    if( ops != &New_OPs )
      OPS_Append_Ops( &New_OPs, ops );

    Process_New_OPs();
    BB_Append_Ops( bb_entry, &New_OPs );
    OPS_Init( &New_OPs );
    OPS_Init( ops );
  }

  // Compare the higher 32-bit here.
  if( compare != OPR_NE ){
    OPS* bb_cmp_hi_ops = &New_OPs;
    TOP jmp = TOP_UNDEFINED;

    switch( compare ){
    case OPR_GT:
    case OPR_GE: jmp = MTYPE_is_signed(cmp_type) ? TOP_jl : TOP_jb;  break;
    case OPR_LE:
    case OPR_LT: jmp = MTYPE_is_signed(cmp_type) ? TOP_jg : TOP_ja;  break;
    case OPR_EQ: jmp = TOP_jne; break;
    }

    Build_OP( cmp_opcode, rflags, cmp_kid1_hi, cmp_kid2_hi, bb_cmp_hi_ops );
    Build_OP( jmp, rflags, Gen_Label_TN( bb_non_set_label, 0 ), bb_cmp_hi_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_cmp_hi, bb_cmp_hi_ops );
    OPS_Init( bb_cmp_hi_ops );
  }

  // Compare the lower 32-bit, given the same higher 32-bit.
  {
    OPS* bb_cmp_lo_ops = &New_OPs;
    TOP jmp = TOP_UNDEFINED;

    switch( compare ){
    case OPR_GT:  jmp = TOP_ja;  break;
    case OPR_GE:  jmp = TOP_jae; break;
    case OPR_LT:  jmp = TOP_jb;  break;
    case OPR_LE:  jmp = TOP_jbe; break;
    case OPR_NE:  jmp = TOP_jne; break;
    case OPR_EQ:  jmp = TOP_je;  break;
    }

    Build_OP( cmp_opcode, rflags, cmp_kid1, cmp_kid2, bb_cmp_lo_ops );
    Build_OP( jmp, rflags, Gen_Label_TN( bb_exit_label, 0 ), bb_cmp_lo_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_cmp_lo, bb_cmp_lo_ops );
    OPS_Init( bb_cmp_lo_ops );
  }

  // Now we reach a false condition
  {
    OPS* bb_non_set_ops = &New_OPs;

    Expand_Copy( result, false_tn, select_type, bb_non_set_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_non_set, bb_non_set_ops );
    OPS_Init( bb_non_set_ops );
  }

  Cur_BB = bb_exit;

  if( result != dest ){
    Expand_Copy( dest, result, select_type, ops );
  }
}


static void  Expand_Split_Int_Cmp( TOP cmp_opcode, TN* src1_lo, TN* src2_lo,
				   TOP set_opcode, TN* result,
				   TYPE_ID mtype,  OPS* ops )
{
  TN* tmp_result = result;

  if( TN_is_dedicated( result ) ){
    tmp_result = Build_TN_Like( result );
  }

  if( tmp_result == src1_lo ){
    TN* tmp = Build_TN_Like(src1_lo);
    Expand_Copy( tmp, src1_lo, mtype, ops );
    src1_lo = tmp;
  }

  if( tmp_result == src2_lo ){
    TN* tmp = Build_TN_Like(src2_lo);
    Expand_Copy( tmp, src2_lo, mtype, ops );
    src2_lo = tmp;
  }

  Exp_Immediate( tmp_result, Gen_Literal_TN(1,4), FALSE, ops );

  switch( cmp_opcode ){
  case TOP_cmp64:   cmp_opcode = TOP_cmp32;   break;
  case TOP_cmpi64:  cmp_opcode = TOP_cmpi32;  break;
  case TOP_test64:  cmp_opcode = TOP_test32;  break;
  case TOP_testi64: cmp_opcode = TOP_testi32; break;
  default:
    FmtAssert( false, ("Expand_Split_Int_Cmp: Unknown compare opcode") );
  }

  TN* src1_hi = Get_TN_Pair( src1_lo );
  TN* src2_hi = Get_TN_Pair( src2_lo );
  TN* rflags = Rflags_TN();

  if( src1_hi == NULL ){
    if( TN_has_value(src1_lo) ){
      const INT64 val = TN_value( src1_lo ) >> 32;
      src1_hi = Gen_Literal_TN( val, 4 );

    } else {
      DevWarn( "The higher 32-bit of TN%d is treated as 0\n",
	       TN_number(src1_lo) );
      src1_hi = Build_TN_Like( src1_lo );
      Build_OP( TOP_ldc32, src1_hi, Gen_Literal_TN(0,4), ops );    
    }
  }

  if( src2_hi == NULL ){
    if( TN_has_value(src2_lo) ){
      const INT64 val = TN_value( src2_lo ) >> 32;
      src2_hi = Gen_Literal_TN( val, 4 );

    } else {
      DevWarn( "The higher 32-bit of TN%d is treated as 0\n",
	       TN_number(src2_lo) );
      src2_hi = Build_TN_Like( src2_lo );
      Build_OP( TOP_ldc32, src2_hi, Gen_Literal_TN(0,4), ops );    
    }
  }

  BB* bb_entry  = Cur_BB;
  BB* bb_cmp_hi = Gen_And_Append_BB( bb_entry );
  BB* bb_cmp_lo = Gen_And_Append_BB( bb_cmp_hi );
  BB* bb_non_set = Gen_And_Append_BB( bb_cmp_lo );
  const LABEL_IDX bb_non_set_label = Gen_Label_For_BB( bb_non_set );
  BB* bb_exit    = Gen_And_Append_BB( bb_non_set );
  const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

  BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_entry)) = NULL;
  WN_label_number(BB_branch_wn(bb_entry)) = bb_exit_label;

  BB_branch_wn(bb_cmp_hi) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_cmp_hi)) = NULL;
  WN_label_number(BB_branch_wn(bb_cmp_hi)) = bb_non_set_label;

  BB_branch_wn(bb_cmp_lo) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_cmp_lo)) = NULL;
  WN_label_number(BB_branch_wn(bb_cmp_lo)) = bb_exit_label;

  // Compare the higher 32-bit here.
  {
    if( set_opcode != TOP_sete ){
      TOP jmp = TOP_UNDEFINED;
      switch( set_opcode ){
      case TOP_setg:
      case TOP_setge: jmp = TOP_jg;  break;
      case TOP_seta:
      case TOP_setae: jmp = TOP_ja;  break;
      case TOP_setl:
      case TOP_setle: jmp = TOP_jl;  break;
      case TOP_setb:
      case TOP_setbe: jmp = TOP_jb;  break;
      case TOP_setne: jmp = TOP_jne; break;
      }

      Build_OP( cmp_opcode, rflags, src1_hi, src2_hi, ops );
      Build_OP( jmp, rflags, Gen_Label_TN( bb_exit_label, 0 ), ops );
    }

    if( &New_OPs != ops )
      OPS_Append_Ops( &New_OPs, ops );

    Process_New_OPs();
    BB_Append_Ops( bb_entry, &New_OPs );
    OPS_Init( &New_OPs );
    OPS_Init( ops );
  }

  // Compare the higher 32-bit here.
  if( set_opcode != TOP_setne ){
    OPS* bb_cmp_hi_ops = &New_OPs;
    TOP jmp = TOP_UNDEFINED;

    switch( set_opcode ){
    case TOP_setg:
    case TOP_setge: jmp = TOP_jl;  break;
    case TOP_seta:
    case TOP_setae: jmp = TOP_jb;  break;
    case TOP_setl:
    case TOP_setle: jmp = TOP_jg;  break;
    case TOP_setb:
    case TOP_setbe: jmp = TOP_ja;  break;
    case TOP_sete:  jmp = TOP_jne; break;
    }

    Build_OP( cmp_opcode, rflags, src1_hi, src2_hi, bb_cmp_hi_ops );
    Build_OP( jmp, rflags, Gen_Label_TN( bb_non_set_label, 0 ), bb_cmp_hi_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_cmp_hi, bb_cmp_hi_ops );
    OPS_Init( bb_cmp_hi_ops );
  }

  // Compare the lower 32-bit, given the same higher 32-bit.
  {
    OPS* bb_cmp_lo_ops = &New_OPs;
    TOP jmp = TOP_UNDEFINED;

    switch( set_opcode ){
    case TOP_setg:
    case TOP_seta:  jmp = TOP_ja;  break;
    case TOP_setge:
    case TOP_setae: jmp = TOP_jae; break;
    case TOP_setl:
    case TOP_setb:  jmp = TOP_jb;  break;
    case TOP_setle:
    case TOP_setbe: jmp = TOP_jbe; break;
    case TOP_setne: jmp = TOP_jne; break;
    case TOP_sete:  jmp = TOP_je;  break;
    }

    Build_OP( cmp_opcode, rflags, src1_lo, src2_lo, bb_cmp_lo_ops );
    Build_OP( jmp, rflags, Gen_Label_TN( bb_exit_label, 0 ), bb_cmp_lo_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_cmp_lo, bb_cmp_lo_ops );
    OPS_Init( bb_cmp_lo_ops );
  }

  // Now we reach a false condition
  {
    OPS* bb_non_set_ops = &New_OPs;

    Build_OP( TOP_ldc32, tmp_result, Gen_Literal_TN(0,4), bb_non_set_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_non_set, bb_non_set_ops );
    OPS_Init( bb_non_set_ops );
  }

  Cur_BB = bb_exit;

  if( result != tmp_result ){
    Exp_COPY( result, tmp_result, ops );
    if( Get_TN_Pair( result ) == NULL )
      return;
  }

  TN* result_hi = Create_TN_Pair( result, MTYPE_I8 );
  Build_OP( TOP_ldc32, result_hi, Gen_Literal_TN(0,4), ops );
}


static void Expand_Split_Cvtl( TYPE_ID mtype, TOP top, TN* result, TN* src, OPS* ops )
{
  TN* result_hi = Create_TN_Pair( result, mtype );

  switch( top ){
  case TOP_movsbq:
    Build_OP( TOP_movsbl, result, src, ops );
    Build_OP( TOP_sari32,  result_hi, src, Gen_Literal_TN(31,4), ops );
    break;

  case TOP_movswq:
    Build_OP( TOP_movswl, result, src, ops );
    Build_OP( TOP_sari32,  result_hi, src, Gen_Literal_TN(31,4), ops );
    break;

  case TOP_movslq:
    Build_OP( TOP_mov32, result, src, ops );
    Build_OP( TOP_sari32, result_hi, src, Gen_Literal_TN(31,4), ops );
    break;

  case TOP_movzbq:
    Build_OP( TOP_movzbl, result, src, ops );
    Build_OP( TOP_ldc32,  result_hi, Gen_Literal_TN(0,4), ops );
    break;

  case TOP_movzwq:
    Build_OP( TOP_movzwl, result, src, ops );
    Build_OP( TOP_ldc32,  result_hi, Gen_Literal_TN(0,4), ops );
    break;

  case TOP_mov32:
    Build_OP( TOP_mov32, result, src, ops );
    Build_OP( TOP_ldc32, result_hi, Gen_Literal_TN(0,4), ops );
    break;

  default:
    FmtAssert( FALSE,
	       ("Expand_Split_Cvtl: Unsupported operation (%s)", TOP_Name(top)) );
  }
}


/* Use two or three 32-bit operations to emulate a 64-bit
   unary operation.
*/
void Expand_Split_UOP( OPERATOR opr, TYPE_ID mtype,
		       TN* result, TN* src,
		       OPS* ops )
{
  TOP top = TOP_UNDEFINED;
  TN* result_h = Create_TN_Pair( result, mtype );
  TN* src_h = TN_has_value(src) ? NULL : Get_TN_Pair(src);

  if( TN_has_value(src) ){
    if( MTYPE_signed( mtype ) ){
      const INT64 val = TN_value( src );
      src   = Gen_Literal_TN( ( val << 32 ) >> 32, 4 );
      src_h = Gen_Literal_TN( ( val >> 32 ), 4 );
    } else {
      const UINT64 val = TN_value( src );
      src   = Gen_Literal_TN( ( val << 32 ) >> 32, 4 );
      src_h = Gen_Literal_TN( ( val >> 32 ), 4 );
    }
  }

  if ( src_h == NULL && opr != OPR_INTCONST ) {
    DevWarn( "The higher 32-bit of TN%d is treated as 0\n",
	     TN_number(src) );
    src_h = Build_TN_Like( src );
    Build_OP( TOP_ldc32, src_h, Gen_Literal_TN(0,4), ops );
  }

  switch( opr ){
  case OPR_BNOT:
    top = TOP_not32;
    break;
  case OPR_INTCONST:
    top = TOP_ldc32;
    break;
  case OPR_NEG:
    {
      Build_OP( TOP_neg32, result, src, ops );
      TN* tmp_src = Build_TN_Like( src );
      Build_OP( TOP_adci32, tmp_src, src_h, Gen_Literal_TN(0,4), ops );
      Set_OP_cond_def_kind( OPS_last(ops), OP_ALWAYS_COND_DEF );
      Build_OP( TOP_neg32, result_h, tmp_src, ops );
      return;
    }
    break;
  case OPR_LDA:
    top = TOP_mov32;
    break;
  default:
    FmtAssert( FALSE, ("Expand_Split_UOP: unknown operator") );
  }

  Build_OP( top, result,   src,   ops );
  Build_OP( top, result_h, src_h, ops );
}


/* Use two 32-bit binary operations to emulate a 64-bit
   binary operation.
*/
static void Expand_Split_BOP( OPERATOR opr, TYPE_ID mtype,
			      TN* result, TN* src1, TN* src2,
			      OPS* ops )
{
  TN* result_h = Create_TN_Pair( result, mtype );
  TN* src1_h = Get_TN_Pair(src1);
  TN* src2_h = TN_has_value(src2) ? NULL : Get_TN_Pair(src2);

  if( TN_has_value(src2) ){
    const INT64 val = TN_value(src2);
    src2   = Gen_Literal_TN( ( val << 32 ) >> 32, 4 );
    src2_h = Gen_Literal_TN( val >> 32, 4 );
  }

  if( src2_h == NULL ){
    DevWarn( "The higher 32-bit of TN%d is treated as 0\n",
	     TN_number(src2) );
    src2_h  = Build_TN_Like( src2 );
    Build_OP( TOP_ldc32, src2_h, Gen_Literal_TN(0,4), ops );    
  }

  TOP top = TOP_UNDEFINED, top_h = TOP_UNDEFINED;

  if( src1_h == NULL ){
    DevWarn( "The higher 32-bit of TN%d is treated as 0\n", TN_number(src1) );
    src1_h = Build_TN_Like( src1 );
    Build_OP( TOP_ldc32,  src1_h, Gen_Literal_TN(0,4), ops );    
  }

  switch( opr ){
  case OPR_ADD:
    top   = TN_has_value(src2) ? TOP_addi32 : TOP_add32;
    top_h = TN_has_value(src2) ? TOP_adci32 : TOP_adc32;
    break;
  case OPR_SUB:
    top   = TN_has_value(src2) ? TOP_subi32 : TOP_sub32;
    top_h = TN_has_value(src2) ? TOP_sbbi32 : TOP_sbb32;
    break;
  case OPR_BAND:
    top_h = top = TN_has_value(src2) ? TOP_andi32 : TOP_and32;
    break;
  case OPR_BXOR:
    top_h = top = TN_has_value(src2) ? TOP_xori32 : TOP_xor32;
    break;
  case OPR_BIOR:
    top_h = top = TN_has_value(src2) ? TOP_ori32 : TOP_or32;
    break;
  default:
    FmtAssert( false, ("Expand_Split_BOP: Unknown operator") );
  }

  Build_OP( top,   result,   src1,   src2,   ops );
  Build_OP( top_h, result_h, src1_h, src2_h, ops );
}


static void Expand_Split_Multiply( TN* result, TN* src1, TN* src2, OPS* ops )
{
  TN* result_hi = Create_TN_Pair( result, MTYPE_I8 );
  TN* src1_hi = Get_TN_Pair( src1 );
  TN* src2_hi = Get_TN_Pair( src2 );

  TN* tmp = NULL;
  if( src1_hi != NULL ){
    tmp = Build_TN_Like( result );
    Build_OP( TOP_imul32, tmp,  src1_hi, src2, ops );
  }

  if( src2_hi == NULL ){
    DevWarn( "The higher 32-bit of TN%d is treated as 0\n", TN_number(src2) );
    src2_hi = Build_TN_Like( src2 );
    Build_OP( TOP_ldc32, src2_hi, Gen_Literal_TN(0,4), ops );    
  }

  TN* tmp1 = Build_TN_Like( result );
  Build_OP( TOP_imul32, tmp1, src2_hi, src1, ops );

  TN* tmp2 = NULL;
  if( tmp != NULL ){
    tmp2 = Build_TN_Like( result );
    Build_OP( TOP_add32, tmp2, tmp, tmp1, ops );
  } else
    tmp2 = tmp1;

  TN* tmp_hi = Build_TN_Like( result );
  Build_OP( TOP_mul32, result, tmp_hi, src1, src2, ops );
  Build_OP( TOP_add32, result_hi, tmp_hi, tmp2, ops );    
}


static void Expand_Split_Shift( SHIFT_DIRECTION shift_dir,
				TN* result, TN* src_lo, TN* shift, OPS* ops )
{
  TN* src_hi = Get_TN_Pair( src_lo );

  if( src_hi == NULL ){
    if( TN_has_value( src_lo ) ){
      const INT64 val = TN_value(src_lo);
      src_hi = Gen_Literal_TN( val >> 32, 4 );

    } else {
      DevWarn( "The higher 32-bit of TN%d is treated as 0\n", TN_number(src_lo) );
      src_hi = Build_TN_Like( src_lo );
      Build_OP( TOP_ldc32, src_hi, Gen_Literal_TN(0,4), ops );
    }
  }

  // Handle case where <shift> has value.
  if( TN_has_value( shift ) ){
    const INT64 shift_amt = TN_value( shift );
    TN* result_hi = Create_TN_Pair( result, MTYPE_I8 );

    if( shift_amt >= 32 ){
      switch( shift_dir ){
      case shift_left:
	Build_OP( TOP_shli32, result_hi, src_lo, Gen_Literal_TN(shift_amt-32,4), ops );
	Build_OP( TOP_ldc32,  result,    Gen_Literal_TN(0,4), ops );
	break;

      case shift_aright:
	Build_OP( TOP_sari32, result,    src_hi, Gen_Literal_TN(shift_amt-32,4), ops );
	Build_OP( TOP_sari32, result_hi, src_hi, Gen_Literal_TN(31,4), ops );
	break;

      case shift_lright:
	Build_OP( TOP_shri32, result,    src_hi, Gen_Literal_TN(shift_amt-32,4), ops );
	Build_OP( TOP_ldc32,  result_hi, Gen_Literal_TN(0,4), ops );
	break;
      }

    } else {
      // for shift_amt < 32
      switch( shift_dir ){
      case shift_left:
	Build_OP( TOP_shldi32, result_hi, src_hi, src_lo, shift, ops );
	Build_OP( TOP_shli32,   result,    src_lo, shift,  ops );
	break;

      case shift_aright:
	Build_OP( TOP_shrdi32, result,    src_lo, src_hi, shift, ops );
	Build_OP( TOP_sari32,  result_hi, src_hi, shift,  ops );
	break;

      case shift_lright:
	if( src_hi == NULL ){
	  Build_OP( TOP_shri32, result,    src_lo, shift, ops );
	  Build_OP( TOP_ldc32,  result_hi, Gen_Literal_TN(0,4), ops );
	} else {
	  Build_OP( TOP_shrdi32, result,    src_lo, src_hi, shift, ops );
	  Build_OP( TOP_shri32,  result_hi, src_hi, shift,  ops );
	}
	break;
      }
    }

  } else {
    // Handle case where <shift> is a variable.
    TN* tmp_result = result;
    TN* tmp_result_hi = Create_TN_Pair( tmp_result, MTYPE_I8 );

    if( TN_is_dedicated( result ) ){
      tmp_result = Build_TN_Like( result );
      tmp_result_hi = Create_TN_Pair( tmp_result, MTYPE_I8 );
    }

    // Under m32, the x86 shift instruction can shift at most 31 bits (the
    // upper bits of the shift count are ignored).  Construct 64-bit shift from
    // 32-bit shifts.

    BB* bb_entry = Cur_BB;
    BB* bb_then = Gen_And_Append_BB( bb_entry );  // for shift_cnt > 31

    BB* bb_exit  = Gen_And_Append_BB( bb_then );
    const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = bb_exit_label;

    // Build bb_entry
    {
      switch( shift_dir ){
      case shift_left:
	Build_OP( TOP_shld32, tmp_result_hi, src_hi, src_lo, shift, ops );
	Build_OP( TOP_shl32,  tmp_result,    src_lo, shift, ops );
	break;

      case shift_aright:
	Build_OP( TOP_shrd32, tmp_result,    src_lo, src_hi, shift, ops );
	Build_OP( TOP_sar32,  tmp_result_hi, src_hi, shift, ops );
	break;

      case shift_lright:
	Build_OP( TOP_shrd32, tmp_result,    src_lo, src_hi, shift, ops );
	Build_OP( TOP_shr32,  tmp_result_hi, src_hi, shift, ops );
	break;
      }

      // Go to the then block if the shift count is in the range [32,63].  If
      // it is < 32, the shrd/shr combo already produces the correct result.
      // If it is > 63, it is treated as modulo 64.  This means go to the then
      // block only if (32 & shift count) is 1.  Bug 9687.

      TN *rflags = Rflags_TN();
      Build_OP(TOP_testi32, rflags, shift, Gen_Literal_TN(32, 4), ops);
      Build_OP(TOP_je, rflags, Gen_Label_TN(bb_exit_label, 0), ops);

      if( &New_OPs != ops )
	OPS_Append_Ops( &New_OPs, ops );
      Process_New_OPs();
      BB_Append_Ops( bb_entry, &New_OPs );
      OPS_Init( &New_OPs );
      OPS_Init( ops );
    }

    // Build bb_then here.
    {
      OPS* bb_then_ops = &New_OPs;

      switch( shift_dir ){
      case shift_left:
	Exp_COPY( tmp_result_hi, tmp_result, bb_then_ops );
	Build_OP( TOP_ldc32, tmp_result, Gen_Literal_TN(0,4), bb_then_ops );
	break;

      case shift_aright:
	Exp_COPY( tmp_result, tmp_result_hi, bb_then_ops );
	Build_OP( TOP_sari32, tmp_result_hi, tmp_result_hi,
		  Gen_Literal_TN(31,4), bb_then_ops );
	break;

      case shift_lright:
	Exp_COPY( tmp_result, tmp_result_hi, bb_then_ops );
	Build_OP( TOP_ldc32,  tmp_result_hi, Gen_Literal_TN(0,4), bb_then_ops );	
	break;
      }
      
      total_bb_insts = 0;
      Last_Processed_OP = NULL;
      Process_New_OPs();
      BB_Append_Ops( bb_then, bb_then_ops );
      OPS_Init( bb_then_ops );
    }

    Cur_BB = bb_exit;
    if( result != tmp_result )
      Expand_Split_UOP( OPR_LDA, MTYPE_I8, result, tmp_result, ops );
  }
}


static void Expand_Split_Abs( TN* dest, TN* src, TYPE_ID mtype, OPS* ops )
{
  TN* result = dest;

  if( TN_is_dedicated(dest) ){
    result = Build_TN_Like( dest );
  }

  TN* src_hi = Get_TN_Pair( src );

  FmtAssert( src_hi != NULL,
	     ("Expand_Split_Abs: the higher 32-bit of source is NULL") );

  BB* bb_entry = Cur_BB;
  BB* bb_then = Gen_And_Append_BB( bb_entry );
  BB* bb_exit = Gen_And_Append_BB( bb_then );

  const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

  BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_entry)) = NULL;
  WN_label_number(BB_branch_wn(bb_entry)) = bb_exit_label;

  // build bb_entry
  {
    Expand_Split_UOP( OPR_LDA, mtype, result, src, ops );

    Exp_OP3v( OPC_TRUEBR,
	      NULL,
	      Gen_Label_TN( bb_exit_label, 0 ),
	      src_hi,
	      Gen_Literal_TN(0,4),
	      V_BR_I4GE,
	      ops );

    if( &New_OPs != ops )
      OPS_Append_Ops( &New_OPs, ops );
    Process_New_OPs();
    BB_Append_Ops( bb_entry, &New_OPs );
    OPS_Init( &New_OPs );
    OPS_Init( ops );    
  }

  // Build bb_then here if src_hi < 0
  {
    OPS* bb_then_ops = &New_OPs;
    Expand_Split_UOP( OPR_NEG, mtype, result, src, bb_then_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_then, bb_then_ops );
    OPS_Init( bb_then_ops );    
  }

  Cur_BB = bb_exit;

  if( result != dest ){
    Expand_Split_UOP( OPR_LDA, MTYPE_I8, dest, result, ops );
  }
}


// If safezero is TRUE, then be careful to map 0 --> MTYPE_bit_size(mtype).
static void
Expand_Split_Leading_Zeros( TN* dest, TN* src, TYPE_ID mtype,
			    BOOL safezero, OPS* ops )
{
  // TN1 :- ldc32 (0x7f)
  // TN2 :- bsr32 TN_src_low ;
  // TN2 :- cmove TN1 (%rflags)
  // TN3 :- xori32 TN2 (0x20)
  // TN4 :- bsr32 TN_src_high;
  // TN4 :- cmove TN3 (%rflags)
  // TN5 :- xori32 TN4 (0x1f)
  if ( mtype != MTYPE_I8 && mtype != MTYPE_U8 )
    Fail_FmtAssertion("Expand_Split_Leading_Zeros: unexpected mtype");
  if ( TN_has_value(src) ) {  // I don't think this will ever happen.
    src = Expand_Immediate_Into_Register( src, TRUE, ops );
  }
  TN *src_hi = Get_TN_Pair(src);
  FmtAssert( src_hi != NULL, ("Expand_Split_Leading_Zeros: the "
			      "higher 32-bit of source is NULL") );
  TN *tmp1, *rflags = Rflags_TN();
  if (safezero) {
    tmp1 = Build_TN_Of_Mtype( MTYPE_I4 );
    Exp_Immediate( tmp1, Gen_Literal_TN(127, 4), TRUE, ops );
  }
  TN *tmp2 = Build_TN_Of_Mtype( MTYPE_I4 );
  Build_OP( TOP_bsr32, tmp2, src, ops );
  if (safezero) {
    Expand_Cmov( TOP_cmove, tmp2, tmp1, rflags, ops );
  }
  TN *tmp3 = Build_TN_Of_Mtype( MTYPE_I4 );
  Expand_Binary_Xor( tmp3, tmp2, Gen_Literal_TN(32, 4), MTYPE_I4, ops );
  TN *tmp4 = Build_TN_Of_Mtype( MTYPE_I4 );
  Build_OP( TOP_bsr32, tmp4, src_hi, ops );
  Expand_Cmov( TOP_cmove, tmp4, tmp3, rflags, ops );
  Expand_Binary_Xor( dest, tmp4, Gen_Literal_TN(31, 4), MTYPE_I4, ops );
}


void
Expand_Copy (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  const BOOL is_128bit = (MTYPE_size_reg(mtype) == 128);

  if( MTYPE_is_F10( mtype ) ){
    Build_OP( TOP_fmov, result, src, ops );

  } else if( MTYPE_is_float(mtype) ){
    if( Is_Target_SSE2() ) {
      if( Is_Target_Barcelona() || Is_Target_EM64T() || 
          Is_Target_Wolfdale()  || Is_Target_Core()  ||
          Is_Target_Orochi() ){
        Build_OP( TOP_movaps, result, src, ops );
      } else {
        Build_OP( TOP_movdq, result, src, ops );
      }
    } else
      Build_OP( TOP_fmov, result, src, ops );

  } else {
    if( OP_NEED_PAIR( mtype ) ){
      Expand_Split_UOP( OPR_LDA, mtype, result, src, ops );
      Set_OP_copy( OP_prev(OPS_last(ops)) );

    } else {
      Build_OP( MTYPE_is_size_double(mtype) ? TOP_mov64: TOP_mov32, 
		result, src, ops);
    }
  }

  Set_OP_copy (OPS_last(ops));
}

//
//  Helper routine to do proper sign extension
//
static void
Fixup_32_Bit_Op(TN *result,TN *src, TYPE_ID dest_type, OPS *ops)
{
  if (dest_type == MTYPE_I8 || dest_type == MTYPE_U8) {
    Expand_Copy(result,src,dest_type,ops);
  } else {
    Expand_Convert_Length (result, src, Gen_Literal_TN(MTYPE_size_reg(dest_type), 4),
			   dest_type, MTYPE_is_signed(dest_type),ops);
  }
}


/* ====================================================================
 *
 * Expand_Convert_Length
 *
 * ====================================================================
 */
void Expand_Convert_Length ( TN *dest, TN *src, TN *length_tn, TYPE_ID mtype,
			     BOOL signed_extension, OPS *ops )
{
  FmtAssert (! MTYPE_float(mtype),
	     ("Expand_Convert_Length: illegal data type\n"));
  FmtAssert (TN_has_value(length_tn),
	     ("Expand_Convert_Length: non-constant length\n"));
  const UINT64 val = TN_value(length_tn);
  const BOOL is_64bit = MTYPE_is_size_double(mtype);

  TOP new_opcode = TOP_UNDEFINED;

  if( val != 8 && val != 16  && val != 32 ){
    // Bug046
    if( signed_extension || val >= 32 ){
      TN* tmp1 = Build_TN_Like( dest );
      TN* tmp2 = Build_TN_Like( dest );
      const int shift_amt = is_64bit ? 64 - val : 32 - val;

      if ( val < 32 )
        Build_OP( TOP_andi32, tmp1, src, Gen_Literal_TN((1<<val)-1, 4), ops );
      else {
	TN* tmp3 = Build_TN_Like( dest );
	if( Is_Target_32bit() && is_64bit && Get_TN_Pair(tmp3) == 0 )
	  (void *) Create_TN_Pair( tmp3, mtype );
	Exp_Immediate( tmp3, Gen_Literal_TN ((1LL<<val)-1LL, 8), FALSE, ops );
	Expand_Binary_And(tmp1, src, tmp3, mtype, ops);
      }
      Expand_Shift( tmp2, tmp1, Gen_Literal_TN( shift_amt, 4 ),
		    mtype, shift_left, ops );
      Expand_Shift( dest, tmp2, Gen_Literal_TN( shift_amt, 4 ),
		    mtype, shift_aright, ops );
      
    } else {
      Build_OP( TOP_andi32, dest, src, Gen_Literal_TN((1<<val)-1, 4), ops );
    }

    return;

  } else if( val == 8 ){
    if( signed_extension ){
      new_opcode = is_64bit ? TOP_movsbq : TOP_movsbl;
    } else {
      new_opcode = is_64bit ? TOP_movzbq : TOP_movzbl;
    }

  } else if( val == 16 ){
    if( signed_extension ){
      new_opcode = is_64bit ? TOP_movswq : TOP_movswl;
    } else {
      new_opcode = is_64bit ? TOP_movzwq : TOP_movzwl;
    }

  } else if( val == 32 ){
    if( is_64bit ) {
      if (signed_extension)
        new_opcode = TOP_movslq;
      else {
	if( OP_NEED_PAIR(mtype) ){
	  Expand_Split_Cvtl( mtype, TOP_mov32, dest, src, ops );
	} else {
	  /* Fix bug#1363.
	     We are doing the OPC_U8U4CVT here.
	   */
	  Build_OP( TOP_movzlq, dest, src, ops);
	}
        return;
      }
    }
    else if( MTYPE_bit_size(mtype) == 32 ){
      // Bug 4117 - use a move here without setting the copy bits 
      // (that is, don't call Expand_Copy).
      // We use a movzlq instead of a mov32 to prevent the copy from
      // being treated as a nop when both the target and destination
      // operands are 8-byte.
      Build_OP( TOP_movzlq, dest, src, ops);
      return;
    }
  }

  FmtAssert( new_opcode != TOP_UNDEFINED,
	     ("Expand_Convert_Length: new opcode is undefined") );

  if( OP_NEED_PAIR(mtype) )
    Expand_Split_Cvtl( mtype, new_opcode, dest, src, ops );
  else
    Build_OP( new_opcode, dest, src, ops );
}

static void Exp_Immediate (TN *dest, TN *src, OPS *ops)
{
  INT64 val = 0;
  TN* tmp = Build_TN_Like(dest);

  if ( TN_has_value(src) ) {
    val = TN_value(src);

  } else  if ( TN_is_symbol(src) ) {
    ST *base;
    Base_Symbol_And_Offset_For_Addressing (TN_var(src), TN_offset(src), &base, &val);

  } else
    FmtAssert(FALSE,("unexpected constant in Exp_Immediate"));

  if( Is_Target_32bit()     &&
      /* TN_is_dedicated(dest) && */
#ifdef KEY // bug 14228
      ! (TN_is_symbol(src) && ST_sym_class(TN_var(src)) == CLASS_NAME) &&
#endif
      Get_TN_Pair(dest) != NULL ){
    Expand_Split_UOP( OPR_INTCONST, MTYPE_I8, dest, src, ops );
    
  } else if (TN_size(dest) == 8) {
    if( OP_NEED_PAIR( MTYPE_I8 ) ){
      Expand_Split_UOP( OPR_INTCONST, MTYPE_I8, dest, src, ops );
    } else {
      Build_OP (TOP_ldc64, dest, src, ops);
    }

  } else if (ISA_LC_Value_In_Class (val, LC_simm32)) {
    Build_OP (TOP_ldc32, dest, src, ops);

  } else if (ISA_LC_Value_In_Class (val, LC_uimm32)) {
    Build_OP (TOP_ldc32, dest, src, ops);

  } else if (val >= INT32_MIN && val <= INT32_MAX) {
    Build_OP (TOP_ldc32, dest, src, ops);

  } else if ((UINT64)val <= UINT32_MAX) {
    Build_OP (TOP_ldc32, dest, src, ops);

  } else if ((UINT64)val > UINT32_MAX) {
    if( Is_Target_32bit() ){
      // The upper 32-bit is dead.
      Build_OP( TOP_ldc32, dest, Gen_Literal_TN( (val & 0xffffffff), 4 ), ops );
    } else
      Build_OP (TOP_ldc64, dest, src, ops);

  } else
    FmtAssert( FALSE, ("UNIMPLEMENTED") );
}

void
Exp_Immediate (TN *dest, TN *src, BOOL is_signed, OPS *ops)
{
  Expand_Immediate(dest, src, is_signed, ops);
}

/* 
 * Expand Immediate value.
 */
void
Expand_Immediate (TN *dest, TN *src, BOOL is_signed, OPS *ops)
{
  FmtAssert((TN_is_constant(src)),
	    ("unexpected non-constant in Expand_Immediate"));
  FmtAssert((TN_has_value(src) || TN_is_symbol(src)), 
	    ("expected value or const in Expand_Immediate"));
  Exp_Immediate (dest, src, ops);
}

TN*
Expand_Immediate_Into_Register (TN *src, BOOL is_64bit, OPS *ops)
{
  /* load into reg and do reg case */
  TN *tmp = Build_TN_Of_Mtype (is_64bit ? MTYPE_I8 : MTYPE_I4);
  Expand_Immediate (tmp, src, TRUE, ops);
  return tmp;
}


void
Expand_Add (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TOP new_opcode;
  INT64 val;
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  BOOL is_vector_type;
  is_vector_type = (mtype == MTYPE_V16I1 || 
		    mtype == MTYPE_V16I2 ||
		    mtype == MTYPE_V16I4 ||
		    mtype == MTYPE_V16I8 ||
		    mtype == MTYPE_M8I1 ||
		    mtype == MTYPE_M8I2 ||
		    mtype == MTYPE_M8I4);
  if (TN_is_constant(src1) && !is_vector_type) {
    if (TN_has_value(src1)) {
      val = TN_value(src1);
      if (val == 0) {
	Expand_Copy (result, src2, mtype, ops);
	return;
      }
    } else if ( TN_is_symbol(src1) ) {
      /* symbolic constant, gp-relative or sp-relative */
      ST *base;
      INT64 val;
      TN *tmp = Build_TN_Of_Mtype (mtype);
      Base_Symbol_And_Offset_For_Addressing (TN_var(src1), TN_offset(src1), 
					     &base, &val);
      new_opcode = is_64bit ? TOP_addi64 : TOP_addi32;
      if( ISA_LC_Value_In_Class (val, LC_simm32) ){
	Build_OP (new_opcode, result, src2, src1, ops);
      } else if (val >= INT32_MIN && val <= INT32_MAX) {
	Build_OP (TOP_ldc32, tmp, Gen_Literal_TN((val >> 16)&0xffff, 4), ops);
	Build_OP (TOP_ori32, tmp, tmp, Gen_Literal_TN(val & 0xffff, 4), ops);
	Build_OP (is_64bit ? TOP_add64 : TOP_add32, result, tmp, src2, ops);
      } else {
	TN* const_tn = Gen_Const_Symbol_TN( val, 0.0, MTYPE_I8, TN_RELOC_GOT_DISP );
	Build_OP( Use_32_Bit_Pointers ? TOP_ld32 : TOP_ld64,
		  tmp, GP_TN, const_tn, ops );
	Build_OP(TOP_ld32, tmp, tmp, Gen_Literal_TN(0, 4), ops);
	Build_OP (is_64bit ? TOP_add64 : TOP_add32, result, tmp, src2, ops);
      }       
      return;
    } 
    else FmtAssert(FALSE,("unexpected constant in Expand_Add"));
    
    if (ISA_LC_Value_In_Class ( val, LC_simm32)) {
      if( OP_NEED_PAIR(mtype) ){
	Expand_Split_BOP( OPR_ADD, mtype, result, src2, Gen_Literal_TN(val,8), ops );

      } else {
	new_opcode = is_64bit ? TOP_addi64 : TOP_addi32;
	Build_OP (new_opcode, result, src2, Gen_Literal_TN(val,4), ops);
      }

    } else {
      src1 = Expand_Immediate_Into_Register( src1, is_64bit, ops );

      if( OP_NEED_PAIR(mtype) ){
	Expand_Split_BOP( OPR_ADD, mtype, result, src2, src1, ops );	

      } else {
	new_opcode = is_64bit ? TOP_add64 : TOP_add32;
	Build_OP (new_opcode, result, src2, src1, ops);
      }
    }
  } else if (TN_is_constant(src2) && !is_vector_type) {
    // switch order of src so immediate is first
    Expand_Add (result, src2, src1, mtype, ops);
  } else {
    switch(mtype) {
    case MTYPE_V16I1:
      Build_OP (TOP_add128v8, result, src1, src2, ops);
      break;
    case MTYPE_V16I2:
      Build_OP (TOP_add128v16, result, src1, src2, ops);
      break;
    case MTYPE_V16I4:
      Build_OP (TOP_add128v32, result, src1, src2, ops);
      break;
    case MTYPE_V16I8:
      Build_OP (TOP_add128v64, result, src1, src2, ops);
      break;
    case MTYPE_V8I1:
      Build_OP (TOP_add128v8, result, src1, src2, ops);
      break;
    case MTYPE_V8I2:
      Build_OP (TOP_add128v16, result, src1, src2, ops);
      break;
    case MTYPE_V8I4:
      Build_OP (TOP_add128v32, result, src1, src2, ops);
      break;
    case MTYPE_V8I8:
      Build_OP (TOP_add128v64, result, src1, src2, ops);
      break;
    case MTYPE_M8I1:
      Build_OP (TOP_add64v8, result, src1, src2, ops);
      break;
    case MTYPE_M8I2:
      Build_OP (TOP_add64v16, result, src1, src2, ops);
      break;
    case MTYPE_M8I4:
      Build_OP (TOP_add64v32, result, src1, src2, ops);
      break;
    case MTYPE_V8F4:
      Build_OP (TOP_fadd128v32, result, src1, src2, ops);
      break;
    case MTYPE_M8F4:
      Fail_FmtAssertion ("Expand_Add: NYI");
    default:
      if( OP_NEED_PAIR(mtype) ){
	Expand_Split_BOP( OPR_ADD, mtype, result, src1, src2, ops );

      } else {
	Build_OP (is_64bit ? TOP_add64 : TOP_add32, result, src1, src2, ops);
      }

      break;
    }
  }
}

void
Expand_Sub (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  INT64 val;
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TOP new_opcode;
  BOOL is_vector_type;
  is_vector_type = (mtype == MTYPE_V16I1 || 
		    mtype == MTYPE_V16I2 ||
		    mtype == MTYPE_V16I4 ||
		    mtype == MTYPE_V16I8 ||
		    mtype == MTYPE_M8I1 ||
		    mtype == MTYPE_M8I2 ||
		    mtype == MTYPE_M8I4);

  if (TN_is_constant(src2) && !is_vector_type) {
    if (TN_has_value(src2)) {
      val = - TN_value(src2);
      if (val == 0) {
	Expand_Copy (result, src1, mtype, ops);
	return;
      }
    } 
    else if ( TN_is_symbol(src2) ) {
      /* symbolic constant, gp-relative or sp-relative */
      ST *base;
      INT64 val;
      Base_Symbol_And_Offset_For_Addressing (TN_var(src2), TN_offset(src2), &base, &val);
      val = - val;
    } 
    else FmtAssert(FALSE,("unexpected constant in Expand_Sub"));
    
    if (ISA_LC_Value_In_Class ( val, LC_simm32)) {
      if( OP_NEED_PAIR(mtype) ){
	Expand_Split_BOP( OPR_ADD, mtype, result, src1, Gen_Literal_TN(val,8), ops );

      } else {
	new_opcode = is_64bit ? TOP_addi64 : TOP_addi32;
	Build_OP (new_opcode, result, src1, Gen_Literal_TN(val,4), ops);
      }

    } else {
      src2 = Expand_Immediate_Into_Register( src2, is_64bit, ops );

      if( OP_NEED_PAIR(mtype) ){
	Expand_Split_BOP( OPR_SUB, mtype, result, src1, src2, ops );

      } else {
	new_opcode = is_64bit ? TOP_sub64 : TOP_sub32;
	Build_OP (new_opcode, result, src1, src2, ops);
      }
    }
  }
  else if (TN_is_constant(src1) && !is_vector_type) {
    TN *tmp = Build_TN_Of_Mtype (mtype);
    // switch order of src so immediate is first
    Expand_Sub (tmp, src2, src1, mtype, ops);
    // generate a negate
    if( OP_NEED_PAIR(mtype) )
      Expand_Split_UOP( OPR_NEG, mtype, result, tmp, ops );
    else
      Build_OP(is_64bit ? TOP_neg64 : TOP_neg32, result, tmp, ops);
  } 
  else {
    switch(mtype) {
    case MTYPE_V16I1:
      Build_OP (TOP_sub128v8, result, src1, src2, ops);
      break;
    case MTYPE_V16I2:
      Build_OP (TOP_sub128v16, result, src1, src2, ops);
      break;
    case MTYPE_V16I4:
      Build_OP (TOP_sub128v32, result, src1, src2, ops);
      break;
    case MTYPE_V16I8:
      Build_OP (TOP_sub128v64, result, src1, src2, ops);
      break;
    case MTYPE_V8I1:
      Build_OP (TOP_sub128v8, result, src1, src2, ops);
      break;
    case MTYPE_V8I2:
      Build_OP (TOP_sub128v16, result, src1, src2, ops);
      break;
    case MTYPE_V8I4:
      Build_OP (TOP_sub128v32, result, src1, src2, ops);
      break;
    case MTYPE_V8I8:
      Build_OP (TOP_sub128v64, result, src1, src2, ops);
      break;
    case MTYPE_M8I1:
      Build_OP (TOP_sub64v8, result, src1, src2, ops);
      break;
    case MTYPE_M8I2:
      Build_OP (TOP_sub64v16, result, src1, src2, ops);
      break;
    case MTYPE_M8I4:
      Build_OP (TOP_sub64v32, result, src1, src2, ops);
      break;
    case MTYPE_M8F4:
      Fail_FmtAssertion ("Expand_Sub: NYI");
    default:
      if( OP_NEED_PAIR(mtype) ){
	Expand_Split_BOP( OPR_SUB, mtype, result, src1, src2, ops );

      } else {
	Build_OP (is_64bit ? TOP_sub64 : TOP_sub32, result, src1, src2, ops);
      }
      break;
    }
  }
}


void
Expand_Neg (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  FmtAssert ((MTYPE_bit_size(mtype) == 32 || 
	      MTYPE_bit_size(mtype) == 64 ||
	      MTYPE_bit_size(mtype) == 96 ||
	      MTYPE_bit_size(mtype) == 128 ),
	               ("Expand_Neg: illegal result size\n"));

  if (mtype == MTYPE_V16F4 || mtype == MTYPE_V16F8 ||
      mtype == MTYPE_V16I1 || mtype == MTYPE_V16I2 ||
      mtype == MTYPE_V16I4 || mtype == MTYPE_V16I8 ||
      mtype == MTYPE_V16C8) {
    switch (mtype) {
    case MTYPE_V16F4: {
      TCON then = Host_To_Targ (MTYPE_I4, 0x80000000);
      TCON now  = Create_Simd_Const (MTYPE_V16F4, then);
      ST *sym = New_Const_Sym (Enter_tcon (now), Be_Type_Tbl(TCON_ty(now)));
      Allocate_Object(sym);
      TN *sym_tn = Gen_Symbol_TN(sym, 0, 0);
      TN *tmp = Build_TN_Like(result);
      Exp_Load(mtype, mtype, tmp, TN_var(sym_tn), TN_offset(sym_tn), ops, 0);
      Build_OP(is_64bit ? TOP_xorpd: TOP_xorps, result, src, tmp, ops);
      break;
    }
    case MTYPE_V16C8:
    case MTYPE_V16F8: {
      TCON then = Host_To_Targ (MTYPE_I8, 0x8000000000000000ULL);
      TCON now  = Create_Simd_Const (MTYPE_V16F8, then);
      ST *sym = New_Const_Sym (Enter_tcon (now), Be_Type_Tbl(TCON_ty(now)));
      Allocate_Object(sym);
      TN *sym_tn = Gen_Symbol_TN(sym, 0, 0);
      TN *tmp = Build_TN_Like(result);
      Exp_Load(mtype, mtype, tmp, TN_var(sym_tn), TN_offset(sym_tn), ops, 0);
      Build_OP(is_64bit ? TOP_xorpd: TOP_xorps, result, src, tmp, ops);
      break;
    }
#ifdef KEY
    //Bug 5701 13347: Vectorize Neg of Integers
    case MTYPE_V16I1:
    case MTYPE_V16I2:
    case MTYPE_V16I4:
    case MTYPE_V16I8:{
      TYPE_ID host_type;
      if(mtype==MTYPE_V16I1) host_type = MTYPE_I1;
      else if(mtype==MTYPE_V16I2) host_type = MTYPE_I2;
           else if(mtype==MTYPE_V16I4) host_type = MTYPE_I4;
                else host_type = MTYPE_I8;
      TCON then = Host_To_Targ (host_type, 0x0);
      TCON now  = Create_Simd_Const (mtype, then);
      ST *sym = New_Const_Sym (Enter_tcon (now), Be_Type_Tbl(TCON_ty(now)));
      Allocate_Object(sym);
      TN *sym_tn = Gen_Symbol_TN(sym, 0, 0);
      TN *tmp = Build_TN_Like(result);
      Exp_Load(mtype, mtype, tmp, TN_var(sym_tn), TN_offset(sym_tn), ops, 0);
      Expand_Sub (result, tmp, src, mtype, ops);
      break;
    }
#endif
    default:
      FmtAssert(FALSE, ("Expand_Neg: unknown mtype"));
      break;
    }

  } else if (!MTYPE_is_float(mtype)) {
    if (MTYPE_is_mmx_vector(mtype)) {
      TYPE_ID host_type;
      if(mtype == MTYPE_M8I1) host_type = MTYPE_I1;
      else if(mtype == MTYPE_M8I2) host_type = MTYPE_I2;
      else if(mtype == MTYPE_M8I4) host_type = MTYPE_I4;
      else host_type = MTYPE_I8;
      TCON then = Host_To_Targ (host_type, 0x0);
      TCON now  = Create_Simd_Const (mtype, then);
      ST *sym = New_Const_Sym (Enter_tcon (now), Be_Type_Tbl(TCON_ty(now)));
      Allocate_Object(sym);
      TN *sym_tn = Gen_Symbol_TN(sym, 0, 0);
      TN *tmp = Build_TN_Like(result);
      Exp_Load(mtype, mtype, tmp, TN_var(sym_tn), TN_offset(sym_tn), ops, 0);
      Expand_Sub (result, tmp, src, mtype, ops);
    }
    else if( OP_NEED_PAIR(mtype) ){
      Expand_Split_UOP( OPR_NEG, mtype, result, src, ops );

    } else {
      Build_OP( is_64bit ? TOP_neg64 : TOP_neg32, result, src, ops );
    }

  } else if( MTYPE_is_F10(mtype) ||
	     !Is_Target_SSE2() ){
    Build_OP( TOP_fchs, result, src, ops );

  } else {
    // Perform neg operation by flipping the msb.
    TCON tcon = is_64bit ? Host_To_Targ (MTYPE_I8, 0x8000000000000000ULL) :
      Host_To_Targ (MTYPE_I4, 0x80000000);
    ST *sym = New_Const_Sym( Enter_tcon (tcon), Be_Type_Tbl(TCON_ty(tcon)) );
    Allocate_Object(sym);
    ST *base_sym; INT64 base_ofst;

    Base_Symbol_And_Offset_For_Addressing (sym, 0, &base_sym, &base_ofst);

    TN *tmp = Build_TN_Like(result);
    if( Is_Target_64bit() ){
      Build_OP(is_64bit ? TOP_ldsd : TOP_ldss, tmp, Rip_TN(),
	       Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_NONE), ops);
    } else {
      Build_OP(is_64bit ? TOP_ldsd_n32 : TOP_ldss_n32, tmp,
	       Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_NONE), ops);
    }

    Set_OP_no_alias( OPS_last(ops)  );
    Build_OP(is_64bit ? TOP_xorpd: TOP_xorps, result, src, tmp, ops);
  }
}


void
Expand_Abs (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
  BOOL is_double = MTYPE_is_size_double(mtype);
  if (!MTYPE_is_float(mtype)) {
    if( dest == src ){
      TN* tmp_src = Build_TN_Like( src );
      Expand_Copy( tmp_src, src, mtype, ops );
      src = tmp_src;
    }

    if( OP_NEED_PAIR(mtype) ){
      Expand_Split_Abs( dest, src, mtype, ops );

    } else {
      Expand_Neg (dest, src, mtype, ops);
      Expand_Cmov (TOP_cmovs, dest, src, Rflags_TN(), ops);
    }

  } else if( MTYPE_is_F10( mtype ) ||
	     !Is_Target_SSE2() ){
    Build_OP( TOP_fabs, dest, src, ops );

  } else if ( MTYPE_is_vector( mtype ) ) {
    FmtAssert( mtype == MTYPE_V16F4 || mtype == MTYPE_V16F8, ("NYI") );
    if ( mtype == MTYPE_V16F4 ) {
      TCON then = Host_To_Targ (MTYPE_I4, 0x7FFFFFFF);
      TCON now  = Create_Simd_Const (MTYPE_V16F4, then);
      ST *sym = New_Const_Sym (Enter_tcon (now), Be_Type_Tbl(TCON_ty(now)));
      Allocate_Object(sym);
      TN *sym_tn = Gen_Symbol_TN(sym, 0, 0);
      TN *tmp = Build_TN_Like(dest);
      Exp_Load(mtype, mtype, tmp, TN_var(sym_tn), TN_offset(sym_tn), ops, 0);
      Build_OP(TOP_andps, dest, src, tmp, ops);      
    } else {
      TCON then = Host_To_Targ (MTYPE_I8, 0x7FFFFFFFFFFFFFFFULL);
      TCON now  = Create_Simd_Const (MTYPE_V16F8, then);
      ST *sym = New_Const_Sym (Enter_tcon (now), Be_Type_Tbl(TCON_ty(now)));
      Allocate_Object(sym);
      TN *sym_tn = Gen_Symbol_TN(sym, 0, 0);
      TN *tmp = Build_TN_Like(dest);
      Exp_Load(mtype, mtype, tmp, TN_var(sym_tn), TN_offset(sym_tn), ops, 0);
      Build_OP(TOP_andpd, dest, src, tmp, ops);
    }

  } else {
    TCON tcon = is_double ? Host_To_Targ (MTYPE_I8, 0x7FFFFFFFFFFFFFFFULL) :
      Host_To_Targ (MTYPE_I4, 0x7FFFFFFF);
    ST *sym = New_Const_Sym (Enter_tcon (tcon), Be_Type_Tbl(TCON_ty(tcon)) );
    Allocate_Object(sym);
    ST *base_sym; INT64 base_ofst;

    Base_Symbol_And_Offset_For_Addressing (sym, 0, &base_sym, &base_ofst);

    TN *tmp = Build_TN_Like(dest);

    if( Is_Target_64bit() ){
      Build_OP(is_double ? TOP_ldsd : TOP_ldss, tmp, Rip_TN(),
	       Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_NONE), ops);
    } else {
      Build_OP(is_double ? TOP_ldsd_n32 : TOP_ldss_n32, tmp,
	       Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_NONE), ops);
    }

    Set_OP_no_alias( OPS_last(ops)  );
    Build_OP(is_double ? TOP_andpd: TOP_andps, dest, src, tmp, ops);
  }
  return;
}

void
Expand_Shift (TN *result, TN *src1, TN *src2, TYPE_ID mtype, SHIFT_DIRECTION kind, OPS *ops)
{
  const TOP shift_top[ 6 /* mtypes */ ][ 3 /* kind */ ] = {
    { TOP_shl32, TOP_sar32, TOP_shr32 },           /* 32bit int */
    { TOP_shl64, TOP_sar64, TOP_shr64 },           /* 64bit int */
    { TOP_UNDEFINED, TOP_UNDEFINED, TOP_UNDEFINED },  /* V16I1 */
    { TOP_psllw, TOP_psraw, TOP_psrlw },              /* V16I2 */
    { TOP_pslld, TOP_psrad, TOP_psrld },              /* V16I4 */
    { TOP_psllq, TOP_UNDEFINED, TOP_psrlq },          /* V16I8 */
  };
  const TOP shift_imm_top[ 6 /* mtypes */ ][ 3 /* kind */ ] = {
    { TOP_shli32, TOP_sari32, TOP_shri32 },           /* 32bit int */
    { TOP_shli64, TOP_sari64, TOP_shri64 },           /* 64bit int */
    { TOP_UNDEFINED, TOP_UNDEFINED, TOP_UNDEFINED },  /* V16I1 */
    { TOP_psllwi, TOP_psrawi, TOP_psrlwi },           /* V16I2 */
    { TOP_pslldi, TOP_psradi, TOP_psrldi },           /* V16I4 */
    { TOP_psllqi, TOP_UNDEFINED, TOP_psrlqi },        /* V16I8 */
  };

  WN *tree;
  TOP top;  
  const BOOL is_vector = MTYPE_is_vector(mtype);
  const BOOL is_64bit = MTYPE_is_size_double(mtype);

  INT kind_index = 0;
  switch (kind) {
  case shift_left:
    kind_index = 0;
    break;
  case shift_aright:
    kind_index = 1;
    break;
  case shift_lright:
    kind_index = 2;
    break;
  default:
    FmtAssert(FALSE, ("invalid shift kind"));
  }

  INT type_index = 0;
  UINT8 shift_amt = 0;
  switch (mtype) {
  case MTYPE_V16I1:
    type_index = 2;
    shift_amt = 7;
    break;
  case MTYPE_V16I2:
    type_index = 3;
    shift_amt = 15;
    break;
  case MTYPE_V16I4:
    type_index = 4;
    shift_amt = 31;
    break;
  case MTYPE_V16I8:
    type_index = 5;
    shift_amt = 63;
    break;
  default:
    FmtAssert(is_vector == FALSE, ("NYI: support vector type other than V16I*"));
    type_index = is_64bit ? 1 : 0;
    shift_amt = is_64bit ? 63 : 31;
  }

  if (TN_is_constant(src1)) {
    FmtAssert(is_vector == FALSE, ("TODO: handle vector immediate"));
    src1 = Expand_Immediate_Into_Register(src1, is_64bit, ops);
  }
  if (TN_has_value(src2)) {
    // In mips, only the low log2(wordsize) bits of the shift count are used. 
    const UINT64 val = TN_value(src2);
    FmtAssert( val <= shift_amt, ("Shift amount > %d", shift_amt) );
    if (kind == shift_left && val == 1) {
      Expand_Add( result, src1, src1, mtype, ops );
      return;
    }

    top = shift_imm_top[type_index][kind_index];
    src2 = Gen_Literal_TN( val & shift_amt, 4 );

  } else {
    top = shift_top[type_index][kind_index];
  }

  FmtAssert(top != TOP_UNDEFINED, ("NYI: Expand Shift: mtype=%d, kind=%d", mtype, kind));

  if( OP_NEED_PAIR( mtype ) ) {
    Is_True(is_vector == FALSE, ("vector types do not need pair"));
    Expand_Split_Shift( kind, result, src1, src2, ops );
  }
  else
    Build_OP(top, result, src1, src2, ops);
}

void
Expand_Rrotate (TN *result, TN *src1, TN *src2, TYPE_ID rtype, TYPE_ID desc, OPS *ops)
{
  WN *tree;
  TOP top;  
  const BOOL value_size = MTYPE_bit_size(desc);
  const BOOL is_64bit = MTYPE_is_size_double(rtype);

  if (TN_is_constant(src1))
    src1 = Expand_Immediate_Into_Register(src1, is_64bit, ops);
  if (TN_has_value(src2)) {
    const UINT64 bits_to_rotate = TN_value(src2);
    UINT bitmask; // only the lowest log2(value_size) bits of bits_to_rotate 
    		  // are used 
    switch (value_size) {
    case 8: top = TOP_rori8; bitmask = 0x7; break;
    case 16: top = TOP_rori16; bitmask = 0xf; break;
    case 32: top = TOP_rori32; bitmask = 0x1f; break;
    case 64: top = TOP_rori64; bitmask = 0x3f; break;
    }

    src2 = Gen_Literal_TN( bits_to_rotate & bitmask, 4 );

  } else {
    switch (value_size) {
    case 8: top = TOP_ror8; break;
    case 16: top = TOP_ror16; break;
    case 32: top = TOP_ror32; break;
    case 64: top = TOP_ror64; break;
    }
  }

  if( OP_NEED_PAIR( rtype ) )
    FmtAssert(FALSE,("RROTATE of simulated 64-bit integer NYI"));
  else
    Build_OP(top, result, src1, src2, ops);
}

void
Expand_Left_Rotate (TN *result, TN *src1, TN *src2, TYPE_ID rtype, TYPE_ID desc, OPS *ops)
{
  WN *tree;
  TOP top;  
  const BOOL value_size = MTYPE_bit_size(desc);
  const BOOL is_64bit = MTYPE_is_size_double(rtype);

  if (TN_is_constant(src1))
    src1 = Expand_Immediate_Into_Register(src1, is_64bit, ops);
  if (TN_has_value(src2)) {
    const UINT64 bits_to_rotate = TN_value(src2);
    UINT bitmask; // only the lowest log2(value_size) bits of bits_to_rotate 
    		  // are used 
    switch (value_size) {
    case 8: top = TOP_roli8; bitmask = 0x7; break;
    case 16: top = TOP_roli16; bitmask = 0xf; break;
    case 32: top = TOP_roli32; bitmask = 0x1f; break;
    case 64: top = TOP_roli64; bitmask = 0x3f; break;
    }

    src2 = Gen_Literal_TN( bits_to_rotate & bitmask, 4 );

  } else {
    switch (value_size) {
    case 8: top = TOP_rol8; break;
    case 16: top = TOP_rol16; break;
    case 32: top = TOP_rol32; break;
    case 64: top = TOP_rol64; break;
    }
  }

  if( OP_NEED_PAIR( rtype ) )
    FmtAssert(FALSE,("Left-rotate of simulated 64-bit integer NYI"));
  else
    Build_OP(top, result, src1, src2, ops);
}

inline void
Expand_G_To_F (TN *ftn, TN *gtn, OPS *ops)
{
  FmtAssert(FALSE,("Unimplemented"));
}

inline void
Expand_F_To_G (TN *gtn, TN *ftn, OPS *ops)
{
  FmtAssert(FALSE,("Unimplemented"));
}

/*
 *  Try to expand a multiply into a sequence of less expensive operations.
 */
static BOOL
Expand_Constant_Multiply (TN *result, TN *var_tn, TARG_INT constant, TYPE_ID mtype, OPS *ops)
{
  if( OP_NEED_PAIR(mtype) ){
    DevWarn( "Expand_Constant_Multiply: SUPPORT ME !!!" );
    return FALSE;
  }

  BOOL did_do_fast;
  INT16 limit;	/* maximum number of operations to replace the multiply */
  TN *x = var_tn;
  INT64 c = constant; // I don't want to depend on TARG_INT
  BOOL needs_sign_extension;

  // fast special cases
  if (c == 0) {
    Expand_Copy (result, Zero_TN, MTYPE_I8, ops);
    return TRUE;
  } else if (c == 1) {
    Expand_Copy (result, var_tn, MTYPE_I8, ops);
    return TRUE;
  } else if (c == -1) {
    Expand_Neg(result, var_tn, mtype, ops);
    return TRUE;
  }
    
  needs_sign_extension = MTYPE_size_reg(mtype) != 64;

  if (c < 0) {
    c = -c;
    x = DUP_TN(var_tn);
    Expand_Neg(x, var_tn, mtype, ops);
  }    

  TOP lea;
  TN *tmp_tn = Build_TN_Like(result);
  TN *tmp1_tn = Build_TN_Like(result);
  if (mtype == MTYPE_I4 || mtype == MTYPE_U4)
    lea = TOP_leax32;
  else
    lea = TOP_leax64;
  switch (c) {
  case 3:
    Build_OP (lea, result, x, x, Gen_Literal_TN (2, 4), 
	      Gen_Literal_TN(0, 4), ops); 
    break;
  case 5:
    Build_OP (lea, result, x, x, Gen_Literal_TN (4, 4), 
	      Gen_Literal_TN (0, 4), ops);
    break;
  case 6:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (2, 4), 
	      Gen_Literal_TN (0, 4), ops); 
    Expand_Add (result, tmp_tn, tmp_tn, mtype, ops);
    break;
  case 7:
    Expand_Shift (tmp_tn, x, 
		  Gen_Literal_TN (3, 4), mtype, shift_left, ops);
    Expand_Sub (result, tmp_tn, x, mtype, ops);
    break;
  case 9:
    Build_OP (lea, result, x, x, Gen_Literal_TN (8, 4), 
	      Gen_Literal_TN (0, 4), ops);
    break;
  case 10:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (4, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Add (result, tmp_tn, tmp_tn, mtype, ops);    
    break;
  case 11:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (8, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Add (tmp1_tn, x, x, mtype, ops);    
    Expand_Add (result, tmp1_tn, tmp_tn, mtype, ops);    
    break;    
  case 12:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (2, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (result, tmp_tn, 
		  Gen_Literal_TN (2, 4), mtype, shift_left, ops);
    break;    
  case 13:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (2, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (tmp1_tn, x, 
		  Gen_Literal_TN (4, 4), mtype, shift_left, ops);
    Expand_Sub (result, tmp1_tn, tmp_tn, mtype, ops);    
    break;    
  case 14:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (1, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (tmp1_tn, x, 
		  Gen_Literal_TN (4, 4), mtype, shift_left, ops);
    Expand_Sub (result, tmp1_tn, tmp_tn, mtype, ops);    
    break;    
  case 15:
    Expand_Shift (tmp_tn, x, 
		  Gen_Literal_TN (4, 4), mtype, shift_left, ops);
    Expand_Sub (result, tmp_tn, x, mtype, ops);    
    break;    
  case 17:
    Expand_Shift (tmp_tn, x, 
		  Gen_Literal_TN (4, 4), mtype, shift_left, ops);
    Expand_Add (result, tmp_tn, x, mtype, ops);    
    break;    
  case 18:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (8, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Add (result, tmp_tn, tmp_tn, mtype, ops);    
    break;
  case 19:
    Build_OP (lea, tmp1_tn, x, x, Gen_Literal_TN (2, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (tmp_tn, x, 
		  Gen_Literal_TN (4, 4), mtype, shift_left, ops);
    Expand_Add (result, tmp_tn, tmp1_tn, mtype, ops);    
    break;
  case 20:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (4, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (result, tmp_tn, 
		  Gen_Literal_TN (2, 4), mtype, shift_left, ops);
    break;
  case 21:
    Build_OP (lea, tmp1_tn, x, x, Gen_Literal_TN (4, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (tmp_tn, x, 
		  Gen_Literal_TN (4, 4), mtype, shift_left, ops);
    Expand_Add (result, tmp_tn, tmp1_tn, mtype, ops);    
    break;
  case 23:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (8, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (tmp1_tn, x, 
		  Gen_Literal_TN (5, 4), mtype, shift_left, ops);
    Expand_Sub (result, tmp1_tn, tmp_tn, mtype, ops);    
    break;    
  case 24:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (2, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (result, tmp_tn, 
		  Gen_Literal_TN (3, 4), mtype, shift_left, ops);
    break;
  case 25:
    Build_OP (lea, tmp1_tn, x, x, Gen_Literal_TN (8, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (tmp_tn, x, 
		  Gen_Literal_TN (4, 4), mtype, shift_left, ops);
    Expand_Add (result, tmp_tn, tmp1_tn, mtype, ops);    
    break;
  case 27:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (4, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (tmp1_tn, x, 
		  Gen_Literal_TN (5, 4), mtype, shift_left, ops);
    Expand_Sub (result, tmp1_tn, tmp_tn, mtype, ops);    
    break;    
  case 28:
    Build_OP (mtype == MTYPE_I4 || 
	      mtype == MTYPE_U4 ? TOP_leaxx32 : 
	      TOP_leaxx64,tmp_tn, x, Gen_Literal_TN (4, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (tmp1_tn, x, 
		  Gen_Literal_TN (5, 4), mtype, shift_left, ops);
    Expand_Sub (result, tmp1_tn, tmp_tn, mtype, ops);    
    break;
  case 29:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (2, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (tmp1_tn, x, 
		  Gen_Literal_TN (5, 4), mtype, shift_left, ops);
    Expand_Sub (result, tmp1_tn, tmp_tn, mtype, ops);    
    break;    
  case 30:
    Build_OP (lea, tmp_tn, x, x, Gen_Literal_TN (1, 4), 
	      Gen_Literal_TN (0, 4), ops);
    Expand_Shift (tmp1_tn, x, 
		  Gen_Literal_TN (5, 4), mtype, shift_left, ops);
    Expand_Sub (result, tmp1_tn, tmp_tn, mtype, ops);    
    break;    
  case 31:
    Expand_Shift (tmp_tn, x, 
		  Gen_Literal_TN (5, 4), mtype, shift_left, ops);
    Expand_Sub (result, tmp_tn, x, mtype, ops);    
    break;    
  default:
    return FALSE;
  }
  return TRUE;
}

void
Expand_Multiply (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);

  FmtAssert( !TN_has_value(src1), ("UNIMPLEMENTED") );
  FmtAssert( MTYPE_is_integral(mtype) && !MTYPE_is_mmx_vector(mtype),
             ("Should be handled in Expand_Flop") );

  if ( mtype == MTYPE_V16I2 || mtype == MTYPE_V8I2 || mtype == MTYPE_V16I4 || mtype == MTYPE_V16I1) {
    Expand_Flop( OPCODE_make_op(OPR_MPY, mtype, MTYPE_V), 
                 result, src1, src2, NULL, ops );
    return;
  }

  if( TN_has_value(src2) ){
    INT64 val = TN_value( src2 );
    if( val > 0 &&
	( val & ( val - 1 ) ) == 0 ){
      int amt = 0;
      while( val != 1 ){
	amt++;
	val >>= 1;
      }
      TN* shift_tn = Gen_Literal_TN( amt, 4 );
      Expand_Shift( result, src1, shift_tn, mtype, shift_left, ops );

      return;

    } else if (CGEXP_cvrt_int_mult_to_add_shift &&
	       Can_Do_Fast_Multiply (mtype, val)) {
      if (Expand_Constant_Multiply (result, src1, val, mtype, ops)) {
	/* able to convert multiply into shifts/adds/subs */
	return;
      }      
    } 
  }

  if (TN_has_value(src2)) {
    src2 = Expand_Immediate_Into_Register (src2, is_64bit, ops);
  }
  FmtAssert(!TN_is_constant(src1),
	    ("Expand_Multiply: unexpected constant operand"));
  if (TN_is_constant(src2))
    src2 = Expand_Immediate_Into_Register(src2, is_64bit, ops);

  if( OP_NEED_PAIR( mtype ) ){
    Expand_Split_Multiply( result, src1, src2, ops );

  } else {
    Build_OP( is_64bit ? TOP_imul64 : TOP_imul32, result, src1, src2, ops );
  }
}

/* return high part of multiply result */
void
Expand_High_Multiply (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert(!TN_is_constant(src1),
		  ("Expand_High_Multiply: unexpected constant operand"));
  if (!MTYPE_is_size_double(mtype)) {
    if (MTYPE_is_signed(mtype)) {
      if (TN_is_constant(src2))
	src2 = 
	  Expand_Immediate_Into_Register(src2, 
					 MTYPE_is_size_double(mtype), ops);
      TN *tmp_tn = Build_TN_Of_Mtype (MTYPE_I8);
      Build_OP(TOP_imul64, tmp_tn, src1, src2, ops); 
      Expand_Shift (result, tmp_tn, Gen_Literal_TN(32, 4), 
		    MTYPE_I8, shift_lright, ops);
   } else {
      if (TN_is_constant(src2))
	src2 = 
	  Expand_Immediate_Into_Register(src2, 
					 MTYPE_is_size_double(mtype), ops);
      TN *tmp_tn = Build_TN_Like( result );
      Build_OP(TOP_mul32, tmp_tn, result, src1, src2, ops);
    }
  } else {    
    BOOL is_signed = MTYPE_is_signed(mtype);
    TN *result1 = Build_TN_Like(result);
    Build_OP(is_signed?TOP_imulx64:TOP_mulx64, 
	     result1, result, src1, src2, ops);
  }
}


void Expand_Logical_Not (TN *dest, TN *src, VARIANT variant, OPS *ops)
{
  /* dest = (src == 0) ? 1 : 0 */
  const BOOL is_64bit = (TN_size(src) == 8 );
  TN *rflags = Rflags_TN();
  // Perform "test" before clearing dest, since dest could be the same as src.
  // Use "move 0" instead of xor to clear dest in order to preserve rflag.
  Build_OP(is_64bit ? TOP_test64 : TOP_test32, rflags, src, src, ops);
  Build_OP(TOP_ldc32, dest, Gen_Literal_TN(0, 4), ops);
  Build_OP(TOP_sete, dest, rflags, ops);
  Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
}

void Expand_Logical_And (TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
  const BOOL is_64bit = (TN_size(src1) == 8 && TN_size(src2) == 8);
  Build_OP( is_64bit ? TOP_and64 : TOP_and32, dest, src1, src2, ops );
}

void Expand_Logical_Or (TN *dest, TN *src1, TN *src2, VARIANT variant, OPS *ops)
{
  const BOOL is_64bit = (TN_size(src1) == 8 && TN_size(src2) == 8);
  Build_OP( is_64bit ? TOP_or64 : TOP_or32, dest, src1, src2, ops );
}


void Expand_Binary_Complement (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
  if (MTYPE_is_vector(mtype)) {
    TCON then = Host_To_Targ(Mtype_vector_elemtype(mtype), -1);
    TCON now = Create_Simd_Const(mtype, then);
    ST *sym = New_Const_Sym(Enter_tcon(now), Be_Type_Tbl(TCON_ty(now)));
    Allocate_Object(sym);
    TN *sym_tn = Gen_Symbol_TN(sym, 0, 0);
    TN *result_tn = Build_TN_Of_Mtype(mtype);
    Exp_Load(mtype, mtype, result_tn, TN_var(sym_tn), TN_offset(sym_tn), ops, 0);
    if (MTYPE_is_mmx_vector(mtype))
      Build_OP(TOP_pxor_mmx, dest, src, result_tn, ops);
    else
      Build_OP(TOP_xorps, dest, src, result_tn, ops);
  } else {
    if( OP_NEED_PAIR(mtype) )
      Expand_Split_UOP( OPR_BNOT, mtype, dest, src, ops );
    else
      Build_OP( MTYPE_is_size_double(mtype) ? TOP_not64 : TOP_not32, dest, src, ops );
  }
}

void Expand_Binary_And (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TOP new_opcode = is_64bit ? TOP_and64 : TOP_and32;

  if (TN_is_constant(src1)) {
    FmtAssert(TN_has_value(src1),("unexpected constant in Expand_Binary_And"));
    INT64 val = TN_value(src1);
    if (val == -1 ||
        !is_64bit && (val << 32 >> 32) == -1) {
      Expand_Copy (dest, src2, mtype, ops);
      return;
    }

    // Change "andl 0xff,src2" to "movzbl src1,src2".  This saves 3 bytes and
    // does not require the src and dest to be the same register.
    if (val == 0xff) {
      if( OP_NEED_PAIR(mtype) )
	Expand_Split_Cvtl( mtype, TOP_movzbq, dest, src2, ops );
      else
	Build_OP( is_64bit ? TOP_movzbq : TOP_movzbl, dest, src2, ops );
      return;
    }

    // Likewise for 32-bit src.
    if (val == 0xffff) {
      new_opcode = is_64bit ? TOP_movzwq : TOP_movzwl;
      if( OP_NEED_PAIR(mtype) )
	Expand_Split_Cvtl( mtype, new_opcode, dest, src2, ops );
      else
	Build_OP (new_opcode, dest, src2, ops);
      return;
    }

    if (!is_64bit || ISA_LC_Value_In_Class ( val, LC_simm32)) 
      new_opcode = is_64bit ? TOP_andi64 : TOP_andi32;
    else {
      src1 = Expand_Immediate_Into_Register(src1, is_64bit, ops);
    }

    if( OP_NEED_PAIR(mtype) )
      Expand_Split_BOP( OPR_BAND, mtype, dest, src2, src1, ops );
    else
      Build_OP (new_opcode, dest, src2, src1, ops);

  } else if (TN_is_constant(src2)) {
    // switch order of src so immediate is first
    Expand_Binary_And (dest, src2, src1, mtype, ops);

  } else {
    switch(mtype) {
    case MTYPE_V16I1: 
      Build_OP(TOP_and128v8, dest, src1, src2, ops); break;
    case MTYPE_V16I2: 
      Build_OP(TOP_and128v16, dest, src1, src2, ops); break;
    case MTYPE_V16I4: 
      Build_OP(TOP_and128v32, dest, src1, src2, ops); break;
    case MTYPE_V16I8: 
      Build_OP(TOP_and128v64, dest, src1, src2, ops); break;
    case MTYPE_V8F4:
      Build_OP(TOP_andps, dest, src1, src2, ops); break;
    case MTYPE_V32F4: 
    case MTYPE_V16F4:
      Build_OP(TOP_fand128v32, dest, src1, src2, ops); break;
    case MTYPE_V32F8:
    case MTYPE_V16F8: 
      Build_OP(TOP_fand128v64, dest, src1, src2, ops); break;
    case MTYPE_M8I1:
    case MTYPE_M8I2:
    case MTYPE_M8I4:
    case MTYPE_M8F4:
      Build_OP(TOP_pand_mmx,dest, src1, src2, ops); break;
    default:
      if( OP_NEED_PAIR(mtype) ){
	Expand_Split_BOP( OPR_BAND, mtype, dest, src1, src2, ops );

      } else {
	Build_OP(new_opcode, dest, src1, src2, ops);
      }
      break;
    }
  }
}

void Expand_Binary_Or (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TOP new_opcode = is_64bit ? TOP_or64 : TOP_or32;

  if (TN_is_constant(src1)) {
    FmtAssert(TN_has_value(src1),("unexpected constant in Expand_Binary_Or"));
    INT64 val = TN_value(src1);
    if (val == 0) {
      Expand_Copy (dest, src2, mtype, ops);
      return;
    }

    if (!is_64bit || ISA_LC_Value_In_Class ( val, LC_simm32)) 
      new_opcode = is_64bit ? TOP_ori64 : TOP_ori32;
    else {
      src1 = Expand_Immediate_Into_Register(src1, is_64bit, ops);
    }

    if( OP_NEED_PAIR( mtype ) )
      Expand_Split_BOP( OPR_BIOR, mtype, dest, src2, src1, ops );
    else
      Build_OP (new_opcode, dest, src2, src1, ops);

  } else if (TN_is_constant(src2)) {
    // switch order of src so immediate is first
    Expand_Binary_Or (dest, src2, src1, mtype, ops);

  } else {
    switch(mtype) {
    case MTYPE_V16I1: 
      Build_OP(TOP_or128v8, dest, src1, src2, ops); break;
    case MTYPE_V16I2: 
      Build_OP(TOP_or128v16, dest, src1, src2, ops); break;
    case MTYPE_V16I4: 
      Build_OP(TOP_or128v32, dest, src1, src2, ops); break;
    case MTYPE_V16I8: 
      Build_OP(TOP_or128v64, dest, src1, src2, ops); break;
    case MTYPE_V8F4:
      Build_OP(TOP_orps, dest, src1, src2, ops); break;
    case MTYPE_M8I1:
    case MTYPE_M8I2:
    case MTYPE_M8I4:
    case MTYPE_M8F4:
      Build_OP(TOP_por_mmx,dest, src1, src2, ops); break;
    default:
      if( OP_NEED_PAIR(mtype) ){
	Expand_Split_BOP( OPR_BIOR, mtype, dest, src1, src2, ops );

      } else {
	Build_OP(new_opcode, dest, src1, src2, ops);
      }
      break;
    }
  }
}

void Expand_Binary_Xor (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TOP new_opcode = is_64bit ? TOP_xor64 : TOP_xor32;

  if (TN_is_constant(src1)) {
    FmtAssert(TN_has_value(src1),("unexpected constant in Expand_Binary_And"));
    INT64 val = TN_value(src1);
    if (val == 0 && src1 == dest ) {
      return;
    }

    if (!is_64bit || ISA_LC_Value_In_Class ( val, LC_simm32)) 
      new_opcode = is_64bit ? TOP_xori64 : TOP_xori32;
    else {
      src1 = Expand_Immediate_Into_Register(src1, is_64bit, ops);
    }

    if( OP_NEED_PAIR(mtype) ){
      Expand_Split_BOP( OPR_BXOR, mtype, dest, src2, src1, ops );
    } else {      
      Build_OP (new_opcode, dest, src2, src1, ops);
    }

  } else if (TN_is_constant(src2)) {
    // switch order of src so immediate is first
    Expand_Binary_Xor (dest, src2, src1, mtype, ops);

  } else {
    switch(mtype) {
    case MTYPE_V16I1: 
      Build_OP(TOP_xor128v8, dest, src1, src2, ops); break;
    case MTYPE_V16I2: 
      Build_OP(TOP_xor128v16, dest, src1, src2, ops); break;
    case MTYPE_V16I4: 
      Build_OP(TOP_xor128v32, dest, src1, src2, ops); break;
    case MTYPE_V16I8: 
      Build_OP(TOP_xor128v64, dest, src1, src2, ops); break;
    case MTYPE_V8F4:
      Build_OP(TOP_xorps, dest, src1, src2, ops); break;
    case MTYPE_M8I1:
    case MTYPE_M8I2:
    case MTYPE_M8I4:
    case MTYPE_M8F4:
      Build_OP(TOP_pxor_mmx, dest, src1, src2, ops); break;
    default:
      if( OP_NEED_PAIR(mtype) ){
	Expand_Split_BOP( OPR_BXOR, mtype, dest, src1, src2, ops );
      } else {      
	Build_OP(new_opcode, dest, src1, src2, ops); break;
      }
    }
  }
}

void Expand_Binary_Nor (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert( FALSE, ("UNIMPLEMENTED") );
}

void Expand_Binary_Not (TN *dest, TN *src1, TYPE_ID mtype, OPS *ops)
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TOP new_opcode = is_64bit ? TOP_not64 : TOP_not32;

  if( OP_NEED_PAIR(mtype) ){
    Expand_Split_UOP( OPR_BNOT, mtype, dest, src1, ops );
  } else {
    Build_OP(new_opcode, dest, src1, ops);
  }
}

static void  Expand_Int_Cmp_With_Branch( TOP cmp_opcode, TN* src1, TN* src2,
					 TOP set_opcode, TN* result, OPS* ops )
{
  BB* bb_entry = Cur_BB;
  BB* bb_then  = Gen_And_Append_BB( bb_entry );  // for condition is satisfied
  BB* bb_exit  = Gen_And_Append_BB( bb_then );
  const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );
  TN* tmp_result = result;

  if( TN_is_dedicated(result) ){
    tmp_result = Build_TN_Like( result );
  }

  if( src1 == tmp_result ){
    TN* tmp = Build_TN_Like( src1 );
    Exp_COPY( tmp, src1, ops );
    src1 = tmp;
  }

  if( src2 == tmp_result ){
    TN* tmp = Build_TN_Like( src2 );
    Exp_COPY( tmp, src2, ops );
    src2 = tmp;
  }

  BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_entry)) = NULL;
  WN_label_number(BB_branch_wn(bb_entry)) = bb_exit_label;

  // Build bb_entry
  {
    TN* rflags = Rflags_TN();
    Build_OP( TOP_zero32, tmp_result, ops );
    Build_OP( cmp_opcode, rflags, src1, src2, ops );

    TOP jmp = TOP_UNDEFINED;

    switch( set_opcode ){
    case TOP_setg:  jmp = TOP_jle; break;
    case TOP_setge: jmp = TOP_jl;  break;
    case TOP_seta:  jmp = TOP_jbe; break;
    case TOP_setae: jmp = TOP_jb;  break;
    case TOP_setl:  jmp = TOP_jge; break;
    case TOP_setle: jmp = TOP_jg;  break;
    case TOP_setb:  jmp = TOP_jae; break;
    case TOP_setbe: jmp = TOP_ja;  break;
    case TOP_sete:  jmp = TOP_jne; break;
    case TOP_setne: jmp = TOP_je;  break;
    }

    Build_OP( jmp, rflags, Gen_Label_TN( bb_exit_label, 0 ), ops );

    if( &New_OPs != ops )
      OPS_Append_Ops( &New_OPs, ops );
    Process_New_OPs();
    BB_Append_Ops( bb_entry, &New_OPs );
    OPS_Init( &New_OPs );
    OPS_Init( ops );
  }

  // Build bb_then here.
  {
    OPS* bb_then_ops = &New_OPs;
    Exp_Immediate( tmp_result, Gen_Literal_TN(1,0), FALSE, bb_then_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_then, bb_then_ops );
    OPS_Init( bb_then_ops );
  }

  Cur_BB = bb_exit;

  if( result != tmp_result ){
    Exp_COPY( result, tmp_result, ops );
  }
}


static void Expand_Int_Cmp( TN* dest, TN* src1, TN* src2,
			    TYPE_ID mtype, TOP set_opcode, OPS* ops )
{
  if (TN_has_value( src1 )){
    FmtAssert( !TN_has_value( src2 ), ("src2 has value") );    
    TOP top = TOP_UNDEFINED;
    // Bug:084
    switch( set_opcode ){
    case TOP_sete:
    case TOP_setne:  top = set_opcode;  break;
    case TOP_setb:   top = TOP_seta;    break;
    case TOP_seta:   top = TOP_setb;    break;
    case TOP_setae:  top = TOP_setbe;   break;
    case TOP_setbe:  top = TOP_setae;   break;
    case TOP_setg:   top = TOP_setl;    break;
    case TOP_setl:   top = TOP_setg;    break;
    case TOP_setge:  top = TOP_setle;   break;
    case TOP_setle:  top = TOP_setge;   break;
    }

    Expand_Int_Cmp(dest, src2, src1, mtype, top, ops);
    return;
  }

  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  TOP cmp_opcode = is_64bit ? TOP_cmp64 : TOP_cmp32;
  TN* rflags = Rflags_TN();

  if( TN_has_value( src2 ) ){
    UINT64 val = TN_value( src2 );
    if( TN_value( src2 ) == 0 ){
      cmp_opcode = is_64bit ? TOP_test64 : TOP_test32;
      src2 = src1;
    } else if( ISA_LC_Value_In_Class( val, LC_simm32 ) ){
      cmp_opcode = is_64bit ? TOP_cmpi64 : TOP_cmpi32;
    } else {
      src2 = Expand_Immediate_Into_Register (src2, is_64bit, ops);
    }
  }

  if( OP_NEED_PAIR( mtype ) ){
    Expand_Split_Int_Cmp( cmp_opcode, src1, src2, set_opcode, dest, mtype, ops );

  } else if( !CG_use_setcc ){
    Expand_Int_Cmp_With_Branch( cmp_opcode, src1, src2, set_opcode, dest, ops );

  } else {
    TN* tmp_tn = Build_TN_Of_Mtype(MTYPE_U1);  
    Build_OP( cmp_opcode, rflags, src1, src2, ops );
    Build_OP( set_opcode, tmp_tn, rflags, ops );

    if( Is_Target_32bit() &&
	TN_size(dest) == 8 ){
      Expand_Split_Cvtl( MTYPE_I8, TOP_movzbq, dest, tmp_tn, ops );
    } else {
      Build_OP( TN_size(dest) == 8 ? TOP_movzbq : TOP_movzbl,
		dest, tmp_tn, ops );
    }
  }
}

void Expand_Int_Less (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  if( MTYPE_is_signed(mtype) &&
      TN_has_value( src2 )   &&
      TN_value( src2 ) == 0 ){
    // Replace "cmp" and "setl" with "shrl".
    TN* shift_amt = Gen_Literal_TN( MTYPE_is_size_double(mtype) ? 63 : 31, 4 );
    Expand_Shift( dest, src1, shift_amt, mtype, shift_lright, ops );

  } else {
    Expand_Int_Cmp( dest, src1, src2, mtype,
		    MTYPE_is_signed(mtype) ? TOP_setl : TOP_setb,
		    ops );
  }
}

void Expand_Int_Less_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Expand_Int_Cmp( dest, src1, src2, mtype,
		  MTYPE_is_signed(mtype) ? TOP_setle : TOP_setbe,
		  ops );
}

void Expand_Int_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Expand_Int_Cmp( dest, src1, src2, mtype, TOP_sete, ops );
}

void Expand_Int_Not_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Expand_Int_Cmp( dest, src1, src2, mtype, TOP_setne, ops );
}

void Expand_Int_Greater_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Expand_Int_Cmp( dest, src1, src2, mtype,
		  MTYPE_is_signed(mtype) ? TOP_setge : TOP_setae,
		  ops );
}

void Expand_Int_Greater (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Expand_Int_Cmp( dest, src1, src2, mtype,
		  MTYPE_is_signed(mtype) ? TOP_setg : TOP_seta,
		  ops );
}

static void
Expand_Bool_Comparison (BOOL equals, TN *dest, TN *src1, TN *src2, OPS *ops)
{
  FmtAssert(FALSE,("Unimplemented"));
}

void
Expand_Bool_Equal (TN *dest, TN *src1, TN *src2, OPS *ops)
{
  FmtAssert(FALSE,("Unimplemented"));
}

void
Expand_Bool_Not_Equal (TN *dest, TN *src1, TN *src2, OPS *ops)
{
  FmtAssert(FALSE,("Unimplemented"));
}

void
Expand_Bool_To_Int (TN *dest, TN *src, TYPE_ID rtype, OPS *ops)
{
  FmtAssert(FALSE,("Unimplemented"));
}

typedef enum {
  ROUND_USER,
  ROUND_NEAREST,
  ROUND_CHOP,
  ROUND_NEG_INF,
  ROUND_PLUS_INF
} ROUND_MODE;


static void Expand_SSE3_Long_Double_To_Int( TN* dest, TN* src,
					    TYPE_ID imtype, OPS* ops )
{
  TOP top = TOP_UNDEFINED;
  TY_IDX mem_ty = MTYPE_To_TY( imtype );

  switch( imtype ){
  case MTYPE_I2:  top = TOP_fisttps;   break;
  case MTYPE_I4:  top = TOP_fisttpl;   break;
  case MTYPE_U4:
    mem_ty = MTYPE_To_TY( MTYPE_U8 );
    /* fall thru */
  case MTYPE_U8:
  case MTYPE_I8:  top = TOP_fisttpll;  break;
  default:
    FmtAssert( false, ("Expand_SSE3_Long_Double_To_Int: unknown imtype") );
  }

  ST* mem_loc = Gen_Temp_Symbol( mem_ty, "x87_2_int" );
  Allocate_Temp_To_Memory( mem_loc );
  ST* mem_base_sym = NULL;
  INT64 mem_base_ofst = 0;

  Base_Symbol_And_Offset_For_Addressing(mem_loc, 0, &mem_base_sym,
					&mem_base_ofst);
  TN* mem_base_tn = mem_base_sym == SP_Sym ? SP_TN : FP_TN;
  TN* mem_ofst_tn = Gen_Literal_TN( mem_base_ofst, 4 );

  Build_OP( top, src, mem_base_tn, mem_ofst_tn, ops );

  CGTARG_Load_From_Memory( dest, mem_loc, ops );

  if( Trace_Exp ){
    Print_OPS( ops );
  }

  return;
}


static void
Expand_Long_Double_To_Int(TN* dest, TN* src, TYPE_ID imtype, OPS* ops)
{
  TN* x87_cw_tn = NULL;
  TN* base_tn = NULL;
  TN* base_tn_new = NULL;
  TN* ofst_tn = NULL;
  TN* ofst_tn_new = NULL;

  // If not using SSE3 "fistt", we must emit code to set the rounding mode to
  // truncation before emitting "fist".

  if (!Is_Target_SSE3()) {
    x87_cw_tn = X87_cw_TN();

    // Allocate space to store the x87 control-word.
    const TY_IDX ty = MTYPE_To_TY( MTYPE_U2 );
    ST* st = Gen_Temp_Symbol( ty, "x87_cw" );
    Allocate_Temp_To_Memory( st );

    ST* base_sym = NULL;
    INT64 base_ofst = 0;

    Base_Symbol_And_Offset_For_Addressing( st, 0, &base_sym, &base_ofst );
    FmtAssert( base_sym == SP_Sym || base_sym == FP_Sym,
	       ("Expand_Long_Double_To_Int: base symbol is at stack") );

    base_tn = base_sym == SP_Sym ? SP_TN : FP_TN;
    ofst_tn = Gen_Literal_TN( base_ofst, 4 );

    // store the x87 control-word.
    Build_OP( TOP_fnstcw, x87_cw_tn, base_tn, ofst_tn, ops );
    Set_OP_volatile( OPS_last(ops) );

    // load the value into a 32-bit int register.
    TN* x87_cw = Gen_Register_TN( ISA_REGISTER_CLASS_integer, 4 );
    Exp_Load( MTYPE_U4, TY_mtype(ty), x87_cw, st, 0, ops, 0 );

    // perform an or to mask out that bit.
    TN* new_x87_cw = Build_TN_Like( x87_cw );
    Expand_Binary_Or(new_x87_cw, x87_cw, Gen_Literal_TN(3072,4), MTYPE_U4, ops);

    // store new_x87_cw back to a new memory location.
    ST* st_new = Gen_Temp_Symbol( ty, "x87_cw_new" );
    Allocate_Temp_To_Memory( st_new );
    ST* base_sym_new = NULL;
    INT64 base_ofst_new = 0;

    Base_Symbol_And_Offset_For_Addressing(st_new, 0, &base_sym_new,
					  &base_ofst_new);

    base_tn_new = base_sym_new == SP_Sym ? SP_TN : FP_TN;
    ofst_tn_new = Gen_Literal_TN( base_ofst_new, 4 );

    Exp_Store( TY_mtype(ty), new_x87_cw, st_new, 0, ops, 0 );

    // load the new x87_cw
    Build_OP( TOP_fldcw, x87_cw_tn, base_tn_new, ofst_tn_new, ops );
    Set_OP_volatile( OPS_last(ops) );
  }

  // do the real convertion work here.
  TOP top = TOP_UNDEFINED;
  TY_IDX mem_ty = MTYPE_To_TY( imtype );

  if (Is_Target_SSE3()) {
    switch( imtype ){
      case MTYPE_I2:  top = TOP_fisttps;   break;
      case MTYPE_I4:  top = TOP_fisttpl;   break;
      case MTYPE_U4:
	mem_ty = MTYPE_To_TY( MTYPE_U8 );
	/* fall thru */
      case MTYPE_U8:
      case MTYPE_I8:  top = TOP_fisttpll;  break;
      default:
	FmtAssert( false, ("Expand_Long_Double_To_Int: unknown imtype") );
    }
  } else {	// not SSE3
    switch (imtype) {
      case MTYPE_I2:    top = TOP_fistps;   break;
      case MTYPE_I4:    top = TOP_fistpl;   break;
      case MTYPE_U4:
	/* bug#658
	   We need bigger space for TOP_fistpll.
	 */
	mem_ty = MTYPE_To_TY( MTYPE_U8 );
	/* fall thru */
      case MTYPE_U8:
      case MTYPE_I8:    top = TOP_fistpll;  break;
      default:
	FmtAssert( false, ("Expand_Long_Double_To_Int: unknown imtype") );
    }
  }

  ST* mem_loc = Gen_Temp_Symbol( mem_ty, "x87_2_int" );
  Allocate_Temp_To_Memory( mem_loc );
  ST* mem_base_sym = NULL;
  INT64 mem_base_ofst = 0;

  Base_Symbol_And_Offset_For_Addressing(mem_loc, 0, &mem_base_sym,
					&mem_base_ofst);
  TN* mem_base_tn = mem_base_sym == SP_Sym ? SP_TN : FP_TN;
  TN* mem_ofst_tn = Gen_Literal_TN( mem_base_ofst, 4 );

  Build_OP( top, src, mem_base_tn, mem_ofst_tn, ops );

  // restore the original x87_cw
  if (!Is_Target_SSE3()) {
    Build_OP( TOP_fldcw, x87_cw_tn, base_tn, ofst_tn, ops );
    Set_OP_volatile( OPS_last(ops) );
  }

  // More work to do for a unsigned long -> long double conversion.

  if( imtype == MTYPE_U8 ){
    BB* bb_entry = Cur_BB;
    BB* bb_then = Gen_And_Append_BB( bb_entry );  // for a negative <src>

    BB* bb_exit  = Gen_And_Append_BB( bb_then );
    const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = bb_exit_label;

    TCON tcon = Host_To_Targ_Float_10(MTYPE_F10, 0); // Host_To_Targ_Quad( 0 );
    // TODO verfy this is the correct long double constant for MTYPE_F10
    TCON_u0( tcon ) = 0x0;
    TCON_u1( tcon ) = 0x80000000;
    TCON_u2( tcon ) = 0x403e;
    TCON_u3( tcon ) = 0x0;      

    ST* tcon_sym = New_Const_Sym(Enter_tcon(tcon), Be_Type_Tbl(TCON_ty(tcon)));
    ST* tcon_base_sym = NULL;
    INT64 tcon_base_ofst = 0;

    Allocate_Object(tcon_sym);
    Base_Symbol_And_Offset_For_Addressing( tcon_sym, 0,
					   &tcon_base_sym, &tcon_base_ofst );

    TN* max_value_tn = Build_TN_Like( src );
    Expand_Const( max_value_tn,
		  Gen_Symbol_TN( tcon_base_sym, tcon_base_ofst, TN_RELOC_NONE ),
		  MTYPE_F10, ops );

    // Build bb_entry
    {
      Exp_OP3v( OPC_TRUEBR,
		NULL,
		Gen_Label_TN( bb_exit_label, 0 ),
		src,
		max_value_tn,
		V_BR_QLT,
		ops );

      if( &New_OPs != ops )
	OPS_Append_Ops( &New_OPs, ops );
      Process_New_OPs();
      BB_Append_Ops( bb_entry, &New_OPs );
      OPS_Init( &New_OPs );
      OPS_Init( ops );
    }

    // Build bb_then here.
    {
      OPS* bb_then_ops = &New_OPs;
      TN* tmp = Build_TN_Like( src );

      Build_OP( TOP_fsub, tmp, src, max_value_tn, bb_then_ops );

      // load the new x87_cw
      if (!Is_Target_SSE3()) {
	Build_OP( TOP_fldcw, x87_cw_tn, base_tn_new, ofst_tn_new, bb_then_ops );
	Set_OP_volatile( OPS_last(bb_then_ops) );
      }

      // convert again.
      Build_OP( TOP_fistpll, tmp, mem_base_tn, mem_ofst_tn, bb_then_ops );

      // restore the original x87_cw
      if (!Is_Target_SSE3()) {
	Build_OP( TOP_fldcw, x87_cw_tn, base_tn, ofst_tn, bb_then_ops );
	Set_OP_volatile( OPS_last(bb_then_ops) );
      }

      TN* tmp_dest = Build_TN_Like( dest );
      CGTARG_Load_From_Memory( tmp_dest, mem_loc, bb_then_ops );
      
      Expand_Binary_Xor( tmp_dest, tmp_dest,
			 Gen_Literal_TN( 0x8000000000000000ULL, 8 ),
			 MTYPE_U8, bb_then_ops );

      /* ebo should get rid of this extra store; otherwise, we need to insert
	 a bb_else_bb.
      */
      CGTARG_Store_To_Memory( tmp_dest, mem_loc, bb_then_ops );

      total_bb_insts = 0;
      Last_Processed_OP = NULL;
      Process_New_OPs();
      BB_Append_Ops( bb_then, bb_then_ops );
      OPS_Init( bb_then_ops );
    }

    Cur_BB = bb_exit;
  }

  // OSP 495
  if ( imtype == MTYPE_U4 && TY_mtype(mem_ty) == MTYPE_U8 ) {
    // in this case, we only need to load lower 4 bytes
    Exp_Load(MTYPE_U4, MTYPE_U4, dest, mem_loc, 0, ops, 0);
  }
  else {
    CGTARG_Load_From_Memory( dest, mem_loc, ops );
  }

  if( Trace_Exp ){
    Print_OPS( ops );
  }
}


static void Expand_Float_To_Long_m32( TN* dest,
				      TN* src,
				      TYPE_ID imtype,
				      TYPE_ID fmtype,
				      OPS* ops )
{
  FmtAssert( TN_register_class(src) == ISA_REGISTER_CLASS_float,
	     ("Expand_Float_To_Long_m32: source is not a xmm register") );

  TN* x87_src = Build_TN_Of_Mtype( MTYPE_F10 );
  Expand_Float_To_Float( x87_src, src, MTYPE_F10, fmtype, ops );
  Expand_Long_Double_To_Int( dest, x87_src, imtype, ops );  
}


static TN* Generate_Cmp_Ctrl_TN( OPERATOR compare )
{
  int imm8 = 0;

  switch( compare ){
  case OPR_EQ: imm8 = 0; break;
  case OPR_LT: imm8 = 1; break;
  case OPR_LE: imm8 = 2; break;
  case OPR_NE: imm8 = 4; break;
  case OPR_GE: imm8 = 5; break;
  case OPR_GT: imm8 = 6; break;
  default:
    FmtAssert( FALSE, ("Unknown opcode") );
  }

  return Gen_Literal_TN( imm8, 4 );
}


static void Expand_Unsigned_Float_To_Int_m32( TN* result,
					      TN* src,
					      TYPE_ID fmtype,
					      OPS* ops )
{
  TN* dest = result;

  if( TN_is_dedicated(result) ){
    dest = Build_TN_Like(result);
  }

  const BOOL is_double = MTYPE_is_size_double(fmtype);
  TN* fp_const = Build_TN_Like( src );

  Build_OP( TOP_ldsd_n32, fp_const,
	    Gen_Const_Symbol_TN( 0, 0x80000000, fmtype ),
	    ops );

  TN* fp_tmp_tn = Build_TN_Like( src );
  TN* ctrl = Generate_Cmp_Ctrl_TN( OPR_LE );
  Build_OP( is_double ? TOP_cmpsd : TOP_cmpss,
	    fp_tmp_tn, fp_const, src, ctrl, ops );
  Build_OP( is_double ? TOP_andpd : TOP_andps, fp_tmp_tn,
	    fp_tmp_tn, fp_const, ops );
  Build_OP( is_double ? TOP_subsd : TOP_subss, fp_tmp_tn,
	    src, fp_tmp_tn, ops );

  TN* int_tmp_tn = Build_TN_Of_Mtype( MTYPE_U4 );
  Exp_Immediate( int_tmp_tn, Gen_Literal_TN(0,4), ops );

  TN* sign_mask = Build_TN_Of_Mtype( MTYPE_U4 );
  Exp_Immediate( sign_mask, Gen_Literal_TN(0x80000000,4), ops );

  TN* rflags = Rflags_TN();
  Build_OP( is_double ? TOP_comisd : TOP_comiss,
	    rflags, fp_const, src, ops );
  Expand_Cmov( TOP_cmovbe, int_tmp_tn, sign_mask, rflags, ops );

  Build_OP( is_double ? TOP_cvttsd2si : TOP_cvttss2si, dest, fp_tmp_tn, ops );
  Build_OP( TOP_add32, dest, dest, int_tmp_tn, ops );

  if( result != dest ){
    Exp_COPY( result, dest, ops );
  }
}


static void
Expand_Float_To_Int (ROUND_MODE rm, TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  TOP top = TOP_UNDEFINED;

  if( MTYPE_is_F10( fmtype ) ||
      !Is_Target_SSE2() ){
    Expand_Long_Double_To_Int( dest, src, imtype, ops );
    return;

  } else if( MTYPE_bit_size(imtype) == 64 &&
	     Is_Target_32bit() ){
    Expand_Float_To_Long_m32( dest, src, imtype, fmtype, ops );
    return;

  } else if( fmtype == MTYPE_F4 ){ 
    if( MTYPE_bit_size(imtype) == 64 ){
      if( MTYPE_is_signed(imtype) )
	top = TOP_cvttss2siq;
      else {
	/* For "float" to "unsigned long long" conversion, operation
	   cvttss2siq will lose some accuracy.  (bug#2867)
	*/
	Expand_Float_To_Long_m32( dest, src, imtype, fmtype, ops );
	return;
      }	

    } else if( MTYPE_bit_size(imtype) == 32 ){
      if( Is_Target_32bit() &&
	  MTYPE_is_unsigned(imtype) ){
	Expand_Unsigned_Float_To_Int_m32( dest, src, fmtype, ops );
	return;
      }

      top = MTYPE_is_signed(imtype) ? TOP_cvttss2si : TOP_cvttss2siq;
    }

  } else if (fmtype == MTYPE_F8) {
    if( MTYPE_bit_size(imtype) == 64 ){
      if( MTYPE_is_signed(imtype) )
	top = TOP_cvttsd2siq;
      else {
	/* For "double" to "unsigned long long" conversion, operation
	   cvttsd2siq will lose some accuracy.  (bug#2867)
	*/
	Expand_Float_To_Long_m32( dest, src, imtype, fmtype, ops );
	return;
      }

    } else if( MTYPE_bit_size(imtype) == 32 ){
      if( Is_Target_32bit() &&
	  MTYPE_is_unsigned(imtype) ){
	Expand_Unsigned_Float_To_Int_m32( dest, src, fmtype, ops );
	return;
      }

      top = MTYPE_is_signed(imtype) ? TOP_cvttsd2si : TOP_cvttsd2siq;
    }
  }

  else if ( fmtype == MTYPE_V16F4 ) {
    if (imtype == MTYPE_V16I4)
      top = TOP_cvttps2dq;
    else if (imtype == MTYPE_V16I8)
      top = TOP_movdq;	// bug 12731
    else
      FmtAssert(FALSE, ("Expand_Float_To_Int: NYI"));
  }

  else if ( fmtype == MTYPE_V16F8 ) {
    // Workaround for bug 3082, not supposed to generate correct code
    top = TOP_cvttpd2dq;
  }
  else if ( fmtype == MTYPE_V32F8 ) {
    // Workaround for bug 3082, not supposed to generate correct code
    top = TOP_vcvttpd2dqy;
  }

  FmtAssert( top != TOP_UNDEFINED,
	     ("Expand_Float_To_Int: undefined opcode") );

  Build_OP( top, dest, src, ops );  
}

void
Expand_Float_To_Int_Cvt (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  Expand_Float_To_Int (ROUND_USER, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Round (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  Expand_Float_To_Int (ROUND_NEAREST, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Trunc (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  Expand_Float_To_Int (ROUND_CHOP, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Tas (TN *dest, TN *src, TYPE_ID imtype, OPS *ops)
{
  Is_True( Is_Target_32bit(), ("Expand_Float_To_Int_Tas should not be invoked under -m64") );
  // Allocate space to store the floating point value
  const TY_IDX ty = MTYPE_To_TY( imtype );
  ST* st = Gen_Temp_Symbol( ty, "float_2_int" );
  Allocate_Temp_To_Memory( st );

  ST* base_sym = NULL;
  INT64 base_ofst = 0;

  Base_Symbol_And_Offset_For_Addressing( st, 0, &base_sym, &base_ofst );
  FmtAssert( base_sym == SP_Sym || base_sym == FP_Sym,
	     ("Expand_Float_To_Int_Tas: base symbol is on stack") );

  TN* base_tn = base_sym == SP_Sym ? SP_TN : FP_TN;
  TN* ofst_tn = Gen_Literal_TN( base_ofst, 4 );

  if (MTYPE_byte_size(imtype) == 4) {
    // store the float value to memory
    Build_OP(Is_Target_SSE2() ? TOP_stss : TOP_fstps, src, base_tn, ofst_tn, ops );
    // load the value into an int register.
    Build_OP(TOP_ld32, dest, base_tn, ofst_tn, ops );
  }
  else {
    // store the float value to memory
    Build_OP(Is_Target_SSE2() ? TOP_stsd : TOP_fstpl, src, base_tn, ofst_tn, ops );
    // load the value into an int register.
    Expand_Load(OPCODE_make_op(OPR_LDID, imtype, imtype), dest, base_tn, ofst_tn, ops);
  }
} 

void
Expand_Int_To_Float_Tas (TN *dest, TN *src, TYPE_ID fmtype, OPS *ops)
{
  Is_True( Is_Target_32bit(), ("Expand_Int_To_Float_Tas should not be invoked under -m64") );
  // Allocate space to store the integer point value
  const TY_IDX ty = MTYPE_To_TY( fmtype );
  ST* st = Gen_Temp_Symbol( ty, "int_2_float" );
  Allocate_Temp_To_Memory( st );

  ST* base_sym = NULL;
  INT64 base_ofst = 0;

  Base_Symbol_And_Offset_For_Addressing( st, 0, &base_sym, &base_ofst );
  FmtAssert( base_sym == SP_Sym || base_sym == FP_Sym,
	     ("Expand_Float_To_Int_Tas: base symbol is on stack") );

  TN* base_tn = base_sym == SP_Sym ? SP_TN : FP_TN;
  TN* ofst_tn = Gen_Literal_TN( base_ofst, 4 );

  if (MTYPE_byte_size(fmtype) == 4) {
    // store the int value to memory
    Build_OP(TOP_store32, src, base_tn, ofst_tn, ops );
    // load the value into a float register.
    Build_OP(Is_Target_SSE2() ? TOP_ldss : TOP_flds, dest, base_tn, ofst_tn, ops );
  }
  else {
    // store the int value to memory
    Expand_Store(MTYPE_U8, src, base_tn, ofst_tn, 0, ops );
    // load the value into a float register.
    Build_OP(Is_Target_SSE2() ? TOP_ldsd : TOP_fldl, dest, base_tn, ofst_tn, ops );
  }
} 

void
Expand_Int_To_Vect_Tas (TN *dest, TN *src, TYPE_ID vectype, OPS *ops)
{
  FmtAssert(MTYPE_byte_size(vectype) == 8,
  	    ("Expand_Int_To_Vect_Tas: 16-byte vector type not handled"));
  FmtAssert(TN_register_class(src) == ISA_REGISTER_CLASS_integer,
  	    ("Expand_Int_To_Vect_Tas: source operand not integer"));

  // Allocate space to store the integer value
  const TY_IDX ty = MTYPE_To_TY( vectype );
  ST* st = Gen_Temp_Symbol( ty, "int_2_vect" );
  Allocate_Temp_To_Memory( st );

  ST* base_sym = NULL;
  INT64 base_ofst = 0;

  Base_Symbol_And_Offset_For_Addressing( st, 0, &base_sym, &base_ofst );
  FmtAssert( base_sym == SP_Sym || base_sym == FP_Sym,
	     ("Expand_Float_To_Int_Tas: base symbol is on stack") );

  TN* base_tn = base_sym == SP_Sym ? SP_TN : FP_TN;
  TN* ofst_tn = Gen_Literal_TN( base_ofst, 4 );

  // store the int value to memory
  Expand_Store(MTYPE_U8, src, base_tn, ofst_tn, 0, ops);

  // load the value into the vector register
  Expand_Load(OPCODE_make_op(OPR_LDID,vectype,vectype), dest, base_tn, ofst_tn,
  	      ops);
}

static void Expand_non_SSE2_Float_Floor( TN* dest, TN* src, OPS* ops )
{
  // Allocate space to store the x87 control-word.
  const TY_IDX ty = MTYPE_To_TY( MTYPE_U2 );
  TN* x87_cw_tn = X87_cw_TN();
  ST* st = Gen_Temp_Symbol( ty, "x87_cw" );
  Allocate_Temp_To_Memory( st );

  ST* base_sym = NULL;
  INT64 base_ofst = 0;

  Base_Symbol_And_Offset_For_Addressing( st, 0, &base_sym, &base_ofst );
  TN* base_tn = base_sym == SP_Sym ? SP_TN : FP_TN;
  TN* ofst_tn = Gen_Literal_TN( base_ofst, 4 );

  // store the x87 control-word.
  Build_OP( TOP_fnstcw, x87_cw_tn, base_tn, ofst_tn, ops );
  Set_OP_volatile( OPS_last(ops) );

  // load the value into a 32-bit int register.
  TN* x87_cw = Gen_Register_TN( ISA_REGISTER_CLASS_integer, 4 );
  Exp_Load( MTYPE_U4, TY_mtype(ty), x87_cw, st, 0, ops, 0 );

  // perform an AND and an OR to mask out that bit.
  TN* new_x87_cw = Build_TN_Like( x87_cw );
  Expand_Binary_And( new_x87_cw, x87_cw, Gen_Literal_TN(-3073,4), MTYPE_U4, ops );
  Expand_Binary_Or( new_x87_cw, new_x87_cw, Gen_Literal_TN(1024,4), MTYPE_U4, ops );

  // store new_x87_cw back to a new memory location.
  ST* st_new = Gen_Temp_Symbol( ty, "x87_cw_new" );
  Allocate_Temp_To_Memory( st_new );
  ST* base_sym_new = NULL;
  INT64 base_ofst_new = 0;

  Base_Symbol_And_Offset_For_Addressing( st_new, 0, &base_sym_new, &base_ofst_new );

  TN* base_tn_new = base_sym_new == SP_Sym ? SP_TN : FP_TN;
  TN* ofst_tn_new = Gen_Literal_TN( base_ofst_new, 4 );

  Exp_Store( TY_mtype(ty), new_x87_cw, st_new, 0, ops, 0 );

  // load the new x87_cw
  Build_OP( TOP_fldcw, x87_cw_tn, base_tn_new, ofst_tn_new, ops );
  Set_OP_volatile( OPS_last(ops) );

  // do the real convertion work here.
  Build_OP( TOP_frndint, dest, src, ops );

  // load the original x87_cw
  Build_OP( TOP_fldcw, x87_cw_tn, base_tn, ofst_tn, ops );
  Set_OP_volatile( OPS_last(ops) );  
}


void Expand_Float_To_Float_Floorl( TN* dest, TN* src,
				   TYPE_ID rtype, TYPE_ID desc, OPS* ops )
{
  Expand_non_SSE2_Float_Floor( dest, src, ops );
  return;
}


void Expand_Float_To_Float_Floorf( TN* dest, TN* src,
				   TYPE_ID rtype, TYPE_ID desc, OPS* ops )
{
  FmtAssert( rtype == MTYPE_F4,
	     ("Expand_Float_To_Float_Floorf: rtype is not float") );
  FmtAssert( rtype == desc,
	     ("Expand_Float_To_Float_Floorf: rtype and desc are different") );

  if( dest == src ){
    TN* tmp = Build_TN_Like( src );
    Expand_Copy( tmp, src, desc, ops );
    src = tmp;
  }

  if( !Is_Target_SSE2() ){
    Expand_non_SSE2_Float_Floor( dest, src, ops );
    return;
  }

  /* First, generate all the necessary values. */

  TN* sign_mask = Build_TN_Like( dest );
  Expand_Const( sign_mask, Gen_Const_Symbol_TN( 0x80000000, 0.0, MTYPE_I4 ),
		rtype, ops );

  TN* mi6_val = Build_TN_Like( dest );
  Expand_Const( mi6_val, Gen_Const_Symbol_TN( 0x4b000000, 0.0, MTYPE_I4 ),
		rtype, ops );

  TN* one_point_zero = Build_TN_Like( dest );
  Expand_Const( one_point_zero, Gen_Const_Symbol_TN( 0x3f800000, 0.0, MTYPE_I4 ),
		rtype, ops );

  /* Execute the floor algorithm. */

  TN* sign_tn = Build_TN_Like( dest );
  TN* mi6_tn = Build_TN_Like( dest );
  TN* tmp1 = Build_TN_Like( dest );
  TN* result1 = Build_TN_Like( dest );
  TN* diff_tn = Build_TN_Like( dest );
  TN* ones_or_zeros = Build_TN_Like( dest );
  TN* fraction_tn = Build_TN_Like( dest );

  Build_OP( TOP_andps, sign_tn, sign_mask, src,     ops );
  Build_OP( TOP_orps,  mi6_tn,  mi6_val,   sign_tn, ops );
  Build_OP( TOP_addss, tmp1,    src,       mi6_tn,  ops );
  Build_OP( TOP_subss, result1, tmp1,      mi6_tn,  ops );
  Build_OP( TOP_subss, diff_tn, result1,   src,     ops );
  TN* ctrl = Generate_Cmp_Ctrl_TN( OPR_GT );
  Build_OP( TOP_cmpss, ones_or_zeros, diff_tn, sign_mask, ctrl, ops );
  Build_OP( TOP_andps, fraction_tn, ones_or_zeros, one_point_zero, ops );
  Build_OP( TOP_subss, dest,    result1,   fraction_tn, ops );
}


void Expand_Float_To_Float_Floor( TN* dest, TN* src,
				  TYPE_ID rtype, TYPE_ID desc, OPS* ops )
{
  FmtAssert( rtype == MTYPE_F8,
	     ("Expand_Float_To_Float_Floor: rtype is not double") );	     
  FmtAssert( rtype == desc,
	     ("Expand_Float_To_Float_Floor: rtype and desc are different") );

  if( dest == src ){
    TN* tmp = Build_TN_Like( src );
    Expand_Copy( tmp, src, desc, ops );
    src = tmp;
  }

  if( !Is_Target_SSE2() ){
    Expand_non_SSE2_Float_Floor( dest, src, ops );
    return;
  }

  /* First, generate all the necessary values. */

  TN* sign_mask = Build_TN_Like( dest );
  Expand_Const( sign_mask,
		Gen_Const_Symbol_TN( 0x8000000000000000ULL, 0.0, MTYPE_I8 ),
		rtype, ops );

  TN* mi6_val = Build_TN_Like( dest );
  Expand_Const( mi6_val,
		Gen_Const_Symbol_TN( 0x4330000000000000ULL, 0.0, MTYPE_I8 ),
		rtype, ops );

  TN* one_point_zero = Build_TN_Like( dest );
  Expand_Const( one_point_zero,
		Gen_Const_Symbol_TN( 0x3ff0000000000000ULL, 0.0, MTYPE_I8 ),
		rtype, ops );

  /* Execute the floor algorithm. */

  TN* sign_tn = Build_TN_Like( dest );
  TN* mi6_tn = Build_TN_Like( dest );
  TN* xor_tn = Build_TN_Like( dest );
  TN* lt_tn = Build_TN_Like( dest );
  TN* and_tn = Build_TN_Like( dest );
  TN* tmp1 = Build_TN_Like( dest );
  TN* result1 = Build_TN_Like( dest );
  TN* diff_tn = Build_TN_Like( dest );
  TN* ones_or_zeros = Build_TN_Like( dest );
  TN* fraction_tn = Build_TN_Like( dest );

  Build_OP( TOP_andpd, sign_tn, sign_mask, src,     ops );
  Build_OP( TOP_xorpd, xor_tn,  src, sign_tn, ops );
  TN* ctrl_lt = Generate_Cmp_Ctrl_TN( OPR_LT );
  Build_OP( TOP_cmpsd, lt_tn, xor_tn, mi6_val, ctrl_lt, ops );
  Build_OP( TOP_andpd, and_tn, lt_tn, mi6_val, ops );
  Build_OP( TOP_orpd, mi6_tn, and_tn, sign_tn, ops );
  Build_OP( TOP_addsd, tmp1,    src,       mi6_tn,  ops );
  Build_OP( TOP_subsd, result1, tmp1,      mi6_tn,  ops );
  Build_OP( TOP_subsd, diff_tn, result1,   src,     ops );
  TN* ctrl = Generate_Cmp_Ctrl_TN( OPR_GT );
  Build_OP( TOP_cmpsd, ones_or_zeros, diff_tn, sign_mask, ctrl, ops );
  Build_OP( TOP_andpd, fraction_tn, ones_or_zeros, one_point_zero, ops );
  Build_OP( TOP_subsd, dest,    result1,   fraction_tn, ops );
}


void
Expand_Float_To_Int_Floor (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  TN* dest1 = Build_TN_Like( dest );
  const BOOL is_double = MTYPE_is_size_double(fmtype);
  TN* rflags = Rflags_TN();

  Expand_Float_To_Int( ROUND_NEG_INF, dest, src, imtype, fmtype, ops );
  Expand_Sub( dest1, dest, Gen_Literal_TN( 1, 4 ), imtype, ops );

  TN* src1  = Build_TN_Like( src );
  if( Is_Target_SSE2() )
    Build_OP( is_double ? TOP_xorpd : TOP_xorps, src1, src, src, ops );
  else
    Build_OP( TOP_fldz, src1, ops );

  // Compare <src> with 0.0
  Build_OP( ( MTYPE_is_F10( imtype ) || !Is_Target_SSE2() )
	    ? TOP_fucomi : ( is_double ? TOP_comisd : TOP_comiss ),
	    rflags, src, src1, ops );
  
  Expand_Cmov( TOP_cmova, dest1, dest, rflags, ops );

  Expand_Int_To_Float( src1, dest, imtype, fmtype, ops );

  // Compare <src> with itself at the integer side.
  Build_OP( ( MTYPE_is_F10( imtype ) || !Is_Target_SSE2() )
	    ? TOP_fucomi : ( is_double ? TOP_comisd : TOP_comiss ),
	    rflags, src, src1, ops );

  Expand_Cmov( TOP_cmovne, dest, dest1, rflags, ops );
}


void
Expand_Float_To_Int_Ceil (TN *result, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  TN* dest = result;

  if( TN_is_dedicated(result) ){
    dest = Build_TN_Like( result );
  }

  TN* dest1 = Build_TN_Like( dest );
  const BOOL is_double = MTYPE_is_size_double(fmtype);
  TN* rflags = Rflags_TN();

  Expand_Float_To_Int( ROUND_NEG_INF, dest, src, imtype, fmtype, ops );
  Expand_Add( dest1, dest, Gen_Literal_TN( 1, 4 ), imtype, ops );

  TN* src1  = Build_TN_Like( src );
  if( Is_Target_SSE2() )
    Build_OP( is_double ? TOP_xorpd : TOP_xorps, src1, src, src, ops );
  else
    Build_OP( TOP_fldz, src1, ops );

  // Compare <src> with 0.0
  Build_OP( ( MTYPE_is_F10( imtype ) || !Is_Target_SSE2() )
	    ? TOP_fucomi : ( is_double ? TOP_comisd : TOP_comiss ),
	    rflags, src, src1, ops );
  
  Expand_Cmov( TOP_cmovbe, dest1, dest, rflags, ops );

  Expand_Int_To_Float( src1, dest, imtype, fmtype, ops );

  // Compare <src> with itself at the integer side.
  Build_OP( ( MTYPE_is_F10( imtype ) || !Is_Target_SSE2() )
	    ? TOP_fucomi : ( is_double ? TOP_comisd : TOP_comiss ),
	    rflags, src, src1, ops );

  Expand_Cmov( TOP_cmovne, dest, dest1, rflags, ops );

  if( dest != result )
    Exp_COPY( result, dest, ops );
}


void
Expand_Float_To_Float (TN *dest, TN *src, TYPE_ID rtype, TYPE_ID desc, OPS *ops)
{
  if( Is_Target_SSE2()        &&
      !MTYPE_is_F10( rtype ) &&
      !MTYPE_is_F10( desc ) ){
    if (!MTYPE_is_vector(rtype)){
#ifdef KEY //bug 14346: fp-fp scalar conversion for barcelona is special
     if(Is_Target_Barcelona() || Is_Target_Orochi())
      Build_OP( (rtype == MTYPE_F8) ? TOP_cvtps2pd : TOP_cvtpd2ps,
		dest, src, ops );
     else
#endif
      Build_OP( (rtype == MTYPE_F8) ? TOP_cvtss2sd : TOP_cvtsd2ss,
                dest, src, ops );
    }
    else {
      if (Is_Target_Orochi() && Is_Target_AVX()) {
        Build_OP( (rtype == MTYPE_V16F8) ? TOP_cvtps2pd : 
                  (TN_size(src) == 32) ? TOP_vcvtpd2psy : TOP_cvtpd2ps,
	  	  dest, src, ops );
      } else
        Build_OP( (rtype == MTYPE_V16F8) ? TOP_cvtps2pd : TOP_cvtpd2ps,
		  dest, src, ops );
    }
    return;
  }

  const TY_IDX ty = MTYPE_To_TY( !MTYPE_is_F10(rtype) ? rtype : desc );
  ST* st = Gen_Temp_Symbol( ty, "x87_cvt" );
  Allocate_Temp_To_Memory( st ); 

  ST* base_sym = NULL;
  INT64 base_ofst = 0;

  Base_Symbol_And_Offset_For_Addressing( st, 0, &base_sym, &base_ofst );
  FmtAssert( base_sym == SP_Sym || base_sym == FP_Sym,
	     ("Expand_Float_To_Float: base symbol is not at stack") );

  TN* base_tn = base_sym == SP_Sym ? SP_TN : FP_TN;
  TN* ofst_tn = Gen_Literal_TN( base_ofst, 4 );

  if( MTYPE_is_F10( desc ) ){
    // long double -> float/double
    CGTARG_Store_To_Memory( src, st, ops );
    CGTARG_Load_From_Memory( dest, st, ops );
    
  } else if( MTYPE_is_F10( rtype ) ){
    // float/double -> long double
    CGTARG_Store_To_Memory( src, st, ops );
    CGTARG_Load_From_Memory( dest, st, ops );

  } else {
    // long double -> long double
    FmtAssert( TN_register_class(dest) == ISA_REGISTER_CLASS_x87,
	       ("Expand_Float_To_Float: dest is not x87 register") );
    FmtAssert( TN_register_class(src)  == ISA_REGISTER_CLASS_x87,
	       ("Expand_Float_To_Float: source is not x87 register") );

    Build_OP( rtype == MTYPE_F8 ? TOP_fstpl : TOP_fstps,
	      src, base_tn, ofst_tn, ops );

    Build_OP( rtype == MTYPE_F8 ? TOP_fldl : TOP_flds,
	      dest, base_tn, ofst_tn, ops );
  }
}


static void Expand_Unsigned_Int_To_Float_m32( TN* dest,
					      TN* src,
					      TYPE_ID fmtype,
					      OPS* ops )
{
  BB* bb_entry = Cur_BB;
  BB* bb_then  = Gen_And_Append_BB( bb_entry );  // for case MSB == 1
  TN* tmp_dest = Build_TN_Like( dest );
  const BOOL is_double = MTYPE_is_size_double(fmtype);

  BB* bb_exit  = Gen_And_Append_BB( bb_then );
  const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

  BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_entry)) = NULL;
  WN_label_number(BB_branch_wn(bb_entry)) = bb_exit_label;

  // Build bb_entry
  {
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      TN *xzero = Build_TN_Like(dest);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( is_double ? TOP_cvtsi2sd : TOP_cvtsi2ss, 
                tmp_dest, xzero, src, ops );
    } else {
      Build_OP( is_double ? TOP_cvtsi2sd : TOP_cvtsi2ss, 
                tmp_dest, src, ops );
    }
    Exp_OP3v( OPC_TRUEBR,
	      NULL,
	      Gen_Label_TN( bb_exit_label, 0 ),
	      src,
	      Gen_Literal_TN(0,4),
	      V_BR_I4GE,
	      ops );

    if( &New_OPs != ops )
      OPS_Append_Ops( &New_OPs, ops );
    Process_New_OPs();
    BB_Append_Ops( bb_entry, &New_OPs );
    OPS_Init( &New_OPs );
    OPS_Init( ops );
  }

  // Build bb_then here.
  {
    OPS* bb_then_ops = &New_OPs;

    TCON tcon = Host_To_Targ_Float( fmtype, 0x100000000LL );
    ST* sym = New_Const_Sym( Enter_tcon (tcon), Be_Type_Tbl( TCON_ty(tcon) ) );
    Allocate_Object( sym );
    ST* base_sym = NULL;
    INT64 base_ofst = 0;
    TN* tmp_c = Build_TN_Of_Mtype( fmtype );

    Base_Symbol_And_Offset_For_Addressing( sym, 0, &base_sym, &base_ofst );
    Build_OP( is_double ? TOP_ldsd_n32 : TOP_ldss_n32, tmp_c,
	      Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_NONE), bb_then_ops );
    Expand_Flop( is_double ? OPC_F8ADD : OPC_F4ADD,
		 tmp_dest, tmp_dest, tmp_c, NULL, bb_then_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_then, bb_then_ops );
    OPS_Init( bb_then_ops );
  }

  Cur_BB = bb_exit;
  if( Is_Target_Barcelona() || Is_Target_EM64T() ||
      Is_Target_Wolfdale()  || Is_Target_Core()  ||
      Is_Target_Orochi() ){
    Build_OP( TOP_movaps, dest, tmp_dest, ops );
  } else {
    Build_OP( TOP_movdq, dest, tmp_dest, ops );
  }
}


static void Expand_Unsigned_Long_To_Float( TN* dest, TN* src, TYPE_ID mtype, OPS* ops )
{
  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  BB* bb_entry = Cur_BB;
  BB* bb_then    = Gen_And_Append_BB( bb_entry );  // for case MSB == 0
  BB* bb_else  = Gen_And_Append_BB( bb_then );     // for case MSB == 1
  const LABEL_IDX bb_else_label = Gen_Label_For_BB( bb_else );

  /* We need to insert a copy at the exit bb, since <dest> could be a
     dedicated reg. for return value, and it must be at a exit bb;
     otherwise, ebo will eliminate many of the useful ops.
  */
  TN* tmp_dest = dest;

  if( TN_is_dedicated(dest) ){
    tmp_dest = Build_TN_Like( dest );
  }

  //BB* bb_exit  = Start_New_Basic_Block();
  BB* bb_exit  = Gen_And_Append_BB( bb_else );
  const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

  BB_branch_wn(bb_then) = WN_Create(OPC_GOTO,0);
  WN_label_number(BB_branch_wn(bb_then)) = bb_exit_label;

  BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_entry)) = NULL;
  WN_label_number(BB_branch_wn(bb_entry)) = bb_else_label;

  // Build bb_entry
  {
    Exp_OP3v( OPC_TRUEBR,
	      NULL,
	      Gen_Label_TN( bb_else_label, 0 ),
	      src,
	      Gen_Literal_TN(0,4),
	      V_BR_I8LT,
	      ops );

    if( &New_OPs != ops )
      OPS_Append_Ops( &New_OPs, ops );
    Process_New_OPs();
    BB_Append_Ops( bb_entry, &New_OPs );
    OPS_Init( &New_OPs );
    OPS_Init( ops );
  }

  // Build bb_then here.
  {
    OPS* bb_then_ops = &New_OPs;
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      TN *xzero = Build_TN_Like(dest);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( is_64bit ? TOP_cvtsi2sdq : TOP_cvtsi2ssq, 
                tmp_dest, xzero, src, bb_then_ops );
    } else {
      Build_OP( is_64bit ? TOP_cvtsi2sdq : TOP_cvtsi2ssq, 
                tmp_dest, src, bb_then_ops );
    }
    Build_OP( TOP_jmp, Gen_Label_TN( bb_exit_label, 0 ), bb_then_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_then, bb_then_ops );
    OPS_Init( bb_then_ops );
  }

  // Build bb_else here.
  {
    OPS* bb_else_ops = &New_OPs;
    TN* tmp1 = Build_TN_Like(src);
    TN* tmp2 = Build_TN_Like(src);
    TN* tmp3 = Build_TN_Like(src);
    TN* dest1 = Build_TN_Like(tmp_dest);

    Build_OP( TOP_shri64, tmp1, src, Gen_Literal_TN( 1, 4 ), bb_else_ops );
    Build_OP( TOP_andi32, tmp2, src, Gen_Literal_TN( 1, 4 ), bb_else_ops );
    Build_OP( TOP_or64, tmp3, tmp1, tmp2, bb_else_ops );
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      TN *xzero = Build_TN_Like(dest);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( is_64bit ? TOP_cvtsi2sdq : TOP_cvtsi2ssq,
  	        dest1, xzero, tmp3, bb_else_ops );
    } else {
      Build_OP( is_64bit ? TOP_cvtsi2sdq : TOP_cvtsi2ssq,
  	        dest1, tmp3, bb_else_ops );
    }
    Build_OP( is_64bit ? TOP_addsd : TOP_addss,
	      tmp_dest, dest1, dest1, bb_else_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_else, bb_else_ops );
    OPS_Init( bb_else_ops );
  }

  if( tmp_dest != dest ){
    if( Is_Target_Barcelona() || Is_Target_EM64T() ||
        Is_Target_Wolfdale()  || Is_Target_Core()  ||
        Is_Target_Orochi() ){
      Build_OP( TOP_movaps, dest, tmp_dest, ops );
    } else {
      Build_OP( TOP_movdq, dest, tmp_dest, ops );
    }
  }

  Cur_BB = bb_exit;
}


static void Expand_Int_To_Long_Double( TN* result, TN* src,
				       TYPE_ID imtype, TYPE_ID fmtype,
				       OPS* ops )
{
  TN* dest = result;

  if( TN_is_dedicated(result) ){
    dest = Build_TN_Like( result );
  }

  if( imtype == MTYPE_U4 ){
    imtype = MTYPE_I8;
    if (Is_Target_32bit()) {
      // Create the high 32 bits and rely on the fact that Expand_Split_Store
      // will automatically store the high part of the pair.  Bug 5688.
      TN *src_h = Create_TN_Pair(src, MTYPE_I8);
      Build_OP(TOP_ldc32, src_h, Gen_Literal_TN(0, 4), ops);
    } else {
      TN* tmp = Build_TN_Of_Mtype( imtype );
      Build_OP( TOP_mov32, tmp, src, ops );
      src = tmp;
    }
  }

  TY_IDX ty = MTYPE_To_TY( imtype );
  ST* st = Gen_Temp_Symbol( ty, "x87_cvt" );
  Allocate_Temp_To_Memory( st ); 

  ST* base_sym = NULL;
  INT64 base_ofst = 0;

  Base_Symbol_And_Offset_For_Addressing( st, 0, &base_sym, &base_ofst );
  FmtAssert( base_sym == SP_Sym || base_sym == FP_Sym,
	     ("Expand_Int_To_Long_Double: base symbol is not at stack") );

  TN* base_tn = base_sym == SP_Sym ? SP_TN : FP_TN;
  TN* ofst_tn = Gen_Literal_TN( base_ofst, 4 );

  CGTARG_Store_To_Memory( src, st, ops );

  TOP top = TOP_UNDEFINED;

  switch( imtype ){
  case MTYPE_I2:   top = TOP_filds;   break;
  case MTYPE_I4:   top = TOP_fildl;   break;
  case MTYPE_U8:
  case MTYPE_I8:   top = TOP_fildll;  break;
  default:
    FmtAssert( false, ("Expand_Int_To_Long_Double: Unknown imtype") );
  }

  Build_OP( top, dest, base_tn, ofst_tn, ops );

  /* More work to do with unsigned long long -> long double.
   */
  if( imtype == MTYPE_U8 ){
    BB* bb_entry = Cur_BB;
    BB* bb_then = Gen_And_Append_BB( bb_entry );  // for src < 0

    BB* bb_exit  = Gen_And_Append_BB( bb_then );
    const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = bb_exit_label;

    // Build bb_entry
    {
      if( &New_OPs != ops )      
	OPS_Append_Ops( &New_OPs, ops );
      Process_New_OPs();
      BB_Append_Ops( bb_entry, &New_OPs );
      OPS_Init( &New_OPs );
      OPS_Init( ops );

      ops = &New_OPs;

      TN* src_hi = OP_NEED_PAIR(imtype) ? Get_TN_Pair(src) : src;
      const VARIANT variant = TN_size(src_hi) > 4 ? V_BR_I8GE : V_BR_I4GE;

      Exp_OP3v( OPC_TRUEBR,
		NULL,
		Gen_Label_TN( bb_exit_label, 0 ),
		src_hi,
		Gen_Literal_TN(0,4),
		variant,
		ops );

      if( bb_entry != Cur_BB ){
	FmtAssert( OPS_length( ops ) == 0,
		   ("Expand_Int_To_Long_Double: ops is not empty") );
	bb_entry = Cur_BB;
      }

      total_bb_insts = 0;
      Last_Processed_OP = NULL;
      Process_New_OPs();
      BB_Append_Ops( bb_entry, ops );
      OPS_Init( ops );
    }

    // Build bb_then here.
    {
      OPS* bb_then_ops = &New_OPs;
      TCON tcon = Host_To_Targ_Float_10(MTYPE_F10, 0); // Host_To_Targ_Quad( 0 );
      // TODO verfy this is the correct long double constant for MTYPE_F10
      TCON_u0( tcon ) = 0x0;
      TCON_u1( tcon ) = 0x80000000;
      TCON_u2( tcon ) = 0x403f;
      TCON_u3( tcon ) = 0x0;      

      ST* sym = New_Const_Sym( Enter_tcon(tcon),  Be_Type_Tbl( TCON_ty(tcon) ) );

      ST* base_sym = NULL;
      INT64 base_ofst = 0;

      Allocate_Object(sym);
      Base_Symbol_And_Offset_For_Addressing( sym, 0, &base_sym, &base_ofst );

      TN* max_value_tn = Build_TN_Like( dest );
      Expand_Const( max_value_tn, Gen_Symbol_TN( base_sym, base_ofst, TN_RELOC_NONE ),
		    MTYPE_F10, bb_then_ops );

      Build_OP( TOP_fadd, dest, dest, max_value_tn, bb_then_ops );

      total_bb_insts = 0;
      Last_Processed_OP = NULL;
      Process_New_OPs();
      BB_Append_Ops( bb_then, bb_then_ops );
      OPS_Init( bb_then_ops );
    }

    Cur_BB = bb_exit;
  }

  if( result != dest ){
    Exp_COPY( result, dest, ops );
  }
}


static void Expand_Long_To_Float_m32( TN* dest,
				      TN* src,
				      TYPE_ID imtype,
				      TYPE_ID fmtype,
				      OPS* ops )
{
  FmtAssert( TN_register_class(dest) == ISA_REGISTER_CLASS_float,
	     ("Expand_Long_To_Float_m32: dest is not float register") );
  /* TODO:
     Get rid of useless loads and stores. (A simple long long to float
     conversion can expose the awkwardness.)
  */

  TN* x87_dest = Build_TN_Of_Mtype( MTYPE_F10 );
  Expand_Int_To_Long_Double( x87_dest, src, imtype, MTYPE_F10, ops );
  // Now, convert <x87_dest> to <dest>.
  Expand_Float_To_Float( dest, x87_dest, fmtype, MTYPE_F10, ops );
}


void
Expand_Int_To_Float (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
  TOP top = TOP_UNDEFINED;

  if( MTYPE_is_F10( fmtype ) ){
    Expand_Int_To_Long_Double( dest, src, imtype, fmtype, ops );
    return;
  }

  /* Without the support of sse2 registers, the conversion from
     U8 to float need to be handled well to preserve the accuracy.
     (bug#2600)
  */
  if( !Is_Target_SSE2() ){
    TN* x87_dest =
      ( imtype == MTYPE_U8 ) ? Build_TN_Of_Mtype( MTYPE_F10 ) : dest;
    Expand_Int_To_Long_Double( x87_dest, src, imtype, MTYPE_F10, ops );

    // Now, convert <x87_dest> to <dest>, if necessary.
    if( x87_dest != dest )
      Expand_Float_To_Float( dest, x87_dest, fmtype, MTYPE_F10, ops );

    return;
  }

  if( fmtype == MTYPE_F4 ){ 
    if( MTYPE_bit_size(imtype) == 64 ){

      if( MTYPE_is_signed(imtype) ){
	if( Is_Target_32bit() ){
	  Expand_Long_To_Float_m32( dest, src, imtype, fmtype, ops );
	  return;
	}

        if (Is_Target_Orochi() && Is_Target_AVX()) {
          TN *xzero = Build_TN_Like(dest);
          Build_OP( TOP_xzero128v32, xzero, ops );
          Build_OP( TOP_cvtsi2ssq, dest, xzero, src, ops );
          return;
        } else {
	  top = TOP_cvtsi2ssq;
        }
      } else {
	if( Is_Target_32bit() ){
	  Expand_Long_To_Float_m32( dest, src, imtype, fmtype, ops );
	} else {
	  Expand_Unsigned_Long_To_Float( dest, src, fmtype, ops );
	}

	return;
      }

    } else if( MTYPE_bit_size(imtype) == 32 ){
      if( MTYPE_is_signed(imtype) )
#ifdef KEY //cvt signed integer to single precision scalar
       if(Is_Target_Barcelona() || Is_Target_Orochi()){
         TN *tmp_dest = Build_TN_Like(dest);
         Build_OP(TOP_movg2x, tmp_dest, src, ops);
         src = tmp_dest;
	 top = TOP_cvtdq2ps;
       }
       else
#endif 
        top = TOP_cvtsi2ss;

      else {
	if( Is_Target_32bit() ){
	  Expand_Unsigned_Int_To_Float_m32( dest, src, fmtype, ops );
	  return;
	}

        TN *tmp = Build_TN_Of_Mtype(MTYPE_I8);      
	Build_OP(TOP_mov32, tmp, src, ops);
	src = tmp;
        if (Is_Target_Orochi() && Is_Target_AVX()) {
          TN *xzero = Build_TN_Like(dest);
          Build_OP( TOP_xzero128v32, xzero, ops );
          Build_OP( TOP_cvtsi2ssq, dest, xzero, src, ops );
          return;
        } else {
	  top = TOP_cvtsi2ssq;
        }
      }
    }

  } else if (fmtype == MTYPE_F8) {
    if( MTYPE_bit_size(imtype) == 64 ){
      if( MTYPE_is_signed(imtype) ){
	if( Is_Target_32bit() ){
	  Expand_Long_To_Float_m32( dest, src, imtype, fmtype, ops );
	  return;
	}

	top = TOP_cvtsi2sdq;
        if (Is_Target_Orochi() && Is_Target_AVX()) {
          TN *xzero = Build_TN_Like(dest);
          Build_OP( TOP_xzero128v32, xzero, ops );
          Build_OP( top, dest, xzero, src, ops );
        } else {
          Build_OP( top, dest, src, ops );
        }
        return;
      } else {
	if( Is_Target_32bit() ){
	  Expand_Long_To_Float_m32( dest, src, imtype, fmtype, ops );
	} else {
	  Expand_Unsigned_Long_To_Float( dest, src, fmtype, ops );
	}

	return;
      }

    } else {
      FmtAssert( MTYPE_bit_size(imtype) == 32,
		 ("Expand_Int_To_Float: size of imtype is not 32-bit-long") );

      if( MTYPE_is_signed(imtype) ){
#ifdef KEY
        if(Is_Target_Barcelona() || Is_Target_Orochi()){
         TN *tmp_dest = Build_TN_Like(dest);
         Build_OP(TOP_movg2x, tmp_dest, src, ops); 
         src = tmp_dest;
         top = TOP_cvtdq2pd;
        }else
#endif
        {
          top = TOP_cvtsi2sd;
          if (Is_Target_Orochi() && Is_Target_AVX()) {
            TN *xzero = Build_TN_Like(dest);
            Build_OP( TOP_xzero128v32, xzero, ops );
            Build_OP( top, dest, xzero, src, ops );
          } else {
            Build_OP( top, dest, src, ops );
          }
          return;
        }
      } else {
	if( Is_Target_32bit() ){
	  Expand_Unsigned_Int_To_Float_m32( dest, src, fmtype, ops );
	  return;
	}

	TN *tmp = Build_TN_Of_Mtype(MTYPE_I8);
	Build_OP( TOP_mov32, tmp, src, ops );
	src = tmp;
        top = TOP_cvtsi2sdq;
        if (Is_Target_Orochi() && Is_Target_AVX()) {
          TN *xzero = Build_TN_Like(dest);
          Build_OP( TOP_xzero128v32, xzero, ops );
          Build_OP( top, dest, xzero, src, ops );
        } else {
          Build_OP( top, dest, src, ops );
        }
        return;
      }
    }

  } else if (fmtype == MTYPE_V16F8) {
    // imtype == V16I8: bug 3082 workaround
    if (imtype == MTYPE_V16I4 || imtype == MTYPE_V8I4 || 
        imtype == MTYPE_V16I8 || imtype == MTYPE_V8I8)
      top = TOP_cvtdq2pd;
    else if (imtype == MTYPE_U8 || imtype == MTYPE_I8) {
      top = TOP_cvtsi2sdq; // bug 3082 workaround, others should not reach here
      if (Is_Target_Orochi() && Is_Target_AVX()) {
        TN *xzero = Build_TN_Like(dest);
        Build_OP( TOP_xzero128v32, xzero, ops );
        Build_OP( top, dest, xzero, src, ops );
      } else {
        Build_OP( top, dest, src, ops );
      }
      return;
    }
  } else if (fmtype == MTYPE_V16F4) {
    if (imtype == MTYPE_V16I4)
      top = TOP_cvtdq2ps;    
    else if (imtype == MTYPE_U8 || imtype == MTYPE_I8) {
      top = TOP_cvtsi2sdq; // bug 3082 workaround, others should not reach here
      if (Is_Target_Orochi() && Is_Target_AVX()) {
        TN *xzero = Build_TN_Like(dest);
        Build_OP( TOP_xzero128v32, xzero, ops );
        Build_OP( top, dest, xzero, src, ops );
      } else {
        Build_OP( top, dest, src, ops );
      }
      return;
    }
  }

  FmtAssert( top != TOP_UNDEFINED, ("Expand_Int_To_Float: Undefined opcode") );

  Build_OP( top, dest, src, ops );
}


static BOOL
Optimize_Select (
	TOP cmp,
  	TN *cond1, 
  	TN *cond2, 
  	TN *dest, 
  	TN *dest2,
  	TN *src1, 
  	TN *src2, 
	BOOL is_float,
	OPS *ops)
{
  ErrMsg( EC_Unimplemented, "Optimize_Select: NYI" );
  return FALSE;
}


static void Expand_Compare_And_Select ( TOP cmp,
					TN *cond1, 
					TN *cond2, 
					TN *dest, 
					TN *opposite_dest, 
					TN *true_tn, 
					TN *false_tn, 
					BOOL is_float,
					OPS *ops)
{
  ErrMsg( EC_Unimplemented, "Expand_Compare_And_Select: NYI" );
}

void
Expand_Select (
  TN *dest_tn, 
  TN *cond_tn, 
  TN *true_tn, 
  TN *false_tn, 
  TYPE_ID mtype, 
  BOOL float_cond,
  OPS *ops)
{
  if (mtype == MTYPE_V16I1) {
    Expand_Select_To_Blend(mtype, dest_tn, cond_tn, true_tn, false_tn, ops);
    return;
  }

  Is_True( TN_register_class(cond_tn) == ISA_REGISTER_CLASS_integer,
	   ("Handle this case in Expand_Select") );
  const BOOL non_sse2_fp = MTYPE_is_F10(mtype) ||
    ( MTYPE_is_float(mtype) && !Is_Target_SSE2() );

  if( dest_tn == false_tn ){
    TN* tmp = Build_TN_Like( false_tn );
    Expand_Copy( tmp, false_tn, mtype, ops );
    false_tn = tmp;
  }

  if( dest_tn == true_tn ){
    TN* tmp = Build_TN_Like( true_tn );
    Expand_Copy( tmp, true_tn, mtype, ops );
    true_tn = tmp;
  }

  if( dest_tn == cond_tn ){ // bug 13180
    TN* tmp = Build_TN_Like( cond_tn );
    Expand_Copy( tmp, cond_tn, mtype, ops );
    cond_tn = tmp;
  }

  if (non_sse2_fp ||
      (TN_register_class(dest_tn) == ISA_REGISTER_CLASS_integer)) {

    // First, assign <true_tn> to <dest_tn>.
    Expand_Copy( dest_tn, true_tn, mtype, ops );
    
    // Next, check whether <cond_tn> is 0 to set rflags
    TN *p = Rflags_TN();
    Build_OP( (TN_size(cond_tn) == 8) ? TOP_test64:TOP_test32,
	      p, cond_tn, cond_tn, ops );
    
    // Now, use the rflags to conditionally move if <cond_tn> is 0
    TN *dest_tn_hi = NULL;
    TN *false_tn_hi = NULL;
    if (OP_NEED_PAIR(mtype)) {
      TN *true_tn_hi = Get_TN_Pair(true_tn);
      dest_tn_hi = Get_TN_Pair(dest_tn);
      false_tn_hi = Get_TN_Pair(false_tn);
    }
    Expand_Cmov(non_sse2_fp ? TOP_fcmove : TOP_cmove, dest_tn, false_tn, p,
		ops, dest_tn_hi, false_tn_hi);
  } else if (TN_register_class(dest_tn) == ISA_REGISTER_CLASS_float) {
    // SSE2 floats, intergral vectors
    TN *tmp3 = Build_TN_Like(dest_tn);
    TN *tmp4 = Build_TN_Like(dest_tn);
    TN *tmp5 = Build_TN_Like(dest_tn);

    // Need to generate a constant of size dest_tn
    BOOL is_double = (TN_size(dest_tn) == 8); 
    TYPE_ID imtype = is_double ? MTYPE_I8 :MTYPE_I4;
    TYPE_ID fmtype = is_double ? MTYPE_F8 :MTYPE_F4;

    TN* tmp1 = Build_TN_Of_Mtype( imtype );
    TN* tmp2 = Build_TN_Of_Mtype( imtype );

    Expand_Shift (tmp1, cond_tn, Gen_Literal_TN(is_double?63:31, 4), 
		  is_double?MTYPE_I8:MTYPE_I4, shift_left, ops);
    Expand_Shift (tmp2, tmp1, Gen_Literal_TN(is_double?63:31, 4), 
		  is_double?MTYPE_I8:MTYPE_I4, shift_aright, ops);
    /* Don't use Expand_Int_To_Float, which will convert the all 1's
       value to fp format. */
    //TY_IDX ty = Spill_Int_Type;
    TY_IDX ty = MTYPE_To_TY( imtype );
    ST* st = Gen_Temp_Symbol( ty, "movd" );
    Allocate_Temp_To_Memory( st );

    //TYPE_ID imtype = TY_mtype(ST_type(st));
    Exp_Store( imtype, tmp2, st, 0, ops, 0 );
    Exp_Load( fmtype, fmtype, tmp3, st, 0, ops, 0 );
    Build_OP( is_double ? TOP_andpd : TOP_andps, tmp4, true_tn, tmp3, ops );
    Build_OP( is_double ? TOP_andnpd : TOP_andnps, tmp5, tmp3, false_tn, ops );
    Build_OP( is_double ? TOP_orpd : TOP_orps, dest_tn, tmp5, tmp4, ops );

  } else {
    FmtAssert(FALSE, ("Handle this case"));
  }
}
  
//Vector type SELECT are expanded to *blend* operation.
//For now we only handle vector type V16I1.
void
Expand_Select_To_Blend (TYPE_ID mtype, TN* result, TN* op0, TN* op1, TN* op2, OPS *ops)
{
  FmtAssert(mtype == MTYPE_V16I1, ("Non-vector type passed to Expand_Select_To_Blend"));
  TN* xmm0;
  if( Trace_Exp ) {
    fprintf(TFile, "expand %s: ", mtype == MTYPE_V16I1? OPCODE_name(OPC_V16I1V16I1SELECT): "***Unsupported opcode***");
    if (result) Print_TN(result,FALSE);
    fprintf(TFile, " :- ");
    if (op0) Print_TN(op0,FALSE);
    fprintf(TFile, " ");
    if (op1) Print_TN(op1,FALSE);
    fprintf(TFile, " ");
    if (op2) Print_TN(op2,FALSE);
    fprintf(TFile, " ");
    fprintf(TFile, "\n");
  }

  if (!Is_Target_AVX()) {
    //pblendvb (non-AVX) uses the 'xmm0' register as an implicit argument containing the mask.
    //To build a TN dedicated to reg xmm0, pass value "1" to Build_Dedicated_TN
    //instead of "XMM0(enum value of 17)". This avoids a bug in out of bound access
    //of the array 'v16_ded_tns' which is size 17. Need to file this bug.
    xmm0 = Build_Dedicated_TN(ISA_REGISTER_CLASS_float,1,16);
    Exp_COPY(xmm0, op2, ops);
    Set_TN_is_global_reg(xmm0);
  }
  switch(mtype) {
  case MTYPE_V16I1:
    if (Is_Target_Orochi() && Is_Target_AVX())
      Build_OP(TOP_blendv128v8, result, op0, op1, op2, ops);
    else
      Build_OP(TOP_blendv128v8, result, op0, xmm0, op1, ops);
    break;
  default:
    FmtAssert(FALSE,
              ("Expand_Select_To_Blend: Unsupported mtype (%d)", mtype));
  }

  if (Trace_Exp) {
    //Print_OPS appears to be printing extra characters at end of string  "into  ||| ..."
    fprintf(TFile, " into "); Print_OPS (ops);
  }
}

void
Expand_Min (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  Is_True( !TN_has_value( src1 ), ("Expand_Min: src1 has value") );

  if( OP_NEED_PAIR(mtype) ){
    if( dest == src2 ){
      TN* tmp = Build_TN_Like( src2 );
      Expand_Copy( tmp, src2, mtype, ops );
      src2 = tmp;
    }

    Expand_Split_Select( dest,
			 OPR_LE,
			 TN_has_value(src2) ? TOP_cmpi64 : TOP_cmp64,
			 src1, src2, mtype,  /* cmp kids       */
			 src1, src2, mtype,  /* true and false */
			 ops );

  } else if( MTYPE_is_float(mtype) &&
	     Is_Target_SSE2()      &&
	     !MTYPE_is_F10( mtype ) ){
    switch(mtype) {
    case MTYPE_V32F4:
    case MTYPE_V16F4:
      Build_OP( TOP_fmin128v32, dest, src1, src2, ops );      
      break;
    case MTYPE_V32F8:
    case MTYPE_V16F8:
      Build_OP( TOP_fmin128v64, dest, src1, src2, ops );      
      break;
    default:
      Build_OP( mtype == MTYPE_F8 ? TOP_minsd : TOP_minss, 
		dest, src1, src2, ops );
      break;
    }

  } else if ( MTYPE_is_vector(mtype) ) { // Integer MIN      
      TN *tmp1;
      TN *tmp2;
      TN *tmp3;
      TN *tmp4;
      TN *tmp5;
      TN *tmp6;
      if (!Is_Target_SSE41()) {
        tmp1 = Build_TN_Like(src1); 
        tmp2 = Build_TN_Like(src1); 
        tmp3 = Build_TN_Like(src1); 
        tmp4 = Build_TN_Like(src1); 
        tmp5 = Build_TN_Like(src1); 
        tmp6 = Build_TN_Like(src1); 
        Build_OP( TOP_movdq, tmp1, src1, ops );
        Build_OP( TOP_movdq, tmp2, tmp1, ops );
        Build_OP( TOP_movdq, tmp3, src2, ops );
      }
      switch(mtype){
        case MTYPE_V16I1: //added for bug 5695, refer to 8676
            if (Is_Target_SSE41()) {
              Build_OP( TOP_mins128v8, dest, src1, src2, ops );
            } else {
              Build_OP( TOP_xor128v8, tmp4, tmp1, tmp3, ops );
              Build_OP( TOP_cmpgt128v8, tmp5, tmp3, tmp2, ops );
              Build_OP( TOP_and128v8, tmp6, tmp5, tmp4, ops );
              Build_OP( TOP_xor128v8, dest, tmp6, tmp3, ops );
            }
            break;
        case MTYPE_V16I2: //added for bug 5695, refer to 8676
            if (Is_Target_SSE41()) {
              Build_OP( TOP_mins128v16, dest, src1, src2, ops );
            } else {
              Build_OP( TOP_xor128v16, tmp4, tmp1, tmp3, ops );
              Build_OP( TOP_cmpgt128v16, tmp5, tmp3, tmp2, ops );
              Build_OP( TOP_and128v16, tmp6, tmp5, tmp4, ops );
              Build_OP( TOP_xor128v16, dest, tmp6, tmp3, ops );
            }
            break;
        case MTYPE_V16I4: /// original one
            if (Is_Target_SSE41()) {
              Build_OP( TOP_mins128v32, dest, src1, src2, ops );
            } else {
              Build_OP( TOP_xor128v32, tmp4, tmp1, tmp3, ops );
              Build_OP( TOP_cmpgt128v32, tmp5, tmp3, tmp2, ops );
              Build_OP( TOP_and128v32, tmp6, tmp5, tmp4, ops );
              Build_OP( TOP_xor128v32, dest, tmp6, tmp3, ops );
            }
            break;
         default:
            FmtAssert(FALSE, ("NYI"));
            break;
      }//end switch
  } else {
    const BOOL is_64bit = MTYPE_is_size_double(mtype);

    TOP cmp_opcode =
      MTYPE_is_float(mtype) ? TOP_fucomi : ( is_64bit ? TOP_cmp64 : TOP_cmp32 );
    TOP mov_opcode =
      MTYPE_is_float(mtype) ? TOP_fmov : ( is_64bit ? TOP_mov64 : TOP_mov32 );
    const TOP cmov_opcode =
      MTYPE_is_float(mtype) ? TOP_fcmovbe : ( MTYPE_is_signed(mtype) ? TOP_cmovle : TOP_cmovbe );
    TN* rflags = Rflags_TN();

    if( TN_has_value( src2 ) ){
      cmp_opcode = is_64bit ? TOP_cmpi64 : TOP_cmpi32;
      mov_opcode = is_64bit ? TOP_ldc64 : TOP_ldc32;
    }

    if( dest == src1 ){
      TN* tmp = src1;
      src1 = src2;
      src2 = tmp;
    }

    // OSP, laijx
    // If is return reg and target does not support cmov,
    // The MIN will be expand to multiple BBs
    // Store the return of MIN(which is also the return of the func)
    //      to a temp reg, then copy the temp reg to return reg
    TN* orig_dest = NULL;
    if( ! Target_Support_Cmov() &&
         TN_is_dedicated(dest) &&
         REGISTER_SET_MemberP(
           REGISTER_CLASS_function_value(TN_register_class(dest)),
             TN_register(dest) ) ) {
      orig_dest = dest;
      dest = Dup_TN_Even_If_Dedicated(orig_dest);
    }

    if( dest != src2 )
      Build_OP( mov_opcode, dest, src2, ops );

    Build_OP( cmp_opcode, rflags, src1, src2, ops );
    Expand_Cmov( cmov_opcode, dest, src1, rflags, ops );

    // OSP, laijx
    if( orig_dest != NULL ) {
      Expand_Copy( orig_dest, dest, mtype, ops );
    }
  
  }
}

void
Expand_Max (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{     
  Is_True( !TN_has_value( src1 ), ("Expand_Max: src1 has value") );

  if( OP_NEED_PAIR(mtype) ){
    if( dest == src2 ){
      TN* tmp = Build_TN_Like( src2 );
      Expand_Copy( tmp, src2, mtype, ops );
      src2 = tmp;
    }

    Expand_Split_Select( dest,
			 OPR_GE,
			 TN_has_value(src2) ? TOP_cmpi64 : TOP_cmp64,
			 src1, src2, mtype,  /* cmp kids       */
			 src1, src2, mtype,  /* true and false */
			 ops );

  } else if( MTYPE_is_float(mtype) &&
	     Is_Target_SSE2()      &&
	     !MTYPE_is_F10(mtype) ){
    switch(mtype) {
    case MTYPE_V32F4:
    case MTYPE_V16F4:
      Build_OP( TOP_fmax128v32, dest, src1, src2, ops );      
      break;
    case MTYPE_V32F8:
    case MTYPE_V16F8:
      Build_OP( TOP_fmax128v64, dest, src1, src2, ops );      
      break;
    default:
      Build_OP( mtype == MTYPE_F8 ? TOP_maxsd : TOP_maxss, 
		dest, src1, src2, ops );
      break;
    }

  } else if ( MTYPE_is_vector(mtype) ) { // Integer MAX
      TN *tmp1;
      TN *tmp2;
      TN *tmp3;
      TN *tmp4;
      TN *tmp5;
      TN *tmp6;
      if (!Is_Target_SSE41()) {
        tmp1 = Build_TN_Like(src1); 
        tmp2 = Build_TN_Like(src1); 
        tmp3 = Build_TN_Like(src1); 
        tmp4 = Build_TN_Like(src1); 
        tmp5 = Build_TN_Like(src1); 
        tmp6 = Build_TN_Like(src1); 
        Build_OP( TOP_movdq, tmp1, src1, ops );
        Build_OP( TOP_movdq, tmp2, tmp1, ops );
        Build_OP( TOP_movdq, tmp3, src2, ops );
      }
      switch(mtype){
        case MTYPE_V16I1: /// added for bug 8676
            if (Is_Target_SSE41()) {
              Build_OP( TOP_maxs128v8, dest, src1, src2, ops );
            } else {
              Build_OP( TOP_xor128v8, tmp4, tmp1, tmp3, ops );
              Build_OP( TOP_cmpgt128v8, tmp5, tmp2, tmp3, ops );
              Build_OP( TOP_and128v8, tmp6, tmp5, tmp4, ops );
              Build_OP( TOP_xor128v8, dest, tmp6, tmp3, ops );
            }
            break;
        case MTYPE_V16I2: /// added for bug 8676
            if (Is_Target_SSE41()) {
              Build_OP( TOP_maxs128v16, dest, src1, src2, ops );
            } else {
	      Build_OP( TOP_xor128v16, tmp4, tmp1, tmp3, ops );
              Build_OP( TOP_cmpgt128v16, tmp5, tmp2, tmp3, ops );
              Build_OP( TOP_and128v16, tmp6, tmp5, tmp4, ops );
              Build_OP( TOP_xor128v16, dest, tmp6, tmp3, ops );
            }
            break;
        case MTYPE_V16I4: /// original one
            if (Is_Target_SSE41()) {
              Build_OP( TOP_maxs128v32, dest, src1, src2, ops );
            } else {
	      Build_OP( TOP_xor128v32, tmp4, tmp1, tmp3, ops );
	      Build_OP( TOP_cmpgt128v32, tmp5, tmp2, tmp3, ops );
	      Build_OP( TOP_and128v32, tmp6, tmp5, tmp4, ops );
	      Build_OP( TOP_xor128v32, dest, tmp6, tmp3, ops );
            }
            break;
         default:
            FmtAssert(FALSE, ("NYI"));
            break;
      }//end switch
  } else {
    const BOOL is_64bit = MTYPE_is_size_double(mtype);
    TN* rflags = Rflags_TN();

    TOP cmp_opcode = MTYPE_is_float(mtype)
      ? TOP_fucomi : ( is_64bit ? TOP_cmp64 : TOP_cmp32 );
    const TOP cmov_opcode = MTYPE_is_float(mtype)
      ? TOP_fcmovnb : ( MTYPE_is_signed(mtype) ? TOP_cmovge : TOP_cmovae );

    if( TN_has_value( src2 ) ){
      cmp_opcode = is_64bit ? TOP_cmpi64 : TOP_cmpi32;
    }

    if( dest == src1 ){
      TN* tmp = src1;
      src1 = src2;
      src2 = tmp;
    }

    // OSP, laijx
    // If is return reg and target does not support cmov,
    // The MAX will be expand to multiple BBs
    // Store the return of MAX(which is also the return of the func)
    //      to a temp reg, then copy the temp reg to return reg
    TN* orig_dest = NULL;
    if( ! Target_Support_Cmov() &&
         TN_is_dedicated(dest) &&
         REGISTER_SET_MemberP(
           REGISTER_CLASS_function_value(TN_register_class(dest)),
             TN_register(dest) ) ) {
      orig_dest = dest;
      dest = Dup_TN_Even_If_Dedicated(orig_dest);
    }

    if( dest != src2 ){
      Expand_Copy( dest, src2, mtype, ops );
    }

    Build_OP( cmp_opcode, rflags, src1, src2, ops );
    Expand_Cmov( cmov_opcode, dest, src1, rflags, ops );

    // OSP, laijx
    if( orig_dest != NULL ) {
      Expand_Copy( orig_dest, dest, mtype, ops );
    }
  
  }
}

void
Expand_MinMax (TN *dest_min, TN *dest_max,
	       TN *src1, TN *src2,
	       TYPE_ID mtype, OPS *ops)
{ 
  Is_True( !TN_has_value( src1 ), ("Expand_MinMax: src1 has value") );

  if( dest_min == src1 || dest_max == src1 ){
    TN* tmp = Build_TN_Like( src1 );
    Expand_Copy( tmp, src1, mtype, ops );
    src1 = tmp;
  }

  if( dest_min == src2 || dest_max == src2 ){
    TN* tmp = Build_TN_Like( src2 );
    Expand_Copy( tmp, src2, mtype, ops );
    src2 = tmp;
  }

  if( OP_NEED_PAIR(mtype) ){
    Expand_Min( dest_min, src1, src2, mtype, ops );
    Expand_Max( dest_max, src1, src2, mtype, ops );

  } else if( MTYPE_is_float(mtype) &&
	     Is_Target_SSE2()      &&
	     !MTYPE_is_F10(mtype) ){
    switch(mtype) {
    case MTYPE_V32F4:
    case MTYPE_V16F4:
      Build_OP( TOP_fmin128v32, dest_min, src1, src2, ops );      
      Build_OP( TOP_fmax128v32, dest_max, src1, src2, ops );      
      break;
    case MTYPE_V32F8:
    case MTYPE_V16F8:
      Build_OP( TOP_fmin128v64, dest_min, src1, src2, ops );      
      Build_OP( TOP_fmax128v64, dest_max, src1, src2, ops );      
      break;
    default:
      Build_OP( mtype == MTYPE_F8 ? TOP_minsd : TOP_minss, 
		dest_min, src1, src2, ops );
      Build_OP( mtype == MTYPE_F8 ? TOP_maxsd : TOP_maxss, 
		dest_max, src1, src2, ops );
      break;
    }

  } else if ( MTYPE_is_vector(mtype) ) { // Integer MINMAX
    if (mtype == MTYPE_V16I4) {
      if (Is_Target_SSE41()) {
        Build_OP( TOP_maxs128v32, dest_max, src1, src2, ops );
        Build_OP( TOP_mins128v32, dest_min, src1, src2, ops );
      } else {
        TN *tmp1 = Build_TN_Like(src1); 
        TN *tmp2 = Build_TN_Like(src1); 
        TN *tmp3 = Build_TN_Like(src1); 
        TN *tmp4 = Build_TN_Like(src1); 
        TN *tmp5 = Build_TN_Like(src1); 
        TN *tmp6 = Build_TN_Like(src1); 
        TN *tmp7 = Build_TN_Like(src1); 
      
        Build_OP( TOP_movdq, tmp1, src1, ops );
        Build_OP( TOP_movdq, tmp2, tmp1, ops );
        Build_OP( TOP_movdq, tmp3, tmp1, ops );      
        Build_OP( TOP_movdq, tmp4, src2, ops );
        Build_OP( TOP_cmpgt128v32, tmp5, tmp2, tmp4, ops );
        Build_OP( TOP_xor128v32, tmp6, tmp3, tmp4, ops );
        Build_OP( TOP_and128v32, tmp7, tmp5, tmp6, ops );
        Build_OP( TOP_xor128v32, dest_max, tmp4, tmp7, ops );
        Build_OP( TOP_xor128v32, dest_min, tmp1, tmp7, ops );
      }
    } else
      FmtAssert(FALSE, ("NYI"));

  } else {
    const BOOL is_64bit = MTYPE_is_size_double(mtype);

    TOP cmp_opcode =
      MTYPE_is_float(mtype) ? TOP_fucomi : ( is_64bit ? TOP_cmp64 : TOP_cmp32 );
    TOP mov_opcode =
      MTYPE_is_float(mtype) ? TOP_fmov : ( is_64bit ? TOP_mov64 : TOP_mov32 );
    TN* rflags = Rflags_TN();

    if( TN_has_value( src2 ) ){
      cmp_opcode = is_64bit ? TOP_cmpi64 : TOP_cmpi32;
      mov_opcode = is_64bit ? TOP_ldc64 : TOP_ldc32;
    }

    Build_OP( mov_opcode, dest_min, src2, ops );
    Build_OP( mov_opcode, dest_max, src1, ops );
    Build_OP( cmp_opcode, rflags, src1, src2, ops );

    const TOP cmov_opcode = MTYPE_is_float(mtype)
      ? TOP_fcmovb : ( MTYPE_is_signed(mtype) ? TOP_cmovl : TOP_cmovb );
    Expand_Cmov( cmov_opcode, dest_min, src1, rflags, ops, dest_max, src2 );
  }
}

/* check whether to eval condition before select */
extern BOOL
Check_Select_Expansion (OPCODE compare)
{
  // in order to get optimal code,
  // don't evaluate the condition first,
  // but pass the condition and kids to exp_select,
  // which will do the compare and use the predicate results.
  return FALSE;
}

static void 
Expand_Ordered_Select_Compare ( OPS* ops, TOP cond_move )
{
  FmtAssert(OPS_length(ops) == 3 || OPS_length(ops) == 5,
	    ("Expand_Ordered_Select_Compare: wrong ops length"));

  if ( cond_move == TOP_cmova || cond_move == TOP_cmovae )
    // We are as good before. 
    return;

  if( TOP_is_x87( cond_move ) ){
    DevWarn( "Expand_Ordered_Select_Compare: %s is not supported yet.\n",
            TOP_Name(cond_move) );
    return;
  }

  // Collect the compare and select operands from ops
  TN *cmp_kid1, *cmp_kid2, *true_tn, *false_tn, *result, *tmp;
  TN *result_hi = NULL;
  TN *true_tn_hi = NULL;
  TN *false_tn_hi = NULL;
  OP* init_op = OPS_first ( ops );
  OP* cmp_op  = init_op->next;
  OP* cmov_op = OPS_last ( ops );
  TOP cmp_opcode = OP_code ( cmp_op );
  BOOL need_pair = FALSE;

  if (OPS_length(ops) == 5) {
    need_pair = TRUE;
    cmp_op  = init_op->next->next;
    cmov_op = cmp_op->next;
    cmp_opcode = OP_code ( cmp_op );
  }

  TOP init_opcode = OP_code ( init_op );
  TN* rflags = Rflags_TN();
  TOP new_cond_move = TOP_UNDEFINED;

  false_tn = OP_opnd(init_op, 0);
  true_tn =  OP_opnd(cmov_op, 0);
  cmp_kid1 = OP_opnd(cmp_op, 0);
  cmp_kid2 = OP_opnd(cmp_op, 1);
  result   = OP_result(init_op, 0);
  
  OPS_Remove_All(ops);

  switch (cond_move) {
  case TOP_cmove: 
    {
      new_cond_move = TOP_cmovne; 
      // Interchange
      tmp = true_tn;
      true_tn = false_tn;
      false_tn = tmp;
      break;
    }
  case TOP_cmovne: new_cond_move = TOP_cmovne; break;
  case TOP_cmovb:  new_cond_move = TOP_cmova; break;
  case TOP_cmovbe: new_cond_move = TOP_cmovae; break;
  default:
    FmtAssert( FALSE,
	       ("Expand_Ordered_Select_Compare: unsupported opcode (%s)",
		TOP_Name(cond_move)) );
  }

  if (need_pair) {
    result_hi = Get_TN_Pair( result );
    true_tn_hi = Get_TN_Pair( true_tn );
    false_tn_hi = Get_TN_Pair( false_tn );
  }

  Build_OP ( init_opcode, result, false_tn, ops );
  if (need_pair)
    Build_OP ( init_opcode, result_hi, false_tn_hi, ops );
  Build_OP ( cmp_opcode, rflags, cmp_kid2, cmp_kid1, ops );
  Expand_Cmov(new_cond_move, result, true_tn, rflags, ops, result_hi,
	      true_tn_hi);
  if ( new_cond_move == TOP_cmovne ) {
    Expand_Cmov(TOP_cmovp, result, true_tn, rflags, ops, result_hi, true_tn_hi);
  }    

  return;
}


/*  <result> = <cmp_kid1> <compare> <cmp_kid2> ? <true_tn> : <false_tn>
 */
static void Expand_non_SSE2_Float_Select( TN* dest, VARIANT variant,
					  TN* cmp_kid1, TN* cmp_kid2,
					  TN* true_tn, TN* false_tn, TYPE_ID select_type,
					  OPS* ops )
{
  TN* result = dest;

  if( TN_is_dedicated(dest) ){
    result = Build_TN_Like( dest );
  }

  Expand_Copy( result, true_tn, select_type, ops );

  BB* bb_entry  = Cur_BB;
  BB* bb_then = Gen_And_Append_BB( bb_entry );
  BB* bb_exit = Gen_And_Append_BB( bb_then );

  const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

  BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_entry)) = NULL;
  WN_label_number(BB_branch_wn(bb_entry)) = bb_exit_label;

  // build bb_entry
  {
    Exp_OP3v( OPC_TRUEBR,
	      NULL,
	      Gen_Label_TN( bb_exit_label, 0 ),
	      cmp_kid1,
	      cmp_kid2,
	      variant,
	      ops );

    if( &New_OPs != ops )
      OPS_Append_Ops( &New_OPs, ops );
    Process_New_OPs();
    BB_Append_Ops( bb_entry, &New_OPs );
    OPS_Init( &New_OPs );
    OPS_Init( ops );    
  }

  // Build bb_then here if <cmp_kid1> <compare> <cmp_kid2> is FALSE.
  {
    OPS* bb_then_ops = &New_OPs;
    Expand_Copy( result, false_tn, select_type, bb_then_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_then, bb_then_ops );
    OPS_Init( bb_then_ops );    
  }

  Cur_BB = bb_exit;

  if( result != dest ){
    Expand_Copy( dest, result, select_type, ops );
  }
}

//implemented to handle storing vectorized floating-point comparison
//to a preg for bug 11088
extern void 
Exp_Stid_And_VComp(
        OPCODE stid, TN *result, TN *cmp_kid1, TN *cmp_kid2,
        OPCODE compare, OPS *ops)
{
  OPS new_ops = OPS_EMPTY;
  const TYPE_ID desc = OPCODE_desc(compare);
  const TYPE_ID rtype =OPCODE_rtype(compare);
  const TYPE_ID stid_desc = OPCODE_desc(stid);
  
  FmtAssert(MTYPE_is_integral(rtype) && MTYPE_is_float(desc)
            && MTYPE_is_vector(rtype) && MTYPE_is_vector(desc),
            ("Exp_Stid_And_VComp: comparison type not handled")); 

  FmtAssert( stid_desc==MTYPE_V16I4 || stid_desc==MTYPE_V16I8,
                 ("Exp_Stid_And_VComp: store type not handled"));

  const OPERATOR compare_opr = OPCODE_operator(compare);
  TN* ctrl = Generate_Cmp_Ctrl_TN( compare_opr );  

  Build_OP( (stid_desc == MTYPE_V16I8 ) ? TOP_cmppd : TOP_cmpps,
               result, cmp_kid1, cmp_kid2, ctrl, ops);

}

//implemented to handle select with vectorized ldid as condition
//for bug 11088
extern void
Exp_Select_And_VLdid(
        OPCODE select, TN *result, TN *true_tn, TN *false_tn,
        OPCODE compare, TN *vldid,  OPS *ops)
{
  OPS new_ops = OPS_EMPTY;
  const TYPE_ID select_type = OPCODE_rtype(select);
  FmtAssert( select_type == MTYPE_V16F4 || select_type == MTYPE_V16F8,
                 ("Exp_Select_And_VLdid: select type not handled"));

  BOOL is_rsize_double = (select_type == MTYPE_V16F8) ? TRUE : FALSE;

   if( result == true_tn ){
    TN* tmp = Build_TN_Like( true_tn );
    Expand_Copy( tmp, true_tn, select_type, ops );
    true_tn = tmp;
  }

  if( result == false_tn ){
    TN* tmp = Build_TN_Like( false_tn );
    Expand_Copy( tmp, false_tn, select_type, ops );
    false_tn = tmp;
  }

  if( result == vldid ){
    TN* tmp = Build_TN_Like(vldid);
    Expand_Copy( tmp, vldid, select_type, ops );
    vldid = tmp;
  }
 
  TN* tmp1 = Build_TN_Like(result);
  TN* tmp2 = Build_TN_Like(result);

  Build_OP( is_rsize_double ? TOP_andpd : TOP_andps, tmp1, true_tn, vldid, &new_ops );
  Build_OP( is_rsize_double ? TOP_andnpd : TOP_andnps, tmp2, vldid, false_tn, &new_ops );
  Build_OP( is_rsize_double ? TOP_orpd : TOP_orps, result, tmp2, tmp1, &new_ops );

  if( Trace_Exp ){
     Print_OPS( &new_ops );
  }

  OPS_Append_Ops(ops, &new_ops);
}


extern void 
Exp_Select_And_Condition (
        OPCODE select, TN *result, TN *true_tn, TN *false_tn,
        OPCODE compare, TN *cmp_kid1, TN *cmp_kid2, VARIANT variant, OPS *ops)
{
  if (Trace_Exp) {
    fprintf(TFile, "expand %s: ", OPCODE_name(select));
    if (result) Print_TN(result,FALSE);
    fprintf(TFile, " :- (");
    if (cmp_kid1) Print_TN(cmp_kid1,FALSE);
    fprintf(TFile, " ");
    fprintf(TFile, OPCODE_name(compare));
    fprintf(TFile, " ");
    if (cmp_kid2) Print_TN(cmp_kid2,FALSE);
    fprintf(TFile, ") ? ");
    if (true_tn) Print_TN(true_tn,FALSE);
    fprintf(TFile, " : ");
    if (false_tn) Print_TN(false_tn,FALSE);
    fprintf(TFile, " ");
    if (variant) fprintf(TFile, "(0x%llx)", (INT64)variant);
    fprintf(TFile, "\n");
  }

  OPS new_ops = OPS_EMPTY;
  const TYPE_ID desc = OPCODE_desc(compare);
  const TYPE_ID select_type = OPCODE_rtype(select);
  BOOL is_rsize_double = MTYPE_is_size_double(select_type);
  BOOL is_ssize_double = MTYPE_is_size_double(desc);
  const OPERATOR compare_opr = OPCODE_operator(compare);

  /* Fix bug#1325
     Before expanding, make sure <result> is none of its operands.
   */

  if( result == true_tn ){
    TN* tmp = Build_TN_Like( true_tn );
    Expand_Copy( tmp, true_tn, select_type, ops );
    true_tn = tmp;
  }

  if( result == false_tn ){
    TN* tmp = Build_TN_Like( false_tn );
    Expand_Copy( tmp, false_tn, select_type, ops );
    false_tn = tmp;
  }

  if (result == cmp_kid1) {
    TN* tmp = Build_TN_Like( cmp_kid1 );
    Expand_Copy( tmp, cmp_kid1, desc, ops );
    cmp_kid1 = tmp;
  }

  if (result == cmp_kid2) {
    TN* tmp = Build_TN_Like( cmp_kid2 );
    Expand_Copy( tmp, cmp_kid2, desc, ops );
    cmp_kid2 = tmp;
  }

  if( MTYPE_is_float(select_type) &&
      !Is_Target_SSE2()           &&
      !MTYPE_is_float(desc)       &&
      MTYPE_is_signed(desc) ){
    Expand_non_SSE2_Float_Select( result, variant,
				  cmp_kid1, cmp_kid2,
				  true_tn, false_tn, select_type,
				  &new_ops );

  } else if( MTYPE_is_float(select_type) &&
      Is_Target_SSE2()            &&
      !MTYPE_is_F10(select_type) ){
    /* For case where <result>, <true_tn> and <false_tn> are fp type.

       Paraphrase Section 6.7 of AMD Opteron Optimization Guide:
       In: <result> = <cmp_kid1> <compare> <cmp_kid2> ? <true_tn> : <false_tn>
       Out: <result>

       tmp1 := cmpss  <cmp_kid1>, <cmp_kid2>, ctrl
       tmp2 := andps  <true_tn>, <tmp1>
       tmp3 := andnps <tmp1>, <false_tn>
       <result> := orps <tmp3>, <tmp2>
    */

    /* Notice that for a comparison made between integers, the result for int cmp
       is set to rflags, and there is no conditional mov for fp.
    */
    if( MTYPE_is_integral(desc) ){
      TN* cmp1 = Build_TN_Of_Mtype(select_type);
      TN* cmp2 = Build_TN_Of_Mtype(select_type);

      FmtAssert( !TN_has_value( cmp_kid1 ),
		 ("Exp_Select_And_Condition: cmp kid1 does not have value") );
      Expand_Int_To_Float( cmp1, cmp_kid1, desc, select_type, &new_ops );
      cmp_kid1 = cmp1;

      if( TN_has_value( cmp_kid2 ) ){
	INT64 val = TN_value( cmp_kid2 );
	if( TN_size( cmp_kid2 ) == 4 )
	  val = (INT32)val;

	// Bug 5084 - The following should expand val as an integer and not 
	// a float because we are going to convert this int back to float.
	//TCON tcon = Host_To_Targ_Float( is_rsize_double ? MTYPE_F8 : MTYPE_F4, val );
	TCON tcon = Host_To_Targ( is_rsize_double ? MTYPE_I8 : MTYPE_I4, val );
	ST* sym = New_Const_Sym( Enter_tcon(tcon),  Be_Type_Tbl( TCON_ty(tcon) ) );
	TN* tmp = Build_TN_Of_Mtype(desc);

	ST* base_sym = NULL;
	INT64 base_ofst = 0;

	Allocate_Object(sym);
	Base_Symbol_And_Offset_For_Addressing( sym, 0, &base_sym, &base_ofst );
      
	Expand_Const( tmp, Gen_Symbol_TN( base_sym, base_ofst, TN_RELOC_NONE ),
		      desc, &new_ops );
	cmp_kid2 = tmp;
      }

      Expand_Int_To_Float( cmp2, cmp_kid2, desc, select_type, &new_ops );
      cmp_kid2 = cmp2;

      // The comparison between integers is changed to a comparison between
      // float/doubles, depending on the select's result type.  Update
      // is_ssize_double to reflect the new comparison's operand type.
      is_ssize_double = MTYPE_is_size_double(select_type);
    }

    TN* tmp1 = Build_TN_Like( cmp_kid1 );
    TN* tmp2 = Build_TN_Like( result );
    TN* tmp3 = Build_TN_Like( result );
    TN* ctrl = Generate_Cmp_Ctrl_TN( compare_opr );
    BOOL zero_tn = FALSE;

    // Bug 2297 - optimize the case where true_tn is 0.0
    // TODO_1.2 : false_tn could also be 0.0 in which case we have to reverse 
    // the compare and set a flag and fall thru. This is not not relevant to
    // this bug.
    if ( TN_is_rematerializable( true_tn ) ) {
      WN* home = TN_home(true_tn);
      if (WN_operator(home) == OPR_CONST) {
	ST* st = WN_st(home);
	TCON tcon = STC_val(st);
	TYPE_ID ty = TCON_ty(tcon);
	if (((ty == MTYPE_F4 || ty == MTYPE_V16F4) &&
	     TCON_R4(tcon) == 0.0) ||
	    ((ty == MTYPE_F8 || ty == MTYPE_V16F8) &&
	     TCON_R8(tcon) == 0.0))
	  zero_tn = TRUE;
      }
    }

    if (zero_tn) {
      OPERATOR rev_cmp_opr = compare_opr;

      switch( compare_opr ){
      case OPR_EQ: rev_cmp_opr = OPR_NE; break;
      case OPR_LT: rev_cmp_opr = OPR_GE; break;
      case OPR_LE: rev_cmp_opr = OPR_GT; break;
      case OPR_NE: rev_cmp_opr = OPR_EQ; break;
      case OPR_GE: rev_cmp_opr = OPR_LT; break;
      case OPR_GT: rev_cmp_opr = OPR_LE; break;
      }

      ctrl = Generate_Cmp_Ctrl_TN( rev_cmp_opr );

      if ( MTYPE_is_vector ( select_type ) ) {
	Build_OP( ( select_type == MTYPE_V16F8 ) ? TOP_cmppd : TOP_cmpps,
		  tmp1, cmp_kid1, cmp_kid2, ctrl, &new_ops );
	is_rsize_double = (select_type == MTYPE_V16F8) ? TRUE : FALSE;
      } else {
	Build_OP(is_ssize_double ? TOP_cmpsd : TOP_cmpss,
		 tmp1, cmp_kid1, cmp_kid2, ctrl, &new_ops );
        if (!is_ssize_double && is_rsize_double) {
	  // cmpss sets the low-order 32 bits.  Extend these 32 bits to 64
	  // bits.  Do this by replicating them across the entire 128-bit xmm
	  // register.  Bug 9497.
	  TN* cmp_64bit_result = Build_TN_Like(result);
	  Build_OP(TOP_pshufd, cmp_64bit_result, tmp1, Gen_Literal_TN(0, 4),
		   &new_ops);
	  tmp1 = cmp_64bit_result;
	}
      }
      Build_OP( is_rsize_double ? TOP_andpd : TOP_andps, result, 
      		false_tn, tmp1, &new_ops );
    
      if( Trace_Exp ){
	Print_OPS( &new_ops );
      }     
      OPS_Append_Ops(ops, &new_ops);      
      return;
    }

    if ( MTYPE_is_vector ( select_type ) ) {
      Build_OP( ( select_type == MTYPE_V16F8 ) ? TOP_cmppd : TOP_cmpps,
		tmp1, cmp_kid1, cmp_kid2, ctrl, &new_ops );
      is_rsize_double = (select_type == MTYPE_V16F8) ? TRUE : FALSE;
    } else {
      Build_OP(is_ssize_double ? TOP_cmpsd : TOP_cmpss,
	       tmp1, cmp_kid1, cmp_kid2, ctrl, &new_ops );
      if (!is_ssize_double && is_rsize_double) {
	// cmpss sets the low-order 32 bits.  Extend these 32 bits to 64 bits.
	// Do this by replicating them across the entire 128-bit xmm register.
	// Bug 9497.
	TN* cmp_64bit_result = Build_TN_Like(result);
	Build_OP(TOP_pshufd, cmp_64bit_result, tmp1, Gen_Literal_TN(0, 4),
		 &new_ops);
	tmp1 = cmp_64bit_result;
      }
    }
    Build_OP( is_rsize_double ? TOP_andpd : TOP_andps, tmp2, true_tn, tmp1, &new_ops );

    Build_OP( is_rsize_double ? TOP_andnpd : TOP_andnps, tmp3, tmp1, false_tn, &new_ops );

    Build_OP( is_rsize_double ? TOP_orpd : TOP_orps, result, tmp3, tmp2, &new_ops );

  } else {
    /* For case where <result>, <true_tn> and <false_tn> are
       integer type. */

    TOP cmov_top = TOP_UNDEFINED;

    switch( compare_opr ){
    case OPR_LT:
      cmov_top = MTYPE_is_float(select_type)
	? TOP_fcmovb : ( MTYPE_is_signed(desc) ? TOP_cmovl : TOP_cmovb );
      break;
    case OPR_LE:
      cmov_top = MTYPE_is_float(select_type)
	? TOP_fcmovbe : ( MTYPE_is_signed(desc) ? TOP_cmovle : TOP_cmovbe );
      break;
    case OPR_EQ:
      cmov_top = MTYPE_is_float(select_type) ? TOP_fcmove : TOP_cmove;
      break;
    case OPR_NE:
      cmov_top = MTYPE_is_float(select_type) ? TOP_fcmovne : TOP_cmovne;
      break;
    case OPR_GE:
      cmov_top = MTYPE_is_float(select_type)
	? TOP_fcmovnb : ( MTYPE_is_signed(desc) ? TOP_cmovge : TOP_cmovae );
      break;
    case OPR_GT:
      cmov_top = MTYPE_is_float(select_type)
	? TOP_fcmovnbe : ( MTYPE_is_signed(desc) ? TOP_cmovg : TOP_cmova );
      break;
    default:
      FmtAssert(FALSE, ("Unknown opcode"));
    }

    TN* rflags = Rflags_TN();

    TOP cmp_opcode = MTYPE_is_float(select_type)
      ? TOP_fucomi : ( is_ssize_double ? TOP_cmp64 : TOP_cmp32 );

    if( MTYPE_is_float(desc) ){
      cmp_opcode = ( MTYPE_is_F10(desc) || !Is_Target_SSE2() )
	? TOP_fucomi : ( is_ssize_double ? TOP_comisd : TOP_comiss );

    } else if( TN_has_value( cmp_kid2 ) ){
      if( TN_value( cmp_kid2 ) == 0 ){
	cmp_opcode = is_ssize_double ? TOP_test64 : TOP_test32;
	cmp_kid2 = cmp_kid1;

      } else {
	cmp_opcode = is_ssize_double ? TOP_cmpi64 : TOP_cmpi32;
      }
    }

    if( result != false_tn || 
	( Force_IEEE_Comparisons && 
	  TOP_is_flop( cmp_opcode ))) {
      // Fix bug091.
      FmtAssert( result != true_tn,
		 ("Exp_Select_And_Condition: result and true_tn are identical") );
      Expand_Copy( result, false_tn, select_type, &new_ops );
    }

    if (TN_has_value(cmp_kid1) && 
	!ISA_LC_Value_In_Class (TN_value(cmp_kid1), LC_simm32)) {
      TN* tmp = Build_TN_Of_Mtype(MTYPE_U8);
      Exp_Immediate(tmp, cmp_kid1, &new_ops);
      cmp_kid1 = tmp;
      cmp_opcode = is_ssize_double ? TOP_cmp64 : TOP_cmp32;
    }
    if (TN_has_value(cmp_kid2) && 
	!ISA_LC_Value_In_Class (TN_value(cmp_kid2), LC_simm32)) {
      TN* tmp = Build_TN_Of_Mtype(MTYPE_U8);
      Exp_Immediate(tmp, cmp_kid2, &new_ops);
      cmp_kid2 = tmp;
      cmp_opcode = is_ssize_double ? TOP_cmp64 : TOP_cmp32;
    }

    if( OP_NEED_PAIR( desc ) ){
      Expand_Split_Select( result, compare_opr, cmp_opcode,
			   cmp_kid1, cmp_kid2, desc,
			   true_tn, false_tn, select_type,
			   &new_ops );

    } else {
      Build_OP( cmp_opcode, rflags, cmp_kid1, cmp_kid2, &new_ops );

      TN* result_hi = NULL;
      TN* true_tn_hi = NULL;
      if (OP_NEED_PAIR(select_type)) {
	result_hi = Get_TN_Pair(result);
	true_tn_hi = Get_TN_Pair(true_tn);
      }

      // Ugly hack to preserve the interaface to Expand_Ordered_Select_Compare,
      // which parses a straight line code involving cmov.  Expand_Cmov, which
      // was added later, breaks this interface by introducing basic blocks.
      // Bug 8087.
      if (Force_IEEE_Comparisons && TOP_is_flop(cmp_opcode)) {
	Build_OP(cmov_top, result, true_tn, rflags, &new_ops);
	Set_OP_cond_def_kind(OPS_last(&new_ops), OP_ALWAYS_COND_DEF);
	if (result_hi != NULL) {
	  Build_OP(cmov_top, result_hi, true_tn_hi, rflags, &new_ops);
	  Set_OP_cond_def_kind(OPS_last(&new_ops), OP_ALWAYS_COND_DEF);
	}
	// To avoid cluttering we will do a post-process on new_ops incase we
	// need to cover unordered FP comparisons.
	Expand_Ordered_Select_Compare ( &new_ops, cmov_top );
      } else {
	Expand_Cmov(cmov_top, result, true_tn, rflags, &new_ops,
		    result_hi, true_tn_hi);
      }
    }
  }

  if( Trace_Exp ){
    Print_OPS( &new_ops );
  }

  OPS_Append_Ops(ops, &new_ops);
}


#define RESET_COND_DEF_LAST(ops) Set_OP_cond_def_kind(OPS_last(ops),OP_ALWAYS_UNC_DEF)

static void
Expand_SGI_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  /*	(p0) frsqrta.s0 f6,p2=src	# y2 = ~1/sqrt(x)
   *
   *	(p2) ldfd	f4=half		# f4 = 0.5 (0x3fe0000000000000)
   *	(p2) ldfd	f7=ah		# f7 = 0x3fe0000000000001
   *
   *	(p2) fmpy.d.s1	f3=src,f6	# g = x*y2
   *	(p2) fmpy.d.s1	f2=f4,f6	# y = 0.5*y2
   *
   *	(p2) fnma.d.s1	f5=f3,f3,src	# d = x - g*g
   *
   *	(p2) fma.d.s1	f3=f2,f5,f3	# g = g + y*d # 16 bit approximation
   *
   *	(p2) fnma.d.s1	f8=f2,f3,f7	# e = ah - y*g
   *	(p2) fnma.d.s1	f5=f3,f3,src    # d = x - g*g
   *	(p2) fma.d.s1	f2=f8,f6,f2	# y = y + e*y2
   *
   *	(p2) fma.d.s1   f3=f2,f5,f3     # g = g + y*d # 32 bit approximation
   *	(p2) fadd.d.s1  f6=f3,f3        # y2 = y + y
   *
   *	(p2) fnma.d.s1	f8=f2,f3,f7	# e = ah - y*g
   *	(p2) fnma.d.s1	f5=f3,f3,src    # d = x - g*g
   *	(p2) fma.d.s1	f2=f8,f6,f2	# y = y + e*y2
   *
   *	(p2) fma.d.s1   f3=f2,f5,f3     # g = g + y*d # 64 bit approximation before rounding
   *	(p2) fadd.d.s1  f6=f3,f3        # y2 = y + y
   *
   *	(p2) fnma.d.s1	f8=f2,f3,f7	# e = ah - y*g
   *	(p2) fnma.d.s1	f5=f3,f3,src    # d = x - g*g
   *	(p2) fma.d.s1	f2=f8,f6,f2	# y = y + e*y2
   *
   *	(p2) fma.d.s0   f6=f2,f5,f3	# result = g + y*d
   */
  // 3-mar-00/ken: this doesn't work for MTYPE_F10!!!!
}

static void
Expand_Intel_F10_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Max_Thr_F8_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Max_Thr_F4_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Min_Lat_F8_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Min_Lat_F4_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Max_Thr_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  switch (mtype) {
  case MTYPE_F4:
    Expand_Intel_Max_Thr_F4_Sqrt(result, src, ops);
    break;
  case MTYPE_F8:
    Expand_Intel_Max_Thr_F8_Sqrt(result, src, ops);
    break;
  case MTYPE_F10:
    Expand_Intel_F10_Sqrt(result, src, ops);
    break;
  default:
    FmtAssert(FALSE, ("Bad type in Expand_Intel_Max_Thr_Sqrt"));
    /*NOTREACHED*/
  }
}


static void
Expand_Intel_Min_Lat_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  switch (mtype) {
  case MTYPE_F4:
    Expand_Intel_Min_Lat_F4_Sqrt(result, src, ops);
    break;
  case MTYPE_F8:
    Expand_Intel_Min_Lat_F8_Sqrt(result, src, ops);
    break;
  case MTYPE_F10:
    Expand_Intel_F10_Sqrt(result, src, ops);
    break;
  default:
    FmtAssert(FALSE, ("Bad type in Expand_Intel_Min_Lat_Sqrt"));
    /*NOTREACHED*/
  }
}


static void 
Expand_Fast_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  FmtAssert( mtype == MTYPE_F4 || mtype == MTYPE_V16F4 , ("NYI"));
  
  TN* tmp0 = Build_TN_Like(result);
  TN* tmp1 = Build_TN_Like(result);
  TN* tmp2 = Build_TN_Like(result);
  TN* tmp3 = Build_TN_Like(result);
  TN* tmp4 = Build_TN_Like(result);
  TN* tmp5 = Build_TN_Like(result);
  TN* tmp6 = Build_TN_Like(result);
  TN* tmp7 = Build_TN_Like(result);
  TN* const0 = Build_TN_Like(result);
  TN* const1 = Build_TN_Like(result);
  TN *xzero = NULL;
  
  if ( mtype == MTYPE_F4 ) {
    Build_OP( TOP_xzero32, tmp0, ops);
    Build_OP( TOP_cmpneqss, tmp1, tmp0, src, ops );
    if ( Is_Target_Orochi() && Is_Target_AVX() ) {
      Build_OP( TOP_rsqrtss, tmp2, src, src, ops );
    } else {
      Build_OP( TOP_rsqrtss, tmp2, src, ops );
    }
    Build_OP( TOP_fand128v32, tmp3, tmp2, tmp1, ops );
    Build_OP( TOP_mulss, tmp4, tmp3, src, ops );
    Build_OP( TOP_mulss, tmp5, tmp4, tmp3, ops );
    Expand_Const( const0, Gen_Const_Symbol_TN( 0x40400000, 0.0, MTYPE_I4 ),
		  mtype, ops );
    Build_OP( TOP_subss, tmp6, tmp5, const0, ops );
    Build_OP( TOP_mulss, tmp7, tmp6, tmp4, ops );
    Expand_Const( const1, Gen_Const_Symbol_TN( 0xbf000000, 0.0, MTYPE_I4 ),
		  mtype, ops );
    Build_OP( TOP_mulss, result, tmp7, const1, ops );    

  } else { // mtype == MTYPE_V16F4
    Build_OP( TOP_xzero128v32, tmp0, ops);
    Build_OP( TOP_cmpneqps, tmp1, tmp0, src, ops );
    Build_OP( TOP_frsqrt128v32, tmp2, src, ops );
    Build_OP( TOP_fand128v32, tmp3, tmp2, tmp1, ops );
    Build_OP( TOP_fmul128v32, tmp4, tmp3, src, ops );
    Build_OP( TOP_fmul128v32, tmp5, tmp4, tmp3, ops );
    TCON then0 = Host_To_Targ (MTYPE_I4, 0x40400000);
    TCON now0  = Create_Simd_Const (MTYPE_V16F4, then0);
    ST *sym0 = New_Const_Sym (Enter_tcon (now0), Be_Type_Tbl(TCON_ty(now0)));
    Allocate_Object(sym0);
    TN *sym_tn0 = Gen_Symbol_TN(sym0, 0, 0);
    Exp_Load(mtype, mtype, const0, TN_var(sym_tn0), TN_offset(sym_tn0), ops, 0);
    Build_OP( TOP_fsub128v32, tmp6, tmp5, const0, ops );
    Build_OP( TOP_fmul128v32, tmp7, tmp6, tmp4, ops );
    TCON then1 = Host_To_Targ (MTYPE_I4, 0xbf000000);
    TCON now1  = Create_Simd_Const (MTYPE_V16F4, then1);
    ST *sym1 = New_Const_Sym (Enter_tcon (now1), Be_Type_Tbl(TCON_ty(now1)));
    Allocate_Object(sym1);
    TN *sym_tn1 = Gen_Symbol_TN(sym1, 0, 0);
    Exp_Load(mtype, mtype, const1, TN_var(sym_tn1), TN_offset(sym_tn1), ops, 0);
    Build_OP( TOP_fmul128v32, result, tmp7, const1, ops );        
  }

  return;
}

void
Expand_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  FmtAssert( MTYPE_is_float(mtype),
	    ("Unimplemented sqrt for integer src/dest") );

  if ( Fast_Sqrt_Allowed && 
       (mtype == MTYPE_F4 || mtype == MTYPE_V16F4 || mtype == MTYPE_V32F4) &&
       Is_Target_SSE2() ) {
    Expand_Fast_Sqrt(result, src, mtype, ops );
    return;
  }
    

  switch(mtype) {
  case MTYPE_V32F4:
  case MTYPE_V16F4:
    Build_OP(TOP_fsqrt128v32, result, src, ops);
    break;
  case MTYPE_V32F8:
  case MTYPE_V16F8:
    Build_OP(TOP_fsqrt128v64, result, src, ops);
    break;
  default:    
    if( MTYPE_is_F10(mtype) ||
	!Is_Target_SSE2() ) {
      Build_OP( TOP_fsqrt, result, src, ops);
    }
    else {
      TOP top = (mtype == MTYPE_F8) ? TOP_sqrtsd : TOP_sqrtss;
      if ( Is_Target_Orochi() && Is_Target_AVX() ) {
        Build_OP( top, result, src, src, ops );
      } else {
        Build_OP( top, result, src, ops );
      }
    }
    break;
  }
}


static void
Expand_Float_Compares(TOP set_opcode, 
		      TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  const BOOL is_double = MTYPE_is_size_double(mtype);
  const TOP top = ( MTYPE_is_F10(mtype) || !Is_Target_SSE2() )
    ? TOP_fucomi : ( is_double ? TOP_comisd : TOP_comiss );

  TN* rflags = Rflags_TN();
  if ( Force_IEEE_Comparisons && 
       ( set_opcode == TOP_setb || set_opcode == TOP_setbe ) )
    Build_OP( top, rflags, src2, src1, ops );
  else
    Build_OP( top, rflags, src1, src2, ops );

  TN *dest_tmp = Build_TN_Of_Mtype(MTYPE_U1);

  if ( Force_IEEE_Comparisons ) {
    if ( set_opcode == TOP_seta || set_opcode == TOP_setae )
      // We are as good before.
      Build_OP( set_opcode, dest_tmp, rflags, ops);
    else if ( set_opcode == TOP_setb || set_opcode == TOP_setbe )
      Build_OP( set_opcode == TOP_setb ? TOP_seta : TOP_setae, 
		dest_tmp, rflags, ops);
    else if ( set_opcode == TOP_sete ) {
      TN *dest_tmp1 = Build_TN_Of_Mtype(MTYPE_U1);
      TN *dest_tmp2 = Build_TN_Of_Mtype(MTYPE_U1);
      Build_OP( set_opcode, dest_tmp, rflags, ops );      
      Build_OP( TOP_setnp, dest_tmp1, rflags, ops );
      Build_OP (TOP_and8, dest_tmp2, dest_tmp, dest_tmp1, ops );
      dest_tmp = dest_tmp2;
    }
    else if (set_opcode == TOP_setne ) {
      TN *dest_tmp1 = Build_TN_Of_Mtype(MTYPE_U1);
      TN *dest_tmp2 = Build_TN_Of_Mtype(MTYPE_U1);
      Build_OP( set_opcode, dest_tmp, rflags, ops );      
      Build_OP( TOP_setp, dest_tmp1, rflags, ops );
      Build_OP (TOP_or8, dest_tmp2, dest_tmp, dest_tmp1, ops );
      dest_tmp = dest_tmp2;
    } else
      FmtAssert ( FALSE,
		  ("Expand_Float_Compares: Unsupported opcode (%s)",
		   TOP_Name(set_opcode)) );
  } else 
    Build_OP( set_opcode, dest_tmp, rflags, ops);

  if( Is_Target_32bit() &&
      TN_size(dest) == 8 ){
    Expand_Split_Cvtl( MTYPE_I8, TOP_movzbq, dest, dest_tmp, ops );

  } else {
    Build_OP( TN_size(dest) == 8 ? TOP_movzbq : TOP_movzbl,
	      dest, dest_tmp, ops );
  }
}

void
Expand_Float_Less (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_setb, dest, src1, src2, mtype, ops);
}

void
Expand_Float_Greater (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_seta, dest, src1, src2, mtype, ops);
}

void
Expand_Float_Less_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_setbe, dest, src1, src2, mtype, ops);
}

void
Expand_Float_Greater_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_setae, dest, src1, src2, mtype, ops);
}

void
Expand_Float_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_sete, dest, src1, src2, mtype, ops);
}

void
Expand_Float_Not_Equal (TN *dest, TN *src1, TN *src2, VARIANT variant, TYPE_ID mtype, OPS *ops)
{
  Expand_Float_Compares(TOP_setne, dest, src1, src2, mtype, ops);
}

void
Expand_Recip_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  /*	(p0) frsqrta.s0 f2,p2=src	# y = ~1/sqrt(x)
   *
   *	(p2) ldfd	f4=half		# f4 = 0.5
   *	(p2) fmpy.d.s1	f5=f4,src	# hx = 0.5*x
   *
   *	(p2) fmpy.d.s1	f3=f2,f2	# y2 = y*y
   *	(p2) fnma.d.s1	f6=f5,f3,f4	# z = 0.5 - 0.5*x*y*y
   *	(p2) fma.d.s1   f2=f2,f6,f2	# y = y + y*z
   *
   *	(p2) fmpy.d.s1	f3=f2,f2	# y2 = y*y
   *	(p2) fnma.d.s1	f6=f5,f3,f4	# z = 0.5 - 0.5*x*y*y
   *	(p2) fma.d.s1   f2=f2,f6,f2	# y = y + y*z
   *
   *	(p2) fmpy.d.s1	f3=f2,f2	# y2 = y*y
   *	(p2) fnma.d.s1	f6=f5,f3,f4	# z = 0.5 - 0.5*x*y*y
   *	(p2) fma.d.s0   f2=f2,f6,f2	# result = y + y*z
   */
  ErrMsg( EC_Unimplemented, "Expand_Recip_Sqrt: NYI" );
}


/* Don't use TOP_rcpss, which gives non-accurate result. */
static void Expand_Recip( TN* result, TN* src2, TYPE_ID mtype, OPS* ops )
{
  const BOOL is_double = MTYPE_is_size_double(mtype);

  if( Recip_Allowed && Is_Target_SSE2() && mtype == MTYPE_V16F4 ) {
    TN *tmp1 = Build_TN_Like(result);
    TN *tmp2 = Build_TN_Like(result);
    TN *tmp3 = Build_TN_Like(result);
    TN *tmp4 = Build_TN_Like(result);
    Build_OP( TOP_frcp128v32, tmp1, src2, ops );
    // Bug 7218 - add a Newton-Raphson iteration to recip computation.
    Build_OP( TOP_fmul128v32, tmp2, src2, tmp1, ops );
    Build_OP( TOP_fmul128v32, tmp3, tmp2, tmp1, ops );
    Build_OP( TOP_fadd128v32, tmp4, tmp1, tmp1, ops );
    Build_OP( TOP_fsub128v32, result, tmp4, tmp3, ops );      
    return;
  } 
    
  TN* src1 = Build_TN_Like( src2 );

  TCON tcon;
  ST* sym;

  if (mtype == MTYPE_V16F4)
    tcon = Create_Simd_Const ( MTYPE_V16F4,  
			       Host_To_Targ_Float_4 ( MTYPE_F4, 1.0 ) );
  else if (mtype == MTYPE_V16F8)
    tcon = Create_Simd_Const ( MTYPE_V16F8,
                               Host_To_Targ_Float_4 ( MTYPE_F8, 1.0 ) );
  else
    tcon = Host_To_Targ_Float( mtype, 1.0 );

  sym = New_Const_Sym( Enter_tcon(tcon),  Be_Type_Tbl( TCON_ty(tcon) ) );

  ST* base_sym = NULL;
  INT64 base_ofst = 0;

  Allocate_Object(sym);
  Base_Symbol_And_Offset_For_Addressing( sym, 0, &base_sym, &base_ofst );

  Expand_Const( src1, Gen_Symbol_TN( base_sym, base_ofst, TN_RELOC_NONE ), 
		mtype, ops );

  const BOOL non_sse2_fp = MTYPE_is_F10(mtype) || !Is_Target_SSE2();
  if (mtype == MTYPE_V16F4)    
    Build_OP( TOP_fdiv128v32, result, src1, src2, ops );
  else if (mtype == MTYPE_V16F8)
    Build_OP( TOP_fdiv128v64, result, src1, src2, ops );
  else
    Build_OP( non_sse2_fp ? TOP_fdiv : ( is_double ? TOP_divsd : TOP_divss ),
	      result, src1, src2, ops );
}

void Expand_Complex( OPCODE opcode, TN *result, 
		     TN *src1, TN *src2, OPS *ops )
{
    Build_OP(TOP_unpcklpd, result, src1, src2, ops);
    return;
}
void Expand_Firstpart( OPCODE opcode, TN *result, 
		     TN *src1, OPS *ops ){
    if( Is_Target_Barcelona() || Is_Target_EM64T() ||
        Is_Target_Wolfdale()  || Is_Target_Core()  ||
        Is_Target_Orochi() ){
      Build_OP( TOP_movaps, result, src1, ops );
    } else {
      Build_OP( TOP_movdq, result, src1, ops );
    }
}
void Expand_Secondpart( OPCODE opcode, TN *result, 
		     TN *src1, OPS *ops ){
    Build_OP(TOP_pshufd, result, src1, Gen_Literal_TN(0xe,1), ops);
}
static void Expand_Complex_Multiply( OPCODE opcode, TN *result, 
				     TN *src1, TN *src2, OPS *ops )
{
  FmtAssert(opcode == OPC_V16C4MPY || opcode == OPC_V16C8MPY, ("NYI"));

  if (opcode == OPC_V16C4MPY) {
    TN* tmp1 = Build_TN_Like(src1);
    TN* tmp2 = Build_TN_Like(src1);
    TN* tmp3 = Build_TN_Like(src1);
    TN* tmp4 = Build_TN_Like(src1);
    TN* tmp5 = Build_TN_Like(src1);
    
    Build_OP(TOP_fmovsldup, tmp1, src2, ops);
    if ((CG_opt_level > 1) && 
        Is_Target_AVX() && 
        Is_Target_FMA4()) {
      Build_OP(TOP_fmovshdup,tmp3, src2, ops);
      Build_OP(TOP_shufps, tmp4, src1, src1, Gen_Literal_TN(177, 1), ops);
      Build_OP(TOP_fmul128v32, tmp5, tmp3, tmp4, ops);
      Build_OP(TOP_vfmaddsubps, result, tmp1, src1, tmp5, ops);  
    } else if ((CG_opt_level > 1) && 
              Is_Target_AVX() && 
              Is_Target_FMA()) {
      Build_OP(TOP_fmovshdup,tmp3, src2, ops);
      Build_OP(TOP_shufps, tmp4, src1, src1, Gen_Literal_TN(177, 1), ops);
      Build_OP(TOP_fmul128v32, tmp5, tmp3, tmp4, ops);
      Build_OP(TOP_xfmaddsub213ps, result, tmp1, src1, tmp5, ops);  
    } else {
      Build_OP(TOP_fmul128v32, tmp2, tmp1, src1, ops);
      Build_OP(TOP_fmovshdup,tmp3, src2, ops);
      Build_OP(TOP_shufps, tmp4, src1, src1, Gen_Literal_TN(177, 1), ops);
      Build_OP(TOP_fmul128v32, tmp5, tmp3, tmp4, ops);
      Build_OP(TOP_faddsub128v32, result, tmp2, tmp5, ops);
    }
  } else if (TN_size(src1) != TN_size(src2)){
    TN* src1_t;
    TN* src2_t;
    if (TN_size(src1) < TN_size(src2)) {
      src1_t = src1;
      src2_t = src2;
    } else {
      src2_t = src1;
      src1_t = src2;
    }
    
    TN* tmp1 = Build_TN_Like(src2_t);

    Build_OP(TOP_fmovddup, tmp1, src1_t, ops);
    Build_OP(TOP_fmul128v64, result, src2_t, tmp1, ops);
  } else { // OPC_V16C8MPY
    // The WN simplifier always orders a multiply between an iload and a ldid 
    // as 'iload * ldid' and so we need to commute the operation to make sure 
    // address folding opportunity is exposed to EBO.
    TN* src1_t = src2;
    TN* src2_t = src1;
    if (!Enable_Cfold_Aggressive) {
      src1_t = src1;
      src2_t = src2;
    }
    TN* tmp1 = Build_TN_Like(src1);
    TN* tmp2 = Build_TN_Like(src1);
    TN* tmp3 = Build_TN_Like(src1);
    TN* tmp4 = Build_TN_Like(src1);
    TN* tmp5 = Build_TN_Like(src1);
    TN* tmp6 = Build_TN_Like(src1);
    
    Build_OP(TOP_fmovddup, tmp1, src2_t, ops);
    if ((CG_opt_level > 1) && 
        Is_Target_AVX() && 
        Is_Target_FMA4()) {
      Build_OP(TOP_shufpd, tmp3, src1_t, src1_t, Gen_Literal_TN(1, 1), ops);
      Build_OP(TOP_shufpd, tmp4, src2_t, src2_t, Gen_Literal_TN(1, 1), ops);
      Build_OP(TOP_fmovddup, tmp5, tmp4, ops);
      Build_OP(TOP_fmul128v64, tmp6, tmp3, tmp5, ops);
      Build_OP(TOP_vfmaddsubpd, result, src1_t, tmp1, tmp6, ops);  
    } else if ((CG_opt_level > 1) && 
                Is_Target_AVX() && 
                Is_Target_FMA()) {
      Build_OP(TOP_shufpd, tmp3, src1_t, src1_t, Gen_Literal_TN(1, 1), ops);
      Build_OP(TOP_shufpd, tmp4, src2_t, src2_t, Gen_Literal_TN(1, 1), ops);
      Build_OP(TOP_fmovddup, tmp5, tmp4, ops);
      Build_OP(TOP_fmul128v64, tmp6, tmp3, tmp5, ops);
      Build_OP(TOP_xfmaddsub213pd, result, src1_t, tmp1, tmp6, ops);  
    } else {
      Build_OP(TOP_fmul128v64, tmp2, src1_t, tmp1, ops);
      Build_OP(TOP_shufpd, tmp3, src1_t, src1_t, Gen_Literal_TN(1, 1), ops);
      Build_OP(TOP_shufpd, tmp4, src2_t, src2_t, Gen_Literal_TN(1, 1), ops);
      Build_OP(TOP_fmovddup, tmp5, tmp4, ops);
      Build_OP(TOP_fmul128v64, tmp6, tmp3, tmp5, ops);
      Build_OP(TOP_faddsub128v64, result, tmp2, tmp6, ops);
    }
  }
  return;
}

static void Expand_Complex_Divide( OPCODE opcode, TN *result, 
				     TN *src1, TN *src2, OPS *ops )
{
  FmtAssert(opcode == OPC_V16C4DIV || opcode == OPC_V16C8DIV, ("NYI"));
  
  if (opcode == OPC_V16C8DIV && TN_size(src2) == TN_size(src1)/2) {
    
    TN* tmp1 = Build_TN_Like(src1);

    Build_OP(TOP_fmovddup, tmp1, src2, ops);
    Build_OP(TOP_fdiv128v64, result, src1, tmp1, ops);

  } else if (opcode == OPC_V16C8DIV && TN_size(src1) == TN_size(src2)) {
    FmtAssert(TN_size(src1) == 16, ("Unexpected operand for V16C8DIV"));
    /*
     *   z / w
     *    real =	(R(z)*R(w) + I(z)*I(w) /  ( R(w)**2 + I(w)**2 )
     *    imag =	(I(z)*R(w) - R(z)*I(w) /  ( R(w)**2 + I(w)**2 )
     *
     */
    
    TN* tmp1 = Build_TN_Like(src1);
    TN* tmp2 = Build_TN_Like(src1);
    TN* tmp3 = Build_TN_Like(src1);
    TN* tmp4 = Build_TN_Like(src1);
    TN* tmp5 = Build_TN_Like(src1);
    TN* tmp6 = Build_TN_Like(src1);
    TN* tmp7 = Build_TN_Like(src1);
    TN* tmp8 = Build_TN_Like(src1);
    TN* tmp9 = Build_TN_Like(src1);
    TN* tmp10 = Build_TN_Like(src1);
    TN* tmp11 = Build_TN_Like(src1);
    TN* tmp12 = Build_TN_Like(src1);
    TN* tmp13 = Build_TN_Like(src1);
    // TODO: adjust tn size for sd
    Build_OP(TOP_mulsd, tmp1, src2, src2, ops); 
    Build_OP(TOP_unpckhpd, tmp2, src2, src2, ops);
    Build_OP(TOP_mulsd, tmp3, tmp2, tmp2, ops);
    Build_OP(TOP_addsd, tmp4, tmp3, tmp1, ops);
    Build_OP(TOP_fmovddup, tmp5, tmp4, ops);
    Build_OP(TOP_fmovddup, tmp6, src2, ops);
    if ((CG_opt_level > 1) && 
        Is_Target_AVX() && 
        Is_Target_FMA4()) {
      Build_OP(TOP_shufpd, tmp9, src1, src1, Gen_Literal_TN(1, 1), ops);
      Expand_Neg(tmp12, tmp2, MTYPE_F8, ops);
      Build_OP(TOP_fmovddup, tmp13, tmp12, ops);
      Build_OP(TOP_fmul128v64, tmp10, tmp9, tmp13, ops);
      Build_OP(TOP_vfmaddsubpd, tmp11, tmp6, src1, tmp10, ops);  
    } else if ((CG_opt_level > 1) && 
               Is_Target_AVX() &&
               Is_Target_FMA()) {
      Build_OP(TOP_shufpd, tmp9, src1, src1, Gen_Literal_TN(1, 1), ops);
      Expand_Neg(tmp12, tmp2, MTYPE_F8, ops);
      Build_OP(TOP_fmovddup, tmp13, tmp12, ops);
      Build_OP(TOP_fmul128v64, tmp10, tmp9, tmp13, ops);
      Build_OP(TOP_xfmaddsub213pd, tmp11, tmp6, src1, tmp10, ops);  
    } else {
      Build_OP(TOP_fmul128v64, tmp8, tmp6, src1, ops);
      Build_OP(TOP_shufpd, tmp9, src1, src1, Gen_Literal_TN(1, 1), ops);
      Expand_Neg(tmp12, tmp2, MTYPE_F8, ops);
      Build_OP(TOP_fmovddup, tmp13, tmp12, ops);
      Build_OP(TOP_fmul128v64, tmp10, tmp9, tmp13, ops);
      Build_OP(TOP_faddsub128v64, tmp11, tmp8, tmp10, ops);
    }
    Build_OP(TOP_fdiv128v64, result, tmp11, tmp5, ops);

  } else if (opcode == OPC_V16C8DIV) {
    FmtAssert( TN_size(src1) == TN_size(src2)/2, ("NYI"));
    // iz == 0
    /*
     *   z / w
     *    real =      R(z)*R(w) /  ( R(w)**2 + I(w)**2 )
     *    imag =     -R(z)*I(w) /  ( R(w)**2 + I(w)**2 )
     *   
     */
    TN* tmp1 = Build_TN_Like(src1);
    TN* tmp2 = Build_TN_Like(src2);
    TN* tmp3 = Build_TN_Like(src1);
    TN* tmp4 = Build_TN_Like(src1);
    TN* tmp5 = Build_TN_Like(src2);
    TN* tmp6 = Build_TN_Like(src2);
    TN* tmp7 = Build_TN_Like(src2);
    TN* tmp8 = Build_TN_Like(src2);
    Build_OP(TOP_mulsd, tmp1, src2, src2, ops); 
    Build_OP(TOP_unpckhpd, tmp2, src2, src2, ops);			
    Build_OP(TOP_mulsd, tmp3, tmp2, tmp2, ops);
    Build_OP(TOP_addsd, tmp4, tmp3, tmp1, ops);
    Build_OP(TOP_fmovddup, tmp5, tmp4, ops);
    Build_OP(TOP_fmovddup, tmp6, src1, ops);
    Build_OP(TOP_fmul128v64, tmp7, tmp6, src2, ops);
    Exp_Intrinsic_Op (INTRN_V16C8CONJG, tmp8, tmp7, NULL, NULL, NULL, NULL, MTYPE_V16C8, ops);
    Build_OP(TOP_fdiv128v64, result, tmp8, tmp5, ops);
    
  } else if (opcode == OPC_V16C4DIV) {
    TN* tmp1 = Build_TN_Like(src1);
    TN* tmp2 = Build_TN_Like(src1);
    TN* tmp3 = Build_TN_Like(src1);
    TN* tmp4 = Build_TN_Like(src1);
    TN* tmp5 = Build_TN_Like(src1);
    TN* tmp6 = Build_TN_Like(src1);
    TN* tmp7 = Build_TN_Like(src1);
    TN* tmp8 = Build_TN_Like(src1);
    TN* tmp9 = Build_TN_Like(src1);
    TN* tmp10 = Build_TN_Like(src1);
    TN* tmp11 = Build_TN_Like(src1);
    TN* tmp12 = Build_TN_Like(src1);
    TN* tmp13 = Build_TN_Like(src1);
    TN* tmp14 = Build_TN_Like(src1);
    TN* tmp15 = result;
    TN* tmp16 = Build_TN_Like(src1);
    TN* tmp17 = Build_TN_Like(src1);
    TN* tmp18 = Build_TN_Like(src1);
    TN* tmp19 = Build_TN_Like(src1);
    TN* tmp20 = Build_TN_Like(src1);
    TN* tmp21 = Build_TN_Like(src1);
    TN* tmp22 = Build_TN_Like(src1);
    TN* tmp23 = Build_TN_Like(src1);
    TN* tmp24 = Build_TN_Like(src1);
    TN* tmp25 = Build_TN_Like(src1);
    TN* tmp26 = Build_TN_Like(src1);
    TN* tmp27 = Build_TN_Like(src1);
    TN* tmp28 = Build_TN_Like(src1);
    
    Build_OP(TOP_cvtps2pd, tmp1, src1, ops);
    Build_OP(TOP_cvtps2pd, tmp2, src2, ops);
    Build_OP(TOP_fmul128v64, tmp3, tmp2, tmp2, ops);
    Build_OP(TOP_fmovddup, tmp4, tmp2, ops);
    Build_OP(TOP_unpckhpd, tmp5, tmp2, tmp2, ops);
    Build_OP(TOP_fmul128v64, tmp6, tmp5, tmp1, ops);
    Build_OP(TOP_shufpd, tmp7, tmp1, tmp1, Gen_Literal_TN(1, 1), ops);
    if ((CG_opt_level > 1) && 
        Is_Target_AVX() && 
        Is_Target_FMA4()) {
      Expand_Reduce_Add(OPC_F8V16F8REDUCE_ADD, tmp9, tmp3, ops);
      Build_OP(TOP_shufps, tmp10, src1, src1, Gen_Literal_TN(238, 1), ops);
      Build_OP(TOP_cvtps2pd, tmp11, tmp10, ops);
      Build_OP(TOP_vfmaddsubpd, tmp12, tmp7, tmp4, tmp6, ops);
    } else if ((CG_opt_level > 1) &&
               Is_Target_AVX() && 
               Is_Target_FMA()) {
      Expand_Reduce_Add(OPC_F8V16F8REDUCE_ADD, tmp9, tmp3, ops);
      Build_OP(TOP_shufps, tmp10, src1, src1, Gen_Literal_TN(238, 1), ops);
      Build_OP(TOP_cvtps2pd, tmp11, tmp10, ops);
      Build_OP(TOP_xfmaddsub213pd, tmp12, tmp7, tmp4, tmp6, ops);
    } else {
      Build_OP(TOP_fmul128v64, tmp8, tmp7, tmp4, ops);
      Build_OP(TOP_fhadd128v64, tmp9, tmp3, tmp3, ops);
      Build_OP(TOP_shufps, tmp10, src1, src1, Gen_Literal_TN(238, 1), ops);
      Build_OP(TOP_cvtps2pd, tmp11, tmp10, ops);
      Build_OP(TOP_faddsub128v64, tmp12, tmp8, tmp6, ops);
    }
    Build_OP(TOP_shufpd, tmp13, tmp12, tmp12, Gen_Literal_TN(1, 1), ops);
    Build_OP(TOP_fdiv128v64, tmp14, tmp13, tmp9, ops);
    Build_OP(TOP_cvtpd2ps, tmp15, tmp14, ops);
    Build_OP(TOP_shufps, tmp16, src2, src2, Gen_Literal_TN(238, 1), ops);
    Build_OP(TOP_cvtps2pd, tmp17, tmp16, ops);
    Build_OP(TOP_fmul128v64, tmp18, tmp17, tmp17, ops);
    Build_OP(TOP_fmovddup, tmp19, tmp17, ops);
    Build_OP(TOP_unpckhpd, tmp20, tmp17, tmp17, ops);
    Build_OP(TOP_fmul128v64, tmp21, tmp20, tmp11, ops);
    Build_OP(TOP_shufpd, tmp22, tmp11, tmp11, Gen_Literal_TN(1, 1), ops);
    if ((CG_opt_level > 1) && 
        Is_Target_AVX() && 
        Is_Target_FMA4()) {
      Expand_Reduce_Add(OPC_F8V16F8REDUCE_ADD, tmp25, tmp18, ops);
      Build_OP(TOP_vfmaddsubpd, tmp25, tmp22, tmp19, tmp21, ops);
    } else if ((CG_opt_level > 1) &&
               Is_Target_AVX() && 
               Is_Target_FMA()) {
      Expand_Reduce_Add(OPC_F8V16F8REDUCE_ADD, tmp25, tmp18, ops);
      Build_OP(TOP_xfmaddsub213pd, tmp25, tmp22, tmp19, tmp21, ops);
    } else {
      Build_OP(TOP_fmul128v64, tmp23, tmp22, tmp19, ops);
      Build_OP(TOP_fhadd128v64, tmp24, tmp18, tmp18, ops);
      Build_OP(TOP_faddsub128v64, tmp25, tmp23, tmp21, ops);
    }
    Build_OP(TOP_shufpd, tmp26, tmp25, tmp25, Gen_Literal_TN(1, 1), ops);
    Build_OP(TOP_fdiv128v64, tmp27, tmp26, tmp24, ops);
    Build_OP(TOP_cvtpd2ps, tmp28, tmp27, ops);
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      Build_OP(TOP_movlhps, result, result, tmp28, ops);
    } else {
      Build_OP(TOP_movlhps, result, tmp28, ops);
    }
  }

  Set_OP_cond_def_kind( OPS_last(ops), OP_ALWAYS_COND_DEF );
  return;
}

void Expand_Flop( OPCODE opcode, TN *result, TN *src1, TN *src2, TN *src3, OPS *ops )
{
  TOP opc;

  switch (opcode) {
  case OPC_F4ADD:
    if( Is_Target_SSE2() ){
      opc = TOP_addss;
      break;
    }  /* fall thru */
  case OPC_F8ADD:
    if( Is_Target_SSE2() ){
      opc = TOP_addsd;
      break;
    }  /* fall thru */
  case OPC_F10ADD:
    opc = TOP_fadd;
    break;
  case OPC_V32F4ADD:
  case OPC_V16F4ADD:
  case OPC_V16C4ADD:
  case OPC_V8F4ADD:
    opc = TOP_fadd128v32;
    break;
  case OPC_V32F8ADD:
  case OPC_V16F8ADD:
    opc = TOP_fadd128v64;
    break;
  case OPC_V16C8ADD:
    if (TN_size(src1) < MTYPE_byte_size(MTYPE_V16C8) || 
	TN_size(src2) < MTYPE_byte_size(MTYPE_V16C8))
    {  
      FmtAssert(TN_size(src1) == MTYPE_byte_size(MTYPE_V16C8) ||
       TN_size(src2) == MTYPE_byte_size(MTYPE_V16C8),
       ("at least one opnd should be complex"));
      if (TN_size(src1) < MTYPE_byte_size(MTYPE_V16C8))
      {
	TN *tmp1 = Build_TN_Like(result);
	Build_OP(TOP_unpckhpd, tmp1, src2, tmp1, ops);
	Build_OP(TOP_addsd, result, src1, src2, ops);
	Build_OP(TOP_unpcklpd, result, result, tmp1, ops);
      }
      else {
	TN *tmp1 = Build_TN_Like(result);
	Build_OP(TOP_unpckhpd, tmp1, src1, tmp1, ops);
	Build_OP(TOP_addsd, result, src1, src2, ops);
	Build_OP(TOP_unpcklpd, result, result, tmp1, ops);
      }
    } else 
      Build_OP(TOP_fadd128v64, result, src1, src2, ops);
    return;
  case OPC_F4SUB:
    if( Is_Target_SSE2() ){
      opc = TOP_subss;
      break;
    }  /* fall thru */
  case OPC_F8SUB:
    if( Is_Target_SSE2() ){
      opc = TOP_subsd;
      break;
    }  /* fall thru */
  case OPC_F10SUB:
    opc = TOP_fsub;
    break;
  case OPC_V16F4SUB:
  case OPC_V16C4SUB:
    opc = TOP_fsub128v32;
    break;
  case OPC_V16F8SUB:
    opc = TOP_fsub128v64;
    break;
  case OPC_V16C8SUB:
    if (TN_size(src1) < MTYPE_byte_size(MTYPE_V16C8) || 
	TN_size(src2) < MTYPE_byte_size(MTYPE_V16C8))
    {  
      FmtAssert(TN_size(src1) == MTYPE_byte_size(MTYPE_V16C8) ||
	      TN_size(src2) == MTYPE_byte_size(MTYPE_V16C8),
	      ("at least one opnd should be complex"));
      if (TN_size(src1) == MTYPE_byte_size(MTYPE_V16C8))
      {
	TN *tmp1 = Build_TN_Like(result);
	Build_OP(TOP_unpckhpd, tmp1, src1, tmp1, ops);
	Build_OP(TOP_subsd, result, src1, src2, ops);
	Build_OP(TOP_unpcklpd, result, result, tmp1, ops);
      } else if (TN_size(src2) == MTYPE_byte_size(MTYPE_V16C8)){
	TN *tmp1 = Build_TN_Like(result);
	TN *tmp2 = Build_TN_Like(result);
	Build_OP(TOP_unpckhpd, tmp1, src2, tmp1, ops);
	Expand_Neg(tmp2, tmp1, MTYPE_F8, ops);
	Build_OP(TOP_subsd, result, src1, src2, ops);
	Build_OP(TOP_unpcklpd, result, result, tmp2, ops);
      }
    } else 
      Build_OP(TOP_fsub128v64, result, src1, src2, ops);
    return;
  case OPC_F4MPY:
    if( Is_Target_SSE2() ){
      opc = TOP_mulss;
      break;
    }  /* fall thru */
  case OPC_F8MPY:
    if( Is_Target_SSE2() ){
      opc = TOP_mulsd;
      break;
    }  /* fall thru */
  case OPC_F10MPY:
    opc = TOP_fmul;
    break;
  case OPC_V32F4MPY:
  case OPC_V16F4MPY:
    opc = TOP_fmul128v32;
    break;
  case OPC_V32F8MPY:
  case OPC_V16F8MPY:
    opc = TOP_fmul128v64;
    break;
  case OPC_V32I2MPY:
  case OPC_V16I2MPY:
    opc = TOP_mul128v16;
    break;
  case OPC_V8I2MPY:
  case OPC_M8I2MPY:
    opc = TOP_pmullw;
    break;
  case OPC_V16I4MPY:
    if( Is_Target_SSE41() ) {
      opc = TOP_mul128v32;
    }
    else {
      FmtAssert( false, ("target does not support OPC_V16I4MPY") );
    }
    break;
  case OPC_F4MADD:	// (src2 * src3) + src1
  case OPC_F4NMADD:	// -((src2 * src3) + src1)
  case OPC_F4MSUB:	// (src2 * src3) - src1
  case OPC_F4NMSUB:	// -((src2 * src3) - src1)
  case OPC_F8MADD:	// (src2 * src3) + src1
  case OPC_F8NMADD:	// -((src2 * src3) + src1)
  case OPC_F8MSUB:	// (src2 * src3) - src1
  case OPC_F8NMSUB:	// -((src2 * src3) - src1)
    FmtAssert( false,
	       ("Expand_Flop: Unsupported opcode (%s)", OPCODE_name(opcode)) );
    break;
  case OPC_F4DIV:
    if( Is_Target_SSE2() ){
      opc = TOP_divss;
      break;
    }  /* fall thru */
  case OPC_F8DIV:
    if( Is_Target_SSE2() ){
      opc = TOP_divsd;
      break;
    }  /* fall thru */
  case OPC_F10DIV:
    opc = TOP_fdiv;
    break;
  case OPC_V32F4DIV:
  case OPC_V16F4DIV:
    opc = TOP_fdiv128v32;
    break;
  case OPC_V32F8DIV:
  case OPC_V16F8DIV:
    opc = TOP_fdiv128v64;
    break;
  case OPC_F4RECIP:
  case OPC_F8RECIP:
  case OPC_F10RECIP:
  case OPC_V16F4RECIP:
  case OPC_V16F8RECIP:
    Expand_Recip( result, src1, OPCODE_rtype(opcode), ops );
    return;
  case OPC_F4RSQRT:
  case OPC_F4ATOMIC_RSQRT:	// bug 6123
    if ( Is_Target_Orochi() && Is_Target_AVX() ) {
      Build_OP( TOP_rsqrtss, result, src1, src1, ops );
    } else {
      Build_OP( TOP_rsqrtss, result, src1, ops );
    }
    return;
  case OPC_V16F4RSQRT:
  case OPC_V16F4ATOMIC_RSQRT:	// bug 6123
    opc = TOP_frsqrt128v32;
    break;

  case OPC_V16C4MPY:
  case OPC_V16C8MPY:
    Expand_Complex_Multiply(opcode, result, src1, src2, ops);
    return;

  case OPC_V16C4DIV:
  case OPC_V16C8DIV:
    Expand_Complex_Divide(opcode, result, src1, src2, ops);
    return;

  case OPC_V8I1MPY:
  case OPC_M8I1MPY:
  {
      TN* tmp_a = Build_TN_Like( src1 );
      TN* tmp_b = Build_TN_Like( src1 );
      TN* tmp_c = Build_TN_Like( src1 );
      TN* tmp_d = Build_TN_Like( src1 );
      TN* tmp_e = Build_TN_Like( src1 );
      TN* tmp_f = Build_TN_Like( src1 );
      TN* tmp_g = Build_TN_Like( src1 );
      TN* tmp_h = Build_TN_Like( src1 );
      TN* tmp_i = Build_TN_Like( src1 );

      Exp_COPY(tmp_a, src1, ops);
      Exp_COPY(tmp_c, src2, ops);
    
      Build_OP(TOP_mov64_m, tmp_b, tmp_a, ops); 
      Build_OP(TOP_punpckhbw, tmp_b, tmp_b, tmp_a, ops);
      Build_OP(TOP_mov64_m, tmp_d, tmp_c, ops);
      Build_OP(TOP_punpckhbw, tmp_d, tmp_d, tmp_c, ops);
      Build_OP(TOP_mov64_m, tmp_e, tmp_a, ops);
      Build_OP(TOP_punpcklbw, tmp_e, tmp_e, tmp_a, ops);
      Build_OP(TOP_mov64_m, tmp_f, tmp_c, ops);
      Build_OP(TOP_punpcklbw, tmp_f, tmp_c, tmp_c, ops);
      Build_OP(TOP_mov64_m, tmp_g, tmp_b, ops);
      Build_OP(TOP_pmullw, tmp_g, tmp_g, tmp_d, ops);
      Build_OP(TOP_pmullw, tmp_f, tmp_f, tmp_e, ops);
      Build_OP(TOP_mov64_m, tmp_h, tmp_f, ops);
      Build_OP(TOP_punpckhbw, tmp_h, tmp_h, tmp_g, ops);
      Build_OP(TOP_punpcklbw, tmp_f, tmp_f, tmp_g, ops);
      Build_OP(TOP_mov64_m, tmp_i, tmp_f, ops);
      Build_OP(TOP_punpckhbw, tmp_i, tmp_i, tmp_h, ops);
      Build_OP(TOP_punpcklbw, tmp_f, tmp_f, tmp_h, ops);
      Build_OP(TOP_punpcklbw, result, tmp_f, tmp_i, ops);
  }
  return; 
	  
  case OPC_V16I1MPY:
  {
      TN* tmp_a = Build_TN_Like( src1 );
      TN* tmp_b = Build_TN_Like( src1 );
      TN* tmp_c = Build_TN_Like( src1 );
      TN* tmp_d = Build_TN_Like( src1 );
      TN* tmp_e = Build_TN_Like( src1 );
      TN* tmp_f = Build_TN_Like( src1 );
      TN* tmp_g = Build_TN_Like( src1 );
      TN* tmp_h = Build_TN_Like( src1 );
      TN* tmp_i = Build_TN_Like( src1 );
      TN* tmp_j = Build_TN_Like( src1 );

      Exp_COPY(tmp_a, src1, ops);
      Exp_COPY(tmp_c, src2, ops);
    
      Build_OP(TOP_movdq, tmp_b, tmp_a, ops); 
      Build_OP(TOP_punpckhbw128, tmp_b, tmp_b, tmp_a, ops);
      Build_OP(TOP_movdq, tmp_d, tmp_c, ops);
      Build_OP(TOP_punpckhbw128, tmp_d, tmp_d, tmp_c, ops);
      Build_OP(TOP_movdq, tmp_e, tmp_a, ops);
      Build_OP(TOP_punpcklbw128, tmp_e, tmp_e, tmp_a, ops);
      Build_OP(TOP_movdq, tmp_f, tmp_c, ops);
      Build_OP(TOP_punpcklbw128, tmp_f, tmp_c, tmp_c, ops);
      Build_OP(TOP_movdq, tmp_g, tmp_b, ops);
      Build_OP(TOP_pmullw128, tmp_g, tmp_g, tmp_d, ops);
      Build_OP(TOP_pmullw128, tmp_f, tmp_f, tmp_e, ops);
      Build_OP(TOP_movdq, tmp_h, tmp_f, ops);
      Build_OP(TOP_punpckhbw128, tmp_h, tmp_h, tmp_g, ops);
      Build_OP(TOP_punpcklbw128, tmp_f, tmp_f, tmp_g, ops);
      Build_OP(TOP_movdq, tmp_i, tmp_f, ops);
      Build_OP(TOP_punpckhbw128, tmp_i, tmp_i, tmp_h, ops);
      Build_OP(TOP_punpcklbw128, tmp_f, tmp_f, tmp_h, ops);
      Build_OP(TOP_movdq, tmp_j, tmp_f, ops);
      Build_OP(TOP_punpckhbw128, tmp_j, tmp_j, tmp_i, ops);
      Build_OP(TOP_punpcklbw128, tmp_f, tmp_f, tmp_i, ops);
      Build_OP(TOP_punpcklbw128, result, tmp_f, tmp_j, ops);
  }  
   return;
 
  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("Unimplemented flop: %s", OPCODE_name(opcode)));
  }

  Build_OP( opc, result, src1, src2, ops );
}

void
Expand_Replicate (OPCODE op, TN *result, TN *op1, OPS *ops)
{
  TN* tmp = Build_TN_Like(result);

  switch (op) {
  case OPC_V16C4F8REPLICA:
    Expand_Copy(result, op1, MTYPE_C4, ops);
    Build_OP(TOP_shufps, result, result, op1, Gen_Literal_TN(68, 4), ops);
    break;
  case OPC_V16I8I8REPLICA:
  {
    TY_IDX ty = MTYPE_To_TY( MTYPE_I8 );
    ST* st = Gen_Temp_Symbol( ty, "movd" );
    Allocate_Temp_To_Memory( st );
    Exp_Store( MTYPE_I8, op1, st, 0, ops, 0);
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      Exp_Load( MTYPE_F8, MTYPE_F8, tmp, st, 0, ops, 0);
      Build_OP(TOP_fmovddup, result, tmp, tmp, ops);
    } else {
      Exp_Load( MTYPE_F8, MTYPE_F8, tmp, st, 0, ops, 0);
      Expand_Copy(result, tmp, MTYPE_F8, ops);
      Build_OP(TOP_unpcklpd, result, result, tmp, ops);
    }
    break;
  }
  case OPC_V16F8F8REPLICA:
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      Build_OP(TOP_fmovddup, result, op1, op1, ops);
    } else {
      Expand_Copy(result, op1, MTYPE_F8, ops);
      Build_OP(TOP_unpcklpd, result, result, op1, ops);
    }
    break;
  case OPC_V16I4I4REPLICA:
  {
    TY_IDX ty = MTYPE_To_TY( MTYPE_I4 );
    ST* st = Gen_Temp_Symbol( ty, "movd" );
    Allocate_Temp_To_Memory( st );
    Exp_Store( MTYPE_I4, op1, st, 0, ops, 0);
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      Exp_Load( MTYPE_F4, MTYPE_F4, tmp, st, 0, ops, 0);
      Build_OP(TOP_unpcklps, result, tmp, tmp, ops);
      Build_OP(TOP_unpcklps, result, result, result, ops);
    } else {
      Exp_Load( MTYPE_F4, MTYPE_F4, tmp, st, 0, ops, 0);
      Expand_Copy(result, tmp, MTYPE_F4, ops);
      Build_OP(TOP_unpcklps, result, result, tmp, ops);
      Build_OP(TOP_unpcklps, result, result, result, ops);
    }
    break;
  }
  case OPC_V16F4F4REPLICA:
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      Build_OP(TOP_unpcklps, result, op1, op1, ops);
      Build_OP(TOP_unpcklps, result, result, result, ops);
    } else {
      Expand_Copy(result, op1, MTYPE_F4, ops);
      Build_OP(TOP_unpcklps, result, result, op1, ops);
      Build_OP(TOP_unpcklps, result, result, result, ops);
    }
    break;
  case OPC_V16I2I2REPLICA:     
  {
    TN* tmp_a = Build_TN_Like(result);
    Build_OP(TOP_movg2x, tmp, op1, ops);
    Build_OP(TOP_punpcklwd128, tmp_a, tmp, tmp, ops);
    Build_OP(TOP_pshufd, result, tmp_a, Gen_Literal_TN(0, 1), ops);
    break;
  }
  case OPC_V16I1I1REPLICA:
  {
    TN* tmp_a = Build_TN_Like(result);
    TN* tmp_b = Build_TN_Like(result);
    Build_OP(TOP_movg2x, tmp, op1, ops);
    Build_OP(TOP_punpcklbw128, tmp_a, tmp, tmp, ops);
    Build_OP(TOP_punpcklbw128, tmp_b, tmp_a, tmp_a, ops);
    Build_OP(TOP_pshufd, result, tmp_b, Gen_Literal_TN(0, 1), ops);
    break;
  }
  default:
    FmtAssert(FALSE, ("Handle this case"));
    break;
  }
  return;
}

void
Expand_Reduce_Add (OPCODE op, TN *result, TN *op1, OPS *ops)
{
  switch (op) {
  case OPC_F8V16F8REDUCE_ADD: 
  {
    TN* tmp = Build_TN_Like(op1);
    Build_OP(TOP_movapd, tmp, op1, ops);
    if ( Is_Target_SSE3() && !Is_Target_Orochi() ) {
      Build_OP(TOP_fhadd128v64, result, tmp, tmp, ops);
    } else {
      TN* tmp_a = Build_TN_Like(op1);
      Build_OP(TOP_unpckhpd, tmp_a, tmp, op1, ops);
      Build_OP(TOP_addsd, result, tmp, tmp_a, ops);
    }
    break;
  }
  case OPC_F4V16F4REDUCE_ADD: 
  {
    TN* tmp = Build_TN_Like(op1);
    Build_OP(TOP_movaps, tmp, op1, ops);
    if ( Is_Target_SSE3() && !Is_Target_Orochi() ) {
      Build_OP(TOP_fhadd128v32, tmp, op1, op1, ops);
      Build_OP(TOP_fhadd128v32, result, tmp, tmp, ops);
    } else {
      TN* tmp_a = Build_TN_Like(op1);
      TN* tmp_b = Build_TN_Like(op1);
      TN* tmp_c = Build_TN_Like(op1);
      TN* tmp_d = Build_TN_Like(op1);
      if (Is_Target_Orochi() && Is_Target_AVX()) {
        Build_OP(TOP_movhlps, tmp_a, tmp, tmp, ops);
      } else {
        Build_OP(TOP_movhlps, tmp_a, tmp, ops);
      }
      Build_OP(TOP_fadd128v32, tmp_b, tmp, tmp_a, ops);
      Build_OP(TOP_movaps, tmp_c, tmp_b, ops);
      Build_OP(TOP_shufps, tmp_d, tmp_c, tmp_c, Gen_Literal_TN(1, 1), ops);
      Build_OP(TOP_addss, result, tmp_b, tmp_d, ops);
    }
    break;
  }
  case OPC_I4V16I1REDUCE_ADD:
  {
    TN* tmp = Build_TN_Like(op1);
    TN* tmp_a = Build_TN_Like(op1);
    TN* tmp_b = Build_TN_Like(op1);
    TN* tmp_c = Build_TN_Like(op1);
    TN* tmp_d = Build_TN_Like(op1);
    TN* tmp_e = Build_TN_Like(op1);
    TN* tmp_f = Build_TN_Like(op1);
    TN* tmp_g = Build_TN_Like(op1);
    TN* tmp_h = Build_TN_Like(op1);
    TN* tmp_i = Build_TN_Like(op1);
    TN* tmp_j = Build_TN_Like(op1);
    TN* tmp_k = Build_TN_Like(op1);
    Build_OP(TOP_movdq, tmp, op1, ops);
    Build_OP(TOP_psrldq, tmp_a, tmp, Gen_Literal_TN(8, 1), ops);
    Build_OP(TOP_add128v8, tmp_b, tmp, tmp_a, ops);
    Build_OP(TOP_movdq, tmp_c, tmp_b, ops);
    Build_OP(TOP_psrldq, tmp_d, tmp_c, Gen_Literal_TN(4, 1), ops);
    Build_OP(TOP_add128v8, tmp_e, tmp_c, tmp_d, ops);
    Build_OP(TOP_movdq, tmp_f, tmp_e, ops);
    Build_OP(TOP_psrldq, tmp_g, tmp_f, Gen_Literal_TN(2, 1), ops);
    Build_OP(TOP_add128v8, tmp_h, tmp_f, tmp_g, ops);
    Build_OP(TOP_movdq, tmp_i, tmp_h, ops);
    Build_OP(TOP_psrldq, tmp_j, tmp_i, Gen_Literal_TN(1, 1), ops);
    Build_OP(TOP_add128v8, tmp_k, tmp_i, tmp_j, ops);
    Build_OP(TOP_movx2g, result, tmp_k, ops);
    break;
  }
  case OPC_I4V16I2REDUCE_ADD:
  {
    TN* tmp = Build_TN_Like(op1);
    TN* tmp_a = Build_TN_Like(op1);
    TN* tmp_b = Build_TN_Like(op1);
    TN* tmp_c = Build_TN_Like(op1);
    TN* tmp_d = Build_TN_Like(op1);
    TN* tmp_e = Build_TN_Like(op1);
    TN* tmp_f = Build_TN_Like(op1);
    TN* tmp_g = Build_TN_Like(op1);
    TN* tmp_h = Build_TN_Like(op1);
    TN* tmp_i = Build_TN_Like(op1);
    TN* tmp_j = Build_TN_Like(op1);
    TN* tmp_k = Build_TN_Like(op1);
    Build_OP(TOP_movdq, tmp, op1, ops);
    Build_OP(TOP_psrldq, tmp_a, tmp, Gen_Literal_TN(8, 1), ops);
    Build_OP(TOP_add128v16, tmp_b, tmp, tmp_a, ops);
    Build_OP(TOP_movdq, tmp_c, tmp_b, ops);
    Build_OP(TOP_psrldq, tmp_d, tmp_c, Gen_Literal_TN(4, 1), ops);
    Build_OP(TOP_add128v16, tmp_e, tmp_c, tmp_d, ops);
    Build_OP(TOP_movdq, tmp_f, tmp_e, ops);
    Build_OP(TOP_psrldq, tmp_g, tmp_f, Gen_Literal_TN(2, 1), ops);
    Build_OP(TOP_add128v16, tmp_h, tmp_f, tmp_g, ops);
    Build_OP(TOP_movx2g, result, tmp_h, ops);
    break;
  }
  case OPC_I4V16I4REDUCE_ADD:
  {
    TN* tmp = Build_TN_Like(op1);
    TN* tmp_a = Build_TN_Like(op1);
    TN* tmp_b = Build_TN_Like(op1);
    TN* tmp_c = Build_TN_Like(op1);
    TN* tmp_d = Build_TN_Like(op1);
    TN* tmp_e = Build_TN_Like(op1);
    Build_OP(TOP_movdq, tmp, op1, ops);
    Build_OP(TOP_psrldq, tmp_a, tmp, Gen_Literal_TN(8, 1), ops);
    Build_OP(TOP_add128v32, tmp_b, tmp, tmp_a, ops);
    Build_OP(TOP_movdq, tmp_c, tmp_b, ops);
    Build_OP(TOP_psrldq, tmp_d, tmp_c, Gen_Literal_TN(4, 1), ops);
    Build_OP(TOP_add128v32, tmp_e, tmp_c, tmp_d, ops);
    Build_OP(TOP_movx2g, result, tmp_e, ops);
    break;
  }
  case OPC_I8V16I8REDUCE_ADD:
  {
    TN* tmp = Build_TN_Like(op1);
    TN* tmp_a = Build_TN_Like(op1);
    TN* tmp_b = Build_TN_Like(op1);
    Build_OP(TOP_movdq, tmp, op1, ops);
    Build_OP(TOP_psrldq, tmp_a, tmp, Gen_Literal_TN(8, 1), ops);
    Build_OP(TOP_add128v64, tmp_b, tmp, tmp_a, ops);
    if (Is_Target_64bit())
      Build_OP(TOP_movx2g64, result, tmp_b, ops);
    else {
      TN* result_hi = Create_TN_Pair(result, MTYPE_I8);      
      TN *tmp_c = Build_TN_Like(op1);
      Build_OP(TOP_movx2g, result, tmp_b, ops);      
      Build_OP(TOP_psrlq128v64, tmp_c, tmp_b, Gen_Literal_TN(32, 4), ops);
      Build_OP(TOP_movx2g, result_hi, tmp_c, ops);      
    }
    break;
  }
  default:
    FmtAssert(FALSE, ("Expand_Reduce_Add: Unsupported opcode (%s)", OPCODE_name(op)));
  }
  return;
}

void
Expand_Reduce_Mpy (OPCODE op, TN *result, TN *op1, OPS *ops)
{
  switch (op) {
  case OPC_F8V16F8REDUCE_MPY: 
  {
    TN* tmp = Build_TN_Like(op1);
    TN* tmp_a = Build_TN_Like(op1);
    Build_OP(TOP_movapd, tmp, op1, ops);
    Build_OP(TOP_unpckhpd, tmp_a, tmp, tmp, ops);
    Build_OP(TOP_mulsd, result, tmp_a, tmp, ops);
    break;
  }
  case OPC_F4V16F4REDUCE_MPY:
  {
    TN* tmp = Build_TN_Like(op1);
    TN* tmp_a = Build_TN_Like(op1);
    TN* tmp_b = Build_TN_Like(op1);
    TN* tmp_c = Build_TN_Like(op1);
    TN* tmp_d = Build_TN_Like(op1);
    Build_OP(TOP_movaps, tmp, op1, ops);
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      Build_OP(TOP_movhlps, tmp_a, tmp, tmp, ops);
    } else {
      Build_OP(TOP_movhlps, tmp_a, tmp, ops);
    }
    Build_OP(TOP_fmul128v32, tmp_b, tmp, tmp_a, ops);
    Build_OP(TOP_movaps, tmp_c, tmp_b, ops);
    Build_OP(TOP_shufps, tmp_d, tmp_c, tmp_c, Gen_Literal_TN(1, 1), ops);
    Build_OP(TOP_mulss, result, tmp_b, tmp_d, ops);
    break;
  }
  case OPC_I4V16I2REDUCE_MPY:
  {
    TN* tmp = Build_TN_Like(op1);
    TN* tmp_a = Build_TN_Like(op1);
    TN* tmp_b = Build_TN_Like(op1);
    TN* tmp_c = Build_TN_Like(op1);
    TN* tmp_d = Build_TN_Like(op1);
    TN* tmp_e = Build_TN_Like(op1);
    TN* tmp_f = Build_TN_Like(op1);
    TN* tmp_g = Build_TN_Like(op1);
    TN* tmp_h = Build_TN_Like(op1);
    Build_OP(TOP_movdq, tmp, op1, ops);
    Build_OP(TOP_psrldq, tmp_a, tmp, Gen_Literal_TN(8, 1), ops); 
    Build_OP(TOP_mul128v16, tmp_b, tmp, tmp_a, ops);
    Build_OP(TOP_movdq, tmp_c, tmp_b, ops);
    Build_OP(TOP_psrldq, tmp_d, tmp_c, Gen_Literal_TN(4, 1), ops);
    Build_OP(TOP_mul128v16, tmp_e, tmp_c, tmp_d, ops);
    Build_OP(TOP_movdq, tmp_f, tmp_e, ops);
    Build_OP(TOP_psrldq, tmp_g, tmp_f, Gen_Literal_TN(2, 1), ops);
    Build_OP(TOP_mul128v16, tmp_h, tmp_f, tmp_g, ops); 
    Build_OP(TOP_movx2g, result, tmp_h, ops);
    break;
  }
  default:
    FmtAssert( FALSE,
	      ("Expand_Reduce_Mpy: Unsupported opcode (%s)", OPCODE_name(op) ) );
  }
  return;
}

void
Expand_Reduce_Max (OPCODE op, TN *result, TN *op1, OPS *ops)
{
  switch(op) {
  case OPC_F8V16F8REDUCE_MAX:
  {
    TN* tmp = Build_TN_Like(op1);
    TN* tmp_a = Build_TN_Like(op1);
    Build_OP(TOP_movapd, tmp, op1, ops);
    Build_OP(TOP_unpckhpd, tmp_a, tmp, tmp, ops);
    Build_OP(TOP_maxsd, result, tmp_a, tmp, ops);
    break;
  }
  case OPC_F4V16F4REDUCE_MAX:
  {
    TN* tmp = Build_TN_Like(op1);
    TN* tmp_a = Build_TN_Like(op1);
    TN* tmp_b = Build_TN_Like(op1);
    TN* tmp_c = Build_TN_Like(op1);
    TN* tmp_d = Build_TN_Like(op1);
    Build_OP(TOP_movaps, tmp, op1, ops);
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      Build_OP(TOP_movhlps, tmp_a, tmp, tmp, ops);
    } else {
      Build_OP(TOP_movhlps, tmp_a, tmp, ops);
    }
    Build_OP(TOP_fmax128v32, tmp_b, tmp, tmp_a, ops);
    Build_OP(TOP_movaps, tmp_c, tmp_b, ops);
    Build_OP(TOP_shufps, tmp_d, tmp_c, tmp_c, Gen_Literal_TN(1, 1), ops);
    Build_OP(TOP_maxss, result, tmp_c, tmp_d, ops);
    break;
  }
  case OPC_I4V16I4REDUCE_MAX:
  {
    TN* tmp1 = Build_TN_Like(op1);
    TN* tmp2 = Build_TN_Like(op1);
    TN* tmp3 = Build_TN_Like(op1);
    TN* tmp4 = Build_TN_Like(op1);
    TN* tmp5 = Build_TN_Like(op1);
    TN* tmp6 = Build_TN_Like(op1);
    TN* tmp7 = Build_TN_Like(op1);
    TN* tmp8 = Build_TN_Like(op1);
    TN* tmp9 = Build_TN_Like(op1);
    TN* tmp10 = Build_TN_Like(op1);
    TN* tmp11 = Build_TN_Like(op1);
    TN* tmp12 = Build_TN_Like(op1);
    TN* tmp13 = Build_TN_Like(op1);
    TN* tmp14 = Build_TN_Like(op1);
    Build_OP(TOP_movdq, tmp1, op1, ops);
    Build_OP(TOP_movdq, tmp2, op1, ops);
    Build_OP(TOP_psrldq, tmp3, tmp1, Gen_Literal_TN(8, 1), ops);
    Build_OP(TOP_xor128v32, tmp4, op1, tmp3, ops);
    Build_OP(TOP_cmpgt128v32, tmp5, tmp2, tmp3, ops);
    Build_OP(TOP_and128v32, tmp6, tmp5, tmp4, ops);
    Build_OP(TOP_xor128v32, tmp7, tmp6, tmp3, ops);
    Build_OP(TOP_movdq, tmp8, tmp7, ops);
    Build_OP(TOP_movdq, tmp9, tmp7, ops);
    Build_OP(TOP_psrldq, tmp10, tmp8, Gen_Literal_TN(4, 1), ops);
    Build_OP(TOP_xor128v32, tmp11, tmp7, tmp10, ops);
    Build_OP(TOP_cmpgt128v32, tmp12, tmp9, tmp10, ops);
    Build_OP(TOP_and128v32, tmp13, tmp12, tmp11, ops);
    Build_OP(TOP_xor128v32, tmp14, tmp13, tmp10, ops);
    Build_OP(TOP_movx2g, result, tmp14, ops);
    break;
  }
  default: 
    FmtAssert( FALSE,
	       ("Expand_Reduce_Max: Unsupported opcode (%s)", OPCODE_name(op) ) );
  }
  return;
}

void
Expand_Reduce_Min (OPCODE op, TN *result, TN *op1, OPS *ops)
{
  switch(op) {
  case OPC_F8V16F8REDUCE_MIN:
  {
    TN* tmp = Build_TN_Like(op1);
    TN* tmp_a = Build_TN_Like(op1);
    Build_OP(TOP_movapd, tmp, op1, ops);
    Build_OP(TOP_unpckhpd, tmp_a, tmp, tmp, ops);
    Build_OP(TOP_minsd, result, tmp_a, tmp, ops);
    break;
  }
  case OPC_F4V16F4REDUCE_MIN:
  {
    TN* tmp = Build_TN_Like(op1);
    TN* tmp_a = Build_TN_Like(op1);
    TN* tmp_b = Build_TN_Like(op1);
    TN* tmp_c = Build_TN_Like(op1);
    TN* tmp_d = Build_TN_Like(op1);
    Build_OP(TOP_movaps, tmp, op1, ops);
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      Build_OP(TOP_movhlps, tmp_a, tmp, tmp, ops);
    } else {
      Build_OP(TOP_movhlps, tmp_a, tmp, ops);
    }
    Build_OP(TOP_fmin128v32, tmp_b, tmp, tmp_a, ops);
    Build_OP(TOP_movaps, tmp_c, tmp_b, ops);
    Build_OP(TOP_shufps, tmp_d, tmp_c, tmp_c, Gen_Literal_TN(1, 1), ops);
    Build_OP(TOP_minss, result, tmp_c, tmp_d, ops);
    break;
  }
  case OPC_I4V16I4REDUCE_MIN:
  {    
    TN* tmp1 = Build_TN_Like(op1);
    TN* tmp2 = Build_TN_Like(op1);
    TN* tmp3 = Build_TN_Like(op1);
    TN* tmp4 = Build_TN_Like(op1);
    TN* tmp5 = Build_TN_Like(op1);
    TN* tmp6 = Build_TN_Like(op1);
    TN* tmp7 = Build_TN_Like(op1);
    TN* tmp8 = Build_TN_Like(op1);
    TN* tmp9 = Build_TN_Like(op1);
    TN* tmp10 = Build_TN_Like(op1);
    TN* tmp11 = Build_TN_Like(op1);
    TN* tmp12 = Build_TN_Like(op1);
    TN* tmp13 = Build_TN_Like(op1);
    TN* tmp14 = Build_TN_Like(op1);
    Build_OP(TOP_movdq, tmp1, op1, ops);
    Build_OP(TOP_movdq, tmp2, op1, ops);
    Build_OP(TOP_psrldq, tmp3, tmp1, Gen_Literal_TN(8, 1), ops);
    Build_OP(TOP_cmpgt128v32, tmp4, tmp2, tmp3, ops);   
    Build_OP(TOP_xor128v32, tmp5, tmp3, op1, ops);
    Build_OP(TOP_and128v32, tmp6, tmp5, tmp4, ops);
    Build_OP(TOP_xor128v32, tmp7, tmp6, op1, ops);
    Build_OP(TOP_movdq, tmp8, tmp7, ops);
    Build_OP(TOP_movdq, tmp9, tmp7, ops);
    Build_OP(TOP_psrldq, tmp10, tmp8, Gen_Literal_TN(4, 1), ops);
    Build_OP(TOP_cmpgt128v32, tmp11, tmp9, tmp10, ops);    
    Build_OP(TOP_xor128v32, tmp12, tmp7, tmp10, ops);
    Build_OP(TOP_and128v32, tmp13, tmp12, tmp11, ops);
    Build_OP(TOP_xor128v32, tmp14, tmp13, tmp7, ops);
    Build_OP(TOP_movx2g, result, tmp14, ops);
    break;
  }
  default: 
    FmtAssert( FALSE,
	       ("Expand_Reduce_Min: Unsupported opcode (%s)", OPCODE_name(op) ) );
  }
  return;
}

void
Expand_Shuffle (OPCODE opc, TN* result, TN* op1, VARIANT variant, OPS *ops)
{
  FmtAssert(variant == V_SHUFFLE_REVERSE, ("NYI"));
  switch(opc) {
  case OPC_V16C8V16C8SHUFFLE:
    Build_OP(TOP_shufpd, result, op1, op1, Gen_Literal_TN(0x1, 1), ops);
    break;    
  case OPC_V16F4V16F4SHUFFLE:
  case OPC_V16I4V16I4SHUFFLE:
    Build_OP(TOP_pshufd, result, op1, Gen_Literal_TN(0x1B, 1), ops);
    break;

  case OPC_V8I4V8I4SHUFFLE:
  case OPC_V8F4V8F4SHUFFLE:
    // Transpose elements 0 and 1. The content of element 2 and 3 are 
    // immaterial.
    //
    Build_OP (TOP_pshufd, result, op1, Gen_Literal_TN(0x1, 1), ops);
    break;

  case OPC_V16I8V16I8SHUFFLE:
  case OPC_V16F8V16F8SHUFFLE:
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      Build_OP(TOP_movhlps, result, op1, op1, ops);
      Build_OP(TOP_movlhps, result, result, op1, ops);
    } else {
      Build_OP(TOP_movhlps, result, op1, ops);
      Build_OP(TOP_movlhps, result, op1, ops);
    }
    Set_OP_cond_def_kind( OPS_last(ops), OP_ALWAYS_COND_DEF );
    break;    
  case OPC_V16I2V16I2SHUFFLE:
    {
      TN* tmp1 = Build_TN_Like(result);
      TN* tmp2 = Build_TN_Like(result);
      if (Is_Target_Orochi() && Is_Target_AVX()) {
        Build_OP(TOP_movhlps, tmp1, op1, op1, ops);
        Build_OP(TOP_movlhps, tmp1, tmp1, op1, ops);
      } else {
        Build_OP(TOP_movhlps, tmp1, op1, ops);
        Build_OP(TOP_movlhps, tmp1, op1, ops);
      }
      Set_OP_cond_def_kind( OPS_last(ops), OP_ALWAYS_COND_DEF );
      Build_OP(TOP_pshuflw, tmp2, tmp1, Gen_Literal_TN(0x1B, 1), ops);
      Build_OP(TOP_pshufhw, result, tmp2, Gen_Literal_TN(0x1B, 1), ops);
      break;
    }
  default:
    FmtAssert(FALSE, ("expand %s, NYI", OPCODE_name(opc)));
  }
  return;
}

extern void
Init_CG_Expand (void)
{
  static BOOL Initialized = FALSE;

  // per PU:
  Trace_Exp = Get_Trace (TP_CGEXP, 1);
  /* whirl2ops uses -ttexp:2 */
  Trace_Exp2 = Get_Trace (TP_CGEXP, 4);
  
  if (Initialized) return;
  Initialized = TRUE;
  // once per file:
  Initialize_Branch_Variants();
}


/* ======================================================================
 * Exp_COPY_Ext
 * 
 * Generate a register transfer copy from 'src_tn' to 'tgt_tn' with
 * appropriate sign/zero extension.
 * ======================================================================*/
void 
Exp_COPY_Ext (TOP opcode, TN *tgt_tn, TN *src_tn, OPS *ops)
{
  TOP new_op;
  switch (opcode) {
  case TOP_ldx8_32:
  case TOP_ldxx8_32:
  case TOP_ld8_32_n32:
  case TOP_ld8_32:
  case TOP_movsbl:
    new_op = TOP_movsbl;
    break;
  case TOP_ldu8_32_n32:
  case TOP_ldu8_32:
  case TOP_ldxu8_32:
  case TOP_ldxxu8_32:
  case TOP_movzbl:
    new_op = TOP_movzbl;
    break;
  case TOP_ld16_32_n32:
  case TOP_ld16_32:
  case TOP_ldx16_32:
  case TOP_ldxx16_32:
  case TOP_movswl:
    new_op = TOP_movswl;
    break;
  case TOP_ldu16_32_n32:
  case TOP_ldu16_32:
  case TOP_ldxu16_32:
  case TOP_ldxxu16_32:
  case TOP_movzwl:
    new_op = TOP_movzwl;
    break;
  case TOP_ld8_64:
  case TOP_ldx8_64:
  case TOP_ldxx8_64:
  case TOP_ld8_64_off:
  case TOP_movsbq:
    new_op = TOP_movsbq;
    break;
  case TOP_ldu8_64:
  case TOP_ldxu8_64:
  case TOP_ldxxu8_64:
  case TOP_ldu8_64_off:
  case TOP_movzbq:
    new_op = TOP_movzbq;
    break;
  case TOP_ld16_64:
  case TOP_ldx16_64:
  case TOP_ldxx16_64:
  case TOP_ld16_64_off:
  case TOP_movswq:
    new_op = TOP_movswq;
    break;
  case TOP_ldu16_64:
  case TOP_ldxu16_64:
  case TOP_ldxxu16_64:
  case TOP_ldu16_64_off:
  case TOP_movzwq:
    new_op = TOP_movzwq;
    break;
  case TOP_ld32_64:
  case TOP_ldx32_64:
  case TOP_ldxx32_64:
  case TOP_ld32_64_off:
  case TOP_movslq:
    new_op = TOP_movslq;
    break;
  case TOP_ld32_n32:
  case TOP_mov32:
    new_op = TOP_mov32;
    break;
  case TOP_fmovsldup:
  case TOP_fmovsldupx:
  case TOP_fmovsldupxx:
  case TOP_fmovsldupxxx:
    new_op = TOP_fmovsldup;
    break;
  case TOP_vmovsldup:
  case TOP_vmovsldupx:
  case TOP_vmovsldupxx:
  case TOP_vmovsldupxxx:
    new_op = TOP_vmovsldup;
    break;
  case TOP_fmovshdup:
  case TOP_fmovshdupx:
  case TOP_fmovshdupxx:
  case TOP_fmovshdupxxx:
    new_op = TOP_fmovshdup;
    break;
  case TOP_vmovshdup:
  case TOP_vmovshdupx:
  case TOP_vmovshdupxx:
  case TOP_vmovshdupxxx:
    new_op = TOP_vmovshdup;
    break;
  case TOP_fmovddupx:
  case TOP_fmovddupxx:
  case TOP_fmovddupxxx:
    new_op = TOP_fmovddup;
    break;
  case TOP_vmovddupx:
  case TOP_vmovddupxx:
  case TOP_vmovddupxxx:
    new_op = TOP_vmovddup;
    break;

  default:
    FmtAssert( FALSE, ("Exp_COPY_Ext: Unsupported opcode (%s)", TOP_Name(opcode)) );
  }
  Build_OP( new_op, tgt_tn, src_tn, ops );
  // TODO: Are the extensions copies?
  // Set_OP_copy (OPS_last(ops));
}

/* ======================================================================
 * Exp_COPY
 * 
 * Generate a register transfer copy from 'src_tn' to 'tgt_tn'. 
 * ======================================================================*/
void 
Exp_COPY (TN *tgt_tn, TN *src_tn, OPS *ops, BOOL copy_pair)
{
  // Warning: Don't return NOP even if src_tn == tgt_tn.  EBO expects a real
  // move OP in order to track the usage info of src_tn.

  // In m64, src_tn and tgt_tn can have different sizes.  If the sizes differ,
  // use 64-bit copy.  (See example in bug 14429.)
  // 
  // In m32, EBO can copy from 8-byte TN to 4-byte TN.  This means a 4-byte
  // copy where the src is a 4-byte hi/lo part of a 8-byte value (bug 14418).
  const BOOL is_64bit = Is_Target_64bit() ?
			  (TN_size(src_tn) == 8) :
			  (TN_size(src_tn) == 8 && TN_size(tgt_tn) == 8);
  const BOOL is_128bit = (TN_size(src_tn) == 16);

  if( TN_is_constant(src_tn) ){
    FmtAssert (TN_has_value(src_tn), ("Exp_COPY: illegal source tn"));
    /* expansion for INTCONST doesn't depend on size */
    Exp_OP1 (OPC_I4INTCONST, tgt_tn, src_tn, ops);

  } else {
    ISA_REGISTER_CLASS tgt_rc = TN_register_class(tgt_tn);
    ISA_REGISTER_CLASS src_rc = TN_register_class(src_tn);

    if (tgt_rc == src_rc && tgt_rc == ISA_REGISTER_CLASS_integer) {
      if(CG_strcmp_expand && Is_Target_32bit())
        Build_OP(TOP_mov32, tgt_tn, src_tn, ops );
      else
        Build_OP( is_64bit ? TOP_mov64 : TOP_mov32, tgt_tn, src_tn, ops );
      Set_OP_copy (OPS_last(ops));

      // Copy the hi part of a TN pair.  Bug 8755.
      if (copy_pair) {
	TN *hi_src_tn = NULL;
	TN *hi_tgt_tn = NULL;
	if (!TN_is_dedicated(src_tn) &&
	    (hi_src_tn = Get_TN_Pair(src_tn)) != NULL) {
	  hi_tgt_tn = Get_TN_Pair(tgt_tn);
	} else if (!TN_is_dedicated(tgt_tn) &&
		   (hi_tgt_tn = Get_TN_Pair(tgt_tn)) != NULL) {
	  hi_src_tn = Get_TN_Pair(src_tn);
	}
	if (hi_src_tn != NULL ||
	    hi_tgt_tn != NULL) {
	  Is_True((hi_tgt_tn != NULL) && (hi_src_tn != NULL),
		  ("Exp_COPY: src or target TN pair missing"));
          if(CG_strcmp_expand && Is_Target_32bit())
	    Build_OP(TOP_mov32, hi_tgt_tn, hi_src_tn, ops);
          else
             Build_OP(is_64bit ? TOP_mov64 : TOP_mov32, hi_tgt_tn, hi_src_tn, ops);
	  Set_OP_copy (OPS_last(ops));
	}
      }
    } else if (tgt_rc == src_rc && tgt_rc == ISA_REGISTER_CLASS_float) {
      /* dedicated TNs always have size 8, so need to check both TNs */
      if( Is_Target_Barcelona() || Is_Target_EM64T() ||
          Is_Target_Wolfdale()  || Is_Target_Core()  ||
          Is_Target_Orochi() ){
        Build_OP(TOP_movaps, tgt_tn, src_tn, ops);
      } else {
        Build_OP( TOP_movdq, tgt_tn, src_tn, ops);
      }
      Set_OP_copy (OPS_last(ops));

    } else if( tgt_rc == src_rc && tgt_rc == ISA_REGISTER_CLASS_x87 ){
      Build_OP( TOP_fmov, tgt_tn, src_tn, ops );
      Set_OP_copy (OPS_last(ops));

    } else if( tgt_rc == src_rc && tgt_rc == ISA_REGISTER_CLASS_mmx ){
      Build_OP( TOP_mov64_m, tgt_tn, src_tn, ops );
      Set_OP_copy (OPS_last(ops));

    } else if( tgt_rc == ISA_REGISTER_CLASS_x87 &&
	       src_rc == ISA_REGISTER_CLASS_float ){
      Expand_Float_To_Float( tgt_tn, src_tn, MTYPE_F10,
			     TN_size(src_tn) == 8 ? MTYPE_F8 : MTYPE_F4,
			     ops );

    } else if( tgt_rc == ISA_REGISTER_CLASS_float &&
	       src_rc == ISA_REGISTER_CLASS_x87 ){
      Expand_Float_To_Float( tgt_tn, src_tn,
			     TN_size(tgt_tn) == 8 ? MTYPE_F8 : MTYPE_F4,
			     MTYPE_F10,
			     ops );

    } else if( tgt_rc == ISA_REGISTER_CLASS_integer &&
	       src_rc == ISA_REGISTER_CLASS_float ){
      // Exposed by Bug 955
      Expand_Float_To_Int_Trunc( tgt_tn, src_tn, 
				 TN_size(tgt_tn) == 8 ? MTYPE_I8 : MTYPE_I4,
				 TN_size(src_tn) == 8 ? MTYPE_F8 : MTYPE_F4,
				 ops );

    } else if( src_rc == ISA_REGISTER_CLASS_integer &&
	       tgt_rc == ISA_REGISTER_CLASS_mmx) {
      // mov int64 to mmx
      if (Is_Target_64bit()) {
        Build_OP (TOP_movi64_2m, tgt_tn, src_tn, ops);
      } else {
        // Move the 64-bit value via memory because there is no 64-bit int
        // register.
        TN *base_tn, *ofst_tn;
        Store_To_Temp_Stack(MTYPE_I8, src_tn, "int64_2_mmx", &base_tn, &ofst_tn,
                            ops);
        Build_OP(TOP_ld64_2m, tgt_tn, base_tn, ofst_tn, ops);
      }
    } else if( src_rc == ISA_REGISTER_CLASS_float &&
	       tgt_rc == ISA_REGISTER_CLASS_mmx) {
      // mov sse to mmx
      Build_OP (TOP_movdq2q, tgt_tn, src_tn, ops);
    } else if( src_rc == ISA_REGISTER_CLASS_mmx &&
	       tgt_rc == ISA_REGISTER_CLASS_float) {
      // mov mmx to sse
      Build_OP (TOP_movq2dq, tgt_tn, src_tn, ops);
    } else {
      /* dedicated TNs always have size 8, so need to check both TNs */
      FmtAssert( FALSE, ("UNIMPLEMENTED") );
    }
  }
}

static ST *tmp_apply_arg = NULL;
void
Generate_Temp_Apply_Arg ( )
{
  TY_IDX tyi;
  TY& ty = New_TY(tyi);
  TY_Init(ty, Is_Target_32bit() ? 144 : 192, KIND_STRUCT, MTYPE_M,
          Save_Str("__apply_arg"));
  Set_TY_align(tyi, 16);
  tmp_apply_arg = New_ST(CURRENT_SYMTAB);
  ST_Init(tmp_apply_arg, TY_name_idx(ty),
          CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, tyi);
  Set_ST_is_temp_var(tmp_apply_arg);
  Allocate_Object(tmp_apply_arg);
}


static void Expand_INTRN_ANINT( TN* result, TN* src, TYPE_ID mtype, OPS* ops )
{
  FmtAssert( mtype == MTYPE_F8, ("Expand_INTRN_ANINT: not double type") );

  if( Fast_ANINT_Allowed ){
    const double rnd_const = 6755399441055744.0;  /* 0.75d0 * 2d0**53  */
    TN* con_tn = Build_TN_Of_Mtype( mtype );
    Expand_Const( con_tn,
		  Gen_Const_Symbol_TN( 0, rnd_const, mtype ),
		  mtype, ops );

    Build_OP( TOP_addsd, result, src, con_tn, ops );
    Build_OP( TOP_subsd, result, result, con_tn, ops );

  } else {

    /* Prepapre all the required data. */

    TN* tmp0 = Build_TN_Like( result );
    TN* tmp4 = Build_TN_Like( result );
    TN* tmp7 = Build_TN_Like( result );
    TN* tmp2 = Build_TN_Like( result );
    TN* tmp6 = Build_TN_Like( result );
    TN* tmp3 = Build_TN_Like( result );

    TN* sign_mask = Build_TN_Of_Mtype( mtype );
    Expand_Const( sign_mask,
		  Gen_Const_Symbol_TN( 0x8000000000000000ULL, 0.0, MTYPE_I8 ),
		  mtype, ops );
    TN* two_exp_52 = Build_TN_Of_Mtype( mtype );
    Expand_Const( two_exp_52,
		  Gen_Const_Symbol_TN( 0, 4503599627370496.0, mtype ),
		  mtype, ops );

    TN* point_5 = Build_TN_Of_Mtype( mtype );
    Expand_Const( point_5,
		  Gen_Const_Symbol_TN( 0, 0.5, mtype ),
		  mtype, ops );

    TN* neg_point_5 = Build_TN_Of_Mtype( mtype );
    Expand_Const( neg_point_5,
		  Gen_Const_Symbol_TN( 0, -0.5, mtype ),
		  mtype, ops );

    /* Emit the algorithm. */

    Build_OP( TOP_andpd, tmp0, sign_mask, src, ops );
    Build_OP( TOP_xorpd, tmp4, tmp0, src, ops );
    Build_OP( TOP_addsd, tmp7, tmp4, two_exp_52, ops );
    Build_OP( TOP_subsd, tmp7, tmp7, two_exp_52, ops );
    Build_OP( TOP_addsd, tmp2, point_5, point_5, ops );
    Build_OP( TOP_subsd, tmp6, tmp7, tmp4, ops );
    Build_OP( TOP_cmpsd, tmp3,
	      tmp6, neg_point_5, Generate_Cmp_Ctrl_TN(OPR_LE), ops );
    Build_OP( TOP_andpd, tmp3, tmp3, tmp2, ops );
    Build_OP( TOP_cmpsd, tmp6,
	      tmp6, point_5, Generate_Cmp_Ctrl_TN(OPR_GT), ops );
    Build_OP( TOP_andpd, tmp6, tmp6, tmp2, ops );
    Build_OP( TOP_subsd, result, tmp7, tmp6, ops );
    Build_OP( TOP_addsd, result, result, tmp3, ops );
    Build_OP( TOP_orpd,  result, result, tmp0, ops );
  }
}

static void
Expand_Count_Trailing_Zeros  (TN *result, TN *op, TYPE_ID mtype, OPS *ops)
{
  TOP top = TOP_UNDEFINED;
  switch (mtype)
  {
    case MTYPE_I4:
    case MTYPE_U4:
      top = TOP_bsf32;
      break;

    case MTYPE_I8:
    case MTYPE_U8:
      top = TOP_bsf64;
      break;
    default:
      Fail_FmtAssertion ("Expand_Count_Trailing_Zeros: unexpected mtype");
  }
  Build_OP (top, result, op, ops);
  return;
}

// If safezero is TRUE, then be careful to map 0 --> MTYPE_bit_size(mtype).
static void
Expand_Count_Leading_Zeros (TN *result, TN *op, TYPE_ID mtype,
			    BOOL safezero, OPS *ops)
{
  // TN1 :- ldc32 (0x3f)
  // TN2 :- bsr32 TN_src;
  // TN2 :- cmove TN1 (%rflags)
  // TN3 :- xori32 TN2 (0x1f)
  if ( mtype != MTYPE_I1 && mtype != MTYPE_U1 &&
       mtype != MTYPE_I2 && mtype != MTYPE_U2 &&
       mtype != MTYPE_I4 && mtype != MTYPE_U4 &&
       mtype != MTYPE_I8 && mtype != MTYPE_U8 )
    Fail_FmtAssertion("Expand_Count_Leading_Zeros: unexpected mtype");

  // Bug 14167: Don't use bsr64 on 32-bit target
  if ( (mtype == MTYPE_I8 || mtype == MTYPE_U8) && Is_Target_32bit() ) {
    Expand_Split_Leading_Zeros( result, op, mtype, safezero, ops );
    return;
  }

  INT bitsize = MTYPE_bit_size(mtype);
  TN *tmp1 = op;
  if (bitsize < 32) {
    tmp1 = Build_TN_Of_Mtype( MTYPE_I4 );
    Expand_Binary_And( tmp1, op, Gen_Literal_TN( (1 << bitsize) - 1, 4 ),
		       MTYPE_I4, ops );
  }
  TN *tmp3, *rflags = Rflags_TN();
  if (safezero) {
    tmp3 = Build_TN_Of_Mtype( MTYPE_I4 );
    Exp_Immediate( tmp3, Gen_Literal_TN(2 * bitsize - 1, 4), ops );
  }
  TN *tmp2 = Build_TN_Of_Mtype( MTYPE_I4 );
  Build_OP( bitsize == 64 ? TOP_bsr64 : TOP_bsr32, tmp2, tmp1, ops );
  if (safezero) {
    Expand_Cmov( TOP_cmove, tmp2, tmp3, rflags, ops );
  }
  Expand_Binary_Xor( result, tmp2, Gen_Literal_TN(bitsize - 1, 4),
		     MTYPE_I4, ops );
}

// Exapnd blendv intrinsic (BLENDVPD, BLENDVPS, PBLENDVB)
static void
Expand_INTRN_BLENDV(INTRINSIC id, TN* result, TN* op0, TN* op1, TN* op2, OPS* ops)
{
  // blendv uses xmm0 as the mask, which is a hidden operand
  TN* xmm0 = Build_TN_Like(op2);
  Exp_COPY(xmm0, op2, ops);
  TN* res = Build_TN_Like(result);
  switch ( id ) {
  case INTRN_BLENDVPD:
    Build_OP(TOP_fblendv128v64, res, op0, xmm0, op1, ops );
    break;
  case INTRN_BLENDVPS:
    Build_OP(TOP_fblendv128v32, res, op0, xmm0, op1, ops );
    break;
  case INTRN_PBLENDVB128:
    Build_OP(TOP_blendv128v8, res, op0, xmm0, op1, ops );
    break;
  default:
    FmtAssert( FALSE, ("Unknown intrn id in Expand_INTRN_BLENDV") );
  }
  Exp_COPY(result, res, ops);
}

static void
Expand_INTRN_PCMPESTR(INTRINSIC id, TN* result, TN* op0, TN* op1, TN* op2, TN* op3, TN* op4, OPS* ops)
{
  TN* len1 = Build_TN_Like(op1);
  Exp_COPY(len1, op1, ops);
  TN* len2 = Build_TN_Like(op3);
  Exp_COPY(len2, op3, ops);
  if ( id == INTRN_PCMPESTRM128 ) {
    TN* res  = Build_TN_Like(result);
    Build_OP(TOP_cmpestrm, res, Rflags_TN(), op0, len1, op2, len2, op4, ops );
    Exp_COPY(result, res, ops);
  }
  else if ( id == INTRN_PCMPESTRI128 ) {
    TN* res  = Build_TN_Like(result);
    Build_OP(TOP_cmpestri, res, Rflags_TN(), op0, len1, op2, len2, op4, ops );
    Exp_COPY(result, res, ops);
  }
  else {
    TN* res  = Build_TN_Of_Mtype(MTYPE_I4);
    Build_OP(TOP_cmpestri, res, Rflags_TN(), op0, len1, op2, len2, op4, ops );
    TN *flag = Build_TN_Of_Mtype(MTYPE_I1);
    switch ( id ) {
    case INTRN_PCMPESTRA128:
      Build_OP(TOP_seta, flag, Rflags_TN(), ops);
      break;
    case INTRN_PCMPESTRC128:
      Build_OP(TOP_setc, flag, Rflags_TN(), ops);
      break;
    case INTRN_PCMPESTRO128:
      Build_OP(TOP_seto, flag, Rflags_TN(), ops);
      break;
    case INTRN_PCMPESTRS128:
      Build_OP(TOP_sets, flag, Rflags_TN(), ops);
      break;
    case INTRN_PCMPESTRZ128:
      Build_OP(TOP_setz, flag, Rflags_TN(), ops);
      break;
    default:
      FmtAssert( FALSE, ("Unknown intrn id in Expand_INTRN_PCMPESTRI_Flag") );
    }
    Build_OP(TOP_movzbl, result, flag, ops);
  }
}

static void
Expand_INTRN_PCMPISTR(INTRINSIC id, TN* result, TN* op0, TN* op1, TN* op2, OPS* ops)
{
  if ( id == INTRN_PCMPISTRM128 ) {
    TN* res  = Build_TN_Like(result);
    Build_OP(TOP_cmpistrm, res, Rflags_TN(), op0, op1, op2, ops );
    Exp_COPY(result, res, ops);
  }
  else if ( id == INTRN_PCMPISTRI128 ) {
    TN* res  = Build_TN_Like(result);
    Build_OP(TOP_cmpistri, res, Rflags_TN(), op0, op1, op2, ops );
    Exp_COPY(result, res, ops);
  }
  else {
    TN* res  = Build_TN_Of_Mtype(MTYPE_I4);
    Build_OP(TOP_cmpistri, res, Rflags_TN(), op0, op1, op2, ops );
    TN *flag = Build_TN_Of_Mtype(MTYPE_I1);
    switch ( id ) {
    case INTRN_PCMPISTRA128:
      Build_OP(TOP_seta, flag, Rflags_TN(), ops);
      break;
    case INTRN_PCMPISTRC128:
      Build_OP(TOP_setc, flag, Rflags_TN(), ops);
      break;
    case INTRN_PCMPISTRO128:
      Build_OP(TOP_seto, flag, Rflags_TN(), ops);
      break;
    case INTRN_PCMPISTRS128:
      Build_OP(TOP_sets, flag, Rflags_TN(), ops);
      break;
    case INTRN_PCMPISTRZ128:
      Build_OP(TOP_setz, flag, Rflags_TN(), ops);
      break;
    default:
      FmtAssert( FALSE, ("Unknown intrn id in Expand_INTRN_PCMPESTRI_Flag") );
    }
    Build_OP(TOP_movzbl, result, flag, ops);
  }
}

void
Exp_Intrinsic_Op (INTRINSIC id, TN *result, TN *op0, TN *op1, TN *op2, TN *op3, TN *op4, TYPE_ID mtype, OPS *ops)
{

  enum XOP_VPCOMP_OP {
    VPCOM_LT = 0,    /* less than */
    VPCOM_LE = 1,    /* less than or equal */
    VPCOM_GT = 2,    /* greater than */
    VPCOM_GE = 3,    /* greater than or equal */
    VPCOM_EQ = 4,    /* equal */
    VPCOM_NE = 5,    /* not euqal */
    VPCOM_FALSE = 6, /* false */
    VPCOM_TRUE = 7,  /* ture */
  };

  TN* rflags = Rflags_TN();
  TN* result_tmp = Build_TN_Of_Mtype( MTYPE_U1 );
  BOOL need_zero_ext = FALSE;	// if zero ext is needed, if yes, `movzb{l|q} result_tmp, result' is appended
  const BOOL is_double = MTYPE_is_size_double(mtype);
  const TOP cmp_opcode = ( MTYPE_is_F10(mtype) || !Is_Target_SSE2() )
    ? TOP_fucomi : ( is_double ? TOP_comisd : TOP_comiss );

  if (INTRN_return_kind(id) == IRETURN_M8I1 ||
      INTRN_return_kind(id) == IRETURN_M8I2 ||
      INTRN_return_kind(id) == IRETURN_M8I4) { // convert operands to MMX TNs
    FmtAssert(TN_register_class(result) == ISA_REGISTER_CLASS_mmx, ("bad class"));

    if (TN_register_class(op0) != ISA_REGISTER_CLASS_mmx) {
      TN *tmp0 = Build_TN_Like(result);
      Exp_COPY( tmp0, op0, ops );
      op0 = tmp0;
    }
    if (id != INTRN_PSHUFW && 
        id != INTRN_PSHUFD &&
        id != INTRN_VEC_EXT_V2SI &&
	TN_register_class(op1) != ISA_REGISTER_CLASS_mmx) {
      TN *tmp1 = Build_TN_Like(result);
      Exp_COPY( tmp1, op1, ops );
      op1 = tmp1;
    }
  }
  switch ( id ) {
  default: FmtAssert( FALSE,
		      ("Exp_Intrinsic_Op: unsupported intrinsic (%s)",
		       INTRN_rt_name(id)) );
    // Note: Frontend generates INTRN_CLZ/INTRN_CTZ64 for library calls,
    // and INTRN_CLZ32/INTRN_CTZ for clz/dclz/ctz/dctz instruction.
    // (Reasons related to x86.)  mtype is the rtype from op0.
  case INTRN_CTZ:
    Expand_Count_Trailing_Zeros (result, op0, mtype, ops);
    break;
  case INTRN_I1LEADZ:
  case INTRN_I2LEADZ:
  case INTRN_I4LEADZ:
  case INTRN_I8LEADZ:
  case INTRN_CLZ32:
    if ( id == INTRN_I1LEADZ ) mtype = MTYPE_I1;
    if ( id == INTRN_I2LEADZ ) mtype = MTYPE_I2;
    Expand_Count_Leading_Zeros (result, op0, mtype, id != INTRN_CLZ32, ops);
    break;
  case INTRN_F8ANINT:
    Expand_INTRN_ANINT( result, op0, mtype, ops );
    break;
  case INTRN_SUBSU2:
    {
      TN* tmp1 = Build_TN_Like(result);
      TN* tmp2 = Build_TN_Like(result);
      TN* rflags = Rflags_TN();
      Build_OP( TOP_movzwl, tmp1, op0, ops );
      Build_OP( TOP_sub32, tmp2, tmp1, op1, ops );
      Exp_Immediate( result, Gen_Literal_TN (0, 4), FALSE, ops );
      Build_OP( TOP_cmpi32, rflags, tmp2, Gen_Literal_TN(0, 4), ops );
      Expand_Cmov( TOP_cmovg, result, tmp2, rflags, ops );
      break;
    }
  case INTRN_SUBSV16I2:
    Build_OP( TOP_subus128v16, result, op0, op1, ops );
    break;
//****************************************************************************
// Bug 9140: generate code for vector F8SIGN and F4SIGN
// algoeirhm: sign(x,y) = abs(x) | sign(y)
//            (1) remove sign bit of x
//            (2) extract sign bit of y
//            (3) bitwise OR of the above result
//****************************************************************************
  case INTRN_SIGNV16F8:
     {  
      // remove sign bit of x  
      TCON thenx = Host_To_Targ (MTYPE_I8, 0x7FFFFFFFFFFFFFFFULL);
      TCON nowx  = Create_Simd_Const (MTYPE_V16F8, thenx);
      ST *symx = New_Const_Sym (Enter_tcon (nowx), Be_Type_Tbl(TCON_ty(nowx)));
      Allocate_Object(symx);
      TN *sym_tnx = Gen_Symbol_TN(symx, 0, 0);
      TN *tmpx = Build_TN_Like(op0);
      Exp_Load(mtype, mtype, tmpx, TN_var(sym_tnx), TN_offset(sym_tnx), ops, 0);
      Build_OP(TOP_andpd, tmpx, tmpx, op0, ops);

      // extract sign bit of y
      TCON then = Host_To_Targ (MTYPE_I8, 0x8000000000000000ULL);
      TCON now  = Create_Simd_Const (MTYPE_V16F8, then);
      ST *sym = New_Const_Sym (Enter_tcon (now), Be_Type_Tbl(TCON_ty(now)));
      Allocate_Object(sym);
      TN *sym_tn = Gen_Symbol_TN(sym, 0, 0);
      TN *tmpy = Build_TN_Like(op1);
      Exp_Load(mtype, mtype, tmpy, TN_var(sym_tn), TN_offset(sym_tn), ops, 0);
      Build_OP(TOP_andpd, tmpy, tmpy, op1, ops);

      // bitwise OR to get result
      Build_OP(TOP_orpd, result, tmpx, tmpy, ops);
     } 
    break;
  case INTRN_SIGNV16F4:
    {
      // remove sign bit of x
      TCON thenx = Host_To_Targ (MTYPE_I4, 0x7FFFFFFF);
      TCON nowx  = Create_Simd_Const (MTYPE_V16F4, thenx);
      ST *symx = New_Const_Sym (Enter_tcon (nowx), Be_Type_Tbl(TCON_ty(nowx)));
      Allocate_Object(symx);
      TN *sym_tnx = Gen_Symbol_TN(symx, 0, 0);
      TN *tmpx = Build_TN_Like(op0);
      Exp_Load(mtype, mtype, tmpx, TN_var(sym_tnx), TN_offset(sym_tnx), ops, 0);
      Build_OP(TOP_andps, tmpx, tmpx, op0, ops);

      // extract sign bit of y
      TCON then = Host_To_Targ (MTYPE_I4, 0x80000000);
      TCON now  = Create_Simd_Const (MTYPE_V16F4, then);
      ST *sym = New_Const_Sym (Enter_tcon (now), Be_Type_Tbl(TCON_ty(now)));
      Allocate_Object(sym);
      TN *sym_tn = Gen_Symbol_TN(sym, 0, 0);
      TN *tmpy = Build_TN_Like(op1);
      Exp_Load(mtype, mtype, tmpy, TN_var(sym_tn), TN_offset(sym_tn), ops, 0);
      Build_OP(TOP_andps, tmpy, tmpy, op1, ops);
      
      // bitwise OR to get result
      Build_OP(TOP_orps, result, tmpx, tmpy, ops);
    }
   break;
  case INTRN_ISGREATER:
    Build_OP( cmp_opcode, rflags, op1, op0, ops );
    Build_OP( TOP_setb, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
  case INTRN_ISGREATEREQUAL:
    Build_OP( cmp_opcode, rflags, op1, op0, ops );
    Build_OP( TOP_setbe, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
  case INTRN_ISLESS:
    Build_OP( cmp_opcode, rflags, op0, op1, ops );
    Build_OP( TOP_setb, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
  case INTRN_ISLESSEQUAL:
    Build_OP( cmp_opcode, rflags, op0, op1, ops );
    Build_OP( TOP_setbe, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
  case INTRN_ISLESSGREATER:
    Build_OP( cmp_opcode, rflags, op1, op0, ops );
    Build_OP( TOP_setne, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
  case INTRN_ISUNORDERED:
    Build_OP( cmp_opcode, rflags, op1, op0, ops );
    Build_OP( TOP_setp, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
  case INTRN_ISORDERED:
    Build_OP( cmp_opcode, rflags, op1, op0, ops );
    Build_OP( TOP_setnp, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
  case INTRN_V16C8MPY_ADDSUB:
    {      
      TN* tmp1 = Build_TN_Like(result);
      TN* tmp2 = Build_TN_Like(result);
      TN* tmp3 = Build_TN_Like(result);
      TN* tmp4 = Build_TN_Like(result);
      TN* tmp5 = Build_TN_Like(result);
      Build_OP(TOP_fmovddup, tmp1, op2, ops);
      Build_OP(TOP_shufpd, tmp2, op2, op2, Gen_Literal_TN(1, 1), ops);
      Build_OP(TOP_fmovddup, tmp3, tmp2, ops);
      if ((CG_opt_level > 1) && 
          Is_Target_AVX() && 
          Is_Target_FMA4()) {
        Build_OP(TOP_fmul128v64, tmp5, op1, tmp3, ops);
        Build_OP(TOP_vfmaddsubpd, result, op0, tmp1, tmp5, ops);
      } else if ((CG_opt_level > 1) &&
                 Is_Target_AVX() && 
                 Is_Target_FMA()) {
        Build_OP(TOP_fmul128v64, tmp5, op1, tmp3, ops);
        Build_OP(TOP_xfmaddsub213pd, result, op0, tmp1, tmp5, ops);
      } else {
        Build_OP(TOP_fmul128v64, tmp4, op0, tmp1, ops);
        Build_OP(TOP_fmul128v64, tmp5, op1, tmp3, ops);
        Build_OP(TOP_faddsub128v64, result, tmp4, tmp5, ops);
      }
      break;
    }
  case INTRN_V16C8CONJG:
    {
      TCON real = Host_To_Targ (MTYPE_I8, 0x0ULL);
      TCON imag = Host_To_Targ (MTYPE_I8, 0x8000000000000000ULL);
      TCON now = Make_Complex (MTYPE_V16C8, real, imag);
      ST *sym = New_Const_Sym (Enter_tcon (now), Be_Type_Tbl(TCON_ty(now)));
      Allocate_Object(sym);
      TN *sym_tn = Gen_Symbol_TN(sym, 0, 0);
      TN *tmp = Build_TN_Like(result);
      Exp_Load(mtype, mtype, tmp, TN_var(sym_tn), TN_offset(sym_tn), ops, 0);
      Build_OP(TOP_fxor128v64, result, op0, tmp, ops);
      break;
    }
  case INTRN_PADDSB:
    Build_OP( TOP_paddsb, result, op0, op1, ops );
    break;
  case INTRN_PADDSW:
    Build_OP( TOP_paddsw, result, op0, op1, ops );
    break;
  case INTRN_PADDQ:
    Build_OP( TOP_paddq, result, op0, op1, ops );
    break;
  case INTRN_PSUBSB:
    Build_OP( TOP_psubsb, result, op0, op1, ops );
    break;
  case INTRN_PSUBSW:
    Build_OP( TOP_psubsw, result, op0, op1, ops );
    break;
  case INTRN_PSUBQ:
    Build_OP( TOP_psubq, result, op0, op1, ops );
    break;
  case INTRN_PADDUSB:
    Build_OP( TOP_paddusb, result, op0, op1, ops );
    break;
  case INTRN_PADDUSW:
    Build_OP( TOP_paddusw, result, op0, op1, ops );
    break;
  case INTRN_PSUBUSB:
    Build_OP( TOP_psubusb, result, op0, op1, ops );
    break;
  case INTRN_PSUBUSW:
    Build_OP( TOP_psubusw, result, op0, op1, ops );
    break;
  case INTRN_PMULLW:
    Build_OP( TOP_pmullw, result, op0, op1, ops );
    break;
  case INTRN_PMULHW:
    Build_OP( TOP_pmulhw, result, op0, op1, ops );
    break;
  case INTRN_PMULUDQ:
    Build_OP( TOP_pmuludq, result, op0, op1, ops );
    break;
  case INTRN_PADDSB128:
    Build_OP( TOP_paddsb128, result, op0, op1, ops );
    break;
  case INTRN_PADDSW128:
    Build_OP( TOP_paddsw128, result, op0, op1, ops );
    break;
  case INTRN_PSUBSB128:
    Build_OP( TOP_psubsb128, result, op0, op1, ops );
    break;
  case INTRN_PSUBSW128:
    Build_OP( TOP_psubsw128, result, op0, op1, ops );
    break;
  case INTRN_PADDUSB128:
    Build_OP( TOP_paddusb128, result, op0, op1, ops );
    break;
  case INTRN_PADDUSW128:
    Build_OP( TOP_paddusw128, result, op0, op1, ops );
    break;
  case INTRN_PSUBUSB128:
    Build_OP( TOP_psubusb128, result, op0, op1, ops );
    break;
  case INTRN_PSUBUSW128:
    Build_OP( TOP_psubusw128, result, op0, op1, ops );
    break;
  case INTRN_PMULLW128:
    Build_OP( TOP_pmullw128, result, op0, op1, ops );
    break;
  case INTRN_PMULHW128:
    Build_OP( TOP_pmulhw128, result, op0, op1, ops );
    break;
  case INTRN_PMULUDQ128:
    Build_OP( TOP_pmuludq128, result, op0, op1, ops );
    break;
  case INTRN_PADDD128:
    Build_OP( TOP_add128v32, result, op0, op1, ops );
    break;
  case INTRN_PADDW128:
    Build_OP( TOP_add128v16, result, op0, op1, ops );
    break;
  case INTRN_PCMPEQB:
    Build_OP( TOP_pcmpeqb, result, op0, op1, ops );
    break;
  case INTRN_PCMPEQB128:
    Build_OP( TOP_cmpeq128v8, result, op0, op1, ops );
    break;
  case INTRN_PCMPEQW:
    Build_OP( TOP_pcmpeqw, result, op0, op1, ops );
    break;
  case INTRN_PCMPEQW128:
    Build_OP( TOP_cmpeq128v16, result, op0, op1, ops );
    break;
  case INTRN_PCMPEQD:
    Build_OP( TOP_pcmpeqd, result, op0, op1, ops );
    break;
  case INTRN_PCMPEQD128:
    Build_OP( TOP_cmpeq128v32, result, op0, op1, ops );
    break;
  case INTRN_PCMPGTB:
    Build_OP( TOP_pcmpgtb, result, op0, op1, ops );
    break;
  case INTRN_PCMPGTB128:
    Build_OP( TOP_cmpgt128v8, result, op0, op1, ops );
    break;
  case INTRN_PCMPGTW:
    Build_OP( TOP_pcmpgtw, result, op0, op1, ops );
    break;
  case INTRN_PCMPGTW128:
    Build_OP( TOP_cmpgt128v16, result, op0, op1, ops );
    break;
  case INTRN_PCMPGTD:
    Build_OP( TOP_pcmpgtd, result, op0, op1, ops );
    break;
  case INTRN_PCMPGTD128:
    Build_OP( TOP_cmpgt128v32, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKHBW:
    Build_OP( TOP_punpckhbw, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKHWD:
    Build_OP( TOP_punpckhwd, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKHDQ:
    Build_OP( TOP_punpckhdq, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKHBW128:
    Build_OP( TOP_punpckhbw128, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKHWD128:
    Build_OP( TOP_punpckhwd128, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKHDQ128:
    Build_OP( TOP_punpckhdq128, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKLBW:
    Build_OP( TOP_punpcklbw, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKLWD:
    Build_OP( TOP_punpcklwd, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKLDQ:
    Build_OP( TOP_punpckldq, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKLBW128:
    Build_OP( TOP_punpcklbw128, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKLWD128:
    Build_OP( TOP_punpcklwd128, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKLDQ128:
    Build_OP( TOP_punpckldq128, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKHQDQ:
    Build_OP( TOP_punpckhqdq, result, op0, op1, ops );
    break;
  case INTRN_PUNPCKLQDQ:
    Build_OP( TOP_punpcklqdq, result, op0, op1, ops );
    break;
  case INTRN_PACKSSWB:
    Build_OP( TOP_packsswb, result, op0, op1, ops );
    break;
  case INTRN_PACKSSDW:
    Build_OP( TOP_packssdw, result, op0, op1, ops );
    break;
  case INTRN_PACKUSWB:
    Build_OP( TOP_packuswb, result, op0, op1, ops );
    break;
  case INTRN_PACKSSWB128:
    Build_OP( TOP_packsswb128, result, op0, op1, ops );
    break;
  case INTRN_PACKSSDW128:
    Build_OP( TOP_packssdw128, result, op0, op1, ops );
    break;
  case INTRN_PACKUSWB128:
    Build_OP( TOP_packuswb128, result, op0, op1, ops );
    break;
  case INTRN_PMULHUW:
    Build_OP( TOP_pmulhuw, result, op0, op1, ops );
    break;
  case INTRN_PAVGB:
    Build_OP( TOP_pavgb, result, op0, op1, ops );
    break;
  case INTRN_PAVGW:
    Build_OP( TOP_pavgw, result, op0, op1, ops );
    break;
  case INTRN_PSADBW:
    Build_OP( TOP_psadbw, result, op0, op1, ops );
    break;
  case INTRN_PMULHUW128:
    Build_OP( TOP_pmulhuw128, result, op0, op1, ops );
    break;
  case INTRN_PAVGB128:
    Build_OP( TOP_pavgb128, result, op0, op1, ops );
    break;
  case INTRN_PAVGW128:
    Build_OP( TOP_pavgw128, result, op0, op1, ops );
    break;
  case INTRN_PSADBW128:
    Build_OP( TOP_psadbw128, result, op0, op1, ops );
    break;
  case INTRN_PMAXUB:
    Build_OP( TOP_max64v8, result, op0, op1, ops );
    break;
  case INTRN_PMAXSW:
    Build_OP( TOP_max64v16, result, op0, op1, ops );
    break;
  case INTRN_PMINUB:
    Build_OP( TOP_min64v8, result, op0, op1, ops );
    break;
  case INTRN_PMINSW:
    Build_OP( TOP_min64v16, result, op0, op1, ops );
    break;
  case INTRN_PMAXUB128:
    Build_OP( TOP_maxu128v8, result, op0, op1, ops );
    break;
  case INTRN_PMAXSW128:
    Build_OP( TOP_maxs128v16, result, op0, op1, ops );
    break;
  case INTRN_PMINUB128:
    Build_OP( TOP_minu128v8, result, op0, op1, ops );
    break;
  case INTRN_PMINSW128:
    Build_OP( TOP_mins128v16, result, op0, op1, ops );
    break;
  case INTRN_PEXTRW0:
    Is_True (op1 == NULL, ("Imm operand should be null"));
    op1 = Gen_Literal_TN (0, 4);
    Build_OP( TOP_pextrw, result, op0, op1, ops );
    break;
  case INTRN_PEXTRW1:
    Is_True (op1 == NULL, ("Imm operand should be null"));
    op1 = Gen_Literal_TN (1, 4);
    Build_OP( TOP_pextrw, result, op0, op1, ops );
    break;
  case INTRN_PEXTRW2:
    Is_True (op1 == NULL, ("Imm operand should be null"));
    op1 = Gen_Literal_TN (2, 4);
    Build_OP( TOP_pextrw, result, op0, op1, ops );
    break;
  case INTRN_PEXTRW3:
    Is_True (op1 == NULL, ("Imm operand should be null"));
    op1 = Gen_Literal_TN (3, 4);
    Build_OP( TOP_pextrw, result, op0, op1, ops );
    break;
  case INTRN_PEXTRB:
    Build_OP( TOP_extr128v8, result, op0, op1, ops);
    break;
  case INTRN_PEXTRW:
    Build_OP( TOP_extr128v16, result, op0, op1, ops);
    break;
  case INTRN_PEXTRD:
    Build_OP( TOP_extr128v32, result, op0, op1, ops);
    break;
  case INTRN_PEXTRQ:
    Build_OP( TOP_extr128v64, result, op0, op1, ops);
    break;
  case INTRN_EXTRPS:
    {
      TN* res = Build_TN_Of_Mtype(MTYPE_I4);
      Build_OP( TOP_fextr128v32, res, op0, op1, ops);
      Build_OP(TOP_movg2x, result, res, ops);
      break;
    }
  case INTRN_EXTRPD:
    FmtAssert(FALSE, ("TODO: support fextr128v64"));
    break;
  case INTRN_PINSRW0:
    Is_True (op2 == NULL, ("Imm operand should be null"));
    op2 = Gen_Literal_TN (0, 4);
    Build_OP( TOP_pinsrw, result, op1, op2, ops );
    break;
  case INTRN_PINSRW1:
    Is_True (op2 == NULL, ("Imm operand should be null"));
    op2 = Gen_Literal_TN (1, 4);
    Build_OP( TOP_pinsrw, result, op1, op2, ops );
    break;
  case INTRN_PINSRW2:
    Is_True (op2 == NULL, ("Imm operand should be null"));
    op2 = Gen_Literal_TN (2, 4);
    Build_OP( TOP_pinsrw, result, op1, op2, ops );
    break;
  case INTRN_PINSRW3:
    Is_True (op2 == NULL, ("Imm operand should be null"));
    op2 = Gen_Literal_TN (3, 4);
    Build_OP( TOP_pinsrw, result, op1, op2, ops );
    break;
  case INTRN_PINSRB:
    Build_OP( TOP_insr128v8, result, op0, op1, op2, ops);
    break;
  case INTRN_PINSRW:
    Build_OP( TOP_insr128v16, result, op0, op1, op2, ops);
    break;
  case INTRN_PINSRD:
    Build_OP( TOP_insr128v32, result, op0, op1, op2, ops);
    break;
  case INTRN_PINSRQ:
    Build_OP( TOP_insr128v64, result, op0, op1, op2, ops);
    break;
  case INTRN_INSRPS:
    Build_OP( TOP_finsr128v32, result, op0, op1, op2, ops);
    break;
  case INTRN_INSRPD:
    FmtAssert(FALSE, ("TODO: support finsr128v64"));
    break;
  case INTRN_PMOVMSKB:
    Build_OP( TOP_pmovmskb, result, op0, ops );
    break;
  case INTRN_PMOVMSKB128:
    Build_OP( TOP_pmovmskb128, result, op0, ops );
    break;
  case INTRN_COMIEQSS:
    Build_OP( TOP_comiss, rflags, op0, op1, ops );
    Build_OP( TOP_sete, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_COMILTSS:
    Build_OP( TOP_comiss, rflags, op0, op1, ops );
    Build_OP( TOP_setb, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_COMILESS:
    Build_OP( TOP_comiss, rflags, op0, op1, ops );
    Build_OP( TOP_setbe, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_COMIGTSS:
    Build_OP( TOP_comiss, rflags, op0, op1, ops );
    Build_OP( TOP_seta, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_COMIGESS:
    Build_OP( TOP_comiss, rflags, op0, op1, ops );
    Build_OP( TOP_setae, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_COMINEQSS:
    Build_OP( TOP_comiss, rflags, op0, op1, ops );
    Build_OP( TOP_setne, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_COMIEQSD:
    Build_OP( TOP_comisd, rflags, op0, op1, ops );
    Build_OP( TOP_sete, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_COMILTSD:
    Build_OP( TOP_comisd, rflags, op0, op1, ops );
    Build_OP( TOP_setb, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_COMILESD:
    Build_OP( TOP_comisd, rflags, op0, op1, ops );
    Build_OP( TOP_setbe, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_COMIGTSD:
    Build_OP( TOP_comisd, rflags, op0, op1, ops );
    Build_OP( TOP_seta, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_COMIGESD:
    Build_OP( TOP_comisd, rflags, op0, op1, ops );
    Build_OP( TOP_setae, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_COMINEQSD:
    Build_OP( TOP_comisd, rflags, op0, op1, ops );
    Build_OP( TOP_setne, result_tmp, rflags, ops);
    need_zero_ext = TRUE;
    break;
  case INTRN_ADDPS:
    Build_OP( TOP_fadd128v32, result, op0, op1, ops );
    break;
  case INTRN_SUBPS:
    Build_OP( TOP_fsub128v32, result, op0, op1, ops );
    break;
  case INTRN_MULPS:
    Build_OP( TOP_fmul128v32, result, op0, op1, ops );
    break;
  case INTRN_DIVPS:
    Build_OP( TOP_fdiv128v32, result, op0, op1, ops );
    break;
  case INTRN_ADDSD:
    Build_OP( TOP_addsd, result, op0, op1, ops );
    break;
  case INTRN_SUBSD:
    Build_OP( TOP_subsd, result, op0, op1, ops );
    break;
  case INTRN_MULSD:
    Build_OP( TOP_mulsd, result, op0, op1, ops );
    break;
  case INTRN_DIVSD:
    Build_OP( TOP_divsd, result, op0, op1, ops );
    break;
  case INTRN_ADDSS:
    Build_OP( TOP_addss, result, op0, op1, ops );
    break;
  case INTRN_SUBSS:
    Build_OP( TOP_subss, result, op0, op1, ops );
    break;
  case INTRN_MULSS:
    Build_OP( TOP_mulss, result, op0, op1, ops );
    break;
  case INTRN_DIVSS:
    Build_OP( TOP_divss, result, op0, op1, ops );
    break;
  case INTRN_CMPEQPD:
    Build_OP( TOP_cmpeqpd, result, op0, op1, ops );
    break;
  case INTRN_CMPLTPD:
    Build_OP( TOP_cmpltpd, result, op0, op1, ops );
    break;
  case INTRN_CMPLEPD:
    Build_OP( TOP_cmplepd, result, op0, op1, ops );
    break;
  case INTRN_CMPGTPD:
    Build_OP( TOP_cmpltpd, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPGEPD:
    Build_OP( TOP_cmplepd, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPUNORDPD:
    Build_OP( TOP_cmpunordpd, result, op0, op1, ops );
    break;
  case INTRN_CMPNEQPD:
    Build_OP( TOP_cmpneqpd, result, op0, op1, ops );
    break;
  case INTRN_CMPNLTPD:
    Build_OP( TOP_cmpnltpd, result, op0, op1, ops );
    break;
  case INTRN_CMPNLEPD:
    Build_OP( TOP_cmpnlepd, result, op0, op1, ops );
    break;
  case INTRN_CMPNGTPD:
    Build_OP( TOP_cmpnltpd, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPNGEPD:
    Build_OP( TOP_cmpnlepd, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPORDPD:
    Build_OP( TOP_cmpordpd, result, op0, op1, ops );
    break;
  case INTRN_CMPEQPS:
    Build_OP( TOP_cmpeqps, result, op0, op1, ops );
    break;
  case INTRN_CMPLTPS:
    Build_OP( TOP_cmpltps, result, op0, op1, ops );
    break;
  case INTRN_CMPLEPS:
    Build_OP( TOP_cmpleps, result, op0, op1, ops );
    break;
  case INTRN_CMPGTPS:
    Build_OP( TOP_cmpltps, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPGEPS:
    Build_OP( TOP_cmpleps, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPUNORDPS:
    Build_OP( TOP_cmpunordps, result, op0, op1, ops );
    break;
  case INTRN_CMPNEQPS:
    Build_OP( TOP_cmpneqps, result, op0, op1, ops );
    break;
  case INTRN_CMPNLTPS:
    Build_OP( TOP_cmpnltps, result, op0, op1, ops );
    break;
  case INTRN_CMPNLEPS:
    Build_OP( TOP_cmpnleps, result, op0, op1, ops );
    break;
  case INTRN_CMPNGTPS:
    Build_OP( TOP_cmpnltps, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPNGEPS:
    Build_OP( TOP_cmpnleps, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPORDPS:
    Build_OP( TOP_cmpordps, result, op0, op1, ops );
    break;
  case INTRN_CMPEQSD:
    Build_OP( TOP_cmpeqsd, result, op0, op1, ops );
    break;
  case INTRN_CMPLTSD:
    Build_OP( TOP_cmpltsd, result, op0, op1, ops );
    break;
  case INTRN_CMPLESD:
    Build_OP( TOP_cmplesd, result, op0, op1, ops );
    break;
  case INTRN_CMPGTSD:
    Build_OP( TOP_cmpltsd, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPGESD:
    Build_OP( TOP_cmplesd, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPUNORDSD:
    Build_OP( TOP_cmpunordsd, result, op0, op1, ops );
    break;
  case INTRN_CMPNEQSD:
    Build_OP( TOP_cmpneqsd, result, op0, op1, ops );
    break;
  case INTRN_CMPNLTSD:
    Build_OP( TOP_cmpnltsd, result, op0, op1, ops );
    break;
  case INTRN_CMPNLESD:
    Build_OP( TOP_cmpnlesd, result, op0, op1, ops );
    break;
  case INTRN_CMPNGTSD:
    Build_OP( TOP_cmpnltsd, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPNGESD:
    Build_OP( TOP_cmpnlesd, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPORDSD:
    Build_OP( TOP_cmpordsd, result, op0, op1, ops );
    break;
  case INTRN_CMPEQSS:
    Build_OP( TOP_cmpeqss, result, op0, op1, ops );
    break;
  case INTRN_CMPLTSS:
    Build_OP( TOP_cmpltss, result, op0, op1, ops );
    break;
  case INTRN_CMPLESS:
    Build_OP( TOP_cmpless, result, op0, op1, ops );
    break;
  case INTRN_CMPGTSS:
    Build_OP( TOP_cmpltss, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPGESS:
    Build_OP( TOP_cmpless, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPUNORDSS:
    Build_OP( TOP_cmpunordss, result, op0, op1, ops );
    break;
  case INTRN_CMPNEQSS:
    Build_OP( TOP_cmpneqss, result, op0, op1, ops );
    break;
  case INTRN_CMPNLTSS:
    Build_OP( TOP_cmpnltss, result, op0, op1, ops );
    break;
  case INTRN_CMPNLESS:
    Build_OP( TOP_cmpnless, result, op0, op1, ops );
    break;
  case INTRN_CMPNGTSS:
    Build_OP( TOP_cmpnltss, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPNGESS:
    Build_OP( TOP_cmpnless, result, op1, op0, ops );  // swap operands
    break;
  case INTRN_CMPORDSS:
    Build_OP( TOP_cmpordss, result, op0, op1, ops );
    break;
  case INTRN_MAXPS:
    Build_OP( TOP_fmax128v32, result, op0, op1, ops );
    break;
  case INTRN_MAXSD:
    Build_OP( TOP_maxsd, result, op0, op1, ops );
    break;
  case INTRN_MAXSS:
    Build_OP( TOP_maxss, result, op0, op1, ops );
    break;
  case INTRN_MINPS:
    Build_OP( TOP_fmin128v32, result, op0, op1, ops );
    break;
  case INTRN_MINSD:
    Build_OP( TOP_minsd, result, op0, op1, ops );
    break;
  case INTRN_MINSS:
    Build_OP( TOP_minss, result, op0, op1, ops );
    break;
  case INTRN_ANDPS:
    Build_OP( TOP_fand128v32, result, op0, op1, ops );
    break;
  case INTRN_ANDNPD:
    Build_OP( TOP_andnpd, result, op0, op1, ops );
    break;
  case INTRN_ANDNPS:
    Build_OP( TOP_andnps, result, op0, op1, ops );
    break;
  case INTRN_ORPS:
    Build_OP( TOP_for128v32, result, op0, op1, ops );
    break;
  case INTRN_XORPS:
    Build_OP( TOP_fxor128v32, result, op0, op1, ops );
    break;
  case INTRN_MOVSS:
    Build_OP( TOP_movss, result, op0, op1, ops );
    break;
  case INTRN_MOVSD:
    Build_OP( TOP_movsd, result, op0, op1, ops );
    break;
  case INTRN_MOVHLPS:
     if (Is_Target_Orochi() && Is_Target_AVX()) {
      Build_OP( TOP_movhlps, result, op0, op1, ops );
    } else {
      // do packing of  op1[2] op1[3] op0[2] op0[3] in result
      Build_OP(TOP_shufps, result, op0, op1, Gen_Literal_TN(187, 1), ops);  
      // reverse fields to get op0[3] op0[2] op1[3] op1[2]
      Build_OP(TOP_shufps, result, result, result, Gen_Literal_TN(27, 1), ops);
    }

    break;
  case INTRN_MOVLHPS:
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      Build_OP( TOP_movlhps, result, op0, op1, ops );
    } else {
      // do packing of  op1[1] op1[0] op0[1] op0[0] in result
      Build_OP(TOP_shufps, result, op0, op1, Gen_Literal_TN(68, 1), ops);
    }

    break;
  case INTRN_UNPCKHPS:
    Build_OP( TOP_unpckhps, result, op0, op1, ops );
    break;
  case INTRN_UNPCKLPS:
    Build_OP( TOP_unpcklps, result, op0, op1, ops );
    break;
  case INTRN_RCPPS:
    Build_OP( TOP_frcp128v32, result, op0, ops );
    break;
  case INTRN_RSQRTPS:
    Build_OP( TOP_frsqrt128v32, result, op0, ops );
    break;
  case INTRN_SQRTPS:
    Build_OP( TOP_fsqrt128v32, result, op0, ops );
    break;
  case INTRN_RCPSS:
    if ( Is_Target_Orochi() && Is_Target_AVX() ) {
      Build_OP( TOP_rcpss, result, op0, op0, ops );
    } else {
      Build_OP( TOP_rcpss, result, op0, ops );
    }
    break;
  case INTRN_RSQRTSS:
    if ( Is_Target_Orochi() && Is_Target_AVX() ) {
      Build_OP( TOP_rsqrtss, result, op0, op0, ops );
    } else {
      Build_OP( TOP_rsqrtss, result, op0, ops );
    }
    break;
  case INTRN_SQRTSD:
    if ( Is_Target_Orochi() && Is_Target_AVX() ) {
      Build_OP( TOP_sqrtsd, result, op0, op0, ops );
    } else {
      Build_OP( TOP_sqrtsd, result, op0, ops );
    }
    break;
  case INTRN_SQRTSS:
    if ( Is_Target_Orochi() && Is_Target_AVX() ) {
      Build_OP( TOP_sqrtss, result, op0, op0, ops );
    } else {
      Build_OP( TOP_sqrtss, result, op0, ops );
    }
    break;
  case INTRN_SHUFPS:
    Build_OP( TOP_shufps, result, op0, op1, op2, ops );
    break;
  case INTRN_LOADAPS:
    Build_OP( TOP_ldaps, result, op0, Gen_Literal_TN(0,4), ops );
    break;
  case INTRN_PSLLDQ:
    Build_OP( TOP_pslldq, result, op0, op1, ops );
    break;
  case INTRN_PSRLDQ:
    Build_OP( TOP_psrldq, result, op0, op1, ops );
    break;
  case INTRN_PSLLW128:
    Build_OP( TOP_psllw, result, op0, op1, ops );
    break;
  case INTRN_PSLLD128:
    Build_OP( TOP_pslld, result, op0, op1, ops );
    break;
  case INTRN_PSLLQ128:
    Build_OP( TOP_psllq, result, op0, op1, ops );
    break;
  case INTRN_PSRLW128:
    Build_OP( TOP_psrlw, result, op0, op1, ops );
    break;
  case INTRN_PSRLD128:
    Build_OP( TOP_psrld, result, op0, op1, ops );
    break;
  case INTRN_PSRLQ128:
    Build_OP( TOP_psrlq, result, op0, op1, ops );
    break;
  case INTRN_PSRAW128:
    Build_OP( TOP_psraw, result, op0, op1, ops );
    break;
  case INTRN_PSRAD128:
    Build_OP( TOP_psrad, result, op0, op1, ops );
    break;
  case INTRN_PSLLWI128:
    Build_OP( TOP_psllwi, result, op0, op1, ops );
    break;
  case INTRN_PSLLDI128:
    Build_OP( TOP_pslldi, result, op0, op1, ops );
    break;
  case INTRN_PSLLQI128:
    Build_OP( TOP_psllqi, result, op0, op1, ops );
    break;
  case INTRN_PSRLWI128:
    Build_OP( TOP_psrlwi, result, op0, op1, ops );
    break;
  case INTRN_PSRLDI128:
    Build_OP( TOP_psrldi, result, op0, op1, ops );
    break;
  case INTRN_PSRLQI128:
    Build_OP( TOP_psrlqi, result, op0, op1, ops );
    break;
  case INTRN_PSRAWI128:
    Build_OP( TOP_psrawi, result, op0, op1, ops );
    break;
  case INTRN_PSRADI128:
    Build_OP( TOP_psradi, result, op0, op1, ops );
    break;
  case INTRN_PSLLW:
    Build_OP( TOP_psllw_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSLLD:
    Build_OP( TOP_pslld_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSLLQ:
    Build_OP( TOP_psllq_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSRLW:
    Build_OP( TOP_psrlw_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSRLD:
    Build_OP( TOP_psrld_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSRLQ:
    Build_OP( TOP_psrlq_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSRAW:
    Build_OP( TOP_psraw_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSRAD:
    Build_OP( TOP_psrad_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSLLWI:
    Build_OP( TOP_psllwi_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSLLDI:
    Build_OP( TOP_pslldi_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSLLQI:
    Build_OP( TOP_psllqi_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSRLWI:
    Build_OP( TOP_psrlwi_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSRLDI:
    Build_OP( TOP_psrldi_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSRLQI:
    Build_OP( TOP_psrlqi_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSRAWI:
    Build_OP( TOP_psrawi_mmx, result, op0, op1, ops );
    break;
  case INTRN_PSRADI:
    Build_OP( TOP_psradi_mmx, result, op0, op1, ops );
    break;
  case INTRN_LOADD:
    Build_OP( TOP_movg2x64, result, op0, ops );
    break;
  case INTRN_PSHUFD:
    Build_OP( TOP_pshufd, result, op0, op1, ops );
    break;
  case INTRN_LOADSS:
    Build_OP( TOP_ldss, result, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_SHUFPD:
    Build_OP( TOP_shufpd, result, op0, op1, op2, ops );
    break;
  case INTRN_XORPD:
    Build_OP( TOP_fxor128v64, result, op0, op1, ops );
    break;
  case INTRN_ANDPD:
    Build_OP( TOP_fand128v64, result, op0, op1, ops );
    break;
  case INTRN_ORPD:
    Build_OP( TOP_for128v64, result, op0, op1, ops );
    break;
  case INTRN_LOADLPD:
    Build_OP( TOP_ldsd, result, op1, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_LOADHPD:
    if (Is_Target_Orochi() && Is_Target_AVX()){
      TN *xzero = Build_TN_Like(result);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( TOP_ldhpd, result, xzero, op1, Gen_Literal_TN (0,4), ops );
    } else {
      Build_OP( TOP_ldhpd, result, op1, Gen_Literal_TN (0,4), ops );
    }
    break;
  case INTRN_UNPCKLPD:
    Build_OP( TOP_unpcklpd, result, op0, op1, ops );
    break;
  case INTRN_UNPCKHPD:
    Build_OP( TOP_unpckhpd, result, op0, op1, ops );
    break;
  case INTRN_PSHUFW:
    Build_OP( TOP_pshufw64v16, result, op0, op1, ops );
    break;
  case INTRN_LOADDQA:
    Build_OP( TOP_lddqa, result, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_LOADDQU:
    Build_OP( TOP_lddqu, result, op0, Gen_Literal_TN (0,4), ops );
    break;

  case INTRN_COSL:
    Build_OP( TOP_fcos, result, op0, ops );
    break;
  case INTRN_SINL:
    Build_OP( TOP_fsin, result, op0, ops );
    break;
  case INTRN_VEC_INIT_V2SI:
    {
      TN* tmp0 = Build_TN_Like(result);
      TN* tmp1 = Build_TN_Like(result);
      if ( TN_register_class(result) == ISA_REGISTER_CLASS_mmx ) {
        Build_OP( TOP_punpckldq, result, op0, op1, ops );
      } else {
        Build_OP( TOP_punpckldq128, result, op0, op1, ops );
      }
      break;
    }
  case INTRN_VEC_EXT_V2SI:
    if ( TN_register_class(op0) == ISA_REGISTER_CLASS_mmx ) {
      if ( TN_has_value(op1) && TN_value(op1) == 1 ) {
        Build_OP( TOP_punpckhdq, op0, op0, op0, ops );
        Build_OP( TOP_movm_2i32, result, op0, ops );
      } else if ( TN_is_zero(op1)) {
        Build_OP( TOP_movm_2i32, result, op0, ops );
      } else {
        FmtAssert(0, ("op1 must be an integer constant in the range 0..1"));
      }
    } else {
      if ( TN_has_value(op1) && TN_value(op1) == 1 ) {
        TN* tmp=Build_RCLASS_TN(ISA_REGISTER_CLASS_mmx);
        Build_OP( TOP_movdq2q, tmp,  op0, ops);
        Build_OP( TOP_punpckhdq128, result, tmp, tmp, ops);
      } else if ( TN_is_zero(op1)) {
        if ( TN_register_class(result) == ISA_REGISTER_CLASS_integer )
          Build_OP( TOP_movx2g, result, op0, ops );
        else
          Build_OP( TOP_movdq2q, result, op0, ops );
      } else {
        FmtAssert(0, ("op1 must be an integer constant in the range 0..1"));
      }
    }
    break;
  case INTRN_PMADDWD:
    Build_OP( TOP_pmaddwd, result, op0, op1, ops );
    break;
  case INTRN_PAND_MMX:
    Build_OP( TOP_pand_mmx, result, op0, op1, ops );
    break;
  case INTRN_PANDN_MMX:
    Build_OP( TOP_pandn_mmx, result, op0, op1, ops );
    break;
  case INTRN_POR_MMX:
    Build_OP( TOP_por_mmx, result, op0, op1, ops );
    break;
  case INTRN_PXOR_MMX:
    Build_OP( TOP_pxor_mmx, result, op0, op1, ops );
    break;
  case INTRN_PAND128:
    Build_OP( TOP_pand, result, op0, op1, ops );
    break;
  case INTRN_PANDN128:
    Build_OP( TOP_pandn, result, op0, op1, ops );
    break;
  case INTRN_POR128:
    Build_OP( TOP_por, result, op0, op1, ops );
    break;
  case INTRN_PXOR128:
    Build_OP( TOP_pxor, result, op0, op1, ops );
    break;
  case INTRN_CVTPI2PS:
    if (TN_register_class(op0) != ISA_REGISTER_CLASS_mmx) {
      TN *tmp0 = Build_RCLASS_TN(ISA_REGISTER_CLASS_mmx);
      Exp_COPY( tmp0, op0, ops );
      op0 = tmp0;
    }
    Build_OP( TOP_cvtpi2ps, result, op0, ops );
    break;
  case INTRN_CVTPS2PI:
    Build_OP( TOP_cvtps2pi, result, op0, ops);
    break;
  case INTRN_CVTTPS2PI:
    Build_OP( TOP_cvttps2pi, result, op0, ops);
    break;
  case INTRN_CVTPI2PD:
    if (TN_register_class(op0) != ISA_REGISTER_CLASS_mmx) {
      TN *tmp0 = Build_RCLASS_TN(ISA_REGISTER_CLASS_mmx);
      Exp_COPY( tmp0, op0, ops );
      op0 = tmp0;
    }
    Build_OP( TOP_cvtpi2pd, result, op0, ops );
    break;
  case INTRN_CVTPD2PI:
    Build_OP( TOP_cvtpd2pi, result, op0, ops);
    break;
  case INTRN_CVTTPD2PI:
    Build_OP( TOP_cvttpd2pi, result, op0, ops);
    break;
  case INTRN_CVTSI2SS:
    if (TN_register_class(op0) != ISA_REGISTER_CLASS_integer) {
      TN *tmp0 = Build_RCLASS_TN(ISA_REGISTER_CLASS_integer);
      Build_OP( TOP_movx2g, tmp0, op0, ops );
      op0 = tmp0;
    }
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      TN *xzero = Build_TN_Like(result);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( TOP_cvtsi2ss, result, xzero, op0, ops );
    } else {
      Build_OP( TOP_cvtsi2ss, result, op0, ops );
    }
    break;
  case INTRN_CVTSI642SS:
    if (TN_register_class(op0) != ISA_REGISTER_CLASS_integer) {
      TN *tmp0 = Build_RCLASS_TN(ISA_REGISTER_CLASS_integer);
      Build_OP( TOP_movx2g64, tmp0, op0, ops );
      op0 = tmp0;
    }
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      TN *xzero = Build_TN_Like(result);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( TOP_cvtsi2ssq, result, xzero, op0, ops );
    } else {
      Build_OP( TOP_cvtsi2ssq, result, op0, ops );
    }
    break;
  case INTRN_CVTSS2SI:
    Build_OP( TOP_cvtss2si, result, op0, ops );
    break;
  case INTRN_CVTSS2SI64:
    Build_OP( TOP_cvtss2siq, result, op0, ops );
    break;
  case INTRN_CVTTSS2SI:
    Build_OP( TOP_cvttss2si, result, op0, ops );
    break;
  case INTRN_CVTTSS2SI64:
    Build_OP( TOP_cvttss2siq, result, op0, ops );
    break;
  case INTRN_CVTSI2SD:
    if (TN_register_class(op0) != ISA_REGISTER_CLASS_integer) {
      TN *tmp0 = Build_RCLASS_TN(ISA_REGISTER_CLASS_integer);
      Build_OP( TOP_movx2g, tmp0, op0, ops );
      op0 = tmp0;
    }
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      TN *xzero = Build_TN_Like(result);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( TOP_cvtsi2sd, result, xzero, op0, ops );
    } else {
      Build_OP( TOP_cvtsi2sd, result, op0, ops );
    }
    break;
  case INTRN_CVTSI642SD:
    if (TN_register_class(op0) != ISA_REGISTER_CLASS_integer) {
      TN *tmp0 = Build_RCLASS_TN(ISA_REGISTER_CLASS_integer);
      Build_OP( TOP_movx2g64, tmp0, op0, ops );
      op0 = tmp0;
    }
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      TN *xzero = Build_TN_Like(result);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( TOP_cvtsi2sdq, result, xzero, op0, ops );
    } else {
      Build_OP( TOP_cvtsi2sdq, result, op0, ops );
    }
    break;
  case INTRN_CVTSD2SI:
    Build_OP( TOP_cvtsd2si, result, op0, ops );
    break;
  case INTRN_CVTSD2SI64:
    Build_OP( TOP_cvtsd2siq, result, op0, ops );
    break;
  case INTRN_CVTTSD2SI:
    Build_OP( TOP_cvttsd2si, result, op0, ops );
    break;
  case INTRN_CVTTSD2SI64:
    Build_OP( TOP_cvttsd2siq, result, op0, ops );
    break;
  case INTRN_CVTDQ2PS:
    Build_OP( TOP_cvtdq2ps, result, op0, ops );
    break;
  case INTRN_CVTPS2DQ:
    Build_OP( TOP_cvtps2dq, result, op0, ops );
    break;
  case INTRN_CVTTPS2DQ:
    Build_OP( TOP_cvttps2dq, result, op0, ops );
    break;
  case INTRN_CVTDQ2PD:
    Build_OP( TOP_cvtdq2pd, result, op0, ops );
    break;
  case INTRN_CVTPD2DQ:
    Build_OP( TOP_cvtpd2dq, result, op0, ops );
    break;
  case INTRN_CVTTPD2DQ:
    Build_OP( TOP_cvttpd2dq, result, op0, ops );
    break;
  case INTRN_CVTPD2PS:
    Build_OP( TOP_cvtpd2ps, result, op0, ops );
    break;
  case INTRN_CVTPS2PD:
    Build_OP( TOP_cvtps2pd, result, op0, ops );
    break;
  case INTRN_CVTSD2SS:
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      TN *xzero = Build_TN_Like(result);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( TOP_cvtsd2ss, result, xzero, op0, ops );
    } else {
      Build_OP( TOP_cvtsd2ss, result, op0, ops );
    }
    break;
  case INTRN_CVTSS2SD:
    if (Is_Target_Orochi() && Is_Target_AVX()) {
      TN *xzero = Build_TN_Like(result);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( TOP_cvtss2sd, result, xzero, op0, ops );
    } else {
      Build_OP( TOP_cvtss2sd, result, op0, ops );
    }
    break;
  case INTRN_LOADUPS:
    Build_OP( TOP_ldups, result, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_LOADUPD:
    Build_OP( TOP_ldupd, result, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_LOADHPS:
    if (Is_Target_Orochi() && Is_Target_AVX()){
      TN *xzero = Build_TN_Like(result);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( TOP_ldhps, result, xzero, op1, Gen_Literal_TN (0,4), ops );
    } else {
      Build_OP( TOP_ldhps, result, op1, Gen_Literal_TN (0,4), ops );
    }
    break;
  case INTRN_LOADLPS:
    if (Is_Target_Orochi() && Is_Target_AVX()){
      TN *xzero = Build_TN_Like(result);
      Build_OP( TOP_xzero128v32, xzero, ops );
      Build_OP( TOP_ldlps, result, xzero, op1, Gen_Literal_TN (0,4), ops );
    } else {
      Build_OP( TOP_ldlps, result, op1, Gen_Literal_TN (0,4), ops );
    }
    break;
  case INTRN_MOVMSKPS:
    Build_OP( TOP_movmskps, result, op0, ops );
    break;
  case INTRN_MOVMSKPD:
    Build_OP( TOP_movmskpd, result, op0, ops );
    break;
  case INTRN_PSHUFLW:
    Build_OP( TOP_pshuflw, result, op0, op1, ops);
    break;
  case INTRN_PSHUFHW:
    Build_OP( TOP_pshufhw, result, op0, op1, ops);
    break;
   case INTRN_EXTRQ:
    Build_OP( TOP_extrq, result, op0, op1, ops );
    break;
   case INTRN_INSERTQ:
    Build_OP(TOP_insertq, result, op0, op1, ops );
    break;
   // SSSE3 intrinsics
   case INTRN_PABSB:
    Build_OP(TOP_pabs128v8, result, op0, ops );
    break;
   case INTRN_PABSD:
    Build_OP(TOP_pabs128v32, result, op0, ops );
    break;
   case INTRN_PABSW:
    Build_OP(TOP_pabs128v16, result, op0, ops );
    break;
   case INTRN_PALIGNR: {
    FmtAssert( TN_has_value(op2), ("op2 does not has value") );
    TN* new_op2 = Gen_Literal_TN ( TN_value(op2)/8, 4 );
    Build_OP(TOP_palignr128, result, op0, op1, new_op2, ops );
    break;
   }
   case INTRN_PHADDD:
    Build_OP(TOP_phadd128v32, result, op0, op1, ops );
    break;
   case INTRN_PHADDSW:
    Build_OP(TOP_phadds128v16, result, op0, op1, ops );
    break;
   case INTRN_PHADDW:
    Build_OP(TOP_phadd128v16, result, op0, op1, ops );
    break;
   case INTRN_PHSUBD:
    Build_OP(TOP_phsub128v32, result, op0, op1, ops );
    break;
   case INTRN_PHSUBSW:
    Build_OP(TOP_phsubs128v16, result, op0, op1, ops );
    break;
   case INTRN_PHSUBW:
    Build_OP(TOP_phsub128v16, result, op0, op1, ops );
    break;
   case INTRN_PMADDUBSW:
    Build_OP(TOP_pmaddubsw128, result, op0, op1, ops );
    break;
   case INTRN_PMULHRSW:
    Build_OP(TOP_pmulhrsw128, result, op0, op1, ops );
    break;
   case INTRN_PSHUFB:
    Build_OP(TOP_pshuf128v8, result, op0, op1, ops );
    break;
   case INTRN_PSIGNB:
    Build_OP(TOP_psign128v8, result, op0, op1, ops );
    break;
   case INTRN_PSIGND:
    Build_OP(TOP_psign128v32, result, op0, op1, ops );
    break;
   case INTRN_PSIGNW:
    Build_OP(TOP_psign128v16, result, op0, op1, ops );
    break;
   // SSE4.1 intrinsics
   case INTRN_BLENDPD:
    Build_OP(TOP_fblend128v64, result, op0, op1, op2, ops );
    break;
   case INTRN_BLENDPS:
    Build_OP(TOP_fblend128v32, result, op0, op1, op2, ops );
    break;
   case INTRN_BLENDVPD: 
   case INTRN_BLENDVPS:
   case INTRN_PBLENDVB128:
    Expand_INTRN_BLENDV(id, result, op0, op1, op2, ops);
    break;
   case INTRN_DPPD:
    Build_OP(TOP_fdp128v64, result, op0, op1, op2, ops );
    break;
   case INTRN_DPPS:
    Build_OP(TOP_fdp128v32, result, op0, op1, op2, ops );
    break;
   case INTRN_INSERTPS128:
    Build_OP(TOP_finsr128v32, result, op0, op1, op2, ops );
    break;
   case INTRN_MOVNTDQA:
    Build_OP(TOP_ldntdqa, result, op0, Gen_Literal_TN(0, 4), ops );
    break;
   case INTRN_MPSADBW128:
    Build_OP(TOP_mpsadbw, result, op0, op1, op2, ops );
    break;
   case INTRN_PACKUSDW128:
    Build_OP(TOP_packusdw, result, op0, op1, ops );
    break;
   case INTRN_PBLENDW128:
    Build_OP(TOP_blend128v16, result, op0, op1, op2, ops );
    break;
   case INTRN_PCMPEQQ:
    Build_OP(TOP_cmpeq128v64, result, op0, op1, op2, ops );
    break;
   case INTRN_PHMINPOSUW128:
    Build_OP(TOP_phminposuw, result, op0, ops );
    break;
   case INTRN_PMAXSB128:
    Build_OP(TOP_maxs128v8, result, op0, op1, ops );
    break;
   case INTRN_PMAXSD128:
    Build_OP(TOP_maxs128v32, result, op0, op1, ops );
    break;
   case INTRN_PMAXUD128:
    Build_OP(TOP_maxu128v32, result, op0, op1, ops );
    break;
   case INTRN_PMAXUW128:
    Build_OP(TOP_maxu128v16, result, op0, op1, ops );
    break;
   case INTRN_PMINSB128:
    Build_OP(TOP_mins128v8, result, op0, op1, ops );
    break;
   case INTRN_PMINSD128:
    Build_OP(TOP_mins128v32, result, op0, op1, ops );
    break;
   case INTRN_PMINUD128:
    Build_OP(TOP_minu128v32, result, op0, op1, ops );
    break;
   case INTRN_PMINUW128:
    Build_OP(TOP_minu128v16, result, op0, op1, ops );
    break;
   case INTRN_PMOVSXBD128:
    Build_OP(TOP_pmovsxbd, result, op0, ops );
    break;
   case INTRN_PMOVSXBQ128:
    Build_OP(TOP_pmovsxbq, result, op0, ops );
    break;
   case INTRN_PMOVSXBW128:
    Build_OP(TOP_pmovsxbw, result, op0, ops );
    break;
   case INTRN_PMOVSXDQ128:
    Build_OP(TOP_pmovsxdq, result, op0, ops );
    break;
   case INTRN_PMOVSXWD128:
    Build_OP(TOP_pmovsxwd, result, op0, ops );
    break;
   case INTRN_PMOVSXWQ128:
    Build_OP(TOP_pmovsxwq, result, op0, ops );
    break;
   case INTRN_PMOVZXBD128:
    Build_OP(TOP_pmovzxbd, result, op0, ops );
    break;
   case INTRN_PMOVZXBQ128:
    Build_OP(TOP_pmovzxbq, result, op0, ops );
    break;
   case INTRN_PMOVZXBW128:
    Build_OP(TOP_pmovzxbw, result, op0, ops );
    break;
   case INTRN_PMOVZXDQ128:
    Build_OP(TOP_pmovzxdq, result, op0, ops );
    break;
   case INTRN_PMOVZXWD128:
    Build_OP(TOP_pmovzxwd, result, op0, ops );
    break;
   case INTRN_PMOVZXWQ128:
    Build_OP(TOP_pmovzxwq, result, op0, ops );
    break;
   case INTRN_PMULDQ128:
    Build_OP(TOP_muldq, result, op0, op1, ops );
    break;
   case INTRN_PMULLD128:
    Build_OP(TOP_mul128v32, result, op0, op1, ops );
    break;
   case INTRN_VEC_SET_V16QI:
    Build_OP(TOP_insr128v8, result, op0, op1, op2, ops );
    break;
   case INTRN_VEC_SET_V2DI:
    Build_OP(TOP_insr128v64, result, op0, op1, op2, ops );
    break;
   case INTRN_VEC_SET_V4SF:
    Build_OP(TOP_finsr128v32, result, op0, op1, op2, ops );
    break;
   case INTRN_VEC_SET_V4SI:
    Build_OP(TOP_insr128v32, result, op0, op1, op2, ops );
    break;
   // SSE4.2 instrinsics
   case INTRN_CRC32DI:
    Build_OP(TOP_crc32q, result, op0, op1, ops );
    break;
   case INTRN_CRC32HI:
    Build_OP(TOP_crc32w, result, op0, op1, ops );
    break;
   case INTRN_CRC32QI:
    Build_OP(TOP_crc32b, result, op0, op1, ops );
    break;
   case INTRN_CRC32SI:
    Build_OP(TOP_crc32d, result, op0, op1, ops );
    break;
   case INTRN_PCMPESTRI128:
   case INTRN_PCMPESTRM128:
   case INTRN_PCMPESTRA128:
   case INTRN_PCMPESTRC128:
   case INTRN_PCMPESTRO128:
   case INTRN_PCMPESTRS128:
   case INTRN_PCMPESTRZ128:
    Expand_INTRN_PCMPESTR(id, result, op0, op1, op2, op3, op4, ops);
    break;
   case INTRN_PCMPGTQ:
    Build_OP(TOP_cmpgt128v64, result, op0, op1, ops );
    break;
   case INTRN_PCMPISTRI128:
   case INTRN_PCMPISTRA128:
   case INTRN_PCMPISTRC128:
   case INTRN_PCMPISTRO128:
   case INTRN_PCMPISTRS128:
   case INTRN_PCMPISTRZ128:
   case INTRN_PCMPISTRM128:
    Expand_INTRN_PCMPISTR(id, result, op0, op1, op2, ops);
    break;
   // AES intrinsics
   case INTRN_AESDEC128:
    Build_OP(TOP_aesdec, result, op0, op1, ops );
    break;
   case INTRN_AESDECLAST128:
    Build_OP(TOP_aesdeclast, result, op0, op1, ops );
    break;
   case INTRN_AESENC128:
    Build_OP(TOP_aesenc, result, op0, op1, ops );
    break;
   case INTRN_AESENCLAST128:
    Build_OP(TOP_aesenclast, result, op0, op1, ops );
    break;
   case INTRN_AESIMC128:
    Build_OP(TOP_aesimc, result, op0, ops );
    break;
   case INTRN_AESKEYGENASSIST128:
    Build_OP(TOP_aeskeygenassist, result, op0, op1, ops );
    break;
   // PCLMUL intrinsics
   case INTRN_PCLMULQDQ128:
    Build_OP(TOP_pclmulqdq, result, op0, op1, op2, ops );
    break;
   // AVX intrinsics
   case INTRN_ADDSUBPD256:
    Build_OP(TOP_vfaddsub128v64, result, op0, op1, ops );
    break;
   case INTRN_ADDSUBPS256:
    Build_OP(TOP_vfaddsub128v32, result, op0, op1, ops );
    break;
   case INTRN_ANDNPD256:
    Build_OP(TOP_vfandn128v64, result, op0, op1, ops );
    break;
   case INTRN_ANDNPS256:
    Build_OP(TOP_vfandn128v32, result, op0, op1, ops );
    break;
   case INTRN_BLENDPD256:
    Build_OP(TOP_vfblend128v64, result, op0, op1, op2, ops );
    break;
   case INTRN_BLENDPS256:
    Build_OP(TOP_vfblend128v32, result, op0, op1, op2, ops );
    break;
   case INTRN_BLENDVPD256:
    Build_OP(TOP_vfblendv128v64, result, op0, op1, op2, ops );
    break;
   case INTRN_BLENDVPS256:
    Build_OP(TOP_vfblendv128v32, result, op0, op1, op2, ops );
    break;
   case INTRN_CMPPD:
   case INTRN_CMPPD256:
    Build_OP(TOP_vfcmp128v64, result, op0, op1, op2, ops );
    break;
   case INTRN_CMPPS:
   case INTRN_CMPPS256:
    Build_OP(TOP_vfcmp128v32, result, op0, op1, op2, ops );
    break;
   case INTRN_CMPSD:
    Build_OP(TOP_vcmpsd, result, op0, op1, op2, ops );
    break;
   case INTRN_CMPSS:
    Build_OP(TOP_vcmpss, result, op0, op1, op2, ops );
    break;
   case INTRN_CVTDQ2PD256:
    Build_OP(TOP_vcvtdq2pd, result, op0, ops );
    break;
   case INTRN_CVTDQ2PS256:
    Build_OP(TOP_vcvtdq2ps, result, op0, ops );
    break;
   case INTRN_CVTPD2DQ256:
    Build_OP(TOP_vcvtpd2dqy, result, op0, ops );
    break;
   case INTRN_CVTPD2PS256:
    Build_OP(TOP_vcvtpd2psy, result, op0, ops );
    break;
   case INTRN_CVTPS2DQ256:
    Build_OP(TOP_vcvtps2dq, result, op0, ops );
    break;
   case INTRN_CVTPS2PD256:
    Build_OP(TOP_vcvtps2pd, result, op0, ops );
    break;
   case INTRN_CVTTPD2DQ256:
    Build_OP(TOP_vcvttpd2dqy, result, op0, ops );
    break;
   case INTRN_CVTTPS2DQ256:
    Build_OP(TOP_vcvttps2dq, result, op0, ops );
    break;
   case INTRN_DIVPD256:
    Build_OP(TOP_vfdiv128v64, result, op0, op1, ops );
    break;
   case INTRN_DIVPS256:
    Build_OP(TOP_vfdiv128v32, result, op0, op1, ops );
    break;
   case INTRN_DPPS256:
    Build_OP(TOP_vfdp128v32, result, op0, op1, op2, ops );
    break;
   case INTRN_HADDPD256:
    Build_OP(TOP_vfhadd128v64, result, op0, op1, ops );
    break;
   case INTRN_HADDPS256:
    Build_OP(TOP_vfhadd128v32, result, op0, op1, ops );
    break;
   case INTRN_HSUBPD256:
    Build_OP(TOP_vfhsub128v64, result, op0, op1, ops );
    break;
   case INTRN_HSUBPS256:
    Build_OP(TOP_vfhsub128v32, result, op0, op1, ops );
    break;
   case INTRN_LDDQU256:
    Build_OP(TOP_vlddqu, result, op0, Gen_Literal_TN (0,4), ops );
    break;
   case INTRN_LOADDQU256:
    Build_OP(TOP_vlddqu, result, op0, Gen_Literal_TN (0,4), ops );
    break;
   case INTRN_LOADUPD256:
    Build_OP(TOP_vldupd, result, op0, Gen_Literal_TN (0,4), ops );
    break;
   case INTRN_LOADUPS256:
    Build_OP(TOP_vldups, result, op0, Gen_Literal_TN (0,4), ops );
    break;
   case INTRN_MASKLOADPD:
   case INTRN_MASKLOADPD256:
    Build_OP(TOP_vfmaskld128v64, result, op1, op0, Gen_Literal_TN (0,4), ops );
    break;
   case INTRN_MASKLOADPS:
   case INTRN_MASKLOADPS256:
    Build_OP(TOP_vfmaskld128v32, result, op1, op0, Gen_Literal_TN (0,4), ops );
    break;
   case INTRN_MAXPD256:
    Build_OP(TOP_vfmax128v64, result, op0, op1, ops );
    break;
   case INTRN_MAXPS256:
    Build_OP(TOP_vfmax128v32, result, op0, op1, ops );
    break;
   case INTRN_MINPD256:
    Build_OP(TOP_vfmin128v64, result, op0, op1, ops );
    break;
   case INTRN_MINPS256:
    Build_OP(TOP_vfmin128v32, result, op0, op1, ops );
    break;
   case INTRN_MOVDDUP256:
    Build_OP(TOP_vmovddup, result, op0, ops );
    break;
   case INTRN_MOVMSKPD256:
    Build_OP(TOP_vmovmskpd, result, op0, ops );
    break;
   case INTRN_MOVMSKPS256:
    Build_OP(TOP_vmovmskps, result, op0, ops );
    break;
   case INTRN_MOVSHDUP256:
    Build_OP(TOP_vmovshdup, result, op0, ops );
    break;
   case INTRN_MOVSLDUP256:
    Build_OP(TOP_vmovsldup, result, op0, ops );
    break;
   case INTRN_MULPD256:
    Build_OP(TOP_vfmul128v64, result, op0, op1, ops );
    break;
   case INTRN_MULPS256:
    Build_OP(TOP_vfmul128v32, result, op0, op1, ops );
    break;
   case INTRN_ORPD256:
    Build_OP(TOP_vfor128v64, result, op0, op1, ops );
    break;
   case INTRN_ORPS256:
    Build_OP(TOP_vfor128v32, result, op0, op1, ops );
    break;
   case INTRN_PD256_PD:
    Set_TN_size(op0, 32);
    Build_OP(TOP_vmovapd, result, op0, ops );
    break;
   case INTRN_PD_PD256:
    Set_TN_size(op0, 16);
    Build_OP(TOP_vmovapd, result, op0, ops );
    break;
   case INTRN_PS256_PS:
    Set_TN_size(op0, 32);
    Build_OP(TOP_vmovaps, result, op0, ops );
    break;
   case INTRN_PS_PS256:
    Set_TN_size(op0, 16);
    Build_OP(TOP_vmovaps, result, op0, ops );
    break;
   case INTRN_I2POPCNT:
    if ( Is_Target_SSE42() || Is_Target_Barcelona()) {
      // popcnt available since Barcelona and Nehalem
      Build_OP(TOP_popcnt16, result, op0, ops);
    }
    else {
      FmtAssert(FALSE, ("Target does not support popcount"));
    }
    break;
   case INTRN_I4POPCNT:
    if ( Is_Target_SSE42() || Is_Target_Barcelona()) {
      // popcnt available since Barcelona and Nehalem
      Build_OP(TOP_popcnt32, result, op0, ops);
    }
    else {
      FmtAssert(FALSE, ("Target does not support popcount"));
    }
    break;
   case INTRN_I8POPCNT:
    if ( Is_Target_64bit() && 
         ( Is_Target_SSE42() || Is_Target_Barcelona()) ) {
      // popcnt available since Barcelona and Nehalem
      Build_OP(TOP_popcnt64, result, op0, ops);
    }
    else {
      FmtAssert(FALSE, ("Target does not support popcount"));
    }
    break;
   case INTRN_PTESTC256:
    Build_OP(TOP_vptest128, rflags, op0, op1, ops );
    Build_OP(TOP_setb, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
   case INTRN_PTESTNZC256:
    Build_OP(TOP_vptest128, rflags, op0, op1, ops );
    Build_OP(TOP_seta, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
   case INTRN_PTESTZ256:
    Build_OP(TOP_vptest128, rflags, op0, op1, ops );
    Build_OP(TOP_sete, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
   case INTRN_RCPPS256:
    Build_OP(TOP_vfrcp128v32, result, op0, ops );
    break;
   case INTRN_ROUNDPD256:
    Build_OP(TOP_vround128v64, result, op0, op1, ops );
    break;
   case INTRN_ROUNDPS256:
    Build_OP(TOP_vround128v32, result, op0, op1, ops );
    break;
   case INTRN_RSQRTPS256:
    Build_OP(TOP_vfrsqrt128v32, result, op0, ops );
    break;
   case INTRN_RSQRTPS_NR256:
    FmtAssert(FALSE, ("INTRN_RSQRTPS_NR256 NYI"));
    break;
   case INTRN_SHUFPD256:
    Build_OP(TOP_vshufpd, result, op0, op1, op2, ops );
    break;
   case INTRN_SHUFPS256:
    Build_OP(TOP_vshufps, result, op0, op1, op2, ops );
    break;
   case INTRN_SI256_SI:
    Set_TN_size(op0, 32);
    Build_OP(TOP_vmovdqa, result, op0, ops );
    break;
   case INTRN_SI_SI256:
    Set_TN_size(op0, 16);
    Build_OP(TOP_vmovdqa, result, op0, ops );
    break;
   case INTRN_SQRTPD256:
    Build_OP(TOP_vfsqrt128v64, result, op0, ops );
    break;
   case INTRN_SQRTPS256:
    Build_OP(TOP_vfsqrt128v32, result, op0, ops );
    break;
   case INTRN_SQRTPS_NR256:
    FmtAssert(FALSE, ("INTRN_SQRTPS_NR256 NYI"));
    Build_OP(TOP_nop, result, op0, op1, op2, ops );
    break;
   case INTRN_SUBPD256:
    Build_OP(TOP_vfsub128v64, result, op0, op1, ops );
    break;
   case INTRN_SUBPS256:
    Build_OP(TOP_vfsub128v32, result, op0, op1, ops );
    break;
   case INTRN_UNPCKHPD256:
    Build_OP(TOP_vunpckh128v64, result, op0, op1, ops );
    break;
   case INTRN_UNPCKHPS256:
    Build_OP(TOP_vunpckh128v64, result, op0, op1, ops );
    break;
   case INTRN_UNPCKLPD256:
    Build_OP(TOP_vunpckl128v64, result, op0, op1, ops );
    break;
   case INTRN_UNPCKLPS256:
    Build_OP(TOP_vunpckl128v32, result, op0, op1, ops );
    break;
   case INTRN_VBROADCASTPD256:
    Build_OP(TOP_vfbroadcastf128, result, op0, Gen_Literal_TN (0,4), ops );
    break;
   case INTRN_VBROADCASTPS256:
    Build_OP(TOP_vfbroadcastf128, result, op0, Gen_Literal_TN (0,4), ops );
    break;
   case INTRN_VBROADCASTSD256:
    Build_OP(TOP_vfbroadcastsd, result, op0, Gen_Literal_TN (0,4), ops );
    break;
   case INTRN_VBROADCASTSS:
   case INTRN_VBROADCASTSS256:
    Build_OP(TOP_vfbroadcastss, result, op0, Gen_Literal_TN (0,4), ops );
    break;
   case INTRN_EXTRACTF128PD256:
    Build_OP(TOP_vfextrf128, result, op0, op1, ops );
    break;
   case INTRN_EXTRACTF128PS256:
    Build_OP(TOP_vfextrf128, result, op0, op1, ops );
    break;
   case INTRN_EXTRACTF128SI256:
    Build_OP(TOP_vfextrf128, result, op0, op1, ops );
    break;
   case INTRN_VINSERTF128PD256:
    Build_OP(TOP_vfinsrf128, result, op0, op1, op2, ops );
    break;
   case INTRN_VINSERTF128PS256:
    Build_OP(TOP_vfinsrf128, result, op0, op1, op2, ops );
    break;
   case INTRN_VINSERTF128SI256:
    Build_OP(TOP_vfinsrf128, result, op0, op1, op2, ops );
    break;
   case INTRN_VPERM2F128PD256:
    Build_OP(TOP_vfperm2f128, result, op0, op1, op2, ops );
    break;
   case INTRN_VPERM2F128PS256:
    Build_OP(TOP_vfperm2f128, result, op0, op1, op2, ops );
    break;
   case INTRN_VPERM2F128SI256:
    Build_OP(TOP_vfperm2f128, result, op0, op1, op2, ops );
    break;
   case INTRN_VPERMILPD:
   case INTRN_VPERMILPD256:
    Build_OP(TOP_vfpermi128v64, result, op0, op1, ops );
    break;
   case INTRN_VPERMILPS:
   case INTRN_VPERMILPS256:
    Build_OP(TOP_vfpermi128v32, result, op0, op1, ops );
    break;
   case INTRN_VPERMILVARPD:
   case INTRN_VPERMILVARPD256:
    Build_OP(TOP_vfperm128v64, result, op0, op1, ops );
    break;
   case INTRN_VPERMILVARPS:
   case INTRN_VPERMILVARPS256:
    Build_OP(TOP_vfperm128v32, result, op0, op1, ops );
    break;
   case INTRN_VTESTCPD:
   case INTRN_VTESTCPD256:
    Build_OP(TOP_vtestpd, rflags, op0, op1, ops );
    Build_OP(TOP_setb, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
   case INTRN_VTESTCPS:
   case INTRN_VTESTCPS256:
    Build_OP(TOP_vtestps, rflags, op0, op1, ops );
    Build_OP(TOP_setb, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
   case INTRN_VTESTNZCPD:
   case INTRN_VTESTNZCPD256:
    Build_OP(TOP_vtestpd, rflags, op0, op1, ops );
    Build_OP(TOP_seta, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
   case INTRN_VTESTNZCPS:
   case INTRN_VTESTNZCPS256:
    Build_OP(TOP_vtestps, rflags, op0, op1, ops );
    Build_OP(TOP_seta, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
   case INTRN_VTESTZPD:
   case INTRN_VTESTZPD256:
    Build_OP(TOP_vtestpd, rflags, op0, op1, ops );
    Build_OP(TOP_sete, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
   case INTRN_VTESTZPS:
   case INTRN_VTESTZPS256:
    Build_OP(TOP_vtestps, rflags, op0, op1, ops );
    Build_OP(TOP_sete, result_tmp, rflags, ops );
    need_zero_ext = TRUE;
    break;
   case INTRN_XORPD256:
    Build_OP(TOP_vfxor128v64, result, op0, op1, ops );
    break;
   case INTRN_XORPS256:
    Build_OP(TOP_vfxor128v32, result, op0, op1, ops );
    break;
   /* FMA3 intrinsics: form1 */
   case INTRN_VFMADDPD_132:
   case INTRN_VFMADDPD256_132:
    Build_OP(TOP_xfmadd132pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDPS_132:
   case INTRN_VFMADDPS256_132:
    Build_OP(TOP_xfmadd132ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSD_132:
    Build_OP(TOP_xfmadd132sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSS_132:
    Build_OP(TOP_xfmadd132ss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSUBPD_132:
   case INTRN_VFMADDSUBPD256_132:
    Build_OP(TOP_xfmaddsub132pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSUBPS_132:
   case INTRN_VFMADDSUBPS256_132:
    Build_OP(TOP_xfmaddsub132ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBADDPD_132:
   case INTRN_VFMSUBADDPD256_132:
    Build_OP(TOP_xfmsubadd132pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBADDPS_132:
   case INTRN_VFMSUBADDPS256_132:
    Build_OP(TOP_xfmsubadd132ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBPD_132:
   case INTRN_VFMSUBPD256_132:
    Build_OP(TOP_xfmsub132pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBPS_132:
   case INTRN_VFMSUBPS256_132:
    Build_OP(TOP_xfmsub132ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBSD_132:
    Build_OP(TOP_xfmsub132sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBSS_132:
    Build_OP(TOP_xfmsub132ss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDPD_132:
   case INTRN_VFNMADDPD256_132:
    Build_OP(TOP_xfnmadd132pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDPS_132:
   case INTRN_VFNMADDPS256_132:
    Build_OP(TOP_xfnmadd132ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDSD_132:
    Build_OP(TOP_xfnmadd132sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDSS_132:
    Build_OP(TOP_xfnmadd132ss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBPD_132:
   case INTRN_VFNMSUBPD256_132:
    Build_OP(TOP_xfnmsub132pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBPS_132:
   case INTRN_VFNMSUBPS256_132:
    Build_OP(TOP_xfnmsub132ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBSD_132:
    Build_OP(TOP_xfnmsub132sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBSS_132:
    Build_OP(TOP_xfnmsub132ss, result, op0, op1, op2, ops );
    break;
   /* FMA3 intrinsics: form2 */
   case INTRN_VFMADDPD_213:
   case INTRN_VFMADDPD256_213:
    Build_OP(TOP_xfmadd213pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDPS_213:
   case INTRN_VFMADDPS256_213:
    Build_OP(TOP_xfmadd213ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSD_213:
    Build_OP(TOP_xfmadd213sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSS_213:
    Build_OP(TOP_xfmadd213ss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSUBPD_213:
   case INTRN_VFMADDSUBPD256_213:
    Build_OP(TOP_xfmaddsub213pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSUBPS_213:
   case INTRN_VFMADDSUBPS256_213:
    Build_OP(TOP_xfmaddsub213ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBADDPD_213:
   case INTRN_VFMSUBADDPD256_213:
    Build_OP(TOP_xfmsubadd213pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBADDPS_213:
   case INTRN_VFMSUBADDPS256_213:
    Build_OP(TOP_xfmsubadd213ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBPD_213:
   case INTRN_VFMSUBPD256_213:
    Build_OP(TOP_xfmsub213pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBPS_213:
   case INTRN_VFMSUBPS256_213:
    Build_OP(TOP_xfmsub213ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBSD_213:
    Build_OP(TOP_xfmsub213sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBSS_213:
    Build_OP(TOP_xfmsub213ss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDPD_213:
   case INTRN_VFNMADDPD256_213:
    Build_OP(TOP_xfnmadd213pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDPS_213:
   case INTRN_VFNMADDPS256_213:
    Build_OP(TOP_xfnmadd213ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDSD_213:
    Build_OP(TOP_xfnmadd213sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDSS_213:
    Build_OP(TOP_xfnmadd213ss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBPD_213:
   case INTRN_VFNMSUBPD256_213:
    Build_OP(TOP_xfnmsub213pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBPS_213:
   case INTRN_VFNMSUBPS256_213:
    Build_OP(TOP_xfnmsub213ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBSD_213:
    Build_OP(TOP_xfnmsub213sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBSS_213:
    Build_OP(TOP_xfnmsub213ss, result, op0, op1, op2, ops );
    break;
   /* FMA3 intrinsics: form3 */
   case INTRN_VFMADDPD_231:
   case INTRN_VFMADDPD256_231:
    Build_OP(TOP_xfmadd231pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDPS_231:
   case INTRN_VFMADDPS256_231:
    Build_OP(TOP_xfmadd231ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSD_231:
    Build_OP(TOP_xfmadd231sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSS_231:
    Build_OP(TOP_xfmadd231ss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSUBPD_231:
   case INTRN_VFMADDSUBPD256_231:
    Build_OP(TOP_xfmaddsub231pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSUBPS_231:
   case INTRN_VFMADDSUBPS256_231:
    Build_OP(TOP_xfmaddsub231ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBADDPD_231:
   case INTRN_VFMSUBADDPD256_231:
    Build_OP(TOP_xfmsubadd231pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBADDPS_231:
   case INTRN_VFMSUBADDPS256_231:
    Build_OP(TOP_xfmsubadd231ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBPD_231:
   case INTRN_VFMSUBPD256_231:
    Build_OP(TOP_xfmsub231pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBPS_231:
   case INTRN_VFMSUBPS256_231:
    Build_OP(TOP_xfmsub231ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBSD_231:
    Build_OP(TOP_xfmsub231sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBSS_231:
    Build_OP(TOP_xfmsub231ss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDPD_231:
   case INTRN_VFNMADDPD256_231:
    Build_OP(TOP_xfnmadd231pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDPS_231:
   case INTRN_VFNMADDPS256_231:
    Build_OP(TOP_xfnmadd231ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDSD_231:
    Build_OP(TOP_xfnmadd231sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDSS_231:
    Build_OP(TOP_xfnmadd231ss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBPD_231:
   case INTRN_VFNMSUBPD256_231:
    Build_OP(TOP_xfnmsub231pd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBPS_231:
   case INTRN_VFNMSUBPS256_231:
    Build_OP(TOP_xfnmsub231ps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBSD_231:
    Build_OP(TOP_xfnmsub231sd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBSS_231:
    Build_OP(TOP_xfnmsub231ss, result, op0, op1, op2, ops );
    break;
   /* FMA4 intrinsics */
   case INTRN_VFMADDPD:
   case INTRN_VFMADDPD256:
    Build_OP(TOP_vfmaddpd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDPS:
   case INTRN_VFMADDPS256:
    Build_OP(TOP_vfmaddps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSD:
    Build_OP(TOP_vfmaddsd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSS:
    Build_OP(TOP_vfmaddss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSUBPD:
   case INTRN_VFMADDSUBPD256:
    Build_OP(TOP_vfmaddsubpd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMADDSUBPS:
   case INTRN_VFMADDSUBPS256:
    Build_OP(TOP_vfmaddsubps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBADDPD:
   case INTRN_VFMSUBADDPD256:
    Build_OP(TOP_vfmsubaddpd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBADDPS:
   case INTRN_VFMSUBADDPS256:
    Build_OP(TOP_vfmsubaddps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBPD:
   case INTRN_VFMSUBPD256:
    Build_OP(TOP_vfmsubpd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBPS:
   case INTRN_VFMSUBPS256:
    Build_OP(TOP_vfmsubps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBSD:
    Build_OP(TOP_vfmsubsd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFMSUBSS:
    Build_OP(TOP_vfmsubss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDPD:
   case INTRN_VFNMADDPD256:
    Build_OP(TOP_vfnmaddpd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDPS:
   case INTRN_VFNMADDPS256:
    Build_OP(TOP_vfnmaddps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDSD:
    Build_OP(TOP_vfnmaddsd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMADDSS:
    Build_OP(TOP_vfnmaddss, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBPD:
   case INTRN_VFNMSUBPD256:
    Build_OP(TOP_vfnmsubpd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBPS:
   case INTRN_VFNMSUBPS256:
    Build_OP(TOP_vfnmsubps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBSD:
    Build_OP(TOP_vfnmsubsd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFNMSUBSS:
    Build_OP(TOP_vfnmsubss, result, op0, op1, op2, ops );
    break;
   /* XOP intrinsics */
   case INTRN_VFRCZPD:
   case INTRN_VFRCZPD256:
    Build_OP(TOP_vfrczpd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFRCZPS:
   case INTRN_VFRCZPS256:
    Build_OP(TOP_vfrczps, result, op0, op1, op2, ops );
    break;
   case INTRN_VFRCZSD:
    Build_OP(TOP_vfrczsd, result, op0, op1, op2, ops );
    break;
   case INTRN_VFRCZSS:
    Build_OP(TOP_vfrczss, result, op0, op1, op2, ops );
    break;
   case INTRN_VPCMOV:
   case INTRN_VPCMOV256:
   case INTRN_VPCMOV_V16HI256:
   case INTRN_VPCMOV_V16QI:
   case INTRN_VPCMOV_V2DF:
   case INTRN_VPCMOV_V2DI:
   case INTRN_VPCMOV_V32QI256:
   case INTRN_VPCMOV_V4DF256:
   case INTRN_VPCMOV_V4DI256:
   case INTRN_VPCMOV_V4SF:
   case INTRN_VPCMOV_V4SI:
   case INTRN_VPCMOV_V8HI:
   case INTRN_VPCMOV_V8SF256:
   case INTRN_VPCMOV_V8SI256:
    Build_OP(TOP_vpcmov, result, op0, op1, op2, ops );
    break;
   case INTRN_VPCOMEQB:
    Build_OP(TOP_vpcomb, result, op0, op1, Gen_Literal_TN(VPCOM_EQ, 4), ops );
    break;
   case INTRN_VPCOMEQD:
    Build_OP(TOP_vpcomd, result, op0, op1, Gen_Literal_TN(VPCOM_EQ, 4), ops );
    break;
   case INTRN_VPCOMEQQ:
    Build_OP(TOP_vpcomq, result, op0, op1, Gen_Literal_TN(VPCOM_EQ, 4), ops );
    break;
   case INTRN_VPCOMEQUB:
    Build_OP(TOP_vpcomub, result, op0, op1, Gen_Literal_TN(VPCOM_EQ, 4), ops );
    break;
   case INTRN_VPCOMEQUD:
    Build_OP(TOP_vpcomud, result, op0, op1, Gen_Literal_TN(VPCOM_EQ, 4), ops );
    break;
   case INTRN_VPCOMEQUQ:
    Build_OP(TOP_vpcomuq, result, op0, op1, Gen_Literal_TN(VPCOM_EQ, 4), ops );
    break;
   case INTRN_VPCOMEQUW:
    Build_OP(TOP_vpcomuw, result, op0, op1, Gen_Literal_TN(VPCOM_EQ, 4), ops );
    break;
   case INTRN_VPCOMEQW:
    Build_OP(TOP_vpcomw, result, op0, op1, Gen_Literal_TN(VPCOM_EQ, 4), ops );
    break;
   case INTRN_VPCOMFALSEB:
    Build_OP(TOP_vpcomb, result, op0, op1, Gen_Literal_TN(VPCOM_FALSE, 4), ops );
    break;
   case INTRN_VPCOMFALSED:
    Build_OP(TOP_vpcomd, result, op0, op1, Gen_Literal_TN(VPCOM_FALSE, 4), ops );
    break;
   case INTRN_VPCOMFALSEQ:
    Build_OP(TOP_vpcomq, result, op0, op1, Gen_Literal_TN(VPCOM_FALSE, 4), ops );
    break;
   case INTRN_VPCOMFALSEUB:
    Build_OP(TOP_vpcomub, result, op0, op1, Gen_Literal_TN(VPCOM_FALSE, 4), ops );
    break;
   case INTRN_VPCOMFALSEUD:
    Build_OP(TOP_vpcomud, result, op0, op1, Gen_Literal_TN(VPCOM_FALSE, 4), ops );
    break;
   case INTRN_VPCOMFALSEUQ:
    Build_OP(TOP_vpcomuq, result, op0, op1, Gen_Literal_TN(VPCOM_FALSE, 4), ops );
    break;
   case INTRN_VPCOMFALSEUW:
    Build_OP(TOP_vpcomuw, result, op0, op1, Gen_Literal_TN(VPCOM_FALSE, 4), ops );
    break;
   case INTRN_VPCOMFALSEW:
    Build_OP(TOP_vpcomw, result, op0, op1, Gen_Literal_TN(VPCOM_FALSE, 4), ops );
    break;
   case INTRN_VPCOMGEB:
    Build_OP(TOP_vpcomb, result, op0, op1, Gen_Literal_TN(VPCOM_GE, 4), ops );
    break;
   case INTRN_VPCOMGED:
    Build_OP(TOP_vpcomd, result, op0, op1, Gen_Literal_TN(VPCOM_GE, 4), ops );
    break;
   case INTRN_VPCOMGEQ:
    Build_OP(TOP_vpcomq, result, op0, op1, Gen_Literal_TN(VPCOM_GE, 4), ops );
    break;
   case INTRN_VPCOMGEUB:
    Build_OP(TOP_vpcomub, result, op0, op1, Gen_Literal_TN(VPCOM_GE, 4), ops );
    break;
   case INTRN_VPCOMGEUD:
    Build_OP(TOP_vpcomud, result, op0, op1, Gen_Literal_TN(VPCOM_GE, 4), ops );
    break;
   case INTRN_VPCOMGEUQ:
    Build_OP(TOP_vpcomuq, result, op0, op1, Gen_Literal_TN(VPCOM_GE, 4), ops );
    break;
   case INTRN_VPCOMGEUW:
    Build_OP(TOP_vpcomuw, result, op0, op1, Gen_Literal_TN(VPCOM_GE, 4), ops );
    break;
   case INTRN_VPCOMGEW:
    Build_OP(TOP_vpcomw, result, op0, op1, Gen_Literal_TN(VPCOM_GE, 4), ops );
    break;
   case INTRN_VPCOMGTB:
    Build_OP(TOP_vpcomb, result, op0, op1, Gen_Literal_TN(VPCOM_GT, 4), ops );
    break;
   case INTRN_VPCOMGTD:
    Build_OP(TOP_vpcomd, result, op0, op1, Gen_Literal_TN(VPCOM_GT, 4), ops );
    break;
   case INTRN_VPCOMGTQ:
    Build_OP(TOP_vpcomq, result, op0, op1, Gen_Literal_TN(VPCOM_GT, 4), ops );
    break;
   case INTRN_VPCOMGTUB:
    Build_OP(TOP_vpcomub, result, op0, op1, Gen_Literal_TN(VPCOM_GT, 4), ops );
    break;
   case INTRN_VPCOMGTUD:
    Build_OP(TOP_vpcomud, result, op0, op1, Gen_Literal_TN(VPCOM_GT, 4), ops );
    break;
   case INTRN_VPCOMGTUQ:
    Build_OP(TOP_vpcomuq, result, op0, op1, Gen_Literal_TN(VPCOM_GT, 4), ops );
    break;
   case INTRN_VPCOMGTUW:
    Build_OP(TOP_vpcomuw, result, op0, op1, Gen_Literal_TN(VPCOM_GT, 4), ops );
    break;
   case INTRN_VPCOMGTW:
    Build_OP(TOP_vpcomw, result, op0, op1, Gen_Literal_TN(VPCOM_GT, 4), ops );
    break;
   case INTRN_VPCOMLEB:
    Build_OP(TOP_vpcomb, result, op0, op1, Gen_Literal_TN(VPCOM_LE, 4), ops );
    break;
   case INTRN_VPCOMLED:
    Build_OP(TOP_vpcomd, result, op0, op1, Gen_Literal_TN(VPCOM_LE, 4), ops );
    break;
   case INTRN_VPCOMLEQ:
    Build_OP(TOP_vpcomq, result, op0, op1, Gen_Literal_TN(VPCOM_LE, 4), ops );
    break;
   case INTRN_VPCOMLEUB:
    Build_OP(TOP_vpcomub, result, op0, op1, Gen_Literal_TN(VPCOM_LE, 4), ops );
    break;
   case INTRN_VPCOMLEUD:
    Build_OP(TOP_vpcomud, result, op0, op1, Gen_Literal_TN(VPCOM_LE, 4), ops );
    break;
   case INTRN_VPCOMLEUQ:
    Build_OP(TOP_vpcomuq, result, op0, op1, Gen_Literal_TN(VPCOM_LE, 4), ops );
    break;
   case INTRN_VPCOMLEUW:
    Build_OP(TOP_vpcomuw, result, op0, op1, Gen_Literal_TN(VPCOM_LE, 4), ops );
    break;
   case INTRN_VPCOMLEW:
    Build_OP(TOP_vpcomw, result, op0, op1, Gen_Literal_TN(VPCOM_LE, 4), ops );
    break;
   case INTRN_VPCOMLTB:
    Build_OP(TOP_vpcomb, result, op0, op1, Gen_Literal_TN(VPCOM_LT, 4), ops );
    break;
   case INTRN_VPCOMLTD:
    Build_OP(TOP_vpcomd, result, op0, op1, Gen_Literal_TN(VPCOM_LT, 4), ops );
    break;
   case INTRN_VPCOMLTQ:
    Build_OP(TOP_vpcomq, result, op0, op1, Gen_Literal_TN(VPCOM_LT, 4), ops );
    break;
   case INTRN_VPCOMLTUB:
    Build_OP(TOP_vpcomub, result, op0, op1, Gen_Literal_TN(VPCOM_LT, 4), ops );
    break;
   case INTRN_VPCOMLTUD:
    Build_OP(TOP_vpcomud, result, op0, op1, Gen_Literal_TN(VPCOM_LT, 4), ops );
    break;
   case INTRN_VPCOMLTUQ:
    Build_OP(TOP_vpcomuq, result, op0, op1, Gen_Literal_TN(VPCOM_LT, 4), ops );
    break;
   case INTRN_VPCOMLTUW:
    Build_OP(TOP_vpcomuw, result, op0, op1, Gen_Literal_TN(VPCOM_LT, 4), ops );
    break;
   case INTRN_VPCOMLTW:
    Build_OP(TOP_vpcomw, result, op0, op1, Gen_Literal_TN(VPCOM_LT, 4), ops );
    break;
   case INTRN_VPCOMNEB:
    Build_OP(TOP_vpcomb, result, op0, op1, Gen_Literal_TN(VPCOM_NE, 4), ops );
    break;
   case INTRN_VPCOMNED:
    Build_OP(TOP_vpcomd, result, op0, op1, Gen_Literal_TN(VPCOM_NE, 4), ops );
    break;
   case INTRN_VPCOMNEQ:
    Build_OP(TOP_vpcomq, result, op0, op1, Gen_Literal_TN(VPCOM_NE, 4), ops );
    break;
   case INTRN_VPCOMNEUB:
    Build_OP(TOP_vpcomub, result, op0, op1, Gen_Literal_TN(VPCOM_NE, 4), ops );
    break;
   case INTRN_VPCOMNEUD:
    Build_OP(TOP_vpcomud, result, op0, op1, Gen_Literal_TN(VPCOM_NE, 4), ops );
    break;
   case INTRN_VPCOMNEUQ:
    Build_OP(TOP_vpcomuq, result, op0, op1, Gen_Literal_TN(VPCOM_NE, 4), ops );
    break;
   case INTRN_VPCOMNEUW:
    Build_OP(TOP_vpcomuw, result, op0, op1, Gen_Literal_TN(VPCOM_NE, 4), ops );
    break;
   case INTRN_VPCOMNEW:
    Build_OP(TOP_vpcomw, result, op0, op1, Gen_Literal_TN(VPCOM_NE, 4), ops );
    break;
   case INTRN_VPCOMTRUEB:
    Build_OP(TOP_vpcomb, result, op0, op1, Gen_Literal_TN(VPCOM_TRUE, 4), ops );
    break;
   case INTRN_VPCOMTRUED:
    Build_OP(TOP_vpcomd, result, op0, op1, Gen_Literal_TN(VPCOM_TRUE, 4), ops );
    break;
   case INTRN_VPCOMTRUEQ:
    Build_OP(TOP_vpcomq, result, op0, op1, Gen_Literal_TN(VPCOM_TRUE, 4), ops );
    break;
   case INTRN_VPCOMTRUEUB:
    Build_OP(TOP_vpcomub, result, op0, op1, Gen_Literal_TN(VPCOM_TRUE, 4), ops );
    break;
   case INTRN_VPCOMTRUEUD:
    Build_OP(TOP_vpcomud, result, op0, op1, Gen_Literal_TN(VPCOM_TRUE, 4), ops );
    break;
   case INTRN_VPCOMTRUEUQ:
    Build_OP(TOP_vpcomuq, result, op0, op1, Gen_Literal_TN(VPCOM_TRUE, 4), ops );
    break;
   case INTRN_VPCOMTRUEUW:
    Build_OP(TOP_vpcomuw, result, op0, op1, Gen_Literal_TN(VPCOM_TRUE, 4), ops );
    break;
   case INTRN_VPCOMTRUEW:
    Build_OP(TOP_vpcomw, result, op0, op1, Gen_Literal_TN(VPCOM_TRUE, 4), ops );
    break;
   case INTRN_VPHADDBD:
    Build_OP(TOP_vphaddbd, result, op0, ops );
    break;
   case INTRN_VPHADDBQ:
    Build_OP(TOP_vphaddbq, result, op0, ops );
    break;
   case INTRN_VPHADDBW:
    Build_OP(TOP_vphaddbw, result, op0, ops );
    break;
   case INTRN_VPHADDDQ:
    Build_OP(TOP_vphadddq, result, op0, ops );
    break;
   case INTRN_VPHADDUBD:
    Build_OP(TOP_vphaddubd, result, op0, ops );
    break;
   case INTRN_VPHADDUBQ:
    Build_OP(TOP_vphaddubq, result, op0, ops );
    break;
   case INTRN_VPHADDUBW:
    Build_OP(TOP_vphaddubw, result, op0, ops );
    break;
   case INTRN_VPHADDUDQ:
    Build_OP(TOP_vphaddudq, result, op0, ops );
    break;
   case INTRN_VPHADDUWD:
    Build_OP(TOP_vphadduwd, result, op0, ops );
    break;
   case INTRN_VPHADDUWQ:
    Build_OP(TOP_vphadduwq, result, op0, ops );
    break;
   case INTRN_VPHADDWD:
    Build_OP(TOP_vphaddwd, result, op0, ops );
    break;
   case INTRN_VPHADDWQ:
    Build_OP(TOP_vphaddwq, result, op0, ops );
    break;
   case INTRN_VPHSUBBW:
    Build_OP(TOP_vphsubbw, result, op0, ops );
    break;
   case INTRN_VPHSUBDQ:
    Build_OP(TOP_vphsubdq, result, op0, ops );
    break;
   case INTRN_VPHSUBWD:
    Build_OP(TOP_vphsubwd, result, op0, ops );
    break;
   case INTRN_VPMACSDD:
    Build_OP(TOP_vpmacsdd, result, op0, op1, op2, ops );
    break;
   case INTRN_VPMACSDQH:
    Build_OP(TOP_vpmacsdqh, result, op0, op1, op2, ops );
    break;
   case INTRN_VPMACSDQL:
    Build_OP(TOP_vpmacsdql, result, op0, op1, op2, ops );
    break;
   case INTRN_VPMACSSDD:
    Build_OP(TOP_vpmacssdd, result, op0, op1, op2, ops );
    break;
   case INTRN_VPMACSSDQH:
    Build_OP(TOP_vpmacssdqh, result, op0, op1, op2, ops );
    break;
   case INTRN_VPMACSSDQL:
    Build_OP(TOP_vpmacssdql, result, op0, op1, op2, ops );
    break;
   case INTRN_VPMACSSWD:
    Build_OP(TOP_vpmacsswd, result, op0, op1, op2, ops );
    break;
   case INTRN_VPMACSSWW:
    Build_OP(TOP_vpmacssww, result, op0, op1, op2, ops );
    break;
   case INTRN_VPMACSWD:
    Build_OP(TOP_vpmacswd, result, op0, op1, op2, ops );
    break;
   case INTRN_VPMACSWW:
    Build_OP(TOP_vpmacsww, result, op0, op1, op2, ops );
    break;
   case INTRN_VPMADCSSWD:
    Build_OP(TOP_vpmadcsswd, result, op0, op1, op2, ops );
    break;
   case INTRN_VPMADCSWD:
    Build_OP(TOP_vpmadcswd, result, op0, op1, op2, ops );
    break;
   case INTRN_VPPERM:
    Build_OP(TOP_vpperm, result, op0, op1, op2, ops );
    break;
   case INTRN_VPROTB:
    Build_OP(TOP_vprotb, result, op0, op1, ops );
    break;
   case INTRN_VPROTB_IMM:
    Build_OP(TOP_vprotbi, result, op0, op1, ops );
    break;
   case INTRN_VPROTD:
    Build_OP(TOP_vprotd, result, op0, op1, ops );
    break;
   case INTRN_VPROTD_IMM:
    Build_OP(TOP_vprotdi, result, op0, op1, ops );
    break;
   case INTRN_VPROTQ:
    Build_OP(TOP_vprotq, result, op0, op1, ops );
    break;
   case INTRN_VPROTQ_IMM:
    Build_OP(TOP_vprotqi, result, op0, op1, ops );
    break;
   case INTRN_VPROTW:
    Build_OP(TOP_vprotw, result, op0, op1, ops );
    break;
   case INTRN_VPROTW_IMM:
    Build_OP(TOP_vprotwi, result, op0, op1, ops );
    break;
   case INTRN_VPSHAB:
    Build_OP(TOP_vpshab, result, op0, op1, ops );
    break;
   case INTRN_VPSHAD:
    Build_OP(TOP_vpshad, result, op0, op1, ops );
    break;
   case INTRN_VPSHAQ:
    Build_OP(TOP_vpshaq, result, op0, op1, ops );
    break;
   case INTRN_VPSHAW:
    Build_OP(TOP_vpshaw, result, op0, op1, ops );
    break;
   case INTRN_VPSHLB:
    Build_OP(TOP_vpshlb, result, op0, op1, ops );
    break;
   case INTRN_VPSHLD:
    Build_OP(TOP_vpshld, result, op0, op1, ops );
    break;
   case INTRN_VPSHLQ:
    Build_OP(TOP_vpshlq, result, op0, op1, ops );
    break;
   case INTRN_VPSHLW:
    Build_OP(TOP_vpshlw, result, op0, op1, ops );
    break;
  }

  if ( need_zero_ext == TRUE )
    Build_OP( TN_size(result) == 4 ? TOP_movzbl : TOP_movzbq,
    	      result, result_tmp, ops);
  return;
}

/* ======================================================================
 * Expand_TOP_intrncall
 * 
 * Given a TOP_intrncall <op>, expand it into the sequence of instructions 
 * that must be generated. If <get_sequence_length> is TRUE, return only
 * the number of instructions in the sequence and don't actually do the 
 * expansion.
 * ======================================================================*/
static INT
Expand_TOP_intrncall (
  const OP *op, 
  OPS *ops, 
  BOOL get_sequence_length,
  INT pc_value)
{
  ErrMsg( EC_Unimplemented, "Expand_TOP_intrncall: NYI" );
  return 0;
}

static BOOL
Intrinsic_Returns_New_Value (INTRINSIC id)
{
  switch (id) {
  case INTRN_ADD_AND_FETCH_I1:
  case INTRN_ADD_AND_FETCH_I2:
  case INTRN_ADD_AND_FETCH_I4:
  case INTRN_ADD_AND_FETCH_I8:
  case INTRN_SUB_AND_FETCH_I1:
  case INTRN_SUB_AND_FETCH_I2:
  case INTRN_SUB_AND_FETCH_I4:
  case INTRN_SUB_AND_FETCH_I8:
  case INTRN_OR_AND_FETCH_I1:
  case INTRN_OR_AND_FETCH_I2:
  case INTRN_OR_AND_FETCH_I4:
  case INTRN_OR_AND_FETCH_I8:
  case INTRN_XOR_AND_FETCH_I1:
  case INTRN_XOR_AND_FETCH_I2:
  case INTRN_XOR_AND_FETCH_I4:
  case INTRN_XOR_AND_FETCH_I8:
  case INTRN_AND_AND_FETCH_I1:
  case INTRN_AND_AND_FETCH_I2:
  case INTRN_AND_AND_FETCH_I4:
  case INTRN_AND_AND_FETCH_I8:
  case INTRN_NAND_AND_FETCH_I1:
  case INTRN_NAND_AND_FETCH_I2:
  case INTRN_NAND_AND_FETCH_I4:
  case INTRN_NAND_AND_FETCH_I8:
	return TRUE;
  default:
	return FALSE;
  }
}

// initial expansion of intrinsic call (may not be complete lowering).
// return result TN (if set).
// If the intrinsic requires a label and loop (2 bb's)
// then ops is for first bb and ops2 is for bb after the label.
// Otherwise only ops is filled in.
TN *
Exp_Intrinsic_Call (INTRINSIC id, TN *op0, TN *op1, TN *op2,
                    OPS *ops, LABEL_IDX *label, OPS *loop_ops)
{
  TN *result = NULL;
  TN *neg_tn = NULL;    // for FETCH_AND_SUB, SUB_AND_FETCH
  TYPE_ID mtype;

  switch (id) {
  case INTRN_SYNCHRONIZE:
    if (Is_Target_SSE2())
      Build_OP (TOP_mfence, ops);    // memory fence
    // else generate nothing since pre-SSE2 CPUs do not do aggressive reordering
    break;
  case INTRN_EMMS:
    Build_OP (TOP_emms, ops);
    break;
  case INTRN_CLFLUSH:
    Build_OP (TOP_clflush, op0, Gen_Literal_TN (0, 4), ops);
    break;
  case INTRN_STOREAPS:
    Build_OP (TOP_staps, op1, op0, Gen_Literal_TN (0, 4), ops);
    break;
  case INTRN_MOVNTDQ:
    Build_OP (TOP_storenti128, op1, op0, Gen_Literal_TN (0,4), ops);
    break;
  case INTRN_MOVNTPS:
    Build_OP (TOP_stntps, op1, op0, Gen_Literal_TN (0,4), ops);
    break;
  case INTRN_SSE_ZERO:
    result = Build_TN_Of_Mtype (MTYPE_V16F4);
    Build_OP (TOP_xzero128v32, result, ops);
    break;
  case INTRN_CLRTI:
    result = Build_TN_Of_Mtype (MTYPE_V16F4);
    Build_OP (TOP_xzero128v64, result, ops);
    break;
  case INTRN_STORELPD:
    Build_OP (TOP_storelpd, op1, op0, Gen_Literal_TN (0,4), ops);
    break;
  case INTRN_STOREHPD:
    Build_OP (TOP_sthpd, op1, op0, Gen_Literal_TN (0, 4), ops);
    break;
  case INTRN_LFENCE:
    Build_OP (TOP_lfence, ops);
    break;
  case INTRN_MFENCE:
    Build_OP (TOP_mfence, ops);
    break;
  case INTRN_SFENCE:
    Build_OP (TOP_sfence, ops);
    break;
  case INTRN_STOREDQA:
    Build_OP( TOP_stdqa, op1, op0, Gen_Literal_TN (0, 4), ops );
    break;
  case INTRN_STOREDQU:
    Build_OP( TOP_stdqu, op1, op0, Gen_Literal_TN (0, 4), ops );
    break;
  case INTRN_STOREUPS:
    Build_OP( TOP_stups, op1, op0, Gen_Literal_TN (0, 4), ops );
    break;
  case INTRN_STOREUPD:
    Build_OP( TOP_stupd, op1, op0, Gen_Literal_TN (0, 4), ops );
    break;
  case INTRN_STOREHPS:
    Build_OP( TOP_sthps, op1, op0, Gen_Literal_TN (0, 4), ops );
    break;
  case INTRN_STORELPS:
    Build_OP( TOP_stlps, op1, op0, Gen_Literal_TN (0, 4), ops );
    break;
  case INTRN_MOVNTPD:
    Build_OP (TOP_stntpd, op1, op0, Gen_Literal_TN (0,4), ops);
    break;
  case INTRN_MOVNTI:
    Build_OP (TOP_storenti32, op1, op0, Gen_Literal_TN (0,4), ops);
    break;
  case INTRN_MOVNTQ:
    if (TN_register_class(op1) != ISA_REGISTER_CLASS_mmx) {
      TN *tmp1 = Build_RCLASS_TN(ISA_REGISTER_CLASS_mmx);
      Exp_COPY( tmp1, op1, ops );
      op1 = tmp1;
    }
    Build_OP (TOP_storent64_fm, op1, op0, Gen_Literal_TN (0,4), ops);
    break;
  case INTRN_MASKMOVDQU:
    Build_OP (TOP_maskmovdqu, op1, op0, ops);
    break;
  case INTRN_MASKMOVQ:
    Build_OP (TOP_maskmovq, op1, op0, ops);
    break;

  //SSE4A instrinsics
  case INTRN_MOVNTSS:
    Build_OP (TOP_stntss, op1, op0, Gen_Literal_TN (0,4), ops);
    break;
  case INTRN_MOVNTSD:
    Build_OP (TOP_stntsd, op1, op0, Gen_Literal_TN (0,4), ops);
    break;

  // AVX instrinsics
  case INTRN_MASKSTOREPD:
  case INTRN_MASKSTOREPD256:
    Build_OP(TOP_vfmaskst128v64, op2, op1, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_MASKSTOREPS:
  case INTRN_MASKSTOREPS256:
    Build_OP(TOP_vfmaskst128v32, op2, op1, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_MOVNTDQ256:
    Build_OP(TOP_vstntdq, op1, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_MOVNTPD256:
    Build_OP(TOP_vstntpd, op1, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_MOVNTPS256:
    Build_OP(TOP_vstntps, op1, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_STOREDQU256:
    Build_OP(TOP_vstdqu, op1, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_STOREUPD256:
    Build_OP(TOP_vstupd, op1, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_STOREUPS256:
    Build_OP(TOP_vstups, op1, op0, Gen_Literal_TN (0,4), ops );
    break;
  case INTRN_VZEROALL:
    Build_OP(TOP_vzeroall, ops );
    break;
  case INTRN_VZEROUPPER:
    Build_OP(TOP_vzeroupper, ops );
    break;

  // FETCH_AND_...

  case INTRN_FETCH_AND_ADD_I1:
    result = Exp_Fetch_and_Add(op0, op1, MTYPE_I1, ops);
    break;
  case INTRN_FETCH_AND_ADD_I2:
    result = Exp_Fetch_and_Add(op0, op1, MTYPE_I2, ops);
    break;
  case INTRN_FETCH_AND_ADD_I4:
    result = Exp_Fetch_and_Add(op0, op1, MTYPE_I4, ops);
    break;
  case INTRN_FETCH_AND_ADD_I8:
    result = Exp_Fetch_and_Add(op0, op1, MTYPE_I8, ops);
    break;

  case INTRN_FETCH_AND_SUB_I1: 
    neg_tn = Build_TN_Like(op1);
    Build_OP(TOP_neg8, neg_tn, op1, ops);
    result = Exp_Fetch_and_Add(op0, neg_tn, MTYPE_I1, ops);
    break;
  case INTRN_FETCH_AND_SUB_I2: 
    neg_tn = Build_TN_Like(op1);
    Build_OP(TOP_neg16, neg_tn, op1, ops);
    result = Exp_Fetch_and_Add(op0, neg_tn, MTYPE_I2, ops);
    break;
  case INTRN_FETCH_AND_SUB_I4: 
    neg_tn = Build_TN_Like(op1);
    Build_OP(TOP_neg32, neg_tn, op1, ops);
    result = Exp_Fetch_and_Add(op0, neg_tn, MTYPE_I4, ops);
    break;
  case INTRN_FETCH_AND_SUB_I8:
    neg_tn = Build_TN_Like(op1);
    Build_OP(TOP_neg64, neg_tn, op1, ops);
    result = Exp_Fetch_and_Add(op0, neg_tn, MTYPE_I8, ops);
    break;

  case INTRN_FETCH_AND_OR_I1:
    result = Exp_Fetch_and_Or(op0, op1, MTYPE_I1, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_OR_I2:
    result = Exp_Fetch_and_Or(op0, op1, MTYPE_I2, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_OR_I4:
    result = Exp_Fetch_and_Or(op0, op1, MTYPE_I4, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_OR_I8:
    result = Exp_Fetch_and_Or(op0, op1, MTYPE_I8, ops, label, loop_ops);
    break;

  case INTRN_FETCH_AND_XOR_I1:
    result = Exp_Fetch_and_Xor(op0, op1, MTYPE_I1, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_XOR_I2:
    result = Exp_Fetch_and_Xor(op0, op1, MTYPE_I2, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_XOR_I4:
    result = Exp_Fetch_and_Xor(op0, op1, MTYPE_I4, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_XOR_I8:
    result = Exp_Fetch_and_Xor(op0, op1, MTYPE_I8, ops, label, loop_ops);
    break;

  case INTRN_FETCH_AND_AND_I1:
    result = Exp_Fetch_and_And(op0, op1, MTYPE_I1, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_AND_I2:
    result = Exp_Fetch_and_And(op0, op1, MTYPE_I2, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_AND_I4:
    result = Exp_Fetch_and_And(op0, op1, MTYPE_I4, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_AND_I8:
    result = Exp_Fetch_and_And(op0, op1, MTYPE_I8, ops, label, loop_ops);
    break;

  case INTRN_FETCH_AND_NAND_I1:
    result = Exp_Fetch_and_Nand(op0, op1, MTYPE_I1, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_NAND_I2:
    result = Exp_Fetch_and_Nand(op0, op1, MTYPE_I2, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_NAND_I4:
    result = Exp_Fetch_and_Nand(op0, op1, MTYPE_I4, ops, label, loop_ops);
    break;
  case INTRN_FETCH_AND_NAND_I8:
    result = Exp_Fetch_and_Nand(op0, op1, MTYPE_I8, ops, label, loop_ops);
    break;

  case INTRN_ADD_AND_FETCH_I1:
    result = Exp_Add_and_Fetch(op0, op1, MTYPE_I1, ops);
    break;
  case INTRN_ADD_AND_FETCH_I2:
    result = Exp_Add_and_Fetch(op0, op1, MTYPE_I2, ops);
    break;
  case INTRN_ADD_AND_FETCH_I4:
    result = Exp_Add_and_Fetch(op0, op1, MTYPE_I4, ops);
    break;
  case INTRN_ADD_AND_FETCH_I8:
    result = Exp_Add_and_Fetch(op0, op1, MTYPE_I8, ops);
    break;

  case INTRN_SUB_AND_FETCH_I1: 
    neg_tn = Build_TN_Like(op1);
    Build_OP(TOP_neg8, neg_tn, op1, ops);
    result = Exp_Add_and_Fetch(op0, neg_tn, MTYPE_I1, ops);
    break;
  case INTRN_SUB_AND_FETCH_I2: 
    neg_tn = Build_TN_Like(op1);
    Build_OP(TOP_neg16, neg_tn, op1, ops);
    result = Exp_Add_and_Fetch(op0, neg_tn, MTYPE_I2, ops);
    break;
  case INTRN_SUB_AND_FETCH_I4: 
    neg_tn = Build_TN_Like(op1);
    Build_OP(TOP_neg32, neg_tn, op1, ops);
    result = Exp_Add_and_Fetch(op0, neg_tn, MTYPE_I4, ops);
    break;
  case INTRN_SUB_AND_FETCH_I8:
    neg_tn = Build_TN_Like(op1);
    Build_OP(TOP_neg64, neg_tn, op1, ops);
    result = Exp_Add_and_Fetch(op0, neg_tn, MTYPE_I8, ops);
    break;

  case INTRN_OR_AND_FETCH_I1:
    result = Exp_Or_and_Fetch(op0, op1, MTYPE_I1, ops, label, loop_ops);
    break;
  case INTRN_OR_AND_FETCH_I2:
    result = Exp_Or_and_Fetch(op0, op1, MTYPE_I2, ops, label, loop_ops);
    break;
  case INTRN_OR_AND_FETCH_I4:
    result = Exp_Or_and_Fetch(op0, op1, MTYPE_I4, ops, label, loop_ops);
    break;
  case INTRN_OR_AND_FETCH_I8:
    result = Exp_Or_and_Fetch(op0, op1, MTYPE_I8, ops, label, loop_ops);
    break;

  case INTRN_XOR_AND_FETCH_I1:
    result = Exp_Xor_and_Fetch(op0, op1, MTYPE_I1, ops, label, loop_ops);
    break;
  case INTRN_XOR_AND_FETCH_I2:
    result = Exp_Xor_and_Fetch(op0, op1, MTYPE_I2, ops, label, loop_ops);
    break;
  case INTRN_XOR_AND_FETCH_I4:
    result = Exp_Xor_and_Fetch(op0, op1, MTYPE_I4, ops, label, loop_ops);
    break;
  case INTRN_XOR_AND_FETCH_I8:
    result = Exp_Xor_and_Fetch(op0, op1, MTYPE_I8, ops, label, loop_ops);
    break;

  case INTRN_AND_AND_FETCH_I1:
    result = Exp_And_and_Fetch(op0, op1, MTYPE_I1, ops, label, loop_ops);
    break;
  case INTRN_AND_AND_FETCH_I2:
    result = Exp_And_and_Fetch(op0, op1, MTYPE_I2, ops, label, loop_ops);
    break;
  case INTRN_AND_AND_FETCH_I4:
    result = Exp_And_and_Fetch(op0, op1, MTYPE_I4, ops, label, loop_ops);
    break;
  case INTRN_AND_AND_FETCH_I8:
    result = Exp_And_and_Fetch(op0, op1, MTYPE_I8, ops, label, loop_ops);
    break;

  case INTRN_NAND_AND_FETCH_I1:
    result = Exp_Nand_and_Fetch(op0, op1, MTYPE_I1, ops, label, loop_ops);
    break;
  case INTRN_NAND_AND_FETCH_I2:
    result = Exp_Nand_and_Fetch(op0, op1, MTYPE_I2, ops, label, loop_ops);
    break;
  case INTRN_NAND_AND_FETCH_I4:
    result = Exp_Nand_and_Fetch(op0, op1, MTYPE_I4, ops, label, loop_ops);
    break;
  case INTRN_NAND_AND_FETCH_I8:
    result = Exp_Nand_and_Fetch(op0, op1, MTYPE_I8, ops, label, loop_ops);
    break;

  case INTRN_LOCK_RELEASE_I1:
    result = Exp_Lock_Release(op0, MTYPE_I1, ops);
    break;
  case INTRN_LOCK_RELEASE_I2:
    result = Exp_Lock_Release(op0, MTYPE_I2, ops);
    break;
  case INTRN_LOCK_RELEASE_I4:
    result = Exp_Lock_Release(op0, MTYPE_I4, ops);
    break;
  case INTRN_LOCK_RELEASE_I8:
    result = Exp_Lock_Release(op0, MTYPE_I8, ops);
    break;

  case INTRN_LOCK_TEST_AND_SET_I1:
    result = Exp_Test_and_Set(op0, op1, MTYPE_I1, ops);
    break;
  case INTRN_LOCK_TEST_AND_SET_I2:
    result = Exp_Test_and_Set(op0, op1, MTYPE_I2, ops);
    break;
  case INTRN_LOCK_TEST_AND_SET_I4:
    result = Exp_Test_and_Set(op0, op1, MTYPE_I4, ops);
    break;
  case INTRN_LOCK_TEST_AND_SET_I8:
    result = Exp_Test_and_Set(op0, op1, MTYPE_I8, ops);
    break;

  case INTRN_COMPARE_AND_SWAP_I1:
    result = Exp_Compare_and_Swap(op0, op1, op2, MTYPE_I1, ops);
    break;
  case INTRN_COMPARE_AND_SWAP_I2:
    result = Exp_Compare_and_Swap(op0, op1, op2, MTYPE_I2, ops);
    break;
  case INTRN_COMPARE_AND_SWAP_I4:
    result = Exp_Compare_and_Swap(op0, op1, op2, MTYPE_I4, ops);
    break;
  case INTRN_COMPARE_AND_SWAP_I8:
    result = Exp_Compare_and_Swap(op0, op1, op2, MTYPE_I8, ops);
    break;

  case INTRN_BOOL_COMPARE_AND_SWAP_I1:
    result = Exp_Bool_Compare_and_Swap(op0, op1, op2, MTYPE_I1, ops);
    break;
  case INTRN_BOOL_COMPARE_AND_SWAP_I2:
    result = Exp_Bool_Compare_and_Swap(op0, op1, op2, MTYPE_I2, ops);
    break;
  case INTRN_BOOL_COMPARE_AND_SWAP_I4:
    result = Exp_Bool_Compare_and_Swap(op0, op1, op2, MTYPE_I4, ops);
    break;
  case INTRN_BOOL_COMPARE_AND_SWAP_I8:
    result = Exp_Bool_Compare_and_Swap(op0, op1, op2, MTYPE_I8, ops);
    break;
  case INTRN_APPLY_ARGS:
    result = Exp_Builtin_Apply_Args(ops);
    break;
  case INTRN_APPLY:
    result = Exp_Builtin_Apply(op0, op1, op2, ops);
    break;
  case INTRN_RETURN:
    result = Exp_Builtin_Return(op0, ops);
    break;

  default:  
    FmtAssert(FALSE, ("Exp_Intrinsic_Call: unimplemented"));
  }
  return result;
}


/* __builtin_apply_args should be insert into the beginning of the function, 
 * not where it defined. */
void
Setup_Builtin_Apply_Args(OPS *ops) {
    FmtAssert(PU_has_builtin_apply_args,
            ("Exp_Apply_Args: __builtin_apply_args is not available in current PU"));

    TYPE_ID type = Is_Target_32bit() ? MTYPE_U4 : MTYPE_U8;
    INT size = Is_Target_32bit() ? 4 : 8;

    // Store register parameters into the new structure
    INT ofst = size;
    if (Is_Target_32bit()) { // 32 bits
        REGISTER int_regs[] = {RAX, RDX, RCX};
        for (int i = 0; i < sizeof(int_regs) / sizeof(REGISTER); i++) {
            TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, int_regs[i], size);
            Exp_Store(type, tn, tmp_apply_arg, ofst, ops, 0);
            ofst += size;
        }
        if (Is_Target_SSE()) {
            REGISTER sse_regs[] = {XMM0, XMM1, XMM2};
            for (int i = 0; i < sizeof(sse_regs) / sizeof(REGISTER); i++) {
                TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_float, sse_regs[i] - Float_Preg_Min_Offset + 1, 16);
                Build_OP(TOP_stups, tn, FP_TN, Gen_Symbol_TN(tmp_apply_arg, ofst, 0), ops);
                ofst += 16;
            }
        }
        if (Is_Target_MMX()) {
            REGISTER mmx_regs[] = {MM0, MM1, MM2};
            for (int i = 0; i < sizeof(mmx_regs) / sizeof(REGISTER); i++) {
                TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_mmx, mmx_regs[i] - MMX_Preg_Min_Offset + 1, 8);
                Build_OP(TOP_store64_fm, tn, FP_TN, Gen_Symbol_TN(tmp_apply_arg, ofst, 0), ops);
                ofst += 8;
            }
        }
    } else { // 64 bits
        REGISTER int_regs[] = {RAX, RDX, RCX, RSI, RDI, R8, R9};
        for (int i = 0; i < sizeof(int_regs) / sizeof(REGISTER); i++) {
            TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, int_regs[i], size);
            Exp_Store(type, tn, tmp_apply_arg, ofst, ops, 0);
            ofst += size;
        }
        REGISTER sse_regs[] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7 };
        for (int i = 0; i < sizeof(sse_regs) / sizeof(REGISTER); i++) {
            TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_float, sse_regs[i] - Float_Preg_Min_Offset + 1, 16);
            Build_OP(TOP_stdqu, tn, FP_TN, Gen_Symbol_TN(tmp_apply_arg, ofst, 0), ops);
            ofst += 16;
        }
    }

    // Store function parameters into the new structure
    TN* function_args = Gen_Register_TN(ISA_REGISTER_CLASS_integer, size);
    Exp_Lda(type, function_args, FP_Sym, Is_Target_32bit() ? 8 : 16, OPR_STID, ops);
    Exp_Store(type, function_args, tmp_apply_arg, 0, ops, 0);
}

TN*
Exp_Builtin_Apply_Args(OPS *ops)
{
    // User may call __builtin_apply_args several times in one function
    if (!PU_has_builtin_apply_args) {
        // Generate_Entry will expand __builtin_apply_args later if this flag is set
        PU_has_builtin_apply_args = TRUE;

        Generate_Temp_Apply_Arg();
    }

    // Return the pointer to the new structure
    TN *return_tn = Build_TN_Of_Mtype(Is_Target_32bit() ? MTYPE_U4 : MTYPE_U8);
    Exp_OP2(Is_Target_32bit() ? OPC_I4ADD : OPC_I8ADD,
            return_tn, FP_TN, Gen_Symbol_TN(tmp_apply_arg, 0, 0), ops);
    return return_tn;
}

TN*
Exp_Builtin_Apply(TN *addr, TN *args, TN *argsize, OPS *ops)
{
    PU_has_builtin_apply = TRUE;

    INT size = Is_Target_32bit() ? 4 : 8;

    // All OPs generated here must set volatile flag, otherwise 
    // EBO_Remove_Unused_Ops may remove them later

    // Restore register parameters
    TN *apply_arg = Gen_Register_TN(ISA_REGISTER_CLASS_integer, size);
    Build_OP(Is_Target_32bit() ? TOP_ld32 : TOP_ld64, apply_arg, args, Gen_Literal_TN(0, size), ops);
    INT ofst = size;
    if (Is_Target_32bit()) { // 32 bits
        REGISTER int_regs[] = {RAX, RDX, RCX};
        for (int i = 0; i < sizeof(int_regs) / sizeof(REGISTER); i++) {
            TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, int_regs[i], size);
            OP *op = Mk_OP(TOP_ld32, tn, args, Gen_Literal_TN(ofst, 4));
            Set_OP_volatile(op);
            OPS_Append_Op(ops, op);
            ofst += size;
        }
        if (Is_Target_SSE()) {
            REGISTER sse_regs[] = {XMM0, XMM1, XMM2};
            for (int i = 0; i < sizeof(sse_regs) / sizeof(REGISTER); i++) {
                TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_float, sse_regs[i] - Float_Preg_Min_Offset + 1, 16);
                OP *op = Mk_OP(TOP_ldups, tn, args, Gen_Literal_TN(ofst, 4));
                Set_OP_volatile(op);
                OPS_Append_Op(ops, op);
                ofst += 16;
            }
        }
        if (Is_Target_MMX()) {
            REGISTER mmx_regs[] = {MM0, MM1, MM2};
            for (int i = 0; i < sizeof(mmx_regs) / sizeof(REGISTER); i++) {
                TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_mmx, mmx_regs[i] - MMX_Preg_Min_Offset + 1, 8);
                OP *op = Mk_OP(TOP_ld64_2m, tn, args, Gen_Literal_TN(ofst, 4));
                Set_OP_volatile(op);
                OPS_Append_Op(ops, op);
                ofst += 8;
            }
            // Need to emit emms when mixing MMX and x87 FPU instructions
            Build_OP(TOP_emms, ops);
        }
    } else { // 64 bits
        REGISTER int_regs[] = {RAX, RDX, RCX, RSI, RDI, R8, R9};
        for (int i = 0; i < sizeof(int_regs) / sizeof(REGISTER); i++) {
            TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, int_regs[i], size);
            OP *op = Mk_OP(TOP_ld64, tn, args, Gen_Literal_TN(ofst, 4));
            Set_OP_volatile(op);
            OPS_Append_Op(ops, op);
            ofst += size;
        }
        REGISTER sse_regs[] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7 };
        for (int i = 0; i < sizeof(sse_regs) / sizeof(REGISTER); i++) {
            TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_float, sse_regs[i] - Float_Preg_Min_Offset + 1, 16);
            OP *op = Mk_OP(TOP_lddqu, tn, args, Gen_Literal_TN(ofst, 4));
            Set_OP_volatile(op);
            OPS_Append_Op(ops, op);
            ofst += 16;
        }
        // with variable arguments, %rax contains the number of vector registers used
        OP *op = Mk_OP(TOP_ldc64, Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, RAX, size),
                Gen_Literal_TN(sizeof(sse_regs) / sizeof(REGISTER), 8));
        Set_OP_volatile(op);
        OPS_Append_Op(ops, op);
    }
    return NULL;
}

TN*
Exp_Builtin_Return(TN *result, OPS *ops)
{
    TN *ded_tn;
    INT ofst = 0;
    TOP integer_top, float_top;
    INT size;
    if (Is_Target_32bit()) {
        integer_top = TOP_ld32;
        float_top = TOP_ldups;
        size = 4;
    } else {
        integer_top = TOP_ld64;
        float_top = TOP_lddqu;
        size = 8;
    }
    ded_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, RAX, size);
    Build_OP(integer_top, ded_tn, result, Gen_Literal_TN(ofst, 4), ops);
    ofst += size;
    ded_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, RDX, size);
    Build_OP(integer_top, ded_tn, result, Gen_Literal_TN(ofst, 4), ops);
    ofst += size;
    ded_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_x87, ST0 - X87_Preg_Min_Offset + 1, 8);
    Build_OP(TOP_fldt, ded_tn, result, Gen_Literal_TN(ofst, 4), ops);
    ofst += 16;
    ded_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_float, XMM0 - Float_Preg_Min_Offset + 1, 16);
    Build_OP(float_top, ded_tn, result, Gen_Literal_TN(ofst, 4), ops);
    ofst += 16;
    return NULL;
}

/* Expansion of INTRN_SAVEXMMS into TOP_savexmms pseudo instruction */
void
Exp_Savexmms_Intrinsic(WN *intrncall, TN *rax_tn, LABEL_IDX *label, OPS *ops) 
{
  TN *ofst_tn;
  ST *base_sym = NULL;
  INT64 base_ofst = 0;
  INT num_xmms = WN_const_val(WN_kid0(WN_kid1(intrncall)));
  TN *tn1 = Gen_Literal_TN(num_xmms, 4);
  WN *lda = WN_kid0(WN_kid2(intrncall));
  FmtAssert(WN_operator(lda) == OPR_LDA, 
      ("Exp_savexmms_Intrinsic: unexpected operand for SAVE_XMMS intrinsic"));
  Base_Symbol_And_Offset_For_Addressing(WN_st(lda), WN_lda_offset(lda),
	  				&base_sym, &base_ofst);
  FmtAssert(base_sym == SP_Sym || base_sym == FP_Sym,
      ("Exp_savexmms_Intrinsic: unexpected addresses in SAVE_XMMS intrinsic"));
  TN *base_tn = (base_sym == SP_Sym) ? SP_TN : FP_TN;
  if (base_sym == WN_st(lda))
    ofst_tn = Gen_Literal_TN(base_ofst-1, 4);
  else ofst_tn = Gen_Symbol_TN(WN_st(lda), WN_lda_offset(lda)-1, 0);
  *label = Gen_Temp_Label();
  TN *r11_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, R11, 8);
  Build_OP(TOP_savexmms, rax_tn, tn1, base_tn, ofst_tn, Gen_Label_TN(*label, 0),
	   r11_tn, ops);
}


/* ======================================================================
 * Exp_Simulated_Op
 *
 * Given a simulated <op>, expand it into the sequence of instructions
 * supported by the target.
 * ======================================================================*/
void Exp_Simulated_Op(const OP *op, OPS *ops, INT pc_value)
{
  TOP top = OP_code(op);
  BB *bb = OP_bb(op);

  switch (top)
  {
  case TOP_savexmms: {
      FmtAssert(BB_last_op(OP_bb(op)) == op, 
	    ("Exp_Simulated_Op: savexmms must be last instruction in the BB"));
      TN *rax_tn = OP_opnd(op, 0);
      Build_OP(TOP_andi64, rax_tn, rax_tn, Gen_Literal_TN(0xff,4), ops);
      INT64 num_xmms = TN_value(OP_opnd(op, 1));
      TN *r11_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, R11, 8);
      if ( Is_Target_Orochi() && Is_Target_AVX() ) {
        // guard the addition of this insn by flag
        if (CG_NoClear_Avx_Simd == false)
          Build_OP(TOP_vzeroupper, ops );

        // The insn size for vstaps is 5 bytes, note that
        // leaq 0(%rax,%rax,4) is (5 * %rax)
        Build_OP(TOP_leax64, r11_tn, rax_tn, rax_tn, Gen_Literal_TN(4, 4), 
	         Gen_Literal_TN(5*(num_xmms-8), 4), ops);
      } else {
        Build_OP(TOP_leaxx64, r11_tn, rax_tn, Gen_Literal_TN(4, 4), 
	         Gen_Literal_TN(4*(num_xmms-8), 4), ops);
      }
      Build_OP(TOP_neg64, r11_tn, r11_tn, ops);
      if ( Gen_PIC_Shared ) {
	Build_OP ( TOP_lea64, rax_tn, Rip_TN(), OP_opnd( op, 4 ), ops );
	ops->last->bb = bb; // to pass Verify_Operand
	Build_OP ( TOP_leax64, r11_tn, r11_tn, rax_tn, 
		   Gen_Literal_TN( 1, 4 ), 
		   Gen_Literal_TN( 0, 4 ), ops );
      } else {
	Build_OP(TOP_addi64, r11_tn, r11_tn, OP_opnd(op, 4), ops);
	ops->last->bb = bb; // to pass Verify_Operand
      }
      Build_OP(TOP_lea64, rax_tn, OP_opnd(op, 2), OP_opnd(op, 3), ops);
      Build_OP(TOP_ijmp, r11_tn, ops);
      for (INT i = 1; i <= num_xmms; i++) {
        Build_OP(TOP_staps, PREG_To_TN(Int_Preg, XMM0+(8-i)), 
	         rax_tn, Gen_Literal_TN(16 * (num_xmms-i) + 1, 4), ops);
      }
      break;
    }
  default:
    FmtAssert(FALSE, ("simulated OP %s not handled", TOP_Name(top)));
  }
}


/* ======================================================================
 * Simulated_Op_Real_Ops
 *
 * Return the number of instructions that will be generated by Exp_Simulated_Op
 * ======================================================================*/
INT
Simulated_Op_Real_Ops(const OP *op)
{
  switch (OP_code(op)) {
  case TOP_intrncall:
    return Expand_TOP_intrncall (op, NULL, TRUE, 0);
  case TOP_spadjust:
    return 1;
  default:

    /* Anything other than the above is presumed to be removed by
     * emit time, therefore we just say the expansion generates 0 ops.
     * (we used to assert, but that isn't a good solution -- see pv 652898).
     */
    return 0;
  }
}


/* ======================================================================
 * Simulated_Op_Real_Inst_Words
 *
 * Return the number of instruction words that will ultimately be emitted
 * for the expansion generated by Exp_Simulated_Op
 * ======================================================================*/
INT
Simulated_Op_Real_Inst_Words(const OP *op)
{
  INT num_bytes = 0;
  TOP top = OP_code(op);

  switch (top)
  {
  case TOP_spadjust:
    return 1;
  case TOP_asm:
    /* We don't know how many instructions are "within" the asm, so we
       just assume 3 bytes. */
    num_bytes = 3;
    break;
  case TOP_savexmms:
    num_bytes = 5 + (INT32)TN_value(OP_opnd(op, 1));
    break;

  default:
    FmtAssert(FALSE, ("simulated OP %s not handled", TOP_Name(OP_code(op))));
  }

  return num_bytes;
}


/* ======================================================================
 * Exp_Is_Large_Stack_Sym
 *
 * determine if a given symbol is a stack relative reference that will
 * require multiple instructions to load or store.
 * ======================================================================*/
BOOL
Exp_Is_Large_Stack_Sym(ST* sym,  INT64 ofst)
{
  ST *base_sym;
  INT64 base_ofst;
  
  if (sym == NULL)
    return FALSE;

  Allocate_Object(sym);
  Base_Symbol_And_Offset_For_Addressing (sym, ofst, &base_sym, &base_ofst);

  /* We can assume that 'sym' is a spill location for an integer
     register, so we can check for l32i/s32i range. */
  
  if ((base_sym == SP_Sym || base_sym == FP_Sym) &&
      !ISA_LC_Value_In_Class(base_ofst, LC_simm32)) {
    return TRUE;
  }

  return FALSE;
}

void Exp_Noop (OPS *ops)
{
  Build_OP (CGTARG_Noop_Top(), ops);
}

void Expand_Const (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{
  FmtAssert( TN_is_symbol(src), ("Expand_Const: src not a symbol TN") );
  if (CG_use_xortozero && 
      ST_class(TN_var(src)) == CLASS_CONST) {

    TCON tcon = STC_val(TN_var(src));

    // Must use Targ_Is_Zero to check for positive zero.  Bug 9734.
    if (TCON_ty(tcon) == MTYPE_F4 && Targ_Is_Zero(tcon)) {
      FmtAssert(TCON_ty(tcon) == mtype || mtype == MTYPE_V16F4, 
		("Expand_Const: inconsistent mtypes"));
      if ( Is_Target_SSE2() )
	Build_OP( mtype == MTYPE_V16F4?TOP_xzero128v32:TOP_xzero32, dest, ops);
      else
	Build_OP( TOP_fldz, dest, ops);
      return;
    }
    if (TCON_ty(tcon) == MTYPE_F8 && Targ_Is_Zero(tcon)) {
      FmtAssert(TCON_ty(tcon) == mtype || mtype == MTYPE_V16F8, 
		("Expand_Const: inconsistent mtypes"));
      if ( Is_Target_SSE2() )
	Build_OP( mtype == MTYPE_V16F8?TOP_xzero128v64:TOP_xzero64, dest, ops);
      else
	Build_OP( TOP_fldz, dest, ops);
      return;
    }
    if (TCON_ty(tcon) == MTYPE_V16F4 && Targ_Is_Zero(tcon)) {
      FmtAssert(TCON_ty(tcon) == mtype && Is_Target_SSE2(),
		("Expand_Const: inconsistent mtypes"));
      Build_OP( TOP_xzero128v32, dest, ops);
      return;
    }
    if (TCON_ty(tcon) == MTYPE_V16F8 && Targ_Is_Zero(tcon)) {
      FmtAssert(TCON_ty(tcon) == mtype && Is_Target_SSE2(),
		("Expand_Const: inconsistent mtypes"));
      Build_OP( TOP_xzero128v64, dest, ops);
      return;
    }
  }
  Exp_Load( mtype, mtype, dest, TN_var(src), TN_offset(src), ops, 0 );
}

static BB* last_bb = NULL;
static TN *last_true_tn = NULL, *last_false_tn = NULL;
void
HB_Reinit_Pred ()
{
  last_true_tn = NULL;
  last_false_tn = NULL;
  last_bb = NULL;
}

void
Exp_True_False_Preds_For_Block(BB *bb, TN* &true_tn, TN * &false_tn)
{ 
  if (last_bb != bb)
    last_bb = bb;
  else {
    true_tn = last_true_tn;
    false_tn = last_false_tn;
    return;
  }
  OP* br_op = BB_branch_op(bb);
  if (!br_op)
    return;

  FmtAssert( FALSE, ("UNIMPLEMENTED") );
}

BOOL
Target_Has_Immediate_Operand (WN *parent, WN *expr)
{
  OPERATOR opr = WN_operator(parent);
  //add for OPR_INTRINSIC_OP INTRN_VEC_EXT_V2SI, the second oprand is const 0 or 1
  INTRINSIC id = (INTRINSIC) WN_intrinsic (parent);
  if ( opr == OPR_INTRINSIC_OP && id == INTRN_VEC_EXT_V2SI && WN_kid0(WN_kid1(parent))== expr )
    return true;
  return opr == OPR_ADD || opr == OPR_SUB || opr == OPR_EQ ||
         opr == OPR_BAND || opr == OPR_BIOR || opr == OPR_BXOR ||
         opr == OPR_LT || opr == OPR_LE || opr == OPR_GT || opr == OPR_GE ||
         opr == OPR_LSHR || opr == OPR_ASHR || opr == OPR_SHL;
}

void 
Exp_Spadjust (TN *dest, TN *size, VARIANT variant, OPS *ops)
{
  // A temp register is needed if the adjustment size can't fit in the
  // immediate field of an add instruction.  spadjust is converted into add
  // after register allocation, at which time there are no more registers.  So
  // create a temp TN now.  Bug 9051.
  if (TN_is_constant(size) &&
      TN_has_value(size) &&
      !CGTARG_Can_Fit_Immediate_In_Add_Instruction(TN_value(size))) {
    TN *temp = Build_TN_Of_Mtype(MTYPE_I8);
    Exp_Immediate(temp, size, TRUE, ops);
    size = temp;
  }

  Build_OP (TOP_spadjust, dest, SP_TN, size, ops);
  OP_variant(OPS_last(ops)) = variant;
}

/* Return a unique name for a symbol representing a literal. */
char *
Exp_Unique_Literal_Name (void)
{
  static int unique;
  static char name[32];

  sprintf(name, ".LC%d", unique);
  unique++;
  return name;
}

void
Exp_Generic_Pred_Calc(TN* result1, TN *result2, COMPARE_TYPE ctype,
                      TN *qual_pred, OPS* ops)
{ FmtAssert(FALSE,("Unimplemented")); }
  
  
void
Exp_Pred_Calc(TN* result, OP* cmp_op, COMPARE_TYPE ctype, BOOL false_result,
	      OPS* ops)
{ FmtAssert(FALSE,("Unimplemented")); }


void
Exp_Landingpadentry_Intrinsic (ST *result1, ST *result2, OPS *ops)
{
  const int size = Is_Target_64bit() ? 8 : 4;
  const TYPE_ID mtype = Is_Target_64bit() ? 8 : 4;
  TN *tn1 = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, RAX, size);
  Exp_Store (mtype, tn1, result1, 0, ops, 0);

  TN *tn2 = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, RDX, size);
  Exp_Store (mtype, tn2, result2, 0, ops, 0);
}

TN* Gen_Second_Immd_Op( TN *src2, OPS* ops )
{
  TN* src2_h = TN_has_value(src2) ? NULL : Get_TN_Pair(src2);

  if( TN_has_value(src2) ){
    const INT64 val = TN_value(src2);
    src2_h = Gen_Literal_TN( val >> 32, 4 );
  }

  if( src2_h == NULL ){
    DevWarn( "The higher 32-bit of TN%d is treated as 0\n",
	     TN_number(src2) );
    src2_h  = Build_TN_Like( src2 );
    Build_OP( TOP_ldc32, src2_h, Gen_Literal_TN(0,4), ops );    
  }
  return src2_h;
}

/* Expand FETCH_AND_ADD intrinsic into the following format
 * result = xadd opnd1, *addr;
 */
static
TN *Exp_Fetch_and_Add (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops )
{
  TOP top = TOP_UNDEFINED;
  TN *result_tn = Build_TN_Like(opnd1);

  switch (mtype) {
    case MTYPE_I1:
    case MTYPE_U1:
      top = TOP_lock_xadd8;
      break;
    case MTYPE_I2:
    case MTYPE_U2:
      top = TOP_lock_xadd16;
      break;
    case MTYPE_I4:
    case MTYPE_U4:
      top = TOP_lock_xadd32;
      break;
    case MTYPE_I8:
    case MTYPE_U8:
      top = TOP_lock_xadd64;
      break;
    default:
      FmtAssert(FALSE, ("Exp_Fetch_and_Add: unsupported mtype"));
  }

  if (Is_Target_32bit() && top == TOP_lock_xadd64)
    FmtAssert(FALSE, ("Exp_Fetch_and_Add: 64-bit fetch-and-add NYI under m32"));
  else
    Build_OP(top, result_tn, opnd1, addr, Gen_Literal_TN(0,4), ops );

  return result_tn;
}

/* Expand FETCH_AND_OR intrinsic into the following format
 * orig_val = *addr
 * do {
 *   result = orig_val;
 *   new_val = result | opnd1;
 *   old_val = cmpxchg orig_val, new_val, *addr;
 *   orig_val = old_val;
 * } while ( old_val != orig_val )
 */
TN *Exp_Fetch_and_Or   (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops)
{
  BOOL is_64bit = mtype == MTYPE_I8 || mtype == MTYPE_U8;
  TN* orig_val = Build_TN_Of_Mtype(mtype);
  TN* new_val  = Build_TN_Of_Mtype(mtype);
  TN* result   = Build_TN_Of_Mtype(mtype);
  Expand_Load ( OPCODE_make_op(OPR_LDID, is_64bit? MTYPE_I8 : MTYPE_I4, mtype),
                orig_val, addr, Gen_Literal_TN(0, 4), ops );
  *label = Gen_Temp_Label();
  Exp_COPY(result, orig_val, loop_ops);
  Expand_Binary_Or ( new_val, orig_val, opnd1, mtype, loop_ops );
  TN* old_val = Exp_Compare_and_Swap ( addr, orig_val, new_val, mtype, loop_ops );
  Exp_COPY(orig_val, old_val, loop_ops);
  Build_OP(TOP_jne, Rflags_TN(), Gen_Label_TN(*label, 0), loop_ops);
  return result;
}

/* Expand FETCH_AND_XOR intrinsic into the following format
 * orig_val = *addr
 * do {
 *   result = orig_val;
 *   new_val = result ^ opnd1;
 *   old_val = cmpxchg orig_val, new_val, *addr;
 *   orig_val = old_val;
 * } while ( old_val != orig_val )
 */
TN *Exp_Fetch_and_Xor  (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops)
{
  BOOL is_64bit = mtype == MTYPE_I8 || mtype == MTYPE_U8;
  TN* orig_val = Build_TN_Of_Mtype(mtype);
  TN* new_val  = Build_TN_Of_Mtype(mtype);
  TN* result   = Build_TN_Of_Mtype(mtype);
  Expand_Load ( OPCODE_make_op(OPR_LDID, is_64bit? MTYPE_I8 : MTYPE_I4, mtype),
                orig_val, addr, Gen_Literal_TN(0, 4), ops );
  *label = Gen_Temp_Label();
  Exp_COPY(result, orig_val, loop_ops);
  Expand_Binary_Xor ( new_val, orig_val, opnd1, mtype, loop_ops );
  TN* old_val = Exp_Compare_and_Swap ( addr, orig_val, new_val, mtype, loop_ops );
  Exp_COPY(orig_val, old_val, loop_ops);
  Build_OP(TOP_jne, Rflags_TN(), Gen_Label_TN(*label, 0), loop_ops);
  return result;
}

/* Expand FETCH_AND_AND intrinsic into the following format
 * orig_val = *addr
 * do {
 *   result = orig_val;
 *   new_val = result & opnd1;
 *   old_val = cmpxchg orig_val, new_val, *addr;
 *   orig_val = old_val;
 * } while ( old_val != orig_val )
 */
TN *Exp_Fetch_and_And  (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops)
{
  BOOL is_64bit = mtype == MTYPE_I8 || mtype == MTYPE_U8;
  TN* orig_val = Build_TN_Of_Mtype(mtype);
  TN* new_val  = Build_TN_Of_Mtype(mtype);
  TN* result   = Build_TN_Of_Mtype(mtype);
  Expand_Load ( OPCODE_make_op(OPR_LDID, is_64bit? MTYPE_I8 : MTYPE_I4, mtype),
                orig_val, addr, Gen_Literal_TN(0, 4), ops );
  *label = Gen_Temp_Label();
  Exp_COPY(result, orig_val, loop_ops);
  Expand_Binary_And ( new_val, orig_val, opnd1, mtype, loop_ops );
  TN* old_val = Exp_Compare_and_Swap ( addr, orig_val, new_val, mtype, loop_ops );
  Exp_COPY(orig_val, old_val, loop_ops);
  Build_OP(TOP_jne, Rflags_TN(), Gen_Label_TN(*label, 0), loop_ops);
  return result;
}

/* Expand FETCH_AND_NAND intrinsic into the following format
 * orig_val = *addr
 * do {
 *   result = orig_val;
 *   not_val = ~orig_val;
 *   new_val = not_val & opnd1;
 *   old_val = cmpxchg orig_val, new_val, *addr;
 *   orig_val = old_val;
 * } while ( old_val != orig_val )
 */
TN *Exp_Fetch_and_Nand (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops)
{
  BOOL is_64bit = mtype == MTYPE_I8 || mtype == MTYPE_U8;
  TN* orig_val = Build_TN_Of_Mtype(mtype);
  TN* not_val  = Build_TN_Of_Mtype(mtype);
  TN* new_val  = Build_TN_Of_Mtype(mtype);
  TN* result   = Build_TN_Of_Mtype(mtype);
  Expand_Load ( OPCODE_make_op(OPR_LDID, is_64bit? MTYPE_I8 : MTYPE_I4, mtype),
                orig_val, addr, Gen_Literal_TN(0, 4), ops );
  *label = Gen_Temp_Label();
  Exp_COPY(result, orig_val, loop_ops);
  Expand_Binary_Not ( not_val, orig_val, mtype, loop_ops );
  Expand_Binary_And ( new_val, not_val, opnd1, mtype, loop_ops );
  TN* old_val = Exp_Compare_and_Swap ( addr, orig_val, new_val, mtype, loop_ops );
  Exp_COPY(orig_val, old_val, loop_ops);
  Build_OP(TOP_jne, Rflags_TN(), Gen_Label_TN(*label, 0), loop_ops);
  return result;
}

/* Expand ADD_AND_FETCH intrinsic into the following format
 * xadd_result = xadd *addr, opnd1
 * result = xadd_result + opnd1
 */
TN *Exp_Add_and_Fetch  (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops)
{
  TN* xadd_result = Exp_Fetch_and_Add(addr, opnd1, mtype, ops);
  TN* result = Build_TN_Like(opnd1);

  Build_OP( (mtype == MTYPE_I8 || mtype == MTYPE_U8)? TOP_add64 : TOP_add32,
            result, xadd_result, opnd1, ops);

  return result;
}

/* Expand OR_AND_FETCH intrinsic into the following format
 * orig_val = *addr
 * do {
 *   result = orig_val | opnd1;
 *   old_val = cmpxchg orig_val, result, *addr;
 *   orig_val = old_val;
 * } while ( old_val != orig_val )
 */
TN *Exp_Or_and_Fetch   (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops)
{
  BOOL is_64bit = mtype == MTYPE_I8 || mtype == MTYPE_U8;
  TN* orig_val = Build_TN_Of_Mtype(mtype);
  TN* result   = Build_TN_Of_Mtype(mtype);
  Expand_Load ( OPCODE_make_op(OPR_LDID, is_64bit? MTYPE_I8 : MTYPE_I4, mtype),
                orig_val, addr, Gen_Literal_TN(0, 4), ops );
  *label = Gen_Temp_Label();
  Expand_Binary_Or ( result, orig_val, opnd1, mtype, loop_ops );
  TN* old_val = Exp_Compare_and_Swap ( addr, orig_val, result, mtype, loop_ops );
  Exp_COPY(orig_val, old_val, loop_ops);
  Build_OP(TOP_jne, Rflags_TN(), Gen_Label_TN(*label, 0), loop_ops);
  return result;
}

/* Expand XOR_AND_FETCH intrinsic into the following format
 * orig_val = *addr
 * do {
 *   result = orig_val ^ opnd1;
 *   old_val = cmpxchg orig_val, result, *addr;
 *   orig_val = old_val;
 * } while ( old_val != orig_val )
 */
TN *Exp_Xor_and_Fetch  (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops)
{
  BOOL is_64bit = mtype == MTYPE_I8 || mtype == MTYPE_U8;
  TN* orig_val = Build_TN_Of_Mtype(mtype);
  TN* result   = Build_TN_Of_Mtype(mtype);
  Expand_Load ( OPCODE_make_op(OPR_LDID, is_64bit? MTYPE_I8 : MTYPE_I4, mtype),
                orig_val, addr, Gen_Literal_TN(0, 4), ops );
  *label = Gen_Temp_Label();
  Expand_Binary_Xor ( result, orig_val, opnd1, mtype, loop_ops );
  TN* old_val = Exp_Compare_and_Swap ( addr, orig_val, result, mtype, loop_ops );
  Exp_COPY(orig_val, old_val, loop_ops);
  Build_OP(TOP_jne, Rflags_TN(), Gen_Label_TN(*label, 0), loop_ops);
  return result;
}

/* Expand AND_AND_FETCH intrinsic into the following format
 * orig_val = *addr
 * do {
 *   result = orig_val | opnd1;
 *   old_val = cmpxchg orig_val, result, *addr;
 *   orig_val = old_val;
 * } while ( old_val != orig_val )
 */
TN *Exp_And_and_Fetch  (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops)
{
  BOOL is_64bit = mtype == MTYPE_I8 || mtype == MTYPE_U8;
  TN* orig_val = Build_TN_Of_Mtype(mtype);
  TN* result   = Build_TN_Of_Mtype(mtype);
  Expand_Load ( OPCODE_make_op(OPR_LDID, is_64bit? MTYPE_I8 : MTYPE_I4, mtype),
                orig_val, addr, Gen_Literal_TN(0, 4), ops );
  *label = Gen_Temp_Label();
  Expand_Binary_And ( result, orig_val, opnd1, mtype, loop_ops );
  TN* old_val = Exp_Compare_and_Swap ( addr, orig_val, result, mtype, loop_ops );
  Exp_COPY(orig_val, old_val, loop_ops);
  Build_OP(TOP_jne, Rflags_TN(), Gen_Label_TN(*label, 0), loop_ops);
  return result;
}

/* Expand NAND_AND_FETCH intrinsic into the following format
 * orig_val = *addr
 * do {
 *   not_val = ~orig_val;
 *   result = not_val & opnd1;
 *   old_val = cmpxchg orig_val, result, *addr;
 *   orig_val = old_val;
 * } while ( old_val != orig_val )
 */
TN *Exp_Nand_and_Fetch (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops, LABEL_IDX *label, OPS *loop_ops)
{
  BOOL is_64bit = mtype == MTYPE_I8 || mtype == MTYPE_U8;
  TN* orig_val = Build_TN_Of_Mtype(mtype);
  TN* not_val = Build_TN_Of_Mtype(mtype);
  TN* result   = Build_TN_Of_Mtype(mtype);
  Expand_Load ( OPCODE_make_op(OPR_LDID, is_64bit? MTYPE_I8 : MTYPE_I4, mtype),
                orig_val, addr, Gen_Literal_TN(0, 4), ops );
  *label = Gen_Temp_Label();
  Expand_Binary_Not( not_val, orig_val, mtype, loop_ops );
  Expand_Binary_And ( result, not_val, opnd1, mtype, loop_ops );
  TN* old_val = Exp_Compare_and_Swap ( addr, orig_val, result, mtype, loop_ops );
  Exp_COPY(orig_val, old_val, loop_ops);
  Build_OP(TOP_jne, Rflags_TN(), Gen_Label_TN(*label, 0), loop_ops);
  return result;
}

/* Expand TEST_AND_SET intrinsic into the following format
 * result = lock xchg opnd1, *addr
 */
TN *Exp_Test_and_Set (TN *addr, TN *opnd1, TYPE_ID mtype, OPS *ops)
{
  TOP top = TOP_UNDEFINED;
  BOOL is_64bit = FALSE;

  switch( mtype ){
  case MTYPE_I1:
  case MTYPE_U1:
    top = TOP_lock_xchg8;
    break;

  case MTYPE_I2:
  case MTYPE_U2:
    top = TOP_lock_xchg16;
    break;

  case MTYPE_I4:
  case MTYPE_U4:
    top = TOP_lock_xchg32;
    break;

  case MTYPE_I8:
  case MTYPE_U8:
    top = TOP_lock_xchg64;
    is_64bit = TRUE;
    break;

  default:
    FmtAssert( FALSE,
	       ("Exp_Test_and_Set: support me now") );
  }

  TN* result = Build_TN_Like(opnd1);
  Build_OP( top, result, opnd1, addr, Gen_Literal_TN(0,4), ops );
  return result;
}

/* Expand LOCK_RELEASE intrinsic into the following format
 * *addr = 0
 */
TN *Exp_Lock_Release (TN *addr, TYPE_ID mtype, OPS *ops)
{
  TOP top = TOP_UNDEFINED;
  TN* val = NULL;
  BOOL is_64bit = FALSE;
  switch( mtype ){
  case MTYPE_I1:
  case MTYPE_U1:
    top = TOP_store8;
    break;

  case MTYPE_I2:
  case MTYPE_U2:
    top = TOP_store16;
    break;

  case MTYPE_I4:
  case MTYPE_U4:
    top = TOP_store32;
    break;

  case MTYPE_I8:
  case MTYPE_U8:
    is_64bit = TRUE;
    top = TOP_store64;
    break;
  
  default:
    FmtAssert( FALSE,
	       ("Exp_Lock_Release: support me now") );
  }

  val = Build_TN_Of_Mtype( is_64bit ? MTYPE_I8 : MTYPE_I4);
  Build_OP( is_64bit ? TOP_ldc64 : TOP_ldc32, val, Gen_Literal_TN(0, 4), ops);
  Build_OP( top, val, addr, Gen_Literal_TN(0, 4), ops);
  return NULL;
}

/* Expand COMPARE_AND_SWAP intrinsic into the following format
 * result = cmpxchg opnd1, opnd2, *addr
 */
TN* Exp_Compare_and_Swap( TN* addr, TN* opnd1, TN* opnd2, TYPE_ID mtype, OPS* ops )
{
  TOP top = TOP_UNDEFINED;

  switch( mtype ){
  case MTYPE_I1:
  case MTYPE_U1:
    top = TOP_lock_cmpxchg8;
    break;

  case MTYPE_I2:
  case MTYPE_U2:
    top = TOP_lock_cmpxchg16;
    break;

  case MTYPE_I4:
  case MTYPE_U4:
    top = TOP_lock_cmpxchg32;
    break;

  case MTYPE_I8:
  case MTYPE_U8:
    top = TOP_lock_cmpxchg64;
    break;

  default:
    FmtAssert( FALSE,
	       ("Exp_Compare_and_Swap: support me now") );
  }

  TN* result = Build_TN_Like(opnd1);
  Build_OP( top, Rflags_TN(), result, opnd1, opnd2, addr, Gen_Literal_TN(0,4), ops );
  return result;
}

/* Expand BOOL_COMPARE_AND_SWAP intrinsic into the following format
 * cmpxchg opnd1, opnd2, *addr
 * result = 0
 * if ZF == 1
 *   result = 1
 */
TN* Exp_Bool_Compare_and_Swap( TN* addr, TN* opnd1, TN* opnd2, TYPE_ID mtype, OPS* ops )
{
  Exp_Compare_and_Swap(addr, opnd1, opnd2, mtype, ops);
  TN* result = Build_TN_Of_Mtype(mtype);
  if ( Is_Target_64bit() ) 
    Build_OP(TOP_ldc64, result, Gen_Literal_TN(0, 8), ops);
  else
    Build_OP(TOP_ldc32, result, Gen_Literal_TN(0, 4), ops);
  Build_OP(TOP_sete, result, Rflags_TN(), ops);
  Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
  return result;
}

// Allocate a temp stack location for SRC and store SRC into it.  Return the
// base TN and the offset TN of the memory location.
static void
Store_To_Temp_Stack(TYPE_ID desc, TN *src, const char *sym_name, TN **mem_base_tn,
		    TN **mem_ofst_tn, OPS *ops)
{
  TY_IDX mem_ty = MTYPE_To_TY(desc);
  ST* mem_loc = Gen_Temp_Symbol(mem_ty, sym_name);
  Allocate_Temp_To_Memory(mem_loc);
  ST* mem_base_sym = NULL;
  INT64 mem_base_ofst = 0;

  Base_Symbol_And_Offset_For_Addressing(mem_loc, 0, &mem_base_sym,
					&mem_base_ofst);
  *mem_base_tn = mem_base_sym == SP_Sym ? SP_TN : FP_TN;
  *mem_ofst_tn = Gen_Literal_TN(mem_base_ofst, 4);
  Expand_Store(desc, src, *mem_base_tn, *mem_ofst_tn, 0, ops);
}

// This function attempts to handle a range of to/from vector conversions,
// exposed by bug 3082 and friends.
void Expand_Conv_To_Vector (TN * dest, TN * src, TYPE_ID desc, TYPE_ID rtype,
                           OPS *ops)
{
  Is_True (MTYPE_is_vector (rtype), ("Expand_Conv_To_Vector, NYI"));

  if (!MTYPE_is_vector (desc))
  {
    if (MTYPE_is_mmx_vector (rtype))
    {
      if (desc == MTYPE_I8 || desc == MTYPE_U8) {
        // mov int64 to mmx
	if (Is_Target_64bit()) {
	  Build_OP (TOP_movi64_2m, dest, src, ops);
	} else {
	  // Move the 64-bit value via memory because there is no 64-bit int
	  // register.
	  TN *base_tn, *ofst_tn;
	  Store_To_Temp_Stack(desc, src, "int64_2_mmx", &base_tn, &ofst_tn,
			      ops);
	  Build_OP(TOP_ld64_2m, dest, base_tn, ofst_tn, ops);
	}
      }
      else if (desc == MTYPE_I4 || desc == MTYPE_U4) {
        Build_OP (TOP_movi32_2m, dest, src, ops);
      } 
      else
        Fail_FmtAssertion ("Expand_Conv_To_Vector: NYI");
    }
    else {
      if (Is_Target_64bit())
	Build_OP(MTYPE_byte_size(desc) == 32 ? TOP_movg2x : TOP_movg2x64, 
		 dest, src, ops);
      else Expand_Int_To_Vect_Tas(dest, src, rtype, ops);
    }
  }
  else if (MTYPE_is_mmx_vector(desc) && ! MTYPE_is_mmx_vector(rtype))
    Build_OP (TOP_movq2dq, dest, src, ops);
  else if (! MTYPE_is_mmx_vector(desc) && MTYPE_is_mmx_vector(rtype))
    Build_OP (TOP_movdq2q, dest, src, ops);
  else // desc, rtype: vector
    Build_OP (TOP_movdq, dest, src, ops);
}


// This function handles from vector to non-vector types
void Expand_Conv_From_Vector(TN * dest, TN * src, TYPE_ID desc, TYPE_ID rtype,
                             OPS *ops)
{
  Is_True(MTYPE_is_vector(desc), ("Expand_Conv_From_Vector, NYI"));
  Is_True(MTYPE_byte_size(desc) == MTYPE_byte_size(rtype), 
  	  ("Expand_Conv_From_Vector, can only handle conversion among types with same size"));
  if (MTYPE_is_mmx_vector(desc)) {
    if (MTYPE_byte_size(rtype) == 8)
      Build_OP (TOP_movm_2i64, dest, src, ops);
    else Build_OP (TOP_movm_2i32, dest, src, ops);
  }
  else {
    if (MTYPE_byte_size(rtype) == 8)
      Build_OP (TOP_movx2g64, dest, src, ops);
    else Build_OP (TOP_movx2g, dest, src, ops);
  }
}
