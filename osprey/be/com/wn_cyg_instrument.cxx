/*
 * Copyright 2007 PathScale, LLC.  All Rights Reserved.
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


// This file contains code to support the -finstrument-functions flag.
// CYG_Instrument traverses the given WHIRL code and insert a call to
// __cyg_profile_func_enter just after the PREAMBLE_END pragma, and a
// call to __profile_func_exit just before each RETURN and RETURN_VAL
// statement.  Also, CYG_Instrument looks for inlined procedures
// and inserts a call to __cyg_profile_func_enter just after the
// INLINE_BODY_START pragma, and a call to __profile_func_exit just
// before each INLINE_BODY_END pragma (and a LABEL to identify the
// call site).
//
// CYG_Instrument_Driver should be invoked after IPA and INLINE have
// finished inlining procedures (because instrumented functions can
// no longer be inlined).  Also, if -finstrument-functions is on, then
// IPA and INLINE must not delete the original, non-instrumented
// functions, because their addresses are needed as arguments to
// __cyg_profile_func_enter/exit.  See bug 750 for more details.


#include "defs.h"
#include "wn.h"
#include "wn_util.h"
#include "data_layout.h"  // Find_Special_Return_Address_Symbol
#include "config_opt.h"   // OPT_Cyg_Instrument


// The -OPT:cyg_instr flag indicates how aggresively we will instrument
// functions:
//   0 = Don't instrument any functions [default].
//   1 = Don't instrument functions the gnu front-end selects for inlining.
//   2 = Don't instrument functions marked "inline" by user.
//   3 = Don't instrument functions marked "extern inline" or always_inline.
//   4 = Instrument all functions; disable deletion of "extern inline"
//       functions.
// CYG_Instrument_Driver should never be called if OPT_CYG_Instrument < 1.

// TODO: Move this code to front-end by setting PU_no_instrument flag.
static BOOL
Do_cyg_instrument_p( ST *st_func )
{
  // Can assume this: if ( OPT_Cyg_Instrument <= 0 ) return FALSE;
  PU &pu = Pu_Table[ST_pu(st_func)];
  if ( PU_no_instrument( pu ) ) return FALSE;
  switch ( OPT_Cyg_Instrument ) {
  case 1:  return ! PU_is_inline_function( pu );
  case 2:  return ! PU_is_marked_inline( pu );
  case 3:  return ! PU_must_inline( pu );
  default: break;
  }
  return TRUE;  // OPT_Cyg_Instrument >= 4
}


BOOL VHO_Cyg_Instrument_PU;
static ST *VHO_return_address_st;
static ST *VHO_Cyg_enter_st;
static ST *VHO_Cyg_exit_st;


// Invoke Initialize_cyg_profile_for_PU at start of VHO lower for each PU
// to initialize the values immediately above.
static void
CYG_Initialize_for_PU()
{
  // Look for __return_address in local symbol table
  VHO_return_address_st = Find_Special_Return_Address_Symbol();

  // If not found, add __return_address to local symbol table
  if ( VHO_return_address_st == NULL ) {
    VHO_return_address_st = New_ST( CURRENT_SYMTAB );
    ST_Init( VHO_return_address_st, Save_Str( "__return_address" ), CLASS_VAR,
	     ( PUSH_RETURN_ADDRESS_ON_STACK ? SCLASS_FORMAL : SCLASS_AUTO ),
	     EXPORT_LOCAL, MTYPE_To_TY(Pointer_Mtype) );
    // Set_ST_is_return_var( VHO_return_address_st );
    Set_PU_has_return_address( Get_Current_PU() );
  }

  // Add __cyg_profile_func_enter/exit to symbol table
  TY_IDX ty = Make_Function_Type( MTYPE_To_TY(MTYPE_V) );
  VHO_Cyg_enter_st = Gen_Intrinsic_Function( ty, "__cyg_profile_func_enter" );
  VHO_Cyg_exit_st = Gen_Intrinsic_Function( ty, "__cyg_profile_func_exit" );

  PU &pu_enter = Pu_Table[ST_pu( VHO_Cyg_enter_st )];
  Clear_PU_no_side_effects( pu_enter );
  Clear_PU_is_pure( pu_enter );
  Set_PU_no_delete( pu_enter );

  PU &pu_exit = Pu_Table[ST_pu( VHO_Cyg_exit_st )];
  Clear_PU_no_side_effects( pu_exit );
  Clear_PU_is_pure( pu_exit );
  Set_PU_no_delete( pu_exit );

  // Update current PU
  PU &pu_cur = Get_Current_PU();
    Set_PU_no_inline( pu_cur );
  if ( PU_must_inline( pu_cur ) ) {
    DevWarn( "Disabling must_inline for PU %s",
	     ST_name( Get_Current_PU_ST() ) );
    if ( OPT_Cyg_Instrument < 4 ) Set_PU_no_instrument( pu_cur );
    Clear_PU_must_inline( pu_cur );
  }
}


static void
Generate_cyg_profile_func( BOOL is_exit, ST *st_func,
			   LABEL_IDX exit_label, WN* wn, WN *block )
{
  // Bug 750: Generate calls for -finstrument-functions
  // void __cyg_profile_func_enter/exit (void *this_fn, void *call_site);

  TY_IDX ty_idx = Make_Pointer_Type( MTYPE_To_TY(MTYPE_V) );  // void *

  // arg0: void *this_fn
  WN *parm0 = WN_Lda( Pointer_Mtype, (WN_OFFSET) 0, st_func, (UINT) 0 );
  WN *arg0 = WN_CreateParm( Pointer_Mtype, parm0, ty_idx, WN_PARM_BY_VALUE );
  Set_ST_addr_passed( st_func );

  // arg1: void *call_site
  WN *parm1;
  if ( exit_label == LABEL_IDX_ZERO ) {
    parm1 = WN_Ldid( Pointer_Mtype, 0, VHO_return_address_st,
		     ST_type( VHO_return_address_st ) );
  } else {
    parm1 = WN_LdaLabel( Pointer_Mtype, exit_label );
    Set_LABEL_addr_saved( exit_label );
  }
  WN *arg1 = WN_CreateParm( Pointer_Mtype, parm1, ty_idx, WN_PARM_BY_VALUE );

  // Generate call to profiler
  WN *call_wn = WN_Call( MTYPE_V, MTYPE_V, 2,
			 is_exit ? VHO_Cyg_exit_st : VHO_Cyg_enter_st );
  WN_kid0( call_wn ) = arg0;
  WN_kid1( call_wn ) = arg1;
  WN_Set_Linenum( call_wn, WN_Get_Linenum(wn) );
  WN_Set_Call_Default_Flags( call_wn );  // 14023
  if ( is_exit ) {
    WN_INSERT_BlockBefore( block, wn, call_wn );
  } else {
    WN_INSERT_BlockAfter( block, wn, call_wn );
  }
}


// CYG_Instrument_Block traverse the statements in the given BLOCK, and
// insert calls to __cyg_profile_func_enter/exit before RETURN and
// RETURN_VAL, after PREAMBLE_END pragma, after INLINE_BODY_START pragma,
// and before INLINE_BODY_END pragma.


static void CYG_Traverse( WN *wn );

static WN *
CYG_Instrument_Block( WN *block, WN *wn_first )
{
  WN *wn, *wn_next, *wn_label;
  LABEL_IDX exit_label;
  BOOL do_inline_inst;

  Is_True( WN_operator(block) == OPR_BLOCK,
	   ( "CYG_Instrument_Block expected BLOCK block" ) );

  for ( wn = wn_first; wn; wn = wn_next ) {
    wn_next = WN_next(wn);

    OPERATOR opr = WN_operator(wn);
    if (! OPERATOR_is_leaf(opr)) CYG_Traverse( wn );

    if ( opr == OPR_RETURN || opr == OPR_RETURN_VAL 
#ifdef KEY
  	 || opr ==  OPR_GOTO_OUTER_BLOCK
#endif
       ) {
      if ( Do_cyg_instrument_p( Get_Current_PU_ST() ) ) {
	Generate_cyg_profile_func( TRUE, Get_Current_PU_ST(),
				   LABEL_IDX_ZERO, wn, block );
      }
    } else if ( opr == OPR_PRAGMA ) {

      switch ( WN_pragma(wn) ) {

      case WN_PRAGMA_PREAMBLE_END:
	if ( Do_cyg_instrument_p( Get_Current_PU_ST() ) ) {
	  Generate_cyg_profile_func( FALSE, Get_Current_PU_ST(),
				     LABEL_IDX_ZERO, wn, block );
	}
	break;

      case WN_PRAGMA_INLINE_BODY_START:

	do_inline_inst = Do_cyg_instrument_p( WN_st(wn) );
	if ( do_inline_inst ) {
	  LABEL_Init( New_LABEL( CURRENT_SYMTAB, exit_label ), 0, LKIND_TAG );
	  Generate_cyg_profile_func( FALSE, WN_st(wn), exit_label, wn, block );
	}

	// Bug 13811: Since we may encounter nested inlined procedures
	// that will overwrite exit_label, invoke CYG_Instrument_Block
	// recursively.
	wn = CYG_Instrument_Block( block, wn_next );
	wn_next = WN_next( wn );

	// Return from recursive call at INLINE_BODY_END of inlined PU.
	Is_True( wn && WN_operator(wn) == OPR_PRAGMA &&
		 WN_pragma(wn) == WN_PRAGMA_INLINE_BODY_END,
		 ( "CYG_Instrument_Block missed INLINE_BODY_END Pragma" ) );
	if ( do_inline_inst ) {
	  Generate_cyg_profile_func( TRUE, WN_st(wn), exit_label, wn, block );
	  wn_label = WN_CreateLabel( (ST_IDX) 0, exit_label, 0, NULL );
	  WN_Set_Linenum( wn_label, WN_Get_Linenum(wn) );
	  WN_INSERT_BlockAfter( block, wn, wn_label );
	}
	break;

      case WN_PRAGMA_INLINE_BODY_END:
	return wn;

      default:
	break;
      }
    }
  }

  return NULL;
}

  
// CYG_Traverse traverses kids of the given wn looking for BLOCKs.
static void CYG_Traverse( WN *wn )
{
  Is_True( WN_operator(wn) != OPR_BLOCK,
	   ( "CYG_Traverse expected non-BLOCK wn" ) );
  for ( INT k = 0; k < WN_kid_count(wn); ++k ) {
    WN *kid = WN_kid( wn, k );
    if (OPERATOR_is_leaf(WN_operator(kid))) /* do nothing */ ;
    else if ( WN_operator(kid) == OPR_BLOCK ) {
      CYG_Instrument_Block( kid, WN_first(kid) );
    } else {
      CYG_Traverse(kid);
    }
  }
}


void
CYG_Instrument_Driver( WN *wn_pu )
{
  Is_True( OPT_Cyg_Instrument >= 1,
	   ( "CYG_Instrument_Driver should not have been invoked." ) );
  CYG_Initialize_for_PU();

  // Fortran PUs sometimes don't end with a RETURN, which is
  // needed to properly insert __cyg_instrument_exit.
  WN *wn_body, *wn_last, *wn_return;
  if ( WN_operator(wn_pu) == OPR_FUNC_ENTRY &&
       ( wn_body = WN_func_body(wn_pu) ) != NULL &&
       WN_operator(wn_body) == OPR_BLOCK &&
       ( ( wn_last = WN_last(wn_body) ) == NULL ||
	 ( WN_operator(wn_last) != OPR_RETURN &&
	   WN_operator(wn_last) != OPR_RETURN_VAL  
#ifdef KEY
  	   && WN_operator(wn_last) != OPR_GOTO_OUTER_BLOCK
#endif
	 ) ) ) {
    wn_return = WN_CreateReturn();
    WN_INSERT_BlockLast( wn_body, wn_return );
    if ( wn_last ) {
      WN_Set_Linenum( wn_return, WN_Get_Linenum(wn_last) );
    }
  }

  CYG_Traverse( wn_pu );
} /* cyg_instrument_driver */
