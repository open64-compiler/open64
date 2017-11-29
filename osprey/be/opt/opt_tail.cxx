//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_tail.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_tail.cxx,v $
//
// Revision history:
//  14-MAR-96 - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// Search the CFG for tail-recursion opportunities and mutate the CFG
// appropriately.
//
// A tail-recursion opportunity is an exit block where the last 
// instructions of the block are a CALL to the current PU followed by
// a RETURN of the return value of the call.  There may be a series of
// LDID/STID pairs between the CALL and the RETURN which store the
// CALL's return value into a temporary and then reload it as the
// PU's return value.
//
// When we convert the call to a jump, we first compute all of the 
// arguments into temporaries, and then copy the temporaries into
// the formals.  This is so if there is a data dependence between the
// formals and the actuals we won't compute the wrong value.
//
// Because we do tail recursion after the GOTO optimization, any loops
// produced by tail recursion will not be converted to DO/WHILE loops,
// and thus loop-specific optimizations will not be performed on them.
// Hopefully this won't be a serious problem.
//
// We do not handle functions with structure arguments because of the
// difficulty in copying a structure to a temporary holding area
// while going through the argument list.  Likewise, we do not currently
// handle functions with structure returns because we don't do sufficient
// analysis of the instructions between a call and a return to tell that
// it is an MSTORE of the structure return value.
//
// TODO: I don't think we check for struct return yet (Fred).
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_tail_CXX	"opt_tail.cxx"
static char *rcs_id = 	opt_tail_CXX"$Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include "opt_tail.h"

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "mempool.h"
#include "tracing.h"
#include "cxx_memory.h"

#include "opt_sys.h"
#include "opt_cfg.h"
#include "opt_defs.h"
#include "bb_node_set.h"
#include "opt_sym.h"
#include "ttype.h"
#include "wn.h"
#include "config.h"
#include "stab.h"


// ====================================================================
// OPT_TAIL constructor
// ====================================================================

OPT_TAIL::OPT_TAIL(CFG *cfg, OPT_STAB *opt_stab)
{
  _cfg = cfg;
  _opt_stab = opt_stab;
  _entry_bb = _label_bb = NULL;
  _entry_wn = _call_wn = _ret_ldid_wn = _ret_ldid_wn1 = 
    _ret_stid_wn = _ret_stid_wn1 = _top_label = NULL;

  OPT_POOL_Push(_cfg->Loc_pool(), TAIL_TRACE_FLAG );

  _do_trace = Get_Trace(TP_GLOBOPT, TAIL_TRACE_FLAG);

  if (_do_trace) {
    fprintf(TFile, "%sCFG on entry to tail recursion\n%s", DBar, DBar);
    _cfg->Print(TFile);
  }
}


// ====================================================================
// OPT_TAIL destructor
// ====================================================================

OPT_TAIL::~OPT_TAIL()
{
  OPT_POOL_Pop(_cfg->Loc_pool(), TAIL_TRACE_FLAG);
}


// ====================================================================
// See if the current function is eligible for tail-recursion
//
// It must have a single entry point, not take any structure arguments,
// and not have varargs.
// ====================================================================

BOOL OPT_TAIL::Entry_is_well_behaved()
{
  // Make sure there is only one real entry
  if (_cfg->Fake_entry_bb() != NULL) {
    INT entry_count = 0;
    BB_LIST_ITER entry_iter(_cfg->Fake_entry_bb()->Succ());
    FOR_ALL_ITEM(entry_iter, Init()) {
      BB_NODE *bb = entry_iter.Cur_bb();
      if (bb->Kind() == BB_ENTRY) {
	_entry_bb = bb;
	entry_count++;
      }
    }
    if (entry_count != 1) {
      _entry_bb = NULL;
    }
  } else {
    _entry_bb = _cfg->Func_entry_bb();
  }

  if (_entry_bb == NULL)
    return FALSE;

  _entry_wn = NULL;
  if (_entry_bb->Kind() == BB_ENTRY)
    _entry_wn = _entry_bb->Entrywn();
  if (_entry_wn == NULL)
    return FALSE;

  // Don't handle varargs functions
  if (TY_is_varargs(ST_type(WN_st(_entry_wn))))
      return FALSE;

  // Don't handle structure arguments
  INT formal_num;
  for (formal_num = 0; formal_num < WN_num_formals(_entry_wn);
       formal_num++) {
      WN     *formal = WN_formal(_entry_wn, formal_num);
      TY_IDX  formal_type = ST_type(WN_st(formal));
      if (MTYPE_is_m(TY_mtype(formal_type)))
	  return FALSE;
  }

  // Entry BB must be empty
  BB_NODE *first_bb = _entry_bb;
  if (first_bb->Firststmt() != NULL)
      return FALSE;

  // Entry BB must have only one successor
  if (first_bb->Succ() == NULL ||
      first_bb->Succ()->Next() != NULL)
      return FALSE;
  
  first_bb = first_bb->Succ()->Node();

  // Likewise, the first BB after the entry had better not have more
  // than one pred - or we're going to be messed up when we try to 
  // find the copies of the function arguments.
  if (first_bb->Pred() == NULL ||
      first_bb->Pred()->Next() != NULL)
      return FALSE;

  return TRUE;
}


// ====================================================================
// See if the current exit is eligible for tail-recursion
//
// It must have a return statement, a call, and possibly some intervening
// loads and stores to/from the return register.
// ====================================================================

BOOL OPT_TAIL::Exit_is_well_behaved(BB_NODE *bb)
{
  STMT_CONTAINER bb_stmts(bb->Firststmt(), bb->Laststmt());
    
  // Last statement must be a RETURN
  WN *stmt_ptr = bb_stmts.Tail();
  if (stmt_ptr == NULL || 
      (WN_operator(stmt_ptr) != OPR_RETURN && 
       WN_operator(stmt_ptr) != OPR_RETURN_VAL)) {
    return FALSE;
  }
  
  _ret_ldid_wn = _ret_ldid_wn1 = NULL;
  _ret_stid_wn = _ret_stid_wn1 = NULL;
  
  while ((stmt_ptr = WN_prev(stmt_ptr)) != NULL) {
    if (WN_operator(stmt_ptr) == OPR_CALL) {
      // See if it's recursive
      _call_wn = stmt_ptr;
      if (WN_st(_call_wn) != WN_st(_entry_wn)) {
	if (_do_trace)
	    fprintf(TFile, "-- Doesn't call this function\n");
	return FALSE;
      }

      // Make sure the number of actuals matches the number of formals
      if ( WN_num_actuals(_call_wn) != WN_num_formals(_entry_wn) ) {
	if (_do_trace)
	  fprintf(TFile, 
	    "-- Number of actuals (%d) != number of formals (%d)\n",
	    WN_num_actuals(_call_wn), WN_num_formals(_entry_wn) );
	return FALSE;
      }

      break;
    }
    
    // Check for a load from the return value or a store to the return value
    if (WN_operator(stmt_ptr) != OPR_STID ||
	WN_operator(WN_kid0(stmt_ptr)) != OPR_LDID) {
      if (_do_trace)
	  fprintf(TFile, "-- Instruction not stid or ldid\n");
      return FALSE;
    }
    ST *stid_st = _opt_stab->St((IDTYPE)WN_st_idx(stmt_ptr));
    if (ST_class(stid_st) == CLASS_PREG &&
	Preg_Is_Dedicated(WN_store_offset(stmt_ptr))) {
      if (_ret_stid_wn == NULL) 
	_ret_stid_wn = stmt_ptr;
      else if (_ret_stid_wn1 == NULL)
	_ret_stid_wn1 = stmt_ptr;
      else {
	if (_do_trace)
	  fprintf(TFile, "-- Found more than two stid into dedpreg\n");
	return FALSE;
      }
      continue;
    }
    ST *ldid_st = _opt_stab->St((IDTYPE)WN_st_idx(WN_kid0(stmt_ptr)));
    if (ST_class(ldid_st) == CLASS_PREG &&
	Preg_Is_Dedicated(WN_load_offset(WN_kid0(stmt_ptr)))) {
      if (_ret_ldid_wn == NULL)
	_ret_ldid_wn = stmt_ptr;
      else if (_ret_ldid_wn1 == NULL)
	_ret_ldid_wn1 = stmt_ptr;
      else {
	if (_do_trace)
	  fprintf(TFile, "-- Found more than two ldid from dedpreg\n");
	return FALSE;
      }
      continue;
    }
    if (_do_trace)
	fprintf(TFile, "-- Instruction not stid into dedpreg or stid of ldid from dedpreg\n");
    return FALSE;
  }

  if (stmt_ptr == NULL)
    return FALSE;

  // Make sure the call's arguments agree with the formals.
  INT formal_num;
  for (formal_num = 0; formal_num < WN_num_actuals(_call_wn);
       formal_num++)
  {
    WN *formal = WN_formal(_entry_wn, formal_num);
    Is_True(WN_operator(formal) == OPR_IDNAME,
	    ("formal is not IDNAME"));
    ST *formal_st = WN_st(formal);
    TYPE_ID formal_type_id = TY_mtype(ST_type(formal_st));
    
    WN *arg = WN_actual(_call_wn, formal_num);
    const OPCODE   arg_opc = WN_opcode(arg);
    const OPERATOR arg_opr = OPCODE_operator(arg_opc);
    Is_True( arg_opr == OPR_PARM,
	    ("Call parameter not OPR_PARM"));
    TYPE_ID arg_type_id = OPCODE_rtype(arg_opc);

    if (formal_type_id != arg_type_id) {
      if (_do_trace)
	fprintf(TFile, "-- Argument %d not proper type\n", formal_num);
      return FALSE;
    }

    // check for passing the address of a locally allocated variable
    // see pv #405171
    WN *arg_kid = WN_kid0(arg);
    const OPERATOR arg_kid_opr = WN_operator(arg_kid);
    if ( arg_kid_opr == OPR_LDA ) {
      ST *lda_st = _opt_stab->St((IDTYPE)WN_st_idx(arg_kid));
      if ( ST_class(lda_st) == CLASS_VAR ) {
	if (ST_sclass(lda_st) == SCLASS_AUTO ||
	    ST_sclass(lda_st) == SCLASS_FORMAL)
	{
	  if (_do_trace)
	    fprintf( TFile, "-- Argument %d is lda of local\n", 
		     formal_num );
	  return FALSE;
	}
      }
    }

  }  

  return TRUE;
}


// ====================================================================
// Find the end of the argument-copying prolog and break the BB at that
// point.  Create a new label, if necessary, and insert it after the
// prolog.  This will be the point where tail recursion will jump back
// to.
// ====================================================================

void OPT_TAIL::Create_top_label()
{
  BB_NODE *first_bb = _entry_bb->Succ()->Node();
  WN *last_copy = NULL;

  for ( WN *tmp_wn = first_bb->Firststmt();
	tmp_wn != NULL;
	tmp_wn = WN_next(tmp_wn) )
  {
    if ( WN_operator(tmp_wn) != OPR_STID )
      break;

    // are we storing a dedicated register?
    WN *load_wn = WN_kid0(tmp_wn);
    if (WN_operator(load_wn) == OPR_CVTL ||
	WN_operator(load_wn) == OPR_CVT)
      load_wn = WN_kid0(load_wn);

    if (WN_operator(load_wn) != OPR_LDID)
      break;

    if (ST_class(_opt_stab->St(WN_aux(load_wn))) != CLASS_PREG ||
	!Preg_Is_Dedicated(WN_load_offset(load_wn)))
      break;

    // at this point, we know we have a store of one of the dedicated
    // registers to its home location, so save a pointer to the last
    // one seen
    last_copy = tmp_wn;
  }

  if (_do_trace) {
    fprintf(TFile, "Last arg copy is:\n");
    fdump_tree_no_st(TFile, last_copy);
  }
	
  // split the block after the last argument-copy, if any
  _label_bb = NULL;
  if ( last_copy == NULL ) {
    _label_bb = first_bb;
  } else {
    BOOL has_call = first_bb->Hascall();
    _label_bb = _cfg->Split_bb_with_wns(first_bb, last_copy);
    first_bb->Reset_hascall();
    _label_bb->Reset_hascall();
    if (has_call)
      first_bb->Set_hascall();
  }

  // add a label if necessary to the block after the last arg-copy
  _top_label = _label_bb->Label_wn();
  if (_top_label == NULL) {
    _top_label = WN_CreateLabel(0, _cfg->Alloc_label(), 0, NULL);
    _cfg->Prepend_wn_in(_label_bb, _top_label);
    _cfg->Append_label_map(WN_label_number(_top_label), _label_bb);
  }
}


// ====================================================================
// Fixup the exit by deleting the old call, return, and copy instructions.
// Add copies from the actuals to the formals and a goto to the entry
// label.
// ====================================================================

void OPT_TAIL::Fixup_exit(BB_NODE *bb)
{
  STMT_CONTAINER bb_stmts(bb->Firststmt(), bb->Laststmt());
    
  // Remove the CALL
  bb_stmts.Remove(_call_wn);
  
  // Remove the store/load to return register, if any
  if (_ret_stid_wn)
      bb_stmts.Remove(_ret_stid_wn);
  if (_ret_stid_wn1)
      bb_stmts.Remove(_ret_stid_wn1);
  if (_ret_ldid_wn)
      bb_stmts.Remove(_ret_ldid_wn);
  if (_ret_ldid_wn1)
      bb_stmts.Remove(_ret_ldid_wn1);
  
  // Remove the RETURN or RETURN_VAL
  bb_stmts.Remove(bb_stmts.Tail());
  
  // Add copies from the actuals to the formals.  We have to first
  // copy the actuals into temporary locations in case there is
  // a use of one of the formals in the actual expressions.
  //
  // The type of the PREG is the type of the PARM expression.
  STACK<AUX_ID> preg_stack(_cfg->Loc_pool());
  
  INT formal_num;
  for (formal_num = 0; formal_num < WN_num_actuals(_call_wn);
       formal_num++) 
  {
    WN *formal = WN_formal(_entry_wn, formal_num);
    Is_True(WN_operator(formal) == OPR_IDNAME,
	    ("formal is not IDNAME"));
    AUX_ID formal_id = _opt_stab->
	Find_sym_with_st_and_ofst(WN_st(formal),
				  WN_idname_offset(formal));

    // No ST found means the variable is never used in the function - don't
    // need to copy the argument.
    if (formal_id == 0) {
      continue;
    }
    WN *arg = WN_actual(_call_wn, formal_num);
    Is_True(WN_operator(arg) == OPR_PARM,
	    ("Call parameter not OPR_PARM"));
    TYPE_ID arg_type_id = WN_rtype(arg);

    arg = WN_kid0(arg);
#ifdef KEY
// Bug 1640
    static INT Temp_Index = 0;
    UINT len = strlen("_temp_") + 17;
#ifdef __MINGW32__
    char *new_str = (char *) __builtin_alloca (len);
#else
    char *new_str = (char *) alloca (len);
#endif /* __MINGW32__ */
    sprintf(new_str, "%s%d", "_temp_", Temp_Index++);
    AUX_ID tmp_preg = _opt_stab->Create_preg(arg_type_id, new_str);
#else
    AUX_ID tmp_preg = _opt_stab->Create_preg(arg_type_id);
#endif
    WN *stid = WN_CreateStid(OPR_STID, MTYPE_V, arg_type_id, 
			     _opt_stab->St_ofst(tmp_preg), 
			     ST_st_idx(_opt_stab->St(tmp_preg)),
			     Be_Type_Tbl(arg_type_id), arg);
    // Set the AUX flag properly
    WN_set_aux(stid, (AUX_ID)tmp_preg);
    bb_stmts.Append(stid);
    // Remember the preg so we can copy to the formal later
    preg_stack.Push(tmp_preg);
  }
  
  // Copy from the temporary pregs into the real formals
  for (formal_num = WN_num_formals(_entry_wn)-1; formal_num >= 0;
       formal_num--) {
    WN *formal = WN_formal(_entry_wn, formal_num);
    AUX_ID formal_id = _opt_stab->
	Find_sym_with_st_and_ofst(WN_st(formal),
				  WN_idname_offset(formal));

    // No ST found means the variable is never used in the function - don't
    // need to copy the argument.
    if (formal_id == 0) {
      continue;
    }
    ST     *formal_st = WN_st(formal);
    TY_IDX  formal_type = ST_type(formal_st);
    
    AUX_ID tmp_preg = preg_stack.Pop();
    TY_IDX arg_type = ST_type(_opt_stab->St(tmp_preg));
    WN *ldid = WN_CreateLdid(OPR_LDID, TY_mtype(arg_type), TY_mtype(arg_type),
			     _opt_stab->St_ofst(tmp_preg), 
			     ST_st_idx(_opt_stab->St(tmp_preg)), arg_type);
    // Set the AUX flag properly
    WN_set_aux(ldid, (AUX_ID)tmp_preg);
    /* CVTL-RELATED start (correctness) */
    if (TY_mtype(formal_type) != TY_mtype(arg_type)) {
      ldid = WN_CreateExp1(OPCODE_make_op(OPR_CVT, TY_mtype(formal_type),
					  TY_mtype(arg_type)), ldid);
    }
    /* CVTL-RELATED finish */
    WN *stid = WN_CreateStid(OPR_STID, MTYPE_V, TY_mtype(formal_type),
			     0, formal_st, formal_type, ldid);
    // Set the AUX flag properly
    WN_set_aux(stid, formal_id);
    bb_stmts.Append(stid);
  }
  
  // Create the new GOTO
  WN *goto_wn = WN_CreateGoto((ST *) NULL, WN_label_number(_top_label));
  bb_stmts.Append(goto_wn);
  
  // Fix up the BB
  bb->Set_kind(BB_GOTO);
  bb->Set_firststmt(bb_stmts.Head());
  bb->Set_laststmt(bb_stmts.Tail());

  // just in case we have an edge to some exit block, fix it up
  if ( bb->Succ()->Contains( _cfg->Exit_bb() ) ) {
    _cfg->DisConnect_predsucc( bb, _cfg->Exit_bb() );
  }

  // connect up with new successor
  _cfg->Connect_predsucc( bb, _label_bb );
}

// ====================================================================
// Find all tail-recursive opportunities and mutate them
// ====================================================================

void OPT_TAIL::Mutate()
{
  if (!Entry_is_well_behaved())
      return;
  
  CFG_ITER cfg_iter(_cfg);
  BB_NODE *bb;

  FOR_ALL_NODE(bb, cfg_iter, Init()) {

    // make sure we're dealing with an exit block
    if ( bb->Kind() != BB_EXIT ) {
      continue;
    }

    if (_do_trace) {
      fprintf(TFile, "Considering exit:\n");
      bb->Print(TFile);
    }
    
    if (Exit_is_well_behaved(bb)) {
      // If we haven't created a tail-recursive entry point, do so now
      if (_top_label == NULL) {
	WN *bb_last_stmt = bb->Laststmt();

	Create_top_label();

	// determine if we split this block
	if ( bb_last_stmt != bb->Laststmt() ) {
	  bb = bb->Next();
	}
      }
      
      // Perform the actual mutation
      Fixup_exit(bb);
      
      if (_do_trace) {
	fprintf(TFile, "New exit is:\n");
	bb->Print(TFile);
      }
    }
  }
  
  if (_do_trace) {
    fprintf(TFile, "%sCFG on exit from tail recursion\n%s", DBar, DBar);
    _cfg->Print(TFile);
  }
}
