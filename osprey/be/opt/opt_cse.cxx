/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
//
// Module: opt_cse.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_cse.cxx,v $
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
// SSA PRE Step 6:
//  For each expression in the work list, find CSEs, and for each CSE,
//  generate the save to temp and the reloads on subsequent usages.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#define opt_cse_CXX	"opt_cse.cxx"

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "opt_defs.h"
#include "opt_base.h"
#include "cxx_memory.h"
#include "opt_bb.h"
#include "opt_fold.h"
#include "opt_htable.h"
#include "opt_ssa.h"
#include "opt_sym.h"
#include "opt_etable.h"
#include "opt_lftr2.h"			// LFTR class
#include "opt_estr.h"
#include "opt_sys.h"
#include "opt_cvtl_rule.h"
#include "config_wopt.h"

class CSE {
private:
  MEM_POOL		_mempool;	// CSE private mempool
  ETABLE               *_etable;        // Pointer to the etable
  EXP_WORKLST	       *_worklist;	// expression being worked on
  STR_RED	       *_str_red;	// strength-reduction class
  BOOL			_tracing;	// Trace output for CSE?

  CSE(void);               	// REQUIRED UNDEFINED UNWANTED methods
  CSE(const CSE&);    		// REQUIRED UNDEFINED UNWANTED methods
  CSE& operator = (const CSE&); // REQUIRED UNDEFINED UNWANTED methods

  ETABLE  *Etable(void) const		{ return _etable; }
  EXP_WORKLST *Worklist(void) const	{ return _worklist; }
  CODEMAP *Htable(void) const		{ return _etable->Htable(); }
  STR_RED *Str_red(void) const		{ return _str_red; }
  BOOL Tracing(void) const		{ return _tracing; }

  void Save_real_occurrence(EXP_OCCURS *occur);
  void Save_hoisted_occurrence(EXP_OCCURS *occur);
  void Save_occurrence_as_l_value(EXP_OCCURS *occur);
  void Save_shrunk_lr_def(EXP_OCCURS *occur);


  // strength-reduction related functions
  CODEREP *Get_new_temp_cr(void) const;
  CODEREP *Find_injury_update( CODEREP *iv, CODEREP *temp ) const;
  CODEREP *Repair_injury_real_real( EXP_OCCURS *def, EXP_OCCURS *use,
				   CODEREP *temp, BOOL temp_is_owner) const;
  CODEREP *Repair_injury_real_phi_opnd( EXP_OCCURS *def, EXP_OCCURS *use,
				       CODEREP *temp ) const;
  CODEREP *Repair_injury_phi_real( EXP_OCCURS *def, EXP_OCCURS *use,
				   CODEREP *temp, BOOL temp_is_owner) const;
  CODEREP *Repair_injury_phi_phi_opnd( EXP_OCCURS *def, EXP_OCCURS *use,
				       CODEREP *temp ) const;
  CODEREP *Repair_injury_rec( CODEREP *iv_def, CODEREP *iv_use, 
			      CODEREP *multiplier, CODEREP *temp,
			      CODEREP *temp_owner_cr)const;
  void Generate_injury_repair( STMTREP *injury, CODEREP *new_temp, 
			 CODEREP *old_temp, CODEREP *multiplier ) const;

public:
			CSE(ETABLE *etable, EXP_WORKLST *worklist);
  		       ~CSE(void);

  void			Do_cse_pass_1(void); // main routine pass 1
  void			Do_cse_pass_2(void); // main routine pass 2

};

// =====================================================================
// Generate a STID (should this be in Htable/CODEMAP class?)
// =====================================================================
STMTREP *
ETABLE::Generate_stid_to_preg( CODEREP *lhs, CODEREP *rhs, MTYPE rhs_type,
			    BB_NODE *bb, SRCPOS linenum ) const
{
  CODEREP *new_cr = Alloc_stack_cr(0);
#ifndef KEY // bug 3022
  if (Split_64_Bit_Int_Ops) { // make sure the types correspond in size
    if (MTYPE_size_min(rhs->Dtyp()) == 64 &&
        MTYPE_size_min(lhs->Dsctyp()) == 32) { // generate a truncation
      new_cr->Init_expr(OPC_U4U8CVT, rhs);
      rhs = Rehash_exp(new_cr, Gvn(rhs));
    }
    else if (MTYPE_size_min(rhs->Dtyp()) == 32 &&
	     MTYPE_size_min(lhs->Dsctyp()) == 64 &&
	     inCODEKIND(rhs->Kind(), CK_VAR|CK_IVAR)) {
      if (MTYPE_signed(rhs->Dtyp()))
        new_cr->Init_expr(OPC_I8I4CVT, rhs);
      else new_cr->Init_expr(OPC_U8U4CVT, rhs);
      rhs = Rehash_exp(new_cr, Gvn(rhs));
    }
  }
#endif

  STMTREP *savestmt;
  savestmt = CXX_NEW(STMTREP, Htable()->Mem_pool());
  savestmt->Init( lhs, rhs,
		 OPCODE_make_op(OPR_STID, MTYPE_V, lhs->Dsctyp()));
  savestmt->Set_chi_list(NULL);
  //  savestmt->Set_rhs_type(rhs_type);
  lhs->Set_defstmt(savestmt);

  savestmt->Set_bb( bb );
  savestmt->Set_linenum( linenum );
  savestmt->Set_live_stmt();

  if (WOPT_Enable_Verbose && Tracing()) {
    fprintf(TFile, "generate_stid_to_preg:\n");
    savestmt->Print(TFile);
  }
  return savestmt;
}

/* CVTL-RELATED start (performance) */
// =====================================================================
// We are about to generate a def of a preg promoted from a variable whose
// desc type is to_mtype.  The rhs of the preg def must be a CK_VAR node.
// If the rhs's value can be determined to be always less than the size
// implied by to_mtype, the truncation can be omitted.
// Note: the first screen of PRE generated PREG size assumes that BDCE's
// algorithms have not been applied; as a result, this routine should not
// be called after BDCE has been applied.
// =====================================================================
BOOL
No_truncation_by_value_size(MTYPE     to_mtype,
			    BOOL      sign_extd,
			    CODEREP  *rhs,
			    OPT_STAB *opt_stab,
			    BOOL      trace_phi)
    
{
  FmtAssert(rhs->Kind() == CK_VAR,
	    ("No_truncation_by_value_size: Invalid input"));

  // The first screen of PRE generated PREG size
  AUX_STAB_ENTRY *aux_entry = opt_stab->Aux_stab_entry(rhs->Aux_id());
  if ( ST_class(aux_entry->St()) == CLASS_PREG &&
      aux_entry->Value_size() > 0) 
    return ((aux_entry->Value_size() <= MTYPE_size_min(to_mtype)) &&
	    (sign_extd && aux_entry->Is_sign_extd() || 
	     !sign_extd && aux_entry->Is_zero_extd()));

  // The secondary screen of variable with direct definition
  if (rhs->Is_flag_set(CF_DEF_BY_CHI) || rhs->Is_flag_set(CF_IS_ZERO_VERSION))
    return FALSE;

  if (trace_phi && rhs->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *defphi = rhs->Defphi();
    if (!defphi || defphi->Is_size_visited()) return FALSE;
    defphi->Set_size_visited();
    for (INT i = 0; i < defphi->Size(); ++i) {
      CODEREP *opnd = defphi->OPND(i);
      if (opnd->Is_flag_set(CF_IS_ZERO_VERSION) ||
	  ! MTYPE_is_integral(opnd->Dtyp()) ||
	  ! No_truncation_by_value_size(to_mtype, sign_extd, opnd, opt_stab, trace_phi)) {
	defphi->Reset_size_visited();
	return FALSE;
      }
    }
    defphi->Reset_size_visited();
    return TRUE;
  }

  STMTREP *stmt = rhs->Defstmt();
  if ( stmt != NULL ) { // def by STID
    if (stmt->Is_size_visited()) return FALSE; // may as well just return TRUE

    if (OPERATOR_is_scalar_store (stmt->Opr())) {
      CODEREP *rhs_new = stmt->Rhs();
      INT signess = 0;
      INT vsize = Actual_data_size( rhs_new, opt_stab, signess );
      if ( vsize > 0 && vsize <= MTYPE_size_min( to_mtype ) &&
	  (sign_extd && (signess & SIGN_1_EXTD) ||
	   !sign_extd && (signess & SIGN_0_EXTD)) ) return TRUE;
      if (rhs_new->Kind() == CK_VAR &&
	  MTYPE_is_integral(rhs_new->Dtyp())) {
	stmt->Set_size_visited();
	BOOL retv = No_truncation_by_value_size(to_mtype, sign_extd, rhs_new, opt_stab, trace_phi);
	stmt->Reset_size_visited();
	return retv;
      }
    }
  } else { // def by LDID
    return ( MTYPE_size_min( rhs->Dsctyp() ) <= MTYPE_size_min( to_mtype ) &&
	     sign_extd == rhs->Is_sign_extd() );
  }
  
  return FALSE;
}
/* CVTL-RELATED finish */

// =====================================================================
// Insert a preceding statement that saves the rhs of the current statement
// into the preg given by pregcr and replace the rhs of the current statement
// by a load of that pregcr. Return the newly created statement.
// Note: Rhs()'s usecount is unchanged.
// =====================================================================
STMTREP *
ETABLE::Save_replace_rhs_by_preg(STMTREP *stmt, 
				 CODEREP *pregcr, 
				 EXP_WORKLST *wk)
{
  CODEREP     *rhs = stmt->Rhs();
  CODEREP     *lhs = stmt->Lhs();
  const UINT32 gvn = Gvn(rhs);

  /* CVTL-related start (correctness) */
  // Insert CVT/CVTL if necessary for ISTORE/STORE
  if (wk != NULL && 
      MTYPE_size_min(wk->Exp()->Dsctyp()) <= MTYPE_size_min(MTYPE_I4) &&
      lhs->Is_integral_load_store()) {

    if (WOPT_Enable_Min_Type && 
	MTYPE_is_integral(rhs->Dtyp()) && 
	(rhs->Kind() == CK_VAR 
#ifdef TARG_X8664 // bug 8056: I4I2LDID of PREG is valid when ASM uses 
		  // 16- or 8-bit registers and the CVTL cannot be omitted
	 && !Opt_stab()->Aux_stab_entry(rhs->Aux_id())->Is_preg() 
#endif
	 ||
	 (rhs->Kind() == CK_IVAR && rhs->Ivar_has_e_num())) &&
	MTYPE_size_min(wk->Exp()->Dsctyp()) == MTYPE_size_min(rhs->Dsctyp()) &&
	wk->Sign_extd() == rhs->Is_sign_extd())
      ; // don't need truncation
    else {
	
      CODEREP *cr = Alloc_stack_cr(0);    
      
      FOLD     ftmp;
      //  We still want the folding of at the top level but because currently
      //  we cannot control how many level simplifier will do.  We turn it
      //  off to avoid changing any subexpression.
      OPCODE opc;
      MTYPE  dsctyp = Mtype_TransferSign(wk->Sign_extd() ? 
					 MTYPE_I2 : MTYPE_U2, 
					 wk->Exp()->Dsctyp());
      INT    cvt_kind  = Need_type_conversion(rhs->Dtyp(), dsctyp, &opc);
      
      if (WOPT_Enable_Min_Type && 
	  rhs->Kind() == CK_VAR &&
	  MTYPE_is_integral(rhs->Dtyp()) && 
	  MTYPE_is_signed(dsctyp)==MTYPE_is_signed(rhs->Dtyp()) &&
	  No_truncation_by_value_size(dsctyp, wk->Sign_extd(), rhs, Htable()->Sym())) 
	cvt_kind = NOT_AT_ALL;

      switch (cvt_kind) {
      case NOT_AT_ALL:
	break;
      case NEED_CVT: 
	if (opc != OPC_U4U8CVT) {
	  cr->Init_expr(opc, rhs);
	  // Fix 629602:  defer constant folding because the RHS might exists in
	  // some worklist.
	  // if (rhs->Kind() == CK_CONST)  {
	  // rhs = ftmp.Fold_Expr(cr);
	  //  if (!rhs) {
	  //  rhs = Rehash_exp(cr, gvn);
	  // }
	  // } else {
	  rhs = Rehash_exp(cr, gvn);
	  // }
	  break;
	}
	opc = OPC_U8CVTL;
	// fall through to generate (U8CVTL 32) high 32 bit zero extended value
      case NEED_CVTL:
	cr->Init_expr(opc, rhs);
	cr->Set_offset(MTYPE_size_min(dsctyp));
	// Fix 629602:  defer constant folding because the RHS might exists in
	// some worklist.
	rhs = Rehash_exp(cr, gvn);
	break;
      }
    }
  }
  /* CVTL-RELATED finish */

  STMTREP *savestmt = Generate_stid_to_preg( pregcr,
    rhs, rhs->Dtyp(), stmt->Bb(), stmt->Linenum() );

  Deferred_cfold_stmts()->Push(savestmt);

  // tricky, use the statement id from the original statement
  savestmt->Set_stmt_id(stmt->Stmt_id());
  // insert the statement
  savestmt->Bb()->Stmtlist()->Insert_Before(savestmt, stmt);

  if (Pre_kind() == PK_VNFRE)
     VNFRE::move_rhs_occurs(stmt, savestmt);

  // replace rhs of ISTORE/STORE with the temp
  stmt->Set_rhs(pregcr);

  //  stmt->Set_rhs_type(pregcr->Dtyp());
  stmt->Reset_volatile_stmt(); // may be set earlier because rhs has volatile
  stmt->Rhs()->Set_defstmt(savestmt);
  pregcr->IncUsecnt();

  return savestmt;
}

// =====================================================================
// Save_occurrence_as_l_value - The given real occurrence is an ISTORE.
// Save the rhs of the ISTORE to a temp and replace the rhs of the ISTORE
// with the temp.  The Temp_cr() field must have been set by an earlier
// call to Get_temp_cr().
// =====================================================================
void
CSE::Save_occurrence_as_l_value(EXP_OCCURS *occur)
{
  STMTREP *savestmt = Etable()->Save_replace_rhs_by_preg(occur->Stmt(), occur->Temp_cr(), Worklist());

  // test if the savestmt is an iv_update
  Str_red()->Determine_iv_update(savestmt, NULL);

  Worklist()->Inc_reload_count();

  // Set the SRF_RHS_SAVED bit
  occur->Stmt()->Set_RHS_saved(); savestmt->Set_saved_RHS();
}

// =====================================================================
// Save_real_occurrence - The given real occurrence node is to be saved
// to a temp.  The Temp_cr() field must have been set by an earlier call to 
// Get_temp_cr().
// =====================================================================
void
CSE::Save_real_occurrence(EXP_OCCURS *occur)
{
  CODEREP *tempcr = occur->Temp_cr();
  CODEREP *rhs    = occur->Occurrence();
  
  /* CVTL-RELATED start (correctness) */
  // Add necessary CVT/CVTL
  if (Etable()->Pre_kind() != PK_VNFRE
      && (occur->Occ_kind() != EXP_OCCURS::OCC_PHI_PRED_OCCUR ||
	  !occur->Inserted_computation())
      && Worklist()->Exp()->Is_integral_load_store()) {
    rhs = Worklist()->Save_use_cr(Etable(), rhs);
  }
  /* CVTL-RELATED finish */
  
  // generate the save statement
  STMTREP *savestmt =
    Etable()->Generate_stid_to_preg(tempcr,
			  rhs, rhs->Dtyp(), occur->Bb(),
			  (occur->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR ?
			   occur->Stmt()->Linenum() :
			   (occur->Bb()->Last_stmtrep() == NULL ?
			    occur->Bb()->Linenum() :
			    occur->Bb()->Last_stmtrep()->Linenum())) );

  // test if the savestmt is an iv_update
  BOOL is_iv_update = Str_red()->Determine_iv_update(savestmt, NULL);

  rhs->IncUsecnt();

  LFTR *lftr = Etable()->Lftr();
  // insert the statement
  if (occur->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR) {
    // tricky, use the statement id from the original statement
    savestmt->Set_stmt_id(occur->Stmt()->Stmt_id());
    savestmt->Bb()->Stmtlist()->Insert_Before(savestmt, occur->Stmt());

    // if this statement saves a boolean comparison, find the
    // corresponding comp occur and update stmt ptr
    if (lftr->Lftr_on() && lftr->Is_comparison(occur->Occurrence())) {
      lftr->Update_comp_occur_stmt(occur, savestmt);
    }
  }
  else {
    Is_True(occur->Inserted_computation(),
	    ("CSE::Save_real_occurrence: saved non-real occurrence "
	     "must be phi-pred"));

    IDX_32 sid = (savestmt->Bb()->Last_stmtrep() != NULL ? 
		  savestmt->Bb()->Last_stmtrep()->Stmt_id() :
		  Etable()->Cfg()->Get_stmt_id());
    savestmt->Set_stmt_id(sid);
    savestmt->Bb()->Append_stmt_before_branch(savestmt);
    occur->Set_enclose_stmt(savestmt);
    occur->Set_encl_stmt_set();

    Is_Trace(Tracing(), (TFile, "CSE::Save_real_occurrence: Inserted\n"));
    Is_Trace_cmd(Tracing(), savestmt->Print(TFile));

    // There is no comp occur since this is a new inserted expression so
    // add a new comp_occur. Don't create a new real occur since this
    // expr has already been processed.
    lftr->Insert_comp_occurrence(occur->Occurrence(),savestmt,0);

    if (is_iv_update) {
      FmtAssert(FALSE, ("CSE: saving insertion, is_iv_udpate must be FALSE"));
    }
    // Folding and copy prop have to be handled after the CodeMotion
    // step is complete because everyone up through CodeMotion
    // (LFTR in particular) assumes that inserted and saved real
    // occurrences will remain instances of the current expression
    // being optimized.
    Etable()->Schedule_for_ocopy(occur);
  }

  if (Etable()->Pre_kind() == PK_VNFRE)
     VNFRE::new_occurs(savestmt);
}


// =====================================================================
// Save_hoisted_occurrence - The given hoisted occurrence node is to be saved
// to a temp.  The Temp_cr() field must have been set by an earlier call to 
// Get_temp_cr().
// =====================================================================
void 
CSE::Save_hoisted_occurrence(EXP_OCCURS *occur)
{
  CODEREP *tempcr = occur->Temp_cr();
  CODEREP *rhs    = occur->Occurrence();

  /* CVTL-RELATED start (correctness) */
  // Add necessary CVT/CVTL
  if (rhs->Is_integral_load_store() && Worklist()->Exp()->Is_integral_load_store()) {
    rhs = Worklist()->Save_use_cr(Etable(), rhs);
  }
  /* CVTL-RELATED finish */

  // generate the save statement
  STMTREP *savestmt = Etable()->Generate_stid_to_preg( tempcr,
					     rhs, rhs->Dtyp(), occur->Bb(), occur->Bb()->Linenum() );

  LFTR *lftr = Etable()->Lftr();
  // insert the statement

  IDX_32 sid = savestmt->Bb()->Last_stmtrep() != NULL ? 
    savestmt->Bb()->Last_stmtrep()->Stmt_id() : Etable()->Cfg()->Get_stmt_id();
  savestmt->Set_stmt_id(sid);
  savestmt->Bb()->Append_stmt_before_branch(savestmt);

  // There is no comp occur since this is a new inserted expression so
  // add a new comp_occur. Don't create a new real occur since this
  // expr has already been processed.
  lftr->Insert_comp_occurrence(occur->Occurrence(),savestmt,0);
  
  // set the statement for the occur to the savestmt
  occur->Set_enclose_stmt(savestmt);
  occur->Reset_hoisted();

  // Put the new occurrence on the list of inserted items to
  // undergo output copy prop after CodeMotion.
  Etable()->Schedule_for_ocopy(occur);

  if (Etable()->Pre_kind() == PK_VNFRE)
     VNFRE::new_occurs(savestmt);
}

// =====================================================================
// Save_shrunk_lr_def (LPRE only)- The given occurrence node is the first use 
// of a sunken l-value def involved in live range shrinking. Generate and insert
// the sunken def statement before the occurrence node, with its lhs being given
// by Temp_cr(), which must have been set by an earlier call to Get_temp_cr().
// occur can be a real occurrence or a phi-pred occurrence.  If it is a phi-pred
// occurrence, then the statement is appended at the end of the BB.
// =====================================================================
void 
CSE::Save_shrunk_lr_def(EXP_OCCURS *occur)
{
  CODEREP      *tempcr = occur->Temp_cr();
  CODEREP      *rhs = occur->Occurrence()->Defstmt()->Rhs();
  const UINT32  gvn = Etable()->Gvn(rhs);

  /* CVTL-RELATED start (correctness) */
  // Add necessary CVT/CVTL
  if (MTYPE_is_integral(rhs->Dtyp()) &&
      MTYPE_size_min(Worklist()->Exp()->Dsctyp()) <= MTYPE_size_min(MTYPE_I4)) {
    CODEREP *cr = Alloc_stack_cr(0);
    FOLD ftmp;
    OPCODE opc;
    MTYPE  dsctyp = Mtype_TransferSign(Worklist()->Sign_extd() ?  MTYPE_I2 : MTYPE_U2, 
				       Worklist()->Exp()->Dsctyp());
    INT    cvt_kind  = Need_type_conversion(rhs->Dtyp(), dsctyp, &opc);
  
    switch (cvt_kind) {
    case NOT_AT_ALL:
      break;
    case NEED_CVT: 
      if (opc != OPC_U4U8CVT) {
        cr->Init_expr(opc, rhs);
        if (rhs->Kind() == CK_CONST)  {
	  rhs = ftmp.Fold_Expr(cr);
	  if (!rhs) {
	    rhs = Etable()->Rehash_exp(cr, gvn);
	  }
        } else {
	  rhs = Etable()->Rehash_exp(cr, gvn);
        }
        break;
      }
      opc = OPC_U8CVTL;
      // fall through to generate (U8CVTL 32) high 32 bit zero extended value
    case NEED_CVTL:
      cr->Init_expr(opc, rhs);
      cr->Set_offset(MTYPE_size_min(dsctyp));
      if (rhs->Kind() == CK_CONST) {
        rhs = ftmp.Fold_Expr(cr);
        if (!rhs) {
	  rhs = Etable()->Rehash_exp(cr, gvn);
        }
      } else {
        rhs = Etable()->Rehash_exp(cr, gvn);
      }
      break;
    }
  }
  /* CVTL-RELATED finish */

  rhs->IncUsecnt();

  STMTREP *savestmt;
  if (occur->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR) {
    STMTREP *curstmt = occur->Stmt();
    BB_NODE *bb = curstmt->Bb();
    savestmt = Etable()->Generate_stid_to_preg(tempcr, rhs, rhs->Dtyp(), bb, 
					       curstmt->Linenum());
    savestmt->Set_stmt_id(curstmt->Stmt_id());
    // insert the statement
    bb->Stmtlist()->Insert_Before(savestmt, curstmt);
  }
  else { // OCC_PHI_PRED_OCCUR
    BB_NODE *bb = occur->Bb();
    savestmt = Etable()->Generate_stid_to_preg(tempcr, rhs, 
				     rhs->Dtyp(), bb,
				     (bb->Last_stmtrep() == NULL ?
				      bb->Linenum() :
				      bb->Last_stmtrep()->Linenum()) );
    IDX_32 sid = (bb->Last_stmtrep() != NULL ? bb->Last_stmtrep()->Stmt_id() :
		  Etable()->Cfg()->Get_stmt_id());
    savestmt->Set_stmt_id(sid);
    bb->Append_stmt_before_branch(savestmt);
    occur->Set_enclose_stmt(savestmt);
    occur->Set_encl_stmt_set();
  }

  if (Etable()->Pre_kind() == PK_VNFRE)
     VNFRE::new_occurs(savestmt);
}


//=====================================================================
// allocate a new temp_cr node (TEMPORARY FUNCTION- MOVE THIS TO 
// SOME CLASS
//=====================================================================

CODEREP	*
CSE::Get_new_temp_cr( void ) const
{
  Is_True(_worklist->Preg() != 0,
	  ("CSE::Get_new_temp_cr: current temp PREG must not be zero"));

  CODEREP *new_temp =
      Htable()->Add_def(_worklist->Preg(), 
			_worklist->Cur_e_version(), NULL,
			_worklist->Exp()->Dtyp(), _worklist->Exp()->Dtyp(),
			Htable()->Sym()->St_ofst(_worklist->Preg()),
			ST_type(MTYPE_To_PREG(_worklist->Exp()->Dtyp())),
			0, TRUE);
  _worklist->New_e_version();

  if (Etable()->Pre_kind() == PK_VNFRE)
     VNFRE::add_valnum(new_temp, _worklist->E_num()/*value number*/);

  return new_temp;
}

//======================================================================
// the temp owns the coderep node (to avoid rehashing); overwrite the
// expr node given by "cr" to make it the CK_VAR node for the temp
//======================================================================
static void
Change_expr_to_temp(CODEREP *cr, EXP_WORKLST *wk, CODEMAP *htable)
{
  Is_True(cr != NULL && cr->Kind() != CK_VAR, 
	  ("EXP_OCCURS::Change_expr_to_temp_cr: already a var node"));
  if (wk->Preg() == 0)
    wk->Set_preg(htable->Sym()->Create_preg(wk->Exp()->Dtyp()));

  htable->Remove(cr);

  UINT coderep_id_save = cr->Coderep_id();
  cr->Init_var(wk->Exp()->Dtyp(), wk->Preg(), wk->Cur_e_version(), 
	       wk->Exp()->Dtyp(), htable->Sym()->St_ofst(wk->Preg()), 
	       ST_type(MTYPE_To_PREG(wk->Exp()->Dtyp())),
	       wk->Exp()->Field_id());
  if (wk->Exp()->Bit_field_valid())
    cr->Set_bit_field_valid();
  cr->Set_coderep_id(coderep_id_save);
  cr->Set_sign_extension_flag();
  cr->Set_defstmt(NULL);

  // enter coderep nodes in aux_stab
  AUX_STAB_ENTRY *aux = htable->Sym()->Aux_stab_entry(wk->Preg());
  aux->Set_cr_list(aux->Cr_list()->Prepend(cr));

  wk->New_e_version();
  wk->Inc_temp_owner_count();
}

// =====================================================================
// Find the temporary generated for the repair of the the injury caused
// by the definition of "iv"
// =====================================================================

CODEREP *
CSE::Find_injury_update( CODEREP *iv, CODEREP *temp ) const
{
  Is_True( !iv->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI|
				      CF_DEF_BY_PHI|
				      CF_IS_ZERO_VERSION)),
    ("CSE::Find_injury_update: iv not defined by stmt") );

  STMTREP *iv_def = iv->Defstmt();
  Is_True( iv_def->Repaired(),
    ("CSE::Find_injury_update: iv's def still injured") );

  // continue searching forward until we find one that defines the temp
  for ( STMTREP *tmp_def = iv_def->Next(); 
	tmp_def != NULL;
	tmp_def = tmp_def->Next() )
  {
    if (OPERATOR_is_scalar_store (tmp_def->Opr())) {
      if ( tmp_def->Lhs()->Aux_id() == temp->Aux_id() ) {
	return tmp_def->Lhs();
      }
    }
  }

  return NULL;
}

// =====================================================================
// Generate the temporary update to repair the injury
// =====================================================================

void
CSE::Generate_injury_repair( STMTREP *injury, CODEREP *new_temp, 
			     CODEREP *old_temp, CODEREP *multiplier ) const
{
  CODEREP *updated_iv;
  CODEREP *incr_amt;
  BOOL is_add;

  if ( !Str_red()->Find_iv_and_incr(injury, &updated_iv, 
				    &incr_amt, &is_add))
  {
    FmtAssert( FALSE,
      ("CSE::Generate_injury_repair: injury is not iv update") );
  }

  CODEREP *temp_incr;
  if ( multiplier == NULL ) {
    // the multiplier is essentially "1".  But, there is the
    // possibility that the expression is CVT(i) rather than i+k,i-k
    if ( Worklist()->Exp()->Kind() == CK_OP &&
	 Worklist()->Exp()->Opr() == OPR_CVT )
    {
      /* CVTL-RELATED start (performance) */
      // Cand: cvt(i)
      // Iv  : i=i+incr_amt
      // do we need to keep the conversion?
      OPCODE cvt_opc;
#ifndef KEY
      INT type_conversion = Need_type_conversion(incr_amt->Dtyp(), 
					old_temp->Dtyp(), &cvt_opc);
#else // bug 7858: it is safer to keep the increment amount as signed
      INT type_conversion = Need_type_conversion(incr_amt->Dtyp(), 
		      Mtype_TransferSign(MTYPE_I8, old_temp->Dtyp()), &cvt_opc);
#endif
      if ( type_conversion == NEED_CVT ) {
	temp_incr = Htable()->Add_unary_node_and_fold(cvt_opc,incr_amt);
      }
      else if ( type_conversion == NEED_CVTL ) {
	FmtAssert( FALSE, ("CSE::Generate_injury_repair: need cvtl") );
      }
      else {
	Is_True( type_conversion == NOT_AT_ALL,
	  ("CSE::Generate_injury_repair: type_conversion=%d",
	   type_conversion) );

	temp_incr = incr_amt;
      }
      /* CVTL-RELATED finish */
    }
    else {
      // Cand: i+k, i-k
      // Iv  : i=i+incr_amt
      temp_incr = incr_amt;
    }
  }
  else if ( incr_amt == NULL ) {
    // Cand: i*k
    // Iv  : i=i
    temp_incr = multiplier;
  }
  else {
    // Cand: i*k
    // Iv  : i=i+incr_amt
    // NOTE: multiplier->Dtyp() in the following statement is
    // wrong. We want the type of the temp. (bug 555210)
    OPCODE mpy_opc;
    mpy_opc = OPCODE_make_op(OPR_MPY,
                                    // multiplier->Dtyp(),
                                    old_temp->Dtyp(),
                                    MTYPE_V);
// bug 11692, OSP_458
    if (MTYPE_signed(incr_amt->Dtyp()))
      mpy_opc = OPCODE_make_op(OPR_MPY,
                                Mtype_TransferSign(MTYPE_I8, old_temp->Dtyp()),
                                MTYPE_V);
    
    temp_incr = Htable()->Add_bin_node_and_fold( mpy_opc, 
						 multiplier, incr_amt );
  }

  OPCODE incr_opc = OPCODE_make_op( (is_add?OPR_ADD:OPR_SUB),
				    old_temp->Dtyp(), MTYPE_V );
  CODEREP *new_rhs = Htable()->Add_bin_node_and_fold( incr_opc, 
						old_temp, temp_incr );

  STMTREP *new_stid = Etable()->Generate_stid_to_preg( new_temp, new_rhs, 
    OPCODE_rtype(incr_opc), injury->Bb(), injury->Linenum() );
  
  // and insert after the injury
  Etable()->Insert_stmtrep_after( new_stid, injury );

  // see if any of the expressions we've created should be added to
  // the expression list.  NOTE that we do not add the rhs to the
  // worklist because we do not want to work on another strength-
  // reduction candidate that includes the temp (it's a new iv),
  // otherwise, we'll get into infinite loop.
  if ( new_rhs->Kind() == CK_OP ) {
    for ( INT k = 0; k < new_rhs->Kid_count(); k++ ) {
      CODEREP *rhs_kid = new_rhs->Opnd(k);

      // Exclude expressions that include the temporary, which
      // becomes an induction variable.  This keeps us from getting
      // into an infinite loop of strength-reducing these exprs.
      if ( rhs_kid->Contains(old_temp) ) {
	Is_Trace(Tracing(),
		 (TFile, "CSE::Generate_injury_repair: "
		  "Exclude new expr (kid of rhs):\n"));
	Is_Trace_cmd(Tracing(), rhs_kid->Print(4, TFile));
      }
      else {
	// new expressions get added to worklist to be dealt with
	// later.
	Is_Trace(Tracing(),
		 (TFile, "CSE::Generate_injury_repair: "
		  "New leaf expression (kid of rhs):\n"));
	Is_Trace_cmd(Tracing(), rhs_kid->Print(4, TFile));

	Etable()->Bottom_up_cr( new_stid, 0, rhs_kid, FALSE,
			       ETABLE::URGENT_INSERT, 0,
			       OPCODE_UNKNOWN, FALSE);
      }
    }
  }

  // mark the statement as an iv update, which it is, unless the
  // result type is a floating point type. This can happen when we
  // strength-reduce an int-to-float convert.
  if (MTYPE_IS_INTEGER(OPCODE_desc(new_stid->Op()))
#ifdef KEY // bug 5029
      && new_rhs->Kind() == CK_OP
#endif
      ) {
    new_stid->Set_iv_update();
  }

  // say we've fixed the injury
  Str_red()->Set_repaired(injury);

  if ( Tracing() ) {
    fprintf( TFile, "CSE::Generate_injury_repair in BB:%d\n",
	     injury->Bb()->Id() );
    new_stid->Print(TFile);
  }

  if (Etable()->Pre_kind() == PK_VNFRE)
     VNFRE::new_occurs(new_stid);
}

// =====================================================================
// Recursive function to repair injuries; if temp_owner_cr is not NULL,
// use it instead of calling Get_new_temp_cr because it is an
// avoid-rehash node.
// =====================================================================

CODEREP *
CSE::Repair_injury_rec(CODEREP *iv_def, CODEREP *iv_use, 
		       CODEREP *multiplier, CODEREP *temp, 
		       CODEREP *temp_owner_cr) const
{
  if ( iv_use->Is_flag_set(CF_DEF_BY_CHI) ) {
    FmtAssert( FALSE, ("CSE::Repair_injury_rec: injured by chi") );
    return NULL;
  }
  else if ( iv_use->Is_flag_set(CF_DEF_BY_PHI) ) {
    FmtAssert( FALSE, 
      ("CSE::Repair_injury_rec: unimplemented injured by phi") );
    return NULL;
  }
  else {
    CODEREP *new_temp = NULL;
    CODEREP *old_temp = temp;
    STMTREP *injury = iv_use->Defstmt();

    Is_True( injury->Iv_update(),
      ("CSE::Repair_injury_rec: injury is not an iv_use update") );

    if ( !Str_red()->Repaired(injury) ) {
      // it's possible we CSE'd the rhs of the iv update
      CODEREP *injury_rhs = injury->Rhs();

      if ( injury_rhs->Kind() == CK_VAR ) {
	injury_rhs = Str_red()->Find_real_defs_rhs(injury_rhs);
      }

      // determine if this is the last injury we need to deal with.
      // We check if the rhs contains the iv_def.  If so, then this
      // is the last injury.  Otherwise, we need to keep going to find
      // other injuries.
      if ( ! injury_rhs->Contains( iv_def ) ) {
	// need to look for the injury that precedes this one.

	// find the iv on the rhs (as a side-effect of seeing if this
	// is an iv-update), which should be true.
	CODEREP *iv_rhs;
	if ( !Str_red()->Determine_iv_update( injury, &iv_rhs ) ) {
	  FmtAssert( FALSE,
            ("CSE::Repair_injury_rec: injury is not iv_update") );
	}

	old_temp = Repair_injury_rec(iv_def, iv_rhs, multiplier, old_temp, NULL);
      }

      // we now need to repair this injury
      if (temp_owner_cr == NULL)
        new_temp = Get_new_temp_cr();
      else {
	Is_True(temp_owner_cr->Coderep_id() != 0,
		("CSE::Repair_injury_rec: temp owner has 0 coderep id"));
	Change_expr_to_temp(temp_owner_cr, _worklist, _etable->Htable());
	new_temp = temp_owner_cr;
      }
      Generate_injury_repair( injury, new_temp, old_temp, multiplier );
      if (_worklist->Exp()->Kind() == CK_OP &&
	  (_worklist->Exp()->Opr() == OPR_ADD || 
	   _worklist->Exp()->Opr() == OPR_SUB))
	injury->Inc_str_red_num();
    }
    else {
      // injury was fixed already, so find the temp that the repair
      // generates.
      new_temp = Find_injury_update( injury->Lhs(), temp );
    }

    return new_temp;
  }
}

// =====================================================================
// Insert any necessary updates to the temporary due to strength-reduced
// injuries.  "temp" is the temporary used to hold the value of "def".
//
// Handles real def and real use
//
// Return the update's result (i.e., a new version of "temp").
// =====================================================================

CODEREP *
CSE::Repair_injury_real_real( EXP_OCCURS *def, EXP_OCCURS *use, CODEREP *temp,
			      BOOL temp_is_owner) const
{
  if ( def->Occurrence() == use->Occurrence() ) {
    // the use's occurrence should just be able to use the def's
    // temporary 
    return temp;
  }
  else {
    CODEREP *iv_def, *iv_use, *multiplier;
    Str_red()->Find_iv_and_mult(def,&iv_def, use, &iv_use, &multiplier);

    if ( Tracing() ) {
      fprintf( TFile, "Repair_injury_real_real: iv_def: " );
      iv_def->Print(0,TFile);
      fprintf( TFile, "Repair_injury_real_real: iv_use: " );
      iv_use->Print(0,TFile);
      fprintf( TFile, "Repair_injury_real_real: multi: " );
      if ( multiplier != NULL )
	multiplier->Print(0,TFile);
      else
	fprintf( TFile, "1\n" );
    }

    CODEREP *new_t = Repair_injury_rec(iv_def,iv_use, multiplier, temp, 
				    temp_is_owner ? use->Occurrence() : NULL);
    return new_t;
  }
}

// =====================================================================
// Insert any necessary updates to the temporary due to strength-reduced
// injuries.  "temp" is the temporary used to hold the value of "def".
//
// Handles real def and phi-opnd use
//
// Return the update's result (i.e., a new version of "temp").
// =====================================================================

CODEREP *
CSE::Repair_injury_real_phi_opnd( EXP_OCCURS *def, EXP_OCCURS *use, CODEREP *temp ) const
{
  Is_True( use->Occ_kind() == EXP_OCCURS::OCC_PHI_PRED_OCCUR,
    ("CSE::Repair_injury_real_phi_opnd: not phi-pred-occur") );

  // it's possible that the def's occur and this pred's occur actually
  // are matching codereps, with the same version of everything.  This happens
  // when the def is a real occurrence that is renamed to identical e-version
  // as a phi that is later determined to be not downsafe.
  if ( def->Occurrence() == use->Occurrence() ) {
    // since the versions of everything match, we use the same temp
    Is_Trace(Tracing(), (TFile, 
			 "CSE::Repair_injury_real_phi_opnd: def/use match\n"));
    return temp;
  }
  CODEREP *iv_def, *iv_use, *multiplier;
  Str_red()->Find_iv_and_mult( def, &iv_def, use, &iv_use, &multiplier);
  if ( Tracing() ) {
    fprintf( TFile, "Repair_injury_real_phi_opnd: iv_def: " );
    iv_def->Print(0,TFile);
    fprintf( TFile, "Repair_injury_real_phi_opnd: iv_use: " );
    iv_use->Print(0,TFile);
    fprintf( TFile, "Repair_injury_real_phi_opnd: multi: " );
    if ( multiplier != NULL )
      multiplier->Print(0,TFile);
    else
      fprintf( TFile, "1\n" );
  }

  CODEREP *new_t = Repair_injury_rec( iv_def, iv_use, multiplier, temp, NULL);
  return new_t;
}

// =====================================================================
// Insert any necessary updates to the temporary due to strength-reduced
// injuries.  "temp" is the temporary used to hold the value of "def".
//
// Handles phi-result def and real use
//
// Return the update's result (i.e., a new version of "temp").
// =====================================================================

CODEREP *
CSE::Repair_injury_phi_real( EXP_OCCURS *def, EXP_OCCURS *use, CODEREP *temp,
			     BOOL temp_is_owner) const
{
  CODEREP *iv_def, *iv_use, *multiplier;

  // this function can handle real or phi-result defs
  Str_red()->Find_iv_and_mult( def, &iv_def, use, &iv_use, &multiplier);

  if ( Tracing() ) {
    fprintf( TFile, "Repair_injury_phi_real: iv_def: " );
    iv_def->Print(0,TFile);
    fprintf( TFile, "Repair_injury_phi_real: iv_use: " );
    iv_use->Print(0,TFile);
    fprintf( TFile, "Repair_injury_phi_real: multi: " );
    if ( multiplier != NULL )
      multiplier->Print(0,TFile);
    else
      fprintf( TFile, "1\n" );
  }

  CODEREP *new_t = Repair_injury_rec( iv_def, iv_use, multiplier, temp, 
				    temp_is_owner ? use->Occurrence() : NULL);
  return new_t;
}

// =====================================================================
// Insert any necessary updates to the temporary due to strength-reduced
// injuries.  "temp" is the temporary used to hold the value of "def".
//
// Handles phi-result def and phi-opnd use
//
// Return the update's result (i.e., a new version of "temp").
// =====================================================================

CODEREP *
CSE::Repair_injury_phi_phi_opnd( EXP_OCCURS *def, EXP_OCCURS *use,
				 CODEREP *temp ) const
{
  Is_True( use->Occ_kind() == EXP_OCCURS::OCC_PHI_PRED_OCCUR,
    ("CSE::Repair_injury_phi_phi_opnd: not phi-pred-occur") );

  CODEREP *iv_def, *iv_use, *multiplier;
  // this function can handle real or phi-result defs
  Str_red()->Find_iv_and_mult( def, &iv_def, use, &iv_use, &multiplier);
  if ( Tracing() ) {
    fprintf(TFile, "Repair_injury_phi_phi_opnd: phi-pred in BB%d\n",
	    use->Bb()->Id());
    fprintf( TFile, "Repair_injury_phi_phi_opnd: iv_def: " );
    iv_def->Print(0,TFile);
    fprintf( TFile, "Repair_injury_phi_phi_opnd: iv_use: " );
    iv_use->Print(0,TFile);
    fprintf( TFile, "Repair_injury_phi_phi_opnd: multi: " );
    if ( multiplier != NULL )
      multiplier->Print(0,TFile);
    else
      fprintf( TFile, "1\n" );
  }

  CODEREP *new_t = Repair_injury_rec( iv_def, iv_use, multiplier, temp, NULL);
  return new_t;
}


// ======================================================================
// Do_cse_pass_1 - pass 1 of main routine for this file (without a stack). 
// To implement rehash avoidance, this first pass rehashes expr nodes that
// will not own their coderep nodes, and, if needed, rehash the trees
// containing them.  It also sets the temp_cr field to remember the original
// node that will be overwritten to become the temp.
// ======================================================================
void
CSE::Do_cse_pass_1(void)
{
  // Iterate over the occurrences list in DPO
  EXP_ALL_OCCURS_ITER exp_occ_iter(_worklist->Real_occurs().Head(),
				   NULL, /* no LFTR */
				   _worklist->Phi_occurs().Head(),
				   _worklist->Phi_pred_occurs().Head(),
				   _etable->Exit_occurs().Head());
  
  EXP_OCCURS *occur, *tos;
  CODEREP *tempcr;
  STMTREP *savestmt;

  FOR_ALL_NODE(occur, exp_occ_iter, Init()) {

    switch (occur->Occ_kind()) {

      case EXP_OCCURS::OCC_REAL_OCCUR:
        if (occur->Occurrence()->Is_flag_set(CF_OWNED_BY_TEMP)) {
	  BOOL dont_rehash;
	  if (occur->Save_to_temp()) {
	    if (occur->T_ver_owns_coderep())
	      occur->Set_temp_cr(occur->Occurrence());
  
	    dont_rehash = occur->Occurs_as_hoisted() ||
			  occur->T_ver_owns_coderep();
	    Etable()->No_replace(occur, dont_rehash);
	  }
	  else if (occur->Delete_comp()) {
	    if (!occur->Occurs_as_hoisted()) {
	      tos = occur->Def_occur();
	      if (! tos->T_ver_owns_coderep())
	        Etable()->No_replace(occur, FALSE);
	    }
	  }
	  else Etable()->No_replace(occur, FALSE);
	}
	break;

      case EXP_OCCURS::OCC_PHI_PRED_OCCUR:
	break;

      case EXP_OCCURS::OCC_PHI_OCCUR:
	{
	  EXP_PHI *exp_phi = occur->Exp_phi();
	  if (exp_phi->Will_b_avail()) {
	    if (!exp_phi->Identity()) {
              if (occur->Occurrence()->Is_flag_set(CF_OWNED_BY_TEMP)) {
	        if (occur->T_ver_owns_coderep()) 
	          occur->Set_temp_cr(occur->Occurrence());
		Etable()->No_replace(occur, TRUE);
	      }
	    } // else
	  } // if (exp_phi->Will_b_avail())
	}
	break;

      case EXP_OCCURS::OCC_EXIT_OCCUR:
	break;

      default:
	Is_True(FALSE, ("CSE::Do_cse_pass_1, unknown occurrence kind: %d",
			occur->Occ_kind()));
	break;
      }
    }
}

// ======================================================================
// Do_cse_pass_2 - second pass of main routine for this file that generates
// code to save expressions to temps and later reload them; it also generates
// variable phi's for the temps introduced.
// ======================================================================
void
CSE::Do_cse_pass_2(void)
{
  // Iterate over the occurrences list in DPO
  EXP_ALL_OCCURS_ITER exp_occ_iter(_worklist->Real_occurs().Head(),
				   NULL, /* no LFTR */
				   _worklist->Phi_occurs().Head(),
				   _worklist->Phi_pred_occurs().Head(),
				   _etable->Exit_occurs().Head());
  
  EXP_OCCURS *occur, *tos;
  CODEREP *tempcr;
  STMTREP *savestmt;

  FOR_ALL_NODE(occur, exp_occ_iter, Init()) {

    switch (occur->Occ_kind()) {

      case EXP_OCCURS::OCC_REAL_OCCUR:
	if (occur->Sunk_lvalue()) {
	  if (occur->Occurs_as_lvalue()) {
	    // sunk def (LPRE only), do nothing (SPRE will delete it later
	  }
	  else { 
	    // insert the sunk def before current use 
	    occur->Set_temp_cr(NULL);	// overlap with non-null _def_occur
	    occur->Get_temp_cr(_worklist, _etable->Htable());
	    Save_shrunk_lr_def(occur);
	    // rehash tree containing this expr being replaced by the temp
	    Etable()->Replace_by_temp(occur, occur->Temp_cr());
	    _worklist->Inc_save_count();
	  }
	}
	else if (occur->Save_to_temp()) {
	  // save the real occurrence
	  if ( occur->Injured_occ() ) {
	    // this must be an injury we can ignore because the def
	    // is null (probably not down-safe).
	    Is_Trace( Tracing(),
	      (TFile, "CSE::Do_cse_pass_2: ignore 'saved' injured"
		      " real in bb:%d\n", occur->Bb()->Id()) );
	  }

	  if (! occur->T_ver_owns_coderep()) 
	    occur->Get_temp_cr(_worklist, _etable->Htable());
	  else 
	    Change_expr_to_temp(occur->Temp_cr(), _worklist, _etable->Htable());

	  if (occur->Occurs_as_lvalue()) {
	    Is_True(_worklist->Exp()->Kind() == CK_IVAR || _worklist->Exp()->Kind() == CK_VAR,
		    ("CSE::Do_cse_pass_2: only ivars/vars can be marked OCC_AS_L_VALUE"));
	    Save_occurrence_as_l_value(occur);
	    _worklist->Inc_save_count();
	  } else if (occur->Occurs_as_hoisted()) {
	    Save_hoisted_occurrence(occur);
	    _worklist->Inc_save_count();
	  }
	  else {
	    Save_real_occurrence(occur);
	    if (! occur->T_ver_owns_coderep()) {
	      // rehash tree containing this expr being replaced by the temp
	      Etable()->Replace_by_temp(occur, occur->Temp_cr());
	    }
	    else {
	      // no need to rehash tree because the expr node has already been
	      // changed to the temp 
	      _etable->Find_new_1st_order_exprs(occur, occur->Temp_cr());
	    }
	    _worklist->Inc_save_count();

            // set the statement for the occur to the savestmt
            occur->Set_enclose_stmt(occur->Temp_cr()->Defstmt());

	    // If the occurrence we just saved was a comparison, we
	    // will have deleted any corresponding COMP_OCCUR nodes
	    // during Replace_by_temp or Find_new_1st_order_exprs, and
	    // we need to introduce a new one referring to the
	    // inserted statement that saves the expression value to
	    // the temp.
	  }
	} // if (occur->Save_to_temp())
	else if (occur->Delete_comp()) {
	  if (!occur->Occurs_as_hoisted()) {
	    tos = occur->Def_occur();

	    // reload the real occurrence
	    _worklist->Inc_reload_count();
	    tempcr = tos->Temp_cr();
	    Is_True(tempcr != NULL, ("CSE::Do_cse_pass_2: wrong logic"));
	    if (tos->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR) {
	      // real occurrence, same version as top of stack
	      Is_True(tempcr->Defstmt() != NULL,
		      ("CSE::Do_cse_pass_2: at a reload location, temp cr has no Defstmt"));
	    
	      if ( occur->Injured_occ() ) {
		// strength-reduced injured expr, so apply medication
		if (occur->Occurrence()->Kind() == CK_VAR)
		  // injury already repaired (only happening if temp owns cr)
		  tempcr = occur->Occurrence(); 
	        else tempcr = Repair_injury_real_real( tos, occur, tempcr, 
						  tos->T_ver_owns_coderep());
	      }
	    } else {
	      Is_True(tos->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR,
		      ("CSE::Do_cse_pass_2: version that is same but not defined by "
		       "real occurrence must be defined by phi occurrence"));

	      if ( occur->Injured_occ() ) {
		// strength-reduced injured expr, so apply medication
		if (occur->Occurrence()->Kind() == CK_VAR)
		  // injury already repaired (only happening if temp owns cr)
		  tempcr = occur->Occurrence(); 
	        else tempcr = Repair_injury_phi_real( tos, occur, tempcr, 
						 tos->T_ver_owns_coderep());
	      }
	    }

	    if (Etable()->Pre_kind() == PK_LPRE && 
		(occur->Stmt()->Op() == OPC_TRUEBR || 
		 occur->Stmt()->Op() == OPC_FALSEBR) &&
		occur->Bb()->Loop() != NULL) {
	      BB_NODE *end_bb = occur->Bb()->Loop()->End();
	      if (end_bb) {
		STMTREP *term_test = end_bb->Branch_stmtrep();
		if (term_test != NULL &&
		    occur->Bb() == end_bb && 
		    occur->Stmt() == term_test &&
		    (occur->Occurrence() == term_test->Rhs() ||
		     (term_test->Rhs()->Kind() == CK_OP && 
                      occur->Occurrence() == term_test->Rhs()->Opnd(0)) ))
		  occur->Bb()->Loop()->Set_iv_replacement(tempcr);
	      }
	    }

	    if (!tos->T_ver_owns_coderep()) {
	      // rehash tree containing this expr being replaced by the temp
	      Etable()->Replace_by_temp(occur, tempcr);
	    }
	    else {
	      // no need to rehash tree because the expr node has already been
	      // changed to the temp 
	      _etable->Find_new_1st_order_exprs(occur, tempcr);
	    }
	  }
	} // else if (occur->Delete_comp())
	break;

      case EXP_OCCURS::OCC_PHI_PRED_OCCUR:
	{
	  if (occur->Save_to_temp()) {
	    Is_True(occur->Def_occur() == NULL, 
		("CSE::Do_cse_pass_2: at a save location, version's OCCUR not NULL"));
	    if (occur->Occurrence()->Is_flag_set(CF_OWNED_BY_TEMP)) {
	      // T_ver_owns_coderep() must not be true
	      Etable()->No_replace(occur, TRUE);
	    }

	    // save the inserted occurrence
	    occur->Get_temp_cr(_worklist, _etable->Htable());
	    Save_real_occurrence(occur);
	  }
	  else if (occur->Sunk_lvalue()) { // insert sunk def at this BB
	    occur->Get_temp_cr(_worklist, _etable->Htable());
	    Save_shrunk_lr_def(occur);
	    _worklist->Inc_save_count();
	  }

	  // process exp_phi operands
	  BB_LIST_ITER bb_iter;
	  BB_NODE *phi_bb;
	  BB_NODE *pred_bb = occur->Bb();

	  tos = NULL;

	  FOR_ALL_ELEM (phi_bb, bb_iter, Init(pred_bb->Succ())) {
	    if (phi_bb->Dom_dfs_id() > pred_bb->Dom_dfs_id())
	      continue;
	    EXP_PHI *exp_phi = _etable->Lookup_exp_phi(phi_bb,
						     _worklist->Exp());
	    if (exp_phi != NULL) {
	      INT32 opnd_num = phi_bb->Pred()->Pos(pred_bb);
	      if (exp_phi->Opnd(opnd_num) != NULL &&
		  exp_phi->Will_b_avail()) {
		// don't use Def_occur for phi operands
		Is_True(tos == NULL ||
			tos == exp_phi->Opnd(opnd_num),
			("CSE::Do_cse_pass_2: TOS disagreement"));
		tos = exp_phi->Opnd(opnd_num);
		tempcr = NULL;
		if (tos->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR ||
		    tos->Occ_kind() == EXP_OCCURS::OCC_PHI_PRED_OCCUR) {
		  // real occurrence, same version as top of stack
		  Is_True(tos->Temp_cr()->Defstmt() != NULL,
			  ("CSE::Do_cse_pass_2: at a phi_pred location, temp cr "
			   "has no Defstmt"));

		  if ( exp_phi->Injured(opnd_num) ) {
		    // strength-reduced injured expr, so apply medication
		    Is_True(!tos->Injured_occ(),
			    ("CSE::Do_cse_pass_2: avail_def's "
			     "injury flag should have been reset"));
		    if (occur->Occurrence()->Kind() == CK_VAR)
		      // injury already repaired (here only if temp owns cr)
		      tempcr = occur->Occurrence(); 
		    else tempcr = Repair_injury_real_phi_opnd( tos, occur, tos->Temp_cr() );
		  }
		}
		else {
		  Is_True(tos->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR,
			  ("CSE::Do_cse_pass_2: version that is same but not "
			   "defined by real occurrence must be defined "
			   "by phi occurrence"));
		  Is_True(tos->Temp_cr() != NULL,
			  ("CSE::Do_cse_pass_2: wrong logic"));

		  if ( exp_phi->Injured(opnd_num) ) {
		    // strength-reduced injured expr, so apply medication
		    if (occur->Occurrence()->Kind() == CK_VAR)
		      // injury already repaired (here only if temp owns cr)
		      tempcr = occur->Occurrence(); 
		    else tempcr = Repair_injury_phi_phi_opnd( tos, occur, tos->Temp_cr() );
		  }
		}

		// set temp phi operands
		if (!exp_phi->Identity()) {
		  // need to set corresponding opnd of the variable phi
		  PHI_NODE *tempphi = _etable->Lookup_var_phi(
				   phi_bb, tos->Temp_cr()->Aux_id());
		  if ( exp_phi->Injured(opnd_num) ) {
		    Is_True( tempcr != NULL,
		      ("CSE::Do_cse_pass_2: no injured tempcr"));
		    tempphi->Set_opnd(opnd_num, tempcr);
		  }
		  else if (tempphi->Opnd(opnd_num) == 0) 
		    tempphi->Set_opnd(opnd_num, tos->Temp_cr());
		  else Is_True(tempphi->OPND(opnd_num) == tos->Temp_cr(),
			   ("CSE::Do_cse_pass_2: contradicatory temp phi operand"));
		}
	      } // if (exp_phi->Opnd(opnd_num) != NULL && ...
	    } // if (exp_phi != NULL)
	  } // FOR_ALL_ELEM (phi_bb, bb_iter, Init(pred_bb->Succ()))
	  break;
	}

      case EXP_OCCURS::OCC_PHI_OCCUR:
	{
	  EXP_PHI *exp_phi = occur->Exp_phi();
	  if (// exp_phi->Is_live() && // need to keep dead phi around
	      exp_phi->Will_b_avail()) {
	    if (!exp_phi->Identity()) {
	      if (!occur->T_ver_owns_coderep()) {
	        Is_True(occur->Temp_cr() == NULL,
		        ("CSE::Do_cse_pass_1: at expression phi, temp_cr of result not NULL"));
	        occur->Get_temp_cr(_worklist, _etable->Htable());
	      }
	      else {
	        Change_expr_to_temp(occur->Temp_cr(), _worklist, _etable->Htable());
	      }

	      // insert a phi for the temporary
	      PHI_NODE *tempphi = exp_phi->Bb()->Phi_list()->
		New_phi_node(_worklist->Preg(), 
			     _etable->Htable()->Ssa()->Mem_pool(), 
			     exp_phi->Bb());
	      if (exp_phi->Is_live()) tempphi->Set_live();
	      tempphi->Set_res_is_cr();
	      tempphi->Set_incomplete();  // might not have phi in its DF+
	      tempphi->Set_result(exp_phi->Result()->Temp_cr());
	      for (INT32 i = 0; i < tempphi->Size(); i++) {
	        if (exp_phi->Pred(i)->Bb()->Dom_dfs_id() >=
		    exp_phi->Bb()->Dom_dfs_id())
		  // It must be the case that we haven't seen the phi
		  // pred corresponding to this operand yet, so delay
		  // until when we process that phi-pred occurrence node
		  continue;
		tos = exp_phi->Opnd(i);
		Is_True(tos != NULL, ("CSE::Do_cse_pass_2: NULL exp phi operand"));
		if ( exp_phi->Injured(i) ) {
		  if (tos->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR ||
		      tos->Occ_kind() == EXP_OCCURS::OCC_PHI_PRED_OCCUR) {
		    if (exp_phi->Pred(i)->Occurrence()->Kind() == CK_VAR)
		      // injury already repaired (here only if temp owns cr)
		      tempcr = exp_phi->Pred(i)->Occurrence();
		    else tempcr = Repair_injury_real_phi_opnd( tos, exp_phi->Pred(i),
							  tos->Temp_cr() );
		  }
		  else {
		    Is_True(tos->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR,
			    ("CSE::Do_cse_pass_2: version that is same but "
			     "not defined by real occurrence must be defined "
			     "by phi occurrence"));
		    if (exp_phi->Pred(i)->Occurrence()->Kind() == CK_VAR)
		      // injury already repaired (here only if temp owns cr)
		      tempcr = exp_phi->Pred(i)->Occurrence();
		    else tempcr = Repair_injury_phi_phi_opnd( tos, exp_phi->Pred(i),
							 tos->Temp_cr() );
		  }
		  tempphi->Set_opnd(i, tempcr);
	        }
		else {
		  Is_True(tos->Temp_cr() != NULL,
			  ("CSE::Do_cse_pass_2: temp_cr not set for exp phi operand"));
		  tempphi->Set_opnd(i, tos->Temp_cr());
		}
	      }

	      // put in the phi hash table
	      _etable->Htable()->Enter_var_phi_hash(tempphi);
	      occur->Temp_cr()->Set_flag(CF_DEF_BY_PHI);
	      occur->Temp_cr()->Set_defphi(tempphi);
	      _worklist->Inc_temp_phi_count();	// statistics
	    }
	    else {
	      // exp_phi may be identical to an injured version of
	      // something. Make sure we get its temp_cr right. There
	      // are two cases: If the phi operands are injured, we
	      // must generate the required injury repair. If the phi
	      // operands are uninjured but the phi's identity is
	      // injured, we retrieve the Temp_cr() from an injured
	      // phi operand's definition. All of the operands'
	      // Temp_cr()'s must be the same. If the phi's identity
	      // is uninjured, we set this phi's Temp_cr() to that of
	      // whatever this phi is identical to.
	      if (exp_phi->Identity_injured()) {
		// The phi's identity is injured.
		if (exp_phi->Any_opnd_injured()) {
		  if (occur->Occurrence()->Kind() == CK_VAR) {
		    // injury already repaired (here only if temp owns cr)
		    tempcr = exp_phi->Result()->Occurrence();
		  }
		  else {
		    INT32 i;
		    // Find an (the) injured operand and repair the injury.
		    for (i = 0; i < exp_phi->Opnd_count(); i++) {
		      if (exp_phi->Injured(i)) {
			break;
		      }
		    }
		    FmtAssert(i < exp_phi->Opnd_count(),
			      ("CSE::Do_cse_pass_2: cannot find "
			       "injured operand"));
		    tos = exp_phi->Opnd(i);

		    Is_Trace(Tracing(), (TFile, "Found inj opnd; Tos is :\n"));
		    Is_Trace_cmd(Tracing(), tos->Print(TFile));

		    if (tos->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR ||
			tos->Occ_kind() == EXP_OCCURS::OCC_PHI_PRED_OCCUR) {
		      tempcr = Repair_injury_real_phi_opnd(tos,
							   exp_phi->Pred(i),
							   tos->Temp_cr());
		    }
		    else {
		      Is_True(tos->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR,
			      ("CSE::Do_cse_pass_2: same version "
			       "not def by real must be def by phi"));
		      tempcr = Repair_injury_phi_phi_opnd(tos,
							  exp_phi->Pred(i),
							  tos->Temp_cr());
		    }
		  }
		}
		else {
		  INT32 i;
		  tempcr = NULL;
		  for (i = 0; i < exp_phi->Opnd_count(); i++) {
		    Is_True(tempcr == NULL ||
			    exp_phi->Opnd(i)->Temp_cr() == NULL ||
			    exp_phi->Opnd(i)->Temp_cr() == tempcr,
			    ("CSE::Do_cse_pass_2: operands of identity phi "
			     "must have same temp"));
		    // As soon as we find a non-NULL opnd temp, stop checking.
		    tempcr = exp_phi->Opnd(i)->Temp_cr();
		    if (tempcr != NULL) {
		      break;
		    }
		  }
		  FmtAssert(tempcr != NULL,
			    ("CSE::Do_cse_pass_2: Can't find injured "
			     "opnd w/known identity"));

		  // A loop for assertion checking. We should optimize
		  // this loop away without Is_True_On.
		  for (i = 0; i < exp_phi->Opnd_count(); i++) {
		    Is_True(exp_phi->Opnd(i)->Temp_cr() == NULL ||
			    exp_phi->Opnd(i)->Temp_cr() == tempcr,
			    ("CSE::Do_cse_pass_2: operands of identity phi "
			     "must have same temp"));
		  }
		}
		Is_True(tempcr != NULL, ("CSE::Do_cse_pass_2: "
					 "injured tempcr must not be NULL"));
		occur->Set_temp_cr(tempcr);
	      }
	      else {
		// No injury encountered.
		occur->Set_temp_cr(exp_phi->Identical_to()->Temp_cr());
	      }
	      Is_True(occur->Temp_cr() != NULL,
		      ("CSE::Do_cse_pass_2: temp identity must be known; "
		       "tempcr=0x%lx", tempcr));
	    }
	  } // if (exp_phi->Will_b_avail())
	}
	break;

      case EXP_OCCURS::OCC_EXIT_OCCUR:
	break;

      // note: LFTR comp occurs are done in the next loop
      default:
	Is_True(FALSE, ("CSE::Do_cse_pass_2, unknown occurrence kind: %d",
			occur->Occ_kind()));
	break;
      }
    }

  // now do all the LFTR comparisons
  if (_etable->Lftr()->Lftr_on() && !_worklist->No_lftr()) {
    EXP_OCCURS_PAIR *comp_list = _etable->Lftr()->Exp_hash(_worklist);
    if (!comp_list->Is_Empty()) {
      Is_Trace(_etable->Lftr()->Trace(),
	       (TFile,"\n====== 2nd Step 6 loop: LFTR ======\n"));
      EXP_ALL_REAL_ITER exp_iter(NULL, comp_list);
      FOR_ALL_NODE(occur, exp_iter, Init()) {

	Is_True(occur->Occ_kind() == EXP_OCCURS::OCC_COMP_OCCUR,
		("CSE::Do_cse_no_stack, illegal occur kind"));
	_etable->Lftr()->
	  Replace_comparison(occur,
			     WOPT_Enable_New_SR &&
			     Str_red()->Candidate_opc(_worklist->
						      Exp()->Op()));
      }
    }
  }

  // statistics
  _etable->Inc_cse_reloads(_worklist->Reload_count());
  _etable->Inc_cse_saves(_worklist->Save_count());
  _etable->Inc_inserted_saves(_worklist->Insert_count());
  _etable->Inc_temp_phis(_worklist->Temp_phi_count());
  _etable->Inc_temp_owners(_worklist->Temp_owner_count());
}

// =====================================================================
// Constructor
// =====================================================================
CSE::CSE(ETABLE *etable, EXP_WORKLST *worklist)
{
  OPT_POOL_Initialize(&_mempool, "pre cse mempool", FALSE, -1);
  OPT_POOL_Push(&_mempool, -1);

  _etable = etable;
  _worklist = worklist;
  _str_red = etable->Str_red();
  _tracing = etable->Tracing();
}

// =====================================================================
// Destructor
// =====================================================================
CSE::~CSE(void)
{
  OPT_POOL_Pop(&_mempool, -1);
  OPT_POOL_Delete(&_mempool, -1);
}

// =====================================================================
// SSA PRE Step 6
// =====================================================================
void
EXP_WORKLST::Generate_save_reload(ETABLE *etable)
{
  CSE cse(etable, this);
  if (WOPT_Enable_Avoid_Rehash) {
    cse.Do_cse_pass_1();
    if (_exp->Is_flag_set(CF_OWNED_BY_TEMP)) {
      // make sure it will not point to temp after call to Fix_no_rehash_temps
      CODEREP *x = _exp;
      // increase usecnt of kids because Rehash will decrement them
      switch (x->Kind()) {
      case CK_VAR:
	break;
      case CK_IVAR:
	x->Ilod_base()->IncUsecnt();
	if (x->Opr() == OPR_MLOAD)
	  x->Mload_size()->IncUsecnt();
	else if (x->Opr() == OPR_ILOADX)
	  x->Index()->IncUsecnt();
	break;
      default: // CK_OP
	for (INT32 i = 0; i < x->Kid_count(); i++)
	  x->Opnd(i)->IncUsecnt();
      }
      _exp = etable->Rehash_exp(x, etable->Gvn(x));
      _exp->Set_e_num(x->E_num());
      _exp->DecUsecnt();	// because Rehash has increased usecnt by 1
    }
  }
  cse.Do_cse_pass_2();

  // Now all the optimizations that assume inserted occurrences of the
  // expression haven't changed in structure are done. Go ahead and
  // copy-propagate/fold them, and put resulting new expressions into
  // the worklist.
  //
  // Pass this for assertion checking.
  etable->Perform_deferred_ocopy_and_get_new_exprs(this);

  if (etable->Tracing()) {
    fprintf(TFile, "====== After ETABLE::Generate_save_reload ======\n");
    Print(TFile);
  }
}
