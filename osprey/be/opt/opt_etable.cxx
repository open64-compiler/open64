/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_etable.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_etable.cxx,v $
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
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#define opt_etable_CXX	"opt_etable.cxx"

#include "id_map.h"

#include "defs.h"
#include "cxx_memory.h"
#include "glob.h"       // for Cur_PU_Name
#include "const.h"      // for New_Const_Sym

#include "opt_base.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_estr.h"
#include "opt_etable.h"
#include "opt_efinalize.h"
#include "opt_mu_chi.h"
#include "opt_htable.h"
#include "opt_main.h"
#include "opt_ssa.h"
#include "opt_sys.h"
#include "opt_util.h"
#include "tracing.h"
#include "opt_lftr2.h"
#include "config_targ.h"	// needed for Pointer_type
#include "idx_32_set.h"
#include "opt_verify.h"		// Def_before_use
#include "opt_cvtl_rule.h"
#include "opt_fold.h"

#include "data_layout.h"	// for ST_has_formal_preg_num

#ifdef KEY
#include "opt_alias_mgr.h"	// for ALIAS_MANAGER
#endif


const char *pre_kind_name(PRE_KIND kind)
{
  switch (kind) {
  case PK_EPRE:  return "epre";
  case PK_LPRE:  return "lpre";
  case PK_SPRE:  return "spre";
  case PK_VNFRE: return "vnfre";
  }
  return "unknown";
}


//  The singly linked list is to cache all the rehashed expression
//  for a given EXP_OCCUR node.   This linked list should be replaced
//  a direct lookup table if CRs are given unique-id that can be
//  used in the lookup table.
//
class REHASH_CACHE_LIST : public SLIST_NODE {
private:
  CODEREP *_cr;
  CODEREP *_rehash_cr;

  REHASH_CACHE_LIST(const EXP_OCCURS_LIST&);
  REHASH_CACHE_LIST& operator = (const EXP_OCCURS_LIST&);

public:
  REHASH_CACHE_LIST(void)        { }
  REHASH_CACHE_LIST(CODEREP *cr, CODEREP *rehash_cr)
      { _cr = cr; _rehash_cr = rehash_cr; }
  ~REHASH_CACHE_LIST(void)        { }

  DECLARE_SLIST_NODE_CLASS( REHASH_CACHE_LIST )

  CODEREP *Cr(void)        { return _cr; }
  CODEREP *Rehash_cr(void) { return _rehash_cr; }
};


CODEREP*
CODEREP::Skip_opnd(INT16 i) const
{
  Is_True(Kind() == CK_OP, ("CODEREP::Opnd, illegal kind %s", Print_kind()));
  if (Opr() == OPR_PARM)
    return Opnd(i)->Opnd(0);

      
  /* CVTL-RELATED start (performance) */
  if (Opr() == OPR_CVT) {
    INT    cvt_kind;
    OPCODE opc;

    cvt_kind = Need_type_conversion(Dsctyp(), Dtyp(), &opc);//326120
    if (cvt_kind == NOT_AT_ALL)
      return Opnd(i)->Opnd(0);
  }
  /* CVTL-RELATED finish */

  return Opnd(i);
}

void
EXP_PHI::Init(EXP_ID e_num, INT opnd_count, BB_NODE *bb, MEM_POOL *pool)
{
  _e_num = e_num; _opnd_count = opnd_count; _bb = bb;
  _cached_identity_assignment = NULL; _flags = 0;
  _result = NULL;
  Set_uses((USE_LIST_ENTRY *) NULL);
  _vec = (PHI_ELEM*) CXX_NEW_ARRAY(PHI_ELEM, opnd_count, pool);
  BZERO(_vec, sizeof(PHI_ELEM) * opnd_count);
}

void
EXP_PHI::Print(FILE *fp, BOOL show_preds_of_unavail) const
{
  INT i;
  if (Not_down_safe())
    fprintf(fp, "!downsafe; ");
  if (Cant_be_avail())
    fprintf(fp, "Cant be available; ");
  if (Will_b_avail())
    fprintf(fp, "Will be available; ");
  if (Is_live())
    fprintf(fp, "Is live; ");
  if (Stops())
    fprintf(fp, "Stops; ");
  if (Partial_avail())
    fprintf(fp, "Partial avail; ");
  fprintf(fp, "phi_opnds: ");
  for (i = 0; i < Opnd_count(); i++) {
    if (Opnd(i) == NULL) {
      fprintf(fp, "nil");
      if (Has_real_occ(i))
	fprintf(fp, "<has-real-occ>");
    } else {
      fprintf(fp, "%d", Opnd(i)->E_version());
      switch (Opnd(i)->Occ_kind()) {
      case EXP_OCCURS::OCC_REAL_OCCUR:
	fprintf(fp, "<real>");
	break;
      case EXP_OCCURS::OCC_PHI_OCCUR:
	fprintf(fp, "<phi>");
	break;
      case EXP_OCCURS::OCC_PHI_PRED_OCCUR:
	fprintf(fp, "<phi pred>");
	break;
      default:
	fprintf(fp, "<bad>");
	break;
      }
      if (Has_real_occ(i))
	fprintf(fp, "<has-real-occ>");
      if (Delayed_rename(i))
	fprintf(fp, "<delayed>");
      if (Injured(i))
	fprintf(fp, "<injured>");
      if (Opnd_stops(i))
	fprintf(fp, "<stops>");
    }
    if (i < Opnd_count()-1)
      fprintf(fp, ",");
  }
  if (! Reverse_phi()) {
    if (show_preds_of_unavail || Will_b_avail()) {
      fprintf(fp, " phi_preds: ");
      for (i = 0; i < Opnd_count(); i++) {
	if (Pred(i) == NULL)
	  fprintf(fp, "nil");
	else {
	  switch (Pred(i)->Occ_kind()) {
	  case EXP_OCCURS::OCC_PHI_PRED_OCCUR:
	    fprintf(fp, "<phi_pred>%d", Pred(i)->Enclosed_in_bb()->Id());
	    break;
	  case EXP_OCCURS::OCC_REAL_OCCUR:
	    fprintf(fp, "<real>%d", Pred(i)->Enclosed_in_bb()->Id());
	    break;
	  default:
	    fprintf(fp, "<bad>");
	    break;
	  }
	}
	if (i < Opnd_count()-1)
	  fprintf(fp, ",");
      }
    }
  }
  else { // iphi
    fprintf(fp, " iphi_succs: ");
    for (i = 0; i < Opnd_count(); i++) {
      if (Succ(i) == NULL)
        fprintf(fp, "nil");
      else {
        switch (Succ(i)->Occ_kind()) {
        case EXP_OCCURS::OCC_PHI_PRED_OCCUR:
	  fprintf(fp, "<phi_succ>%d", Succ(i)->Enclosed_in_bb()->Id());
	  break;
        case EXP_OCCURS::OCC_REAL_OCCUR:
	  fprintf(fp, "<real>%d", Succ(i)->Enclosed_in_bb()->Id());
	  break;
        default:
	  fprintf(fp, "<bad>");
	  break;
        }
      }
      if (i < Opnd_count()-1)
        fprintf(fp, ",");
    }
  }
  if (!Identity()) {
    fprintf(fp, " required");
  }
  else {
    if (Replacing_occur_known()) {
      fprintf(fp, " identical: ");
      if (Identical_to() != NULL) {
	fprintf(fp, "%d", Identical_to()->E_version());
	if (Identity_injured()) {
	  fprintf(fp, "<inj>");
	}
      }
      else {
	fprintf(fp, "<NULL>");
      }
    }
  }
  fprintf(fp, "\n");
}

// TODO: Implement this function more efficiently by keeping track of
// the transitions of the EPOF_INJURED flag for each
// operand. Currently the only client for this function is SSA
// minimization.
BOOL
EXP_PHI::Any_opnd_injured(void) const
{
  for (INT i = 0; i < Opnd_count(); i++) {
    if (Injured(i)) {
      return TRUE;
    }
  }
  return FALSE;
}

// See if an EXP_PHI at the header (tail) of a loop is an identity
// assignment (i.e., is loop invariant)
BOOL 
EXP_PHI::Identity_assignment(BOOL &seen_real_occur, EXP_PHI *orig_occur, 
			     BOOL forward_pre, STACK<EXP_PHI*> &phi_stack)
{
  Is_True(!I_A_Seen(),("EXP_PHI::Identity_assignment: visited twice"));

  Set_I_A_Seen();
  phi_stack.Push(this);

  Is_True(Result() != NULL,
	  ("EXP_PHI::Identity_assignment: NULL expr phi result"));
  Is_True(Result()->E_version() != EXP_OCCURS::ILLEGAL_E_VERSION,
	  ("EXP_PHI::Identity_assignment: Illegal e-version for EXP_PHI result"));

  if (Opnd_count() == 1) return FALSE;
  
  BB_NODE       *orig_bb = orig_occur->Bb();
  EXP_OCCURS    *opnd;
  EXP_PHI       *opnd_phi;

  if (this == orig_occur) {
    BB_NODE       *bb_pred;
    BB_LIST_ITER   bb_iter;
    INT            which_pred = 0;

    FOR_ALL_ELEM( bb_pred, bb_iter, 
		 Init(forward_pre ? orig_bb->Pred() : orig_bb->Succ()) ) {
      opnd = Opnd(which_pred);
      ++which_pred;

      if ( opnd == NULL ) {
	if ( forward_pre && orig_bb->Dominates( bb_pred ) ||
	    !forward_pre && orig_bb->Postdominates( bb_pred ) )  {
	  return FALSE;
	} else {
	  continue;
	}
      }

      // opnd != NULL
      Is_True(opnd->E_version() != EXP_OCCURS::ILLEGAL_E_VERSION,
	      ("EXP_PHI::Identity_assignment: Illegal e-version for EXP_PHI operand"));
      if ( forward_pre && orig_bb->Dominates( opnd->Bb() ) ||
	  !forward_pre && orig_bb->Postdominates( opnd->Bb() ) ) {
	if (opnd->Occurs_as_lvalue()) {
	  if (Has_real_occ(which_pred-1)) seen_real_occur = TRUE;
	  continue;
	}
	if (opnd->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) {
	  opnd_phi = opnd->Exp_phi();
	  if (opnd_phi->I_A_Seen() ||
	      opnd_phi->Identity_assignment(seen_real_occur, orig_occur,
					    forward_pre, phi_stack)) {
	    if (Has_real_occ(which_pred-1)) seen_real_occur = TRUE;
	    continue;
	  }
	} 
	return FALSE;
      }
    }

    return TRUE;

  } else { // this != orig_occur
    for (INT32 i = 0; i < Opnd_count(); i++) {
      opnd = Opnd(i);
      if (opnd == NULL) return FALSE;
      Is_True(opnd->E_version() != EXP_OCCURS::ILLEGAL_E_VERSION,
	      ("EXP_PHI::Identity_assignment: Illegal e-version for EXP_PHI operand"));

      if ( forward_pre && orig_occur->Bb()->Dominates( opnd->Bb() ) ||
	  !forward_pre && orig_occur->Bb()->Postdominates( opnd->Bb() ) ) {
	if (opnd->Occurs_as_lvalue()) {
	  if (Has_real_occ(i)) seen_real_occur = TRUE;
	  continue;
	}
	if (opnd->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) {
	  EXP_PHI *opnd_phi = opnd->Exp_phi();
	  if (opnd_phi->I_A_Seen() ||
	      opnd_phi->Identity_assignment(seen_real_occur, orig_occur,
					    forward_pre, phi_stack)) {
	    if (Has_real_occ(i)) seen_real_occur = TRUE;
	    continue;
	  }
	} 
      }
      return FALSE;
    }
    return TRUE;
  }
}

BOOL            
EXP_OCCURS::Temp_eligible_as_owner(EXP_WORKLST *worklist) const
{ 
  return (Occurs_as_lvalue() ||
	  (worklist->Exp()->Is_integral_load_store() &&
	   Occurrence()->Is_sign_extd() != worklist->Sign_extd())); 
}

void
EXP_OCCURS::Init(void)
{
  Set_kind(OCC_UNKNOWN_OCCUR);
  Reset_flag();
  Set_e_version(EXP_OCCURS::ILLEGAL_E_VERSION);
  _temp._temp_cr = NULL;  // also clear def_occur as a side effect
  Set_rehash_cost(0);
  Set_Next(NULL);
}

STMTREP*
EXP_OCCURS::Stmt(void) const
{
  Is_True(Occ_kind() == OCC_REAL_OCCUR ||
	  Occ_kind() == OCC_COMP_OCCUR ||
	  (Occ_kind() == OCC_PHI_PRED_OCCUR &&
	   (Inserted_computation() || Sunk_lvalue()) && Encl_stmt_set()),
	  ("EXP_OCCURS::Stmt: Bad occurrence kind"));
  STMTREP *stmt = _enclosed_in._stmt;
  // It is possible to have two consecutive statements with Is_RHS_saved TRUE
  // See pv 800413 for an example.
  while (stmt->Is_RHS_saved()) {
    Is_True(stmt->Prev() && stmt->Prev()->Rhs(),
	    ("EXP_OCCURS::stmt: cannot find the real statement"));
    Is_True(!Occurs_as_hoisted(), 
	    ("EXP_OCCURS::stmt: hoisted occur has no real stmt"));
    stmt = stmt->Prev();
  }
  return stmt;
}

BB_NODE*
EXP_OCCURS::Bb(void) const
{
  switch (Occ_kind()) {
  case OCC_REAL_OCCUR:
    if (Is_flag_set(OCC_HOISTED))
      return _enclosed_in._bb;
    else 
      return Stmt()->Bb();

  case OCC_PHI_OCCUR:
    return Exp_phi()->Bb();

  case OCC_PHI_PRED_OCCUR:
    if (Encl_stmt_set())
      return Stmt()->Bb();
    else
      return _enclosed_in._bb;

  case OCC_COMP_OCCUR:
    return Stmt()->Bb();

  case OCC_EXIT_OCCUR:
    return _enclosed_in._bb;

  default:
    return NULL;
  }
}

// =====================================================================
// Create a new WN to represent the home of a LPRE PREG
// Returns NULL if home too complicated.
// =====================================================================
WN *
CODEREP::Rvi_home_wn( OPT_STAB *opt_stab ) const
{
  WN      *home_wn = NULL;

  switch (Kind()) {
  case CK_VAR:
    if ( ST_sclass(opt_stab->St(Aux_id())) != SCLASS_FORMAL_REF ) {
      // Partial fix 653956: change IDNAME into LDID to carry
      // size information.
      // home_wn = WN_CreateIdname(Offset(),opt_stab->St(Aux_id()));
      home_wn = WN_CreateLdid(OPR_LDID, Dtyp(), Dsctyp(),
			      Offset(), opt_stab->St(Aux_id()), Lod_ty(),
			      Field_id());
			      
      if (opt_stab->Bit_size (Aux_id()) > 0) {
	WN_change_operator (home_wn, OPR_LDBITS);
	WN_set_bit_offset_size (home_wn, Bit_offset (), Bit_size ());
      }
    }
    break;
  case CK_LDA:
    if ( ST_sclass(opt_stab->St(Lda_aux_id())) == SCLASS_FORMAL_REF ) {
      if (!ST_has_formal_preg_num(Lda_base_st())) {
	// Partial fix 653956: change IDNAME into LDID to carry
	// size information.
	// home_wn = WN_CreateIdname(Offset(), Lda_base_st());
	home_wn = WN_CreateLdid(OPR_LDID, Pointer_type, Pointer_type,
				Offset(), Lda_base_st(),
				Lda_ty());
      }
    } else {
      home_wn = WN_CreateLda(OPR_LDA, Dtyp(), MTYPE_V, Offset(), Lda_ty(),
			     Lda_base_st()); 
    }
    break;
  case CK_RCONST:
    home_wn = WN_CreateConst (OPR_CONST, Dtyp(), MTYPE_V,
			      ST_st_idx (Const_id())); 
    break;
  case CK_CONST:
    home_wn = WN_CreateIntconst (OPR_INTCONST, Dtyp(), MTYPE_V, Const_val());
    break;
#ifdef TARG_NVISA // other targets don't know how to rematerialize these
  case CK_OP:
    // add ability to create preg home for expression,
    // so can later still find the base of the temporary.
    switch (Opr()) {
    case OPR_ADD:
    case OPR_SUB:
    case OPR_MPY:
	{
	WN *kid0 = Opnd(0)->Rvi_home_wn(opt_stab);
	WN *kid1 = Opnd(1)->Rvi_home_wn(opt_stab);
	if (kid0 && kid1) {
	  home_wn = WN_CreateExp2 (Opr(), Dtyp(), MTYPE_V, kid0, kid1);
	}
	}
	break;
    case OPR_CVT:
	{
	WN *kid = Opnd(0)->Rvi_home_wn(opt_stab);
	if (kid) {
	  home_wn = WN_CreateExp1 (Opr(), Dtyp(), Dsctyp(), kid);
	}
	}
	break;
    }
    break;
  case CK_IVAR:
    if (Opr() == OPR_ILOAD && Ilod_base()) {
      WN *kid0 = Ilod_base()->Rvi_home_wn(opt_stab);
      if (kid0) {
        home_wn = WN_CreateIload (Opr(), Dtyp(), Dsctyp(),
          Offset(), Ilod_ty(), Ilod_base_ty(), kid0);
      }
    }
    break;
#endif
  default:
    Is_True(FALSE, ("CODEREP::Rvi_home_wn: unexpected CR kind"));
  }

  return home_wn;
}

//======================================================================
// if needed, allocate the temp_cr node; set the _temp_cr field;
// wk is the worklist node to which this occurrence belongs
//======================================================================

CODEREP	*
EXP_OCCURS::Get_temp_cr(EXP_WORKLST *wk, CODEMAP *htable)
{
  if (Temp_cr()) return Temp_cr();

  CODEREP *exp = wk->Exp();
  MTYPE dtyp = exp->Dtyp();
  INT vsize = 0;
  INT signess = 0;

  if (wk->Preg()) {
    vsize = htable->Sym()->Aux_stab_entry(wk->Preg())->Value_size();
  } else if ( WOPT_Enable_Min_Type ) {
    // get the minimum result size
    vsize = Actual_data_size(exp, htable->Sym(), signess);
    if (wk->Pre_kind() == PK_VNFRE && vsize != MTYPE_size_min(dtyp))
    {
       vsize = MTYPE_size_min(dtyp);  // Minimizes converts for VNFRE
       Is_True(vsize >= 32, 
	       ("Unexpected size of result type in EXP_OCCURS::Get_temp_cr"));
    }
// ia32,x86,ia64,nvisa,sl all don't want to do this cause it can cause 
// inconsistent sizes for the same symbol
#if defined(TARG_IA32) || defined(TARG_X8664) || defined(TARG_NVISA) || defined(TARG_SL)
#elif !defined(TARG_IA64)
    // ia64 has similar problem (inconsistent sizes for the same symbol)
    // -- OSP_185
    if (vsize <= 32 && dtyp == MTYPE_I8 && (signess & SIGN_1_EXTD)) 
      dtyp = MTYPE_I4;
#endif
  }
  
  if (wk->Preg() == 0) {
    WN *home_wn = NULL;
#ifdef TARG_NVISA // only do for nvisa till other targets learn rematerialize
    home_wn = exp->Rvi_home_wn(htable->Sym());
#endif
    switch (wk->Pre_kind()) {
    case PK_VNFRE:
      {
	 // This is similar to the EPRE case, but we do not set sign extension
	 // specially for loads.
	 // 
#ifndef TARG_NVISA
        AUX_ID          id = htable->Sym()->Create_preg(dtyp);
#else
        AUX_ID id = htable->Sym()->Create_preg(dtyp, "vnfre_cst", home_wn);
#endif
	AUX_STAB_ENTRY *aux_preg = htable->Sym()->Aux_stab_entry(id);
	ADDRESSABILITY  addressable = 
	   exp->Check_if_result_is_address(htable->Sym());

	aux_preg->Set_LPRE_VNFRE_temp();
        wk->Set_preg(id);
        aux_preg->Set_value_size(vsize);

	// at most 1 of following can be true
	if (addressable == ADDRESSABILITY_IS_ADDRESS)
	  aux_preg->Set_is_address();
	else if (addressable == ADDRESSABILITY_NOT_ADDRESS)
	  aux_preg->Set_not_address();
	// set the sign extension flag
	if (signess & SIGN_1_EXTD) aux_preg->Set_sign_extd();
	if (signess & SIGN_0_EXTD) aux_preg->Set_zero_extd();
      }
      break;

    case PK_EPRE:
      {
#ifndef TARG_NVISA
        AUX_ID id = htable->Sym()->Create_preg(dtyp);
#else
        AUX_ID id = htable->Sym()->Create_preg(dtyp, "epre_cst", home_wn);
#endif
	AUX_STAB_ENTRY *aux_preg = htable->Sym()->Aux_stab_entry(id);
	ADDRESSABILITY addressable = Occurrence()->Check_if_result_is_address(htable->Sym());
        wk->Set_preg(id);
	aux_preg->Set_EPRE_temp();
        aux_preg->Set_value_size(vsize);
	// at most 1 of following can be true
	if (addressable == ADDRESSABILITY_IS_ADDRESS)
	  aux_preg->Set_is_address();
	else if (addressable == ADDRESSABILITY_NOT_ADDRESS)
	  aux_preg->Set_not_address();

	// set the sign extension flag
	if (exp->Is_integral_load_store()) {
	  if (wk->Sign_extd()) 
	    aux_preg->Set_sign_extd();
	  else
	    aux_preg->Set_zero_extd();
	} else {
	  if (signess & SIGN_1_EXTD) aux_preg->Set_sign_extd();
	  if (signess & SIGN_0_EXTD) aux_preg->Set_zero_extd();
	}
        break;
      }
    case PK_LPRE:
      {
#ifndef TARG_NVISA // done above
	WN *home_wn = exp->Rvi_home_wn(htable->Sym());
#endif
	if (inCODEKIND(exp->Kind(), CK_LDA|CK_RCONST|CK_CONST)) {
	  wk->Set_preg(htable->Sym()->Create_preg(dtyp, "lpre_cst", home_wn));
	  AUX_STAB_ENTRY *aux_preg = htable->Sym()->Aux_stab_entry(wk->Preg());
	  aux_preg->Set_LPRE_VNFRE_temp();
	  aux_preg->Set_value_size(vsize);
	  if (signess & SIGN_1_EXTD) aux_preg->Set_sign_extd();
	  if (signess & SIGN_0_EXTD) aux_preg->Set_zero_extd();
	} else {
#ifdef KEY // Bug 9821: Set the alias map id.
	  if (home_wn)
	    htable->Sym()->Alias_Mgr()->Gen_alias_id (home_wn,
	                                       exp->Points_to(htable->Sym()));
#endif
	  // Set the home location for the preg
	  AUX_ID aux_id = exp->Aux_id();
	  const char *aux_name = htable->Sym()->Aux_stab_entry(aux_id)->St_name();
	  wk->Set_preg(htable->Sym()->Create_preg(dtyp, aux_name, home_wn));
	  
	  // Note that the call to Create_preg() may realloc the AUX_STAB_ENTRY
	  // nodes, hence we cannot put the aux_entry into temporary until
	  // here.
	  //
	  AUX_STAB_ENTRY *aux_entry = htable->Sym()->Aux_stab_entry(aux_id);
	  AUX_STAB_ENTRY *aux_preg = htable->Sym()->Aux_stab_entry(wk->Preg());

	  Is_True(aux_entry->Home_sym() == 0, 
		  ("EXP_OCCURS::Get_temp_cr: variable already has home PREG"));
	  aux_entry->Set_home_sym(wk->Preg());

	  aux_preg->Set_home_sym(aux_id);
	  aux_preg->Set_LPRE_VNFRE_temp();
	  aux_preg->Set_value_size(vsize);
	  if (wk->Sign_extd()) 
	    aux_preg->Set_sign_extd();
	  else
	    aux_preg->Set_zero_extd();

	  if (wk->Sign_extd())
	    aux_preg->Set_LPRE_sign_extd();
	}
	break;
      }
    default:
      Is_True(FALSE, ("EXP_OCCURS::Get_temp_cr: wrong PRE_KIND."));
    }
  }
  
  // At this point, we require a new Temp_cr().
  Set_temp_cr(htable->Add_def(wk->Preg(), wk->Cur_e_version(), NULL, 
			      dtyp, dtyp, htable->Sym()->St_ofst(wk->Preg()), 
#ifdef KEY
		      Occurrence()->Kind() == CK_VAR ? Occurrence()->Lod_ty() :
#endif
			      ST_type(MTYPE_To_PREG(dtyp)), 
			      0, TRUE));
  wk->New_e_version();

  if (wk->Pre_kind() == PK_VNFRE)
     VNFRE::add_valnum(Temp_cr(), wk->E_num()/*value number*/);

  return Temp_cr();
}


CODEREP	*
ETABLE::New_temp_cr(MTYPE dtype, ADDRESSABILITY addressable, CODEREP *rhs)
{
   // Simplified version of EXP_OCCURS::Get_temp_cr(), which creates a
   // temporary of exactly the type specified, updates the htable, but
   // does not update worklists.  Some other differences  a user of this
   // subroutine ought to be aware of:
   //
   // Note (1) that we do not try to minimize the type w.r.t. a saved 
   // expression when WOPT_Enable_Min_Type==TRUE, so the user must 
   // take care to do this if relevant.
   //
   // Note (2) that we do not give this temporary register a unique
   // version as we do for temporaries generated w.r.t. an occurrence
   // list.  Instead we always give the temporary the smallest legal
   // version number.
   //
   // Note (3) that we rely on any VNFRE user of this routine to update
   // value numbering for the returned coderep independently.
   //
   // Note (4) that we could implement EXP_OCCURS::Get_temp_cr() with a
   // call to this routine, and then set the worklist and version
   // attributes after the call.
   //
   // Change:  pass rhs so can get preg home info.
   //
   const INT32 init_version = EXP_OCCURS::ILLEGAL_E_VERSION + 1;
   INT32       vsize = MTYPE_size_min(dtype);

   Is_True(vsize >= 32, 
	   ("Unexpected size of result type in EXP_OCCURS::New_temp_cr"));

#ifdef TARG_NVISA
   WN *home_wn = rhs->Rvi_home_wn(Htable()->Sym());
   AUX_ID          id = Htable()->Sym()->Create_preg(dtype, "new_cst", home_wn);
#else
   AUX_ID          id = Htable()->Sym()->Create_preg(dtype);
#endif
   AUX_STAB_ENTRY *aux_preg = Htable()->Sym()->Aux_stab_entry(id);

   if (Pre_kind() == PK_LPRE || Pre_kind() == PK_VNFRE)
     aux_preg->Set_LPRE_VNFRE_temp();
   else aux_preg->Set_EPRE_temp();
   aux_preg->Set_value_size(vsize);

   // at most 1 of following can be true
   //
   if (addressable == ADDRESSABILITY_IS_ADDRESS)
      aux_preg->Set_is_address();
   else if (addressable == ADDRESSABILITY_NOT_ADDRESS)
      aux_preg->Set_not_address();

   if (MTYPE_is_signed(dtype)) aux_preg->Set_sign_extd();
   if (MTYPE_is_unsigned(dtype)) aux_preg->Set_zero_extd();

   // Return a coderep for the symbol coderep.
   //
   return Htable()->Add_def(id, init_version, NULL, 
			    dtype, dtype, Htable()->Sym()->St_ofst(id), 
			    ST_type(MTYPE_To_PREG(dtype)), 0, TRUE);
} // ETABLE::New_temp_cr


//======================================================================
// Test if 'this' is a descendant of 'anc' in the Dominator Tree(DT)
//======================================================================

BOOL
EXP_OCCURS::Is_DT_descendant_of( EXP_OCCURS *anc )
{
  Warn_todo("EXP_OCCURS::Is_DT_descendant_of: not implemented");
  return FALSE;
}

BOOL
STMTREP::Stmt_order_less_or_equal(const STMTREP *const x) const
{
  Is_True(Stmt_id() != INVALID_STMT_ID,
          ("EXP_OCCURS::Stmt_order_less_or_equal: Stmt_id not set"));
  Is_True(x->Stmt_id() != INVALID_STMT_ID,
          ("EXP_OCCURS::Stmt_order_less_or_equal: Stmt_id not set"));
  if (Stmt_id() < x->Stmt_id())
    return TRUE;
  else if (Stmt_id() > x->Stmt_id())
    return FALSE;
  else {
    // same stmt_id, these statements correspond to inserted
    // occurrences. Search forward from *this till the
    // Stmt_id changes.  Return TRUE if found x.
    const STMTREP *cur = this;
    if (cur == x) return TRUE; // same statement node

    INT32    stmt_id = cur->Stmt_id();
    for (cur = cur->Next();
	 cur && cur->Stmt_id() == stmt_id;
	 cur = cur->Next()){
      if (cur == x) return TRUE;
    }
    return FALSE;
  }
}

// Test if the DPO of 'this' less than the DPO of 'x'.
// Asserts if they are the same. [ Obviously not. -- RK ]
BOOL
EXP_OCCURS::Is_DPO_less_than(EXP_OCCURS *x)
{
  if (For_spre()) {
    // in same BB, follow this order: iphi, real, phi_succ, exit
    // among different BB, follow Pdom_dfs_id
    if (Bb()->Pdom_dfs_id() < x->Bb()->Pdom_dfs_id())
      return TRUE;
    if (Bb() == x->Bb()) { // same dpo number
      return Occ_kind() <= x->Occ_kind();
    }
  }
  else {
    // in same BB, follow this order: phi, real, phi_pred, exit
    // among different BB, follow Dom_dfs_id
    if (Bb()->Dom_dfs_id() < x->Bb()->Dom_dfs_id())
      return TRUE;

    // Shouldn't the following line assert that the BB's have to be the
    // same, instead of simply checking?
    if (Bb() == x->Bb()) { // same dpo number
      if ((Occ_kind() == OCC_COMP_OCCUR || Occ_kind() == OCC_REAL_OCCUR) &&
	  x->Occurs_as_hoisted()) {
	// hoisted occurrence is inserted right before a branch
	OPCODE opc = Stmt()->Op();
	if (opc == OPC_COMPGOTO ||
	    opc == OPC_AGOTO ||
	    opc == OPC_TRUEBR ||
	    opc == OPC_FALSEBR)
	  return FALSE;
	else
	  return TRUE;
      }
      if ((x->Occ_kind() == OCC_COMP_OCCUR || x->Occ_kind() == OCC_REAL_OCCUR) &&
	  Occurs_as_hoisted()) {
	// hoisted occurrence is inserted right before a branch
	OPCODE opc = x->Stmt()->Op();
	if (opc == OPC_COMPGOTO ||
	    opc == OPC_AGOTO ||
	    opc == OPC_TRUEBR ||
	    opc == OPC_FALSEBR)
	  return TRUE;
	else
	  return FALSE;
      }

      if ((Occ_kind() == OCC_COMP_OCCUR || Occ_kind() == OCC_REAL_OCCUR) && 
	  (x->Occ_kind() == OCC_COMP_OCCUR || x->Occ_kind() == OCC_REAL_OCCUR)){
        // if one is comp occur, the other must be either comp occur or
        // real occur.  Check the statement id
        return Stmt_order_less_or_equal(x);
      }
      else
        return Occ_kind() <= x->Occ_kind();
    }
  }

  // How can we get here?
  return FALSE;
}

void
EXP_OCCURS::Render_coderep_unownable(const ETABLE                 *const etable,
				     ID_MAP<REHASH_INFO *, INT32> &      id_map)
{
  REHASH_INFO *rehash_info = id_map.Lookup(Occurrence()->Coderep_id());

  if (Occurrence()->Is_flag_set(CF_OWNED_BY_TEMP)) {
    Is_True(rehash_info != NULL, ("EXP_OCCURS::Render_coderep_unownable: "
				  "Owned coderep must occur in ID map"));
    rehash_info->Owning_t_ver()->Reset_t_ver_owns_coderep();
    Occurrence()->Reset_flag(CF_OWNED_BY_TEMP);
  }

  if (rehash_info == NULL) {
    rehash_info = CXX_NEW(REHASH_INFO(Occurrence()),
			  etable->Etable_local_pool());
    id_map.Insert(Occurrence()->Coderep_id(), rehash_info);
  }
  rehash_info->Set_coderep_unownable();
}

void
EXP_OCCURS::Bid_for_coderep(const ETABLE                       *const etable,
			          ID_MAP<REHASH_INFO *, INT32> &      id_map,
			    const UINT32                              rehash_cost)
{
  REHASH_INFO *rehash_info  = id_map.Lookup(Occurrence()->Coderep_id());

  if (rehash_info == NULL) {
    rehash_info = CXX_NEW(REHASH_INFO(Occurrence()),
			  etable->Etable_local_pool());
    id_map.Insert(Occurrence()->Coderep_id(), rehash_info);
  }

  if (!rehash_info->Coderep_unownable() &&
	   (rehash_info->Owning_t_ver() == NULL ||
	    rehash_info->Max_rehash_cost() < rehash_cost)) {
    if (Occurrence()->Omitted()) {
      // Nobody can own this coderep, since there are instances of it in
      // the program that don't show up in the worklist. Because the
      // state of this flag cannot change during the bidding sequence,
      // we don't need to do a full Render_coderep_unownable operation
      // here. It's enough to set the flag in *rehash_info because we're
      // guaranteed to see the correct value of Omitted() the first time
      // we process this coderep.
      rehash_info->Set_coderep_unownable();
    }
    else {
      rehash_info->Set_max_rehash_cost(rehash_cost);
      if (rehash_info->Owning_t_ver() != NULL) {
	// We're pushing someone else out of the way. Make sure they
	// know they've been booted.
	rehash_info->Owning_t_ver()->Reset_t_ver_owns_coderep();
      }
      else {
	rehash_info->Coderep()->Set_flag(CF_OWNED_BY_TEMP);
      }
      rehash_info->Set_owning_t_ver(this);
      Set_t_ver_owns_coderep();
    }
  }
}

// Return the occurrence this occurrence is identical to. This
// function cannot be used prior to the SSA minimization step. A NULL
// return value should never arise after the SSA minimization step,
// although it is possible during that step's processing.

EXP_OCCURS *
EXP_OCCURS::Occ_identical_to(void)
{
  OCC_KIND    kind = Occ_kind();
  EXP_OCCURS *identical_to = NULL;

  if (kind == OCC_REAL_OCCUR) {
    Is_True(Save_to_temp(),
	    ("EXP_OCCURS::Identical_to: real occurrence "
	     "must be saved"));
    return this;
  }
  else if (kind == OCC_PHI_PRED_OCCUR) {
    Is_True(Inserted_computation() || Sunk_lvalue(),
	    ("EXP_OCCURS::Identical_to: phi-pred occurrence "
	     "must be inserted"));
    return this;
  }
  else if (kind == EXP_OCCURS::OCC_PHI_OCCUR) {
    if (Exp_phi()->Identity() &&
	Exp_phi()->Replacing_occur_known()) {
      Is_True(Exp_phi()->Identical_to() != NULL,
	      ("EXP_OCCURS::Identical_to: known replacing occur "
	       "must be non-NULL"));
      return Exp_phi()->Identical_to();
    }
    else if (!Exp_phi()->Identity()) {
      return this;
    }
  }
  return NULL;
}

EXP_OCCURS::EXP_OCCURS(CODEREP *occurrence,
                       STMTREP *enclose_stmt,
                       EXP_PHI *exp_phi,
                       BOOL     is_real_occurrence)
{
  // whenever you add some initialization statement here,
  // also add it to the bottom of the EXP_WORKLST::Remove_occurs
  Set_occurrence(occurrence);
  Clear_temp_cr(); // clears def_occur as a side effect
  Set_e_version(EXP_OCCURS::ILLEGAL_E_VERSION);
  Clear_flags();
  if (is_real_occurrence) {
    Set_kind(OCC_REAL_OCCUR);
    Set_enclose_stmt(enclose_stmt);
    Set_rehash_cost(0);
  } else {
    Set_kind(OCC_PHI_OCCUR);
    Set_exp_phi(exp_phi);
  }
}

// For SPRE (notice the mem_pool is allocated differently from normal PRE)
EXP_OCCURS*
ETABLE::New_phi_occurrence(EXP_WORKLST *worklst,
			   MEM_POOL    *pool,
			   BB_NODE     *bb)
{
  EXP_PHI *new_phi = CXX_NEW(EXP_PHI(worklst->E_num(),
				     bb->Iphi_list()->Out_degree(),
				     bb, pool), pool);
  EXP_OCCURS *occurs = Alloc_occurs_node();
  occurs->Set_occurrence(NULL);
  occurs->Set_kind(EXP_OCCURS::OCC_PHI_OCCUR);
  occurs->Set_exp_phi(new_phi);
  new_phi->Set_result(occurs);
  occurs->Set_spre_wk(worklst);

  return occurs;
}

void
EXP_OCCURS_CONTAINER::Init(void)
{
  Set_Head(NULL);
  Set_Tail(NULL);
}

EXP_OCCURS *
OCC_ITER_LIST::Get_cur(void) const
{
  return (Is_occ_iter()) ? Occ_iter()->Cur() :
  All_real_iter()->Cur();
}

EXP_OCCURS *
OCC_ITER_LIST::Next_occ(void) const
{
  return (Is_occ_iter()) ? Occ_iter()->Next() :
  All_real_iter()->Next();
}

void
OCC_CONTAINER::Insert_sort(OCC_ITER_LIST *l)
{
  EXP_OCCURS *l_cur = l->Get_cur();
  // occ_iter_list l is empty, do not insert
  if (l_cur == NULL) return;

  // sort according to DPO order
  OCC_ITER_LIST_ITER occ_iter_list_iter(this);
  OCC_ITER_LIST *tmp, *prev = NULL;
  FOR_ALL_NODE(tmp, occ_iter_list_iter, Init()) {
    EXP_OCCURS *exp_tmp = tmp->Get_cur();
    if (exp_tmp->Is_DPO_less_than(l_cur))
      prev = tmp;
    else
      break;
  }
  if (prev == NULL)
    Prepend(l); // prepend before the Head
  else if (prev == Tail())
    Append(l);
  else
    prev->Insert_After(l);    // append after prev
}

EXP_ALL_REAL_ITER::EXP_ALL_REAL_ITER(EXP_OCCURS *real_occ,
                                     EXP_OCCURS_PAIR *comp_occ):
                                     _real(real_occ),
                                     _real_iter(&_real),
                                     _comp1_iter(&_comp1),
                                     _comp2_iter(&_comp2)
{
  if (comp_occ) {
    _comp1.Init( comp_occ->Occ1() );
    _comp2.Init( comp_occ->Occ2() );
  }
}

EXP_OCCURS*
EXP_ALL_REAL_ITER::Get_cur(void) const
{
  const OCC_ITER_LIST *first_iter = _all_iter.Head();
  if (first_iter == NULL) return NULL;
  return first_iter->Get_cur();
}

EXP_OCCURS*
EXP_ALL_REAL_ITER::First(void)
{
  if (this == NULL)
    return NULL;

  // follow the order of phi, real, phi_pred, and exit
  _real.First();
  _comp1.First();
  _comp2.First();

  _all_iter.Insert_sort(&_real_iter);
  _all_iter.Insert_sort(&_comp1_iter);
  _all_iter.Insert_sort(&_comp2_iter);

  return Get_cur();
}

EXP_OCCURS*
EXP_ALL_REAL_ITER::Next(void)
{
  OCC_ITER_LIST *first_iter = _all_iter.Head();
  if (first_iter == NULL) return NULL;

  EXP_OCCURS *retval = first_iter->Get_cur();
  EXP_OCCURS *next = first_iter->Next_occ();

  if (next == NULL) {
    _all_iter.Remove_Headnode();
  }
  else {
    _all_iter.Remove_Headnode();
    _all_iter.Insert_sort(first_iter);
  }

  EXP_OCCURS *result = Get_cur();

  Is_True((result==NULL) || retval->Is_DPO_less_than(result),
	  ("EXP_ALL_OCCURS_ITER::Next(): Not dpo order"));

  return result;
}

BOOL
EXP_ALL_REAL_ITER::Is_Empty(void) const
{
  return Get_cur() == NULL;
}
// constructor for slow iterator (some parameters can be NULL)
EXP_ALL_OCCURS_ITER::EXP_ALL_OCCURS_ITER(EXP_OCCURS      *real,
					 EXP_OCCURS_PAIR *comp,
                                         EXP_OCCURS      *phi,
                                         EXP_OCCURS      *phi_pred,
					 EXP_OCCURS      *exit):
                                         _real(real,comp),
					 _phi(phi),
                                         _phi_pred(phi_pred),
					 _exit(exit),
                                         _real_iter(&_real),
                                         _phi_iter(&_phi),
                                         _pred_iter(&_phi_pred),
                                         _exit_iter(&_exit)
{
  _fast_array = NULL;
}

// constructor for fast reuse iterator (requires all lists non-NULL)
EXP_ALL_OCCURS_ITER::EXP_ALL_OCCURS_ITER(EXP_WORKLST *worklst,
					 ETABLE *etable, LFTR *lftr):
	_real(worklst->Real_occurs().Head(), lftr->Exp_hash(worklst)),
	_phi(worklst->Phi_occurs().Head()),
	_phi_pred(worklst->Phi_pred_occurs().Head()),
	_exit(etable->Exit_occurs().Head()),
	_real_iter(&_real),
	_phi_iter(&_phi),
	_pred_iter(&_phi_pred),
	_exit_iter(&_exit)
{
  // first calculate size of array, all lists required except comp (LFTR)
  INT32 size = worklst->Real_occurs().Len() +
    	       worklst->Phi_occurs().Len() +
	       worklst->Phi_pred_occurs().Len() +
	       etable->Exit_occurs().Len();
  if (lftr->Lftr_on())
    size += lftr->Len();

  if (lftr->Lftr_on()) {
    Is_Trace(lftr->Trace(),(TFile,"PPP LFTR size =            %d\n",
			    lftr->Len()));
  }

#ifdef XXX
  // now allocate array and fill in
  _fast_array = (EXP_OCCURS **) CXX_NEW_ARRAY(EXP_OCCURS, size, mem_pool);

  EXP_OCCURS *occur;
  EXP_ALL_OCCURS_ITER *exp_occ_iter = this;
  INT32 i = 0;
  FOR_ALL_NODE(occur, *exp_occ_iter, Init()) {
    _fast_array[i++] = occur;
    Is_True(i < size,("EXP_ALL_OCCURS_ITER::Fill_fast_array, array overflow"));
  }
#endif
}

EXP_OCCURS*
EXP_ALL_OCCURS_ITER::Get_cur(void) const
{
  const OCC_ITER_LIST *first_iter = _all_iter.Head();
  if (first_iter == NULL) return NULL;
  return first_iter->Get_cur();
}

EXP_OCCURS*
EXP_ALL_OCCURS_ITER::Next(void)
{
  OCC_ITER_LIST *first_iter = _all_iter.Head();
  if (first_iter == NULL) return NULL;

  EXP_OCCURS *retval = first_iter->Get_cur();
  EXP_OCCURS *next = first_iter->Next_occ();

  if (next == NULL) {
    _all_iter.Remove_Headnode();
  }
  else if (retval->Bb() !=  next->Bb()) {
    _all_iter.Remove_Headnode();
    _all_iter.Insert_sort(first_iter);
  }

  EXP_OCCURS *result = Get_cur();

  Is_True((result==NULL) || retval->Is_DPO_less_than(result),
	  ("EXP_ALL_OCCURS_ITER::Next(): Not dpo order"));

  return result;
}

EXP_OCCURS*
EXP_ALL_OCCURS_ITER::First(void)
{
  if (this == NULL)
    return NULL;

  // follow the order of phi, real, phi_pred, and exit
  _phi.First();
  _real.First();
  _phi_pred.First();
  _exit.First();

  _all_iter.Insert_sort(&_phi_iter);
  _all_iter.Insert_sort(&_real_iter); // REAL_OCCUR and COMP_OCCUR
  _all_iter.Insert_sort(&_pred_iter);
  _all_iter.Insert_sort(&_exit_iter);

  return Get_cur();
}

BOOL
EXP_ALL_OCCURS_ITER::Is_Empty(void) const
{
  return Get_cur() == NULL;
}

void
EXP_ALL_OCCURS_ITER::Remove_iter(void)
{
  // PPP
}

void
EXP_WORKLST::Init(IDTYPE e_num, CODEREP *exp)
{
  Set_Next(NULL);	// hash table bucket list
  _e_num = e_num;
  _exp = exp;
  Set_preg(ILLEGAL_PREG);
  _u1._iphi_bbs = NULL;
  _real_occurs.Init();
  _phi_occurs.Init();
  _phi_pred_occurs.Init();
  Clear_flags();
  Reset_statistics();
  Init_e_version();
}

// This routine examine the _exp tree for const/rconst/lda node and
// set the corresponding flags in 'this' worklst
void
EXP_WORKLST::Exam_const(void)
{
  if (Exp()->Kind() != CK_OP) return;
  for (INT i = 0; i < Exp()->Kid_count(); i++) {
    switch (Exp()->Get_opnd(i)->Kind()) {
    case CK_CONST:
      Set_has_const();
      break;
    case CK_RCONST:
      Set_has_rconst();
      break;
    case CK_LDA:
      Set_has_lda();
      break;
    }
  }
}

// verify if the occ is in dpo order of occ is the last in the worklist
BOOL
EXP_WORKLST::Verify_dpo_order(EXP_OCCURS_CONTAINER &worklist, EXP_OCCURS *occ)
{
  EXP_OCCURS *tail = worklist.Tail();
  Is_True(!tail || tail->Is_DPO_less_than(occ),
	  ("EXP_WORKLST::Verify_dpo_order: Not dpo order"));
  return TRUE;
}

BOOL
EXP_WORKLST::Verify_dpo_order(EXP_OCCURS_CONTAINER &worklist)
{
  EXP_OCCURS *occ_pre = worklist.Head();
  EXP_OCCURS *occ_cur;
  EXP_OCCURS_ITER exp_occ_iter;
  FOR_ALL_NODE(occ_cur, exp_occ_iter, Init(worklist.Head()->Next())) {
    Is_True(occ_pre->Is_DPO_less_than(occ_cur),
	    ("EXP_WORKLST::Verify_dpo_order: Not dpo order"));
    occ_pre = occ_cur;
  }
  return TRUE;
}

// Append an occurrence node to one of the list.
// Asserts if it is not the highest in DPO.
void
EXP_WORKLST::Append_occurrence(EXP_OCCURS *occ)
{
  switch (occ->Occ_kind()) {
  case EXP_OCCURS::OCC_REAL_OCCUR:
    Is_True(Verify_dpo_order(Real_occurs(), occ),
            ("EXP_WORKLST::Append_occurrence: failed dpo_order test"));
    Real_occurs().Append(occ);
    break;
  case EXP_OCCURS::OCC_PHI_OCCUR:
    Is_True(Verify_dpo_order(Phi_occurs(), occ),
            ("EXP_WORKLST::Append_occurrence: failed dpo_order test"));
    Phi_occurs().Append(occ);
    break;
  case EXP_OCCURS::OCC_PHI_PRED_OCCUR:
    Is_True(Verify_dpo_order(Phi_pred_occurs(), occ),
            ("EXP_WORKLST::Append_occurrence: failed dpo_order test"));
    Phi_pred_occurs().Append(occ);
    break;
  default:
    Is_True(FALSE,("EXP_WORKLST::Append_occurrence: Illegal occurrence type"));
    break;
  }
}

// Insert occurrence node to one of the lists in the right slot to
// keep the DPO. BB that determines DPO is specified explicitly, so
// insertion into the occurrence list can be made before the
// assignment STMTREP is built.

void
EXP_WORKLST::Insert_occurrence(EXP_OCCURS *occ, BB_NODE *bb)
{
  Is_True(occ->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR,
	  ("EXP_WORKLST::Insert_occurrence: insert not implemented "
	   "for non-real occ"));

  EXP_OCCURS      *real_occ;
  EXP_OCCURS_ITER  real_occ_iter;

  EXP_OCCURS *prev_occ = NULL;
  FOR_ALL_NODE(real_occ, real_occ_iter, Init(Real_occurs().Head())) {
    if (bb->Dom_dfs_id() < real_occ->Bb()->Dom_dfs_id()) {
      // Found the first place in the real occurrence list where the
      // DPO id exceeds that of the given BB.
      break;
    }
    prev_occ = real_occ;
  }

  // insertion is made at the end of a BB, but before the any occur of
  // the branch stmt.

  if (real_occ != NULL) {
    // Insert before real_occ; TODO: inefficiency here -- in principle
    // we don't need to traverse the list twice, and SLIST::Prepend()
    // traverses the list to guarantee that real_occ is in the list.

    OPCODE opc;
    if (prev_occ != NULL &&
	prev_occ->Bb() == bb && 
	((opc = prev_occ->Stmt()->Op()) == OPC_COMPGOTO ||
	 opc == OPC_AGOTO ||
	 opc == OPC_TRUEBR ||
	 opc == OPC_FALSEBR)) {
      BOOL success = Real_occurs().Prepend(occ, prev_occ);
      Is_True(success, ("EXP_WORKLST::Insert_occurrence failed!"));
    } else {
      BOOL success = Real_occurs().Prepend(occ, real_occ);
      Is_True(success, ("EXP_WORKLST::Insert_occurrence failed!"));
    }
  }
  else {
    // Insert at the end of the list. This is a hack; Prepend() should
    // be willing to take NULL as its second operand, and place its
    // first operand at the end of the list in that case.
    Real_occurs().Append(occ);
  }
  Is_True(Verify_dpo_order(Real_occurs()), 
	  ("EXP_WORKLST::Insert_occurrence: Not in DPO order after "
	   "real_occurrence insertion"));
}

void
ETABLE::Add_to_occ_freelist(EXP_OCCURS *node)
{
  if (node != NULL) {
    CODEREP *cr = (CODEREP*)node->Occurrence();
    // for LPRE and EPRE, reset the bitpos such that RVI can work
    // correctly.
    if (Pre_kind() != PK_EPRE &&
        (cr && (node->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR ||
                node->Occ_kind() == EXP_OCCURS::OCC_COMP_OCCUR)))
      cr->Set_Bitpos(ILLEGAL_BP);
    if (node->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) {
      node->Exp_phi()->Bb()->Set_exp_phi(NULL);
    }
    Occ_freelist()->Push(node);
  }
}

void
EXP_WORKLST::Insert_occurrence(EXP_OCCURS *occ, ETABLE *etable)
{
  Is_True(occ->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR,
	  ("EXP_WORKLST::Insert_occurrence: insert not implemented "
	   "for non-real occ"));

  EXP_OCCURS      *real_occ;
  EXP_OCCURS_ITER  real_occ_iter;
  EXP_OCCURS      *real_pred = NULL;

  FOR_ALL_NODE(real_occ, real_occ_iter, Init(Real_occurs().Head())) {
    if (real_occ->Bb()->Dom_dfs_id() > occ->Bb()->Dom_dfs_id()) {
      // Found the first place in the real occurrence list where the
      // DPO id exceeds that of the given occ.
      break;
    }
    if (real_occ->Bb() == occ->Bb()) {
      if (!real_occ->Stmt_order_less_or_equal(occ))
        break;
      else if (real_occ->Stmt() == occ->Stmt()) {
	if (real_occ->Stmt_kid_num() > occ->Stmt_kid_num())
	  break;
      }
    }
    real_pred = real_occ;
  }
  if (real_pred != NULL) {
    if (real_pred->Enclosed_in_stmt() == occ->Enclosed_in_stmt() &&
	real_pred->Stmt_kid_num() == occ->Stmt_kid_num()) {
      real_pred->Set_mult_real();
      etable->Add_to_occ_freelist(occ);
    }
    else {
      real_pred->Insert_After(occ);
    }
  }
  else if (real_occ == NULL) 
    Real_occurs().Append(occ);
  else 
    Real_occurs().Prepend(occ);

  Is_True(Verify_dpo_order(Real_occurs()), 
	  ("EXP_WORKLST::Insert_occurrence: Not in DPO order after "
	   "real_occurrence insertion"));
  
}


//  This is called by Is_the_same_as() to check if the "leaf" nodes are different.
//  Leaf nodes are CK_VAR, CK_LDA, CK_CONST, CK_RCONST and 
//   PARM nodes of INTRINSIC_OP.
//
static BOOL
Are_different(CODEREP *cr1, CODEREP *cr2)
{
  if (cr1 == cr2) return FALSE;
  if (cr1->Kind() != cr2->Kind()) return TRUE;

  switch (cr1->Kind()) {
  case CK_VAR:
    return cr1->Aux_id() != cr2->Aux_id();
  case CK_IVAR:
    if (cr1->Opr() == OPR_PARM ) {
      // must have same opcode
      if ( cr1->Op() != cr2->Op() ) return TRUE;

      // same opcodes, but check if kids are different
      if (Are_different( cr1->Ilod_base(), cr2->Ilod_base()))
	  return TRUE;

      if ((cr1->Ivar_mu_node() == NULL) != (cr2->Ivar_mu_node() == NULL))
	return TRUE;

      return FALSE;
    }
    Is_True(FALSE, ("EXP_WORKLST::Are_different:  must not reach here."));
  case CK_OP:
    Is_True(FALSE, ("EXP_WORKLST::Are_different:  must not reach here."));
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
  default:
    return TRUE;
  }
}

// DONT CALL THIS FUNCTION WITHOUT READING THE CODE.
//
// Returns TRUE if  'this' is lexically identical to 'cr'.
//  With EPRE, both this->Exp() and cr must be 1st order expressions.
//  With LPRE and SPRE, both must be CK_VAR.
//
BOOL
EXP_WORKLST::Is_the_same_as(const CODEREP *cr)
{

#ifdef Is_True_On
  if (Pre_kind() == PK_LPRE) {
    Is_True(inCODEKIND(Exp()->Kind(), CK_CONST|CK_RCONST|CK_LDA|CK_VAR),
            ("EXP_WORKLST::Is_the_same_as(cr), not a leaf node, kind=0x%x",
             Exp()->Kind()));
    Is_True(inCODEKIND(Exp()->Kind(), CK_CONST|CK_RCONST|CK_LDA|CK_VAR),
            ("EXP_WORKLST::Is_the_same_as(cr), not a leaf node, kind=0x%x",
             cr->Kind()));
  }
  else if (Pre_kind() == PK_SPRE) {
    Is_True(Exp()->Kind() == CK_VAR,
            ("EXP_WORKLST::Is_the_same_as(cr), not a VAR, kind=0x%x",
             Exp()->Kind()));
    Is_True(cr->Kind() == CK_VAR,
            ("EXP_WORKLST::Is_the_same_as(cr), not a VAR, kind=0x%x",
             cr->Kind()));
  }
  else {
    Is_True(Exp()->Non_leaf(),
            ("EXP_WORKLST::Is_the_same_as(cr), not a OP or IVAR, kind=0x%x",
             Exp()->Kind()));
    Is_True(cr->Non_leaf(),
            ("EXP_WORKLST::Is_the_same_as(cr), not a OP or IVAR, kind=0x%x",
             cr->Kind()));
  }
#endif

  if (Exp() == cr) return TRUE;         // same node

  if (Exp()->Kind() != cr->Kind()) return FALSE;  // different KIND

  // Constants must have the same CR node to match
  if (inCODEKIND(cr->Kind(), CK_CONST|CK_RCONST|CK_LDA) ||
      inCODEKIND(Exp()->Kind(), CK_CONST|CK_RCONST|CK_LDA)) return FALSE;

  if (Pre_kind() == PK_LPRE || Pre_kind() == PK_SPRE)
    return Exp()->Aux_id() == cr->Aux_id();

  if (Exp()->Kind() == CK_IVAR && cr->Kind() == CK_IVAR) {       // IVAR
    if (Exp()->Opr() != cr->Opr())
      return FALSE;

    CODEREP *gcr = Exp()->Ilod_base();
    CODEREP *lcr = cr->Ilod_base();
    if (gcr == NULL)
      gcr = Exp()->Istr_base();
    if (lcr == NULL) 
      lcr = cr->Istr_base();
    if (Are_different(lcr, gcr))
      return FALSE;

    if (cr->Opr() == OPR_ILOADX) {
      if (Are_different(cr->Index(), Exp()->Index()))
	return FALSE;
    }
    else {
      if (cr->Offset() != Exp()->Offset())
	return FALSE;	// offset not the same
    }
    if (Get_mtype_class(cr->Dtyp()) != Get_mtype_class(Exp()->Dtyp()))
      return FALSE;	// type class not the same
    if (MTYPE_size_min(cr->Dsctyp()) != MTYPE_size_min(Exp()->Dsctyp()))
      return FALSE;	// size not the same

    if (cr->Ivar_occ()->Aux_id() != Exp()->Ivar_occ()->Aux_id()) // different vsym
      return FALSE;

    if (cr->Opr() == OPR_MLOAD) {
      gcr = Exp()->Mload_size();
      if (gcr == NULL)
	gcr = Exp()->Mstore_size();
      lcr = cr->Mload_size();
      if (lcr == NULL)
	lcr = cr->Mstore_size();
      if (Are_different(lcr, gcr))
	return FALSE;
    }
    else if (OPERATOR_is_scalar_iload (cr->Opr())) {
      // must have identical alignment
      const TY_IDX exp_addr_ty = Exp()->Ilod_base_ty();
      const TY_IDX cr_addr_ty = cr->Ilod_base_ty();
      if (exp_addr_ty != cr_addr_ty) {
        if (! exp_addr_ty || ! cr_addr_ty)
	  return FALSE;
        if (TY_kind(exp_addr_ty) != KIND_POINTER)
	  return FALSE;
        if (TY_kind(cr_addr_ty) != KIND_POINTER)
	  return FALSE;
        if (TY_align_exp(TY_pointed(exp_addr_ty)) !=
	    TY_align_exp(TY_pointed(cr_addr_ty)))
	  return FALSE;
      }
      if (cr->Opr() == OPR_ILDBITS) {
	if (cr->I_bit_offset() != Exp()->I_bit_offset () ||
	    cr->I_bit_size() != Exp()->I_bit_size ())
	  return FALSE;
      }
    }

    // Return FALSE if one is PARM and the other is not.
    if ((Exp()->Opr() == OPR_PARM) != (cr->Opr() == OPR_PARM))
      return FALSE;

    return TRUE;
  }
  else {				        // OP
    if (Exp()->Op() != cr->Op()) return FALSE;

    if (Exp()->Kid_count() != cr->Kid_count())	// arrays can be different
      return FALSE;

#ifdef Is_True_On
    if (OPCODE_commutative_op(cr->Op()) == cr->Op()) {
      // commutative ops must have only 2 kids
      // TODO: handle opcodes that are commutative with different opcode
      //       e.g. a <= b becoming b >= a
      if (!Are_different(cr->Opnd(0), Exp()->Opnd(1)) &&
	  !Are_different(cr->Opnd(1), Exp()->Opnd(0)) &&
	  Are_different(cr->Opnd(0), cr->Opnd(1))) {
	DevWarn("EXP_WORKLST::Is_the_same_as: expr not canonicalized.");
	return FALSE;
      }
    }
#endif

    for (IDX_32 i=0; i<Exp()->Kid_count(); i++) {
      if (Are_different(cr->Opnd(i),Exp()->Opnd(i)))
	return FALSE;
    }

    // check additional fields for some specific opcodes
    if (cr->Opr() == OPR_INTRINSIC_OP &&
	cr->Intrinsic() != Exp()->Intrinsic())
      return FALSE;
#ifdef KEY
    else if (cr->Opr() == OPR_PURE_CALL_OP &&
             cr->Call_op_aux_id() != Exp()->Call_op_aux_id())
      return FALSE;
#endif
    else if (cr->Opr() == OPR_CVTL && cr->Offset() != Exp()->Offset())
      return FALSE;
    else if ((cr->Opr() == OPR_EXTRACT_BITS || cr->Opr() == OPR_COMPOSE_BITS)
	     && (cr->Op_bit_offset() != Exp()->Op_bit_offset() ||
		 cr->Op_bit_size() != Exp()->Op_bit_size()))
      return FALSE;
    return TRUE; // all the kids are either the same bitpos or same consts
  }
}

// The following should be implemented differently when hash functions
// work.
EXP_OCCURS *
EXP_WORKLST::Lookup_phi_occ(const BB_NODE *bb)
{
  // slow implementation for now.
  EXP_OCCURS_ITER phi_occ_iter(Phi_occurs().Head());
  EXP_OCCURS *occur;
  FOR_ALL_NODE(occur, phi_occ_iter, Init()) {
    if (occur->Bb() == bb)
      return occur;
  }
  return NULL;
}

// find a EXP_WORKLST that points to a specific CODEREP
EXP_WORKLST_NODE *
EXP_WORKLST_NODE_CONTAINER::Find_exp_worklst(const CODEREP *cr)
{
  EXP_WORKLST_NODE *etmp, *result = NULL;
  EXP_WORKLST_NODE_ITER exp_worklst_iter;
  FOR_ALL_NODE(etmp, exp_worklst_iter, Init(this)) {
    Set_Tail(etmp);		// tricky: Head set in Init, Tail set here
    if (etmp->Node()->Is_the_same_as(cr))
      result = etmp;		// don't return here, Tail needs to go to end
  }
  return result;
}

// append an EXPREP to a bucket in the etable
void
EXP_WORKLST_NODE_CONTAINER::Append(EXP_WORKLST_NODE *worklst,
                                   IDX_32            idx,
                                   ETABLE           *etable)
{
  SLIST::Append(worklst);
  etable->Set_exp_hash_bucket(idx,Head()); // needed for first time through
}

EXP_WORKLST*
EXP_WORKLST_ITER2::First(void)
{
  if (_cur = _urgent_worklst->Head()) {
    _urgent_worklst->Remove_Headnode();
  }
  else if (_cur = _exp_worklst->Head()) {
    _exp_worklst->Remove_Headnode();
  }
  else
    _cur = NULL;

  return _cur;
}

EXP_WORKLST*
EXP_WORKLST_ITER2::Next(void)
{
  if (_cur) {
    if (_cur->Is_urgent())
      _cur->Reset_is_urgent();
  }
  return First();
}

// Add the mapping from an expr to its hashed from to the cache.
void
ETABLE::Add_rehash_expr(CODEREP *cr, CODEREP *rehash_cr) 
{
  REHASH_CACHE_LIST *p = CXX_NEW(REHASH_CACHE_LIST(cr, rehash_cr), Per_expr_pool());
  if (_rehash_cache == NULL)
    _rehash_cache = p;
  else {
    _rehash_cache->Insert_Before(p);
    _rehash_cache = p;
  }
}

// Remove all occurrences in the work list
void
EXP_WORKLST::Remove_occurs(ETABLE *etable)
{
  EXP_OCCURS *exp_occ, *prev_occ, *next_occ;
  EXP_OCCURS_ITER exp_occ_iter;
  prev_occ = NULL;

  // Free each real occurrence that meets the criterion of no longer
  // referring to the current worklist's expression.
  // FOR_ALL_NODE(exp_occ, exp_occ_iter, Init(Real_occurs().Head())) {
  exp_occ_iter.Init(Real_occurs().Head());
  for (exp_occ = exp_occ_iter.First();
       ! exp_occ_iter.Is_Empty();
       exp_occ = next_occ) {
    // move to freelist if it is deleted, otherwise leave it here
    next_occ = exp_occ_iter.Next();
    BOOL ok_to_remove;
    switch (Pre_kind()) {
    case PK_LPRE:
    case PK_SPRE:
    case PK_VNFRE:
      ok_to_remove = TRUE; // since we do not do second order effect 
      break;
    case PK_EPRE:
      ok_to_remove = (!(exp_occ->Occurrence()->Non_leaf() &&
			Is_the_same_as(exp_occ->Occurrence())) ||
		      exp_occ->Delete_comp());
      break;
    default:
      Is_True(FALSE, ("unexpected PRE_KIND."));
    }
    if (ok_to_remove) {
      etable->Add_to_occ_freelist(exp_occ);
      Real_occurs().Remove(prev_occ, exp_occ);
    }
    else {
      prev_occ = exp_occ;
    }
  }

  // Free all phi occurrences, since they aren't needed any more.
  FOR_ALL_NODE(exp_occ, exp_occ_iter, Init(Phi_occurs().Head())) 
    etable->Add_to_occ_freelist(exp_occ);

  Phi_occurs().Clear();

  // Free all phi pred occurrences except the ones referring to EPRE
  // insertions that have been embodied in the code (an inserted
  // occurrence that got hoisted to somewhere else might not have been
  // embodied, hence the Encl_stmt_set() check, I believe -- RK).
  prev_occ = NULL;
  exp_occ_iter.Init(Phi_pred_occurs().Head());
  for (exp_occ = exp_occ_iter.First();
       ! exp_occ_iter.Is_Empty();
       exp_occ = next_occ) {
    next_occ = exp_occ_iter.Next();
    if (Pre_kind() != PK_EPRE ||
        !exp_occ->Inserted_computation() || !exp_occ->Encl_stmt_set()) {
      etable->Add_to_occ_freelist(exp_occ);
      Phi_pred_occurs().Remove(prev_occ, exp_occ);
    }
    else {
      prev_occ = exp_occ;
    }
  }

  // Move the remaining occurrence in phi_pred list into real_occur
  // list in case we revisit this expression later due to second-order
  // effects.
  if (Pre_kind() == PK_EPRE) {
    EXP_ALL_OCCURS_ITER exp_all_occ_iter(Real_occurs().Head(),
                                         NULL,
                                         NULL,
                                         Phi_pred_occurs().Head(),
                                         NULL);
    prev_occ = NULL;
    exp_all_occ_iter.Init();
    for (exp_occ = exp_all_occ_iter.First();
         ! exp_all_occ_iter.Is_Empty();
         exp_occ = next_occ) {
      next_occ = exp_all_occ_iter.Next();
      BOOL is_multi_occ = FALSE;
      BOOL is_l_value;
      if (exp_occ->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR) {
	is_l_value = exp_occ->Occurs_as_lvalue();
        prev_occ = exp_occ;
        if (exp_occ->Mult_real()) is_multi_occ = TRUE;
        if (exp_occ->Save_to_temp())
          exp_occ->Set_stmt_kid_num(0);
      }
      else {
        Is_True (exp_occ->Inserted_computation(),
                 ("EXP_WORKLST::Remove_occurs: Not inserted computation"));
        Is_True (exp_occ->Encl_stmt_set(),
                 ("EXP_WORKLST::Remove_occurs: Inclosed stmt not set"));
        Is_True(prev_occ == NULL || prev_occ->Is_DPO_less_than(exp_occ),
                ("EXP_WORKLST::Remove_occurs: Not dpo order"));
        // change the phi_pred occurrence to a real occurrence
	is_l_value = FALSE;
        exp_occ->Set_kind(EXP_OCCURS::OCC_REAL_OCCUR);
	exp_occ->Set_stmt_kid_num(0);
        Phi_pred_occurs().Remove(NULL, exp_occ); // remove from phi_pred
        Real_occurs().SLIST::Append(exp_occ, prev_occ); // append after prev_occ
	prev_occ = exp_occ;
      }
      exp_occ->Set_rehash_cost(0);
      // check out EXP_OCCURS::EXP_OCCURS
      exp_occ->Clear_temp_cr(); // clears def_occur as a side effect
      exp_occ->Set_e_version(EXP_OCCURS::ILLEGAL_E_VERSION);
      exp_occ->Clear_flags();
      if (is_multi_occ) exp_occ->Set_mult_real();
      if (is_l_value) exp_occ->Set_occurs_as_lvalue();
    }
  }
  Set_is_processed();
  Set_preg(ILLEGAL_PREG);  // reset the PREG
}

BOOL
EXP_WORKLST::Remove_real_occurrence( STMTREP *stmt)
{
  EXP_OCCURS *exp_occ, *prev_occ = NULL, *next_occ;
  EXP_OCCURS_ITER exp_occ_iter;
  exp_occ_iter.Init(Real_occurs().Head());
  for (exp_occ = exp_occ_iter.First();
       ! exp_occ_iter.Is_Empty();
       exp_occ = next_occ) {
    next_occ = exp_occ_iter.Next();

    if (exp_occ->Stmt() == stmt) { // found it, remove from list
      Real_occurs().Remove(prev_occ, exp_occ);
      return TRUE;
    }
    else
      prev_occ = exp_occ;
  }
  return FALSE;
}

BOOL
EXP_WORKLST::Remove_phi_pred_occurrence( STMTREP *stmt)
{
  EXP_OCCURS *exp_occ, *prev_occ = NULL, *next_occ;
  EXP_OCCURS_ITER exp_occ_iter;
  exp_occ_iter.Init(Phi_pred_occurs().Head());
  for (exp_occ = exp_occ_iter.First();
       ! exp_occ_iter.Is_Empty();
       exp_occ = next_occ) {
    next_occ = exp_occ_iter.Next();

    if (exp_occ->Encl_stmt_set()
	&& (exp_occ->Stmt() == stmt)) { // found it, remove from list
      Phi_pred_occurs().Remove(prev_occ, exp_occ);
      return TRUE;
    }
    else
      prev_occ = exp_occ;
  }
  return FALSE;
}


// compare this with w in terms of weight.
BOOL
EXP_WORKLST::Weight_less_than(EXP_WORKLST *w)
{
  if (this->Has_const()) return TRUE;
  if (w->Has_const()) return FALSE;
  if (this->Has_rconst()) return TRUE;
  if (w->Has_rconst()) return FALSE;
  if (this->Has_lda()) return TRUE;
  if (w->Has_lda()) return FALSE;

  // should consider CK_VAR of loop invariant in the future
  return TRUE;
}


void
EXP_WORKLST_CONTAINER::Insert_sorted(EXP_WORKLST *worklst)
{
  // sort according to the weight of the expression
  // const < lda < rconst < no_const
  EXP_WORKLST_ITER worklst_iter(this);
  EXP_WORKLST *tmp, *prev = NULL;
  FOR_ALL_NODE(tmp, worklst_iter, Init()) {
    if (worklst->Weight_less_than(tmp))
      break;
    else
      prev = tmp;
  }
  if (prev == NULL)
    this->Prepend(worklst);
  else if (prev == Tail())
    this->Append(worklst);
  else
    prev->Insert_After(worklst);
}


// Lookup the rehashed form of the expr, if its rehashed form is different.
// Otherwise returns NULL.
//
CODEREP *
ETABLE::Lookup_rehash_expr(CODEREP *cr)
{
  REHASH_CACHE_LIST *p = _rehash_cache;
  while (p != NULL) {
    if (p->Cr() == cr) 
      return p->Rehash_cr();
    p = p->Next();
  }
  // It might reach this point when there is an expression occured
  // multiple times in the statement, but does not need to rehash.
  return NULL;
}


// Hash cr and find exp_worklst for it, if none then create one and add it.
// If exp_iter is non-null then put at beginning of worklst.
// (called by ETABLE::Insert_real_occurrence and Append_real_occurrence)
EXP_WORKLST*
ETABLE::Get_worklst(CODEREP *cr, BOOL urgent, BOOL lookup_only)
{
  // Special case worklists in the VNFRE namespace
  if (Pre_kind() == PK_VNFRE)
    return VNFRE::get_worklst(cr); // Worklists are in the VNFRE namespace
  
  EXP_WORKLST_NODE_CONTAINER bucket;
  IDX_32 hash_idx = Hash_exp(cr);
  bucket.Init_Head(Exp_hash_bucket(hash_idx));
  EXP_WORKLST_NODE *worklist_node = bucket.Find_exp_worklst(cr);
  EXP_WORKLST      *worklist = (worklist_node)? worklist_node->Node() : NULL;

  if (lookup_only) return worklist;

  // if it is a new expression, create a new WORKLST
  // or we found it and it's the one LFTR wants to insert but it is
  // currently being processed so create a new one anyway.
  if (worklist == NULL) {
    worklist = (EXP_WORKLST *)CXX_NEW(EXP_WORKLST(Alloc_e_num(),cr,Pre_kind()),
				      Etable_pool());
    worklist->Exp()->Set_e_num(worklist->E_num());  // because cr has the wrong e-num
    if (Pre_kind() == PK_LPRE && WOPT_Enable_Shrink && cr->Kind() == CK_VAR &&
	Opt_stab()->Aux_stab_entry(cr->Aux_id())->Lr_shrink_cand())
      worklist->Set_LR_shrink_cand();

    worklist_node = (EXP_WORKLST_NODE *)CXX_NEW(EXP_WORKLST_NODE(worklist),
                                                Etable_pool());
    bucket.Append(worklist_node, hash_idx, this);	// Add to hash table
    if (urgent) {
      worklist->Set_is_urgent();
      //_urgent_worklst.Append(worklist);
      _urgent_worklst.Insert_sorted(worklist);
    }
    else
      _exp_worklst.Append(worklist);		// Append to list of worklsts
  }
  else if (worklist->Is_processed()) {          // Second order effect
    if (!WOPT_Enable_Second_Order) return NULL; // it is disabled
    worklist->Clear_flags();
    worklist->Set_e_num(Alloc_e_num());
    worklist->Init_e_version();

    // reinitalize the E_num field in all the occurrence node
    EXP_OCCURS *exp_occ;
    EXP_OCCURS_ITER occur_iter;
    FOR_ALL_NODE(exp_occ, occur_iter, Init(worklist->Real_occurs().Head()))
      exp_occ->Occurrence()->Set_e_num(worklist->E_num());

    if (urgent) {
      worklist->Set_is_urgent();
      //_urgent_worklst.Append(worklist);
      _urgent_worklst.Insert_sorted(worklist);
    }
    else
      _exp_worklst.Append(worklist);		// Append to list of worklsts
  }

  // if it is already in the worklist, ignore the boolean urgent.

  return worklist;
}

// Remve real occurrence in one of the worklist container.  
// we have multiple worklist based on priority.
BOOL
ETABLE::Remove_real_occurrence(EXP_WORKLST_CONTAINER *worklist,
                               CODEREP *old_cr, STMTREP *stmt)
{
  // first find the right exp_worklst
  EXP_WORKLST *worklst, *prev_worklst = NULL;

  EXP_WORKLST_ITER worklst_iter(worklist->Head());
  FOR_ALL_NODE(worklst, worklst_iter, Init()) {
    if (worklst->Is_the_same_as(old_cr)) {
      if (worklst->Remove_real_occurrence(stmt)) {
        Is_Trace(Lftr()->Trace(),(TFile,
                "ETABLE::Remove_real_occurrence, after removed occurrence\n"));
        Is_Trace_cmd(Lftr()->Trace(),worklst->Print(TFile));
      }

      if (worklst->Real_occurs().Head() == NULL) {
        // now we need to remove the entire worklst because we removed 
        // the last real occurrence
        worklist->Remove(prev_worklst, worklst);
        worklst->Set_is_processed();
      }
      return TRUE; // didn't find it and that is ok
    }
    prev_worklst = worklst;
  }
  return FALSE;
}

// see if a comparison op does not cause extra instruction because it can
// be subsumed by a branch instruction
BOOL 
Subsumable_by_branch(CODEREP *cr)
{
#ifdef TARG_MIPS
  // The following only applies to the MIPS instruction set
  // It might make sense to move target dependent code into
  // a separate file.  -Raymond 12/3/98.
  if (cr->Kind() != CK_OP)
    return FALSE;
  const OPERATOR opr = cr->Opr();
  if ((opr == OPR_EQ || opr == OPR_NE || opr == OPR_LNOT) &&
      MTYPE_is_integral(cr->Dsctyp()))
    return TRUE;
  if ((opr == OPR_LT || opr == OPR_LE || opr == OPR_GT || opr == OPR_GE) &&
      MTYPE_is_integral(cr->Dsctyp())) {
    CODEREP *opnd = cr->Opnd(0);
    if (opnd->Kind() == CK_CONST && opnd->Const_val() == 0)
      return TRUE;
    opnd = cr->Opnd(1);
    if (opnd->Kind() == CK_CONST && opnd->Const_val() == 0)
      return TRUE;
  }
#endif
  return FALSE;
}

// Search real occurrences for original comparison and remove it.
// Note: it may not be found, it is possible for a comp occur to exist
// without a real occur. For example, if a < b is partially redundant,
// the new real occur should not be generated for the inserted expression
// because it has already been processed. However, a comp occur should
// exits for it.
void
ETABLE::Remove_real_occurrence(CODEREP *old_cr, STMTREP *stmt)
{
  if (Remove_real_occurrence(Urgent_worklst(), old_cr, stmt)) return;
  if (Remove_real_occurrence(Exp_worklst(), old_cr, stmt)) return;

  // it may still be in processed worklist
  EXP_WORKLST *worklist = Get_worklst(old_cr, FALSE, TRUE);
  if (worklist) {
    worklist->Remove_real_occurrence(stmt);
    return; // didn't find it and that is ok
  }

  // This case is OK, do not assert
  if ((stmt->Op() == OPC_TRUEBR || stmt->Op() == OPC_FALSEBR) &&
      Subsumable_by_branch(old_cr))
    return;

#if defined(TARG_IA32) || defined(TARG_X8664)
  // This case is OK too, do not assert
  if ((stmt->Op() == OPC_TRUEBR || stmt->Op() == OPC_FALSEBR) &&
      old_cr->Kind() == CK_OP &&
      (old_cr->Opr() == OPR_EQ || old_cr->Opr() == OPR_NE ||
       old_cr->Opr() == OPR_LT || old_cr->Opr() == OPR_LE || 
       old_cr->Opr() == OPR_GT || old_cr->Opr() == OPR_GE)) 
    return;
#endif

  // Cannot not find it in urgent, or regular worklist.
  // This is a problem happen in earlier phase
  Is_True(FALSE, ("ETABLE::Remove_real_occurrences, logic error"));
}
//Bug# 1153
#ifdef KEY
void
ETABLE::Mark_phi_live(PHI_NODE *phi)
{  
  if ( phi->Live() )
    return;
                                                                                                                                                             
  phi->Set_live();
  for ( INT pkid = 0; pkid < phi->Size(); pkid++ ) {
    CODEREP *cr;
    cr = phi->OPND(pkid);
    if (cr->Is_flag_set(CF_DEF_BY_PHI))
      Mark_phi_live(cr->Defphi());
  }
}
#endif
//  Generate an expr with current versions for the opnd_num-th operand
//  of the phi_occur node.  Real_occ is the expr with current versions
//  at the phi result.
//
CODEREP *
ETABLE::Generate_cur_expr(const BB_NODE *bb, INT opnd_num, CODEREP *newcr, BOOL fix_zero_ver)
{
  switch (newcr->Kind()) {
  case CK_VAR:
    {
      PHI_NODE *var_phi = Lookup_var_phi(bb, newcr->Aux_id());
      if (var_phi != NULL) {
	Is_True(var_phi->Live(), 
		("ETABLE::Generate_cur_expr: encounter dead phi "
		 "node bb:%d Aux:%d.", bb->Id(), newcr->Aux_id()));
	Is_True(var_phi->OPND(opnd_num) != NULL,
		("ETABLE::Generate_cur_expr: encounter bad phi opnd"
		 "node bb:%d Aux:%d.", bb->Id(), newcr->Aux_id()));
	if (fix_zero_ver && var_phi->OPND(opnd_num)->Is_flag_set(CF_IS_ZERO_VERSION))
	  Htable()->Fix_zero_version(var_phi, opnd_num);
#ifdef KEY // bug 7916
	if (var_phi->OPND(opnd_num)->Dsctyp() == MTYPE_UNKNOWN ||
	    var_phi->OPND(opnd_num)->Is_flag_set(CF_MADEUP_TYPE)) {
	  Is_True(newcr->Dsctyp() != MTYPE_UNKNOWN,
		  ("ETABLE::Generate_cur_expr: cannot fix types for phi opnd "
		   "%d at node bb:%d Aux:%d.", opnd_num, bb->Id(), 
		   newcr->Aux_id()));
	  var_phi->OPND(opnd_num)->Set_dtyp(newcr->Dtyp());
	  var_phi->OPND(opnd_num)->Set_dsctyp(newcr->Dsctyp());
	  var_phi->OPND(opnd_num)->Set_lod_ty(newcr->Lod_ty());
	  var_phi->OPND(opnd_num)->Set_field_id(newcr->Field_id());
	  if (newcr->Bit_field_valid())
	    var_phi->OPND(opnd_num)->Set_bit_field_valid();
	  var_phi->OPND(opnd_num)->Set_sign_extension_flag();
	  var_phi->OPND(opnd_num)->Reset_flag(CF_MADEUP_TYPE);
	}
#endif
	newcr = var_phi->OPND(opnd_num);
      }
    }
    break;
  case CK_IVAR:
    {
      CODEREP *ilod_base = newcr->Ilod_base();
      CODEREP *istr_base = newcr->Istr_base();
      if (ilod_base == istr_base) {
	if (ilod_base->Kind() == CK_VAR) {
	  PHI_NODE *var_phi = Lookup_var_phi(bb, ilod_base->Aux_id());
	  if (var_phi != NULL) {
	    Is_True(var_phi->Live(),
		    ("ETABLE::Generate_cur_expr: encounter dead phi node."));
	    Is_True(var_phi->OPND(opnd_num) != NULL,
		    ("ETABLE::Generate_cur_expr: encounter bad phi opnd"
		     "node bb:%d Aux:%d.", bb->Id(), ilod_base->Aux_id()));
	    if (fix_zero_ver && var_phi->OPND(opnd_num)->Is_flag_set(CF_IS_ZERO_VERSION))
	      Htable()->Fix_zero_version(var_phi, opnd_num);
	    newcr->Set_ilod_base(var_phi->OPND(opnd_num));
	    newcr->Set_istr_base(var_phi->OPND(opnd_num));
	  }
	}
      } else {
	if (ilod_base && ilod_base->Kind() == CK_VAR) {
	  PHI_NODE *var_phi = Lookup_var_phi(bb, ilod_base->Aux_id());
	  if (var_phi != NULL) {
	    Is_True(var_phi->Live(),
		    ("ETABLE::Generate_cur_expr: encounter dead phi node."));
	    Is_True(var_phi->OPND(opnd_num) != NULL,
		    ("ETABLE::Generate_cur_expr: encounter bad phi opnd"
		     "node bb:%d Aux:%d.", bb->Id(), ilod_base->Aux_id()));
	    if (fix_zero_ver && var_phi->OPND(opnd_num)->Is_flag_set(CF_IS_ZERO_VERSION))
	      Htable()->Fix_zero_version(var_phi, opnd_num);
	    newcr->Set_ilod_base(var_phi->OPND(opnd_num));
	  }
	}
	if (istr_base && istr_base->Kind() == CK_VAR) {
	  PHI_NODE *var_phi = Lookup_var_phi(bb, istr_base->Aux_id());
	  if (var_phi != NULL) {
	    Is_True(var_phi->Live(),
		    ("ETABLE::Generate_cur_expr: encounter dead phi node."));
	    Is_True(var_phi->OPND(opnd_num) != NULL,
		    ("ETABLE::Generate_cur_expr: encounter bad phi opnd"
		     "node bb:%d Aux:%d.", bb->Id(), istr_base->Aux_id()));
	    if (fix_zero_ver && var_phi->OPND(opnd_num)->Is_flag_set(CF_IS_ZERO_VERSION))
	      Htable()->Fix_zero_version(var_phi, opnd_num);
	    newcr->Set_istr_base(var_phi->OPND(opnd_num));
	  }
	}
      }
    }
    // update the mu-node too.
    if (newcr->Ivar_mu_node() != NULL) {
      PHI_NODE *var_phi = Lookup_var_phi(bb, newcr->Ivar_occ()->Aux_id());
      if (var_phi != NULL) {
#ifdef KEY
        if (!var_phi->Live())
          Mark_phi_live(var_phi);
#endif
	Is_True(var_phi->Live(),
		("ETABLE::Generate_cur_expr: encounter dead phi node."));
	Is_True(var_phi->OPND(opnd_num) != NULL,
		("ETABLE::Generate_cur_expr: encounter bad phi opnd"
		 "node bb:%d Aux:%d.", bb->Id(), newcr->Ivar_occ()->Aux_id()));
	if (fix_zero_ver && var_phi->OPND(opnd_num)->Is_flag_set(CF_IS_ZERO_VERSION))
	  Htable()->Fix_zero_version(var_phi, opnd_num);
	newcr->Ivar_mu_node()->Set_OPND(var_phi->OPND(opnd_num));
      }
    }
    break;
  case CK_OP:
    {
      for (INT i = 0; i < newcr->Kid_count(); i++) {
	CODEREP *kid = newcr->Opnd(i);
	switch (kid->Kind()) {
	case CK_VAR:
	  {
	    PHI_NODE *var_phi = Lookup_var_phi(bb, kid->Aux_id());
	    if (var_phi != NULL) {
	      Is_True(var_phi->Live(), 
		      ("ETABLE::Generate_cur_expr: encounter dead phi "
		       "node bb:%d Aux:%d.", bb->Id(), kid->Aux_id()));
	      Is_True(var_phi->OPND(opnd_num) != NULL,
		      ("ETABLE::Generate_cur_expr: encounter bad phi opnd"
		       "node bb:%d Aux:%d.", bb->Id(), kid->Aux_id()));
	      if (fix_zero_ver && var_phi->OPND(opnd_num)->Is_flag_set(CF_IS_ZERO_VERSION))
		Htable()->Fix_zero_version(var_phi, opnd_num);
#ifdef KEY // bug 3070
	      if (var_phi->OPND(opnd_num)->Dsctyp() == MTYPE_UNKNOWN ||
		  var_phi->OPND(opnd_num)->Is_flag_set(CF_MADEUP_TYPE)) {
		var_phi->OPND(opnd_num)->Set_dtyp(kid->Dtyp());
		var_phi->OPND(opnd_num)->Set_dsctyp(kid->Dsctyp());
		var_phi->OPND(opnd_num)->Set_lod_ty(kid->Lod_ty());
		var_phi->OPND(opnd_num)->Set_field_id(kid->Field_id());
		if (kid->Bit_field_valid())
		  var_phi->OPND(opnd_num)->Set_bit_field_valid();
		var_phi->OPND(opnd_num)->Set_sign_extension_flag();
		var_phi->OPND(opnd_num)->Reset_flag(CF_MADEUP_TYPE);
	      }
#endif
	      newcr->Set_opnd(i, var_phi->OPND(opnd_num));
	    }
	  }
	  break;
	case CK_CONST:
	case CK_RCONST:
	case CK_LDA:
	  break;
	case CK_IVAR:
	  {
	    const CODEREP *ilod_base = kid->Ilod_base(); 
	    if (ilod_base && ilod_base->Kind() == CK_VAR) {
	      PHI_NODE *var_phi = Lookup_var_phi(bb, ilod_base->Aux_id());
	      if (var_phi != NULL) {
		Is_True(var_phi->Live(),
			("ETABLE::Generate_cur_expr: encounter dead phi node."));
		Is_True(var_phi->OPND(opnd_num) != NULL,
			("ETABLE::Generate_cur_expr: encounter bad phi opnd"
			 "node bb:%d Aux:%d.", bb->Id(), ilod_base->Aux_id()));
		if (fix_zero_ver && var_phi->OPND(opnd_num)->Is_flag_set(CF_IS_ZERO_VERSION))
		  Htable()->Fix_zero_version(var_phi, opnd_num);
		kid->Set_ilod_base(var_phi->OPND(opnd_num));
	      }
	    }
	  }

	  // a CK_IVAR must have mu-node unless it is a by-value PARM node
	  Is_True(kid->Ivar_mu_node() != NULL || kid->Opr() == OPR_PARM,
		  ("ETABLE::Generate_cur_expr: non PARM CK_IVAR must have a mu-node."));

	  // update the mu-node too
	  if (kid->Ivar_mu_node() != NULL) {
	    const PHI_NODE *var_phi = Lookup_var_phi(bb, kid->Ivar_occ()->Aux_id());
	    if (var_phi != NULL) {
	      Is_True(var_phi->Live(),
		      ("ETABLE::Generate_cur_expr: encounter dead phi node."));
	      kid->Ivar_mu_node()->Set_OPND(var_phi->OPND(opnd_num));
	    }
	  }
	  break;
	default:
	  Is_True(FALSE, ("unexpected CK_KIND."));
	}
      }
    }
    break;
  default:
    Is_True(FALSE, ("unexpected CK_KIND."));
  }

  // Trace new expr generated
  if (Tracing()) {
    fprintf(TFile, "ETABLE::Generate_cur_expr for phi opnd %d in BB%d\n",
	    opnd_num, bb->Id());
    newcr->Print(10, TFile);
  }

  if (newcr->Kind() != CK_VAR)
    newcr->Set_coderep_id(0);
  return newcr;
}


CODEREP *
ETABLE::Get_cached_cur_expr(const BB_NODE *bb, INT opnd_num)
{
  BB_NODE *nth_pred = bb->Nth_pred(opnd_num);
  // see if we already generated one
  return Phi_pred_cr( nth_pred );
}

void
ETABLE::Update_cached_cur_expr(const BB_NODE *bb, INT opnd_num, CODEREP *newcr)
{
  BB_NODE *nth_pred = bb->Nth_pred(opnd_num);
  Set_phi_pred_cr( nth_pred, newcr );
}

CODEREP *
ETABLE::Alloc_and_generate_cur_expr(const CODEREP *result_expr,
				    const BB_NODE *bb, INT opnd_num,
				    MEM_POOL *mpool,
				    BOOL fix_zero_ver)
{
  BB_NODE *nth_pred = bb->Nth_pred(opnd_num);
  CODEREP *newcr;

  // see if we already generated one
  CODEREP *cached_newcr = Phi_pred_cr( nth_pred );
  if (!fix_zero_ver && cached_newcr != NULL) {
    return cached_newcr;
  }

  // allocate and copy from result_occ.
  if (cached_newcr == NULL) {
    if ( inCODEKIND(result_expr->Kind(), CK_VAR|CK_LDA|CK_RCONST|CK_CONST) ) {
      newcr = (CODEREP *) result_expr;
    } else {
      newcr = CXX_NEW_VARIANT(CODEREP(*result_expr),
			      result_expr->Extra_space_used(), mpool);
    }
    
    if (newcr->Kind() == CK_IVAR) {

      // keep the original ivar_occ!
      // Rehash_tree_rec will clone the ivar_occ data structure.
      newcr->Set_ivar_mu_node(CXX_NEW(MU_NODE(*result_expr->Ivar_mu_node()), mpool));
					      
    } else if (newcr->Kind() == CK_OP && (newcr->Opr() == OPR_INTRINSIC_OP
#ifdef KEY
	       || newcr->Opr() == OPR_PURE_CALL_OP
#endif
      )) {
      for (INT i = 0; i < newcr->Kid_count(); i++) {
	CODEREP *kid = result_expr->Opnd(i);
	CODEREP *newkid = CXX_NEW_VARIANT(CODEREP(*kid), kid->Extra_space_used(), mpool);
	newcr->Set_opnd(i, newkid);
	if (kid->Ivar_mu_node() != NULL) 
	  newkid->Set_ivar_mu_node(CXX_NEW(MU_NODE(*kid->Ivar_mu_node()), mpool));
	else 
	  newkid->Set_ivar_mu_node(NULL);
      }
    }
  } else
    newcr = cached_newcr;  // call Generate_cur_expr again to fix_zero_ver.

  if (!inCODEKIND(newcr->Kind(), CK_LDA|CK_RCONST|CK_CONST)) 
    newcr = Generate_cur_expr(bb, opnd_num, newcr, fix_zero_ver);

  // cache this generated cr
  Set_phi_pred_cr( nth_pred, newcr );

  Is_True(Def_before_use(newcr, nth_pred),
	  ("ETABLE::Alloc_and_generate_cur_expr: Generated use before def"));

  return newcr;
}

// =====================================================================
// The following routine exists because getting second-order effects
// from EPRE requires that we perform some simple and limited
// constant- and copy-propagation on the expressions we insert, but
// LFTR and possibly strength reduction assume the inserted
// expressions will take the same form (up to CVT/CVTL) as the
// canonicalized form of the current expression being processed.
//
// Therefore, we go ahead and make our insertions without
// copy/constant propagation, and allow LFTR and SR to complete their
// jobs during CodeMotion. After CodeMotion is complete, we revisit
// those inserted occurrences that might need propagation, and
// propagate them using CODEMAP::Rehash_tree() with the
// enable-cprop parameter switched on.
// =====================================================================

void
ETABLE::Schedule_for_ocopy(EXP_OCCURS *const occ)
{
  Is_True(occ != NULL, ("ETABLE::Schedule_for_ocopy: must not defer NULL"));
  Is_Trace(Tracing(), (TFile, "Deferring\n"));
  Is_Trace_cmd(Tracing(), occ->Print(TFile));
  Is_Trace_cmd(Tracing(), occ->Occurrence()->Print(3, TFile));

  Deferred_ocopy_occurs()->Push(occ);
}

// =====================================================================
// Two sorts of occcurrences can be saved away for deferred copy
// propagation: Occurrences inserted to eliminate redundancy (Finalize
// step), and occurrences inserted by hoisting (Hoisting step). The
// former sort are phi-pred occurrences transformed to real, and the
// latter are genuine real occurrences inserted into the EXP_WORKLST
// via linear search. For both kinds, the treatment here is the same:
// Get the RHS of the STMTREP from the occurrence and
// htable->Rehash_tree() it, then Bottom_up_cr() the result of that.
// =====================================================================

/* ARGSUSED */
void
ETABLE::Perform_deferred_ocopy_and_get_new_exprs(EXP_WORKLST *const worklist)
{
  while (!Deferred_ocopy_occurs()->Is_Empty()) {
    EXP_OCCURS *occ  = Deferred_ocopy_occurs()->Pop();

    Is_True(occ != NULL, ("ETABLE::Perform_deferred_ocopy...: NULL was deferred"));

    Is_Trace(Tracing(), (TFile, "\n<<<<<<<<< Retrieve deferred (2nd order effects): >>>>>>>>>\n"));

    Is_Trace_cmd(Tracing(), occ->Print(TFile));
    Is_Trace_cmd(Tracing(), occ->Occurrence()->Print(3, TFile));

    STMTREP    *stmt;

    if (occ->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR) {
      stmt = occ->Stmt();
    }
    else {
      stmt = occ->Bb()->Last_stmtrep();
      if (! OPERATOR_is_scalar_store (stmt->Opr())) {
	// stmt is a branch that ends the BB.
	Is_True(stmt->Op() == OPC_GOTO ||
		stmt->Op() == OPC_COMPGOTO ||
		stmt->Op() == OPC_AGOTO ||
		stmt->Op() == OPC_TRUEBR ||
		stmt->Op() == OPC_FALSEBR ||
		stmt->Op() == OPC_REGION_EXIT,
		("ETABLE::Perform_deferred_ocopy_and_get_new_exprs: "
		 "BB with insertion ends anomalously"));
	stmt = stmt->Prev();
	Is_True(OPERATOR_is_scalar_store (stmt->Opr()),
		("ETABLE::Perform_deferred_ocopy_and_get_new_exprs: "
		 "Can't find inserted store"));
      }
      else {
	// stmt must be the save to temp. Do nothing.
      }
    }
    Is_True((Opt_stab()->Aux_stab_entry(stmt->Lhs()->Aux_id())->EPRE_temp() &&
	     worklist->Pre_kind() == PK_EPRE) ||
	    worklist->Pre_kind() == PK_LPRE,
	    ("ETABLE::Perform_deferred_ocopy_and_get_new_exprs: "
	     "Found store, but store isn't inserted"));
    Is_True(worklist->Is_the_same_as(stmt->Rhs()) ||
	    ((stmt->Rhs()->Opr() == OPR_CVT ||
	      stmt->Rhs()->Opr() == OPR_CVTL) &&
	     worklist->Is_the_same_as(stmt->Rhs()->Opnd(0))),
	    ("ETABLE::Perform_deferred_ocopy_and_get_new_exprs: "
	     "Store to temp must save current expression"));
    Is_True(occ->Occurrence() == stmt->Rhs() ||
	    ((stmt->Rhs()->Opr() == OPR_CVT ||
	      stmt->Rhs()->Opr() == OPR_CVTL) &&
	     occ->Occurrence() == stmt->Rhs()->Opnd(0)),
	    ("ETABLE::Perform_deferred_ocopy_and_get_new_exprs: "
	     "Occurrence coderep must match RHS of statement"));

    // Now stmt is the inserted store to temp. We need to rehash its
    // RHS and Bottom_up_cr() the result if it got folded.
    BOOL     tree_changed = occ->Rehash_changed_expr();
    CODEREP *new_rhs = Htable()->Rehash_tree(stmt->Rhs(),
					     WOPT_Enable_Output_Copy,
					     &tree_changed, stmt->Bb());

    // Are the following two lines right?
    stmt->Rhs()->DecUsecnt();
    new_rhs->IncUsecnt();

    Is_Trace(Tracing(), (TFile, "New RHS derived from deferred:\n"));
    Is_Trace_cmd(Tracing(), new_rhs->Print(3, TFile));

    stmt->Set_rhs(new_rhs);
    if (tree_changed) {
      if (occ->Occ_kind() == EXP_OCCURS::OCC_PHI_PRED_OCCUR) {
	Is_True(occ->Inserted_computation(),
		("ETABLE::Deferred_ocopy: Phi pred must be inserted\n"));
	occ->Set_enclose_bb(occ->Stmt()->Bb());
	occ->Reset_encl_stmt_set();
      }
      else {
	Is_True(occ->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR,
		("ETABLE::Deferred_ocopy: Non-phi-pred must "
		 "be real occurrence\n"));
	// This occurrence was computed and saved, and its tree changed.
	// At this point we set its Delete_comp() flag so the
	// occurrence will be removed from the present worklist by
	// EXP_WORKLST::Remove_occurs() (because the tree changed).
	// Bottom_up_cr will place the new expression into the proper
	// worklist if need be.
        occ->Set_delete_comp();
      }
      Bottom_up_cr(stmt, 0, new_rhs, FALSE, URGENT_INSERT,
		   0, OPCODE_UNKNOWN, worklist->Exclude_sr_cand());
    }
#ifdef KEY // bug 4674: restore original expression in occurrence node so later
    	   // reapplication of SSAPRE due to second order effect will be OK
    if (occ->Occurrence()->Kind() != worklist->Exp()->Kind()) {
      Is_True(occ->Occurrence()->Opr() == OPR_CVT ||
              occ->Occurrence()->Opr() == OPR_CVTL,
	      ("ETABLE::Deferred_ocopy: inconsistent expression operation\n"));
      occ->Set_occurrence(occ->Occurrence()->Opnd(0));
    }
#endif
  }
}


void
ETABLE::Perform_deferred_cfold()
{
  while (!Deferred_cfold_stmts()->Is_Empty()) {
    STMTREP *sr = Deferred_cfold_stmts()->Pop();
    FOLD ftmp;
    CODEREP *expr = ftmp.Fold_Expr(sr->Rhs());
    if (expr != NULL) 
      sr->Set_rhs(expr);
  }
}


ETABLE::ETABLE(CFG *cfg,
               OPT_STAB  *opt_stab,
               CODEMAP   *htable,
	       ALIAS_RULE *ar,
               IDX_32     exp_hash_size,
               MEM_POOL  *etable_pool,
               MEM_POOL  *per_expr_pool,
	       MEM_POOL  *etable_local_pool,
	       COMP_UNIT *comp_unit,
               PRE_KIND   pre_kind):
	       _phi_work_set(cfg->Total_bb_count(), cfg, etable_pool,
			     BBNS_EMPTY),
	       _var_phi_set(cfg->Total_bb_count(), cfg, etable_pool,
			    BBNS_EMPTY),
               _exp_hash(exp_hash_size, etable_pool),
               _pre_kind(pre_kind),
	       _nil_exp_phi_opnd(NULL, NULL, NULL, TRUE),
	       _occ_freelist(etable_pool),
	       _deferred_cfold_stmts(etable_pool),
	       _deferred_ocopy_occurs(etable_pool)
{
  _cfg = cfg;
  _opt_stab = opt_stab;
  _htable = htable;
  _ssu = NULL;
  _arule = ar;
  _etable_pool = etable_pool;
  _per_expr_pool = per_expr_pool;
  _etable_local_pool = etable_local_pool;
  _cur_e_num = ILLEGAL_E_NUMBER; 
  _dpo_exp_phi = (EXP_OCCURS **)
        CXX_NEW_ARRAY(EXP_OCCURS*, Cfg()->Total_bb_count(), _etable_pool);
  _phi_pred_cr = 
        CXX_NEW_ARRAY(CODEREP*, Cfg()->Total_bb_count(), _etable_pool);
  _nil_exp_phi_opnd.Init();	// will make e_version ILLEGAL_E_VERSION
  if (pre_kind == PK_EPRE || pre_kind == PK_VNFRE)
    _tracing = Get_Trace( TP_GLOBOPT, EPRE_DUMP_FLAG );
  else if (pre_kind == PK_SPRE)
    _tracing = Get_Trace( TP_GLOBOPT, SPRE_DUMP_FLAG );
  else // if (pre_kind == PK_SPRE)
    _tracing = Get_Trace( TP_GLOBOPT, LPRE_DUMP_FLAG );
  _comp_unit = comp_unit;
  _num_cse_reloads = _num_cse_saves = _num_inserted_saves = _num_temp_phis = _num_hoisted = _num_temp_owners = 0;

  LPRE_set_do_loads(pre_kind == PK_LPRE);
  LPRE_set_do_consts(pre_kind == PK_LPRE);
  _complex_loop_map.clear();
}

ETABLE::~ETABLE(void)
{
  Perform_deferred_cfold();

  //CXX_DELETE_ARRAY(_dpo_exp_phi,_etable_pool);
  //CXX_DELETE_ARRAY(_phi_pred_cr,_etable_pool);
}

EXP_PHI*
ETABLE::Lookup_exp_phi(const BB_NODE *bb, const CODEREP *cr) const
{
  EXP_PHI *exp_phi = bb->Exp_phi();
  if (exp_phi == NULL) return NULL;
  Is_True(exp_phi->Bb() == bb,
	  ("ETABLE::Lookup_exp_phi: Inconsistent Exp_phi() on BB%d",
	   bb->Id()));
  Is_True(( (Pre_kind() == PK_VNFRE &&
	     (cr == NULL || 
	      exp_phi->E_num() == VNFRE::get_valnum(cr))) ||
	    exp_phi->E_num() == cr->E_num() ),
	  ("ETABLE::Lookup_exp_phi: Inconsistent E_num() for EXP_PHI "
	   "in BB%d", bb->Id()));
  return exp_phi;
}

// return a value for hashing purpose
IDX_32
ETABLE::Exp_hashvalue(const CODEREP *cr)
{
  if (inCODEKIND(cr->Kind(), CK_IVAR|CK_OP)) 
    return cr->Bitpos() << 6;
  if (cr->Kind() == CK_VAR)
    return cr->Aux_id() << 6;
  if (cr->Kind() == CK_CONST)
    return cr->Const_val() << 6;
  return cr->Coderep_id() << 6;
}

IDX_32
ETABLE::Hash_exp(const CODEREP *cr)
{
  if (cr->Kind() == CK_IVAR) {
    CODEREP *bas = cr->Istr_base() ? cr->Istr_base() : cr->Ilod_base();
    return (Exp_hashvalue(bas) + cr->Offset()) % _exp_hash.Size();
  } else if (cr->Kind() == CK_VAR) {
    return (cr->Aux_id() % _exp_hash.Size());
  } else if (inCODEKIND(cr->Kind(), CK_LDA|CK_CONST|CK_RCONST)) {
    return (Exp_hashvalue(cr) % _exp_hash.Size());
  }
  // CK_OP, including intrinsic ops
  IDX_32 hvalue = 0;
  for (INT32 i = 0; i < cr->Kid_count(); i++)
    hvalue += Exp_hashvalue(cr->Opnd(i));
  return (cr->Op() + hvalue) % _exp_hash.Size();
}

CODEREP *
ETABLE::Rehash_exp(CODEREP *new_cr, UINT32 gvn, BOOL canon) const
{
   // We should never see calls to htable->Rehash() in any ETABLE or CSE
   // routine other than here.  This allows us to keep other datastructures
   // up to date with new versions of codereps.
   //
   CODEREP * const rehashed_cr = Htable()->Rehash(new_cr, canon);
   
   if (Pre_kind() == PK_VNFRE && rehashed_cr != new_cr)
   {
      VNFRE::add_valnum(rehashed_cr, gvn);
   }
   return rehashed_cr;
}

EXP_OCCURS*
ETABLE::Alloc_occurs_node(void)
{
  EXP_OCCURS *retval;
  if (Occ_freelist()->Is_Empty()) {
    // allocate memory from the memory pool
    retval = CXX_NEW(EXP_OCCURS, Etable_pool());
  }
  else {
    // get from the freelist
    retval = Occ_freelist()->Pop();
  }
  retval->Init();
  return retval;
}

EXP_OCCURS*
ETABLE::Alloc_occurs_node(CODEREP *occurrence)
{
  EXP_OCCURS *retval;
  if (Occ_freelist()->Is_Empty()) {
    // allocate memory from the memory pool
    retval = CXX_NEW(EXP_OCCURS(occurrence,
				(STMTREP *) NULL,
				(EXP_PHI *) NULL,
				TRUE),
		     Etable_pool());
    retval->Clear_temp_cr();  // also clear def_occur as a side effect
    retval->Set_rehash_cost(0);
  }
  else {
    // get from the freelist
    retval = Occ_freelist()->Pop();
    retval->Init();
    retval->Set_occurrence(occurrence);
    retval->Reset_enclosed_in();
    // Not needed -- constructor doesn't bother; why should we?
    // retval->Set_stmt_kid_num(0);
  }
  return retval;
}

// check if needed to set Lftr_non_candidates for this variable based on
// the operation on it
void            
ETABLE::Check_lftr_non_candidate(STMTREP *stmt, CODEREP *cr, OPCODE opc)
{
  if (cr->Kind() != CK_VAR)
    return;
  if (opc == OPCODE_UNKNOWN)
    return;
  // these opcodes correpond to those in STR_RED::Candidate() and 
  // LFTR::Is_comparison()
  switch (OPCODE_operator(opc)) {
  case OPR_ADD: case OPR_SUB: case OPR_MPY: case OPR_NEG: 
  case OPR_LNOT: case OPR_EQ: case OPR_NE: 
  case OPR_GT: case OPR_GE: case OPR_LT: case OPR_LE:
    return;
  case OPR_CVT: 
    if (! (MTYPE_is_float(OPCODE_rtype(opc)) ||
	   MTYPE_is_float(OPCODE_desc(opc))))
      return;
    // fall-thru because some float is involved
  default:  {  // set Lftr_non_candidates() in all enclosing loops
    BB_LOOP *loop = stmt->Bb()->Innermost();
    while (loop) {
      if (loop->Lftr_non_candidates() == NULL)
	loop->Set_lftr_non_candidates(
	    CXX_NEW(IDX_32_SET(Opt_stab()->Lastidx()+1, Cfg()->Loc_pool(),
			       OPTS_FALSE), Cfg()->Loc_pool()));
      loop->Lftr_non_candidates()->Union1D(cr->Aux_id());
      loop = loop->Parent();
    }
    return;
    }
  }
}

#ifdef TARG_SL2

/* this function is used to decide if nth parameter in following intrinsic function can 
  * be etable candiate. These parameter is address  expression and is offset from
  * internal buffer start address.
  */ 

static BOOL 
Is_Intrncall_Nth_Parm_Candidate(INTRINSIC id,  INT nth_parm ) {
      switch(id) {
        case INTRN_C2_LD_C_IMM:
        case INTRN_C2_ST_C_IMM:				
            if(nth_parm == 1) return TRUE;
	     return FALSE;
        case INTRN_C2_LD_V2G_IMM:
        case INTRN_C2_ST_G2V_IMM:		
        case INTRN_C2_LD_G_IMM:
        case INTRN_C2_ST_G_IMM:			
            if(nth_parm == 2) return TRUE;
            return FALSE;
        case INTRN_C2_ST_V_IMM:
	     if(nth_parm == 3) return TRUE;
	     return FALSE;
        case INTRN_C2_LD_V_IMM:
	     if(nth_parm == 4) return TRUE;
	     return FALSE;
         default:
	     return FALSE;
      }
}
#endif 

// ===================================================================
// Bottom-up traversal of CODEREP nodes in a statement to create
// real occurrence list for Etable.
// ===================================================================
void
ETABLE::Bottom_up_stmt(STMTREP *stmt)
{
  const OPERATOR stmt_opr = stmt->Opr();

  		  // New_temp_id();  
		  // alloc new temp_id for this statement,
                  // fix exponential compile-time problem.
                  // See the use of Cur_temp_id in ETABLE::Bottom_up_cr().

  Is_Trace(Tracing(),
	   (TFile, "----- stmt: %s -----\n", OPCODE_name(stmt->Op())));
  Is_Trace_cmd(Tracing(),stmt->Print(TFile));
    
  stmt->Set_stmt_id(Cfg()->Get_stmt_id());

  // for each statement see if they have a rhs and lhs and traverse
  // any expressions there
  CODEREP *rhs = stmt->Rhs();
  BOOL is_iv_update = FALSE;
  if (OPERATOR_is_scalar_istore (stmt_opr) ||
      OPERATOR_is_scalar_store (stmt_opr)) {

    // determine if this statement is an Induction Variable update
    if (WOPT_Enable_New_SR &&
	(is_iv_update = Str_red()->Determine_iv_update(stmt, NULL))) {
      Is_Trace(Tracing(),(TFile,"-Is iv update-\n"));
    }
  }

  if (OPCODE_is_fake(stmt->Op())) {
    for (INT32 i = 0; i < rhs->Kid_count(); i++) {
#ifdef TARG_SL2
           if(rhs->Opr()==OPR_INTRINSIC_CALL && Is_Intrncall_Nth_Parm_Candidate(rhs->Intrinsic(), i)) {
                 continue;
	    }
#endif 
      New_temp_id();
      Bottom_up_cr(stmt, i, rhs->Opnd(i), FALSE, NOT_URGENT, 0, rhs->Op(), FALSE);
    }
  } else if (rhs != NULL) {
    if (!is_iv_update) {
      New_temp_id();
      Bottom_up_cr(stmt, 0, rhs, FALSE, NOT_URGENT, 0, stmt->Op(), FALSE);
    }
    else {
      BOOL all_kids_are_terminal = TRUE;
      for (INT i = 0; i < rhs->Kid_count(); i++) { 
	if (!rhs->Opnd(i)->Is_non_volatile_terminal(Opt_stab())) {
	  all_kids_are_terminal = FALSE;
	  break;
	}
      }
      if (all_kids_are_terminal) {
	// set omitted flag for step 5, 6 to disable avoiding
	// rehashing
	Is_Trace(Tracing(), (TFile, " --- omitting rhs ---\n"));
	rhs->Set_omitted();
      } else {
	/* Perform Bottom_up_cr on operands of RHS, but not on RHS
	 * itself.
	 */
	Is_True(rhs->Kind() == CK_OP,
		("ETABLE::Bottom_up_stmt: iv update must be CK_OP"));
	for (INT i = 0; i < rhs->Kid_count(); ++i) {
	  New_temp_id();
	  Bottom_up_cr(stmt, 0, rhs->Opnd(i), FALSE, NOT_URGENT, 
		       1 /* depth */, rhs->Op(), FALSE);
	}
      }
    }
  }
  if (stmt->Lhs()) {
    Is_Trace(Tracing(),(TFile,"Lhs\n"));
    New_temp_id();
    Bottom_up_cr(stmt, 1, stmt->Lhs(), OPCODE_is_store(stmt->Op()), NOT_URGENT,
		 0, OPCODE_UNKNOWN, FALSE);
  }
}

// ===================================================================
// Bottom-up traversal of CODEREP nodes, assumes CODEREP is not NULL
// ===================================================================
void    
ETABLE::Bottom_up_cr(STMTREP *stmt, INT stmt_kid_num, CODEREP *cr,
		     BOOL is_istore, URGENCY urgent, UINT depth, 
		     OPCODE opc, BOOL no_estr)
{
  Is_True(cr != NULL,("ETABLE::Bottom_up_cr, null CODEREP"));

  Is_Trace(Tracing(),(TFile, "ETABLE::Bottom_up_cr called on ----- cr -----\n"));
  Is_Trace_cmd(Tracing(),cr->Print(2,TFile));
  switch (cr->Kind()) {
    case CK_CONST:	// constant terminal, do nothing
    case CK_RCONST:
    case CK_LDA:
      break;
    case CK_VAR:	// check if needed to set Lftr_non_candidates
      Check_lftr_non_candidate(stmt, cr, opc);
      break;
    case CK_IVAR:	// non-terminal
      {
	BOOL same_base = TRUE; 
	if (cr->Ilod_base() != NULL && cr->Istr_base() != NULL && 
	    cr->Ilod_base() != cr->Istr_base()) {
	  Warn_todo("CODEREP ilod_base != istr_base.");
	  same_base = FALSE;
	}
	if (cr->Opr() == OPR_ILOADX)
	  Warn_todo("ETABLE::Bottom_up_cr: Indexed load.");
	if (!is_istore) {
	  CODEREP *ivar_vsym = cr->Get_ivar_vsym();
	  if (cr->Ilod_base()->Is_non_volatile_terminal(Opt_stab()) &&
	      ! cr->Is_ivar_volatile()) {
	    Check_lftr_non_candidate(stmt, cr->Ilod_base(), cr->Op());
	    if (same_base && WOPT_Enable_Ivar_PRE && cr->Ivar_has_e_num()) {
#ifdef KEY
	      if (WOPT_Enable_Preserve_Mem_Opnds && opc != OPCODE_UNKNOWN &&
		  (OPCODE_operator(opc) == OPR_ADD ||
		   OPCODE_operator(opc) == OPR_SUB ||
		   MTYPE_is_float(OPCODE_rtype(opc)) &&
		   OPCODE_operator(opc) == OPR_MPY)) {
	      }
	      else
#endif
	      if (urgent == NOT_URGENT) {
		Is_Trace(Tracing(),
			 (TFile,"====== ETABLE::Bottom_up_cr, Append coderep:\n"));
		Is_Trace_cmd(Tracing(),cr->Print(2,TFile));
		Append_real_occurrence(cr, stmt, stmt_kid_num, depth);
	      }
	      else {
		Is_Trace(Tracing(),
			 (TFile,"====== ETABLE::Bottom_up_cr, Insert coderep:\n"));
		Is_Trace_cmd(Tracing(),cr->Print(2,TFile));
		Insert_real_occurrence(cr, stmt, stmt_kid_num, depth, FALSE,
				       urgent == URGENT_INSERT);
	      }
	    }
	  } else {
	    Bottom_up_cr(stmt, stmt_kid_num, cr->Ilod_base(), FALSE, urgent, 
			 depth+1, cr->Op(), FALSE);
	  }
	} else {
	  // Ilod_base can be different from Istr_base if the Iload side is
	  // dead, i.e. when Usecnt() == 0
	  CODEREP *ivar_vsym = cr->Get_ivar_vsym();
	  if (cr->Istr_base()->Is_non_volatile_terminal(Opt_stab()) &&
	      ! cr->Is_ivar_volatile()) {
	    Check_lftr_non_candidate(stmt, cr->Istr_base(), cr->Op());
	    if (same_base && WOPT_Enable_Ivar_PRE && cr->Ivar_has_e_num()) {
	      if (!urgent) {
		Append_real_occurrence(cr, stmt, stmt_kid_num, depth, TRUE);
		Is_Trace(Tracing(),
			 (TFile,"====== ETABLE::Bottom_up_cr, Append coderep:\n"));
		Is_Trace_cmd(Tracing(),cr->Print(2,TFile));
	      } else {
		Is_Trace(Tracing(),
			 (TFile,"====== ETABLE::Bottom_up_cr, Insert coderep:\n"));
		Is_Trace_cmd(Tracing(),cr->Print(2,TFile));
		Insert_real_occurrence(cr, stmt, stmt_kid_num, depth, FALSE, urgent);
	      }
	    }
	  } else {
	    Bottom_up_cr(stmt, stmt_kid_num, cr->Istr_base(), FALSE, urgent,
			 depth+1, cr->Op(), FALSE);
	  }
	}
      }
      {
	const OPERATOR ivar_opr = cr->Opr();
	if ( ivar_opr == OPR_MLOAD) {
	  if (cr->Mload_size())
	    Bottom_up_cr(stmt, stmt_kid_num, cr->Mload_size(), FALSE, urgent,
			 depth+1, cr->Op(), FALSE);
	  else
	    Bottom_up_cr(stmt, stmt_kid_num, cr->Mstore_size(), FALSE, urgent,
			 depth+1, cr->Op(), FALSE);
	}
	if ( ivar_opr == OPR_ILOADX)
	  Bottom_up_cr(stmt, stmt_kid_num, cr->Index(), FALSE, urgent,
		       depth+1, cr->Op(), FALSE);
      }
      break;
    case CK_OP:		// non-terminal
      {
	if (OPCODE_is_volatile(cr->Op())) {
	  for (INT32 i=0; i<cr->Kid_count(); i++) { 
	    CODEREP *kid = cr->Opnd(i);
	    if (!kid->Is_non_volatile_terminal(Opt_stab())) {
	      Bottom_up_cr(stmt, stmt_kid_num, kid, FALSE, urgent,
			   depth, cr->Op(), no_estr);
	    }
	  }
	} else {
	  //  A CR has its temp_id field initialized to -1, and might be
	  //  set to the temp_id of statement processed. If
	  //  ETABLE::New_temp_id() guarantees not to repeatedly use an
	  //  id, CRs reached here the first time will have an id
	  //  different from the Cur_temp_id.
	  //  
	  //   cr->Temp_id() != Cur_temp_id()               -->  first visit 
	  //   cr->Temp_id() == Cur_temp_id() && !Is_lcse   -->  second visit
	  //   cr->Temp_id() == Cur_temp_id() && Is_lcse    -->  third+ visit
	  //  
	  if (cr->Temp_id() == Cur_temp_id())
	    if (cr->Is_lcse() && (cr->Max_depth() >= depth ||
				  cr->Max_depth() == 255))
	      return;   // return because this CR has been visited twice
	    else
	      cr->Set_is_lcse();  // this is the second time
	  else {
	    cr->Set_temp_id(Cur_temp_id());  // this is the first time
	    cr->Reset_is_lcse();
	    cr->Set_max_depth(depth);
	  }

	  // remember maximum depth 
	  if (cr->Max_depth() < depth)
	    if (depth > 255)
	      cr->Set_max_depth(255);
	    else cr->Set_max_depth(depth);

	  INT32 i;
	  Is_True(cr->Opr() != OPR_ARRAY,
		  ("ETABLE::Bottom_up_cr: reach an OPR_ARRAY node,"
		   "this is a bug in lowering process"));
	
	  CODEREP *kid;
	  BOOL all_kids_are_terminal = TRUE;
	
	  for (i=0; i<cr->Kid_count(); i++)	{ 
	    kid = cr->Opnd(i);
	    if (!kid->Is_non_volatile_terminal(Opt_stab())) {
	      all_kids_are_terminal = FALSE;
	      Bottom_up_cr(stmt, stmt_kid_num, kid, FALSE, urgent,
			   depth+1, cr->Op(), no_estr);
	    }
	    else Check_lftr_non_candidate(stmt, kid, cr->Op());
	  }
#ifdef TARG_NVISA
	  // check if all the kids are const (used below)
	  BOOL all_kids_are_const = TRUE;
	  switch (cr->Opr()) {
	  case OPR_ADD:
	  case OPR_SUB:
	  case OPR_MPY:
	  case OPR_DIV:
	  case OPR_MOD:
	  case OPR_REM:
	  case OPR_SHL:
	  case OPR_LSHR:
	  case OPR_ASHR:
	  case OPR_BAND:
	  case OPR_BIOR:
	  case OPR_BXOR:
	  case OPR_BNOR:
	  case OPR_BNOT:
	  case OPR_NEG:
		break;
	  default:
		all_kids_are_const = FALSE;
		break;
	  }
	  for (i=0; i<cr->Kid_count(); i++)	{ 
	    kid = cr->Opnd(i);
  	    switch (kid->Kind()) {
	    case CK_CONST: 
		break;
	    case CK_VAR:
		if ( ! ST_is_const_var( Opt_stab()->St(kid->Aux_id())))
			all_kids_are_const = FALSE;
		break;
	    default:
		// TODO: handle depth > 1 
		all_kids_are_const = FALSE;
		break;
	    }
	  }
#endif

	  if (cr->Exp_has_e_num()) {
	    if (all_kids_are_terminal) {
	      OPERATOR opr = cr->Opr();
	      if ((stmt->Op() == OPC_TRUEBR || stmt->Op() == OPC_FALSEBR) &&
		  (depth == 0 || 
		   depth == 1 && stmt->Rhs()->Opr() == OPR_LNOT) && 
		  Subsumable_by_branch(cr)) {
		cr->Set_omitted();
	      } else if (!WOPT_Enable_CSE_FP_comparison &&
			 (opr == OPR_EQ || opr == OPR_NE ||
			  opr == OPR_LT || opr == OPR_LE || 
			  opr == OPR_GT || opr == OPR_GE) &&
			 MTYPE_is_float(cr->Dsctyp())) {
		cr->Set_omitted();
#if defined(TARG_IA32) || defined(TARG_X8664)
	      } else if ((opc == OPC_TRUEBR || opc == OPC_FALSEBR) &&
			 (opr == OPR_EQ || opr == OPR_NE ||
			  opr == OPR_LT || opr == OPR_LE || 
			  opr == OPR_GT || opr == OPR_GE)) {
		cr->Set_omitted();
#endif
#ifdef TARG_NVISA
	      // if all kids are const or const_var, and op is arith, then omit
	      } else if (!WOPT_Enable_Const_Op_PRE && all_kids_are_const) {
		Is_Trace(Tracing(),
			 (TFile,"omit from epre cause all kids are const\n"));
		cr->Set_omitted();
#endif
	      } else if (!urgent) {
		Is_Trace(Tracing(),
			 (TFile,"====== ETABLE::Bottom_up_cr, Append coderep:\n"));
		Is_Trace_cmd(Tracing(),cr->Print(2,TFile));
		Append_real_occurrence(cr, stmt, stmt_kid_num, depth, FALSE);
	      } else {
		Is_Trace(Tracing(),
			 (TFile,"====== ETABLE::Bottom_up_cr, Insert coderep:\n"));
		Is_Trace_cmd(Tracing(),cr->Print(2,TFile));
		Insert_real_occurrence(cr, stmt, stmt_kid_num, depth, FALSE, urgent);
	      }
	      if (no_estr) {
		EXP_WORKLST *worklist = Get_worklst(cr, FALSE, TRUE);
		Is_True(worklist, ("ETABLE::Bottome_up_cr, cannot find worklist"));
		worklist->Set_exclude_sr_cand();
	      }
	    }
	    if (all_kids_are_terminal || WOPT_Enable_Aggressive_Lftr)
	      Lftr()->Insert_comp_occurrence(cr, stmt, stmt_kid_num);
	  }
	}
      }
      break;
    case CK_DELETED:	// should never happen
    default:		// illegal kind
      Is_True(0,("ETABLE::Bottom_up_cr(), unexpected kind 0x%x",cr->Kind()));
      break;
    }
}

/* CVTL-RELATED start (correctness) */

// =====================================================================
// Insert necessary cvt/cvtl for reload of a saved ILOAD/LDID
// =====================================================================
CODEREP *
EXP_OCCURS::Load_use_cr(ETABLE *etable, CODEREP * old_cr, CODEREP *cr)
{
  EXP_WORKLST *worklist = etable->Get_worklst(old_cr, FALSE, TRUE);
  CODEREP * new_cr = Alloc_stack_cr(0);

  Is_True(etable->Pre_kind() != PK_VNFRE, 
	  ("EXP_WORKLST::Load_use_cr: Unexpected call for VNFRE"));
  Is_True(old_cr->Is_integral_load_store(),("EXP_OCCUR::Load_use_cr: not an integral iload/ldid"));
  Is_True(cr!=NULL,("EXP_OCCUR::Load_use_cr: cr is NULL"));
  Is_True(worklist != NULL, ("EXP_OCCUR::Load_use_cr: Null worklist"));
  
  if (MTYPE_size_min(old_cr->Dsctyp()) > MTYPE_size_min(MTYPE_I4))
    return cr;
  
  OPCODE opc;
  INT load_need_cvt = 
    Need_load_type_conversion(worklist->Sign_extd(), 
			      old_cr->Is_sign_extd(),
			      old_cr->Dtyp(), old_cr->Dsctyp(), &opc);
  switch (load_need_cvt) {
  case NOT_AT_ALL:
    break;
  case NEED_CVT:
    cr->IncUsecnt();
    new_cr->Init_expr(opc, cr);
    cr = etable->Rehash_exp(new_cr, etable->Gvn(cr));
    break;
  case NEED_CVTL:
    cr->IncUsecnt();
    new_cr->Init_expr(opc, cr);
    new_cr->Set_offset(MTYPE_size_min(old_cr->Dsctyp()));
    cr = etable->Rehash_exp(new_cr, etable->Gvn(cr));
    break;
  }

#ifndef KEY
  if (Split_64_Bit_Int_Ops && MTYPE_size_min(cr->Dtyp()) == 32 &&
      MTYPE_size_min(old_cr->Dtyp()) == 64) {
    opc = MTYPE_signed(old_cr->Dtyp()) ? OPC_I8I4CVT : OPC_U8U4CVT;
    new_cr->Init_expr(opc, cr);
    return etable->Rehash_exp(new_cr, etable->Gvn(cr));
  }
#else
  cr = cr->Fixup_type(old_cr->Dtyp(), etable->Htable());
#endif

  return cr; // to satisfy the compiler
}

// =====================================================================
// Insert necessary cvt/cvtl for normal save of a ILOAD/LDID
// =====================================================================
CODEREP * 
EXP_WORKLST::Save_use_cr(const ETABLE *etable, CODEREP * old_cr)
{
  Is_True(Pre_kind() != PK_VNFRE, 
	  ("EXP_WORKLST::Save_use_cr: Unexpected call for VNFRE"));
  Is_True(old_cr->Is_integral_load_store(),("EXP_WORKLST::Save_use_cr: not an integral iload/ldid"));
  
  if (MTYPE_size_min(old_cr->Dsctyp()) > MTYPE_size_min(MTYPE_I4))
    return old_cr;

  const UINT32  gvn = etable->Gvn(old_cr);
  CODEREP      *new_cr = Alloc_stack_cr(0);
  OPCODE        opc;
  INT           load_need_cvt = 
    Need_load_type_conversion(old_cr->Is_sign_extd(), Sign_extd(), 
			      old_cr->Dtyp(), old_cr->Dsctyp(), &opc);
  switch (load_need_cvt) {
  case NOT_AT_ALL:
    return old_cr;
  case NEED_CVT:
    if ( old_cr->Coderep_id() == 0 || 
	(old_cr->Kind() == CK_VAR && old_cr->Is_flag_set(CF_MADEUP_TYPE)) ) {
      old_cr->Reset_flag( CF_MADEUP_TYPE );
      if (opc == OPC_U8U4CVT) {
	old_cr->Set_dtyp(MTYPE_U8);
	old_cr->Set_dsctyp(MTYPE_U4);
	old_cr->Set_sign_extension_flag();
#ifndef KEY
      } else if (opc == OPC_U4U8CVT) {
	old_cr->Set_dtyp(MTYPE_U4);
#else
      } else if (opc == OPC_I8I4CVT) {
	old_cr->Set_dtyp(MTYPE_I8);
	old_cr->Set_dsctyp(MTYPE_I4);
#endif
	old_cr->Set_sign_extension_flag();
      } else {
	Is_True(FALSE, ("EXP_WORKLST::Save_use_cr: wrong type conversion"));
      }
      return old_cr;
    } else {
      old_cr->IncUsecnt();
      new_cr->Init_expr(opc, old_cr);
      return etable->Rehash_exp(new_cr, gvn);
    }
  case NEED_CVTL:
    if ( old_cr->Coderep_id() == 0 || 
	(old_cr->Kind() == CK_VAR && old_cr->Is_flag_set(CF_MADEUP_TYPE)) ) {
      old_cr->Reset_flag( CF_MADEUP_TYPE );
      if (opc == OPC_U4CVTL || opc == OPC_U8CVTL) {
	old_cr->Set_dtyp(Mtype_TransferSign(MTYPE_U4,old_cr->Dtyp()));
	old_cr->Set_dsctyp(Mtype_TransferSign(MTYPE_U4,old_cr->Dsctyp()));
	old_cr->Set_sign_extension_flag();
      } else if (opc == OPC_I4CVTL || opc == OPC_I8CVTL) {
	old_cr->Set_dtyp(Mtype_TransferSign(MTYPE_I4,old_cr->Dtyp()));
	old_cr->Set_dsctyp(Mtype_TransferSign(MTYPE_I4,old_cr->Dsctyp()));
	old_cr->Set_sign_extension_flag();
      } else {
	Is_True(FALSE, ("EXP_WORKLST::Save_use_cr: wrong type conversion"));
      }
      return old_cr;
    } else {
      old_cr->IncUsecnt();
      new_cr->Init_expr(opc, old_cr);
      new_cr->Set_offset(MTYPE_size_min(old_cr->Dsctyp()));
      return etable->Rehash_exp(new_cr, gvn);
    }
  }

  return old_cr; // to satisfy the compiler
}
/* CVTL-RELATED finish */


// =====================================================================
// Replace_occurs - The real occurrences defined by the "occur" are to 
// be replaced by a CODEREP corresponding to a given tcon or a given
// coderep (i.e. an OCCUR_REPLACEMENT).  Since the traversal mechanism 
// through stmts and codereps is the same regardless of what is replaced 
// for the found occurrences, an object OCCUR_REPLACEMENT to carry out 
// the acual replacement once an occurrence is found, and three routines
// to search for the occurrences:
//
//    ETABLE::Recursive_rehash_and_replace() // called by Rehash_and_replace()
//    ETABLE::Rehash_and_replace()           // called by Replace_occurs()
//    ETABLE::Replace_occurs()               // Top level replacement routine
// =====================================================================

class OCCUR_REPLACEMENT
{
private:

   enum REPLACEMENT_KIND {REPL_BY_CR, REPL_BY_TYPED_CR, REPL_BY_CONST};

   REPLACEMENT_KIND _kind;
   CODEREP         *_cr;
   TCON             _tcon;

   // If the occurrence is a vectorized constant (all the elements should hold 
   // same value), <_vect_ty> indicate the vect type, and the value of 
   // elements are held by _tcon.
   //
   TYPE_ID _vect_ty;

   CODEREP *_replace_by_cr(CODEREP *x)
   {
      // Replace "x" by "_cr". Adjust usecnts.
      //
      x->DecUsecnt_rec();
      _cr->IncUsecnt();
      return _cr;
   }

   CODEREP *_replace_by_typed_cr(ETABLE     *etable,
				 EXP_OCCURS *occur,
				 CODEREP    *x)
   {
      // Replace "x" by "_cr". Adjust types and usecnts.
      //
      CODEREP *replacement = _replace_by_cr(x);
      if (x->Is_integral_load_store())
      {
	 // Type conversion may be necessary, except for value numbering.
	 //
	 replacement = occur->Load_use_cr(etable, x, replacement);
      }
      return replacement;
   }

   CODEREP *_replace_by_scalar_const(CODEMAP *htable, CODEREP *x)
   {
      // Create a CODEREP corresponding to the TCON and the result-type
      // of "x", and return it. Adjust usecnts.
      //
      TCON_IDX tcon_idx;

      // Note that there may be cases when a scalar can replace an m-type,
      // e.g. as the rhs of an MSTORE, and we therefore allow such 
      // replacements here without conversion.
      //
      if (x->Dtyp() != TCON_ty(_tcon) &&
	  x->Dtyp() != MTYPE_M )
	 tcon_idx = Enter_tcon(Targ_Conv(x->Dtyp(), _tcon));
      else
	 tcon_idx = Enter_tcon(_tcon);

      CODEREP *tcon_cr = htable->Add_tcon(tcon_idx);

      x->DecUsecnt_rec();
      tcon_cr->IncUsecnt();
      return tcon_cr;
   }

   CODEREP *_replace_by_vect_const(CODEMAP *htable, CODEREP *x)
   {
      // step 1: derive element type from vector type
      //
      TYPE_ID elem_ty = Mtype_vector_elemtype (_vect_ty);

      Is_True (MTYPE_byte_size (_vect_ty) == MTYPE_byte_size(x->Dtyp()),
               ("discrepance in vector type"));
        
      // step 2: generate a symbol for the constant value.
      //
      ST* sym;
      sym = New_Const_Sym (Enter_tcon (_tcon), Be_Type_Tbl (TCON_ty(_tcon)));
    
      // step 3: generate the const vector
      //
      CODEREP* cr = Alloc_stack_cr (0);
      cr->Init_rconst (_vect_ty, sym);
      cr = htable->Hash_Rconst(cr);

      // step 4: get rid of the orignial expr
      //
      x->DecUsecnt_rec();

      return cr;
   }

   CODEREP *_replace_by_const(CODEMAP *htable, CODEREP *x)
   {
      if (_vect_ty == MTYPE_UNKNOWN) {
        return _replace_by_scalar_const (htable, x);
      }

      return _replace_by_vect_const (htable, x);
   }
   
public:

   OCCUR_REPLACEMENT(CODEREP *replace_by_cr, BOOL type_conv = FALSE):
      _cr(replace_by_cr)
   {
      _kind = (type_conv? REPL_BY_TYPED_CR : REPL_BY_CR);
   }
   
   OCCUR_REPLACEMENT(TCON replace_by_tcon, TYPE_ID vect_ty):
      _tcon(replace_by_tcon), 
      _kind(REPL_BY_CONST)
   {
     _vect_ty = vect_ty;
   }
   
   CODEREP *apply(CODEMAP    *htable,
		  ETABLE     *etable,
		  EXP_OCCURS *occur,
		  CODEREP    *x)
   {
      // replace "x" by this replacement object.
      //
      CODEREP *replacement = NULL;
      
      switch (_kind)
      {
      case REPL_BY_CR:
	 replacement = _replace_by_cr(x);
	 break;
	 
      case REPL_BY_TYPED_CR:
	 replacement = _replace_by_typed_cr(etable, occur, x);
	 break;

      case REPL_BY_CONST:
	 replacement = _replace_by_const(htable, x);

	 // VNFRE assumes the original expression (x) and the new constant
	 // have the same value number.
	 //
	 if (etable->Pre_kind() == PK_VNFRE)
	    VNFRE::add_valnum(replacement, VNFRE::get_valnum(x));
	 break;

      default:
	 FmtAssert(FALSE,("Unknown OCCUR_REPLACEMENT"));
	 break;
      }
      return replacement;
   }
}; // OCCUR_REPLACEMENT


CODEREP *
ETABLE::Recursive_rehash_and_replace(CODEREP           *x,
				     EXP_OCCURS        *occur,
				     OCCUR_REPLACEMENT *repl,
				     const BOOL         replacing_istr_base,
				     UINT               depth,
				     OPCODE		opc)
{
  // Recursive_rehash_and_replace - Recursively descend the tree, and
  // at each occurrence of the expression node occur->Occurrence(), replace 
  // it by "repl" using GET_REPLACEMENT_CR.  On the way back up, rehash. 
  // If no replacement, no rehash.  Return the resulting tree if rehashing;
  // if no rehashing, return NULL.
  //
  // Note that this is a generic function, since it may have several
  // instances (e.g. for literal substitution in VNFRE, temporary substitution
  // in PRE and VNFRE, and just rehashing without any replacement).
  //
  BOOL     need_rehash;
  BOOL     all_kids_are_terminal;
  INT32    i;
  CODEREP *cr, *expr;  
  CODEREP *original_cr = x;

  Is_True(x != NULL, ("ETABLE::Recursive_rehash_and_replace, NULL CR"));
  Is_True(occur != NULL, ("ETABLE::Recursive_rehash_and_replace, NULL occur"));

  if (Pre_kind() == PK_VNFRE)
  {
    // The case of (x->Coderep_id() == 0) should have been taken care of in
    // Rehash_and_replace_cr() and should only occur for a top-level 
    // call to this recursive routine. Hence, no need to do:
    //
    //   original_cr = occur->Stmt()->Lhs();
    //
    Is_True(x->Coderep_id() != 0, ("Unexpected coderep id in "
				   "ETABLE::Recursive_rehash_and_replace()"));

    if (!replacing_istr_base                                           &&
	VNFRE::get_valnum(x) == VNFRE::get_valnum(occur->Occurrence()) &&
	!(x->Non_leaf() && x->Opr() == OPR_PARM)                       &&
	x->Kind() != CK_CONST                                          &&
	x->Kind() != CK_RCONST)
    {
      // Replace "x" when "x" matches the value number we are looking for,
      // but never replace a PARM, a CK_CONST, a CK_RCONST, or a lhs of
      // an istore coderep (only the kid of PARM nodes and bases of lhs
      // may be replaced).
      //
      // No conversion necessary for value numbering.  Note that there
      // is not need to call VNFRE::replace_occurs() here, assuming we
      // are currently processing the worklist in which the "repl" occurs
      // and, hence, we are done with that worklist.
      //

      // bug fix for OSP_188
      // As to expression whose operator is OPR_CVT, if the descriptor type
      // of this expr is MTYPE_B, we should not modify the return type of
      // it's kid, which is MTYPE_B, to some type else.
      OPERATOR opr = OPCODE_operator(opc);
      TYPE_ID desc = OPCODE_desc(opc);
      if (opr == OPR_CVT && desc == MTYPE_B && x->Kind() == CK_VAR &&
	  x->Dtyp() == MTYPE_B && x->Dsctyp() == MTYPE_B)
	{
	  return x;
	}
      
      return repl->apply(Htable(), this, occur, x);
    }
  }
  else if (x == occur->Occurrence())
      return repl->apply(Htable(), this, occur, x);

  switch (x->Kind()) {
  case CK_LDA:
  case CK_VAR:
  case CK_CONST:
  case CK_RCONST:
    return NULL;
    
  case CK_IVAR: 
    cr = Alloc_stack_cr(x->Extra_ptrs_used());
    need_rehash = FALSE;
    cr->Copy(*x);	
    cr->Set_usecnt(0);
    // process the base expression
    if (replacing_istr_base) {
      expr = Recursive_rehash_and_replace(x->Istr_base(), occur, repl, 
					  FALSE, depth+1, x->Op());
      if (expr) {
	need_rehash = TRUE;
	cr->Set_istr_base(expr);
	cr->Set_ilod_base(NULL);
      }
      else cr->Set_istr_base(x->Istr_base());	// Redundant? We already copied
      all_kids_are_terminal = cr->Istr_base()->Is_non_volatile_terminal(Opt_stab());
    }
    else {
      expr = Recursive_rehash_and_replace(x->Ilod_base(), occur, repl,
					  FALSE, depth+1, x->Op());
      if (expr) {
	need_rehash = TRUE;
	cr->Set_ilod_base(expr);
	cr->Set_istr_base(NULL); 
      }
      else cr->Set_ilod_base(x->Ilod_base());	// Redundant? We already copied
      all_kids_are_terminal = cr->Ilod_base()->Is_non_volatile_terminal(Opt_stab());
    }
    if (x->Opr() == OPR_MLOAD) {
      // process the MLOAD size expression
      expr = Recursive_rehash_and_replace(x->Mload_size(), occur, repl,
					  FALSE, depth+1, x->Op());
      if (expr) {
        need_rehash = TRUE;
        cr->Set_mload_size(expr);
      }
      else cr->Set_mload_size(x->Mload_size());
    }
    else if (x->Opr() == OPR_ILOADX) {
      // process the index expression
      expr = Recursive_rehash_and_replace(x->Index(), occur, repl,
					  FALSE, depth+1, x->Op());
      if (expr) {
        need_rehash = TRUE;
        cr->Set_index(expr);
      }
      else cr->Set_index(x->Index());
    }
    if (!need_rehash)
      return NULL;
    x->DecUsecnt();
    x = Rehash_exp(cr, Gvn(original_cr));
    
    if (Pre_kind() == PK_VNFRE)
       VNFRE::replace_occurs(original_cr, x, occur->Stmt());

#ifdef KEY
    if (WOPT_Enable_Preserve_Mem_Opnds && opc != OPCODE_UNKNOWN &&
	MTYPE_is_float(OPCODE_rtype(opc)) &&
	(OPCODE_operator(opc) == OPR_ADD ||
	 OPCODE_operator(opc) == OPR_MPY ||
	 OPCODE_operator(opc) == OPR_SUB)) {
    }
    else
#endif
    if (Pre_kind() == PK_EPRE && 
	all_kids_are_terminal &&
	OPERATOR_is_scalar_iload (x->Opr()) &&
	x->Dtyp() != MTYPE_M &&
	! x->Is_ivar_volatile()) {
      // need_rehash must be TRUE
      Is_Trace(Tracing(),
	       (TFile, "ETABLE::Recursive_rehash_and_replace: "
		"New leaf expression for CK_IVAR case:\n"));
      Is_Trace_cmd(Tracing(), x->Print(4, TFile));
      Insert_real_occurrence(x, occur->Stmt(), occur->Stmt_kid_num(), depth,
				      replacing_istr_base);
    }
    return x;

  case CK_OP: 
    //  A CR has its temp_id field initialized to -1, and might be
    //  set to the temp_id of statement processed.  If ETABLE::New_temp_id()
    //  guarantees not to repeatedly use an id, CRs reached here the
    //  first time will have an id different from the Cur_temp_id.
    //  
    //   cr->Temp_id() != Cur_temp_id()     -->  first visit 
    //   cr->Temp_id() != Cur_temp_id() && !Is_lcse  -->  second visit 
    //   cr->Temp_id() == Cur_temp_id() && Is_lcse   -->  third+ visit 
    //  
    if (x->Temp_id() == Cur_temp_id() && x->Is_lcse()) {
      // CR third time seen.  Lookup rehash_cache uses linear search.
      // Notice this is a n^2 algorithm.  If CR has a unique id, the lookup
      // can be made constant time.
      Warn_todo("Recursive_rehash_and_replace: using unique CR id to "
		"lookup rehashed expr."); 
      x = Lookup_rehash_expr(x);
      if (x == NULL) // Lookup_rehash_expr returns NULL when the expr needs no rehashing.
	return NULL;
      all_kids_are_terminal = TRUE;
      for  (INT32 i = 0; i < x->Kid_count(); i++) {
	if (! x->Opnd(i)->Is_non_volatile_terminal(Opt_stab()))
	  all_kids_are_terminal = FALSE;
      }
    } else {
      if (!OPCODE_is_volatile(x->Op())) {
	if (x->Temp_id() != Cur_temp_id()) {
	  // CR seen the first time.
	  x->Set_temp_id(Cur_temp_id());
	  x->Reset_is_lcse();
	}
	else x->Set_is_lcse();	// seen the second time
      }
	
      cr = Alloc_stack_cr(x->Extra_ptrs_used());     
      need_rehash = FALSE;
      all_kids_are_terminal = TRUE;
      cr->Copy(*x);	
      cr->Set_usecnt(0);
      for  (INT32 i = 0; i < x->Kid_count(); i++) {
	// bug 12471: __builtin_expect's first kid must be constant
	if (cr->Opr() == OPR_INTRINSIC_OP && cr->Intrinsic() == INTRN_EXPECT &&
	    i == 1)
	  continue;
	expr = Recursive_rehash_and_replace(x->Opnd(i), occur, repl,
					    FALSE, depth+1, x->Op());
	if (expr) {
	  need_rehash = TRUE;
	  cr->Set_opnd(i, expr);
	} else
	  cr->Set_opnd(i, x->Opnd(i));
	if (! cr->Opnd(i)->Is_non_volatile_terminal(Opt_stab()))
	  all_kids_are_terminal = FALSE;
      }
      if (! need_rehash)
	return NULL;
      x->DecUsecnt();

      CODEREP *rehash_x = 
	 Rehash_exp(cr, Gvn(original_cr), !OPCODE_is_compare(cr->Op()));

      // This is to fix compile-time.
      if (x->Is_lcse()) 	// seen the second time
        Add_rehash_expr(x, rehash_x);
      x = rehash_x;
    }

    if (!OPCODE_is_volatile(x->Op())) {
      if (Pre_kind() == PK_VNFRE)
	VNFRE::replace_occurs(original_cr, x, occur->Stmt());

      if (Pre_kind() == PK_EPRE) {
	if (all_kids_are_terminal) {
	  // need_rehash must be TRUE
	  Is_Trace(Tracing(),
		   (TFile, "ETABLE::Recursive_rehash_and_replace: "
		    "New leaf expression for CK_OP case:\n"));
	  Is_Trace_cmd(Tracing(), x->Print(4, TFile));
	  OPERATOR opr = x->Opr();
	  if (occur->Stmt()->Iv_update() ||
	      (depth == 0 &&
	       (occur->Stmt()->Op() == OPC_TRUEBR ||
		occur->Stmt()->Op() == OPC_FALSEBR) &&
	       Subsumable_by_branch(x)) ||
	      (depth == 1 &&
	       (occur->Stmt()->Op() == OPC_TRUEBR ||
		occur->Stmt()->Op() == OPC_FALSEBR) &&
	       occur->Stmt()->Rhs()->Opr() == OPR_LNOT &&
	       Subsumable_by_branch(x))) {
	    Is_Trace(Tracing(), (TFile, " -----> omitted\n"));
	    x->Set_omitted();
	  } else if (!WOPT_Enable_CSE_FP_comparison &&
		     (opr == OPR_EQ || opr == OPR_NE ||
		      opr == OPR_LT || opr == OPR_LE || 
		      opr == OPR_GT || opr == OPR_GE) &&
		     MTYPE_is_float(x->Dsctyp())) {
	    x->Set_omitted();
	  } else {
	    Is_Trace(Tracing(), (TFile, " -----> inserting...\n"));
	    Insert_real_occurrence(x, occur->Stmt(),
				   occur->Stmt_kid_num(), depth/*, FALSE, TRUE*/);
	  }
	}
	if (all_kids_are_terminal || WOPT_Enable_Aggressive_Lftr)
	  Lftr()->Insert_comp_occurrence(x, occur->Stmt(), occur->Stmt_kid_num());
      }
    }
    return x;
  }
  Is_True(FALSE, ("ETABLE::Recursive_rehash_and_replace, unknown kind"));
  return NULL;	// to satisfy compiler
} // ETABLE::Recursive_rehash_and_replace


CODEREP *
ETABLE::Rehash_and_replace(CODEREP           *x,
			   EXP_OCCURS        *occur,
			   OCCUR_REPLACEMENT *repl,
			   const BOOL         replacing_istr_base,
			   OPCODE	      parent_opc)
{
  // Top level routine, traversing down from coderep level.
  //
  CODEREP *new_cr = NULL;
   
  if (Pre_kind() == PK_VNFRE)
  {
     // Presumably there is at least one occurrence of the value number
     // associated with occur->Occurrence() in the given coderep node (x),
     // and "x" is a part of the stmt denoted by the occurrence node.
     // All such occurrences should be replaced with the given replacement
     // "repl" by recursive descent through Recursive_rehash_and_replace(),
     // and we must update worklists yet to be processed by VNFRE to account
     // for all such replacements in the stmt kid denoted by the occurrence.
     //
     // We use a hack here to set the Coderep_id() of "x" when it is not set
     // and represents a lhs of an ISTORE.
     //
     BOOL set_cr_id = (x->Coderep_id() == 0 && replacing_istr_base);
     
     if (set_cr_id)
       x->Set_coderep_id(occur->Stmt()->Lhs()->Coderep_id());

     VNFRE::delete_occurs(occur, x);
     new_cr = 
       Recursive_rehash_and_replace(x, occur, repl, replacing_istr_base, 0, parent_opc);

     if (set_cr_id)
       x->Set_coderep_id(0);
  }
  else
  {
    new_cr = 
      Recursive_rehash_and_replace(x, occur, repl, replacing_istr_base, 0, parent_opc);
  }
  return new_cr;

} // ETABLE::Rehash_and_replace


void
ETABLE::Replace_occurs(EXP_OCCURS *occur, OCCUR_REPLACEMENT *repl)
{
  // Top level routine, traversing down from statement level.
  //
  Is_True(occur->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR ||
	  occur->Occ_kind() == EXP_OCCURS::OCC_COMP_OCCUR,
	  ("ETABLE::Replace_occurs: can't handle occurrence kind"));

  STMTREP *stmt = occur->Stmt();
  INT32 kid_num = occur->Stmt_kid_num();

  // if this expression is a comparison, look it up in the comp occur list
  // and remove it.
  if (occur->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR && 
      Lftr()->Lftr_on() && Lftr()->Is_comparison(occur->Occurrence()))
    Lftr()->Remove_comp_occur(occur);

  New_temp_id();  // generate a new temp id when processing this EXP_OCCUR
  Clear_rehash_cache();

  Is_Trace(Tracing(), (TFile, "ETABLE::Replace_occurs: Replacing\n"));
  Is_Trace_cmd(Tracing(), occur->Occurrence()->Print(0, TFile));
  Is_Trace(Tracing(), (TFile, "    in kid %d of\n", kid_num));
  Is_Trace_cmd(Tracing(), stmt->Print(TFile));
  Is_Trace(Tracing(), (TFile, "    for occurrence\n"));
  Is_Trace_cmd(Tracing(), occur->Print(TFile));
  Is_Trace_cmd(Tracing(), fflush(TFile));

  if (OPCODE_is_fake(stmt->Op()))
  {
    CODEREP *new_opnd = 
       Rehash_and_replace(stmt->Rhs()->Opnd(kid_num), occur, repl, FALSE,
	       		  stmt->Rhs()->Op());

    Is_True(new_opnd != NULL, 
	    ("ETABLE::Replace_occurs: RHS opnd must rehash"));

    stmt->Rhs()->Set_opnd(kid_num, new_opnd );
  }
  else if (OPCODE_is_store(stmt->Op()))
  {
    switch (kid_num) {
    case 0:
      if (Pre_kind() == PK_VNFRE ||
	  !stmt->Iv_update()     ||
	  stmt->Rhs()->E_num() != occur->Occurrence()->E_num())
      { 
	CODEREP *new_rhs = Rehash_and_replace(stmt->Rhs(), occur, repl, FALSE,
					      stmt->Op());

#ifdef KEY // bug 5980: there can be duplicate real occur nodes inserted due
	   // to LFTR
	if (new_rhs == NULL) {
#ifdef Is_True_On
  	  EXP_WORKLST *worklist = Get_worklst(occur->Occurrence());
	  EXP_ALL_OCCURS_ITER exp_occ_iter(worklist->Real_occurs().Head(),
			   NULL, NULL, NULL, NULL); /* only real occurrences */
	  EXP_OCCURS *oc;
	  FOR_ALL_NODE(oc, exp_occ_iter, Init()) {
	    if (oc == occur) 
	      FmtAssert(FALSE,("ETABLE::Replace_occurs: RHS must need rehash"));
	    if (oc->Stmt() == occur->Stmt() && 
		oc->Stmt_kid_num() == occur->Stmt_kid_num())
	      break; // found the earlier duplicate real occur, so no error
	  }
#endif
	  break;
	}
#else
	Is_True(new_rhs != NULL,
		("ETABLE::Replace_occurs: RHS must need rehash"));
#endif
	stmt->Set_rhs(new_rhs);

	// Let the IV update status of the statement be reanalyzed.
	//
	stmt->Reset_iv_update();
      }
      else
      {
	// although we avoid create occurrence of RHS of iv_update in step1,6
	// *p = *p + 1 could still cause this to happen
	//Is_True(FALSE, ("ETABLE::Replace_by_temp: logic error"));
      }
      break;
    case 1:
      {
	Is_True(stmt->Opr() == OPR_MSTORE ||
		OPERATOR_is_scalar_istore (stmt->Opr()) ||
		OPERATOR_is_scalar_store (stmt->Opr()),
	      ("ETABLE::Replace_occurs: bad stmt_kid_num"));
	CODEREP *x = Alloc_stack_cr(stmt->Lhs()->Extra_ptrs_used());
	x->Copy(*stmt->Lhs());
	x->Set_usecnt(0);
	x = Rehash_and_replace(x, occur, repl, TRUE, stmt->Op());

	Is_True(x == NULL || x->Istr_base() != NULL,
		("ETABLE::Replace_occurs: Istr_base should not be NULL"));

	if (x != NULL)
	  stmt->Set_lhs(x);
      }
      break;
    case 2:
      {
	Is_True(stmt->Opr() == OPR_MSTORE, 
		("ETABLE::Replace_occurs: bad stmt_kid_num"));
	CODEREP *new_mstore_size = 
	  Rehash_and_replace(stmt->Lhs()->Mstore_size(), occur, repl, FALSE,
		  	     stmt->Lhs()->Op());
	Is_True(new_mstore_size != NULL,
		("ETABLE::Replace_occurs: Mstore_size() must not be NULL"));

	stmt->Lhs()->Set_mstore_size(new_mstore_size);
      }
      break;
    default:
      Is_True(FALSE, ("ETABLE::Replace_occurs: bad stmt_kid_num"));
      break;
    }
  }
  else if (stmt->Opr() == OPR_PREFETCH) 
  {
    Is_True(kid_num == 0, ("ETABLE::Replace_occurs: wrong stmt_kid_num"));
    CODEREP *new_ilod_base = 
       Rehash_and_replace(stmt->Rhs()->Ilod_base(), occur, repl, FALSE, 
	       		  stmt->Rhs()->Op());

    Is_True(new_ilod_base != NULL,
	    ("ETABLE::Replace_occurs: new_ilod_base must not be NULL"));
    stmt->Rhs()->Set_ilod_base(new_ilod_base);
  }
  else
  {
    Is_True(kid_num == 0, ("ETABLE::Replace_occurs: wrong stmt_kid_num"));
    CODEREP *new_rhs = Rehash_and_replace(stmt->Rhs(), occur, repl, FALSE, 
	    				  stmt->Op());
    Is_True(new_rhs != NULL,
	    ("ETABLE::Replace_occurs: new_rhs must not be NULL"));
    stmt->Set_rhs(new_rhs);
  }
  Is_Trace(Tracing(), (TFile, "ETABLE::Replace_occurs: Resulting stmt:\n"));
  Is_Trace_cmd(Tracing(), stmt->Print(TFile));
} // ETABLE::Replace_occurs


// =====================================================================
// Replace_by_temp - The original occurrence is to be replaced by the temp,
// and the tree containing it needs to be rehashed.
// =====================================================================
void
ETABLE::Replace_by_temp(EXP_OCCURS *occur, CODEREP *tempcr)
{
  OCCUR_REPLACEMENT repl(tempcr, (Pre_kind() != PK_VNFRE)/*type_conv*/);
  Replace_occurs(occur, &repl);
}


// =====================================================================
// Replace_by_const - The original occurrence is to be replaced by the tcon,
// and the tree containing it needs to be rehashed.
// =====================================================================
void
ETABLE::Replace_by_const(EXP_OCCURS *occur, TCON tcon, TYPE_ID vect_ty)
{
  // Top level routine, traversing down from statement level.
  //
  OCCUR_REPLACEMENT repl(tcon, vect_ty);
  Replace_occurs(occur, &repl);
}


// =====================================================================
// No_replace - The original occurrence is not to be replaced by any temp,
// but because some temp now owns the coderep, the tree needs to be rehashed.
// The code more or less corresponds to Replace_by_temp, so modifications
// to these two routines should be kept in sync.
// =====================================================================
void
ETABLE::No_replace(EXP_OCCURS *occur, BOOL dont_rehash)
{
  Is_True(FALSE,
	  ("ETABLE::No_replace: not yet correct "
	   "(e.g. the assignment to original_occurs below)"));
  
  Is_True(occur->Occurrence()->Is_flag_set(CF_OWNED_BY_TEMP),
	  ("ETABLE::No_replace: called with wrong coderep node"));

  EXP_OCCURS original_occur(occur->Occurrence(), 
			    occur->Stmt(), NULL, TRUE/*real*/);
  CODEREP   *x = occur->Occurrence();

  // if this expression is a comparison, look it up in the comp occur list
  // and remove it.
  if (occur->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR && 
      Lftr()->Lftr_on() && Lftr()->Is_comparison(x))
    Lftr()->Remove_comp_occur(occur);

  // increase usecnt of kids
  if (x->Kind() == CK_IVAR) {
    x->Ilod_base()->IncUsecnt();
    if (x->Opr() == OPR_MLOAD)
      x->Mload_size()->IncUsecnt();
    else if (x->Opr() == OPR_ILOADX)
      x->Index()->IncUsecnt();
  }
  else { // CK_OP
    for (INT32 i = 0; i < x->Kid_count(); i++)
      x->Opnd(i)->IncUsecnt();
  }
  CODEREP *new_cr = Rehash_exp(x, Gvn(x));
  new_cr->Set_e_num(x->E_num());
  new_cr->DecUsecnt();	// will increase usecnt back when seeing it in tree
  occur->Set_occurrence(new_cr);

  if (dont_rehash)
    return;

  OCCUR_REPLACEMENT repl(occur->Occurrence(), FALSE/*type_conv*/);
  STMTREP          *stmt = occur->Stmt();
  INT32             kid_num = occur->Stmt_kid_num();
  New_temp_id();  // generate a new temp id when processing this EXP_OCCUR
  Clear_rehash_cache();

  Is_Trace(Tracing(), (TFile, "ETABLE::No_replace: called on\n"));
  Is_Trace_cmd(Tracing(), occur->Occurrence()->Print(0, TFile));
  Is_Trace(Tracing(), (TFile, "    in kid %d of\n", kid_num));
  Is_Trace_cmd(Tracing(), stmt->Print(TFile));

  if (OPCODE_is_fake(stmt->Op())) {
    CODEREP *new_opnd =
      Rehash_and_replace(stmt->Rhs()->Opnd(kid_num),
			 &original_occur, &repl, FALSE, stmt->Rhs()->Op());
    Is_True(new_opnd != NULL,
	    ("ETABLE::No_replace: RHS opnd must need rehash"));
    stmt->Rhs()->Set_opnd(kid_num, new_opnd );
  }
  else if (OPCODE_is_store(stmt->Op())) {
    switch (kid_num) {
    case 0: {
      CODEREP *new_rhs = 
	 Rehash_and_replace(stmt->Rhs(), &original_occur, &repl, FALSE, 
		 	    stmt->Rhs()->Op());
      Is_True(new_rhs != NULL,
	      ("ETABLE::No_replace: RHS must need rehash"));
      stmt->Set_rhs(new_rhs);
      break;
      }
    case 1:
      {
      Is_True(stmt->Opr() == OPR_MSTORE ||
	      OPERATOR_is_scalar_istore (stmt->Opr()), 
	      ("ETABLE::No_replace: bad stmt_kid_num"));
      CODEREP *x = Alloc_stack_cr(stmt->Lhs()->Extra_ptrs_used());
      x->Copy(*stmt->Lhs());
      x->Set_usecnt(0);
      x = Rehash_and_replace(x, &original_occur, &repl, TRUE, stmt->Op());

      Is_True(x==NULL || x->Istr_base() != NULL,
	      ("ETABLE::No_replace: Istr_base must not be NULL"));

      if (x != NULL)
	stmt->Set_lhs(x);
      }
      break;
    case 2:
      {
      Is_True(stmt->Opr() == OPR_MSTORE, 
	      ("ETABLE::No_replace: bad stmt_kid_num"));
      CODEREP *new_mstore_size =
	 Rehash_and_replace(stmt->Lhs()->Mstore_size(),
			    &original_occur, &repl, TRUE, stmt->Lhs()->Op());
      Is_True(new_mstore_size != NULL,
	      ("ETABLE::No_replace: Mstore_size() must not be NULL"));
      stmt->Lhs()->Set_mstore_size(new_mstore_size);
      }
      break;
    default:
      Is_True(FALSE, ("ETABLE::No_replace: bad stmt_kid_num"));
      break;
    }
  }
  else if (stmt->Opr() == OPR_PREFETCH) {
    Is_True(kid_num == 0, ("ETABLE::No_replace: wrong stmt_kid_num"));
    CODEREP *new_ilod_base =
      Rehash_and_replace(stmt->Rhs()->Ilod_base(),
			 &original_occur, &repl, TRUE, stmt->Rhs()->Op());

    Is_True(new_ilod_base != NULL,
	    ("ETABLE::No_replace: new_ilod_base must not be NULL"));
    stmt->Rhs()->Set_ilod_base(new_ilod_base);
  }
  else {
    Is_True(kid_num == 0, ("ETABLE::No_replace: wrong stmt_kid_num"));
    CODEREP *new_rhs = 
       Rehash_and_replace(stmt->Rhs(), &original_occur, &repl, TRUE, stmt->Op());
    Is_True(new_rhs != NULL,
	    ("ETABLE::No_replace: new_rhs must not be NULL"));
    stmt->Set_rhs(new_rhs);
  }
}


// ===================================================================
// Same as ETABLE::Bottom_up_cr but only include 1st order expressions 
// containing tempcr.
// ===================================================================
void    
ETABLE::Find_1st_order_exprs_with_temp(STMTREP *stmt, INT stmt_kid_num, 
				       CODEREP *cr, CODEREP *tempcr,
				       BOOL is_istore, UINT depth)
{
  switch (cr->Kind()) {
    case CK_CONST:	// constant terminal, do nothing
    case CK_RCONST:
    case CK_LDA:
    case CK_VAR:	// variable terminal, do nothing
      break;
    case CK_IVAR:	// non-terminal
      {
	BOOL same_base = TRUE;
	if (cr->Ilod_base() != NULL && cr->Istr_base() != NULL && 
	    cr->Ilod_base() != cr->Istr_base()) {
	  same_base = FALSE;
	}

	if (!is_istore) {
	  CODEREP *ivar_vsym = cr->Get_ivar_vsym();
	  if (cr->Ilod_base() == tempcr && ! cr->Is_ivar_volatile()) {
	    if (same_base && cr->Ivar_has_e_num()) {
	      Append_real_occurrence(cr, stmt, stmt_kid_num, depth);
	    }
	  } else {
	    Find_1st_order_exprs_with_temp(stmt, stmt_kid_num, cr->Ilod_base(), 
					   tempcr, FALSE, depth+1);
	  }
	} else {
	  // Ilod_base can be different from Istr_base if the Iload side is
	  // dead, i.e. when Usecnt() == 0
	  CODEREP *ivar_vsym = cr->Get_ivar_vsym();
	  if (cr->Istr_base() == tempcr && ! cr->Is_ivar_volatile()) {
	    if (same_base && cr->Ivar_has_e_num()) {
	      Append_real_occurrence(cr, stmt, stmt_kid_num, depth, TRUE);
	    }
	  } else {
	    Find_1st_order_exprs_with_temp(stmt, stmt_kid_num, cr->Istr_base(), 
					   tempcr, FALSE, depth+1);
	  }
	}
      }
      {
	const OPERATOR ivar_opr = cr->Opr();
	if ( ivar_opr == OPR_MLOAD) {
	  if (cr->Mload_size())
	    Find_1st_order_exprs_with_temp(stmt, stmt_kid_num, cr->Mload_size(),
					 tempcr, FALSE, depth+1);
	  else
	    Find_1st_order_exprs_with_temp(stmt, stmt_kid_num, cr->Mstore_size(),
					 tempcr, FALSE, depth+1);
	}
	else if ( ivar_opr == OPR_ILOADX) {
	  Find_1st_order_exprs_with_temp(stmt, stmt_kid_num, cr->Index(),
				       tempcr, FALSE, depth+1);
	}
      }
      break;
    case CK_OP:		// non-terminal
      {
	//  A CR has its temp_id field initialized to -1, and might be
	//  set to the temp_id of statement processed. If
	//  ETABLE::New_temp_id() guarantees not to repeatedly use an
	//  id, CRs reached here the first time will have an id
	//  different from the Cur_temp_id.
	//  
	//   cr->Temp_id() != Cur_temp_id()               -->  first visit 
	//   cr->Temp_id() == Cur_temp_id() && !Is_lcse   -->  second visit
	//   cr->Temp_id() == Cur_temp_id() && Is_lcse    -->  third+ visit
	//  
	if (cr->Temp_id() == Cur_temp_id())
	  if (cr->Is_lcse() && (cr->Max_depth() >= depth ||
			        cr->Max_depth() == 255))
	    return;   // return because this CR has been visited twice
	  else
	    cr->Set_is_lcse();  // this is the second time
	else {
	  cr->Set_temp_id(Cur_temp_id());  // this is the first time
	  cr->Reset_is_lcse();
	  cr->Set_max_depth(depth);
	}

	// remember maximum depth 
	if (cr->Max_depth() < depth)
	  if (depth > 255)
	    cr->Set_max_depth(255);
	  else cr->Set_max_depth(depth);

	INT32 i;
	CODEREP *kid;
	BOOL all_kids_are_terminal = TRUE;
	BOOL has_tempcr_as_opnd = FALSE;
	
	for (i=0; i<cr->Kid_count(); i++) { 
	  kid = cr->Opnd(i);
	  if (!kid->Is_non_volatile_terminal(Opt_stab())) {
	    all_kids_are_terminal = FALSE;
	    Find_1st_order_exprs_with_temp(stmt, stmt_kid_num, kid, 
					   tempcr, FALSE, depth+1);
	  }
	  else if (kid == tempcr)
	    has_tempcr_as_opnd = TRUE;
	}
	    
	if (all_kids_are_terminal && has_tempcr_as_opnd && cr->Exp_has_e_num()) {
	  Append_real_occurrence(cr, stmt, stmt_kid_num, depth, FALSE);
        }
      }
      break;
    case CK_DELETED:	// should never happen
    default:		// illegal kind
      Is_True(0,("ETABLE::Find_1st_order_exprs_with_temp(), unexpected kind 0x%x",cr->Kind()));
      break;
    }
}

// =====================================================================
// Find_new_1st_order_exprs - The original occurrence has been replaced by a 
// temp without rehashing; find new 1st order expressions formed due to
// these replacements; do not include 1st order expressions that exist
// even before these replacements; modeled after Replace_by_temp.
// =====================================================================
void
ETABLE::Find_new_1st_order_exprs(EXP_OCCURS *occur, CODEREP *tempcr)
{
  Is_True(occur->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR,
	  ("ETABLE::Find_new_1st_order_exprs: can't handle occurrence kind"));
  STMTREP *stmt = occur->Stmt();
  INT32 kid_num = occur->Stmt_kid_num();

  New_temp_id();  // generate a new temp id when processing this EXP_OCCUR
  CODEREP *tree;
  BOOL is_istr_base = FALSE;

  if (OPCODE_is_fake(stmt->Op())) 
    tree = stmt->Rhs()->Opnd(kid_num);
  else if (OPCODE_is_store(stmt->Op())) {
    switch (kid_num) {
    case 0: tree = stmt->Rhs(); break;
    case 1: 
      Is_True(stmt->Opr() == OPR_MSTORE ||
	      OPERATOR_is_scalar_istore (stmt->Opr()), 
	      ("ETABLE::Find_new_1st_order_exprs: bad stmt_kid_num"));
      if (OPERATOR_is_scalar_istore (stmt->Opr())) {
        is_istr_base = TRUE;
        tree = stmt->Lhs(); 
      }
      else tree = stmt->Lhs()->Istr_base(); 
      break;
    case 2: tree = stmt->Lhs()->Mstore_size(); break;
    default: Is_True(FALSE, ("ETABLE::Find_new_1st_order_exprs: bad stmt_kid_num")); break;
    }
  }
  else if (stmt->Opr() == OPR_PREFETCH) 
    tree = stmt->Rhs()->Ilod_base();
  else tree = stmt->Rhs();
  Find_1st_order_exprs_with_temp(stmt, kid_num, tree, tempcr, is_istr_base, 0);
}

/* CVTL-RELATED start (checking code) */
// ====================================================================
// Given a CODEREP and the EXP_OCCURS already seen,
// adjust the data type and dsc type accordingly.
// The combined types are used to generate save and CVTL for reload.
// ====================================================================
void
EXP_WORKLST::Adjust_combined_types(CODEREP *cr)
{
  Is_True(Exp()->Is_integral_load_store(), ("EXP_WORKLST::Adjust_combined_types: wrong cr"));
#if defined(TARG_NVISA)
  if ( (cr->Kind() == CK_VAR) && 
       (MTYPE_size_min(cr->Dsctyp()) != MTYPE_size_min(Exp()->Dsctyp())))
    Set_has_unequal_sizes();
#endif
  Is_True(MTYPE_size_min(cr->Dsctyp()) == MTYPE_size_min(Exp()->Dsctyp()),
	  ("EXP_WORKLST::Adjust_combined_types: mismatch Dsc types"));

#ifdef Is_True_On
  BOOL is_sign_extd = cr->Is_sign_extd();
  cr->Set_sign_extension_flag();
  Is_True(is_sign_extd==cr->Is_sign_extd(),
	  ("EXP_WORKLST::Adjust_combined_types: cr's sign extension flag is inconsistent"));
#endif
  
  if (cr->Is_sign_extd()) Set_sign_extd();

}
/* CVTL-RELATED finish */

// add a real occurrence to the end of the worklst
// if you make changes, check Insert_real_occurrence also
void
ETABLE::Append_real_occurrence(CODEREP *cr, STMTREP *stmt, INT stmt_kid_num,
                               UINT depth, BOOL is_istore)
{
  // Use the hash function to find the WORKLST
  EXP_WORKLST *worklist = Get_worklst(cr);

  if (worklist == NULL) // second order effect disabled
    return;

  Is_True (cr->Kind() != CK_OP || !OPCODE_is_volatile(cr->Op()),
	   ("ETABLE::Append_real_occurrence: entering volatile op."));

  // Adjust the combined types
  if (worklist->Exp()->Is_integral_load_store()) 
    worklist->Adjust_combined_types(cr);

  // First check if this is a second real occurrence in the same stmt kid tree
  // First check to see if the tail of the list is DPO less than or
  // equal to the current occurrence
  EXP_OCCURS *tail_occ = worklist->Real_occurs().Tail();

  // If they are the same, set flag to OCC_MULT_REAL_OCCUR and return
  if (tail_occ && (tail_occ->Enclosed_in_stmt() == stmt) &&
      tail_occ->Stmt_kid_num() == stmt_kid_num) {
    tail_occ->Set_mult_real();
    if (tail_occ->Rehash_cost() < depth)
      tail_occ->Set_rehash_cost(depth);
    return;
  }
  
  // Create the EXP_OCCURS node
  EXP_OCCURS *occurs = Alloc_occurs_node();
  occurs->Set_occurrence(cr);
  occurs->Set_kind(EXP_OCCURS::OCC_REAL_OCCUR);
  occurs->Set_enclose_stmt(stmt);
  occurs->Set_stmt_kid_num(stmt_kid_num);
  occurs->Set_rehash_cost(depth);
  cr->Set_e_num(worklist->E_num());
  if (is_istore)
    occurs->Set_occurs_as_lvalue();

#if defined(TARG_SL) //PARA_EXTENSION
  if( stmt && stmt->Bb() && stmt->Bb()->SL2_para_region())
    occurs->Set_occ_in_para_region(); 
#endif

  // call the WORKLST append
  worklist->Append_occurrence(occurs);
}

// to insert a real occurrence into the worklst in DPO 
// (temporarily for step 6)
// if you make changes, check Append_real_occurrence also
void
ETABLE::Insert_real_occurrence(CODEREP *cr, STMTREP *stmt, INT stmt_kid_num,
			       UINT depth, BOOL is_istore, BOOL urgent)
{
  // Use the hash function to find the WORKLST
  EXP_WORKLST *worklist = Get_worklst(cr, urgent);

  Is_Trace(Tracing(), (TFile, "ETABLE::Insert_real_occurrence: "));

  if (worklist == NULL) {
    // second order effect disabled
    Is_Trace(Tracing(), (TFile, "2nd-order fx disabled; doing nothing\n"));
    return;
  }

  Is_True (cr->Kind() != CK_OP || !OPCODE_is_volatile(cr->Op()),
	   ("ETABLE::Insert_real_occurrence: entering volatile op."));

  // Adjust the combined types
  if (worklist->Exp()->Is_integral_load_store()) 
    worklist->Adjust_combined_types(cr);
  
  // First check if this is a second real occurrence in the same stmt kid tree
  // First check to see if the tail of the list is DPO less than or
  // equal to the current occurrence
  EXP_OCCURS *tail_occ = worklist->Real_occurs().Tail();

  // If they are the same, set flag to OCC_MULT_REAL_OCCUR and return
  if (tail_occ && (tail_occ->Enclosed_in_stmt() == stmt) &&
      tail_occ->Stmt_kid_num() == stmt_kid_num) {
    Is_Trace(Tracing(), (TFile, "mult-real at tail\n"));
    tail_occ->Set_mult_real();
    if (tail_occ->Rehash_cost() < depth)
      tail_occ->Set_rehash_cost(depth);
    return;
  }

  Is_Trace(Tracing(), (TFile, "building occ... "));

  // Create the EXP_OCCURS node
  EXP_OCCURS *occurs = Alloc_occurs_node();
  occurs->Set_occurrence(cr);
  occurs->Set_kind(EXP_OCCURS::OCC_REAL_OCCUR);
  occurs->Set_enclose_stmt(stmt);
  occurs->Set_stmt_kid_num(stmt_kid_num);
  occurs->Set_rehash_cost(depth);

#if defined(TARG_SL) //PARA_EXTENSION
  if( stmt && stmt->Bb() && stmt->Bb()->SL2_para_region())
    occurs->Set_occ_in_para_region(); 
#endif 

  cr->Set_e_num(worklist->E_num());
  if (is_istore)
    occurs->Set_occurs_as_lvalue();

  // call the WORKLST append or insert depends on whether it is already
  // in DPO order
  if ( tail_occ == NULL || tail_occ->Is_DPO_less_than(occurs) ) 
    worklist->Append_occurrence(occurs);
  else
    worklist->Insert_occurrence(occurs, this);

  Is_Trace(Tracing(), (TFile, "placed in worklist\n"));
}

// to append a phi occurence into the worklst
EXP_OCCURS*
ETABLE::Append_phi_occurrence(CODEREP *cr, EXP_PHI *phi, EXP_WORKLST *worklist)
{
  // Create the EXP_OCCURS node
  EXP_OCCURS *occurs = Alloc_occurs_node();
  occurs->Set_occurrence(NULL);
  occurs->Set_kind(EXP_OCCURS::OCC_PHI_OCCUR);
  occurs->Set_exp_phi(phi);
  cr->Set_e_num(worklist->E_num());
  phi->Set_result(occurs);

  // call the WORKLST append
  worklist->Append_occurrence(occurs);

  // Enter this phi to phi hash
  // Enter_exp_phi_hash(occurs, phi);

  return occurs;
}

// to append a phi pred occurence into the worklst
EXP_OCCURS *
ETABLE::Append_phi_pred_occurrence(CODEREP *cr, BB_NODE *bb, EXP_WORKLST *worklist)
{
  // Create the EXP_PRED_OCCURS node
  EXP_OCCURS *occurs = Alloc_occurs_node();
  occurs->Set_occurrence(NULL);
  occurs->Set_kind(EXP_OCCURS::OCC_PHI_PRED_OCCUR);
  occurs->Set_enclose_bb(bb);
  cr->Set_e_num(worklist->E_num());

  // call the WORKLST append
  worklist->Append_occurrence(occurs);
  return occurs;
}

// to append an exit occurence into the etable
void
ETABLE::Append_exit_occurrence(BB_NODE *bb)
{
  // Create the EXP_PRED_OCCURS node
  EXP_OCCURS *occurs = Alloc_occurs_node();
  occurs->Set_occurrence(NULL);
  occurs->Set_kind(EXP_OCCURS::OCC_EXIT_OCCUR);
  occurs->Set_enclose_bb(bb);

  // call the WORKLST append
  Exit_occurs().Append(occurs);
}

// ============================================================
// Remove all the phi preds that have no Will_b_avail()
// successor. Avoid using the EXP_OCCURS_ITER, since we will
// be changing the shape of the list as we go. The structure
// of the code here depends on the SLIST_CLASS declarations in
// be/com/cxx_base.h.
//
// We don't remove the phi occurrences that are not Will_b_avail()
// because some of them will be Def_occur()'s for their e-versions, so
// hoisting will need to refer to them.
//
// Because we leave in place phi occurrences whose phi-preds have been
// recycled, we make a note in a flag field in the EXP_WORKLST saying
// that we've taken away the preds of non-Will_b_avail() phi's. This
// way the EXP_PHI::Print() function can do something sensible to
// avoid referring to those recycled phi-preds.
// ============================================================
void
EXP_WORKLST::Prune_phi_phi_pred(ETABLE *etable)
{
  EXP_OCCURS_CONTAINER *occurs_list = &Phi_occurs();
  EXP_OCCURS           *current_occ;

  // Fix 459353.  The phi-node is released to the free list.
  // The memory is then recycled to hold a hoisted occur.
  // However, there are still real-occ's def_occur pointing to 
  // this node.   The fix is not to free these phi-nodes.

  for (current_occ = occurs_list->Head();
       current_occ != NULL; 
       current_occ = current_occ->Next()) {
    EXP_PHI *current_phi = current_occ->Exp_phi();
    if (current_phi->Will_b_avail()) {
      for (INT i = 0; i < current_phi->Opnd_count(); i++) {
	current_phi->Pred(i)->Set_required_pred();
      }
    }
    else {
      // Make the Pred fields NULL for those phi's whose phi-preds
      // might get recycled. This way we make sure to detect any
      // attempted use of those preds. Such use is not allowed.
      for (INT i = 0; i < current_phi->Opnd_count(); i++) {
	current_phi->Set_pred(i, NULL);
      }
    }
  }

  // Now the phi list is pruned on the basis of
  // Exp_phi()->Will_b_avail(). We have to go through the
  // phi-pred list and do the same thing now on the basis of the
  // Required_pred() flag.
  occurs_list = &Phi_pred_occurs();

  while ((current_occ = occurs_list->Head()) != NULL &&
	 !current_occ->Required_pred()) {
    etable->Add_to_occ_freelist(occurs_list->Remove_Headnode());
  }

  while (current_occ != NULL) {
    EXP_OCCURS *next_occ = current_occ->Next();
    while (next_occ != NULL &&
	   !next_occ->Required_pred()) {
      occurs_list->Remove(current_occ, next_occ);
      etable->Add_to_occ_freelist(next_occ);
      next_occ = current_occ->Next();
    }
    current_occ = next_occ;
  }
  Set_phi_preds_pruned();
}

// perform the PRE/SSA optimization
void
ETABLE::Perform_PRE_optimization(void)
{
  if (Tracing()) {
    fprintf( TFile, "%sProgram before Expr PRE:\n%s",
             DBar, DBar );
    Cfg()->Print(TFile);
  }

  _str_red = CXX_NEW(STR_RED(Cfg(), Htable(), _etable_pool, _tracing),
		     _etable_pool);
  _lftr = CXX_NEW(LFTR(this, Htable(), Cfg(), LFTR_HASH_SIZE), _etable_pool);

  // Phase numbers for various steps:
  INT32 Exc_sr_cands_phase  = 0;
  INT32 Phi_placement_phase = 0;
  INT32 Iterator_phase      = 0;
  INT32 Exp_renaming_phase  = 0;
  INT32 Downsafe_prop_phase = 0;
  INT32 Phi_liveness_phase  = 0;
  INT32 Avail_insert_phase  = 0;
  INT32 Worklst_prune_phase = 0;
  INT32 Hoist_phase         = 0;
  INT32 Save_reload_phase   = 0;
  INT32 Ssa_min_phase       = 0;
  INT32 Finalize_phase      = 0;

  // Initialize Dpo_vec before Dpo_Bb(dpo_id) in EXP_WORKLST::Insert_exp_phi
  Cfg()->Dpo_vec();

  Cfg()->Reset_stmt_id();
  if (Lftr()->Lftr_on()) {
    Cfg()->Analyze_loops();
  }

  if (WOPT_Enable_Hoisting)
    _exp_hoisting = New_EXP_HOISTING(this, _etable_pool);

  SET_OPT_PHASE("New PRE: Build initial occurrence lists");

  // To get memory allocation tracing, use -ta25
  if (Get_Trace(TKIND_ALLOC, TP_WOPT1)) {
    MEM_Tracing_Enable();
  }

  Init_worklst(); // Step 1, all real occurances, LFTR comparison list
  INT first_rank_e_num = _cur_e_num;
  INT bb_cnt = 0, edge_cnt = 0;

#ifdef Is_True_On 
  CFG_ITER cfg_iter;
  BB_NODE *bb;
  FOR_ALL_ELEM(bb, cfg_iter, Init(Cfg())) {
    bb_cnt++;
    edge_cnt += bb->Succ()->Len();
  }
#endif

  Is_Trace(Tracing(),
  	   (TFile, "NEWPRE: initial worklist has %d candidates\n", _cur_e_num));

  EXP_WORKLST *cur_worklst;
  EXP_WORKLST_ITER2 worklst_iter(Exp_worklst(), Urgent_worklst());
  Lftr()->Set_exp_iter(&worklst_iter);	// for use when moving worklsts around
  INT32 cur_worklst_idx = 0;
  INT total_phi_count = 0;
  INT total_opt_ssa_count = 0;
  INT total_dense_ssa_count = 0;
  INT orig_coderep_id_cnt = Htable()->Coderep_id_cnt();
  FOR_ALL_NODE(cur_worklst, worklst_iter, Init()) {
    if (cur_worklst->Real_occurs().Head() == NULL)
      continue;

    ++cur_worklst_idx;
    if (WOPT_Enable_Exp_PRE_Limit != -1 &&
	/*cur_worklst->E_num()*/cur_worklst_idx > WOPT_Enable_Exp_PRE_Limit) {
      DevWarn("NEWPRE: skip PRE for expression with e_num > %d",
	      WOPT_Enable_Exp_PRE_Limit);
      break;
    }

    OPT_POOL_Push(Per_expr_pool(), -1);

    Is_Trace(Tracing(),
	     (TFile, "\nprocessing %dth expression b=%s\n", cur_worklst_idx,
	      cur_worklst->Exp()->Print_bit()));
    Is_Trace_cmd(Tracing(),cur_worklst->Exp()->Print(0,TFile));
    Is_Trace_cmd(Tracing(),cur_worklst->Print(TFile, Lftr()->Exp_hash(cur_worklst)));


    // do stuff for each expression
    Per_worklst_cleanup(cur_worklst);
    _str_red->Perform_per_expr_cleanup();

    if ( !WOPT_Enable_New_SR || !MTYPE_is_integral(cur_worklst->Exp()->Dtyp())) {
      // exclude all expressions from being handled by Strength
      // Reduction if it is disabled.  Otherwise, we allow all
      // expressions to be considered
      cur_worklst->Set_exclude_sr_cand();
    }

    SET_OPT_REPEAT_PHASE(Phi_placement_phase, "New PRE: Expr phi placement");
    // Step 2 may cause bypass of later steps
    if (cur_worklst->Insert_exp_phi(this)) {
      // set up the iterator array for fast iteration (used by
      // Rename, Finalize, and CodeMotion)
      SET_OPT_REPEAT_PHASE(Iterator_phase, "New PRE: Iterator");
      EXP_ALL_OCCURS_ITER *exp_occ_iter = (EXP_ALL_OCCURS_ITER *)
	CXX_NEW(EXP_ALL_OCCURS_ITER(cur_worklst,this,Lftr()), Per_expr_pool());
      cur_worklst->Set_iterator(exp_occ_iter); // pointer to this iterator

      SET_OPT_REPEAT_PHASE(Exp_renaming_phase, "New PRE: Rename");
      cur_worklst->Rename_expression(this); // Step 3

      SET_OPT_REPEAT_PHASE(Downsafe_prop_phase, "New PRE: DownSafety");

      // Downsafety may cause bypass of later steps
      if (cur_worklst->Propagate_downsafe(this)) {
	// A flag to indicate whether we need to bother updating the
	// SSA representation of the program. This flag turns out to
	// be FALSE if and only if no redundancy gets eliminated.
	BOOL optimization_needed;

	{
	  // Scope for clarity -- to delimit lifetime of the def-use frame
	  // on the Etable_local_pool() for the case with ssa minimization
	  // enabled.

	  SET_OPT_REPEAT_PHASE(Avail_insert_phase,
			       "New PRE: WillBeAvail");

	  // Push Etable_local_pool() in preparation for allocating space
	  // for def-use information.
	  OPT_POOL_Push(Etable_local_pool(), -1);

	  cur_worklst->Compute_forward_attributes(this);

	  if (!WOPT_Enable_SSA_Minimization) {
	    // Free the def-use space.
	    OPT_POOL_Pop(Etable_local_pool(), -1);
	  }

	  if (WOPT_Enable_Worklist_Pruning) {
	    SET_OPT_REPEAT_PHASE(Worklst_prune_phase,
				 "New PRE: Phi/phi-pred pruning");
	    cur_worklst->Prune_phi_phi_pred(this);
	  }

	  if (WOPT_Enable_Hoisting) {
	    SET_OPT_REPEAT_PHASE(Hoist_phase,
				 "New PRE: Expr hoisting.");
	    cur_worklst->Hoist_expression(Exp_hoisting());
	  }

	  SET_OPT_REPEAT_PHASE(Save_reload_phase,
			       "New PRE: Expr save/reload");

	  optimization_needed =
	    cur_worklst->
	      Compute_save_delete(Htable(), this,
				  Lftr()->Exp_hash(cur_worklst));

	  if (WOPT_Enable_SSA_Minimization && optimization_needed) {
	    SET_OPT_REPEAT_PHASE(Ssa_min_phase, "New PRE: SSA minimization");
	    cur_worklst->Minimize_temp_ssa(this, Tracing());
	  }

	  if (WOPT_Enable_SSA_Minimization) {
	    // Free the def-use space.
	    OPT_POOL_Pop(Etable_local_pool(), -1);
	  }
	}

	if (optimization_needed) {
	  SET_OPT_REPEAT_PHASE(Finalize_phase, "New PRE: CodeMotion");
	  cur_worklst->Generate_save_reload(this); // Step 6
	}
	else {
	  Is_Trace(Tracing(), (TFile,
			       "ETABLE::Perform_PRE_optimization: "
			       "skipping CodeMotion for expr %d\n",
			       cur_worklst_idx));
	}

#ifdef Is_True_On
	if (WOPT_Enable_Verbose && Tracing()) {
	  fprintf(TFile, "%sCFG after expression %d\n%s", DBar,
		  cur_worklst_idx, DBar);
	  Cfg()->Print(TFile);
	}
	cur_worklst->Verify();
#endif

      } // bypass by DownSafety step

      Is_Trace(Tracing(),
  	       (TFile, "NEWPRE: entire worklist has %d candidates\n", _cur_e_num));

      Opt_tlog("New_PRE", 0,
	       "%d-th expression: Inserts=%d, Saves=%d, Reloads=%d, Temp phis=%d, Hoisted=%d",
	       cur_worklst_idx,
	       cur_worklst->Insert_count(),
	       cur_worklst->Save_count(),
	       cur_worklst->Reload_count(),
	       cur_worklst->Temp_phi_count(),
	       cur_worklst->Hoisted_count());

#ifdef Is_True_On
    // This tlog is under Is_True_On because it could be expensive to collect them,
    // so we don't want it in the production compiler.
    //
    Opt_tlog( "New_PRE", 0,
	      "%d-th expression: Phis=%d(%d%%), Realoccs=%d(%d%%), Optimistic_SSA=%d(%d%%), Dense_SSA=%d(%d%%)",
             cur_worklst_idx,
             cur_worklst->Phi_count(),
             cur_worklst->Phi_count() * 100 / bb_cnt,
	     cur_worklst->Realocc_count(),
	     cur_worklst->Realocc_count() * 100 / bb_cnt,
             cur_worklst->Optimistic_ssa_count(), 
             cur_worklst->Optimistic_ssa_count() * 100 / (bb_cnt+edge_cnt),
             cur_worklst->Dense_ssa_count(),
             cur_worklst->Dense_ssa_count() * 100 / (bb_cnt+edge_cnt));


    total_phi_count += cur_worklst->Phi_count();
    total_opt_ssa_count += cur_worklst->Optimistic_ssa_count();
    total_dense_ssa_count += cur_worklst->Dense_ssa_count();
#endif

      exp_occ_iter->Remove_iter();
      cur_worklst->Set_iterator(NULL);
    } // step 2 bypass

    cur_worklst->Remove_occurs(this);
    OPT_POOL_Pop(Per_expr_pool(), -1);

    if (WOPT_Enable_Verify >= 4) {
      Is_True(_comp_unit->Verify_CODEMAP(), ("CODEMAP corrupted."));
      _comp_unit->Verify_version();
    }
  }

#ifdef Is_True_On
  Opt_tlog("New_PRE_PU_info", 0,
	   "CFG nodes=%d, edges=%d, nodes+edges=%d, init_enum=%d, final_enum=%d",
	   bb_cnt,
	   edge_cnt,
	   bb_cnt + edge_cnt,
	   first_rank_e_num,
	   _cur_e_num);

  if (_cur_e_num > 0) {
    total_phi_count /= _cur_e_num;
    total_opt_ssa_count /= _cur_e_num;
    total_dense_ssa_count /= _cur_e_num;
  }

  Opt_tlog("New_PRE_PU_info", 0,
	   "PU Average:  Phis=%d(%d%%), Optimistic_SSA=%d(%d%%), Dense_SSA=%d(%d%%)",
	   total_phi_count,
	   total_phi_count * 100 / bb_cnt,
	   total_opt_ssa_count,
	   total_opt_ssa_count * 100 / (bb_cnt+edge_cnt),
	   total_dense_ssa_count,
	   total_dense_ssa_count * 100 / (bb_cnt+edge_cnt));
#endif

  if (Tracing()) {
    fprintf(TFile, "%sAfter SSA PRE\n%s", DBar, DBar);
    fprintf(TFile, "Statistics (all expressions): Insert Count %d, "
	    "Save Count %d, Reload Count %d, Temp Phi Count %d, Hoisted Count %d\n",
	    _num_inserted_saves, _num_cse_saves, _num_cse_reloads, 
	    _num_temp_phis, _num_hoisted);
    fprintf(TFile, "Coderep Statistics (entire PU): previous count: %d new count: %d\n", 
	    orig_coderep_id_cnt, Htable()->Coderep_id_cnt());
    fprintf(TFile, "     Expr nodes changed to temps without rehashing: %d\n",
	    _num_temp_owners);
    Cfg()->Print(TFile);
    Lftr()->Print(TFile);
    if (Get_Trace(TKIND_ALLOC, TP_WOPT1)) {
      MEM_Trace();
    }
  }

  CXX_DELETE(_str_red,_etable_pool);
  CXX_DELETE(_lftr,_etable_pool);
  if (WOPT_Enable_Hoisting)
    Delete_EXP_HOISTING(_exp_hoisting);

#ifdef Is_True_On
#endif
}


void
ETABLE::Clear_dpo_exp_phi(void)
{
  BZERO( _dpo_exp_phi, Cfg()->Total_bb_count()*sizeof(_dpo_exp_phi[0]));
}
    
void
ETABLE::Clear_dpo_exp_phi(EXP_OCCURS_CONTAINER &worklist)
{
#ifdef Is_True_On
  INT32 count = 0;
#endif
  EXP_OCCURS        *exp_phi;
  EXP_OCCURS_ITER    exp_phi_iter;
  
  FOR_ALL_NODE (exp_phi, exp_phi_iter, Init(worklist.Head())) {
    _dpo_exp_phi[exp_phi->Bb()->Dom_dfs_id()] = NULL;
#ifdef Is_True_On
    count++;
#endif
  }
  Is_True(count == worklist.Len(),
	  ("ETABLE::Clear_dpo_exp_phi, list size is wrong"));
}

EXP_OCCURS*
ETABLE::Set_exp_phi_bb(const BB_NODE *bb, EXP_OCCURS *exp_phi)
{
  return _dpo_exp_phi[bb->Dom_dfs_id()] = exp_phi;
}
 
EXP_OCCURS*
ETABLE::Get_exp_phi_bb(const BB_NODE *bb)
{
  return _dpo_exp_phi[bb->Dom_dfs_id()];
}

static void Cleanup_loop_flags(BB_LOOP *first_loop, BOOL trace)
{
  if (first_loop != NULL) {
    // Clear LOOP_HAS_REAL_OCC flags used by LFTR on a per-expression basis.
    BB_LOOP_ITER  loop_iter;
    BB_LOOP      *loop;

    FOR_ALL_NODE(loop, loop_iter, Init(first_loop)) {
      Is_Trace(trace, (TFile, "BB%d ", loop->Body()->Id()));
      loop->Clear_flag(LOOP_HAS_REAL_OCC);
      Cleanup_loop_flags(loop->Child(), trace);
    }
  }
}

// per expression cleanup
void
ETABLE::Per_worklst_cleanup(EXP_WORKLST *exp_worklst) const
{
  // Clear phi pred cr
  BZERO(_phi_pred_cr, Cfg()->Total_bb_count() * sizeof(_phi_pred_cr[0]));

  // Clear LFTR Def_occur() pointers (comp occurs live across worklsts)
  Lftr()->Clear_def_occurs(exp_worklst);

  if (WOPT_Enable_New_SR &&
      Lftr()->Lftr_on() &&
      Str_red()->Candidate_opc(exp_worklst->Exp()->Op())) {
    Is_Trace(WOPT_Enable_Verbose && Tracing(),
	     (TFile, "Clearing LOOP_HAS_REAL_OCC flags:\n "));
    Cleanup_loop_flags(Cfg()->Loops(),
		       WOPT_Enable_Verbose && Tracing());
    Is_Trace(WOPT_Enable_Verbose && Tracing(),
	     (TFile, "\n"));
  }
}

void
ETABLE::Insert_stmtrep_after(STMTREP *new_stmt,
                             STMTREP *old_stmt)
{
  STMTREP *stmt = old_stmt;
  if (stmt->Is_saved_RHS()) {
    Is_True(stmt->Next(),
            ("ETABLE::Insert_stmtrep: cannot find the real statement"));
    stmt = stmt->Next();
  }
  stmt->Bb()->Insert_stmtrep_after( new_stmt, stmt );
  new_stmt->Set_stmt_id(stmt->Stmt_id());
}

void
COMP_UNIT::Do_new_pre(void)
{
  MEM_POOL etable_pool, phi_pool, etable_local_pool;

  OPT_POOL_Initialize(&etable_pool, "etable pool", FALSE, -1);
  OPT_POOL_Initialize(&phi_pool, "phi pool", FALSE, -1);
  OPT_POOL_Initialize(&etable_local_pool, "etable local pool", FALSE, -1);
  OPT_POOL_Push(&etable_pool, -1);
  OPT_POOL_Push(&phi_pool, -1);
  OPT_POOL_Push(&etable_local_pool, -1);

  {
    ETABLE etable(Cfg(), Opt_stab(), Htable(), Arule(), 10,
                  &etable_pool, &phi_pool, &etable_local_pool, this, PK_EPRE);
    etable.Perform_PRE_optimization();
  } // the etable destructor is called here

  OPT_POOL_Pop(&etable_local_pool, -1);
  OPT_POOL_Pop(&phi_pool, -1);
  OPT_POOL_Pop(&etable_pool, -1);
  OPT_POOL_Delete(&etable_local_pool, -1);
  OPT_POOL_Delete(&phi_pool, -1);
  OPT_POOL_Delete(&etable_pool, -1);
}

void
EXP_WORKLST::Verify(void)
{
  Is_True(_real_occurs.Head() != NULL && _real_occurs.Tail() != NULL,
	  ("EXP_WORKLST::Verify, null occurrence list"));
  Is_True(_exp != NULL,
	  ("EXP_WORKLST::Verify, null exp"));
  Is_True(_iterator != NULL,
	  ("EXP_WORKLST::Verify, null iterator"));
}

void
EXP_OCCURS::Print(FILE *fp, BOOL show_phi_preds) const
{
  if (Occurrence() != NULL) {
    if (WOPT_Enable_Verbose)
      Occurrence()->Print(1, fp);
    else
      fprintf(fp, "cr%d", Occurrence()->Coderep_id());
  } else {
    fprintf(fp, "cr???");
  }
  if (Occ_kind() != OCC_PHI_PRED_OCCUR ||
      Inserted_computation())
    fprintf(fp, " Ver%02d ", E_version());
  else
    fprintf(fp, "       ");
  switch (Occ_kind()) {
  case OCC_PHI_PRED_OCCUR:
  case OCC_REAL_OCCUR:
    if (Occ_kind() == OCC_PHI_PRED_OCCUR) {
      fprintf(fp, " (%s in BB%d)",
	      (For_spre())? "iphi-succ" : "phi-pred",
	      ((BB_NODE *) Bb())->Id());
    }
    if (Sunk_lvalue())
      fprintf(fp, " (sunk-lvalue)");
    if (Occ_kind() == OCC_REAL_OCCUR || Inserted_computation()) {
      if (Occ_kind() == OCC_PHI_PRED_OCCUR &&
	  Inserted_computation()) {
	fprintf(fp, " (inserted real)");
      }
      else {
	if (Occ_kind() != OCC_REAL_OCCUR) {
	  fprintf(fp, " (BAD KIND in BB%d Sid%d)",
		  Enclosed_in_stmt()->Bb()->Id(),
		  Enclosed_in_stmt()->Stmt_id());
	}
	else {
	  fprintf(fp, " (real in BB%d Sid%d)",
		  Bb()->Id(),
		  Is_flag_set(OCC_HOISTED) ? 0 : Enclosed_in_stmt()->Stmt_id());
	  if (WOPT_Enable_Verbose && !Is_flag_set(OCC_HOISTED) && !For_spre()) {
	    fprintf(fp, "Enclosing statement:\n");
	    Enclosed_in_stmt()->Print(fp);
	  }
	}
      }
      if (! For_spre()) {
        if (Occ_kind() == OCC_REAL_OCCUR)
	  fprintf(fp, " kid %d", Stmt_kid_num());
        if (Mult_real())
	  fprintf(fp, " (multi-real)");
	if (Occ_kind() == OCC_PHI_PRED_OCCUR) {
	  if (Is_flag_set(OCC_REQUIRED_PRED)) {
	    fprintf(fp, " (required)");
	  }
	}
	else {
	  if (Is_flag_set(OCC_AS_L_VALUE)) 
	    fprintf(fp, " (left-value)");
	}
        if (Is_flag_set(OCC_INJURED)) 
	  fprintf(fp, " (injured)");
        if (Occ_kind() == OCC_REAL_OCCUR) {
	  if (Delete_comp()) 
	    fprintf(fp, " (reload)");
	  if (Save_to_temp()) {
	    fprintf(fp, " (compute and save)");
	    if (T_ver_owns_coderep()) 
	      fprintf(fp, " (owns coderep)");
	  }
	  fprintf(fp, " rehash_cost: %d", Rehash_cost());
        }
      }
      else {
        if (Occ_kind() == OCC_REAL_OCCUR) {
	  if (Fake_store())
	    fprintf(fp, " (fake-store)");
	  if (Is_flag_set(OCC_AS_L_VALUE)) 
	    fprintf(fp, " (left-value)");
	  if (Delete_comp()) 
	    fprintf(fp, " (deleted)");
	  if (Save_to_temp()) 
	    fprintf(fp, " (cause earlier deletions)");
	  }
      }
    }
    fprintf(fp, "\n");
    break;
  case OCC_PHI_OCCUR:
    fprintf(fp, " (%s in BB%d) ", (For_spre()? "iphi" : "phi"), Bb()->Id());
    if (T_ver_owns_coderep()) 
      fprintf(fp, " (owns coderep)");
    Exp_phi()->Print(fp, show_phi_preds);
    break;
  case OCC_COMP_OCCUR:
    fprintf(fp, " (comp in BB%d Sid%d)%s\n",
	    Enclosed_in_stmt()->Bb()->Id(),
	    Enclosed_in_stmt()->Stmt_id(),
	    (Obsolete_comparison() ? " (obsolete)" : ""));
    break;
  case OCC_EXIT_OCCUR:
    fprintf(fp, "(exit in BB%d)\n",((BB_NODE *) Enclosed_in_bb())->Id());
    break;
  default:
    FmtAssert(FALSE,("Is an undefined occurrence (Kind == %d).\n",Occ_kind()));
    break;
  }
}
  
void
EXP_WORKLST::Print(FILE *fp, EXP_OCCURS_PAIR *comp_occurs)
{
  if (Pre_kind() != PK_SPRE)
    fprintf(fp,"<E_NUM=%d|PREG=%d", E_num(), Preg());
  else
    fprintf(fp,"<E_NUM=%d", E_num());
  if (Exp() != NULL) {
    Exp()->Print_node(1, fp);
  } else {
    fprintf(fp, "Has no expression coderep\n");
  }
  fprintf(fp," occurrences are:\n");
  EXP_OCCURS *exp_occ;

  EXP_ALL_OCCURS_ITER exp_occ_iter(Real_occurs().Head(),
				   comp_occurs,
                                   Phi_occurs().Head(),
				   Phi_pred_occurs().Head(),
                                   NULL /*no exit list*/);
  FOR_ALL_NODE(exp_occ, exp_occ_iter, Init())
    exp_occ->Print(fp, !Phi_preds_pruned());

  if (Pre_kind() != PK_SPRE) {
    fprintf(fp, "Statistics: %d insertions, "
	    "%d saves, %d reloads, %d temp phis\n",
	    _insert_cnt, _save_cnt, _reload_cnt, _temp_phi_cnt);
    fprintf(fp, "Statistics: %d expr nodes changed to temps without rehashing\n",
	    _temp_owner_cnt);
  }
  else {
    fprintf(fp, "Statistics: %d deletions, %d insertions\n",
	    _reload_cnt, _insert_cnt);
  }
}

void
EXP_OCCURS_CONTAINER::Print(FILE *fp)
{
  INT32 count = 0;
  EXP_OCCURS *exp_occ;
  EXP_OCCURS_ITER exp_occ_iter;
  FOR_ALL_NODE (exp_occ, exp_occ_iter, Init(Head())) {
    exp_occ->Print(fp);
    count++;
  }
  Is_True(count == Len(),("EXP_OCCURS_CONTAINER::Print, list size is wrong"));
}

#ifdef Is_True_On
// classes EXPREP and XTABLE are for use by ETABLE::Count_lex_ident_exprs() only

class EXPREP: public SLIST_NODE {
  friend class EXPREP_CONTAINER;
  friend class XTABLE;
private:
  CODEREP *_cr;				// corr CODEREP node (CK_IVAR or CK_OP)
  INT32 _exprep_id;			// unique id for each exprep node

  EXPREP(void);				// required undefined unwanted
  EXPREP(const EXPREP&);		// required undefined unwanted
  EXPREP& operator = (const EXPREP&);	// required undefined unwanted

  EXPREP(CODEREP *cr, INT32 id): _cr(cr), _exprep_id(id) {}
  ~EXPREP(void)	{}
};

class EXPREP_CONTAINER: public SLIST {
private:
  EXPREP_CONTAINER(const EXPREP_CONTAINER&);	// required undefined unwanted
  EXPREP_CONTAINER& operator = (const EXPREP_CONTAINER&);// required undefined unwanted

  DECLARE_SLIST_CLASS(EXPREP_CONTAINER,EXPREP)
  ~EXPREP_CONTAINER(void) {}
};

class EXPREP_ITER: public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(EXPREP_ITER, EXPREP, EXPREP_CONTAINER)
};

class XTABLE {
  friend void ETABLE::Count_lex_ident_exprs(INT32);
private:
  ARRAY<EXPREP *> _e_hash;		// the hash table
  ARRAY<EXPREP *> _cr_id_2_exprep;	// table mapping coderep_id to exprep
  INT32 _expr_count;			// count number of EXPREP nodes created
  INT32 _compound_count;
  MEM_POOL *_local_pool;
  OPT_STAB *_opt_stab;

  XTABLE(void);				// required undefined unwanted
  XTABLE(const XTABLE&);		// required undefined unwanted
  XTABLE& operator = (const XTABLE&);	// required undefined unwanted

  XTABLE(MEM_POOL *lpool, OPT_STAB *opt_stab, CODEMAP *htable): 
		_local_pool(lpool), _opt_stab(opt_stab), _expr_count(0), 
		_compound_count(0), _e_hash(CODE_ITABLE_SIZE, lpool),
		_cr_id_2_exprep(htable->Coderep_id_cnt(), lpool) {}
  ~XTABLE(void) {}

  BOOL Opnd_lex_identical(CODEREP *cr1, CODEREP *cr2) const;
  BOOL Lexically_identical(CODEREP *cr1, CODEREP *cr2) const;
  BOOL Is_compound(CODEREP *cr) const;
  IDX_32 Hashvalue(CODEREP *cr) const;
  IDX_32 Hash(CODEREP *cr) const;
  void Add_nonterm(CODEREP *cr);
  void Bottom_up_cr(CODEREP *cr);
};

// both cr1 and cr2 have already been processed for entry into xtable;
// if they are non-leaves, can compare their exprep's.
BOOL
XTABLE::Opnd_lex_identical(CODEREP *cr1, CODEREP *cr2) const
{
  if (cr1->Kind() != cr2->Kind()) 
    return FALSE;
  if (! cr1->Non_leaf() && cr1->Kind() != CK_VAR) 
    return cr1 == cr2;
  if (cr1->Kind() == CK_VAR) 
    return cr1->Aux_id() == cr2->Aux_id();
  if (cr1->Op() != cr2->Op()) 
    return FALSE;
  return _cr_id_2_exprep[cr1->Coderep_id()] == 
	 _cr_id_2_exprep[cr2->Coderep_id()];
}

// both cr1 and cr2 are non-terminals; their descendents must have been
// processed for entry into xtable.
BOOL
XTABLE::Lexically_identical(CODEREP *cr1, CODEREP *cr2) const
{
  if (cr1->Kind() != cr2->Kind()) return FALSE;
  if (cr1->Kind() == CK_IVAR) {
    if (cr1->Op() != cr1->Op()) return FALSE;
    if (cr1->Offset() != cr2->Offset()) return FALSE;
    if (Get_mtype_class(cr1->Dtyp()) != Get_mtype_class(cr2->Dtyp()))
      return FALSE;
    if (MTYPE_size_min(cr1->Dsctyp()) != MTYPE_size_min(cr2->Dsctyp()))
      return FALSE;
    CODEREP *base1 = cr1->Ilod_base() ? cr1->Ilod_base() : cr1->Istr_base();
    CODEREP *base2 = cr2->Ilod_base() ? cr2->Ilod_base() : cr2->Istr_base();
    if (! Opnd_lex_identical(base1, base2)) 
      return FALSE;
    if (cr1->Opr() == OPR_MLOAD) {
      if (! Opnd_lex_identical(cr1->Mload_size(), cr2->Mload_size())) 
	return FALSE;
    }
    else if (cr1->Opr() == OPR_ILOADX) {
      if (! Opnd_lex_identical(cr1->Index(), cr2->Index())) 
	return FALSE;
    }
  }
  else { // CK_OP
    if (cr1->Kid_count() != cr2->Kid_count()) return FALSE;
    if (OPCODE_commutative_op(cr1->Op()) == cr1->Op()) {
      if (! (Opnd_lex_identical(cr1->Opnd(0), cr2->Opnd(0)) &&
             Opnd_lex_identical(cr1->Opnd(1), cr2->Opnd(1))) &&
          ! (Opnd_lex_identical(cr1->Opnd(0), cr2->Opnd(1)) &&
             Opnd_lex_identical(cr1->Opnd(1), cr2->Opnd(0)))) 
	return FALSE;
    }
    else {
      for (INT32 i =0; i < cr1->Kid_count(); i++)
        if (! Opnd_lex_identical(cr1->Opnd(i), cr2->Opnd(i))) 
	  return FALSE;
    }
    // check additional fields for specific opcodes
    if (cr1->Opr() == OPR_INTRINSIC_OP)
      if (cr1->Intrinsic() != cr2->Intrinsic()) return FALSE;
#ifdef KEY
    if (cr1->Opr() == OPR_PURE_CALL_OP)
      if (cr1->Call_op_aux_id() != cr2->Call_op_aux_id()) return FALSE;
#endif
    if (cr1->Opr() == OPR_CVTL)
      if (cr1->Offset() != cr2->Offset()) return FALSE;
  }
  return TRUE;
}

IDX_32 
XTABLE::Hashvalue(CODEREP *cr) const
{
  if (inCODEKIND(cr->Kind(), CK_CONST|CK_RCONST|CK_LDA))
    return cr->Coderep_id() << 6;
  else if (cr->Kind() == CK_VAR)
    return cr->Aux_id() << 6;
  return _cr_id_2_exprep[cr->Coderep_id()]->_exprep_id << 6;
}

// hash values are made out of exprep IDs
IDX_32
XTABLE::Hash(CODEREP *cr) const
{
  if (cr->Kind() == CK_IVAR) {
    CODEREP *base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    return (Hashvalue(base) + cr->Offset()) % _e_hash.Size();
  }
  // CK_OP
  IDX_32 hvalue = 0;
  if (cr->Opr() != OPR_INTRINSIC_OP
#ifdef KEY
      && cr->Opr() != OPR_PURE_CALL_OP
#endif
     ) {
    for (INT32 i = 0; i < cr->Kid_count(); i++)
      hvalue += Hashvalue(cr->Opnd(i));
  }
  else {
    for (INT32 i = 0; i < cr->Kid_count(); i++)
      hvalue += Hashvalue(cr->Opnd(i)->Ilod_base());
  }
  return (cr->Op() + hvalue) % _e_hash.Size();
}

BOOL
XTABLE::Is_compound(CODEREP *cr) const
{
  if (cr->Kind() == CK_IVAR) {  // cannot be PARM node
    if (cr->Opr() == OPR_MLOAD)
      return TRUE;
    if (cr->Opr() == OPR_ILOADX)
      if (! cr->Index()->Is_non_volatile_terminal(_opt_stab))
	return TRUE;
    CODEREP *base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    if (! base->Is_non_volatile_terminal(_opt_stab))
      return TRUE;
    return FALSE;
  }
  // CK_OP
  if (cr->Is_isop_flag_set(ISOP_SSAPRE_OMITTED))
    return TRUE;
  if (cr->Opr() != OPR_INTRINSIC_OP
#ifdef KEY
      && cr->Opr() != OPR_PURE_CALL_OP
#endif
     ) {
    for (INT32 i = 0; i < cr->Kid_count(); i++)
      if (! cr->Opnd(i)->Is_non_volatile_terminal(_opt_stab)) 
        return TRUE;
  }
  else { // skip the PARM nodes
    for (INT32 i = 0; i < cr->Kid_count(); i++)
      if (! cr->Opnd(i)->Ilod_base()->Is_non_volatile_terminal(_opt_stab)) 
        return TRUE;
  }
  return FALSE;
}

// cr must be non terminal and must not be PARM node
void
XTABLE::Add_nonterm(CODEREP *cr)
{
  if (_cr_id_2_exprep[cr->Coderep_id()] != NULL)
    return;
  IDX_32 hash_idx = Hash(cr);
  EXPREP_CONTAINER bucket;
  bucket.Set_Head(_e_hash[hash_idx]);
  EXPREP_ITER exprep_iter;
  EXPREP *exp;
  BOOL found = FALSE;
  FOR_ALL_NODE(exp, exprep_iter, Init(&bucket)) 
    if (Lexically_identical(exp->_cr, cr)) {
      found = TRUE;
      break;
    } 
  if (! found) {
    _expr_count++;
    exp = (EXPREP *) CXX_NEW(EXPREP(cr, _expr_count), _local_pool);
    if (Is_compound(cr))
      _compound_count++;
    bucket.Prepend(exp);
    _e_hash[hash_idx] = bucket.Head();
  }
  else if (cr->Kind() == CK_OP &&
	   exp->_cr->Is_isop_flag_set(ISOP_SSAPRE_OMITTED)) {
    // check for need to decrement _compound_count
    if (! Is_compound(cr)) {
      exp->_cr = cr;
      _compound_count--;
    }
  }
  _cr_id_2_exprep[cr->Coderep_id()] = exp;
}

// PARM nodes (only due to INTRINSIC_OP here) are skipped and not entered 
// into XTABLE
// KEY: PARM nodes of PURE_CALL_OP are also skipped here.
void
XTABLE::Bottom_up_cr(CODEREP *cr)
{
  switch(cr->Kind()) {
  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
  case CK_VAR:
    return;
  case CK_IVAR: { // cannot be PARM node
    const OPERATOR ivar_opr = cr->Opr();
    if (cr->Ilod_base())
      Bottom_up_cr(cr->Ilod_base());
    else Bottom_up_cr(cr->Istr_base());
    if (ivar_opr == OPR_MLOAD)
      Bottom_up_cr(cr->Mload_size());
    else if (ivar_opr == OPR_ILOADX)
      Bottom_up_cr(cr->Index());
    Add_nonterm(cr);
    return;
    }
  case CK_OP: {
    if (cr->Opr() != OPR_INTRINSIC_OP
#ifdef KEY
        && cr->Opr() != OPR_PURE_CALL_OP
#endif
       ) {
      for (INT32 i=0; i < cr->Kid_count(); i++) 
        Bottom_up_cr(cr->Opnd(i));
      Add_nonterm(cr);
    }
    else { // skip the PARM nodes
      for (INT32 i=0; i < cr->Kid_count(); i++) 
        Bottom_up_cr(cr->Opnd(i)->Ilod_base());
      Add_nonterm(cr);
    }
    return;
    }
  default:
    Is_True(0, ("XTABLE::Bottom_up_cr: unexpected coderep kind"));
    return;
  }
}

// this is to count the number of simple and compound lexically identical
// expressions seen by SSAPRE, for statistics purpose only; called only if
// tracing is on
void
ETABLE::Count_lex_ident_exprs(INT32 simple_count)
{
  MEM_POOL local_pool;
  OPT_POOL_Initialize(&local_pool, "Count_lex_ident_exprs local pool", FALSE, -1);
  OPT_POOL_Push(&local_pool, -1);

  XTABLE xtable(&local_pool, Opt_stab(), Htable());

  // go through program, traverse CODEREPs bottom-up to build xtable
  BB_NODE *bb;
  CFG_ITER cfg_iter(Cfg());
  FOR_ALL_ELEM(bb, cfg_iter, Init()) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      CODEREP *rhs = stmt->Rhs();
      if (OPCODE_is_fake(stmt->Op())) {
	for (INT32 i = 0; i < rhs->Kid_count(); i++) 
	  if (rhs->Opnd(i)->Kind() == CK_IVAR &&
	      rhs->Opnd(i)->Opr() == OPR_PARM)
	    xtable.Bottom_up_cr(rhs->Opnd(i)->Ilod_base());
	  else xtable.Bottom_up_cr(rhs->Opnd(i));
	}
      else if (rhs != NULL) {
	xtable.Bottom_up_cr(rhs);
      }
      if (stmt->Lhs())
	xtable.Bottom_up_cr(stmt->Lhs());
    }
  }

  fprintf(TFile, "%8d simple and %8d compound expressions seen by SSAPRE in PU %s\n",
	  simple_count, xtable._compound_count, Cur_PU_Name);

  OPT_POOL_Pop(&local_pool, -1);
  OPT_POOL_Delete(&local_pool, -1);
}
#endif
