/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2007 (C) PathScale, LLC.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_verify.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_verify.cxx,v $
//
// Revision history:
//  23-MAR-95 dahl - Original Version
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
// 	Verifies the optimizer IR and warns of anything weird.
//
// 1) Make sure each ISTORE and MSTORE has a valid Istr_base.
// 2) Recalculates usecnts and compares them to the existing ones. If it
//	finds a descrepancy, it emits a warning and corrects the problem
//	so we can get it later. Also makes sure each CODEREP has a valid kind.
// 3) Verifies that dead code is really dead by making sure that there
//	are no pointers to it.
// 4) Make sure each STMTREP points to the BB it is located in.
// 5) Make sure each statements chi nodes point to a CR.
//      Also search CRs for def by chi, must have valid Defchi.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#define opt_verify_CXX	"opt_verify.cxx"

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "mempool.h"
#include "tracing.h"		// TFile and Get_Trace definition
#include "stab.h"
#include "opt_cfg.h"
#include "opt_ssa.h"
#include "opt_sym.h"		// for CHI
#include "opt_mu_chi.h"
#include "opt_util.h"		// for Warn_todo()
#include "opt_defs.h"		// for ILLEGAL_BP, must be before opt_itable.h
#include "opt_main.h"
#include "opt_base.h"		// FOR_ALL_NODE definition
#include "opt_htable.h"
#include "opt_rvi.h"
#include "opt_alias_mgr.h"
#include "bb_node_set.h"

#ifdef _LP64
#define UNDEFINED_PTR	(void *)0xa5a5a5a5a5a5a5a5LL
#else
#define UNDEFINED_PTR	(void *)0xa5a5a5a5
#endif /* _LP64 */

static BOOL past_ret_reg_def;	// for verifying no INTRINSIC_OP past it


BOOL
COMP_UNIT::Verify_IR(CFG *cfg, CODEMAP *htable, INT which)
{
#ifdef Is_True_On
  if (WOPT_Enable_Verify < 3)
    return TRUE;

  if (TFile != stdout)		// print header if trace file
    fprintf(TFile,"%sVerify_IR %d, checking IR\n%s",DBar,which,DBar);

  BB_NODE *bb;
  CFG_ITER cfg_iter(cfg);

  // 1) verify that each ISTORE and MSTORE has a valid Istr_base()
  FOR_ALL_ELEM (bb, cfg_iter, Init(cfg)) {	// go through all BBs
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init())	// go through all STMTs
      switch (stmt->Opr()) {
        case OPR_ISTORE:
        case OPR_ISTBITS:
        case OPR_MSTORE:
	  if (stmt->Lhs() == NULL || stmt->Lhs() == UNDEFINED_PTR ||
	      stmt->Lhs()->Istr_base() == NULL ||
	      stmt->Lhs()->Istr_base() == UNDEFINED_PTR)
	    fprintf(TFile,"*** Verify_IR %d, BB%d, %s null Istr_base\n",
		    which,bb->Id(),OPCODE_name(stmt->Op()));
	  break;
	default:
	  break;
      }
  }

  // 2) verify usecnts by counting parents for each node, go through
  //	entire program for each CODEREP.
  FOR_ALL_ELEM (bb, cfg_iter, Init(cfg)) {	// go through all BBs
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init())	// go through all STMTs
      stmt->Verify_IR(cfg,htable,which);
  }

  // 3) verify dead code by searching the program for each CODEREP,
  //	if nothing points to it then it's usecnt should be zero
  CODEREP_ITER cr_iter;
  CODEREP *tmpcr, *bucket;
  CODEMAP_ITER codemap_iter;
  FOR_ALL_ELEM(bucket, codemap_iter, Init(htable)) {
    FOR_ALL_NODE(tmpcr, cr_iter, Init(bucket)) {
      if (tmpcr->Count_parents(cfg,htable) == 0 && tmpcr->Usecnt() != 0) {
	fprintf(TFile,"*** Verify_IR %d, dead code with usecnt = %d\n",
		which,tmpcr->Usecnt());
	tmpcr->Print(0,TFile);
      }
    }
  }  

  // 4) verify that each stmtrep has it's bb pointer set appropriately
  FOR_ALL_ELEM (bb, cfg_iter, Init(cfg)) {	// go through all BBs
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init())	// go through all STMTs
      if (stmt->Bb() != bb) {
	char str[20];
	if (stmt->Bb() != NULL && stmt->Bb() != UNDEFINED_PTR)
	  sprintf(str,"BB%d",stmt->Bb()->Id());
	else
	  strcpy(str,"<null>");
	fprintf(TFile,
		"*** Verify_IR %d, BB%d, stmtrep bb pointer is wrong, %s\n",
		which,bb->Id(),str);
	stmt->Print(TFile);
	stmt->Set_bb(bb);	// fix the pointer
      }
  }

  // 5) verify that each stmtrep's chi list has chi nodes that point to a CR
  FOR_ALL_ELEM (bb, cfg_iter, Init(cfg)) {	// go through all BBs
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {	// go through all STMTs
      if (stmt->Has_chi()) {
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	CHI_LIST *chi_list = stmt->Chi_list();
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  if (cnode->Live()) {

	    // see if they point to anything at all
	    if (cnode->RESULT() == NULL || cnode->RESULT() == UNDEFINED_PTR) {
	      fprintf(TFile,"*** Verify_IR %d, BB%d, chi RESULT is null\n",
		      which,bb->Id());
	      continue;
	    }
	    if (cnode->OPND() == NULL || cnode->OPND() == UNDEFINED_PTR) {
	      fprintf(TFile,"*** Verify_IR %d, BB%d, chi OPND is null\n",
		      which,bb->Id());
	      continue;
	    }

	    // search CRs for RESULT and OPND
	    BOOL found_r = FALSE, found_o = FALSE;
	    FOR_ALL_ELEM(bucket, codemap_iter, Init(htable)) {
	      FOR_ALL_NODE(tmpcr, cr_iter, Init(bucket)) {
		if (tmpcr == cnode->RESULT())
		  found_r = TRUE;
		if (tmpcr == cnode->OPND())
		  found_o = TRUE;
	      }
	    }
	    if (!found_r)
	      fprintf(TFile,"*** Verify_IR %d, BB%d, chi RESULT not right\n",
		      which,bb->Id());
	    if (!found_o)
	      fprintf(TFile,"*** Verify_IR %d, BB%d, chi OPND not right\n",
		      which,bb->Id());

	    // search CRs for def by chi, must have valid Defchi
	    FOR_ALL_ELEM(bucket, codemap_iter, Init(htable)) {
	      FOR_ALL_NODE(tmpcr, cr_iter, Init(bucket)) {
		if (tmpcr->Is_flag_set(CF_DEF_BY_CHI)) {
		  if (tmpcr->Defchi() == NULL ||
		      tmpcr->Defchi() == UNDEFINED_PTR){
		    fprintf(TFile,"*** Verify_IR %d, CR with CF_DEF_BY_CHI ",
			    which);
		    fprintf(TFile,"set but no Defchi 0x%p\n",
			    tmpcr->Defchi());
		    tmpcr->Print(0,TFile);
		  }
		}
	      }
	    }
	    
	  } // Is_Valid
	}   // chi_list
      }     // Has_chi
    }       // stmt_list
  }         // bb

  if (TFile != stdout) {
    fprintf(TFile,"%s",DBar);
    htable->Print(TFile);	// print out the htable each time for reference
    fprintf(TFile,"%sVerify_IR %d, done checking IR\n%s",DBar,which,DBar);
  }
#endif
  return TRUE;
}

BOOL
STMTREP::Verify_IR(CFG *cfg, CODEMAP *htable, INT which)
{
#ifdef Is_True_On
  if (WOPT_Enable_Verify < 3)
    return TRUE;

  if (Rhs())
    Rhs()->Verify_IR(cfg,htable,TRUE,which);
  if (Lhs())
    Lhs()->Verify_IR(cfg,htable,FALSE,which);

  // there is no independent way to verify the statement node itself ??
#endif
  return TRUE;
}

BOOL
CODEREP::Verify_IR(CFG *cfg, CODEMAP *htable, BOOL is_rhs, INT which)
{
#ifdef Is_True_On
  if (WOPT_Enable_Verify < 3)
    return TRUE;

  switch (Kind()) {
    case CK_OP:
      {
	if (Kind() == CK_OP) {
	  for (INT i=0; i<Kid_count(); i++)
	    Opnd(i)->Verify_IR(cfg,htable,is_rhs,which);// recursive for kids
	}
      }
      // no break!
    case CK_CONST:
    case CK_RCONST:
    case CK_LDA:
    case CK_VAR:
    case CK_IVAR:
      {
	INT Nparents = Count_parents(cfg,htable);
	if (Nparents != Usecnt()) {
	  fprintf(TFile,"*** CODEREP::Verify_IR %d, usecnt (%d) and ",
		  which,Usecnt());
	  fprintf(TFile,"#parents (%d) don't agree\n",Nparents);
	  Print(0,TFile);
	} else if (Usecnt() <= 0 && is_rhs) {
	  fprintf(TFile,"*** CODEREP::Verify_IR %d, usecnt should not be zero\n",
		  which);
	  Print(0,TFile);
	}
      }
      break;
    case CK_DELETED:
    default:
      fprintf(TFile,"*** CODEREP::Verify_IR %d, unknown kind (0x%x)\n",
	      which,Kind());
      Print(0,TFile);
      break;
  }
#endif
  return TRUE;
}

// traverse the htable and see how many CODEREPs or STMTREPs
// point to a given CODEREP.
INT
CODEREP::Count_parents(CFG *cfg, CODEMAP *htable)
{
  INT	count = 0;

#ifdef Is_True_On
  // count the number of CODEREPs that point to this CODEREP
  CODEREP_ITER cr_iter;
  CODEREP *tmpcr, *bucket;
  CODEMAP_ITER codemap_iter;
  FOR_ALL_ELEM(bucket, codemap_iter, Init(htable)) {
    FOR_ALL_NODE(tmpcr, cr_iter, Init(bucket)) {
      switch (tmpcr->Kind()) {
        case CK_OP:
	  {
	    for (INT i=0; i<tmpcr->Kid_count(); i++)
	      if (tmpcr->Usecnt() > 0 && tmpcr->Opnd(i) == this)
		count++;
	  }
	  break;
	case CK_IVAR:
	  {
	    CODEREP *ctmp;
	    if (tmpcr->Ilod_base() != NULL &&
		tmpcr->Ilod_base() != UNDEFINED_PTR)
	      ctmp = tmpcr->Ilod_base();
	    else if (tmpcr->Istr_base() != NULL &&
		     tmpcr->Istr_base() != UNDEFINED_PTR)
	      ctmp = tmpcr->Istr_base();
	    else
	      FmtAssert(FALSE,("CODEREP::Count_parents, null base for IVAR"));

	    if (ctmp->Usecnt() > 0 && ctmp == this)
	      count++;
	  }
	  break;
	default:
	  break;
      }
    }
  }

  // count the number of STMTREPs that point to this CODEREP    
  CFG_ITER cfg_iter;
  STMTREP *stmt;
  BB_NODE *bb;
  FOR_ALL_NODE(bb, cfg_iter, Init(cfg)) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (stmt->Rhs() == this)
	count++;
      // do not count lhs because lhs is not part of usecnt
    }
  }

#endif
  return count;
} 

// verify that each WN has some alias information
// returns TRUE for ok, FALSE for not ok
// uses a C interface because it can be called anywhere in the backend
//
BOOL Verify_alias(ALIAS_MANAGER *am, WN *wn)
{
  BOOL ret = TRUE;
#ifdef Is_True_On

  if (!WOPT_Enable_Verify)
    return ret;

  if (wn == NULL)
    return ret;

  OPERATOR opr = WN_operator(wn);

  if (opr == OPR_BLOCK)  {
    if (WN_first(wn) && WN_prev(WN_first(wn))) {
      fprintf(TFile, "### Verify_alias, WN_first has prev stmt.");
      fdump_tree(TFile,wn);
    }
    if (WN_last(wn) && WN_next(WN_last(wn))) {
      fprintf(TFile, "### Verify_alias, WN_last has next stmt.");
      fdump_tree(TFile,wn);
    }
    for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt))  {
      if (!Verify_alias(am,stmt)) 
	ret = FALSE;
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn); i++) 
      if (!Verify_alias(am,WN_kid(wn,i)))
	ret = FALSE; 
  }

  // verify it has alias information
  if (OPERATOR_is_load(opr) || OPERATOR_is_store(opr)) {
    if (Valid_alias(am,wn) == FALSE) {
#ifdef WARN_TODO
      Warn_todo("Verify_alias: WN has no alias information");
      fprintf(TFile,"Verify_alias: WN has no alias information\n");
      fdump_wn_no_st(TFile, wn);
#endif
      ret = FALSE;
    }
  }
#endif  
  return ret;
}

#ifdef Is_True_On
STMTREP *cur_stmt;
#endif

inline void
Save_cur_stmt(STMTREP *stmt)
{
#ifdef Is_True_On
  cur_stmt = stmt;
#endif
}

static BOOL
Cur_stmt_is_entry_chi(void)
{
#ifdef Is_True_On
  return (cur_stmt->Opr() == OPR_OPT_CHI);
#endif
}

static void
Reset_find_def_processed(CFG *cfg)
{
  BB_NODE *bb;
  CFG_ITER cfg_iter(cfg);
  FOR_ALL_ELEM (bb, cfg_iter, Init(cfg)) {
    PHI_NODE *phi; 
    PHI_LIST_ITER phi_iter;
    //  Iterate through each phi-node 
    FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
      phi->Reset_find_def_processed();
    }
  }
}

// Verify that all SPRE-temp are defined.
static void
Verify_spre_temp(CODEREP *cr)
{
  Is_True(cr->Kind() == CK_VAR, ("Verify_spre_temp: not a CK_VAR."));
  Is_True(!cr->Is_flag_set(CF_IS_ZERO_VERSION), ("Verify_spre_temp: spre temp cannot be a zero ver."));
  Is_True(!cr->Is_flag_set(CF_DEF_BY_CHI), ("Verify_spre_temp: spre temp cannot be defined by chi."));
  if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    if (!cr->Defphi()->Find_def_processed()) {
      cr->Defphi()->Set_find_def_processed();
      PHI_OPND_ITER phi_opnd_iter(cr->Defphi());
      CODEREP *opnd;
      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
	Verify_spre_temp(opnd);
      }
    }
  } else {
    // defined by stmt, done!
    Is_True(cr->Defstmt() != NULL, ("Verify_spre_temp: null defstmt."));
  }
}


void
CODEREP::Verify_CODEMAP(CODEMAP *htable, OPT_STAB *opt_stab, BOOL allow_zero_ver)
{
#ifdef Is_True_On
  if (!WOPT_Enable_Verify)
    return;

  switch (Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return;
  case CK_VAR:
    FmtAssert(Offset() == opt_stab->St_ofst(Aux_id()) || Dsctyp() == MTYPE_BS,
	      ("bad offset field in CODEREP."));

    if (!Is_flag_set( (CR_FLAG) (CF_DEF_BY_PHI|CF_DEF_BY_CHI)) &&
        Defstmt() != NULL) {
       FmtAssert(Lod_ty() != 0, ("load ty is NULL."));
    }

    // Test volatile variable for consistency
    if (opt_stab->Is_volatile(Aux_id())) {
      Is_True( Is_var_volatile(),
	("Volatile variable %d has non-volatile cr%d",Aux_id(), Coderep_id()) );
      break;
    }
    if (Is_var_volatile()) {
      // the coderep should not be marked volatile if the opt-stab
      // is not (as checked immediately above)
      Is_True( opt_stab->Is_volatile(Aux_id()),
	("Volatile cr for var %d is not volatile in stab",Aux_id()) );
      break;
    }

    if (!allow_zero_ver) {
      FmtAssert(!Is_flag_set(CF_IS_ZERO_VERSION),
		("zero ver not allowed in expr."));
    }

    if (Is_var_nodef() && !Is_flag_set(CF_IS_ZERO_VERSION))
      Is_True(Cur_stmt_is_entry_chi(),
              ("corrupted htable: CR loses its definition, but "
	       "current statement is not entry chi"));

    FmtAssert(!Is_var_nodef() || Version() == 0 || Version() == 1 ||
	       Is_flag_set(CF_IS_ZERO_VERSION),
	      ("corrupted htable:  CR loses its definition."));

    if (opt_stab->Aux_stab_entry(Aux_id())->SPRE_temp() &&
	!Cur_stmt_is_entry_chi() && !allow_zero_ver) {
      // Verify that all SPRE temp are fully available.  
      // Notice non SPRE temp might have their value not defined on some paths.
      Verify_spre_temp(this);
    }

    break;
  case CK_IVAR:
    {
      Ilod_base()->Verify_CODEMAP(htable, opt_stab, FALSE);
      if (Opr() == OPR_MLOAD)
	Mload_size()->Verify_CODEMAP(htable, opt_stab, FALSE);
      if (Opr() == OPR_ILOADX)
	Index()->Verify_CODEMAP(htable, opt_stab, FALSE);
      MU_NODE *mnode = Ivar_mu_node();
      if (mnode)
	FmtAssert(mnode->Aux_id() != opt_stab->Return_vsym(),
		  ("return vsym cannot be in mu of ivar node"));
      if (OPERATOR_is_scalar_iload (Opr()) ||
	  Opr() == OPR_MLOAD ||
	  Opr() == OPR_ILOADX) {
	if (mnode == NULL)
	  Warn_todo("ILOAD has no mu node.");
      }
      if (mnode)
	mnode->OPND()->Verify_CODEMAP(htable, opt_stab, TRUE);
    }
    break;
  case CK_OP:
    if (Is_isop_flag_set(ISOP_VER_MAP_VISITED))
      return;
    Set_isop_flag(ISOP_VER_MAP_VISITED);
    {
      for (INT32 i = 0; i < Kid_count(); i++) 
	Opnd(i)->Verify_CODEMAP(htable, opt_stab, FALSE);
    }
    break;
  }
#endif
}

void
STMTREP::Verify_CODEMAP(CODEMAP *htable, OPT_STAB *opt_stab)
{
#ifdef Is_True_On
  if (!WOPT_Enable_Verify)
    return;

  Save_cur_stmt(this);
  if (OPERATOR_is_call(Opr()))
    for (INT32 i = 0; i < Rhs()->Kid_count(); i++) {
      Rhs()->Opnd(i)->Verify_CODEMAP(htable, opt_stab, FALSE);
    }
  else if (Rhs()) 
    Rhs()->Verify_CODEMAP(htable, opt_stab, FALSE);

  switch (Opr()) {
  case OPR_ISTORE:
  case OPR_ISTBITS:
    Lhs()->Istr_base()->Verify_CODEMAP(htable, opt_stab, FALSE);
    FmtAssert(Opr() == OPR_ISTBITS && Lhs()->Opr() == OPR_ILDBITS ||
	      Opr() == OPR_ISTORE && Lhs()->Opr() == OPR_ILOAD,
	      ("STMTREP::Verify_CODEMAP: inconsistent store operator with respect to bitfields"));
    break;
  case OPR_MSTORE:
    Lhs()->Istr_base()->Verify_CODEMAP(htable, opt_stab, FALSE);
    Lhs()->Mstore_size()->Verify_CODEMAP(htable, opt_stab, FALSE);
    break;
  case OPR_STID:
  case OPR_STBITS: {
    // Verify the CODEREP defined by this statement has 'def' 
    FmtAssert(!Lhs()->Is_var_nodef(),
	      ("corrupted htable:  STMTREP loses its definition."));
    FmtAssert(!Lhs()->Is_flag_set(CF_DEF_BY_PHI) &&
	      !Lhs()->Is_flag_set(CF_DEF_BY_CHI) &&
	      Lhs()->Defstmt() == this,
	      ("STMTREP::Verify_CODEMAP: LHS of store must be def by store"));

    ST *st = opt_stab->St(Lhs()->Aux_id());

    // do not test registers because epre might not introduce all phi functions
    if (st != NULL && ST_sclass(st) != SCLASS_REG) {
      BB_NODE_SET_ITER    df_iter;
      BB_NODE *bb_phi;
      FOR_ALL_ELEM (bb_phi, df_iter, Init(Bb()->Dom_frontier())) {
	if (htable->Phi_hash_valid() &&
	    !htable->Lookup_var_phi(bb_phi, Lhs()->Aux_id()))
	  DevWarn("CODEMAP: STID has no phi.");
      }
    }

    FmtAssert(Opr() == OPR_STBITS && Lhs()->Bit_field_valid() ||
	      Opr() == OPR_STID && ! Lhs()->Bit_field_valid(),
	      ("STMTREP::Verify_CODEMAP: inconsistent store operator with respect to bitfields"));
    break;	
    }
  default: ;
  }
  if (Has_mu()) {
    MU_LIST_ITER mu_iter;
    MU_NODE *mnode;
    FOR_ALL_NODE( mnode, mu_iter, Init(Mu_list())) {
      if (mnode->OPND())
	mnode->OPND()->Verify_CODEMAP(htable,opt_stab, TRUE);
    }
  }
  if (Has_chi()) {
    CHI_LIST_ITER chi_iter;
    CHI_NODE *cnode;
    FOR_ALL_NODE( cnode, chi_iter, Init(Chi_list())) {
      if (! cnode->Live()) continue;
      if (cnode->RESULT()) {
	cnode->RESULT()->Verify_CODEMAP(htable,opt_stab, TRUE);
	
	CODEREP *res = cnode->RESULT();

	FmtAssert(res->Is_flag_set(CF_IS_ZERO_VERSION) ||
		  res->Is_var_nodef() ||
		  (res->Is_flag_set(CF_DEF_BY_CHI) &&
		   res->Defchi() == cnode),
		  ("STMTREP::Verify_CODEMAP: LHS of chi must be def by "
		   "chi stmt"));

	if (!res->Is_flag_set(CF_IS_ZERO_VERSION)) {
//  
//   The aux symbol table allows volatile variables that are virtual variable
//   to be entered into the mu/chi list.  Real volative variables are not
//   entered though.  It is inconsistent and should be clean up.   After the
//   cleanup, changes in be/opt/opt_remame -r1.5 can be backed out.
//      -Raymond 6/24/98.
// 
//	  if (res->Is_var_volatile() ||
//	      opt_stab->Is_volatile(res->Aux_id())) {
//	    DevWarn("Found volatile in chi list, aux ID %d",
//		    res->Aux_id());
//	  }
	  cnode->OPND()->Verify_CODEMAP(htable,opt_stab,TRUE);

	  // Verify the CHI result has def_chi
	  FmtAssert(res->Is_flag_set(CF_DEF_BY_CHI), 
		    ("corrupted htable:  CHI result is not DEF_BY_CHI."));
	  FmtAssert(!res->Is_var_nodef(),
		    ("corrupted htable:  CHI result loses its definition."));

	  ST *st = opt_stab->St(res->Aux_id());

	  // do not test registers because epre might not introduce all phi functions
	  if (st != NULL && ST_sclass(st) != SCLASS_REG) {
	    BB_NODE_SET_ITER    df_iter;
	    BB_NODE *bb_phi;
	    FOR_ALL_ELEM (bb_phi, df_iter, Init(Bb()->Dom_frontier())) {
	      if (htable->Phi_hash_valid() &&
		  !htable->Lookup_var_phi(bb_phi, res->Aux_id()))
		DevWarn("CODEMAP: chi result has no phi.");
	    }
	  }
	}
	else {
	  // don't visit chi operand if chi result is zero version???
	}
      }
    }
  }
#endif
}


// Verify the CODEMAP table.
//    1. the offset field of each CR
//    2. the defstmt/defchi/defphi of each CR.
//
BOOL
COMP_UNIT::Verify_CODEMAP(void)
{
#ifdef Is_True_On
  if (!WOPT_Enable_Verify)
    return TRUE;

  Reset_find_def_processed(Cfg());

  BB_NODE *bb;
  CFG_ITER cfg_iter(Cfg());
  
  FOR_ALL_ELEM (bb, cfg_iter, Init(Cfg())) {	
    PHI_LIST_ITER phi_iter;
    PHI_NODE     *phi;
    FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
      if (phi->Live()) {
        BB_NODE *pred; BB_LIST_ITER bb_iter;
        FOR_ALL_ELEM (pred, bb_iter, Init(bb->Pred())) {
          CODEREP *opnd = phi->OPND(bb_iter.Idx());
          FmtAssert(opnd, ("NULL phi operand."));
          opnd->Verify_CODEMAP(Htable(), Opt_stab(), TRUE);
        }      
      }
    }

    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      stmt->Verify_CODEMAP(Htable(), Opt_stab());
    }
  }

  Reset_find_def_processed(Cfg());

#endif
  return TRUE;
}


// #ifdef  Verify_version...
#ifdef Is_True_On


// Check that the given coderep's Defbb() dominates the bb where the
// coderep is used.
BOOL
Def_before_use(CODEREP *cr, const BB_NODE *use_bb)
{
  // Defbb() is not set for zero versions since their points of
  // definition are unknown.
  switch (cr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return TRUE;
  case CK_VAR:
    if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
	cr->Is_var_volatile()) return TRUE;
    FmtAssert(cr->Defbb(), ("Def_before_use: Defbb not defined"));
    return cr->Defbb()->Dominates(use_bb);
  case CK_IVAR:
    if (!Def_before_use(cr->Ilod_base(), use_bb)) {
      DevWarn("Def_before_use: ilod_base not def before use.");
      return FALSE;
    }
    if (cr->Ivar_mu_node() != NULL) {
      if (!(cr->Ivar_mu_node()->OPND()->Is_flag_set(CF_IS_ZERO_VERSION) ||
	    Def_before_use(cr->Ivar_mu_node()->OPND(), use_bb))) {
	DevWarn("Def_before_use: mu opnd not def before use.");
	return FALSE;
      }
    } else {
      FmtAssert(cr->Opr() == OPR_PARM ||
		cr->Opr() == OPR_PREFETCH, 
		("Def_before_use:  CK_IVAR node has null mu-node."));
    }
    return TRUE;
  case CK_OP:
    {
        /* If this node has been visited and has the same BB
         * return true. If there was a use before the def it would
         * have been flaged on the first traversal.
         * This limits but not eliminates redundant calls.
         */
        if (cr->Is_isop_flag_set(ISOP_DEF_BEFORE_VISITED)
        &&  use_bb == cr->Get_ISOP_def_before_use_cache())
        {
            // Would have already warned
            return TRUE;
        }
        else
        {
            /* Set the visited flag and cache the BB.
             * We cache the BB as this functions makes decisions based
             * in it. Therefore, only paths with the same BB  as the last
             * can be eliminated.
             */
            cr->Set_isop_flag(ISOP_DEF_BEFORE_VISITED);
            cr->Set_ISOP_def_before_use_cache(use_bb);
        }
      for (INT j = 0; j < cr->Kid_count(); j++)
	if (!Def_before_use(cr->Opnd(j), use_bb)) {
	  DevWarn("Def_before_use: inserted use before def (Opnd(%d))", j);
	  return FALSE;
	}
      return TRUE;
    }
  default:
    return FALSE;
  }
}


static void
Verify_has_zver(STMTREP *stmt)
{
  CHI_LIST_ITER chi_iter;
  CHI_NODE *chi;
  bool has_zver = false;
  FOR_ALL_NODE(chi, chi_iter, Init(stmt->Chi_list())) {
    if (! chi->Live())
      continue;
    if (chi->Dse_dead()) 
      continue;
    CODEREP *res = chi->RESULT();
    if (res->Is_flag_set(CF_IS_ZERO_VERSION)) {
      has_zver = true;
      break;
    }
  }
  FmtAssert(has_zver == (stmt->Has_zver() != 0),
	    ("Stmt has_zver is wrong."));
}


// Verify CODEREP
static void
Verify_version_CR(CODEREP *cr, OPT_STAB *opt_stab, BB_NODE *bb, INT32 linenum)
{
  if (!WOPT_Enable_Verify)
    return;

  IDTYPE id = cr->Aux_id();

  if ( cr->Kind() == CK_VAR && cr->Is_var_nodef() && 
       !cr->Is_flag_set(CF_IS_ZERO_VERSION) && !cr->Is_var_volatile())
  {
    // mu references may have null-TYs, which is the only way we can
    // check for volatiles with the cr directly.  So, also check the
    // symbol table.
    if ( ! opt_stab->Is_volatile(cr->Aux_id()) ) {
      // Check that the definition is entry chi.
      // Alternatively, the definition may not exist: for example if
      // a region defines a variable that is not used in the surrounding
      // PU (the var will be in the region's chi but nowhere else).
      Is_True(Cur_stmt_is_entry_chi() ||
	      (cur_stmt->Op() == OPC_REGION && cur_stmt->Black_box()),
	("Verify_version_CR: Found no-def, but current statement is "
	 "not entry chi"));
    }
  }

  if (opt_stab->NULL_coderep(id)) {
    if (!cr->Is_flag_set(CF_IS_ZERO_VERSION) && !cr->Is_var_volatile()) {
      FmtAssert(FALSE, ("sym%dv%d(cr%d) use before def in bb%d line %d",
			cr->Aux_id(), cr->Version(), cr->Coderep_id(), 
			bb->Id(), linenum));
    }
    else {
      // No definition for zero version or volatile is OK.
      return;
    }
  }

  const CODEREP *cur_ver = opt_stab->Top_coderep(id);

  // version match, return OK
  if (cr == cur_ver) 
    return;
  
  if (cr->Is_var_volatile())
    return;

  if (cur_ver->Is_var_volatile())
    return;

  // if the expr version is zero version, it is OK to not agree with
  // the current version.   
  if (cr->Is_flag_set(CF_IS_ZERO_VERSION)) {
    // A zero version cannot be defined by non-zero versions
    // (unless the definition has been resurrected by Find_def and marked
    // CF_INCOMPLETE_USES).  
    // Otherwise, we might have a definition that has some use
    // but delete-able by DCE.
    Is_True(cur_ver->Is_flag_set(CF_INCOMPLETE_USES),
	    ("Zero version cr at BB%d live-range-overlaps with sym%dv%d(cr%d) not marked CF_INCOMPLETE_USES", 
	     bb->Id(), cur_ver->Aux_id(), cur_ver->Version(),
	     cur_ver->Coderep_id()));
    return;
  }

  // open64.net bug889. The Chi/Phis to return_vsym are marked live only 
  // when the return stmt in the exit BB is marked live and 
  // then propagated back to references in other BBs. However, in the case of
  // return stmt is deleted by DCE or (unreachable from the first BB), 
  // remaining stmts with Chi to return_vsym will get a reference error
  // we allow this corner case since we don't want change the main DCE algorithm
  // and allow this still do right compiling.

  if (cr->Aux_id() == opt_stab->Return_vsym() &&
      !(opt_stab->Cfg()->Exit_bb()->Reached() ||
        opt_stab->Cfg()->Fake_exit_bb()->Reached()))
    return;

  FmtAssert(FALSE, ("sym%dv%d(cr%d) live range overlapped with cur_version sym%dv%d(cr%d) in bb%d line %d",
		    cr->Aux_id(), cr->Version(), cr->Coderep_id(), 
		    cur_ver->Aux_id(), cur_ver->Version(), cur_ver->Coderep_id(),
		    bb->Id(), linenum));
}


// Verify the expression is current version
static void
Verify_version_mu_list(MU_LIST *mu_list, OPT_STAB *opt_stab, BB_NODE *bb, INT32 linenum)
{
  if (!WOPT_Enable_Verify)
    return;

  MU_NODE *mnode;
  MU_LIST_ITER mu_iter;
  FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
    Verify_version_CR(mnode->OPND(), opt_stab, bb, linenum);
  }
}

// Verify the chi opnd is the current version
static void
Verify_version_chi_list(CHI_LIST *chi_list, OPT_STAB *opt_stab, BB_NODE *bb, INT32 linenum)
{
  if (!WOPT_Enable_Verify)
    return;

  CHI_NODE *cnode;
  CHI_LIST_ITER chi_iter; 
  FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
    if ( cnode->Live() ) {
      Verify_version_CR(cnode->OPND(), opt_stab, bb, linenum);
    }
  }
}


// Verify the expression is current version
static void
Verify_version_expr(CODEREP *expr, OPT_STAB *opt_stab, BB_NODE *bb, INT32 linenum)
{
  if (!WOPT_Enable_Verify)
    return;

  switch (expr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST: return;
  case CK_VAR:
    {
      TY_IDX ty = expr->Lod_ty();
      if (ty && TY_is_volatile(ty))
	return;
      Verify_version_CR(expr, opt_stab, bb, linenum);
      return;
    }
  case CK_IVAR:
    if (expr->Ilod_base())
      Verify_version_expr(expr->Ilod_base(), opt_stab, bb, linenum); 
    if (expr->Istr_base())
      Verify_version_expr(expr->Istr_base(), opt_stab, bb, linenum);
    if (expr->Opr() == OPR_MLOAD)
      Verify_version_expr(expr->Mload_size(), opt_stab, bb, linenum); 
    if (expr->Opr() == OPR_ILOADX)
      Verify_version_expr(expr->Index(), opt_stab, bb, linenum); 
    if (! WOPT_Enable_VN_Full) {
      MU_NODE *mnode = expr->Ivar_mu_node();
      if (mnode)
	Verify_version_CR(mnode->OPND(), opt_stab, bb, linenum);
      // full value numbering causes overlap for the vsyms
    }
    // Virtual variable of indirect load always has non-zero version.
    if (expr->Ivar_mu_node() != NULL &&
	OPERATOR_is_scalar_iload (expr->Opr())) {
      FmtAssert(!expr->Ivar_mu_node()->OPND()->Is_flag_set(CF_IS_ZERO_VERSION),
		("CK_IVAR cr%d has zero version mu operand.", expr->Coderep_id()));
    }
    return;
    
  case CK_OP:
    {
        /* Here if the node has been visited we simply return
         *  as any corrective action should have been taken previously.
         *  otherwise we set visited and continue.
         *  Note: If ever this function OR a called function conditionally
         *  depends on anything other then the CR (expr) we can not simply
         *  return and must cache the information per path see Def_before_use
         */
        if (expr->Is_isop_flag_set(ISOP_VERIFY_EXPR_VISITED))
            return;
        else
            expr->Set_isop_flag (ISOP_VERIFY_EXPR_VISITED);
      for (INT32 i = 0; i < expr->Kid_count(); i++) {
	Verify_version_expr(expr->Opnd(i), opt_stab, bb, linenum); 
      }

      FmtAssert(! past_ret_reg_def || expr->Opr() != OPR_INTRINSIC_OP,
		("cr%d is INTRINSIC_OP appearing after the def of a return register", expr->Coderep_id()));
#ifdef KEY
      FmtAssert(! past_ret_reg_def || expr->Opr() != OPR_PURE_CALL_OP,
		("cr%d is PURE_CALL_OP appearing after the def of a return register", expr->Coderep_id()));
#endif
    }
    return;
    
  default:
    return;
  }
}


// the mu-node of an CK_IVAR must either
//   1) is null;
//   2) match the result of a !dse_dead chi node.
static void
Verify_version_istore(STMTREP *stmt, BB_NODE *bb, CODEMAP *htable, OPT_STAB *opt_stab)
{
  CODEREP *lhs = stmt->Lhs();
  MU_NODE *mnode = lhs->Ivar_mu_node();
  if (mnode == NULL) return;

  CHI_LIST_ITER chi_iter;
  CHI_NODE *cnode;
  FOR_ALL_NODE( cnode, chi_iter, Init(stmt->Chi_list())) {
    if (cnode->Aux_id() == lhs->Ivar_occ()->Aux_id()) {
      FmtAssert(!cnode->Dse_dead(), ("CK_IVAR has non-null mu-node, but dse-dead chi."));
      FmtAssert(cnode->RESULT() == mnode->OPND(), ("CK_IVAR has different mu/chi of vsym."));
      return;
    }
  }
  FmtAssert(TRUE, ("CK_IVAR has no chi of vsym."));
}


// Verify that no overlapped live range in the BB
static void
Verify_version_BB(BB_NODE *bb, CODEMAP *htable, OPT_STAB *opt_stab)
{
  if (!WOPT_Enable_Verify)
    return;

  PHI_NODE *phi; PHI_LIST_ITER phi_iter;

  //  Iterate through each phi-node 
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    if (phi->Live()) 
      opt_stab->Push_coderep(phi->RESULT()->Aux_id(), phi->RESULT());
  }

  past_ret_reg_def = FALSE;
  
  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    Save_cur_stmt(stmt);

    INT32 linenum = Srcpos_To_Line(stmt->Linenum());
    if (stmt->Has_chi()) {
      Verify_version_chi_list(stmt->Chi_list(), opt_stab, bb, linenum);
      Verify_has_zver(stmt);
    }

    if (stmt->Has_mu())
      Verify_version_mu_list(stmt->Mu_list(), opt_stab, bb, linenum);

    if (stmt->Rhs()) {
      Verify_version_expr(stmt->Rhs(), opt_stab, bb, linenum);
      FmtAssert(Def_before_use(stmt->Rhs(), bb), ("use before def."));
    }

    if (stmt->Lhs() && stmt->Lhs()->Kind() == CK_IVAR) {
      Verify_version_expr(stmt->Lhs()->Istr_base(), opt_stab, bb, linenum);
      FmtAssert(Def_before_use(stmt->Lhs()->Istr_base(), bb), ("use before def."));
    }

    if (OPERATOR_is_scalar_store (stmt->Opr()))
      opt_stab->Push_coderep(stmt->Lhs()->Aux_id(), stmt->Lhs());
    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	if (! cnode->Live()) continue;
	CODEREP *res = cnode->RESULT();
	if (res) 
	  opt_stab->Push_coderep(res->Aux_id(), res);
      }
    }

    if (OPERATOR_is_scalar_istore (stmt->Opr()))
      Verify_version_istore(stmt, bb, htable, opt_stab);

    if (OPERATOR_is_scalar_store (stmt->Opr()) &&
        opt_stab->Aux_stab_entry(stmt->Lhs()->Aux_id())->Is_dedicated_preg())
      past_ret_reg_def = TRUE;
  }

  if (WOPT_Enable_Verify >= 2) {
    BB_LIST_ITER bb_iter;
    BB_NODE *succ;
    FOR_ALL_ELEM(succ, bb_iter, Init(bb->Succ())) {
      INT32 pos = succ->Pred()->Pos(bb);
      FOR_ALL_ELEM (phi, phi_iter, Init(succ->Phi_list())) {
        if (phi->Live()) 
	  Verify_version_CR(phi->OPND(pos), opt_stab, succ, -1/* to indicate phi operand */);
      }
    }
  }
  
  // recursive call for kids in dominator tree
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Verify_version_BB(dom_bb, htable, opt_stab);
  }
  
  // iterate through each statement in this bb
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	if (! cnode->Live()) continue;
	CODEREP *res = cnode->RESULT();
	if (res)
	  opt_stab->Pop_coderep(res->Aux_id());
      }
    } 
    if (OPERATOR_is_scalar_store (stmt->Opr()))
      opt_stab->Pop_coderep(stmt->Lhs()->Aux_id());
  }
  
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    if (phi->Live()) 
      opt_stab->Pop_coderep(phi->RESULT()->Aux_id());
  }
}
#endif


//  Verify version
void
COMP_UNIT::Verify_version(void)
{
#ifdef Is_True_On
  if (!WOPT_Enable_Verify)
    return;

  MEM_POOL verify_pool;
  OPT_POOL_Initialize(&verify_pool, "verify pool", FALSE, MEM_DUMP_FLAG+13);
  OPT_POOL_Push(&verify_pool, MEM_DUMP_FLAG+13);
  
  Opt_stab()->New_coderep(&verify_pool);
  Opt_stab()->Clear_coderep();
  Opt_stab()->Check_stack();
  Verify_version_BB(Cfg()->Entry_bb(), Htable(), Opt_stab());
  
  OPT_POOL_Pop(&verify_pool, MEM_DUMP_FLAG+13);
  OPT_POOL_Delete(&verify_pool, MEM_DUMP_FLAG+13);
#endif
}


// Verify expr in the bucket are still hashed to the same bucket.
// (detect if any expr is modified directly)
//
void
CODEMAP::Verify_hashing(void)
{
#ifndef KEY // bug 13042: this is found to cause the WHIRL output by wopt to
  	    // have some operands of ADD arbitrarily swapped
#ifdef Is_True_On
  CODEREP_ITER cr_iter;
  CODEREP *cr,*bucket;
  CODEMAP_ITER codemap_iter;

  FOR_ALL_ELEM(bucket, codemap_iter, Init(this)) {
    IDX_32 hash_idx = codemap_iter.Cur();
    FOR_ALL_NODE(cr, cr_iter, Init(bucket)) {
      switch (cr->Kind()) {
      case CK_VAR:	
	break;
      case CK_IVAR:  
        break;   // do not check until the rehash of istore is resolved.
      case CK_OP:
	if (OPERATOR_is_call(cr->Opr()))
	  break;
      case CK_LDA:	
      case CK_CONST: 
      case CK_RCONST:
	// Make this into assertion after bug fixes
	if (hash_idx != Hash(cr)) {
	}
      }
    }
  }
#endif
#endif
}
