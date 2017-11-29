//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_essa.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_essa.cxx,v $
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
// SSA PRE Step 3:
//  Assign same e_version to the EXP_OCCURS that have the same value.
//  It works on one worklist at a time.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#define opt_essa_CXX	"opt_essa.cxx"

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "opt_defs.h"
#include "opt_base.h"
#include "opt_config.h"
#include "cxx_memory.h"
#include "bb_node_set.h"
#include "opt_bb.h"
#include "opt_htable.h"
#include "opt_ssa.h"
#include "opt_etable.h"
#include "opt_estr.h"
#include "opt_alias_rule.h"
#include "opt_mu_chi.h"
#include "opt_lftr2.h"


// ESSA:  Expr SSA class
//
class ESSA {

private:
  MEM_POOL            _mpool;           // ESSA private mempool
  STACK<EXP_OCCURS*> *_stack;           // Rename stack
  ETABLE             *_etable;          // Pointer to the etable
  STR_RED            *_str_red;         // Strength-reduction class
  EXP_WORKLST        *_worklist;        // current worklist
  const CODEREP      *_cur_e_expr;      // the expr corrs to the worklist
  const ALIAS_RULE   *_rule;      	// alias rule
  OPT_STAB           *_opt_stab;        // opt_stab
 
  STACK<EXP_OCCURS*> *Stack(void) const      { return _stack; }
  ETABLE	     *Etable(void) const     { return _etable; }
  STR_RED	     *Str_red(void) const    { return _str_red; }
  EXP_WORKLST        *Worklist(void) const   { return _worklist; }
  const CODEREP      *Cur_e_expr(void) const { return _cur_e_expr; }
  const ALIAS_RULE   *Rule(void) const       { return _rule; }
  OPT_STAB           *Opt_stab(void) const   { return _opt_stab; }
  BOOL		      Tracing(void) const    { return _etable->Tracing(); }

  inline BOOL         Lod_modified_real_occ_real_occ(const CODEREP *,
						     const CODEREP *) const;
  BOOL                Lod_modified_phi_result(const BB_NODE *,
					      const CODEREP *) const;
  BOOL                Lod_modified_real_occ_phi_opnd(const CODEREP *, 
						     const BB_NODE *,
						     INT) const;

  BOOL                Ilod_modified_phi_result(const BB_NODE *,
					       const CODEREP *) const;
  BOOL                Ilod_modified_real_occ_phi_opnd(const BB_NODE *,
						      const CODEREP *,
						      const BB_NODE *,
						      INT) const;
  BOOL                Ilod_modified_real_occ_real_occ(const BB_NODE *,
						      const CODEREP *,
						      const CODEREP *) const;

  BOOL                Same_e_version_real_occ_real_occ(const EXP_OCCURS *,
						       EXP_OCCURS *) const;
  BOOL                Same_e_version_phi_result(const EXP_OCCURS *,
						const CODEREP *,
						EXP_OCCURS *) const;
  BOOL                Same_e_version_real_occ_phi_opnd(const EXP_OCCURS *,
						       BB_NODE *,
						       INT,
						       const CODEREP *,
						       BOOL *) const;

  BOOL Injured_real_occ_real_occ(const EXP_OCCURS *def, EXP_OCCURS *use) const;
  BOOL Injured_phi_result_real_occ(const EXP_OCCURS *def,
				   const CODEREP *use_cr,
				   BB_NODE *use_bb) const;
  BOOL Injured_real_occ_phi_opnd( const EXP_OCCURS *def, 
				  BB_NODE *use_bb, INT opnd_num ) const;

  void Canonicalize_operands( CODEREP *def0, CODEREP *def1,
			      CODEREP **use0, CODEREP **use1 ) const;

  inline void         Create_new_version(EXP_OCCURS *);
  inline void         Reset_tos_downsafe(void);

  CODEREP            *Alloc_and_generate_injured_phi_res(const EXP_OCCURS *,
							 const EXP_OCCURS *,
							 MEM_POOL *) const;
  void                Process_delayed_rename(EXP_OCCURS *, CODEREP *) const;
  BOOL                Same_base_diff_offset(const CODEREP *,
					    const CODEREP *) const;
  BOOL                Same_base_same_offset(const CODEREP *,
					    const CODEREP *) const;
public:
                    ESSA(ETABLE *etable, EXP_WORKLST *worklist,
			 ALIAS_RULE *ar);
                   ~ESSA(void);
  void              Rename(void);

};


inline BOOL
ESSA::Lod_modified_real_occ_real_occ(const CODEREP *def, const CODEREP *use) const
{
  return (def != use);
}


// Returns TRUE if cr is modified anywhere between the
// beginning of bb (not counting the phis) to its use point.
BOOL
ESSA::Lod_modified_phi_result(const BB_NODE *bb, const CODEREP *cr) const
{
  if (cr->Is_flag_set(CF_IS_ZERO_VERSION))
    return TRUE;

  if (cr->Defbb() != bb) {
    // different BB, check dominance
    if (cr->Defbb()->Dominates(bb)) 
      return FALSE;
  } else {
    // same BB
    if (cr->Is_flag_set(CF_DEF_BY_PHI)) 
      return FALSE;
  }
  return TRUE;
}


// use_bb is the one associated with use exp_phi result.
BOOL
ESSA::Lod_modified_real_occ_phi_opnd(const CODEREP *cr, const BB_NODE *use_bb,
				     INT opnd_num) const
{
  const PHI_NODE *var_phi = _etable->Lookup_var_phi(use_bb, cr->Aux_id());
  if (var_phi == NULL) 
    return FALSE;
  if (!var_phi->Live())  // conservatively
    return TRUE;

  const CODEREP *newcr = var_phi->OPND(opnd_num);
  if (newcr->Is_flag_set(CF_IS_ZERO_VERSION))
    return TRUE;

  return (cr != newcr);
}


// Return TRUE if their base is the same, and the difference
// in their offset is bigger than ilod_size.
// Return FALSE otherwise and conservatively.
//
BOOL
ESSA::Same_base_diff_offset(const CODEREP *cr1, const CODEREP *cr2) const
{
  if (cr1->Kind() != CK_IVAR || cr2->Kind() != CK_IVAR)
    return FALSE;

  CODEREP *base1 = cr1->Ilod_base() ? cr1->Ilod_base() : cr1->Istr_base();
  CODEREP *base2 = cr2->Ilod_base() ? cr2->Ilod_base() : cr2->Istr_base();
  if (base1 == base2) {
    // make cr1 to be the one with smaller offset
    if (cr1->Offset() > cr2->Offset()) {
      const CODEREP *tcr = cr1;
      cr1 = cr2;
      cr2 = tcr;
    }
    INT32 size1 = (MTYPE_size_min(OPCODE_rtype(cr1->Op())) >> 3);
    if (cr1->Offset() + size1 > cr1->Offset()) { // overflow detection
      if (cr1->Offset() + size1 <= cr2->Offset())
	return TRUE;
    }
  }
  return FALSE;
}


// Return TRUE if their base and offset are the same
BOOL
ESSA::Same_base_same_offset(const CODEREP *cr1, const CODEREP *cr2) const
{
  if (cr1->Kind() != CK_IVAR || cr2->Kind() != CK_IVAR)
    return FALSE;

  CODEREP *base1 = cr1->Ilod_base() ? cr1->Ilod_base() : cr1->Istr_base();
  CODEREP *base2 = cr2->Ilod_base() ? cr2->Ilod_base() : cr2->Istr_base();
  if (base1 == base2) {
    if (cr1->Offset() ==  cr2->Offset()) 
      return TRUE;
  }
  return FALSE;
}


BOOL
ESSA::Ilod_modified_phi_result(const BB_NODE *phi_bb, const CODEREP *cr) const
{
  const CODEREP *cr_vsym = cr->Ivar_mu_node()->OPND();
  if (cr_vsym->Is_flag_set(CF_IS_ZERO_VERSION))
    return TRUE;

  for (const CODEREP *vsym = cr_vsym;
       vsym != NULL;
       vsym = vsym->Defchi()->OPND()) {
    
    if (vsym->Is_flag_set(CF_IS_ZERO_VERSION))
      return TRUE;

    if (vsym->Defbb() != phi_bb && !phi_bb->Dominates(vsym->Defbb()))
      return FALSE;

    // Now we know the phi dominates the vsym's definition, since
    // either the phi and the vsym def are in the same bb, or the
    // phi_bb dominates the vsym's defbb.

    if (vsym->Is_flag_set(CF_DEF_BY_PHI))
      return FALSE;

    if (!vsym->Is_flag_set(CF_DEF_BY_CHI))
      return TRUE;

    STMTREP *sr = vsym->Defstmt();
    if (sr == NULL || !OPCODE_is_store(sr->Op())) return TRUE;

#ifdef KEY // bug 7814
#ifdef TARG_NVISA
// bug 7814 is for a fortran program that was too aggressive in optimizing.
// But for nvidia's bug 417551, this is a safe optimization that helps perf.
// The root problem seems to be a weakness of alias analysis, as taking
// the address of a temp is blocking the optimization.
// Until we find a non-working program or come up with a better workaround,
// put this check under a flag.
    if ( ! WOPT_Enable_Aggressive_Iload_CSE)
#endif
    if (vsym->Aux_id() == Opt_stab()->Default_vsym())
      return TRUE;
#endif

    //since the ansi rule is turned on by default, but now there're
    //still some unsafe things. we need to do a fix here
    //this's definite alias case, which we can do before apply the
    //the alias rule
    if(Same_base_same_offset(sr->Lhs(), cr))
      return TRUE;

    // Seems like the following can lead to use-before-def if the
    // defstmt's result is not aliased with the use (i.e., with
    // vsym). Why? Because we may say the phi result is the same
    // e-version as cr if the operations aren't aliased, and when that
    // happens we will set the phi's result occurrence to be cr. cr
    // has a mu for the version of the vsym that is the LHS of sr's
    // chi, and sr cannot dominate the phi!
    if (Rule()->Aliased_Memop(sr->Lhs()->Points_to(Opt_stab()),
			      cr->Points_to(Opt_stab()),
			      (sr->Lhs()->Kind() == CK_VAR) ?
			      sr->Lhs()->Lod_ty() : sr->Lhs()->Ilod_ty(),
			      cr->Ilod_ty(), TRUE) &&
	!Same_base_diff_offset(sr->Lhs(), cr))
      return TRUE;
    

    // Fix 459756.  Also see comments above.  Update the mu-node of the
    // CK_IVAR to point the chi of the aliased defstmt.  This fixed the
    // use-before-def assertion, but the best fix is *not* to check
    // Aliased_Memop() in ESSA, if the CODEREP in htable has already been
    // canonicalized.
    // cr->Ivar_mu_node()->Set_OPND(vsym->Defchi()->OPND());

    // Fix 461898.  The above fix is wrong if the new opnd is a zero version!
    if (vsym->Defchi()->OPND()->Is_flag_set(CF_IS_ZERO_VERSION))
      return TRUE;

    cr->Ivar_mu_node()->Set_OPND(vsym->Defchi()->OPND());
  }

  // if unable to follow the use-def chain, return TRUE.
  return TRUE;
}


BOOL
ESSA::Ilod_modified_real_occ_phi_opnd(const BB_NODE *def_bb, const CODEREP *cr,
				     const BB_NODE *use_bb, INT opnd_num) const
{
  const PHI_NODE *var_phi = _etable->Lookup_var_phi(use_bb,
						    cr->Ivar_occ()->Aux_id());
  if (var_phi == NULL) 
    return FALSE;
  if (!var_phi->Live())  // conservatively
    return TRUE;

  const CODEREP *cr_vsym = cr->Get_ivar_vsym();
  if (cr_vsym == NULL || cr_vsym->Is_flag_set(CF_IS_ZERO_VERSION))
    return TRUE;
  
  for (CODEREP *vsym = var_phi->OPND(opnd_num);
       vsym != NULL;
       vsym = vsym->Defchi()->OPND()) {

    if (vsym->Is_flag_set(CF_IS_ZERO_VERSION))
      return TRUE;

    if (vsym == cr_vsym) // guarantee that we don't follow the use-def and pass the real-occ.
      return FALSE;

    if (vsym->Defbb() != def_bb && !def_bb->Dominates(vsym->Defbb()))
      return FALSE;

    if (vsym->Is_flag_set(CF_DEF_BY_PHI))
      return FALSE;

    if (!vsym->Is_flag_set(CF_DEF_BY_CHI))
      return TRUE;

    if (vsym->Defstmt() == cr->Ivar_defstmt())
      return FALSE;

    STMTREP *sr = vsym->Defstmt();
    if (sr == NULL || !OPCODE_is_store(sr->Op())) return TRUE;

#ifdef KEY // bug 7814
    // NVISA:  this instance of the check so far seems to not cause a problem
    if (vsym->Aux_id() == Opt_stab()->Default_vsym())
      return TRUE;
#endif

    if (Rule()->Aliased_Memop(sr->Lhs()->Points_to(Opt_stab()),
			      cr->Points_to(Opt_stab()),
			      sr->Lhs()->Kind() == CK_VAR ? sr->Lhs()->Lod_ty() : sr->Lhs()->Ilod_ty(),
			      cr->Ilod_ty(), TRUE) &&
	!Same_base_diff_offset(sr->Lhs(), cr))
      return TRUE;
  }

  // if unable to follow the use-def chain, return TRUE.
  return TRUE;
}


BOOL
ESSA::Ilod_modified_real_occ_real_occ(const BB_NODE *def_bb,
				      const CODEREP *def_cr,
				      const CODEREP *use_cr) const
{
  CODEREP *def_vsym = def_cr->Get_ivar_vsym();

  if (def_vsym == NULL ||
      def_vsym->Is_flag_set(CF_IS_ZERO_VERSION))
    return TRUE;

  for (CODEREP *vsym = use_cr->Ivar_mu_node()->OPND(); 
       vsym != NULL;
       vsym = vsym->Defchi()->OPND()) {
    
    if (vsym->Is_flag_set(CF_IS_ZERO_VERSION))
      return TRUE;

    if (def_vsym == vsym)
      return FALSE;

    if (vsym->Defbb() != def_bb && !def_bb->Dominates(vsym->Defbb()))
      return FALSE;

    if (vsym->Is_flag_set(CF_DEF_BY_PHI))
      return FALSE;

    if (!vsym->Is_flag_set(CF_DEF_BY_CHI))
      return TRUE;

    STMTREP *sr = vsym->Defstmt();
    if (sr == NULL || !OPCODE_is_store(sr->Op())) return TRUE;

#ifdef KEY // bug 7814
    if (vsym->Aux_id() == Opt_stab()->Default_vsym())
      return TRUE;
#endif

    if (Rule()->Aliased_Memop(sr->Lhs()->Points_to(Opt_stab()),
			      use_cr->Points_to(Opt_stab()), 
			      sr->Lhs()->Kind() == CK_VAR ? sr->Lhs()->Lod_ty() : sr->Lhs()->Ilod_ty(),
			      use_cr->Ilod_ty(), TRUE) &&
	!Same_base_diff_offset(sr->Lhs(), use_cr))
      return TRUE;
  }

  // if unable to follow the use-def chain, return TRUE.
  return TRUE;
}


// See Rules for Assigning e-versions 
//    INPUT:  def is either real occurrence or phi result
//            use must be a real occurrence
//    OUTPUT: true if both occurrences should have same version
//
BOOL 
ESSA::Same_e_version_real_occ_real_occ(const EXP_OCCURS *def,
				       EXP_OCCURS *use) const
{
  Is_True(use->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR,
	  ("ESSA::Same_e_version_real_occ_rel_occ: use is not a real occur"));
  Is_True(def->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR,
	  ("ESSA::Same_e_version_real_occ_rel_occ: def is not a real occur"));

  //  Rule #1:  both are real occurrences
  if (def->Occurrence() == use->Occurrence()) {
    // propagate the injured flag
    if ( def->Injured_occ() )
      use->Set_injured_occ();
    return TRUE;
  }

  // Can do some extra analysis for CK_IVAR.
  // opt_htable.cxx should already hashed identical *p into same CR
  // in lieu of effects of optimizations.
  if (use->Occurrence()->Kind() == CK_IVAR) {
    CODEREP *def_base = def->Occurrence()->Ilod_base() ?
      def->Occurrence()->Ilod_base() :
      def->Occurrence()->Istr_base();
    CODEREP *use_base = use->Occurrence()->Ilod_base() ?
      use->Occurrence()->Ilod_base() :
      use->Occurrence()->Istr_base();
    if (!Lod_modified_real_occ_real_occ(def_base, use_base) &&
	!Ilod_modified_real_occ_real_occ(def->Bb(), def->Occurrence(),
					 use->Occurrence())) 
      {
	Is_Trace(Tracing(),
		 (TFile, "ESSA: identical iloads have different CR *:\n"));
	Is_Trace_cmd(Tracing(), def->Occurrence()->Print(4, TFile));
	Is_Trace(Tracing(), (TFile, " with Defstmt():\n"));
	if (def->Occurrence()->Ivar_defstmt() != NULL) {
	  Is_Trace_cmd(Tracing(),
		       def->Occurrence()->Ivar_defstmt()->Print(TFile));
	}
	else {
	  Is_Trace(Tracing(), (TFile, "<NULL>\n"));
	}
	Is_Trace(Tracing(), (TFile, " with mu Defstmt():\n"));
	if (def->Occurrence()->Ivar_mu_node() != NULL &&
	    def->Occurrence()->Ivar_mu_node()->OPND()->Defstmt() != NULL) {
	  Is_Trace_cmd(Tracing(),
		       def->Occurrence()->Ivar_mu_node()->OPND()->Defstmt()->Print(TFile));
	}
	else {
	  Is_Trace(Tracing(), (TFile, "<NULL> mu node or mu node Defstmt()\n"));
	}
	Is_Trace(Tracing(), (TFile, "-----------------------------\n"));
	Is_Trace_cmd(Tracing(), use->Occurrence()->Print(4, TFile));
	Is_Trace(Tracing(), (TFile, " with Defstmt():\n"));
	if (use->Occurrence()->Ivar_defstmt() != NULL) {
	  Is_Trace_cmd(Tracing(),
		       use->Occurrence()->Ivar_defstmt()->Print(TFile));
	}
	else {
	  Is_Trace(Tracing(), (TFile, "<NULL>\n"));
	}
	Is_Trace(Tracing(), (TFile, " with mu Defstmt():\n"));
	if (use->Occurrence()->Ivar_mu_node() != NULL &&
	    use->Occurrence()->Ivar_mu_node()->OPND()->Defstmt() != NULL) {
	  Is_Trace_cmd(Tracing(),
		       use->Occurrence()->Ivar_mu_node()->OPND()->Defstmt()->Print(TFile));
	}
	else {
	  Is_Trace(Tracing(), (TFile, "<NULL> mu node or mu node Defstmt()\n"));
	}
	Is_Trace(Tracing(), (TFile, "-----------------------------\n"));
	// This situation can arise because of unique (or
	// restricted?) pointers and copy-propagation. Unique
	// pointers and copy propagation can result in a situation
	// where the vsyms associated with two different ILOADs are
	// not said to alias. Usually this means an LNO problem, but
	// it is probably also possible for users to create this
	// situation in their source code.
	//
	// DevWarn("ESSA: found identical iloads that have different CR *.");
	return TRUE;
      } 
  } else if (use->Occurrence()->Kind() == CK_OP && 
	     (OPCODE_operator(use->Occurrence()->Op()) == OPR_INTRINSIC_OP
#ifdef KEY
	      || OPCODE_operator(use->Occurrence()->Op()) == OPR_PURE_CALL_OP
#endif
	     )) {
      
    for (INT32 i = 0; i < use->Occurrence()->Kid_count(); i++) {
      CODEREP *opnd = use->Occurrence()->Opnd(i);
      if (opnd->Ivar_mu_node() != NULL ||
	  (opnd->Ilod_base()->Kind() == CK_VAR &&
	   Lod_modified_real_occ_real_occ(def->Occurrence()->Opnd(i)->Ilod_base(),
					  use->Occurrence()->Opnd(i)->Ilod_base())))
	return FALSE;
    }
  }

  // Can we perform strength-reduction on this expression?
  if ( !Worklist()->Exclude_sr_cand() && 
       Injured_real_occ_real_occ( def, use ) ) {
    use->Set_injured_occ();
    Is_Trace(Tracing(),(TFile,"Injured occurrence\n"));
    return TRUE;
  }
  
  return FALSE;
}

//=====================================================================
// Determine if the use occurrence is injured
//=====================================================================

BOOL 
ESSA::Injured_real_occ_real_occ(const EXP_OCCURS *def, EXP_OCCURS *use) const
{
  const CODEREP *def_cr = def->Occurrence();
  const CODEREP *use_cr = use->Occurrence();

  if ( use_cr->Kind() == CK_OP &&
       Str_red()->Candidate_opc( use_cr->Op() ) )
  {
    CODEREP *def_opnd0 = def_cr->Kid_count() > 0 ? 
			       def_cr->Opnd(0) : NULL;
    CODEREP *def_opnd1 = def_cr->Kid_count() > 1 ? 
			       def_cr->Opnd(1) : NULL;
    CODEREP *use_opnd0 = use_cr->Kid_count() > 0 ? 
			       use_cr->Opnd(0) : NULL;
    CODEREP *use_opnd1 = use_cr->Kid_count() > 1 ? 
			       use_cr->Opnd(1) : NULL;

    // we may need to swap operands if one of the codereps wasn't
    // canonicalized
    Canonicalize_operands( def_opnd0, def_opnd1,
			  &use_opnd0,&use_opnd1 );

    if ( Str_red()->Candidate( use_cr,
			       def_opnd0, def_opnd1, def->Bb(),
			       use_opnd0, use_opnd1, use->Bb() ) )
    {
      return TRUE;
    }
  }

  return FALSE;
}

//=====================================================================
// See Rules for Assigning e-versions 
//=====================================================================
//
BOOL 
ESSA::Same_e_version_phi_result(const EXP_OCCURS *def, const CODEREP *cr,
				EXP_OCCURS *use) const
{
  Is_True(def->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR,
	  ("ESSA::Same_e_version_phi_result: def is not a phi occur"));

  //  Rule #2:  def is phi occurrence and use is real occurrence
  //  Rule #4:  def is phi occurrence and use is phi-opnd

  switch (cr->Kind()) {
  case CK_IVAR:
    {
      CODEREP *addr = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
      if (addr->Kind() == CK_VAR) {
	if (Lod_modified_phi_result(def->Bb(), addr))
	  return FALSE;
      }
    }
    if (Ilod_modified_phi_result(def->Bb(), cr))
      return FALSE;
    return TRUE;
  case CK_OP:
    {
      BOOL not_mod = TRUE;

      for (INT i = 0; i < cr->Kid_count(); i++) {
	CODEREP *kid = cr->Opnd(i);
	switch (kid->Kind()) {
	case CK_VAR:
	  if (Lod_modified_phi_result(def->Bb(), kid)) {
	    not_mod = FALSE;
	  }
	  break;
	case CK_IVAR:  // should be OPR_PARM
	  if (kid->Ivar_mu_node() != NULL ||
	      (kid->Ilod_base()->Kind() == CK_VAR &&
	       Lod_modified_phi_result(def->Bb(), kid->Ilod_base())))
	    not_mod = FALSE;
	  break;
	case CK_CONST:
	case CK_RCONST:
	case CK_LDA:
	  break;
	default:
	  Is_True(FALSE, ("unexpected CK_KIND."));
	}
      }

      if ( not_mod ) {
        // not modified, so all is fine
	return TRUE;
      }
      // modified, so see if simply injured
      else if ( !Worklist()->Exclude_sr_cand() && 
		Injured_phi_result_real_occ( def, cr, use->Bb())) 
      {
	use->Set_injured_occ();
	Is_Trace(Tracing(),
		 (TFile,"Same_e_version_phi_result: Injured occurrence\n"));
	return TRUE;
      }

      // modified, and not injured
      return FALSE;
    }
  case CK_VAR: // LPRE
    return (! Lod_modified_phi_result(def->Bb(), cr));
  default:
    Is_True(FALSE, ("unexpected CK_KIND."));
  }
  return FALSE;
}

//=====================================================================
// Determine if the use occurrence is injured
//=====================================================================

BOOL 
ESSA::Injured_phi_result_real_occ(const EXP_OCCURS *def, const CODEREP *use_cr,
				  BB_NODE *use_bb) const
{
  if ( use_cr->Kind() == CK_OP &&
       Str_red()->Candidate_opc( use_cr->Op() ) )
  {
    CODEREP *use_opnd0 = use_cr->Kid_count() > 0 ? 
			       use_cr->Opnd(0) : NULL;
    CODEREP *use_opnd1 = use_cr->Kid_count() > 1 ? 
			       use_cr->Opnd(1) : NULL;

    if ( Str_red()->Candidate_phi_res( use_cr,
				     def->Bb(),
				     use_opnd0, use_opnd1, use_bb) )
    {
      return TRUE;
    }
  }

  return FALSE;
}


//=====================================================================
// See Rule #3.
//    INPUT:  eexpr is the expression corresponding to the e-number
//            def is either real occurrence or phi result
//            use is the phi node at bb, with opnd_num indicating which opnd.
//    OUTPUT: true if both occurrences should have same version
//	      Set *injured=TRUE if injured occurrence.
//=====================================================================

BOOL 
ESSA::Same_e_version_real_occ_phi_opnd(const EXP_OCCURS *def, 
				       BB_NODE *use_bb,
				       INT opnd_num, 
				       const CODEREP *use_cr,
				       BOOL *injured) const
{
  Is_True(def->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR, ("not real occur."));
  *injured = FALSE;

  const CODEREP *cr = def->Occurrence();
  switch (cr->Kind()) {
  case CK_IVAR:
    {
      CODEREP *def_addr = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
      CODEREP *use_addr = use_cr->Ilod_base();
      if (def_addr->Kind() == CK_VAR) {
	if (Lod_modified_real_occ_real_occ(def_addr, use_addr))
	  return FALSE;
      }
    }
    if (Ilod_modified_real_occ_real_occ(def->Bb(), cr, use_cr))
      return FALSE;
    return TRUE;
  case CK_OP:
    {
      BOOL not_mod = TRUE;
      for (INT i = 0; i < cr->Kid_count(); i++) {
	CODEREP *kid = cr->Opnd(i);
	switch (kid->Kind()) {
	case CK_VAR:
	  if (Lod_modified_real_occ_real_occ(kid, use_cr->Opnd(i))) {
	    not_mod = FALSE;
	  }
	  break;
	case CK_IVAR:  // should be OPR_PARM
	  if (kid->Ivar_mu_node() != NULL ||
	      (kid->Ilod_base()->Kind() == CK_VAR && 
	       Lod_modified_real_occ_real_occ(kid->Ilod_base(), use_cr->Opnd(i)->Ilod_base())))
	    not_mod = FALSE;
	  break;
	case CK_CONST:
	case CK_RCONST:
	case CK_LDA:
	  break;
	default:
	  Is_True(FALSE, ("unexpected CK_KIND."));
	}
      }

      if ( not_mod ) {
	// all is well
	return TRUE;
      }
      // modified, so see if simply injured
      else if ( !Worklist()->Exclude_sr_cand() && 
		Injured_real_occ_phi_opnd( def, use_bb, opnd_num ) )
      {
	*injured = TRUE;
	Is_Trace(Tracing(),
	     (TFile,"Same_e_version_real_occ_phi_opnd: Injured occurrence\n"));
	return TRUE;
      }

      // modified, and not injured
      return FALSE;
    }
  case CK_VAR: // LPRE
    return (! Lod_modified_real_occ_real_occ(cr, use_cr));
  default:
    Is_True(FALSE, ("unexpected CK_KIND."));
  }
  return FALSE;
}

//=====================================================================
// Determine if the phi_opnd is injured
//=====================================================================

BOOL 
ESSA::Injured_real_occ_phi_opnd( const EXP_OCCURS *def, 
				 BB_NODE *use_bb, INT opnd_num ) const
{
  const CODEREP *def_cr = def->Occurrence();

  if ( def_cr->Kind() == CK_OP &&
       Str_red()->Candidate_opc( def_cr->Op() ) )
  {
    CODEREP *opnd = Etable()->
      Alloc_and_generate_cur_expr(def->Occurrence(), 
				  use_bb, opnd_num, Etable()->Per_expr_pool(),
				  FALSE /* do not convert zero ver */);

    CODEREP *def_opnd0 = def_cr->Kid_count() > 0 ? 
			       def_cr->Opnd(0) : NULL;
    CODEREP *def_opnd1 = def_cr->Kid_count() > 1 ? 
			       def_cr->Opnd(1) : NULL;
    CODEREP *use_opnd0 = opnd->Kid_count() > 0 ? 
			       opnd->Opnd(0) : NULL;
    CODEREP *use_opnd1 = opnd->Kid_count() > 1 ? 
			       opnd->Opnd(1) : NULL;

    // we may need to swap operands if one of the codereps wasn't
    // canonicalized
    Canonicalize_operands( def_opnd0, def_opnd1,
			  &use_opnd0,&use_opnd1 );

    if ( Str_red()->Candidate( def_cr,
			       def_opnd0, def_opnd1, def->Bb(),
			       use_opnd0, use_opnd1, use_bb->Nth_pred(opnd_num) ) )
    {
      return TRUE;
    }
  }

  return FALSE;
}

//=====================================================================
// May need to get operands in right order for checking strength-
// reduction candidates
//=====================================================================

void 
ESSA::Canonicalize_operands( CODEREP *def0, CODEREP *def1,
			     CODEREP **use0, CODEREP **use1 ) const
{
  CODEREP *tuse0 = *use0;

  // see if the first kids don't match
  if ( def0->Kind() != tuse0->Kind() ||
       (def0->Kind() == CK_VAR && def0->Aux_id() != tuse0->Aux_id()) )
  {
    // swap the kids
    *use0 = *use1;
    *use1 = tuse0;
  }
  else {
#ifdef Is_True_On
    CODEREP *tuse1 = *use1;

    // verify that the second kids match also
    if( tuse1 != NULL ) {
      Is_True( def1 != NULL,
	("ESSA::Canonicalize_operands: non-null kids don't match") );

      Is_True( def1->Kind() == tuse1->Kind(),
	("ESSA::Canonicalize_operands: kids kind don't match") );

      if ( def1->Kind() == CK_VAR ) {
	Is_True( def1->Aux_id() == tuse1->Aux_id(),
	  ("ESSA::Canonicalize_operands: kids aux_id's don't match") );
      }
    }
    else {
      Is_True( def1 == NULL,
	("ESSA::Canonicalize_operands: null kids don't match") );
    }
#endif // Is_True_On
  }
}

//=====================================================================
//  Create a new e-version and push it onto the stack.
//=====================================================================

inline void
ESSA::Create_new_version(EXP_OCCURS *occur)
{
  occur->Set_e_version(Worklist()->Cur_e_version());
  Worklist()->New_e_version();
  Stack()->Push(occur);
}


// If the TOS is a phi-occur, reset its downsafe bit.
//
inline void
ESSA::Reset_tos_downsafe(void)
{
  // Reset down-safe if the phi result does have any real use.
  // (if there is any real use, the stack top should be a real occur.)
  if (!Stack()->Is_Empty() &&
      Stack()->Top() != NULL &&
      Stack()->Top()->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) 
    Stack()->Top()->Exp_phi()->Set_not_down_safe();
}


//======================================================================
// Strength-reduction has injured the result of this phi.  Need to
// create a valid coderep that represents this phi result rather than
// using the use's occurrence which has wrong versions.
//======================================================================

CODEREP *
ESSA::Alloc_and_generate_injured_phi_res( const EXP_OCCURS *def, 
			  const EXP_OCCURS *use, MEM_POOL *mpool ) const
{
  Is_True( def->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR,
    ("ESSA::Alloc_and_generate_injured_phi_res: not phi def") );

  // allocate and copy from def.
  CODEREP *oldcr = use->Occurrence();
  CODEREP *newcr = CXX_NEW_VARIANT(CODEREP(*oldcr),
				   oldcr->Extra_space_used(), mpool);

  switch ( newcr->Kind() ) {
    case CK_OP:
      {
	for (INT i = 0; i < newcr->Kid_count(); i++) {
	  CODEREP *kid = newcr->Opnd(i);
	  switch (kid->Kind()) {
	  case CK_VAR:
	    {
	      const PHI_NODE *var_phi = 
		Etable()->Lookup_var_phi(def->Bb(), kid->Aux_id());

	      if (var_phi != NULL) {
	        // we have a phi, so its result is the correct version
		Is_True(var_phi->Live(), 
			("ESSA::Alloc_and_generate_injured_phi_res: "
			 "encounter dead phi node bb:%d Aux:%d.",
			 def->Bb()->Id(), kid->Aux_id()));
		newcr->Set_opnd(i, var_phi->RESULT());
	      }
	      else {
		// no phi, so we need to figure out based upon the
		// injuring statement what the valid version is
		CODEREP *iv_def, *iv_use, *dummy_mult;
		Str_red()->Find_iv_and_mult( def, &iv_def, use,
					    &iv_use, &dummy_mult );
		// need to make sure the iv it found matches which one
		// we're interested in
		if ( kid->Aux_id() == iv_def->Aux_id() ) {
		  newcr->Set_opnd( i, iv_def );
		}
	      }
	    }
	    break;

	  case CK_CONST:
	  case CK_RCONST:
	  case CK_LDA:
	    break;

	  case CK_IVAR:
	  default:
	    Is_True(FALSE, ("unexpected CK_KIND."));
	  }
	}
      }
      break;
    
    default:
      FmtAssert( FALSE,
	("ESSA::Alloc_and_generate_injured_phi_res: unexpected") );
      return NULL;
  }

  // Trace new expr generated
  if (Tracing()) {
    fprintf(TFile,
	    "ESSA::Alloc_and_generate_injured_phi_res for phi res in BB%d\n",
	    def->Bb()->Id());
    newcr->Print(10, TFile);
  }

  return newcr;
}

//======================================================================
//======================================================================

void
ESSA::Process_delayed_rename(EXP_OCCURS *use, CODEREP *real_occ) const
{
  Is_True(use->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR, 
    ("ESSA::Process_delayed_rename: not a phi occur."));
  EXP_PHI *exp_phi = use->Exp_phi();

  BOOL is_const_lpre = inCODEKIND(Cur_e_expr()->Kind(), CK_LDA|CK_RCONST|CK_CONST);

  for (INT opnd_num = 0; opnd_num < exp_phi->Opnd_count(); opnd_num++) {
    if (exp_phi->Delayed_rename(opnd_num)) {
      exp_phi->Reset_delayed_rename(opnd_num);
      EXP_OCCURS *def = exp_phi->Opnd(opnd_num);

      CODEREP *newcr = Etable()->
	Alloc_and_generate_cur_expr(real_occ, use->Bb(),
				    opnd_num, Etable()->Per_expr_pool(),
				    FALSE /* do not convert zero ver */);

      switch (def->Occ_kind()) {
      case EXP_OCCURS::OCC_PHI_OCCUR:
	{
	  // come up with a use that looks valid
	  EXP_OCCURS *tmp_use = exp_phi->Pred(opnd_num);
	  Is_True(tmp_use->Occ_kind() == EXP_OCCURS::OCC_PHI_PRED_OCCUR,
	    ("ESSA::Process_delayed_rename: Pred not phi-pred occur."));
	  tmp_use->Set_occurrence( newcr );

	  if (is_const_lpre ||
	      Same_e_version_phi_result(def, newcr, tmp_use) ) {

	    // transfer the injured flag to the phi 
	    if (tmp_use->Injured_occ()) {
	      tmp_use->Reset_injured_occ();
	      exp_phi->Set_injured(opnd_num);
	    }

	    if (def->Occurrence() == NULL) {

	      // propagate the current version to the phi result
	      if ( exp_phi->Injured(opnd_num) ) {
		// injured phi operand, so need to come up with valid
		// phi result with correct versions
		newcr = Alloc_and_generate_injured_phi_res( def, 
		  exp_phi->Pred(opnd_num), Etable()->Per_expr_pool());
	      }
	      def->Set_occurrence(newcr);
	      Process_delayed_rename(def, newcr);
	    }
#ifdef Is_True_On
	    Worklist()->Inc_dense_ssa_count();
#endif
	  } else {
	    // Notice that if a (phi_result, phi_opnd) is delayed, there
	    // must be no real occ between them, so it is ok to set 
	    // not down safe.
	    def->Exp_phi()->Set_not_down_safe();
	    exp_phi->Set_opnd(opnd_num, NULL);
	  }
	}
	break;
      case EXP_OCCURS::OCC_REAL_OCCUR:
	{
	  BOOL injured = FALSE;
	  if ( is_const_lpre ||
	      Same_e_version_real_occ_phi_opnd
	      (def, use->Bb(), opnd_num, newcr, &injured) )
	  {
	    EXP_OCCURS *tmp_use = exp_phi->Pred(opnd_num);
	    exp_phi->Set_has_real_occ(opnd_num);
	    if (injured || def->Injured_occ())
	      exp_phi->Set_injured(opnd_num);
	    if ((injured || def->Injured_occ()) && 
		! def->Occurrence()->Match(newcr)) {
	      tmp_use->Set_occurrence(newcr); 	      // update Phi_pred occurrence
	    } else {
	      tmp_use->Set_occurrence(def->Occurrence());        // update Phi_pred occurrence
	    }
	    // this real occ is defined by a phi
	    if (def->Def_occur() != NULL)
	      exp_phi->Set_opnd(opnd_num, def->Def_occur());
	    else
	      exp_phi->Set_opnd(opnd_num, def);
#ifdef Is_True_On
	    Worklist()->Inc_dense_ssa_count();
#endif
	  } else {
	    exp_phi->Set_opnd(opnd_num, NULL);  
	  }
	}
	break;
      default:
	Is_True(FALSE, ("unexpected occ_kind."));
      }
    }
  }
}


//  Pass1  Links up phi opnds to its potential def and
//      renames the phi results and real occur
//  Pass2  renames the phi opnd and phi result links.
//
void
ESSA::Rename(void)
{
  // Verify that the worklist Cur_e_expr() is in an acceptable form.
#ifdef Is_True_On
  const CODEREP *cr = Cur_e_expr();
  switch (cr->Kind()) {
  case CK_VAR:
    break;
  case CK_IVAR:
    {
      Is_True(OPCODE_operator(cr->Op()) != OPR_PARM,("opr_parm not allowed."));
      const CODEREP *addr = cr->Ilod_base() ? cr->Ilod_base():cr->Istr_base();
      Is_True(addr->Kind() == CK_VAR ||
	      addr->Kind() == CK_CONST ||
	      addr->Kind() == CK_RCONST ||
	      addr->Kind() == CK_LDA, ("expr form not acceptable."));
    }
    break;
  case CK_OP:
    {
      for (INT i = 0; i < cr->Kid_count(); i++) {
	CODEREP *kid = cr->Opnd(i);
	Is_True((kid->Kind() == CK_IVAR &&
		 OPCODE_operator(kid->Op()) == OPR_PARM) ||
		kid->Kind() == CK_VAR ||
		kid->Kind() == CK_CONST ||
		kid->Kind() == CK_RCONST ||
		kid->Kind() == CK_LDA, ("expr form not acceptable."));
	if (kid->Kind() == CK_IVAR && OPCODE_operator(kid->Op()) == OPR_PARM)
	    Is_True(kid->Ilod_base()->Kind() == CK_VAR ||
		    kid->Ilod_base()->Kind() == CK_CONST ||
		    kid->Ilod_base()->Kind() == CK_RCONST ||
		    kid->Ilod_base()->Kind() == CK_LDA,
		    ("expr form not acceptable."));
      }
    }
    break;
  case CK_LDA:
  case CK_RCONST:
  case CK_CONST:
    Is_True(Etable()->Pre_kind() != PK_EPRE, ("const is not acceptable in EPRE"));
    break;
  default:
    Is_True(FALSE, ("expr form not acceptable."));
  }
#endif

  BOOL is_const_lpre = inCODEKIND(Cur_e_expr()->Kind(), CK_LDA|CK_RCONST|CK_CONST);

  // Set initial e-version to 1.
  Worklist()->Init_e_version();

  // Empty the stack
  Stack()->Clear();

  // Iterate over the occurences list in dominator preorder
  EXP_ALL_OCCURS_ITER exp_occ_iter(Worklist()->Real_occurs().Head(),
				   _etable->Lftr() ? 
				   _etable->Lftr()->Exp_hash(Worklist()) :
				   NULL,
				   Worklist()->Phi_occurs().Head(),
				   Worklist()->Phi_pred_occurs().Head(),
				   _etable->Exit_occurs().Head());
  EXP_OCCURS *occur;
  FOR_ALL_NODE(occur, exp_occ_iter, Init()) {
    // unwind the stack until all EXP_OCCUR on the stack dominates
    // the current EXP_OCCUR
    while (!Stack()->Is_Empty() &&
	   !Stack()->Top()->Bb()->Dominates(occur->Bb())) 
      Stack()->Pop();

    if (Stack()->Is_Empty() || Stack()->Top() == NULL) {

      // TOS is nil
      switch (occur->Occ_kind()) {
      case EXP_OCCURS::OCC_PHI_OCCUR:        // create new ver
	Is_True(occur->Exp_phi()->Result() == occur,
		("OCC_PHI_OCCUR phi result is wrong."));
#ifdef Is_True_On
	Worklist()->Inc_phi_count();
	Worklist()->Inc_optimistic_ssa_count(occur->Bb()->Pred()->Len());
#endif
	Create_new_version(occur);           
	break;
      case EXP_OCCURS::OCC_REAL_OCCUR:     
#ifdef Is_True_On
	Worklist()->Inc_realocc_count();
	if (occur->Mult_real())
	  Worklist()->Inc_realocc_count();
#endif
	Create_new_version(occur);           
	break;
      case EXP_OCCURS::OCC_PHI_PRED_OCCUR:   // do nothing
      case EXP_OCCURS::OCC_EXIT_OCCUR:       // do nothing
	break;
      case EXP_OCCURS::OCC_COMP_OCCUR:	     // reset comp version (shared)
	occur->Set_e_version(EXP_OCCURS::ILLEGAL_E_VERSION);
	break;
      default:
	Is_True(FALSE, ("ESSA::Rename, unknown occurrence kind: %d",
			occur->Occ_kind()));
	break;
      }

    } else {      // TOS is non-nil

      switch (occur->Occ_kind()) {
      case EXP_OCCURS::OCC_REAL_OCCUR:
	{
	  // UGLINESS: The following doesn't really have anything to
	  // do with SSAPRE renaming. Rename was just a convenient
	  // place to put this initialization of the LOOP_HAS_REAL_OCC
	  // flag. In principle, any pass before the CodeMotion step
	  // would work.
	  //
	  // If the current expression is a candidate for strength
	  // reduction in the presence of LFTR, we note that the
	  // innermost loop containing *occur contains a real
	  // occurrence. Comparisons whose innermost containing loop
	  // does not contain a real occurrence whose innermost
	  // containing loop is the same as the one for the comparison
	  // will not be eligible for LFTR. This condition is
	  // established to guarantee termination of
	  // LFTR/SR/OCOPY/2OE. See the comments for
	  // LFTR::Replace_comparison (in opt_lftr2.cxx) for more
	  // details.
	  if (WOPT_Enable_New_SR &&
	      Etable()->Lftr()->Lftr_on() &&
	      Str_red()->Candidate_opc(occur->Occurrence()->Op())) {
	    BB_LOOP *loop = occur->Bb()->Innermost();
	    if (loop != NULL) {
	      Is_Trace(Etable()->Tracing(), (TFile, "Real occ: "));
	      Is_Trace_cmd(Etable()->Tracing(), occur->Print(TFile));
	      Is_Trace(Etable()->Tracing(), (TFile, " in loop headed "
					     "at BB%d\n",
					     loop->Header()->Id()));
	      loop->Set_flag(LOOP_HAS_REAL_OCC);
	    }
	  }
	  // End of UGLINESS. Back to your regularly-scheduled
	  // renaming.

#ifdef Is_True_On
	  Worklist()->Inc_realocc_count();
	  if (occur->Mult_real())
	    Worklist()->Inc_realocc_count();
#endif

	  EXP_OCCURS *tos = Stack()->Top();
	  switch (tos->Occ_kind()) {
	  case EXP_OCCURS::OCC_REAL_OCCUR:
	    if ( is_const_lpre || 
		( !occur->Occurs_as_lvalue() &&
		  Same_e_version_real_occ_real_occ(tos, occur) ) ) {
	      occur->Set_e_version(tos->E_version());
	      occur->Set_def_occur(tos->Def_occur() != NULL ? tos->Def_occur() : tos);
#ifdef Is_True_On
	      Worklist()->Inc_ssa_edge_count();
#endif
	    } else
	      Create_new_version(occur);
	    break;
	  case EXP_OCCURS::OCC_PHI_OCCUR:
	    if ( is_const_lpre ||
		( !occur->Occurs_as_lvalue() &&
		 Same_e_version_phi_result(tos, occur->Occurrence(), occur) ) )
	    {
	      occur->Set_e_version(tos->E_version());
	      occur->Set_def_occur(tos);

	      if ( !occur->Injured_occ() ) {
		tos->Set_occurrence(occur->Occurrence());
	      }
	      else {
		// the use is injured, so we need to come up with a
		// valid cr for this phi result.
		CODEREP *new_phi_res = Alloc_and_generate_injured_phi_res(
		  tos, occur, Etable()->Per_expr_pool());
		tos->Set_occurrence( new_phi_res );
	      }
	      Stack()->Push(occur);   // push real occ to compute downsafety
#ifdef Is_True_On
	      Worklist()->Inc_ssa_edge_count();
#endif
	    } else {
	      tos->Exp_phi()->Set_not_down_safe();
	      Create_new_version(occur);
	    }
	    break;
	  default:
	    Is_True(FALSE, ("unexpected EXP_OCCURS kind."));
	  }
	}
	break;

      case EXP_OCCURS::OCC_PHI_OCCUR:
	Is_True(occur->Exp_phi()->Result() == occur,
		("OCC_PHI_OCCUR phi result is wrong."));
#ifdef Is_True_On
	Worklist()->Inc_phi_count();
	Worklist()->Inc_optimistic_ssa_count(occur->Bb()->Pred()->Len());
#endif
	Create_new_version(occur);
	break;

      case EXP_OCCURS::OCC_PHI_PRED_OCCUR:
	{
	  BB_LIST_ITER bb_iter;
	  BOOL updated = FALSE;  // for verification only
	  EXP_OCCURS *tos = Stack()->Top();
	  BB_NODE *use_bb;                    // BB of the EXP_PHI
	  BB_NODE *pred_bb = occur->Bb();     // BB of the PHI_PRED node

	  // A PHI_PRED to PHI mapping is a 1-many mapping!
	  // So find all EXP_PHI that needs to be updated.
	  //
	  //  Notice that tos might be a real occurrence, and the real occurrence
	  //  might be a use and points to another "def".
	  //
	  //  DO NOT CHANGE tos to the "def".  This is required for downsafety correctness.
	  //

	  FOR_ALL_ELEM (use_bb, bb_iter, Init(pred_bb->Succ())) {
	    EXP_PHI *exp_phi = _etable->Lookup_exp_phi(use_bb, Cur_e_expr());
	    if (exp_phi != NULL) {
	      INT opnd_num = use_bb->Pred()->Pos(pred_bb);
	      updated = TRUE;
	      exp_phi->Set_opnd(opnd_num, (EXP_OCCURS *) tos);
	      exp_phi->Set_delayed_rename(opnd_num);
#ifdef Is_True_On
	      Worklist()->Inc_ssa_edge_count();
#endif
	    }
	  }
	  Is_True(updated,
		  ("OCC_PHI_OPND_OCC has no matching phi node for PREDBB%d.",
		   pred_bb->Id()));
	}
	break;
      
      case EXP_OCCURS::OCC_EXIT_OCCUR:
	Is_True(occur->Bb()->Kind() == BB_EXIT, ("not an exit BB."));
	Reset_tos_downsafe();
	break;

      case EXP_OCCURS::OCC_COMP_OCCUR:
	occur->Set_e_version(Stack()->Top()->E_version());
	break;

      default:
	Is_True(FALSE, ("ESSA::Rename, unknown occurrence kind: %d",
			occur->Occ_kind()));
	break;
      }
    }
  }

  // TODO: need to rewrite the following with a real iterator
  //
  EXP_OCCURS_ITER    phi_occur_iter;

  FOR_ALL_NODE (occur, phi_occur_iter, Init(Worklist()->Phi_occurs().Head())) {
    if (occur->Occurrence() != NULL)
      Process_delayed_rename(occur, occur->Occurrence());
  }
  
  FOR_ALL_NODE (occur, phi_occur_iter, Init(Worklist()->Phi_occurs().Head())) {
    EXP_PHI *exp_phi = occur->Exp_phi();
    for (INT i = 0; i < exp_phi->Opnd_count(); i++) {
      if (exp_phi->Delayed_rename(i)) {
	EXP_OCCURS *def = exp_phi->Opnd(i);
	// Notice that if a (phi_result, phi_opnd) is delayed, there must
	// be no real occ between them, so it is ok to set not down safe.
	if (def && def->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) 
	  def->Exp_phi()->Set_not_down_safe();
	exp_phi->Set_opnd(i, NULL);
      }
    }
  }
}

ESSA::ESSA(ETABLE *etable, EXP_WORKLST *worklist, ALIAS_RULE *ar)
{
  OPT_POOL_Initialize(&_mpool, "pre rename mempool", FALSE, -1);
  OPT_POOL_Push(&_mpool, -1);

  _etable = etable;
  _str_red = etable->Str_red();
  _opt_stab = etable->Opt_stab();
  _worklist = worklist;
  _rule = ar;
  _cur_e_expr = worklist->Real_occurs().Head()->Occurrence();
  _stack = CXX_NEW(STACK<EXP_OCCURS *>(&_mpool), &_mpool);
}

ESSA::~ESSA(void)
{
  OPT_POOL_Pop(&_mpool, -1);
  OPT_POOL_Delete(&_mpool, -1);
}


// ======================================================================
// Expr SSA rename
// ======================================================================
void
EXP_WORKLST::Rename_expression(ETABLE *etable)
{
  ESSA essa(etable, this, etable->Arule());
  essa.Rename();

  Is_Trace(etable->Tracing(),(TFile,
      "====== After ETABLE::Rename_expression ======\n"));
  Is_Trace_cmd(etable->Tracing(),Print(TFile));
}
