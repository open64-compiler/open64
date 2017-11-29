/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_find.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_find.cxx,v $
//
// Revision history:
//  08-APR-95 dahl - Original Version
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
//   Search statements upward from end of BB to beginning looking for a
// given CODEREP. Can search both LHS and RHS expressions (Find_cr) or 
// just LHS (Find_def). Note that Find_cr() and Find_def() are almost
// identical, they are put here in the same file for maintenance reasons.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_find_CXX	"opt_find.cxx"
static char *rcs_id = 	opt_find_CXX"$Revision: 1.7 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "erglob.h"
#include "tracing.h"

#include "opt_base.h"
#include "opt_util.h"
#include "opt_main.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_sym.h"
#include "opt_htable.h"
#include "opt_mu_chi.h"
#include "opt_ssa.h"
#include "bb_node_set.h"



// returns TRUE if the two CODEREPs have same bit position
// for CK_VAR need to make sure comparison is Vx to Vy (bitpos type)
// for !CK_VAR comparison is Ex to Ey (bitpos type)
BOOL
CODEREP::Compare_bitpos(const CODEREP *cr) const
{
  Is_True(this->Bitpos() != ILLEGAL_BP, ("this has illegal BP."));
  Is_True(cr->Bitpos() != ILLEGAL_BP, ("this has illegal BP."));

  if (Kind() != cr->Kind()) return FALSE;
  if (Kind() == CK_VAR) {
    if (cr->Kind() == CK_VAR && Bitpos() == cr->Bitpos())	// Vx <--> Vy
      return TRUE;
  } else if (Bitpos() == cr->Bitpos())				// Ex <--> Ey
      return TRUE;
  return FALSE;
}


//  Handle VAR or IVAR coderep
//  Return TRUE if the coderep should have the same bitpos.
//  This routine does not require that the CR has a bitpos assigned.
//
BOOL
CODEREP::Same_bitpos(const CODEREP *cr) const
{
  if (Kind() != cr->Kind()) return FALSE;

  if (Bitpos() != ILLEGAL_BP && cr->Bitpos() != ILLEGAL_BP)
    return Bitpos() == cr->Bitpos();
  else {
    switch (Kind()) {
    case CK_VAR:
      if (Aux_id() == cr->Aux_id() && Version()==cr->Version())
	return TRUE;
      return FALSE;
    case CK_IVAR:
      {
	CODEREP *gcr = Ilod_base();
	CODEREP *lcr = cr->Ilod_base();
	if (gcr == NULL)
	  gcr = Istr_base();
	if (lcr == NULL) 
	  lcr = cr->Istr_base();
	if (!lcr->Same_bitpos(gcr))
	  return FALSE;
	if (Opr() != OPR_ILOADX) {
	  if (Offset() != cr->Offset())
	    return FALSE;
	}
	else {
	  gcr = Index();
	  lcr = cr->Index();
	  if (!lcr->Same_bitpos(gcr))
	    return FALSE;
	}
	if (Get_mtype_class(Dtyp()) != Get_mtype_class(cr->Dtyp()))
	  return FALSE;	// type class not the same
	if (MTYPE_size_min(Dsctyp()) != MTYPE_size_min(cr->Dsctyp()))
	  return FALSE;	// size not the same

	if (OPCODE_operator(Op()) == OPR_MLOAD) {
	  gcr = Mload_size();
	  if (gcr == NULL)
	    gcr = Mstore_size();
	  lcr = cr->Mload_size();
	  if (lcr == NULL)
	    lcr = cr->Mstore_size();
	  if (!lcr->Same_bitpos(gcr))
	    return FALSE;
	}
      }
      return TRUE;
    case CK_OP:
      {
	if (Op() != cr->Op())
	  return FALSE;
	if (Kid_count() != cr->Kid_count())	// arrays can be different
	  return FALSE;
       
	// check additional fields for some specific opcodes
	if (OPCODE_operator(Op()) == OPR_INTRINSIC_OP &&
	    Intrinsic() != cr->Intrinsic())
	  return FALSE;
#ifdef KEY
	else if (OPCODE_operator(Op()) == OPR_PURE_CALL_OP &&
	         Call_op_aux_id() != cr->Call_op_aux_id())
	  return FALSE;
#endif
	else if (OPCODE_operator(Op()) == OPR_CVTL && Offset() != cr->Offset())
	  return FALSE;

	for (IDX_32 i=0; i< Kid_count(); i++) {
	  if (!Opnd(i)->Same_bitpos(cr->Opnd(i)))
	    return FALSE;
	}
      }
      return TRUE; 
      
    case CK_CONST:
    case CK_RCONST:
    case CK_LDA:
      return this == cr;
    default:
      Is_True(FALSE, ("CR kind not handled."));
      break;
    }
  }
  return FALSE;
}


//----------------------------------------------------------------------------
// search both LHS and RHS for cr
// searches only the given BB
//
CODEREP *
BB_NODE::Find_cr(const CODEREP *cr)
{
  CODEREP *retcr, *lhs;
  STMTREP *stmt;
  STMTREP_ITER stmt_iter(Stmtlist());

  FOR_ALL_NODE_REVERSE(stmt,stmt_iter,Init()) {

    // check RHS first
    if (stmt->Rhs()) {
      retcr = stmt->Rhs()->Find_cr(cr);
      if (retcr)
	return retcr;
    }

    // check LHS for DEF of this CR
    lhs = stmt->Lhs();
    if (lhs) {
      switch (stmt->Opr()) {
        case OPR_STID:
        case OPR_STBITS:
	  if (cr->Compare_bitpos(lhs))
	    return lhs;
          break;
	case OPR_ISTORE:
        case OPR_ISTBITS:
	  retcr = lhs->Istr_base()->Find_cr(cr);
	  if (retcr)
	    return retcr;
	  break;
	case OPR_MSTORE:
	  retcr = lhs->Istr_base()->Find_cr(cr);
	  if (retcr)
	    return retcr;
	  retcr = lhs->Mstore_size()->Find_cr(cr);
	  if (retcr)
	    return retcr;
	  break;
	default:
	  break;
      }
    }
    
    // check CHI node
    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      FOR_ALL_NODE(cnode,chi_iter,Init(stmt->Chi_list())) {
	if (cnode->Live()) {
	  retcr = cnode->RESULT();
	  if (retcr && cr->Same_bitpos(retcr))
	    return retcr;
	}
      }
    }
  } // FOR_ALL_NODE_REVERSE

  // check PHI node
  PHI_LIST_ITER phi_iter;
  PHI_NODE *pnode;
  FOR_ALL_ELEM(pnode,phi_iter,Init(Phi_list())) {
    if (!pnode->Dce_dead()) {
      if ((retcr = pnode->RESULT()) != NULL)
	if (retcr->Bitpos() != ILLEGAL_BP) 
	  if (cr->Same_bitpos(retcr))
	    return retcr;
    }
  }
  
  return NULL;	  // not found
}

//----------------------------------------------------------------------------
// Find the coderep with the same var bitpos as 'cr'.
// This routine is recursive.
//
CODEREP*
CODEREP::Find_cr(const CODEREP *cr)
{
  INT i;
  CODEREP *retval = NULL;

  switch (Kind()) {
    case CK_VAR:
      if (Compare_bitpos(cr))
        return this;
      break;
    case CK_OP:
      for (i=0; i<Kid_count(); i++) {
	retval = Opnd(i)->Find_cr(cr);
	if (retval)
	  return retval;
      }
      break;
    case CK_IVAR:
      retval = (Ilod_base()) ? Ilod_base()->Find_cr(cr) :
                               Istr_base()->Find_cr(cr);
      if (retval)
	return retval;
      if (OPCODE_operator(Op()) == OPR_MLOAD)
	return Mload_size()->Find_cr(cr);
      else if (OPCODE_operator(Op()) == OPR_ILOADX)
	return Index()->Find_cr(cr);
      break;
  }
  return retval;
}

//----------------------------------------------------------------------------
// search LHS for definition point
// searches this BB and all dominator BBs until found
//
//  Returns NULL if a dead phi is found.
//
CODEREP *
BB_NODE::Find_def(const CODEREP *cr)
{
  CODEREP *retcr, *lhs;
  BB_NODE *bb;

  // are there cr's that will never have a def?
  if ( cr->Kind() == CK_IVAR ) {
    if ( cr->Opr() == OPR_PARM ) 
      return NULL;
  }

  for (bb = this; bb != NULL; bb = bb->Idom()) {

    STMTREP *stmt;
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    FOR_ALL_NODE_REVERSE(stmt,stmt_iter,Init()) {

      // check CHI node
      if (stmt->Has_chi()) {
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	FOR_ALL_NODE(cnode,chi_iter,Init(stmt->Chi_list())) {
	  if (cnode->Live()) {
	    retcr = cnode->RESULT();
	    if (retcr && cr->Same_bitpos(retcr)) {
              if (retcr->Dsctyp() == MTYPE_UNKNOWN ||
		  retcr->Is_flag_set(CF_MADEUP_TYPE)) {
                retcr->Set_dtyp(cr->Dtyp());
                retcr->Set_dsctyp(cr->Dsctyp()); 
                retcr->Set_lod_ty(MTYPE_To_TY(cr->Dsctyp()));
		retcr->Set_sign_extension_flag();
		retcr->Reset_flag(CF_MADEUP_TYPE);
              }
	      return retcr;
            }
	  }
	}
      }

      // check LHS for DEF of this CR
      lhs = stmt->Lhs();
      if (lhs) {
	switch (stmt->Opr()) {
        case OPR_STID:
	case OPR_STBITS:
	  if (cr->Compare_bitpos(lhs))
	    return lhs;
          break;
	case OPR_ISTORE:
	case OPR_ISTBITS:
	  retcr = lhs->Istr_base()->Find_cr(cr);
	  if (retcr)
	    return retcr;
	  break;
	case OPR_MSTORE:
	  retcr = lhs->Istr_base()->Find_cr(cr);
	  if (retcr)
	    return retcr;
	  retcr = lhs->Mstore_size()->Find_cr(cr);
	  if (retcr)
	    return retcr;
	  break;
	case OPR_ISTOREX:
	  retcr = lhs->Istr_base()->Find_cr(cr);
	  if (retcr)
	    return retcr;
	  retcr = lhs->Index()->Find_cr(cr);
	  if (retcr)
	    return retcr;
	  break;
	default:
	  Is_True(0,("BB_NODE::Find_def, unknown case"));
	  break;
	}
      }
    
    } // stmt loop

    // check PHI node
    PHI_LIST_ITER phi_iter;
    PHI_NODE *pnode;
    FOR_ALL_ELEM(pnode,phi_iter,Init(bb->Phi_list())) {
      if (!pnode->Dse_dead() && !pnode->Dce_dead() && (retcr = pnode->RESULT()) != NULL) {
	  if (cr->Same_bitpos(retcr)) {
            if (retcr->Dsctyp() == MTYPE_UNKNOWN ||
		retcr->Is_flag_set(CF_MADEUP_TYPE)) {
              retcr->Set_dtyp(cr->Dtyp());
              retcr->Set_dsctyp(cr->Dsctyp()); 
              retcr->Set_lod_ty(MTYPE_To_TY(cr->Dsctyp()));
	      retcr->Set_sign_extension_flag();
	      retcr->Reset_flag(CF_MADEUP_TYPE);
            }
	    return retcr;
          }
      } else 
	return NULL;
    } // phi loop
  } // bb loop

  return NULL;	  // not found
}

// CODEREP::Create_exp_in_bb
//   find or create the CODEREP for 'this' exprep with current version
//   in the 'bb'.
//
CODEREP *
CODEREP::Create_exp_in_bb(BB_NODE *bb, CODEMAP *htable)
{
  CODEREP *retv;
  switch (Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return this;
  case CK_VAR:
  case CK_IVAR:
    retv = bb->Find_def(this);
    if (retv)
      retv->Set_Bitpos(this->Bitpos());
    return retv;
  case CK_OP:
    {
      CODEREP *cr = Alloc_stack_cr(Extra_ptrs_used());
      INT i;
      BOOL need_rehash = FALSE;
      cr->Copy(*this);
      for (i=0; i<Kid_count(); i++) {
        CODEREP *opnd = Opnd(i);
        retv = opnd->Create_exp_in_bb(bb, htable);

        // cannot find current version down the tree
        if (retv == NULL) return NULL;

        retv->Set_Bitpos(opnd->Bitpos());
        cr->Set_opnd(i, retv);
        if (opnd != retv)
          need_rehash = TRUE;
      }
      if (!need_rehash)
        return this;
      retv = htable->Rehash(cr);

      if (retv->Bitpos() == ILLEGAL_BP)
        retv->Set_Bitpos(this->Bitpos());

      Is_True(retv->Bitpos() ==  this->Bitpos(),
              ("CODEREP::Create_exp_in_bb: two bitpos, %d and %d, are merged", retv->Bitpos(), Bitpos()));

      return retv;
    }
  }
  return this;
}


class DEFREP {
private:
  typedef enum {
    DEF_IS_NONE,
    DEF_IS_STMT,
    DEF_IS_PHI,
    DEF_IS_CHI
  } DEF_KIND;

  BOOL     _tracing;
  DEF_KIND _kind;
  union {
    STMTREP  *_stmt;
    PHI_NODE *_phi;
    struct {
      CHI_NODE *_chi;
      STMTREP  *_chi_stmt;
    } _chi_info;
  };

public:
  DEFREP(const BOOL tracing,
	 STMTREP *stmt,
	 BB_NODE *bb,
	 const CODEREP *cr)
    { _tracing = tracing; 
      Find_def_stmt_in_same_bb(stmt, bb, cr);
      if (_kind == DEF_IS_NONE) 
	Find_def_stmt(bb->Idom(), cr);
    }

  DEFREP(const BOOL tracing,
	       BB_NODE *bb,
	 const CODEREP *cr)
    { _tracing = tracing;
      Find_def_stmt(bb, cr); }

  ~DEFREP(void) {}

  void Find_def_stmt(BB_NODE *, const CODEREP *);

  void Find_def_stmt_in_same_bb(STMTREP *, BB_NODE *bb, const CODEREP *);

  BOOL      Tracing(void) const { return _tracing; }

  DEF_KIND  Kind(void) const    { return _kind; }

  void      Set_kind(DEF_KIND kind)
    { _kind = kind; }

  void      Set_stmt(STMTREP *stmt)
    {
      Set_kind(DEF_IS_STMT);
      _stmt = stmt;
    }

  void      Set_phi(PHI_NODE *phi)
    {
      Set_kind(DEF_IS_PHI);
      _phi = phi;
    }

  void      Set_chi(CHI_NODE *chi)
    {
      Set_kind(DEF_IS_CHI);
      _chi_info._chi = chi;
    }

  void      Set_chi_stmt(STMTREP *stmt)
    {
      Is_True(Is_chi_node(),
	      ("def must be by chi"));
      _chi_info._chi_stmt = stmt;
    }

  BOOL      Is_stmt_node(void) const
    { return Kind() == DEF_IS_STMT; }

  BOOL      Is_phi_node(void)  const
    { return Kind() == DEF_IS_PHI; }

  BOOL      Is_chi_node(void)  const
    { return Kind() == DEF_IS_CHI; }

  STMTREP  *Stmt(void)     const { return _stmt; }
  PHI_NODE *Phi(void)      const { return _phi; }
  CHI_NODE *Chi(void)      const { return _chi_info._chi; }
  STMTREP  *Chi_stmt(void) const { return _chi_info._chi_stmt; }

  CODEREP  *Result(void) const
    {
      if (Is_chi_node())
	return Chi()->RESULT();
      else if (Is_phi_node())
	return Phi()->RESULT();
      else {
	FmtAssert(Is_stmt_node(),
		  ("Unknown DEFREP kind"));
	return Stmt()->Lhs();
      }
    }

  void      Set_result(CODEREP *cr)
    {
      if (Is_chi_node()) {
	Chi()->Set_RESULT(cr);
	// The following is something of a hack, based on the fact
	// that we never set the DEFREP result to a zero-version
	// coderep.
	if (!cr->Is_flag_set(CF_IS_ZERO_VERSION)) {
	  Chi_stmt()->Recompute_has_zver();
	}
      }
      else if (Is_phi_node()) {
	Phi()->Set_result(cr);
      }
      else {
	FmtAssert(Is_stmt_node(),
		  ("Unknown DEFREP kind"));
	Stmt()->Set_lhs(cr);
      }
    }

  // Annotate_defined_coderep should probably be a member of class
  // CODEREP, since it encapsulates the maintenance of invariants
  // about CODEREP instances, not really about DEFREPs.

  void Annotate_defined_coderep(CODEREP *cr, CODEMAP *htable)
    {
      // Adjust fields of *cr to reflect its definition by *this.
      if (Is_phi_node()) {
	cr->Set_defphi(Phi());
	cr->Set_flag(CF_DEF_BY_PHI);
	cr->Set_flag(CF_INCOMPLETE_USES);
	Is_Trace(Tracing(), (TFile, "phi:\n"));
	Is_Trace_cmd(Tracing(), Phi()->Print(Phi()->Size(), TFile));
      }
      else if (Is_chi_node()) {
	cr->Set_defchi(Chi());
	cr->Set_defstmt(Chi_stmt());
	cr->Set_flag(CF_DEF_BY_CHI);
	cr->Set_flag(CF_INCOMPLETE_USES);
	Is_Trace(Tracing(), (TFile, "chi:\n"));
	Is_Trace_cmd(Tracing(), Chi()->Print(TFile));
      }
      else {
	Is_True(Is_stmt_node(),
		("DEFREP::Annotate_defined_coderep: Illegal DEFREP kind"));
	cr->Set_defstmt(Stmt());
	Is_Trace(Tracing(), (TFile, "stmt:\n"));
	Is_Trace_cmd(Tracing(), Stmt()->Print(TFile));
      }
      Is_Trace_cmd(Tracing(), cr->Print(2, TFile));
    }
};

// Find_def_stmt finds the statement defining the given coderep.
void
DEFREP::Find_def_stmt(      BB_NODE *bb,
		      const CODEREP *cr)
{
  for (; bb != NULL; bb = bb->Idom()) {
    // First check for definition by a real statement.
    STMTREP      *stmt;
    STMTREP_ITER stmt_iter(bb->Stmtlist());

    FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
      if (OPERATOR_is_scalar_store (stmt->Opr())) {
	if (stmt->Lhs()->Aux_id() == cr->Aux_id()) {
	  Set_stmt(stmt);
	  Is_Trace(Tracing(), (TFile, "Found def by stmt in BB:\n"));
	  Is_Trace_cmd(Tracing(), bb->Print(TFile));
	  return;
	}
      }
      if (stmt->Has_chi()) {
	CHI_LIST_ITER  chi_iter;
	CHI_NODE      *cnode;
	FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
	  if (cnode->Live() &&
	      cnode->RESULT()->Aux_id() == cr->Aux_id()) {
	    Set_chi(cnode);
	    Set_chi_stmt(stmt);
	    Is_Trace(Tracing(), (TFile, "Found def by chi in BB:\n"));
	    Is_Trace_cmd(Tracing(), bb->Print(TFile));
	    return;
	  }
	}
      }
    }

    PHI_LIST_ITER  phi_iter;
    PHI_NODE      *phi;

    FOR_ALL_ELEM(phi, phi_iter, Init(bb->Phi_list())) {
      CODEREP *res = phi->RESULT();
      if (phi->Aux_id() == cr->Aux_id()) {
	Set_phi(phi);
	Is_Trace(Tracing(), (TFile, "Found def by phi in BB:\n"));
	Is_Trace_cmd(Tracing(), bb->Print(TFile));
	return;
      }
    }
  }

  _kind = DEF_IS_NONE;
  _stmt = NULL;
}


// Find_def_stmt finds the statement defining the given coderep.
void
DEFREP::Find_def_stmt_in_same_bb(STMTREP *stmt, BB_NODE *bb, const CODEREP *cr)
{
  for (; stmt != NULL; stmt = stmt->Prev()) {
    if (OPERATOR_is_scalar_store (stmt->Opr())) {
      if (stmt->Lhs()->Aux_id() == cr->Aux_id()) {
	Set_stmt(stmt);
	Is_Trace(Tracing(), (TFile, "Found def by stmt in BB:\n"));
	Is_Trace_cmd(Tracing(), bb->Print(TFile));
	return;
      }
    }
    if (stmt->Has_chi()) {
      CHI_LIST_ITER  chi_iter;
      CHI_NODE      *cnode;
      FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
	if (cnode->Live() &&
	    cnode->RESULT()->Aux_id() == cr->Aux_id()) {
	  Set_chi(cnode);
	  Set_chi_stmt(stmt);
	  Is_Trace(Tracing(), (TFile, "Found def by chi in BB:\n"));
	  Is_Trace_cmd(Tracing(), bb->Print(TFile));
	  return;
	}
      }
    }
  }

  PHI_LIST_ITER  phi_iter;
  PHI_NODE      *phi;

  FOR_ALL_ELEM(phi, phi_iter, Init(bb->Phi_list())) {
    CODEREP *res = phi->RESULT();
    if (phi->Aux_id() == cr->Aux_id()) {
      Set_phi(phi);
      Is_Trace(Tracing(), (TFile, "Found def by phi in BB:\n"));
      Is_Trace_cmd(Tracing(), bb->Print(TFile));
      return;
    }
  }

  _kind = DEF_IS_NONE;
  _stmt = NULL;
}


//  Replace the phi opnd with a non-zero version.
//
void
CODEMAP::Fix_zero_version(PHI_NODE *phi, INT opnd_idx, bool allow_real_or_no_def)
{
  CODEREP *retval = phi->OPND(opnd_idx);
  Is_True(retval->Is_flag_set(CF_IS_ZERO_VERSION),
	  ("CODEMAP::Fix_zero_version: not a zero version."));

  Is_Trace(Tracing(),
	   (TFile, "CODEMAP::Fix_zero_version aux-id %d, bb %d, opnd %d\n",
	    phi->Aux_id(), phi->Bb()->Id(), opnd_idx));

  // It is possible that phi->BB's pred changes 
  if (phi->Bb()->Pred()->Len() <= opnd_idx) return;
  
  // Constructor for def_stmt will perform the find-def operation
  // (by aux_id) and set the fields of def_stmt accordingly.

  DEFREP def_stmt(Tracing(), phi->Bb()->Nth_pred(opnd_idx), retval);

  // The following assertion is too strong because a zero-version
  // might be defined by real assignment, after a conditional
  // branch becomes an unconditional branch.   Consider this situation.
  // real def --> phi1  --> phi2.    phi1 defines a zero version.
  // phi1 is removed after converting a conditional into unconditional br.
  // Then opnd of phi2 is defined by a real stmt.
  //
  if (!allow_real_or_no_def) {
    Is_True(def_stmt.Is_phi_node() || def_stmt.Is_chi_node(),
	    ("Zero version def should be phi or chi"));
  } else {
    // Fix 630415: in Insert_delete_phi of second renaming,
    //  it might not possible to find the defintion of a zero-version 
    //  if the "defining" phi has been removed (e.g., because of different opnd count)
    //  In that case, there is no need to fix the zero version because deletion of
    //  the "defining" phi guarantees that the definitions of its operands
    //  are not zero-versions.
    if (!def_stmt.Is_phi_node() && 
	!def_stmt.Is_chi_node() &&
	!def_stmt.Is_stmt_node()) {
      Is_Trace(Tracing(),
	       (TFile, "CODEMAP::Fix_zero_version: cannot fix aux-id %d, bb %d, opnd %d\n",
		phi->Aux_id(), phi->Bb()->Id(), opnd_idx));
      return;
    }
  }

  if ((def_stmt.Is_phi_node() && 
       (!def_stmt.Phi()->Live() ||
	def_stmt.Phi()->Dse_dead() ||
	def_stmt.Phi()->Dce_dead())) ||
      def_stmt.Result()->Is_flag_set(CF_IS_ZERO_VERSION)) {

    // Add the new definition to the htable; this coderep can no
    // longer be zero version.
    Is_Trace(Tracing(),
	     (TFile, " zero version defined by another zero version\n"));

    const CODEREP *phi_res = phi->RESULT();
    retval = Add_def(phi_res->Aux_id(),
		     -1,   /* dummy version number */
		     NULL, /* defstmt placeholder; will set later */
		     phi_res->Dtyp(),
		     phi_res->Dsctyp(),
		     phi_res->Offset(),
		     phi_res->Lod_ty(),
		     phi_res->Field_id(),
		     TRUE);

    if (phi_res->Is_flag_set(CF_MADEUP_TYPE))
      retval->Set_flag(CF_MADEUP_TYPE);

      // Fix 815093: Set volatile flag
     if ( Opt_stab()->Is_volatile( phi_res->Aux_id() ) ) {
       retval->Set_is_volatile();
     }
    
    // TODO: Probably need to set a bunch of *retval's fields
    // here. See opt_ivr.cxx for examples.
    
    def_stmt.Set_result(retval);
    def_stmt.Annotate_defined_coderep(retval, this);
  } else {
    Is_Trace(Tracing(),
	     (TFile, "   var_phi opnd defined by nonzero version\n"));
    retval = def_stmt.Result();
  }

  // *retval is now guaranteed to be non-zero-version. Replace the
  // zero version use with a use of the non-zero-version def we
  // found.
  Is_True(!retval->Is_flag_set(CF_IS_ZERO_VERSION),
	  ("Defining coderep must not be zero version"));

  phi->Set_opnd(opnd_idx, retval);
}


//  Replace the mu opnd with a non-zero version.
//
void
CODEMAP::Fix_zero_version(CHI_NODE *chi, STMTREP *stmt)
{
  CODEREP *retval;

  Is_True(chi->OPND()->Is_flag_set(CF_IS_ZERO_VERSION),
	  ("CODEMAP::Fix_zero_version: not a zero version."));

  Is_Trace(Tracing(),
	   (TFile, "CODEMAP::Fix_zero_version mu-node aux-id %d, bb %d\n",
	    chi->OPND()->Aux_id(), stmt->Bb()->Id()));

  // Constructor for def_stmt will perform the find-def operation
  // (by aux_id) and set the fields of def_stmt accordingly.

  DEFREP def_stmt(Tracing(), stmt->Prev(), stmt->Bb(), chi->OPND());

  Is_True(def_stmt.Is_phi_node() || def_stmt.Is_chi_node(),
	  ("Zero version def should be phi or chi"));

  if ((def_stmt.Is_phi_node() && 
       (!def_stmt.Phi()->Live() ||
	def_stmt.Phi()->Dse_dead() ||
	def_stmt.Phi()->Dce_dead())) ||
      def_stmt.Result()->Is_flag_set(CF_IS_ZERO_VERSION)) {

    // Add the new definition to the htable; this coderep can no
    // longer be zero version.
    Is_Trace(Tracing(),
	     (TFile, " zero version defined by another zero version\n"));

    const CODEREP *chi_opnd = chi->OPND();
    retval = Add_def(chi_opnd->Aux_id(),
		     -1,   /* dummy version number */
		     NULL, /* defstmt placeholder; will set later */
		     chi_opnd->Dtyp(),
		     chi_opnd->Dsctyp(),
		     chi_opnd->Offset(),
		     chi_opnd->Lod_ty(),
		     chi_opnd->Field_id(),
		     TRUE);

     // Fix 815093: Set volatile flag
     if ( Opt_stab()->Is_volatile( chi_opnd->Aux_id() ) ) {
       retval->Set_is_volatile();
     }

    // TODO: Probably need to set a bunch of *retval's fields
    // here. See opt_ivr.cxx for examples.
    
    def_stmt.Set_result(retval);
    def_stmt.Annotate_defined_coderep(retval, this);
  } else {
    Is_Trace(Tracing(),
	     (TFile, "   mu opnd defined by nonzero version\n"));
    retval = def_stmt.Result();
  }

  // *retval is now guaranteed to be non-zero-version. Replace the
  // zero version use with a use of the non-zero-version def we
  // found.
  Is_True(!retval->Is_flag_set(CF_IS_ZERO_VERSION),
	  ("Defining coderep must not be zero version"));

  chi->Set_OPND(retval);
}

