//-*-c++-*-

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_loop.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_loop.cxx,v $
//
// Revision history:
//  28-JAN-95 shin - Original Version
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
// The first part of this file implements COMP_UNIT::Normalize_loop,
// which normalizes DO loops for the first step of IVR.  Most of the
// work is done within the class NORMALIZE_LOOP.  This code acts on
// WHIRL nodes, while the other code in this file acts on CODEREPs.
//
// The second part implements several useful methods for loops:
// -> BB_LOOP::Invariant_cr
// -> BB_LOOP::Index_relative_expr
// -> CODEMAP::Convert_iload_to_loop_invariant
//
// The third part implements the procedures Can_raise_to_doloop and
// Fix_do_loop, which are called by the emitter to prepare loops to
// be converted back to WHIRL.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_loop_CXX	"opt_loop.cxx"
static char *rcs_id = 	opt_loop_CXX"$Revision: 1.7 $";
#endif /* _KEEP_RCS_ID */

#define USE_STANDARD_TYPES
#include <set>
#include <algorithm>

#include "defs.h"
#include "config.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "cxx_memory.h"
#include "wn.h"
#include "wn_util.h"
#include "region_util.h"
#include "stab.h"

#include "opt_base.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_ivr.h"
#include "opt_main.h"
#include "opt_util.h"
#include "opt_wn.h"
#include "opt_mu_chi.h"
#include "opt_alias_rule.h"

using std::set;

// ====================================================================
//
// NORMALIZE_LOOP
//
// ====================================================================


class NORMALIZE_LOOP {
private:
  WN_MAP    _wn_map;  // map to maintain the parent pointer
  OPT_PHASE _phase;

  // private constructor so it cannot be used
  NORMALIZE_LOOP(void);
  NORMALIZE_LOOP(const NORMALIZE_LOOP&);
  NORMALIZE_LOOP& operator = (const NORMALIZE_LOOP&);

  BOOL Check_if_index_is_passed(WN *wn, ST *st);
  BOOL Find_barrier_in_tree(WN *wn);
  BOOL Is_pdo_loop(WN *wn);
  WN  *Find_enclosing_parallel_region(WN *wn);
  BOOL Is_outermost_loop_in_parallel_region(WN *,WN_PRAGMA_ID);
  WN  *Normalize_do_loop( WN *wn, OPT_PHASE phase );

public:
  NORMALIZE_LOOP( WN_MAP wn_map, OPT_PHASE phase )
    : _wn_map(wn_map), _phase(phase) { }
  ~NORMALIZE_LOOP( void )            { }

  WN  *Normalize_loop_stmt(WN *wn, WN *parent);
};


// Returns TRUE iff the tree at wn contains and LDA for st

static BOOL
Find_addr_passed(WN *wn, ST *st)
{
  OPERATOR opr = WN_operator(wn);
  
  if (OPERATOR_is_load(opr))
    return FALSE;
  
  if (opr == OPR_LDA) {
    if (WN_st(wn) == st)
      return TRUE;
    else
      return FALSE;
  }
  for (INT i = 0; i < WN_kid_count(wn); i++) 
    if (Find_addr_passed(WN_kid(wn,i), st))
      return TRUE;

  return FALSE;
}


// Returns TRUE if there is CALL that can access the index st
// in the WHIRL tree wn
//
BOOL
NORMALIZE_LOOP::Check_if_index_is_passed(WN *wn, ST *st)
{
  if (wn == NULL)
    return FALSE;

  OPCODE   opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

  if (OPCODE_is_call(opc)) {
    for (INT32 i = 0; i < WN_kid_count(wn); i++) {
      WN *kid = WN_kid(wn,i);
      if (WN_operator(kid) == OPR_PARM &&
	  Find_addr_passed(kid, st))
	return TRUE;
    }
  }
  else if (opr == OPR_BLOCK) {
    for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt))
      if (Check_if_index_is_passed(stmt, st))
	return TRUE;
  } else if (! OPCODE_is_black_box(opc)) {
    for (INT32 i = 0; i < WN_kid_count(wn); i++)
      if (Check_if_index_is_passed(WN_kid(wn,i), st))
	return TRUE;
  }
  return FALSE;
}


// Returns TRUE if there is BARRIER in the tree
//
BOOL
NORMALIZE_LOOP::Find_barrier_in_tree(WN *wn)
{
  if (wn == NULL)
    return FALSE;

  OPCODE   opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

  if (opr == OPR_FORWARD_BARRIER ||
      opr == OPR_BACKWARD_BARRIER)
    return TRUE;

  else if (opr == OPR_BLOCK) {
    for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt))
      if (Find_barrier_in_tree(stmt))
	return TRUE;
  } else if (! OPCODE_is_black_box(opc)) {
    for (INT32 i = 0; i < WN_kid_count(wn); i++)
      if (Find_barrier_in_tree(WN_kid(wn,i)))
	return TRUE;
  }
  return FALSE;
}


// Find the enclosing parallel region
WN *
NORMALIZE_LOOP::Find_enclosing_parallel_region(WN *wn)
{
  WN *parent = (WN *) WN_MAP_Get(_wn_map, wn);
  while (parent != NULL && WN_opcode(parent) != OPC_FUNC_ENTRY) {
    if (WN_opcode(parent) == OPC_REGION)
      return parent;
    parent = (WN *) WN_MAP_Get(_wn_map, parent);
  }
  return NULL;
}


BOOL
NORMALIZE_LOOP::Is_pdo_loop(WN *wn)
{
  WN *parent = (WN *) WN_MAP_Get(_wn_map, wn);

  Is_True(WN_opcode(parent) == OPC_BLOCK,
          ("NORMALIZE_LOOP::Is_pdo_loop: do loop's parent node is not block"));

  parent = (WN *) WN_MAP_Get(_wn_map, parent);

  // the parent node should be a region node if wn is a pdo
  if (WN_opcode(parent) != OPC_REGION)
    return FALSE;

  return Is_region_with_pragma(parent,WN_PRAGMA_PDO_BEGIN);
}


// ====================================================================
// Determine if this loop is the outermost loop within a parallel region
// ====================================================================

BOOL
NORMALIZE_LOOP::Is_outermost_loop_in_parallel_region(
  WN *wn,
  WN_PRAGMA_ID pragma_id )
{
  WN *parent = (WN *) WN_MAP_Get(_wn_map, wn);

  Is_True(WN_opcode(parent) == OPC_BLOCK,
          ("NORMALIZE_LOOP::Is_outermost_loop_in_parallel_region: do loop's parent node is not block"));

  parent = (WN *) WN_MAP_Get(_wn_map, parent);

  // traverse up parents until we find either a loop (which means we're
  // not the outermost loop) or a parallel region
  while ( parent != NULL && WN_operator(parent) != OPR_FUNC_ENTRY ) {
    OPERATOR oper = WN_operator(parent);

    if ( oper == OPR_FUNC_ENTRY )
      return FALSE;
    else if ( oper == OPR_DO_LOOP )
      return FALSE;
    else if ( oper == OPR_REGION ) {
      if ( Is_region_with_pragma(parent,pragma_id) ) {
	return TRUE;
      }
    }

    // follow up parent chain
    parent = (WN *) WN_MAP_Get(_wn_map, parent);
  }

  return FALSE;
}


// ====================================================================
// Normalize the DO_LOOP
// If we create a new loop tree, return it, else if we just change some
// of the kids or do nothing, return NULL.
// ====================================================================

WN *
NORMALIZE_LOOP::Normalize_do_loop( WN *wn, OPT_PHASE phase )
{
  const OPCODE opc = WN_opcode(wn);
  Is_True( opc == OPC_DO_LOOP,
    ("Normalize_do_loop: Not a loop: %s", OPCODE_name(opc)) );

  WN *step = WN_step(wn);
  if (WN_operator(step) == OPR_STID &&
      ((WN_operator(WN_kid0(step)) == OPR_ADD &&
	WN_operator(WN_kid0(WN_kid0(step))) == OPR_INTCONST &&
	WN_const_val(WN_kid0(WN_kid0(step))) != 1) ||
       (WN_operator(WN_kid0(step)) == OPR_ADD &&
	WN_operator(WN_kid1(WN_kid0(step))) == OPR_INTCONST &&
	WN_const_val(WN_kid1(WN_kid0(step))) != 1) ||
       (WN_operator(WN_kid0(step)) == OPR_SUB &&
	WN_operator(WN_kid1(WN_kid0(step))) == OPR_INTCONST &&
	WN_const_val(WN_kid1(WN_kid0(step))) != -1)))
  {
    WN *oldstep = WN_step(wn);
    WN *oldstart = WN_start(wn);

    // If the index variable is address taken and there is some CALL in the
    // loopbody, then do not normalize.
    // 
    // Lots of problem here!  There are only so many we can cover.
    //  -Raymond 12/9/98.
    //
    if (phase != MAINOPT_PHASE && ST_class(WN_st(oldstart)) == CLASS_VAR) {
      ST *index_st = WN_st(oldstart);
      if (ST_sclass(index_st) != SCLASS_AUTO && 
	  ST_sclass(index_st) != SCLASS_PSTATIC &&
	  ST_sclass(index_st) != SCLASS_FORMAL_REF)
	return NULL;
      if (Check_if_index_is_passed(WN_do_body(wn), index_st))
	return NULL;
    }

    // work around 499285:  the complex fix required 
    //  to remember the original do-loop index variable.
    //  
    if (phase != MAINOPT_PHASE &&
	Find_barrier_in_tree(WN_do_body(wn)))
      return NULL;

    if (phase != PREOPT_PHASE && 
        phase != PREOPT_LNO_PHASE &&
        phase != PREOPT_IPA0_PHASE &&
        phase != PREOPT_IPA1_PHASE)
      return NULL;

    // if the loop is outermost in parallel region, we cannot
    // normalize it (sort of pv #548916, but this doesn't fix
    // that bug)
    if ( !WOPT_Enable_IVR_Outermost_Loop_Parallel_Region &&
	 (Is_outermost_loop_in_parallel_region(wn,WN_PRAGMA_PDO_BEGIN)))
    {
      return NULL;
    }

    // create a block to hold the old init statement
    WN *newblock = WN_CreateBlock();
    WN_Set_Linenum(newblock, WN_Get_Linenum(wn));
    WN_INSERT_BlockBefore(newblock, NULL, oldstart);

    MTYPE mtype = WN_desc (oldstep );
    
    // Do not try to handle I2 induction variables, at least not
    // with do-loops
    if (mtype != MTYPE_I4 && mtype != MTYPE_I8)
      return NULL;

    ST   *preg_st = (MTYPE_size_min(mtype) > 32) ? Int64_Preg : Int32_Preg;
    
    TY_IDX ty = WN_ty(WN_kid0(WN_kid0(step)));
    IDTYPE newindex = Create_Preg(TY_mtype(ST_type(preg_st)),".do_ivar", NULL);
    WN *kid0, *kid1, *init, *incr;
    kid0 = WN_CreateIntconst( OPCODE_make_op(OPR_INTCONST, mtype, MTYPE_V),
			      (INT64) 0 );
    init = WN_CreateStid(OPCODE_make_op(OPR_STID, MTYPE_V, mtype),
			 newindex, preg_st, ty, kid0);
    WN_CopyMap(init, RID_map, wn);
    WN_Set_Linenum(init, WN_Get_Linenum(wn));

    kid0 = WN_CreateLdid(OPCODE_make_op(OPR_LDID, mtype, mtype),
			 newindex, preg_st, ty);
    kid1 = WN_CreateIntconst( OPCODE_make_op(OPR_INTCONST, mtype, MTYPE_V),
			      (INT64) 1 );
    kid0 = WN_CreateExp2(OPCODE_make_op(OPR_ADD, mtype, MTYPE_V),
			 kid0, kid1);
    incr = WN_CreateStid(OPCODE_make_op(OPR_STID, MTYPE_V, mtype),
			 newindex, preg_st, ty, kid0);
    WN_CopyMap(incr, RID_map, wn);
    WN_Set_Linenum(incr, WN_Get_Linenum(wn));
    
    Is_True(WN_opcode(WN_do_body(wn)) == OPC_BLOCK,
	    ("DO_LOOP body is not OPC_BLOCK."));
    // insert oldstep at the end of the loop body
    WN_next(oldstep) = WN_prev(oldstep) = NULL;
    WN_INSERT_BlockBefore( WN_do_body(wn), NULL, oldstep );
    WN *newloop = WN_CreateDO(WN_CreateIdname(newindex, preg_st),
			      init, WN_end(wn), incr, WN_do_body(wn), NULL);
    WN_COPY_All_Maps(newloop, wn);

    WN_CopyMap(newloop, RID_map, wn);
    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_duplicate_node( wn, newloop );
    WN_Set_Linenum(newloop, WN_Get_Linenum(wn));
    Set_wn_flags( newloop, Wn_flags(newloop) | WN_FLAG_DO_LOOP);
    // newblocks holds the original init statement and the transformed loop.
    WN_INSERT_BlockBefore( newblock, NULL, newloop);

    // update LASTLOCAL,LOCAL,SHARED,REDUCTION pragma
    WN *region_wn = wn;
    while (region_wn = Find_enclosing_parallel_region(region_wn)) {
      WN *pragma_block = WN_region_pragmas(region_wn);
      STMT_ITER stmt_iter; WN *stmt;
      FOR_ALL_ELEM (stmt, stmt_iter,
		    Init(WN_first(pragma_block), WN_last(pragma_block))) {
	if (WN_operator(stmt) == OPR_PRAGMA &&
	    (WN_pragma(stmt) == WN_PRAGMA_LASTLOCAL ||
	     WN_pragma(stmt) == WN_PRAGMA_LOCAL ||
	     WN_pragma(stmt) == WN_PRAGMA_SHARED ||
	     WN_pragma(stmt) == WN_PRAGMA_FIRSTPRIVATE ||
	     WN_pragma(stmt) == WN_PRAGMA_REDUCTION) &&
	    WN_st(stmt) == WN_st(oldstart) &&
	    WN_pragma_arg1(stmt) == WN_offset(oldstart)) {
	  WN_PRAGMA_ID pragma_id = (WN_PRAGMA_ID) WN_pragma(stmt);
	  WN *new_pragma = WN_CreatePragma(pragma_id, preg_st, newindex, 0);
	  WN_INSERT_BlockBefore( pragma_block, stmt, new_pragma);
	  return newblock;
	}
      }
    }
    return newblock;
  }

  return NULL;
}


// Normalize_loop_stmt invokes Normalize_do_loop on each OPC_DO_LOOP

WN *
NORMALIZE_LOOP::Normalize_loop_stmt(WN *wn, WN *parent)
{
  if (wn == NULL)
    return NULL;

  const OPCODE opc = WN_opcode(wn);
  
  WN_MAP_Set(_wn_map, wn, (void *)parent);

  if ( opc == OPC_BLOCK ) {
    WN *stmt, *replace, *nextstmt = NULL;
    for ( stmt = WN_first(wn); stmt != NULL; stmt = nextstmt ) {
      // save it in case we get rid of stmt
      
      nextstmt = WN_next(stmt);
      if ( (replace = Normalize_loop_stmt(stmt, wn)) != NULL ) {
	// replace the current statement with the new one, by inserting
	// after it and then pulling out the current one
	WN_INSERT_BlockAfter( wn, stmt, replace );
	WN_EXTRACT_FromBlock( wn, stmt );
	// and we can get rid of the one we replaced
	// NOTE: do not delete the whole tree, just this stmt node
	//       because we probably didn't copy trees that we use
	//       from the original
	WN_Delete(stmt);
      }
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      if (OPCODE_is_scf(WN_opcode(WN_kid(wn,i)))) {
	WN *insert = Normalize_loop_stmt(WN_kid(wn,i), wn);
	Is_True(insert == NULL, ("not a BLOCK"));
      }
    }
  }
  
  if (opc == OPC_DO_LOOP) 
    return Normalize_do_loop( wn, _phase );

  return NULL;
}


WN *
COMP_UNIT::Normalize_loop(WN *wn)
{
  if (wn == NULL)
    return NULL;

  OPT_POOL_Push( Loc_pool(), -1 );
  WN_MAP wn_map = WN_MAP_Create( Loc_pool());

  WN *result;
  {
    NORMALIZE_LOOP norm_loop(wn_map, _phase);
    result = norm_loop.Normalize_loop_stmt(wn, NULL);
  }

  WN_MAP_Delete(wn_map);
  OPT_POOL_Pop( Loc_pool(), -1);
  return result;
}


// ====================================================================
//
// Invariant_cr returns TRUE only if the value of cr is determined to
// be loop invariant for the given loop.
//
// Invariant_cr_rec is a private helper function called only by
// Invariant_cr.  Invariant_cr_rec recursively examines the definition
// and children of cr as necessary to determine whether the value of cr
// is loop invariant.
//
// If WOPT_Enable_Prune is TRUE, then Invariant_cr and Invariant_cr_rec
// set the flag ISOP_INVARIANT_VISITED for OPs found to be loop
// invariant.
//
// ====================================================================


BOOL
BB_LOOP::Invariant_cr_rec( CODEREP *cr ) const
{
  // Some CODEREP kinds are always constant
  if ( inCODEKIND( cr->Kind(), CK_LDA | CK_CONST | CK_RCONST ) ) {
    return TRUE;
  }

  // If cr is defined by a phi-statement, then cr is loop-invariant
  // iff the phi-statement's BB is outside this loop.
  if ( cr->Is_flag_set( CF_DEF_BY_PHI ) ) {
    return ( ! True_body_set()->MemberP( cr->Defphi()->Bb() ) );
  }

  // If cr is defined by a chi node, then cr is loop-invariant
  // iff the chi's statement's BB is outside this loop.
  if ( cr->Is_flag_set( CF_DEF_BY_CHI ) ) {
    return ( ! True_body_set()->MemberP( cr->Defstmt()->Bb() ) );
  }

  switch ( cr->Kind() ) {
  case CK_VAR:
    // volatile references are never invariant
    if ( cr->Is_var_volatile() ) {
      return FALSE;
    }

    // CK_VAR may be a virtual variable
    if ( cr->Defstmt() == NULL ) {
      return FALSE;        // no def means it's zero version
    }

    return ( ! True_body_set()->MemberP( cr->Defstmt()->Bb() ) );

  case CK_IVAR:
    {
      CODEREP *base = cr->Istr_base() ? cr->Istr_base() : cr->Ilod_base();

      // volatile references are never loop invariant
      if (cr->Is_ivar_volatile() ||
         !Invariant_cr_rec(base) ||
        (cr->Opr() == OPR_MLOAD && ! Invariant_cr_rec( cr->Mload_size())) ||
        (cr->Opr() == OPR_ILOADX && ! Invariant_cr_rec( cr->Index()))) {
        if ( WOPT_Enable_Prune ) 
            cr->Reset_isop_visited( ISOP_INVARIANT_VISITED );
        return FALSE;
      }
      MU_NODE *mnode = cr->Ivar_mu_node();
      if ( mnode ) {
	    CODEREP *opnd = mnode->OPND();
	    if ( opnd && ! Invariant_cr_rec( opnd ) ) {
            if ( WOPT_Enable_Prune )
                cr->Reset_isop_visited( ISOP_INVARIANT_VISITED );
	        return FALSE;
        }  
      }

    // no need to check Ivar_defstmt.  It is NULL unless
    // there is a ISTORE of the base address.
    // 
    // if ( cr->Ivar_defstmt() == NULL ) {
    // return FALSE;	// no def means it's zero version
    // }
    // return ( ! True_body_set()->MemberP( cr->Ivar_defstmt()->Bb() ) );

    return TRUE;
  }  
  case CK_OP:
    {
      if ( cr->Is_isop_flag_set( ISOP_INVARIANT_VISITED )
	   && WOPT_Enable_Prune )
	return TRUE;

      // make sure all of the kids are invariant
      for ( INT32 ikid = 0; ikid < cr->Kid_count(); ikid++ ) {
	if ( ! Invariant_cr_rec( cr->Opnd( ikid ) ) ) {
	  if ( WOPT_Enable_Prune ) {
	    for ( INT32 j = 0; j < ikid; j++ ) // reset ones visited so far
	      cr->Opnd(j)->Reset_isop_visited( ISOP_INVARIANT_VISITED );
	  }
	  return FALSE;
	}
      }
      // if we make it here, must be invariant
      if ( WOPT_Enable_Prune )
	cr->Set_isop_flag( ISOP_INVARIANT_VISITED );
      return TRUE;
    }

  default:
    FmtAssert( FALSE, ("BB_LOOP::Invariant_cr_rec: Bad coderep") );
    return FALSE;
  }
}


BOOL
BB_LOOP::Invariant_cr( CODEREP *cr ) const
{
  if ( this == NULL )  // this BB is not in a loop
    return FALSE;

  BOOL ret = Invariant_cr_rec( cr );
  if ( ret && WOPT_Enable_Prune )
    cr->Reset_isop_visited( ISOP_INVARIANT_VISITED );
  return ret;
}


// ====================================================================
//
// Index_relative_expr determines if expr is constant relative to the
// index variable in the loop.  Index_relative_expr returns TRUE iff
// all parts of the expression are loop-invariant except for the CK_VAR
// index.
// NOTE: Caller should verify that expr->Contains(index) is TRUE.
//
// ====================================================================


BOOL
BB_LOOP::Index_relative_expr( CODEREP *expr, const CODEREP *index ) const
{
  Is_True( index->Kind() == CK_VAR,
    ( "BB_LOOP::Index_relative_expr: index is not var" ) );

  switch ( expr->Kind() ) {
  case CK_OP:
    {
      for ( INT32 ekid = 0; ekid < expr->Kid_count(); ekid++ ) {
        if ( ! Index_relative_expr( expr->Opnd( ekid ), index ) )
          return FALSE;
      }
    }
    return TRUE;

  case CK_VAR:
    // only valid variable is the index itself, or invariant one
    return ( expr == index || Invariant_cr( expr ) );

  case CK_IVAR:
    return ( Invariant_cr( expr ) );

  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return TRUE;

  default:
    FmtAssert( FALSE, ( "BB_LOOP::Index_relative_expr: invalid kind" ) );
    break;
  }

  // get to here, must not be valid
  return FALSE;
}


// ====================================================================
//
// Find_variant returns NULL if expr is loop invariant.  Otherwise,
// Find_variant returns one CK_VAR or CK_IVAR within expr that is not
// loop invariant.
//
// NOTE: Maybe we can merge this functionality into Invariant_cr
// without affecting its run time (but, Find_variant skips
// ISOP_INVARIANT_VISITED).
//
// ====================================================================


CODEREP *Find_variant(BB_LOOP *loop, CODEREP *expr) 
{
  switch (expr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return NULL;
    
  case CK_IVAR:
  case CK_VAR:
    if (loop->Invariant_cr(expr))
      return NULL;
    else
      return expr;
    
  case CK_OP:
    {
      for (INT i = 0; i < expr->Kid_count(); i++) {
	CODEREP *variant = Find_variant( loop, expr->Opnd(i) );
	if ( variant )
	  return variant;
      }
      return NULL;
    }

  default:
    Is_True( FALSE, ( "BB_LOOP::Find_variant: unsupported CK_KIND" ) );
  }
  return NULL;
}


// ====================================================================
//
// Is_linear_function_of returns TRUE if the expr is a linear function
// of the VAR/IVAR variant, i.e.,
//    expr == c1 * variant + c2.
//
//  Note that an invariant is also a linear function, i.e. c1==0.
//
//  Note that this function could be conservative, e.g.,
//     i + sqrt(i) - sqrt(i) is not recongized as a linear function of i.
//
// Linear_function is a helper procedure.
// EXPR_TYPE and LINEAR_FUNCTION are helper structs.
//
// ====================================================================


enum EXPR_TYPE { INVAR_EXPR, LINEAR_EXPR, NON_LINEAR_EXPR };

struct LINEAR_FUNCTION {
  EXPR_TYPE  type;
  CODEREP   *variant;
  LINEAR_FUNCTION(EXPR_TYPE e, CODEREP *v) : type(e), variant(v) {}
  LINEAR_FUNCTION(EXPR_TYPE e) : type(e) {}
};


static LINEAR_FUNCTION 
Linear_function(BB_LOOP *loop, CODEREP *expr) 
{
  switch (expr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return LINEAR_FUNCTION(INVAR_EXPR);
    
  case CK_IVAR:
  case CK_VAR:
    if (loop->Invariant_cr(expr))
      return LINEAR_FUNCTION(INVAR_EXPR);
    else 
      return LINEAR_FUNCTION(LINEAR_EXPR, expr);
    
  case CK_OP:
    OPERATOR opr = expr->Opr();
    switch (opr) {
    case OPR_PAREN:
    case OPR_NEG:
      return Linear_function(loop, expr->Opnd(0));

    case OPR_MPY:
      {
	LINEAR_FUNCTION l0 = Linear_function(loop, expr->Opnd(0));
	LINEAR_FUNCTION l1 = Linear_function(loop, expr->Opnd(1));

	if (l0.type == INVAR_EXPR)
	  return l1;
	if (l1.type == INVAR_EXPR)
	  return l0;

	return LINEAR_FUNCTION(NON_LINEAR_EXPR);
      }

    case OPR_ADD:
    case OPR_SUB:
      {
	LINEAR_FUNCTION l0 = Linear_function(loop, expr->Opnd(0));
	LINEAR_FUNCTION l1 = Linear_function(loop, expr->Opnd(1));

	if (l0.type == INVAR_EXPR)
	  return l1;
	if (l1.type == INVAR_EXPR)
	  return l0;
	if (l0.type == LINEAR_EXPR && l1.type == LINEAR_EXPR &&
	    l0.variant == l1.variant)
	  return l0;
	return LINEAR_FUNCTION(NON_LINEAR_EXPR);
      }

    default:
      if (loop->Invariant_cr(expr))
	return LINEAR_FUNCTION(INVAR_EXPR);
      else
	return LINEAR_FUNCTION(NON_LINEAR_EXPR);
    }
  }
  if (loop->Invariant_cr(expr))
    return LINEAR_FUNCTION(INVAR_EXPR);
  else
    return LINEAR_FUNCTION(NON_LINEAR_EXPR);
}
    

inline BOOL
Is_linear_function_of(CODEREP *expr, CODEREP *variant, BB_LOOP *loop)
{
  LINEAR_FUNCTION l = Linear_function(loop, expr);
  return (l.type == INVAR_EXPR ||
	  (l.type == LINEAR_EXPR && l.variant == variant));
}


// ====================================================================
//
// Convert_iload_to_loop_invariant searches the expression cr and
// attempts to replace IVARs (ILOADs) with loop invariant versions.
//
// Found_aliasing_store_in_loop is a helper procedure invoked only by
// Convert_iload_to_loop_invariant.  Found_aliasing_store_in_loop
// returns TRUE if the loop contains any STID/ISTORE/MSTORE statement
// that may alias with (pt, ty).  When it is first called, bb must
// dominate all blocks in the loop.
//
// ====================================================================


static BOOL
Found_aliasing_store_in_loop(POINTS_TO *pt, TY_IDX ty, BB_LOOP *loop,
			     BB_NODE *bb, OPT_STAB *opt_stab)
{
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    POINTS_TO *pt2;
    TY_IDX     ty2;

    switch (stmt->Opr()) {
    case OPR_STBITS:
    case OPR_STID:
      ty2 = stmt->Lhs()->Lod_ty();
      pt2 = stmt->Lhs()->Points_to(opt_stab);
      if (opt_stab->Rule()->Aliased_Memop(pt, pt2, ty, ty2))
	return TRUE;
      break;

    case OPR_ISTORE:
    case OPR_ISTBITS:
    case OPR_ISTOREX:
      ty2 = stmt->Lhs()->Ilod_ty();
      pt2 = stmt->Lhs()->Points_to(opt_stab);
      if (opt_stab->Rule()->Aliased_Memop(pt, pt2, ty, ty2))
	return TRUE;
      break;

    case OPR_MSTORE:
      ty2 = TY_IDX_ZERO;
      pt2 = stmt->Lhs()->Points_to(opt_stab);
      if (opt_stab->Rule()->Aliased_Memop(pt, pt2, ty, ty2))
	return TRUE;
      break;
      
    case OPR_RETURN:
    case OPR_RETURN_VAL:
    case OPR_GOTO:
    case OPR_LABEL:
    case OPR_TRUEBR:
    case OPR_FALSEBR:
    case OPR_EVAL:
    case OPR_PRAGMA:
    case OPR_XPRAGMA:
#ifdef KEY
    case OPR_GOTO_OUTER_BLOCK:
#endif
      break;

    default:
      return TRUE;
    }
  }
  
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    if (loop->True_body_set()->MemberP(dom_bb))
      if (Found_aliasing_store_in_loop(pt, ty, loop, dom_bb, opt_stab))
	return TRUE;
  }
  return FALSE;
}


void
CODEMAP::Convert_iload_to_loop_invariant(BB_LOOP *loop, CODEREP *cr)
{
  switch (cr->Kind()) {
  case CK_OP:
    {
      for (INT i = 0; i < cr->Kid_count(); i++) {
	Convert_iload_to_loop_invariant(loop, cr->Opnd(i));
      }
    }
    break;
  case CK_IVAR:
    {
      if (cr->Is_ivar_volatile())
	return;
      
      MU_NODE *mnode = cr->Ivar_mu_node();
      if (mnode == NULL)
	return;

      CODEREP *vsym = mnode->OPND();
      BB_NODE *defbb = vsym->Defbb();
      if ( defbb == NULL || ! loop->True_body_set()->MemberP( defbb ) )
	return;

      BB_NODE *startbb = loop->Header();
	
#ifdef KEY
      if (! WOPT_Enable_Invariant_Loop_Bounds)
#endif
      // Use alias analysis to see if the vsym is an invariant
      // If vsym aliases with any STID/ISTORE/MSTORE is the loop, then
      // don't replace it.
      if (Found_aliasing_store_in_loop(cr->Points_to(Opt_stab()),
				       cr->Ilod_ty(),
				       loop,
				       startbb,
				       Opt_stab()))
	return;


      if (Get_Trace(TP_GLOBOPT, IVR_DUMP_FLAG)) {
	fprintf(TFile,
		"IVR:  convert iload to invar in loop with body BB %d.\n",
		startbb->Id());
      }

      // update the vsym to a different version

      //  Iterate through each phi-node 
      PHI_NODE *phi;
      PHI_LIST_ITER phi_iter;

      FOR_ALL_ELEM (phi, phi_iter, Init(startbb->Phi_list())) {
	if (phi->Live() && phi->Aux_id() == vsym->Aux_id()) {
	  BB_NODE *pred;
	  BB_LIST_ITER bb_iter;
	  FOR_ALL_ELEM (pred, bb_iter, Init(startbb->Pred())) {
	    if (pred == loop->Preheader()) {
	      CODEREP *tmp = phi->OPND(bb_iter.Idx());
	      mnode->Set_OPND(tmp);
	      return;
	    }
	  }
	}
      }
    }
    break;

  default:
    break;
  }
}


// ====================================================================
//
// Compute_dependence
//
// ====================================================================


// Iterate thru all subexpr
//
template <class BOOL_FUNC>
BOOL expr_iter(CODEREP *cr, BOOL_FUNC f)
{
  if (f(cr)) return TRUE;

  switch ( cr->Kind() ) {
  case CK_IVAR:
    {
      CODEREP *base =
	(cr->Istr_base()) ? cr->Istr_base() : cr->Ilod_base();
      if (expr_iter(base, f))
	return TRUE;
      if (cr->Opr() == OPR_MLOAD && expr_iter(cr->Mload_size(), f))
	return TRUE;
      if (cr->Opr() == OPR_ILOADX && expr_iter(cr->Index(), f))
	return TRUE;
      MU_NODE *mnode = cr->Ivar_mu_node();
      if (mnode) {
	CODEREP *opnd = mnode->OPND();
	if (opnd && expr_iter(opnd, f))
	  return TRUE;
      }
    }
    break;

  case CK_OP:
    for (INT32 ikid = 0; ikid < cr->Kid_count(); ikid++ ) {
      if (expr_iter(cr->Opnd(ikid), f))
	return TRUE;
    }
    break;
  }
  return FALSE;
}


template <class BOOL_FUNC>
BOOL ref_iter(CODEREP *cr, BOOL_FUNC f)
{
  expr_iter(cr, f);
}


template <class BOOL_FUNC>
BOOL ref_iter(STMTREP *stmt, BOOL_FUNC f)
{
  if (stmt->Rhs())   // OPT_CHI has no RHS!
    if (expr_iter(stmt->Rhs(), f))
      return TRUE;
  if (OPERATOR_is_scalar_istore (stmt->Opr())
#ifdef KEY
      || stmt->Opr() == OPR_MSTORE
#endif
	  ) {
    if (expr_iter(stmt->Lhs(), f)) return TRUE;
  }
  return FALSE;
}


// Iterate thru all mods
template <class BOOL_FUNC>
BOOL mod_iter(STMTREP *stmt, BOOL_FUNC f)
{
  if (stmt->Has_chi()) {
    CHI_LIST_ITER chi_iter;
    CHI_NODE *cnode;
    CHI_LIST *chi_list = stmt->Chi_list();
    FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
      if (!cnode->Dse_dead()) {
	if (f(cnode->RESULT()))
	  return TRUE;
      } else {
	if (f(cnode->Aux_id()))
	  return TRUE;
      }
    }
  }
  if (OPERATOR_is_scalar_store (stmt->Opr()))
    if (f(stmt->Lhs())) return TRUE;
  return FALSE;
}


struct collect_use {
  set<AUX_ID> *use;
  BOOL operator()(CODEREP *cr) {
    if (cr->Kind() == CK_VAR)
      use->insert(cr->Aux_id());
    return FALSE;
  }
  collect_use(set<AUX_ID> *u) : use(u) {}
};


struct collect_def {
  set<AUX_ID> *def;
  BOOL operator()(CODEREP *cr) {
    if (cr->Kind() == CK_VAR)
      def->insert(cr->Aux_id());
    return FALSE;
  }
  BOOL operator()(AUX_ID id) {
    def->insert(id);
    return FALSE;
  }
  collect_def(set<AUX_ID> *d) : def(d) {}
};


enum DEPENDENCE_KIND {
  NO_DEPENDENCE      = 0,
  ANTI_DEPENDENCE    = 1,
  FLOW_DEPENDENCE    = 2,
  OUTPUT_DEPENDENCE  = 4,
  UNKNOWN_DEPENDENCE = 8,
};


//  Compute the dependence of stmt w.r.t to the set of statement starting
//  after 'stmt' and terminates at <end_bb, after_this_stmt>.
//
//   * after_this_stmt could be NULL if 'end_bb' contains no statements.
//
static DEPENDENCE_KIND
Compute_dependence(STMTREP *stmt, BB_NODE *end_bb, STMTREP *after_this_stmt)
{
  Is_True(after_this_stmt == NULL || after_this_stmt->Bb() == end_bb,
	  ("Compute_dependence: inconsistency input"));
  Is_True(stmt->Bb()->Dominates(end_bb), 
	  ("Compute_dependence: inconsistency input"));  // this impl didn't know liveness

  set<AUX_ID> use;
  set<AUX_ID> def;
  {
    collect_use collect_use(&use);
    collect_def collect_def(&def);
    ref_iter(stmt, collect_use);
    mod_iter(stmt, collect_def);
  }
	  
  BB_NODE *begin_bb = stmt->Bb();
  BB_NODE *bb = begin_bb;
  STMTREP *begin_stmt;
  STMTREP *end_stmt;

  set<AUX_ID> path_use;
  set<AUX_ID> path_def;
  collect_use collect_use(&path_use);
  collect_def collect_def(&path_def);

  while (bb) {
    if (bb == begin_bb)
      begin_stmt = stmt->Next();
    else
      begin_stmt = bb->First_stmtrep();
    
    if (bb == end_bb)
      end_stmt = after_this_stmt;
    else
      end_stmt = bb->Last_stmtrep();
    
    stmt = begin_stmt; 
    while (stmt) {
      ref_iter(stmt, collect_use);
      mod_iter(stmt, collect_def);

      if (stmt == end_stmt) break;
      stmt = stmt->Next();
    }
      
    if (bb == end_bb) break;
    if (bb->Succ()->Len() > 1) return UNKNOWN_DEPENDENCE;
    bb = bb->Nth_succ(0);
  }

  DEPENDENCE_KIND ret = NO_DEPENDENCE;
  
  vector<AUX_ID> dummy_vec;

  // anti-dependence
  set_intersection(use.begin(), use.end(), path_def.begin(), path_def.end(), 
		   inserter(dummy_vec, dummy_vec.begin()));
  if (dummy_vec.begin() != dummy_vec.end())
    ret = (DEPENDENCE_KIND)(ret | ANTI_DEPENDENCE);

  // output-dependence
  dummy_vec.erase(dummy_vec.begin(), dummy_vec.end());
  set_intersection(def.begin(), def.end(), path_def.begin(), path_def.end(),
		   inserter(dummy_vec, dummy_vec.begin()));
  if (dummy_vec.begin() != dummy_vec.end())
    ret = (DEPENDENCE_KIND)(ret | OUTPUT_DEPENDENCE);

  // flow-dependence
  dummy_vec.erase(dummy_vec.begin(), dummy_vec.end());
  set_intersection(def.begin(), def.end(), path_use.begin(), path_use.end(),
		   inserter(dummy_vec, dummy_vec.begin()));
  if (dummy_vec.begin() != dummy_vec.end())
    ret = (DEPENDENCE_KIND)(ret | FLOW_DEPENDENCE);

  return ret;
}


// ====================================================================
//
// Can_raise_to_doloop is called by EMITTER::Emit to ...
//
// ====================================================================


//  A while-loop can be raised to a do-loop if
//   1) it is a loop;
//   2) the loop header has one backedge;
//   3) the loop header has one non-backedge;
//   4) the loop header has one non-label statement 
//   5) the last statement is a conditional branch
//   6) the condition branch has a comparison expression
//   ...

#ifdef Is_True_On
inline BOOL RAISE(BOOL r, const char *msg)
{
  if (Get_Trace(TP_GLOBOPT, EMIT_DUMP_FLAG))
    if (!r)
      fprintf(TFile,
	      "Can_raise_to_doloop: doloop not raised because %s\n", msg);
  return r;
}
#else
/* ARGSUSED */
inline BOOL RAISE(BOOL r, const char *msg) { return r; }
#endif


BOOL
Can_raise_to_doloop(BB_LOOP *loop, BOOL repair, CODEMAP *htable)
{
  if (!loop->Well_formed()) return RAISE(FALSE, "loop not well formed");

  // Fix 620119: the current proposal is not to raise a loop that have 
  // early exit (i.e., the first do-loop) in PREOPT.   -Raymond 7/28/98.
  // 
  // Message from Dror:
  // I still think the proposed solution is fine.  If the outer loop was
  // originally (in the source) a while loop, and it has an early exit,
  // do not raise it to a DO loop.  MP loops can not have early exits,
  // so this is not an issue.  

#if !defined(OSP_OPT) || !defined(TARG_IA64)
  if (loop->Exit_early()) return RAISE(FALSE, "loop exits early");
#endif
  BB_NODE *header = loop->Header();
  BB_NODE *preheader = loop->Preheader();
  BB_NODE *loopback = loop->Loopback();
  INT preheader_opnd_num = loop->Preheader_pred_num();
  INT loopback_opnd_num = loop->Loopback_pred_num();

  STMTREP_ITER stmt_iter(header->Stmtlist());
  STMTREP *sr;
  FOR_ALL_NODE( sr, stmt_iter, Init() ) {
    if (sr->Op() != OPC_LABEL && sr != header->Last_stmtrep()) 
      return RAISE(FALSE, "too many stmt in header BB");
  }
  
  STMTREP *cond_br = header->Branch_stmtrep();

  // make sure we had a conditional branch
  if ( cond_br->Op() != OPC_TRUEBR && cond_br->Op() != OPC_FALSEBR ) 
    return RAISE(FALSE, "not a conditional branch");

  // make sure we have a comparison with two operands
  CODEREP *cmp = cond_br->Rhs();
  if ( cmp->Kind() != CK_OP || !OPCODE_is_compare(cmp->Op()) ) 
    return RAISE(FALSE, "not a compare opcode");

  if ( cmp->Get_opnd(1)->Kind() != CK_CONST && loop->Exit_early())
    return RAISE(FALSE, "early exit without const comparison");
  // DO loop does not allow OPR_NE and OPR_EQ.
  const OPERATOR cond_opr = cmp->Opr();
  if (!(cond_opr == OPR_LE || cond_opr == OPR_GE ||
	cond_opr == OPR_LT || cond_opr == OPR_GT))
    return RAISE(FALSE, "== or != is not allowed.");

  // Fix 445056: Try to convert comp to loop invariants
  if (WOPT_Enable_Aggr_Invariant) {
    if (!loop->Invariant_cr(cmp->Opnd(0)) &&
	!loop->Invariant_cr(cmp->Opnd(1))) {
      htable->Convert_iload_to_loop_invariant(loop, cmp);
    }
  }

  CODEREP *variant0 = Find_variant(loop, cmp->Opnd(0));
  CODEREP *variant1 = Find_variant(loop, cmp->Opnd(1));

  // one side of the comparison must be invariant
  if ((variant0 == NULL && variant1 == NULL) ||
      (variant0 != NULL && variant1 != NULL))
    return RAISE(FALSE, "expecting variant cmp_op invariant | invariant cmp_op variant");
  
  // the variant expression must be a linear function of an IV
  CODEREP *iv;
  CODEREP *expr;
  if (variant0) {
    iv = variant0;
    expr = cmp->Opnd(0);
  } else {
    iv = variant1;
    expr = cmp->Opnd(1);
  }

  if (iv->Kind() != CK_VAR)
    return RAISE(FALSE, "induction variable is not CK_VAR");

  if (iv->Is_var_volatile())
    return RAISE(FALSE, "induction variable is volatile");

  if (!Is_linear_function_of(expr,iv,loop))
    return RAISE(FALSE, "exit test is not a linear function of induction var");

  loop->Set_iv(iv);

  // there must be a phi function defining iv
  Is_True(iv->Is_flag_set(CF_DEF_BY_PHI), ("BB_LOOP::Can_raise_to_doloop"));
  PHI_NODE *phi = iv->Defphi();

  // find init and incr statement of the iv
  STMTREP *init_stmt = phi->OPND(preheader_opnd_num)->Defstmt();
  STMTREP *incr_stmt = phi->OPND(loopback_opnd_num)->Defstmt();

  if (init_stmt == NULL)
    return RAISE(FALSE, "cannot find init stmt");

  if (init_stmt->Opr() != OPR_STID)
    return RAISE(FALSE, "init stmt is not STID");

  if (incr_stmt == NULL)
    return RAISE(FALSE, "cannot find incr stmt");

  if (incr_stmt->Opr() != OPR_STID)
    return RAISE(FALSE, "incr stmt is not STID");
     
  if (init_stmt != preheader->Last_stmtrep() &&
      !(repair && Compute_dependence(init_stmt, preheader, NULL) == NO_DEPENDENCE)) 
    return RAISE(FALSE, "init stmt is not the last statement in preheader.");
  
  BOOL sink_init_stmt = (repair && init_stmt != preheader->Last_stmtrep());

  STMTREP *last_stmt_in_loopback = loopback->Last_stmtrep();
  if (last_stmt_in_loopback != NULL && 
      last_stmt_in_loopback == loopback->Branch_stmtrep())
    last_stmt_in_loopback = last_stmt_in_loopback->Prev();
  
  if (incr_stmt != last_stmt_in_loopback && 
      !(repair && Compute_dependence(incr_stmt, loopback, NULL) == NO_DEPENDENCE))
    return RAISE(FALSE, "incr stmt is not the last statement in loopback.");

  BOOL sink_incr_stmt = (repair && incr_stmt != last_stmt_in_loopback);
  
  // incr_stmt must be of the form  iv2 = iv1 +- invar |  invar + iv1
  //  where iv1 is defined by a phi node at header
  if (incr_stmt->Opr() != OPR_STID)
    return RAISE(FALSE, "incr stmt is not an assignment stmt");

  CODEREP *iv2 = incr_stmt->Rhs();
  CODEREP *iv1 = phi->RESULT();
  if (iv2->Kind() == CK_OP && iv2->Opr() == OPR_ADD) {
    if (iv1 == iv2->Opnd(0)) {
      if (!loop->Invariant_cr(iv2->Opnd(1)))
	return RAISE(FALSE, "incr amount is not invariant");

    } else if (iv1 == iv2->Opnd(1)) {
      if (!loop->Invariant_cr(iv2->Opnd(0)))
	return RAISE(FALSE, "incr amount is not invariant");

    } else
      return RAISE(FALSE, "cannot find induction variable in incr stmt");

  } else if (iv2->Kind() == CK_OP && iv2->Opr() == OPR_SUB) {
    if (iv1 != iv2->Opnd(0) ||
	!loop->Invariant_cr(iv2->Opnd(1)))
      return RAISE(FALSE, "incr amount is not invariant");

  } else
    return RAISE(FALSE, "incr stmt is neither + or -");

  BOOL     trace = Get_Trace(TP_GLOBOPT, EMIT_DUMP_FLAG);

  // Perform all the repair here.
  //
  if (sink_init_stmt) {
    if (trace)
      fprintf(TFile, "Can_raise_do_loop:  sinking the init stmt\n");
    init_stmt->Bb()->Remove_stmtrep(init_stmt);
    preheader->Append_stmt_before_branch(init_stmt);
  }

  if (sink_incr_stmt) {
    if (trace)
      fprintf(TFile, "Can_raise_to_doloop:  sinking the incr stmt\n");
    incr_stmt->Bb()->Remove_stmtrep(incr_stmt);
    loopback->Append_stmt_before_branch(incr_stmt);
  }

#ifdef Is_True_On
  {
    STMTREP *last_stmt_in_loopback = loopback->Last_stmtrep();
    if (last_stmt_in_loopback != NULL &&
	last_stmt_in_loopback == loopback->Branch_stmtrep())
      last_stmt_in_loopback = last_stmt_in_loopback->Prev();
    Is_True(incr_stmt == last_stmt_in_loopback,
	    ("incr stmt not last stmt in loopback"));
  }
#endif

  return RAISE(TRUE, "OK");
}



// ====================================================================
//
// Fix_do_loop is called by EMITTER::Emit to ....
//
// ====================================================================


// Fix 582040:  manufacture an identity assignment.
// Fix 606530:  sink the init stmt is the last statement of
//   in the loop preheader.  If the init stmt cannot be sinked,
//   create an identity assignment to satsify do-loop raising 
//   requirement.
//
//  only used in PREOPT.
//
void Fix_do_loop(BB_LOOP *loop, CODEMAP *htable)
{
  AUX_ID aux;
  PHI_NODE *phi;

  if (!loop->Well_formed()) return;  // nothing can fix a invalid loop!
  if (loop->End() == NULL) return;   // only work for SCF loop

  if (loop->Iv() != NULL) {
    aux = loop->Iv()->Aux_id();
    phi = htable->Lookup_var_phi(loop->Header(), aux);
  } else {
    BOOL found = FALSE;
    WN *index_wn = loop->Index();
    ST *index_st = index_wn ? WN_st(index_wn) : NULL;
    WN_OFFSET index_ofs = index_wn ? WN_idname_offset(index_wn) : 0;

    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM (phi, phi_iter, Init(loop->Header()->Phi_list())) {
      if (!phi->Live()) continue;
      CODEREP *cr = phi->RESULT();
      if ( htable->Opt_stab()->St(cr->Aux_id()) == index_st &&
	   cr->Offset() == index_ofs ) {
	aux = cr->Aux_id();
	found = TRUE;
	break;
      }
    }
    if (!found) return;
  }

  FmtAssert(phi, ("Fix_do_loop: cannot locate phi."));

  CODEREP *init_value = phi->OPND(loop->Preheader_pred_num());
  if (init_value->Is_flag_set(CF_IS_ZERO_VERSION)) {
    htable->Fix_zero_version(phi, loop->Preheader_pred_num()); 
    init_value = phi->OPND(loop->Preheader_pred_num());
  }
  BOOL create_identity_asgn = FALSE;
  BOOL sink_init_stmt = FALSE;
  BB_NODE *preheader = loop->Preheader();
  STMTREP *init_stmt = init_value->Defstmt();

  if (init_stmt != NULL) {
    if (init_stmt->Bb() != preheader)
      create_identity_asgn = TRUE;
    else {
      if (init_stmt != preheader->Last_stmtrep()) {
	if (Compute_dependence(init_stmt, preheader, NULL) == NO_DEPENDENCE)
	  sink_init_stmt = TRUE;
	else
	  create_identity_asgn = TRUE;
      }
    }
  } else
    create_identity_asgn = TRUE;

  Is_True( create_identity_asgn ||
	   (sink_init_stmt && init_stmt->Bb() == preheader) ||
	   (init_stmt->Bb() == preheader && init_stmt == preheader->Last_stmtrep()),
	   ("Fix_do_loop:  unable to fix init stmt."));

  BOOL trace = Get_Trace(TP_GLOBOPT, EMIT_DUMP_FLAG);

  if (sink_init_stmt) {
    if (trace)
      fprintf(TFile, "Fix_do_loop:  sinking the init stmt\n");
    init_stmt->Bb()->Remove_stmtrep(init_stmt);
    preheader->Append_stmt_before_branch(init_stmt);
  }

  if (create_identity_asgn) {
    if (trace)
      fprintf(TFile, "Fix_do_loop:  creating identity asgn\n");
    MTYPE dtype = init_value->Dtyp();
    MTYPE dsctype = init_value->Dsctyp();
    CODEREP *init_cr = htable->Add_def(aux, -1, NULL, dtype, dsctype,
				       htable->Opt_stab()->Aux_stab_entry(aux)->St_ofst(),
				       MTYPE_To_TY(dtype),
				       init_value->Field_id(), TRUE);
    STMTREP *init_stmt = init_value->Create_cpstmt(init_cr, htable->Mem_pool());
    loop->Preheader()->Append_stmtrep(init_stmt);
    init_stmt->Set_bb(loop->Preheader());
    phi->Set_opnd(loop->Preheader_pred_num(), init_cr);
  }
}


// ====================================================================
