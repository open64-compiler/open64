/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007. PathScale, LLC.  All rights reserved.
 */
/*
 * Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */
/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

#include "defs.h"
#include "glob.h"
#include "config.h"
#include "wn.h"
#include "wn_util.h"

extern "C"{
#include "gspin-wgen-interface.h"
}
#include "wgen_misc.h"
#include "wgen_dst.h"
#include "wgen_expr.h"
#include "wgen_stmt.h"
#include "wgen_spin_symbol.h"
#include "omp_types.h"
#include "omp_directive.h"
#include "wgen_omp_directives.h"
#include "wgen_omp_check_stack.h"

#include "stdio.h"
#include "errors.h"
#include "const.h"
#include "erglob.h"

#include <stack>
// The structure of the stack is expected to be:
//   dtor1
//   dtor2
//   OPR_REGION node (r1) that should contain dtor1, dtor2 at the end.
//   dtor_a
//   dtor_b
//   OPR_REGION node (r2) that should contain dtor_a, dtor_b << STACK TOP >>
//
// i.e. r1 should contain the corresponding ctor calls for 1 and 2, and r2
// should contain the ctor calls for a and b.
// OPR_BLOCK as marker is fine, but changing it to be consistent with the
// following stack format.
std::stack<WN *> dtor_call_stack;
// local_node_stack format:
//   pragma1
//   pragma2
//   OPR_REGION
//   pragma_a
//   pragma_b
//   OPR_REGION << STACK TOP >>
// We need the marker node to be the region since we need to localize
// in the region pragmas also.
// Used for localizing variables in a parallel region.
std::stack<WN *> local_node_stack;

// Stack of assignment-operator calls for lastlocal (lastprivate) variables.
std::stack<WN *> lastlocal_node_stack;

// vector for storing DO-loop side-effects, to be emitted before the loop.
std::vector<WN *> doloop_side_effects;

BOOL Trace_Omp = FALSE;

// Put in per-file OpenMP specific initializations here.
void WGEN_Omp_Init (void)
{
  WGEN_CS_Init ();
  if (getenv ("CFE_OMP_DEBUG") && !strcmp (getenv ("CFE_OMP_DEBUG"), "1"))
    Trace_Omp = TRUE;
}

// Returns the created region
WN *  WGEN_region(REGION_KIND kind)
{
  WN *body,*pragmas,*exits,*region;

  body    = WN_CreateBlock ();
  pragmas = WN_CreateBlock ();
  exits   = WN_CreateBlock ();
  region  = WN_CreateRegion (kind,
			     body,
			     pragmas,
			     exits,
			     -1, 
			     0);
  WGEN_Stmt_Append (region, Get_Srcpos());
  WGEN_Stmt_Push (body, wgen_stmk_scope, Get_Srcpos());
  WGEN_Stmt_Push (pragmas, wgen_stmk_region_pragmas, Get_Srcpos());
  
  return region;
}

/////////////////////////
//////  error report routine:

void WGEN_omp_error(CHECK_STMT* cs, bool chkflag, char * msg)
{
  char dirname[100];
  if(chkflag==false)
    return;
  switch(cs->kind)
  {
    case wgen_omp_parallel: sprintf(dirname,"#PRAGMA OMP PARALLEL");
    					  break;	
    case wgen_omp_for: sprintf(dirname,"#PRAGMA OMP FOR");
    			      break;	 	
    case wgen_omp_single: sprintf(dirname,"#PRAGMA OMP SINGLE");
						 break;					
    case wgen_omp_sections: sprintf(dirname,"#PRAGMA OMP SECTIONS");
						 break;	
    case wgen_omp_parallel_sections: sprintf(dirname,"#1PRAGMA OMP PARRALLEL SECTIONS");
						 break;	
    case wgen_omp_parallel_for: sprintf(dirname,"#PRAGMA OMP PARRALLEL FOR");
						break;
    default: sprintf(dirname,"OTHER DIRECTIVES ");
  }

  if (msg)
    ErrMsg (EC_Bad_Omp, msg);
}
/*********** Data clause check***********/
void WGEN_check_private(WN *wn_p, bool chkflag)
{ 
  if(ST_is_const_var(* WN_st(wn_p)))
          	{ fprintf(stderr ,"A variable specified in a private\
		clause must not have a const-qualified type \
          	    unless it has a class type with a mutable member.\n");
          	    chkflag=true;
          	}
  if(WN_st(wn_p)->storage_class==SCLASS_FORMAL_REF)
          	{
          	  fprintf(stderr,"A variable specified in a private\
		  clause must not have an incomplete type \
          	 	   or a reference type. \n");
          	 	   chkflag=true;
          	}

}
void WGEN_check_firstprivate(WN *wn_fp, bool chkflag)
{
    if(WN_st(wn_fp)->storage_class==SCLASS_FORMAL_REF)
          	{
          	  fprintf(stderr,"A variable specified in a first private\
		  clause must not have an incomplete type \
          	 	   or a reference type. \n");
          	 	   chkflag=true;
          	}
}
void WGEN_check_lastprivate(WN *wn_lp, bool chkflag)
{ 
  if(ST_is_const_var(* WN_st(wn_lp)))
    { 
      fprintf(stderr," A variable specified in a lastprivate clause must\
      not have a const-qualified type \
          	   unless it has a class type with a mutable member.\n");
      chkflag=true;
    }
  if(WN_st(wn_lp)->storage_class==SCLASS_FORMAL_REF)
    {
      fprintf(stderr,"A variable specified in a lastprivate clause must\
      not have an incomplete type \
          	 or a reference type. \n");
      chkflag=true;
    }

}
void WGEN_check_reduction(WN* wn_r, bool chkflag)
{
  if(ST_is_const_var(* WN_st(wn_r)))
    { 
      fprintf(stderr," A variable that is specified in the reduction\
      clause must not be const-qualified\n");
      chkflag=true;
    }
}

// Given a function decl, this function returns TRUE if it is a constructor
// AND not a copy constructor AND ( it either takes no arguments OR all
// arguments are optional ). Returns FALSE otherwise.
static BOOL WGEN_is_default_constructor (gs_t fndecl)
{
  Is_True (gs_tree_code (fndecl) == GS_FUNCTION_DECL, ("Invalid function decl"));
  if (!gs_decl_constructor_p (fndecl)) return FALSE;
  if (gs_decl_copy_constructor_p (fndecl)) return FALSE;

  gs_t args = gs_function_first_user_parmtype (fndecl);

  // check if all arguments have default values
  for (; args; args = gs_tree_chain (args))
    if (args != gs_void_list_node() && !gs_tree_purpose (args)) return FALSE;
  return TRUE;
}

// Calling default constructor implies also a call to destructor at the
// end of the parallel region.
// Call default contructor and destructor for variable VAR.
// Return the 2 calls in C and D.
static BOOL WGEN_maybe_call_default_ctor (gs_t var, WN ** c, WN ** d)
{
  gs_t type = gs_tree_type (var);
  if (gs_tree_code (type) != GS_RECORD_TYPE || !gs_classtype_non_pod_p (type))
    return FALSE;

  ST * obj = Get_ST (var);
  FmtAssert (gs_type_has_default_constructor (type),
             ("private clause on non-pod object %s needs default constructor",
              ST_name (obj)));
  gs_t found_ctor = NULL;
  gs_t found_dtor = NULL;
  for (gs_t methods = gs_type_methods (type);
       methods;
       methods = gs_tree_chain (methods))
  {
    if (gs_decl_complete_constructor_p (methods) &&
        WGEN_is_default_constructor (methods))
    {
      FmtAssert (!found_ctor,
                 ("Multiple default constructor candidates for %s",
                  ST_name (obj)));
      found_ctor = methods;
    }
    if (gs_decl_complete_destructor_p (methods))
    {
      FmtAssert (!found_dtor, ("Multiple destructor candidates for %s",
                               ST_name (obj)));
      found_dtor = methods;
    }
  }

  FmtAssert (found_ctor && found_dtor,
             ("No default ctor/dtor found for non-pod object %s",
              ST_name (obj)));
  // Call the default constructor
  ST * ctor_st = Get_ST (found_ctor);
  WN * call_ctor = WN_Call (MTYPE_V, MTYPE_V, 1, ST_st_idx (ctor_st));
  WN * lda_obj = WN_Lda (Pointer_Mtype, 0, obj, 0);
  WN_actual (call_ctor, 0) = WN_CreateParm (WN_rtype (lda_obj),
                                       lda_obj,
                                       WN_ty (lda_obj),
                                       WN_PARM_BY_VALUE);
  WN_Set_Call_Default_Flags (call_ctor);
  *c = call_ctor;

  // Call the destructor
  ST * dtor_st = Get_ST (found_dtor);
  WN * call_dtor = WN_Call (MTYPE_V, MTYPE_V, 1, ST_st_idx (dtor_st));
  WN_actual (call_dtor, 0) = WN_COPY_Tree (WN_kid0 (call_ctor));
  WN_Set_Call_Default_Flags (call_dtor);
  *d = call_dtor;

  return TRUE;
}

// Call copy contructor and destructor for variable VAR.
// Return the 2 calls in C and D.
static BOOL WGEN_maybe_call_copy_ctor (gs_t var, WN ** c, WN ** d)
{
  gs_t type = gs_tree_type (var);
  if (gs_tree_code (type) != GS_RECORD_TYPE || !gs_classtype_non_pod_p (type))
    return FALSE;

  ST * obj = Get_ST (var);
  gs_t found_ctor = NULL;
  gs_t found_dtor = NULL;
  BOOL no_implicit_copy_ctor = !gs_type_has_implicit_copy_constructor(type);
  for (gs_t methods = gs_type_methods (type);
       methods;
       methods = gs_tree_chain (methods))
  {
    if (no_implicit_copy_ctor &&
        gs_decl_complete_constructor_p (methods) &&
        gs_decl_copy_constructor_p (methods))
    {
      FmtAssert (!found_ctor,
                 ("Multiple copy constructor candidates for %s",
                  ST_name (obj)));
      found_ctor = methods;
    }
    if (gs_decl_complete_destructor_p (methods))
    {
      FmtAssert (!found_dtor, ("Multiple destructor candidates for %s",
                               ST_name (obj)));
      found_dtor = methods;
    }
  }
  if (!found_ctor || !found_dtor)
    DevWarn ("No copy-ctor/dtor found for non-pod object %s", ST_name (obj));
  // Call the copy constructor
  // Generate "C c1 = <empty>", localizer will later change it
  // to give "C __mplocalfe_c1 = c1".
  if (found_ctor)
  {
    ST * ctor_st = Get_ST (found_ctor);
    WN * call_ctor = WN_Call (MTYPE_V, MTYPE_V, 2, ST_st_idx (ctor_st));
    WN * lda_obj = WN_Lda (Pointer_Mtype, 0, obj, 0);
    WN_actual (call_ctor, 0) = WN_CreateParm (WN_rtype (lda_obj),
                                         lda_obj,
                                         WN_ty (lda_obj),
                                         WN_PARM_BY_VALUE);
    WN_actual (call_ctor, 1) = NULL;
    WN_Set_Call_Default_Flags (call_ctor);
    *c = call_ctor;
  }

  if (found_dtor)
  {  // Call the destructor
    ST * dtor_st = Get_ST (found_dtor);
    WN * call_dtor = WN_Call (MTYPE_V, MTYPE_V, 1, ST_st_idx (dtor_st));
    WN * lda_obj = WN_Lda (Pointer_Mtype, 0, obj, 0);
    WN_actual (call_dtor, 0) = WN_CreateParm (WN_rtype (lda_obj),
                                              lda_obj,
                                              WN_ty (lda_obj),
                                              WN_PARM_BY_VALUE);
    WN_Set_Call_Default_Flags (call_dtor);
    *d = call_dtor;
  }

  return TRUE;
}

static BOOL WGEN_maybe_call_assignment_opr (gs_t var, WN ** c, WN ** d, WN ** a)
{
  gs_t type = gs_tree_type (var);
  if (gs_tree_code (type) != GS_RECORD_TYPE || !gs_classtype_non_pod_p (type))
    return FALSE;

  ST * obj = Get_ST (var);
  gs_t found_ctor = NULL;
  gs_t found_dtor = NULL;
  gs_t found_asn_opr = NULL;

  for (gs_t methods = gs_type_methods (type);
       methods;
       methods = gs_tree_chain (methods))
  {
    if (gs_decl_complete_constructor_p (methods) &&
        WGEN_is_default_constructor (methods))
    {
      FmtAssert (!found_ctor,
                 ("Multiple copy constructor candidates for %s",
                  ST_name (obj)));
      found_ctor = methods;
    }
    if (gs_decl_assignment_operator_p (methods))
    {
      FmtAssert (!found_asn_opr, ("Multiple assignment operators for %s",
                                  ST_name (obj)));
      found_asn_opr = methods;
    }
    if (gs_decl_complete_destructor_p (methods))
    {
      FmtAssert (!found_dtor, ("Multiple destructor candidates for %s",
                               ST_name (obj)));
      found_dtor = methods;
    }
  }
  if (!found_ctor || !found_dtor || !found_asn_opr)
    DevWarn ("No copy-ctor/dtor found for non-pod object %s", ST_name (obj));

  // Call the default constructor
  if (found_ctor)
  {
    ST * ctor_st = Get_ST (found_ctor);
    WN * call_ctor = WN_Call (MTYPE_V, MTYPE_V, 1, ST_st_idx (ctor_st));
    WN * lda_obj = WN_Lda (Pointer_Mtype, 0, obj, 0);
    WN_actual (call_ctor, 0) = WN_CreateParm (WN_rtype (lda_obj),
                                         lda_obj,
                                         WN_ty (lda_obj),
                                         WN_PARM_BY_VALUE);
    WN_Set_Call_Default_Flags (call_ctor);
    *c = call_ctor;
  }
  // Call the assignment operator
  // Generate "C <empty> = c1", localizer will later change it
  // to give "C c1 = __mplocalfe_c1".
  if (found_asn_opr)
  {
    ST * asn_opr_st = Get_ST (found_asn_opr);
    WN * asn_opr_call = WN_Call (MTYPE_V, MTYPE_V, 2, ST_st_idx (asn_opr_st));
    WN * lda_obj = WN_Lda (Pointer_Mtype, 0, obj, 0);
    WN_actual (asn_opr_call, 0) = NULL;
    WN_actual (asn_opr_call, 1) = WN_CreateParm (WN_rtype (lda_obj),
                                         lda_obj,
                                         WN_ty (lda_obj),
                                         WN_PARM_BY_VALUE);
    WN_Set_Call_Default_Flags (asn_opr_call);
    *a = asn_opr_call;
  }

  if (found_dtor)
  {  // Call the destructor
    ST * dtor_st = Get_ST (found_dtor);
    WN * call_dtor = WN_Call (MTYPE_V, MTYPE_V, 1, ST_st_idx (dtor_st));
    WN * lda_obj = WN_Lda (Pointer_Mtype, 0, obj, 0);
    WN_actual (call_dtor, 0) = WN_CreateParm (WN_rtype (lda_obj),
                                              lda_obj,
                                              WN_ty (lda_obj),
                                              WN_PARM_BY_VALUE);
    WN_Set_Call_Default_Flags (call_dtor);
    *d = call_dtor;
  }

  return TRUE;
}

// Call any destructors as required due to introducing constructors for
// MP clauses.
static void WGEN_maybe_call_dtors (WN * wn)
{
  Is_True (WN_operator (wn) == OPR_BLOCK, ("BLOCK node expected"));
  Is_True (dtor_call_stack.empty() ||
           WN_operator (dtor_call_stack.top()) == OPR_REGION,
           ("REGION node expected"));

  if (dtor_call_stack.empty() || WN_region_body (dtor_call_stack.top()) != wn)
    return;

  dtor_call_stack.pop(); // pop off the top REGION node
  while (!dtor_call_stack.empty() && 
         WN_operator (dtor_call_stack.top()) == OPR_CALL)
  {
    WN_INSERT_BlockLast (wn, dtor_call_stack.top());
    dtor_call_stack.pop ();
  }
  Is_True (dtor_call_stack.empty() ||
           WN_operator (dtor_call_stack.top()) == OPR_REGION,
           ("REGION node expected"));
}

// Walk block and replace old_st with new_st
static void WGEN_localize_var (WN * block, ST * old_st, ST * new_st)
{
  if (WN_has_sym (block) && WN_st (block) == old_st)
    WN_st_idx (block) = ST_st_idx (new_st);

  OPERATOR opr = WN_operator (block);

  if (opr == OPR_BLOCK)
  {
    WN * node = WN_first (block);
    while (node)
    {
      WGEN_localize_var (node, old_st, new_st);
      node = WN_next (node);
    }
  }
  else
  {
    // Special case handling for firstprivate, lastprivate..:
    // e.g. the copy ctor call takes 2 arguments, both would be the same
    // object and we need to localize one here, but we won't know which
    // one not to localize.
    //
    // We fix it by generating only the argument which should be localized.
    // The other argument would be a copy of the ORIGINAL former argument,
    // and must be introduced here.
    if (WN_operator (block) == OPR_CALL && WN_kid_count (block) == 2 &&
        (WN_kid0 (block) == NULL || WN_kid1 (block) == NULL))
    {
      if (WN_kid0 (block))
      {
        WN * lda = WN_kid0 (WN_kid0 (block));
        if (WN_operator(lda) == OPR_LDA /* we generate only LDA */
            && WN_st(lda) == old_st)
        {
          WN_kid1 (block) = WN_COPY_Tree (WN_kid0 (block));
          WGEN_localize_var (WN_kid0 (block), old_st, new_st);
        }
      }
      else
      {
        WN * lda = WN_kid0 (WN_kid1 (block));
        if (WN_operator(lda) == OPR_LDA /* we generate only LDA */
            && WN_st(lda) == old_st)
        {
          WN_kid0 (block) = WN_COPY_Tree (WN_kid1 (block));
          WGEN_localize_var (WN_kid1 (block), old_st, new_st);
        }
      }
    }
    else
    {
      for (int i=0; i<WN_kid_count (block); i++)
        WGEN_localize_var (WN_kid (block, i), old_st, new_st);
    }
  }
}

// If required localize vars in MP region that contains WN.
static void WGEN_maybe_localize_vars (WN * wn)
{
  Is_True (WN_operator (wn) == OPR_BLOCK, ("BLOCK node expected"));
  Is_True (local_node_stack.empty() ||
           WN_operator (local_node_stack.top()) == OPR_REGION,
           ("REGION node expected"));

  if (local_node_stack.empty() || WN_region_body (local_node_stack.top()) != wn)
    return;

  wn = local_node_stack.top(); // now the parent REGION node
  local_node_stack.pop(); // pop off the top REGION node
  while (!local_node_stack.empty() &&
         WN_operator (local_node_stack.top()) == OPR_PRAGMA)
  {
    WN * pragma = local_node_stack.top();
    ST * st = WN_st (pragma);
    // create a local copy
    FmtAssert (ST_class (st) != CLASS_PREG, ("NYI"));
    TY_IDX ty = ST_type (st);
    char * localname = (char *) alloca (strlen (ST_name (st)) +
                                        strlen ("__mplocalfe_") + 1);

    sprintf (localname, "__mplocalfe_%s", ST_name (st));
    ST * new_st = New_ST (CURRENT_SYMTAB);
    ST_Init (new_st, Save_Str (localname), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, ty);
    if (ST_addr_saved(st))
      Set_ST_addr_saved(new_st);
    if (ST_addr_passed(st))
      Set_ST_addr_passed(new_st);

    WGEN_localize_var (wn, st, new_st);
    local_node_stack.pop();
  }
  Is_True (local_node_stack.empty() ||
           WN_operator (local_node_stack.top()) == OPR_REGION,
           ("REGION node expected"));
}

// Invoke default constructor/copy constructor/assignment opr according
// to pragma_type.
// Generate calls to corresponding destructors.
// Returns true if new calls are generated to handle non-pods.
static BOOL WGEN_handle_non_pods (gs_t var, WN * block, gs_omp_clause_code_t p)
{
  WN * c = NULL, * d = NULL, * a = NULL;
  // Call constructor/destructor for non-pod class variables
  BOOL constructed = FALSE;
  switch (p)
  {
    case GS_OMP_CLAUSE_PRIVATE:
      constructed = WGEN_maybe_call_default_ctor (var, &c, &d);
      break;
    case GS_OMP_CLAUSE_FIRSTPRIVATE:
      constructed = WGEN_maybe_call_copy_ctor (var, &c, &d);
      break;
    case GS_OMP_CLAUSE_LASTPRIVATE:
      constructed = WGEN_maybe_call_assignment_opr (var, &c, &d, &a);
      break;
    default:
      break;
  }

  if (constructed)
  {
    ST * st = Get_ST (var);
    // Needed by MP-lowerer
    Set_TY_is_non_pod (ST_type (st));
    // Insert call to constructor:
    // Insert BlockFirst instead of BlockLast. This is because when we
    // are inserting into the current region, we are starting to create
    // the region, so either would work. For the sections clause, if
    // we attempt to insert in the enclosing parallel region, then we
    // have already started creating the body of the parallel region,
    // so insert at the First.
    if (c)
      WN_INSERT_BlockFirst (block, c);
    // Push destructor call to stack, for emission at the end
    // of the region.
    if (d)
      dtor_call_stack.push (d);
    if (a)
      lastlocal_node_stack.push (a);
    return TRUE;
  }
  return FALSE;
}

static OPERATOR get_reduction_code (gs_t clause)
{
  OPERATOR opr = OPERATOR_UNKNOWN;
  Is_True (gs_omp_clause_code(clause) == GS_OMP_CLAUSE_REDUCTION,
           ("get_reduction_code: invalid clause type"));

  switch (gs_omp_clause_reduction_code(clause))
  {
    case GS_BIT_AND_EXPR:  //'&'
      opr = OPR_BAND;
      break;
    case GS_BIT_IOR_EXPR:  //"|"
      opr = OPR_BIOR;
      break;
    case GS_BIT_XOR_EXPR:  //'^'
      opr = OPR_BXOR;
      break;
    case GS_PLUS_EXPR:   //'+'
      opr = OPR_ADD;
      break;
    case GS_MULT_EXPR:   //'*'
      opr = OPR_MPY;
      break;
    case GS_MINUS_EXPR:   //'-'
      opr = OPR_SUB;
      break;
    case GS_TRUTH_AND_EXPR:  //ANDAND
    case GS_TRUTH_ANDIF_EXPR:
      opr = OPR_CAND;
      break;
    case GS_TRUTH_OR_EXPR:  //OROR
    case GS_TRUTH_ORIF_EXPR:
      opr = OPR_CIOR;
      break;
  }

  return opr;
}

// A REGION need not be always passed. If provided, it could be an
// EH region, or an MP parallel region.
static void WGEN_process_omp_clause (gs_t clauses, WN * region = 0)
{
  ST * st = NULL;
  WN * wn = NULL;
  BOOL non_pod = FALSE;
  switch (gs_omp_clause_code(clauses))
  {
    case GS_OMP_CLAUSE_PRIVATE:
      {
        gs_t var = gs_omp_clause_decl(clauses);
        WGEN_Set_Cflag(clause_private);   //set clause flag for check
        st = Get_ST(var);
        wn = WN_CreatePragma(WN_PRAGMA_LOCAL, st, 0, 0);
        if (lang_cplus && region && (non_pod = WGEN_handle_non_pods (var,
                                  WN_region_body(region),
                                  GS_OMP_CLAUSE_PRIVATE)))
          local_node_stack.push (wn);
      }
      break;

    case GS_OMP_CLAUSE_IF:
      wn = WN_CreateXpragma(WN_PRAGMA_IF, (ST_IDX) NULL, 1);
      WN_kid0(wn) = WGEN_Expand_Expr (gs_omp_clause_if_expr(clauses));
      break;

    case GS_OMP_CLAUSE_FIRSTPRIVATE:
      {
        gs_t var = gs_omp_clause_decl(clauses);
        WGEN_Set_Cflag(clause_firstprivate); 
        st = Get_ST(var);
        wn = WN_CreatePragma(WN_PRAGMA_FIRSTPRIVATE, st, 0, 0);
        if (lang_cplus && region && (non_pod = WGEN_handle_non_pods (var,
                                  WN_region_body(region),
                                  GS_OMP_CLAUSE_FIRSTPRIVATE)))
          local_node_stack.push (wn);
      }
      break;

    case GS_OMP_CLAUSE_LASTPRIVATE:
      {
        gs_t var = gs_omp_clause_decl(clauses);
        WGEN_Set_Cflag(clause_lastprivate); 
        st = Get_ST(var);
        wn = WN_CreatePragma(WN_PRAGMA_LASTLOCAL, st, 0, 0);
        if (lang_cplus && region && (non_pod = WGEN_handle_non_pods (var,
                                  WN_region_body(region),
                                  GS_OMP_CLAUSE_LASTPRIVATE)))
          local_node_stack.push (wn);
      }
      break;

    case GS_OMP_CLAUSE_SHARED:
      WGEN_Set_Cflag(clause_shared); 
      st = Get_ST(gs_omp_clause_decl(clauses));
      wn = WN_CreatePragma(WN_PRAGMA_SHARED, st, 0, 0);
      break;

    case GS_OMP_CLAUSE_NUM_THREADS:
      wn = WN_CreateXpragma(WN_PRAGMA_NUMTHREADS, (ST_IDX) NULL, 1);
      WN_kid0(wn) = WGEN_Expand_Expr (gs_omp_clause_num_threads_expr(clauses));
      break;

    case GS_OMP_CLAUSE_DEFAULT:
      {
        gs_omp_clause_default_kind_t default_value =
                                    gs_omp_clause_default_kind (clauses);
        WN_PRAGMA_DEFAULT_KIND kind = WN_PRAGMA_DEFAULT_UNKNOWN;

        switch (default_value)
        {
          case GS_OMP_CLAUSE_DEFAULT_SHARED:
                 kind = WN_PRAGMA_DEFAULT_SHARED; break;
          case GS_OMP_CLAUSE_DEFAULT_NONE:
                 kind = WN_PRAGMA_DEFAULT_NONE; break;
          case GS_OMP_CLAUSE_DEFAULT_PRIVATE:
                 kind = WN_PRAGMA_DEFAULT_PRIVATE; break;
        }
        wn = WN_CreatePragma(WN_PRAGMA_DEFAULT, 
                             (ST_IDX) NULL, 
                             kind,
                             0);
      }
      break;

    case GS_OMP_CLAUSE_COPYIN:
      WGEN_Set_Cflag(clause_copyin); 
      st = Get_ST(gs_omp_clause_decl(clauses));
      wn = WN_CreatePragma(WN_PRAGMA_COPYIN, st, 0, 0);
      break;

    case GS_OMP_CLAUSE_COPYPRIVATE:
      WGEN_Set_Cflag(clause_copyprivate); 
      st = Get_ST(gs_omp_clause_decl(clauses));
      wn = WN_CreatePragma(WN_PRAGMA_COPYPRIVATE, st, 0, 0);
      break;

    case GS_OMP_CLAUSE_REDUCTION:
      {
        WGEN_Set_Cflag(clause_reduction);
        OPERATOR opr = get_reduction_code(clauses);
        st = Get_ST(gs_omp_clause_decl(clauses));
        wn = WN_CreatePragma(WN_PRAGMA_REDUCTION, st, 0, opr);
      }
      break;

    case GS_OMP_CLAUSE_NOWAIT:
      wn = WN_CreatePragma(WN_PRAGMA_NOWAIT, (ST_IDX)NULL, 0, 0);
      break;

    case GS_OMP_CLAUSE_ORDERED:
      WGEN_Set_Cflag(clause_ordered); 
      wn = WN_CreatePragma(WN_PRAGMA_ORDERED, (ST_IDX)NULL, 0, 0);
      break;

    case GS_OMP_CLAUSE_SCHEDULE:
    {
      WN_PRAGMA_SCHEDTYPE_KIND schedtype_kind;
      switch(gs_omp_clause_schedule_kind(clauses)) {
        case GS_OMP_CLAUSE_SCHEDULE_STATIC:
          schedtype_kind = WN_PRAGMA_SCHEDTYPE_SIMPLE;
          break;
        case GS_OMP_CLAUSE_SCHEDULE_DYNAMIC:
          schedtype_kind = WN_PRAGMA_SCHEDTYPE_DYNAMIC;
          break;
        case GS_OMP_CLAUSE_SCHEDULE_GUIDED:
          schedtype_kind = WN_PRAGMA_SCHEDTYPE_GSS;
          break;
        case GS_OMP_CLAUSE_SCHEDULE_RUNTIME:
          schedtype_kind = WN_PRAGMA_SCHEDTYPE_RUNTIME;
          break;
        default:
          Fail_FmtAssertion ("Invalid OpenMP schedule kind");
      }

      wn = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                           (ST_IDX)NULL, 
                           schedtype_kind,
                           0);
      WN_set_pragma_omp(wn);
      WGEN_Stmt_Append (wn, Get_Srcpos());
      // statement already appended
      wn = NULL;

      gs_t chunk_size = gs_omp_clause_schedule_chunk_expr(clauses);
      if (chunk_size)
      {
        wn = WN_CreateXpragma(WN_PRAGMA_CHUNKSIZE,
                              (ST_IDX) NULL,
                              1);
        WN * chunk_wn = WGEN_Expand_Expr(chunk_size);
        if (lang_cplus && region && WN_has_side_effects(chunk_wn))
        {
          TY_IDX ty_idx = MTYPE_TO_TY_array[WN_rtype(chunk_wn)];
          ST * st = Gen_Temp_Symbol (ty_idx, "__mp_chunk_size");
          // Mark as local in surrounding parallel region.
          WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st, TRUE);
          WN * stid = WN_Stid (TY_mtype(ty_idx), 0, st, ty_idx, chunk_wn);
          // Use the following process to make sure if the expression
          // had a function call, any EH region enclosing that call would
          // be terminated. Then extract this statement region out.
          WGEN_Stmt_Append(stid, Get_Srcpos());
          WN * top = WGEN_Stmt_Top();
          stid = WN_EXTRACT_FromBlock(top, WN_last(top));
          WN_INSERT_BlockFirst (WN_region_body(region), stid);
          chunk_wn = WN_Ldid (TY_mtype(ty_idx), 0, st, ty_idx);
        }
        WN_kid0(wn) = chunk_wn;
        WN_set_pragma_omp(wn);
        WGEN_Stmt_Append (wn, Get_Srcpos());
        // statement already appended
        wn = NULL;
      }
    }
    break;

    default:
      DevWarn ("WGEN_process_omp_clause: unhandled OpenMP clause");
  }

  if (wn)
  {
    WN_set_pragma_omp(wn);  
    // If an MP region is provided, insert a "local" pragma on non-pod
    // objects in the pragmas of that region.
    if (region && non_pod && WN_region_kind(region) == REGION_KIND_MP)
      WN_INSERT_BlockLast(WN_region_pragmas(region), wn);
    else
      WGEN_Stmt_Append (wn, Get_Srcpos());
  }
}

void WGEN_expand_start_parallel (gs_t stmt)
{
  /* create a region on current block */
       
  WN * region = WGEN_region(REGION_KIND_MP);

  WN *wn;

  wn = WN_CreatePragma(WN_PRAGMA_PARALLEL_BEGIN, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0);   
  WN_set_pragma_omp(wn);
  WGEN_Stmt_Append (wn, Get_Srcpos());
       
  ///// omp check stack action ///////
  SRCPOS srcpos = Get_Srcpos();
  WGEN_CS_push(wgen_omp_parallel, SRCPOS_linenum(srcpos),
               SRCPOS_filenum(srcpos));
  WGEN_Set_Prag(WGEN_Stmt_Top());
  WGEN_Set_Region (region);


  /////required?///////
  Set_PU_has_mp (Get_Current_PU ());
  Set_FILE_INFO_has_mp (File_info);
  Set_PU_uplevel (Get_Current_PU ());

  gs_t clauses = gs_omp_parallel_clauses (stmt);

  for (; clauses; clauses = gs_omp_clause_chain(clauses))
    WGEN_process_omp_clause(clauses, region);

  WGEN_Stmt_Pop (wgen_stmk_region_pragmas);

  if (lang_cplus)
  {
    if (!dtor_call_stack.empty() &&
        WN_operator (dtor_call_stack.top()) == OPR_CALL)
      dtor_call_stack.push (region);

    if (!local_node_stack.empty() &&
        WN_operator (local_node_stack.top()) == OPR_PRAGMA)
      local_node_stack.push (region);
  }
}

void WGEN_expand_end_parallel ()
{
    if (lang_cplus)
    {
      WN * wn = WGEN_Stmt_Top ();
      WGEN_maybe_call_dtors (wn);
      WGEN_maybe_localize_vars (wn);
      WGEN_maybe_do_eh_cleanups ();
    }

    WGEN_Stmt_Pop (wgen_stmk_scope);
    WGEN_CS_pop (wgen_omp_parallel);
};



static WN * Setup_MP_Enclosing_Region (void);

void WGEN_expand_start_for (gs_t stmt)
{
       FmtAssert (lastlocal_node_stack.empty(),
                  ("Lastlocal non-POD variables from previous region?"));
       // For documentation, see WGEN_expand_start_sections() .
       WN * enclosing_region = NULL;
       if (lang_cplus)
       {
         CHECK_STMT * enclosing = WGEN_CS_top();
         if (enclosing)
         {
           enclosing_region = enclosing->region;

           FmtAssert (enclosing_region,
                      ("NULL enclosing region of SECTIONS clause"));
           // sanity check, must be a parallel region.
           WN * first_pragma = WN_first(WN_region_pragmas(enclosing_region));
           FmtAssert (WN_region_kind(enclosing_region) == REGION_KIND_MP &&
                      first_pragma &&
                      WN_pragma(first_pragma) == WN_PRAGMA_PARALLEL_BEGIN,
                      ("Unexpected parent region of SECTIONS clause"));
         }
         else
           enclosing_region = Setup_MP_Enclosing_Region();
       }

       /* create a region on current block */
       
       WN * region = WGEN_region(REGION_KIND_MP);

       WN *wn;

       wn = WN_CreatePragma(WN_PRAGMA_PDO_BEGIN, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0);
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());
       /////////  omp ///////////////////////////
       SRCPOS srcpos = Get_Srcpos();
       WGEN_CS_push(wgen_omp_for,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       WGEN_Set_Prag(WGEN_Stmt_Top());
       WGEN_Set_Region (region);

 
       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());
      
  gs_t clauses = gs_omp_for_clauses (stmt);

  if (lang_cplus)
    region = enclosing_region;
  for (; clauses; clauses = gs_omp_clause_chain(clauses))
    WGEN_process_omp_clause(clauses, region);

  WGEN_Stmt_Pop (wgen_stmk_region_pragmas);

  if (lang_cplus)
  {
    if (!dtor_call_stack.empty() &&
        WN_operator (dtor_call_stack.top()) == OPR_CALL)
      dtor_call_stack.push (region);

    if (!local_node_stack.empty() &&
        WN_operator (local_node_stack.top()) == OPR_PRAGMA)
      local_node_stack.push (region);
  }
}

void WGEN_expand_end_for ( )
{
//    WGEN_check_for (wn);
    WGEN_Stmt_Pop (wgen_stmk_scope);

    WN *wn = WGEN_Stmt_Top ();
    WN *anchor = WN_first(wn);

    // Output DO loop side-effects
    for (INT i=0; i<doloop_side_effects.size(); i++)
      WN_INSERT_BlockBefore(wn, anchor, doloop_side_effects[i]);
    doloop_side_effects.clear ();

    // Note the FOR is still in stack. If there is no enclosing
    // (parallel) region, then process and pop the enclosing EH
    // region.
    if (lang_cplus && !WGEN_CS_enclose())
    {
      WGEN_maybe_call_dtors (wn);
      WGEN_maybe_localize_vars (wn);
      WGEN_maybe_do_eh_cleanups ();
      // Pop enclosing EH region body.
      WGEN_Stmt_Pop (wgen_stmk_region_body);
    }
    WGEN_CS_pop(wgen_omp_for);
}


// Called from WGEN_expand_start_sections and WGEN_expand_start_for,
// to enclose the SECTIONS/DO region inside an EH region. The properties
// of the EH region below may need to be updated later. Any relevant
// pragmas are maintained within the respective MP regions, but any
// constructor and destructor calls will be inside this EH region, before
// and after the MP region, respectively. This is because, by definition,
// the DO and the SECTIONS regions cannot contain extra code such as these
// calls inside their body.
static WN *
Setup_MP_Enclosing_Region (void)
{
    if (!lang_cplus) return NULL;

    WN * region_body = WN_CreateBlock();

    INITV_IDX iv;
    LABEL_IDX pad = 0;

    iv = New_INITV();
    INITV_Set_ZERO (Initv_Table[iv], MTYPE_U4, 1);

    INITV_IDX initv_label = New_INITV();
    INITV_Set_ZERO (Initv_Table[initv_label], MTYPE_U4, 1);
    INITV_IDX blk = New_INITV();
    INITV_Init_Block (blk, initv_label);

    Set_INITV_next (initv_label, iv);

    TY_IDX ty = MTYPE_TO_TY_array[MTYPE_U4];
    ST * ereg = Gen_Temp_Named_Symbol (ty, "dummy1", CLASS_VAR,
                                SCLASS_EH_REGION_SUPP);
    Set_ST_is_initialized (*ereg);
    Set_ST_is_not_used (*ereg);
    INITO_IDX ereg_supp = New_INITO (ST_st_idx(ereg), blk);

    WN * region = WN_CreateRegion (REGION_KIND_EH, region_body,
      WN_CreateBlock(), WN_CreateBlock(), New_Region_Id(), ereg_supp);

    WGEN_Stmt_Append (region, Get_Srcpos());
    Set_PU_has_region (Get_Current_PU());
    Set_PU_has_exc_scopes (Get_Current_PU());

    WGEN_Stmt_Push (region_body, wgen_stmk_region_body, Get_Srcpos());

    return region;
}

void WGEN_expand_start_sections (gs_t stmt)
{
       FmtAssert (lastlocal_node_stack.empty(),
                  ("Lastlocal non-POD variables from previous region?"));
       // For a SECTIONS construct, if it has an enclosing parallel
       // region then the required constructor and destructor calls
       // will be outside the SECTIONS region, in the parent parallel
       // region. Any "local" pragmas for non-pod objects will be in
       // the pragmas section of the enclosing parallel region. They
       // are required to be in the enclosing parallel region for
       // "local" object references in ctor/dtor calls in the parallel
       // region to be localized.
       //
       // If there is no lexically enclosing parallel region, enclose
       // it inside an EH region. Any "local" pragmas for non-pod objects
       // will be in the pragmas section of the SECTIONS region.
       // But any constructor and destructor calls will be outside
       // the SECTIONS region, in the parent EH region.
       WN * enclosing_region = NULL;
       if (lang_cplus)
       {
         CHECK_STMT * enclosing = WGEN_CS_top();
         if (enclosing)
         {
           enclosing_region = enclosing->region;

           FmtAssert (enclosing_region,
                      ("NULL enclosing region of SECTIONS clause"));
           // sanity check, must be a parallel region.
           WN * first_pragma = WN_first(WN_region_pragmas(enclosing_region));
           FmtAssert (WN_region_kind(enclosing_region) == REGION_KIND_MP &&
                      first_pragma &&
                      WN_pragma(first_pragma) == WN_PRAGMA_PARALLEL_BEGIN,
                      ("Unexpected parent region of SECTIONS clause"));
         }
         else
           enclosing_region = Setup_MP_Enclosing_Region();
       }

       /* create a region on current block */
       
       WN * region = WGEN_region(REGION_KIND_MP);

       WN *wn;
       
//////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WGEN_CS_push(wgen_omp_sections,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       WGEN_Set_Prag(WGEN_Stmt_Top());
       WGEN_Set_Region (region);
////////////////////////////////////////////////////////
	  
       wn = WN_CreatePragma(WN_PRAGMA_PSECTION_BEGIN, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0);   
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());
 
       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());

  gs_t clauses = gs_omp_sections_clauses (stmt);

  if (lang_cplus)
    region = enclosing_region;

  for (; clauses; clauses = gs_omp_clause_chain(clauses))
    WGEN_process_omp_clause(clauses, region);

  WGEN_Stmt_Pop (wgen_stmk_region_pragmas);

  if (lang_cplus)
  {
    // If non-pod, push the enclosing parallel region.
    if (!dtor_call_stack.empty() &&
        WN_operator (dtor_call_stack.top()) == OPR_CALL)
      dtor_call_stack.push (region);

    // If non-pod, push the enclosing parallel region.
    if (!local_node_stack.empty() &&
        WN_operator (local_node_stack.top()) == OPR_PRAGMA)
      local_node_stack.push (region);
  }
}


void WGEN_expand_start_section ()
{
       WN *wn;

       wn = WN_CreatePragma(WN_PRAGMA_SECTION, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0); 
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());
// If not required, the following 3 lines can be removed
       WN * body = WN_CreateBlock ();
       WGEN_Stmt_Push (body, wgen_stmk_scope, Get_Srcpos());
//////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WGEN_CS_push(wgen_omp_section,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
}


void WGEN_expand_end_section ( )
{
// If a block not required for a pragma_section, the following lines can be removed and
// left this function doing nothing
       WN *wn = WGEN_Stmt_Top ();
       WGEN_Stmt_Pop (wgen_stmk_scope);
       WGEN_Stmt_Append (wn, Get_Srcpos());
       
       WGEN_CS_pop(wgen_omp_section);
}


static void WGEN_generate_non_pod_lastlocal_finalization (void);

void WGEN_expand_end_sections ( )
{
    WGEN_generate_non_pod_lastlocal_finalization ();
    WGEN_Stmt_Pop (wgen_stmk_scope);

    // Note the SECTIONS is still in stack. If there is no enclosing
    // (parallel) region, then process and pop the enclosing EH region.
    if (lang_cplus && !WGEN_CS_enclose())
    {
      WN *wn = WGEN_Stmt_Top ();
      // Insert any required destructor calls in the end of enclosing EH region.
      WGEN_maybe_call_dtors(wn);
      WGEN_maybe_localize_vars(wn);
      WGEN_maybe_do_eh_cleanups();
      // Pop enclosing EH region body.
      WGEN_Stmt_Pop (wgen_stmk_region_body);
    }

    WGEN_CS_pop(wgen_omp_sections);
}



void WGEN_expand_start_single (gs_t stmt)
{
  /* create a region on current block */

  WN * region = WGEN_region(REGION_KIND_MP);

  WN *wn;

  wn = WN_CreatePragma(WN_PRAGMA_SINGLE_PROCESS_BEGIN, 
                       (ST_IDX) NULL, 0, 0);   
  WN_set_pragma_omp(wn);
  WGEN_Stmt_Append (wn, Get_Srcpos());
  //////////////// OPENMP CHECK STACK /////////////
  SRCPOS srcpos = Get_Srcpos();
  WGEN_CS_push(wgen_omp_single,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
  WGEN_Set_Prag(WGEN_Stmt_Top());
  WGEN_Set_Region (region);
 
  /////required?///////
  Set_PU_has_mp (Get_Current_PU ());
  Set_FILE_INFO_has_mp (File_info);
  Set_PU_uplevel (Get_Current_PU ());

  gs_t clauses = gs_omp_single_clauses (stmt);

  for (; clauses; clauses = gs_omp_clause_chain(clauses))
    WGEN_process_omp_clause(clauses, region);

  WGEN_Stmt_Pop (wgen_stmk_region_pragmas);

  if (lang_cplus)
  {
    if (!dtor_call_stack.empty() &&
        WN_operator (dtor_call_stack.top()) == OPR_CALL)
      dtor_call_stack.push (region);

    if (!local_node_stack.empty() &&
        WN_operator (local_node_stack.top()) == OPR_PRAGMA)
      local_node_stack.push (region);
  }
}

void WGEN_expand_end_single ()
{
    if (lang_cplus)
    {
      WN *wn = WGEN_Stmt_Top ();
      WGEN_maybe_call_dtors (wn);
      WGEN_maybe_localize_vars (wn);
      WGEN_maybe_do_eh_cleanups ();
    }

    WGEN_Stmt_Pop (wgen_stmk_scope);
    WGEN_CS_pop(wgen_omp_single);
}

 
void WGEN_expand_start_parallel_for (gs_t stmt)
{
  Is_True (gs_tree_code(stmt) == GS_OMP_PARALLEL,
           ("WGEN_expand_start_parallel_for: unexpected tree code"));
  /* create a region on current block */

  WN * region = WGEN_region(REGION_KIND_MP);

  WN *wn;

  wn = WN_CreatePragma(WN_PRAGMA_PARALLEL_DO, (ST_IDX) NULL, 0, 0);   
  WN_set_pragma_omp(wn);
  WGEN_Stmt_Append (wn, Get_Srcpos());
  //////////////// OPENMP CHECK STACK /////////////
  SRCPOS srcpos = Get_Srcpos();
  WGEN_CS_push (wgen_omp_parallel_for, SRCPOS_linenum(srcpos),
                SRCPOS_filenum(srcpos));
  WGEN_Set_Prag (WGEN_Stmt_Top());
  WGEN_Set_Region (region);
  /////required?///////
  Set_PU_has_mp (Get_Current_PU ());
  Set_FILE_INFO_has_mp (File_info);
  Set_PU_uplevel (Get_Current_PU ());

  // First process any clauses in PARALLEL pragma
  gs_t clauses = gs_omp_parallel_clauses (stmt);

  for (; clauses; clauses = gs_omp_clause_chain(clauses))
    WGEN_process_omp_clause(clauses);

  // Now process any clauses in enclosed FOR pragma
  stmt = gs_omp_parallel_body(stmt);
  Is_True (gs_tree_code(stmt) == GS_BIND_EXPR,
           ("WGEN_expand_start_parallel_for: unexpected scope stmt"));
  stmt = gs_bind_expr_body(stmt);
  Is_True (gs_tree_code(stmt) == GS_OMP_FOR,
           ("WGEN_expand_start_parallel_for: expected FOR pragma"));

  clauses = gs_omp_for_clauses (stmt);
  for (; clauses; clauses = gs_omp_clause_chain(clauses))
    if (gs_omp_clause_code(clauses) != GS_OMP_CLAUSE_NOWAIT)
      WGEN_process_omp_clause(clauses);

  WGEN_Stmt_Pop (wgen_stmk_region_pragmas);
}

void WGEN_expand_end_parallel_for ()
{
  WN *wn = WGEN_Stmt_Top ();
  //WGEN_check_parallel_for (wn);
  WGEN_Stmt_Pop (wgen_stmk_scope);
  WGEN_CS_pop(wgen_omp_parallel_for);
}


/////////////////////////
//////  parallel_sections directive


void WGEN_expand_start_parallel_sections (gs_t stmt)
{
  /* create a region on current block */

  WN * region = WGEN_region(REGION_KIND_MP);

  WN *wn;

  wn = WN_CreatePragma(WN_PRAGMA_PARALLEL_SECTIONS, (ST_IDX) NULL, 0, 0);   
  WN_set_pragma_omp(wn);
  WGEN_Stmt_Append (wn, Get_Srcpos());

  //////////////// OPENMP CHECK STACK /////////////
  SRCPOS srcpos = Get_Srcpos();
  WGEN_CS_push (wgen_omp_parallel_sections, SRCPOS_linenum(srcpos),
                SRCPOS_filenum(srcpos));
  WGEN_Set_Prag (WGEN_Stmt_Top());
  WGEN_Set_Region (region);

  /////required?///////
  Set_PU_has_mp (Get_Current_PU ());
  Set_FILE_INFO_has_mp (File_info);
  Set_PU_uplevel (Get_Current_PU ());

  // First process any clauses in PARALLEL pragma
  gs_t clauses = gs_omp_parallel_clauses (stmt);

  for (; clauses; clauses = gs_omp_clause_chain(clauses))
    WGEN_process_omp_clause(clauses);

  // Now process any clauses in enclosed SECTIONS pragma
  stmt = gs_omp_parallel_body(stmt);
  Is_True (gs_tree_code(stmt) == GS_BIND_EXPR,
           ("WGEN_expand_start_parallel_for: unexpected scope stmt"));
  stmt = gs_bind_expr_body(stmt);
  Is_True (gs_tree_code(stmt) == GS_OMP_SECTIONS,
           ("WGEN_expand_start_parallel_for: expected FOR pragma"));

  clauses = gs_omp_sections_clauses (stmt);
  for (; clauses; clauses = gs_omp_clause_chain(clauses))
    if (gs_omp_clause_code(clauses) != GS_OMP_CLAUSE_NOWAIT)
      WGEN_process_omp_clause(clauses);

  WGEN_Stmt_Pop (wgen_stmk_region_pragmas);
}

void WGEN_expand_end_parallel_sections ()
{
     WN *wn = WGEN_Stmt_Top ();
//     WGEN_check_parallel_sections (wn);
     WGEN_Stmt_Pop (wgen_stmk_scope);

     WGEN_CS_pop(wgen_omp_parallel_sections);
}


///////// master directive ////////
void WGEN_check_master()
{
	//master directives are not permitted in the dynamic extent of for, sections,
    //and single directives if the master directives bind to the same parallel 
    //as the work-sharing directives.
    bool chkflag=false;
    char * msg = NULL;

    if( WGEN_bind_to_same(wgen_omp_master,wgen_omp_for,wgen_omp_parallel)||
        WGEN_bind_to_same(wgen_omp_master,wgen_omp_sections,wgen_omp_parallel)||
        WGEN_bind_to_same(wgen_omp_master,wgen_omp_single,wgen_omp_parallel))
    {
    	msg = "Master directives are not permitted in the dynamic extent  \
    		of for, sections,and single directives if the master directives bind to  \
    		the same parallel as the work-sharing directives."; 
       chkflag=true;
    }

    WGEN_omp_error(WGEN_CS_top(), chkflag, msg);
    return;
	
	
}

void WGEN_expand_start_master ( )
{
       /* create a region on current block */
       
      WGEN_region(REGION_KIND_MP);
      WN *wn;
      wn = WN_CreatePragma(WN_PRAGMA_MASTER_BEGIN, 
      	                     (ST_IDX) NULL, 
      	                     0, 
      	                     0);   
      WN_set_pragma_omp(wn);
      WGEN_Stmt_Append (wn, Get_Srcpos());
      //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WGEN_CS_push(wgen_omp_master,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));


        /////required?///////
      Set_PU_has_mp (Get_Current_PU ());
      Set_FILE_INFO_has_mp (File_info);
      Set_PU_uplevel (Get_Current_PU ());
      
      WGEN_Stmt_Pop (wgen_stmk_region_pragmas);
}

void WGEN_expand_end_master ()
{
      WGEN_Stmt_Pop (wgen_stmk_scope);
      WGEN_CS_pop(wgen_omp_master);
};


void  WGEN_expand_start_critical (ST *region_phrase,char* critical_name)
{
       WN *wn;
       TCON  tcon;
       //char *critical_name = NULL;

       WN * pragma_wn = wn = WN_CreatePragma(WN_PRAGMA_CRITICAL_SECTION_BEGIN,
                             region_phrase,
                             0,
                             0);

       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());
       wn = WN_CreateBarrier( FALSE, 0 );
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());

       // pass on the 'st' to critical-section-end
       WGEN_Stmt_Push (pragma_wn, wgen_stmk_dummy, Get_Srcpos());
       
       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WGEN_CS_push(wgen_omp_critical,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));

       // Check for the same critical name first, before setting the name,
       // since we don't want to find ourself for a match

       WGEN_Set_Nameflag(critical_name);

       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());

       WN * body = WN_CreateBlock ();
       WGEN_Stmt_Push (body, wgen_stmk_scope, Get_Srcpos());
}

void  WGEN_expand_end_critical ( )
{
       WN *wn;
       ST * st;

       WN *wn1 = WGEN_Stmt_Top ();
       WGEN_Stmt_Pop (wgen_stmk_scope);

       wn = WGEN_Stmt_Top ( );
       st = WN_st (wn);
           

       WGEN_Stmt_Pop ( wgen_stmk_dummy );

       WGEN_Stmt_Append (wn1, Get_Srcpos());

       wn = WN_CreateBarrier( TRUE, 0 );
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());
       wn = WN_CreatePragma(WN_PRAGMA_CRITICAL_SECTION_END,
                             st,
                             0,
                             0);
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());
       
       WGEN_CS_pop(wgen_omp_critical);

}

///////// atomic directive ////////

void  WGEN_expand_start_atomic ()
{
       WN *wn;
       wn = WN_CreatePragma(WN_PRAGMA_ATOMIC, 
       	                    (ST_IDX) NULL,
       	                    0,
       	                    0);   
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());

       WN * body = WN_CreateBlock ();
       WGEN_Stmt_Push (body, wgen_stmk_scope, Get_Srcpos());
       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WGEN_CS_push(wgen_omp_atomic,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       
	   /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());
}

// Import function from omp_lower.cxx
// Is this op a Direct load or store
static BOOL Direct_Memory(WN *wn1)
{
  OPERATOR oper = WN_operator(wn1);
  return oper == OPR_LDID || oper == OPR_STID;
}
                                                                                
// Import function from omp_lower.cxx
static BOOL Equiv_Expression(WN *wn1, WN *wn2)
{
  if (!WN_Equiv(wn1,wn2)) return FALSE;
  for (INT kidno=0; kidno<WN_kid_count(wn1); kidno++) {
    if (!Equiv_Expression(WN_kid(wn1,kidno),WN_kid(wn2,kidno))) {
      return FALSE;
    }
  }
  return TRUE;
}
                                                                                
// Import function from omp_lower.cxx
// Are the two operations memory refs to the same location
static BOOL Same_Location(WN *wn1, WN *wn2)
{
  OPCODE opc1 = WN_opcode(wn1);
  OPCODE opc2 = WN_opcode(wn2);
  if (!OPCODE_is_load(opc1) && !OPCODE_is_store(opc1)) return FALSE;
  if (!OPCODE_is_load(opc2) && !OPCODE_is_store(opc2)) return FALSE;
  if (WN_offset(wn1) != WN_offset(wn2)) return FALSE;
  if (Direct_Memory(wn1)) {
    if (!Direct_Memory(wn2)) return FALSE;
    return WN_st(wn1) == WN_st(wn2);
  }
 if (Direct_Memory(wn2)) return FALSE;
 WN *addr_kid1, *addr_kid2;
 if (OPCODE_is_store(opc1)) {
   addr_kid1 = WN_kid1(wn1);
 } else {
   addr_kid1 = WN_kid0(wn1);
 }
 if (OPCODE_is_store(opc2)) {
   addr_kid2 = WN_kid1(wn2);
 } else {
   addr_kid2 = WN_kid0(wn2);
 }
 return Equiv_Expression(addr_kid1,addr_kid2);
}

// Import function from omp_lower.cxx, with modifications
// Find under wn the location refered to in loc
// return NULL if you do not find it
static WN *Find_Same_Location(WN *loc,WN *wn, WN ** parent, int * kidnum)
{
  if (Same_Location(loc,wn)) {
    return wn;
  } else {
    // Don't expect it to be inside a block
    if (WN_operator (wn) == OPR_BLOCK)
      return NULL;
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      WN *tmp;
      tmp = Find_Same_Location(loc,WN_kid(wn,kidno), parent, kidnum);
      if (tmp)
      {
        if (WN_kid (wn, kidno) == tmp)
        {
          *kidnum = kidno;
          *parent = wn;
        }
        return tmp;
      }
    }
    return NULL;
  }
}

// For atomic operations of the form
//
// x binop= expr
//
// sometimes we have 'x' buried into the whole expression
//
// e.g. x += y + 1
// i.e. x = x + y + 1
// we often associate as 
// x = (x + y) + 1
// omp-lowerer expects x to be on top, so fix such expressions here
// Replace x by zero, and add 'x' to the result
// Currently this function expects 'binop' to be +/-
//
static void format_rhs_atomic_stmt (WN * wn)
{
  Is_True (WN_operator (wn) == OPR_BLOCK, ("Expected block"));
  Is_True (WN_first (wn) && (WN_first (wn) == WN_last (wn)), 
           ("Expected 1 stmt in block"));
  WN * store = WN_first (wn);
  WN * op = WN_kid0 (store);
  if (Same_Location (store, WN_kid0 (op)) ||
      Same_Location (store, WN_kid1 (op)))
    return;
  // We probably don't need this for operations other than +/-
  FmtAssert (WN_operator (op) == OPR_ADD || WN_operator (op) == OPR_SUB,
             ("Support other operations"));
  // Now find the load of the lhs st buried somewhere down

  WN * parent;
  int kidno;
  WN * find = Find_Same_Location (store, op, &parent, &kidno);
  FmtAssert (find, ("Invalid atomic operation stmt"));
  Is_True (WN_kid (parent, kidno) == find, ("Operand mismatch"));

  WN_kid (parent, kidno) = WN_Intconst (WN_rtype (find), 0);
  WN_kid0 (store) = WN_Add (WN_rtype (op), find, op);
}

static WN * check_atomic (WN * block)
{
  Is_True (WN_operator(block) == OPR_BLOCK, (""));

  if (WN_first(block) == WN_last(block)) return NULL;

  return WN_last(block);
}

static void adjust_atomic (WN * stmt)
{
  WN * top = WGEN_Stmt_Top();

  Is_True (WN_operator(top) == OPR_BLOCK,
           ("adjust_atomic: expected BLOCK node"));
  Is_True (WN_last(top) == stmt,
           ("adjust_atomic: expected atomic operation to be last stmt in blk"));
  WN * prev = WN_prev(stmt);

  while (prev && (WN_operator(prev) != OPR_PRAGMA ||
                  WN_pragma(prev) != WN_PRAGMA_ATOMIC))
    prev = WN_prev(prev);

  Is_True (prev, ("adjust_atomic: did not find ATOMIC pragma"));

  prev = WN_EXTRACT_FromBlock (top, prev);
  WN_INSERT_BlockBefore (top, stmt, prev);
}

void WGEN_expand_end_atomic ()
{
       WN *wn = WGEN_Stmt_Top ();
       WN * ptr = check_atomic(wn);
       WGEN_Stmt_Pop (wgen_stmk_scope);
       WGEN_Stmt_Append (wn, Get_Srcpos());
       if (ptr)
         adjust_atomic(ptr);
       WGEN_CS_pop(wgen_omp_atomic);
}



void  WGEN_expand_start_ordered (void)
{
       WN *wn;
       wn = WN_CreatePragma(WN_PRAGMA_ORDERED_BEGIN, 
                             (ST_IDX) NULL, 
                             0, 
                             0);   
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());
       wn = WN_CreateBarrier( FALSE, 0 );
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());
       
       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WGEN_CS_push(wgen_omp_ordered,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));

       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());
}

void  WGEN_expand_end_ordered (void)
{
       WN *wn;
       wn = WN_CreateBarrier( TRUE, 0 );
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());
       wn = WN_CreatePragma(WN_PRAGMA_ORDERED_END, (ST_IDX) NULL, 
       	                     0,
       	                     0);   
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());       
      // WGEN_check_ordered();
       WGEN_CS_pop(wgen_omp_ordered);
}



void  WGEN_expand_barrier ()
{
       WN *wn;
       wn = WN_CreatePragma(WN_PRAGMA_BARRIER, 
                             (ST_IDX) NULL,
                             0,
                             0);   
       WN_set_pragma_omp(wn);
       WGEN_Stmt_Append (wn, Get_Srcpos());
       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WGEN_CS_push(wgen_omp_barrier,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
    
    //   WGEN_check_barrier();
 
       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());
       
       WGEN_CS_pop(wgen_omp_barrier);
}



// Generate OMP non-pod finalization code required for lastprivate
// variables.
// Generate:
//  forward-barrier
//  if __omp_non_pod_lastlocal != 0
//  then
//    backward-barrier
//    assignment-operator call(s)
//    forward-barrier
//  else
//  endif  
//  backward-barrier
static void
WGEN_generate_non_pod_lastlocal_finalization (void)
{
  const char * omp_non_pod_lastlocal_var_name = "__omp_non_pod_lastlocal";
  static ST * omp_non_pod_lastlocal_var_st = NULL;

  if (!lang_cplus) return;

  if (lastlocal_node_stack.empty()) return;

  WN * then_block = WN_CreateBlock();
  WN * else_block = WN_CreateBlock();

  if (!omp_non_pod_lastlocal_var_st)
  {
    omp_non_pod_lastlocal_var_st = New_ST(GLOBAL_SYMTAB);
    ST_Init (omp_non_pod_lastlocal_var_st,
             Save_Str(omp_non_pod_lastlocal_var_name),
             CLASS_VAR, SCLASS_EXTERN, EXPORT_PREEMPTIBLE,
             MTYPE_TO_TY_array[MTYPE_I4]);
  }

  WN * test = WN_NE (MTYPE_I4,
                     WN_Ldid (MTYPE_I4,
                              0,
                              omp_non_pod_lastlocal_var_st,
                              ST_type(omp_non_pod_lastlocal_var_st)),
                     WN_Intconst(MTYPE_I4, 0));

  // then-block
  // backward barrier
  WN_INSERT_BlockLast (then_block, WN_CreateBarrier(FALSE, 0));
  // assignment operators
  while (!lastlocal_node_stack.empty())
  {
    WN * lastlocal = lastlocal_node_stack.top();
    lastlocal_node_stack.pop();
    WN_INSERT_BlockLast (then_block, lastlocal);
  }
  // forward barrier
  WN_INSERT_BlockLast (then_block, WN_CreateBarrier(TRUE, 0));

  WN * if_stmt = WN_CreateIf (test, then_block, else_block);

  WGEN_Stmt_Append (WN_CreateBarrier(TRUE, 0), Get_Srcpos());
  WGEN_Stmt_Append (if_stmt, Get_Srcpos());
  WGEN_Stmt_Append (WN_CreateBarrier(FALSE, 0), Get_Srcpos());
}

///////// do loop expander ////////

void WGEN_expand_start_do_loop(WN * index, WN * start, WN * end, WN * step)
{
     WN *doloop;

     WN * body = WN_CreateBlock ();

     doloop = WN_CreateDO(index, start, end, step, body, NULL);
     WGEN_Stmt_Append (doloop, Get_Srcpos());

     WGEN_Stmt_Push (doloop, wgen_stmk_for_cond, Get_Srcpos());
     WGEN_Stmt_Push (body, wgen_stmk_for_body, Get_Srcpos());
}

void WGEN_expand_end_do_loop (void)
{
  WGEN_generate_non_pod_lastlocal_finalization ();

  WGEN_Stmt_Pop (wgen_stmk_for_body);

  WGEN_Stmt_Pop (wgen_stmk_for_cond);
}

