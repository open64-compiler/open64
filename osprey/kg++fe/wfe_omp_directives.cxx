/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

#include "gnu_config.h"
#include "system.h"
extern "C"
{
#include "gnu/tree.h"
#include "cp-tree.h"
}
#if defined(TARG_PPC32)
// the definition in gnu/config/ppc32/rs6000.h causes problem
// with the enumeration in common/com/ppc32/config_targ.h
#undef TARGET_POWERPC
#endif /* TARG_PPC32 */

#include "wn.h"
#include "wn_util.h"
#include "wfe_misc.h"
#include "wfe_stmt.h"
#include "omp_types.h"
#include "omp_directive.h"
#include "wfe_omp_directives.h"
#include "wfe_omp_check_stack.h"
#include "tree_symtab.h"

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

enum PRAGMA_TYPE
{
  PRIVATE,
  FIRSTPRIVATE
};

BOOL Trace_Omp = FALSE;

// Put in per-file OpenMP specific initializations here.
void WFE_Omp_Init (void)
{
  WFE_CS_Init ();
  if (getenv ("CFE_OMP_DEBUG") && !strcmp (getenv ("CFE_OMP_DEBUG"), "1"))
    Trace_Omp = TRUE;
}

// Returns the created region
WN *  WFE_region(REGION_KIND kind)
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
  WFE_Stmt_Append (region, Get_Srcpos());
  WFE_Stmt_Push (body, wfe_stmk_scope, Get_Srcpos());
  WFE_Stmt_Push (pragmas, wfe_stmk_region_pragmas, Get_Srcpos());
  
  return region;
}

/////////////////////////
//////  error report routine:
void WFE_omp_error(CHECK_STMT* cs, bool chkflag, char * msg)
{
  char dirname[100];
  if(chkflag==false)
    return;
  switch(cs->kind)
  {
    case wfe_omp_parallel: sprintf(dirname,"#PRAGMA OMP PARALLEL");
    					  break;	
    case wfe_omp_for: sprintf(dirname,"#PRAGMA OMP FOR");
    			      break;	 	
    case wfe_omp_single: sprintf(dirname,"#PRAGMA OMP SINGLE");
						 break;					
    case wfe_omp_sections: sprintf(dirname,"#PRAGMA OMP SECTIONS");
						 break;	
    case wfe_omp_parallel_sections: sprintf(dirname,"#1PRAGMA OMP PARRALLEL SECTIONS");
						 break;	
    case wfe_omp_parallel_for: sprintf(dirname,"#PRAGMA OMP PARRALLEL FOR");
						break;
    default: sprintf(dirname,"OTHER DIRECTIVES ");
  }

  if (msg)
    ErrMsg (EC_Bad_Omp, msg);
}

/*********** Data clause check***********/
void WFE_check_private(WN *wn_p, bool chkflag)
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

void WFE_check_firstprivate(WN *wn_fp, bool chkflag)
{
    if(WN_st(wn_fp)->storage_class==SCLASS_FORMAL_REF)
          	{
          	  fprintf(stderr,"A variable specified in a first private\
		  clause must not have an incomplete type \
          	 	   or a reference type. \n");
          	 	   chkflag=true;
          	}
}

void WFE_check_lastprivate(WN *wn_lp, bool chkflag)
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

void WFE_check_reduction(WN* wn_r, bool chkflag)
{
  if(ST_is_const_var(* WN_st(wn_r)))
    { 
      fprintf(stderr," A variable that is specified in the reduction\
      clause must not be const-qualified\n");
      chkflag=true;
    }
}

void WFE_check_default(WN* wn_d, bool chkflag)
{
  /* bool flag; 
   if(WN_pragma_arg1(wn_d)==default_none)
   	{ 
   	   if(ST_is_thread_private(*WN_st(wn_d)) || ST_is_const_var(* WN_st(wn_d)) )
   	   	{
   	   		flag=true;
   	   		break;   	   	
   	   		}
   	}
   if(flag==false)
   	printf("Default(none) clause error, cannot match one of five restriction!\n");
   else return; 
   */
}

/////////////////////////
//////  check PARALLEL directive
void WFE_check_parallel ( WN *parallel_wn)
{
    // to check for parallel directive node represented by parallel_wn 
    // not to check the clause part of the parallel directive
    // only check the current block, not necessory to check into lower nested levels
 
    bool chkflag=false;     //set false for find error flag
    char * msg = NULL;
      //set space for error checking content
    CHECK_STMT* cs1;
    WN* wn1,*wn2;
    
    cs1=WFE_CS_top();   //get top of stack (this parallel directive)
    wn1=cs1->wn_prag;   //get its prama list
    wn2=WN_first(wn1);

    while(wn2!=NULL)
    {
      if(WN_st(wn2)==NULL)
      {
        wn2=WN_next(wn2);
        continue;
      }
  
      if(ST_is_thread_private(* WN_st(wn2)) )
      {
      	if(WN_pragma(wn2)!=WN_PRAGMA_COPYIN&&WN_pragma(wn2)!=WN_PRAGMA_COPYPRIVATE
      	  &&WN_pragma(wn2)!=WN_PRAGMA_MPSCHEDTYPE&&WN_pragma(wn2)!=WN_PRAGMA_IF
      	  &&WN_pragma(wn2)!=WN_PRAGMA_NUMTHREADS)
      	{
         msg = "A threadprivate variable must not appear\
	 in any clause except the copyin,\
		copyprivate, schedule, num_threads,or the if clause.";
	 
		  chkflag=true;
	  
      	}
      }
      //for private clause
      if(WN_pragma(wn2)==WN_PRAGMA_LOCAL)
      	WFE_check_private(wn2,chkflag);
      //for firstprivate clause
      if(WN_pragma(wn2)==WN_PRAGMA_FIRSTPRIVATE)
      	WFE_check_firstprivate(wn2,chkflag);
      //for reduction clause:
      if(WN_pragma(wn2)==WN_PRAGMA_REDUCTION)
      	WFE_check_reduction(wn2,chkflag);
      //for default clause
      if(WN_pragma(wn2)==WN_PRAGMA_DEFAULT)
      	WFE_check_default(wn2,chkflag);
      //for copyin clause:
      if(WN_pragma(wn2)==WN_PRAGMA_COPYIN)
      	{ 
          if (!ST_is_thread_private(* WN_st(wn2)))
	  {
          	msg = "A variable that is specified\
		in the copyin clause must be a threadprivate variable.";
            chkflag=true;
	  }
      	}
     wn2=WN_next(wn2);
    }      
    WFE_omp_error(WFE_CS_top(), chkflag, msg);
}

// Given a function decl, this function returns TRUE if it is a constructor
// AND not a copy constructor AND ( it either takes no arguments OR all
// arguments are optional ). Returns FALSE otherwise.
static BOOL WFE_is_default_constructor (tree fndecl)
{
  Is_True (TREE_CODE (fndecl) == FUNCTION_DECL, ("Invalid function decl"));
  if (!DECL_CONSTRUCTOR_P (fndecl)) return FALSE;
  if (DECL_COPY_CONSTRUCTOR_P (fndecl)) return FALSE;

  tree args = FUNCTION_FIRST_USER_PARMTYPE (fndecl);

  // check if all arguments have default values
  for (; args; args = TREE_CHAIN (args))
    if (args != void_list_node && !TREE_PURPOSE (args)) return FALSE;
  return TRUE;
}

// Calling default constructor implies also a call to destructor at the
// end of the parallel region.
// Call default contructor and destructor for variable VAR.
// Return the 2 calls in C and D.
static BOOL WFE_maybe_call_default_ctor (tree var, WN ** c, WN ** d)
{
  tree type = TREE_TYPE (var);
  if (TREE_CODE (type) != RECORD_TYPE || !CLASSTYPE_NON_POD_P (type))
    return FALSE;

  ST * obj = Get_ST (var);
  FmtAssert (TYPE_HAS_DEFAULT_CONSTRUCTOR (type),
             ("private clause on non-pod object %s needs default constructor",
              ST_name (obj)));
  tree found_ctor = NULL;
  tree found_dtor = NULL;
  for (tree methods = TYPE_METHODS (type);
       methods;
       methods = TREE_CHAIN (methods))
  {
    if (DECL_COMPLETE_CONSTRUCTOR_P (methods) &&
        WFE_is_default_constructor (methods))
    {
      FmtAssert (!found_ctor,
                 ("Multiple default constructor candidates for %s",
                  ST_name (obj)));
      found_ctor = methods;
    }
    if (DECL_COMPLETE_DESTRUCTOR_P (methods))
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
static BOOL WFE_maybe_call_copy_ctor (tree var, WN ** c, WN ** d)
{
  tree type = TREE_TYPE (var);
  if (TREE_CODE (type) != RECORD_TYPE || !CLASSTYPE_NON_POD_P (type))
    return FALSE;

  ST * obj = Get_ST (var);
  tree found_ctor = NULL;
  tree found_dtor = NULL;
  for (tree methods = TYPE_METHODS (type);
       methods;
       methods = TREE_CHAIN (methods))
  {
    if (DECL_COMPLETE_CONSTRUCTOR_P (methods) &&
        DECL_COPY_CONSTRUCTOR_P (methods))
    {
      FmtAssert (!found_ctor,
                 ("Multiple copy constructor candidates for %s",
                  ST_name (obj)));
      found_ctor = methods;
    }
    if (DECL_COMPLETE_DESTRUCTOR_P (methods))
    {
      FmtAssert (!found_dtor, ("Multiple destructor candidates for %s",
                               ST_name (obj)));
      found_dtor = methods;
    }
  }
  FmtAssert (found_ctor && found_dtor,
             ("No copy-ctor/dtor found for non-pod object %s", ST_name (obj)));
  // Call the copy constructor
  // Generate "C c1 = <empty>", localizer will later change it
  // to give "C __mplocalfe_c1 = c1".
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

  // Call the destructor
  ST * dtor_st = Get_ST (found_dtor);
  WN * call_dtor = WN_Call (MTYPE_V, MTYPE_V, 1, ST_st_idx (dtor_st));
  WN_actual (call_dtor, 0) = WN_COPY_Tree (WN_kid0 (call_ctor));
  WN_Set_Call_Default_Flags (call_dtor);
  *d = call_dtor;

  return TRUE;
}

// Call any destructors as required due to introducing constructors for
// MP clauses.
static void WFE_maybe_call_dtors (WN * wn)
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
static void WFE_localize_var (WN * block, ST * old_st, ST * new_st)
{
  if (WN_has_sym (block) && WN_st (block) == old_st)
    WN_st_idx (block) = ST_st_idx (new_st);

  OPERATOR opr = WN_operator (block);

  if (opr == OPR_BLOCK)
  {
    WN * node = WN_first (block);
    while (node)
    {
      WFE_localize_var (node, old_st, new_st);
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
        WN_kid1 (block) = WN_COPY_Tree (WN_kid0 (block));
        WFE_localize_var (WN_kid0 (block), old_st, new_st);
      }
      else
      {
        WN_kid0 (block) = WN_COPY_Tree (WN_kid1 (block));
        WFE_localize_var (WN_kid1 (block), old_st, new_st);
      }
    }
    else
    {
      for (int i=0; i<WN_kid_count (block); i++)
        WFE_localize_var (WN_kid (block, i), old_st, new_st);
    }
  }
}

// If required localize vars in MP region that contains WN.
static void WFE_maybe_localize_vars (WN * wn)
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

    WFE_localize_var (wn, st, new_st);
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
static BOOL WFE_handle_non_pods (tree var, WN * block, PRAGMA_TYPE p)
{
  WN * c, * d;
  // Call constructor/destructor for non-pod class variables
  BOOL constructed = FALSE;
  switch (p)
  {
    case PRIVATE:
      constructed = WFE_maybe_call_default_ctor (var, &c, &d);
      break;
    case FIRSTPRIVATE:
      constructed = WFE_maybe_call_copy_ctor (var, &c, &d);
      break;
    default:
      break;
  }

  if (constructed)
  {
    ST * st = Get_ST (var);
    // Needed by MP-lowerer
    Set_TY_is_non_pod (ST_type (st));
    // Insert call to constructor
    WN_INSERT_BlockLast (block, c);
    // Push destructor call to stack, for emission at the end
    // of the region.
    dtor_call_stack.push (d);
    return TRUE;
  }
  return FALSE;
}

void WFE_expand_start_parallel (struct Parallel_clause_wn_type *parallel_clause_wn)
{
       /* create a region on current block */
       
       WN * region = WFE_region(REGION_KIND_MP);

       WN *wn, *expr;
       WN_list *wn_list;
       ST *st;
       ST_list *st_list;
       // Used only for private clause.
       // The parser introduced a marker that marks the end of variables
       // declared MP private, and the begin of variables that are private
       // by virtue of being local to the region.
       // Special non-POD handling for private variables should only be
       // done for variables actually marked MP private in source code.
       BOOL declared_private = TRUE;

       wn = WN_CreatePragma(WN_PRAGMA_PARALLEL_BEGIN, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0);   
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
       
       ///// omp check stack action ///////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push(wfe_omp_parallel,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       WFE_Set_Prag(WFE_Stmt_Top());
       WFE_Set_Region (region);


       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());
      
       
       // Add all other pragmas/xpragmas¡­¡­.

       /********For if_clause ***************/
       expr = parallel_clause_wn->if_clause;
       if (expr)
         {
            wn = WN_CreateXpragma(WN_PRAGMA_IF, (ST_IDX) NULL, 1);
            WN_kid0(wn) = expr;
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For num_threads_clause ***************/
       expr = parallel_clause_wn->num_threads_clause;
       if (expr)
         {
            wn = WN_CreateXpragma(WN_PRAGMA_NUMTHREADS, 
            	                   (ST_IDX) NULL, 
            	                   1);
            WN_kid0(wn) = expr;
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For default_clause ***************/
       enum default_type  default_value = parallel_clause_wn->default_clause;
       if (default_value != no_default)
         {
            wn = WN_CreatePragma(WN_PRAGMA_DEFAULT, 
            	                  (ST_IDX) NULL, 
            	                  default_value,
            	                  0);
            	                  
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For private_clause ***************/
       if(parallel_clause_wn->private_clause!=NULL)
       	{
       	   WFE_Set_Cflag(clause_private);   //set clause flag for check
       	}
       for (st_list = parallel_clause_wn->private_clause; st_list != NULL; st_list = st_list->next)
         {
            tree var = st_list->var;
            if (!var)
            {
              FmtAssert (declared_private, ("private var handling error"));
              declared_private = FALSE;
              continue;
            }
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_LOCAL, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
            if (declared_private &&
                WFE_handle_non_pods (var, WN_region_body (region), PRIVATE))
	      local_node_stack.push (wn);
         }

       /********For shared_clause ***************/
        if(parallel_clause_wn->shared_clause!=NULL)
       	{
       	   WFE_Set_Cflag(clause_shared); 
       	}
       for (st_list = parallel_clause_wn->shared_clause; st_list != NULL; st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_SHARED, st, 0, 0);
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());

         }

       /********For firstprivate_clause ***************/
       if(parallel_clause_wn->firstprivate_clause!=NULL)
       	   WFE_Set_Cflag(clause_firstprivate); 
       
       for (st_list = parallel_clause_wn->firstprivate_clause; st_list != NULL;st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_FIRSTPRIVATE, st, 0, 0);
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());
            if (WFE_handle_non_pods (var, WN_region_body (region),
                                     FIRSTPRIVATE))
	      local_node_stack.push (wn);
         }

       /********For copyin_clause ***************/
       if(parallel_clause_wn->copyin_clause!=NULL)
       	   WFE_Set_Cflag(clause_copyin); 
       for (st_list = parallel_clause_wn->copyin_clause; st_list != NULL;st_list = st_list->next) 
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_COPYIN, st, 0, 0);
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());

         }

       /********For reduction_clause ***************/
       if(parallel_clause_wn->reduction_clause!=NULL)
       	{
       	   WFE_Set_Cflag(clause_reduction);
       	}
       for (wn_list = parallel_clause_wn->reduction_clause; wn_list != NULL; wn_list = wn_list->next) 
         {
            wn = wn_list->wn;
            WFE_Stmt_Append (wn, Get_Srcpos());
         }   
       
       WFE_Stmt_Pop (wfe_stmk_region_pragmas);

       if (!dtor_call_stack.empty() &&
           WN_operator (dtor_call_stack.top()) == OPR_CALL)
         dtor_call_stack.push (region);

       if (!local_node_stack.empty() &&
           WN_operator (local_node_stack.top()) == OPR_PRAGMA)
         local_node_stack.push (region);
}

void WFE_expand_end_parallel ( )
{
    WN *wn = WFE_Stmt_Top ();
    WFE_maybe_call_dtors (wn);
    WFE_maybe_localize_vars (wn);
    WFE_check_parallel (wn);
    WFE_Stmt_Pop (wfe_stmk_scope);
    WFE_CS_pop(wfe_omp_parallel);
}

/////////////////////
///////  for directive
void WFE_check_for ( WN *for_wn)
{
    // to check for "for" directive node represented by for_wn 
    // not to check the clause part of the for directive
    // only check the current block, not necessory to check into lower nested levels

    bool chkflag=false;

    WN* wn1,*wn2,*wn3,*wn4;
    CHECK_STMT *cs1,*cs2;
    char * msg = NULL;
   
    if (WN_operator(for_wn)!=OPR_BLOCK)
    {
    	fprintf(stderr,"WFE_check_for can't deal with Non-block item!\n");
    	chkflag=true;
    }
    else if (WN_first (for_wn) &&
             WN_operator (WN_first (for_wn)) == OPR_DO_LOOP)
    { // check that loop iteration variable is not threadprivate
      ST * index_st = WN_st (WN_index (WN_first (for_wn)));
      if (ST_is_thread_private (index_st))
      {
        msg = "A for loop iteration variable may not appear in a threadprivate directive.";
        chkflag = true;
      }
    }
    //solve directive nesting problem: for, sections, and single directives that bind 
    //to the same parallel are not allowed to be nested inside each other.
    if( WFE_bind_to_same(wfe_omp_for,wfe_omp_for,wfe_omp_parallel)||
        WFE_bind_to_same(wfe_omp_for,wfe_omp_sections,wfe_omp_parallel)||
        WFE_bind_to_same(wfe_omp_for,wfe_omp_single,wfe_omp_parallel) )
    {
    	msg = "for, sections, and single directives\
	that bind to the \
    	same parallel are not allowed to be nested inside each other."; 
      chkflag=true;
    }
    //solve directive nesting problem:for, sections, and single directives are not 
    //permitted in the dynamic extent of critical, ordered, and master regions if the
    // directives bind to the same parallel as the regions.
    if( WFE_bind_to_same(wfe_omp_for,wfe_omp_critical,wfe_omp_parallel)|| 
        WFE_bind_to_same(wfe_omp_for,wfe_omp_ordered,wfe_omp_parallel)|| 
        WFE_bind_to_same(wfe_omp_for,wfe_omp_master,wfe_omp_parallel) )
    {
    	msg = "for, sections, and single directives\
	are not permitted in the dynamic extent of critical, ordered,\
	and master regions if the directives\
    	bind to the same parallel as the regions.";
       chkflag=true;
    }

    //deal with parallel reduction clause:
    //printf("begin check reduction:::\n");
    cs1=WFE_CS_Find_Rtn(wfe_omp_parallel);
    cs2=WFE_CS_top();
    bool fg1,fg2,fg3,fg4,fg5;
    fg1=fg2=fg3=fg4=fg5=false;
    if(cs1)
    {
       //printf("Enter cs1 ... c1 is %d, c2 is %d \n",cs1->kind,cs2->kind);
      // printf("checking .... cflag cs1 : %d\n",cs1->cflag);
       fg1=WFE_Check_Cflag(cs1, clause_reduction);
       fg2=WFE_Check_Cflag(cs1, clause_private);
       fg3=WFE_Check_Cflag(cs2, clause_private);
       fg4=WFE_Check_Cflag(cs2,clause_firstprivate);
       fg5=WFE_Check_Cflag(cs2,clause_lastprivate);
       //printf("a=%d,b=%d\n",a,b );
       if(fg1&&(fg3||fg4||fg5)||fg2&&(fg4||fg5))
       {
           wn1=cs1->wn_prag;
           wn2=cs2->wn_prag;
           wn3=WN_first(wn1);
           wn4=WN_first(wn2);
           while(wn3!=NULL)
           {
             if(WN_pragma(wn3)!=WN_PRAGMA_REDUCTION&&
             	WN_pragma(wn3)!=WN_PRAGMA_LOCAL)
             	{ 
             	  wn3=WN_next(wn3);
             	  continue;
             	}
             //printf("reduction clause detected \n");
             while(wn4!=NULL)
             {
                if(WN_pragma(wn4)==WN_PRAGMA_LOCAL&&WN_pragma(wn3)==WN_PRAGMA_REDUCTION
                	&&WN_st_idx(wn3)==WN_st_idx(wn4))
                {
                  msg = "Variables that appear in the reduction clause of a parallel  \
                  	    directive cannot be specified in a private clause on a work-sharing \
                  	    directive that binds to the parallel construct.";
                  chkflag=true;
                }
                if(WN_pragma(wn4)==WN_PRAGMA_FIRSTPRIVATE&&
                	WN_st_idx(wn3)==WN_st_idx(wn4))
                {
                  msg = "Variables that are private within a parallel region or that appear  \
                  	   in the reduction clause of a parallel directive cannot be specified in a \
                  	   firstprivateclause on for directive that binds to the parallel  construct.";
                  chkflag=true;
                }
                if(WN_pragma(wn4)==WN_PRAGMA_LASTLOCAL&&
                	WN_st_idx(wn3)==WN_st_idx(wn4))
                {
                  msg = "Variables that are private within a parallel region or that appear  \
                  	   in the reduction clause of a parallel directive cannot be specified in a \
                  	   lastprivate clause on a for directive that binds to the parallel  construct.";
                  chkflag=true;
                }
                wn4=WN_next(wn4);
             }
             wn3=WN_next(wn3);
           } 
           
       }
    }
    	
    cs1=WFE_CS_top();
    wn1=cs1->wn_prag;
    wn2=WN_first(wn1);
    while(wn2!=NULL)
    {

      if(WN_st(wn2)==NULL)
      {
        wn2=WN_next(wn2);
        continue;
      }
      if(ST_is_thread_private(* WN_st(wn2)) )
      {
      	if(WN_pragma(wn2)!=WN_PRAGMA_COPYIN&&WN_pragma(wn2)!=WN_PRAGMA_COPYPRIVATE
      	  &&WN_pragma(wn2)!=WN_PRAGMA_MPSCHEDTYPE&&WN_pragma(wn2)!=WN_PRAGMA_IF
      	  &&WN_pragma(wn2)!=WN_PRAGMA_NUMTHREADS)
      	{
          msg = "A threadprivate variable must not appear in any clause except the copyin, \
				copyprivate, schedule, num_threads,or the if clause.";
	     chkflag=true;
      	}
      }
      
       //for private clause
      if(WN_pragma(wn2)==WN_PRAGMA_LOCAL)
      WFE_check_private(wn2,chkflag);
      //for firstprivate clause
      if(WN_pragma(wn2)==WN_PRAGMA_FIRSTPRIVATE)
      WFE_check_firstprivate(wn2,chkflag);
       //for lastprivate clause
      if(WN_pragma(wn2)==WN_PRAGMA_LASTLOCAL)
      WFE_check_lastprivate(wn2,chkflag);
      //for reduction clause
      if(WN_pragma(wn2)==WN_PRAGMA_REDUCTION)
      	WFE_check_reduction(wn2,chkflag);
      //for default clause
      if(WN_pragma(wn2)==WN_PRAGMA_DEFAULT)
      	WFE_check_default(wn2,chkflag);
      
      wn2=WN_next(wn2);
  
    }       
    WFE_omp_error(WFE_CS_top(), chkflag, msg);
    return;
   
}

void WFE_expand_start_for ( struct For_clause_wn_type * for_clause_wn )
{
       /* create a region on current block */
       
       WN * region = WFE_region(REGION_KIND_MP);

       WN *wn, *expr;
       WN_list *wn_list;
       ST *st;
       ST_list *st_list;

       wn = WN_CreatePragma(WN_PRAGMA_PDO_BEGIN, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0);
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
       /////////  omp ///////////////////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push(wfe_omp_for,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       WFE_Set_Prag(WFE_Stmt_Top());
       WFE_Set_Region (region);

 
       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());
      
       // Add all other pragmas/xpragmas¡­¡­.

       /********For private_clause ***************/
       if(for_clause_wn->private_clause!=NULL)
       	   WFE_Set_Cflag(clause_private); 
       for (st_list = for_clause_wn->private_clause; st_list != NULL; st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_LOCAL, st, 0, 0);
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());
            if (WFE_handle_non_pods (var, WN_region_body (region), PRIVATE))
	      local_node_stack.push (wn);
         }

       /********For lastprivate_clause ***************/
       if(for_clause_wn->lastprivate_clause!=NULL)
       	   WFE_Set_Cflag(clause_lastprivate); 
       for (st_list = for_clause_wn-> lastprivate_clause; st_list != NULL;st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_LASTLOCAL, st, 0, 0);
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For firstprivate_clause ***************/
       if(for_clause_wn->firstprivate_clause!=NULL)
       	   WFE_Set_Cflag(clause_firstprivate); 
       for (st_list = for_clause_wn-> firstprivate_clause; st_list != NULL;st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_FIRSTPRIVATE, st, 0, 0);
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());
            if (WFE_handle_non_pods (var, WN_region_body (region),
                                     FIRSTPRIVATE))
	      local_node_stack.push (wn);
         }


       /********For reduction_clause ***************/
       if(for_clause_wn->reduction_clause!=NULL)
       	   WFE_Set_Cflag(clause_reduction); 
       for (wn_list = for_clause_wn-> reduction_clause; wn_list != NULL; wn_list = wn_list->next) 
         {
            wn = wn_list->wn;
            WFE_Stmt_Append (wn, Get_Srcpos()); 
         }

       /********For ordered_clause ***************/
       if(for_clause_wn->ordered_clause)
       	   WFE_Set_Cflag(clause_ordered); 
       if (for_clause_wn->ordered_clause)
         {
            wn = WN_CreatePragma(WN_PRAGMA_ORDERED, (ST_IDX)NULL, 0, 0);
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For schedule_1_clause ***************/
       if (for_clause_wn->schedule_1_clause != SK_NONE)
         { 
            WN_PRAGMA_SCHEDTYPE_KIND schedtype_kind;
            switch(for_clause_wn->schedule_1_clause) {
               case SK_STATIC:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_SIMPLE;
                  break;
               case SK_DYNAMIC:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_DYNAMIC;
                  break;
               case SK_GUIDED:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_GSS;
                  break;
               case SK_RUNTIME:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_RUNTIME;
                  break;
            }

            wn = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                  (ST_IDX)NULL, 
                                  schedtype_kind,
                                  0);
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());
          }

       /********For schedule_2_clause ***************/
       if (for_clause_wn->schedule_2_clause.schedule_2_kind != SK_NONE)
         { 
            WN_PRAGMA_SCHEDTYPE_KIND schedtype_kind;
            switch(for_clause_wn->schedule_2_clause.schedule_2_kind) {
               case SK_STATIC:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_SIMPLE;
                  break;
               case SK_DYNAMIC:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_DYNAMIC;
                  break;
               case SK_GUIDED:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_GSS;
                  break;
               case SK_RUNTIME:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_RUNTIME;
               break;
            }

            wn = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                  (ST_IDX)NULL, 
                                  schedtype_kind,
                                  0);
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());
            
            wn = WN_CreateXpragma(WN_PRAGMA_CHUNKSIZE,
                                  (ST_IDX) NULL,
                                  1);
            WN_kid0(wn) = for_clause_wn->schedule_2_clause.chunk_size_wn;
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());

         }

       /********For nowait_clause ***************/
       if (for_clause_wn->nowait_clause)
         {
            wn = WN_CreatePragma(WN_PRAGMA_NOWAIT, (ST_IDX)NULL, 0, 0);
            WN_set_pragma_omp(wn);
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       WFE_Stmt_Pop (wfe_stmk_region_pragmas);
  
       if (!dtor_call_stack.empty() &&
           WN_operator (dtor_call_stack.top()) == OPR_CALL)
         dtor_call_stack.push (region);
       
       if (!local_node_stack.empty() &&
           WN_operator (local_node_stack.top()) == OPR_PRAGMA)
         local_node_stack.push (region);
}

void WFE_expand_end_for ( )
{
    WN *wn = WFE_Stmt_Top ();
    WFE_maybe_call_dtors (wn);
    WFE_maybe_localize_vars (wn);
    WFE_check_for (wn);
    WFE_Stmt_Pop (wfe_stmk_scope);
    WFE_CS_pop(wfe_omp_for);
}

/////////////////////
///////  sections directive
void WFE_check_sections ( WN *sections_wn)
{
    bool chkflag=false;
    char * msg = NULL;


    WN* wn1,*wn2,*wn3,*wn4;
    CHECK_STMT *cs1,*cs2;

    // to check for "sections" directive node represented by sections_wn 
    // not to check the clause part of the sections directive
    // only check the current block, not necessory to check into lower nested levels
    
    //solve directive nesting problem: for, sections, and single directives that bind 
    //to the same parallel are not allowed to be nested inside each other.
    if( WFE_bind_to_same(wfe_omp_sections,wfe_omp_for,wfe_omp_parallel) ||
        WFE_bind_to_same(wfe_omp_sections,wfe_omp_single,wfe_omp_parallel) )
    {
    	msg = "for, sections, and single directives that bind to the same parallel  \
    	   are not allowed to be nested inside each other."; 
    	chkflag=true;
    }
    //solve directive nesting problem:for, sections, and single directives are not 
    //permitted in the dynamic extent of critical, ordered, and master regions if the
    // directives bind to the same parallel as the regions.
    if( WFE_bind_to_same(wfe_omp_sections,wfe_omp_critical,wfe_omp_parallel)||
        WFE_bind_to_same(wfe_omp_sections,wfe_omp_ordered,wfe_omp_parallel)||
        WFE_bind_to_same(wfe_omp_sections,wfe_omp_master,wfe_omp_parallel))
        
    {
    	msg = "for, sections, and single directives are not permitted in  \
    	the dynamic extent of critical, ordered, and master regions if the directives \
    	bind to the same parallel as the regions."; 
    	chkflag=true;
    }

    //deal with parallel reduction clause:
    cs1=WFE_CS_Find_Rtn(wfe_omp_parallel);
    cs2=WFE_CS_top();
    bool fg1,fg2,fg3,fg4,fg5;
    fg1=fg2=fg3=fg4=fg5=false;
    if(cs1)
    {
       //printf("Enter cs1 ... c1 is %d, c2 is %d \n",cs1->kind,cs2->kind);
      // printf("checking .... cflag cs1 : %d\n",cs1->cflag);
       fg1=WFE_Check_Cflag(cs1, clause_reduction);
       fg2=WFE_Check_Cflag(cs1, clause_private);
       fg3=WFE_Check_Cflag(cs2, clause_private);
       fg4=WFE_Check_Cflag(cs2,clause_firstprivate);
       fg5=WFE_Check_Cflag(cs2,clause_lastprivate);
       //printf("a=%d,b=%d\n",a,b );
       if(fg1&&(fg3||fg4||fg5)||fg2&&(fg4||fg5))
       {	
           wn1=cs1->wn_prag;
           wn2=cs2->wn_prag;
           wn3=WN_first(wn1);
           wn4=WN_first(wn2);
           while(wn3!=NULL)
           {
             if(WN_pragma(wn3)!=WN_PRAGMA_REDUCTION&&
             	WN_pragma(wn3)!=WN_PRAGMA_LOCAL)
             	{ 
             	  wn3=WN_next(wn3);
             	  continue;
             	}
             while(wn4!=NULL)
             {
                if(  WN_pragma(wn4)==WN_PRAGMA_LOCAL&&WN_pragma(wn3)==WN_PRAGMA_REDUCTION
                	&&WN_st_idx(wn3)==WN_st_idx(wn4))
                {
                  msg = "Variables that appear in the reduction clause of a parallel  \
                  	    directive cannot be specified in a private clause on a work-sharing  \
                  	    directive that binds to the parallel construct.";
                 chkflag=true;
                }
                if(WN_pragma(wn4)==WN_PRAGMA_FIRSTPRIVATE&&
                	WN_st_idx(wn3)==WN_st_idx(wn4))
                {
                  msg = "Variables that are private within a parallel region or that appear  \
                  	   in the reduction clause of a parallel directive cannot be specified in a  \
                  	   firstprivate clause on for directive that binds to the parallel construct.";
                  chkflag=true;
                }
                if(WN_pragma(wn4)==WN_PRAGMA_LASTLOCAL&&
                	WN_st_idx(wn3)==WN_st_idx(wn4))
                {
                 msg = "Variables that are private within a parallel region or that appear  \
                  	   in the reduction clause of a parallel directive cannot be specified in a  \
                  	   lastprivate clause on a for directive that binds to the parallel construct.";
                  chkflag=true;
                }
                wn4=WN_next(wn4);
             }
             wn3=WN_next(wn3);
           } 
           
       }
    }
    cs1=WFE_CS_top();
    wn1=cs1->wn_prag;
    wn2=WN_first(wn1);
    while(wn2!=NULL)
    {
      if(WN_st(wn2)==NULL)
      {
        wn2=WN_next(wn2);
        continue;
      }
      if(ST_is_thread_private(* WN_st(wn2)) )
      {
      	if(WN_pragma(wn2)!=WN_PRAGMA_COPYIN&&WN_pragma(wn2)!=WN_PRAGMA_COPYPRIVATE
      	  &&WN_pragma(wn2)!=WN_PRAGMA_MPSCHEDTYPE&&WN_pragma(wn2)!=WN_PRAGMA_IF
      	  &&WN_pragma(wn2)!=WN_PRAGMA_NUMTHREADS)
      	{
          msg = "A threadprivate variable must not appear in any clause except the copyin, \
				copyprivate, schedule, num_threads,or the if clause.";
	      chkflag=true;
      	}
      }
       //for private clause
      if(WN_pragma(wn2)==WN_PRAGMA_LOCAL)
      WFE_check_private(wn2,chkflag);
      //for firstprivate clause
      if(WN_pragma(wn2)==WN_PRAGMA_FIRSTPRIVATE)
      WFE_check_firstprivate(wn2,chkflag);
       //for lastprivate clause
      if(WN_pragma(wn2)==WN_PRAGMA_LASTLOCAL)
      WFE_check_lastprivate(wn2,chkflag);
       //for reduction clause
      if(WN_pragma(wn2)==WN_PRAGMA_REDUCTION)
      	WFE_check_reduction(wn2,chkflag);
      //for default clause
      if(WN_pragma(wn2)==WN_PRAGMA_DEFAULT)
      	WFE_check_default(wn2,chkflag);
      
      wn2=WN_next(wn2);
  
    }      
    WFE_omp_error(WFE_CS_top(), chkflag, msg);
    return;
}

void WFE_expand_start_sections ( struct Sections_clause_wn_type * sections_clause_wn )
{
       /* create a region on current block */
       
       WN * region = WFE_region(REGION_KIND_MP);

       WN *wn, *expr;
       WN_list *wn_list;
       ST *st;
       ST_list *st_list;
       
//////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push(wfe_omp_sections,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       WFE_Set_Prag(WFE_Stmt_Top());
       WFE_Set_Region (region);
////////////////////////////////////////////////////////
	  
       wn = WN_CreatePragma(WN_PRAGMA_PSECTION_BEGIN, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0);   
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
 
       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());

       // Add all other pragmas/xpragmas¡­¡­.

       /********For private_clause ***************/
       if(sections_clause_wn->private_clause!=NULL)
       	   WFE_Set_Cflag(clause_private); 
       for (st_list = sections_clause_wn->private_clause; st_list != NULL; st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_LOCAL, st, 0, 0);
                      WN_set_pragma_omp(wn);   
            WFE_Stmt_Append (wn, Get_Srcpos());
            WFE_Set_Cflag(clause_private);
            if (WFE_handle_non_pods (var, WN_region_body (region), PRIVATE))
	      local_node_stack.push (wn);
         }

       /********For lastprivate_clause ***************/
       if(sections_clause_wn->lastprivate_clause!=NULL)
       	   WFE_Set_Cflag(clause_lastprivate); 
       for (st_list = sections_clause_wn->lastprivate_clause; st_list != NULL;st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_LASTLOCAL, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For firstprivate_clause ***************/
       if(sections_clause_wn->firstprivate_clause!=NULL)
       	   WFE_Set_Cflag(clause_firstprivate); 
       for (st_list = sections_clause_wn-> firstprivate_clause; st_list != NULL;st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_FIRSTPRIVATE, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
            if (WFE_handle_non_pods (var, WN_region_body (region),
                                     FIRSTPRIVATE))
	      local_node_stack.push (wn);
         }


       /********For reduction_clause ***************/
       if(sections_clause_wn->reduction_clause!=NULL)
       	   WFE_Set_Cflag(clause_reduction); 
       for (wn_list = sections_clause_wn-> reduction_clause; wn_list != NULL; wn_list = wn_list->next) 
         {
            wn = wn_list->wn;
            WFE_Stmt_Append (wn, Get_Srcpos());   
         }   

       /********For nowait_clause ***************/
       if (sections_clause_wn->nowait_clause)
         {
            wn = WN_CreatePragma(WN_PRAGMA_NOWAIT, (ST_IDX)NULL, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }
       
       WFE_Stmt_Pop (wfe_stmk_region_pragmas);

       if (!dtor_call_stack.empty() &&
           WN_operator (dtor_call_stack.top()) == OPR_CALL)
         dtor_call_stack.push (region);

       if (!local_node_stack.empty() &&
           WN_operator (local_node_stack.top()) == OPR_PRAGMA)
         local_node_stack.push (region);
}

void WFE_expand_start_section ()
{

       WN *wn;

       wn = WN_CreatePragma(WN_PRAGMA_SECTION, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0); 
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
// If not required, the following 3 lines can be removed
       WN * body = WN_CreateBlock ();
       WFE_Stmt_Push (body, wfe_stmk_scope, Get_Srcpos());
//////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push(wfe_omp_section,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
      
     
};

#ifdef TARG_SL2 //fork_joint
void WFE_expand_start_sl2_sections (BOOL is_minor_thread)
{
       /* create a region on current block */
       
      WN * region = WFE_region(is_minor_thread ? REGION_KIND_MINOR : REGION_KIND_MAJOR);

       WN *wn, *expr;
       WN_list *wn_list;
       ST *st;
       ST_list *st_list;
       
//////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
	  
       wn = WN_CreatePragma(is_minor_thread ? WN_PRAGMA_SL2_MINOR_PSECTION_BEGIN : WN_PRAGMA_SL2_MAJOR_PSECTION_BEGIN, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0);   
       	                     
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());


       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
//       Set_PU_uplevel (Get_Current_PU ());

       // Add all other pragmas/xpragmas¡­¡­.
       WFE_Stmt_Pop (wfe_stmk_region_pragmas);

}


void WFE_expand_start_sl2_section (BOOL is_minor_thread)
{

       WN *wn;

      wn = WN_CreatePragma(WN_PRAGMA_SL2_SECTION,  
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0); 
       WN_set_pragma_omp(wn);

       WFE_Stmt_Append (wn, Get_Srcpos());     
};

void WFE_expand_end_sl2_section ( )
{
       WFE_Stmt_Pop (wfe_stmk_scope);
};



void WFE_expand_end_sl2_sections ( )
{
    WFE_Stmt_Pop (wfe_stmk_scope);
};

#endif 


void WFE_check_section ( )
{
  int i;
  bool chkflag=false;
  char * msg = NULL;

  if(WFE_CS_Find (wfe_omp_sections) >= 0 ||
     WFE_CS_Find (wfe_omp_parallel_sections) >= 0)
    return;   //check passed

  msg = "Section directive appeared outside the lexical extent of  \
	     	     directive sections or directive parallel sections.";
  chkflag=true;
  WFE_omp_error(WFE_CS_top(), chkflag, msg);
  return;
}

void WFE_expand_end_section ( )
{
// If a block not required for a pragma_section, the following lines can be removed and
// left this function doing nothing
       WN *wn = WFE_Stmt_Top ();
       WFE_check_section();
       WFE_Stmt_Pop (wfe_stmk_scope);
       WFE_Stmt_Append (wn, Get_Srcpos());
       
       WFE_CS_pop(wfe_omp_section);
}

void WFE_expand_end_sections ( )
{
    WN *wn = WFE_Stmt_Top ();
    WFE_maybe_call_dtors (wn);
    WFE_maybe_localize_vars (wn);
    WFE_check_sections (wn);
    WFE_Stmt_Pop (wfe_stmk_scope);
    WFE_CS_pop(wfe_omp_sections);
}

/////////////////////////
//////  single directive
void WFE_check_single ()
{ 
    bool chkflag=false;
    char * msg = NULL;


    WN* wn1,*wn2,*wn3,*wn4,*wn5,*wn6;
    CHECK_STMT *cs1,*cs2,*cs3;
    bool fg1,fg2,fg3,fg4;
    int find=-1;
    
    // to check for "sections" directive node represented by sections_wn 
    // not to check the clause part of the sections directive
    // only check the current block, not necessory to check into lower nested levels
    
    //solve directive nesting problem: for, sections, and single directives that bind 
    //to the same parallel are not  allowed to be nested inside each other.
    if( WFE_bind_to_same(wfe_omp_single,wfe_omp_for,wfe_omp_parallel) ||
        WFE_bind_to_same(wfe_omp_single,wfe_omp_sections,wfe_omp_parallel) )
    {
    	msg = "for, sections, and single directives that bind to the same parallel  \
    	   are not  allowed to be nested inside each other."; 
    	chkflag=true;
    }
    //solve directive nesting problem:for, sections, and single directives are not 
    //permitted in the dynamic extent of critical, ordered, and master regions if the
    // directives bind to the same parallel as the regions.
    if( WFE_bind_to_same(wfe_omp_single,wfe_omp_critical,wfe_omp_parallel)||
        WFE_bind_to_same(wfe_omp_single,wfe_omp_ordered,wfe_omp_parallel)||
        WFE_bind_to_same(wfe_omp_single,wfe_omp_master,wfe_omp_parallel))
        
    {
    	msg = "for, sections, and single directives are not permitted in  \
    	the dynamic extent of critical, ordered, and master regions if the directives \
    	bind to the same parallel as the regions.";  
    	chkflag=true;
    }    
    //deal with copyprivate 
    fg1=fg2=fg3=fg4=false;
    cs1=WFE_CS_top();
    fg1=WFE_Check_Cflag(cs1, clause_copyprivate);
    if(fg1&&(WFE_CS_Find(wfe_omp_parallel)>=0))
	{
      cs2=WFE_CS_enclose();
      cs3=WFE_CS_Find_Rtn(wfe_omp_parallel);
      wn1=cs1->wn_prag;
      wn2=cs2->wn_prag;
      wn3=WN_first(wn1);
      wn4=WN_first(wn2);
      wn5=cs3->wn_prag;
      wn6=WN_first(wn5);

      if((cs2!=NULL)&&(cs2->kind>=wfe_omp_parallel&&cs2->kind<=wfe_omp_parallel_for))
      {
          while(wn3!=NULL)
           {
             // Shouldn't wn4 be initialized in each iteration of this loop?
             if(WN_pragma(wn3)!=WN_PRAGMA_COPYPRIVATE)
             	{ 
             	  wn3=WN_next(wn3);
             	  continue;
             	}
             if (ST_is_thread_private (WN_st (wn3)))
               fg2 = true;
             if (!fg2)
               while(wn4!=NULL)
               {
                  if(WN_pragma(wn4)==WN_PRAGMA_LOCAL&&WN_st_idx(wn3)==WN_st_idx(wn4))
                  {
                    fg2=true;
                    break;
                  }
                  wn4=WN_next(wn4);
               }
            
             if(fg2==false)
             {
               msg = "A single directive with copyprivate clause encountered in the \
               	dynamic extent of parrellel region, but the variables speicified copyprivate are not \
               	private in the enclosing context.";
               chkflag=true;
             }  

             wn3=WN_next(wn3);    
          	}
      }
	}
	
    fg1=WFE_Check_Cflag(cs1, clause_copyprivate);
    fg2=WFE_Check_Cflag(cs1, clause_private);
    fg3=WFE_Check_Cflag(cs1, clause_firstprivate);
    if(fg1&&(fg2||fg3))
    {
       wn1=cs1->wn_prag;
       wn2=wn3=WN_first(wn1);
       while(wn2)
       {
		 if(WN_pragma(wn2)==WN_PRAGMA_COPYPRIVATE)
		 	break;
         wn2=WN_next(wn2);
       }
       if(wn2!=NULL)
       {
         while(wn3)
         {
       	    if((WN_pragma(wn3)==WN_PRAGMA_LOCAL||WN_pragma(wn3)==WN_PRAGMA_FIRSTPRIVATE)
       	 	&&WN_st(wn2)==WN_st(wn3))
       	    {
       	      msg = "A variable that is specified copyprivate cannot appear in  \
       	      	private or firstprivate clause in the same single directive.";
       	      	chkflag=true;
       	    }	
       	    wn3=WN_next(wn3);
         }

       }     
    	
    }

    //deal with parallel reduction and private clause:
    cs1=WFE_CS_Find_Rtn(wfe_omp_parallel);
    cs2=WFE_CS_top();
    
    fg1=fg2=fg3=fg4=false;
    if(cs1)
    {
       fg1=WFE_Check_Cflag(cs1, clause_reduction);
       fg2=WFE_Check_Cflag(cs1, clause_private);
       fg3=WFE_Check_Cflag(cs2, clause_private);
       fg4=WFE_Check_Cflag(cs2,clause_firstprivate);
 
       //printf("a=%d,b=%d\n",a,b );
       if(fg1&&(fg3||fg4)||fg2&&fg4)
       {
           wn1=cs1->wn_prag;
           wn2=cs2->wn_prag;
           wn3=WN_first(wn1);
           wn4=WN_first(wn2);
           while(wn3!=NULL)
           {
             if(WN_pragma(wn3)!=WN_PRAGMA_REDUCTION&&
             	WN_pragma(wn3)!=WN_PRAGMA_LOCAL)
             	{ 
             	  wn3=WN_next(wn3);
             	  continue;
             	}
             while(wn4!=NULL)
             {
                
                if(  WN_pragma(wn4)==WN_PRAGMA_LOCAL&&WN_pragma(wn3)==WN_PRAGMA_REDUCTION
                	&&WN_st_idx(wn3)==WN_st_idx(wn4))
                {
                  msg = "Variables that appear in the reduction clause of a parallel  \
                  	    directive cannot  be specified in a private clause on a work-sharing  \
                  	    directive that binds to the  parallel construct.";
                 chkflag=true;
                }
                if(  WN_pragma(wn4)==WN_PRAGMA_FIRSTPRIVATE&&
                	WN_st_idx(wn3)==WN_st_idx(wn4))
                {
                  msg = "Variables that are private within a parallel region or that appear  \
                  	   in the  reduction clause of a parallel directive cannot be specified in a  \
                  	   firstprivateclause on for directive that binds to the parallel  construct.";
                  chkflag=true;
                }
            
                wn4=WN_next(wn4);
             }
             wn3=WN_next(wn3);
           } 
           
       }
    }
    //check for thread private variables:
    cs1=WFE_CS_top();
    wn1=cs1->wn_prag;
    wn2=WN_first(wn1);
    //printf("wn pragma is %d \n",WN_pragma(wn2));
    while(wn2!=NULL)
    {
      if(WN_st(wn2)==NULL)
      {
        wn2=WN_next(wn2);
        continue;
      }
      if(ST_is_thread_private(* WN_st(wn2)) )
      {
      	if(WN_pragma(wn2)!=WN_PRAGMA_COPYIN&&WN_pragma(wn2)!=WN_PRAGMA_COPYPRIVATE
      	  &&WN_pragma(wn2)!=WN_PRAGMA_MPSCHEDTYPE&&WN_pragma(wn2)!=WN_PRAGMA_IF
      	  &&WN_pragma(wn2)!=WN_PRAGMA_NUMTHREADS)
      	{
          msg = "A threadprivate variable must not appear in any clause except the copyin, \
				copyprivate, schedule, num_threads,or the if clause.";
	      chkflag=true;
      	}
      }
       //for private clause
      //for private clause
      if(WN_pragma(wn2)==WN_PRAGMA_LOCAL)
      WFE_check_private(wn2,chkflag);
      //for firstprivate clause
      if(WN_pragma(wn2)==WN_PRAGMA_FIRSTPRIVATE)
      WFE_check_firstprivate(wn2,chkflag);
      //for reduction clause
      if(WN_pragma(wn2)==WN_PRAGMA_REDUCTION)
      	WFE_check_reduction(wn2,chkflag);
      //for default clause
      if(WN_pragma(wn2)==WN_PRAGMA_DEFAULT)
      	WFE_check_default(wn2,chkflag);
      
      
      wn2=WN_next(wn2);
  
    }
    
    WFE_omp_error(WFE_CS_top(), chkflag, msg);
    return;
}

void WFE_expand_start_single (struct Single_clause_wn_type * single_clause_wn)
{
       /* create a region on current block */
       
       WN * region = WFE_region(REGION_KIND_MP);

       WN *wn, *expr;
       WN_list *wn_list;
       ST *st;
       ST_list *st_list;

       wn = WN_CreatePragma(WN_PRAGMA_SINGLE_PROCESS_BEGIN, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0);   
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push(wfe_omp_single,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       WFE_Set_Prag(WFE_Stmt_Top());
       WFE_Set_Region (region);
 
       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());

       // Add all other pragmas/xpragmas¡­¡­.
       
       /********For private_clause ***************/
       if(single_clause_wn->private_clause!=NULL)
       	   WFE_Set_Cflag(clause_private); 
       for (st_list = single_clause_wn->private_clause; st_list != NULL; st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_LOCAL, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());            
            if (WFE_handle_non_pods (var, WN_region_body (region), PRIVATE))
	      local_node_stack.push (wn);
         }

       /********For copyprivate_clause ***************/
       if(single_clause_wn->copyprivate_clause!=NULL)
       	   WFE_Set_Cflag(clause_copyprivate); 
       for (st_list = single_clause_wn->copyprivate_clause; st_list != NULL;st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_COPYPRIVATE, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For firstprivate_clause ***************/
       if(single_clause_wn->firstprivate_clause!=NULL)
       	   WFE_Set_Cflag(clause_firstprivate); 
       for (st_list = single_clause_wn-> firstprivate_clause; st_list != NULL;st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_FIRSTPRIVATE, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
            WFE_Set_Cflag(clause_firstprivate);
            if (WFE_handle_non_pods (var, WN_region_body (region),
                                     FIRSTPRIVATE))
	      local_node_stack.push (wn);
         }


       /********For nowait_clause ***************/
       if (single_clause_wn->nowait_clause)
         {
            wn = WN_CreatePragma(WN_PRAGMA_NOWAIT, (ST_IDX)NULL, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }
     
       WFE_Stmt_Pop (wfe_stmk_region_pragmas);

       if (!dtor_call_stack.empty() &&
           WN_operator (dtor_call_stack.top()) == OPR_CALL)
         dtor_call_stack.push (region);

       if (!local_node_stack.empty() &&
           WN_operator (local_node_stack.top()) == OPR_PRAGMA)
         local_node_stack.push (region);
}

void WFE_expand_end_single ()
{
    WN *wn = WFE_Stmt_Top ();
    WFE_maybe_call_dtors (wn);
    WFE_maybe_localize_vars (wn);
    WFE_check_single ();
    WFE_Stmt_Pop (wfe_stmk_scope);
    WFE_CS_pop(wfe_omp_single);
}

/////////////////////////
//////  parallel_for directive
void WFE_check_parallel_for ( WN *parallel_for_wn)
{
    // to check for parallel_for directive node represented by parallel_for_wn 
    // not to check the clause part of the parallel_for directive
    // only check the current block, not necessory to check into lower nested levels

    //directive parallel for combined the restrictions of directive parallel and for:
    bool chkflag=false;

   
    char * msg = NULL;
    CHECK_STMT* cs1;
    WN* wn1,*wn2,*wn3;
    bool fg1,fg2,fg3,fg4;
    fg1=fg2=fg3=fg4=false;
    
    if (WN_operator(parallel_for_wn)!=OPR_BLOCK)
    {
    	fprintf(stderr,"WFE_check_parallel_for can't deal with Non-block item!\n");
	chkflag = true;
    }
    else if (WN_first (parallel_for_wn) &&
             WN_operator (WN_first (parallel_for_wn)) == OPR_DO_LOOP)
    { // check that loop iteration variable is not threadprivate
      ST * index_st = WN_st (WN_index (WN_first (parallel_for_wn)));
      if (ST_is_thread_private (index_st))
      {
        msg = "Warning: A for loop iteration variable may not appear in a threadprivate directive.";
        chkflag = true;
      }
    }
    
  //check for thread private variables:
    cs1=WFE_CS_top();
    wn1=cs1->wn_prag;
    wn2=wn3=WN_first(wn1);
    while(wn2!=NULL)
    {
      if(WN_st(wn2)==NULL)
      {
        wn2=WN_next(wn2);
        continue;
      }
      //for private clause
      if (WN_pragma(wn2)==WN_PRAGMA_LOCAL)
        WFE_check_private(wn2,chkflag);
      //for firstprivate clause
      if (WN_pragma(wn2)==WN_PRAGMA_FIRSTPRIVATE)
        WFE_check_firstprivate(wn2,chkflag);
       //for lastprivate clause
      if (WN_pragma(wn2)==WN_PRAGMA_LASTLOCAL)
        WFE_check_lastprivate(wn2,chkflag);
      //for reduction clause
      if (WN_pragma(wn2)==WN_PRAGMA_REDUCTION)
      	WFE_check_reduction(wn2,chkflag);
      //for default clause
      if(WN_pragma(wn2)==WN_PRAGMA_DEFAULT)
      	WFE_check_default(wn2,chkflag);
       
      //for copyin clause:
      if(WN_pragma(wn2) == WN_PRAGMA_COPYIN &&
         !ST_is_thread_private(* WN_st(wn2)))
      	{ 
          msg = "A variable that is specified in the copyin clause  \
				must be a threadprivate variable.";
	  chkflag=true;
      	}
      //for threadprivate clause
      if(ST_is_thread_private(* WN_st(wn2)) )
      {
      	if(WN_pragma(wn2)!=WN_PRAGMA_COPYIN&&WN_pragma(wn2)!=WN_PRAGMA_COPYPRIVATE
      	  &&WN_pragma(wn2)!=WN_PRAGMA_MPSCHEDTYPE&&WN_pragma(wn2)!=WN_PRAGMA_IF
      	  &&WN_pragma(wn2)!=WN_PRAGMA_NUMTHREADS)
      	{
          msg = "A threadprivate variable must not appear in any clause except the copyin, \
				copyprivate, schedule, num_threads, or the if clause.";
	      chkflag=true;
      	}
      }  
      //check for parallel reduction clause
        if(WN_pragma(wn2)!=WN_PRAGMA_REDUCTION&&
             	WN_pragma(wn2)!=WN_PRAGMA_LOCAL)
         { 
            wn2=WN_next(wn2);
            continue;
         }
       fg1=WFE_Check_Cflag(cs1, clause_reduction);
       fg2=WFE_Check_Cflag(cs1, clause_private);
       fg3=WFE_Check_Cflag(cs1,clause_firstprivate);
       fg4=WFE_Check_Cflag(cs1,clause_lastprivate); 
      if(!(fg1&&(fg2||fg3||fg4)||fg2&&(fg3||fg4)))
      {
	wn2 = WN_next (wn2);
      	continue;
      }
      while(wn3!=NULL)
      	{
      	  if( WN_pragma(wn3)==WN_PRAGMA_LOCAL&&WN_pragma(wn2)==WN_PRAGMA_REDUCTION
                	&&WN_st(wn3)==WN_st(wn2))
          {
            msg = "Variables that appear in the reduction clause of a parallel  \
                  directive cannot  be specified in a private clause on a work-sharing  \
                 directive that binds to the parallel construct.";
            chkflag=true;     
          }
          if( WN_pragma(wn3)==WN_PRAGMA_FIRSTPRIVATE&&
                	WN_st(wn3)==WN_st(wn2))
            {
             msg = "Variables that are private within a parallel region or that appear  \
                  in the  reduction clause of a parallel directive cannot be specified in a  \
                  firstprivateclause on for directive that binds to the parallel  construct.";
             chkflag=true;     
            }
          if( WN_pragma(wn3)==WN_PRAGMA_LASTLOCAL&&
                	WN_st(wn3)==WN_st(wn2))
            {
              msg = "Variables that are private within a parallel region or that appear  \
                  	in the  reduction clause of a parallel directive cannot be specified in a  \
                  	lastprivate clause on a for directive that binds to the parallel  construct.";
               chkflag=true;   
            }
          wn3=WN_next(wn3);
      	} 
       
       wn2=WN_next(wn2);
      }
      WFE_omp_error(WFE_CS_top(), chkflag, msg);

}    
 
void WFE_expand_start_parallel_for (struct Parallel_for_clause_wn_type *parallel_for_clause_wn)
{
       /* create a region on current block */
       
       WN * region = WFE_region(REGION_KIND_MP);

       WN *wn, *expr;
       WN_list *wn_list;
       ST *st;
       ST_list *st_list;
       BOOL declared_private = TRUE;

       wn = WN_CreatePragma(WN_PRAGMA_PARALLEL_DO, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0);   
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push (wfe_omp_parallel_for, SRCPOS_linenum(srcpos),
                    SRCPOS_filenum(srcpos));
       WFE_Set_Prag (WFE_Stmt_Top());
       WFE_Set_Region (region);
       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());
      
       
       // Add all other pragmas/xpragmas¡­¡­.

       /********For if_clause ***************/
       expr = parallel_for_clause_wn->if_clause;
       if (expr)
         {
            wn = WN_CreateXpragma(WN_PRAGMA_IF, (ST_IDX) NULL, 1);
            WN_kid0(wn) = expr;
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For num_threads_clause ***************/
       expr = parallel_for_clause_wn->num_threads_clause;
       if (expr)
         {
            wn = WN_CreateXpragma(WN_PRAGMA_NUMTHREADS, 
            	                   (ST_IDX) NULL, 
            	                   1);
            WN_kid0(wn) = expr;
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For default_clause ***************/
       enum default_type  default_value = parallel_for_clause_wn->default_clause;
       if (default_value != no_default)
         {
            wn = WN_CreatePragma(WN_PRAGMA_DEFAULT, 
            	                  (ST_IDX) NULL, 
            	                  default_value,
            	                  0);    //To be completed for arg1 amd arg2.
            	                  
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For private_clause ***************/
       if(parallel_for_clause_wn->private_clause!=NULL)
       	   WFE_Set_Cflag(clause_private); 
       for (st_list = parallel_for_clause_wn->private_clause; st_list != NULL; st_list = st_list->next)
         {
            tree var = st_list->var;
            if (!var)
            {
              FmtAssert (declared_private, ("private var handling error"));
              declared_private = FALSE;
              continue;
            }
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_LOCAL, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
            WFE_Set_Cflag(clause_private);
            if (declared_private &&
                WFE_handle_non_pods (var, WN_region_body (region), PRIVATE))
	      local_node_stack.push (wn);
         }

       /********For shared_clause ***************/
       if(parallel_for_clause_wn->shared_clause!=NULL)
       	   WFE_Set_Cflag(clause_shared); 
       for (st_list = parallel_for_clause_wn-> shared_clause; st_list != NULL; st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_SHARED, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For firstprivate_clause ***************/
       if(parallel_for_clause_wn->firstprivate_clause!=NULL)
       	   WFE_Set_Cflag(clause_firstprivate); 
       for (st_list = parallel_for_clause_wn->firstprivate_clause; st_list != NULL;st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_FIRSTPRIVATE, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
            WFE_Set_Cflag(clause_firstprivate);
            if (WFE_handle_non_pods (var, WN_region_body (region),
                                     FIRSTPRIVATE))
	      local_node_stack.push (wn);
         }

       /********For copyin_clause ***************/
       if(parallel_for_clause_wn->copyin_clause!=NULL)
       	   WFE_Set_Cflag(clause_copyin); 
       for (st_list = parallel_for_clause_wn->copyin_clause; st_list != NULL;st_list = st_list->next) 
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_COPYIN, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }   

       /********For reduction_clause ***************/
       if(parallel_for_clause_wn->reduction_clause!=NULL)
       	   WFE_Set_Cflag(clause_reduction); 
       for (wn_list = parallel_for_clause_wn-> reduction_clause; wn_list != NULL; wn_list = wn_list->next) 
         {
            wn = wn_list->wn;
            WFE_Stmt_Append (wn, Get_Srcpos());   
         }   

       /********For lastprivate_clause ***************/
       if(parallel_for_clause_wn->lastprivate_clause!=NULL)
       	   WFE_Set_Cflag(clause_lastprivate);
       for (st_list = parallel_for_clause_wn-> lastprivate_clause; st_list != NULL;st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_LASTLOCAL, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
            WFE_Set_Cflag(clause_lastprivate);
         }


       /********For ordered_clause ***************/
       if(parallel_for_clause_wn->ordered_clause)
       	   WFE_Set_Cflag(clause_ordered); 
       if (parallel_for_clause_wn->ordered_clause)
         {
            wn = WN_CreatePragma(WN_PRAGMA_ORDERED, (ST_IDX)NULL, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
            // set parallel_for with ordered clause for check of directive ordered
            CHECK_STMT *cs;
            cs=WFE_CS_top();
        
         }

       /********For schedule_1_clause ***************/
       if (parallel_for_clause_wn->schedule_1_clause != SK_NONE)
         { 
            WN_PRAGMA_SCHEDTYPE_KIND schedtype_kind;
            switch(parallel_for_clause_wn->schedule_1_clause) {
               case SK_STATIC:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_SIMPLE;
                  break;
               case SK_DYNAMIC:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_DYNAMIC;
                  break;
               case SK_GUIDED:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_GSS;
                  break;
               case SK_RUNTIME:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_RUNTIME;
                  break;
            }

            wn = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                  (ST_IDX)NULL, 
                                  schedtype_kind,
                                  0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
          }

       /********For schedule_2_clause ***************/
       if (parallel_for_clause_wn->schedule_2_clause. schedule_2_kind != SK_NONE)
         { 
            WN_PRAGMA_SCHEDTYPE_KIND schedtype_kind;
            switch(parallel_for_clause_wn->schedule_2_clause. schedule_2_kind) {
               case SK_STATIC:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_SIMPLE;
                  break;
               case SK_DYNAMIC:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_DYNAMIC;
                  break;
               case SK_GUIDED:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_GSS;
                  break;
               case SK_RUNTIME:
                  schedtype_kind = WN_PRAGMA_SCHEDTYPE_RUNTIME;
               break;
            }

            wn = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                  (ST_IDX)NULL, 
                                  schedtype_kind,
                                  0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
            
            wn = WN_CreateXpragma(WN_PRAGMA_CHUNKSIZE,
                                  (ST_IDX) NULL,
                                  1);
            WN_kid0(wn) = parallel_for_clause_wn->schedule_2_clause. chunk_size_wn;
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());

         }

       WFE_Stmt_Pop (wfe_stmk_region_pragmas);
       
       if (!dtor_call_stack.empty() &&
           WN_operator (dtor_call_stack.top()) == OPR_CALL)
         dtor_call_stack.push (region);

       if (!local_node_stack.empty() &&
           WN_operator (local_node_stack.top()) == OPR_PRAGMA)
         local_node_stack.push (region);
}

void WFE_expand_end_parallel_for ()
{
  
    WN *wn = WFE_Stmt_Top ();
    WFE_maybe_call_dtors (wn);
    WFE_maybe_localize_vars (wn);
    WFE_check_parallel_for (wn);
    WFE_Stmt_Pop (wfe_stmk_scope);
    WFE_CS_pop(wfe_omp_parallel_for);
}

/////////////////////////
//////  parallel_sections directive
void WFE_check_parallel_sections ( WN *parallel_sections_wn)
{
    // to check for parallel_sections directive node represented by parallel_sections_wn 
    // not to check the clause part of the parallel_sections directive
    // only check the current block, not necessory to check into lower nested levels

    bool chkflag=false;
    char * msg = NULL;

    
    CHECK_STMT* cs1;
    WN* wn1,*wn2,*wn3;
    bool fg1,fg2,fg3,fg4;
    fg1=fg2=fg3=fg4=false;
    
    cs1=WFE_CS_top();
    wn1=cs1->wn_prag;
    wn2=wn3=WN_first(wn1);
    while(wn2!=NULL)
    {
      if(WN_st(wn2)==NULL)
      {
        wn2=WN_next(wn2);
        continue;
      }
      //for private clause
      if(WN_pragma(wn2)==WN_PRAGMA_LOCAL)
      WFE_check_private(wn2,chkflag);
      //for firstprivate clause
      if(WN_pragma(wn2)==WN_PRAGMA_FIRSTPRIVATE)
      WFE_check_firstprivate(wn2,chkflag);
       //for lastprivate clause
      if(WN_pragma(wn2)==WN_PRAGMA_LASTLOCAL)
      WFE_check_lastprivate(wn2,chkflag);
      //for reduction clause
      if(WN_pragma(wn2)==WN_PRAGMA_REDUCTION)
      	WFE_check_reduction(wn2,chkflag);
      //for default clause
      if(WN_pragma(wn2)==WN_PRAGMA_DEFAULT)
      	WFE_check_default(wn2,chkflag);
      
      
      //for copyin clause:
      if(WN_pragma(wn2)==WN_PRAGMA_COPYIN)
      	{ 
          if(!ST_is_thread_private(* WN_st(wn2)))
          	msg = "A variable that is specified in the copyin clause  \
          	     must be a threadprivate variable.";
      	}
      if(ST_is_thread_private(* WN_st(wn2)) )
      {
      	if(WN_pragma(wn2)!=WN_PRAGMA_COPYIN&&WN_pragma(wn2)!=WN_PRAGMA_COPYPRIVATE
      	  &&WN_pragma(wn2)!=WN_PRAGMA_MPSCHEDTYPE&&WN_pragma(wn2)!=WN_PRAGMA_IF
      	  &&WN_pragma(wn2)!=WN_PRAGMA_NUMTHREADS)
      	{
          msg = "A threadprivate variable must not appear in any clause except the copyin, \
				copyprivate, schedule, num_threads,or the if clause.";
	      chkflag=true;
      	}
      }    
      //check for parallel reduction clause
        if(WN_pragma(wn2)!=WN_PRAGMA_REDUCTION&&
             	WN_pragma(wn2)!=WN_PRAGMA_LOCAL)
         { 
            wn2=WN_next(wn2);
            continue;
         }
       fg1=WFE_Check_Cflag(cs1, clause_reduction);
       fg2=WFE_Check_Cflag(cs1, clause_private);
       fg3=WFE_Check_Cflag(cs1,clause_firstprivate);
       fg4=WFE_Check_Cflag(cs1,clause_lastprivate); 
      if(!(fg1&&(fg2||fg3||fg4)||fg2&&(fg3||fg4)))
      {
	wn2 = WN_next (wn2);
      	continue;
      }
      while(wn3!=NULL)
      	{
      	  if( WN_pragma(wn3)==WN_PRAGMA_LOCAL&&WN_pragma(wn2)==WN_PRAGMA_REDUCTION
                	&&WN_st(wn3)==WN_st(wn2))
          {
            msg = "Variables that appear in the reduction clause of a parallel  \
                  directive cannot  be specified in a private clause on a work-sharing  \
                 directive that binds to the  parallel construct.";
            chkflag=true;     
          }
          if( WN_pragma(wn3)==WN_PRAGMA_FIRSTPRIVATE&&
                	WN_st(wn3)==WN_st(wn2))
            {
             msg = "Variables that are private within a parallel region or that appear  \
                  in the  reduction clause of a parallel directive cannot be specified in a  \
                  firstprivateclause on for directive that binds to the parallel  construct.";
             chkflag=true;     
            }
          if( WN_pragma(wn3)==WN_PRAGMA_LASTLOCAL&&
                	WN_st(wn3)==WN_st(wn2))
            {
               msg = "Variables that are private within a parallel region or that appear  \
                  	in the  reduction clause of a parallel directive cannot be specified in a  \
                  	lastprivate clause on a for directive that binds to the parallel construct.";
               chkflag=true;   
            }
          wn3=WN_next(wn3);
      	} 
       
       wn2=WN_next(wn2);
      }
    WFE_omp_error(WFE_CS_top(), chkflag, msg);
}  

void WFE_expand_start_parallel_sections (struct Parallel_sections_clause_wn_type *parallel_sections_clause_wn)
{
       /* create a region on current block */
       
       WN * region = WFE_region(REGION_KIND_MP);

       WN *wn, *expr;
       WN_list *wn_list;
       ST *st;
       ST_list *st_list;
       BOOL declared_private = TRUE;

       

       wn = WN_CreatePragma(WN_PRAGMA_PARALLEL_SECTIONS, 
       	                     (ST_IDX) NULL, 
       	                     0, 
       	                     0);   
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());

       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push (wfe_omp_parallel_sections, SRCPOS_linenum(srcpos),
                    SRCPOS_filenum(srcpos));
       WFE_Set_Prag (WFE_Stmt_Top());
       WFE_Set_Region (region);

       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());
      
       
       // Add all other pragmas/xpragmas¡­¡­.

       /********For if_clause ***************/
       expr = parallel_sections_clause_wn->if_clause;
       if (expr)
         {
            wn = WN_CreateXpragma(WN_PRAGMA_IF, (ST_IDX) NULL, 1);
            WN_kid0(wn) = expr;
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For num_threads_clause ***************/
       expr = parallel_sections_clause_wn->num_threads_clause;
       if (expr)
         {
            wn = WN_CreateXpragma(WN_PRAGMA_NUMTHREADS, 
            	                   (ST_IDX) NULL, 
            	                   1);
            WN_kid0(wn) = expr;
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For default_clause ***************/
       enum default_type  default_value = parallel_sections_clause_wn->default_clause;
       if (default_value != no_default)
         {
            wn = WN_CreatePragma(WN_PRAGMA_DEFAULT, 
            	                  (ST_IDX) NULL, 
            	                  default_value,
            	                  0);    //To be completed for arg1 amd arg2.
            	                  
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For private_clause ***************/
       if(parallel_sections_clause_wn->private_clause!=NULL)
       	   WFE_Set_Cflag(clause_private); 
       for (st_list = parallel_sections_clause_wn->private_clause; st_list != NULL; st_list = st_list->next)
         {
            tree var = st_list->var;
            if (!var)
            {
              FmtAssert (declared_private, ("private var handling error"));
              declared_private = FALSE;
              continue;
            }
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_LOCAL, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
            if (declared_private &&
                WFE_handle_non_pods (var, WN_region_body (region), PRIVATE))
	      local_node_stack.push (wn);
         }

       /********For shared_clause ***************/
       if(parallel_sections_clause_wn->shared_clause!=NULL)
       	   WFE_Set_Cflag(clause_shared);
       for (st_list = parallel_sections_clause_wn->shared_clause; st_list != NULL; st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_SHARED, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       /********For firstprivate_clause ***************/
       if(parallel_sections_clause_wn->firstprivate_clause!=NULL)
       	   WFE_Set_Cflag(clause_firstprivate);
       for (st_list = parallel_sections_clause_wn->firstprivate_clause; st_list != NULL;st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_FIRSTPRIVATE, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
            if (WFE_handle_non_pods (var, WN_region_body (region),
                                     FIRSTPRIVATE))
	      local_node_stack.push (wn);
         }

       /********For copyin_clause ***************/
       if(parallel_sections_clause_wn->copyin_clause!=NULL)
       	   WFE_Set_Cflag(clause_copyin);
       for (st_list = parallel_sections_clause_wn->copyin_clause; st_list != NULL;st_list = st_list->next) 
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_COPYIN, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }   

       /********For reduction_clause ***************/
       if(parallel_sections_clause_wn->reduction_clause!=NULL)
       	   WFE_Set_Cflag(clause_reduction);
       
       for (wn_list = parallel_sections_clause_wn->reduction_clause; wn_list != NULL; wn_list = wn_list->next) 
         {
            wn = wn_list->wn;
            WFE_Stmt_Append (wn, Get_Srcpos());   
         }   

       /********For lastprivate_clause ***************/
       if(parallel_sections_clause_wn->lastprivate_clause!=NULL)
       	   WFE_Set_Cflag(clause_lastprivate);
       for (st_list = parallel_sections_clause_wn->lastprivate_clause; st_list != NULL;st_list = st_list->next)
         {
            tree var = st_list->var;
            Is_True (DECL_ST (var), ("ST expected in TREE var"));
            st = Get_ST (var);
            wn = WN_CreatePragma(WN_PRAGMA_LASTLOCAL, st, 0, 0);
            WN_set_pragma_omp(wn);  
            WFE_Stmt_Append (wn, Get_Srcpos());
         }

       WFE_Stmt_Pop (wfe_stmk_region_pragmas);
       
       if (!dtor_call_stack.empty() &&
           WN_operator (dtor_call_stack.top()) == OPR_CALL)
         dtor_call_stack.push (region);

       if (!local_node_stack.empty() &&
           WN_operator (local_node_stack.top()) == OPR_PRAGMA)
         local_node_stack.push (region);
}

void WFE_expand_end_parallel_sections ()
{
    WN *wn = WFE_Stmt_Top ();
    WFE_maybe_call_dtors (wn);
    WFE_maybe_localize_vars (wn);
    WFE_check_parallel_sections (wn);
    WFE_Stmt_Pop (wfe_stmk_scope);
    WFE_CS_pop(wfe_omp_parallel_sections);
}

///////// master directive ////////
void WFE_check_master()
{
	//master directives are not permitted in the dynamic extent of for, sections,
    //and single directives if the master directives bind to the same parallel 
    //as the work-sharing directives.
    bool chkflag=false;
    char * msg = NULL;

    if( WFE_bind_to_same(wfe_omp_master,wfe_omp_for,wfe_omp_parallel)||
        WFE_bind_to_same(wfe_omp_master,wfe_omp_sections,wfe_omp_parallel)||
        WFE_bind_to_same(wfe_omp_master,wfe_omp_single,wfe_omp_parallel))
    {
    	msg = "Master directives are not permitted in the dynamic extent  \
    		of for, sections,and single directives if the master directives bind to  \
    		the same parallel as the work-sharing directives."; 
       chkflag=true;
    }

    WFE_omp_error(WFE_CS_top(), chkflag, msg);
    return;
	
	
}

void WFE_expand_start_master ( )
{
       /* create a region on current block */
       
      WFE_region(REGION_KIND_MP);
      WN *wn, *expr;
      WN_list *wn_list;
      ST *st;
      wn = WN_CreatePragma(WN_PRAGMA_MASTER_BEGIN, 
      	                     (ST_IDX) NULL, 
      	                     0, 
      	                     0);   
      WN_set_pragma_omp(wn);
      WFE_Stmt_Append (wn, Get_Srcpos());
      //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push(wfe_omp_master,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));


        /////required?///////
      Set_PU_has_mp (Get_Current_PU ());
      Set_FILE_INFO_has_mp (File_info);
      Set_PU_uplevel (Get_Current_PU ());
      
      WFE_Stmt_Pop (wfe_stmk_region_pragmas);
}

void WFE_expand_end_master ()
{
      WFE_Stmt_Pop (wfe_stmk_scope);
      WFE_CS_pop(wfe_omp_master);
}

///////// critical directive ////////
void WFE_check_critical(char* name)
{
     bool chkflag=false;
     char * msg = NULL;
  
     
	 if(WFE_CS_Find_fgname(wfe_omp_critical, name)>-1)
	 {
	   msg = "Critical directives with the same name are not  \
	       allowed to be nested inside each other.";
	   chkflag=true;
	 }
	 WFE_omp_error(WFE_CS_top(), chkflag, msg);  
	 return;
	
}

void  WFE_expand_start_critical (ST *region_phrase,char* critical_name)
{
       WN *wn;
       TCON  tcon;
       //char *critical_name = NULL;

       WN * pragma_wn = wn = WN_CreatePragma(WN_PRAGMA_CRITICAL_SECTION_BEGIN,
                             region_phrase,
                             0,
                             0);

       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
       wn = WN_CreateBarrier( FALSE, 0 );
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());

       // pass on the 'st' to critical-section-end
       WFE_Stmt_Push (pragma_wn, wfe_stmk_dummy, Get_Srcpos());
       
       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push(wfe_omp_critical,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));

       // Check for the same critical name first, before setting the name,
       // since we don't want to find ourself for a match
       if (critical_name) WFE_check_critical(critical_name);
       WFE_Set_Nameflag(critical_name);

       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());

       WN * body = WN_CreateBlock ();
       WFE_Stmt_Push (body, wfe_stmk_scope, Get_Srcpos());

       

}

void  WFE_expand_end_critical ( )
{
       WN *wn;
       ST * st;

       WN *wn1 = WFE_Stmt_Top ();
       WFE_Stmt_Pop (wfe_stmk_scope);

       wn = WFE_Stmt_Top ( );
       st = WN_st (wn);
           

       WFE_Stmt_Pop ( wfe_stmk_dummy );

       WFE_Stmt_Append (wn1, Get_Srcpos());

       wn = WN_CreateBarrier( TRUE, 0 );
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
       wn = WN_CreatePragma(WN_PRAGMA_CRITICAL_SECTION_END,
                             st,
                             0,
                             0);
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
       
       WFE_CS_pop(wfe_omp_critical);

}

///////// atomic directive ////////
void  WFE_expand_start_atomic ()
{
       WN *wn;
       wn = WN_CreatePragma(WN_PRAGMA_ATOMIC, 
       	                    (ST_IDX) NULL,
       	                    0,
       	                    0);   
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());

       WN * body = WN_CreateBlock ();
       WFE_Stmt_Push (body, wfe_stmk_scope, Get_Srcpos());
       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push(wfe_omp_atomic,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       
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

void WFE_expand_end_atomic ()
{
       WN *wn = WFE_Stmt_Top ();
       WFE_Stmt_Pop (wfe_stmk_scope);
       WFE_Stmt_Append (wn, Get_Srcpos());
       WFE_CS_pop(wfe_omp_atomic);
}

///////// ordered directive ////////
void WFE_check_ordered()
{ /*(1) An ordered directive must not be in the dynamic extent of a for directive
	that does not have the ordered clause specified.
    	(2) An iteration of a loop with a for construct must not execute the same ordered
       directive more than once, and it must not execute more than one ordered directive.  */
   bool chkflag=false;
   char * msg = NULL;

   if ((WFE_CS_Find (wfe_omp_for) >=0 &&
        WFE_CS_Find_Cflag(wfe_omp_for,clause_ordered) < 0) ||
       (WFE_CS_Find (wfe_omp_parallel_for) >= 0 &&
      	WFE_CS_Find_Cflag(wfe_omp_parallel_for,clause_ordered) < 0))
      {
      	msg = "An ordered directive must not be in the dynamic extent  \
      	   of a for directive that does not have the ordered clause specified.";
      	chkflag=true;
      }
      	
    //ordered directives are not allowed in the dynamic extent of critical
    //regions  if the directives bind to the same parallel as the regions.
   if( WFE_CS_Find(wfe_omp_critical)>WFE_CS_Find(wfe_omp_parallel)
        && WFE_CS_Find(wfe_omp_parallel)>=0)     
    {
     msg = "Ordered directives are not allowed in the dynamic extent of  \
    	critical regions if the directives bind to the same parallel as the regions.";   
     chkflag=true;
    }
    WFE_omp_error(WFE_CS_top(), chkflag, msg);
    return;
      
}

void  WFE_expand_start_ordered ( )
{
       WN *wn;
       wn = WN_CreatePragma(WN_PRAGMA_ORDERED_BEGIN, 
                             (ST_IDX) NULL, 
                             0, 
                             0);   
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
       wn = WN_CreateBarrier( FALSE, 0 );
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
       
       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push(wfe_omp_ordered,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));

       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());
}

void  WFE_expand_end_ordered ( )
{
       WN *wn;
       wn = WN_CreateBarrier( TRUE, 0 );
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
       wn = WN_CreatePragma(WN_PRAGMA_ORDERED_END, (ST_IDX) NULL, 
       	                     0,
       	                     0);   
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());       
       WFE_check_ordered();
       WFE_CS_pop(wfe_omp_ordered);
}

///////// barrier directive ////////
void WFE_check_barrier ( )
{
//   (1)  A barrier directive may not appear as the immediate subordinate of a C/C++ control statement
//         (if, switch, while, do, for), and it can not be labeled (with either a user or a
//         case/default label). 
//   (2) The smallest statement that contains a barrier directive must be a block (or
//         compound-statement).

        //deal with (1) listed above
       bool chkflag=false;
       char * msg = NULL;
 
       CHECK_STMT* cs = WFE_CS_top ();
       SRCPOS srcpos = Get_Srcpos();
       WFE_Set_LFnum(cs, SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));

       
       if(WFE_is_top(wfe_cscf))
       {
         msg = "A barrier directive appeared as the immediate  \
           	subordinate of a C/C++ control statement if, switch, while, do, for.";
           chkflag=true;
      	}

       WN* wn1;                     //deal with (2) listed above
       wn1=WFE_Stmt_Top();
       if(WN_operator(wn1)!=OPR_BLOCK)
       {
  	   msg = "The smallest statement that contains a barrier directive must be a block (or \
		         compound-statement).";
		chkflag=true;
       }
       //barrier directives are not permitted in the dynamic extent of for, ordered,
       // sections, single, master, and critical regions if the directives bind to the 
       //same parallel as the regions.
       if( WFE_bind_to_same(wfe_omp_barrier,wfe_omp_for,wfe_omp_parallel)||
       	   WFE_bind_to_same(wfe_omp_barrier,wfe_omp_ordered,wfe_omp_parallel)||
           WFE_bind_to_same(wfe_omp_barrier,wfe_omp_sections,wfe_omp_parallel)||
           WFE_bind_to_same(wfe_omp_barrier,wfe_omp_single,wfe_omp_parallel)||
           WFE_bind_to_same(wfe_omp_barrier,wfe_omp_master,wfe_omp_parallel)||
           WFE_bind_to_same(wfe_omp_barrier,wfe_omp_critical,wfe_omp_parallel))
        
    {
      msg = "Barrier directives are not permitted in the dynamic extent of for, ordered, \
          sections, single, master, and critical regions if the directives bind to the  \
          same parallel as the regions."; 
        chkflag=true;
    }
    WFE_omp_error(cs, chkflag, msg);   
    return;
       
}

void  WFE_expand_barrier ()
{
       WN *wn;
       wn = WN_CreatePragma(WN_PRAGMA_BARRIER, 
                             (ST_IDX) NULL,
                             0,
                             0);   
       WN_set_pragma_omp(wn);
       WFE_Stmt_Append (wn, Get_Srcpos());
       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push(wfe_omp_barrier,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
    
       WFE_check_barrier();
 
       /////required?///////
       Set_PU_has_mp (Get_Current_PU ());
       Set_FILE_INFO_has_mp (File_info);
       Set_PU_uplevel (Get_Current_PU ());
       
       WFE_CS_pop(wfe_omp_barrier);
}


///////// flush directive ////////
void WFE_check_flush ( )
{
//   (1)  A flush directive may not appear as the immediate subordinate of a C/C++ control statement
//         (if, switch, while, do, for), and it can not be labeled (with either a user or a
//         case/default label). 
//   (2) The smallest statement that contains a flush directive must be a block (or
//         compound-statement).
     bool chkflag=false;
     char * msg = NULL;

     CHECK_STMT* cs = WFE_CS_top ();
     SRCPOS srcpos = Get_Srcpos();
     WFE_Set_LFnum(cs, SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));

   
     if(WFE_is_top(wfe_cscf))
       {
         msg = "A barrier directive appeared as the immediate subordinate of a C/C++ control statement.";
         chkflag=true;
      	}

       WN* wn1;                     //deal with (2) listed above
       wn1=WFE_Stmt_Top();
       if(WN_operator(wn1)!=OPR_BLOCK)
       {
  	    msg = "The smallest statement that contains a barrier directive must be a block (or \
		         compound-statement).";
		chkflag=true;
       }
   WFE_omp_error(cs, chkflag, msg);     	
       
}

void  WFE_expand_flush (WN_list *flush_variables)
{
       WN * sync,  * wn1,  * wn2;
       WN * wn;
       ST * st;
       WN_list * wn_list;
       UINT num =0, i = 0 ;

       sync = WN_Create_Intrinsic(OPC_VINTRINSIC_CALL,
       INTRN_SYNCHRONIZE,0,NULL);

       for (wn_list = flush_variables; wn_list != NULL; wn_list = wn_list->next) 
       	  num++;
      
       wn1 = WN_CreateBarrier(TRUE, num);
       wn2 = WN_CreateBarrier(FALSE, num); 

       for (wn_list = flush_variables; wn_list != NULL; wn_list = wn_list->next) 
         {
            wn = wn_list->wn;
            st = WN_st(wn);
            if (Barrier_Lvalues_On) {
              WN_kid(wn1,i) = wn;
              WN_kid(wn2,i) = wn;
            }
            else {
              WN_kid(wn1,i) = WN_CreateIdname(0,st);
              WN_kid(wn2,i) = WN_CreateIdname(0,st);
            }
            i++;
         }   

       WFE_Stmt_Append (wn1, Get_Srcpos());
       WFE_Stmt_Append (sync, Get_Srcpos());
       WFE_Stmt_Append (wn2, Get_Srcpos());
       //////////////// OPENMP CHECK STACK /////////////
       SRCPOS srcpos = Get_Srcpos();
       WFE_CS_push(wfe_omp_flush,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       WFE_check_flush();
       WFE_CS_pop(wfe_omp_flush);
       
}

///////// threadprivate directive ////////
void WFE_check_threadprivate(ST_list* threadprivate_variables)
{
    bool chkflag=false;


    ST* st;
    ST_list *st_list;
    SYMTAB_IDX si1,si2;

    si1=PU_lexical_level(Get_Current_PU());

    st_list=threadprivate_variables;
    while(st_list!=NULL)
    {
      st = Get_ST (st_list->var);
      if(ST_is_thread_private(* st))
      {
        si2=ST_level(st);
      	if(si1!=si2&&ST_storage_class(*st)==SCLASS_PSTATIC)
      	{
          fprintf(stderr,"Warning: A threadprivate directive for static block-scope variables must appear in \
 				the   scope of the variable and not in a nested scope.\n");
	      chkflag=true;
      	}
      }
     st_list=st_list->next;  
    }
    /*printf("entering check threadprivate:: \n");
    st_list=threadprivate_variables;
    while(st_list!=NULL)
    {
      s=st_list->st;
      if(ST_is_thread_private(* s))
      {
      	if(!ST_is_not_used(*s))
      	{
          printf("Warning: The threadprivate directive must lexically  precede all references 
          	to any of the variables in its list.\n");
	  
      	}
      }
     st_list=st_list->next;  
    }   */
   // WFE_omp_error(wfe_omp_threadprivate, chkflag );
    
}

void WFE_expand_start_do_loop(WN * index, WN * start, WN * end, WN * step)
{
     WN *doloop;

     WN * body = WN_CreateBlock ();

     doloop = WN_CreateDO(index, start, end, step, body, NULL);
     WFE_Stmt_Append (doloop, Get_Srcpos());

     WFE_Stmt_Push (doloop, wfe_stmk_for_cond, Get_Srcpos());
     WFE_Stmt_Push (body, wfe_stmk_for_body, Get_Srcpos());
}

void WFE_expand_end_do_loop (void)
{
     WFE_Stmt_Pop (wfe_stmk_for_body);

     WFE_Stmt_Pop (wfe_stmk_for_cond);
}

