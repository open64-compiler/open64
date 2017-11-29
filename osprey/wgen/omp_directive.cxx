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

#include "srcpos.h"

#include "ir_reader.h"
#include <cmplrs/rcodes.h>
extern "C"{
#include "gspin-wgen-interface.h"
}
#include "wgen_expr.h"
#include "wgen_misc.h"
#include "wgen_dst.h"
#include "wgen_stmt.h"
#include "wgen_spin_symbol.h"
#include "omp_types.h"
#include "omp_directive.h"
#include "wgen_omp_directives.h"
#include "wgen_omp_check_stack.h"

#include <stdio.h>
#include "errors.h"
#include "const.h"

extern std::vector<WN *> doloop_side_effects;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


// c_split_parallel_clauses() in GNU splits combined parallel clauses. We
// combine them back here, and we remove the NOWAIT clause.
void
expand_start_parallel_or_combined_parallel (gs_t stmt)
{
  gs_t stmts = gs_omp_parallel_body(stmt);

  // bug 13686: Disable the following for C++, i.e., split a combined-parallel
  // construct into two for C++. c_split_parallel_clauses in GNU has been
  // fixed for lastprivates. This splitting is useful for C++ due to the need
  // to insert constructors/destructors outside the DO region, and inside
  // an enclosing parallel region.
  if (!lang_cplus &&
      gs_omp_parallel_combined(stmt) && // Combined by GNU?
      // Make sure it is in the expected format for us to be able to combine.
      gs_tree_code(stmts) == GS_BIND_EXPR)
  {
    gs_t body = gs_bind_expr_body(stmts);

    if (gs_tree_code(body) == GS_OMP_FOR)
    {
      expand_start_parallel_for (stmt);
      return;
    }
    else if (gs_tree_code(body) == GS_OMP_SECTIONS)
    {
      expand_start_parallel_sections (stmt);
      return;
    }
  }

  expand_start_parallel (stmt);
}

void
expand_start_parallel (gs_t stmt)
{
     WGEN_expand_start_parallel (stmt);
     WGEN_Expand_Stmt (gs_omp_parallel_body(stmt));
     expand_end_parallel ();

}

void
expand_end_parallel ()
{
     WGEN_expand_end_parallel ();
}



extern void WGEN_Expand_DO (gs_t);

void
expand_start_for (gs_t stmt)
{
     WGEN_expand_start_for (stmt);
     WGEN_Expand_DO (stmt);
     expand_end_for ();
}

void
expand_end_for (void)
{
     WGEN_expand_end_for ();
}



void
expand_start_sections (gs_t stmt)
{
  WGEN_expand_start_sections (stmt);
  WGEN_Expand_Stmt (gs_omp_sections_body(stmt));
  expand_end_sections ();
}

void
expand_end_sections (void)
{
  WGEN_expand_end_sections ();
}


///////// section directive ////////


void expand_start_section (gs_t stmt)
{
  Is_True (gs_tree_code(stmt) == GS_OMP_SECTION,
           ("expand_start_section: invalid statement type"));
  WGEN_expand_start_section ();
  WGEN_Expand_Stmt (gs_omp_section_body(stmt));
  expand_end_section ();
}

void expand_end_section (void)
{
  WGEN_expand_end_section ();
}

///////// single directive ////////



void
expand_start_single (gs_t stmt)
{
  WGEN_expand_start_single (stmt);
  WGEN_Expand_Stmt (gs_omp_single_body(stmt));
  expand_end_single ();
}

void expand_end_single( )
{
     WGEN_expand_end_single ();
}



void
expand_start_parallel_for (gs_t stmt)
{
     WGEN_expand_start_parallel_for (stmt);
     WGEN_Expand_DO (gs_bind_expr_body(gs_omp_parallel_body(stmt)));
     expand_end_parallel_for ();
}

void
expand_end_parallel_for (void)
{
     WGEN_expand_end_parallel_for ();
}



void
expand_start_parallel_sections(gs_t stmt)
{
  WGEN_expand_start_parallel_sections (stmt);
  WGEN_Expand_Stmt (gs_omp_sections_body(
                     gs_bind_expr_body(
                      gs_omp_parallel_body(stmt))));
  expand_end_parallel_sections ();
}

void
expand_end_parallel_sections(void)
{
    WGEN_expand_end_parallel_sections ();
}

///////// master directive ////////


void expand_start_master (gs_t stmt)
{
     WGEN_expand_start_master ();
     WGEN_Expand_Stmt (gs_omp_master_body(stmt));
     expand_end_master ();
}

void expand_end_master (void)
{
     WGEN_expand_end_master ();
}

///////// critical directive ////////


void expand_start_critical( gs_t stmt )
{
    char *critical_name = NULL;
    ST *st = NULL;        
    TCON           tcon;
    TY_IDX ty;

    gs_t region_phrase = gs_omp_critical_name(stmt);
    if (region_phrase)
    {
      critical_name = gs_identifier_pointer (region_phrase);
      tcon = Host_To_Targ_String (MTYPE_STRING, critical_name,
                                  strlen(critical_name));
      st = Gen_String_Sym (&tcon, MTYPE_To_TY(MTYPE_STRING), FALSE);
    }
    if (Trace_Omp)
      printf("critical name is %s \n",critical_name);
    WGEN_expand_start_critical (st, critical_name);
    WGEN_Expand_Stmt (gs_omp_critical_body(stmt));
    expand_end_critical ();
}
 
void expand_end_critical (void)
{
      WGEN_expand_end_critical ( );
}



void  expand_start_atomic (gs_t stmt)
{
    WGEN_expand_start_atomic ();
    WN * rhs_wn = WGEN_Expand_Expr (gs_tree_operand(stmt,1));
    gs_t lhs = gs_tree_operand(stmt,0);
    WGEN_Lhs_Of_Modify_Expr (GS_MODIFY_EXPR, lhs, NULL,
                             0, 0, 0, 0, 0, rhs_wn, 0, 0, 0);
    expand_end_atomic ();
}

void  expand_end_atomic (void)
{
    WGEN_expand_end_atomic ();
}

///////// ordered directive ////////


void expand_start_ordered(gs_t stmt)
{
    WGEN_expand_start_ordered ();
    WGEN_Expand_Stmt (gs_omp_ordered_body(stmt));
    expand_end_ordered ();
} 

void expand_end_ordered (void)
{
    WGEN_expand_end_ordered ();
}


extern OPERATOR Operator_From_Tree_node (gs_code_t);
extern bool Has_Subsumed_Cvtl (OPERATOR);

static void save_block_stmts(WN *wn)
{
    WN *i;
    for (i = WN_first(wn); i; i = WN_next(i))
      doloop_side_effects.push_back(WN_COPY_Tree(i));
    return;
}

// it should be ldid lcv
static bool expand_load_lcv(WN **wn, ST_IDX lcv, gs_t expr)
{
    WN *wn_tmp;
    WN *result;

    WGEN_Stmt_Push(WN_CreateBlock (), wgen_stmk_comma, Get_Srcpos());
    result = WGEN_Expand_Expr (expr);
    Check_For_Call_Region();
    wn_tmp = WGEN_Stmt_Pop (wgen_stmk_comma);

    if (WN_first(wn_tmp) == NULL)
    {
       if (WN_operator(result) == OPR_LDID && WN_st_idx(result) == lcv)
       {
         *wn = result;
         return true;
       }
    }
    return false;
}

// expand the rhs expression, and move any node with side effects into the side
// effect stack. The result is cached by a temp variable named by the prefix.
//
// specifically, we deal with block expressions and call with exceptions.
static WN *expand_rhs_expr_and_cache_doloop_side_effects(gs_t expr, const char *prefix)
{
    WN *wn_tmp;
    WN *result;

    WGEN_Stmt_Push(WN_CreateBlock (), wgen_stmk_comma, Get_Srcpos());
    result = WGEN_Expand_Expr (expr);
    if (WN_has_side_effects(result))
    {
      TY_IDX ty_idx = MTYPE_TO_TY_array[WN_rtype(result)];
      ST * st = Gen_Temp_Symbol (ty_idx, prefix);
      // Mark as local in surrounding parallel region.
      WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st, TRUE);
      WN * stid = WN_Stid (TY_mtype(ty_idx), 0, st, ty_idx, result);
      WN_Set_Linenum(stid, Get_Srcpos());

      WN_INSERT_BlockAfter(WGEN_Stmt_Top(), NULL, stid);
      Check_For_Call_Region();
      result = WN_Ldid (TY_mtype(ty_idx), 0, st, ty_idx);
    }
    wn_tmp = WGEN_Stmt_Pop (wgen_stmk_comma);
    save_block_stmts(wn_tmp);
    WN_DELETE_Tree(wn_tmp);
    return result; 
}
static bool expand_increment_term(WN **wn, ST_IDX lcv, gs_t expr)
{
    WN *result;
    bool is_ldid_lcv = false;
    result = expand_rhs_expr_and_cache_doloop_side_effects(expr, "__doloop_incr");

    if (WN_operator(result) == OPR_LDID && WN_st_idx(result) == lcv)
    {
      is_ldid_lcv = true;
    }
    *wn = result;
    return !is_ldid_lcv;
}

void expand_start_do_loop (gs_t init_expr, gs_t logical_expr, gs_t incr_expr)
{
    bool valid_for_expr = true;
    gs_code_t code, code1;
    gs_t temp; 
    
    WN *index, *start, *end, *step;
    
    WN *wn_tmp,  * lcv , * incv;
    WN *wn,*wn1;
    WN *tdecl;
    ST *tst;
    ST_IDX stlcv;
    char *detail = NULL;

    TYPE_ID lcv_t;

    FmtAssert (doloop_side_effects.empty(),
               ("Expected DO-loop side-effects to be empty."));
    code = gs_tree_code (init_expr);

    if (code != GS_MODIFY_EXPR && code != GS_VAR_DECL) 
    {
      detail = ("invalid init expression");
      valid_for_expr = false;
      WGEN_Stmt_Push (WN_CreateBlock (), wgen_stmk_comma, Get_Srcpos());
    }
    else 
    {
      if(code == GS_VAR_DECL)
      {
        tst = DECL_ST(init_expr);
        index = WN_CreateIdname(0,tst);
        WGEN_Stmt_Push (WN_CreateBlock (), wgen_stmk_comma, Get_Srcpos());
              
        wn1 = WGEN_Expand_Expr (gs_decl_initial(init_expr)); // r.h.s.
        wn_tmp = WGEN_Lhs_Of_Modify_Expr(GS_MODIFY_EXPR, init_expr, NULL, FALSE, 
                                         0, 0, 0, FALSE, wn1, 0, FALSE, FALSE);
        wn_tmp = WGEN_Stmt_Pop (wgen_stmk_comma);
        start = WN_COPY_Tree( WN_first( wn_tmp ));
        WN_DELETE_Tree( wn_tmp );
      }
      else
      {
        lcv = WGEN_Expand_Expr (gs_tree_operand (init_expr, 0));
        lcv_t = TY_mtype(ST_type(WN_st(lcv)));

        if (lcv_t != MTYPE_I4 && lcv_t != MTYPE_I8 &&
            lcv_t != MTYPE_I2 && lcv_t != MTYPE_I1)
        {
          if (lcv_t == MTYPE_U4 || lcv_t == MTYPE_U8 ||
              lcv_t == MTYPE_U2 || lcv_t == MTYPE_U1)
            detail = ("unsigned induction variable is supported by OpenMP 3.0 and we support version 2.5");
          else
            detail = ("invalid induction variable type");
          valid_for_expr = false;		
        }
        else
        {
          index = WN_CreateIdname(0,WN_st_idx(lcv));
          WN *rhs;
          rhs = expand_rhs_expr_and_cache_doloop_side_effects(gs_tree_operand(init_expr, 1), "__doloop_init"); 
          
          WGEN_Stmt_Push (WN_CreateBlock (), wgen_stmk_comma, Get_Srcpos());
          WGEN_Lhs_Of_Modify_Expr(code, gs_tree_operand (init_expr, 0), NULL, FALSE,
                                  0, 0, 0, FALSE, rhs, 0, FALSE, FALSE);
          wn_tmp = WGEN_Stmt_Pop (wgen_stmk_comma);
          // loop init
          start = WN_COPY_Tree (WN_first (wn_tmp));
          WN_DELETE_Tree (wn_tmp);
        }
      }
    }
    code = gs_tree_code (logical_expr);
    if (valid_for_expr && (code != GS_LT_EXPR) && (code != GS_LE_EXPR) && (code != GS_GT_EXPR) && (code != GS_GE_EXPR)) 
    {
      detail = ("invalid logical expression, the logical operators can only be <=, <, > or >=");
      valid_for_expr = false;		
    }
    else
    {
        // index variable
        WN * var = WGEN_Expand_Expr(gs_tree_operand(logical_expr, 0));
        WN *ub;
        // upper bound
        //WN * ub = WGEN_Expand_Expr(gs_tree_operand(logical_expr, 1));

        ub = expand_rhs_expr_and_cache_doloop_side_effects(gs_tree_operand(logical_expr, 1), "__doloop_ub"); 

        // Mimick WGEN_Expand_Expr for type conversion.
        TY_IDX ty_idx = Get_TY (gs_tree_type(logical_expr));
        TYPE_ID mtyp = TY_mtype(ty_idx);
        TY_IDX ty_idx0 = Get_TY(gs_tree_type(gs_tree_operand(logical_expr, 0)));
        TYPE_ID mtyp0 = TY_mtype(ty_idx0);
        TY_IDX ty_idx1 = Get_TY(gs_tree_type(gs_tree_operand(logical_expr, 1)));
        TYPE_ID mtyp1 = TY_mtype(ty_idx1);

        if (MTYPE_size_min(mtyp1) > MTYPE_size_min(mtyp0) &&
            ! Has_Subsumed_Cvtl(WN_operator(var)))
          var = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp0), MTYPE_V,
                              MTYPE_size_min(mtyp0), var);
        if (MTYPE_size_min(mtyp0) > MTYPE_size_min(mtyp1) &&
            ! Has_Subsumed_Cvtl(WN_operator(ub)))
          ub = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp1), MTYPE_V,
                              MTYPE_size_min(mtyp1), ub);

        end  = WN_CreateExp2(Operator_From_Tree_node(code),
                             Widen_Mtype(mtyp), Widen_Mtype(mtyp0),
                             var, ub);
    }
    
    code = gs_tree_code (incr_expr);

    if (valid_for_expr && (code != GS_MODIFY_EXPR) && (code != GS_PREDECREMENT_EXPR) && (code != GS_PREINCREMENT_EXPR) 
           && (code != GS_POSTDECREMENT_EXPR) && (code != GS_POSTINCREMENT_EXPR)) 
    {
      detail = ("invalid step expression");
      valid_for_expr = false;		
    }
    else
    {
      incv = WGEN_Expand_Expr (gs_tree_operand (incr_expr, 0));
      if (gs_tree_code(init_expr) == GS_VAR_DECL)
        stlcv = ST_st_idx(tst);
      else
        stlcv = WN_st_idx(lcv);
      if (stlcv!=WN_st_idx(incv))
      {
        detail = ("invalid step expression"
		       	      "induction variable not modified");
        valid_for_expr = false;		
      }
      else 
      {
        code1 = gs_tree_code (gs_tree_operand (incr_expr, 1));
        if (code == GS_MODIFY_EXPR)
        {
          WN *incr_term;
          WN *term0, *term1;
          gs_t gs_term0, gs_term1;

          gs_term0 = gs_tree_operand(gs_tree_operand(incr_expr, 1), 0);
          gs_term1 = gs_tree_operand(gs_tree_operand(incr_expr, 1), 1);
          if (code1 == GS_MINUS_EXPR)
          {
            if (!expand_load_lcv(&term0, stlcv, gs_term0) || !expand_increment_term(&term1, stlcv, gs_term1))
            {
              detail = "messy step expression";
              valid_for_expr = false;
            }
          } else if (code1 == GS_PLUS_EXPR){ // GS_PLUS_EXPR
            if (expand_load_lcv(&term0, stlcv, gs_term0) && expand_increment_term(&term1, stlcv, gs_term1) ||
                expand_load_lcv(&term1, stlcv, gs_term1) && expand_increment_term(&term0, stlcv, gs_term0))
            {
              // do nothing, it should be noted that expand_increment_term has side effect, and 
	      // we always call it after sucessful call of expand_load_lcv.
            } else {
              detail = "messy step expression";
              valid_for_expr = false;
            }
          } else {
	    detail = ("invalid arithmatic operator of the step expression");
	    valid_for_expr = false;
	  }
          if (valid_for_expr)
          {
            // create the step WN 
            step  = WN_CreateExp2(Operator_From_Tree_node(gs_tree_code(gs_tree_operand(incr_expr, 1))),
			     TY_mtype(Get_TY(gs_tree_type(gs_tree_operand(incr_expr, 1)))), MTYPE_V,
                             term0, term1);
            step = WN_Stid(TY_mtype(ST_type(stlcv)), 0, WN_st(lcv), 
                           ST_type(stlcv), step);
	    WN_Set_Linenum(step, Get_Srcpos());
          }
	} else if (code == GS_POSTDECREMENT_EXPR || code == GS_POSTINCREMENT_EXPR ||
                   code == GS_PREDECREMENT_EXPR || code == GS_PREINCREMENT_EXPR)
	{
          WGEN_Stmt_Push (WN_CreateBlock (), wgen_stmk_comma, Get_Srcpos());
          WGEN_Expand_Expr (incr_expr, FALSE);
          wn_tmp = WGEN_Stmt_Pop (wgen_stmk_comma);
          step = WN_COPY_Tree( WN_last( wn_tmp ));
	  WN_DELETE_Tree( wn_tmp);
	} else {
	  detail = ("invalid arithmatic operator of the step expression");
	  valid_for_expr = false;
	}
      }
    }
    if (valid_for_expr) 
    {
      WGEN_expand_start_do_loop (index, start, end, step);
    }
    else
    {
      char *filename, *dirname; 
      SRCPOS srcpos = Get_Srcpos();
      IR_Srcpos_Filename(srcpos, (const char **)&filename, (const char**)&dirname);
      printf("%s:%d: error: invalid syntax in the omp for statement, %s.\n", 
        filename, SRCPOS_linenum(srcpos), detail);
      exit(RC_USER_ERROR);
    }
}

void expand_end_do_loop (void)
{
    WGEN_expand_end_do_loop();
}
